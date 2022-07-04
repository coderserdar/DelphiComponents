{*******************************************************}
{                                                       }
{              CA Drawing Engine Library                }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCXML;

interface

uses
  Windows, Messages, SysUtils, Classes, SCUnicodeLib, SCXMLRes;

type
  TSCDomDocument = class;

  TSCXMLError = record
    No: Integer;
    Desc: String;
  end;

  TSCTokenKind = (stkTag, stkAttribute, stkAttributeValue, stkComment, stkCData,
    stkText, stkPI, stkPIData, stkDocumentTop, stkDeclarationLoop,
    stkDeclaration, stkDTD, stkDTDElement, stkDTDElementGroup,
    stkDTDAttribute, stkDTDEntity, stkDTDNotation);

  TSCDTDElementContentType = (dtdUndefinedElement, dtdAny, dtdEmpty, dtdElement, dtdPCDATA, dtdMixed);
  TSCDTDAttributeType = (dtdUndefinedAttr, dtdCDATA, dtdENUM, dtdID, dtdIDREF, dtdIDREFS,
    dtdNMTOKEN, dtdNMTOKENS, dtdENTITY, dtdENTITIES, dtdNOTATION, dtdXML);
  TSCDTDAttributeDefault = (dtdAttrImplied, dtdAttrRequired, dtdAttrFixed, dtdAttrUndefined);
  TSCDTDElementOccurrence = (dtdElementOnlyOne, dtdElementZeroOrOne, dtdElementZeroOrMore,
    dtdElementOneOrMore);

  TSCEscapeError = (escNoError, escNeedEscapeClose, escUndefinedEscape, escInvalidCharacter,
    escInvalidNameStart, escInvalidUnicodeCharacter);

  TSCXmlInstructionError = (xieNoError, xieNoData, xieInvalidEncoding, xieInvalidVersion,
    xieInvalidStandaloneValue, xieInvalidAttribute, xieValueNotFound, xieInvalidXmlInstReplacement);

  TSCDTDError = (dteNoError, dteUserTerminated, dteSpaceRequired, dteExpectedCharacterNotFound,
    dteInvalidCharacter, dteInvalidDTDName, dteInvalidLocationType, dteInvalidPublicID,
    dteInvalidSystemID, dteInvalidParameter, dteInvalidDefination, dteInvalidEnd, dtdXMLElementFound,
    dteInvalidDeclaration, dteInvalidName, dteInvalidNameStart, dteNameExpected, dteInvalidContentModel,
    dteInvalidExternalID, dteInvalidAttributeType, dteInvalidAttributeDefault, dteInvalidAttributeDefaultValue,
    dteDuplicateElement, dteCommentNotClosed, dteCommentHasTermination);

  TSCDTDType = (dtyUndefined, dtyEmbedded, dtySystem);

  TSCDocumentEvent = procedure(Sender: TObject; var CanContinue: Boolean) of object;
  TSCTokenEvent = procedure(Sender: TObject; Line, Column: LongInt; ReleasedToken,
    EnteredToken: TSCTokenKind; const Value, Path: WideString; var CanContinue: Boolean) of object;
  TSCErrorEvent = procedure(Sender: TObject; ErrorCode, Line, Column: LongInt;
    ReleasedToken, EnteredToken: TSCTokenKind; const Value, Path: WideString) of object;
  TSCNodeEvent = procedure(Sender: TObject; const AName: WideString; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCResolveEntityEvent = procedure(Sender: TObject; const Entity: WideString; var CanContinue: Boolean) of object;
  TSCAttributeEvent = procedure(Sender: TObject; const AName, Value: WideString; Delimiter: WideChar;
    Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCTextEvent = procedure(Sender: TObject; const Chars: Widestring; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCProcessingInstructionEvent = procedure(Sender: TObject; const Target,
    Data: WideString; Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCCommentEvent = procedure(Sender: TObject; const Chars: WideString; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCCDataEvent = procedure(Sender: TObject; const CData: WideString; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCDomInternalError = procedure(Sender: TObject; ErrorCode: Integer; const ErrorValue: WideString) of object;


  TSCDTDEvent = procedure(Sender: TObject; const AName, Data: WideString; Delimiter: WideChar;
    Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCDTDStartEvent = procedure(Sender: TObject; const AName, PublicID, SystemID: WideString;
    Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCDTDFileStartEvent = procedure(Sender: TObject; const AName, PublicID, SystemID: WideString;
    Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCDTDEndEvent = procedure(Sender: TObject; const AName: WideString; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCDTDElementEvent = procedure(Sender: TObject; const AName, Child: WideString;
    ElementContentType: TSCDTDElementContentType; Occurrence: TSCDTDElementOccurrence;
    Line, Column: LongInt; var CanContinue: Boolean) of object;
  TSCDTDAttributeEvent = procedure(Sender: TObject; const Element, AName: WideString;
    AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
    const EnumValues, DefaultValue: WideString; Delimiter: WideChar; Line, Column: LongInt;
    var CanContinue: Boolean) of object;
  TSCDTDNotationEvent = procedure(Sender: TObject; const AName, PublicID, SystemID: WideString;
    Line, Column: LongInt; var CanContinue: Boolean) of object;


  TSCSaxErrorOption = (saxCheckEscapes, saxDuplicateDTDElement);
  TSCSaxErrorOptions = set of TSCSaxErrorOption;

  TSCEscapeParser = class(TObject)
  public
    function IsValidData(const Data: Widestring; var ALine, ACol: LongInt;
      var ErrorChar: WideChar; var Error: TSCEscapeError; InvalidChars: WideString): boolean;
    function ReplaceEscapes(const Data: Widestring; Validate: Boolean; var IsValid: Boolean;
      InvalidChars: WideString): WideString;
  end;

  TSCElementContentNode = class(TObject)
  private
    FNodes: TList;
    FElement: WideString;
    FOccurence: TSCDTDElementOccurrence;
    FListOperator: WideChar;
    FOwner: TSCElementContentNode;
    FIncludeSelf: Boolean;
    procedure SetOwner(Value: TSCElementContentNode);
    procedure SetElement(const Value: WideString);
    function  GetNode(Index: Integer): TSCElementContentNode;
    procedure InsertNode(N: TSCElementContentNode);
    procedure RemoveNode(N: TSCElementContentNode);
  public
    constructor Create(AOwner: TSCElementContentNode); virtual;
    destructor Destroy; override;

    function  Count: Integer;
    function  Add: TSCElementContentNode;
    procedure Delete(Index: Integer);
    procedure Clear;
    function  AsString: WideString;

    property Owner: TSCElementContentNode read FOwner write SetOwner;
    property Element: WideString read FElement write SetElement;
    property IncludeSelf: Boolean read FIncludeSelf write FIncludeSelf default True;
    property Occurence: TSCDTDElementOccurrence read FOccurence write FOccurence;
    property ListOperator: WideChar read FListOperator write FListOperator;
    property Nodes[Index: Integer]: TSCElementContentNode read GetNode;
  end;

  TSCSaxContentParser = class(TObject)
  public
    function GetOccurrence(const C: WideChar): TSCDTDElementOccurrence;
    function GetElementOccurrence(const ElmName: WideString): TSCDTDElementOccurrence;

    function ParseContent(const S: Widestring; var RootNode: TSCElementContentNode;
      var ALine, ACol: LongInt; var ExpectedChar: WideChar;
      var ErrorStr: WideString; Level: Word): TSCDTDError;
  end;

  TSCSaxDTDParser = class(TObject)
  private
    FContentParser: TSCSaxContentParser;
    FOnDTDComment: TSCCommentEvent;
    FOnDTDElement: TSCDTDElementEvent;
    FOnDTDAttribute: TSCDTDAttributeEvent;
    FOnDTDEntity: TSCDTDEvent;
    FOnDTDNotation: TSCDTDNotationEvent;
    FErrorOptions: TSCSaxErrorOptions;
  protected
    procedure DoDTDComment(const Data: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDElement(const AName, Child: WideString;
      ElementContentType: TSCDTDElementContentType; Occurrence: TSCDTDElementOccurrence;
      ALine, ACol: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDAttribute(const Element, AName: WideString;
      AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
      const EnumValues, DefaultValue: WideString; Delimiter: WideChar;
      ALine, ACol: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDEntity(const AName, Data: WideString; Delimiter: WideChar;
      ALine, ACol: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDNotation(const AName, PublicID, SystemID: WideString; ALine, ACol: LongInt;
      var CanContinue: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAttributeType(const Attr: WideString): TSCDTDAttributeType;
    function GetAttributeDefault(const AttrDef: WideString): TSCDTDAttributeDefault;
    function GetElementContentType(const ElmType: WideString): TSCDTDElementContentType;
    function GetOccurrence(const C: WideChar): TSCDTDElementOccurrence;
    function GetElementOccurrence(const ElmName: WideString): TSCDTDElementOccurrence;

    function ParseDTD(const S: WideString; var ALine, ACol: LongInt;
      var ExpectedChar: WideChar; var ErrorStr: WideString;
      ExternalDTD: Boolean): TSCDTDError;

    property ErrorOptions: TSCSaxErrorOptions read FErrorOptions write FErrorOptions default [];
    property OnDTDComment: TSCCommentEvent read FOnDTDComment write FOnDTDComment;
    property OnDTDElement: TSCDTDElementEvent read FOnDTDElement write FOnDTDElement;
    property OnDTDAttribute: TSCDTDAttributeEvent read FOnDTDAttribute write FOnDTDAttribute;
    property OnDTDEntity: TSCDTDEvent read FOnDTDEntity write FOnDTDEntity;
    property OnDTDNotation: TSCDTDNotationEvent read FOnDTDNotation write FOnDTDNotation;
  end;

  TSCPathInfo = class(TObject)
  private
    FItems: TList;
    FDestroying: Boolean;
    FName: Widestring;
    FStartLine: Integer;
    FStartColumn: Integer;
    FEndLine: Integer;
    FEndColumn: Integer;
    FDonotNotify: Boolean;
    FOwner: TSCPathInfo;
    FCursor: TSCPathInfo;
    FTagClosed: Boolean;
    function GetItem(Index: Integer): TSCPathInfo;
    function GetCount: Integer;
    function AddItem: TSCPathInfo;
  protected
    procedure ClearItems;
    procedure ItemAdded(AItem: TSCPathInfo);
    procedure ItemDestroyed(AItem: TSCPathInfo);
    procedure GoUp;
  public
    constructor Create(AOwner: TSCPathInfo); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function ItemByName(AName: WideString): TSCPathInfo;
    function ItemAtPos(ALine, ACol: Integer): TSCPathInfo;
    function Level: Integer;

    property Items[Index: Integer]: TSCPathInfo read GetItem;
    property Count: Integer read GetCount;
    property Owner: TSCPathInfo read FOwner;
    property TagClosed: Boolean read FTagClosed;

    property Name: Widestring read FName;
    property StartLine: Integer read FStartLine;
    property StartColumn: Integer read FStartColumn;
    property EndLine: Integer read FEndLine;
    property EndColumn: Integer read FEndColumn;
  end;

  TSCXMLParseError = record
    Error: Integer;
    Line: Integer;
    Column: Integer;
  end;

  TSCCustomSaxParser = class(TComponent)
  private
    FProcessTime: DWord;
    FIgnoreWhiteSpaces: Boolean;
    FAttributeName: WideString;
    FAttributeList: Widestring;
    FPI: WideString;
    FRoot: WideString;
    FElement: WideString;
    FPrevToken: TSCTokenKind;
    FToken: TSCTokenKind;
    FPath: Widestring;
    FLinePos: LongInt;
    FColumnPos: LongInt;
    FLastTokenLine: LongInt;
    FLastTokenColumn: LongInt;
    FTokenCharCount: LongInt;
    FRaiseErrors: Boolean;
    FEscapeParser: TSCEscapeParser;
    FErrorChar: WideChar;
    FExpectedChar: WideChar;
    FPathInfo: TSCPathInfo;
    FTagLine: LongInt;
    FTagCol: LongInt;
    FVersion: WideString;
    FEncoding: WideString;
    FStandalone: Boolean;
    FLastError: Integer;
    FEncodingRequested: Boolean;
    FParseLevel: Integer;
    FIgnoreRoot: Boolean;
    FParseCount: Integer;
    FErrorLine: Integer;
    FErrorColumn: Integer;
    FErrorStr: WideString;
    FErrorOptions: TSCSaxErrorOptions;
    FDTDType: TSCDTDType;
    FOnToken: TSCTokenEvent;
    FOnError: TSCErrorEvent;
    FOnStartDocument: TSCDocumentEvent;
    FOnEndDocument: TSCDocumentEvent;
    FOnStartElement: TSCNodeEvent;
    FOnEndElement: TSCNodeEvent;
    FOnStartCData: TSCCDataEvent;
    FOnEndCData: TSCCDataEvent;
    FOnComment: TSCCommentEvent;
    FOnAttributeStart: TSCNodeEvent;
    FOnAttributeName: TSCAttributeEvent;
    FOnAttributeValueStart: TSCNodeEvent;
    FOnAttributeValue: TSCAttributeEvent;
    FOnText: TSCTextEvent;
    FOnProcessingInstruction: TSCProcessingInstructionEvent;
    FOnResolveEntity: TSCResolveEntityEvent;
    FOnDTDStart: TSCDTDStartEvent;
    FOnDTDFileStart: TSCDTDFileStartEvent;
    FOnDTDEnd: TSCDTDEndEvent;
    FOnDTDComment: TSCCommentEvent;
    FOnDTDElement: TSCDTDElementEvent;
    FOnDTDAttribute: TSCDTDAttributeEvent;
    FOnDTDEntity: TSCDTDEvent;
    FOnDTDNotation: TSCDTDNotationEvent;
    FStopOnDTDEnd: Boolean;
    FDTDParser: TSCSaxDTDParser;
    FValueStart: WideChar;
    procedure SetPath(const APath: Widestring);
    function  ChangeToken(EnteredToken: TSCTokenKind; const Value: WideString): Boolean;
    function  CheckForError(ReleasedToken, EnteredToken: TSCTokenKind;
      const Value, APath: WideString): Boolean;
  protected
    procedure SetVersion(const AVersion: Widestring); virtual;
    procedure SetEncoding(const AEncoding: Widestring); virtual;

    function ConvertXmlInstError(AError: TSCXmlInstructionError): Integer;
    function ParseXmlInstruction(const xmlData: WideString; var AEncoding, AVersion,
      AStandalone: WideString; var HasEncoding, HasVersion, HasStandalone: Boolean;
      var ErrorLine, ErrorCol: Integer): TSCXmlInstructionError;

    function  IsValidCharForToken(const Chr: WideChar; InToken: TSCTokenKind;
      Position: LongInt): Boolean;
    procedure DoToken(ReleasedToken, EnteredToken: TSCTokenKind;
      const ReleasedValue: WideString; var CanContinue: Boolean); virtual;
    function  DoError(ErrorCode: Integer; ReleasedToken, EnteredToken: TSCTokenKind;
      const Value: WideString; ErrLine, ErrColumn: LongInt): Boolean;
    procedure DoPathInfo(const APath, CurTag: WideString; IsNew, TagClosedInTag: Boolean);

    function  IsWhiteSpace(const AChar: WideChar): boolean;
    function  ParseDTDName(const DTD: WideString; var Name, PublicID,
      SystemID: WideString; var ALine, ACol: LongInt): TSCDTDError;

    procedure DoInvalidNameError(const AName: WideString);
    procedure DoInvalidReservedNameError(const AName: WideString);
    procedure DoInvalidCharError(const AChar: WideChar);
    procedure DoEmptyDocumentError;
    procedure DoSwitchEncodingError(const AEncoding: WideString);
    procedure DoSwitchVersionError(const AVersion: WideString);
    procedure DoUnsupportedEncodingError(const AEncoding: WideString);
    procedure DoNonUnicodeDocumentError;

    procedure DoDocumentStart(var CanContinue: Boolean); dynamic;
    procedure DoDocumentEnd(var CanContinue: Boolean); dynamic;
    procedure DoStartElement(const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoEndElement(const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoStartCData(const CData: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoEndCData(const CData: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoComment(const Chars: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoAttributeStart(const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoAttributeName(const AName, Value: WideString; Delimiter: WideChar; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoAttributeValueStart(const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoAttributeValue(const AName, Value: WideString; Delimiter: WideChar; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoResolveEntity(const Entity : WideString; var CanContinue: Boolean); virtual;
    procedure DoText(const Data: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;
    function  DoProcessingInstruction(const Target, Data: WideString; Line, Column: LongInt;
      var CanContinue: Boolean): Boolean; virtual;

    procedure DoEscapeError(E: TSCEscapeError; const Str: WideString); virtual;
    procedure DoDTDError(E: TSCDTDError; const Str: WideString); virtual;

    procedure DoDTDStart(const AName, PublicID, SystemID: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoDTDFileStart(const AName, PublicID, SystemID: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoDTDEnd(const AName: WideString; Line, Column: LongInt; var CanContinue: Boolean); virtual;

    procedure DoDTDElement(Sender: TObject; const AName, Child: WideString;
      ElementContentType: TSCDTDElementContentType; Occurrence: TSCDTDElementOccurrence;
      Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDAttribute(Sender: TObject; const Element, AName: WideString;
      AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
      const EnumValues, DefaultValue: WideString; Delimiter: WideChar; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;
    procedure DoDTDEntity(Sender: TObject; const AName, Data: WideString; Delimiter: WideChar;
      Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDNotation(Sender: TObject; const AName, PublicID, SystemID: WideString;
      Line, Column: LongInt; var CanContinue: Boolean); virtual;
    procedure DoDTDComment(Sender: TObject; const Data: WideString; Line, Column: LongInt;
      var CanContinue: Boolean); virtual;

    property OnToken: TSCTokenEvent read FOnToken write FOnToken;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  GetLastError: TSCXMLParseError;
    function  ErrorDescription(No: Integer): String;

    function  IsParsing: Boolean;
    function  ParseFile(AFileName: String): Boolean;
    function  ParseStream(AStream: TStream): Boolean;
    function  ParseString(const S: WideString): Boolean; dynamic;
    function  GetPathInfos(const S: WideString): TSCPathInfo;
    procedure DetectEncoding(const S: WideString);
    function  ParseForDTD(const S: WideString): Boolean;

    property Root: WideString read FRoot;
    property Element: WideString read FElement;
    property Path: Widestring read FPath;
    property Line: LongInt read FLinePos;
    property Column: LongInt read FColumnPos;
    property LastTokenLine: LongInt read FLastTokenLine;
    property LastTokenColumn: LongInt read FLastTokenColumn;
    property IgnoreRoot: Boolean read FIgnoreRoot write FIgnoreRoot;
    property IgnoreWhiteSpaces: Boolean read FIgnoreWhiteSpaces write FIgnoreWhiteSpaces;
    property RaiseErrors: Boolean read FRaiseErrors write FRaiseErrors;
    property Version: WideString read FVersion write SetVersion;
    property Encoding: WideString read FEncoding write SetEncoding;
    property Standalone: Boolean read FStandalone write FStandalone default False;
    property ErrorOptions: TSCSaxErrorOptions read FErrorOptions write FErrorOptions default [];
    property ParseLevel: Integer read FParseLevel;
    property ProcessTime: DWord read FProcessTime;
    property ExpectedChar: WideChar read FExpectedChar;
    property DTDType: TSCDTDType read FDTDType;
    property ValueStart: WideChar read FValueStart;

    property OnError: TSCErrorEvent read FOnError write FOnError;
    property OnStartDocument: TSCDocumentEvent read FOnStartDocument write FOnStartDocument;
    property OnStartElement: TSCNodeEvent read FOnStartElement write FOnStartElement;
    property OnEndElement: TSCNodeEvent read FOnEndElement write FOnEndElement;
    property OnEndDocument: TSCDocumentEvent read FOnEndDocument write FOnEndDocument;
    property OnStartCData: TSCCDataEvent read FOnStartCData write FOnStartCData;
    property OnEndCData: TSCCDataEvent read FOnEndCData write FOnEndCData;
    property OnComment: TSCCommentEvent read FOnComment write FOnComment;
    property OnAttributeStart: TSCNodeEvent read FOnAttributeStart write FOnAttributeStart;
    property OnAttributeName: TSCAttributeEvent read FOnAttributeName write FOnAttributeName;
    property OnAttributeValueStart: TSCNodeEvent read FOnAttributeValueStart write FOnAttributeValueStart;
    property OnAttributeValue: TSCAttributeEvent read FOnAttributeValue write FOnAttributeValue;
    property OnResolveEntity: TSCResolveEntityEvent read FOnResolveEntity write FOnResolveEntity;
    property OnText: TSCTextEvent read FOnText write FOnText;
    property OnProcessingInstruction: TSCProcessingInstructionEvent read FOnProcessingInstruction write
      FOnProcessingInstruction;

    property OnDTDStart: TSCDTDStartEvent read FOnDTDStart write FOnDTDStart;
    property OnDTDFileStart: TSCDTDFileStartEvent read FOnDTDFileStart write FOnDTDFileStart;
    property OnDTDEnd: TSCDTDEndEvent read FOnDTDEnd write FOnDTDEnd;
    property OnDTDElement: TSCDTDElementEvent read FOnDTDElement write FOnDTDElement;
    property OnDTDAttribute: TSCDTDAttributeEvent read FOnDTDAttribute write FOnDTDAttribute;
    property OnDTDEntity: TSCDTDEvent read FOnDTDEntity write FOnDTDEntity;
    property OnDTDNotation: TSCDTDNotationEvent read FOnDTDNotation write FOnDTDNotation;
    property OnDTDComment: TSCCommentEvent read FOnDTDComment write FOnDTDComment;
  end;

  TSCSaxParser = class(TSCCustomSaxParser)
  published
    property ErrorOptions;
    property IgnoreWhiteSpaces;
    property RaiseErrors;
    property OnError;
    property OnStartDocument;
    property OnEndDocument;
    property OnStartElement;
    property OnEndElement;
    property OnAttributeStart;
    property OnAttributeName;
    property OnAttributeValueStart;
    property OnAttributeValue;
    property OnResolveEntity;
    property OnText;
    property OnProcessingInstruction;
    property OnComment;
    property OnStartCData;
    property OnEndCData;

    property OnDTDStart;
    property OnDTDFileStart;
    property OnDTDEnd;
    property OnDTDElement;
    property OnDTDAttribute;
    property OnDTDEntity;
    property OnDTDNotation;
    property OnDTDComment;
  end;

  TSCDomItem = class(TObject)
  private
    FName: WideString;
    FErrorLinePos: LongInt;
    FErrorColumnPos: LongInt;
    FErrorChar: WideChar;
    FToken: TSCTokenKind;
    FLine: LongInt;
    FColumn: LongInt;
    FOwner: TSCDomItem;
  protected
    procedure ItemDestroyed(AItem: TSCDomItem); virtual;
    function  ExtractItem(AItem: TSCDomItem): Boolean; dynamic;

    procedure CheckIfNameIsValid(const AName: WideString; AllowEmpty: Boolean = True); virtual;
    procedure ResetName(const Newvalue: WideString); virtual;
    procedure SetToken(Value: TSCTokenKind);

    procedure ValidateName(const AName: WideString); virtual;
    procedure SetName(const Newvalue: WideString); virtual;
    function  GetLevel: Integer; virtual;
    procedure DoError(ErrorCode: Integer; TokenKind: TSCTokenKind;
      const Value: WideString);

    function  GetDocument: TSCDomDocument; virtual;
  public
    constructor Create(AOwner: TSCDomItem); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure NormalizeTab(TabSpace: Integer); dynamic;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); dynamic;

    procedure Clear; dynamic;

    function ToString(Indent: Word): WideString; dynamic;

    property Name: WideString read FName write SetName;
    property Owner: TSCDomItem read FOwner;
    property Level: Integer read GetLevel;
    property Document: TSCDomDocument read GetDocument;
    property Line: LongInt read FLine;
    property Column: LongInt read FColumn;
  end;

  TSCDomAttribute = class(TSCDomItem)
  private
    FValue: WideString;
    FDelimiter: WideChar;
    FEscapeParser: TSCEscapeParser;
    procedure SetValue(const Newvalue: WideString);
    procedure SetDelimiter(const Newvalue: WideChar);
  protected
    procedure ValidateValue(const AValue: Widestring); virtual;
    function  GetLevel: Integer; override;
    property EscapeParser: TSCEscapeParser read FEscapeParser;
  public
    constructor Create(AOwner: TSCDomItem); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    procedure Clear; override;

    property Value: WideString read FValue write SetValue;
    property Delimiter: WideChar read FDelimiter write SetDelimiter;
  end;

  TSCDomTreeNode = class(TSCDomItem)
  protected
    function CanDoLastCRLF: Boolean; virtual;
  public
    destructor Destroy; override;

    procedure NormalizeData; dynamic;
    procedure PrettyTrimData; dynamic;
  end;

  TSCDomTextBag = class(TSCDomTreeNode)
  private
    FText: WideString;
    FInsertSpace: Boolean;
    FCanNormalizeData: Boolean;
  protected
    procedure ValidateData(const AValue: Widestring); virtual;
    procedure ValidateName(const AName: WideString); override;
    procedure SetName(const Newvalue: WideString); override;
    procedure SetText(const Newvalue: WideString); virtual;

    function  GetStoredData: WideString; dynamic;
    function  GetTagStarter: WideString; dynamic;
    function  GetTagStoper: WideString; dynamic;

    property  CanNormalizeData: Boolean read FCanNormalizeData write FCanNormalizeData;
    property  InsertSpace: Boolean read FInsertSpace write FInsertSpace;
  public
    constructor Create(AOwner: TSCDomItem); override;
    procedure AfterConstruction; override;
    procedure NormalizeTab(TabSpace: Integer); override;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    procedure Clear; override;

    property Text: WideString read FText write SetText;
  end;

  TSCDomCData = class(TSCDomTextBag)
  protected
    procedure ValidateData(const AValue: Widestring); override;
    function  GetTagStarter: WideString; override;
    function  GetTagStoper: WideString; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
  end;

  TSCDomComment = class(TSCDomTextBag)
  protected
    procedure ValidateData(const AValue: Widestring); override;
    function  GetTagStarter: WideString; override;
    function  GetTagStoper: WideString; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
  end;

  TSCDomDeclaration = class(TSCDomTextBag)
  protected
    procedure ValidateName(const AName: WideString); override;
    procedure SetName(const Newvalue: WideString); override;

    procedure ValidateData(const AValue: Widestring); override;
    function  GetTagStarter: WideString; override;
    function  GetTagStoper: WideString; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
  end;

  TSCDomProcessInst = class(TSCDomTextBag)
  protected
    procedure ValidateData(const AValue: Widestring); override;
    procedure ValidateName(const AName: WideString); override;
    procedure SetName(const Newvalue: WideString); override;
    function  GetTagStarter: WideString; override;
    function  GetTagStoper: WideString; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
  end;

  TSCDomText = class(TSCDomTextBag)
  protected
    procedure ValidateData(const AValue: Widestring); override;
    function  GetTagStarter: WideString; override;
    function  GetTagStoper: WideString; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
  end;

  TSCDomElement = class(TSCDomTreeNode)
  private
    FNodes: TList;
    FAttributes: TList;
    function GetAttributes(Index: Integer): TSCDomAttribute;
    function GetNode(Index: Integer): TSCDomTreeNode;
  protected
    function  ExtractItem(AItem: TSCDomItem): Boolean; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;
    procedure Clear; override;

    procedure PrettyTrimData; override;
    procedure NormalizeData; override;
    procedure NormalizeTab(TabSpace: Integer); override;

    function  AttributeCount: Integer; virtual;
    function  SetAttribute(const AName, AValue: WideString): TSCDomAttribute; overload; virtual;
    function  GetAttribute(const AName: WideString): WideString; virtual;
    function  AddAttribute(const AName, AValue: WideString): TSCDomAttribute; overload; virtual;
    function  AddAttribute(const AName, AValue: WideString; Delimiter: WideChar): TSCDomAttribute; overload; virtual;
    function  AddAttribute(AAttribute: TSCDomAttribute): Boolean; overload; virtual;
    function  DeleteAttribute(Index: Integer): Boolean; virtual;
    function  ExtractAttribute(AAttribute: TSCDomAttribute): TSCDomAttribute; virtual;
    function  RemoveAttribute(AAttribute: TSCDomAttribute): Boolean; virtual;
    function  InsertAttribute(AAttribute: TSCDomAttribute; Index: Integer): Boolean; virtual;
    procedure ClearAttributes; virtual;
    function  IndexOfAttribute(AAttr: TSCDomAttribute): Integer; virtual;
    function  AttributeByName(const AName: WideString): TSCDomAttribute; virtual;

    function  ChildNodeCount: Integer; virtual;
    procedure ClearChildNodes; virtual;
    function  AddNode(ANode: TSCDomTreeNode): Boolean; virtual;
    function  DeleteNode(Index: Integer): Boolean; virtual;
    function  ExtractNode(ANode: TSCDomTreeNode): TSCDomItem; virtual;
    function  RemoveNode(ANode: TSCDomTreeNode): Boolean; virtual;
    function  InsertNode(ANode: TSCDomTreeNode; Index: Integer): Boolean; virtual;
    function  IndexOfNode(ANode: TSCDomTreeNode): Integer; virtual;

    function  AddElement(const AName: WideString): TSCDomElement; overload; virtual;
    function  AddElement(AElement: TSCDomElement): Boolean; overload; virtual;
    procedure ClearElements; virtual;
    function  ElementByName(const AName: WideString): TSCDomElement; virtual;

    function  AddComment(const Data: WideString): TSCDomComment; overload; virtual;
    function  AddComment(AComment: TSCDomComment): Boolean; overload; virtual;
    procedure ClearComments; virtual;

    function  AddCData(const Data: WideString): TSCDomCData; overload; virtual;
    function  AddCData(ACData: TSCDomCData): Boolean; overload; virtual;
    procedure ClearCDatas; virtual;

    function  AddText(const Data: WideString): TSCDomText; overload; virtual;
    function  AddText(AText: TSCDomText): Boolean; overload; virtual;
    procedure ClearTexts; virtual;

    function  AddPI(const AName, Data: WideString): TSCDomProcessInst; overload; virtual;
    function  AddPI(API: TSCDomProcessInst): Boolean; overload; virtual;
    procedure ClearPIs; virtual;

    property Attributes[Index: Integer]: TSCDomAttribute read GetAttributes;
    property ChildNodes[Index: Integer]: TSCDomTreeNode read GetNode;
  end;

  TSCDomDTDAttribute = class(TSCDomAttribute)
  private
    FEnumValues: WideString;
    FDefaultValue: WideString;
    FAttributeType: TSCDTDAttributeType;
    FAttributeDefault: TSCDTDAttributeDefault;
    procedure SetDefaultValue(const Newvalue: WideString);
    procedure SetEnumValues(const Newvalue: WideString);
  public
    constructor Create(AOwner: TSCDomItem); override;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    property DefaultValue: WideString read FDefaultValue write SetDefaultValue;
    property EnumValues: WideString read FEnumValues write SetEnumValues;
    property AttributeType: TSCDTDAttributeType read FAttributeType write FAttributeType default dtdCDATA;
    property AttributeDefault: TSCDTDAttributeDefault read FAttributeDefault write FAttributeDefault default dtdAttrUndefined;
  end;

  TSCDomDTDComment = class(TSCDomComment)
  protected
    function GetTagStarter: WideString; override;
  end;

  TSCDomDTDElement = class(TSCDomItem)
  private
    FAttrList: TList;
    FStoredData: WideString;
    FChildrenNode: TSCElementContentNode;
    FContentType: TSCDTDElementContentType;
    FOccurrence: TSCDTDElementOccurrence;
    FCreatedByAttr: Boolean;
    function  GetAttribute(Index: Integer): TSCDomDTDAttribute;
    procedure SetStoredData(const NewValue: WideString);
  protected
    function  ExtractItem(AItem: TSCDomItem): Boolean; override;
  public
    constructor Create(AOwner: TSCDomItem); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    function  AttributeCount: Integer;
    function  AddAttribute(const AName, Enums, DefaultValue: WideString; AttrType: TSCDTDAttributeType;
      AttrDef: TSCDTDAttributeDefault = dtdAttrUndefined): TSCDomDTDAttribute; overload;
    function  AddAttribute(AAttribute: TSCDomDTDAttribute): Boolean; overload;
    function  DeleteAttribute(Index: Integer): Boolean;
    function  ExtractAttribute(AAttribute: TSCDomDTDAttribute): TSCDomDTDAttribute;
    function  RemoveAttribute(AAttribute: TSCDomDTDAttribute): Boolean;
    function  InsertAttribute(AAttribute: TSCDomDTDAttribute; Index: Integer): Boolean;
    function  IndexOfAttribute(AAttr: TSCDomDTDAttribute): Integer;
    function  AttributeByName(const AName: WideString): TSCDomDTDAttribute;

    property Attributes[Index: Integer]: TSCDomDTDAttribute read GetAttribute;
    property StoredData: WideString read FStoredData write SetStoredData;
    property ChildrenNode: TSCElementContentNode read FChildrenNode;
    property ContentType: TSCDTDElementContentType read FContentType write FContentType default dtdMixed;
    property Occurrence: TSCDTDElementOccurrence read FOccurrence write FOccurrence default dtdElementOnlyOne;
  end;

  TSCDomDTDEntity = class(TSCDomAttribute)
  public
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;
  end;

  TSCDomDTDNotation = class(TSCDomAttribute)
  private
    FPublicId: WideString;
    FSystemId: WideString;
    procedure SetPublicId(const Newvalue: WideString);
    procedure SetSystemId(const Newvalue: WideString);
  public
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    property PublicId: WideString read FPublicId write SetPublicId;
    property SystemId: WideString read FSystemId write SetSystemId;
  end;

  TSCDomDTD = class(TSCDomTreeNode)
  private
    FItems: TList;
    FPublicId: WideString;
    FSystemId: WideString;
    FDocumentElement: WideString;
    FInternalValidation: Boolean;
    procedure SetDocumentElement(const Newvalue: WideString);
    procedure SetPublicId(const Newvalue: WideString);
    procedure SetSystemId(const Newvalue: WideString);

    function  GetItem(Index: Integer): TSCDomItem;
  protected
    procedure ItemDestroyed(AItem: TSCDomItem); override;
    function  ExtractItem(AItem: TSCDomItem): Boolean; override;
    procedure DoError(ErrorCode, Line, Column: LongInt;
      const Value: WideString); dynamic;
  public
    constructor Create(AOwner: TSCDomItem); override;
    destructor Destroy; override;
    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;

    procedure ValidateDTD;

    function  Count: Integer;
    function  IndexOf(AItem: TSCDomItem): Integer;
    function  Delete(Index: Integer): Boolean;
    function  Remove(AItem: TSCDomItem): Boolean;
    procedure Clear; override;

    function  AddComment(const Data: WideString): TSCDomDTDComment; overload;
    function  AddElement(const AName: WideString; var IsNew: Boolean;
      AContentType: TSCDTDElementContentType = dtdMixed): TSCDomDTDElement; overload;
    function  ElementByName(const AName: WideString): TSCDomDTDElement;
    function  AddEntity(const AName, AValue: WideString; var IsNew: Boolean): TSCDomDTDEntity; overload;
    function  EntityByName(const AName: WideString): TSCDomDTDEntity;
    function  AddNotation(const AName, APublicId, ASystemId: WideString;
      var IsNew: Boolean): TSCDomDTDNotation; overload;
    function  NotationByName(const AName: WideString): TSCDomDTDNotation;

    property DocumentElement: WideString read FDocumentElement write SetDocumentElement;
    property PublicId: WideString read FPublicId write SetPublicId;
    property SystemId: WideString read FSystemId write SetSystemId;

    property Item[Index: Integer]: TSCDomItem read GetItem;
  end;

  TSCDomDocument = class(TSCDomElement)
  private
    FIndent: Word;
    FDTD: TSCDomDTD;
    FFileName: String;
    FVersion: WideString;
    FEncoding: WideString;
    FStandalone: Boolean;
    FRaiseError: Boolean;
    FInternalValidation: Boolean;
    function  GetRoot: TSCDomElement;
    procedure SetIndent(Newvalue: Word);
  protected
    procedure ValidateName(const AName: WideString); override;

    procedure SetName(const Newvalue: WideString); override;
    procedure SetVersion(const AVersion: Widestring); virtual;
    procedure SetEncoding(const AEncoding: Widestring); virtual;

    procedure DoInternalError(Sender: TObject; ErrorCode: Integer; const ErrorValue: WideString);
  public
    constructor Create(AOwner: TSCDomItem); override;
    destructor Destroy; override;

    function  AddText(const Data: WideString): TSCDomText; overload; override;
    function  AddText(AText: TSCDomText): Boolean; overload; override;
    procedure ValidateDocument;

    procedure SaveToStream(AStream: TStream; Indent: Integer; Normalized,
      PrettyTrim: Boolean); override;
    procedure SaveToFile(AFile: String); dynamic;
    procedure Save;

    property FileName: String read FFileName write FFileName;
    property DTD: TSCDomDTD read FDTD;
    property Indent: Word read FIndent write SetIndent;
    property Version: WideString read FVersion write SetVersion;
    property Encoding: WideString read FEncoding write SetEncoding;
    property Standalone: Boolean read FStandalone write FStandalone default False;
    property RaiseError: Boolean read FRaiseError write FRaiseError default True;
    property Root: TSCDomElement read GetRoot;
  end;

  TSCDomParser = class(TComponent)
  private
    FParser: TSCCustomSaxParser;
    FErrorOccured: Boolean;
    FContentParser: TSCSaxContentParser;
    FDomDocument: TSCDomDocument;
    FPathList: TList;
    FNormalizeData: Boolean;
    FIgnoreWhiteSpaces: Boolean;
    FLastErrorNo: Integer;
    FErrorLine: LongInt;
    FErrorCol: LongInt;
    FErrorPath: WideString;
    FErrorValue: WideString;
    FRaiseErrors: Boolean;
    FOnError: TSCErrorEvent;
    FParsing: Boolean;
    FStartLine: Integer;
    FStartColumn: Integer;
    FOnStartDocument: TSCDocumentEvent;
    FOnEndDocument: TSCDocumentEvent;
    FOnStartElement: TSCNodeEvent;
    FOnEndElement: TSCNodeEvent;
    FOnStartCData: TSCCDataEvent;
    FOnEndCData: TSCCDataEvent;
    FOnComment: TSCCommentEvent;
    FOnAttributeName: TSCAttributeEvent;
    FOnAttributeValue: TSCAttributeEvent;
    FOnText: TSCTextEvent;
    FOnProcessingInstruction: TSCProcessingInstructionEvent;
    FOnDTDComment: TSCCommentEvent;
    FOnDTDStart: TSCDTDStartEvent;
    FOnDTDFileStart: TSCDTDFileStartEvent;
    FOnDTDEnd: TSCDTDEndEvent;
    FOnDTDElement: TSCDTDElementEvent;
    FOnDTDAttribute: TSCDTDAttributeEvent;
    FOnDTDEntity: TSCDTDEvent;
    FOnDTDNotation: TSCDTDNotationEvent;
    FOnResolveEntity: TSCResolveEntityEvent;
    function  PathCount: Integer;
    function  GetCurrentElement: TSCDomElement;
    procedure AddToPath(AElement: TSCDomElement);
    procedure GotoUpLevel;
    procedure SetRaiseErrors(Value: Boolean);
    procedure SetIgnoreWhiteSpaces(Value: Boolean);
    function  GetLastTokenLine: LongInt;
    function  GetLastTokenColumn: LongInt;
  protected
    procedure DocumentStart(var CanContinue: Boolean);
    procedure DocumentEnd(var CanContinue: Boolean);
    procedure StartElement(Sender: TObject; const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure EndElement(Sender: TObject; const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure StartCData(Sender: TObject; const CData: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure EndCData(Sender: TObject; const CData: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure Comment(Sender: TObject; const Chars: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure AttributeName(Sender: TObject; const AName, Value: WideString; Delimiter: WideChar;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure AttributeValue(Sender: TObject; const AName, Value: WideString; Delimiter: WideChar;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure Text(Sender: TObject; const Chars: Widestring; Line, Column: LongInt;
      var CanContinue: Boolean);
    procedure ProcessingInstruction(Sender: TObject; const Target, Data: WideString;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure Error(Sender: TObject; ErrorCode, Line, Column: LongInt;
      ReleasedToken, EnteredToken: TSCTokenKind; const Value, Path: WideString);

    procedure DomDTDStart(Sender: TObject; const AName, PublicID, SystemID: WideString;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDFileStart(Sender: TObject; const AName, PublicID, SystemID: WideString;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDEnd(Sender: TObject; const AName: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);

    procedure DomDTDElement(Sender: TObject; const AName, Child: WideString;
      ElementContentType: TSCDTDElementContentType; Occurrence: TSCDTDElementOccurrence;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDAttribute(Sender: TObject; const Element, AName: WideString;
      AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
      const EnumValues, DefaultValue: WideString; Delimiter: WideChar;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDEntity(Sender: TObject; const AName, Data: WideString; Delimiter: WideChar;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDNotation(Sender: TObject; const AName, PublicID, SystemID: WideString;
      Line, Column: LongInt; var CanContinue: Boolean);
    procedure DomDTDComment(Sender: TObject; const Data: WideString; Line, Column: LongInt;
      var CanContinue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ErrorDescription(No: Integer): String;
    function ParseFile(AFileName: String; Validate: Boolean = True): TSCDomDocument;
    function ParseStream(AStream: TStream; Validate: Boolean = True): TSCDomDocument;
    function ParseString(const S: WideString; Validate: Boolean = True): TSCDomDocument;

    property Parsing: Boolean read FParsing;
    property LastErrorNo: Integer read FLastErrorNo;
    property ErrorLine: LongInt read FErrorLine;
    property ErrorCol: LongInt read FErrorCol;
    property ErrorPath: WideString read FErrorPath;
    property ErrorValue: WideString read FErrorValue;
    property LastTokenLine: LongInt read GetLastTokenLine;
    property LastTokenColumn: LongInt read GetLastTokenColumn;
  published
    property IgnoreWhiteSpaces: Boolean read FIgnoreWhiteSpaces write SetIgnoreWhiteSpaces;
    property NormalizeData: Boolean read FNormalizeData write FNormalizeData;
    property RaiseErrors: Boolean read FRaiseErrors write SetRaiseErrors;

    property OnError: TSCErrorEvent read FOnError write FOnError;
    property OnStartDocument: TSCDocumentEvent read FOnStartDocument write FOnStartDocument;
    property OnStartElement: TSCNodeEvent read FOnStartElement write FOnStartElement;
    property OnEndElement: TSCNodeEvent read FOnEndElement write FOnEndElement;
    property OnEndDocument: TSCDocumentEvent read FOnEndDocument write FOnEndDocument;
    property OnStartCData: TSCCDataEvent read FOnStartCData write FOnStartCData;
    property OnEndCData: TSCCDataEvent read FOnEndCData write FOnEndCData;
    property OnComment: TSCCommentEvent read FOnComment write FOnComment;
    property OnAttributeName: TSCAttributeEvent read FOnAttributeName write FOnAttributeName;
    property OnAttributeValue: TSCAttributeEvent read FOnAttributeValue write FOnAttributeValue;
    property OnResolveEntity: TSCResolveEntityEvent read FOnResolveEntity write FOnResolveEntity;
    property OnText: TSCTextEvent read FOnText write FOnText;
    property OnProcessingInstruction: TSCProcessingInstructionEvent read FOnProcessingInstruction
      write FOnProcessingInstruction;

    property OnDTDStart: TSCDTDStartEvent read FOnDTDStart write FOnDTDStart;
    property OnDTDFileStart: TSCDTDFileStartEvent read FOnDTDFileStart write FOnDTDFileStart;
    property OnDTDEnd: TSCDTDEndEvent read FOnDTDEnd write FOnDTDEnd;
    property OnDTDElement: TSCDTDElementEvent read FOnDTDElement write FOnDTDElement;
    property OnDTDAttribute: TSCDTDAttributeEvent read FOnDTDAttribute write FOnDTDAttribute;
    property OnDTDEntity: TSCDTDEvent read FOnDTDEntity write FOnDTDEntity;
    property OnDTDNotation: TSCDTDNotationEvent read FOnDTDNotation write FOnDTDNotation;
    property OnDTDComment: TSCCommentEvent read FOnDTDComment write FOnDTDComment;
  end;


function SCErrorDescription(ErrorNo: Integer; ErrorToken: TSCTokenKind;
  ErrorLine, ErrorColumn: LongInt; ErrorChar: WideChar; const ErrorValue: WideString): String;
function SCTokenAsString(AToken: TSCTokenKind): String;

function IndexOfEncoding(const AEncoding: WideString): Integer;
function IsValidEncoding(const AEncoding: WideString): Boolean;
function CanSwitchToEncoding(const FromEncoding, ToEncoding: WideString): Boolean;
function EncodingDescription(const AEncoding: WideString): WideString;

function IsValidVersionNumber(const AVersion: WideString): Boolean;
function IsValidElementChar(const C: WideChar; P: Integer): Boolean;
function IsValidElementName(const AName: WideString): Boolean;
function IsValidInstructionName(const AName: WideString): Boolean;

function SC_CleanInvalidChars(const Str: WideString): WideString;
function SC_CleanXMLChars(const Str: WideString): WideString;


type
  TSCEncodingMap = record
    WinCharset: Integer;
    WinCodePage: WideString;
    Encoding: WideString;
    Description: WideString;
    CanSwitch: Boolean;
  end;

const
  SCElementNameError            = -900;
  SCElementNameStartError       = -901;
  SCInvalidElementNameError     = -902;
  SCInvalidCharForToken         = -903;
  SCBlankElementNameError       = -904;

  SCInternalError               = -1000;
  SCErrorSameTokens             = -1001;
  SCErrorAttrValueNotFound      = -1002;
  SCErrorTagOpenedInTag         = -1003;
  SCErrorMissingWhiteSpace      = -1004;
  SCErrorBadTokenSequence       = -1005;
  SCErrorFileNotFound           = -1006;
  SCErrorElementMismatch        = -1007;
  SCErrorTagCloseExpected       = -1008;
  SCErrorWhiteSpaceNotAllowed   = -1009;
  SCErrorWrongElementStart      = -1010;
  SCErrorOneRootAllowed         = -1011;
  SCErrorTagsNotClosed          = -1012;
  SCErrorInvalidDeclaration     = -1013;
  SCErrorCommentHasTermination  = -1014;
  SCErrorTextAtDocumentTop  = -1015;
  SCErrorElementNameHasSpace    = -1016;
  SCErrorEmptyDocument          = -1017;
  SCErrorNoRootDefined          = -1018;
  SCErrorNonUnicodeDocument     = -1019;
  SCErrorBlankAttrName          = -1020;
  SCErrorDuplicateAttr          = -1021;
  SCErrorNeedEscapeClose        = -1022;
  SCErrorUndefinedEscape        = -1023;
  SCInvalidCharacter            = -1024;
  SCReservedNameMustBeLowerCase = -1025;
  SCCannotSwitchEncoding        = -1026;
  SCCannotSwitchVersion         = -1027;
  SCUnsupportedEncoding         = -1028;
  SCInvalidXmlDecSyntax         = -1029;
  SCUnexpectedAttribute         = -1030;
  SCInvalidVersionNumber        = -1031;
  SCCDATANotClosed              = -1032;
  SCCommentNotClosed            = -1033;
  SCInvalidUnicodeCharacter     = -1034;
  SCInvalidStandaloneValue      = -1035;
  SCInvalidXmlInstReplacement   = -1036;
  SCErrorDuplicateElement       = -1037;
  SCErrorDuplicateEntity        = -1038;
  SCErrorDuplicateNotation      = -1039;
  SCInvalidExpectedCharacter    = -1040;
  SCMissingWhiteSpace           = -1041;
  SCInvalidAttributeValue       = -1042;
  SCExpectedCharacterNotFound   = -1043;
  SCMultipleDOCTYPEDeclaration  = -1044;
  SCOutSideDOCTYPEDeclaration   = -1045;

  SCInvalidDTDName              = -1500;
  SCInvalidDTDLocation          = -1501;
  SCInvalidDTDPublicValue       = -1502;
  SCInvalidDTDSystemValue       = -1503;
  SCInvalidDTDParameter         = -1504;
  SCInvalidDTDElementName       = -1505;
  SCInvalidDTDAttributeValue    = -1506;
  SCInvalidDTDAttributeDefault  = -1507;
  SCInvalidDTDEntityValue       = -1508;
  SCInvalidDTDCharacter         = -1509;
  SCInvalidDTDDefination        = -1510;
  SCInvalidDTDXMLElement        = -1511;
  SCInvalidDTDDeclarationName   = -1512;
  SCInvalidDTDContentModel      = -1513;
  SCInvalidExternalID           = -1514;
  SCInvalidAttributeType        = -1515;
  SCInvalidAttributeDefault     = -1516;
  SCDTDElementNameExpected      = -1517;
  SCDuplicateDTDElement         = -1518;
  SCUndefinedDTDElement         = -1519;

  SCXMLErrors: array[0..66] of TSCXMLError = (
    (No: SCBlankElementNameError; Desc: SCRes_ErrorBlankElementName),
    (No: SCInternalError; Desc: SCRes_ErrorInternal),
    (No: SCErrorSameTokens; Desc: SCRes_ErrorSameTokens),
    (No: SCErrorAttrValueNotFound; Desc: SCRes_ErrorAttrValueNotFound),
    (No: SCErrorTagOpenedInTag; Desc: SCRes_ErrorTagOpenedInTag),
    (No: SCErrorMissingWhiteSpace; Desc: SCRes_ErrorMissingWhiteSpace),
    (No: SCErrorBadTokenSequence; Desc: SCRes_ErrorBadTokenSequence),
    (No: SCErrorFileNotFound; Desc: SCRes_ErrorFilenotfound),
    (No: SCErrorElementMismatch; Desc: SCRes_ErrorElementMismatch),
    (No: SCErrorTagCloseExpected; Desc: SCRes_ErrorTagCloseExpected),
    (No: SCErrorWhiteSpaceNotAllowed; Desc: SCRes_ErrorWhiteSpaceNotAllowed),
    (No: SCErrorWrongElementStart; Desc: SCRes_ErrorWrongElementStart),
    (No: SCErrorOneRootAllowed; Desc: SCRes_ErrorOneRootAllowed),
    (No: SCErrorTagsNotClosed; Desc: SCRes_ErrorTagsNotClosed),
    (No: SCErrorInvalidDeclaration; Desc: SCRes_ErrorInvalidDeclaration),
    (No: SCErrorCommentHasTermination; Desc: SCRes_ErrorDataHasCommentTermination),
    (No: SCErrorTextAtDocumentTop; Desc: SCRes_ErrorTextAtDocumentTop),
    (No: SCErrorElementNameHasSpace; Desc: SCRes_ErrorElementNameHasSpace),
    (No: SCErrorEmptyDocument; Desc: SCRes_ErrorEmptyDocument),
    (No: SCErrorNoRootDefined; Desc: SCRes_ErrorNoRootDefined),
    (No: SCErrorNonUnicodeDocument; Desc: SCRes_ErrorNonUnicodeDocument),
    (No: SCErrorBlankAttrName; Desc: SCRes_ErrorBlankAttrName),
    (No: SCErrorDuplicateAttr; Desc: SCRes_ErrorDuplicateAttr),
    (No: SCErrorDuplicateElement; Desc: SCRes_ErrorDuplicateElement),
    (No: SCErrorDuplicateEntity; Desc: SCRes_ErrorDuplicateEntity),
    (No: SCErrorDuplicateNotation; Desc: SCRes_ErrorDuplicateNotation),
    (No: SCErrorNeedEscapeClose; Desc: SCRes_NeedEscapeClose),
    (No: SCErrorUndefinedEscape; Desc: SCRes_UndefinedEscape),
    (No: SCInvalidCharacter; Desc: SCRes_InvalidCharacter),
    (No: SCReservedNameMustBeLowerCase; Desc: SCRes_ReservedNameMustBeLowerCase),
    (No: SCCannotSwitchEncoding; Desc: SCRes_CannotSwitchEncoding),
    (No: SCCannotSwitchVersion; Desc: SCRes_CannotSwitchVersion),
    (No: SCUnsupportedEncoding; Desc: SCRes_UnsupportedEncoding),
    (No: SCInvalidXmlDecSyntax; Desc: SCRes_InvalidXmlDecSyntax),
    (No: SCUnexpectedAttribute; Desc: SCRes_UnexpectedAttribute),
    (No: SCInvalidVersionNumber; Desc: SCRes_InvalidVersionNumber),
    (No: SCCDATANotClosed; Desc: SCRes_CDATANotClosed),
    (No: SCCommentNotClosed; Desc: SCRes_CommentNotClosed),
    (No: SCMissingWhiteSpace; Desc: SCRes_MissingWhiteSpace),
    (No: SCExpectedCharacterNotFound; Desc: SCRes_ExpectedCharacterNotFound),
    (No: SCInvalidAttributeValue; Desc: SCRes_InvalidAttributeValue),
    (No: SCInvalidUnicodeCharacter; Desc: SCRes_InvalidUnicodeCharacter),
    (No: SCInvalidStandaloneValue; Desc: SCRes_InvalidStandaloneValue),
    (No: SCInvalidXmlInstReplacement; Desc: SCRes_InvalidXmlInstReplacement),
    (No: SCInvalidExpectedCharacter; Desc: SCRes_InvalidExpectedCharacter),
    (No: SCInvalidDTDName; Desc: SCRes_InvalidDTDName),
    (No: SCInvalidDTDLocation; Desc: SCRes_InvalidDTDLocation),
    (No: SCInvalidDTDParameter; Desc: SCRes_InvalidDTDParameter),
    (No: SCInvalidDTDPublicValue; Desc: SCRes_InvalidDTDPublicValue),
    (No: SCInvalidDTDSystemValue; Desc: SCRes_InvalidDTDSystemValue),
    (No: SCInvalidDTDElementName; Desc: SCRes_InvalidDTDElementName),
    (No: SCDTDElementNameExpected; Desc: SCRes_ElementNameExpected),
    (No: SCInvalidDTDAttributeValue; Desc: SCRes_InvalidDTDAttributeValue),
    (No: SCInvalidDTDAttributeDefault; Desc: SCRes_InvalidDTDAttributeDefault),
    (No: SCInvalidDTDEntityValue; Desc: SCRes_InvalidDTDEntityValue),
    (No: SCInvalidDTDCharacter; Desc: SCRes_InvalidDTDCharacter),
    (No: SCInvalidDTDDefination; Desc: SCRes_InvalidDTDDefination),
    (No: SCInvalidDTDXMLElement; Desc: SCRes_InvalidDTDXMLElement),
    (No: SCInvalidDTDDeclarationName; Desc: SCRes_InvalidDTDDeclarationName),
    (No: SCInvalidDTDContentModel; Desc: SCRes_InvalidDTDContentModel),
    (No: SCInvalidExternalID; Desc: SCRes_InvalidExternalID),
    (No: SCInvalidAttributeType; Desc: SCRes_InvalidAttributeType),
    (No: SCInvalidAttributeDefault; Desc: SCRes_InvalidAttributeDefault),
    (No: SCMultipleDOCTYPEDeclaration; Desc: SCRes_MultipleDOCTYPEDeclaration),
    (No: SCOutSideDOCTYPEDeclaration; Desc: SCRes_OutSideDOCTYPEDeclaration),
    (No: SCDuplicateDTDElement; Desc: SCRes_DuplicateDTDElement),
    (No: SCUndefinedDTDElement; Desc: SCRes_UndefinedDTDElement)
    );

  UniInvalidAttributeChars: WideString = 'é?<>[]{}()#$@*&/\!''^+%~;,|`';
  // UniInvalidAttributeFirstChars: WideString = '-:.0123456789';
  UniInvalidAttributeFirstChars: WideString = '0123456789.:<>[]{}()#$@=*&\!''^+%~;,|`''"-:';
  UniInvalidTagChars: WideString = '?<>[]{}()$@"=*&/\!''^+%~;,|`';
  // UniInvalidTagFirstChars: WideString = '-:.0123456789';
  UniInvalidTagFirstChars: WideString = '0123456789.:<>[]{}()#$@=*&\!''^+%~;,|`''"-:';
  UniInvalidElementChars: WideString = 'é?<>[]{}()#$@"=*&/\!''^+%~;,|`';
  UniInvalidElementFirstChars: WideString = '0123456789.:<>[]{}()#$@=*&\!''^+%~;,|`''"-:';
  UniInvalidAttributeValueChars: Widestring = '<';
  UniInvalidDataValueChars: WideString = '<>';
  UniValidEscapeFirstChars: WideString = 'x0123456789';
  UniValidEscapeChars: WideString = 'abcdefABCDEF0123456789#';
  UniValidNameSeperators: WideString = '.-_:';

  UniTagStarter = WideChar('<');
  UniTagTerminator = WideChar('>');
  UniPathTerminator = WideChar('/');
  UniAttrStarter = UniWideSpace;
  UniAttrTerminator = UniWideEqual;
  UniValueStarter = UniWideDoubleQuatMark;
  UniValueTerminator = UniWideDoubleQuatMark;
  UniValueStarter_2 = UniWideQuatMark;
  UniValueTerminator_2 = UniWideQuatMark;
  UniDeclarationStarter = UniWideExclaim;
  UniCommentStarter = UniWideExclaim;
  UniCommentTerminator = WideChar('-');
  UniCData = WideString('![CDATA[');
  UniCDataStarter = WideChar('[');
  UniCDataTerminator = WideChar(']');
  UniCDataTerminationTag = Widestring(']]>');
  UniInstructionStarter = UniWideQuestion;
  UniInstructionDataStarter = UniWideSpace;
  UniInstructionTerminator = UniWideQuestion;
  UniComment = WideString('--');
  UniCommentStart = WideString('!--');
  UniEntityRefStarter = WideChar('&');
  UniEntityRefTerminator = WideChar(';');
  UniWideXmlInstruction = WideString('xml');
  UniEncoding = WideString('encoding');
  UniVersion = WideString('version');
  UniStandalone = WideString('standalone');
  UniYes = WideString('yes');
  UniNo  = WideString('no');
  UniWideNumber = WideChar('#');
  UniDTDPublicStarter = WideString('PUBLIC');
  UniDTDSystemStarter = WideString('SYSTEM');

  UniDTD = WideString('!DOCTYPE');
  UniDTDDefinationStarter = WideChar('<');
  UniDTDDefinationTerminator = WideChar('>');
  UniDTDSysSource = WideString('SYSTEM');
  UniDTDPubSource = WideString('PUBLIC');
  UniInternalDTDStarter = WideChar('[');
  UniInternalDTDTerminator = WideChar(']');
  UniDTDElement = WideString('!ELEMENT');
  UniDTDAttributeList = WideString('!ATTLIST');
  UniDTDEntity = WideString('!ENTITY');
  UniDTDNotation = WideString('!NOTATION');
  UniDTDPCDATA = WideString('#PCDATA');
  UniDTDAny = WideString('ANY');
  UniDTDEmpty = WideString('EMPTY');
  UniDTDMixed = WideString('mixed');
  UniDTDImplied = WideString('#IMPLIED');
  UniDTDRequired = WideString('#REQUIRED');
  UniDTDFixed = WideString('#FIXED');
  UniDTDContentStarter = WideChar('(');
  UniDTDContentTerminator = WideChar(')');
  UniDTDChildSeperator = WideChar(',');
  UniDTDChildZeroOrOne = WideChar('?');
  UniDTDChildZeroOrMore = WideChar('*');
  UniDTDChildOneOrMore = WideChar('+');
  UniDTDAttrDefStarter = WideChar('#');
  UniDTDChoiceSeperator = WideChar('|');
  UniDTDAttrCDATA = WideString('CDATA');
  UniDTDAttrID = WideString('ID');
  UniDTDAttrIDREF = WideString('IDREF');
  UniDTDAttrIDREFS = WideString('IDREFS');
  UniDTDAttrNMTOKEN = WideString('NMTOKEN');
  UniDTDAttrNMTOKENS = WideString('NMTOKENS');
  UniDTDAttrENTITY = WideString('ENTITY');
  UniDTDAttrENTITIES = WideString('ENTITIES');
  UniDTDAttrNOTATION = WideString('NOTATION');
  UniDTDAttrXML = WideString('xml:');
  UniDTDAttrDefs = WideString('CDATA,ID,IDREF,IDREFS,NMTOKEN,NMTOKENS,ENTITY,ENTITIES,NOTATION,xml:,');

  SCReservedInstNames: array[0..0] of string = (
    'xml'
    );

  SCValidVersionNumbers: array[0..0] of string = (
    '1.0'
    );

  SCEscapes: array[0..4, 0..1] of string = (
    ('&amp;', '&'), ('&lt;', '<'), ('&gt;', '>'), ('&apos;', ''''), ('&quot;', '"')
    );

  SCEncodingSet: array[0..28] of TSCEncodingMap = (
    (WinCharset: 0;   WinCodePage: '1252'; Encoding: 'iso-8859-1';
      Description: 'US/Western European (Latin-1)'; CanSwitch: True),
    (WinCharset: 128; WinCodePage: '932'; Encoding: 'Shift_JIS';
      Description: 'Japanese (Shift JIS)'; CanSwitch: True),
    (WinCharset: 128; WinCodePage: '932'; Encoding: 'iso-2022-jp';
      Description: 'Japanese (JIS)'; CanSwitch: True),
    (WinCharset: 128; WinCodePage: '932'; Encoding: 'euc-jp';
      Description: 'Japanese (EUC)'; CanSwitch: True),
    (WinCharset: 134; WinCodePage: '936'; Encoding: 'gb2312';
      Description: 'Simplified Chinese (gb2312)'; CanSwitch: True),
    (WinCharset: 136; WinCodePage: '950'; Encoding: 'big5';
      Description: 'Traditional Chinese (Big5)'; CanSwitch: True),
    (WinCharset: 161; WinCodePage: '1253'; Encoding: 'iso-8859-7';
      Description: 'Greek (iso-8859-7)'; CanSwitch: True),
    (WinCharset: 161; WinCodePage: '1253'; Encoding: 'windows-1253';
      Description: 'Greek (Windows-1253)'; CanSwitch: True),
    (WinCharset: 162; WinCodePage: '1254'; Encoding: 'iso-8859-9';
      Description: 'Turkish (iso-8859-9)'; CanSwitch: True),
    (WinCharset: 162; WinCodePage: '1254'; Encoding: 'windows-1254';
      Description: 'Turkish (Windows-1254)'; CanSwitch: True),
    (WinCharset: 163; WinCodePage: '1258'; Encoding: 'windows-1258';
      Description: 'Vietnamese'; CanSwitch: True),
    (WinCharset: 177; WinCodePage: '1255'; Encoding: 'iso-8859-8';
      Description: 'Hebrew (iso-8859-8)'; CanSwitch: True),
    (WinCharset: 177; WinCodePage: '1255'; Encoding: 'windows-1255';
      Description: 'Hebrew (Windows-1255)'; CanSwitch: True),
    (WinCharset: 178; WinCodePage: '1256'; Encoding: 'iso-8859-6';
      Description: 'Arabic (iso-8859-6)'; CanSwitch: True),
    (WinCharset: 178; WinCodePage: '1256'; Encoding: 'windows-1256';
      Description: 'Arabic (Windows-1256)'; CanSwitch: True),
    (WinCharset: 186; WinCodePage: '1257'; Encoding: 'windows-1257';
      Description: 'Baltic (Windows-1257)'; CanSwitch: True),
    (WinCharset: 186; WinCodePage: '1257'; Encoding: 'iso-8859-13';
      Description: 'Baltic (iso-8859-13)'; CanSwitch: True),
    (WinCharset: 204; WinCodePage: '1251'; Encoding: 'iso-8859-5';
      Description: 'Cyrillic (iso-8859-5)'; CanSwitch: True),
    (WinCharset: 204; WinCodePage: '1251'; Encoding: 'windows-1251';
      Description: 'Cyrillic (Windows-1251)'; CanSwitch: True),
    (WinCharset: 204; WinCodePage: '1251'; Encoding: 'koi8-r';
      Description: 'Cyrillic (KOI8-R)'; CanSwitch: True),
    (WinCharset: 222; WinCodePage: '874'; Encoding: 'windows-874';
      Description: 'Thai (Windows-874)'; CanSwitch: True),
    (WinCharset: 222; WinCodePage: '874'; Encoding: 'iso-8859-11';
      Description: 'Thai (iso-8859-11)'; CanSwitch: True),
    (WinCharset: 238; WinCodePage: '1250'; Encoding: 'iso-8859-2';
      Description: 'Central European (iso-8859-2)'; CanSwitch: True),
    (WinCharset: 238; WinCodePage: '1250'; Encoding: 'windows-1250';
      Description: 'Central European (Windows-1250)'; CanSwitch: True),
    (WinCharset: -1; WinCodePage: '-1'; Encoding: 'ks_c_5601-1987';
      Description: 'Korean (Wansung)'; CanSwitch: True),
    (WinCharset: -1; WinCodePage: '-1'; Encoding: 'euc-kr';
      Description: 'Korean (EUC-KR)'; CanSwitch: True),
    (WinCharset: -1; WinCodePage: '-1'; Encoding: 'iso-8859-1';
      Description: 'Icelandic SC (iso-8859-1)'; CanSwitch: True),
    (WinCharset: -1; WinCodePage: '-1'; Encoding: 'utf-8';
      Description: 'Utf-8'; CanSwitch: True),
    (WinCharset: -1; WinCodePage: '-1'; Encoding: 'utf-16';
      Description: 'Utf-16'; CanSwitch: False)
    );

implementation

type
  TDTDParseState = (dtdSpace, dtdInComment, dtdInTypeStore, dtdInElement, dtdInElementOccurance,
    dtdInElementGroup, dtdInAttributeList, dtdInAttributeName, dtdInAttributeType,
    dtdInAttributeDefault, dtdInAttributeDefaultValue, dtdInEntity, dtdInEntityDescription);

function IsValidVersionNumber(const AVersion: WideString): Boolean;
var
  I: Integer;
begin
  Result := AVersion <> '';
  if not Result then Exit;

  for I := Low(SCValidVersionNumbers) to High(SCValidVersionNumbers) do
    if SC_IsSameStr(AVersion, SCValidVersionNumbers[I]) then
    begin
      Result := True;
      Exit;
    end;
end;

function IsValidElementChar(const C: WideChar; P: Integer): Boolean;
begin
  if P = 1 then
  begin
    Result := not ((Pos(C, UniInvalidElementFirstChars) > 0) or
      (Pos(C, UniInvalidElementChars) > 0) or
      ((C <> WideChar('_')) and not (Char(C) in ['a'..'z', 'A'..'Z'])));
  end else
    Result := not ((Pos(C, UniInvalidElementChars) > 0) or
      ((Pos(C, UniValidNameSeperators) = 0) and not (Char(C) in ['a'..'z', 'A'..'Z', '0'..'9'])));
end;

function IsValidElementName(const AName: WideString): Boolean;
var
  I, Ln: LongInt;
begin
  Result := False;

  Ln := Length(AName);
  if Ln > 0 then
  begin
    I := 0;
    while I < Ln do
    begin
      Inc(I);
      if not IsValidElementChar(AName[I], I) then
        Exit;
    end;

    Result := True;
  end;  
end;

function IsValidInstructionName(const AName: WideString): Boolean;
var
  I: Integer;
begin
  Result := IsValidElementName(AName);
  if not Result then Exit;

  Result := False;

  for I := Low(SCReservedInstNames) to High(SCReservedInstNames) do
    if SameText(AName, SCReservedInstNames[I]) and
      (CompareText(AName, SCReservedInstNames[I]) <> 0) then
      Exit;

  Result := True;
end;

function IndexOfEncoding(const AEncoding: WideString): Integer;
var
  I: Integer;
begin
  Result := -1;

  if SC_WideTrim(AEncoding) <> '' then
    for I := Low(SCEncodingSet) to High(SCEncodingSet) do
      if AnsiSameText(AEncoding, 'windows-' + SCEncodingSet[I].WinCodePage) or
        AnsiSameText(AEncoding, SCEncodingSet[I].Encoding) then
      begin
        Result := I;
        Exit;
      end;
end;

function IsValidEncoding(const AEncoding: WideString): Boolean;
begin
  Result := IndexOfEncoding(AEncoding) > -1;
end;

function CanSwitchToEncoding(const FromEncoding, ToEncoding: WideString): Boolean;
var
  Indx: Integer;
begin
  Result := (FromEncoding = '') and (ToEncoding <> '');
  
  if Result then
  begin
    Indx := IndexOfEncoding(FromEncoding);
    Result := Indx > -1;

    if not (Result and SCEncodingSet[Indx].CanSwitch) then
    begin
      Indx := IndexOfEncoding(ToEncoding);
      Result := (Indx > -1) and SCEncodingSet[Indx].CanSwitch;
    end;
  end;
end;

function EncodingDescription(const AEncoding: WideString): WideString;
var
  Indx: Integer;
begin
  Result := '';
  Indx := IndexOfEncoding(AEncoding);

  if Indx > -1 then Result := SCEncodingSet[Indx].Description;
end;


function SC_CleanInvalidChars(const Str: WideString): WideString;
var
  I: Integer;
begin
  Result := '';

  for I := 1 to Length(Str) do
  begin
    if Str[I] = '&' then
      Result := Result + '&amp;'
    else
    if Str[I] = '''' then
      Result := Result + '&apos;'
    else
    if Str[I] = '<' then
      Result := Result + '&lt;'
    else
    if Str[I] = '>' then
      Result := Result + '&gt;'
    else
    if Str[I] = '"' then
      Result := Result + '&quot;'
    else
    if Word(Str[I]) > 255 then
      Result := Result + '&#' + IntToStr(Word(Str[I])) + ';'
    else
      Result := Result + Str[I];
  end;
end;

function SC_CleanXMLChars(const Str: WideString): WideString;
var
  S: WideString;
  I, CharAsNum: LongInt;
begin
  Result := '';

  I := 0;
  while I < Length(Str) do
  begin
    Inc(I);

    if Str[I] = '&' then
    begin
      S := '&';
      while I < Length(Str) do
      begin
        Inc(I);

        S := S + Str[I];
        if Str[I] = ';' then
          Break;
      end;

      if S = '&amp;' then
        Result := Result + '&'
      else
      if S = '&apos;' then
        Result := Result + ''''
      else
      if S = '&lt;' then
        Result := Result + '<'
      else
      if S = '&gt;' then
        Result := Result + '>'
      else
      if S = '&quot;' then
        Result := Result + '"'
      else
      if (Length(S) > 3) and (S[2] = '#') then
      begin
        CharAsNum := StrToIntDef(Copy(S, 3, Length(S) - 3) , -1);

        if CharAsNum = -1 then
          Result := Result + S
        else
          Result := Result + WideChar(CharAsNum);
      end else
        Result := Result + S;
    end else
      Result := Result + Str[I];

  end;
end;

{ TSCCustomSaxParser }

function TSCCustomSaxParser.ChangeToken(EnteredToken: TSCTokenKind;
  const Value: WideString): Boolean;
var
  ReleasedToken: TSCTokenKind;
begin
  FTokenCharCount := 0;

  ReleasedToken := FToken;
  Result := CheckForError(ReleasedToken, EnteredToken, Value, FPath);

  if Result then
  begin
    if FEncodingRequested and (EnteredToken <> stkDocumentTop) and (FPath <> '') then
    begin
      Result := False;
      Exit;
    end;

    if (FToken <> EnteredToken) then
    begin
      FLastTokenLine   := FLinePos;
      FLastTokenColumn := FColumnPos;

      FToken := EnteredToken;
      DoToken(ReleasedToken, EnteredToken, Value, Result);

      FPrevToken := ReleasedToken;
    end;

    if FToken = stkAttribute then
      DoAttributeStart('', FLinePos, FColumnPos, Result)
    else if FToken = stkAttributeValue then
      DoAttributeValueStart(Value, FLinePos, FColumnPos, Result)
  end;
end;

function TSCCustomSaxParser.CheckForError(ReleasedToken,
  EnteredToken: TSCTokenKind; const Value, APath: WideString): Boolean;
begin
  Result := True;

  if (ReleasedToken in [stkTAG, stkAttribute]) and not IsValidElementName(Value) then
  begin
    Result := False;
    DoInvalidNameError(Value);
    Exit;
  end;

  if (ReleasedToken = stkComment) and (Pos(UniComment, Value) > 0) then
    Result := DoError(SCErrorCommentHasTermination, ReleasedToken, EnteredToken, Value, -1, -1)
  else
  if (ReleasedToken = stkAttribute) and (EnteredToken = stkText) then
    Result := DoError(SCErrorAttrValueNotFound, ReleasedToken, EnteredToken, Value, -1, -1)
  else
  if Result and (ReleasedToken = stkAttribute) and (EnteredToken = stkTAG) then
    Result := DoError(SCErrorTagOpenedInTag, ReleasedToken, EnteredToken, Value, -1, -1)
  else
  if (ReleasedToken = EnteredToken) and not (ReleasedToken in [stkDocumentTop, stkText]) then
    Result := DoError(SCErrorSameTokens, ReleasedToken, EnteredToken, Value, -1, -1);
end;

constructor TSCCustomSaxParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToken := stkDocumentTop;
  FPrevToken := stkDocumentTop;
  FLinePos := 0;
  FColumnPos := 0;
  FLastTokenLine := 0;
  FLastTokenColumn := 0;
  FStandalone := False;
  FErrorChar := UniWideNull;
  FExpectedChar := UniWideNull;
  FErrorOptions := [];

  SetLength(FRoot, 0);
  SetLength(FElement, 0);
  SetLength(FPath, 0);
  SetLength(FAttributeName, 0);
  SetLength(FAttributeList, 0);
  SetLength(FPI, 0);
  FValueStart := UniValueStarter;

  SetLength(FVersion, 0);
  SetLength(FEncoding, 0);

  FEscapeParser := TSCEscapeParser.Create;
  FDTDParser := TSCSaxDTDParser.Create;

  FDTDParser.OnDTDElement   := DoDTDElement;
  FDTDParser.OnDTDAttribute := DoDTDAttribute;
  FDTDParser.OnDTDEntity    := DoDTDEntity;
  FDTDParser.OnDTDNotation  := DoDTDNotation;
  FDTDParser.OnDTDComment   := DoDTDComment;
end;

procedure TSCCustomSaxParser.DoDocumentEnd(var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnEndDocument) then FOnEndDocument(Self, CanContinue);
end;

procedure TSCCustomSaxParser.DoDocumentStart(var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnStartDocument) then FOnStartDocument(Self, CanContinue);
end;

function TSCCustomSaxParser.DoError(ErrorCode: Integer; ReleasedToken,
  EnteredToken: TSCTokenKind; const Value: WideString; ErrLine,
  ErrColumn: LongInt): Boolean;
var
  L: Integer;
begin
  Result := false;
  FLastError := ErrorCode;

  L := FLinePos;
  if FIgnoreRoot then
    Dec(L);

  if ErrLine = -1 then ErrLine := L;
  if ErrColumn = -1 then ErrColumn := FColumnPos;

  FErrorLine   := ErrLine;
  FErrorColumn := ErrColumn;

  if Assigned(FOnError) then FOnError(Self, ErrorCode, FErrorLine, FErrorColumn,
    ReleasedToken, EnteredToken, Value, FPath);

  if FRaiseErrors then
    raise Exception.CreateFmt(SCRes_ParseError,
      [ErrorDescription(ErrorCode), FErrorLine, FErrorColumn]);
end;

function TSCCustomSaxParser.IsWhiteSpace(const AChar: WideChar): boolean;
begin
  Result := SC_WideCharIsSpace(AChar) or SC_WideCharIsControl(AChar);
end;

function TSCCustomSaxParser.ParseFile(AFileName: String): Boolean;
var
  ReadStream: TFileStream;
begin
  ReadStream := nil;
  try
    ReadStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    Result := ParseStream(ReadStream);
  finally
    if ReadStream <> nil then ReadStream.Free;
  end;
end;

function TSCCustomSaxParser.ParseStream(AStream: TStream): Boolean;
var
  SW: WideString;
  SA: AnsiString;
  UTFSignature: WideChar;
  UTF8_3rdSignature: Char;
  Size, BytesRead: Integer;
begin
  Result := False;
  if AStream = nil then Exit;

  Size := AStream.Size - AStream.Position;

  if Size = 0 then
  begin
    DoEmptyDocumentError;
    Exit;
  end;

  BytesRead := 0;

  UTFSignature := #0;
  if Size > 1 then
    BytesRead := AStream.Read(UTFSignature, 2);

  UTF8_3rdSignature := #0;
  if Size > 2 then
    BytesRead := BytesRead + AStream.Read(UTF8_3rdSignature, 1);

  if (UTFSignature = UniBOM) or (UTFSignature = UniBigEndianBOM) then
  begin
    AStream.Seek(-1, soFromCurrent);

    SetLength(SW, (Size - 2) div 2);
    AStream.Read(PWideChar(SW)^, Size - 2);

    if UTFSignature = UniBigEndianBOM then
      SC_WideSwapByteOrder(SW);

    Result := ParseString(SW);
  end else
  if (UTFSignature = Utf8BOM_1) and (UTF8_3rdSignature = Utf8BOM_2) then
  begin
    SetLength(SA, Size-3);
    AStream.Read(PChar(SA)^, Size-3);

    SW := SC_UTF8ToWideString(SA);
    Result := ParseString(SW);
  end else
  begin
    AStream.Seek(-BytesRead, soFromCurrent);

    SetLength(SA, Size);
    AStream.Read(PChar(SA)^, Size);

    SW := WideString(SA);
    Result := ParseString(SW);
  end;
end;

function TSCCustomSaxParser.ConvertXmlInstError(AError: TSCXmlInstructionError): Integer;
begin
  Result := 0;

  case AError of
    xieNoData:
      Result := SCInvalidXmlDecSyntax;
    xieInvalidEncoding,
    xieInvalidVersion, xieInvalidAttribute:
      Result := SCUnexpectedAttribute;
    xieInvalidStandaloneValue:
      Result := SCInvalidStandaloneValue;
    xieInvalidXmlInstReplacement:
      Result := SCInvalidXmlInstReplacement;
    xieValueNotFound:
      Result := SCErrorAttrValueNotFound;
  end;
end;

function TSCCustomSaxParser.ParseXmlInstruction(const xmlData: WideString;
  var AEncoding, AVersion, AStandalone: WideString; var HasEncoding, HasVersion,
  HasStandalone: Boolean; var ErrorLine, ErrorCol: Integer): TSCXmlInstructionError;
var
  ACol, ALine, Ln, i,
  StartLine, StartCol,
  PrevCol, PrevLine: LongInt;

  Attr, AValue: WideString;

  IsWhite, InAttrName, InValue,
  EncodeFound, VersionFound,
  StandaloneFound, IsEncoding,
  IsVersion, IsStandalone: Boolean;

  AttrValueDel, PrChar, IChar, NxChar: WideChar;

  procedure ArrangeLineCol;
  begin
    if IChar = UniWideLF then
    begin
      PrevCol := ACol;
      ACol   := 0;
    end else
    if IChar = UniWideCR then
    begin
      PrevCol  := ACol;
      PrevLine := ALine;

      // FColumnPos := 0;
      ACol := 0;
      Inc(ALine);
    end else
    begin
      PrevCol := ACol;
      Inc(ACol);
    end;
  end;

  function NextChar: boolean;
  begin
    Result := false;
    if i > Ln then Exit;

    PrChar := IChar;
    NxChar := UniWideNull;

    Inc(i);
    Result := i <= Ln;
    if not result then exit;

    IChar := xmlData[i];
    ArrangeLineCol;

    if i + 1 <= Ln then NxChar := xmlData[i + 1];
  end;

  function LoopWhileSpace: boolean;
  begin
    while i <= Ln do
    begin
      if not IsWhiteSpace(IChar) then Break;

      Result := NextChar;
      if not result then exit;
    end;

    Result := i <= Ln;
  end;

begin
  HasEncoding   := False;
  HasVersion    := False;
  HasStandalone := False;

  SetLength(AEncoding, 0);
  SetLength(AVersion, 0);
  SetLength(AStandalone, 0);

  SetLength(Attr, 0);
  SetLength(AValue, 0);

  ErrorLine := -1;
  ErrorCol  := -1;
  Result    := xieNoError;

  Ln := Length(xmlData);
  if Ln = 0 then
  begin
    ErrorLine := 0;
    ErrorCol  := 0;
    Result    := xieNoData;

    Exit;
  end;

  ACol  := 0;
  ALine := 0;

  PrevCol  := 0;
  PrevLine := 0;

  PrChar := UniWideNull;
  NxChar := UniWideNull;
  IChar  := UniWideNull;

  I := 0;

  if not LoopWhileSpace then
  begin
    ErrorLine := ALine;
    ErrorCol  := ACol;
    Result    := xieNoData;

    Exit;
  end;

  InAttrName   := True;
  InValue      := False;
  IsEncoding   := False;
  IsVersion    := False;
  IsStandalone := False;

  EncodeFound     := False;
  VersionFound    := False;
  StandaloneFound := False;

  StartLine := ALine;
  StartCol  := ACol;

  AttrValueDel := UniWideNull;

  try
    while NextChar do
    begin
      if InAttrName then
      begin
        IsWhite := IsWhiteSpace(IChar);

        if IsWhite or (IChar = UniAttrTerminator) then
        begin
          StartLine := ALine;
          StartCol  := ACol - 1;

          if not IsValidElementName(Attr) then
          begin
            ErrorLine := StartLine;
            ErrorCol  := StartCol;
            Result    := xieInvalidAttribute;

            Exit;
          end;

          SetLength(AValue, 0);

          if (IsWhite and not LoopWhileSpace) or
            not (NextChar and LoopWhileSpace and
            ((IChar = UniValueStarter) or (IChar = UniValueStarter_2))) then
          begin
            ErrorLine := ALine;
            ErrorCol  := ACol;
            Result    := xieValueNotFound;

            Exit;
          end;

          AttrValueDel := IChar;

          IsEncoding   := SC_IsSameStr(Attr, UniEncoding);
          IsVersion    := not IsEncoding and SC_IsSameStr(Attr, UniVersion);
          IsStandalone := not (IsEncoding or IsVersion) and SC_IsSameStr(Attr, UniStandalone);

          if not (IsEncoding or IsVersion or IsStandalone) or
            (IsEncoding and EncodeFound) or (IsVersion and VersionFound) or
            (IsStandalone and StandaloneFound) then
          begin
            ErrorLine := StartLine;
            ErrorCol  := StartCol;
            Result    := xieInvalidAttribute;

            Exit;
          end;

          if IsVersion and not VersionFound then
          begin
            if EncodeFound or StandaloneFound then
            begin
              ErrorLine := StartLine;
              ErrorCol  := StartCol;
              Result    := xieInvalidXmlInstReplacement;

              Exit;
            end;

            VersionFound := true;
          end else
          if IsEncoding and not EncodeFound then
          begin
            if StandaloneFound or not VersionFound then
            begin
              ErrorLine := StartLine;
              ErrorCol  := StartCol;
              Result    := xieInvalidXmlInstReplacement;

              Exit;
            end;

            EncodeFound := true;
          end else
          if IsStandalone and not StandaloneFound then
          begin
            if not (VersionFound and EncodeFound) then
            begin
              ErrorLine := StartLine;
              ErrorCol  := StartCol;
              Result    := xieInvalidXmlInstReplacement;

              Exit;
            end;

            StandaloneFound := true;
          end;

          InAttrName := False;
          InValue    := True;

          SetLength(Attr, 0);

          // StartLine  := ALine;
          // StartCol   := ACol;
        end else
          Attr := Attr + IChar;
      end else
      if InValue then
      begin
        if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
        begin
          AttrValueDel := UniWideNull;

          SetLength(Attr, 0);

          InAttrName := True;
          InValue    := False;

          if IsEncoding then
          begin
            AEncoding   := AEncoding + AValue;
            HasEncoding := True;
          end else
          if IsVersion then
          begin
            AVersion   := AVersion + AValue;
            HasVersion := True;
          end else
          if IsStandalone then
          begin
            AStandalone   := AStandalone + AValue;
            HasStandalone := True;

            if (AStandalone <> WideString(UniYes)) and (AStandalone <> WideString(UniNo)) then
            begin
              ErrorLine := StartLine;
              ErrorCol  := StartCol;
              Result    := xieInvalidStandaloneValue;

              Exit;
            end;
          end;

          if not (NextChar and IsWhiteSpace(IChar) and LoopWhileSpace) then
            Exit;

          Attr := IChar;

          StartLine := ALine;
          StartCol  := ACol;
        end else
          AValue := AValue + IChar;
      end;
    end;
  finally
    if InValue then
    begin
      ErrorLine := ALine;
      ErrorCol  := ACol;
      Result    := xieValueNotFound;
    end else
    if InAttrName and (Attr <> '') then
    begin
      ErrorLine := StartLine;
      ErrorCol  := StartCol;
      Result    := xieInvalidAttribute;
    end else
    if not (HasEncoding or HasVersion or HasStandalone) then
    begin
      ErrorLine := ALine;
      ErrorCol  := ACol;
      Result    := xieNoData;
    end;
  end;
end;

function TSCCustomSaxParser.ParseString(const S: WideString): Boolean;
var
  i, LenS, StrLen,
  PrevLine, PrevCol,
  ALine, ACol, ValueLine, ValueCol: LongInt;

  TagPos, TagCnt: LongInt;
  WideTx, InvalidValueChars: WideString;

  EscapeError: TSCEscapeError;
  DTDError: TSCDTDError;

  Str, PrevElement: WideString;
  DTDName, PublicID, SystemID: WideString;

  TkCnt: DWord;

  IsNextRoot, ValidPath: Boolean;
  AttrValueDel, IChar, PrChar, NxChar: WideChar;
  NextToken: TSCTokenKind;
  IsSpace, CanContinue, DTDDefined: Boolean;

  procedure ResetHolder;
  begin
    SetLength(Str, 0);
  end;

  function ElementFromPath(const APath: WideString): WideString;
  var
    DotP: Integer;
  begin
    Result := APath;
    DotP := Pos(UniWidePathSeparator, Result);

    while DotP > 0 do
    begin
      Delete(Result, 1, DotP);
      DotP := Pos(UniWidePathSeparator, Result);
    end;
  end;

  function ArrangePath(const ATag: WideString; var IsValidPath, AnotherRoot: Boolean;
    const TagClosedInTag: Boolean): WideString;
  var
    APath, Tag: WideString;
    SepPos: Integer;
  begin
    Tag := ATag;
    APath := FPath;

    IsValidPath := true;

    AnotherRoot := false;

    SetLength(Result, 0);

    if Length(Tag) > 0 then
    begin
      if Tag[1] = UniPathTerminator then
      begin
        Delete(Tag, 1, 1);

        if Length(Tag) > 0 then
        begin
          APath := SC_WideInvertStr(APath);
          Tag := SC_WideInvertStr(Tag) + UniWidePathSeparator;

          if Pos(Tag, APath) = 1 then
          begin
            Delete(APath, 1, Length(Tag));
            APath := SC_WideInvertStr(APath);
            Dec(FParseLevel);

            FElement := ElementFromPath(APath);
            DoPathInfo(APath, ATag, False, TagClosedInTag);
          end else
          begin
            IsValidPath := false;
            DoError(SCErrorElementMismatch, FToken, NextToken, ATag, -1, -1);
            APath := UniWideNull;
            
            Exit;
          end;
        end else
        begin
          APath := SC_WideInvertStr(APath);

          SepPos := Pos(UniWidePathSeparator, APath);
          if SepPos > 0 then
          begin
            Delete(APath, 1, SepPos);
            Dec(FParseLevel);
            APath := SC_WideInvertStr(APath);

            DoPathInfo(APath, ATag, False, TagClosedInTag);
          end;

          FElement := ElementFromPath(APath);
        end;
      end else
        if Tag[Length(Tag)] <> UniPathTerminator then
        begin
          if Length(FPath) = 0 then
          begin
            AnotherRoot := Length(FRoot) > 0;
            FParseLevel := 0;
            if not AnotherRoot then FRoot := Tag;
          end;

          APath := FPath + UniWidePathSeparator + Tag;
          Inc(FParseLevel);
          FElement := Tag;

          DoPathInfo(APath, ATag, True, False);
        end;
    end;

    if FParseLevel < 0 then FParseLevel := 0;
    Result := APath;
  end;

  procedure ConcatChar(var AStr: WideString; const AChar: WideChar);
  begin
    AStr := AStr + AChar;
  end;

  procedure ArrangeLineCol;
  begin
    if IChar = UniWideLF then
    begin
      PrevCol := FColumnPos;
      FColumnPos := 0;
    end else
    if IChar = UniWideCR then
    begin
      PrevCol := FColumnPos;
      PrevLine := FLinePos;

      FColumnPos := 0;
      Inc(FLinePos);
    end else
    begin
      PrevCol := FColumnPos;
      Inc(FColumnPos);
    end;
  end;

  procedure ArangeLineColWithStr(AStr: WideString);
  var
    P: PWideChar;
    Start, PIndx, PLn: LongInt;
  begin
    P := Pointer(AStr);

    if P = nil then Exit;

    PIndx := 0;
    PLn := Length(AStr);

    while PIndx < PLn do
    begin
      Start := PIndx;

      while not (P^ in [UniWideCR, UniWideLF]) do
      begin
        Inc(P);
        Inc(PIndx);

        PrevCol := FColumnPos;
        Inc(FColumnPos);

        if PIndx >= PLn then
          Exit;
      end;

      PrevCol := FColumnPos + PIndx - Start;
      FColumnPos := 0;

      if P^ = UniWideCR then
      begin
        PrevLine := FLinePos;
        Inc(FLinePos);
      end;

      if P^ = UniWideCR then
      begin
        Inc(P);
        Inc(PIndx);
      end;

      if PIndx >= PLn then
        Exit;

      if P^ = UniWideLF then
      begin
        Inc(P);
        Inc(PIndx);
      end;
    end;
  end;

  function NextChar: boolean;
  begin
    PrChar := IChar;
    NxChar := UniWideNull;

    Inc(i);
    Result := i <= LenS;
    if not result then exit;

    IChar := S[i];
    ArrangeLineCol;

    if i + 1 <= LenS then NxChar := S[i + 1];
  end;

  function PrevChar: boolean;
  begin
    NxChar := IChar;
    IChar := PrChar;
    PrChar := UniWideNull;

    if PrevCol <> FColumnPos - 1 then FLinePos := PrevLine;
    FColumnPos := PrevCol;

    Dec(i);
    Result := i > 0;
    if i > 1 then PrChar := S[i-1];
  end;

  function LoopWhileSpace: boolean;
  begin
    while i <= LenS do
    begin
      if not IsWhiteSpace(IChar) then Break;

      Result := NextChar;
      if not result then exit;
    end;

    Result := i <= LenS;
  end;

  function DecodePath(const APath: WideString): WideString;
  var
   i, Ln: Integer;
  begin
    Result := APath;
    Ln := Length(Result);

    if Ln > 0 then
    begin
      if Result[1] = UniWidePathSeparator then
      begin
        Delete(Result, 1, 1);
        Ln := Length(Result);
      end;

      i := 1;
      while i <= Ln do
      begin
        if Result[i] = UniWidePathSeparator then
        begin
          Delete(Result, i, 1);
          Insert(', ', Result, i);
          Inc(i);
        end;

        Inc(i);
      end;
    end;
  end;

  function CheckIfElmNameIsValid(const AName: WideString): Boolean;
  var
    I, Ln: LongInt;
    Chr: WideChar;
  begin
    Result := true;

    I := 1;
    Ln := Length(AName);
    while I <= Ln do
    begin
      Chr := AName[I];

      if (I = 1) and (Pos(Chr, UniInvalidElementFirstChars) > 0) then
      begin
        Result := False;
        DoError(SCElementNameStartError, FToken, FToken, AName, -1, -1);

        Exit;
      end;

      if SC_IsCharWhiteSpace(Chr) or (Pos(Chr, UniInvalidElementChars) > 0) then
      begin
        Result := False;
        DoError(SCElementNameError, FToken, FToken, AName, -1, -1);
        
        Exit;
      end;

      Inc(I);
    end;
  end;

begin
  Result := False;
  Inc(FParseCount);

  try
    FErrorChar := UniWideNull;
    FParseLevel := 0;
    FLastError := 0;
    LenS := Length(S);

    CanContinue := True;
    DTDDefined  := False;

    if LenS = 0 then
    begin
      DoError(SCErrorEmptyDocument, stkDocumentTop, stkDocumentTop, '', -1, -1);
      Exit;
    end;

    SetLength(FPath, 0);
    SetPath('');

    SetLength(FElement, 0);
    SetLength(FRoot, 0);

    SetLength(FVersion, 0);
    SetLength(FEncoding, 0);
    SetLength(FErrorStr, 0);

    ResetHolder;

    NextToken    := stkDocumentTop;

    FPrevToken   := stkDocumentTop;
    FToken       := stkDocumentTop;
    FLinePos     := 1;
    FColumnPos   := 0;
    FErrorLine   := -1;
    FErrorColumn := -1;

    FTagLine := 0;
    FTagCol  := 0;

    FLastTokenLine   := 1;
    FLastTokenColumn := 0;                    

    PrevLine := 0;
    PrevCol  := 0;
    FTokenCharCount := 0;

    TkCnt := GetTickCount;
    FProcessTime := 0;

    try
      DoDocumentStart(CanContinue);
      if not CanContinue then Exit;

      IChar := UniWideNull;

      i := 0;
      NextChar;

      if IsWhiteSpace(IChar) and not LoopWhileSpace then
      begin
        DoError(SCErrorEmptyDocument, stkDocumentTop, stkDocumentTop, '', -1, -1);
        Exit;
      end;

      ValueLine := 1;
      ValueCol := 0;

      FDTDType := dtyUndefined;

      AttrValueDel := UniWideNull;

      while i <= LenS do
      begin
        StrLen := Length(Str);
        Inc(FTokenCharCount);

        if not (FToken in [stkAttributeValue, stkText]) then
        begin
          ValueLine := FLinePos;
          ValueCol := FColumnPos;
        end;

        case FToken of
          stkTAG:
          begin
            if FTokenCharCount = 1 then
            begin
              FTagLine := FLinePos;
              FTagCol := FColumnPos;
            end;

            case IChar of
              UniPathTerminator:
              begin
                if NxChar = UniTagTerminator then
                begin
                  if FTokenCharCount = 1 then
                  begin
                    NextToken := stkTAG;
                    DoError(SCErrorWrongElementStart, FToken, NextToken, IChar, -1, -1);
                    Exit;
                  end;

                  SetLength(FAttributeList, 0);
                  DoStartElement(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
                  
                  if not (CanContinue and CheckIfElmNameIsValid(Str)) then
                    Exit;

                  SetPath(ArrangePath(Str, ValidPath, IsNextRoot, true));

                  if IsNextRoot then
                  begin
                    DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                    Exit;
                  end;

                  if not ValidPath then Exit;

                  SetLength(FAttributeList, 0);

                  DoEndElement(FElement, FLinePos, FColumnPos, CanContinue);
                  if not CanContinue then Exit;

                  NextToken := stkText;

                  SetPath(ArrangePath(UniPathTerminator, ValidPath, IsNextRoot, true));
                  if IsNextRoot then
                  begin
                    DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                    Exit;
                  end;

                  if not ValidPath then
                    Exit;

                  if not NextChar then
                    Break;

                  if FPath = '' then
                    NextToken := stkDocumentTop;

                  if not ChangeToken(NextToken, Str) then Exit;

                  ResetHolder;
                end else
                begin
                  if IsWhiteSpace(NxChar) then
                  begin
                    NextChar;
                    ConcatChar(Str, IChar);

                    NextToken := stkText;

                    DoError(SCErrorWhiteSpaceNotAllowed, FToken, NextToken, Str, -1, -1);
                    Exit;
                  end;

                  if (PrChar = UniTagStarter) and (FTokenCharCount = 1) then
                  begin
                    ConcatChar(Str, IChar);

                    if not NextChar then Break;

                    ConcatChar(Str, IChar);
                  end else
                  begin
                    NextToken := stkText;
                    ConcatChar(Str, NxChar);

                    DoError(SCErrorTagCloseExpected, FToken, NextToken, Str, -1, -1);
                    Exit;
                  end;
                end;  
              end;
              UniAttrStarter, UniWideTab,
              UniWideCR, UniWideLF:
              begin
                if FTokenCharCount = 1 then
                begin
                  NextToken := stkAttribute;
                  DoError(SCErrorWhiteSpaceNotAllowed, FToken, NextToken, IChar, -1, -1);
                  
                  Exit;
                end;

                if (Length(Str) > 0) and
                  (Str[1] = UniPathTerminator) and (FPrevToken = stkText) then
                begin
                  Delete(Str, 1, 1);

                  if Str <> FElement then
                  begin
                    NextToken := stkText;
                    DoError(SCErrorElementMismatch, FToken, NextToken, Str, -1, -1);
                    
                    Exit;
                  end;

                  if not LoopWhileSpace then Break;

                  if IChar <> UniTagTerminator then
                  begin
                    NextToken := stkText;
                    DoError(SCInvalidCharForToken, FToken, NextToken, IChar, -1, -1);
                    
                    Exit;
                  end;

                  Str := UniPathTerminator + Str;
                  Continue;
                end;

                if not CheckIfElmNameIsValid(Str) then Exit;

                SetPath(ArrangePath(Str, ValidPath, IsNextRoot, false));
                if IsNextRoot then
                begin
                  DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                  Exit;
                end;

                if not ValidPath then
                  Exit;

                SetLength(FAttributeList, 0);

                DoStartElement(FElement, FLastTokenLine, FLastTokenColumn, CanContinue);
                if not CanContinue then Exit;

                if not LoopWhileSpace then Break;

                NextToken := stkAttribute;

                if IChar = UniPathTerminator then
                begin
                  NextToken := stkText;

                  if not NextChar then Break;

                  if IChar <> UniTagTerminator then
                  begin
                    ResetHolder;
                    Str := Str + UniPathTerminator + IChar;

                    DoError(SCErrorTagCloseExpected, stkTAG, stkText, Str, -1, -1);
                    Exit;
                  end;

                  Str := SC_WideTrimRight(Str);

                  if not CheckIfElmNameIsValid(Str) then Exit;

                  DoEndElement(FElement, FLinePos, FColumnPos, CanContinue);
                  if not CanContinue then Exit;

                  SetPath(ArrangePath(UniPathTerminator, ValidPath, IsNextRoot, true));

                  if IsNextRoot then
                  begin
                    DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                    Exit;
                  end;

                  if not ValidPath then
                    Exit;

                  if FPath = '' then
                    NextToken := stkDocumentTop;

                  if not ChangeToken(NextToken, Str) then
                    Exit;

                  ResetHolder;
                  Inc(i);
                end else
                begin
                  if IChar = UniAttrTerminator then
                  begin
                    if i > 1 then PrevChar;

                    DoError(SCErrorBlankAttrName, FToken, stkAttributeValue, '', -1, -1);
                    Exit;
                  end;

                  if IChar = UniTagTerminator then
                  begin
                    NextToken := stkText;

                    if FPath = '' then
                      NextToken := stkDocumentTop;

                    if not ChangeToken(NextToken, Str) then Exit;

                    ResetHolder;
                  end else
                  begin
                    if not ChangeToken(NextToken, Str) then Exit;

                    ResetHolder;
                    ConcatChar(Str, IChar);
                  end;
                end;
              end;
              UniTagTerminator:
              begin
                if (PrChar = UniTagStarter) and (FTokenCharCount = 1) then
                begin
                  NextToken := stkText;
                  ConcatChar(Str, IChar);

                  DoError(SCErrorWrongElementStart, FToken, NextToken, Str, -1, -1);
                  Exit;
                end;

                if (StrLen > 0) and (Str[1] = UniPathTerminator) then
                begin
                  Delete(Str, 1, 1);

                  if Str <> FElement then
                  begin
                    NextToken := stkText;
                    DoError(SCErrorElementMismatch, FToken, NextToken, Str, -1, -1);
                      
                    Exit;
                  end;

                  SetPath(ArrangePath(UniPathTerminator, ValidPath, IsNextRoot, Length(Str) = 0));
                  if IsNextRoot then
                  begin
                    DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                    Exit;
                  end;

                  if not ValidPath then
                    Exit;

                  SetLength(FAttributeList, 0);

                  DoEndElement(Str, FLinePos, FColumnPos, CanContinue);
                  if not CanContinue then Exit;
                end else
                begin
                  if not CheckIfElmNameIsValid(Str) then Exit;

                  SetPath(ArrangePath(Str, ValidPath, IsNextRoot, false));
                  if IsNextRoot then
                  begin
                    DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                    Exit;
                  end;
                  
                  if not ValidPath then
                    Exit;

                  SetLength(FAttributeList, 0);

                  DoStartElement(FElement, FLastTokenLine, FLastTokenColumn, CanContinue);
                  if not CanContinue then Exit;
                end;

                NextToken := stkText;

                if FPath = '' then NextToken := stkDocumentTop;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
              end;
              UniDeclarationStarter:
              begin
                if FTokenCharCount = 1 then
                begin
                  if IsWhiteSpace(NxChar) then
                  begin
                    ResetHolder;

                    Str := Str + IChar + NxChar;
                    DoError(SCErrorWhiteSpaceNotAllowed, FToken, FToken, Str, -1, -1);
                    Exit;
                  end;

                  FToken := stkDeclarationLoop;
                  FTokenCharCount := 0;

                  ResetHolder;
                  ConcatChar(Str, UniDeclarationStarter);
                end else
                  ConcatChar(Str, IChar);
              end;
              UniInstructionStarter:
              begin
                if FTokenCharCount = 1 then
                begin
                  NextToken := stkPI;

                  if not ChangeToken(NextToken, Str) then Exit;

                  ResetHolder;
                end else
                  ConcatChar(Str, IChar);
              end;
              else begin
                if not IsValidCharForToken(IChar, FToken, FTokenCharCount) then
                begin
                  if FTokenCharCount = 1 then
                    DoError(SCElementNameStartError, FToken, FToken, IChar, -1, -1)
                  else
                    DoInvalidCharError(IChar);

                  Exit;
                end;

                ConcatChar(Str, IChar);
              end;
            end;
          end;
          stkAttribute:
          begin
            IsSpace := IsWhiteSpace(IChar);

            if IsSpace and not LoopWhileSpace then
            begin
              if Str <> '' then
              begin
                FAttributeName := Str;
                FAttributeList := FAttributeList + Str + UniWidePathSeparator;

                DoAttributeName(Str, '', IChar, FLastTokenLine,
                  FLastTokenColumn, CanContinue);
              end;

              Break;
            end;

            if IChar = UniAttrTerminator then
            begin
              DoAttributeName(Str, '', IChar, FLastTokenLine,
                FLastTokenColumn, CanContinue);

              if not CanContinue then
                Exit;

              if Length(Str) = 0 then
              begin
                if i > 1 then PrevChar;

                DoError(SCErrorBlankAttrName, FToken, stkAttributeValue, '', -1, -1);
                Exit;
              end;

              NextChar;

              if IsWhiteSpace(IChar) and not LoopWhileSpace then
                Break;

              if (IChar = UniValueStarter) or (IChar = UniValueStarter_2) then
              begin
                AttrValueDel := IChar;
                FValueStart  := IChar;

                NextToken := stkAttributeValue;

                ValueLine := FLinePos;
                ValueCol := FColumnPos;

                if Pos(Str + UniWidePathSeparator, FAttributeList) > 0 then
                begin
                  DoError(SCErrorDuplicateAttr, FToken, NextToken, Str, -1, -1);
                  Exit;
                end;

                FAttributeName := Str;
                FAttributeList := FAttributeList + Str + UniWidePathSeparator;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
              end else
              begin
                Str := Str + UniWideSpace + IChar;

                DoError(SCErrorAttrValueNotFound, FToken, stkAttributeValue, Str, -1, -1);
                Exit;
              end;
            end else
            begin
              if IsSpace then
              begin
                if Str <> '' then
                begin
                  FAttributeName := Str;
                  FAttributeList := FAttributeList + Str + UniWidePathSeparator;

                  DoAttributeName(Str, '', IChar, FLastTokenLine,
                    FLastTokenColumn, CanContinue);
                end;

                Break;
              end;

              if (IChar = UniTagTerminator) or (IChar = UniPathTerminator) then
              begin
                if Str = '' then ConcatChar(Str, IChar);

                if i > 1 then
                  PrevChar;

                DoError(SCErrorAttrValueNotFound, FToken, FToken, Str, -1, -1);
                Exit;
              end else
              begin
                if not IsValidCharForToken(IChar, FToken, FTokenCharCount) then
                begin
                  DoInvalidCharError(IChar);
                  Exit;
                end;

                ConcatChar(Str, IChar);
              end;
            end;
          end;
          stkAttributeValue:
          begin
            if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
            begin
              AttrValueDel := UniWideNull;

              InvalidValueChars := UniInvalidAttributeValueChars;
              if FValueStart = UniValueStarter then
                InvalidValueChars := InvalidValueChars + UniValueStarter
              else
                InvalidValueChars := InvalidValueChars + UniValueStarter_2;

              FValueStart := UniValueStarter;

              if (saxCheckEscapes in FErrorOptions) and (Pos(UniWideEscapeStart, Str) > 0) and
                not FEscapeParser.IsValidData(Str, ALine, ACol, FErrorChar, EscapeError,
                InvalidValueChars) then
              begin
                FLinePos := ValueLine;
                FColumnPos := ValueCol;

                Inc(FLinePos, ALine);
                if ALine = 0 then
                  Inc(FColumnPos, ACol)
                else FColumnPos := ACol;

                DoEscapeError(EscapeError, Str);

                Exit;
              end;

              ValueLine := FLinePos;
              ValueCol := FColumnPos;

              DoAttributeValue(FAttributeName, Str, IChar, FLastTokenLine, FLastTokenColumn, CanContinue);
              if not CanContinue then
                Exit;

              SetLength(FAttributeName, 0);

              if not NextChar then
                Break;

              if IsWhiteSpace(IChar) and not LoopWhileSpace then
                Break;

              NextToken := stkAttribute;

              if IChar = UniPathTerminator then
              begin
                PrevElement := ElementFromPath(FPath);

                SetPath(ArrangePath(UniPathTerminator, ValidPath, IsNextRoot, true));
                if IsNextRoot then
                begin
                  DoError(SCErrorOneRootAllowed, FToken, stkTAG, FElement, -1, -1);
                  Exit;
                end;
                
                if not ValidPath then
                  Exit;

                if not NextChar then Break;

                if IChar <> UniTagTerminator then
                begin
                  ConcatChar(Str, IChar);

                  DoError(SCErrorTagCloseExpected, FToken, stkTAG, Str, -1, -1);
                  Exit;
                end;

                SetLength(FAttributeList, 0);

                DoEndElement(PrevElement, FLinePos, FColumnPos, CanContinue);
                if not CanContinue then Exit;
              end;

              if IChar = UniTagTerminator then
              begin
                NextToken := stkText;

                if FPath = '' then NextToken := stkDocumentTop;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
              end else
              begin
                if IChar = UniAttrTerminator then
                begin
                  if i > 1 then PrevChar;

                  DoError(SCErrorBlankAttrName, FToken, stkAttributeValue, '', -1, -1);
                  Exit;
                end;

                NextToken := stkAttribute;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
                ConcatChar(Str, IChar);
              end;
            end else
              ConcatChar(Str, IChar);
          end;
          stkText:
          begin
            if FIgnoreWhiteSpaces and (FTokenCharCount = 1) and
              IsWhiteSpace(IChar) and not LoopWhileSpace then Break;

            if IChar = UniTagStarter then
            begin
              if (saxCheckEscapes in FErrorOptions) and (Pos(UniWideEscapeStart, Str) > 0) and
                not FEscapeParser.IsValidData(Str, ALine, ACol, FErrorChar, EscapeError,
                UniInvalidDataValueChars) then
              begin
                FLinePos := ValueLine;
                FColumnPos := ValueCol;

                Inc(FLinePos, ALine);
                if ALine = 0 then Inc(FColumnPos, ACol)
                else FColumnPos := ACol;

                DoEscapeError(EscapeError, Str);

                Exit;
              end;

              DoText(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
              if not CanContinue then Exit;

              NextToken := stkTAG;

              if NxChar = UniInstructionStarter then
              begin
                if not NextChar then Break;

                ResetHolder;
                NextToken := stkPI;
              end;

              if not ChangeToken(NextToken, Str) then
                Exit;

              ResetHolder;
            end else
            begin
              if FTokenCharCount < 100 then
                ConcatChar(Str, IChar)
              else begin
                TagPos := SC_WideScan(UniTagStarter, S, i-1);

                if TagPos = 0 then
                begin
                  Str := Str + Copy(S, i, LenS - i + 1);

                  Dec(i, FTokenCharCount);
                  IChar := S[i];
                  PrChar := S[i-1];

                  PrevChar;
                  NextChar;

                  Break;
                end;

                Inc(FTokenCharCount, TagPos-i-1);

                if IChar = UniWideCR then
                  WideTx := Copy(S, i + 1, TagPos - i - 1)
                else
                  WideTx := Copy(S, i, TagPos - i);

                ArangeLineColWithStr(WideTx);

                if IChar = UniWideCR then
                  Str := Str + IChar + WideTx
                else
                  Str := Str + WideTx;

                if (FPath = '') and (SC_WideTrimLeft(Str) = '') then
                begin
                  DoError(SCErrorTextAtDocumentTop, stkDocumentTop, stkText, Str, -1, -1);
                  Exit;
                end;

                if (saxCheckEscapes in FErrorOptions) and
                  not FEscapeParser.IsValidData(Str, ALine, ACol, FErrorChar, EscapeError,
                  UniInvalidDataValueChars) then
                begin
                  FLinePos := ValueLine;
                  FColumnPos := ValueCol;

                  Inc(FLinePos, ALine);
                  if ALine = 0 then
                    Inc(FColumnPos, ACol)
                  else FColumnPos := ACol;

                  case EscapeError of
                    escNeedEscapeClose:
                      DoError(SCErrorNeedEscapeClose, FToken, FToken, Str, -1, -1);
                    escUndefinedEscape:
                      DoError(SCErrorUndefinedEscape, FToken, FToken, Str, -1, -1);
                    escInvalidCharacter:
                      DoError(SCInvalidCharacter, FToken, FToken, Str, -1, -1);
                    escInvalidUnicodeCharacter:
                      DoError(SCInvalidUnicodeCharacter, FToken, FToken, Str, -1, -1);
                    escInvalidNameStart:
                      DoError(SCErrorWrongElementStart, FToken, FToken, Str, -1, -1);
                  end;

                  exit;
                end;

                if TagPos > 1 then
                begin
                  i := TagPos;
                  IChar := S[i];
                  PrChar := S[i-1];

                  PrevChar;
                end else
                begin
                  i := TagPos - 1;
                  IChar := UniWideNull;
                  PrChar := UniWideNull;
                end;

                if not NextChar then Break;

                DoText(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
                if not CanContinue then Exit;

                NextToken := stkTAG;

                if NxChar = UniInstructionStarter then
                begin
                  if not NextChar then Break;

                  ResetHolder;
                  NextToken := stkPI;
                end;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
              end;

              if (FPath = '') and not IsWhiteSpace(IChar) then
              begin
                DoError(SCErrorTextAtDocumentTop, stkDocumentTop, stkText, Str, -1, -1);
                Exit;
              end;
            end;
          end;
          stkDeclarationLoop:
          begin
            ConcatChar(Str, IChar);

            if Str = UniCommentStart then
            begin
              ResetHolder;

              NextToken := stkComment;
              if not ChangeToken(NextToken, '') then Exit;
            end else
            if Str = UniDTD then
            begin
              if FPath <> '' then
              begin
                DoError(SCOutSideDOCTYPEDeclaration, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              if DTDDefined then
              begin
                DoError(SCMultipleDOCTYPEDeclaration, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              ResetHolder;

              NextToken := stkDTD;
              if not ChangeToken(NextToken, UniDTD) then
                Exit;

              DTDDefined := True;
            end else
            if Str = UniCData then
            begin
              ResetHolder;

              DoStartCData('', FLastTokenLine, FLastTokenColumn, CanContinue);
              if not CanContinue then Exit;

              NextToken := stkCData;
              if not ChangeToken(NextToken, UniCDATA) then
                Exit;
            end else
            if IsWhiteSpace(IChar) then
            begin
              Str := UniDeclarationStarter + Str;

              if FTokenCharCount = 1 then
              begin
                DoError(SCErrorWhiteSpaceNotAllowed, FToken, FToken, Str, -1, -1)
              end else
                DoError(SCErrorInvalidDeclaration, FToken, FToken, Str, -1, -1);

              Exit;
            end else
            if IChar in [UniTagStarter, UniTagTerminator, UniPathTerminator] then
            begin
              Str := Str + UniDeclarationStarter + IChar;

              if FTokenCharCount = 1 then
                DoError(SCErrorWrongElementStart, FToken, FToken, Str, -1, -1)
              else
                DoError(SCErrorInvalidDeclaration, FToken, FToken, Str, -1, -1);

              Exit;
            end;
          end;
          stkComment:
          begin
            if (IChar = UniCommentTerminator) and (PrChar = UniCommentTerminator) then
            begin
              if NxChar <> UniTagTerminator then
              begin
                DoError(SCErrorCommentHasTermination, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              Delete(Str, StrLen, 1);

              DoComment(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
              if not CanContinue then Exit;

              NextToken := stkText;
              if FPath = '' then NextToken := stkDocumentTop;

              if not ChangeToken(NextToken, Str) then Exit;

              if not NextChar then Break;

              ResetHolder;
            end else
            begin
              TagPos := SC_WideScan(UniComment, S, i-1);

              if TagPos = 0 then
              begin
                Str := Str + Copy(S, i, LenS - i + 1);
                DoError(SCCommentNotClosed, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              Inc(TagPos, Length(UniComment));
              Inc(FTokenCharCount, TagPos-i-1);

              if IChar = UniWideCR then
                WideTx := Copy(S, i + 1, TagPos - i - Length(UniComment) - 1)
              else
                WideTx := Copy(S, i, TagPos - i - Length(UniComment));

              ArangeLineColWithStr(WideTx);

              if IChar = UniWideCR then
                Str := Str + IChar + WideTx
              else
                Str := Str + WideTx;

              Dec(TagPos, Length(UniComment));

              if TagPos > 1 then
              begin
                i := TagPos;
                IChar := S[i];
                PrChar := S[i-1];

                PrevChar;
              end else
              begin
                i := TagPos - 1;
                IChar := UniWideNull;
                PrChar := UniWideNull;
              end;

              for TagCnt := 0 to Length(UniComment) do
                if not NextChar then Break;

              if IChar <> UniTagTerminator then
              begin
                PrevChar;

                DoError(SCErrorCommentHasTermination, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              DoComment(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
              if not CanContinue then Exit;

              NextToken := stkText;
              if FPath = '' then NextToken := stkDocumentTop;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
            end;
          end;
          stkCData:
          begin
            TagPos := SC_WideScan(UniCDataTerminationTag, S, i-1);

            if TagPos = 0 then
            begin
              Str := Copy(S, i, LenS - i + 1);
              DoError(SCCDATANotClosed, FToken, FToken, Str, -1, -1);
              Exit;
            end;

            Inc(TagPos, Length(UniCDataTerminationTag));
            Inc(FTokenCharCount, TagPos-i-1);

            if IChar = UniWideCR then
              Str := Copy(S, i + 1, TagPos - i - Length(UniCDataTerminationTag) - 1)
            else
              Str := Copy(S, i, TagPos - i - Length(UniCDataTerminationTag));

            ArangeLineColWithStr(Str);
            if IChar = UniWideCR then
              Str := IChar + Str;

            Dec(TagPos, Length(UniCDataTerminationTag));

            if TagPos > 1 then
            begin
              i := TagPos;
              IChar := S[i];
              PrChar := S[i-1];

              PrevChar;
            end else
            begin
              i := TagPos - 1;
              IChar := UniWideNull;
              PrChar := UniWideNull;
            end;

            for TagCnt := 0 to Length(UniCDataTerminationTag)-1 do
              if not NextChar then Break;

            DoEndCData(Str, FLinePos, FColumnPos, CanContinue);
            if not CanContinue then Exit;

            // if not NextChar then Break;

            NextToken := stkText;
            if FPath = '' then NextToken := stkDocumentTop;

            if not ChangeToken(NextToken, Str) then Exit;

            ResetHolder;
          end;
          stkDTD:
          begin
            if (IChar = UniInternalDTDStarter) or (IChar = UniTagTerminator) then
            begin
              SetLength(DTDName,  0);
              SetLength(PublicID, 0);
              SetLength(SystemID, 0);

              DTDError := ParseDTDName(Str, DTDName, PublicID, SystemID, ALine, ACol);
              
              if DTDError <> dteNoError then
              begin
                if (ALine > -1) and (ACol > -1) then
                begin
                  if ALine = 0 then
                    FColumnPos := ACol + FLastTokenColumn - 1;

                  FLinePos := ALine + FLastTokenLine;
                end;

                DoDTDError(DTDError, Str);
                Exit;
              end;

              FDTDType := dtyEmbedded;
              if SystemID <> '' then
                FDTDType := dtySystem;

              ResetHolder;
              if not LoopWhileSpace then
                Break;

              if IChar = UniTagTerminator then
              begin
                DoDTDFileStart(DTDName, PublicID, SystemID, FLastTokenLine,
                  FLastTokenColumn, CanContinue);

                if not CanContinue then
                  Exit;
              end else
              if IChar = UniInternalDTDStarter then
              begin
                DoDTDStart(DTDName, PublicID, SystemID, FLastTokenLine,
                  FLastTokenColumn, CanContinue);
                  
                if not CanContinue then
                  Exit;

                TagPos := SC_WideScan(UniInternalDTDTerminator, S, i);

                if TagPos = 0 then
                begin
                  if IChar = UniWideCR then
                    Str := Copy(S, i + 2, LenS - i - 2)
                  else
                    Str := Copy(S, i + 1, LenS - i - 1);

                  ArangeLineColWithStr(Str);

                  FExpectedChar := UniInternalDTDTerminator;
                  FErrorStr := IChar;

                  DoDTDError(dteInvalidEnd, FErrorStr);
                  Exit;
                end;

                Str := Copy(S, i + 1, TagPos - i - 1);

                FDTDParser.ErrorOptions := Self.ErrorOptions;
                DTDError := FDTDParser.ParseDTD(Str, ALine, ACol, FExpectedChar, FErrorStr, False);

                if DTDError = dteUserTerminated then
                  Exit;

                if DTDError <> dteNoError then
                begin
                  if (ALine > -1) and (ACol > -1) then
                  begin
                    if ALine = 0 then
                      FColumnPos := ACol + FLastTokenColumn - 1
                    else
                      FColumnPos := ACol;

                    FLinePos := ALine + FLastTokenLine;
                  end;

                  if Length(FErrorStr) = 0 then
                    FErrorStr := ' ';

                  DoDTDError(DTDError, FErrorStr);
                  Exit;
                end;

                Inc(i, Length(Str) + 1);

                ArangeLineColWithStr(Str + UniInternalDTDTerminator);
                if not NextChar then
                  Exit;

                if IChar <> UniTagTerminator then
                begin
                  PrevChar;

                  FExpectedChar := UniTagTerminator;
                  FErrorStr := IChar;

                  DoDTDError(dteInvalidEnd, FErrorStr);
                  Exit;
                end;

                ResetHolder;
              end;

              DoDTDEnd(DTDName, FLinePos, FColumnPos, CanContinue);
              if not CanContinue or FStopOnDTDEnd then Exit;

              NextToken := stkText;
              if FPath = '' then NextToken := stkDocumentTop;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
            end else
              ConcatChar(Str, IChar);
          end;
          stkPI:
          begin
            if IsWhiteSpace(IChar) then
            begin
              if FTokenCharCount = 1 then
              begin
                DoError(SCErrorWhiteSpaceNotAllowed, FToken, FToken, Str, -1, -1);
                Exit;
              end;

              FPI := Str;
              if not LoopWhileSpace then Break;

              NextToken := stkPIData;

              if not IsValidInstructionName(FPI) then
              begin
                DoInvalidReservedNameError(FPI);
                Exit;
              end;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
              ConcatChar(Str, IChar);
            end else
            if (IChar = UniInstructionTerminator) and (NxChar = UniTagTerminator) then
            begin
              if PrChar = UniInstructionStarter then
              begin
                ResetHolder;
                Str := Str + UniInstructionTerminator + UniInstructionStarter;

                DoError(SCErrorWrongElementStart, FToken, stkPIData, Str, -1, -1);
                Exit;
              end;

              if not IsValidInstructionName(Str) then
              begin
                DoInvalidReservedNameError(Str);
                Exit;
              end;

              if not (DoProcessingInstruction(Str, '', FLastTokenLine, FLastTokenColumn,
                CanContinue) and CanContinue) then
                Exit;

              NextToken := stkText;
              if FPath = '' then NextToken := stkDocumentTop;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
            end else
            if NxChar = UniInstructionTerminator then
            begin
              if not IsWhiteSpace(IChar) and (IChar <> UniInstructionStarter) then
                ConcatChar(Str, IChar);

              NextToken := stkPIData;

              FPI := Str;

              if not IsValidInstructionName(FPI) then
              begin
                DoInvalidReservedNameError(FPI);
                Exit;
              end;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
            end else
              ConcatChar(Str, IChar);
          end;
          stkPIData:
          begin
            if IChar = UniInstructionTerminator then
            begin
              if not NextChar then Break;

              if IChar = UniTagTerminator then
              begin
                if not (DoProcessingInstruction(FPI, Str, FLastTokenLine, FLastTokenColumn,
                  CanContinue) and CanContinue) then
                  Exit;

                NextToken := stkText;
                if FPath = '' then NextToken := stkDocumentTop;

                if not ChangeToken(NextToken, Str) then Exit;

                ResetHolder;
              end else
              begin
                if IsWhiteSpace(IChar) then
                  DoError(SCErrorWhiteSpaceNotAllowed, stkPIData, stkText, Str, -1, -1)
                else
                  DoError(SCErrorTagCloseExpected, stkPIData, stkText, Str, -1, -1);

                Exit;
              end;
            end else
              ConcatChar(Str, IChar);
          end;
          stkDocumentTop:
          begin
            if IsWhiteSpace(IChar) then
            begin
              ResetHolder;
              if not LoopWhileSpace then Break;
            end;

            if IChar = UniTagStarter then
            begin
              NextToken := stkTAG;

              if NxChar = UniInstructionStarter then
              begin
                if not NextChar then Break;

                ResetHolder;
                NextToken := stkPI;
              end;

              if not ChangeToken(NextToken, Str) then Exit;

              ResetHolder;
            end else
            begin
              ConcatChar(Str, IChar);
              DoError(SCErrorTextAtDocumentTop, FToken, stkText, Str, -1, -1);
              Exit;
            end;
          end;
        end;

        if not NextChar then
        begin
          if (FToken = stkAttribute) and (Str <> '') then
          begin
            FAttributeName := Str;
            FAttributeList := FAttributeList + Str + UniWidePathSeparator;

            DoAttributeName(Str, '', IChar, FLastTokenLine, FLastTokenColumn, CanContinue);
          end else
          if (FToken = stkTag) and (Str <> '') then
          begin
            DoStartElement(Str, FLastTokenLine, FLastTokenColumn, CanContinue);
            if CheckIfElmNameIsValid(Str) then
              SetPath(ArrangePath(Str, ValidPath, IsNextRoot, True));
          end;

          Break;
        end;
      end;

      if Length(FPath) > 0 then
      begin
        if (FToken = stkAttribute) and (Str <> '') then
          Str := DecodePath(FPath) + ', ' + Str
        else
          Str := DecodePath(FPath);

        DoError(SCErrorTagsNotClosed, FToken, stkDocumentTop, Str, -1, -1);
        Exit;
      end;
      
      if Length(FRoot) = 0 then
      begin
        DoError(SCErrorNoRootDefined, stkDocumentTop, stkDocumentTop, '', -1, -1);
        Exit;
      end;

      Result := true;

      NextToken := stkDocumentTop;
      ChangeToken(NextToken, Str);
    finally
      FProcessTime := GetTickCount - TkCnt;

      SetLength(FAttributeList, 0);
      SetLength(FAttributeName, 0);
      SetLength(FPI, 0);

      FToken := stkDocumentTop;

      if Result then
      begin
        FParseLevel := 0;
        SetPath('');
        
        FExpectedChar := UniWideNull;
        SetLength(FErrorStr, 0);

        SetLength(FPath, 0);
        SetLength(FElement, 0);
        SetLength(FRoot, 0);

        FLinePos := 0;
        FColumnPos := 0;
        FLastTokenLine := 0;
        FLastTokenColumn := 0;

        DoDocumentEnd(CanContinue);
      end;
    end;
  finally
    Dec(FParseCount);
  end;
end;

procedure TSCCustomSaxParser.DoToken(ReleasedToken, EnteredToken: TSCTokenKind;
  const ReleasedValue: WideString; var CanContinue: Boolean);
begin
  if Assigned(FOnToken) then FOnToken(Self, FLinePos, FColumnPos,
    ReleasedToken, EnteredToken, ReleasedValue, FPath, CanContinue);
end;

procedure TSCCustomSaxParser.DoEndElement(const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnEndElement) then
    FOnEndElement(Self, AName, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoStartElement(const AName: WideString; Line,
  Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnStartElement) then
    FOnStartElement(Self, AName, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoEndCData(const CData: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnEndCData) then
    FOnEndCData(Self, CData, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoStartCData(const CData: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnStartCData) then
    FOnStartCData(Self, CData, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoComment(const Chars: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnComment) then
    FOnComment(Self, Chars, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoAttributeStart(const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnAttributeStart) then
    FOnAttributeStart(Self, AName, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoAttributeName(const AName, Value: WideString;
  Delimiter: WideChar; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnAttributeName) then
    FOnAttributeName(Self, AName, Value, Delimiter, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoAttributeValueStart(const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnAttributeValueStart) then
    FOnAttributeValueStart(Self, AName, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoAttributeValue(const AName, Value: WideString;
  Delimiter: WideChar; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnAttributeValue) then
    FOnAttributeValue(Self, AName, Value, Delimiter, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoText(const Data: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnText) then
    FOnText(Self, Data, Line, Column, CanContinue);
end;

function TSCCustomSaxParser.DoProcessingInstruction(const Target,
  Data: WideString; Line, Column: LongInt; var CanContinue: Boolean): Boolean;
var
  ErrLine, ErrCol: LongInt;
  XmlInstError: TSCXmlInstructionError;
  HasEncoding, HasVersion, HasStandalone: Boolean;
  AEncoding, AVersion, AStandalone: WideString;
begin
  Result := True;
  CanContinue := True;

  if Target = UniWideXmlInstruction then
  begin
    SetLength(AEncoding, 0);
    SetLength(AVersion, 0);
    SetLength(AStandalone, 0);

    XmlInstError := ParseXmlInstruction(Data, AEncoding, AVersion, AStandalone,
      HasEncoding, HasVersion, HasStandalone, ErrLine, ErrCol);

    Result := XmlInstError = xieNoError;
    if not Result then
    begin
      if (ErrLine > -1) and (ErrCol > -1) then
      begin
        if ErrLine = 0 then Inc(ErrCol, FLastTokenColumn - 1);
        Inc(ErrLine, FLastTokenLine);
      end;

      DoError(ConvertXmlInstError(XmlInstError), stkPIData, stkPIData, Data, ErrLine, ErrCol);
      Exit;
    end;

    if HasVersion then Version := AVersion;
    
    if (FLastError = 0) and HasEncoding then
      Encoding := AEncoding;

    if (FLastError = 0) and HasStandalone then
      Standalone := AStandalone = WideString(UniYes);
  end;

  if (FLastError = 0) and Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(Self, Target, Data, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoResolveEntity(const Entity: WideString;
  var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnResolveEntity) then FOnResolveEntity(Self, Entity, CanContinue);
end;

function TSCCustomSaxParser.GetLastError: TSCXMLParseError;
begin
  with Result do
  begin
    Error := FLastError;
    Line  := FErrorLine;
    Column := FErrorColumn;
  end;
end;

function TSCCustomSaxParser.ErrorDescription(No: Integer): String;
var
  I: Integer;
begin
  Result := SCRes_ErrorUndefined;

  case No of
    SCErrorTagsNotClosed:
    begin
      if WideChar(FPath[1]) = UniWidePathSeparator then
        Result := Copy(FPath, 2, Length(FPath)-1)
      else
        Result := Copy(FPath, 1, Length(FPath));

      if Result = '' then
        Result := SCRes_ErrorTagsNotClosed_2
      else begin
        Result := SC_WideStringReplace(Result, UniWidePathSeparator, ', ', True);
        Result := Format(SCRes_ErrorTagsNotClosed, [Result]);
      end;

      Exit;
    end;
    SCElementNameStartError:
    begin
      Result := Format(SCRes_ErrorElementNameStarts, [UniInvalidElementFirstChars]);
      Exit;
    end;
    SCElementNameError:
    begin
      Result := Format(SCRes_ErrorElementNameContains, [UniInvalidElementChars]);
      Exit;
    end;
    SCInvalidElementNameError:
    begin
      Result := Format(SCRes_InvalidElementName, [UniInvalidElementFirstChars,
        UniInvalidElementChars]);
      Exit;
    end;
    SCInvalidAttributeValue:
    begin
      Result := Format(SCRes_InvalidAttributeValue, [UniInvalidAttributeValueChars]);
      Exit;
    end;
    SCInvalidCharForToken:
    begin
      Result := Format(SCRes_InvalidCharForToken, [AnsiLowerCase(SCTokenAsString(FToken))]);
      Exit;
    end;
    SCInvalidCharacter:
    begin
      Result := Format(SCRes_InvalidCharacter, [WideString(FErrorChar), AnsiLowerCase(SCTokenAsString(FToken))]);
      Exit;
    end;
    SCReservedNameMustBeLowerCase:
    begin
      Result := Format(SCRes_ReservedNameMustBeLowerCase, [FPI]);
      Exit;
    end;
    SCInvalidExpectedCharacter:
    begin
      Result := Format(SCRes_InvalidExpectedCharacter, [WideString(FExpectedChar)]);
      Exit;
    end;
    SCDuplicateDTDElement:
    begin
      Result := SCRes_DuplicateDTDElementWithoutName;
      if FErrorStr <> '' then
        Result := Format(SCRes_DuplicateDTDElement, [WideString(FErrorStr)]);
        
      Exit;
    end;
    SCUndefinedDTDElement:
    begin
      Result := Format(SCRes_UndefinedDTDElement, [WideString(FErrorStr)]);
      Exit;
    end;
  end;

  for I := Low(SCXMLErrors) to High(SCXMLErrors) do
    if SCXMLErrors[I].No = No then
    begin
      Result := SCXMLErrors[I].Desc;
      Exit;
    end;
end;

procedure TSCCustomSaxParser.DoEmptyDocumentError;
begin
  SetLength(FPath, 0);
  SetLength(FElement, 0);
  SetLength(FRoot, 0);

  FToken := stkDocumentTop;
  FLinePos := 1;
  FColumnPos := 0;

  FLastTokenLine := 1;
  FLastTokenColumn := 0;

  DoError(SCErrorEmptyDocument, stkDocumentTop, stkDocumentTop, '', -1, -1);
end;

procedure TSCCustomSaxParser.DoNonUnicodeDocumentError;
begin
  SetLength(FPath, 0);
  SetLength(FElement, 0);
  SetLength(FRoot, 0);

  FToken := stkDocumentTop;
  FLinePos := 1;
  FColumnPos := 0;
  FLastTokenLine := 1;
  FLastTokenColumn := 0;

  DoError(SCErrorNonUnicodeDocument, stkDocumentTop, stkDocumentTop, '', -1, -1);
end;

function TSCCustomSaxParser.IsValidCharForToken(const Chr: WideChar;
  InToken: TSCTokenKind; Position: LongInt): Boolean;
begin
  Result := True;
  case InToken of
    stkAttribute:
    begin
      if Position <= 1 then
        Result := Pos(Chr, UniInvalidAttributeFirstChars) = 0
      else Result := Pos(Chr, UniInvalidAttributeChars) = 0;
    end;
    stkTag:
    begin
      if Position <= 1 then
        Result := Pos(Chr, UniInvalidTagFirstChars) = 0
      else Result := Pos(Chr, UniInvalidTagChars) = 0;
    end;
  end;
end;

procedure TSCCustomSaxParser.DoInvalidReservedNameError(const AName: WideString);
begin
  DoError(SCReservedNameMustBeLowerCase, FToken, FToken, AName, -1, -1);
end;

procedure TSCCustomSaxParser.DoInvalidNameError(const AName: WideString);
begin
  DoError(SCInvalidElementNameError, FToken, FToken, AName, -1, -1);
end;

procedure TSCCustomSaxParser.DoInvalidCharError(const AChar: WideChar);
begin
  DoError(SCInvalidCharForToken, FToken, FToken, AChar, -1, -1);
end;

destructor TSCCustomSaxParser.Destroy;
begin
  FreeAndNil(FEscapeParser);
  FreeAndNil(FDTDParser);
  inherited Destroy;
end;

function TSCCustomSaxParser.GetPathInfos(const S: WideString): TSCPathInfo;
begin
  try
    Result := TSCPathInfo.Create(nil);
    FPathInfo := Result;

    try
      ParseString(S);
    except
      if Result <> nil then FreeAndNil(Result);
      raise;
    end;
  finally
    FPathInfo := nil;
  end;
end;

procedure TSCCustomSaxParser.SetPath(const APath: Widestring);
begin
  FPath := APath;
end;

procedure TSCCustomSaxParser.DoPathInfo(const APath, CurTag: WideString;
  IsNew, TagClosedInTag: Boolean);
begin
  if FPathInfo = nil then exit;

  if not IsNew then
  begin
    FTagLine := FLinePos;
    FTagCol := FColumnPos;

    if TagClosedInTag then Inc(FTagCol);

    with FPathInfo.FCursor do
    begin
      if not TagClosed then
      begin
        FEndLine := FTagLine;
        FEndColumn := FTagCol;

        if Level > 0 then FTagClosed := true;
      end;
    end;

    FPathInfo.GoUp;

    with FPathInfo.FCursor do
      if not TagClosed then
      begin
        FEndLine := FTagLine;
        FEndColumn := FTagCol;
      end;
  end else
  begin
    if Length(APath) = 0 then exit;

    if FTagCol > 1 then Dec(FTagCol, 2)
    else if FTagCol > 0 then Dec(FTagCol);

    if FPathInfo.FCursor = nil then FPathInfo.FCursor := FPathInfo;

    if (FPathInfo.Count = 0) and (FPathInfo.Name = '') then
      with FPathInfo do
      begin
        FName := APath;
        FStartLine := FTagLine;
        FStartColumn := FTagCol;

        FEndLine := FLinePos;
        FEndColumn := FColumnPos;

        exit;
      end;

    if FPathInfo.FCursor.Count > 0 then
      with FPathInfo.FCursor.Items[FPathInfo.FCursor.Count-1] do
        if not TagClosed then
        begin
          FEndLine := FTagLine;
          FEndColumn := FTagCol;

          if Level > 0 then FTagClosed := true;
        end;

    with FPathInfo.FCursor.AddItem do
    begin
      FName := APath;
      FStartLine := FTagLine;
      FStartColumn := FTagCol;

      FEndLine := FLinePos;
      FEndColumn := FColumnPos;
    end;
  end;
end;

procedure TSCCustomSaxParser.SetEncoding(const AEncoding: Widestring);
begin
  if FEncoding <> AEncoding then
  begin
    if (FEncoding <> '') and IsParsing and
      not CanSwitchToEncoding(FEncoding, AEncoding) then
    begin
      DoSwitchEncodingError(AEncoding);
      Exit;
    end;

    if (IsParsing or (AEncoding <> '')) and not IsValidEncoding(AEncoding) then
    begin
      DoUnsupportedEncodingError(AEncoding);
      Exit;
    end;

    FEncoding := AEncoding;
  end;
end;

procedure TSCCustomSaxParser.SetVersion(const AVersion: Widestring);
begin
  if FVersion <> AVersion then
  begin
    if not IsValidVersionNumber(AVersion) then
    begin
      DoError(SCInvalidVersionNumber, stkPIData, stkPIData, AVersion, -1, -1);
      Exit;
    end;

    FVersion := AVersion;
  end;
end;

procedure TSCCustomSaxParser.DoSwitchEncodingError(const AEncoding: WideString);
begin
  DoError(SCCannotSwitchEncoding, FToken, FToken, AEncoding, -1, -1);
end;

procedure TSCCustomSaxParser.DoSwitchVersionError(const AVersion: WideString);
begin
  DoError(SCCannotSwitchVersion, FToken, FToken, AVersion, -1, -1);
end;

procedure TSCCustomSaxParser.DoUnsupportedEncodingError(const AEncoding: WideString);
begin
  DoError(SCUnsupportedEncoding, FToken, FToken, AEncoding, -1, -1);
end;

procedure TSCCustomSaxParser.DetectEncoding(const S: WideString);
begin
  if FEncodingRequested then Exit;

  try
    FEncodingRequested := True;
    ParseString(S);
  finally
    FEncodingRequested := False;
  end;
end;

function TSCCustomSaxParser.IsParsing: Boolean;
begin
  Result := FParseCount > 0;
end;

procedure TSCCustomSaxParser.DoDTDEnd(const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDEnd) then
    FOnDTDEnd(Self, AName, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoDTDEntity(Sender: TObject; const AName,
  Data: WideString; Delimiter: WideChar; Line, Column: LongInt;
  var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDEntity) then
    FOnDTDEntity(Self, AName, Data, Delimiter, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoDTDNotation(Sender: TObject; const AName,
  PublicID, SystemID: WideString; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDNotation) then
    FOnDTDNotation(Self, AName, PublicID, SystemID, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoEscapeError(E: TSCEscapeError; const Str: WideString);
begin
  case E of
    escNeedEscapeClose:
      DoError(SCErrorNeedEscapeClose, FToken, FToken, Str, -1, -1);
    escUndefinedEscape:
      DoError(SCErrorUndefinedEscape, FToken, FToken, Str, -1, -1);
    escInvalidCharacter:
      DoError(SCInvalidCharacter, FToken, FToken, Str, -1, -1);
    escInvalidUnicodeCharacter:
      DoError(SCInvalidUnicodeCharacter, FToken, FToken, Str, -1, -1);
    escInvalidNameStart:
      DoError(SCErrorWrongElementStart, FToken, FToken, Str, -1, -1);
  end;
end;

procedure TSCCustomSaxParser.DoDTDError(E: TSCDTDError; const Str: WideString);
var
  ErrorCode: Integer;
begin
  ErrorCode := 0;
  case E of
    dteExpectedCharacterNotFound:
      ErrorCode := SCExpectedCharacterNotFound;
    dteInvalidDTDName:
      ErrorCode := SCInvalidDTDName;
    dteInvalidCharacter:
      ErrorCode := SCInvalidDTDCharacter;
    dteInvalidLocationType:
      ErrorCode := SCInvalidDTDLocation;
    dteInvalidPublicID:
      ErrorCode := SCInvalidDTDPublicValue;
    dteInvalidSystemID:
      ErrorCode := SCInvalidDTDSystemValue;
    dteInvalidParameter:
      ErrorCode := SCInvalidDTDParameter;
    dteInvalidDefination:
      ErrorCode := SCInvalidDTDDefination;
    dteInvalidEnd:
      ErrorCode := SCInvalidExpectedCharacter;
    dtdXMLElementFound:
      ErrorCode := SCInvalidDTDXMLElement;
    dteSpaceRequired:
      ErrorCode := SCMissingWhiteSpace;
    dteInvalidDeclaration:
      ErrorCode := SCInvalidDTDDeclarationName;
    dteInvalidName:
      ErrorCode := SCInvalidDTDElementName;
    dteNameExpected:
      ErrorCode := SCDTDElementNameExpected;
    dteInvalidContentModel:
      ErrorCode := SCInvalidDTDContentModel;
    dteInvalidExternalID:
      ErrorCode := SCInvalidExternalID;
    dteInvalidAttributeType:
      ErrorCode := SCInvalidAttributeType;
    dteInvalidAttributeDefault:
      ErrorCode := SCInvalidAttributeDefault;
    dteInvalidAttributeDefaultValue:
      ErrorCode := SCInvalidAttributeValue;
    dteDuplicateElement:
      ErrorCode := SCDuplicateDTDElement;
    dteCommentNotClosed:
      ErrorCode := SCCommentNotClosed;
    dteCommentHasTermination:
      ErrorCode := SCErrorCommentHasTermination;
    dteInvalidNameStart:
      ErrorCode := SCErrorWrongElementStart;
  end;

  if ErrorCode <> 0 then
    DoError(ErrorCode, FToken, FToken, Str, -1, -1);
end;

procedure TSCCustomSaxParser.DoDTDStart(const AName, PublicID,
  SystemID: WideString; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDStart) then FOnDTDStart(Self, AName, PublicID,
    SystemID, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoDTDFileStart(const AName, PublicID, SystemID: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDFileStart) then FOnDTDFileStart(Self, AName, PublicID,
    SystemID, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoDTDAttribute(Sender: TObject;
  const Element, AName: WideString; AttributeType: TSCDTDAttributeType;
  AttributeDefault: TSCDTDAttributeDefault; const EnumValues,
  DefaultValue: WideString; Delimiter: WideChar; Line, Column: LongInt;
  var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDAttribute) then
    FOnDTDAttribute(Self, Element, AName, AttributeType, AttributeDefault,
      EnumValues, DefaultValue, Delimiter, Line, Column, CanContinue);
end;

procedure TSCCustomSaxParser.DoDTDElement(Sender: TObject; const AName,
  Child: WideString; ElementContentType: TSCDTDElementContentType;
  Occurrence: TSCDTDElementOccurrence; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDElement) then FOnDTDElement(Self, AName, Child,
    ElementContentType, Occurrence, Line, Column, CanContinue);
end;

type
  TDTDNameParseState = (dtdNameStarted, dtdInName, dtdInLocationType, dtdInPublicID, dtdInSystemID);

function TSCCustomSaxParser.ParseDTDName(const DTD: WideString; var Name,
  PublicID, SystemID: WideString; var ALine, ACol: LongInt): TSCDTDError;
var
  LocateType: WideString;
  St: TDTDNameParseState;
  AttrValueDel, IChar, PrChar, NxChar: WideChar;
  I, PrevLine, PrevCol, Ln: Integer;

  procedure ArrangeLineCol;
  begin
    if IChar = UniWideLF then
    begin
      PrevCol := ACol;
      ACol := 0;
    end else
      if IChar = UniWideCR then
      begin
        PrevCol := ACol;
        PrevLine := ALine;

        ACol := 0;
        Inc(ALine);
      end else
      begin
        PrevCol := ACol;
        Inc(ACol);
      end;
  end;

  function NextChar: boolean;
  begin
    PrChar := IChar;
    NxChar := UniWideNull;

    Inc(i);
    Result := i <= Ln;
    if not result then exit;

    IChar := DTD[i];
    ArrangeLineCol;

    if i + 1 <= Ln then NxChar := DTD[i + 1];
  end;

  function PrevChar: boolean;
  begin
    NxChar := IChar;
    IChar  := PrChar;
    PrChar := UniWideNull;

    if PrevCol <> ACol - 1 then ALine := PrevLine;
    ACol := PrevCol;

    Dec(i);
    Result := i > 0;
    if i > 1 then PrChar := DTD[i-1];
  end;

  function LoopWhileSpace: boolean;
  begin
    while i <= Ln do
    begin
      if not IsWhiteSpace(IChar) then Break;

      Result := NextChar;
      if not result then exit;
    end;

    Result := i <= Ln;
  end;

begin
  Result := dteNoError;

  ALine := 0;
  ACol  := 0;

  IChar := UniWideNull;

  PrevLine := 0;
  PrevCol  := 0;

  SetLength(Name, 0);
  SetLength(PublicID, 0);
  SetLength(SystemID, 0);

  try
    Ln := Length(DTD);

    St := dtdNameStarted;
    SetLength(LocateType, 0);

    I := 0;

    if not NextChar or (IsWhiteSpace(IChar) and not LoopWhileSpace) then
      Exit;

    AttrValueDel := UniWideNull;
       
    while I < Ln do
    begin
      if (St <> dtdNameStarted) and not NextChar then
        Exit;

      case St of
        dtdNameStarted:
        begin
          St   := dtdInName;
          Name := IChar;

          Continue;
        end;
        dtdInName:
        begin
          if IsWhiteSpace(IChar) then
          begin
            if not IsValidElementName(Name) then
            begin
              Result := dteInvalidDTDName;
              Exit;
            end;

            if not LoopWhileSpace then
              Exit;

            St := dtdInLocationType;
            LocateType := LocateType + IChar;
          end else
            Name := Name + IChar;
        end;
        dtdInLocationType:
        begin
          if IsWhiteSpace(IChar) then
          begin
            if not LoopWhileSpace then
            begin
              Result := dteInvalidLocationType;
              Exit;
            end;

            if LocateType = UniDTDPublicStarter then
            begin
              if (Length(PublicID) > 0) or (Length(SystemID) > 0) or
                ((IChar <> UniValueStarter) and (IChar <> UniValueStarter_2)) then
              begin
                Result := dteInvalidPublicID;
                Exit;
              end;

              AttrValueDel := IChar;
              St := dtdInPublicID;
            end else
            if LocateType = UniDTDSystemStarter then
            begin
              if (Length(SystemID) > 0) or
                ((IChar <> UniValueStarter) and (IChar <> UniValueStarter_2)) then
              begin
                Result := dteInvalidSystemID;
                Exit;
              end;

              AttrValueDel := IChar;
              St := dtdInSystemID;
            end else
            begin
              Result := dteInvalidLocationType;
              Exit;
            end;

            SetLength(LocateType, 0);
          end else
            LocateType := LocateType + IChar;
        end;
        dtdInPublicID:
        begin
          if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
          begin
            if Length(PublicID) = 0 then
            begin
              Result := dteInvalidPublicID;
              Exit;
            end;

            if NextChar and not IsWhiteSpace(IChar) then
            begin
              Result := dteInvalidParameter;
              Exit;
            end;

            if not LoopWhileSpace then
              Exit;

            if (AttrValueDel <> UniWideNull) and (IChar <> AttrValueDel) then
            begin
              FExpectedChar := AttrValueDel;

              Result := dteInvalidDefination;
              Exit;
            end;

            AttrValueDel := UniWideNull;
            St := dtdInSystemID;
          end else
            PublicID := PublicID + IChar;
        end;
        dtdInSystemID:
        begin
          if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
          begin
            AttrValueDel := UniWideNull;

            if Length(SystemID) = 0 then
            begin
              Result := dteInvalidSystemID;
              Exit;
            end;

            if NextChar and not IsWhiteSpace(IChar) then
            begin
              Result := dteInvalidParameter;
              Exit;
            end;

            if not LoopWhileSpace then
              Exit;

            if i < Ln then
            begin
              Result := dteInvalidParameter;
              Exit;
            end;
          end else
            SystemID := SystemID + IChar;
        end;
      end;
    end;
  finally
    if (Result = dteNoError) and (Length(Name) = 0) then
      Result := dteInvalidDTDName;
  end;
end;

function TSCCustomSaxParser.ParseForDTD(const S: WideString): Boolean;
begin
  FStopOnDTDEnd := True;
  try
    Result := ParseString(S); 
  finally
    FStopOnDTDEnd := False;
  end;
end;

procedure TSCCustomSaxParser.DoDTDComment(Sender: TObject;
  const Data: WideString; Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDComment) then
    FOnDTDComment(Self, Data, Line, Column, CanContinue);
end;

{ TSCDomItem }

procedure TSCDomItem.AfterConstruction;
begin
  inherited AfterConstruction;
  SetLength(FName, 0);
end;

procedure TSCDomItem.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
  if FOwner <> nil then FOwner.ItemDestroyed(Self);
end;

procedure TSCDomItem.CheckIfNameIsValid(const AName: WideString; AllowEmpty: Boolean);
var
  Chr: WideChar;
  I, Ln: LongInt;
  Root: TSCDomDocument;
begin
  Ln := Length(AName);
  if (Ln = 0) and not AllowEmpty then
  begin
    Root := GetDocument;
    if Root <> nil then
       Root.DoInternalError(Self, SCBlankElementNameError, '')
    else
      raise Exception.Create(SCRes_ErrorBlankElementName);

    Exit;
  end;

  I := 1;
  while I <= Ln do
  begin
    Chr := AName[I];

    if (I = 1) and (Pos(Chr, UniInvalidElementFirstChars) > 0) then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCElementNameStartError, '')
      else
        raise Exception.CreateFmt(SCRes_ErrorElementNameStarts, [UniInvalidElementFirstChars]);

      Exit;
    end;

    if SC_IsCharWhiteSpace(Chr) or (Pos(Chr, UniInvalidElementChars) > 0) then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCElementNameError, '')
      else
        raise Exception.CreateFmt(SCRes_ErrorElementNameContains, [UniInvalidElementChars]);

      Exit;
    end;

    Inc(I);
  end;
end;

procedure TSCDomItem.Clear;
begin
  //
end;

constructor TSCDomItem.Create(AOwner: TSCDomItem);
begin
  inherited Create;
  FOwner := AOwner;

  FToken := stkDocumentTop;
  FErrorLinePos := 0;
  FErrorColumnPos := 0;
  FErrorChar := UniWideNull;
  FLine := -1;
  FColumn := -1;
end;

procedure TSCDomItem.DoError(ErrorCode: Integer; TokenKind: TSCTokenKind;
  const Value: WideString);
var
  ErrorText: string;
begin
  ErrorText := SCErrorDescription(ErrorCode, TokenKind, FErrorLinePos,
    FErrorColumnPos, FErrorChar, Value);
  raise Exception.CreateFmt(SCRes_ParseError, [ErrorText, FErrorLinePos, FErrorColumnPos]);
end;

function TSCDomItem.ExtractItem(AItem: TSCDomItem): Boolean;
begin
  Result := True;
end;

function TSCDomItem.GetDocument: TSCDomDocument;
var
  AOwner: TSCDomItem;
begin
  Result := nil;

  AOwner := FOwner;
  while AOwner <> nil do
  begin
    if AOwner is TSCDomDocument then
    begin
      Result := TSCDomDocument(AOwner);
      Exit;
    end;

    AOwner := AOwner.Owner;
  end;
end;

function TSCDomItem.GetLevel: Integer;
begin
  Result := 0;
  if Owner <> nil then Result := Owner.Level + 1;
end;

procedure TSCDomItem.ItemDestroyed(AItem: TSCDomItem);
begin
  ExtractItem(AItem);
end;

procedure TSCDomItem.NormalizeTab(TabSpace: Integer);
begin
  //
end;

procedure TSCDomItem.ResetName(const Newvalue: WideString);
begin
  ValidateName(Newvalue);
  FName := Newvalue;
end;

procedure TSCDomItem.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
begin
  // no item saving
end;

procedure TSCDomItem.SetName(const Newvalue: WideString);
begin
  ResetName(Newvalue);
end;

procedure TSCDomItem.SetToken(Value: TSCTokenKind);
begin
  FToken := Value;
end;

function TSCDomItem.ToString(Indent: Word): WideString;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream, Indent, False, True);

    Stream.Position := 0;

    SetLength(Result, Stream.Size div 2);
    Stream.Read(PWideChar(Result)^, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TSCDomItem.ValidateName(const AName: WideString);
begin
  CheckIfNameIsValid(AName);
end;

{ TSCDomDocument }

function TSCDomDocument.AddText(const Data: WideString): TSCDomText;
begin
  raise Exception.Create(SCRes_ErrorTextAtDocumentTop);
end;

function TSCDomDocument.AddText(AText: TSCDomText): Boolean;
begin
  raise Exception.Create(SCRes_ErrorTextAtDocumentTop);
end;

constructor TSCDomDocument.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FName := 'DOM';
  FIndent := 4;
  FDTD := TSCDomDTD.Create(Self);
  FStandalone := False;
  FRaiseError := True;

  SetLength(FEncoding, 0);
  SetLength(FVersion, 0);
end;

destructor TSCDomDocument.Destroy;
begin
  FreeAndNil(FDTD);
  inherited Destroy;
end;

procedure TSCDomDocument.DoInternalError(Sender: TObject; ErrorCode: Integer;
  const ErrorValue: WideString);
var
  ErrValue: WideString;
begin
  if FRaiseError then
  begin
    ErrValue := ErrorValue;
    if (ErrValue <> '') and (ErrValue[1] <> UniSpace) then
      ErrValue := UniSpace + ErrValue;

    raise Exception.CreateFmt(SCRes_Error, [SCErrorDescription(ErrorCode,
      stkAttribute, -1, -1, UniWideNull, ''), ErrValue]);
  end;
end;

function TSCDomDocument.GetRoot: TSCDomElement;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ChildNodeCount-1 do
    if ChildNodes[I] is TSCDomElement then
    begin
      Result := TSCDomElement(ChildNodes[I]);
      Break;
    end;
end;

procedure TSCDomDocument.Save;
begin
  SaveToFile(FFileName);
end;

procedure TSCDomDocument.SaveToFile(AFile: String);
var
  BOM: WideString;
  Stream: TFileStream;
begin
  if FFileName = '' then FFileName := AFile;

  if FFileName = '' then
    raise Exception.Create(SCRes_EmptyDocumentName);

  Stream := TFileStream.Create(AFile, fmCreate);
  try
    BOM := UniBOM;
    Stream.WriteBuffer(PWideChar(BOM)^, 2);

    SaveToStream(Stream, FIndent, False, True);
  finally
    Stream.Free;
  end;
end;

procedure TSCDomDocument.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  I: Integer;
  Buf: WideString;
begin
  for I := 0 to FNodes.Count-1 do
    if TObject(FNodes[I]) is TSCDomProcessInst then
      TSCDomTreeNode(FNodes[I]).SaveToStream(AStream, Indent,
        Normalized, PrettyTrim);

  if FStandalone or (Length(FVersion) > 0) or (Length(FEncoding) > 0) then
  begin
    SetLength(Buf, 0);
    Buf := Buf + UniTagStarter + UniInstructionStarter + UniWideXmlInstruction;

    if Length(FVersion) > 0 then
      Buf := Buf + UniAttrStarter + UniVersion + UniAttrTerminator +
        UniValueStarter + FVersion + UniValueTerminator;

    if Length(FEncoding) > 0 then
      Buf := Buf + UniAttrStarter + UniEncoding + UniAttrTerminator +
        UniValueStarter + FEncoding + UniValueTerminator;

    if FStandalone then
      Buf := Buf + UniAttrStarter + UniStandalone + UniAttrTerminator +
        UniValueStarter + UniYes + UniValueTerminator;

    Buf := Buf + UniInstructionTerminator + UniTagTerminator + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end;

  if (Length(FDTD.FPublicId) > 0) or
    (Length(FDTD.FSystemId) > 0) or (FDTD.FItems.Count > 0) then
  begin
    FDTD.SaveToStream(AStream, Indent, Normalized, PrettyTrim);

    Buf := UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end;

  for I := 0 to FNodes.Count-1 do
    if not (TObject(FNodes[I]) is TSCDomProcessInst) then
      TSCDomTreeNode(FNodes[I]).SaveToStream(AStream, Indent,
        Normalized, PrettyTrim);
end;

procedure TSCDomDocument.SetEncoding(const AEncoding: Widestring);
begin
  if FEncoding <> AEncoding then
  begin
    if (FEncoding <> '') and
      not CanSwitchToEncoding(FEncoding, AEncoding) then
    begin
      DoError(SCCannotSwitchEncoding, stkPIData, AEncoding);
      Exit;
    end;

    if not IsValidEncoding(AEncoding) then
    begin
      DoError(SCUnsupportedEncoding, stkPIData, AEncoding);
      Exit;
    end;

    FEncoding := AEncoding;
  end;
end;

procedure TSCDomDocument.SetIndent(Newvalue: Word);
begin
  FIndent := Newvalue;
end;

procedure TSCDomDocument.SetName(const Newvalue: WideString);
begin
  // Document element can not have a name
end;

procedure TSCDomDocument.SetVersion(const AVersion: Widestring);
begin
  if FVersion <> AVersion then
  begin
    if not IsValidVersionNumber(AVersion) then
    begin
      DoError(SCInvalidVersionNumber, stkPIData, AVersion);
      Exit;
    end;

    FVersion := AVersion;
  end;
end;

procedure TSCDomDocument.ValidateDocument;
begin
  if FDTD.FItems.Count > 0 then
  begin
    FDTD.FInternalValidation := FInternalValidation;
    try
      FDTD.ValidateDTD;
    finally
      FDTD.FInternalValidation := FInternalValidation;
    end;
  end;
end;

procedure TSCDomDocument.ValidateName(const AName: WideString);
begin
  // no name validation
end;

{ TSCDomAttribute }

procedure TSCDomAttribute.AfterConstruction;
begin
  inherited AfterConstruction;
  SetLength(FValue, 0);
end;

procedure TSCDomAttribute.Clear;
begin
  inherited Clear;
  SetValue('');
end;

constructor TSCDomAttribute.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FEscapeParser := TSCEscapeParser.Create;
  FToken := stkAttribute;
  FDelimiter := UniValueStarter;
end;

destructor TSCDomAttribute.Destroy;
begin
  if FOwner is TSCDomElement then TSCDomElement(FOwner).ExtractAttribute(Self);
  FreeAndNil(FEscapeParser);
  inherited Destroy;
end;

function TSCDomAttribute.GetLevel: Integer;
begin
  Result := 0;
  if FOwner <> nil then Result := FOwner.Level;
end;

procedure TSCDomAttribute.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  Ln: LongInt;
  C: WideChar;
  Buf: WideString;
begin
  Ln := Length(FValue);
  SetLength(Buf, Ln);
  System.Move(Pointer(FValue)^, Pointer(Buf)^, 2*Ln);

  if Normalized then Buf := SC_WideTrim(Buf)
  else if PrettyTrim then
    Buf := SC_WidePretyTrim(Buf);

  C := FDelimiter;
  if (C <> UniValueStarter) and (C <> UniValueStarter_2) then
    C := UniValueStarter;

  Buf := UniWideSpace + FName + UniAttrTerminator + C + Buf + C;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);
end;

procedure TSCDomAttribute.SetDelimiter(const Newvalue: WideChar);
begin
  FDelimiter := Newvalue;
  if (FDelimiter <> UniValueStarter) and (FDelimiter <> UniValueStarter_2) then
    FDelimiter := UniValueStarter;
end;

procedure TSCDomAttribute.SetValue(const Newvalue: WideString);
begin
  ValidateValue(Newvalue);
  FValue := Newvalue;
end;

procedure TSCDomAttribute.ValidateValue(const AValue: Widestring);
var
  S: WideString;
  Root: TSCDomDocument;
  ErrorCode: Integer;
  ALine, ACol: LongInt;
  EscapeError: TSCEscapeError;
begin
  S := UniInvalidAttributeValueChars;
  if FDelimiter = UniValueStarter then
    S := S + UniValueStarter
  else
    S := S + UniValueStarter_2;

  if not FEscapeParser.IsValidData(AValue, ALine, ACol, FErrorChar, EscapeError, S) then
  begin
    S := AValue;

    ErrorCode := 0;
    case EscapeError of
      escNeedEscapeClose:
        ErrorCode := SCErrorNeedEscapeClose;
      escUndefinedEscape:
        ErrorCode := SCErrorUndefinedEscape;
      escInvalidCharacter:
      begin
        S := FErrorChar;
        ErrorCode := SCInvalidCharacter;
      end;
      escInvalidUnicodeCharacter:
      begin
        S := FErrorChar;
        ErrorCode := SCInvalidUnicodeCharacter;
      end;
      escInvalidNameStart:
      begin
        S := FErrorChar;
        ErrorCode := SCErrorWrongElementStart;
      end;
    end;

    if ErrorCode <> 0 then
    begin
      Root := GetDocument;
      if Root <> nil then
        Root.DoInternalError(Self, ErrorCode, S)
      else
        DoError(ErrorCode, stkAttributeValue, AValue);
    end;    
  end;
end;

{ TSCDomElement }

function TSCDomElement.AddAttribute(const AName, AValue: WideString; Delimiter: WideChar): TSCDomAttribute;
var
  C: WideChar;
  Root: TSCDomDocument;
begin
  Result := nil;
  try
    CheckIfNameIsValid(AName, False);

    if AttributeByName(AName) <> nil then
    begin
      Root := GetDocument;
      if Root <> nil then
        Root.DoInternalError(Self, SCErrorDuplicateAttr, '')
      else
        raise Exception.Create(SCRes_ErrorDuplicateAttr);

      Exit;
    end;

    Result := TSCDomAttribute.Create(Self);

    C := Delimiter;
    if (C <> UniValueStarter) and (C <> UniValueStarter_2) then
      C := UniValueStarter;

    if Result <> nil then
    begin
      Result.Name := AName;
      Result.Delimiter := C;
      Result.Value := AValue;

      FAttributes.Add(Result);
    end;
  except
    if Result <> nil then FreeAndNil(Result);
    raise;
  end;
end;

function TSCDomElement.AddAttribute(AAttribute: TSCDomAttribute): Boolean;
var
  Root: TSCDomDocument;
begin
  Result := False;
  if (AAttribute <> nil) and (IndexOfAttribute(AAttribute) = -1) then
  begin
    CheckIfNameIsValid(AAttribute.Name, False);

    if AttributeByName(AAttribute.Name) <> nil then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCErrorDuplicateAttr, '')
      else
        raise Exception.Create(SCRes_ErrorDuplicateAttr);

      Exit;
    end;

    if AAttribute.FOwner <> nil then
      AAttribute.FOwner.ExtractItem(AAttribute);

    Result := FAttributes.Add(AAttribute) > -1;
    if Result then
      AAttribute.FOwner := Self;
  end;
end;

function TSCDomElement.AddElement(const AName: WideString): TSCDomElement;
begin
  Result := nil;
  try
    if AName <> UniWideNull then Result := TSCDomElement.Create(Self);
    if Result <> nil then Result.Name := AName;

    FNodes.Add(Result);
  except
    if Result <> nil then FreeAndNil(Result);
  end;
end;

function TSCDomElement.AddComment(AComment: TSCDomComment): Boolean;
begin
  Result := False;

  if (AComment <> nil) and (IndexOfNode(AComment) = -1) then
  begin
    if AComment.FOwner <> nil then
      AComment.FOwner.ExtractItem(AComment);

    Result := FNodes.Add(AComment) > -1;
    if Result then
      AComment.FOwner := Self;
  end;
end;

function TSCDomElement.AddComment(const Data: WideString): TSCDomComment;
begin
  Result := nil;
  try
    Result := TSCDomComment.Create(Self);
    if Result <> nil then Result.Text := Data;

    FNodes.Add(Result);
  except
    if Result <> nil then FreeAndNil(Result);
  end;
end;

function TSCDomElement.AddElement(AElement: TSCDomElement): Boolean;
begin
  Result := False;
  if (AElement <> nil) and (IndexOfNode(AElement) = -1) then
  begin
    if AElement.FOwner <> nil then
      AElement.FOwner.ExtractItem(AElement);

    Result := FNodes.Add(AElement) > -1;
    if Result then AElement.FOwner := Self;
  end;
end;

function TSCDomElement.AttributeByName(const AName: WideString): TSCDomAttribute;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AttributeCount-1 do
    if Attributes[I].Name = AName then
    begin
      Result := Attributes[I];
      Exit;
    end;
end;

function TSCDomElement.AttributeCount: Integer;
begin
  Result := FAttributes.Count;
end;

procedure TSCDomElement.ClearAttributes;
begin
  while AttributeCount > 0 do DeleteAttribute(0);
end;

procedure TSCDomElement.ClearComments;
var
  I: Integer;
begin
  for I := FNodes.Count-1 downto 0 do
    if TSCDomItem(FNodes[I]) is TSCDomComment then
      DeleteNode(I);
end;

procedure TSCDomElement.ClearElements;
var
  I: Integer;
begin
  for I := FNodes.Count-1 downto 0 do
    if TSCDomItem(FNodes[I]) is TSCDomElement then
    begin
      TSCDomItem(FNodes[I]).FOwner := nil;
      FNodes.Delete(I);

      TSCDomItem(FNodes[I]).Free;
    end;
end;

constructor TSCDomElement.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkTag;

  FAttributes := TList.Create;
  FNodes := TList.Create;
end;

function TSCDomElement.DeleteAttribute(Index: Integer): Boolean;
var
  Attr: TSCDomAttribute;
begin
  Result := False;
  if (Index > -1) and (Index < FAttributes.Count) then
  begin
    Attr := Attributes[Index];
    FAttributes.Delete(Index);

    Attr.FOwner := nil;
    Attr.Free;

    Result := True;
  end;
end;

function TSCDomElement.DeleteNode(Index: Integer): Boolean;
var
  ANode: TSCDomTreeNode;
begin
  Result := False;

  if (Index > -1) and (Index < FNodes.Count) then
  begin
    ANode := TSCDomTreeNode(FNodes[Index]);

    ANode.FOwner := nil;
    FNodes.Delete(Index);

    ANode.Free;
    Result := True;
  end;
end;

destructor TSCDomElement.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FNodes);
  inherited Destroy;
end;

function TSCDomElement.ElementByName(const AName: WideString): TSCDomElement;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count-1 do
    if (TSCDomItem(FNodes[I]) is TSCDomElement) and
      (TSCDomItem(FNodes[I]).Name = AName) then
    begin
      Result := TSCDomElement(FNodes[I]);
      Exit;
    end;
end;

function TSCDomElement.ChildNodeCount: Integer;
begin
  Result := FNodes.Count;
end;

function TSCDomElement.ExtractAttribute(AAttribute: TSCDomAttribute): TSCDomAttribute;
var
  Index: Integer;
begin
  Result := nil;

  Index := IndexOfAttribute(AAttribute);
  if Index > -1 then
  begin
    Result := TSCDomAttribute(FAttributes[Index]);
    FAttributes.Delete(Index);

    if Result <> nil then
      Result.FOwner := nil;
  end;
end;

function TSCDomElement.ExtractNode(ANode: TSCDomTreeNode): TSCDomItem;
begin
  Result := nil;
  if (ANode <> nil) and (ANode.Owner = Self) and (IndexOfNode(ANode) > -1) then
  begin
    Result := TSCDomTreeNode(FNodes.Extract(ANode));
    Result.FOwner := nil;
  end;
end;

function TSCDomElement.GetAttributes(Index: Integer): TSCDomAttribute;
begin
  Result := nil;
  if (Index > -1) and (Index < FAttributes.Count) then
    Result := TSCDomAttribute(FAttributes[Index]);
end;

function TSCDomElement.GetNode(Index: Integer): TSCDomTreeNode;
begin
  Result := nil;
  if (Index > -1) and (Index < FNodes.Count) then
    Result := TSCDomTreeNode(FNodes[Index]);
end;

function TSCDomElement.IndexOfAttribute(AAttr: TSCDomAttribute): Integer;
begin
  Result := -1;
  if AAttr <> nil then Result := FAttributes.IndexOf(AAttr);
end;

function TSCDomElement.IndexOfNode(ANode: TSCDomTreeNode): Integer;
begin
  Result := -1;
  if (ANode <> nil) and (ANode.Owner = Self) then Result := FNodes.IndexOf(ANode);
end;

function TSCDomElement.InsertAttribute(AAttribute: TSCDomAttribute;
  Index: Integer): Boolean;
var
  Root: TSCDomDocument;
begin
  Result := (AAttribute <> nil) and (IndexOfAttribute(AAttribute) = -1) and
    (Index > -1) and (Index <= FAttributes.Count);

  if Result then
  begin
    Result := False;

    CheckIfNameIsValid(AAttribute.Name, False);

    if AttributeByName(AAttribute.Name) <> nil then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCErrorDuplicateAttr, '')
      else
        raise Exception.Create(SCRes_ErrorDuplicateAttr);

      Exit;
    end;

    if AAttribute.FOwner <> nil then
      AAttribute.FOwner.ExtractItem(AAttribute);

    FAttributes.Insert(Index, AAttribute);
    AAttribute.FOwner := Self;

    Result := True;
  end;
end;

function TSCDomElement.InsertNode(ANode: TSCDomTreeNode; Index: Integer): Boolean;
var
  Cnt: Integer;
begin
  Result := (ANode <> nil) and (ANode.Owner <> Self) and
    (Index > -1) and (Index < FNodes.Count) and (IndexOfNode(ANode) = -1);

  if Result then
  begin
    if ANode.FOwner <> nil then
      ANode.FOwner.ExtractItem(ANode);

    Cnt := FNodes.Count;
    FNodes.Insert(Index, ANode);

    Result := FNodes.Count > Cnt;
    if Result then ANode.FOwner := Self;
  end;
end;

function TSCDomElement.RemoveAttribute(AAttribute: TSCDomAttribute): Boolean;
begin
  Result := FAttributes.Remove(AAttribute) > -1;
  if Result then
  begin
    AAttribute.FOwner := nil;
    AAttribute.Free;
  end;
end;

function TSCDomElement.RemoveNode(ANode: TSCDomTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.Owner = Self) and
    (FNodes.Remove(ANode) > -1);

  if Result then
  begin
    ANode.FOwner := nil;
    ANode.Free;
  end;
end;

procedure TSCDomElement.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  Lv, I, Indx: Integer;
  PrevIsText, NextIsText: Boolean;
  StartSpace, EndSpace, Buf: WideString;
begin
  PrevIsText := False;
  NextIsText := False;

  if (Owner <> nil) and (Owner is TSCDomElement) then
  begin
    Indx := TSCDomElement(Owner).IndexOfNode(Self) - 1;
    if Indx > -1 then
      PrevIsText := TSCDomElement(Owner).ChildNodes[Indx] is TSCDomText;

    Indx := TSCDomElement(Owner).IndexOfNode(Self) + 1;
    if Indx < TSCDomElement(Owner).ChildNodeCount then
      NextIsText := TSCDomElement(Owner).ChildNodes[Indx] is TSCDomText;
  end;

  Lv := Indent*(Level-1);
  if Lv < 0 then Lv := 0;
  EndSpace := StringOfChar(UniWideSpace, Lv);

  StartSpace := '';
  if not PrevIsText then StartSpace := EndSpace;

  Buf := StartSpace + UniTagStarter + FName;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);

  for I := 0 to AttributeCount-1 do
    Attributes[I].SaveToStream(AStream, Indent, Normalized, PrettyTrim);

  if FNodes.Count = 0 then
  begin
    SetLength(Buf, 0);
    Buf := Buf + UniPathTerminator + UniTagTerminator;
    if not NextIsText and CanDoLastCRLF then Buf := Buf + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end else
  begin
    SetLength(Buf, 1);
    Buf := UniTagTerminator;
    if not (ChildNodes[0] is TSCDomText) then Buf := Buf + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);

    for I := 0 to FNodes.Count-1 do
      TSCDomTreeNode(FNodes[I]).SaveToStream(AStream, Indent, Normalized, PrettyTrim);

    SetLength(Buf, 0);
    Buf := Buf + UniTagStarter + UniPathTerminator + FName + UniTagTerminator;

    if (FNodes.Count > 0) and not (ChildNodes[ChildNodeCount-1] is TSCDomText) then
      Buf := EndSpace + Buf;

    if not NextIsText and CanDoLastCRLF then Buf := Buf + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end;
end;

function TSCDomElement.AddCData(ACData: TSCDomCData): Boolean;
begin
  Result := False;
  if (ACData <> nil) and (IndexOfNode(ACData) = -1) then
  begin
    if ACData.FOwner <> nil then
      ACData.FOwner.ExtractItem(ACData);

    Result := FNodes.Add(ACData) > -1;
    if Result then
      ACData.FOwner := Self;
  end;
end;

function TSCDomElement.AddCData(const Data: WideString): TSCDomCData;
begin
  Result := nil;
  try
    Result := TSCDomCData.Create(Self);
    if Result <> nil then Result.Text := Data;

    FNodes.Add(Result);
  except
    if Result <> nil then FreeAndNil(Result);
  end;
end;

procedure TSCDomElement.ClearCDatas;
var
  I: Integer;
begin
  for I := FNodes.Count-1 downto 0 do
    if TSCDomItem(FNodes[I]) is TSCDomCData then
      DeleteNode(I);
end;

procedure TSCDomElement.ClearChildNodes;
begin
  while ChildNodeCount > 0 do DeleteNode(0);
end;

function TSCDomElement.AddPI(const AName, Data: WideString): TSCDomProcessInst;
begin
  Result := nil;
  try
    Result := TSCDomProcessInst.Create(Self);
    if Result <> nil then
    begin
      Result.Name := AName;
      Result.Text := Data;
    end;

    FNodes.Add(Result);
  except
    if Result <> nil then FreeAndNil(Result);
  end;
end;

function TSCDomElement.AddPI(API: TSCDomProcessInst): Boolean;
begin
  Result := False;
  if (API <> nil) and (IndexOfNode(API) = -1) then
  begin
    if API.FOwner <> nil then
      API.FOwner.ExtractItem(API);

    Result := FNodes.Add(API) > -1;
    if Result then
      API.FOwner := Self;
  end;
end;

procedure TSCDomElement.ClearPIs;
var
  I: Integer;
begin
  for I := FNodes.Count-1 downto 0 do
    if TSCDomItem(FNodes[I]) is TSCDomCData then
      DeleteNode(I);
end;

procedure TSCDomElement.NormalizeData;
var
  I: LongInt;
begin
  for I := 0 to ChildNodeCount-1 do
    TSCDomTreeNode(FNodes[I]).NormalizeData;
end;

procedure TSCDomElement.NormalizeTab(TabSpace: Integer);
var
  I: Integer;
begin
  for I := 0 to ChildNodeCount-1 do
    TSCDomTreeNode(FNodes[I]).NormalizeTab(TabSpace);
end;

procedure TSCDomElement.PrettyTrimData;
var
  I: LongInt;
begin
  for I := 0 to ChildNodeCount-1 do
    TSCDomTreeNode(FNodes[I]).NormalizeData;
end;

function TSCDomElement.AddText(const Data: WideString): TSCDomText;
begin
  Result := nil;
  try
    Result := TSCDomText.Create(Self);
    if Result <> nil then Result.Text := Data;

    FNodes.Add(Result);
  except
    if Result <> nil then FreeAndNil(Result);
  end;
end;

function TSCDomElement.AddText(AText: TSCDomText): Boolean;
begin
  Result := False;
  if (AText <> nil) and (IndexOfNode(AText) = -1) then
  begin
    if AText.FOwner <> nil then
      AText.FOwner.ExtractItem(AText);

    Result := FNodes.Add(AText) > -1;
    if Result then
      AText.FOwner := Self;
  end;
end;

procedure TSCDomElement.ClearTexts;
var
  I: Integer;
begin
  for I := FNodes.Count-1 downto 0 do
    if TSCDomItem(FNodes[I]) is TSCDomText then
      DeleteNode(I);
end;

function TSCDomElement.ExtractItem(AItem: TSCDomItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AItem <> nil) and (AItem.FOwner = Self) then
  begin
    AItem.FOwner := nil;

    if AItem is TSCDomAttribute then
    begin
      I := FAttributes.IndexOf(AItem);
      if I > -1 then FAttributes.Delete(I);
    end;

    if AItem is TSCDomTreeNode then
    begin
      I := FNodes.IndexOf(AItem);
      if I > -1 then FNodes.Delete(I);
    end;

    Result := True;
  end;
end;

procedure TSCDomElement.Clear;
begin
  inherited Clear;
  ClearAttributes;
  ClearChildNodes;
end;

function TSCDomElement.AddAttribute(const AName, AValue: WideString): TSCDomAttribute;
var
  C: WideChar;
begin
  C := UniValueStarter;
  if Pos(C, AValue) > 0 then C := UniValueStarter_2;

  Result := Self.AddAttribute(AName, AValue, C);
end;

function TSCDomElement.AddNode(ANode: TSCDomTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (IndexOfNode(ANode) = -1);

  if Result then
  begin
    if ANode.FOwner <> nil then ANode.FOwner.ExtractItem(ANode);

    FNodes.Add(ANode);
    ANode.FOwner := Self;
  end;
end;

function TSCDomElement.SetAttribute(const AName,
  AValue: WideString): TSCDomAttribute;
begin
  Result := AttributeByName(AName);
  if Result <> nil then
    Result.SetValue(AValue)
  else
    Result := AddAttribute(AName, AValue);
end;

function TSCDomElement.GetAttribute(const AName: WideString): WideString;
var
  Attr: TSCDomAttribute;
begin
  Result := '';
  Attr := AttributeByName(AName);
  if Attr <> nil then Result := Attr.Value;
end;

{ TSCDomTextBag }

procedure TSCDomTextBag.AfterConstruction;
begin
  inherited AfterConstruction;
  SetLength(FText, 0);
end;

procedure TSCDomTextBag.Clear;
begin
  SetText('');
end;

constructor TSCDomTextBag.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FInsertSpace := True;
  FCanNormalizeData := False;
end;

function TSCDomTextBag.GetStoredData: WideString;
begin
  Result := FText;
end;

function TSCDomTextBag.GetTagStarter: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniTagStarter;
end;

function TSCDomTextBag.GetTagStoper: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniTagTerminator;
end;

procedure TSCDomTextBag.NormalizeTab(TabSpace: Integer);
begin
  FText := SC_WideTabToSpace(FText, TabSpace);
end;

procedure TSCDomTextBag.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  Space, Buf, StoredDT: WideString;
  Lv: Integer;
  Ln: LongInt;
  Indx: Integer;
  PrevIsText, NextIsText: Boolean;
begin
  PrevIsText := False;
  NextIsText := False;

  if (Owner <> nil) and (Owner is TSCDomElement) then
  begin
    Indx := TSCDomElement(Owner).IndexOfNode(Self) - 1;
    if Indx > -1 then
      PrevIsText := TSCDomElement(Owner).ChildNodes[Indx] is TSCDomText;

    Indx := TSCDomElement(Owner).IndexOfNode(Self) + 1;
    if Indx < TSCDomElement(Owner).ChildNodeCount then
      NextIsText := TSCDomElement(Owner).ChildNodes[Indx] is TSCDomText;
  end;

  StoredDT := GetStoredData;

  Ln := Length(StoredDT);
  SetLength(Buf, Ln);
  System.Move(Pointer(StoredDT)^, Pointer(Buf)^, 2*Ln);

  if FCanNormalizeData then
  begin
    if Normalized then Buf := SC_WideTrim(Buf)
    else if PrettyTrim then Buf := SC_WidePretyTrim(Buf);
  end;

  Space := '';
  if FInsertSpace then
  begin
    Lv := Indent*(Level-1);
    if Lv < 0 then Lv := 0;
    Space := StringOfChar(UniWideSpace, Lv);
  end;

  if not PrevIsText and FInsertSpace then
    Buf := Space + GetTagStarter + Buf + GetTagStoper
  else Buf := GetTagStarter + Buf + GetTagStoper;

  if FInsertSpace and not NextIsText and CanDoLastCRLF then
    Buf := Buf + UniWideCRLF;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);
end;

procedure TSCDomTextBag.SetText(const Newvalue: WideString);
begin
  ValidateData(Newvalue);
  FText := Newvalue;
end;

procedure TSCDomTextBag.SetName(const Newvalue: WideString);
begin
  // Datastore objects can not have a name
end;

procedure TSCDomTextBag.ValidateData(const AValue: Widestring);
begin
  // for future use
end;

procedure TSCDomTextBag.ValidateName(const AName: WideString);
begin
  // Datastore objects can not have a name
end;

{ TSCDomCData }

constructor TSCDomCData.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkCData;
end;

function TSCDomCData.GetTagStarter: WideString;
begin
  Result := inherited GetTagStarter + UniCData;
end;

function TSCDomCData.GetTagStoper: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniCDataTerminator + UniCDataTerminator +
    inherited GetTagStoper;
end;

procedure TSCDomCData.ValidateData(const AValue: Widestring);
begin
  if Pos(UniCDataTerminationTag, AValue) > 0 then
    raise Exception.Create(SCRes_DataHasCDataTerminationTag);
end;

{ TSCDomComment }

constructor TSCDomComment.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkComment;
end;

function TSCDomComment.GetTagStarter: WideString;
begin
  Result := inherited GetTagStarter + UniCommentStart;
end;

function TSCDomComment.GetTagStoper: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniComment + inherited GetTagStoper;
end;

procedure TSCDomComment.ValidateData(const AValue: Widestring);
begin
  if Pos(UniComment, AValue) > 0 then
    raise Exception.Create(SCRes_ErrorDataHasCommentTermination);
end;

{ TSCDomDeclaration }

constructor TSCDomDeclaration.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkDeclaration;
end;

function TSCDomDeclaration.GetTagStarter: WideString;
begin
  Result := inherited GetTagStarter + UniCommentStarter + FName;
end;

function TSCDomDeclaration.GetTagStoper: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniWideSpace + inherited GetTagStoper;
end;

procedure TSCDomDeclaration.SetName(const Newvalue: WideString);
begin
  ResetName(Newvalue);
end;

procedure TSCDomDeclaration.ValidateData(const AValue: Widestring);
begin

end;

procedure TSCDomDeclaration.ValidateName(const AName: WideString);
begin
  CheckIfNameIsValid(AName);
end;

{ TSCDomDTD }

function TSCDomDTD.AddElement(const AName: WideString; var IsNew: Boolean;
  AContentType: TSCDTDElementContentType): TSCDomDTDElement;
begin
  IsNew := False;
  Result := nil;
  try
    CheckIfNameIsValid(AName, False);

    Result := ElementByName(AName);
    if Result <> nil then
    begin
      Result.ContentType := AContentType;
      Exit;
    end;

    Result := TSCDomDTDElement.Create(Self);
    if Result <> nil then
    begin
      IsNew := True;
      
      Result.Name  := AName;
      Result.ContentType := AContentType;

      FItems.Add(Result);
    end;
  except
    if Result <> nil then FreeAndNil(Result);
    raise;
  end;
end;

function TSCDomDTD.AddEntity(const AName, AValue: WideString;
  var IsNew: Boolean): TSCDomDTDEntity;
begin
  IsNew := False;
  Result := nil;
  try
    Result := EntityByName(AName);
    if Result = nil then
    begin
      CheckIfNameIsValid(AName, False);

      Result := TSCDomDTDEntity.Create(Self);

      IsNew := True;

      Result.Name  := AName;
      FItems.Add(Result);
    end;

    Result.Value := AValue;
  except
    if Result <> nil then FreeAndNil(Result);
    raise;
  end;
end;

procedure TSCDomDTD.Clear;
begin
  inherited Clear;

  if FItems <> nil then
    while Count > 0 do Delete(0);
end;

constructor TSCDomDTD.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken  := stkDTD;
  FItems  := TList.Create;

  SetLength(FDocumentElement, 0);
  SetLength(FPublicId, 0);
  SetLength(FSystemId, 0);
end;

destructor TSCDomDTD.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSCDomDTD.ElementByName(const AName: WideString): TSCDomDTDElement;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count-1 do
    if (TSCDomItem(FItems[I]) is TSCDomDTDElement) and
      (TSCDomItem(FItems[I]).Name = AName) then
    begin
      Result := TSCDomDTDElement(FItems[I]);
      Exit;
    end;
end;

function TSCDomDTD.EntityByName(const AName: WideString): TSCDomDTDEntity;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count-1 do
    if (TSCDomItem(FItems[I]) is TSCDomDTDEntity) and
      (TSCDomDTDEntity(FItems[I]).Name = AName) then
    begin
      Result := TSCDomDTDEntity(FItems[I]);
      Exit;
    end;
end;

procedure TSCDomDTD.SetDocumentElement(const Newvalue: WideString);
var
  S: WideString;
begin
  S := SC_WideTrim(Newvalue);
  CheckIfNameIsValid(S);
  FDocumentElement := S;
end;

procedure TSCDomDTD.SetPublicId(const Newvalue: WideString);
var
  Ln: Integer;
begin
  FPublicId := SC_WideTrim(Newvalue);

  Ln := Length(FPublicId);
  if (Ln > 0) and ((FPublicId[1] = UniValueStarter) or
    (FPublicId[1] = UniValueStarter_2)) then
  begin
    System.Delete(FPublicId, 1, 1);
    FPublicId := SC_WideTrim(FPublicId);
  end;

  Ln := Length(FPublicId);
  if (Ln > 0) and ((FPublicId[Ln] = UniValueTerminator) or
    (FPublicId[Ln] = UniValueTerminator_2)) then
  begin
    System.Delete(FPublicId, Ln, 1);
    FPublicId := SC_WideTrim(FPublicId);
  end;
end;

procedure TSCDomDTD.SetSystemId(const Newvalue: WideString);
var
  Ln: Integer;
begin
  FSystemId := SC_WideTrim(Newvalue);

  Ln := Length(FSystemId);
  if (Ln > 0) and ((FSystemId[1] = UniValueStarter) or
    (FSystemId[1] = UniValueStarter_2)) then
  begin
    System.Delete(FSystemId, 1, 1);
    FSystemId := SC_WideTrim(FSystemId);
  end;

  Ln := Length(FSystemId);
  if (Ln > 0) and ((FSystemId[Ln] = UniValueTerminator) or
    (FSystemId[Ln] = UniValueTerminator_2)) then
  begin
    System.Delete(FSystemId, Ln, 1);
    FSystemId := SC_WideTrim(FSystemId);
  end;
end;

function TSCDomDTD.AddNotation(const AName, APublicId, ASystemId: WideString;
  var IsNew: Boolean): TSCDomDTDNotation;
begin
  IsNew := False;
  Result := nil;
  try
    CheckIfNameIsValid(AName, False);

    Result := NotationByName(AName);
    if Result = nil then
    begin
      Result := TSCDomDTDNotation.Create(Self);

      IsNew := True;
      FItems.Add(Result);

      Result.Name  := AName;
    end;

    Result.PublicId := APublicId;
    Result.SystemId := ASystemId;
  except
    if Result <> nil then FreeAndNil(Result);
    raise;
  end;
end;

function TSCDomDTD.NotationByName(const AName: WideString): TSCDomDTDNotation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count-1 do
    if (TSCDomItem(FItems[I]) is TSCDomDTDEntity) and
      (TSCDomDTDNotation(FItems[I]).Name = AName) then
    begin
      Result := TSCDomDTDNotation(FItems[I]);
      Exit;
    end;
end;

procedure TSCDomDTD.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  I, LastElm, Lv: Integer;
  Space, Buf: WideString;
  Elm: TSCDomDTDElement;
begin
  Lv := Indent*(Level-1);
  if Lv < 0 then Lv := 0;
  Space := StringOfChar(UniWideSpace, Lv);

  Buf := Space + UniDTDDefinationStarter + UniDTD + UniWideSpace + FName;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);

  if Length(FPublicId) > 0 then
  begin
    Buf := Buf + UniWideSpace + UniDTDPubSource + UniWideSpace +
      UniValueStarter + FPublicID + UniValueTerminator;

    if Length(FSystemId) > 0 then
      Buf := Buf + UniWideSpace + UniValueStarter + FSystemId + UniValueTerminator;
  end else
  if Length(FSystemId) > 0 then
    Buf := Buf + UniWideSpace + UniDTDSysSource + UniWideSpace +
      UniValueStarter + FSystemId + UniValueTerminator;

  if FItems.Count = 0 then
  begin
    Buf := Buf + UniDTDDefinationTerminator + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end else
  begin
    Buf := Buf + UniWideSpace + UniInternalDTDStarter + UniWideSpace;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);

    for I := 0 to FItems.Count-1 do
      if TObject(FItems[I]) is TSCDomDTDEntity then
        TSCDomItem(FItems[I]).SaveToStream(AStream, Indent, Normalized, PrettyTrim);

    for I := 0 to FItems.Count-1 do
      if TObject(FItems[I]) is TSCDomDTDNotation then
        TSCDomItem(FItems[I]).SaveToStream(AStream, Indent, Normalized, PrettyTrim);

    LastElm := -1;
    for I := 0 to FItems.Count-1 do
      if TObject(FItems[I]) is TSCDomDTDElement then
        Inc(LastElm);

    for I := 0 to FItems.Count-1 do
      if TObject(FItems[I]) is TSCDomComment then
        TSCDomItem(FItems[I]).SaveToStream(AStream, Indent, Normalized, PrettyTrim)
      else
      if TObject(FItems[I]) is TSCDomDTDElement then
      begin
        Elm := TSCDomDTDElement(FItems[I]);

        Elm.SaveToStream(AStream, Indent, Normalized, PrettyTrim);

        if (I < LastElm) and (Elm.FAttrList.Count > 0) then
        begin
          Buf := UniWideCRLF;

          AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
          SetLength(Buf, 0);
        end;
      end;

    Buf := UniWideCRLF + Space + UniInternalDTDTerminator +
      UniDTDDefinationTerminator + UniWideCRLF;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end;
end;

function TSCDomDTD.ExtractItem(AItem: TSCDomItem): Boolean;
begin
  Result := False;

  if (AItem <> nil) and (FItems <> nil) then
  begin
    FItems.Remove(AItem);

    Result := True;
    if AItem.FOwner = Self then
      AItem.FOwner := nil;
  end;
end;

function TSCDomDTD.GetItem(Index: Integer): TSCDomItem;
begin
  Result := nil;
  if (Index > -1) and (Index < FItems.Count) then
    Result := FItems.Items[Index];
end;

procedure TSCDomDTD.ItemDestroyed(AItem: TSCDomItem);
begin
  if FItems <> nil then
    FItems.Remove(AItem);
end;

function TSCDomDTD.Count: Integer;
begin
  Result := 0;
  if FItems <> nil then
    Result := FItems.Count;
end;

function TSCDomDTD.Delete(Index: Integer): Boolean;
var
  AItem: TSCDomItem;
begin
  Result := False;
  if (Index > -1) and (FItems <> nil) and (Index < FItems.Count) then
  begin
    AItem := TSCDomItem(FItems[Index]);
    FItems.Remove(AItem);

    if (AItem.FOwner = Self) then
    begin
      AItem.FOwner := nil;
      AItem.Free;

      Result := True;
    end;
  end;
end;

function TSCDomDTD.IndexOf(AItem: TSCDomItem): Integer;
begin
  Result := -1;
  if (AItem <> nil) and (FItems <> nil) then
    Result := FItems.IndexOf(AItem);
end;

function TSCDomDTD.Remove(AItem: TSCDomItem): Boolean;
begin
  Result := False;
  if (AItem <> nil) and (FItems <> nil) then
  begin
    FItems.Remove(AItem);

    if (AItem.FOwner = Self) then
    begin
      AItem.FOwner := nil;
      AItem.Free;

      Result := True;
    end;
  end;
end;

function TSCDomDTD.AddComment(const Data: WideString): TSCDomDTDComment;
begin
  Result := nil;
  try
    Result := TSCDomDTDComment.Create(Self);
    if Result <> nil then
    begin
      Result.Text := Data;
      FItems.Add(Result);
    end;
  except
    if Result <> nil then FreeAndNil(Result);
    raise;
  end;
end;

procedure TSCDomDTD.ValidateDTD;

  function ValidateChildrenOf(N: TSCElementContentNode; var AName: WideString): Boolean;
  var
    I: Integer;
    C: TSCElementContentNode;
  begin
    Result := True;

    for I := 0 to N.Count-1 do
    begin
      C := N.FNodes[I];
      if (Length(C.Element) > 0) and (ElementByName(C.Element) = nil) then
      begin
        AName  := C.Element;
        Result := False;
        
        Break;
      end;

      Result := ValidateChildrenOf(C, AName);
      if not Result then
        Break;
    end;
  end;

var
  I: Integer;
  AName: WideString;
  Elm: TSCDomDTDElement;
begin
  for I := 0 to FItems.Count-1 do
    if TObject(FItems[I]) is TSCDomDTDElement then
    begin
      Elm := TSCDomDTDElement(FItems[I]);

      SetLength(AName, 0);

      if (Elm.FChildrenNode.Count > 0) and
        not ValidateChildrenOf(Elm.FChildrenNode, AName) then
      begin
        Beep;
        Break;
      end;
    end;
end;

procedure TSCDomDTD.DoError(ErrorCode, Line, Column: Integer;
  const Value: WideString);
begin
  //
end;

{ TSCDomProcessInst }

constructor TSCDomProcessInst.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkPI;
end;

function TSCDomProcessInst.GetTagStarter: WideString;
begin
  Result := inherited GetTagStarter + UniInstructionStarter + FName;
  if Length(GetStoredData) > 0 then Result := Result + UniWideSpace;
end;

function TSCDomProcessInst.GetTagStoper: WideString;
begin
  SetLength(Result, 0);
  Result := Result + UniInstructionTerminator + inherited GetTagStoper;
end;

procedure TSCDomProcessInst.SetName(const Newvalue: WideString);
begin
  ResetName(Newvalue);
end;

procedure TSCDomProcessInst.ValidateData(const AValue: Widestring);
begin
  //
end;

procedure TSCDomProcessInst.ValidateName(const AName: WideString);
begin
  CheckIfNameIsValid(AName);
end;

{ TSCDomParser }

procedure TSCDomParser.AddToPath(AElement: TSCDomElement);
begin
  FPathList.Add(AElement);
end;

procedure TSCDomParser.AttributeName(Sender: TObject; const AName, Value: WideString;
  Delimiter: WideChar; Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
  AElement: TSCDomElement;
begin
  if (Delimiter <> UniValueStarter) and (Delimiter <> UniValueStarter_2) then
    Delimiter := UniValueStarter;

  AElement := GetCurrentElement;
  if AElement <> FDomDocument then
  begin
    AItem := AElement.AddAttribute(AName, Value, Delimiter);

    if AItem <> nil then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnAttributeName) then
    FOnAttributeName(Self, AName, Value, Delimiter, Line, Column, CanContinue);
end;

procedure TSCDomParser.AttributeValue(Sender: TObject; const AName, Value: WideString;
  Delimiter: WideChar; Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
  AElement: TSCDomElement;
begin
  if (Delimiter <> UniValueStarter) and (Delimiter <> UniValueStarter_2) then
    Delimiter := UniValueStarter;

  AElement := GetCurrentElement;
  if AElement <> FDomDocument then
  begin
    AItem := AElement.AttributeByName(AName);

    if AItem <> nil then
      TSCDomAttribute(AItem).SetValue(Value)
    else begin
      AItem := AElement.AddAttribute(AName, Value, Delimiter);

      if AItem <> nil then
      begin
        AItem.FLine := Line;
        AItem.FColumn := Column;
      end;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnAttributeValue) then
    FOnAttributeValue(Self, AName, Value, Delimiter, Line, Column, CanContinue);
end;

procedure TSCDomParser.Text(Sender: TObject; const Chars: Widestring;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
  AElement: TSCDomElement;
begin
  AElement := GetCurrentElement;

  if (Length(SC_WideTrim(Chars)) > 0) and (AElement <> FDomDocument) then
  begin
    if FNormalizeData then
      AItem := AElement.AddText(SC_WideTrim(Chars))
    else AItem := AElement.AddText(Chars);

    if AItem <> nil then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnText) then
    FOnText(Self, Chars, Line, Column, CanContinue);
end;

procedure TSCDomParser.Comment(Sender: TObject; const Chars: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
begin
  AItem := GetCurrentElement.AddComment(Chars);

  if AItem <> nil then
  begin
    AItem.FLine := Line;
    AItem.FColumn := Column;
  end;

  CanContinue := True;
  if Assigned(FOnComment) then
    FOnComment(Self, Chars, Line, Column, CanContinue);
end;

constructor TSCDomParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPathList := TList.Create;
  FParser := TSCCustomSaxParser.Create(Self);

  FContentParser := TSCSaxContentParser.Create;

  FErrorLine := -1;
  FErrorCol := -1;
  SetLength(FErrorPath, 0);
  SetLength(FErrorValue, 0);

  with FParser do
  begin
    FErrorOptions := FErrorOptions + [saxCheckEscapes, saxDuplicateDTDElement];

    OnStartElement := StartElement;
    OnEndElement := EndElement;
    OnStartCData := StartCData;
    OnEndCData := EndCData;
    OnComment := Comment;
    OnAttributeName := AttributeName;
    OnAttributeValue := AttributeValue;
    OnText := Text;
    OnProcessingInstruction := ProcessingInstruction;
    OnError := Error;

    OnDTDStart := DomDTDStart;
    OnDTDFileStart := DomDTDFileStart;
    OnDTDEnd := DomDTDEnd;

    OnDTDElement := DomDTDElement;
    OnDTDAttribute := DomDTDAttribute;
    OnDTDEntity := DomDTDEntity;
    OnDTDNotation := DomDTDNotation;
    OnDTDComment := DomDTDComment;
  end;
end;

destructor TSCDomParser.Destroy;
begin
  FreeAndNil(FContentParser);
  FreeAndNil(FPathList);
  inherited Destroy;
end;

procedure TSCDomParser.DocumentEnd(var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnEndDocument) then FOnEndDocument(Self, CanContinue);
end;

procedure TSCDomParser.DocumentStart(var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnStartDocument) then FOnStartDocument(Self, CanContinue);
end;

procedure TSCDomParser.DomDTDAttribute(Sender: TObject; const Element, AName: WideString;
  AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
  const EnumValues, DefaultValue: WideString; Delimiter: WideChar;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  IsNew: Boolean;
  Elm: TSCDomDTDElement;
begin
  if FDomDocument <> nil then
  begin
    Elm := FDomDocument.DTD.ElementByName(Element);

    if Elm = nil then
    begin
      Elm := FDomDocument.DTD.AddElement(Element, IsNew);
      Elm.FCreatedByAttr := True;
    end;

    Elm.AddAttribute(AName, EnumValues, DefaultValue, AttributeType, AttributeDefault);
  end;

  CanContinue := True;
  if Assigned(FOnDTDAttribute) then
    FOnDTDAttribute(Self, Element, AName, AttributeType, AttributeDefault,
      EnumValues, DefaultValue, Delimiter, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDComment(Sender: TObject; const Data: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
begin
  if (FDomDocument <> nil) and (FDomDocument.DTD <> nil) then
  begin
    AItem := FDomDocument.DTD.AddComment(Data);

    if AItem <> nil then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnDTDComment) then
    FOnDTDComment(Self, Data, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDElement(Sender: TObject; const AName,
  Child: WideString; ElementContentType: TSCDTDElementContentType;
  Occurrence: TSCDTDElementOccurrence; Line, Column: LongInt;
  var CanContinue: Boolean);
var
  IsNew: Boolean;
  ALine, ACol: Integer;
  ErrorStr: WideString;
  ExpectedChar: WideChar;
  Elm: TSCDomDTDElement;
  Content: TSCElementContentNode;
begin
  if FDomDocument <> nil then
  begin
    if not FParsing and (FDomDocument.DTD.ElementByName(AName) <> nil) then
    begin
      CanContinue := False;

      if FParsing then
        Error(Self, SCDuplicateDTDElement, FParser.FLinePos, FParser.FColumnPos,
          FParser.FPrevToken, FParser.FToken, AName, FParser.Path)
      else
        Error(Self, SCDuplicateDTDElement, -1, -1,
          stkDTDElement, stkDTDElement, AName, '');

      Exit;
    end;

    Elm := FDomDocument.DTD.AddElement(AName, IsNew, ElementContentType);
    Elm.Occurrence := Occurrence;

    if IsNew or Elm.FCreatedByAttr then
    begin
      Elm.FCreatedByAttr := False;

      Elm.FLine := Line;
      Elm.FColumn := Column;
    end;

    if (Child <> '') and (ElementContentType in [dtdElement, dtdMixed]) then
    begin
      Content := nil;
      SetLength(ErrorStr, 0);
      ExpectedChar := UniWideNull;

      if FContentParser.ParseContent(Child, Content, ALine, ACol,
        ExpectedChar, ErrorStr, 0) <> dteNoError then
      begin
        Elm.StoredData := Child;
        
        CanContinue := False;
        if Content <> nil then
          Content.Free;

        Exit;
      end;

      Content.SetOwner(Elm.FChildrenNode);
    end;
  end;

  CanContinue := True;
  if Assigned(FOnDTDElement) then FOnDTDElement(Self, AName, Child,
    ElementContentType, Occurrence, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDEnd(Sender: TObject; const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDEnd) then
    FOnDTDEnd(Self, AName, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDEntity(Sender: TObject; const AName,
  Data: WideString; Delimiter: WideChar; Line, Column: LongInt;
  var CanContinue: Boolean);
var
  IsNew: Boolean;
  AItem: TSCDomItem;
begin
  if FDomDocument <> nil then
  begin
    AItem := FDomDocument.DTD.AddEntity(AName, Data, IsNew);

    if IsNew and (AItem <> nil) then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnDTDEntity) then
    FOnDTDEntity(Self, AName, Data, Delimiter, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDFileStart(Sender: TObject; const AName, PublicID,
  SystemID: WideString; Line, Column: LongInt; var CanContinue: Boolean);
begin
  if FDomDocument <> nil then
    with FDomDocument do
    begin
      DTD.Clear;

      DTD.Name := AName;
      DTD.PublicID := PublicID;
      DTD.SystemID := SystemID;
    end;

  CanContinue := True;
  if Assigned(FOnDTDFileStart) then FOnDTDFileStart(Self, AName, PublicID,
    SystemID, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDNotation(Sender: TObject; const AName, PublicID,
  SystemID: WideString; Line, Column: LongInt; var CanContinue: Boolean);
var
  IsNew: Boolean;
  AItem: TSCDomItem;
begin
  if FDomDocument <> nil then
  begin
    AItem := FDomDocument.DTD.AddNotation(AName, PublicID, SystemID, IsNew);

    if IsNew and (AItem <> nil) then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  CanContinue := True;
  if Assigned(FOnDTDNotation) then FOnDTDNotation(Self,
    AName, PublicID, SystemID, Line, Column, CanContinue);
end;

procedure TSCDomParser.DomDTDStart(Sender: TObject; const AName, PublicID,
  SystemID: WideString; Line, Column: LongInt; var CanContinue: Boolean);
begin
  if FDomDocument <> nil then
    with FDomDocument do
    begin
      DTD.Clear;

      DTD.Name := AName;
      DTD.PublicID := PublicID;
      DTD.SystemID := SystemID;
    end;

  CanContinue := True;
  if Assigned(FOnDTDStart) then FOnDTDStart(Self, AName, PublicID,
    SystemID, Line, Column, CanContinue);
end;

procedure TSCDomParser.EndCData(Sender: TObject; const CData: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
begin
  AItem := GetCurrentElement.AddCData(CData);

  if AItem <> nil then
  begin
    AItem.FLine := FStartLine;
    AItem.FColumn := FStartColumn;
  end;

  FStartLine := -1;
  FStartColumn := -1;

  CanContinue := True;
  if Assigned(FOnEndCData) then
    FOnEndCData(Self, CData, Line, Column, CanContinue);
end;

procedure TSCDomParser.EndElement(Sender: TObject; const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  GotoUpLevel;

  CanContinue := True;
  if Assigned(FOnEndElement) then
    FOnEndElement(Self, AName, Line, Column, CanContinue);
end;

procedure TSCDomParser.Error(Sender: TObject; ErrorCode, Line, Column: Integer;
  ReleasedToken, EnteredToken: TSCTokenKind; const Value, Path: WideString);
begin
  FErrorOccured := True;
  FLastErrorNo  := ErrorCode;
  FErrorLine    := Line;
  FErrorCol     := Column;
  FErrorPath    := Path;
  FErrorValue   := Value;

  if Assigned(FOnError) then FOnError(Self, ErrorCode, Line, Column,
    ReleasedToken, EnteredToken, Value, Path);

  if FRaiseErrors then
    raise Exception.CreateFmt(SCRes_ParseError, [ErrorDescription(ErrorCode), Line, Column]);
end;

function TSCDomParser.ErrorDescription(No: Integer): String;
begin
  Result := FParser.ErrorDescription(No);
end;

function TSCDomParser.GetCurrentElement: TSCDomElement;
begin
  if FPathList.Count = 0 then Result := FDomDocument
  else Result := TSCDomElement(FPathList[FPathList.Count-1]);
end;

function TSCDomParser.GetLastTokenColumn: LongInt;
begin
  Result := FParser.LastTokenColumn;
end;

function TSCDomParser.GetLastTokenLine: LongInt;
begin
  Result := FParser.LastTokenLine;
end;

procedure TSCDomParser.GotoUpLevel;
begin
  if PathCount > 0 then FPathList.Delete(PathCount-1);
end;

function TSCDomParser.ParseFile(AFileName: String; Validate: Boolean): TSCDomDocument;
var
  AStream: TFileStream;
begin
  Result := nil;
  if FParsing then
    Exit;

  FParsing := True;
  AStream := nil;
  try
    AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

    FParsing := False;
    Result := ParseStream(AStream, Validate);
  finally
    FParsing := False;
    if AStream <> nil then AStream.Free;
  end;
end;

function TSCDomParser.ParseStream(AStream: TStream; Validate: Boolean): TSCDomDocument;
var
  SW: WideString;
  SA: AnsiString;
  UTFSignature: WideChar;
  UTF8_3rdSignature: Char;
  Size, BytesRead: Integer;
begin
  Result := nil;

  if not FParsing then
  begin
    FParsing := True;
    try
      Size := AStream.Size - AStream.Position;
      try
        if Size = 0 then
        begin
          FParser.DoEmptyDocumentError;
          Exit;
        end;

        BytesRead := 0;

        UTFSignature := UniWideNull;
        if Size > 1 then BytesRead := AStream.Read(UTFSignature, 2);

        if (UTFSignature = UniBOM) or (UTFSignature = UniBigEndianBOM) then
        begin
          SetLength(SW, (Size - 2) div 2);
          AStream.Read(PWideChar(SW)^, Size - 2);

          if UTFSignature = UniBigEndianBOM then SC_WideSwapByteOrder(SW);

          FParsing := False;
          Result := ParseString(SW, Validate);
        end else
        begin
          UTF8_3rdSignature := #0;
          if Size > 2 then BytesRead := BytesRead + AStream.Read(UTF8_3rdSignature, 1);

          if (UTFSignature = Utf8BOM_1) and (UTF8_3rdSignature = Utf8BOM_2) then
          begin
            SetLength(SA, Size-3);
            AStream.Read(PChar(SA)^, Size-3);

            SW := SC_UTF8ToWideString(SA);

            FParsing := False;
            Result := ParseString(SW, Validate);
          end else
          begin
            AStream.Seek(-BytesRead, soFromCurrent);

            SetLength(SA, Size);
            AStream.Read(PChar(SA)^, Size);

            FParsing := False;
            Result := ParseString(SA, Validate);
          end;
        end;
      except
        if FRaiseErrors then
          raise;
      end;
    finally
      FParsing := False;
    end;
  end;
end;

function TSCDomParser.ParseString(const S: WideString; Validate: Boolean): TSCDomDocument;
const
  ValErrors = [saxCheckEscapes, saxDuplicateDTDElement];
begin
  Result := nil;
  if FParsing then
    Exit;

  FParsing := True;
  try
    FErrorOccured := False;
    try
      FLastErrorNo := 0;
      FErrorLine   := -1;
      FErrorCol    := -1;

      FStartLine   := -1;
      FStartColumn := -1;

      SetLength(FErrorPath, 0);
      SetLength(FErrorValue, 0);

      FPathList.Clear;
      try
        FDomDocument := TSCDomDocument.Create(nil);

        with FParser do
        begin
          FErrorOptions := FErrorOptions - ValErrors;
          if Validate then FErrorOptions := FErrorOptions + ValErrors;
        end;

        FParser.ParseString(S);

        if Validate and not FErrorOccured and 
          (FDomDocument.DTD <> nil) and (FDomDocument.DTD.Count > 0)  then
          with FDomDocument do
          begin
            FInternalValidation := True;
            try
              ValidateDocument;
            finally
              FInternalValidation := False;
            end;
          end;

        Result := FDomDocument;
      finally
        FErrorOccured := False;

        FDomDocument := nil;
        FPathList.Clear;
      end;
    except
      if Result <> nil then FreeAndNil(Result);
      FLastErrorNo := SCInternalError;
      if FRaiseErrors then raise;
    end;
  finally
    FParsing := False;
  end;
end;

function TSCDomParser.PathCount: Integer;
begin
  Result := FPathList.Count;
end;

procedure TSCDomParser.ProcessingInstruction(Sender: TObject; const Target,
  Data: WideString; Line, Column: LongInt; var CanContinue: Boolean);
var
  AItem: TSCDomItem;
  AElement: TSCDomElement;
begin
  AElement := GetCurrentElement;
  if (AElement = FDomDocument) and (Target = UniWideXmlInstruction) then
  begin
    with FDomDocument do
    begin
      Encoding := FParser.Encoding;
      Version  := FParser.Version;
      Standalone := FParser.Standalone;
    end;
  end else
  begin
    AItem := AElement.AddPI(Target, Data);

    if AItem <> nil then
    begin
      AItem.FLine := Line;
      AItem.FColumn := Column;
    end;
  end;

  if Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(Self, Target, Data, Line, Column, CanContinue);
end;

procedure TSCDomParser.SetIgnoreWhiteSpaces(Value: Boolean);
begin
  FParser.FIgnoreWhiteSpaces := Value;
  FIgnoreWhiteSpaces := Value;
end;

procedure TSCDomParser.SetRaiseErrors(Value: Boolean);
begin
  FParser.FRaiseErrors := Value;
  FRaiseErrors := Value;
end;

procedure TSCDomParser.StartCData(Sender: TObject; const CData: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  FStartLine   := Line;
  FStartColumn := Column;

  CanContinue := True;
  if Assigned(FOnStartCData) then
    FOnStartCData(Self, CData, Line, Column, CanContinue);
end;

procedure TSCDomParser.StartElement(Sender: TObject; const AName: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
var
  AElement: TSCDomElement;
begin
  AElement := GetCurrentElement.AddElement(AName);
  if AElement <> nil then
  begin
    AddToPath(AElement);
    
    AElement.FLine := Line;
    AElement.FColumn := Column;
  end;
end;

{ TSCDomTreeNode }

function TSCDomTreeNode.CanDoLastCRLF: Boolean;
begin
  Result := True;
  if (Owner <> nil) and (Owner is TSCDomDocument) then
    Result := TSCDomDocument(Owner).IndexOfNode(Self) <
      TSCDomDocument(Owner).ChildNodeCount - 1;
end;

destructor TSCDomTreeNode.Destroy;
begin
  if FOwner is TSCDomElement then TSCDomElement(FOwner).ExtractNode(Self);
  inherited Destroy;
end;

procedure TSCDomTreeNode.NormalizeData;
begin
  //
end;

procedure TSCDomTreeNode.PrettyTrimData;
begin
  //
end;

{ TSCEscapeParser }

function TSCEscapeParser.IsValidData(const Data: Widestring; var ALine,
  ACol: Integer; var ErrorChar: WideChar; var Error: TSCEscapeError;
  InvalidChars: WideString): boolean;
var
  IChar: WideChar;
  Ln, I, J, EscCol,
  EscLine, EscValue, EscLn: LongInt;
  Success, InEscaping,
  InvalidEscape, EscJustStarted: Boolean;
  Str: Widestring;

  function NextChar(var HasChar: Boolean): WideChar;
  begin
    Result := UniWideNull;

    HasChar := I <= Ln;
    if not HasChar then exit;

    Result := Data[I];
    Inc(I);

    Inc(ACol);
    if result = UniWideCR then
    begin
      Inc(ALine);
      ACol := 0;
    end else
      if result = UniWideLF then ACol := 0;
  end;
  
begin
  ALine := 0;
  ACol := 0;

  Result := true;
  Error := escNoError;
  ErrorChar := UniWideNull;

  Ln := Length(Data);
  if Ln = 0 then exit;

  InEscaping := false;

  Str := '';
  EscCol := 0;
  EscLine := 0;
  EscJustStarted := false;
  IChar := UniWideNull;

  if InvalidChars = '' then
    InvalidChars := UniInvalidAttributeValueChars + UniValueStarter;

  I := 1;
  while I <= Ln do
  begin
    IChar := NextChar(Success);
    if not Success then
    begin
      Result := not InEscaping;

      if not result then
        Error := escNeedEscapeClose;
      exit;
    end;

    EscJustStarted := false;

    if Pos(IChar, InvalidChars) > 0 then
    begin
      Result := false;
      ErrorChar := IChar;
      Error := escInvalidCharacter;
      
      exit;
    end;

    case IChar of
      UniWideEscapeStart:
      begin
        if InEscaping then
        begin
          Result := false;
          Error := escUndefinedEscape;

          ACol  := EscCol;
          ALine := EscLine;
          exit;
        end;

        InEscaping := true;
        SetLength(Str, 0);

        EscCol  := ACol;
        EscLine := ALine;

        EscJustStarted := true;
      end;
      UniWideEscapeEnd:
      begin
        if InEscaping then
        begin
          InEscaping := false;

          Str := UniEntityRefStarter + Str + UniEntityRefTerminator;

          InvalidEscape := true;
          for J := Low(SCEscapes) to High(SCEscapes) do
            if Str = SCEscapes[J, 0] then
            begin
              InvalidEscape := false;
              Break;
            end;

          if InvalidEscape then
          begin
            EscLn := Length(Str);

            if EscLn > 1 then
            begin
              if Str[2] <> UniWideNumber then
              begin
                Result := false;
                ErrorChar := IChar;
                Error := escInvalidNameStart;

                ACol  := EscCol;
                ALine := EscLine;

                exit;
              end;

              if (EscLn > 2) and (Str[2] = UniWideNumber) then
              begin
                Delete(Str, 1, 2);

                if (Length(Str) > 1) and (Str[Length(Str)] = UniWideEscapeEnd) then
                begin
                  Delete(Str, Length(Str), 1);

                  EscValue := -1;
                  if Pos(Str[1], UniValidEscapeFirstChars) > 0 then
                  begin
                    EscValue := StrToIntDef(Str, -1);

                    if (EscValue > -1) and (EscValue <= 65535) then
                      InvalidEscape := false
                    else
                    if (Length(Str) > 1) and (Str[1] = WideChar('x')) then
                    begin
                      Delete(Str, 1, 1);

                      if Pos(Str[1], UniValidEscapeChars) > 0 then
                      begin
                        Str := WideChar('$') + Str;

                        EscValue := StrToIntDef(Str, -1);

                        if (EscValue > -1) and (EscValue <= 65535) then
                          InvalidEscape := false;
                      end;
                    end;
                  end;

                  if InvalidEscape and (EscValue > 65535) then
                  begin
                    Result := false;
                    Error := escInvalidUnicodeCharacter;

                    exit;
                  end;
                end;
              end;
            end;
          end;

          if InvalidEscape then
          begin
            Result := false;
            Error := escUndefinedEscape;

            ACol  := EscCol;
            ALine := EscLine;
            exit;
          end;

          ACol  := EscCol;
          ALine := EscLine;
          SetLength(Str, 0);
        end;
      end;
      else begin
        if InEscaping then Str := Str + IChar;
      end;
    end;
  end;

  if InEscaping then
  begin
    if EscJustStarted then
    begin
      Error := escInvalidNameStart;
      ErrorChar := IChar;
    end else
      Error := escNeedEscapeClose;
    Result := false;
  end;
end;

function TSCEscapeParser.ReplaceEscapes(const Data: Widestring; Validate: Boolean;
  var IsValid: Boolean; InvalidChars: WideString): WideString;
var
  ALine, ACol: LongInt;
  I: Integer;
  ErrorChar: WideChar;
  Error: TSCEscapeError;
begin
  Result := Data;

  IsValid := IsValidData(result, ALine, ACol, ErrorChar, Error, InvalidChars);
  if not IsValid and Validate then exit;

  for I := Low(SCEscapes) to High(SCEscapes) do
    Result := SC_WideStringReplace(result, SCEscapes[I, 0], SCEscapes[I, 1], true);
end;


function SCErrorDescription(ErrorNo: Integer; ErrorToken: TSCTokenKind;
  ErrorLine, ErrorColumn: LongInt; ErrorChar: WideChar; const ErrorValue: WideString): String;
var
  I: Integer;
begin
  Result := SCRes_ErrorUndefined;

  case ErrorNo of
    SCElementNameStartError:
    begin
      Result := Format(SCRes_ErrorElementNameStarts, [UniInvalidElementFirstChars]);
      Exit;
    end;
    SCElementNameError:
    begin
      Result := Format(SCRes_ErrorElementNameContains, [UniInvalidElementChars]);
      Exit;
    end;
    SCInvalidElementNameError:
    begin
      Result := Format(SCRes_InvalidElementName, [UniInvalidElementFirstChars,
        UniInvalidElementChars]);
      Exit;
    end;
    SCInvalidCharForToken:
    begin
      Result := Format(SCRes_InvalidCharForToken, [AnsiLowerCase(SCTokenAsString(ErrorToken))]);
      Exit;
    end;
    SCInvalidCharacter:
    begin
      Result := Format(SCRes_InvalidCharacter, [String(ErrorChar), AnsiLowerCase(SCTokenAsString(ErrorToken))]);
      Exit;
    end;
  end;

  for I := Low(SCXMLErrors) to High(SCXMLErrors) do
    if SCXMLErrors[I].No = ErrorNo then
    begin
      Result := SCXMLErrors[I].Desc;
      Exit;
    end;
end;

function SCTokenAsString(AToken: TSCTokenKind): String;
begin
  Result := '';
  case AToken of
    stkTag:
      Result := SCRes_Element;
    stkAttribute:
      Result := SCRes_Attribute;
    stkAttributeValue:
      Result := SCRes_AttributeValue;
    stkComment:
      Result := SCRes_Comment;
    stkCData:
      Result := SCRes_CDATA;
    stkText:
      Result := SCRes_CharacterData;
    stkPI:
      Result := SCRes_PI;
    stkPIData:
      Result := SCRes_PIData;
    stkDTD:
      Result := SCRes_DTD;
    stkDocumentTop:
      Result := SCRes_Document;
    stkDeclarationLoop:
      Result := SCRes_DeclarationLoop;
    stkDeclaration:
      Result := SCRes_Declaration;
  end;
end;

{ TSCDomText }

constructor TSCDomText.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FToken := stkText;
  FInsertSpace := False;
  FCanNormalizeData := True;
end;

function TSCDomText.GetTagStarter: WideString;
begin
  Result := '';
end;

function TSCDomText.GetTagStoper: WideString;
begin
  Result := '';
end;

procedure TSCDomText.ValidateData(const AValue: Widestring);
begin
  //
end;

{ TSCPathInfo }

function TSCPathInfo.AddItem: TSCPathInfo;
begin
  Result := TSCPathInfo.Create(Self);
  if result = nil then exit;

  FItems.Add(result);
  ItemAdded(result);
end;

procedure TSCPathInfo.BeforeDestruction;
begin
  if not FDonotNotify and (FOwner <> nil) then
    FOwner.ItemDestroyed(Self);

  inherited BeforeDestruction;
end;

procedure TSCPathInfo.ClearItems;
var
  I: Integer;
  AItem: TSCPathInfo;
begin
  for I := FItems.Count-1 downto 0 do
  begin
    AItem := TSCPathInfo(FItems[i]);
    AItem.FDonotNotify := true;
    AItem.Free;
  end;

  FItems.Clear;
  FCursor := Self;
end;

constructor TSCPathInfo.Create(AOwner: TSCPathInfo);
begin
  inherited Create;
  FCursor := Self;
  FItems := TList.Create;
  FOwner := AOwner;
  FName := '';
  FTagClosed := false;
end;

destructor TSCPathInfo.Destroy;
begin
  FDestroying := true;
  ClearItems;
  inherited Destroy;
end;

function TSCPathInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSCPathInfo.GetItem(Index: Integer): TSCPathInfo;
begin
  Result := nil;
  if (Index > -1) and (Index < FItems.Count) then
    Result := TSCPathInfo(FItems[Index]);
end;

procedure TSCPathInfo.GoUp;
begin
  if (FCursor <> Self) and
    (FCursor <> nil) and (FCursor.FOwner <> nil) then
      FCursor := FCursor.FOwner;
end;

procedure TSCPathInfo.ItemAdded(AItem: TSCPathInfo);
begin
  if AItem = nil then FCursor := Self
  else FCursor := AItem;

  if FOwner <> nil then FOwner.ItemAdded(AItem);
end;

function TSCPathInfo.ItemAtPos(ALine, ACol: Integer): TSCPathInfo;
var
  i : Integer;
  AItem : TSCPathInfo;
begin
  Result := nil;
  if (ALine < 0) or (ACol < 0) or
    (ALine < StartLine) or (ALine > EndLine) then exit;

  Result := Self;

  if ((ALine = StartLine) and (ACol < StartColumn)) or
    ((ALine = EndLine) and (ACol >= EndColumn)) then
  begin
    Result := nil;
    exit;
  end;

  for i := 0 to Count - 1 do
  begin
    AItem := Items[i].ItemAtPos(ALine, ACol);

    if AItem <> nil then
    begin
      Result := AItem;
      exit;
    end;
  end;
end;

function TSCPathInfo.ItemByName(AName: WideString): TSCPathInfo;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to FItems.Count-1 do
    if Items[i].Name = AName then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TSCPathInfo.ItemDestroyed(AItem: TSCPathInfo);
begin
  if FDestroying or (AItem = nil) then exit;

  if (FItems.Remove(AItem) > -1) and
    (FCursor = AItem) then FCursor := Self;

  if FOwner <> nil then FOwner.ItemDestroyed(AItem);
end;

function TSCPathInfo.Level: Integer;
begin
  Result := 0;
  if FOwner <> nil then
    Result := FOwner.Level + 1;
end;

type
  TSCDomDTDChildElement = record
    Name: WideString;
    Existence: TSCDTDElementOccurrence;
  end;
  PSCDomDTDChildElement = ^TSCDomDTDChildElement;

{ TSCDomDTDElement }

function TSCDomDTDElement.AddAttribute(const AName, Enums,
  DefaultValue: WideString; AttrType: TSCDTDAttributeType;
  AttrDef: TSCDTDAttributeDefault = dtdAttrUndefined): TSCDomDTDAttribute;
var
  Created: Boolean;
begin
  Result := nil;
  Created := False;
  try
    CheckIfNameIsValid(AName, False);

    Result := AttributeByName(AName);
    if Result = nil then
    begin
      Result  := TSCDomDTDAttribute.Create(Self);
      FAttrList.Add(Result);

      Created := True;
    end;

    if Result <> nil then
    begin
      Result.Name := AName;
      Result.EnumValues := Enums;
      Result.DefaultValue := DefaultValue;
      Result.AttributeType := AttrType;
      Result.AttributeDefault := AttrDef;
    end;
  except
    if Created and (Result <> nil) then
      FreeAndNil(Result);

    raise;
  end;
end;

function TSCDomDTDElement.ExtractAttribute(AAttribute: TSCDomDTDAttribute): TSCDomDTDAttribute;
var
  Index: Integer;
begin
  Result := nil;

  Index := IndexOfAttribute(AAttribute);
  if Index > -1 then
  begin
    Result := TSCDomDTDAttribute(FAttrList[Index]);
    FAttrList.Delete(Index);

    if Result <> nil then
      Result.FOwner := nil;
  end;
end;

function TSCDomDTDElement.IndexOfAttribute(AAttr: TSCDomDTDAttribute): Integer;
begin
  Result := -1;
  if AAttr <> nil then Result := FAttrList.IndexOf(AAttr);
end;

function TSCDomDTDElement.InsertAttribute(AAttribute: TSCDomDTDAttribute;
  Index: Integer): Boolean;
var
  Root: TSCDomDocument;
begin
  Result := (AAttribute <> nil) and (IndexOfAttribute(AAttribute) = -1) and
    (Index > -1) and (Index <= FAttrList.Count);

  if Result then
  begin
    Result := False;

    CheckIfNameIsValid(AAttribute.Name, False);

    if AttributeByName(AAttribute.Name) <> nil then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCErrorDuplicateAttr, '')
      else
        raise Exception.Create(SCRes_ErrorDuplicateAttr);

      Exit;
    end;

    if AAttribute.FOwner <> nil then
      AAttribute.FOwner.ExtractItem(AAttribute);

    FAttrList.Insert(Index, AAttribute);
    AAttribute.FOwner := Self;

    Result := True;
  end;
end;

function TSCDomDTDElement.RemoveAttribute(AAttribute: TSCDomDTDAttribute): Boolean;
begin
  Result := FAttrList.Remove(AAttribute) > -1;
  if Result then
  begin
    AAttribute.FOwner := nil;
    AAttribute.Free;
  end;
end;

function TSCDomDTDElement.ExtractItem(AItem: TSCDomItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AItem <> nil) and (AItem.FOwner = Self) then
  begin
    AItem.FOwner := nil;

    if AItem is TSCDomDTDAttribute then
    begin
      I := FAttrList.IndexOf(AItem);
      if I > -1 then
        FAttrList.Delete(I);
    end;

    Result := True;
  end;
end;

function TSCDomDTDElement.AddAttribute(AAttribute: TSCDomDTDAttribute): Boolean;
var
  Root: TSCDomDocument;
begin
  Result := False;
  if (AAttribute <> nil) and (IndexOfAttribute(AAttribute) = -1) then
  begin
    CheckIfNameIsValid(AAttribute.Name, False);

    if AttributeByName(AAttribute.Name) <> nil then
    begin
      Root := GetDocument;
      if Root <> nil then
         Root.DoInternalError(Self, SCErrorDuplicateAttr, '')
      else
        raise Exception.Create(SCRes_ErrorDuplicateAttr);

      Exit;
    end;

    if AAttribute.FOwner <> nil then
      AAttribute.FOwner.ExtractItem(AAttribute);

    Result := FAttrList.Add(AAttribute) > -1;
    if Result then
      AAttribute.FOwner := Self;
  end;
end;

function TSCDomDTDElement.AttributeByName(const AName: WideString): TSCDomDTDAttribute;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FAttrList.Count-1 do
    if TSCDomDTDAttribute(FAttrList[I]).Name = AName then
    begin
      Result := TSCDomDTDAttribute(FAttrList[I]);
      Exit;
    end;
end;

procedure TSCDomDTDElement.Clear;
var
  I: Integer;
  AItem: TSCDomItem;
begin
  inherited Clear;

  FContentType := dtdMixed;
  FOccurrence  := dtdElementOnlyOne;

  if FChildrenNode <> nil then
    FChildrenNode.Clear;

  if FAttrList <> nil then
    for I := FAttrList.Count-1 downto 0 do
    begin
      AItem := FAttrList.Items[I];
      FAttrList.Delete(I);

      AItem.FOwner := nil;
      AItem.Free;
    end;
end;

constructor TSCDomDTDElement.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FContentType := dtdMixed;
  FOccurrence := dtdElementOnlyOne;
  FCreatedByAttr := False;
  FAttrList := TList.Create;

  FChildrenNode := TSCElementContentNode.Create(nil);
  FChildrenNode.FIncludeSelf := False;
end;

function TSCDomDTDElement.DeleteAttribute(Index: Integer): Boolean;
var
  Attr: TSCDomDTDAttribute;
begin
  Result := False;
  if (Index > -1) and (Index < FAttrList.Count) then
  begin
    Attr := FAttrList[Index];
    FAttrList.Delete(Index);

    Attr.FOwner := nil;
    Attr.Free;

    Result := True;
  end;
end;

destructor TSCDomDTDElement.Destroy;
begin
  FreeAndNil(FAttrList);
  FreeAndNil(FChildrenNode);
  inherited Destroy;
end;

function TSCDomDTDElement.AttributeCount: Integer;
begin
  Result := FAttrList.Count;
end;

function TSCDomDTDElement.GetAttribute(Index: Integer): TSCDomDTDAttribute;
begin
  Result := nil;
  if (Index > -1) and (FAttrList <> nil) and (Index < FAttrList.Count) then
    Result := FAttrList.Items[Index];
end;

procedure TSCDomDTDElement.SetStoredData(const NewValue: WideString);
begin
  FStoredData := NewValue;
end;

procedure TSCDomDTDElement.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  I, Lv: Integer;
  Space, Buf, S: WideString;
begin
  Lv := Indent;
  if Lv < 0 then Lv := 0;
  Space := StringOfChar(UniWideSpace, Lv);

  if not FCreatedByAttr then
  begin
    Buf := UniWideCRLF + Space + UniDTDDefinationStarter + UniDTDElement +
      UniWideSpace + FName;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);

    case FContentType of
      dtdAny:
        Buf := Buf + UniWideSpace + UniDTDAny;
      dtdEmpty:
        Buf := Buf + UniWideSpace + UniDTDEmpty;
      dtdPCDATA, dtdUndefinedElement:
      begin
        Buf := Buf + UniWideSpace + UniDTDContentStarter + UniDTDPCDATA + UniDTDContentTerminator;

        case FOccurrence of
          dtdElementZeroOrOne:
            Buf := Buf + UniDTDChildZeroOrOne;
          dtdElementZeroOrMore:
            Buf := Buf + UniDTDChildZeroOrMore;
          dtdElementOneOrMore:
            Buf := Buf + UniDTDChildOneOrMore;
        end;
      end;
    end;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);

    S := FChildrenNode.AsString;
    if Length(S) > 0 then
    begin
      case FOccurrence of
        dtdElementZeroOrOne:
          S := S + UniDTDChildZeroOrOne;
        dtdElementZeroOrMore:
          S := S + UniDTDChildZeroOrMore;
        dtdElementOneOrMore:
          S := S + UniDTDChildOneOrMore;
      end;

      Buf := UniWideSpace + S;

      AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
      SetLength(Buf, 0);
    end;

    Buf := UniDTDDefinationTerminator;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  end;

  SetLength(Buf, 0);

  if FAttrList.Count > 0 then
  begin
    Buf := UniWideCRLF + Space + UniDTDDefinationStarter + UniDTDAttributeList +
      UniWideSpace + FName + UniWideSpace;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);

    if FAttrList.Count = 1 then
      Indent := 0;

    for I := 0 to FAttrList.Count-1 do
    begin
      if FAttrList.Count > 1 then
      begin
        Buf := UniWideCRLF;

        AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
        SetLength(Buf, 0);
      end;

      TSCDomDTDAttribute(FAttrList.Items[I]).SaveToStream(AStream,
        Indent, Normalized, PrettyTrim);
    end;

    Buf := UniDTDDefinationTerminator;

    AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
    SetLength(Buf, 0);
  end;
end;

{ TSCDomDTDAttribute }

constructor TSCDomDTDAttribute.Create(AOwner: TSCDomItem);
begin
  inherited Create(AOwner);
  FAttributeType := dtdCDATA;
  FAttributeDefault := dtdAttrUndefined;
end;

procedure TSCDomDTDAttribute.SaveToStream(AStream: TStream;
  Indent: Integer; Normalized, PrettyTrim: Boolean);
var
  Lv: Integer;
  C: WideChar;
  Space, Buf, S: WideString;
begin
  Lv := 2*Indent;
  if Lv < 0 then Lv := 0;
  Space := StringOfChar(UniWideSpace, Lv);

  Buf := Space + FName;

  case FAttributeType of
    dtdUndefinedAttr, dtdCDATA:
      Buf := Buf + UniWideSpace + UniDTDAttrCDATA;
    dtdENUM, dtdNOTATION:
    begin
      if FAttributeType = dtdNOTATION then
        Buf := Buf + UniWideSpace + UniDTDAttrNOTATION;

      S := SC_WideTrim(FEnumValues);
      if Length(S) = 0 then
      begin
        SetLength(S, 3);

        S[1] := UniDTDContentStarter;
        S[2] := UniWideSpace;
        S[3] := UniDTDContentTerminator;
      end;

      if S[1] <> UniDTDContentStarter then
        S := UniDTDContentStarter + S;

      if S[Length(S)] <> UniDTDContentTerminator then
        S := S + UniDTDContentTerminator;

      Buf := Buf + UniWideSpace + S;
    end;
    dtdID:
      Buf := Buf + UniWideSpace + UniDTDAttrID;
    dtdIDREF:
      Buf := Buf + UniWideSpace + UniDTDAttrIDREF;
    dtdIDREFS:
      Buf := Buf + UniWideSpace + UniDTDAttrIDREFS;
    dtdNMTOKEN:
      Buf := Buf + UniWideSpace + UniDTDAttrNMTOKEN;
    dtdNMTOKENS:
      Buf := Buf + UniWideSpace + UniDTDAttrNMTOKENS;
    dtdENTITY:
      Buf := Buf + UniWideSpace + UniDTDAttrENTITY;
    dtdENTITIES:
      Buf := Buf + UniWideSpace + UniDTDAttrENTITIES;
    dtdXML:
      Buf := Buf + UniWideSpace + UniDTDAttrXML;
  end;

  case FAttributeDefault of
    dtdAttrImplied:
      Buf := Buf + UniWideSpace + UniDTDImplied;
    dtdAttrRequired:
      Buf := Buf + UniWideSpace + UniDTDRequired;
    dtdAttrFixed:
      Buf := Buf + UniWideSpace + UniDTDFixed;
  end;

  S := SC_WideTrim(FDefaultValue);
  if Length(S) > 0 then
  begin
    C := FDelimiter;
    if (C <> UniValueStarter) and (C <> UniValueStarter_2) then
      C := UniValueStarter;

    if S[1] <> C then S := C + S;
    if S[Length(S)] <> C then S := S + C;

    Buf := Buf + UniWideSpace + S;
  end;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);
end;

procedure TSCDomDTDAttribute.SetDefaultValue(const Newvalue: WideString);
begin
  ValidateValue(NewValue);

  FDefaultValue := NewValue;
end;

procedure TSCDomDTDAttribute.SetEnumValues(const Newvalue: WideString);
begin
  FEnumValues := NewValue;
end;

{ TSCDomDTDNotation }

procedure TSCDomDTDNotation.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  Lv: Integer;
  C: WideChar;
  Space, Buf: WideString;
begin
  Lv := Indent;
  if Lv < 0 then Lv := 0;
  Space := StringOfChar(UniWideSpace, Lv);

  Buf := UniWideCRLF + Space + UniDTDDefinationStarter + UniDTDNotation;

  if (Length(FPublicId) > 0) or (Length(FSystemId) > 0) then
  begin
    Buf := Buf + UniWideSpace + FName;

    C := FDelimiter;
    if (C <> UniValueStarter) and (C <> UniValueStarter_2) then
      C := UniValueStarter;

    if Length(FPublicId) > 0 then
    begin
      Buf := Buf + UniWideSpace + UniDTDPubSource + UniWideSpace +
        C + FPublicId + C;

      if Length(FSystemId) > 0 then
        Buf := Buf + UniWideSpace + C + FSystemId + C;
    end else
    if Length(FSystemId) > 0 then
      Buf := Buf + UniWideSpace + UniDTDSysSource + UniWideSpace +
        C + FSystemId + C;
  end;

  Buf := Buf + UniDTDDefinationTerminator;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);
end;

procedure TSCDomDTDNotation.SetPublicId(const Newvalue: WideString);
begin
  FPublicId := Newvalue;
end;

procedure TSCDomDTDNotation.SetSystemId(const Newvalue: WideString);
begin
  FSystemId := Newvalue;
end;

{ TSCSaxDTDParser }

procedure TSCSaxDTDParser.DoDTDAttribute(const Element, AName: WideString;
  AttributeType: TSCDTDAttributeType; AttributeDefault: TSCDTDAttributeDefault;
  const EnumValues, DefaultValue: WideString; Delimiter: WideChar; ALine,
  ACol: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDAttribute) then
    FOnDTDAttribute(Self, Element, AName, AttributeType, AttributeDefault,
      EnumValues, DefaultValue, Delimiter, ALine, ACol, CanContinue);
end;

procedure TSCSaxDTDParser.DoDTDElement(const AName, Child: WideString;
  ElementContentType: TSCDTDElementContentType; Occurrence: TSCDTDElementOccurrence;
  ALine, ACol: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDElement) then
    FOnDTDElement(Self, AName, Child, ElementContentType, Occurrence,
    ALine, ACol, CanContinue);
end;

procedure TSCSaxDTDParser.DoDTDEntity(const AName, Data: WideString;
  Delimiter: WideChar; ALine, ACol: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDEntity) then
    FOnDTDEntity(Self, AName, Data, Delimiter, ALine, ACol, CanContinue);
end;

procedure TSCSaxDTDParser.DoDTDNotation(const AName, PublicID,
  SystemID: WideString; ALine, ACol: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDNotation) then FOnDTDNotation(Self,
    AName, PublicID, SystemID, ALine, ACol, CanContinue);
end;

function TSCSaxDTDParser.GetAttributeDefault(const AttrDef: WideString): TSCDTDAttributeDefault;
begin
  Result := dtdAttrUndefined;

  if AttrDef = UniDTDImplied then
    Result := dtdAttrImplied
  else
  if AttrDef = UniDTDRequired then
    Result := dtdAttrRequired
  else
  if AttrDef = UniDTDFixed then
    Result := dtdAttrFixed;
end;

function TSCSaxDTDParser.GetAttributeType(const Attr: WideString): TSCDTDAttributeType;
var
  Ln: Integer;
begin
  Result := dtdUndefinedAttr;

  Ln := Length(Attr);
  if Ln = 0 then
    Exit;

  if Attr = UniDTDAttrCDATA then
    Result := dtdCDATA
  else
  if (Ln > 2) and (Attr[1] = UniDTDContentStarter) and
    (Attr[Ln] = UniDTDContentTerminator) then
    Result := dtdENUM
  else
  if Attr = UniDTDAttrID then
    Result := dtdID
  else
  if Attr = UniDTDAttrIDREF then
    Result := dtdIDREF
  else
  if Attr = UniDTDAttrIDREFS then
    Result := dtdIDREFS
  else
  if Attr = UniDTDAttrNMTOKEN then
    Result := dtdNMTOKEN
  else
  if Attr = UniDTDAttrNMTOKENS then
    Result := dtdNMTOKENS
  else
  if Attr = UniDTDAttrENTITY then
    Result := dtdENTITY
  else
  if Attr = UniDTDAttrENTITIES then
    Result := dtdENTITIES
  else
  if Attr = UniDTDAttrNOTATION then
    Result := dtdNOTATION
  else
  if Attr = UniDTDAttrXML then
    Result := dtdXML;
end;

function TSCSaxDTDParser.GetElementOccurrence(const ElmName: WideString): TSCDTDElementOccurrence;
begin
  Result := dtdElementOnlyOne;

  if (Length(ElmName) > 0) and (ElmName <> UniDTDPCDATA) then
    Result := GetOccurrence(ElmName[Length(ElmName)]);
end;

function TSCSaxDTDParser.GetElementContentType(const ElmType: WideString): TSCDTDElementContentType;
begin
  Result := dtdUndefinedElement;

  if ElmType = UniDTDAny then
    Result := dtdAny
  else
  if ElmType = UniDTDEmpty then
    Result := dtdEmpty
  else
  if ElmType = UniDTDPCDATA then
    Result := dtdPCDATA
  else
  if Length(ElmType) > 0 then
    Result := dtdElement;
end;

function TSCSaxDTDParser.GetOccurrence(const C: WideChar): TSCDTDElementOccurrence;
begin
  Result := dtdElementOnlyOne;

  if C = UniDTDChildZeroOrOne then
    Result := dtdElementZeroOrOne
  else
  if C = UniDTDChildZeroOrMore then
    Result := dtdElementZeroOrMore
  else
  if C = UniDTDChildOneOrMore then
    Result := dtdElementOneOrMore;
end;

function TSCSaxDTDParser.ParseDTD(const S: WideString; var ALine,
  ACol: LongInt; var ExpectedChar: WideChar; var ErrorStr: WideString;
  ExternalDTD: Boolean): TSCDTDError;
var
  St: TDTDParseState;
  CanContinue, InChoiceList: Boolean;
  I, StartLine, StartCol, TagPos,
  TagCnt, PrevLine, PrevCol, Ln: LongInt;
  AttrValueDel, IChar, PrChar, NxChar: WideChar;
  Str, Name, AttrName, AttrType,
  AttrDefault, AttrDefaultValue, ChildName: WideString;
  BracketCount, GroupCount, ErrLine, ErrCol: Integer;
  ElementList: TSCUniStringList;

  Occurrence: TSCDTDElementOccurrence;
  ElementContentType: TSCDTDElementContentType;
  AttributeType: TSCDTDAttributeType;
  AttributeDefault: TSCDTDAttributeDefault;

  ContentNode: TSCElementContentNode;

  function IsWhiteSpace(const AChar: WideChar): boolean;
  begin
    Result := SC_WideCharIsSpace(AChar) or SC_WideCharIsControl(AChar);
  end;

  procedure ArrangeLineCol;
  begin
    if IChar = UniWideLF then
    begin
      PrevCol := ACol;
      ACol := 0;
    end else
      if IChar = UniWideCR then
      begin
        PrevCol  := ACol;
        PrevLine := ALine;

        ACol := 0;
        Inc(ALine);
      end else
      begin
        PrevCol := ACol;
        Inc(ACol);
      end;
  end;

  procedure ArangeLineColWithStr(AStr: WideString);
  var
    P: PWideChar;
    Start, PIndx, PLn: LongInt;
  begin
    P := Pointer(AStr);

    if P = nil then Exit;

    PIndx := 0;
    PLn := Length(AStr);

    while PIndx < PLn do
    begin
      Start := PIndx;

      while not (P^ in [UniWideCR, UniWideLF]) do
      begin
        Inc(P);
        Inc(PIndx);

        Inc(ACol);
        if PIndx >= PLn then
          Exit;
      end;

      PrevCol := ACol + PIndx - Start;
      ACol := 0;

      PrevLine := ALine;
      Inc(ALine);

      if P^ = UniWideCR then
      begin
        Inc(P);
        Inc(PIndx);
      end;

      if PIndx >= PLn then
        Exit;

      if P^ = UniWideLF then
      begin
        Inc(P);
        Inc(PIndx);
      end;
    end;
  end;

  function NextChar: boolean;
  begin
    PrChar := IChar;
    NxChar := UniWideNull;

    Inc(i);
    Result := i <= Ln;
    if not result then exit;

    IChar := S[i];
    ArrangeLineCol;

    if i + 1 <= Ln then NxChar := S[i + 1];
  end;

  function PrevChar: boolean;
  begin
    NxChar := IChar;
    IChar  := PrChar;
    PrChar := UniWideNull;

    if PrevCol <> ACol - 1 then ALine := PrevLine;
    ACol := PrevCol;

    Dec(i);
    Result := i > 0;
    if i > 1 then PrChar := S[i-1];
  end;

  function LoopWhileSpace: boolean;
  begin
    while i <= Ln do
    begin
      if not IsWhiteSpace(IChar) then Break;

      Result := NextChar;
      if not result then exit;
    end;

    Result := i <= Ln;
  end;

  procedure ChangeState(NewState: TDTDParseState);
  begin
    St := NewState;

    StartLine := ALine;
    StartCol  := ACol;

    GroupCount := 0;
    BracketCount := 0;

    InChoiceList := False;

    case St of
      dtdSpace:
        ExpectedChar := UniWideNull;
      dtdInTypeStore, dtdInElement,
      dtdInAttributeList, dtdInEntity,
      dtdInAttributeName, dtdInAttributeType:
        ExpectedChar := UniSpace;
      dtdInElementGroup:
        ExpectedChar := UniDTDContentTerminator;
      dtdInElementOccurance,
      dtdInEntityDescription:
        ExpectedChar := UniDTDDefinationTerminator;
      dtdInAttributeDefault:
        ExpectedChar := UniValueStarter;
      dtdInAttributeDefaultValue:
        ExpectedChar := UniValueTerminator;
    end;
  end;

  procedure ResetAttribute;
  begin
    SetLength(AttrName, 0);
    SetLength(AttrType, 0);
    SetLength(AttrDefault, 0);
    SetLength(AttrDefaultValue, 0);
    AttributeType := dtdUndefinedAttr;
    AttributeDefault := dtdAttrUndefined;
  end;

  procedure SetLineColToErrorStart;
  begin
    ALine := StartLine;
    ACol  := StartCol;

    if (ACol > 0) and (PrChar <> UniWideNull) and
      (PrChar <> UniWideCR) and (PrChar <> UniWideLF) then
      Dec(ACol);
  end;

begin
  Result := dteNoError;

  ALine := 0;
  ACol  := 0;

  StartLine := 0;
  StartCol  := 0;

  IChar := UniWideNull;
  PrChar := UniWideNull;

  PrevLine := -1;
  PrevCol  := 0;

  SetLength(Str, 0);
  SetLength(Name, 0);
  SetLength(ChildName, 0);
  SetLength(ErrorStr, 0);

  ResetAttribute;

  Ln := Length(S);
  ExpectedChar := UniWideNull;

  St := dtdSpace;

  I := 0;
  if not NextChar then
    Exit;

  ElementList := TSCUniStringList.Create;
  try
    CanContinue := True;

    AttrValueDel := UniWideNull;
    
    while I < Ln do
    begin
      if (St <> dtdSpace) and not NextChar then
        Exit;

      case St of
        dtdSpace:
        begin
          ExpectedChar := UniWideNull;

          if IsWhiteSpace(IChar) and not LoopWhileSpace then
            Exit;

          if IChar <> UniDTDDefinationStarter then
          begin
            SetLineColToErrorStart;
            ErrorStr := IChar;

            Result := dteInvalidCharacter;
            Exit;
          end;

          SetLength(Name, 0);

          ChangeState(dtdInTypeStore);
          if not NextChar then
            Exit;

          if IChar <> UniDeclarationStarter then
          begin
            ErrorStr := IChar;

            Result := dtdXMLElementFound;
            Exit;
          end;

          Str := IChar;
        end;
        dtdInTypeStore:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            Result := dteSpaceRequired;
            ErrorStr := UniDTDDefinationTerminator;

            if Str = UniDeclarationStarter then
            begin
              Result := dteInvalidDeclaration;
              ErrorStr := Copy(Str, 1, Length(Str));
            end;

            Exit;
          end;

          if IsWhiteSpace(IChar) or (Str = UniCommentStart) then
          begin
            SetLength(Name, 0);

            if Str = UniCommentStart then
              ChangeState(dtdInComment)
            else
            if Str = UniDTDElement then
              ChangeState(dtdInElement)
            else
            if Str = UniDTDAttributeList then
              ChangeState(dtdInAttributeList)
            else
            if Str = UniDTDEntity then
              ChangeState(dtdInEntity)
            else begin
              SetLineColToErrorStart;
              ErrorStr := Copy(Str, 1, Length(Str));

              Result := dteInvalidDeclaration;
              Exit;
            end;

            SetLength(Str, 0);
            ResetAttribute;

            if (St <> dtdInComment) and not LoopWhileSpace then
              Exit;

            Str := IChar;
          end else
            Str := Str + IChar;
        end;
        dtdInComment:
        begin
          TagPos := SC_WideScan(UniComment, S, i-1);

          if TagPos = 0 then
          begin
            ErrorStr := Copy(S, i, Ln - i + 1);
            Result := dteCommentNotClosed;

            Exit;
          end;

          Inc(TagPos, Length(UniComment));
          Str := Copy(S, i - 1, TagPos - i - Length(UniComment) + 1);

          ArangeLineColWithStr(Str);
          Dec(TagPos, Length(UniComment));

          if TagPos > 1 then
          begin
            i := TagPos;
            IChar := S[i];
            PrChar := S[i-1];

            PrevChar;
          end else
          begin
            i := TagPos - 1;

            IChar := UniWideNull;
            PrChar := UniWideNull;
          end;

          for TagCnt := 0 to Length(UniComment) do
            if not NextChar then Break;

          if IChar <> UniDTDDefinationTerminator then
          begin
            PrevChar;

            ExpectedChar := UniDTDDefinationTerminator;

            ErrorStr := Copy(Str, 1, Length(Str));
            Result := dteCommentHasTermination;

            Exit;
          end;

          DoDTDComment(Str, StartLine, StartCol, CanContinue);
          if not CanContinue then Exit;

          SetLength(Str, 0);
          ChangeState(dtdSpace);

          if not NextChar then
            Exit;
        end;
        dtdInEntity:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            Result := dteSpaceRequired;
            ErrorStr := UniDTDDefinationTerminator;

            Exit;
          end;

          if IsWhiteSpace(IChar) then
          begin
            if not IsValidElementName(Str) then
            begin
              SetLineColToErrorStart;
              ErrorStr := Copy(Str, 1, Length(Str));

              Result := dteInvalidName;
              Exit;
            end;

            Name := Copy(Str, 1, Length(Str));
            SetLength(Str, 0);

            if not LoopWhileSpace then
              Exit;

            if (IChar <> UniValueStarter) and (IChar <> UniValueStarter_2) then
            begin
              ExpectedChar := UniValueStarter;
              ErrorStr := IChar;

              Result := dteInvalidExternalID;
              Exit;
            end;

            AttrValueDel := IChar;

            ChangeState(dtdInEntityDescription);
          end else
            Str := Str + IChar;
        end;
        dtdInEntityDescription:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            ErrorStr := IChar;

            ExpectedChar := AttrValueDel;
            if ExpectedChar = UniWideNull then
              ExpectedChar := UniValueTerminator;

            Result := dteInvalidEnd;
            Exit;
          end;

          if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
          begin
            AttrValueDel := UniWideNull;

            if not (NextChar and LoopWhileSpace) then
              Exit;

            if IChar <> UniDTDDefinationTerminator then
            begin
              Result := dteInvalidEnd;

              ExpectedChar := UniDTDDefinationTerminator;
              ErrorStr := IChar;

              Exit;
            end;

            DoDTDEntity(Name, Str, IChar, StartLine, StartCol, CanContinue);
            
            if not CanContinue then
            begin
              Result := dteUserTerminated;
              Exit;
            end;

            if not NextChar then
              Exit;

            ChangeState(dtdSpace);

            SetLength(Str, 0);
          end else
            Str := Str + IChar;
        end;
        dtdInElement:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            Result := dteSpaceRequired;
            ErrorStr := UniDTDDefinationTerminator;

            Exit;
          end;

          if IsWhiteSpace(IChar) then
          begin
            if not IsValidElementName(Str) then
            begin
              SetLineColToErrorStart;
              ErrorStr := Copy(Str, 1, Length(Str));

              Result := dteInvalidName;
              Exit;
            end;

            Name := Copy(Str, 1, Length(Str));
            SetLength(Str, 0);

            if (saxDuplicateDTDElement in FErrorOptions) and
              (ElementList.IndexOf(Name) > -1) then
            begin
              SetLineColToErrorStart;

              Result := dteDuplicateElement;
              ErrorStr := Copy(Name, 1, Length(Name));

              Exit;
            end;

            if not LoopWhileSpace then
              Exit;

            ChangeState(dtdInElementGroup);

            if IChar <> UniDTDContentStarter then
            begin
              ChangeState(dtdInElementOccurance);
              Str := IChar;
            end;
          end else
            Str := Str + IChar;
        end;
        dtdInElementOccurance:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            Str := SC_WideTrim(Str);

            ElementContentType := GetElementContentType(Str);

            if ElementContentType in [dtdEmpty, dtdAny] then
            begin
              DoDTDElement(Name, '', ElementContentType, dtdElementOnlyOne,
                StartLine, StartCol, CanContinue);

              if not CanContinue then
              begin
                Result := dteUserTerminated;
                Exit;
              end;

              if not NextChar then
                Exit;

              ChangeState(dtdSpace);
            end else
            begin
              SetLineColToErrorStart;

              Result := dteInvalidContentModel;
              ErrorStr := Copy(Str, 1, Length(Str));

              Exit;
            end;

            SetLength(Str, 0);
          end else
            Str := Str + IChar;
        end;
        dtdInElementGroup:
        begin
          if (BracketCount = 0) and (IChar = UniDTDContentTerminator) then
          begin
            if not NextChar then
              Exit;

            Occurrence := GetOccurrence(IChar);

            if ((Occurrence <> dtdElementOnlyOne) and not NextChar) or
              (IsWhiteSpace(IChar) and not LoopWhileSpace) then
              Exit;

            if IChar <> UniDTDDefinationTerminator then
            begin
              Result := dteInvalidEnd;

              ExpectedChar := UniDTDDefinationTerminator;
              ErrorStr := IChar;

              Exit;
            end;

            ChildName := SC_WideTrim(Str);

            if ChildName = UniDTDPCDATA then
              DoDTDElement(Name, '', dtdPCDATA, Occurrence,
                StartLine, StartCol, CanContinue)
            else begin
              ContentNode := nil;
              try
                Result := FContentParser.ParseContent(Str, ContentNode,
                  ErrLine, ErrCol, ExpectedChar, ErrorStr, 0);

                if Result <> dteNoError then
                begin
                  SetLineColToErrorStart;

                  if (ErrLine > -1) and (ErrCol > -1) then
                  begin
                    if ErrLine = 0 then
                      Inc(ACol, ErrCol - 1);

                    Inc(ALine, ErrLine);
                  end;
                end;
              finally
                if ContentNode <> nil then
                  ContentNode.Free;
              end;

              if Result <> dteNoError then
              begin
                ErrorStr := Copy(Str, 1, Length(Str));
                Exit;
              end;

              SetLength(Str, 0);
              DoDTDElement(Name, ChildName, dtdElement, Occurrence,
                StartLine, StartCol, CanContinue);
            end;

            if not CanContinue then
            begin
              Result := dteUserTerminated;
              Exit;
            end;

            ChangeState(dtdSpace);

            if not NextChar then
              Exit;
          end else
          begin
            if IChar = UniDTDContentStarter then
              Inc(BracketCount)
            else
            if (BracketCount > 0) and (IChar = UniDTDContentTerminator) then
              Dec(BracketCount);

            Str := Str + IChar;
          end;
        end;
        dtdInAttributeList,
        dtdInAttributeName, dtdInAttributeType:
        begin
          if IChar = UniDTDDefinationTerminator then
          begin
            Result := dteSpaceRequired;
            ErrorStr := UniDTDDefinationTerminator;

            Exit;
          end;

          if not InChoiceList and IsWhiteSpace(IChar) then
          begin
            if (St in [dtdInAttributeList, dtdInAttributeName]) and
              not IsValidElementName(Str) then
            begin
              SetLineColToErrorStart;
              ErrorStr := Copy(Str, 1, Length(Str));

              Result := dteInvalidName;
              Exit;
            end;

            case St of
              dtdInAttributeList:
                Name := Copy(Str, 1, Length(Str));
              dtdInAttributeName:
                AttrName := Copy(Str, 1, Length(Str));
              dtdInAttributeType:
              begin
                AttrType := Copy(Str, 1, Length(Str));
                AttributeType := GetAttributeType(AttrType);

                if AttributeType = dtdUndefinedAttr then
                begin
                  SetLineColToErrorStart;

                  ErrorStr := Copy(AttrType, 1, Length(AttrType));
                  Result := dteInvalidAttributeType;

                  Exit;
                end;
              end;
            end;

            SetLength(Str, 0);
            if not LoopWhileSpace then
              Exit;

            Str := IChar;

            case St of
              dtdInAttributeList:
                ChangeState(dtdInAttributeName);
              dtdInAttributeName:
              begin
                ChangeState(dtdInAttributeType);
                InChoiceList := IChar = UniDTDContentStarter;
              end;
              dtdInAttributeType:
              begin
                if IChar = UniDTDAttrDefStarter then
                  ChangeState(dtdInAttributeDefault)
                else begin
                  if (IChar <> UniValueStarter) and (IChar <> UniValueStarter_2) then
                  begin
                    Result := dteInvalidDefination;

                    ExpectedChar := UniValueStarter;
                    ErrorStr := IChar;

                    Exit;
                  end;

                  SetLength(Str, 0);
                  AttrValueDel := IChar;
                  ChangeState(dtdInAttributeDefaultValue);
                end;
              end;
            end;
          end else
          begin
            Str := Str + IChar;
            if InChoiceList and (IChar = UniDTDContentTerminator) then
              InChoiceList := False;
          end;
        end;
        dtdInAttributeDefault:
        begin
          if (IChar = UniDTDDefinationTerminator) or IsWhiteSpace(IChar) then
          begin
            AttributeDefault := GetAttributeDefault(Str);

            if AttributeDefault in [dtdAttrImplied, dtdAttrRequired] then
            begin
              AttrDefault := Copy(Str, 1, Length(Str));
              SetLength(Str, 0);

              DoDTDAttribute(Name, AttrName, AttributeType, AttributeDefault, AttrType,
                '', UniValueStarter, StartLine, StartCol, CanContinue);

              ResetAttribute;

              if not CanContinue then
              begin
                Result := dteUserTerminated;
                Exit;
              end;

              if IsWhiteSpace(IChar) and not LoopWhileSpace then
                Exit;

              if IChar = UniDTDDefinationTerminator then
              begin
                if not NextChar then
                  Exit;

                ChangeState(dtdSpace);
              end else
              begin
                Str := IChar;
                ChangeState(dtdInAttributeName);
              end;
            end else
            if AttributeDefault = dtdAttrFixed then
            begin
              if IsWhiteSpace(IChar) and not LoopWhileSpace then
                Exit;

              if (IChar <> UniValueStarter) and (IChar <> UniValueStarter_2) then
              begin
                Result := dteInvalidEnd;

                ExpectedChar := UniValueStarter;
                ErrorStr := IChar;

                Exit;
              end;

              AttrValueDel := IChar;
              
              AttrDefault := Copy(Str, 1, Length(Str));
              SetLength(Str, 0);

              ChangeState(dtdInAttributeDefaultValue);
            end else
            begin
              SetLineColToErrorStart;
              
              Result := dteInvalidAttributeDefault;
              ErrorStr := Copy(Str, 1, Length(Str));

              Exit;
            end;
          end else
            Str := Str + IChar;
        end;
        dtdInAttributeDefaultValue:
        begin
          if (AttrValueDel <> UniWideNull) and (IChar = AttrValueDel) then
          begin
            AttrValueDel := UniWideNull;

            AttrDefaultValue := Copy(Str, 1, Length(Str));
            SetLength(Str, 0);

            if AttributeType <> dtdEnum then
              SetLength(AttrType, 0);

            DoDTDAttribute(Name, AttrName, AttributeType, AttributeDefault,
              AttrType, AttrDefaultValue, IChar, ALine, ACol, CanContinue);

            ResetAttribute;

            if not CanContinue then
            begin
              Result := dteUserTerminated;
              Exit;
            end;

            if not (NextChar and LoopWhileSpace) then
              Exit;

            if IChar = UniDTDDefinationTerminator then
            begin
              if not NextChar then
                Exit;

              ChangeState(dtdSpace);
            end else
            begin
              Str := IChar;
              ChangeState(dtdInAttributeName);
            end;  
          end else
            Str := Str + IChar;
        end;
      end;
    end;
  finally
    ElementList.Free;

    if (Result = dteNoError) and (St <> dtdSpace) then
    begin
      Result := dteInvalidEnd;
      ErrorStr := IChar;

      if ExpectedChar = UniWideNull then
        Result := dteExpectedCharacterNotFound
      else
      if ExpectedChar = UniSpace then
        Result := dteSpaceRequired;
    end;
  end;
end;

constructor TSCSaxDTDParser.Create;
begin
  inherited Create;
  FErrorOptions := [];
  FContentParser := TSCSaxContentParser.Create;
end;

destructor TSCSaxDTDParser.Destroy;
begin
  FreeAndNil(FContentParser);
  inherited Destroy;
end;

procedure TSCSaxDTDParser.DoDTDComment(const Data: WideString;
  Line, Column: LongInt; var CanContinue: Boolean);
begin
  CanContinue := True;
  if Assigned(FOnDTDComment) then
    FOnDTDComment(Self, Data, Line, Column, CanContinue);
end;

{ TSCElementContentNode }

function TSCElementContentNode.Add: TSCElementContentNode;
begin
  Result := TSCElementContentNode.Create(Self);
end;

function TSCElementContentNode.AsString: WideString;
var
  I: Integer;
  C: WideChar;
begin
  SetLength(Result, 0);

  if FNodes.Count = 0 then
  begin
    if Length(FElement) > 0 then
      Result := FElement;
  end else
  begin
    C := FListOperator;
    if (C = UniWideNull) or (C <> UniDTDChoiceSeperator) then
      C := UniDTDChildSeperator;

    for I := 0 to FNodes.Count-1 do
    begin
      Result := Result + TSCElementContentNode(FNodes.Items[I]).AsString;

      if C = UniDTDChoiceSeperator then
        Result := Result + UniWideSpace;
      Result := Result + C + UniWideSpace;
    end;

    System.Delete(Result, Length(Result) - 1, 2);
    if C = UniDTDChoiceSeperator then
      System.Delete(Result, Length(Result), 1);

    if FIncludeSelf and (Length(Result) > 0) then
      Result := UniDTDContentStarter + Result + UniDTDContentTerminator;
  end;

  if FIncludeSelf and (Length(Result) > 0) then
    case FOccurence of
      dtdElementZeroOrOne:
        Result := Result + UniDTDChildZeroOrOne;
      dtdElementZeroOrMore:
        Result := Result + UniDTDChildZeroOrMore;
      dtdElementOneOrMore:
        Result := Result + UniDTDChildOneOrMore;
    end;
end;

procedure TSCElementContentNode.Clear;
begin
  while Count > 0 do Delete(0);
end;

function TSCElementContentNode.Count: Integer;
begin
  Result := FNodes.Count;
end;

constructor TSCElementContentNode.Create(AOwner: TSCElementContentNode);
begin
  inherited Create;
  FNodes := TList.Create;
  if AOwner <> nil then AOwner.InsertNode(Self);

  FIncludeSelf := True;
  FOccurence := dtdElementOnlyOne;
  FListOperator := UniWideNull;
end;

procedure TSCElementContentNode.Delete(Index: Integer);
var
  N: TSCElementContentNode;
begin
  if (Index > -1) and (Index < FNodes.Count) then
  begin
    N := FNodes.Items[Index];
    FNodes.Delete(Index);

    N.FOwner := nil;
    N.Free;
  end;
end;

destructor TSCElementContentNode.Destroy;
begin
  Clear;
  if FOwner <> nil then FOwner.RemoveNode(Self);

  FreeAndNil(FNodes);
  inherited Destroy;
end;

function TSCElementContentNode.GetNode(Index: Integer): TSCElementContentNode;
begin
  Result := nil;
  if (Index > -1) and (Index < FNodes.Count) then
    Result := FNodes.Items[Index];
end;

procedure TSCElementContentNode.InsertNode(N: TSCElementContentNode);
begin
  if (N <> nil) and (N.FOwner <> Self) then
  begin
    if N.FOwner <> nil then
      N.FOwner.RemoveNode(N);

    N.FOwner := Self;
    FNodes.Add(N);
  end;
end;

procedure TSCElementContentNode.RemoveNode(N: TSCElementContentNode);
begin
  if (N <> nil) and (N.FOwner = Self) then
  begin
    N.FOwner := nil;
    FNodes.Remove(N);
  end;
end;

procedure TSCElementContentNode.SetElement(const Value: WideString);
begin
  FElement := Value;
end;

procedure TSCElementContentNode.SetOwner(Value: TSCElementContentNode);
begin
  if Value <> FOwner then
  begin
    if FOwner <> nil then
      FOwner.RemoveNode(Self);

    if Value <> nil then
      Value.InsertNode(Self);
  end;
end;

{ TSCSaxContentParser }

function TSCSaxContentParser.GetElementOccurrence(
  const ElmName: WideString): TSCDTDElementOccurrence;
begin
  Result := dtdElementOnlyOne;

  if (Length(ElmName) > 0) and (ElmName <> UniDTDPCDATA) then
    Result := GetOccurrence(ElmName[Length(ElmName)]);
end;

function TSCSaxContentParser.GetOccurrence(const C: WideChar): TSCDTDElementOccurrence;
begin
  Result := dtdElementOnlyOne;

  if C = UniDTDChildZeroOrOne then
    Result := dtdElementZeroOrOne
  else
  if C = UniDTDChildZeroOrMore then
    Result := dtdElementZeroOrMore
  else
  if C = UniDTDChildOneOrMore then
    Result := dtdElementOneOrMore;
end;

type
  TSCElementContentParseState = (ecpsSpace, ecpsInName, ecpsInBracket);

function TSCSaxContentParser.ParseContent(const S: Widestring; var RootNode: TSCElementContentNode;
  var ALine, ACol: Integer; var ExpectedChar: WideChar; var ErrorStr: WideString;
  Level: Word): TSCDTDError;
var
  St, PrState: TSCElementContentParseState;
  I, StartLine, StartCol,
  PrevLine, PrevCol, Ln: LongInt;
  Str, Name: WideString;
  IsSpace, IsStop, PCDataStart: Boolean;
  IChar, PrChar, NxChar,
  LastValidChar, ListOperator: WideChar;
  BracketCount, ErrLine, ErrCol, OffsetStr: Integer;
  Occurrence: TSCDTDElementOccurrence;
  ChildNode: TSCElementContentNode;

  function IsWhiteSpace(const AChar: WideChar): boolean;
  begin
    Result := SC_WideCharIsSpace(AChar) or SC_WideCharIsControl(AChar);
  end;

  procedure ArrangeLineCol;
  begin
    if IChar = UniWideLF then
    begin
      PrevCol := ACol;
      ACol := 0;
    end else
      if IChar = UniWideCR then
      begin
        PrevCol  := ACol;
        PrevLine := ALine;

        ACol := 0;
        Inc(ALine);
      end else
      begin
        PrevCol := ACol;
        Inc(ACol);
      end;
  end;

  procedure ArangeLineColWithStr(AStr: WideString);
  var
    P: PWideChar;
    Start, PIndx, PLn: LongInt;
  begin
    P := Pointer(AStr);

    if P = nil then Exit;

    PIndx := 0;
    PLn := Length(AStr);

    while PIndx < PLn do
    begin
      Start := PIndx;

      while not (P^ in [UniWideCR, UniWideLF]) do
      begin
        Inc(P);
        Inc(PIndx);

        Inc(ACol);
        if PIndx >= PLn then
          Exit;
      end;

      PrevCol := ACol + PIndx - Start;
      ACol := 0;

      PrevLine := ALine;
      Inc(ALine);

      if P^ = UniWideCR then
      begin
        Inc(P);
        Inc(PIndx);
      end;

      if PIndx >= PLn then
        Exit;

      if P^ = UniWideLF then
      begin
        Inc(P);
        Inc(PIndx);
      end;
    end;
  end;

  function NextChar: boolean;
  begin
    PrChar := IChar;
    NxChar := UniWideNull;

    if (PrChar <> UniWideNull) and not IsWhiteSpace(PrChar) then
      LastValidChar := PrChar;

    Inc(i);
    Result := i <= Ln;
    if not result then exit;

    IChar := S[i];
    ArrangeLineCol;

    if i + 1 <= Ln then NxChar := S[i + 1];
  end;

  function PrevChar: boolean;
  begin
    NxChar := IChar;
    IChar  := PrChar;
    PrChar := UniWideNull;

    if PrevCol <> ACol - 1 then ALine := PrevLine;
    ACol := PrevCol;

    Dec(i);
    Result := i > 0;
    if i > 1 then PrChar := S[i-1];
  end;

  function LoopWhileSpace: boolean;
  begin
    while i <= Ln do
    begin
      if not IsWhiteSpace(IChar) then Break;

      Result := NextChar;
      if not result then exit;
    end;

    Result := i <= Ln;
  end;

  procedure ChangeState(NewState: TSCElementContentParseState);
  begin
    PrState := St;
    St := NewState;

    StartLine := ALine;
    StartCol  := ACol;

    BracketCount := 0;
  end;

  procedure SetLineColToErrorStart;
  begin
    ALine := StartLine;
    ACol  := StartCol;

    if (ACol > 0) and (PrChar <> UniWideNull) and
      (PrChar <> UniWideCR) and (PrChar <> UniWideLF) then
      Dec(ACol);
  end;

  function AddChildElement(const AName: WideString; var C: TSCElementContentNode): TSCDTDError;
  var
    IsPCData: Boolean;
    AOccurrence: TSCDTDElementOccurrence;
  begin
    Result := dteNoError;
    Name := SC_WideTrim(AName);

    AOccurrence := GetElementOccurrence(Name);
    if AOccurrence <> dtdElementOnlyOne then
      Delete(Name, Length(Name), 1);

    IsPCData := (Name = UniDTDPCDATA) and (Level = 0) and (RootNode.Count = 0) and
      ((ListOperator = UniWideNull) or (ListOperator = UniDTDChoiceSeperator));

    if not (IsPCData or IsValidElementName(Name)) then
    begin
      SetLineColToErrorStart;
      ErrorStr := Copy(Name, 1, Length(Name));

      Result := dteInvalidName;
      Exit;
    end;

    PCDataStart := PCDataStart or IsPCData;

    C := RootNode.Add;

    C.Element := Name;
    C.ListOperator := ListOperator;
    C.Occurence := AOccurrence;
  end;

begin
  Result := dteNoError;

  SetLength(ErrorStr, 0);
  ExpectedChar := UniWideNull;

  ALine := 0;
  ACol  := 0;

  Ln := Length(S);
  if Ln = 0 then
    Exit;

  StartLine := 0;
  StartCol  := 0;

  IChar := UniWideNull;
  PrChar := UniWideNull;
  LastValidChar := UniWideNull;

  PrevLine := -1;
  PrevCol  := 0;

  SetLength(Str, 0);

  St := ecpsSpace;
  PrState := ecpsSpace;

  if RootNode = nil then
    RootNode := TSCElementContentNode.Create(nil);

  I := 0;
  try
    PCDataStart  := False;
    ListOperator := UniWideNull;

    while (I < Ln) and (Result = dteNoError) do
    begin
      if not NextChar then
        Exit;

      // ecpsSpace, ecpsInName, ecpsInBracket
      case St of
        ecpsSpace:
        begin
          ExpectedChar := UniWideNull;

          SetLength(Name, 0);
          if IsWhiteSpace(IChar) and not LoopWhileSpace then
          begin
            if (PrState = ecpsInName) and (LastValidChar <> UniWideNull) then
            begin
              // SetLineColToErrorStart;
              Result := dteNameExpected;
            end;

            Exit;
          end;

          SetLength(Str, 0);
          ChangeState(ecpsInName);
          
          if IChar = UniDTDContentStarter then
          begin
            if PCDataStart then
            begin
              if not NextChar then
                Inc(ACol);

              ErrorStr := IChar;

              Result := dteInvalidNameStart;
              Exit;
            end;
            
            ChangeState(ecpsInBracket);
            Inc(BracketCount);
          end else
          if not IsValidElementChar(IChar, 1) and
            (PCDataStart or (IChar <> UniDTDAttrDefStarter)) then
          begin
            if not NextChar then
              Inc(ACol);

            ErrorStr := IChar;
            Result := dteInvalidNameStart;

            Exit;
          end;

          Str := IChar;
        end;
        ecpsInName:
        begin
          IsSpace := False;
          IsStop := (IChar = UniDTDChoiceSeperator) or (IChar = UniDTDChildSeperator);
          if not IsStop then
            IsSpace := IsWhiteSpace(IChar);

          if IsStop or IsSpace then
          begin
            if IsSpace and LoopWhileSpace then
            begin
              IsStop := (IChar = UniDTDChoiceSeperator) or (IChar = UniDTDChildSeperator);

              if not IsStop then
              begin
                Result := dteInvalidCharacter;

                ErrorStr := IChar;
                ExpectedChar := ListOperator;

                if ListOperator = UniWideNull then
                  ExpectedChar := UniDTDChildSeperator;

                Exit;
              end;
            end;

            if IsStop and (ListOperator = UniWideNull) then
            begin
              ListOperator := IChar;

              if (RootNode.ListOperator <> UniWideNull) and
                (RootNode.ListOperator <> IChar) then
              begin
                ExpectedChar := ListOperator;
                ErrorStr := IChar;

                Result := dteInvalidCharacter;
                Exit;
              end;

              RootNode.ListOperator := IChar;
            end else
            if IsStop and (IChar <> ListOperator) then
            begin
              ExpectedChar := ListOperator;
              ErrorStr := IChar;

              Result := dteInvalidCharacter;
              Exit;
            end;

            Result := AddChildElement(Str, ChildNode);

            if Result = dteNoError then
            begin
              SetLength(Str, 0);
              ChangeState(ecpsSpace);
            end;
          end else
          begin
            if not IsValidElementChar(IChar, Length(Str) + 1) and
              not ((GetOccurrence(IChar) <> dtdElementOnlyOne) and (Length(Str) > 0)) then
            begin
              ErrorStr := IChar;

              Result := dteInvalidName;
              if Length(Str) = 0 then
              begin
                if not NextChar then
                  Inc(ACol);

                Result := dteInvalidNameStart;
              end;

              Exit;
            end;

            Str := Str + IChar;
          end;
        end;
        ecpsInBracket:
        begin
          if IChar = UniDTDContentStarter then
            Inc(BracketCount)
          else
          if IChar = UniDTDContentTerminator then
            Dec(BracketCount);

          if BracketCount = 0 then
          begin
            OffsetStr := 0;

            if Length(Str) > 0 then
            begin
              if Str[1] = UniDTDContentStarter then
              begin
                Delete(Str, 1, 1);
                Inc(OffsetStr);
              end;

              if (Length(Str) > 0) and (Str[Length(Str)] = UniDTDContentTerminator) then
              begin
                Delete(Str, Length(S), 1);
                Inc(OffsetStr);
              end;
            end;

            Name := SC_WideTrim(Str);
            if Length(Name) = 0 then
            begin
              SetLineColToErrorStart;

              Result := dteInvalidName;
              ErrorStr := UniDTDContentStarter + Str + UniDTDContentTerminator;

              Exit;
            end;

            ChildNode := nil;
            Result := ParseContent(Str, ChildNode, ErrLine, ErrCol,
              ExpectedChar, ErrorStr, Level + 1);

            if Result = dteNoError then
            begin
              SetLength(Str, 0);
              ChangeState(ecpsSpace);

              if ChildNode <> nil then
                ChildNode.SetOwner(RootNode);
            end else
            begin
              if ChildNode <> nil then
                ChildNode.Free;

              SetLineColToErrorStart;

              if (ErrLine > -1) and (ErrCol > -1) then
              begin
                if ErrLine = 0 then
                  Inc(ACol, ErrCol + OffsetStr);

                Inc(ALine, ErrLine);
              end;

              Exit;
            end;

            if not NextChar then
              Exit;

            Occurrence := GetOccurrence(IChar);
            if Occurrence <> dtdElementOnlyOne then
            begin
              ChildNode.Occurence := Occurrence;
              if not NextChar then
                Exit;
            end;

            if IsWhiteSpace(IChar) and not LoopWhileSpace then
              Exit;

            ChangeState(ecpsSpace);

            if (IChar <> UniDTDChoiceSeperator) and (IChar <> UniDTDChildSeperator) then
            begin
              Result := dteInvalidCharacter;

              ErrorStr := IChar;
              ExpectedChar := ListOperator;

              if ListOperator = UniWideNull then
                ExpectedChar := UniDTDChildSeperator;

              {if ListOperator = UniWideNull then
              begin
                SetLineColToErrorStart;

                Result := dteInvalidName;
                ErrorStr := Copy(Str, 1, Length(Str)) + UniWideSpace;
              end else
              begin
                Result := dteInvalidCharacter;
                ExpectedChar := ListOperator;
              end;}

              Exit;
            end;
          end else
            Str := Str + IChar;
        end;
      end;
    end;
  finally
    if Result = dteNoError then
    begin
      if St = ecpsInName then
      begin
        Result := AddChildElement(Str, ChildNode);
        
        if Result <> dteNoError then
        begin
          SetLineColToErrorStart;
          ErrorStr := Copy(Str, 1, Length(Str));
        end;
      end else
      if St = ecpsInBracket then
      begin
        Result := dteInvalidEnd;

        ExpectedChar := UniDTDContentTerminator;
        ErrorStr := Copy(Str, 1, Length(Str));
      end;
    end;
  end;
end;

{ TSCDomDTDEntity }

procedure TSCDomDTDEntity.SaveToStream(AStream: TStream; Indent: Integer;
  Normalized, PrettyTrim: Boolean);
var
  Lv: Integer;
  C: WideChar;
  Space, Buf: WideString;
begin
  Lv := Indent;
  if Lv < 0 then Lv := 0;
  Space := StringOfChar(UniWideSpace, Lv);

  C := FDelimiter;
  if (C <> UniValueStarter) and (C <> UniValueStarter_2) then
    C := UniValueStarter;

  Buf := UniWideCRLF + Space + UniDTDDefinationStarter + UniDTDEntity +
    UniWideSpace + FName + UniWideSpace + C + FValue + C +
    UniDTDDefinationTerminator;

  AStream.WriteBuffer(Pointer(Buf)^, 2*Length(Buf));
  SetLength(Buf, 0);
end;

{ TSCDomDTDComment }

function TSCDomDTDComment.GetTagStarter: WideString;
begin
  Result := UniWideCRLF + inherited GetTagStarter;
end;

end.
