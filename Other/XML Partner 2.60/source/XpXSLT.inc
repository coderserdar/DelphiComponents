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
{* XMLPartner Pro: XpXSLT.INC                            *}
{*********************************************************}
{* XMLPartner Pro: Stylesheet element classes            *}
{*********************************************************}

{$I XpDefine.inc}

{$IFDEF Dummy}
unit XpvXSLT;
{$ENDIF}
interface

uses
  Classes,
  XpBase,
  XpDOM,
  XpExcept,
  XpHash,
  XpSort;

type
{====================================================================}
  EXpXSLException = class(EXpException);

{====================================================================}
  TXpDoubleValueType = (xpdvNaN, xpdvDouble,
                        xpdvNegativeInfinity, xpdvNegativeZero,
                        xpdvPositiveZero, xpdvPositiveInfinity);

  TXpImportEvent = procedure(oOwner   : TObject;                       {!!.57}
                         var sDocPath : DOMString) of object;          {!!.57}

  TXpDouble = class
  protected
    FValue : Double;
    FValueType : TXpDoubleValueType;
    procedure SetValue(dValue : Double);
  public
    constructor Create;

    function HasValue : Boolean;
    property Value : Double
      read FValue write SetValue;
  end;
{====================================================================}

  { Forward declarations. }
  TXpBaseXSLElement = class;
  TXpXSLDecimalFormat = class;
  TXpXSLStylesheet = class;
  TXpXSLTemplate = class;
  TXpElementMediator = class;
  TXpXSLAttributeSet = class;
  TXpXSLKey = class;
  TXpXSLOutput = class;
  TXpXSLWhitespaceControl = class;

  TXpDOMStringItem = class
  public
    sString : DOMString;
    Tag : Integer;
  end;

  TXpDOMStringListItem = class(TXpDOMStringItem)
  public
    NextItem : TXpDOMStringListItem;
    PrevItem : TXpDOMStringListItem;
  end;

{====================================================================}
  TXpVarBindings = class
    { This class represents all the local parameters and variables declared
      within a template. A binding item is destroyed when its template has
      finished executing. Variables are added to a binding item as they are
      encountered & validated. }
  protected
    procedure OnDisposeVar(Sender : TXpStrHash; aData : Pointer);
  public
    Bindings : TXpStrHash;
    ForEachItem : TXpVarBindings;
      { Due to the repetitive nature of the xsl:for-each element, the code
        instantiates a concurrent binding for each iteration of the for-each
        loop. Variables declared within the for-each loop go into ForEachItem
        and are removed once the loop iteration has finished. }
    PrevItem : TXpVarBindings;

    constructor Create;
    destructor Destroy; override;

    function GetVarBinding(const sName : DOMString) : TXpValue;
      { Retrieve the value for a specific variable or parameter
        (local or global). Returns an instance of TXpValue if the variable/param
        is declared at this level otherwise returns nil. }
  end;
{====================================================================}

{====================================================================}
  TXpCurNodeItem = class
    { Used to track the current stylesheet node. }
    FNode : TXpNode;

    procedure SetNode(oNode : TXpNode);

  public
    PrevItem : TXpCurNodeItem;

    destructor Destroy; override;

    property Node : TXpNode
      read FNode
      write SetNode;

  end;


  TXpXSLContext = class
    { TXpXSLContext defines the current context of an expression or template. }
  protected
    FCurNode : TXpNode;
      { The current node from the XML document. }
    FCurNodeList : TXpNodeList;
    FCurNodeListStack : TList;
      { Stack of current node lists that is maintained via SaveState &
        RestoreState methods. }
    FCurSLNodeStack : TXpCurNodeItem;
      { Stack of current stylesheet nodes. }
    FCurTemplate : TXpXSLTemplate;
      { The current template being instantiated. }
    FGlobalVars : TXpStrHash;
      { Reference to the bound global variables maintained by the processor.
        Processor is responsible for freeing this info. }
    FLocalVars : TXpVarBindings;
      { Reference to the top-most item in a stack of local variable bindings. }
    FPassedParms : TXpVarBindings;
      { Reference to the top-most item in a stack of passed parameter
        bindings. }
    FPrevContext : TXpXSLContext;
    FUsePrevVars : Boolean;
      { Set this variable to True when an xsl:with-param element is being
        evaluated. This causes variable lookups to occur within the next to
        last variable binding of the current context. We need to do this
        because an xsl:with-param is being evaluated in a new variable
        binding but it may reference variables that are declared in the
        previous variable binding. }
    sMode : DOMString;

    function GetCurSLNode : TXpNode;
{Begin !!.57}
    function SearchBinding(oBinding : TXpVarBindings;
                     const sName : DOMString) : TXpValue;
{End !!.57}
    procedure SetCurNodeList(oNodeList : TXpNodeList);
    procedure SetCurSLNode(oStylesheetNode : TXpNode);
      { Pushes a stylesheet node onto the top of the current stylesheet node
        list. }

  public
    constructor Create;
    destructor Destroy; override;

    procedure DropForEachVarBinding;
    procedure DropParmBinding;
    procedure DropVarBinding;
    function GetVar(const sName : DOMString) : TxpValue;
    procedure MakeForEachVarBinding;
    procedure MakeParmBinding;
    procedure MakeVarBinding;
    procedure PopCurSLNode;
      { Pops the current node off the top of the current stylesheet node list. }
    procedure RestoreCurNodeList;
    procedure SetVar(const sName : DOMString; oValue : TXpValue);

    property CurrentNode : TXpNode
      read FCurNode write FCurNode;
      { The current node from the XML document. }

    property CurrentNodeList : TXpNodeList
      read FCurNodeList write SetCurNodeList;

    property CurrentStylesheetNode : TXpNode
      read GetCurSLNode
      write SetCurSLNode;

    property CurrentTemplate : TXpXSLTemplate
      read FCurTemplate write FCurTemplate;

    property Mode : DOMString
      read sMode write sMode;

    property PrevContext : TXpXSLContext
      read FPrevContext write FPrevContext;

    property GlobalVars : TXpStrHash
      read FGlobalVars write FGlobalVars;

  end;
{====================================================================}

{====================================================================}
{                       PATTERN CLASSES                              }
{====================================================================}
  TXpPatternContext = class
    { The context at the time the pattern is parsed. }
  protected
    FCurSLNode : TXpBaseXSLElement;
    FTokens : TXpNodeList;
  public
    destructor Destroy; override;

    procedure ClearTokens;
      { Removes the current set of tokens. }

    property CurrentSLNode : TXpBaseXSLElement
      read FCurSLNode write FCurSLNode;
      { The current stylesheet node at the time the stylesheet is being
        evaluated. How is this used? An example: If the pattern contains a
        namespace prefix as part of a nametest, the pattern must resolve the
        prefix to a URI. The URI will be resolved based upon the current
        stylesheet element. }

    property Tokens : TXpNodeList
      read FTokens write FTokens;
      { The set of tokens for the parsed pattern. }
  end;
{====================================================================}

{====================================================================}
  TXpBasePattern = class(TXpNode)
    { Abstract class representing a pattern. }
  protected

    FContext : TXpPatternContext;
      { Maintains misc information applicable to all patterns including:
        1. Current stylesheet node; used for namespace URI lookup.
        2. Tokenized form of pattern. }

    procedure Build; virtual;
      { Build a pattern using the tokens in FContext. }

    function CurrentToken : TXpXQLToken;
      { Returns the current token based upon FTokenPos. }

    function GetPriority : Double; virtual;
      { Returns the pattern's default priority. }

    function IsUnion : Boolean;
      { Returns True if the current token is a union. }

    procedure Next;
      { Move to the next token in the token list. Note that position is
        maintained via the FContext.Tokens.Tag property. }

    function Optimize : TXpBasePattern; virtual;
      { Determine if pattern can be reconstructed more efficiently.
        Returns nil if the pattern cannot be optimized or a new pattern
        instance if the pattern can be optimized. }

{Dead code}
//    procedure Require(wType : Integer);
      { Verify the current token is of a specific type. If not, an exception is
        raised. }

    procedure RequireLiteral;
      { Verify the current token is a literal number or string. If not,
        an exception is raised. }

    procedure RequireLeftParen;
      { Verify the current token is a left parenthese. If not, an exception
        is raised. }

    procedure RequireRightParen;
      { Verify the current token is a right parenthese. If not, an exception
        is raised. }

    procedure RequireSlash;
      { Verify the current token is a slash or doubleslash. If not, an
        exception is raised. }

    procedure RequireUnion;
      { Verify the current token is a union ("|"). If not, an exception
        is raised. }

    function TokensLeft : Boolean;
      { Returns True if there are tokens left to process. }

  public

    constructor CreateEx(oContext : TXpPatternContext); virtual;

    function CloneNode(bDeep : Boolean) : TXpNode; override;

    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; virtual;
      { Use this function to determine if a specific node matches a
        pattern. Returns True if the node matches the pattern. The Parse
        method must be called before using this method. }

    property Priority : Double read GetPriority;
      { Return the pattern's default priority. }

  end;
{====================================================================}

{====================================================================}
  TXpPatternClasses = class of TXpBasePattern;

  TXpLocationPathPattern = class(TxpBasePattern)
  protected
    FPriority : TXpDouble;
      { Calculated priority. }
    procedure HandleRelativePath;
    procedure HandleStepPattern;
    procedure Build; override;
    function GetPriority : Double; override;
  public
    constructor CreateEx(oContext : TXpPatternContext); override;
    destructor Destroy; override;
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
  end;
{====================================================================}

{====================================================================}
  TXpIDPattern = class(TxpBasePattern)
  protected
    FParm1 : DOMString;
  public
    property Parm1 : DOMString
      read FParm1 write FParm1;
  end;
{====================================================================}

{====================================================================}
  TXpKeyPattern = class(TxpBasePattern)
  protected
    FParm1 : DOMString;
    FParm2 : DOMString;
  public
    property Parm1 : DOMString
      read FParm1 write FParm1;

    property Parm2 : DOMString
      read FParm2 write FParm2;
  end;
{====================================================================}

{====================================================================}
  TXpRootPattern = class(TXpBasePattern)
    { This represents the repositioning of the match context to the root
      node of the document. }
  protected
  public
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
  end;

  TXpDescendantRootPattern = class(TXpBasePattern);
    { This represents a leading '//' at the beginning of the pattern.
      This pattern always matches. }

  TXpSlashPattern = class(TXpBasePattern);
    { This represents the repositioning of the match context to the
      child elements of the current node. }

  TXpDescendantPattern = class(TXpSlashPattern);
    { This represents the repositioning of the match context to the
      child elements of the current node in anticipation of a descendant
      test. }

  TXpWhichPattern = (xpwpHighest, xpwpLeft, xpwpRight);
    { Tells the TXpOrPattern how to calculate the priority for the
      GetPriority method. Values:

      xpwpHighest - Return the highest priority among the two patterns.
      xpwpLeft - Return the priority of the left-hand alternative.
      xpwpRight - Return the priority of the right-hand alternative. }

  TXpOrPattern = class(TXpBasePattern)
    { Represents the ORing of two or more location path patterns.
      It will have two child elements. Each may be a location path
      pattern or another OR pattern. }
  protected
    FWhichPattern : TXpWhichPattern;

    function GetPriority : Double; override;
      { According to the XSLT spec, Section 5.5, the Or pattrn is to be treated
        equivalently to a set of template rules, one for each alternative.
        This routine determines the priority of the template that matches the
        node. It is based upon the last call to the Matches method. }
  public
    constructor CreateEx(oContext : TXpPatternContext); override;
    procedure Connect(oNodeL, oNodeR : TXpNode); virtual;
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
  end;
{====================================================================}

{====================================================================}
  TXpNodeTest = class(TXpBasePattern)
  protected
    FBlockPredicates : Boolean;
      { Set to True when building node list for evaluation of positional
        predicate. }
    FNodeType : TXpNodeClasses;
  public
    constructor CreateEx(oContext : TXpPatternContext); override;
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
      { The implementation of this method runs through any predicates
        associated with this test. This method should be called by the
        inheriting classes after their matching algorithms have
        evaluated to True. }

    property NodeType : TXpNodeClasses read FNodeType write FNodeType;
      { The type of nodes to match. Applicable to both name & node type
        tests. }
  end;
{====================================================================}

{====================================================================}
  TXpFailTest = class(TXpNodeTest)
    { This class is used in situations where a failure must be forced.
      For example, specifying "@text()" for a pattern. }
  protected
    function GetPriority : Double; override;
  public
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
      { Always returns False. }
  end;
{====================================================================}

{====================================================================}
  TXpNameTest = class(TXpNodeTest)
  protected
    FLocalName : DOMString;
    FNamespaceURI : DOMString;
    function GetPriority : Double; override;
    procedure SetName(const sName : DOMString);
  public
    constructor CreateEx(oContext : TXpPatternContext); override;

    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;

    property Name : DOMString write SetName;
      { Use this property to set the name for the test. It breaks the
        name into a namespaceURI (if specified) and a local name. }
  end;
{====================================================================}

{====================================================================}
  TXpNodeTypeTest = class(TXpNodeTest)
  protected
    function GetPriority : Double; override;
  public
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
  end;
{====================================================================}

{====================================================================}
  TXpPILiteralTest = class(TXpNameTest)
  protected
    function GetPriority : Double; override;
  public
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;
  end;
{====================================================================}

{====================================================================}
  TXpPredicate = class(TXpBasePattern)
    FParentTest : TXpBasePattern;
      { Used for positional predicate. Filled as needed. }
    FPositional : Boolean;
      { If True then predicate contains position() and/or last(). }
    procedure Build; override;
    procedure FindMatchingRFRAME(var wTokenIndex : Integer);
    function IsNthOccurrence(N : Integer;
                             oNode : TXpNode;
                             oContext : TXpNodeList;
                             oParentTest : TXpBasePattern) : Boolean;
      { See if oNode is the Nth occurrence of a particular node type
        within its parent nodes. oParentTest is the pattern that
        identifies how the test is to be conducted (i.e., via node type
        or node name). For example, the test for node()[1] is different
        than the test for para[1]. }
    function Optimize : TXpBasePattern; override;
  public
    destructor Destroy; override;
    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;

    property Positional : Boolean
      read FPositional;
      { Returns True if predicate contains position() and/or last(). }
  end;
{====================================================================}

{====================================================================}
  TXpPattern = class(TXpBasePattern)
    { Represents a pattern specified as an attribute of an XSLT element
      such as xsl:template. It will contain only 1 child node and the
      child node will either be a union pattern or a location path
      pattern. }
  protected
    FTagStr : DOMString;
      { Extra string for developer's use. Used by key manager to store
        the use attribute of a key definition. }
    procedure BuildPattern;
    function GetPriority : Double; override;
  public
    function IsUnionPattern : Boolean;
      { Returns True if the pattern is a union pattern. }

    function Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean; override;

    procedure Parse(const sExpr : DOMString); virtual;
      { Use this function to parse a pattern. Once this method has been
        called, the Matches function may be used to see if a node matches
        the pattern. }

    property TagStr : DOMString
      read FTagStr write FTagStr;
      { Extra string for developer's use. Used by key manager to store
        the use attribute of a key definition. }
  end;
{====================================================================}

{====================================================================}
  TXpPatternMaker = class
  protected
    FContext : TXpPatternContext;
  public

    constructor Create;
    destructor Destroy; override;

    function GeneratePattern(oCurrentStylesheetNode : TXpNode;
                       const sExpr : DOMString) : TXpPattern;

    property Context : TXpPatternContext
      read FContext write FContext;

  end;
{====================================================================}

{====================================================================}
  TxpBaseTemplateManager = class
  protected
  public
    procedure Add(aTemplate : TXpXSLTemplate); virtual;

    procedure Clear; virtual;

    function Match(oNode : TXpElement;
             const sMode : DOMString) : TXpXSLTemplate; virtual;

    function MatchImport(oTemplate : TXpXSLTemplate) : TXpXSLTemplate; virtual;

    function MatchName(const sName : DOMString) : TXpXSLTemplate; virtual;
      { Use this method to find a named template. }

    procedure SetMediator(oMediator : TXpElementMediator;
                          bUsingNS : Boolean); virtual;
      { Set the mediator for the built-in templates. }

  end;
{====================================================================}

{====================================================================}
  TXpDecimalFormatManager = class
    { Manages the default & named decimal formats. The XSL processor
      creates one decimal format manager. }
  protected
    FDefault : TXpXSLDecimalFormat;
      { The default decimal format. }
    FFirstTime : Boolean;
    FHash : TXpStrHash;
      { Contains the named decimal formats. The hash key is the name of
        the decimal format. The hash value is an instance of
        TXpXSLDecimalFormat. }
    FUserDefinedDefault : Boolean;
      { If set to True then FDefault is a user-defined default decimal format.
        If set to False then the FDefault is the default default decimal
        format. }

    procedure CreateDefault;
      { Creates the default default decimal format. }

    procedure OnDisposeFmt(Sender : TXpStrHash; aData : Pointer);
      { Called when FHash is emptied. This method calls TXpNode.Release
        on the TXpXSLDecimalFormat referenced by aData. }

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFormat(oFormat : TXpXSLDecimalFormat);

    procedure Clear;
      { Used to reset the decimal format manager when a new stylesheet
        is loaded. }

    function GetFormat(const sName : DOMString) : TXpXSLDecimalFormat;
      { Find the decimal format instance having the specified name.
        For the default decimal format, set sName to an empty string. }

  end;
{====================================================================}

{====================================================================}
  TXpBaseOutputMgr = class
  protected
    FAttribs : TXpStrHash;
      { Hash table of attribute values. Hash on an attribute name
        & get an instance of TXpDOMStringItem. The sString property
        of TXpDOMStringItem is the attribute's value. The Tag property
        of TXpDOMStringItem points to the xsl:output element.
        This allows us to derive the import precedence
        & to resolve any namespaces used in attribute values (e.g., method
        attribute). }
    FOutputSet : Boolean;

    procedure OnDisposeAttrib(Sender : TXpStrHash; aData : Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAttrib(const sName, sValue : DOMString;
                        oOutputNode : TXpXSLOutput);
      { Add an output attribute to the consolidate list of xsl:output
        attribute values. }

    procedure Clear;

    function BuildFilter : TObject; virtual; abstract;

    property OutputSet : Boolean
      read FOutputSet;
      { Returns True if one or more xsl:output statements set attribute
        values via the output manager. }
  end;
{====================================================================}

{====================================================================}
  TXpNSAliasItem = class
    { Used to track namespace aliases. }
  public
    ResultPrefix : DOMString;
    ImportPrecedence : Integer;
      { Retain the one with the highest import precedence. }
  end;

  TXpNSAliasManager = class
    { Maintains namespace alias information. }
  protected
    FAliases : TXpStrHash;
      { Hash on the stylesheet namespace prefix & get back an instance of
        TXpNSAliasItem. }
    FHaveAliases : Boolean;
      { Set to True if one or more xsl:namespace-alias elements were
        processed. }
    procedure OnDisposeAlias(Sender : TXpStrHash; aData : Pointer);    {!!.54}
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAlias(const sStylePrefix, sResultPrefix : DOMString;
                       const wImpPrecedence : Integer);
      { Add an alias. Parameter wImpPrecedence is the import precedence
        of the stylesheet containing the xsl:namespace-alias element. }

    procedure Clear;

    procedure MapNSNode(var sName, sURI : DOMString;
                            oCurrentSLNode : TXpNode);
      { Maps a given namespace node to its alias prefix and new URI.
        Parameter oCurrentSLNode is the current node in the stylesheet. }

    function MapName(const sName : DOMString) : DOMString;
      { Maps a given prefix and local name to its alias prefix and local
        name. Returns the aliases named. }

    function MapPrefix(oNode : TXpNode) : DOMString;
      { Map the namespace prefix to its alias. If an alias does not exist
        for the specified prefix then this function returns an empty string.
        If the node does not have a namespace prefix then this function
        uses the default namespace for the node.
        Returns:
        If no alias exists then this function returns the node's current
        namespace prefix suffixed with a colon.
        If an alias exists & it is not the default namespace then this
        function returns the new prefix suffixed with a colon.
        If an alias exists & it maps to the default namespace then this
        function returns an empty string. }

    property HaveAliases : Boolean
      read FHaveAliases;
      { Returns True if one or more namespace aliases have been defined. }

  end;
{====================================================================}

{====================================================================}

  TXpNamespaceManager = class
    { Maintains namespace information during the execution of a stylesheet. }
  protected
    FEStack : TXpPointerList;
      { Stack of namespace exclusions.  Each item in the stack is the pointer
        to an instance of TXpDOMString. Each instance of TXpDOMString contains
        one or more excluded namespaces. }
    FNStack : TXpPointerList;
      { Stack of namespace declarations. Each item in the stack is a pointer
        to an instance of TXpNamedNodeMap. }
    FPrefixPrefix : DOMString;
      { Prefix used to generate dynamic prefix names for namespaces. }
    FPrefixSeed : Integer;
      { Number used to generate dynamic prefix names for namespaces. }
    FStylesheet : TXpXSLStylesheet;
      { The stylesheet element owning this manager. }
    FUsing : Boolean;
      { If set to True then namespace management is in effect (i.e., the
        stylesheet is connected to an XML filter). }

    procedure OnDisposeNSItem(Sender : TXpStrHash; aData : Pointer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddExcludes(const sPrefixes : DOMString);
      { Add one or more prefixes, separated by whitespaces, to be excluded
        from final output. }

    procedure AddNS(const sPrefix, sURI : DOMString);
      { Add a namespace to the current level. }

    procedure Clear;
      { Clears current namespace stack information, etc. }

    procedure EndLevel;
      { End the current namespace level. }

    function Excluded(const sPrefix : DOMString) : Boolean;
      { Returns True if the specified namespace prefix is excluded. }

    function GetUniquePrefix : DOMString;
      { Returns a unique prefix taking the current NS stack into account. }

    function InScope(const sPrefix : DOMString) : Boolean;
      { Use this function to determine if a prefix is in scope.
        This method returns True if the specified prefix is within scope.}

    procedure StartLevel;
      { Start a new namespace level. }

    procedure WriteNSNodes(oCurrentSLNode : TXpNode;
                           const sAddtlExcludes : DOMString);          {!!.57}
      { Write the namespace nodes currently in context minus:
        1. Those specified via sAddtlExcludes parameter
        2. Those currently in the excludes stack
        Parameter oCurrentSLNode is the node currently being processed by
        the stylesheet.
      }

    property Stylesheet : TXpXSLStylesheet
      read FStylesheet write FStylesheet;
      { The stylesheet element owning this manager. }

    property Using : Boolean
      read FUsing write FUsing;
      { If set to True then namespace management is in effect (i.e., the
        stylesheet is connected to an XML filter). }

  end;
{====================================================================}

{===TXpAttrSetManager================================================}
  TXpAttrSetManager = class
    { Attribute set manager }
  protected
    FSets : TXpStrHash;
      { Attribute sets hashed by name. Hash on the expanded name of
        an attribute set & get back an instance of TXpPointerList contained
        attribute sets sorted by import precedence. }
    FTagSeed : Integer;
      { Next tag to use for circular reference detection. }

    function CircularRefPrim(oAttrSet : TXpXSLAttributeSet;
                       const wMarker : Integer) : Boolean;
      { Recursive function that performs the dirty work for the circular
        reference check. }

    function CompareSets(oLSet, oRSet : TXpXSLAttributeSet) : Integer;
      { Compare the import precedence of each attribute set. Used when
        adding an attribute set. }

    function GetInsertionPoint(oAttrSet : TXpXSLAttributeSet;
                               oList : TXpPointerList) : Longint;

    procedure OnDisposeSet(Sender : TXpStrHash; aData : Pointer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(oAttrSet : TXpXSLAttributeSet);

    procedure Clear;

    function Get(const sAttrSetName : DOMString) : TXpPointerList;
      { Retrieve the list of attribute sets matching a specific name. }

    function HasCircularRef(const sAttrSetName : DOMString) : Boolean;
      { Returns True if the specified attribute set has a circular reference
        to itself. Should only be called after all xsl:attribute-set
        elements have been preprocessed. Parameter sAttrSetName is the name
        of the attribute set to be checked. }

    procedure MergeSets(oAttrSet : TXpXSLAttributeSet;
                        oContext : TXpXSLContext);
      { Merge the attribute sets referenced by the specified attribute
        set. If the set references two or more attribute sets with the
        same name then the referenced attribute sets are executed from
        lowest import precedence to highest import precedence. }

    procedure MergeSetsViaName(const sAttrSetName : DOMString;         {!!.57}
                               oContext : TXpXSLContext);
      { Merge the attribute sets having the name specified by sAttrSetName. }

  end;
{====================================================================}

{====================================================================}
{                      Key management stuff                          }
{====================================================================}
  TXpKeyDefinition = class
    { Represents a key definition. }
  public
    Match : DOMString;
    Use : DOMString;
  end;

  TXpKeySet = class
    { An instance of this class is created when a key definition is used by
      the stylesheet via the key() function. }
  protected
    FKeysByDoc : TXpIntHash;
      { Generated keys sorted by XML document. Hash on Longint(XMLDocument) &
        get back a hash table containing the keys for that document. Using the
        returned hash table, hash on a key value to get back an instance of
        TXpNodeList containing all nodes that match the key value. }
    FKeyDefs : TXpPointerList;
      { List of key definitions having the same value for their name
        attribute. }

    procedure GenerateKeys(oNode : TXpNode; oPattern : TXpPattern;
                           oStorage : TXpStrHash;
                           oContext : TXpXSLContext);
    function GetKeyDef(const wInx : Integer) : TXpKeyDefinition;
    procedure OnDisposeKeys(Sender : TXpBaseHash; aData : Pointer);
      { Called when freeing FKeysByDoc. }
    procedure OnDisposeKeysForValue(Sender : TXpStrHash; aData : Pointer);
      { Called when freeing a string hash table that is part of FKeysByDoc.
        aData is an instance of TXpNodeList containing references to all
        the nodes that match a specific key value. }
    procedure ProcessNode(oNode : TXpNode; oPattern : TXpPattern;
                          oStorage : TXpStrHash;
                           oContext : TXpXSLContext);

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddKeyDef(oKey : TXpXSLKey);

    function KeyDefCount : Integer;

    function KeysForDoc(oDoc : TXpDocument;
                        oContext : TXpXSLContext) : TXpStrHash;

    property KeyDefs[const wInx : Integer] : TXpKeyDefinition
      read GetKeyDef;
  end;

  TXpKeyManager = class
    { Manages the key definitions for an XSL stylesheet.
      Keys are specific to a source XML document.
      Keys are generated only when a key definition is used for a specific
      document. }
  protected
    FKeys : TXpStrHash;
      { Key definitions hashed by name. Hash on a key name & get an instance
        of TXpKeySet. }
    procedure OnDisposeKeySet(Sender : TXpStrHash; aData : Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddKeyDef(oKey : TXpXSLKey);
    procedure Clear;
    function KeysForDoc(const sKeyName : DOMString;                    {!!.57}
                        oDoc : TXpDocument;
                        oContext : TXpXSLContext) : TXpStrHash;
  end;
{====================================================================}

{====================================================================}
  TXpConflictType = (xpctTemplateMatch,
                     xpctCommentNodes,
                     xpctPINodes,
                     xpctStripPatterns);
    { Conflicts whose resolution may be influenced by the user of the XSL
      processor. Values:
      xpctTemplateMatch - When searching for templates that match a particular
        node, the XSL processor found more than one template matching the
        node with each template having the same import precedence & priority.
      xpctCommentNodes - Instantiating the content of an xsl:comment element
        created nodes other than text nodes.
      xpctPINodes - Instantiating the content of an xsl:processing-instruction
        element created nodes other than text nodes.
      xpctStripPatterns - When trying to determine if an element's whitespace
        nodes should be stripped or preserved, the XSL processor found more
        than one xsl:strip-space and/or xsl:preserve-space directive that
        matched the element's name.
    }

  TXpAttributeEvent = procedure(oOwner : TObject;
                                sName,                        
                                sValue : DOMString) of object;
  TXpResolveConflictEvent = procedure(oOwner : TObject;
                                      oNode : TXpNode;
                                      eEventCode : TXpConflictType;
                                  var bRaiseError : Boolean) of object;

  TXpProcessorState = (xppsIdle, xppsExecuting);

  TXpXSLMessageEvent = procedure(Sender : TObject; const Msg : DOMString;
                                 var Terminate : Boolean) of object;
    { Handler for xsl:message element. }

  TXpValueEvent = procedure(oOwner : TObject;
                            sValue : DOMString) of object;  

{$IFDEF UsingCLX}
  TXpQBaseXSLProcessor = class(TXpComponent)
{$ELSE}
  TXpBaseXSLProcessor = class(TXpComponent)
{$ENDIF}
  protected
    FAttrSetMgr : TXpAttrSetManager;
    FContextTail : TXpXSLContext;
      { The last context on the context stack. The processor maintains a list
        of active contexts so that it may appropriately handle the getting &
        setting of global parameters & variables by the outside application
        during the execution of a stylesheet. }
    FDecimalFmtMgr : TXpDecimalFormatManager;
    FErrors : TStringList;
    FKeyMgr : TXpKeyManager;
    FMediator : TXpElementMediator;
    FNSAliasMgr : TXpNSAliasManager;
    FOnComment : TXpValueEvent;
    FOnImport : TXpImportEvent;                                        {!!.57}
    FOnProcessingInstruction : TXpAttributeEvent;
    FOnResolveConflict : TXpResolveConflictEvent;
    FOnXSLMessage : TXpXSLMessageEvent;
    FOutputMgr : TXpBaseOutputMgr;
    FOverrideFilter : Boolean;
    FRaiseErrors : Boolean;
    FState : TXpProcessorState;
    FTemplateMgr : TXpBaseTemplateManager;

    procedure AddComment(const sComment : DOMString);
      { Adds a comment to the output document via the filter. }

    procedure AddPI(const sName, sValue : DOMString);
      { Adds a processing instruction to the output document via the filter. }

    procedure AddStrip(oContext: TXpXSLContext;
                       oSLNode : TXpXSLWhitespaceControl); virtual;
      { Add one or more element name patterns to the strip manager. }

    procedure ApplyImports(oContext : TXpXSLContext); virtual;
      { Find & apply a template of lower import precedence matching the
        current template's match and mode attributes. }

    procedure CopyAttribute(const sName, sValue : DOMString;
                                  oCurrentSLNode : TXpNode); virtual;
      { Copy an attribute to the result tree. sName is the attribute's
        name & sValue is the attribute's value. oCurrentSLNode is the
        current stylesheet node. If namespace aliasing is in effect then
        oCurrentSLNode will be used by the XSL processor to resolve
        namespace prefixes. }

    procedure CopyCDATANode(oNode : TXpCDATASection); virtual;
      { Copy a CDATA node to the result tree. }

    procedure CopyFO(oNode : TXpNode); virtual;
      { Copy a formatting object to the result tree. }

    procedure CopyText(const sValue : DOMString); virtual;
      { Copy text to the result tree. }

    procedure CopyTextNode(oNode : TXpText); virtual;
      { Copy a text node to the result tree. }

    procedure EndElement(oNode : TxpNode); virtual;
      { End the copy of a literal result element to the result tree. }

    procedure FOEnd; virtual;
      { Notify the filter that we have finished processing a formatting
        object. }

    function GetCurrentResultTreeElement : TXpElement; virtual;
      { Get the current element in the result tree. Used by xsl:attribute. }

    function GetPatternMaker : TXpPatternMaker; virtual;
      { Return the pattern maker instantiated by the stylesheet. }

    function GetStyleURL : DOMString; virtual;
      { Return the URL of the stylesheet. }

    function GetVersion : string;

    function HasFilter : Boolean; virtual;
      { Returns True if a filter is attached to the XSL processor. }

    procedure OnFormatNumber(oOwner : TXpNode;
                       const sNumber, sPattern, sFormat : DOMString;
                         var sFormattedNumber : DOMString);
      { Called from XPath engine when the format-number function is
        encountered. }

    function PopContext : TXpXSLContext;

    function PopFilter : TObject; virtual;
      { Returns the current filter, replacing it with the top filter on the
        filter stack. Calling routine is responsible for freeing the
        returned filter. }

    procedure PushContext(oContext : TXpXSLContext);
    procedure PushFilter(oFilter : TObject); virtual;
      { Pushes the current filter onto the filter stack, replacing it with
        another filter. }

    procedure RaiseXSLMessage(Sender : TObject;
                        const Msg : DOMString;
                          var Terminate : Boolean);
      { Causes the XSL processor to call its OnXSLMessage event handler. }

    procedure ResolveConflict(oNode : TXpNode;
                              eEventCode : TXpConflictType;
                          var bRaiseError : Boolean);
      { Causes the XSL processor to call its OnResolveConflict event handler. }

    procedure SetGlobalParm(const sName : DOMString;
                                 oValue : TXpValue); virtual;
      { Set a global parameter. }

    procedure SetGlobalVar(const sName : DOMString;
                                 oValue : TXpValue); virtual;
      { Set a global variable. }

    procedure SetVersion(const Value : string);

    procedure StartElement(oNode : TxpNode); virtual;
      { Start the copy of a literal result element to the result tree.
        The literal result element's child nodes must be processed.
        EndElement must be called once they are processed. }
  public
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

    property Errors : TStringList
      read FErrors;

    property Mediator : TXpElementMediator
      read FMediator;
      { Returns the element mediator which serves as a connection between
        an XSL processor and the elements in a stylesheet. }

  published

    property OnComment : TXpValueEvent
      read FOnComment
      write FOnComment;

    property OnProcessingInstruction : TXpAttributeEvent
      read FOnProcessingInstruction
      write FOnProcessingInstruction;

    property OnResolveConflict : TXpResolveConflictEvent
      read FOnResolveConflict
      write FOnResolveConflict;

    property OnXSLMessage : TXpXSLMessageEvent
      read FOnXSLMessage
      write FOnXSLMessage;

    property OverrideFilter : Boolean
      read FOverrideFilter
      write FOverridefilter
      default False;
      { If set to True then the filter connected to the XSL processor
        is overridden by the xsl:output statement(s) in the XSL
        stylesheet. }

    property RaiseErrors : Boolean
      read FRaiseErrors
      write FRaiseErrors
      default False;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  end;
{====================================================================}

{====================================================================}
  { TXpElementMediator provides a connection between the XSL processor &
    its stylesheet elements. }
  TXpElementMediator = class
  protected

   FOnImport : TXpImportEvent;                                         {!!.57}

{$IFDEF UsingCLX}
    FXSLProcessor : TXpQBaseXSLProcessor;
{$ELSE}
    FXSLProcessor : TXpBaseXSLProcessor;
{$ENDIF}

    function GetPatternMaker : TXpPatternMaker;

    function GetTemplateMgr : TXpBaseTemplateManager;

  public
{$IFDEF UsingCLX}
    constructor Create(oProcessor : TXpQBaseXSLProcessor);
{$ELSE}
    constructor Create(oProcessor : TXpBaseXSLProcessor);
{$ENDIF}

    procedure AddAttrSet(oAttrSet : TXpXSLAttributeSet);
      { Have the XLS processor manage an attribute set. }

    procedure AddComment(const sComment : DOMString);
      { Adds a comment to the output document via the filter. }

    procedure AddKeyDef(oKey : TXpXSLKey);
      { Adds a key definition to the XSL processor's key manager. }

    procedure AddNSAlias(const sStylePrefix, sResultPrefix : DOMString;
                        const wImpPrecedence : Integer);
      { Adds a namespace alias to the XSL processor's namespace alias
        manager. }

    procedure AddOutputAttribute(const sName, sValue : DOMString;
                                 oOutputNode : TXpXSLOutput);
      { Add an attribute with the specified name & value to the
        output manager. Used by xsl:output element to consolidate
        attributes. }

    procedure AddPI(const sName, sValue : DOMString);
      { Adds a processing instruction to the output document via the filter. }

    procedure AddStrip(oContext: TXpXSLContext; oSLNode : TXpXSLWhitespaceControl);
      { Add one or more element name patterns to the strip manager. }

    procedure AddTemplate(oTemplate : TXpXSLTemplate); virtual;
      { Tell the XSL processor to add a template to its store of templates. }

    procedure ApplyImports(oContext : TXpXSLContext); virtual;
      { Tell the XSL processor to apply a template of lower import precedence
        to the current node. }

    procedure AttrSetMergeSets(oAttrSet : TXpXSLAttributeSet;
                               oContext : TXpXSLContext);
      { Merge the attribute sets referenced by the specified attribute set. }

    procedure AttrSetMergeSetsViaName(const sAttrSet : DOMString;
                                            oContext : TXpXSLContext);
      { Merge the attribute sets referenced bearing the specified attribute
        set name. }

    function AttrSetHasCircularRef(const sAttrSetName : DOMString) : Boolean;
      { Asks the XSL processor's attribute set manager to check for circular
        references for a given attribute set. }

    procedure CopyAttribute(const sName, sValue : DOMString;
                                  oCurrentSLNode : TXpNode); virtual;
      { Copy an attribute to the result tree. }

    procedure CopyCDATANode(oNode : TXpCDATASection); virtual;
      { Copy a CDATA node to the result tree. }

    procedure CopyFO(oNode : TXpNode); virtual;
      { Copy a formatting object to the result tree. }

    procedure CopyText(const sValue : DOMString); virtual;
      { Tell the XSL processor to copy a text string to the result tree. }

    procedure CopyTextNode(oNode : TXpText); virtual;
      { Tell the XSL processor to copy the text node to the result tree. }

    procedure EndElement(oNode : TxpNode); virtual;
      { End the copy of a literal result element to the result tree. }

    function FilterCanBeOverridden : Boolean;
      { Returns True if the filter attached to the XSL processor may
        be overridden by an xsl:output element. }

    procedure FOEnd; virtual;
      { Notify the filter that we have finished processing a formatting
        object. }

    function GetCurrentResultTreeElement : TXpElement; virtual;
      { Get the current element in the result tree. Used by xsl:attribute. }

    function GetDecimalFmtMgr : TXpDecimalFormatManager; virtual;
      { Return the XSL processor's decimal format manager. }

    function GetStyleURL : DOMString; virtual;
      { Return the URL of the stylesheet. }

    function PopContext : TXpXSLContext;

    function PopFilter : TObject; virtual;
      { Returns the current filter, replacing it with the top filter on the
        filter stack. Calling routine is responsible for freeing the
        returned filter. }

    procedure PushContext(oContext : TXpXSLContext);

    procedure PushFilter(oFilter : TObject); virtual;
      { Pushes the current filter onto the filter stack, replacing it with
        another filter. }

    procedure RaiseXSLMessage(Sender : TObject; const Msg : DOMString;
                              var Terminate : Boolean);
      { Causes the XSL processor to call its OnXSLMessage event handler. }

    procedure ResolveConflict(oNode : TXpNode;
                              eEventCode : TXpConflictType;
                          var bRaiseError : Boolean);
      { Causes the XSL processor to call its OnResolveConflict event handler. }

    procedure SetGlobalParm(const sName : DOMString;
                                 oValue : TXpValue); virtual;
      { Set a global parameter. }

    procedure SetGlobalVar(const sName : DOMString;
                                 oValue : TXpValue); virtual;
      { Set a global variable. }

    procedure StartElement(oNode : TxpNode); virtual;
      { Start the copy of a literal result element to the result tree.
        The literal result element's child nodes must be processed.
        EndElement must be called once they are processed. }

    property OnImport : TXpImportEvent                                 {!!.57}
      read FOnImport                                                   {!!.57}
      write FOnImport;                                                 {!!.57}

    property PatternMaker : TXpPatternMaker
      read GetPatternMaker;

    property TemplateMgr : TXpBaseTemplateManager
      read GetTemplateMgr;
  end;
{====================================================================}

{====================================================================}
  { TXpBaseXSLElement is an abstract class defining the foundational
    properties and methods for an XSLT element within a stylesheet. }
  TXpBaseXSLElement = class(TXpElement)
  protected

    FAllowsTemplateBody : Boolean;
      { Set to True if the element may have a template body. }

    FInitialized : Boolean;
      { Set to True if the attributes have been processed. }

    FXSLProcessor : TObject;
      { The XSL processor with which this element is associated. }

    function EvaluateAttrib(oContext : TXpXSLContext;
                            const sAttrib : DOMString) : DOMString; {!!.57}
      { Evaluate an attribute as an attribute value template. }

    procedure ExecuteChildPrim(oNode : TXpNode;
                            oContext : TXpXSLContext); virtual;
      { Primitive method to execute a child node. Called from
        ExecuteChildren. }

    procedure ExecuteChildren(oContext : TXpXSLContext); virtual;
      { Executes the children of the XSLT element. Typically called from the
        element's Execute method. }

    function ExpandName(const sName : DOMString) : DOMString;
      { Expands a QName attribute into its namespace URI and
        local name. }

    function Fallback(oFailedNode : TXpNode;
                      oContext : TXpXSLContext) : Boolean; virtual;
      { This function is called when an element is invalid. This function
        searches for xsl:fallback elements that are children of oFailedNode.
        Each xsl:fallback element found is executed. If at least one
        xsl:fallback element is found then this function returns True otherwise
        it returns False. }

    function GetEffectiveVersion(oNode : TXpNode) : DOMString; virtual;
      { Returns the effective XSLT version number starting with the parent of
        the specified node. The effective version number is the value of
        the xsl:version attribute found on the nearest ancestor or, if
        xsl:version is not found on an ancestor, the value of the xsl:stylesheet
        element's version attribute. }

    function MissingAttrib(const sAttribName : DOMString) : DOMString;
      { Generate an error message string for a missing mandatory attribute. }

    function ParseUseAttrSet(const sUseAttrSet : DOMString) : TXpPointerList;
      { Parse the specified string for the name of one or more attribute sets.
        Used by xsl:attribute-set, xsl:copy, and xsl:element. }

    procedure ProcessAttribPrim(oContext : TXpXSLContext;
                                oAttrib : TXpAttribute;
                                bAttribValTemp : Boolean);
      { Handles the copying of a single attribute to the result tree. }

    procedure ProcessAttribs(oContext       : TXpXSLContext;
                             oLRE           : TXpElement;
                             bAttribValTemp : Boolean); virtual;
      { Process the attributes of a literal result element or a formatting
        object. The attributes will be transferred to the result tree.
        If the attributes may be evaluated as though they are attribute
        value templates, set the bAttribValTemp parameter to True. }

    function Supported(var ErrMsg : string) : Boolean; virtual;
      { This function determines if the element is supported by the XSL
        processor. The base implementation returns True by default meaning
        that the element is supported.
        If the element is not supported then the ErrMsg parameter contains an
        empty string otherwise it contains a string describing why the element
        is not supported. }

    procedure UpdateNSMgr(oContext : TXpXSLContext); virtual;
      { Copies namespace nodes to the namespace manager. This ensures LREs
        are assigned the namespace nodes currently in context. }

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; virtual;
      { This function determines if the element is valid.
        The base implementation returns True by default.
        Concrete classes may  do such things as verify they are a top-level
        element or within a template body. Returns True if the element is valid.
        If the element is not valid then the ErrMsg parameter contains an
        empty string otherwise it contains a string describing why the element
        is invalid. }

  public

    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Execute(oContext : TXpXSLContext); virtual;
      { Executes the XSLT element. }

    function InTemplateBody : Boolean;
      { Returns True if the element is in a template body (i.e., not
        a top-level element). }

    function IsEmpty : Boolean;
      { Returns True if the element does not have any child nodes. }

    function IsTopLevelElement : Boolean;
      { Returns True if the element is a top-level element. }

    function ParentStylesheet : TXpXSLStylesheet;
      { Fetch the parent stylesheet for the current element. }

    procedure Preprocess(oContext : TXpXSLContext); virtual;
      { Called when a stylesheet is first loaded and its top-level elements
        must be analyzed. }

    procedure Reset; virtual;
      { Resets the element so that it's Preprocess method will execute
        again. An example of when this occurs is when the application
        using an XSL processor changes the value of a global variable
        after the stylesheet has already been applied to an XML document. }

    property XSLProcessor : TObject
      read FXSLProcessor
      write FXSLProcessor;
      { The XSL processor with which this element is associated. }
  end;
{====================================================================}

{====================================================================}
  TXpXSLApplyImports = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpSortableXSLElement = class(TXpBaseXSLElement)
    { Base class for XSLT elements that are influenced by xsl:sort elements. }
  protected
    FKeyDefinitions : TXpSortKeyDefinitions;
    FSorter : TXpNodeSorter;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLApplyTemplates = class(TXpSortableXSLElement)
  protected
    FMode : DOMString;
    FSelect : DOMString;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;

    procedure Preprocess(oContext : TXpXSLContext); override;

  end;
{====================================================================}

{====================================================================}
  TXpXSLAttribute = class(TXpBaseXSLElement)
  protected
    FName : DOMString;
    FNamespace : DOMString;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;

    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLAttributeSet = class(TXpBaseXSLElement)
  protected
    FCircChecked : Boolean;
      { Set to True if already performed circular reference check. }
    FName : DOMString;
    FTag : Integer;
      { Marker used to detect circular references. }
    FUsed : TXpPointerList;
      { Linked list of names of used attribute sets. }

    function GetUsed(const wInx : Longint) : DOMString;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;

    function UsedCount : Longint;
      { Return the number of attribute sets referenced by this attribute
        set. }

    property AttrSetName : DOMString
      read FName;

    property Tag : Integer
      read FTag write FTag;

    property UsedAttrSets[const wInx : Longint] : DOMString
      read GetUsed;
  end;
{====================================================================}

{====================================================================}
  TXpXSLCallTemplate = class(TXpBaseXSLElement)
  protected
    FName : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Execute(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLChoose = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Execute(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLComment = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;                                      {!!.57} 
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLCopy = class(TXpBaseXSLElement)
  protected
    FUsed : TXpPointerList;
      { Linked list of names of used attribute sets. }
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLCopyOf = class(TXpBaseXSLElement)
  protected
    FSelect : DOMString;
    procedure CopyNode(oContext : TXpXSLContext;
                       oMediator : TXpElementMediator;
                       oNode : TXpNode);
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLDecimalFormat = class(TXpBaseXSLElement)
  protected
    { Saved element attributes. }
    FDecSep : DOMChar;
    FDigit : DOMChar;
    FGroupSep : DOMChar;
    FInfinity : DOMString;
    FMinusSign : DOMChar;
    FName : DOMString;
    FNaN : DOMString;
    FPattSep : DOMChar;
    FPercent : DOMChar;
    FPerMille : DOMChar;
    FZeroDigit : DOMChar;

    { Values saved for current pattern. }
    FDecPt : Integer;
      { The location of the decimal point within the pattern. }
    FDigLeft : Integer;
      { Number of digits (i.e., # chars) to left of zero digits. }
    FDigRight : Integer;
      { Number of digits (i.e., # chars) to right of zero digits. }
    FDigZero : Integer;
      { Number of zero digits (i.e., 0 chars) in between left digits &
        right digits. }
    FGroupSize : Integer;
      { The size of a group within the formatted number. }
    FLastPattern : DOMString;
      { The last pattern that was formatted. }
    FMaxFracDigits : Integer;
      { Th maximum # of digits that may appear in the fractional portion of
        the formatted number. }
    FMaxIntDigits : Integer;
      { The maximum # of digits that may appear in the integer portion of
        the formatted number. }
    FMinFracDigits : Integer;
      { The minimum # of digits that must appear in the fractional portion of
        the formatted number. }
    FMinIntDigits : Integer;
      { The minimum # of digits that must appear in the integer portion of
        the formatted number. }
    FMultiplier : Integer;
      { The number by which the number is multiplied before being formatted.
        Used when percentage and per mille symbols are present in the pattern. }
    FNegPattern : Boolean;
      { Set to True if a negative pattern was specified. If False then
        when a negative number is encountered, a minus sign is prefixed to
        the formatted number. }
    FNegPrefix : DOMString;
      { Prefix for negative pattern }
    FNegSuffix : DOMString;
      { Suffix for negative pattern }
    FPosPrefix : DOMString;
      { Prefix for positive pattern }
    FPosSuffix : DOMString;
      { Suffix for positive pattern }
    FZeroOffset : Integer;
      { Used to shift numbers into the same range as the zero character. }

    procedure AnalyzePattern(const sPattern : DOMString);
      { Parses the specified pattern, setting internal variables such that
        the pattern will be used on subsequent calls to ApplyPattern. }

    function ApplyPattern(const sNumber : DOMString) : DOMString;
      { Applies the current pattern to the specified number. }

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;

    function FormatNumber(const sNumber, sPattern : DOMString) : DOMString;
      { Format a number using the specified pattern. }

    function IsCompatibleWith(oDecimalFmt : TXpXSLDecimalFormat) : Boolean;
      { Compares the current decimal format instance with the specified
        decimal format instance for compatibility. Compatibility means both
        instances have the same value for all attributes, taking into account
        any default values. Returns True if the instances are compatible
        otherwise returns False. }

    procedure Preprocess(oContext : TXpXSLContext); override;

    property DecimalSeparator : DOMChar
      read FDecSep write FDecSep;

    property Digit : DOMChar
      read FDigit write FDigit;

    property GroupingSeparator : DOMChar
      read FGroupSep write FGroupSep;

    property Infinity : DOMString
      read FInfinity write FInfinity;

    property MinusSign : DOMChar
      read FMinusSign write FMinusSign;

    property Name : DOMString
      read FName write FName;

    property NaN : DOMString
      read FNaN write FNaN;

    property PatternSeparator : DOMChar
      read FPattSep write FPattSep;

    property Percent : DOMChar
      read FPercent write FPercent;

    property PerMille : DOMChar
      read FPerMille write FPerMille;

    property ZeroDigit : DOMChar
      read FZeroDigit write FZeroDigit;

  end;
{====================================================================}

{====================================================================}
  TXpXSLElement = class(TXpBaseXSLElement)
  protected
    FAttrSets : TXpPointerList;
      { Attribute sets referenced by this element. Each item in the
        list is an instance of TXpDOMStringItem. }
    FName : DOMString;
    FNamespace : DOMString;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLFallback = class(TXpBaseXSLElement)
  protected
  public
    constructor Create; override;
    { No special code needed. Uses the base Execute method. }
  end;
{====================================================================}

{====================================================================}
  TXpXSLForEach = class(TXpSortableXSLElement)
    FSelect : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLIf = class(TXpBaseXSLElement)
  protected
    FTest : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLMerger = class(TXpBaseXSLElement)
    { This class contains common functionality for the xsl:include and
      xsl:import elements. }
  protected
    FHref : DOMString;
    function GetImporter : Boolean; virtual;
      { Returns True if this element imports a stylesheet (i.e., xsl:import).
        The base implementation of this method returns False. }
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Preprocess(oContext : TXpXSLContext); override;

    property Importer : Boolean
      read GetImporter;
  end;
{====================================================================}

{====================================================================}
  TXpXSLImport = class(TXpXSLMerger)
  protected
    function GetImporter : Boolean; override;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
  end;
{====================================================================}

{====================================================================}
  TXpXSLInclude = class(TXpXSLMerger)
  public
    { Nothing extra needed. It is all encapsulated in TXpXSLMerger. }
  end;
{====================================================================}

{====================================================================}
  TXpXSLKey = class(TXpBaseXSLElement)
  protected
    FMatch : DOMString;
      { Defines the nodes to which this key is applicable. }
    FName : DOMString;
      { The name of the key. }
    FUse : DOMString;
      { Expression used to determine the value of the key for each
        node obtained by FMatch. }

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Preprocess(oContext : TXpXSLContext); override;

    property Match : DOMString
      read FMatch;
    property Name : DOMString
      read FName;
    property Use : DOMString
      read FUse;
  end;
{====================================================================}

{====================================================================}
  TXpXSLMessage = class(TXpBaseXSLElement)
  protected
    FTerminate : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLNamespaceAlias = class(TXpBaseXSLElement)
  protected
    FStylePrefix : DOMString;
    FResultPrefix : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpNumberFormatType = (xpftSeparator,
                         xpftNumeric,
                         xpftAlphaLower,
                         xpftAlphaUpper,
                         xpftRomanLower,
                         xpftRomanUpper);

  PXpIntegerArray = ^TXpIntegerArray;
  TXpIntegerArray = array[0..99] of Integer;

  TXpNumberToken = class(TXpNode)
    { The TXpXSLNumber.ParseFormat method breaks down the xsl:number format
      string into tokens of type TXpNumberToken. }
    protected
      FNumType : TXpNumberFormatType;
    public
      constructor CreateToken(oNumType : TXpNumberFormatType;
                              const sValue   : DOMString);             {!!.57}

      property NumberType : TXpNumberFormatType
         read FNumType
         write FNumType;
  end;

  TXpXSLNumber = class(TXpBaseXSLElement)
  protected
    FCount : DOMString;
    FFrom : DOMString;
    FFromPattern : TXpBasePattern;
    FHasLastSepToken : Boolean;
      { If True then the last token is a separator. It must be appended
        to all formatted numbers. }
    FLastSepToken : DOMString;
      { The last separator token in the format string. Used in situations
        where there are more numbers than format tokens & also when appending
        the last token to the formatted number (i.e., fewer numbers than
        formatting tokens). }
    FLevel : DOMString;
    FLastFmtToken : TXpNumberToken;
      { The last formatting token in the format string. Used for situation
        where there are more numbers than format token. }
    FValue : DOMString;
    FFormat : DOMString;
    FTokens : TXpNodeList;
    FLang : DOMString;
    FGrouping : Boolean;
      { If True then both the grouping-separator and grouping-size attributes
        were specified. }
    FGroupSep : DOMString;
    FGroupSize : Integer;

    function AsRoman(const wNum : Integer) : DOMString;
      { Converts an integer to a Roman numeral. }

    function FormatNumber(const aValues : TXpIntegerArray;
                          const wCount : Integer) : DOMString;
      { Formats a number using the value of the format attribute. }

    function GetCountPattern(oContext : TXpXSLContext) : TXpBasePattern;
      { Generate a pattern object for the count attribute. }

    function GroupitizeNumber(const sNumber : DOMString) : DOMString;
      { Uses the group separator and group size attributes to group the
        digits within the specified string.

        And yes, Groupitize is not a real word <g> ...}

    procedure HandleAny(oContext : TXpXSLContext;
                        pValues : PXpIntegerArray;
                    var wCnt : Integer);
    procedure HandleMultiple(oContext : TXpXSLContext;
                             pValues : PXpIntegerArray;
                         var wCnt : Integer);
    procedure HandleSingle(oContext : TXpXSLContext;
                           pValues : PXpIntegerArray;
                     const wInx : Integer;
                       var wCnt : Integer);

    function IsFormatToken(const sChar : WideChar) : Boolean;
      { Returns True if the specified character is a format token. }

    function ParseFormat(const sFormat : DOMString) : TXpNodeList;
      { Parses a format string into a set of tokens. }

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    destructor Destroy; override;
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLOtherwise = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLOutput = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLBaseVar = class(TXpBaseXSLElement)
  protected
    FName : DOMString;
    FSelect : DOMString;
    function Evaluate(oContext : TXpXSLContext) : TXpValue; virtual;
      { Evaluate a variable. }
    function IsGlobal : Boolean;
      { Returns True if this is a global variable or parameter (i.e., parent
        node is an instance of TXpXSLStylesheet. }
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    procedure Reset; override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLParam = class(TXpXSLBaseVar)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLWhitespaceControl = class(TXpBaseXSLElement)
    { Common functionality for xsl:preserve-space & xsl:strip-space. }
  protected
    function GetElementNames : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Preprocess(oContext : TXpXSLContext); override;
    function ShouldStrip : Boolean; virtual;
    property ElementNames : DOMString
      read GetElementNames;
  end;
{====================================================================}

{====================================================================}
  TXpXSLPreserveSpace = class(TXpXSLWhitespaceControl)
  protected
  public
  end;
{====================================================================}

{====================================================================}
  TXpXSLProcessingInstruction = class(TXpBaseXSLElement)
  protected
    FName : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLSort = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLStripSpace = class(TXpXSLWhitespaceControl)
  protected
  public
    function ShouldStrip : Boolean; override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLStylesheet = class(TXpBaseXSLElement)
  protected
    FImpPrecedence : Integer;
    FMediator : TXpElementMediator;
    FNSMgr : TXpNamespaceManager;
    FVersion : DOMString;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure EndNSLevel;
      { End a namespace level. }

    function Ignore(oNode : TXpNode) : Boolean; virtual;
      { Determines if the node should be ignored (e.g., the <?xsl?> prolog). }

    procedure Preprocess(oContext : TXpXSLContext); override;

    procedure StartNSLevel;
      { Start a namespace level. }

    property Mediator : TXpElementMediator
      read FMediator
      write FMediator;
      { The object mediating between the XSL processor and the stylesheet
        elements. }

    property ImportPrecedence : Integer
      read FImpPrecedence
      write FImpPrecedence;
      { The import precedence of the stylesheet. The higher the number,
        the higher the import precedence. }

    property NSMgr : TXpNamespaceManager
      read FNSMgr;

    property Version : DOMString
      read FVersion;
      { The value of the stylesheet's version attribute. }
  end;
{====================================================================}

{====================================================================}
  TXpXSLTemplate = class(TXpBaseXSLElement)
  protected
    FMatch : DOMString;
    FMode : DOMString;
    FName : DOMString;
    FPattern : TXpPattern;
    FPriority : Double;

    function GetPatternPriority : Double;
    function GetPrecedence : Integer;
      { Returns the precedence of the parent stylesheet. }
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

  public
    constructor Create; override;

    procedure Clear; override;
      { Gets rid of information that should be regenerated from run-to-run. }

    procedure Execute(oContext : TXpXSLContext); override;

    function IsUnionPattern : Boolean;
      { Returns True if the template's pattern is an instance of TXpOrPattern. }

    function Matches(oNode : TXpElement) : Boolean;
      { Returns True if oNode matches the pattern specified in the
        template's Match attribute. }

    procedure Preprocess(oContext : TXpXSLContext); override;
      { Ensures the template is a top-level element. Analyzes attributes. }

    property Match : DOMString
      read FMatch;
      { The value of the template's match attribute. }

    property Mode : DOMString
      read FMode;
      { The value of the template's mode attribute. }

    property Name : DOMString
      read FName;
      { The value of the template's name attribute. }

    property PatternPriority : Double
      read GetPatternPriority;
      { Used when a template's match attribute is a Union pattern. Allows the
        pattern to return the priority of the matching alternative. }

    property Precedence : integer
      read GetPrecedence;
      { The import precedence of the template. Calculated as the top-level
        elements are processed. }

    property Priority : Double
      read FPriority
      write FPriority;
      { The priority of the template element. May be specified explicitly via
        an attribute or calculated using the algorithm describing in Section 5.5
        of the XSLT specification. }

  end;
{====================================================================}

{====================================================================}
  TXpXSLText = class(TXpBaseXSLElement)
  protected
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLTransform = class(TXpXSLStylesheet)
    { Nothing extra required as this is a synonym for xsl:stylesheet. }
  end;
{====================================================================}

{====================================================================}
  TXpXSLValueOf = class(TXpBaseXSLElement)
    FSelect : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLVariable = class(TXpXSLBaseVar)
  protected
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLWhen = class(TXpBaseXSLElement)
  protected
    FTest : DOMString;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    constructor Create; override;
    function ConditionMet(oContext : TxpXSLContext) : Boolean;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

{====================================================================}
  TXpXSLWithParam = class(TXpXSLBaseVar)
  protected
    function Evaluate(oContext : TXpXSLContext) : TXpValue; override;
      { Evaluate a variable. }
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  public
    procedure Execute(oContext : TXpXSLContext); override;
    procedure Preprocess(oContext : TXpXSLContext); override;
  end;
{====================================================================}

implementation

uses
  SysUtils,
{$IFDEF UsingCLX}
  XpQFlXML,
{$ELSE}
  XpvFlXML,
{$ENDIF}
  XpXSLCon;                                                            

const
  { Decimal format constants }
  csCurrency  = DOMChar($A4);
    { International currency symbol }
  csDecSep    = '.';
  csDigit     = '#';
  csGroupSep  = ',';
  csMinus     = '-';
  csPattSep   = ';';
  csPercent   = '%';
  csPerMille  = '?';
  csPrefixPrefix = 'ns';
    { Used by namespace manager to generate unique prefixes. }
  csQuote     = '''';
  csZeroDigit = '0';

var
  xpoFOPropertyHash : TXpStrHash;

{$I XpXSLMsg.inc}

{===Utility functions================================================}
function XpStripChar(const sStr : DOMString; const wChar : DOMChar;
                     const bLeading, bTrailing : Boolean) : DOMString;
  { Strip a leading and/or trailing character from a string. }
var
  wDest,
  wEnd,
  wSrc : Integer;
begin
  wDest := 1;
  wSrc := 1;

  { Strip trailing? }
  wEnd := Length(sStr);
  if bTrailing then
    while (wEnd > 0) and (sStr[wEnd] = wChar) do
      dec(wEnd);

  if wEnd > 0 then begin
    SetLength(Result, wEnd);
    { Strip leading? }
    if bLeading then
      while (wSrc < wEnd) and (sStr[wSrc] = wChar) do
        inc(wSrc);

    while wSrc <= wEnd do begin
      Result[wDest] := sStr[wSrc];
      inc(wDest);
      inc(wSrc);
    end;

    SetLength(Result, Pred(wDest));

  end
  else
    Result := '';
end;
{--------}
procedure XpAppendChar(const wChar : DOMChar; var sStr : DOMString);
  { Appends a DOMChar to a DOMString. Used because Delphi 3 does not support
    concatenation of WideChar to Widestring. }
var
  wLen : Integer;
begin
  wLen := Length(sStr);
  SetLength(sStr, wLen + 1);
  sStr[wLen + 1] := wChar;
end;
{Begin !!.55}
{--------}
procedure XpRoundNumStr(const sGroupSep, sDecSep : DOMChar;
                        const wGroupSize : Integer;
                          var Result : DOMString);
 { Given the string representation of a number, this function rounds up the
   last digit by one. It carries as necessary. }
var
  bTrackGrp,
  bDone : Boolean;
  wGrpCnt,
  wInx : Integer;
begin
  bDone := False;
  bTrackGrp := False;
  wInx := Length(Result);
  wGrpCnt := 0;
  if Result[wInx] < '9' then
    Result[wInx] := DOMChar(Ord(Result[wInx]) + 1)
  else begin
    repeat
      if (wInx = 0) or                                                 {!!.57}
         (Result[wInx] <> sDecSep) and                                 {!!.57}
         (Result[wInx] <> sGroupSep) then begin
        bDone := (wInx = 0) or (Result[wInx] < '9');
        if (wInx > 0) then begin
          if Result[wInx] < '9' then
            Result[wInx] := DOMChar(Ord(Result[wInx]) + 1)
          else
            Result[wInx] := '0';
          if bTrackGrp then
            inc(wGrpCnt);
        end
        else begin
          { Time for a grouping separator? }
          if bTrackGrp and (wGrpCnt mod wGroupSize = 0) then
            Result := DOMString('1') + DOMString(sGroupSep) + Result
          else
            Result := DOMString('1') + Result;
        end;
      end
      else if (Result[wInx] = sDecSep) then
        bTrackGrp := (wGroupSize > 0);
      dec(wInx);
    until bDone;
  end;  { if }
end;
{====================================================================}

{===TXpDouble========================================================}
constructor TXpDouble.Create;
begin
  inherited;
  FValueType := xpdvNaN;
  FValue := 0.0;
end;
{--------}
function TXpDouble.HasValue : Boolean;
begin
 Result := (FValueType <> xpdvNaN);
end;
{--------}
procedure TXpDouble.SetValue(dValue : Double);
begin
  FValueType := xpdvDouble;
  FValue := dValue;
end;
{====================================================================}

{===TXpXSLNodeListStackItem==========================================}
{ This class is used to store the current node list within the
  stack maintained by TXpXSLContext. This allows situations where
  the stack is nil to be handled without any kludgy logic. }
type
  TXpNodeListStackItem = class
  public
    NodeList : TXpNodeList;
    PrevItem : TxpNodeListStackItem;
  end;
{====================================================================}

{===TXpVarBindings===================================================}
constructor TXpVarBindings.Create;
begin
  inherited;
  Bindings := TXpStrHash.Create(xpc_Size59);
  Bindings.OnDisposeData := OnDisposeVar;
end;
{--------}
destructor TXpVarBindings.Destroy;
begin
  Bindings.Free;
  inherited;
end;
{--------}
function TXpVarBindings.GetVarBinding(const sName : DOMString) : TXpValue;
begin
  Result := Bindings.Get(sName);
end;
{--------}
procedure TXpVarBindings.OnDisposeVar(Sender : TXpStrHash; aData : Pointer);
begin
  TXpValue(aData).Free;
end;
{====================================================================}

{===TXpCurNodeItem===================================================}
destructor TXpCurNodeItem.Destroy;
begin
  FNode.Release;
  inherited;
end;
{--------}
procedure TXpCurNodeItem.SetNode(oNode : TXpNode);
begin
  FNode := oNode;
  FNode.AddRef;
end;
{====================================================================}

{===TXpXSLContext====================================================}
constructor TXpXSLContext.Create;
begin
  inherited Create;
  FPrevContext := nil;
  FCurNodeListStack := TList.Create;
end;
{--------}
destructor TXpXSLContext.Destroy;
var
  wInx : Integer;
  oItem, oPrevItem : TXpCurNodeItem;
  oBinding, oPrevBinding : TXpVarBindings;
begin
  { Make sure any remaining stack items are freed. }
  for wInx := Pred(FCurNodeListStack.Count) downto 0 do
    TXpNodeListStackItem(FCurNodeListStack.Items[wInx]).Free;
  FCurNodeListStack.Free;

  { Make sure any remaining local variable bindings are freed. }
  oBinding := FLocalVars;
  while oBinding <> nil do begin
    oPrevBinding := oBinding.PrevItem;
    oBinding.Free;
    oBinding := oPrevBinding;
  end;

  { Make sure any remaining current stylesheet node items are freed. }
  oItem := FCurSLNodeStack;
  while oItem <> nil do begin
    oPrevItem := oItem;
    oItem.Free;
    oItem := oPrevItem;
  end;

  inherited;
end;
{--------}
procedure TXpXSLContext.DropForEachVarBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := FLocalVars.ForEachItem;
  FLocalVars.ForEachItem := oBinding.PrevItem;
  oBinding.Free;
end;
{--------}
procedure TXpXSLContext.DropParmBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := FPassedParms;
  FPassedParms := oBinding.PrevItem;
  oBinding.Free;
end;
{--------}
procedure TXpXSLContext.DropVarBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := FLocalVars;
  FLocalVars := oBinding.PrevItem;
  oBinding.Free;
end;
{--------}
function TXpXSLContext.GetCurSLNode : TXpNode;
begin
  if FCurSLNodeStack <> nil then
    Result := FCurSLNodeStack.Node
  else
    Result := nil;
end;
{--------}
function TXpXSLContext.GetVar(const sName : DOMString) : TXpValue;
{Rewritten !!.57}
begin
  { Look at the most recent set of variable bindings. If variable not found
    there then look in global variable bindings. }
  Result := nil;
  if FLocalVars <> nil then
    Result := SearchBinding(FLocalVars, sName);

  { Found the variable? }
  if Result = nil then
    { No. Look in global variable bindings. }
    Result := FGlobalVars.Get(sName);
end;
{Begin !!.57}
{--------}
function TXpXSLContext.SearchBinding(oBinding : TXpVarBindings;
                               const sName : DOMString) : TXpValue;
begin
  Assert(oBinding <> nil, 'Binding must be specified.');
  Result := nil;
  { Is a for-each binding in effect? }
  if oBinding.ForEachItem <> nil then
    Result := SearchBinding(oBinding.ForEachItem, sName);

  if Result = nil then
    Result := oBinding.GetVarBinding(sName);

  if (Result = nil) and
     (oBinding.PrevItem <> nil) then
    Result := SearchBinding(oBinding.PrevItem, sName);

end;
{End !!.57}
{--------}
procedure TXpXSLContext.MakeForEachVarBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := TXpVarBindings.Create;
  oBinding.PrevItem := FLocalVars.ForEachItem;
  FLocalVars.ForEachItem := oBinding;
end;
{--------}
procedure TXpXSLContext.MakeParmBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := TXpVarBindings.Create;
  oBinding.PrevItem := FPassedParms;
  FPassedParms := oBinding;
end;
{--------}
procedure TXpXSLContext.MakeVarBinding;
var
  oBinding : TXpVarBindings;
begin
  oBinding := TXpVarBindings.Create;
  oBinding.PrevItem := FLocalVars;
  FLocalVars := oBinding;
end;
{--------}
procedure TXpXSLContext.PopCurSLNode;
var
  oItem : TXpCurNodeItem;
begin
  oItem := FCurSLNodeStack;
  FCurSLNodeStack := oItem.PrevItem;
  oItem.Free;
end;
{--------}
procedure TXpXSLContext.RestoreCurNodeList;
var
  anInx : Integer;
  oStackItem : TXpNodeListStackItem;
begin
  anInx := Pred(FCurNodeListStack.Count);
  oStackItem := FCurNodeListStack.Items[anInx];
  { Note: Do not free the current node list at this point. We don't know if
    it was pulled directly from an element or generated via the select
    attribute of xsl:apply-templates. }
  FCurNodeList := oStackItem.NodeList;
  FCurNodeListStack.Delete(anInx);
  oStackItem.Free;
end;
{--------}
procedure TXpXSLContext.SetCurNodeList(oNodeList : TXpNodeList);
var
  oStackItem : TXpNodeListStackItem;
begin
  oStackItem := TXpNodeListStackItem.Create;
  oStackItem.NodeList := FCurNodeList;
  FCurNodeListStack.Add(oStackItem);
  FCurNodeList := oNodeList;
end;
{--------}
procedure TXpXSLContext.SetCurSLNode(oStylesheetNode : TXpNode);
var
  oItem : TXpCurNodeItem;
begin
  oItem := TXpCurNodeItem.Create;
  oItem.Node := oStylesheetNode;
  oItem.PrevItem := FCurSLNodeStack;
  FCurSLNodeStack := oItem;
end;
{--------}
procedure TXpXSLContext.SetVar(const sName : DOMString; oValue : TXpValue);
begin
  Assert(Assigned(FLocalVars), 'Local variable bindings not established.');
  if FLocalVars.ForEachItem = nil then
    FLocalVars.Bindings.Add(sName, oValue)
  else
    FLocalVars.ForEachItem.Bindings.Add(sName, oValue);
end;
{====================================================================}

{===TXpPatternContext=================================================}
destructor TXpPatternContext.Destroy;
begin
  FTokens.Free;
  inherited;
end;
{--------}
procedure TXpPatternContext.ClearTokens;
begin
  FTokens.Free;
  FTokens := nil;
end;
{=====================================================================}

{===TXpBasePattern====================================================}
constructor TXpBasePattern.CreateEx(oContext : TXpPatternContext);
begin
  inherited Create;
  FContext := oContext;
    { Note that FContext is global & the pattern maker is responsible
      for freeing it. }
end;
{--------}
procedure TXpBasePattern.Build;
begin
  { Do nothing. }
end;
{--------}
function TXpBasePattern.CloneNode(bDeep : Boolean) : TXpNode;
var
  oNode    : TXpBasePattern;
  oNodeClass : TXpPatternClasses;
begin
  oNodeClass := TXpPatternClasses(Self.ClassType);
  oNode := oNodeClass.CreateEx(nil);
  oNode.noNodeType := noNodeType;
  oNode.noNodeName := noNodeName;
  oNode.noNodeValue := noNodeValue;
  oNode.noNodeID := noNodeID;
  oNode.noOwnerDocument := noOwnerDocument;
  oNode.noDefaultNameSpace := noDefaultNameSpace;
  Result := oNode;
  { Never perform a deep copy. }
end;
{--------}
function TXpBasePattern.CurrentToken : TXpXQLToken;
begin
  if FContext.Tokens.Tag < FContext.Tokens.Length then
    Result := TXpXQLToken(FContext.Tokens.Item(FContext.Tokens.Tag))
  else
    Result := nil
end;
{--------}
function TXpBasePattern.GetPriority : Double;
begin
  Result := 0.5;
end;
{--------}
function TXpBasePattern.IsUnion : Boolean;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  Result := ((oToken <> nil) and (oToken.TokenID = TOK_UNION));
end;
{--------}
function TXpBasePattern.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  Result := True;
end;
{--------}
procedure TXpBasePattern.Next;
begin
  FContext.Tokens.Tag := FContext.Tokens.Tag + 1;
end;
{--------}
function TXpBasePattern.Optimize : TXpBasePattern;
var
  oNode, oOptNode : TXpBasePattern;
  wInx : Integer;
begin
  { Assume that child nodes of this node are subpatterns. For each,
    tell it to optimize. If the subpattern returns something other than nil
    then replace it with what it has returned. }
  Result := nil;
  for wInx := 0 to Pred(ChildNodes.Length) do begin
    oNode := TXpBasePattern(ChildNodes.Item(wInx));
    oOptNode := oNode.Optimize;
    if oOptNode <> nil then
      ChildNodes.Replace(wInx, oOptNode);
  end;
end;
{--------}
{ Dead code }
//procedure TXpBasePattern.Require(wType : Integer);
//var
//  oToken : TXpXQLToken;
//begin
//  oToken := CurrentToken;
//  if (oToken = nil) or (oToken.TokenID <> wType) then
//    raise EXpXSLException.Create(Format(sPatternRequireToken,
{ TODO:: Translate token ID to token character }
//                                     [IntToStr(wType),
//                                      QuotedStr(FContext.Tokens.XMLDocument),
//                                      IntToStr(oToken.TokenID)]));
//end;
{--------}
procedure TxpBasePattern.RequireLiteral;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  if (oToken = nil) or
     (not (oToken.TokenID in [TOK_NUMBER, TOK_STRING_TYPE])) then
    raise EXpXSLException.Create(Format(sPatternRequireLiteral,
                                     [QuotedStr(FContext.Tokens.XMLDocument)]));
end;
{--------}
procedure TXpBasePattern.RequireLeftParen;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  if (oToken = nil) or (not (oToken.TokenID = TOK_LPAREN)) then
    raise EXpXSLException.Create(Format(sPatternRequireLeftParen,
                                     [FContext.Tokens.Item(FContext.Tokens.Tag - 1).NodeName]));
end;
{--------}
procedure TXpBasePattern.RequireRightParen;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  if (oToken = nil) or (not (oToken.TokenID = TOK_RPAREN)) then
    raise EXpXSLException.Create(Format(sPatternRequireRightParen,
                                     [FContext.Tokens.Item(FContext.Tokens.Tag - 1).NodeName]));
end;
{--------}
procedure TXpBasePattern.RequireSlash;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  if (oToken = nil) or (not (oToken.TokenID in [TOK_SLASH, TOK_2SLASH])) then
    raise EXpXSLException.Create(Format(sPatternRequireSlash,
                                     [QuotedStr(FContext.Tokens.XMLDocument)]));
end;
{--------}
procedure TXpBasePattern.RequireUnion;
var
  oToken : TXpXQLToken;
begin
  oToken := CurrentToken;
  if (oToken = nil) or (not (oToken.TokenID = TOK_UNION)) then
    raise EXpXSLException.Create(Format(sPatternRequireUnion,
                                     [QuotedStr(oToken.NodeName)]));
end;
{--------}
function TXpBasePattern.TokensLeft : Boolean;
begin
  Result := (FContext.Tokens.Tag < FContext.Tokens.Length);
end;
{=====================================================================}

{===TXpLocationPathPattern============================================}
constructor TXpLocationPathPattern.CreateEx(oContext : TXpPatternContext);
begin
  inherited;
  FPriority := TXpDouble.Create;
end;
{--------}
destructor TXpLocationPathPattern.Destroy;
begin
  FPriority.Free;
  inherited;
end;
{--------}
procedure TXpLocationPathPattern.Build;
var
  oNode : TXpBasePattern;
  oIDTest : TXpIDPattern;
  oKeyTest : TXpKeyPattern;
  oToken : TXpXQLToken;
  sParm1,
  sParm2 : DOMString;
begin
  { [2] LocationPathPattern ::= '/' RelativePathPattern?
                                | IdKeyPattern (('/' | '//') RelativePathPattern)?
                                | '//'? RelativePathPattern
    [3] IdKeyPattern        ::= 'id' '(' Literal ')'
                                | 'key' '(' Literal ',' Literal ')'
  }
  oToken := CurrentToken;
  case oToken.TokenId of
    TOK_SLASH  :
      begin
        Next;
        oNode := TXpRootPattern.CreateEx(FContext);
        AppendChild(oNode);
        oNode.Release;
        { Are there any tokens left? }
        if TokensLeft and (not IsUnion) then
          { Yes. Must be a relative path pattern. }
          HandleRelativePath;
      end;
    TOK_2SLASH :
      begin
        Next;
        oNode := TxpDescendantRootPattern.CreateEx(FContext);
        AppendChild(oNode);
        oNode.Release;
        { Are there any tokens left? }
        if TokensLeft then
          { Yes. Must be a relative path pattern. }
          HandleRelativePath;
      end;
    TOK_ID :
      begin
        Next;
        RequireLeftParen;
        Next;
        { Obtain the ID. }
        RequireLiteral;
        sParm1 := CurrentToken.NodeName;
        Next;
        RequireRightParen;
        Next;
        oIDTest := TXpIDPattern.CreateEx(FContext);
        oIDTest.Parm1 := sParm1;
        AppendChild(oIDTest);
        oIDTest.Release;
        { Any tokens left? }
        if TokensLeft then begin
          { Yes. Should have a location path. }
          oNode := TXpLocationPathPattern.CreateEx(FContext);
          try
            oNode.Build;
          except
            oNode.Free;
            raise;
          end;
          AppendChild(oNode);
          oNode.Release;
        end;
      end;
    TOK_KEY :
      begin
        Next;
        RequireLeftParen;
        Next;
        { Obtain the parameters. }
        RequireLiteral;
        sParm1 := CurrentToken.NodeName;
        Next;
        RequireLiteral;
        sParm2 := CurrentToken.NodeValue;
        Next;
        RequireRightParen;
        Next;
        oKeyTest := TXpKeyPattern.CreateEx(FContext);
        oKeyTest.Parm1 := sParm1;
        oKeyTest.Parm2 := sParm2;
        AppendChild(oKeyTest);
        oKeyTest.Release;
        { Any tokens left? }
        if TokensLeft then begin
          { Yes. Should have a location path. }
          oNode := TXpLocationPathPattern.CreateEx(FContext);
          try
            oNode.Build;
          except
            oNode.Free;
            raise;
          end;
          AppendChild(oNode);
          oNode.Release;
        end;
      end;
    TOK_TRUE..TOK_FALSE,
    TOK_LAST..TOK_POSITION,
    TOK_DOCUMENT..TOK_FUNCTION :
      { It's a disallowed function. Raise an exception. }
      raise EXpXSLException.Create(Format(sLocPathPatternInvalidFunc,
                                     [QuotedStr(oToken.NodeName),
                                      QuotedStr(FContext.Tokens.XMLDocument)]));
    else
      { Handle relative path pattern }
      HandleRelativePath;
  end;  { case }
end;
{--------}
function TXpLocationPathPattern.GetPriority : Double;
begin
  { Priority already calculated? }
  if not FPriority.HasValue then begin
    { No. Is there only one subpattern? }
    if ChildNodes.Length = 1 then
      { Yes. Obtain its priority. }
      FPriority.Value := TXpBasePattern(ChildNodes.Item(0)).GetPriority
    else
      { No. This is a more specific pattern (e.g., /A/B or /A[@name="test"),
        so use the default priority for such situations. }
      FPriority.Value := 0.5;
  end;  { if }

  { Return the calculated priority. }
  Result := FPriority.Value
end;
{--------}
procedure TXpLocationPathPattern.HandleRelativePath;
var
  oNode : TXpBasePattern;
  oToken : TXpXQLToken;
begin
  { [4] RelativePathPattern ::= StepPattern
                                | RelativePathPattern '/' StepPattern
                                | RelativePathPattern '//' StepPattern

    In other words, one or more StepPatterns separated by '/' and/or '//'.
  }
  while TokensLeft do begin
    HandleStepPattern;

    { Any tokens left? }
    if TokensLeft then begin
      { Yes. If next token is a Union then return otherwise the next
        token must be a slash or doubleslash. }
      oToken := CurrentToken;
      if oToken.TokenID = TOK_UNION then
        break;

      RequireSlash;
      { Verified. Add the appropriate object to the pattern list. }
      if oToken.TokenID = TOK_SLASH then begin
        oNode := TXpSlashPattern.CreateEx(FContext);
        AppendChild(oNode);
        oNode.Release;
      end
      else begin
        oNode := TXpDescendantPattern.CreateEx(FContext);
        AppendChild(oNode);
        oNode.Release;
      end;
      Next;
      { At this point, should be positioned to deal with the next StepPattern. }
    end;  { if }
  end;  { while }
end;
{--------}
procedure TXpLocationPathPattern.HandleStepPattern;
var
  oPredicate : TXpPredicate;
  oNodeTest : TXpNodeTest;
  oToken : TXpXQLToken;
begin
  { [5] Step Pattern ::= ChildOrAttributeAxisSpecifier NodeTest Predicate*
    [6] ChildOrAttributeAxisSpecifier ::= AbbreviatedAxisSpecifier
                                          | ('child' | 'attribute') '::'

    ---------------------
    XPath 1.0 productions
    ---------------------
    [7]  NodeTest ::= NameTest
                      | NodeType'('')'
                      | 'processing-instruction''(' Literal ')'
    [13] AbbreviatedAxisSpecifier ::= '@'?
    [38] NodeType    ::=    'comment'
                            | 'text'
                            | 'processing-instruction'
                            | 'node'
  }

  oToken := CurrentToken;

  { Is the next token an invalid axis specifier? }
  if oToken.TokenID in [TOK_AXIS_ANCESTOR,
                        TOK_AXIS_ANCESTOR_OR_SELF,
                        TOK_AXIS_DESCENDANT,
                        TOK_AXIS_DESCENDANT_OR_SELF,
                        TOK_AXIS_FOLLOWING,
                        TOK_AXIS_FOLLOWING_SIBLING,
                        TOK_AXIS_NAMESPACE,
                        TOK_AXIS_PARENT,
                        TOK_AXIS_PRECEDING,
                        TOK_AXIS_PRECEDING_SIBLING,
                        TOK_AXIS_SELF] then
    raise EXpXSLException.Create(Format(sPatternInvalidAxis,
                                     [QuotedStr(oToken.NodeName)]));

  { Is the next token an attribute or child axis specifier? }
  if oToken.TokenID in [TOK_ATTRIBUTE, TOK_AXIS_ATTRIBUTE] then begin
    { Attribute axis. Move to next token. }
    Next;
    oToken := CurrentToken;
    { Handle the node test. }
    case oToken.TokenID of
      TOK_ELEMENT :
        begin
          oNodeTest := TXpNameTest.CreateEx(FContext);
          try
            TXpNameTest(oNodeTest).Name := oToken.NodeName;
            TXpNameTest(oNodeTest).NodeType := TXpAttribute;
          except
            oNodeTest.Free;
            raise;
          end;
          AppendChild(oNodeTest);
          oNodeTest.Release;
        end;
      TOK_WILD, TOK_CMNODE :
        begin
          { Since the name doesn't matter, match on any node that is an
            attribute. }
          oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
          TXpNodeTypeTest(oNodeTest).NodeType := TXpAttribute;
          AppendChild(oNodeTest);
          oNodeTest.Release;
          if oToken.TokenID = TOK_CMNODE then begin
            Next;
            RequireLeftParen;
            Next;
            RequireRightParen;
          end;
        end;
      TOK_CMPI, TOK_CMCOMMENT, TOK_CMTEXTNODE :
        begin
          { These may be specified in the pattern but, since we'll be on the
            attribute axis, they'll never match anything. }
          oNodeTest := TXpFailTest.CreateEx(FContext);
          AppendChild(oNodeTest);
          oNodeTest.Release;
          Next;
          RequireLeftParen;
          Next;
          { Is this a processing-instruction node? }
          if oToken.TokenID = TOK_CMPI then begin
            { Yes. Has a literal been supplied? }
            oToken := CurrentToken;
            if oToken.TokenID <> TOK_RPAREN then begin
              if oToken.TokenID <> TOK_STRING_TYPE then
                raise EXpXSLException.Create(sPatternRequirePILiteral)
              else
                { Yes. Move to next token. Since this situation always
                  fails, we don't need to put in a nametest. }
                Next;
            end;
          end;
          RequireRightParen;
        end;
      else
        raise EXpXSLException.Create(Format(sPatternUnexpectedToken,
                                         [QuotedStr(oToken.NodeName)]));
    end;  { case }
  end
  else begin
    { Child axis. If the child axis was explicitly specified then move to
      the next token. Otherwise we are already at the nodetest. }
    if oToken.TokenID = TOK_AXIS_CHILD then begin
      Next;
      oToken := CurrentToken;
    end;

    { Handle the nodetest. }
    case oToken.TokenID of
      TOK_ELEMENT :
        begin
          oNodeTest := TXpNameTest.CreateEx(FContext);
          try
            TXpNameTest(oNodeTest).Name := oToken.NodeName;
          except
            oNodeTest.Free;
            raise;
          end;
          AppendChild(oNodeTest);
          oNodeTest.Release;
        end;
      TOK_WILD :
        begin
          { Since the name doesn't matter, match on any node that is an
            element. }
          oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
          TXpNodeTypeTest(oNodeTest).NodeType := TXpElement;
          AppendChild(oNodeTest);
          oNodeTest.Release;
        end;
      TOK_CMPI :
        begin
          { Processing instructions can match on a node type & optionally
            a literal element name. }
          Next;
          RequireLeftParen;
          Next;
          { Is a literal element name present? }
          oToken := CurrentToken;
          if oToken.TokenID = TOK_RPAREN then begin
            { No. Create a nodetype test. }
            oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
            TXpNodeTypeTest(oNodeTest).NodeType := TXpProcessingInstruction;
            AppendChild(oNodeTest);
            oNodeTest.Release;
          end
          else begin
            { Yes.  Verify the PI contains a literal value & create a PI
              literal test. }
            if oToken.TokenID = TOK_STRING_TYPE then begin
              oNodeTest := TXpPILiteralTest.CreateEx(FContext);
              try
                TXpNameTest(oNodeTest).Name := oToken.NodeName;
                TXpNameTest(oNodeTest).NodeType := TXpProcessingInstruction;
              except
                oNodeTest.Free;
                raise;
              end;
              AppendChild(oNodeTest);
              oNodeTest.Release;
              Next;
            end
            else
              raise EXpXSLException.Create(sPatternRequirePILiteral);
          end;
          RequireRightParen;
        end;
      TOK_CMCOMMENT :
        begin
          oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
          TXpNodeTypeTest(oNodeTest).NodeType := TXpComment;
          AppendChild(oNodeTest);
          oNodeTest.Release;
          Next;
          RequireLeftParen;
          Next;
          RequireRightParen;
        end;
      TOK_CMTEXTNODE :
        begin
          oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
          TXpNodeTypeTest(oNodeTest).NodeType := TXpText;
          AppendChild(oNodeTest);
          oNodeTest.Release;
          Next;
          RequireLeftParen;
          Next;
          RequireRightParen;
        end;
      TOK_CMNODE :
        begin
          oNodeTest := TXpNodeTypeTest.CreateEx(FContext);
          TXpNodeTypeTest(oNodeTest).NodeType := TXpNode;
          AppendChild(oNodeTest);
          oNodeTest.Release;
          Next;
          RequireLeftParen;
          Next;
          RequireRightParen;
        end;
      else
        raise EXpXSLException.Create(Format(sPatternUnexpectedToken,
                                         [QuotedStr(oToken.NodeName)]));
    end;  { case }
  end;

  { Handle predicates. This code reuses the XPath expression code embedded
    in unit XPDOM, class TXpElement. }
  Next;

  { By this point, variable oNodeTest should have been assigned a node test.
    The predicates will be child nodes of the node test. }
  Assert(oNodeTest <> nil, 'Nodetest not created.');

  { Any predicates? }
  oToken := CurrentToken;
  while (oToken <> nil) and (oToken.TokenID = TOK_LFRAME) do begin
    oPredicate := TXpPredicate.CreateEx(FContext);
    try
      oPredicate.Build;
    except
      oPredicate.Free;
      raise;
    end;
    oNodeTest.AppendChild(oPredicate);
    oPredicate.Release;
    { The predicate should reposition us onto the next token. }
    oToken := CurrentToken;
  end;
end;
{--------}
function TXpLocationPathPattern.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
var
  bAncestorTest,
  bOnlyItem : Boolean;
  oCurNode : TXpNode;
  oStep : TXpBasePattern;
  wInx : Integer;
begin
  Result := True;
  oCurNode := oNode;
  bAncestorTest := False;

  wInx := Pred(ChildNodes.Length);
  bOnlyItem := (wInx = 0);

  { Perform a left to right match. }
  repeat
    oStep := TXpBasePattern(ChildNodes.Item(wInx));
    if oStep is TXpDescendantPattern then begin
      { This pattern is a doubleslash that occurs midway in the pattern.
        The previous pattern will be a node test that must match an ancestor
        of the current node. Position to the parent node in preparation for
        the test.
        Note: This IF statement must occur before the next because
        TXpDescendantPattern descends from TXpSlashPattern. }
      oCurNode := oCurNode.ParentNode;
      bAncestorTest := True;
    end
    else if oStep is TXpSlashPattern then begin
      { This pattern is a backslash. The previous pattern will be a node test
        that must match the parent of the current node. Position to the parent
        node. }
      oCurNode := oCurNode.ParentNode;
      bAncestorTest := False;
    end
    else if oStep is TXpRootPattern then begin
      { Is this the only item in the pattern? }
      if not bOnlyItem then
        { No. We have a pattern like '/root/A'. Position to parent node
          of current node. If we have a pattern '/' then we must make
          sure that we don't alter our position otherwise the match
          will always fail. }
        oCurNode := oCurNode.ParentNode;
      Result := oStep.Matches(oCurNode, oContext);
    end
    else begin
      Result := oStep.Matches(oCurNode, oContext);
      { If we didn't match but we are conducting an ancestor test then
        find out if one of the ancestors matches. }
      if (not Result) and bAncestorTest then
        while (oCurNode <> nil) and (not Result) do begin
          oCurNode := oCurNode.ParentNode;
          if oCurNode <> nil then
            Result := oStep.Matches(oCurNode, oContext);
        end;  { while }
    end;
    dec(wInx);
  until (not Result) or (wInx < 0);
end;
{=====================================================================}

{===TXpOrPattern======================================================}
constructor TXpOrPattern.CreateEx(oContext : TXpPatternContext);
begin
  inherited CreateEx(oContext);
  FWhichPattern := xpwpHighest;
end;
{--------}
procedure TXpOrPattern.Connect(oNodeL, oNodeR : TXpNode);
begin
  AppendChild(oNodeL);
  oNodeL.Release;
  AppendChild(oNodeR);
  oNodeR.Release;
end;
{--------}
function TXpOrPattern.GetPriority : Double;
var
  wTmp : Double;
begin
  Result := 0.5;
  case FWhichPattern of
    xpwpHighest :
      begin
        Result := TXpBasePattern(ChildNodes.Item(0)).GetPriority;
        wTmp := TXpBasePattern(ChildNodes.Item(1)).GetPriority;
        if wTmp > Result then
          Result := wTmp;
      end;
    xpwpLeft :
      Result := TXpBasePattern(ChildNodes.Item(0)).GetPriority;
    xpwpRight :
      Result := TXpBasePattern(ChildNodes.Item(1)).GetPriority;
  end;  { case }
end;
{--------}
function TXpOrPattern.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  FWhichPattern := xpwpHighest;
  Result := TXpBasePattern(ChildNodes.Item(0)).Matches(oNode, oContext);
  if Result then
    FWhichPattern := xpwpLeft
  else begin
    Result := TXpBasePattern(ChildNodes.Item(1)).Matches(oNode, oContext);
    if Result then
      FWhichPattern := xpwpRight;
  end;
end;
{=====================================================================}

{===TXpNodeTest=======================================================}
constructor TXpNodeTest.CreateEx(oContext : TXpPatternContext);
begin
  inherited CreateEx(oContext);
  FBlockPredicates := False;
  FNodeType := TXpElement;
end;
{--------}
function TXpNodeTest.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
var
  anInx,
  anInx2 : Integer;
  aPredicate : TXpPredicate;
  oNodeList : TXpNodeList;
  oParent : TXpNode;
begin
  Result := True;
  { Any predicates to test? }
  if (not FBlockPredicates) then
    for anInx := 0 to Pred(ChildNodes.Length) do begin
      aPredicate := TXpPredicate(ChildNodes.Item(anInx));
      if (anInx = 0) and aPredicate.Positional then begin
        oParent := oNode.ParentNode;
        FBlockPredicates := True;
        oNodeList := TXpNodeList.Create;
        try
          { Build up a node list of elements matching the current test. }
          for anInx2 := 0 to Pred(oParent.ChildNodes.Length) do begin
            if Self.Matches(oParent.ChildNodes.Item(anInx2), nil) then
              oNodeList.Add(oParent.ChildNodes.Item(anInx2));
          end;
          Result := aPredicate.Matches(oNode, oNodeList);
        finally
          FBlockPredicates := False;
          oNodeList.Free;
        end;
      end
      else
        Result := aPredicate.Matches(oNode, oContext);
      if (not Result) then
        Break;
    end;
end;
{=====================================================================}

{===TXpFailTest=======================================================}
function TXpFailTest.GetPriority : Double;
begin
  Result := -0.5;
end;
{--------}
function TXpFailTest.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  Result := False;
end;
{=====================================================================}

{====TXpNameTest======================================================}
constructor TXpNameTest.CreateEx(oContext : TXpPatternContext);
begin
  inherited CreateEx(oContext);
  FLocalName := '';
  FNamespaceURI := '';
end;
{--------}
function TXpNameTest.GetPriority : Double;
begin
  { Priority depends upon the form of the name. If has a predicate then
    0.5 else if like NCName:* then -0.25 otherwise it will be 0. }
  if (ChildNodes.Length > 0) and (ChildNodes.Item(0) is TXpPredicate) then
    Result := 0.5
  else if (FNamespaceURI = '') or (FLocalName <> '*') then
    Result := 0
  else
    { namespace defined & local name is wildcard }
    Result := -0.25;
end;
{--------}
function TXpNameTest.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  { Matches if
    1. The node is an element.
    2. If no namespace prefix was specified then the node must have a null
       namespace URI (default namespace excluded) and the node's local name
       must match the name specified in the test.
    3. If a namespace prefix was specified then the node must have a matching
       namespace URI and the local name specified for the test. }
  Result := (oNode is FNodeType) and
            (((FNamespaceURI = '') and
              (oNode.NamespaceURI = '') and
              (FLocalName = oNode.LocalName)
             ) or
             ((FNamespaceURI = oNode.NamespaceURI) and
              ((FLocalName = '*') or
               (FLocalName = oNode.LocalName)
              )
             )
            );
  if Result then
    Result := inherited Matches(oNode, oContext);
end;
{--------}
procedure TXpNameTest.SetName(const sName : DOMString);
var
  sPrefix : DOMString;
  wInx : Integer;
  wLen : Integer;
begin
  wLen := Length(sName);

  { Is there a prefix? }
  wInx := XpPos(':', sName);
  if wInx > 0 then begin
    { Yes. Break name into prefix & local name. }
    sPrefix := XpCopy(sName, 1, wInx - 1);
    if sPrefix <> '' then begin
      FNamespaceURI := '';
      if FContext.CurrentSLNode <> nil then
        FNamespaceURI := FContext.CurrentSLNode.ResolveNSPrefix(sPrefix);
      if FNamespaceURI = '' then
        raise EXpXSLException.Create(Format(sXSLUnresolvedNSPrefix,
                                         [QuotedStr(sPrefix),
                                          QuotedStr(sName)]));
    end;
    FLocalName := XpCopy(sName, wInx + 1, wLen - wInx);
  end
  else
    { No. Leave prefix blank & set local name. }
    FLocalName := sName;
end;
{=====================================================================}

{====TXpNodeTypeTest==================================================}
function TXpNodeTypeTest.GetPriority : Double;
begin
  if (ChildNodes.Length > 0) and (ChildNodes.Item(0) is TXpPredicate) then
    Result := 0.5
  else
    Result := -0.5;
end;
{--------}
function TXpNodeTypeTest.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  Result := oNode is FNodeType;
  if Result then
    Result := inherited Matches(oNode, oContext);
end;
{=====================================================================}

{====TXpPILiteralTest=================================================}
function TXpPILiteralTest.GetPriority : Double;
begin
  Result := 0
end;
{--------}
function TXpPILiteralTest.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  Result := oNode is FNodeType;
  if Result then
    Result := inherited Matches(oNode, oContext);
end;
{=====================================================================}

{===TXpPredicate======================================================}
type
  TXpExposedElement = class(TXpElement);

destructor TXpPredicate.Destroy;
begin
  if FParentTest <> nil then
    FParentTest.Release;
  inherited;
end;
{--------}
procedure TXpPredicate.Build;
var
  aPos, anInx : Integer;
  oToken : TXpXQLToken;
begin
  { We need to scan through oTokens, adding those tokens for the next predicate
    as children of this instance.
    At this point, we should be positioned on the left brace (i.e., TOK_LFRAME).
    Scan for the corresponding TOK_RFRAME. }
  aPos := FContext.Tokens.Tag;
  FindMatchingRFRAME(aPos);

  { Append the tokens as children of this node. }
  for anInx := FContext.Tokens.Tag to aPos do begin
    oToken := CurrentToken;
    AppendChild(oToken);
    if (oToken.TokenID in [TOK_LAST, TOK_POSITION]) then
      FPositional := True;
    Next;
  end;
end;
{--------}
procedure TXpPredicate.FindMatchingRFRAME(var wTokenIndex : Integer);
var
  aCount : Integer;
  aLevel : Integer;
begin
  aCount := FContext.Tokens.Length;
  aLevel := 0;
  inc(wTokenIndex);
  repeat
    case TXpXqlToken(FContext.Tokens.Item(wTokenIndex)).TokenID of
      TOK_RFRAME :
        if aLevel = 0 then
          Exit
        else
          dec(aLevel);
      TOK_LFRAME :
        inc(aLevel);
    end;  { case }
    inc(wTokenIndex);
  until wTokenIndex = aCount;

  { Could not find matching ']'. }
  raise EXpXSLException.Create(Format(sPatternNoMatchingRFRAME,
                                  [FContext.Tokens.XMLDocument]));
end;
{--------}
function TXpPredicate.IsNthOccurrence(N : Integer;
                                      oNode : TXpNode;
                                      oContext : TXpNodeList;
                                      oParentTest : TXpBasePattern) : Boolean;
var
  wInx : Integer;
  oTmpNode : TXpNode;
begin
  Result := False;
  if N < 1 then
    Exit;
  wInx := 0;
  oTmpNode := oNode.ParentNode.FirstChild;
  while oTmpNode <> nil do begin
    { Is this the same node or does it have the same node name? }
    if (oTmpNode = oNode) or
       oParentTest.Matches(oTmpNode, oContext) then
      { Yes. Increment the counter. }
      inc(wInx);

    { Is this the same node? }
    if (oTmpNode = oNode) then begin
      { Yes. Does the counter match the expected position? }
      Result := (wInx = N);
      exit;
    end
    else
      { Not the same node. See if we have passed the expected position. }
      if (wInx > N) then
        Exit;
    { Move to the next sibling. }
    oTmpNode := oTmpNode.NextSibling;
  end;  { while }
end;
{--------}
function TXpPredicate.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
var
  oElem : TXpExposedElement;
  oList : TXpNodeList;
  oLocalExpType : TXpExpressionType;
  sResult : DOMString;
  wPos : Integer;
begin
  wPos := 0;
  oElem := TXpExposedElement.Create;
//  oElem.noOwnerDocument := oNode.OwnerDocument;
    { A match attribute may not contain a variable reference. This is to prevent
      circular definitions. }
  if oContext = nil then
    oList := TXpNodeList.Create
  else
    oList := oContext;
  try
    if oList <> oContext then
      oList.Add(oNode);
    oLocalExpType := xpetNodeSet;
    sResult := oElem.noParseOrExpr(oList, ChildNodes, wPos, oLocalExpType,
                                   oNode);
    case oLocalExpType of
      xpetNumber:
        begin
          { Copy the parent test without its filter nodes. }
          if FParentTest = nil then
            FParentTest := TXpBasePattern(noParentNode.CloneNode(False));
          Result := IsNthOccurrence(StrToIntDef(sResult, -1), oNode, oContext,
                                    FParentTest);
        end;
      else
        Result := XpConvertBoolean(sResult, oList, oLocalExpType);
    end;  { case }
  finally
    if oContext = nil then
      oList.Free;
    oElem.Free;
  end;
end;
{--------}
function TXpPredicate.Optimize : TXpBasePattern;
begin
  { Do nothing }
  Result := nil;
end;
{=====================================================================}

{===TXpRootPattern====================================================}
function TXpRootPattern.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
begin
  { This pattern matches if the node is the document root. }
  Result := (oNode <> nil) and (oNode = oNode.OwnerDocument);
end;
{=====================================================================}

{===TXpPattern========================================================}
procedure TXpPattern.BuildPattern;
var
  oNodeL,
  oNodeR : TXpBasePattern;
  oNodeOr : TXpOrPattern;
begin
  { [1] Pattern ::= LocationPathPattern
                    | Pattern '|' LocationPathPattern }

  { A pattern always starts off as a location path. }
  oNodeL := TXpLocationPathPattern.CreateEx(FContext);
  try
    oNodeL.Build;

    { If there are other tokens remaining, this must be a union of two or more
      location paths. }
    while TokensLeft do begin
      RequireUnion;
      Next;
      { Build the right-hand side of the union. }
      oNodeR := TXpLocationPathPattern.CreateEx(FContext);
      try
        oNodeR.Build;
        { Create the union & place the left and right-hand side patterns into the
          union. }
        oNodeOr := TXpOrPattern.CreateEx(FContext);
        oNodeOr.Connect(oNodeL, oNodeR);
        { The left-hand side now becomes the union. This positions us to properly
          handle another union. }
        oNodeL := oNodeOr;
      except
        oNodeR.Free;
        raise;
      end;
    end;  { while }
  except
    oNodeL.Free;
    raise;
  end;
  AppendChild(oNodeL);
  oNodeL.Release;
end;
{--------}
function TXpPattern.GetPriority : Double;
begin
  Result := TXpBasePattern(ChildNodes.Item(0)).Priority;
end;
{--------}
function TXpPattern.IsUnionPattern : Boolean;
begin
  if ChildNodes.Length > 0 then
    Result := (ChildNodes.Item(0) is TXpOrPattern)
  else
    Result := False;
end;
{--------}
function TXpPattern.Matches(oNode : TXpNode; oContext : TXpNodeList) : Boolean;
var
  oSubpattern : TXpBasePattern;
  wInx : Integer;
begin
  if oNode = nil then
    Result := False
  else begin
    Result := True;
    for wInx := 0 to Pred(ChildNodes.Length) do begin
      oSubpattern := TXpBasePattern(ChildNodes.Item(wInx));
      Result := oSubpattern.Matches(oNode, oContext);
      if (not Result) then
        Break;
    end;  { for }
  end;
end;
{--------}
procedure TXpPattern.Parse(const sExpr : DOMString);
var
  oXPathParser : TXpXPathParser;
begin
  oXPathParser := TXpXPathParser.Create;
  try
    FContext.Tokens := oXPathParser.Tokenize(sExpr);
    FContext.Tokens.Tag := 0;
    BuildPattern;
    Optimize;
      { Assume that TXpPattern is always implemented to return nil. }
  finally
    oXPathParser.Free;
  end;
end;
{=====================================================================}

{===TXpPatternMaker===================================================}
constructor TXpPatternMaker.Create;
begin
  inherited;
  FContext := TXpPatternContext.Create;
end;
{--------}
destructor TXpPatternMaker.Destroy;
begin
  FContext.Free;
  inherited;
end;
{--------}
function TXpPatternMaker.GeneratePattern(oCurrentStylesheetNode : TXpNode;
                                   const sExpr : DOMString) : TXpPattern;
begin
  Assert((oCurrentStylesheetNode = nil) or
         (oCurrentStylesheetNode is TXpBaseXSLElement));
  FContext.ClearTokens;
  FContext.CurrentSLNode := TXpBaseXSLElement(oCurrentStylesheetNode);
  Result := TXpPattern.CreateEx(FContext);
  try
    Result.Parse(sExpr);
  except
    Result.Free;
    raise;
  end;
end;
{=====================================================================}

{===TXpBaseTemplateManager===========================================}
procedure TXpBaseTemplateManager.Add(aTemplate : TXpXSLTemplate);
begin
  { Do nothing }
end;
{--------}
procedure TXpBaseTemplateManager.Clear;
begin
  { Do nothing }
end;
{--------}
function TXpBaseTemplateManager.Match(oNode : TXpElement;
                                const sMode : DOMString) : TXpXSLTemplate;
begin
  { Do nothing }
  Result := nil;
end;
{--------}
function TXpBaseTemplateManager.MatchImport(oTemplate : TXpXSLTemplate) : TXpXSLTemplate;
begin
  { Do nothing }
  Result := nil;
end;
{--------}
function TXpBaseTemplateManager.MatchName(const sName : DOMString) : TXpXSLTemplate;
begin
  { Do nothing }
  Result := nil;
end;
{--------}
procedure TXpBaseTemplateManager.SetMediator(oMediator : TXpElementMediator;
                                             bUsingNS : Boolean);
begin
  { Do nothing }
end;
{====================================================================}

{===TXpDecimalFormatManager==========================================}
constructor TXpDecimalFormatManager.Create;
begin
  inherited;
  FHash := TXpStrHash.Create(xpc_Size59);
  FFirstTime := True;
    { To force creation of default default. }
  CreateDefault;
end;
{--------}
destructor TXpDecimalFormatManager.Destroy;
begin
  FHash.Free;
  if not FUserDefinedDefault then
    FDefault.Free;
  inherited;
end;
{--------}
procedure TXpDecimalFormatManager.AddFormat(oFormat : TXpXSLDecimalFormat);
var
  oNamedFmt : TXpXSLDecimalFormat;
begin
  { Is this a named or default format? }
  if oFormat.Name = '' then begin
    { Default. Was a user-defined default already specified? }
    if FUserDefinedDefault then begin
      { Yes. Verify the new default is compatible with current default. }
      if not FDefault.IsCompatibleWith(oFormat) then
        raise EXpXSLException.Create(sXSLDefaultDecFmtConflict);
    end
    else
      { No. Free the default default since it is being replaced. }
      FDefault.Free;

    FDefault := oFormat;
    FDefault.AddRef;
    FUserDefinedDefault := True;
  end
  else begin
    { Named. Was a format of the same name previously specified? }
    oNamedFmt := FHash.Get(oFormat.Name);
    if oNamedFmt <> nil then begin
      { Yes. See if the prior format and the new format are compatible. }
      if not oNamedFmt.IsCompatibleWith(oFormat) then
        raise EXpXSLException.Create(Format(sXSLNamedDecFmtConflict,
                                            [QuotedStr(oFormat.Name)]))
      else
        { Compatible. Remove existing format. It will be replaced by the
          new format. }
        FHash.Remove(oFormat.Name);
    end;  { if }

    FHash.Add(oFormat.Name, oFormat);
  end;
end;
{--------}
procedure TXpDecimalFormatManager.Clear;
begin
  FHash.Clear;
  CreateDefault;
end;
{--------}
procedure TXpDecimalFormatManager.CreateDefault;
begin
  { Create a new default default only if we don't have one already. }
  if FFirstTime or FUserDefinedDefault then begin
    if FDefault <> nil then
      FDefault.Release;
    FDefault := TXpXSLDecimalFormat.Create;
    FUserDefinedDefault := False;
    FFirstTime := False;
  end;
end;
{--------}
function TXpDecimalFormatManager.GetFormat(const sName : DOMString) : TXpXSLDecimalFormat;
begin
  if sName = '' then
    Result := FDefault
  else
    Result := FHash.Get(sName);
end;
{--------}
procedure TXpDecimalFormatManager.OnDisposeFmt(Sender : TXpStrHash; aData : Pointer);
begin
  TXpNode(aData).Release;
end;
{====================================================================}

{====================================================================}
constructor TXpBaseOutputMgr.Create;
begin
  inherited;
  FAttribs := TXpStrHash.Create(xpc_Size59);
  FAttribs.OnDisposeData := OnDisposeAttrib;
  FOutputSet := False;
end;
{--------}
destructor TXpBaseOutputMgr.Destroy;
begin
  FAttribs.Free;
  inherited;
end;
{--------}
procedure TXpBaseOutputMgr.AddAttrib(const sName, sValue : DOMString;
                                     oOutputNode : TXpXSLOutput);
var
  oItem : TXpDOMStringItem;
begin
  { Future:: Support cdata-section-elements }
  FOutputSet := True;
  { Does the attribute already exist in the hash table? }
  oItem := FAttribs.Get(sName);
  if oItem <> nil then begin
    { Yes. Does the new value have a higher import precedence? }
    if oOutputNode.ParentStylesheet.ImportPrecedence >=
       TXpXSLOutput(oItem.Tag).ParentStylesheet.ImportPrecedence then begin
      oItem.sString := sValue;
      oItem.Tag := LongInt(oOutputNode);
    end;
  end
  else begin
    { No. Create a new entry in the hash table. }
    oItem := TXpDOMStringItem.Create;
    oItem.sString := sValue;
    oItem.Tag := LongInt(oOutputNode);
    FAttribs.Add(sName, oItem);
  end;
end;
{--------}
procedure TXpBaseOutputMgr.Clear;
begin
  FAttribs.Clear;
  FOutputSet := False;
end;
{--------}
procedure TXpBaseOutputMgr.OnDisposeAttrib(Sender : TXpStrHash; aData : Pointer);
begin
  TXpDOMStringItem(aData).Free;
end;
{====================================================================}

{===TXpNSAliasManager================================================}
constructor TXpNSAliasManager.Create;
begin
  inherited;
  { Note: FAliases created on an as-needed basis. }
  FHaveAliases := False;
end;
{--------}
destructor TXpNSAliasManager.Destroy;
begin
  FAliases.Free;
  inherited;
end;
{--------}
procedure TXpNSAliasManager.AddAlias(const sStylePrefix, sResultPrefix : DOMString;
                                     const wImpPrecedence : Integer);
var
  oItem : TXpNSAliasItem;
begin
  { Alias container created yet? }
  if FAliases = nil then begin
    { No. }
    FAliases := TXpStrHash.Create(xpc_Size59);
    FAliases.OnDisposeData := OnDisposeAlias;                          {!!.54}
    oItem := nil;
  end
  else
    oItem := FAliases.Get(sStylePrefix);

  { Namespace alias previously declared? }
  if oItem = nil then begin
    { No. }
    oItem := TXpNSAliasItem.Create;
    oItem.ResultPrefix := sResultPrefix;
    oItem.ImportPrecedence := wImpPrecedence;
    FAliases.Add(sStylePrefix, oItem);
  end
  else begin
    { Yes. See if have duplicate import precedence or if this
      one has higher import precedence. }
    if oItem.ImportPrecedence = wImpPrecedence then
      raise EXpXSLException.Create(Format(sXSLNSAliasSamePrecedence,
                                          [QuotedStr(sStylePrefix)]))
    else if oItem.ImportPrecedence < wImpPrecedence then
      oItem.ImportPrecedence := wImpPrecedence;
  end;
  FHaveAliases := True;
end;
{--------}
procedure TXpNSAliasManager.Clear;
begin
  if FAliases <> nil then
    FAliases.Clear;
end;
{--------}
procedure TXpNSAliasManager.MapNSNode(var sName, sURI : DOMString;
                                          oCurrentSLNode : TXpNode);
var
  oItem : TXpNSAliasItem;
  sLocalName : DOMString;
  wPos : Integer;
begin
  { Is this a default namespace node? }
  if sName = XpsXMLNS then begin
    { Is there an alias for the default namespace? }
    oItem := FAliases.Get(XpsDefault);
    if (oItem <> nil) and (oItem.ResultPrefix <> XpsDefault) then begin
      { Yes. Change the name to the prefix and get the new URI. }
      sName := oItem.ResultPrefix + ':' + sName;
      sURI := oCurrentSLNode.ResolveNSPrefix(oItem.ResultPrefix);
    end;
  end
  else begin
    { No. Grab the prefix. Assumption: If sName is not 'xmlns' then it
      has the value 'xmlns:*'. }
    wPos := XpPos(':', sName);
    sLocalName := XpCopy(sName, wPos + 1, Length(sName) - wPos);
    oItem := FAliases.Get(sLocalName);
    if oItem <> nil then begin
      { Found an alias. Are we mapping to the default namespace? }
      if oItem.ResultPrefix = XpsDefault then begin
        { Yes. The new name is xmlns and URI should be set to the default
          URI. }
        sName := XpsXMLNS;
        sURI := oCurrentSLNode.DefaultNamespace;
      end
      else begin
        { No. Update name and URI. }
        sName := XpsXMLNS + ':' + oItem.ResultPrefix;
        sURI := oCurrentSLNode.ResolveNSPrefix(oItem.ResultPrefix);
      end;
    end;
  end;
end;
{--------}
function TXpNSAliasManager.MapName(const sName : DOMString) : DOMString;
var
  oItem : TXpNSAliasItem;
  sLocalName : DOMString;
  wPos : Integer;
begin
  { By default, return the name as passed to this method. }
  Result := sName;
  { Does the name have a prefix? }
  wPos := XpPos(':', sName);
  if wPos > 0 then begin
    { Yes. See if there is an alias for the prefix. }
    sLocalName := XpCopy(sName, wPos + 1, Length(sName) - wPos);
    oItem := FAliases.Get(XpCopy(sName, 1, wPos - 1));
    if oItem <> nil then begin
      { Found an alias. If it maps to #default then return the local name
        otherwise return the new prefix plus the local name. }
      if oItem.ResultPrefix = XpsDefault then
        Result := sLocalName
      else
        Result := oItem.ResultPrefix + ':' + sLocalName;
    end;
  end
  else begin
    { No. See if there is an alias for the default namespace. }
    oItem := FAliases.Get(XpsDefault);
    if (oItem <> nil) and (oItem.ResultPrefix <> XpsDefault) then
      { Found a non-default prefix. Return it prefixed to the name. }
      Result := oItem.ResultPrefix + ':' + sName;
  end;
end;
{--------}
function TXpNSAliasManager.MapPrefix(oNode : TXpNode) : DOMString;
var
  oItem : TXpNSAliasItem;
  sPrefix : DOMString;
begin
  { Does this node have a prefix? }
  sPrefix := oNode.Prefix;
  Result := sPrefix;
    { Will be a prefix or empty string. }
  if sPrefix = '' then
    { No. Use #default as the key. }
    sPrefix := XpsDefault;

  oItem := FAliases.Get(sPrefix);
  if oItem <> nil then
    if oItem.ResultPrefix = XpsDefault then
      Result := ''
    else
      Result := oItem.ResultPrefix;

  if Result <> '' then
    Result := Result + ':';
end;
{Begin !!.54}
{--------}
procedure TXpNSAliasManager.OnDisposeAlias(Sender : TXpStrHash; aData : Pointer);
begin
  TXpNSAliasItem(aData).Free;
end;
{End !!.54}
{====================================================================}

{===TXpNamespaceManager==============================================}
constructor TXpNamespaceManager.Create;
begin
  inherited;
  FEStack := TXpPointerList.Create;
  FNStack := TXpPointerList.Create;
  FPrefixPrefix := csPrefixPrefix;
  FPrefixSeed := 1;
  FUsing := True;
  StartLevel;
end;
{--------}
destructor TXpNamespaceManager.Destroy;
begin
  Clear;
  FNStack.Free;
  FEStack.Free;
  inherited;
end;
{--------}
procedure TXpNamespaceManager.AddExcludes(const sPrefixes : DOMString);
var
  oItem : TXpDOMStringItem;
begin
  oItem := TXpDOMStringItem(FEStack.Pointers[Pred(FEStack.Count)]);
  oItem.sString := oItem.sString + ' ' + sPrefixes + ' ';
end;
{--------}
procedure TXpNamespaceManager.AddNS(const sPrefix, sURI : DOMString);
var
  oMap : TXpNamedNodeMap;
  oNode : TXpAttribute;
begin
  oMap := TXpNamedNodeMap(FNStack.Pointers[Pred(FNStack.Count)]);
  oNode := TXpAttribute.Create;
  oNode.NodeName := sPrefix;
  oNode.NodeValue := sURI;
  oMap.Add(oNode);
  oNode.Release;
end;
{--------}
procedure TXpNamespaceManager.Clear;
var
  wInx : Integer;
begin
  for wInx := Pred(FNStack.Count) downto 0 do begin
    TXpNamedNodeMap(FNStack.Pointers[wInx]).Free;
    FNStack.RemoveAt(wInx);
  end;

  for wInx := Pred(FEStack.Count) downto 0 do begin
    TXpDOMStringItem(FEStack.Pointers[wInx]).Free;
    FEStack.RemoveAt(wInx);
  end;

  FPrefixPrefix := csPrefixPrefix;
  FPrefixSeed := 1;
end;
{--------}
procedure TXpNamespaceManager.EndLevel;
begin
  TXpNamedNodeMap(FNStack.Pointers[Pred(FNStack.Count)]).Free;
  FNStack.RemoveAt(Pred(FNStack.Count));
  TXpDOMStringItem(FEStack.Pointers[Pred(FEStack.Count)]).Free;        {!!.54}
  FEStack.RemoveAt(Pred(FEStack.Count));
end;
{--------}
function TXpNamespaceManager.Excluded(const sPrefix : DOMString) : Boolean;
var
  oItem : TXpDOMStringItem;
  sKey : DOMString;
  wInx : Integer;
begin
  Result := (sPrefix = XpsXSL);
  if not Result then begin
    if sPrefix = '' then
      sKey := ' ' + XpsDefault + ' '
    else
      sKey := ' ' + sPrefix + ' ';
    for wInx := Pred(FEStack.Count) downto 0 do begin
      oItem := TXpDOMStringItem(FEStack.Pointers[wInx]);
      Result := XpPos(sKey, oItem.sString) > 0;
      if Result then
        break;
    end;
  end;
end;
{--------}
function TXpNamespaceManager.GetUniquePrefix : DOMString;
begin
  while InScope(FPrefixPrefix) do
    FPrefixPrefix := FPrefixPrefix + 'x';
  Result := FPrefixPrefix + IntToStr(FPrefixSeed);
  inc(FPrefixSeed);
end;
{--------}
function TXpNamespaceManager.InScope(const sPrefix : DOMString) : Boolean;
var
  oList : TXpNamedNodeMap;
  wInx : Integer;
begin
  Result := False;
  for wInx := 0 to Pred(FNStack.Count) do begin
    oList := TXpNamedNodeMap(FNStack.Pointers[wInx]);
    Result := (oList.GetNamedItem(sPrefix) <> nil);
    if Result then
      Break;
  end;  { for }
end;
{--------}
procedure TXpNamespaceManager.OnDisposeNSItem(Sender : TXpStrHash;
                                              aData : Pointer);
begin
  TXpNSAliasItem(aData).Free;
end;
{--------}
procedure TXpNamespaceManager.StartLevel;
begin
  { Append an empty node map to FNStack. }
  FNStack.Append(TXpNamedNodeMap.Create);

  { Append an empty item to FEStack. }
  FEStack.Append(TXpDOMStringItem.Create);
end;
{--------}
procedure TXpNamespaceManager.WriteNSNodes(oCurrentSLNode : TXpNode;
                                           const sAddtlExcludes : DOMString); {!!.57}
var
  oNS : TXpNode;
  oList : TXpNamedNodeMap;
  sAttrName : DOMString;
  wInx,
  wInx2 : Integer;
begin
  for wInx := 0 to Pred(FNStack.Count) do begin
    oList := TXpNamedNodeMap(FNStack.Pointers[wInx]);
    for wInx2 := 0 to Pred(oList.Length) do begin
      oNS := TXpNode(oList.Item(wInx2));
      if (oNS.Tag = 0) and (not Excluded(oNS.LocalName)) then begin
        if oNS.NodeName = '' then
          sAttrName := XpsXmlns
        else
          sAttrName := XpsXmlns + ':' + oNS.NodeName;
        FStylesheet.Mediator.CopyAttribute(sAttrName, oNS.NodeValue,
                                           oCurrentSLNode);
        oNS.Tag := 1;
      end;  { if }
    end;  { for }
  end;  { for }
end;
{====================================================================}

{===TXpAttrSetManager================================================}
constructor TXpAttrSetManager.Create;
begin
  inherited;
  FSets := TXpStrHash.Create(xpc_Size59);
  FSets.OnDisposeData := OnDisposeSet;
  FTagSeed := 1;
end;
{--------}
destructor TXpAttrSetManager.Destroy;
begin
  FSets.Free;
  inherited;
end;
{--------}
procedure TXpAttrSetManager.Add(oAttrSet : TXpXSLAttributeSet);
var
  oList : TXpPointerList;
  wInx : Longint;
begin
  { Do we have an entry for the attribute set's name? }
  oList := FSets.Get(oAttrSet.AttrSetName);
  if oList = nil then begin
    { No. Create an entry for this mode. }
    oList := TXpPointerList.Create;
    oList.Append(Pointer(oAttrSet));
    FSets.Add(oAttrSet.AttrSetName, oList);
  end
  else begin
    { Yes. The sets are sorted by import precedence. Add this
      template into the correct position within the list. }
    wInx := GetInsertionPoint(oAttrSet, oList);
    oList.Insert(Pointer(oAttrSet), wInx);
  end;
end;
{--------}
procedure TXpAttrSetManager.Clear;
begin
  FSets.Clear;
end;
{--------}
function TXpAttrSetManager.CompareSets(oLSet, oRSet : TXpXSLAttributeSet) : Integer;
var
  wLPrec,
  wRPrec : Integer;
begin
  wLPrec := oLSet.ParentStylesheet.ImportPrecedence;
  wRPrec := oRSet.ParentStylesheet.ImportPrecedence;
  if (wLPrec = wRPrec) then
    Result := 0
  else if (wLPrec < wRPrec) then
    Result := -1
  else
    Result := 1;
end;
{--------}
function TXpAttrSetManager.GetInsertionPoint(oAttrSet : TXpXSLAttributeSet;
                                             oList : TXpPointerList) : Longint;
var
  L, R, M : Longint;
  oItem : TXpXSLAttributeSet;
  wResult : Integer;
begin
  if oList.Count = 0 then
    Result := 0
  else begin
    { The attribute set needs to be positioned in the list based upon its
      import precedence. }
    L := 0;
    R := Pred(oList.Count);
    repeat
      M := (L + R) div 2;
      oItem := TXpXSLAttributeSet(oList.Pointers[M]);
      wResult := CompareSets(oItem, oAttrSet);
      if wResult = 0 then
        { Equivalent. Raise an error. }
        raise EXpXSLException.Create
                (Format(sXSLAttrSetSamePrec,
                        [QuotedStr(TXpXSLAttributeSet(oList.Pointers[M]).AttrSetName)]))
      else if wResult < 0 then
        R := M - 1
      else
        L := M + 1;
    until L > R;
    Result := L;
  end;
end;
{--------}
function TXpAttrSetManager.Get(const sAttrSetName : DOMString) : TXpPointerList;
begin
  Result := FSets.Get(sAttrSetName);
end;
{--------}
function TXpAttrSetManager.CircularRefPrim(oAttrSet : TXpXSLAttributeSet;
                                     const wMarker : Integer) : Boolean;
var
  oList : TXpPointerList;
  oNextName : DOMString;
  oNextSet : TXpXSLAttributeSet;
  wInx : Integer;
begin
  { Did we come across a breadcrumb? }
  Result := (oAttrSet.Tag = wMarker);
  if not Result then begin
    { No. Make breadcrumb trails on referenced attribute sets. }
    oAttrSet.Tag := wMarker;
    for wInx := 0 to Pred(oAttrSet.UsedCount) do begin
      { Get the next used attribute set. }
      oNextName := oAttrSet.UsedAttrSets[wInx];
      { Get the first attribute set instance associated with that name. }
      oList := Get(oNextName);
      { Did we find any attribute sets matching the specified name? }
      if oList = nil then
        { No. Raise an error as each entry in the use-attribute-sets
          property must refer to an actual xsl:attribute-set element. }
        raise EXpXSLException.Create(Format(sXSLAttrSetBadRef,
                                            [QuotedStr(oAttrSet.AttrSetName),
                                             QuotedStr(oNextName)]))
      else begin
        { Yes. Get the first attribute set in the list. }
        oNextSet := oList.Pointers[0];
        { Continue checking with that instance. }
        if oNextSet <> nil then begin
          Result := CircularRefPrim(oNextSet, wMarker);
          if Result then
            break;
        end;  { if }
      end;  { if }
    end;  { for }
  end;  { if }
end;
{--------}
function TXpAttrSetManager.HasCircularRef(const sAttrSetName : DOMString) : Boolean;
var
  oSet : TXpXSLAttributeSet;
  wMarker : Integer;
begin
  { Strategy: Mark each attribute set, starting with this one, with the
    same integer value (i.e., a breadcrumb). If we run across this breadcrumb
    at any point then we have a circular reference. }
  wMarker := FTagSeed;
  inc(FTagSeed);
  oSet := Get(sAttrSetName).Pointers[0];
  Result := CircularRefPrim(oSet, wMarker);
end;
{--------}
procedure TXpAttrSetManager.MergeSets(oAttrSet : TXpXSLAttributeSet;
                                      oContext : TXpXSLContext);
var
  oList : TXpPointerList;
  oNextSet : TXpXSLAttributeSet;
  wInx : Integer;
  wInx2 : Longint;
begin
  for wInx := 0 to Pred(oAttrSet.UsedCount) do begin
    { Get the list of attribute sets bearing the referenced attribute name. }
    oList := Get(oAttrSet.UsedAttrSets[wInx]);
    { Merge each attribute set from lowest import precedence to highest
      import precedence. }
    for wInx2 := Pred(oList.Count) downto 0 do begin
      oNextSet := oList.Pointers[wInx2];
      { Instantiate the attributes within the attribute set. This will
        merge its used attribute sets before executing its child xsl:attribute
        elements. }
      oNextSet.Execute(oContext);
    end;  { for }
  end;  { for }
end;
{--------}
procedure TXpAttrSetManager.MergeSetsViaName(const sAttrSetName : DOMString; {!!.57}
                                             oContext : TXpXSLContext);
var
  oList : TXpPointerList;
  oNextSet : TXpXSLAttributeSet;
  wInx : Longint;
begin
  oList := Get(sAttrSetName);
  if oList <> nil then begin
    for wInx := 0 to Pred(oList.Count) do begin
      { Grab the attribute set. }
      oNextSet := oList.Pointers[wInx];
      { Execute the attribute set. }
      oNextSet.Execute(oContext);
    end;  { for }
  end;  { if }
end;
{--------}
procedure TXpAttrSetManager.OnDisposeSet(Sender : TXpStrHash; aData : Pointer);
begin
  TXpPointerList(aData).Free;
end;
{====================================================================}

{===TXpKeySet========================================================}
constructor TXpKeySet.Create;
begin
  inherited;
  FKeysByDoc := TXpIntHash.Create(xpc_Size59);
  FKeysByDoc.OnDisposeData := OnDisposeKeys;
  FKeyDefs := TXpPointerList.Create;
end;
{--------}
destructor TXpKeySet.Destroy;
var
  wInx : Integer;
begin
  FKeysByDoc.Free;
  for wInx := Pred(FKeyDefs.Count) downto 0 do
    TXpKeyDefinition(FKeyDefs.Pointers[wInx]).Free;
  FKeyDefs.Free;
  inherited;
end;
{--------}
procedure TXpKeySet.AddKeyDef(oKey : TXpXSLKey);
var
  oKeyDef : TXpKeyDefinition;
begin
  oKeyDef := TXpKeyDefinition.Create;
  with oKeyDef do begin
    Match := oKey.Match;
    Use := oKey.Use;
  end;
  FKeyDefs.Append(oKeyDef);
end;
{--------}
procedure TXpKeySet.GenerateKeys(oNode : TXpNode; oPattern : TXpPattern;
                                 oStorage : TXpStrHash;
                                 oContext : TXpXSLContext);
var
  i : Integer;
begin
  while Assigned(oNode) do begin
    ProcessNode(oNode, oPattern, oStorage, oContext);
    if oNode.HasAttributes then
      for i := 0 to Pred(oNode.Attributes.Length) do
        ProcessNode(oNode.Attributes.Item(i), oPattern, oStorage, oContext);
    if oNode.HasChildNodes then
      GenerateKeys(oNode.FirstChild, oPattern, oStorage, oContext);
    oNode := oNode.NextSibling;
  end; {while}
end;
{--------}
function TXpKeySet.GetKeyDef(const wInx : Integer) : TXpKeyDefinition;
begin
  Result := FKeyDefs.Pointers[wInx];
end;
{--------}
function TXpKeySet.KeyDefCount : Integer;
begin
  Result := FKeyDefs.Count;
end;
{--------}
function TXpKeySet.KeysForDoc(oDoc : TXpDocument;
                              oContext : TXpXSLContext) : TXpStrHash;
var
  oCurNodeSav : TXpNode;
  oKeyDef : TXpKeyDefinition;
  oPattern : TXpPattern;
  oPatternMaker : TXpPatternMaker;
  wID,
  wInx : Longint;
begin
  wID := Longint(oDoc);
  Result := FKeysByDoc.Get(wID);
  { Keys generated yet? }
  if Result = nil then begin
    { No. Build the keys for each key definition. }
    Result := TXpStrHash.Create(xpc_Size59);
    Result.OnDisposeData := OnDisposeKeysForValue;
    FKeysByDoc.Add(wID, Result);
    try
      oPatternMaker := TXpPatternMaker.Create;
      oCurNodeSav := oContext.CurrentNode;
      try
        for wInx := 0 to Pred(FKeyDefs.Count) do begin
          oKeyDef := TXpKeyDefinition(FKeyDefs.Pointers[wInx]);
          oPattern := oPatternMaker.GeneratePattern
                        (oContext.CurrentStylesheetNode, oKeyDef.Match);
          oPattern.TagStr := oKeyDef.Use;
          try
            GenerateKeys(oDoc.FirstChild, oPattern, Result, oContext);
          finally
            oPattern.Free;
          end;
        end;
      finally
        oContext.CurrentNode := oCurNodeSav;
        oPatternMaker.Free;
      end;
    except
      FKeysByDoc.Remove(wID);
    end;
  end;
end;
{--------}
procedure TXpKeySet.OnDisposeKeys(Sender : TXpBaseHash; aData : Pointer);
begin
  TXpStrHash(aData).Free;
end;
{--------}
procedure TXpKeySet.OnDisposeKeysForValue(Sender : TXpStrHash; aData : Pointer);
begin
  TXpNodeList(aData).Free;
end;
{--------}
procedure TXpKeySet.ProcessNode(oNode : TXpNode; oPattern : TXpPattern;
                                oStorage : TXpStrHash;
                                oContext : TXpXSLContext);
var
  oList : TXpNodeList;
  oNodeSet : TXpNodeList;
  oSavNode : TXpNode;
  oValue : TXpValue;
  sKeyValue : DOMString;
  wInx : Integer;
begin
  { Does the node match the pattern? }
  if oPattern.Matches(oNode, nil) then begin
    { Yes. Generate the key value. }
    oSavNode := oContext.CurrentNode;
    oContext.CurrentNode := oNode;
    oContext.CurrentNodeList := TXpNodeList.Create;
    try
      oContext.CurrentNodeList.Add(oNode);
      oValue := oNode.Select(oPattern.TagStr, oContext.CurrentNodeList);
      try
        if oValue.ValueType = xpetNodeSet then begin
          oNodeSet := oValue.AsNodeSet;
          for wInx := 0 to Pred(oNodeSet.Length) do begin
            if oNodeSet.Item(wInx).NodeType = ATTRIBUTE_NODE then
              sKeyValue := oNodeSet.Item(wInx).NodeValue
            else
              sKeyValue := oNodeSet.Item(wInx).Text;
            oList := oStorage.Get(sKeyValue);
            if oList = nil then begin
              oList := TXpNodeList.Create;
              oStorage.Add(sKeyValue, oList);
            end;
            oList.Add(oNode);
          end;  { for }
        end
        else begin
          sKeyValue := oValue.AsString;
          oList := oStorage.Get(sKeyValue);
          if oList = nil then begin
            oList := TXpNodeList.Create;
            oStorage.Add(sKeyValue, oList);
          end;
          oList.Add(oNode);
        end;
      finally
        oValue.Free;
      end;
    finally
      oContext.CurrentNodeList.Free;
      oContext.RestoreCurNodeList;
      oContext.CurrentNode := oSavNode;
    end;
  end;
end;
{====================================================================}

{===TXpKeyManager====================================================}
constructor TXpKeyManager.Create;
begin
  inherited;
  FKeys := TXpStrHash.Create(xpc_Size59);
  FKeys.OnDisposeData := OnDisposeKeySet;
end;
{--------}
destructor TXpKeyManager.Destroy;
begin
  FKeys.Free;
  inherited;
end;
{--------}
procedure TXpKeyManager.AddKeyDef(oKey : TXpXSLKey);
var
  oKeySet : TXpKeySet;
begin
  { Was a keyset already created for keys bearing this name? }
  oKeySet := FKeys.Get(oKey.Name);
  if oKeySet = nil then begin
    oKeySet := TXpKeySet.Create;
    FKeys.Add(oKey.Name, oKeySet);
  end;
  oKeySet.AddKeyDef(oKey);
end;
{--------}
procedure TXpKeyManager.Clear;
begin
  FKeys.Clear;
end;
{--------}
function TXpKeyManager.KeysForDoc(const sKeyName : DOMString;          {!!.57}
                                  oDoc : TXpDocument;
                                  oContext : TXpXSLContext) : TXpStrHash;
var
  oKeySet : TXpKeySet;
begin
  oKeySet := FKeys.Get(sKeyName);
  if oKeySet = nil then
    raise EXpXSLException.Create(Format(sXSLNoKeyDef,
                                        [QuotedStr(sKeyName)]))
  else
    Result := oKeySet.KeysForDoc(oDoc, oContext);
end;
{--------}
procedure TXpKeyManager.OnDisposeKeySet(Sender : TXpStrHash; aData : Pointer);
begin
  TXpKeySet(aData).Free;
end;
{====================================================================}

{===TXpElementMediator===============================================}
{$IFDEF UsingCLX}
constructor TXpElementMediator.Create(oProcessor : TXpQBaseXSLProcessor);
{$ELSE}
constructor TXpElementMediator.Create(oProcessor : TXpBaseXSLProcessor);
{$ENDIF}
begin
  inherited Create;
  FXSLProcessor := oProcessor;
end;
{--------}
procedure TXpElementMediator.AddAttrSet(oAttrSet : TXpXSLAttributeSet);
begin
  FXSLProcessor.FAttrSetMgr.Add(oAttrSet);
end;
{--------}
procedure TXpElementMediator.AddComment(const sComment : DOMString);
begin
  FXSLProcessor.AddComment(sComment);
end;
{--------}
procedure TXpElementMediator.AddKeyDef(oKey : TXpXSLKey);
begin
  FXSLProcessor.FKeyMgr.AddKeyDef(oKey);
end;
{--------}
procedure TXpElementMediator.AddNSAlias(const sStylePrefix, sResultPrefix : DOMString;
                                        const wImpPrecedence : Integer);
begin
  FXSLProcessor.FNSAliasMgr.AddAlias(sStylePrefix, sResultPrefix,
                                     wImpPrecedence);
end;
{--------}
procedure TXpElementMediator.AddOutputAttribute(const sName, sValue : DOMString;
                                                oOutputNode :TXpXSLOutput);
begin
  FXSLProcessor.FOutputMgr.AddAttrib(sName, sValue, oOutputNode);
end;
{--------}
procedure TXpElementMediator.AddPI(const sName, sValue : DOMString);
begin
  FXSLProcessor.AddPI(sName, sValue);
end;
{--------}
procedure TXpElementMediator.AddStrip(oContext: TXpXSLContext;
                                      oSLNode : TXpXSLWhitespaceControl);
begin
  FXSLProcessor.AddStrip(oContext, oSLNode);
end;
{--------}
procedure TXpElementMediator.AddTemplate(oTemplate : TXpXSLTemplate);
begin
  FXSLProcessor.FTemplateMgr.Add(oTemplate);
end;
{--------}
procedure TXpElementMediator.ApplyImports(oContext : TXpXSLContext);
begin
  FXSLProcessor.ApplyImports(oContext);
end;
{--------}
procedure TXpElementMediator.AttrSetMergeSets(oAttrSet : TXpXSLAttributeSet;
                                              oContext : TXpXSLContext);
begin
  FXSLProcessor.FAttrSetMgr.MergeSets(oAttrSet, oContext);
end;
{--------}
procedure TXpElementMediator.AttrSetMergeSetsViaName(const sAttrSet : DOMString;
                                                           oContext : TXpXSLContext);
begin
  FXSLProcessor.FAttrSetMgr.MergeSetsViaName(sAttrSet, oContext);
end;
{--------}
function TXpElementMediator.AttrSetHasCircularRef(const sAttrSetName : DOMString) : Boolean;
begin
  Result := FXSLProcessor.FAttrSetMgr.HasCircularRef(sAttrSetName);
end;
{--------}
procedure TXpElementMediator.CopyAttribute(const sName, sValue : DOMString;
                                                 oCurrentSLNode : TXpNode);
begin
  FXSLProcessor.CopyAttribute(sName, sValue, oCurrentSLNode);
end;
{--------}
procedure TXpElementMediator.CopyCDATANode(oNode : TXpCDATASection);
begin
  FXSLProcessor.CopyCDATANode(oNode);
end;
{--------}
procedure TXpElementMediator.CopyFO(oNode : TXpNode);
begin
  FXSLProcessor.CopyFO(oNode);
end;
{--------}
procedure TXpElementMediator.CopyText(const sValue : DOMString);
begin
  FXSLProcessor.CopyText(sValue);
end;
{--------}
procedure TXpElementMediator.CopyTextNode(oNode : TXpText);
begin
  FXSLProcessor.CopyTextNode(oNode);
end;
{--------}
procedure TXpElementMediator.EndElement(oNode : TxpNode);
begin
  FXSLProcessor.EndElement(oNode);
end;
{--------}
function TXpElementMediator.FilterCanBeOverridden : Boolean;
begin
  Result := (not FXSLProcessor.HasFilter) or
            FXSLProcessor.OverrideFilter;
end;
{--------}
procedure TXpElementMediator.FOEnd;
begin
  FXSLProcessor.FOEnd;
end;
{--------}
function TXpElementMediator.GetCurrentResultTreeElement : TXpElement;
begin
  Result := FXSLProcessor.GetCurrentResultTreeElement;
end;
{--------}
function TXpElementMediator.GetPatternMaker : TXpPatternMaker;
begin
  Result := FXSLProcessor.GetPatternMaker;
end;
{--------}
function TXpElementMediator.GetStyleURL : DOMString;
begin
  Result := FXSLProcessor.GetStyleURL;
end;
{--------}
function TXpElementMediator.GetDecimalFmtMgr : TXpDecimalFormatManager;
begin
  Result := FXSLProcessor.FDecimalFmtMgr;
end;
{--------}
function TXpElementMediator.GetTemplateMgr : TXpBaseTemplateManager;
begin
  Result := FXSLProcessor.FTemplateMgr;
end;
{--------}
function TXpElementMediator.PopContext : TXpXSLContext;
begin
  Result := FXSLProcessor.PopContext;
end;
{--------}
function TXpElementMediator.PopFilter : TObject;
begin
  Result := FXSLProcessor.PopFilter;
end;
{--------}
procedure TXpElementMediator.PushContext(oContext : TXpXSLContext);
begin
  FXSLProcessor.PushContext(oContext);
end;
{--------}
procedure TXpElementMediator.PushFilter(oFilter : TObject);
begin
  FXSLProcessor.PushFilter(oFilter);
end;
{--------}
procedure TXpElementMediator.RaiseXSLMessage(Sender : TObject;
                                       const Msg : DOMString;
                                         var Terminate : Boolean);
begin
  FXSLProcessor.RaiseXSLMessage(Sender, Msg, Terminate);
end;
{--------}
procedure TXpElementMediator.ResolveConflict(oNode : TXpNode;
                                             eEventCode : TXpConflictType;
                                         var bRaiseError : Boolean);
begin
  FXSLProcessor.ResolveConflict(oNode, eEventCode, bRaiseError);
end;
{--------}
procedure TXpElementMediator.SetGlobalParm(const sName : DOMString;
                                                 oValue : TXpValue);
begin
  FXSLProcessor.SetGlobalParm(sName, oValue);
end;
{--------}
procedure TXpElementMediator.SetGlobalVar(const sName : DOMString;
                                                oValue : TXpValue);
begin
  FXSLProcessor.SetGlobalVar(sName, oValue);
end;
{--------}
procedure TXpElementMediator.StartElement(oNode : TxpNode);
begin
  FXSLProcessor.StartElement(oNode);
end;
{====================================================================}

{===TXpBaseXSLProcessor==============================================}
{$IFDEF UsingCLX}
constructor TXpQBaseXSLProcessor.Create(oOwner : TComponent);
{$ELSE}
constructor TXpBaseXSLProcessor.Create(oOwner : TComponent);
{$ENDIF}
begin
  inherited;
  FAttrSetMgr := TXpAttrSetManager.Create;
  FDecimalFmtMgr := TXpDecimalFormatManager.Create;
  FErrors := TStringList.Create;
  FKeyMgr := TXpKeyManager.Create;
  FNSAliasMgr := TXpNSAliasManager.Create;
  FOverrideFilter := False;
end;
{--------}
{$IFDEF UsingCLX}
destructor TXpQBaseXSLProcessor.Destroy;
{$ELSE}
destructor TXpBaseXSLProcessor.Destroy;
{$ENDIF}
begin
  FNSAliasMgr.Free;
  FKeyMgr.Free;
  FErrors.Free;
  FDecimalFmtMgr.Free;
  FAttrSetMgr.Free;
  inherited;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.AddComment(const sComment : DOMString);
{$ELSE}
procedure TXpBaseXSLProcessor.AddComment(const sComment : DOMString);
{$ENDIF}
begin
  if Assigned(FOnComment) then
    FOnComment(Self, sComment);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.AddPI(const sName, sValue : DOMString);
{$ELSE}
procedure TXpBaseXSLProcessor.AddPI(const sName, sValue : DOMString);
{$ENDIF}
begin
  if Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(Self, sName, sValue);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.AddStrip(oContext: TXpXSLContext;
                                        oSLNode : TXpXSLWhitespaceControl);
{$ELSE}
procedure TXpBaseXSLProcessor.AddStrip(oContext: TXpXSLContext;
                                        oSLNode : TXpXSLWhitespaceControl);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.ApplyImports(oContext : TXpXSLContext);
{$ELSE}
procedure TXpBaseXSLProcessor.ApplyImports(oContext : TXpXSLContext);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.CopyAttribute(const sName, sValue : DOMString;
{$ELSE}
procedure TXpBaseXSLProcessor.CopyAttribute(const sName, sValue : DOMString;
{$ENDIF}
                                                  oCurrentSLNode : TXpNode);
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.CopyCDATANode(oNode : TXpCDATASection);
{$ELSE}
procedure TXpBaseXSLProcessor.CopyCDATANode(oNode : TXpCDATASection);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.CopyFO(oNode : TXpNode);
{$ELSE}
procedure TXpBaseXSLProcessor.CopyFO(oNode : TXpNode);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.CopyText(const sValue : DOMString);
{$ELSE}
procedure TXpBaseXSLProcessor.CopyText(const sValue : DOMString);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.CopyTextNode(oNode : TXpText);
{$ELSE}
procedure TXpBaseXSLProcessor.CopyTextNode(oNode : TXpText);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.EndElement(oNode : TxpNode);
{$ELSE}
procedure TXpBaseXSLProcessor.EndElement(oNode : TxpNode);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.FOEnd;
{$ELSE}
procedure TXpBaseXSLProcessor.FOEnd;
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.HasFilter : Boolean;
{$ELSE}
function TXpBaseXSLProcessor.HasFilter : Boolean;
{$ENDIF}
begin
  { Do nothing }
  Result := False;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.OnFormatNumber(oOwner : TXpNode;
{$ELSE}
procedure TXpBaseXSLProcessor.OnFormatNumber(oOwner : TXpNode;
{$ENDIF}
                                       const sNumber, sPattern, sFormat : DOMString;
                                         var sFormattedNumber : DOMString);
var
  oFmt : TXpXSLDecimalFormat;
begin
  { Find the decimal format. }
  oFmt := FDecimalFmtMgr.GetFormat(sFormat);
  if oFmt = nil then
    raise EXpXSLException.Create(Format(sXSLNoDecimalFormat,
                                        [QuotedStr(sFormat)]))
  else
    sFormattedNumber := oFmt.FormatNumber(sNumber, sPattern);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.GetCurrentResultTreeElement : TXpElement;
{$ELSE}
function TXpBaseXSLProcessor.GetCurrentResultTreeElement : TXpElement;
{$ENDIF}
begin
  { Do nothing }
  Result := nil;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.GetPatternMaker : TXpPatternMaker;
{$ELSE}
function TXpBaseXSLProcessor.GetPatternMaker : TXpPatternMaker;
{$ENDIF}
begin
  { Do nothing }
  Result := nil;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.GetStyleURL : DOMString;
{$ELSE}
function TXpBaseXSLProcessor.GetStyleURL : DOMString;
{$ENDIF}
begin
  { Do nothing }
  Result := '';
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.GetVersion : string;
{$ELSE}
function TXpBaseXSLProcessor.GetVersion : string;
{$ENDIF}
begin
  Result := Format('%5.4f', [XpVersionNumber / 10000.0]);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.PopContext : TXpXSLContext;
{$ELSE}
function TXpBaseXSLProcessor.PopContext : TXpXSLContext;
{$ENDIF}
begin
  Result := FContextTail;
  if Result <> nil then
    FContextTail := Result.PrevContext;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQBaseXSLProcessor.PopFilter : TObject;
{$ELSE}
function TXpBaseXSLProcessor.PopFilter : TObject;
{$ENDIF}
begin
  { Do nothing }
  Result := nil;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.PushContext(oContext : TXpXSLContext);
{$ELSE}
procedure TXpBaseXSLProcessor.PushContext(oContext : TXpXSLContext);
{$ENDIF}
begin
  oContext.PrevContext := FContextTail;
  FContextTail := oContext;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.PushFilter(oFilter : TObject);
{$ELSE}
procedure TXpBaseXSLProcessor.PushFilter(oFilter : TObject);
{$ENDIF}
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.RaiseXSLMessage(Sender : TObject;
{$ELSE}
procedure TXpBaseXSLProcessor.RaiseXSLMessage(Sender : TObject;
{$ENDIF}
                                        const Msg : DOMString;
                                          var Terminate : Boolean);
begin
  if Assigned(FOnXSLMessage) then
    FOnXSLMessage(Sender, Msg, Terminate);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.ResolveConflict(oNode : TXpNode;
{$ELSE}
procedure TXpBaseXSLProcessor.ResolveConflict(oNode : TXpNode;
{$ENDIF}
                                              eEventCode : TXpConflictType;
                                          var bRaiseError : Boolean);
begin
 if Assigned(FOnResolveConflict) then
   FOnResolveConflict(Self, oNode, eEventCode, bRaiseError);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.SetGlobalParm(const sName : DOMString;
{$ELSE}
procedure TXpBaseXSLProcessor.SetGlobalParm(const sName : DOMString;
{$ENDIF}
                                                  oValue : TXpValue);
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.SetGlobalVar(const sName : DOMString;
{$ELSE}
procedure TXpBaseXSLProcessor.SetGlobalVar(const sName : DOMString;
{$ENDIF}
                                                 oValue : TXpValue);
begin
  { Do nothing }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.SetVersion(const Value : string);
{$ELSE}
procedure TXpBaseXSLProcessor.SetVersion(const Value : string);
{$ENDIF}
begin
  {do nothing}
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQBaseXSLProcessor.StartElement(oNode : TxpNode);
{$ELSE}
procedure TXpBaseXSLProcessor.StartElement(oNode : TxpNode);
{$ENDIF}
begin
  { Do nothing }
end;
{====================================================================}

{===TXpBaseXSLElement================================================}
constructor TXpBaseXSLElement.Create;
begin
  inherited Create;
  FAllowsTemplateBody := False;
  FInitialized := False;
  FXSLProcessor := nil;
end;
{--------}
destructor TXpBaseXSLElement.Destroy;
begin
  Clear;
  inherited;
end;
{--------}
procedure TXpBaseXSLElement.Clear;
begin
  { Do nothing }
end;
{--------}
function TXpBaseXSLElement.EvaluateAttrib(oContext : TXpXSLContext;
                                          const sAttrib : DOMString) : DOMString; {!!.57}
var
  wInx,
  wLen,
  wPotentialLen,
  wRealLen,
  wPos1,
  wPos2,
  wPos3  : Integer;
  sValue,
  sExpr : DOMString;
  bCurly, bDoubleCurly : Boolean;
begin
  wRealLen := 0;
  wPos1 := 1;                                                          {!!.05}
  if wPos1 > 0 then begin
    wPos2 := 1;
    wLen := Length(sAttrib);
    wPotentialLen := wLen;
    SetLength(Result, wLen);
    { Scan through the string looking for the XPath expressions. }
    while wPos1 <= wLen do begin
      { Is this a left curly brace? }
      bCurly := (sAttrib[wPos1] = '{');
      bDoubleCurly := False;
      if bCurly then
        bDoubleCurly := (sAttrib[wPos1 + 1] = '{');
          { Double curly braces are used to signify a literal curly brace. }

      if (not bCurly) or bDoubleCurly then begin
        { No. Add as is to result string. }
        Result[wPos2] := sAttrib[wPos1];
        inc(wPos1);
        inc(wPos2);
        inc(wRealLen);
        if bDoubleCurly then begin
//          Result[wPos2] := sAttrib[wPos1];                           {Deleted !!.55}
          inc(wPos1);
//          inc(wPos2);                                                {Deleted !!.55}
//          inc(wRealLen);                                             {Deleted !!.55}
        end;
      end
      else begin
        { Yes. Get the expression. }
        SetLength(sExpr, Length(sAttrib));
        wPos3 := 1;
        inc(wPos1);
        while (wPos1 <= wLen) and (sAttrib[wPos1] <> '}') do begin
          sExpr[wPos3] := sAttrib[wPos1];
          inc(wPos1);
          inc(wPos3);
        end;
        SetLength(sExpr, Pred(wPos3));
        { Move past the right curly brace. }
        inc(wPos1);
        { Evaluate the expression. }
        sValue := oContext.CurrentNode.SelectStringContext             {!!.57}
                    (sExpr, oContext.CurrentNodeList);                 {!!.57}
        inc(wRealLen, Length(sValue));
        inc(wPotentialLen, Length(sValue));
        SetLength(Result, wPotentialLen);
        { Copy the expression into the result string. }
        for wInx := 1 to Length(sValue) do begin
          Result[wPos2] := sValue[wInx];
          inc(wPos2);
        end;
      end;
    end;
    SetLength(Result, wRealLen);
  end
  else
    Result := sAttrib;
end;
{--------}
procedure TXpBaseXSLElement.Execute(oContext : TXpXSLContext);
var
  ErrMsg : string;
begin
  { Is the element supported? }
  if not Supported(ErrMsg) then begin
    { No. See if the element can fallback. }
    if not Fallback(Self, oContext) then
      { No fallback specified. Raise an exception. }
      raise EXpXSLException.Create(ErrMsg);
  end
  else begin
    UpdateNSMgr(oContext);
    ExecuteChildren(oContext);
  end;
end;
{--------}
procedure TXpBaseXSLElement.ExecuteChildPrim(oNode : TXpNode;
                                             oContext : TXpXSLContext);
var
  aInx : Integer;
  oChildNode : TXpNode;
  oMediator : TXpElementMediator;
  oStylesheet : TXpXSLStylesheet;
begin
  oContext.CurrentStylesheetNode := oNode;
  try
    oStylesheet := ParentStylesheet;
    oMediator := ParentStylesheet.Mediator;
    { Is this an XSLT element? }
    if oNode is TXpBaseXSLElement then begin
      { Yes. Preprocess & execute it. }
      oStylesheet.StartNSLevel;
      try
        TXpBaseXSLElement(oNode).Preprocess(oContext);
        TXpBaseXSLElement(oNode).Execute(oContext);
      finally
        oStylesheet.EndNSLevel;
      end;
    end
    else if oNode.NodeType = ELEMENT_NODE then begin
      { Is this within the xsl namespace (i.e., an element with prefix xsl:
        but one that we do not support? }
      if XpStartsWith(XpsXSLColon, oNode.NodeName) then begin
        { Yes. If this xsl:stylesheet's version attribute is 1.0 then report
          an error. Otherwise see if there is fallback processing.  }
        if (GetEffectiveVersion(oNode) = XpsBaseXSLTSpec) or
           (not Fallback(oNode, oContext)) then
          raise EXpXSLException.Create(Format(sXSLInvalidXSLTName,
                                           [QuotedStr(oNode.NodeName)]));
      end
      else begin
        oStylesheet.StartNSLevel;
        try
          { No. It's a literal result element. Pass it on to the result tree &
            process its children. }
          oMediator.StartElement(oNode);
          ProcessAttribs(oContext, TXpElement(oNode), True);
          for aInx := 0 to Pred(oNode.ChildNodes.Length) do begin
            oChildNode := oNode.ChildNodes.Item(aInx);
            ExecuteChildPrim(oChildNode, oContext);
          end;
          oMediator.EndElement(oNode);
        finally
          oStylesheet.EndNSLevel;
        end;
      end;
    end
    else if oNode.NodeType = TEXT_NODE then
      { It's a text node. Pass it on to the result tree. }
      oMediator.CopyTextNode(TXpText(oNode))
    else if oNode.NodeType = CDATA_SECTION_NODE then
      oMediator.CopyCDATANode(TXpCDATASection(oNode));
    { Assumption: All other elements ignored. }
  finally
    oContext.PopCurSLNode;
  end;
end;
{--------}
procedure TXpBaseXSLElement.ExecuteChildren(oContext : TXpXSLContext);
var
  aInx : Integer;
  oNode : TXpNode;
begin
  for aInx := 0 to pred(ChildNodes.Length) do begin
    oNode := ChildNodes.Item(aInx);
    { Is this an xsl:fallback element? }
    if not (oNode is TXpXSLFallback) then
      { No. Execute it. }
      ExecuteChildPrim(oNode, oContext);
  end;
end;
{--------}
function TXpBaseXSLElement.ExpandName(const sName : DOMString) : DOMString;
var
  sURI : DOMString;
  wPos : Integer;
begin
  Result := sName;
  wPos := XpPos(':', Result);
  { Namespace indicator found? }
  if wPos > 0 then begin
    { Yes. Resolve the namespace prefix into a URI. The URI prefixed to
      the local name is the final name of the variable as seen by the
      processor. Variable name matches involve the namespace URI and
      the local name. }
    sURI := ResolveNSPrefix(XpCopy(Result, 1, wPos - 1));
    if sURI = '' then
      raise EXpXSLException.Create
              (Format(sXSLUnresolvedNSPrefix,
                      [QuotedStr(XpCopy(Result, 1, wPos - 1)),
                       'name attribute having value ' + QuotedStr(Result) +
                       ' for ' + NodeName + ' element']))
    else
    Result := sURI + ':' + XpCopy(Result, wPos + 1, Length(Result) - wPos);
  end;
end;
{--------}
function TXpBaseXSLElement.Fallback(oFailedNode : TXpNode;
                                    oContext : TXpXSLContext) : Boolean;
var
  anInx : Integer;
  oNode : TXpNode;
begin
  Result := False;
  { Scan through the body of the element, looking for xsl:fallback elements. }
  for anInx := 0 to Pred(oFailedNode.ChildNodes.Length) do begin
    oNode := oFailedNode.ChildNodes.Item(anInx);
    if oNode is TXpXSLFallback then begin
      Result := True;
      TXpXSLFallback(oNode).Execute(oContext);
    end;
  end;
end;
{--------}
function TXpBaseXSLElement.GetEffectiveVersion(oNode : TXpNode) : DOMString;
var
  oAncestor : TXpNode;
begin
  Result := '';
  oAncestor := oNode.ParentNode;
  while (Result = '') and (oAncestor <> nil) do begin
    if (oAncestor is TXpElement) then
      Result := TXpElement(oAncestor).GetAttribute(XpsXSLVersion);
    oAncestor := oAncestor.ParentNode;
  end;
  if Result = '' then
    Result := ParentStylesheet.Version;
end;
{--------}
function TXpBaseXSLElement.InTemplateBody : Boolean;
begin
  if ParentNode is TXpBaseXSLElement then
    Result := TXpBaseXSLElement(ParentNode).FAllowsTemplateBody
  else
    { Must be in a literal result element. }
    Result := True;
end;
{--------}
function TXpBaseXSLElement.IsEmpty : Boolean;
begin
  Result := (ChildNodes.Length = 0);
end;
{--------}
function TXpBaseXSLElement.IsTopLevelElement : Boolean;
begin
  Result := (ParentNode is TXpXSLStylesheet);
end;
{--------}
function TXpBaseXSLElement.MissingAttrib(const sAttribName : DOMString) : DOMString;
begin
  Result := Format(sXSLAttribReq, [QuotedStr(sAttribName), QuotedStr(NodeName)]);
end;
{--------}
function TXpBaseXSLElement.ParentStylesheet : TXpXSLStylesheet;
var
  oNode : TXpBaseXSLElement;
begin
  oNode := Self;
  while (oNode <> nil) and (not (oNode is TXpXSLStylesheet)) do
    oNode := TXpBaseXSLElement(oNode.ParentNode);
  if oNode = nil then
    Result := nil
  else
    Result := TXpXSLStylesheet(oNode);
end;
{--------}
function TXpBaseXSLElement.ParseUseAttrSet(const sUseAttrSet : DOMString) : TXpPointerList;
var
  bInWhite : Boolean;
  oItem : TXpDOMStringItem;
  wInx,
  wLen,
  wStart : Integer;
begin
  Result := TXpPointerList.Create;
  wLen := Length(sUseAttrSet);
  wInx := 1;
  wStart := 1;
  bInWhite := True;
  while wInx <= wLen do begin
    { Find the start of a name. }
    while bInWhite and (wInx <= wLen) do begin
      bInWhite := XpIsWhiteSpace(sUseAttrSet[wInx]);
      if not bInWhite then
        wStart := wInx
      else
        inc(wInx);
    end;  { while }

    { Find the end of the name. }
    while (not bInWhite) and (wInx <= wLen) do begin
      bInWhite := XpIsWhiteSpace(sUseAttrSet[wInx]);
      if not bInWhite then
        inc(wInx);
    end;  { while }

    if Pred(wInx) >= wStart then begin
      oItem := TXpDOMStringItem.Create;
      oItem.sString := XpCopy(sUseAttrSet, wStart, wInx - wStart);
      Result.Append(oItem);
    end;
  end;  { while }
end;
{--------}
procedure TXpBaseXSLElement.Preprocess(oContext : TXpXSLContext);
var
  ErrMsg : string;
begin
  if not Valid(oContext, ErrMsg) then
    raise EXpXSLException.Create(ErrMsg);
end;
{--------}
procedure TXpBaseXSLElement.ProcessAttribPrim(oContext : TXpXSLContext;
                                              oAttrib : TXpAttribute;
                                              bAttribValTemp : Boolean);
var
  oStylesheet : TXpXSLStylesheet;
  sAttrValue,
  sNSName : DOMString;
begin
  oStylesheet := ParentStylesheet;

  { Is this an XSL attribute? }
  if oAttrib.Prefix = XpsXSL then begin
    { Yes. Handle appropriately. }
    if oAttrib.LocalName = XpsExclResultPrefixes then
      oStylesheet.NSMgr.AddExcludes(oAttrib.Value);
  end
  else if (oAttrib.Prefix = XpsXMLNS) or
          (oAttrib.NodeName = XpsXMLNS) then begin
    { Namespace node. }
    if oStylesheet.NSMgr.Using then begin
      if bAttribValTemp then
        sAttrValue:= EvaluateAttrib(oContext, oAttrib.Value)
      else
        sAttrValue := oAttrib.Value;
      oStylesheet.NSMgr.AddNS(oAttrib.Name, sAttrValue);
      { Is this NS excluded? }
      if (oAttrib.Name <> XpsXSL) and
         (not oStylesheet.NSMgr.Excluded(oAttrib.Name)) then begin
        { No. Copy to result tree. }
        if oAttrib.Name = XpsXMLNS then
          sNSName := oAttrib.Name
        else
          sNSName := XpsXMLNS + ':' + oAttrib.Name;
        oStylesheet.Mediator.CopyAttribute(sNSName, sAttrValue,
                                           oContext.CurrentStylesheetNode);
      end;
    end;  { if using NS management }
  end
  else begin
    { Not a namespace node. Evaluate it as an attribute value template? }
    if bAttribValTemp then
      sAttrValue:= EvaluateAttrib(oContext, oAttrib.Value)
    else
      sAttrValue := oAttrib.Value;
    oStylesheet.Mediator.CopyAttribute(oAttrib.Name, sAttrValue,
                                       oContext.CurrentStylesheetNode);
  end;
end;
{--------}
procedure TXpBaseXSLElement.ProcessAttribs(oContext : TXpXSLContext;
                                           oLRE : TXpElement;
                                           bAttribValTemp : Boolean);
var
  oAttrib : TXpAttribute;
  oList : TXpPointerList;
  oStylesheet : TXpXSLStylesheet;
  sAttrValue : DOMString;
  sNSName,
  sWrittenNS : DOMString;        { NS nodes already written to output }
  wInx : Integer;
  wInx2 : Longint;
begin
  { This method processes the attributes of a Literal Result Element (LRE).

    When this routine has finished, the following nodes will have been
    transferred to the result tree:
    1. Namespace nodes for namespaces in scope for the LRE (except those
       that are excluded).
    2. Namespace nodes declared on the LRE.
    3. non-namespace attributes (after being evaluated as attribute templates).

    The following nodes will not go to the result tree:
    1. Attributes in the xsl namespace.
    2. Namespace nodes that are to be excluded from the result tree.
  }
  oStylesheet := ParentStylesheet;

  if oLRE.HasAttributes then begin
    { Scan for use-attribute-set references first. As per the spec, these
      must be added to the LRE before the literal attributes. }
    for wInx := 0 to Pred(oLRE.Attributes.Length) do begin
      oAttrib := TXpAttribute(oLRE.Attributes.Item(wInx));
      if (oAttrib.NodeName = XpsXSLColon + XpsUseAttrSets) then begin
        { Break the attribute down into a list of attribute set names. }
        oList := ParseUseAttrSet(oAttrib.Value);
        try
          for wInx2 := 0 to Pred(oList.Count) do
            oStylesheet.Mediator.AttrSetMergeSetsViaName
             (TXpDOMStringItem(oList.Pointers[wInx2]).sString, oContext);
        finally
          for wInx2 := Pred(oList.Count) downto 0 do
            TXpDOMStringItem(oList.Pointers[wInx2]).Free;
          oList.Free;
        end;
      end;
    end; { for }

    { Handle non-namespace attributes. }
    for wInx := 0 to Pred(oLRE.Attributes.Length) do begin
      oAttrib := TXpAttribute(oLRE.Attributes.Item(wInx));

      { Is this an XSL attribute? }
      if oAttrib.Prefix = XpsXSL then begin
        { Yes. Handle appropriately. }
        if oAttrib.LocalName = XpsExclResultPrefixes then
          oStylesheet.NSMgr.AddExcludes(oAttrib.Value);
      end
      else if (oAttrib.Prefix <> XpsXMLNS) and
              (oAttrib.NodeName <> XpsXMLNS) then begin
        { Not a namespace node. Evaluate it as an attribute value template? }
        if bAttribValTemp then
          sAttrValue:= EvaluateAttrib(oContext, oAttrib.Value)
        else
          sAttrValue := oAttrib.Value;
        oStylesheet.Mediator.CopyAttribute(oAttrib.Name, sAttrValue, oLRE);
      end;
    end;

    { Handle namespace attributes. }
    if oStylesheet.NSMgr.Using then begin
      sWrittenNS := '';
      if oLRE.NamespaceList <> nil then begin
        { Copy namespace nodes to result tree. }
        for wInx := 0 to Pred(oLRE.NamespaceList.Length) do begin
          oAttrib := TXpAttribute(oLRE.NamespaceList.Item(wInx));
          { Evaluate it as an attribute value template? }
          if bAttribValTemp then
            sAttrValue:= EvaluateAttrib(oContext, oAttrib.Value)
          else
            sAttrValue := oAttrib.Value;
          oStylesheet.NSMgr.AddNS(oAttrib.Name, sAttrValue);
          { Is this NS excluded? }
          if (oAttrib.Name <> XpsXSL) and
             (not oStylesheet.NSMgr.Excluded(oAttrib.Name)) then begin
            { No. Copy to result tree. }
            if oAttrib.Name = XpsXMLNS then
              sNSName := oAttrib.Name
            else
              sNSName := XpsXMLNS + ':' + oAttrib.Name;
            oStylesheet.Mediator.CopyAttribute(sNSName, sAttrValue, oLRE);
            sWrittenNS := sWrittenNS + ' ' + sNSName + ' ';
          end;
        end;  { for }
      end;
    end;  { if using NS management }
  end;  { if has attributes }

  { Write the namespace attributes currently in scope for this LRE.
    Note that we will be encountering the NS nodes we just added but
    they will be skipped over due to their existence in sWrittenNS. }
  if oStylesheet.NSMgr.Using then
    oStylesheet.NSMgr.WriteNSNodes(oLRE, sWrittenNS);

end;
{--------}
procedure TXpBaseXSLElement.Reset;
begin
  FInitialized := False;
end;
{--------}
function TXpBaseXSLElement.Supported(var ErrMsg : string) : Boolean;
begin
  ErrMsg := '';
  Result := True;
end;
{--------}
procedure TXpBaseXSLElement.UpdateNSMgr(oContext : TXpXSLContext);
var
  oAttrib : TXpAttribute;
  oStylesheet : TXpXSLStylesheet;
  sAttrValue : DOMString;
  wInx : Integer;
begin
  oStylesheet := ParentStylesheet;
  if oStylesheet.NSMgr.Using then begin
    { Handle default namespace. }
    if DefaultNamespace <> '' then begin
      sAttrValue := EvaluateAttrib(oContext, DefaultNamespace);
      if sAttrValue <> XpsXSLTURI then
        oStylesheet.NSMgr.AddNS('', sAttrValue);
    end;  { if }

    if NamespaceList <> nil then begin
      for wInx := 0 to Pred(NamespaceList.Length) do begin
        oAttrib := TXpAttribute(NamespaceList.Item(wInx));
        sAttrValue := EvaluateAttrib(oContext, oAttrib.Value);
        if sAttrValue <> XpsXSLTURI then
          oStylesheet.NSMgr.AddNS(oAttrib.Localname, sAttrValue);
      end;  { for }
    end;
  end;
end;
{--------}
function TXpBaseXSLElement.Valid(oContext : TXpXSLContext;
                             var ErrMsg : string) : Boolean;
begin
  ErrMsg := '';
  Result := True;
end;
{=====================================================================}

{===TXpXSLApplyImports================================================}
procedure TXpXSLApplyImports.Execute(oContext : TXpXSLContext);
begin
  UpdateNSMgr(oContext);
  ParentStylesheet.Mediator.ApplyImports(oContext);
end;
{--------}
function TXpXSLApplyImports.Valid(oContext : TXpXSLContext;
                              var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if not Result then
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpSortableXSLElement=============================================}
constructor TXpSortableXSLElement.Create;
begin
  inherited;
  FKeyDefinitions := TXpSortKeyDefinitions.Create;
  FSorter := TXpNodeSorter.Create;
end;
{--------}
destructor TXpSortableXSLElement.Destroy;
begin
  FKeyDefinitions.Free;
  FSorter.Free;
  inherited;
end;
{=====================================================================}

{===TXpXSLApplyTemplates==============================================}
procedure TXpXSLApplyTemplates.Execute(oContext : TXpXSLContext);
var
  oChildXSLNode : TXpBaseXSLElement;
  oNewContext : TXpXSLContext;
  oNode : TXpElement;
  oNodeList : TXpNodeList;
  oStylesheet : TXpXSLStylesheet;
  oTemplate : TXpXSLTemplate;
  oTemplateMgr : TXpBaseTemplateManager;
  wInx, wInx2 : Integer;
begin
  oNodeList := nil;
  try
    { Was a select attribute specified? }
    if FSelect = '' then
      { No. All child nodes of current node are to be processed. }
      oNodeList := oContext.CurrentNode.ChildNodes
    else
      { Yes. Need to select child nodes. }
      oNodeList := oContext.CurrentNode.SelectNodes(FSelect);

    { Any nodes to process? }
    if (oNodeList.Length > 0) then begin
      { Yes. }
      UpdateNSMgr(oContext);
      oStylesheet := ParentStylesheet;
      oTemplateMgr := oStylesheet.Mediator.TemplateMgr;
      oNewContext := TXpXSLContext.Create;
      try
        oStylesheet.Mediator.PushContext(oNewContext);
        oNewContext.GlobalVars := oContext.GlobalVars;
        oNewContext.CurrentNodeList := oContext.CurrentNodeList;       {!!.55}
        oNewContext.CurrentNode := oContext.CurrentNode;               {!!.55}
        oNewContext.MakeVarBinding;

        { Execute the children of the xsl:apply-templates element
          (e.g., xsl:sort, xsl:with-param). This sets up the sort keys
           & instantiates the parameters. }
        for wInx2 := 0 to Pred(ChildNodes.Length) do begin
          { Future: If item is not an XSL element and is not a comment then
            raise error. }
{Begin !!.55}
          if ChildNodes.Item(wInx2) is TXpBaseXSLElement then begin
            oChildXSLNode := TXpBaseXSLElement(ChildNodes.Item(wInx2));
            oChildXSLNode.Preprocess(oNewContext);
            oChildXSLNode.Execute(oNewContext);
          end;
{End !!.55}
        end;

//        oNewContext.CurrentNodeList := oNodeList;                      {!!.55}{Deleted !!.57}

        { Sort the current node list. }
        if FKeyDefinitions.Count > 0 then
          oContext.CurrentNodeList := FSorter.Sort(oNodeList, FKeyDefinitions)
        else
          oContext.CurrentNodeList := oNodeList;
        oNewContext.CurrentNodeList := oContext.CurrentNodeList;       {!!.57}
        try
          { Find a template for each child node of the current node in the XML
            document. }
          for wInx := 0 to pred(oContext.CurrentNodeList.Length) do begin
            oNode := TXpElement(oContext.CurrentNodeList.Item(wInx));
            { Is this an element we should ignore? }
            if not oStylesheet.Ignore(oNode) then begin
              { Is there a template that matches the document element? }
              oTemplate := oTemplateMgr.Match(TXpElement(oNode), FMode);
              if oTemplate <> nil then begin
                oNewContext.MakeForEachVarBinding;
                try
                  { Yes. Execute the template. Note that other nodes in the
                    XML document are processed only when xsl:apply-templates
                    is called. }
                  oNewContext.CurrentNode := oNode;
                  oTemplate.Execute(oNewContext);
                finally
                  oNewContext.DropForEachVarBinding;
                end;
              end;  { if }
            end;  { if }
          end;  { for }
        finally
          if FKeyDefinitions.Count > 0 then
            oContext.CurrentNodeList.Free;
          oContext.RestoreCurNodeList;
        end;
      finally
        oStylesheet.Mediator.PopContext;
        oNewContext.DropVarBinding;
        oNewContext.Free;
      end;
    end;
  finally
    if FSelect <> '' then
      oNodeList.Free;
  end;
end;
{--------}
procedure TXpXSLApplyTemplates.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Mode attribute. }
    FMode := GetAttribute(XpsMode);

    { Select attribute. }
    FSelect := GetAttribute(XpsSelect);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLApplyTemplates.Valid(oContext : TXpXSLContext;
                                var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if not Result then
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLAttribute===================================================}
constructor TXpXSLAttribute.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLAttribute.Execute(oContext : TXpXSLContext);
var
  bHasPrefix : Boolean;
  oAttr : TXpNode;
  oElement : TXpElement;
  oFilter : TXpFilterXML;
  oMediator : TXpElementMediator;
  oStylesheet : TXpXSLStylesheet;
  sName,
  sNamespace,
  sPrefix,
  sURI,
  sValue : DOMString;
  wPos : Integer;
begin
  { Note: For xsl:attribute elements that are children of xsl:attribute-set
    elements, this method is called only when the attribute set is
    instantiated. }

  { Is there a current element & does it have any child elements? }
  oStylesheet := ParentStylesheet;
  oMediator := oStylesheet.Mediator;
  oElement := oMediator.GetCurrentResultTreeElement;
  if oElement = nil then
    raise EXpXSLException.Create(Format(sXSLAttribNoCurrentElement,
                                        [QuotedStr(FName),
                                         QuotedStr(FNamespace)]))
  else if oElement.ChildNodes.Length > 0 then
    raise EXpXSLException.Create(Format(sXSLAttribElNoChildNodes,
                                        [QuotedStr(FName),
                                         QuotedStr(FNamespace)]));

  UpdateNSMgr(oContext);

  { Evaluate the name and namespace attributes. }
  sName := EvaluateAttrib(oContext, FName);
  sNamespace := EvaluateAttrib(oContext, FNamespace);

  { Requirement: The attribute name may not be xmlns or xmlns:* }
  if XpStartsWith(XpsXmlns, sName) then
    raise EXpXSLException.Create(Format(sXSLAttribNoXMLNS,
                                        [QuotedStr(FName),
                                         QuotedStr(FNamespace),
                                         QuotedStr(sName)]));

  { Generate the value of the attribute. }
  oFilter := TXpFilterXML.Create(nil);
  try
    oMediator.PushFilter(oFilter);
    ExecuteChildren(oContext);
    sValue := oFilter.StringValue;
  finally
    oMediator.PopFilter;
    oFilter.Free;
  end;

  { If no namespace specified and sName contains a namespace prefix then
    the namespace must be in scope at this point. }
  wPos := XpPos(':', sName);
  bHasPrefix := (wPos > 0);
  if bHasPrefix then begin
    sPrefix := XpCopy(sName, 1, wPos - 1);
    if (sNamespace = '') then begin
      sURI := ResolveNSPrefix(sPrefix);
      if sURI = '' then
        raise EXpXSLException.Create
                (Format(sXSLUnresolvedNSPrefix,
                        [QuotedStr(sPrefix),
                         'name attribute having value ' + QuotedStr(sName) +
                         ' for ' + NodeName +  ' element']))
    end;  { if }
  end;  { if }

  { If namespace specified but no prefix then must generate a prefix.
    If namespace & prefix both specified but prefix conflicts with prefix
    used by another attribute on same element then must create a new prefix. }
  if sNamespace <> '' then begin
    { Prefix specified? }
    if bHasPrefix then begin
      { Yes. Does this prefix conflict with the prefix on an existing
        attribute? }
      if oElement.NamespaceList <> nil then begin
        oAttr := oElement.NamespaceList.GetNamedItem(XpsXmlns + ':' + sPrefix);
        if (oAttr <> nil) and (oAttr.NodeValue <> sNamespace) then begin
          { Yes. Generate a unique prefix. }
          sPrefix := oStylesheet.NSMgr.GetUniquePrefix;
          sName := sPrefix + XpCopy(sName, wPos, Length(sName) - wPos + 1);
        end;
      end;
    end
    else
      { No prefix. Generate new prefix. }
      sPrefix := oStylesheet.NSMgr.GetUniquePrefix;
  end;

  oMediator.CopyAttribute(sName, sValue, Self);
  if sNamespace <> '' then begin
    oMediator.CopyAttribute(XpsXmlns + ':' + sPrefix, sNamespace, Self);
    oStylesheet.NSMgr.AddNS(sPrefix, sNamespace);
  end;

end;
{--------}
procedure TXpXSLAttribute.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    FNamespace := GetAttribute(XpsNamespace);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLAttribute.Valid(oContext : TXpXSLContext;
           var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Must have a name attribute }
      FName := GetAttribute(XpsName);
      Result := (FName <> '');
      if not Result then
        ErrMsg := MissingAttrib(XpsName);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLAttributeSet================================================}
constructor TXpXSLAttributeSet.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
  FCircChecked := False;
  FTag := 0;
end;
{--------}
destructor TXpXSLAttributeSet.Destroy;
var
  wInx : Longint;
begin
  if FUsed <> nil then begin
    for wInx := Pred(FUsed.Count) downto 0 do
      TXpDOMStringItem(FUsed.Pointers[wInx]).Free;
    FUsed.Free;
  end;
  inherited;
end;
{--------}
procedure TXpXSLAttributeSet.Execute(oContext : TXpXSLContext);
var
  oMediator : TXpElementMediator;
begin
  { NOTE: This method is called from xsl:copy, xsl:element, or LRE when a
    use-attribute-sets attribute is encountered. }

  oMediator := ParentStylesheet.Mediator;

  { Need to check for circular references? }
  if not FCircChecked then begin
    if oMediator.AttrSetHasCircularRef(FName) then
      raise EXpXSLException.Create(Format(sXSLAttrSetCircRef,
                                          [QuotedStr(FName)]));
    FCircChecked := True;
  end;

  { Use the referenced attribute sets. This executes each referenced attribute
    set. }
  if (FUsed <> nil) and (FUsed.Count > 0) then
    oMediator.AttrSetMergeSets(Self, oContext);

  { Execute the child attribute elements. This will place the attributes
    on the current element of the result tree. }
  ExecuteChildren(oContext);

end;
{--------}
function TXpXSLAttributeSet.GetUsed(const wInx : Longint) : DOMString;
begin
  Result := '';
  if FUsed <> nil then
    Result := TXpDOMStringItem(FUsed.Pointers[wInx]).sString;
end;
{--------}
procedure TXpXSLAttributeSet.Preprocess(oContext : TXpXSLContext);
var
  sUsed : DOMString;
begin
  if not FInitialized then begin
    inherited;
    sUsed := GetAttribute(XpsUseAttrSets);
    if sUsed <> '' then
      FUsed := ParseUseAttrSet(sUsed);
    ParentStylesheet.Mediator.AddAttrSet(Self);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLAttributeSet.UsedCount : Longint;
begin
  if FUsed = nil then
    Result := 0
  else
    Result := FUsed.Count;
end;
{--------}
function TXpXSLAttributeSet.Valid(oContext : TXpXSLContext;
           var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Must have a name attribute }
      FName := ExpandName(GetAttribute(XpsName));
      Result := (FName <> '');
      if not Result then
        ErrMsg := MissingAttrib(XpsName);
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLCallTemplate================================================}
constructor TXpXSLCallTemplate.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLCallTemplate.Execute(oContext : TXpXSLContext);
var
  oChildXSLNode : TXpBaseXSLElement;
  oTemplate : TXpXSLTemplate;
  oTemplateMgr : TXpBaseTemplateManager;
  wInx : Integer;
begin
  UpdateNSMgr(oContext);
  oTemplateMgr := ParentStylesheet.Mediator.TemplateMgr;
  try
    oContext.MakeVarBinding;

    { Execute the children of the xsl:call-template element
      (e.g., xsl:with-param). This instantiates the parameters. }
    for wInx := 0 to Pred(ChildNodes.Length) do begin
      if ChildNodes.Item(wInx) is TXpBaseXSLElement then begin
        oChildXSLNode := TXpBaseXSLElement(ChildNodes.Item(wInx));
        oChildXSLNode.Preprocess(oContext);
        oChildXSLNode.Execute(oContext);
      end;
    end;

    { Is there a template that matches the specified name? }
    oTemplate := oTemplateMgr.MatchName(FName);
    if oTemplate <> nil then
      { Yes. Execute the template. }
      oTemplate.Execute(oContext);
  finally
    oContext.DropVarBinding;
  end;
end;
{--------}
function TXpXSLCallTemplate.Valid(oContext : TXpXSLContext;
                              var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Verify it has the mandatory name attribute. }
    FName := GetAttribute(XpsName);
    if FName = '' then begin
      Result := False;
      ErrMsg := sXSLVarNameRequired;
    end;  { if }
  end;  { if }
end;
{=====================================================================}

{===TXpXSLChoose======================================================}
constructor TXpXSLChoose.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLChoose.Execute(oContext : TXpXSLContext);
var
  anInx : Integer;
  ConditionMet : Boolean;
  oNode : TXpNode;
  oOtherwise : TXpXSLOtherwise;
  oWhen : TXpXSLWhen;
begin
  ConditionMet := False;
  oOtherWise := nil;
  { Evaluate each xsl:when element until we find one that evaluates to True. }
  for anInx := 0 to Pred(ChildNodes.Length) do begin
    oNode := ChildNodes.Item(anInx);
    { Is this an xsl:otherwise element? }
    if oNode is TXpXSLOtherwise then
      { Yes. Save it for later. }
      oOtherwise := TXpXSLOtherwise(oNode)
    else begin
      { If this is an xsl:when element, see if its condition is met. }
      if oNode is TXpXSLWhen then begin
        oWhen := TXpXSLWhen(oNode);
        ConditionMet := oWhen.ConditionMet(oContext);
        if ConditionMet then begin
          oWhen.Execute(oContext);
          break;
        end;
      end;  { if }
    end;  { if }
  end;  { for }

  { No conditions met & an xsl:otherwise element was specified? }
  if (not ConditionMet) and (oOtherwise <> nil) then
    oOtherwise.Execute(oContext);
end;
{--------}
function TXpXSLChoose.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
var
  anInx : Integer;
  oNode : TXpNode;
  WhenFound : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    WhenFound := False;
    Result := InTemplateBody;
    if Result then begin
      { Must have at least one xsl:when element. }
      for anInx := 0 to Pred(ChildNodes.Length) do begin
        oNode := ChildNodes.Item(anInx);
        { Is this an xsl:when element? }
        if (oNode.NodeName = XpsXSLWhen) then begin
          WhenFound := True;
          TXpBaseXSLElement(oNode).Preprocess(oContext);
        end
        else if (not (oNode is TXpComment)) and
                (oNode.NodeName <> XpsXSLOtherwise) then begin
          { No. Is it something other than an xsl:otherwise element? }
          Result := False;
          ErrMsg := Format(sXSLInvalidChooseElement,[QuotedStr(oNode.NodeName)]);
          Exit;
        end;
      end;
      Result := WhenFound;
      if (not Result) then
        ErrMsg := sXSLWhenRequired;
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLComment=====================================================}
{Begin !!.57}
constructor TXpXSLComment.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
end;
{--------}
{End !!.57}
procedure TXpXSLComment.Execute(oContext : TXpXSLContext);
var
  bIgnore,
  bRaiseError : Boolean;
  oList : TXpNodeList;
  oMediator : TXpElementMediator;
  oXMLFilter : TXpFilterXML;
  sComment : DOMString;
  wInx : Integer;
begin
  bIgnore := False;
  sComment := '';
  { Build the message by executing the children. }
  oList := TXpNodeList.Create;
  try
    oMediator := ParentStylesheet.Mediator;
    oXMLFilter := TXpFilterXML.Create(nil);
    try
      oMediator.PushFilter(oXMLFilter);
      ExecuteChildren(oContext);
      if oXMLFilter.ResultTreeRoot <> nil then
        oList.CopyList(oXMLFilter.ResultTreeRoot.ChildNodes);
      { Verify that all child nodes are text nodes. }
      for wInx := 0 to Pred(oList.Length) do
        if not (oList.Item(wInx) is TXpText) then begin
          { Ask the XSL processor how the situation should be resolved. }
          bRaiseError := True;
          oMediator.ResolveConflict(Self, xpctCommentNodes, bRaiseError);
          if bRaiseError then
            raise EXpXSLException.Create(Format(sXSLOnlyTextNodes,
                                                [NodeName]))
          else
            bIgnore := True;
          break;
        end;

      if (not bIgnore) and (oXMLFilter.ResultTreeRoot <> nil) then begin
        sComment := oXMLFilter.ResultTreeRoot.StringValue;

        { See if the comment contains -- or ends with -. }
        if (XpPos('--', sComment) > 0) or
           (sComment[Length(sComment)] = '-') then
          raise EXpXSLException.Create(sXSLCommentChars)
      end; { if }

    finally
      oMediator.PopFilter;
      oXMLFilter.Free;
    end;
  finally
    oList.Free;
  end;
  { Call the OnComment event handler in the XSL processor. }
  if not bIgnore then
    oMediator.AddComment(sComment);
end;
{--------}
procedure TXpXSLComment.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLComment.Valid(oContext : TXpXSLContext;
                         var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if not Result then
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLCopy========================================================}
constructor TXpXSLCopy.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
end;
{--------}
destructor TXpXSLCopy.Destroy;
var
  wInx : Longint;
begin
  if FUsed <> nil then begin
    for wInx := Pred(FUsed.Count) downto 0 do
      TXpDOMStringItem(FUsed.Pointers[wInx]).Free;
    FUsed.Free;
  end;
  inherited;
end;
{--------}
procedure TXpXSLCopy.Execute(oContext : TXpXSLContext);
var
  oElement : TXpElement;
  oMediator : TXpElementMediator;
  oNode,
  oNSNode : TXpNode;
  oStylesheet: TXpXSLStylesheet;
  sPrefix,
  sURI : DOMString;
  wInx,
  wPos : Integer;
begin
  oStylesheet := ParentStylesheet;
  oMediator := oStylesheet.Mediator;

  UpdateNSMgr(oContext);

  { Get the current node in the XML document. }
  oNode := oContext.CurrentNode;
  { Is this the document root element? }
  case oNode.NodeType of
    ATTRIBUTE_NODE :
      begin
        oElement := oMediator.GetCurrentResultTreeElement;
        if oElement = nil then
          raise EXpXSLException.Create(Format(sXSLCopyNoCurrentElement,
                                              [QuotedStr(oNode.NodeName)]))
        else if oElement.ChildNodes.Length > 0 then
          raise EXpXSLException.Create(Format(sXSLCopyElNoChildNodes,
                                              [QuotedStr(oNode.NodeName)]));
        oMediator.CopyAttribute(oNode.NodeName, oNode.NodeValue, Self);
      end;
    COMMENT_NODE :
      oMediator.AddComment(oNode.NodeValue);
    DOCUMENT_NODE :
      { Instantiate the template body. }
      ExecuteChildren(oContext);
    ELEMENT_NODE :
      begin
        { Create element on result tree. }
        oElement := OwnerDocument.CreateElement(oNode.NodeName);
        try
          { Does the current element have a namespace prefix?}
          wPos := XpPos(':', oNode.NodeName);
          if (wPos > 0) then begin
            sPrefix := XpCopy(oNode.NodeName, 1, wPos - 1);
            sURI := ResolveNSPrefix(sPrefix);
            if sURI = '' then
              raise EXpXSLException.Create
                      (Format(sXSLUnresolvedNSPrefix,
                              [QuotedStr(sPrefix),
                               'current element having name ' +
                               oNode.NodeName + ' begin processed by ' +
                               'an xsl:copy element']))
          end;  { if }

          oMediator.StartElement(oElement);

          { Generate attributes from the referenced attribute sets. }
          if FUsed <> nil then
            for wInx := 0 to Pred(FUsed.Count) do
              oMediator.AttrSetMergeSetsViaName
               (TXpDOMStringItem(FUsed.Pointers[wInx]).sString, oContext);

          { Copy namespace nodes. }
          if oNode.NamespaceList <> nil then
            for wInx := 0 to Pred(oNode.NamespaceList.Length) do begin
              oNSNode := oNode.NamespaceList.Item(wInx);
              oMediator.CopyAttribute(oNSNode.NodeName, oNSNode.NodeValue, Self);
            end;  { for }

          { Instantiate template body. }
          ExecuteChildren(oContext);

          oMediator.EndElement(oElement);
        finally
          oElement.Release;
        end;
      end;
    PROCESSING_INSTRUCTION_NODE :
      { Create a PI node in the result tree. }
      oMediator.AddPI(TXpProcessingInstruction(oNode).Target,
                      TXpProcessingInstruction(oNode).Data);
    TEXT_NODE :
      { Create a text node in the result tree. }
      oMediator.CopyTextNode(TXpText(oNode));
  end;  { case }
end;
{--------}
procedure TXpXSLCopy.Preprocess(oContext : TXpXSLContext);
var
  sUsed : DOMString;
begin
  if not FInitialized then begin
    inherited;
    sUsed := GetAttribute(XpsUseAttrSets);
    if sUsed <> '' then
      FUsed := ParseUseAttrSet(sUsed);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLCopy.Valid(oContext : TXpXSLContext;
                         var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if not Result then
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLCopyOf======================================================}
procedure TXpXSLCopyOf.CopyNode(oContext : TXpXSLContext;
                                oMediator : TXpElementMediator;
                                oNode : TXpNode);
var
  wInx : Integer;
begin
  { Assumption: All nodes other than element and text nodes
    are ignored. }
  case oNode.NodeType of
    ATTRIBUTE_NODE :
      ProcessAttribPrim(oContext, TXpAttribute(oNode), True);
    ELEMENT_NODE :
      begin
        oMediator.StartElement(oNode);
        ProcessAttribs(oContext, TXpElement(oNode), True);
        for wInx := 0 to Pred(oNode.ChildNodes.Length) do
          CopyNode(oContext, oMediator, oNode.ChildNodes.Item(wInx));
        oMediator.EndElement(oNode);
      end;
    TEXT_NODE : oMediator.CopyTextNode(TXpText(oNode));
{Begin !!.57}
    CDATA_SECTION_NODE :
      oMediator.CopyCDataNode(TXpCDataSection(oNode));
    PROCESSING_INSTRUCTION_NODE :
      oMediator.AddPI(TXpProcessingInstruction(oNode).Target,
                      TXpProcessingInstruction(oNode).Data);
    COMMENT_NODE :
      oMediator.AddComment(oNode.NodeValue);
{End !!.57}
  end;  { case }
end;
{--------}
procedure TXpXSLCopyOf.Execute(oContext : TXpXSLContext);
var
  oMediator : TXpElementMediator;
  oValue : TXpValue;
  oNodeList : TXpNodeList;
  wInx : Integer;
begin
  UpdateNSMgr(oContext);
  oValue := oContext.CurrentNode.Select(FSelect, oContext.CurrentNodeList);
  try
    oMediator := ParentStylesheet.Mediator;
    case oValue.ValueType of
      xpetNodeSet :
        begin
          oNodeList := oValue.AsNodeSet;
          for wInx := 0 to Pred(oNodeList.Length) do
            CopyNode(oContext, oMediator, oNodeList.Item(wInx));
        end;
    else
      oMediator.CopyText(oValue.AsString);
    end;  { case }
  finally
    oValue.Free;
  end;
end;
{--------}
function TXpXSLCopyOf.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Verify it has the mandatory name attribute. }
    FSelect := GetAttribute(XpsSelect);
    Result := (FSelect <> '');
    if Result then begin
      { Is it within a template body? }
      Result := InTemplateBody;
      if not Result then
        ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
    end
    else
      ErrMsg := Format(sXSLSelectRequired, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLDecimalFormat==============================================}
constructor TXpXSLDecimalFormat.Create;
begin
  inherited;
  { Set default values. }
  FDecSep := csDecSep;
  FDigit := csDigit;
  FGroupSep := csGroupSep;
  FInfinity := 'Infinity';
  FLastPattern := '';
  FMinusSign := csMinus;
  FName := '';
  FNaN := 'NaN';
  FPattSep := csPattSep;
  FPercent := csPercent;
  FPerMille := DOMChar($2030);
  FZeroDigit := csZeroDigit;
end;
{--------}
procedure TXpXSLDecimalFormat.AnalyzePattern(const sPattern : DOMString);
{ NOTE: This implementation of format-number does not support exponents.
  The XSLT specification, Section 12.3, states "The format pattern string is in
  the syntax specified by the JDK 1.1 DecimalFormat class". Information about
  the pattern as taken from that class is as follows:

  The following shows the structure of the pattern.
    pattern    := subpattern[;subpattern]
    subpattern := [prefix]integer[.fraction][suffix]
    prefix     := '\\u0000'..'\\uFFFD' - specialCharacters
    suffix     := '\\u0000'..'\\uFFFD' - specialCharacters
    integer    := '#'* '0'* '0'
    fraction   := '0'* '#'*
    Notation:
      X*       0 or more instances of X
      (X | Y)  either X or Y.
      X..Y     any character from X up to Y, inclusive.
      S - T    characters in S, except those in T

  The first subpattern is for positive numbers. The second (optional) subpattern
  is for negative numbers. (In both cases, ',' can occur inside the integer
  portion--it is just too messy to indicate in BNF.)
  Here are the special characters used in the parts of the subpattern, with
  notes on their usage.

    Symbol Meaning
     0      a digit
     #      a digit, zero shows as absent
     .      placeholder for decimal separator
     ,      placeholder for grouping separator.
     ;      separates formats.
     -      default negative prefix.
     %      multiply by 100 and show as percentage
     ?      multiply by 1000 and show as per mille
     ¤      currency sign; character $A4; replaced by currency symbol; if
              doubled, replaced by international currency symbol.
              If present in a pattern, the monetary decimal separator
              is used instead of the decimal separator.
      X      any other characters can be used in the prefix or suffix
     '      used to quote special characters in a prefix or suffix.
}
type
  ParseStage = (psPrefix, psDigits, psSuffix);
var
  bInQuote : Boolean;       { inside quotes? }
  eStage : ParseStage;      { current stage }
  wAddChar,                 { character to be added to compiled pattern }
  wChar : DOMChar;          { current character being analyzed }
  wAddStr : DOMString;      { string to be added to compiled pattern }
  wInx,                     { position in sPattern }
  wPattLen,                 { length of pattern }
  wTotalDigits : Integer;   { total # digits found in pattern }
begin
  FLastPattern := sPattern;
  FMultiplier := 1;
  FNegPattern := False;

  bInQuote := False;
  eStage := psPrefix;
  FDecPt := -1;
  FDigLeft := 0;
  FDigRight := 0;
  FDigZero := 0;
  FGroupSize := -1;
  FMaxFracDigits := 0;
  FMaxIntDigits := 0;
  FMinFracDigits := 0;
  FMinIntDigits := 0;
  FNegPrefix := '';
  FNegSuffix := '';
  FPosPrefix := '';
  FPosSuffix := '';
  FZeroOffset := Ord(FZeroDigit) - Ord('0');
  wInx := 1;
  wPattLen := Length(sPattern);

  while wInx <= wPattLen do begin
    wChar := sPattern[wInx];
    wAddChar := #0;
    wAddStr := '';
    case eStage of
      psPrefix, psSuffix :
        begin
          { Are we inside quotes? }
          if bInQuote then begin
            { Is this a quote? }
            if wChar = csQuote then begin
              { Yes. It may be the end quote or a double quote. }
              if (wInx + 1 <= wPattLen) and (sPattern[wInx + 1] = csQuote) then
                { Double quote. Add a single quote to the prefix/suffix. }
                wAddChar := csQuote
              else
                bInQuote := False;
            end
          end  { if in quotes }
          else begin
            { Starting the next phase? }
            if (wChar = FDigit) or
               (wChar = FZeroDigit) or
               (wChar = FGroupSep) or
               (wChar = FDecSep) then begin
              if eStage = psSuffix then
                raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                    [sPattern,
                                                     sXSLInvalidSuffixChars]))
              else begin
                { Changing to digits stage. Move current position back one so
                  that digit is correctly processed. }
                eStage := psDigits;
                dec(wInx);
              end;  { if }
            end  { if }
            else if wChar = FPattSep then begin
              { Have we encountered digits for the positive pattern? }
              if eStage = psPrefix then
                { No. Raise an exception. }
                raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                    [sPattern,
                                                     sXSLNoDigitsPosPatt]))
              else begin
                { Yes. Move to prefix stage for negative pattern. }
                eStage := psPrefix;
                FNegPattern := True;
              end;
            end  { if pattern separator }
            else if wChar = csQuote then begin
              { Is this a double quote? }
              if (wInx + 1 <= wPattLen) and (sPattern[wInx + 1] = csQuote) then
                { Double quote. Add a single quote to the prefix/suffix. }
                wAddChar := csQuote
              else
                bInQuote := True;
            end { if quote }
            else if wChar = csMinus then
              wAddChar := FMinusSign
            else if wChar = FPercent then begin
              if FMultiplier > 1 then
                raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                    [sPattern,
                                                     sXSLDupMultipliers]))
              else begin
                FMultiplier := 100;
                wAddChar := FPercent;
              end;
            end  { if percent }
            else if wChar = FPerMille then begin
              if FMultiplier > 1 then
                raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                    [sPattern,
                                                     sXSLDupMultipliers]))
              else begin
                FMultiplier := 1000;
                wAddChar := FPerMille;
              end;
            end { if per mille }
            else if wChar = csCurrency then begin
              { Is this a double currency symbol? }
              if (wInx + 1 <= wPattLen) and (sPattern[wInx + 1] = csCurrency) then
                { Yes. Add international currency symbol. }
                wAddChar := csCurrency
              else
                { No. Add locale currency symbol. }
                wAddStr := CurrencyString;
            end  { if currency }
            else
              wAddChar := wChar;
          end;  { if not in quotes }

          if wAddChar <> #0 then
            case eStage of
              psPrefix :
                if FNegPattern then
                  XpAppendChar(wAddChar, FNegPrefix)
                else
                  XpAppendChar(wAddChar, FPosPrefix);
              psSuffix :
                if FNegPattern then
                  XpAppendChar(wAddChar, FNegSuffix)
                else
                  XpAppendChar(wAddChar, FPosSuffix);
            end;  { case }

          if wAddStr <> '' then
            case eStage of
              psPrefix :
                if FNegPattern then
                  FNegPrefix := FNegPrefix + wAddStr
                else
                  FPosPrefix := FPosPrefix + wAddStr;
              psSuffix :
                if FNegPattern then
                  FNegSuffix := FNegSuffix + wAddStr
                else
                  FPosSuffix := FPosSuffix + wAddStr;
            end;  { case }
        end;
      psDigits :
        { When parsing the negative pattern, only the prefix & suffix are
          taken into account. The digits are ignored. }
        if FNegPattern then begin
          if (wChar <> FGroupSep) and
             (wChar <> FDecSep) and
             (wChar <> FDigit) and
             (wChar <> FZeroDigit) then begin
            { Moving into the suffix stage. Decrement current position
              so that current character is correctly processed. }
            eStage := psSuffix;
            dec(wInx);
          end;
        end
        else if wChar = FGroupSep then
          FGroupSize := 0
        else if wChar = FDecSep then begin
          { Is this the 2nd decimal separator we've encountered? }
          if FDecPt > 0 then
            raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                [sPattern,
                                                 sXSLMultDecSep]));

          FDecPt := FDigLeft + FDigZero;
        end  { if decimal separator }
        else if wChar = FDigit then begin
          if FDigZero = 0 then
            inc(FDigLeft)
          else begin
            if FDecPt = -1 then
              raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                  [sPattern,
                                                   sXSLDecSepPos]));

            inc(FDigRight);
          end;

          { Update group size? }
          if (FGroupSize >= 0) and (FDecPt = -1) then
            inc(FGroupSize);
        end  { if # }
        else if wChar = FZeroDigit then begin
          { Have we already encountered # characters to the right of 0
            characters? }
          if FDigRight > 0 then
            raise EXpXSLException.Create(Format(sXSLMalformedPattern,
                                                [sPattern,
                                                 sXSLUnexpectedZeroDigit]));
          inc(FDigZero);
          { Update group size? }
          if (FGroupSize >= 0) and (FDecPt = -1) then
            inc(FGroupSize);
        end  { if 0 }
        else begin
          { Must be moving into the suffix stage. Decrement current position
            so that current character is correctly processed. }
          eStage := psSuffix;
          dec(wInx);
        end;
    end;  { case }

    { Move to next character in pattern. }
    inc(wInx);

  end;  { while }

  { Adjust digit counts for the following cases:
    #.##, #., .# }
  if (FDigZero = 0) and (FDigLeft > 0) and (FDecPt > 0) then begin
    wInx := FDecPt;
    { Is this the .# case? }
    if wInx = 0 then
      inc(wInx);
    FDigRight := FDigLeft - wInx;
    FDigLeft := wInx - 1;
    FDigZero := 1;
  end;

  { Calculate min and max for integer and fraction portions of number. }
  wTotalDigits := FDigLeft + FDigZero + FDigRight;
  if FDecPt > 0 then begin
    FMaxFracDigits := wTotalDigits - FDecPt;
    FMinFracDigits := FDigLeft + FDigZero - FDecPt;
      { ##0.0# }
  end;
  FMaxIntDigits := 309;
  if FDecPt = -1 then
    FMinIntDigits := wTotalDigits - FDigLeft
  else
    FMinIntDigits := FDecPt - FDigLeft;
end;
{--------}
function TXpXSLDecimalFormat.ApplyPattern(const sNumber : DOMString) : DOMString;
var
  bNegative,
  bNaN : Boolean;
  sPrefix,                                                             {!!.55}
  sStripped : DOMString;
  wCount,
  wDecPos,
  wDigInx,               { current digit being formatted }
  wInx,
  wLen : Integer;
  wNum : Double;
begin
  { Is this NaN or negative? }
  bNaN := False;
  bNegative := False;
  wNum := 0;
  try
    wNum := XpStrToFloat(sNumber);                                     {!!.57}
    wNum := wNum * FMultiplier;
    bNegative := (wNum < 0);
  except
    bNaN := True;
  end;

  if bNaN then
    Result := FNaN
  else begin
    { Convert number back to string, removing the leading minus sign if
      the number is negative. }
    sStripped := XpStripChar(XpFloatToStr(wNum), '-', True, False);    {!!.57}

    wLen := Length(sStripped);

    { Do we have a negative number but only a positive pattern? }
    if bNegative and (not FNegPattern) then
      { Yes. Result will contain positive pattern prefixed with minus sign. }
      sPrefix := FMinusSign                                            {!!.55}
    else
      sPrefix := '';                                                   {!!.55}

    { Handle prefix }
    if (not bNegative) or (not FNegPattern) then
      sPrefix := sPrefix + FPosPrefix                                  {!!.55}
    else
      sPrefix := sPrefix + FNegPrefix;                                 {!!.55}

    { How many integer digits will be displayed? The number depends upon:
      1. # minimum digits in pattern
      2. # maximum digits in pattern
      3. # digits in the number to be formatted
    }
    wDecPos := XpPos(csDecSep, sStripped);
    if wDecPos = 0 then
      wDecPos := Length(sStripped) + 1;
    wDigInx := 1;
    wCount := FMinIntDigits;
    { Is the number a double & does it contain more integer digits than the
      pattern's minimum? }
    if (wDecPos > 0) and (wDecPos > (wCount + 1)) then
      { Yes. Increase the minimum. }
      wCount := wDecPos - 1;

    { Are there more digits in the number than the maximum # of integer
      digits? }
    if wCount > FMaxIntDigits then begin
      { Yes. Decrease to max int digits. Note that this will not usually
        happen. }
      wCount := FMaxIntDigits;
      wDigInx := wDecPos - wCount;
        { Chops off leading digits that are beyond maximum. }
    end;

    { Move through the integer digits. }
    for wInx := wCount downto 1 do begin
      { Output leading zero or digit from number? }
      if (wInx < wDecPos) and (wDigInx <= wLen) then begin
        XpAppendChar(DOMChar(Ord(sStripped[wDigInx]) + FZeroOffset), Result);
        inc(wDigInx);
      end
      else
        XpAppendChar(FZeroDigit, Result);

      { Time for a grouping separator? }
      if (FGroupSize > 0) and (wInx > 1) and (Pred(wInx) mod FGroupSize = 0) then
        XpAppendChar(FGroupSep, Result);
    end;  { for }

    { Output a decimal separator & fractional part? }
    if (FMinFracDigits > 0) or ((FDecPt > -1) and (wDecPos < wLen)) then begin
      inc(wDigInx);

      { Is this a case of .99 or similar? }
      if wDecPos = 1 then
        { Yes. Display leading zero before decimal separator. }
        XpAppendChar(FZeroDigit, Result);

      XpAppendChar(FDecSep, Result);
      wInx := 1;
      repeat
        if wDigInx <= wLen then begin
          XpAppendChar(DOMChar(Ord(sStripped[wDigInx]) + FZeroOffset), Result);
          inc(wDigInx);
        end
        else
          XpAppendChar(FZeroDigit, Result);
        inc(wInx);
      until ((wInx > FMinFracDigits) and (wDigInx > wLen)) or
            (wInx > FMaxFracDigits);

      { Are there more digits in the number than are displayed by the pattern
        & should the last digit be rounded up? }
      if (wDigInx <= wLen) and
         (sStripped[wDigInx] > '4') then
        XpRoundNumStr(FGroupSep, FDecSep, FGroupSize, Result);         {!!.55}
    end  { if }
    else begin
      { If number has a fractional part then see if last integer digit needs
        to be rounded up. }
      if (wDecPos < wLen) and
         (sStripped[wDigInx + 1] > '4') then
        XpRoundNumStr(FGroupSep, FDecSep, FGroupSize, Result);         {!!.55}
    end;

    { Prepend the prefix. }                                            {!!.55}
    Result := sPrefix + Result;                                        {!!.55}

    { Handle the suffix. }
    if (not bNegative) or (not FNegPattern) then
      Result := Result + FPosSuffix
    else
      Result := Result + FNegSuffix;

  end;
end;
{--------}
function TXpXSLDecimalFormat.FormatNumber(const sNumber, sPattern : DOMString) : DOMString;
begin
  if sPattern <> FLastPattern then
    AnalyzePattern(sPattern);
  Result := ApplyPattern(sNumber);
end;
{--------}
function TXpXSLDecimalFormat.IsCompatibleWith(oDecimalFmt : TXpXSLDecimalFormat) : Boolean;
begin
  Result :=
    (FDecSep = oDecimalFmt.DecimalSeparator) and
    (FDigit = oDecimalFmt.Digit) and
    (FGroupSep = oDecimalFmt.GroupingSeparator) and
    (FInfinity = oDecimalFmt.Infinity) and
    (FMinusSign = oDecimalFmt.MinusSign) and
    (FNaN = oDecimalFmt.NaN) and
    (FPattSep = oDecimalFmt.PatternSeparator) and
    (FPercent = oDecimalFmt.Percent) and
    (FPerMille = oDecimalFmt.PerMille) and
    (FZeroDigit = oDecimalFmt.ZeroDigit);
end;
{--------}
procedure TXpXSLDecimalFormat.Preprocess(oContext : TXpXSLContext);
var
  sAttr : DOMString;
begin
  if not FInitialized then begin
    inherited;

    { Fetch and check attributes. }
    sAttr := GetAttribute(XpsDecimalSep);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FDecSep := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                            [QuotedStr(XpsDecimalSep),
                                             QuotedStr(NodeName),
                                             QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsDigit);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FDigit := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                            [QuotedStr(XpsDigit),
                                             QuotedStr(NodeName),
                                             QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsGroupSep);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FGroupSep := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsGroupSep),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsInfinity);
    if sAttr <> '' then
      FInfinity := sAttr;

    sAttr := GetAttribute(XpsMinusSign);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FMinusSign := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsMinusSign),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsName);
    if sAttr <> '' then
      FName := ExpandName(sAttr);
        { Name comparison uses URI, not prefix. }

    sAttr := GetAttribute(XpsNaN);
    if sAttr <> '' then
      FNaN := sAttr;

    sAttr := GetAttribute(XpsPattSep);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FPattSep := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsPattSep),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsPercent);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FPercent := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsPercent),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsPerMille);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FPerMille := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsPerMille),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    sAttr := GetAttribute(XpsZeroDigit);
    if (sAttr <> '') then
      if Length(sAttr) = 1 then
        FZeroDigit := sAttr[1]
      else
        raise EXpXSLException.Create(Format(sXSLCharAttrib,
                                     [QuotedStr(XpsZeroDigit),
                                      QuotedStr(NodeName),
                                      QuotedStr(sAttr)]));

    { Register self with the XSL processor's decimal format manager. }
    ParentStylesheet.Mediator.GetDecimalFmtMgr.AddFormat(Self);

    FInitialized := True;
  end;
end;
{--------}
function TXpXSLDecimalFormat.Valid(oContext : TXpXSLContext;
                               var ErrMsg : string) : Boolean;
begin
  { NOTE: Property validation occurs in the Preprocess method. }
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if not Result then
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{====================================================================}

{===TXpXSLElement=====================================================}
constructor TXpXSLElement.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
destructor TXpXSLElement.Destroy;
var
  wInx : Longint;
begin
  if FAttrSets <> nil then begin
    for wInx := Pred(FAttrSets.Count) downto 0 do
      TXpDOMStringItem(FAttrSets.Pointers[wInx]).Free;
    FAttrSets.Free;
  end;
  inherited;
end;
{--------}
procedure TXpXSLElement.Execute(oContext : TXpXSLContext);
var
  bHasPrefix : Boolean;
  oElement : TXpElement;
  oMediator : TXpElementMediator;
  oStylesheet : TXpXSLStylesheet;
  sName,
  sNamespace,
  sPrefix,
  sURI : DOMString;
  wInx : Longint;
  wPos : Integer;
begin
  oStylesheet := ParentStylesheet;
  oMediator := oStylesheet.Mediator;

  UpdateNSMgr(oContext);

  { Evaluate the name and namespace attributes. }
  sName := EvaluateAttrib(oContext, FName);
  sNamespace := EvaluateAttrib(oContext, FNamespace);

  { If no namespace specified and sName contains a namespace prefix then
    the namespace must be in scope at this point. }
  wPos := XpPos(':', sName);
  bHasPrefix := (wPos > 0);
  if bHasPrefix and (sNamespace = '') then begin
    sPrefix := XpCopy(sName, 1, wPos - 1);
    sURI := ResolveNSPrefix(sPrefix);
    if sURI = '' then
      raise EXpXSLException.Create
              (Format(sXSLUnresolvedNSPrefix,
                      [QuotedStr(sPrefix),
                       'name attribute having value ' + QuotedStr(sName) +
                       ' for ' + NodeName +  ' element']))
  end;

  { If namespace specified but no prefix then must generate a prefix. }
  if sNamespace <> '' then begin
    { Prefix specified? }
    if bHasPrefix then begin
      { Yes. Grab the prefix for use later on. }
      sPrefix := XpCopy(sName, 1, wPos - 1);
    end
    else begin
      { No prefix. Generate new prefix. }
      sPrefix := oStylesheet.NSMgr.GetUniquePrefix;
      sName := sPrefix + sName;
    end;
  end;

  { Create the element. }
  oElement := OwnerDocument.CreateElement(sName);
  try
    oMediator.StartElement(oElement);

    { Namespace specified? }
    if sNamespace <> '' then
      { Yes. Add a namespace attribute to the element. }
      oMediator.CopyAttribute(XpsXmlns + ':' + sPrefix, sNamespace, Self);

    { Generate attributes from the referenced attribute sets. }
    if FAttrSets <> nil then
      for wInx := 0 to Pred(FAttrSets.Count) do
        oMediator.AttrSetMergeSetsViaName
         (TXpDOMStringItem(FAttrSets.Pointers[wInx]).sString, oContext);

    { Generate the element content by executing the child elements. }
    ExecuteChildren(oContext);

    oMediator.EndElement(oElement);
  finally
    oElement.Release;
  end;
end;
{--------}
procedure TXpXSLElement.Preprocess(oContext : TXpXSLContext);
var
  sUsed : DOMString;
begin
  if not FInitialized then begin
    inherited;
    FNamespace := GetAttribute(XpsNamespace);
    sUsed := GetAttribute(XpsUseAttrSets);
    if sUsed <> '' then
      FAttrSets := ParseUseAttrSet(sUsed);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLElement.Valid(oContext : TXpXSLContext;
           var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Must have a name attribute }
      FName := GetAttribute(XpsName);
      Result := (FName <> '');
      if not Result then
        ErrMsg := MissingAttrib(XpsName);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLFallback====================================================}
constructor TXpXSLFallback.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
end;
{=====================================================================}

{===TXpXSLForEach=====================================================}
constructor TXpXSLForEach.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLForEach.Execute(oContext : TXpXSLContext);
var
  oChildXSLNode : TXpBaseXSLElement;
  oNodes,
  oNodesSorted : TXpNodeList;
  oSavCurNode : TXpNode;
  oTemplate : TXpXSLTemplate;
  wInx : Integer;
begin
  UpdateNSMgr(oContext);
  { Evaluate the select attribute. }
  oNodes := oContext.CurrentNode.SelectNodes(FSelect);

  if oNodes.Length = 0 then                                            {!!.55}
    Exit;                                                              {!!.55}

  { Preprocess child xsl:sort elements. }
  for wInx := 0 to Pred(ChildNodes.Length) do begin
    { Future: If encounter invalid child element then raise an error. }
{Begin !!.55}
    if ChildNodes.Item(wInx) is TXpXSLSort then begin
      oChildXSLNode := TXpBaseXSLElement(ChildNodes.Item(wInx));
      oChildXSLNode.Preprocess(oContext);
    end
    else if not (ChildNodes.Item(wInx).NodeType = COMMENT_NODE) then
      break;
{End !!.55}
  end;

  { Any sort keys? }
  if FKeyDefinitions.Count > 0 then begin
    { Yes. Sort the node set. }
    oNodesSorted := FSorter.Sort(oNodes, FKeyDefinitions);
    oNodes.Free;
    oNodes := oNodesSorted;
  end;

  oContext.CurrentNodeList := oNodes;
  oTemplate := oContext.CurrentTemplate;
  oSavCurNode := oContext.CurrentNode;
  oContext.FUsePrevVars := True;                                       {!!.55}
  try
    oContext.CurrentTemplate := nil;
    for wInx := 0 to Pred(oNodes.Length) do begin
      oContext.MakeForEachVarBinding;
      try
        oContext.CurrentNode := oNodes.Item(wInx);
        ExecuteChildren(oContext);
      finally
        oContext.DropForEachVarBinding;
      end;
    end;
  finally
    oContext.FUsePrevVars := False;                                    {!!.55}
    oContext.CurrentNode := oSavCurNode;
    oContext.RestoreCurNodeList;
    oContext.CurrentTemplate := oTemplate;
    oNodes.Free;
  end;
end;
{--------}
procedure TXpXSLForEach.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Obtain the select attribute. }
    FSelect := GetAttribute(XpsSelect);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLForEach.Valid(oContext : TXpXSLContext;
                         var ErrMsg : string) : Boolean;
var
  oNode : TXpNode;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Select attribute is mandatory. }
      oNode := GetAttributeNode(XpsSelect);
      Result := (oNode <> nil) and (oNode.NodeValue <> '');
      if (not Result) then
        ErrMsg := MissingAttrib(XpsSelect);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLIf==========================================================}
constructor TXpXSLIf.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLIf.Execute(oContext : TXpXSLContext);
var
  oNode : TXpNode;
begin
  UpdateNSMgr(oContext);
  { Does the current node match the test? }
  oNode := oContext.CurrentNode;
  if oNode.SelectBooleanContext(FTest, oContext.CurrentNodeList) then
    ExecuteChildren(oContext);
end;
{--------}
procedure TXpXSLIf.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Obtain test attribute. }
    FTest := GetAttribute(XpsTest);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLIf.Valid(oContext : TXpXSLContext;
                    var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Does it have a test attribute? }
      Result := (GetAttributeNode(XpsTest) <> nil);
      if not Result then
        ErrMsg := MissingAttrib(XpsTest);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLMerger======================================================}
function TXpXSLMerger.GetImporter : Boolean;
begin
  Result := False;
end;
{--------}
procedure TXpXSLMerger.Preprocess(oContext : TXpXSLContext);
var
  oDOM : TXpObjModel;
  oParentStyleSheet,
  oStyleSheet : TXpXSLStylesheet;
  sStylesheet : DOMString;
begin
  if not FInitialized then begin
    inherited;
    oParentStylesheet := ParentStylesheet;
    { An xsl:import or xsl:include element loads the referenced stylesheet into
      an instance of TXpObjModel. It then clones the document element of the
      merged stylesheet which should be an xsl:stylesheet element. The cloned
      tree fragment is then made a child of the xsl:import|xsl:include element.

      In the CUESoft version, they replaced the element with the merged
      stylesheet. I don't see a reason to do that since we will never traverse
      the stylesheet tree from top to bottom. Instead, the XSL processor
      traverses the XML document, searching for templates that match
      the nodes. The XSL processor then traverses the matching template. }
    oDOM := TXpObjModel.Create(nil);
    oDOM.NormalizeData := False;
    try
      { Determine the path for the merged stylesheet from the href
        attribute. }
      sStylesheet := XpMakeAbsolutePath(FHref, oParentStylesheet.Mediator.GetStyleURL);
      if (Assigned(oParentStylesheet.Mediator.OnImport)) then                          {!!.57}
        oParentStylesheet.Mediator.FOnImport(oParentStylesheet.Mediator.FXSLProcessor, {!!.57}
                                             sStylesheet);                             {!!.57}
      if oDOM.LoadDataSource(sStylesheet) then begin
        if oDOM.Document.DocumentElement is TXpXSLStylesheet then begin
          oDOM.Document.Normalize(False);
          oDOM.Document.DocumentElement.StripWhitespaceNodes(True);
          oStylesheet := TXpXSLStylesheet(oDOM.Document.DocumentElement.CloneNode(True));
          ForceOwnerDocument(oStyleSheet);
          AppendChild(oStyleSheet);
          oStylesheet.Mediator := oParentStylesheet.Mediator;
          oStylesheet.NSMgr.Using := oParentStylesheet.NSMgr.Using;
          { Set the stylesheet's import precedence. Our strategy is to pass
            our current import precedence down to the merged stylesheet.
            If the stylesheet is included then we leave our parent stylesheet's
            import precedence as is. If imported then we increment our parent
            stylesheet's import precedence. We wind up with the nodes numbered
            for a post order traversal. }
          oStylesheet.ImportPrecedence := oParentStylesheet.ImportPrecedence;
          { Now preprocess the merged stylesheet. Note that preprocessing
            the stylesheet will force it to resolve its xsl:import and xsl:include
            elements. This will in turn update the ImportPrecedence of the
            merged stylesheet. }
          oStylesheet.Preprocess(oContext);
          if Importer then
            oParentStylesheet.ImportPrecedence := oStylesheet.ImportPrecedence + 1
          else
            oParentStylesheet.ImportPrecedence := oStylesheet.ImportPrecedence;
          oStylesheet.Release;
        end
        else
          raise EXpXSLException.Create(Format(sXSLNotStylesheet,[sStylesheet]));
      end;
    finally
      oDOM.Free;
    end;

    FInitialized := True;
  end;
end;
{--------}
function TXpXSLMerger.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Is it empty? }
      Result := IsEmpty;
      if Result then begin
        { Does it have an href attribute? }
        FHref := GetAttribute(XpsHref);
        Result := (FHref <> '');
        if not Result then
          ErrMsg := MissingAttrib(XpsHref);
      end
      else
        ErrMsg := Format(sXSLMustBeEmpty, [QuotedStr(NodeName)]);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLImport======================================================}
function TXpXSLImport.GetImporter : Boolean;
begin
  Result := True;
end;
{--------}
function TXpXSLImport.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
var
  oNode : TXpNode;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { This element must be the first sibling of the xsl:stylesheet element
      or be preceded only other xsl:import elements. }
    oNode := PreviousSibling;
    Result := (oNode = nil) or (oNode is TXpXSLImport);
    if not Result then begin
      { Scan back until we encounter the beginning of the sibling list or
        something that is not a sibling. }
      while (oNode <> nil) and (oNode is TXpComment) do
        oNode := oNode.PreviousSibling;
      Result := (oNode = nil);
      if not Result then
        ErrMsg := sXSLImportPosition;
    end;
  end;
end;
{=====================================================================}

{===TXpXSLKey=========================================================}
procedure TXpXSLKey.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    ParentStylesheet.Mediator.AddKeyDef(Self);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLKey.Valid(oContext : TXpXSLContext;
                     var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Validate attributes. }
      FName := ExpandName(GetAttribute(XpsName));
      Result := (FName <> '');
      if Result then begin
        FMatch := GetAttribute(XpsMatch);
        Result := (FMatch <> '');
        if Result then begin
          FUse := GetAttribute(XpsUse);
          Result := (FUse <> '');
          if not Result then
            ErrMsg := MissingAttrib(XpsUse);
        end
        else
          ErrMsg := MissingAttrib(XpsMatch);
      end
      else
        ErrMsg := MissingAttrib(XpsName);
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLMessage=====================================================}
procedure TXpXSLMessage.Execute(oContext : TXpXSLContext);
var
  bTerminate : Boolean;
  oMediator : TXpElementMediator;
  oXMLFilter : TXpFilterXML;
  sMsg : DOMString;
begin
  { Build the message by executing the children. }
  oMediator := ParentStylesheet.Mediator;
  oXMLFilter := TXpFilterXML.Create(nil);
  try
    oMediator.PushFilter(oXMLFilter);
    ExecuteChildren(oContext);
    sMsg := oXMLFilter.StringValue;
  finally
    oMediator.PopFilter;
    oXMLFilter.Free;
  end;
  bTerminate := (FTerminate = XpsYes);
  { Call the OnXSLMessage event handler in the XSL processor. }
  oMediator.RaiseXSLMessage(Self, sMsg, bTerminate);
  if bTerminate then
    raise EXpXSLException.Create(sXSLTerminatedByMsg);
end;
{--------}
procedure TXpXSLMessage.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLMessage.Valid(oContext : TXpXSLContext;
                         var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Does it have a valid terminate attribute? }
      FTerminate := GetAttribute(XpsTerminate);
      Result := (FTerminate = XpsYes) or
                (FTerminate = XpsNo) or
                (FTerminate = '');
      if not Result then
        ErrMsg := Format(sXSLMsgTerminate, [QuotedStr(FTerminate)]);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLNamespaceAlias==============================================}
procedure TXpXSLNamespaceAlias.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    ParentStylesheet.Mediator.AddNSAlias(FStylePrefix, FResultPrefix,
                                         ParentStylesheet.ImportPrecedence);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLNamespaceAlias.Valid(oContext : TXpXSLContext;
                                var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Validate attributes. }
      FStylePrefix := GetAttribute(XpsStylePrefix);
      Result := (FStylePrefix <> '');
      if Result then begin
        FResultPrefix := GetAttribute(XpsResultPrefix);
        Result := (FResultPrefix <> '');
        if not Result then
          ErrMsg := MissingAttrib(XpsResultPrefix);
      end
      else
        ErrMsg := MissingAttrib(XpsStylePrefix);
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpNumberToken====================================================}
constructor TXpNumberToken.CreateToken(oNumType : TXpNumberFormatType;
                                       const sValue   : DOMString);    {!!.57}
begin
  inherited Create;
  FNumType := oNumType;
  noNodeValue := sValue;
end;
{=====================================================================}

{===TXpXSLNumber======================================================}
destructor TXpXSLNumber.Destroy;
begin
  FFromPattern.Free;
  FTokens.Free;
  inherited;
end;
{--------}
function TXpXSLNumber.AsRoman(const wNum : Integer) : DOMString;
const
  iNumVals = 13;
  NumberTests : array[1..iNumVals] of Integer =
    (1000, 900,  500,  400, 100,   90,  50,   40,  10,    9,   5,    4, 1);
  Numerals : array[1..iNumVals] of DOMString =
    ('m', 'cm', 'd', 'cd', 'c', 'xc', 'l', 'xl', 'x', 'ix', 'v', 'iv', 'i');
var
  i : 1..iNumVals;
  wCnt : Integer;
begin
  Result := '';
  i := 1;
  wCnt := wNum;
  while wCnt > 0 do begin
    while NumberTests[i] > wCnt do
      inc(i);
    dec(wCnt, NumberTests[i]);
    Result := Result + Numerals[i];
  end;
end;
{--------}
procedure TXpXSLNumber.Execute(oContext : TXpXSLContext);
var
  aValues : TXpIntegerArray;
  sTmpGrpSize : DOMString;
  wCnt : Integer;
begin
  UpdateNSMgr(oContext);
  { Evaluate those attributes that may be attribute value templates. }
  if FTokens = nil then begin
    FFormat := EvaluateAttrib(oContext, GetAttribute(XpsFormat));
    if FFormat = '' then
      FFormat := XpsDefaultNumberFormat;
    FTokens := ParseFormat(FFormat);
    FLang := EvaluateAttrib(oContext, GetAttribute(XpsLang));
    FGroupSep := EvaluateAttrib(oContext, GetAttribute(XpsGroupSep));
      { TODO:: What if FGroupSep returns more than 1 character? }
    sTmpGrpSize := EvaluateAttrib(oContext, GetAttribute(XpsGroupSize));
    if sTmpGrpSize <> '' then
      { TODO:: Handle case where this is not a valid integer. }
      FGroupSize := StrToInt(sTmpGrpSize)
    else
      FGroupSize := -1;
    FGrouping := (FGroupSep <> '') and (FGroupSize > 0);
  end;

  { Was a value specified? }
  if FValue <> '' then begin
    { Yes. Evaluate the expression. }
    aValues[0] := Round(oContext.CurrentNode.SelectNumberContext
                          (FValue, oContext.CurrentNodeList));
    ParentStylesheet.Mediator.CopyText(FormatNumber(aValues, 1));
  end
  else begin
    { No. Generate a sequence number. }
    if FLevel = XpsSingle then
      HandleSingle(oContext, @aValues, 0, wCnt)
    else if FLevel = XpsAny then
      HandleAny(oContext, @aValues, wCnt)
    else if FLevel = XpsMultiple then
      HandleMultiple(oContext, @aValues, wCnt);
    ParentStylesheet.Mediator.CopyText(FormatNumber(aValues, wCnt));
  end;
end;
{--------}
function TXpXSLNumber.FormatNumber(const aValues : TXpIntegerArray;
                                   const wCount : Integer) : DOMString;
var
  bLastWasSep, bUsingLast : Boolean;
  wNumTokens : Integer;
  wNumInx : Integer;
  wPos, wValue : Integer;
  oToken : TXpNumberToken;
  sInsert     : DOMString;
begin
  Result := '';
  bLastWasSep := False;
    { As per spec, the second and subsequent formatted numbers must be preceded
      with a separator token. If no preceding separator token then must be
      preceded by a '.' (i.e., period character). }
  bUsingLast := False;
  wPos := 0;
  wNumInx := 0;

  wNumTokens := FTokens.Length;
  if wNumTokens = 0 then
    Exit;

  { Were we passed an empty array? }
  if wCount = 0 then begin
    { Yes. The result should contain the leading and trailing punctuation
      tokens. }
    oToken := TXpNumberToken(FTokens.Item(0));
    if oToken.NumberType = xpftSeparator then
      Result := Result + oToken.NodeValue;
    if FHasLastSepToken then
      Result := Result + FLastSepToken;
  end
  else begin
    { No. Loop through the integers. }
    while wNumInx < wCount do begin
      { Have we used up all the formatting tokens? }
      if wPos = wNumTokens then begin
        { Yes. Use the last formatting token to format the current number. }
        bUsingLast := True;
        oToken := FLastFmtToken;
      end
      else begin
        { No. Obtain the next token. }
        oToken := TXpNumberToken(FTokens.Item(wPos));
        inc(wPos);
      end;

      { Is this a separator token? }
      if oToken.NumberType = xpftSeparator then begin
        { Yes. Add it to the result string. }
        Result := Result + oToken.NodeValue;
        bLastWasSep := True;
      end
      else begin
        { No. It is a formatting token. Format the current number. }
        case oToken.NumberType of
          xpftNumeric :
            begin
              sInsert := Format('%.' + IntToStr(Length(oToken.NodeValue)) +
                                'd',
                                [aValues[wNumInx]]);
              if FGrouping and (Length(sInsert) > FGroupSize) then
                sInsert := GroupitizeNumber(sInsert);
            end;
          xpftAlphaLower,
          xpftAlphaUpper :
            begin
              wValue := aValues[wNumInx];
              while wValue <> 0 do begin
                sInsert := Chr(97 + ((wValue - 1) mod 26)) + sInsert;
                wValue := wValue div 26;
              end;
              if oToken.NumberType = xpftAlphaUpper then
                sInsert := UpperCase(sInsert);
            end;
          xpftRomanLower,
          xpftRomanUpper :
            begin
              sInsert := AsRoman(aValues[wNumInx]);
              if oToken.NumberType = xpftRomanUpper then
                sInsert := UpperCase(sInsert);
            end;
        end;  { case }
        { Preceding separator token? }
        if (wNumInx > 0) and (not bLastWasSep) then
          { No. Prefix as necessary. }
          if FHasLastSepToken and bUsingLast then
            Result := Result + FLastSepToken
          else
            Result := Result + csDecSep;
        bLastWasSep := False;
        Result := Result + sInsert;
        inc(wNumInx);
      end;
    end;  { for }
    if FHasLastSepToken and (not bLastWasSep) then
      Result := Result + FLastSepToken;
  end;  { if }
end;
{--------}
function TXpXSLNumber.GetCountPattern(oContext : TXpXSLContext) : TXpBasePattern;
var
  oNode : TXpNode;
  oPattContext : TXpPatternContext;
begin
  oPattContext := TXpPatternContext.Create;
  try
    oPattContext.CurrentSLNode := Self;
    if FCount = '' then begin
      Result := TXpNameTest.CreateEx(oPattContext);
      with TXpNameTest(Result) do begin
        oNode := oContext.CurrentNode;
        Name := oNode.NodeName;
        NodeType := TXpNodeClasses(oNode.ClassType);
      end;
    end
    else
      Result := ParentStylesheet.Mediator.PatternMaker.GeneratePattern(Self, FCount);
  finally
    oPattContext.Free;
  end;
end;
{--------}
function TXpXSLNumber.GroupitizeNumber(const sNumber : DOMString) : DOMString;
var
  wCnt, wLen, wParmInx, wPos : Integer;
begin
  wLen := Length(sNumber);
  wPos := wLen + (wLen div FGroupSize);
  if wLen mod FGroupSize = 0 then
    dec(wPos);
  wCnt := 0;
  SetLength(Result, wPos);
  for wParmInx := wLen downto 1 do begin
    Result[wPos] := sNumber[wParmInx];
    inc(wCnt);
    { Time to insert a separator? }
    if (wParmInx > 1) and (wCnt mod FGroupSize = 0) then begin
      dec(wPos);
      Result[wPos] := FGroupSep[1];
    end;
    dec(wPos);
  end;
end;
{--------}
procedure TXpXSLNumber.HandleAny(oContext : TXpXSLContext;
                                 pValues : PXpIntegerArray;
                             var wCnt : Integer);
var
  bStop : Boolean;
  oCountPatt : TXpBasePattern;
  oNode : TxpNode;
begin
  { Walk backwards through the document in reverse document order, searching
    for nodes that match the count attribute. Stop when encounter a node
    that matches the from attribute. The number returned is the number
    of nodes encountered up to but not including the from attribute. }
  bStop := False;
  pValues^[0] := 0;
  wCnt := 1;
  oNode := oContext.CurrentNode;
  { Get a pattern for the Count attribute. }
  oCountPatt := GetCountPattern(oContext);
  try
    { TODO: Verify default values for count and from }
    while (oNode <> nil) and (not bStop) do begin
      bStop := FFromPattern.Matches(oNode, nil);
      if not bStop then begin
        if oCountPatt.Matches(oNode, nil) then
          inc(pValues^[0]);
        oNode := oNode.PreviousInDocument;
      end;
    end;
  finally
    oCountPatt.Free;
  end;
end;
{--------}
procedure TXpXSLNumber.HandleMultiple(oContext : TXpXSLContext;
                                      PValues : PXpIntegerArray;
                                  var wCnt : Integer);
var
  aTmpValues : TXpIntegerArray;
  oCountPatt : TXpBasePattern;
  oNode : TXpNode;
  oTmpContext : TXpXSLContext;
  wInx, wInx2 : Integer;
begin
  oCountPatt := GetCountPattern(oContext);
  oNode := oContext.CurrentNode;
  oTmpContext := TXpXSLContext.Create;
  wCnt := 0;
  wInx := 0;
  try
    oTmpContext.GlobalVars := oContext.GlobalVars;
    { Need to start at the current node and work backwards through the
      ancestors. }
    while (oNode <> nil) do begin
      { Does the current node match the count attribute? }
      if oCountPatt.Matches(oNode, nil) then begin
        { Yes. Determine this node's sequence number. }
        oTmpContext.CurrentNode := oNode;
        HandleSingle(oTmpContext, @aTmpValues, wInx, wCnt);
        inc(wInx);
      end;
      oNode := oNode.ParentNode;
      { Does the ancestor node match the From pattern? }
      if FFromPattern.Matches(oNode, nil) then
        { Yes. Time to stop. }
        break;
    end;  { while }

    { Did we generate any numbers? }
    if (wCnt > 0) then begin
      { Yes. They are in reverse order. Take care of it. }
      wInx := 0;
      for wInx2 := Pred(wCnt) downto 0 do begin
        PValues^[wInx] := aTmpValues[wInx2];
        inc(wInx);
      end;
    end;
  finally
    oTmpContext.Free;
    oCountPatt.Free;
  end;
end;
{--------}
procedure TXpXSLNumber.HandleSingle(oContext : TXpXSLContext;
                                    PValues : PXpIntegerArray;
                              const wInx : Integer;
                                var wCnt : Integer);
var
  oCountPatt : TXpBasePattern;
  oNode : TXpNode;
begin
  oCountPatt := GetCountPattern(oContext);
  try
    oNode := oContext.CurrentNode;

    { Need to find a target node. The target node is the current node if it
      matches the count pattern or the first ancestor that matches the count
      pattern. Determine if the current node or one of its ancestors matches the
      count pattern. }
    while (oNode <> nil) and (not oCountPatt.Matches(oNode, nil)) do begin
      oNode := oNode.ParentNode;
      { Have we encountered an ancestor matching the From pattern? }
      if FFromPattern.Matches(oNode, nil) then
        { Yes. Exit. }
        oNode := nil;
    end;

    { Did we find a target node? }
    if oNode <> nil then begin
      { Yes. Count the previous siblings that match the Count pattern. }
      PValues^[wInx] := 1;
      inc(wCnt);
      while oNode <> nil do begin
        oNode := oNode.PreviousSibling;
        if oCountPatt.Matches(oNode, nil) then
          inc(PValues^[wInx]);
      end;
    end;
  finally
    oCountPatt.Free;
  end;
end;
{--------}
function TXpXSLNumber.IsFormatToken(const sChar : WideChar) : Boolean;
begin
  Result := ((sChar = '0') or
             (sChar = '1') or
             (sChar = 'a') or
             (sChar = 'A') or
             (sChar = 'i') or
             (sChar = 'I'));
end;
{--------}
function TXpXSLNumber.ParseFormat(const sFormat : DOMString) : TXpNodeList;
var
  oFormatType : TXpNumberFormatType;
  oList     : TXpNodeList;
  oToken    : TXpNumberToken;
  sTmp      : DOMString;
  i,
  wLen,
  wPos   : Integer;
begin
  FHasLastSepToken := False;
  FLastSepToken := '';
  FLastFmtToken := nil;
  oList := TXpNodeList.Create;
  try
    wLen := Length(sFormat);
    i := 1;
    while i <= wLen do begin
      sTmp := sFormat[i];
      case sFormat[i] of
        '0' :
          begin
            oFormatType := xpftNumeric;
            wPos := 1;
            SetLength(sTmp, wLen);
            while (i+1 <= wLen) and
                  ((sFormat[i+1] = '0') or
                   (sFormat[i+1] = '1')) do begin
              inc(i);
              inc(wPos);
              sTmp[wPos] := sFormat[i];
            end;
            SetLength(sTmp, wPos);
          end;
        '1' : oFormatType := xpftNumeric;
        'a' : oFormatType := xpftAlphaLower;
        'A' : oFormatType := xpftAlphaUpper;
        'i' : oFormatType := xpftRomanLower;
        'I' : oFormatType := xpftRomanUpper;
        else begin
          oFormatType := xpftSeparator;
          wPos := 1;
          SetLength(sTmp, wLen);
          while (i+1 <= wLen) and (not IsFormatToken(sFormat[i+1])) do begin
            inc(i);
            inc(wPos);
            sTmp[wPos] := sFormat[i];
          end;
          SetLength(sTmp, wPos);
          FLastSepToken := sTmp;
        end;
      end;  { case }
      oToken := TXpNumberToken.CreateToken(oFormatType, sTmp);
      if oFormatType <> xpftSeparator then
        FLastFmtToken := oToken;
      oList.Add(oToken);
      inc(i);
      { Is this the last token and is it a separator token? }
      if (i > wLen) and (oFormatType = xpftSeparator) then
        { Yes. Set a flag for use by FormatNumber method. }
        FHasLastSepToken := True;
    end;  { while }
  except
    oList.Free;
    raise;
  end;
  Result := oList;

  { Decrease ref count }
  for i := 0 to Pred(oList.Length) do
    oList.Item(i).Release;
end;
{--------}
procedure TXpXSLNumber.Preprocess(oContext : TXpXSLContext);
var
  oPattContext : TXpPatternContext;
begin
  if not FInitialized then begin
    inherited;
    { Grab the attributes. }
    FLevel := GetAttribute(XpsLevel);
    FCount := GetAttribute(XpsCount);
    FFrom := GetAttribute(XpsFrom);
    FValue := GetAttribute(XpsValue);
    { Note: format, lang, grouping-separator, and grouping-size are evaluated
      in the Execute method as attribute value templates. }

    if FLevel = '' then
      FLevel := XpsSingle;

    { Set up a context for parsing the from attribute. }
    oPattContext := TXpPatternContext.Create;
    try
      oPattContext.CurrentSLNode := Self;
      if FFrom = '' then
        FFromPattern := TXpFailTest.CreateEx(oPattContext)
      else
        FFromPattern :=
          ParentStylesheet.Mediator.PatternMaker.GeneratePattern(Self, FFrom);
    finally
      oPattContext.Free;
    end;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLNumber.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      Result := IsEmpty;
      if not Result then
        ErrMsg := Format(sXSLMustBeEmpty, [QuotedStr(NodeName)]);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLOtherwise===================================================}
constructor TXpXSLOtherwise.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
procedure TXpXSLOtherwise.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLOtherwise.Valid(oContext : TXpXSLContext;
                           var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it a child of an xsl:choose element? }
    Result := (ParentNode is TXpXSLChoose);
    if not Result then
      ErrMsg := Format(sXSLNotInChoose, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLOutput======================================================}
procedure TXpXSLOutput.Preprocess(oContext : TXpXSLContext);
var
  bHandle : Boolean;
  oMediator : TXpElementMediator;
  sAttrName,
  sAttrValue : DOMString;
  wInx : Integer;
begin
  if not FInitialized then begin
    inherited;
    { Read & evaluate attributes. }
    if HasAttributes then begin
      oMediator := ParentStylesheet.Mediator;
      for wInx := 0 to Pred(Attributes.Length) do begin
        sAttrName := Attributes.Item(wInx).NodeName;
        bHandle := (sAttrName = XpsCDataSectElements) or
                   (sAttrName = XpsDocTypePub) or
                   (sAttrName = XpsDocTypeSys) or
                   (sAttrName = XpsEncoding) or
                   (sAttrName = XpsIndent) or
                   (sAttrName = XpsMediaType) or
                   (sAttrName = XpsMethod) or
                   (sAttrName = XpsOmitXMLDecl) or
                   (sAttrName = XpsStandalone) or
                   (sAttrName = XpsVersion);
        if bHandle then begin
          sAttrValue := Attributes.Item(wInx).NodeValue;
          if sAttrValue <> '' then begin
            sAttrValue := EvaluateAttrib(oContext, sAttrValue);
            oMediator.AddOutputAttribute(sAttrName, sAttrValue, Self);
          end;
        end;  { if }
      end;  { for }
    end;  { if}

    FInitialized := True;
  end;  { if }
end;
{--------}
function TXpXSLOutput.Valid(oContext : TXpXSLContext;
                        var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Yes. May the filter may be overridden? }
      if not ParentStylesheet.Mediator.FilterCanBeOverridden then
        { No. Avoid initialization of attribute values (i.e.,
          avoid unnecessary work). }
        FInitialized := True;
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLBaseVar=====================================================}
constructor TXpXSLBaseVar.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
type
  TXpElementCracker = class(TXpElement);
    { Used to gain access to protected var noBaseURI. }

function TXpXSLBaseVar.Evaluate(oContext : TXpXSLContext) : TXpValue;
var
  oList : TXpNodeList;
  oMediator : TXpElementMediator;
  oParentStylesheet : TXpXSLStylesheet;
  oXMLFilter : TXpFilterXML;
begin
  oContext.CurrentStylesheetNode := Self;
  try
    { Was a select attribute specified? }
    if FSelect = '' then begin
      { No. The variable will be a result tree fragment. Redirect output to a
        temporary XML filter & process the template body. }
      Result := TXpValue.Create;
      oParentStylesheet := ParentStylesheet;
      oMediator := oParentStylesheet.Mediator;
      oXMLFilter := TXpFilterXML.Create(nil);
      oList := TXpNodeList.Create;
      try
//        try                                                          {Deleted !!.54}
          oMediator.PushFilter(oXMLFilter);
          ExecuteChildren(oContext);
          if oXMLFilter.ResultTreeRoot <> nil then begin
            oList.Add(oXMLFilter.ResultTreeRoot);
            { Set the base URI of this node to that of the stylesheet. This
              is necessary in case the variable is used as a parameter to
              the document() function. }
            TXpElementCracker(oXMLFilter.ResultTreeRoot).noBaseURI :=
              oParentStylesheet.BaseURI;
//            oXMLFilter.ResultTreeRoot.Release;                       {Deleted !!.54}
          end;
          Result.AsNodeSet := oList;
{Begin !!.54}
//        except
//          oList.Free;
//          raise;
//        end;
      finally
        oList.Free;
{End !!.54}
        oMediator.PopFilter;
        oXMLFilter.Free;
      end;
    end
    else begin
      { Yes. Select the value. }
      if oContext.CurrentNode is TXpDocument then
        Result := TXpDocument(oContext.CurrentNode).DocumentElement.Select
                    (FSelect, oContext.CurrentNodeList)
      else
        Result := oContext.CurrentNode.Select
                    (FSelect, oContext.CurrentNodeList);
    end;
  finally
    oContext.PopCurSLNode;
  end;
end;
{--------}
function TXpXSLBaseVar.IsGlobal : Boolean;
begin
  Result := (ParentNode is TXpXSLStylesheet);
end;
{--------}
procedure TXpXSLBaseVar.Reset;
begin
  inherited;
  { Reset the parent stylesheet so that this element's preprocess method
    is called. }
  ParentStylesheet.Reset;
end;
{--------}
function TXpXSLBaseVar.Valid(oContext : TXpXSLContext;
                            var ErrMsg : string) : Boolean;
var
  bHasChildren : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Verify it has the mandatory name attribute. }
    FName := GetAttribute(XpsName);
    if FName = '' then begin
      Result := False;
      ErrMsg := sXSLVarNameRequired;
    end
    else begin
      { Verify it has a select attribute or a template body but not both. }
      FSelect := GetAttribute(XpsSelect);
      bHasChildren := (ChildNodes.Length > 0);
      if (FSelect <> '') and bHasChildren then begin
        Result := False;
        ErrMsg := Format(sXSLVarNotBoth,
                         [NodeName, QuotedStr(GetAttribute(XpsName))]);
      end;  { if }
    end;  { if }
  end;  { if }
end;
{=====================================================================}

{===TXpXSLParam=======================================================}
procedure TXpXSLParam.Execute(oContext : TXpXSLContext);
var
  oValue : TxpValue;
begin
  if (not IsGlobal) then begin
    oValue := oContext.GetVar(FName);
    { Did calling routine pass in a parameter value? }
    if oValue = nil then
      { No. Derive the default value. }
      oContext.SetVar(FName, Evaluate(oContext));
  end;
end;
{--------}
procedure TXpXSLParam.Preprocess(oContext : TXpXSLContext);
var
  oValue : TXpValue;
begin
  if not FInitialized then begin
    inherited;
    { Expand the namespace prefix (if present) of the name attribute. }
    FName := ExpandName(FName);

    { If global then evaluate and send value to the processor.
      Local variables are evaluated in the Execute method. }
    if IsGlobal then begin
      oValue := Evaluate(oContext);
      ParentStylesheet.Mediator.SetGlobalParm(FName, oValue);
      oValue.Free;
    end;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLParam.Valid(oContext : TXpXSLContext;
                       var ErrMsg : string) : Boolean;
var
  oPrevSibling : TXpNode;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { A parameter is valid if:
      1. It passes the validity checks for TXpXSLBaseVar
      2. It is a top-level element -OR-
      3. It is the first child of an element or its previous sibling is
         also an xsl:param element.
    }
    oPrevSibling := PreviousSibling;
    { Verify position first. }
    Result := (ParentNode is TXpXSLStylesheet) or
              ((ParentNode is TXpXSLTemplate) and
               ((oPrevSibling = nil) or
                (oPrevSibling is TXpXSLParam) or
                (oPrevSibling is TXpComment) or
                ((oPrevSibling is TXpText) and
                 (TXpText(oPrevSibling).IsWhitespaceNode))
                ));
    if not Result then
      ErrMsg := Format(sXSLParamPosition,
                       [QuotedStr(GetAttribute(XpsName))]);
  end;
end;
{=====================================================================}

{===TXpXSLWhitespaceControl==========================================}
function TXpXSLWhitespaceControl.GetElementNames : DOMString;
begin
  Result := GetAttribute(xpsElements);
end;
{--------}
procedure TXpXSLWhitespaceControl.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Register with strip manager }
    ParentStylesheet.Mediator.AddStrip(oContext, Self);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLWhitespaceControl.ShouldStrip : Boolean;
begin
  Result := False;
end;
{--------}
function TXpXSLWhitespaceControl.Valid(oContext : TXpXSLContext;
                                   var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { elements attribute is mandatory }
      Result := (GetElementNames <> '');
      if not Result then
        ErrMsg := MissingAttrib(XpsElements);
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{====================================================================}

{===TXpXSLProcessingInstruction=======================================}
procedure TXpXSLProcessingInstruction.Execute(oContext : TXpXSLContext);
var
  bIgnore,
  bRaiseError : Boolean;
  oList : TXpNodeList;
  oMediator : TXpElementMediator;
  oXMLFilter : TXpFilterXML;
  sValue : DOMString;
  wInx : Integer;
begin
  sValue := '';
  bIgnore := False;
  { Build the message by executing the children. }
  oList := TXpNodeList.Create;
  try
    oMediator := ParentStylesheet.Mediator;
    oXMLFilter := TXpFilterXML.Create(nil);
    try
      oMediator.PushFilter(oXMLFilter);
      ExecuteChildren(oContext);
      if oXMLFilter.ResultTreeRoot <> nil then
        oList.CopyList(oXMLFilter.ResultTreeRoot.ChildNodes);
      { Verify that all child nodes are text nodes. }
      for wInx := 0 to Pred(oList.Length) do
        if not (oList.Item(wInx) is TXpText) then begin
          { Ask the XSL processor how the situation should be resolved. }
          bRaiseError := True;
          oMediator.ResolveConflict(Self, xpctPINodes, bRaiseError);
          if bRaiseError then
            raise EXpXSLException.Create(Format(sXSLOnlyTextNodes,
                                                [NodeName]))
          else
            bIgnore := True;
          break;
        end;
      if (not bIgnore) and (oXMLFilter.ResultTreeRoot <> nil) then
        sValue := oXMLFilter.ResultTreeRoot.StringValue;

    finally
      oMediator.PopFilter;
      oXMLFilter.Free;
    end;
  finally
    oList.Free;
  end;
  { Call the OnPI event handler in the XSL processor. }
  if not bIgnore then
    oMediator.AddPI(FName, sValue);
end;
{--------}
procedure TXpXSLProcessingInstruction.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLProcessingInstruction.Valid(oContext : TXpXSLContext;
                                       var ErrMsg : string) : Boolean;
var
  wInx : Integer;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Yes. Does it have a valid name? }
      FName := EvaluateAttrib(oContext, GetAttribute(XpsName));
      { Name requirements:
        1. Must be a valid name.
        2. May not contain a colon.
        3. May not be any form of 'xml'.
      }
      if LowerCase(FName) = XpsXML then begin
        Result := False;
        ErrMsg := sXSLPINameIsXML;
      end
      else begin
        for wInx := 1 to Length(FName) do begin
          if XpValidNameChar((wInx = 1), FName[wInx]) then begin
            if FName[wInx] = ':' then begin
              Result := False;
              ErrMsg := Format(sXSLPINameColon, [QuotedStr(FName)]);
              break;
            end;
          end
          else begin
            Result := False;
            ErrMsg := Format(sXSLPINameInvalid,
                             [QuotedStr(FName), QuotedStr(FName[wInx])]);
            break;
          end;
        end;  { for }
      end;  { else }
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;  { if }
end;
{=====================================================================}

{===TXpXSLSort========================================================}
procedure TXpXSLSort.Execute(oContext : TXpXSLContext);
begin
  { Do nothing. }
end;
{--------}
procedure TXpXSLSort.Preprocess(oContext : TXpXSLContext);
var
  oKeyDef : TXpSortKeyDefinition;
  sAttr : DOMString;
begin
  if not FInitialized then begin
    inherited;
    { Create a key definition. It will be attached to the parent node. }
    oKeyDef := TXpSortKeyDefinition.Create;
    oKeyDef.Ascending :=
      not (EvaluateAttrib(oContext, GetAttribute(XpsOrder)) = XpsDescending);

    sAttr := EvaluateAttrib(oContext, GetAttribute(XpsDataType));
    if sAttr = XpsNumber then
      oKeyDef.DataType := xpsdtNumber
    else
      oKeyDef.DataType := xpsdtText;

    oKeyDef.Lang := EvaluateAttrib(oContext, GetAttribute(XpsLang));

    oKeyDef.Select := EvaluateAttrib(oContext, GetAttribute(XpsSelect));
    if oKeyDef.Select = '' then
      oKeyDef.Select := '.';

    oKeyDef.UpperFirst := (GetAttribute(XpsCaseOrder) = XpsUpperFirst);
    oKeyDef.SetComparator;

    { Attach the key definition to the parent node. The parent will use the
      key definitions when the parent is executed. }
    TXpSortableXSLElement(ParentNode).FKeyDefinitions.Add(oKeyDef);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLSort.Valid(oContext : TXpXSLContext;
                      var ErrMsg : string) : Boolean;
var
  oParent : TXpNode;
  sAttr : DOMString;
begin
  { Requirement: Must have xsl:apply-templates or xsl:for-each as parent
    node. }
  oParent := ParentNode;
  Result := (oParent is TXpXSLApplyTemplates) or
            (oParent is TXpXSLForEach);
  if Result then begin
    { Requirement: When used in an xsl:for-each element, must occur before
      all other child elements of the template body. }
    if (oParent is TXpXSLForEach) then begin
      Result := (PreviousSibling = nil) or
                (PreviousSibling is TXpXSLSort);
      if not Result then begin
        ErrMsg := sXSLSortPrecedingSibling;
        Exit;
      end;
    end;

    { Validate attribute values. }
    { Order }
    sAttr := GetAttribute(XpsOrder);
    Result := (sAttr = XpsAscending) or
              (sAttr = '') or
              (sAttr = XpsDescending);
    if not Result then begin
      ErrMsg := Format(sXSLSortAttribInvalid,
                       [XpsOrder, QuotedStr(sAttr),
                        QuotedStr(XpsAscending), QuotedStr(XpsDescending)]);
      Exit;
    end;

    { Case order }
    sAttr := GetAttribute(XpsCaseOrder);
    Result := (sAttr = XpsUpperFirst) or
              (sAttr = '') or
              (sAttr = XpsLowerFirst);
    if not Result then begin
      ErrMsg := Format(sXSLSortAttribInvalid,
                       [XpsCaseOrder,  QuotedStr(sAttr),
                        QuotedStr(XpsUpperFirst), QuotedStr(XpsLowerFirst)]);
      Exit;
    end;

    { Data type }
    sAttr := GetAttribute(XpsDataType);
    Result := (sAttr = XpsNumber) or
              (sAttr = '') or
              (sAttr = XpsText);
    if not Result then begin
      ErrMsg := Format(sXSLSortAttribInvalid,
                       [XpsDataType,  QuotedStr(sAttr),
                        QuotedStr(XpsNumber), QuotedStr(XpsText)]);
      Exit;
    end;

    Result := inherited Valid(oContext, ErrMsg);
  end
  else
    ErrMsg := sXSLSortParent;
end;
{=====================================================================}

{===TXpXSLStripSpace==================================================}
function TXpXSLStripSpace.ShouldStrip : Boolean;
begin
  Result := True;
end;
{=====================================================================}

{===TXpXSLStylesheet==================================================}
constructor TXpXSLStylesheet.Create;
begin
  inherited;
  FImpPrecedence := XpiDefaultImportPrecedence;
  FNSMgr := TXpNamespaceManager.Create;
  FNSMgr.Stylesheet := Self;
end;
{--------}
destructor TXpXSLStylesheet.Destroy;
begin
  FNSMgr.Free;
  inherited;
end;
{--------}
procedure TXpXSLStylesheet.EndNSLevel;
begin
  if FNSMgr.Using then
    FNSMgr.EndLevel;
end;
{--------}
function TXpXSLStylesheet.Ignore(oNode : TXpNode) : Boolean;
begin
  { Need to ignore the xsl prolog & embedded stylesheet commands. }
  Result := (oNode.NodeName = XpsXML) or
            (oNode.NodeName = XpsXSLStylesheet);
end;
{--------}
procedure TXpXSLStylesheet.Preprocess(oContext : TXpXSLContext);
var
  oTopLvlElement : TXpNode;
  sExcludes : DOMString;
  sAttr : DOMString;
  wInx : Integer;
begin
  if not FInitialized then begin
    inherited;
    UpdateNSMgr(oContext);
    { Was an exclude-result-prefixes attribute specified? }
    sExcludes := GetAttribute(XpsExclResultPrefixes);
    if sExcludes <> '' then
      FNSMgr.AddExcludes(sExcludes);

    { Preprocess the top-level elements. }
    for wInx := 0 to Pred(ChildNodes.Length) do begin
      oTopLvlElement := ChildNodes.Item(wInx);
      oContext.CurrentStylesheetNode := oTopLvlElement;
      try
        { Is this a comment? }
        if (not (oTopLvlElement is TXpComment)) then
          { No. Is this an XSL element? }
          if oTopLvlElement is TXpBaseXSLElement then
            { Yes. Tell it to preprocess. }
            TXpXSLElement(oTopLvlElement).Preprocess(oContext)
          else if (oTopLvlElement is TXpElement) and
                  (ChildNodes.Length = 1) then begin
            { No but this is a simple stylesheet situation. Verify
              required attributes. }
            sAttr := TXpElement(oTopLvlElement).GetAttribute(XpsXSLColon + XpsVersion);
            if sAttr = '' then
              raise EXpXSLException.Create(MissingAttrib(XpsVersion));
            sAttr := TXpElement(oTopLvlElement).GetAttribute(XpsXSLNS);
            if (sAttr = '') then
              raise EXpXSLException.Create(MissingAttrib(XpsXSLNS));
            if (sAttr <> XpsXSLTURI) then
              raise EXpXSLException.Create(Format(sXSLInvalidAttrib +
                                                  sXSLCorrectValue,
                                                  [QuotedStr(sAttr),
                                                   QuotedStr(XpsXSLNS),
                                                   oTopLvlElement.NodeName,
                                                   QuotedStr(XpsXSLTURI)]));
          end
          else begin
            { No. It must have a non-null namespace URI. }
            if (oTopLvlElement.Prefix = '') or
               (oTopLvlElement.ResolveNSPrefix(oTopLvlElement.Prefix) = '') then
              raise EXpXSLException.Create(Format(sXSLInvalidTopLvlElement,
                                                  [oTopLvlElement.NodeName]));
          end;
      finally
        oContext.PopCurSLNode;
      end;
    end;  { for }
    FInitialized := True;
  end;
end;
{--------}
procedure TXpXSLStylesheet.StartNSLevel;
begin
  if FNSMgr.Using then
    FNSMgr.StartLevel;
end;
{--------}
function TXpXSLStylesheet.Valid(oContext : TXpXSLContext;
                            var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Version attribute is mandatory. }
    FVersion := GetAttribute(XpsVersion);
    Result := (FVersion <> '');
    if not Result then
      ErrMsg := MissingAttrib(XpsVersion);
  end;
end;
{=====================================================================}

{===TXpXSLTemplate====================================================}
constructor TXpXSLTemplate.Create;
begin
  inherited Create;
  FAllowsTemplateBody := True;
  FMode := '';
  FPriority := 0.5;
end;
{--------}
procedure TXpXSLTemplate.Clear;
begin
  inherited;
  FPattern.Free;
  FPattern := nil;
end;
{--------}
procedure TXpXSLTemplate.Execute(oContext : TXpXSLContext);
var
  oSavTemplate : TXpXSLTemplate;
begin
  oSavTemplate := oContext.CurrentTemplate;
  try
    oContext.CurrentTemplate := Self;
    inherited Execute(oContext);
  finally
    oContext.CurrentTemplate := oSavTemplate;
  end;
end;
{--------}
function TXpXSLTemplate.GetPatternPriority : Double;
begin
  Result := FPattern.Priority;
end;
{--------}
function TXpXSLTemplate.GetPrecedence : Integer;
begin
  Result := ParentStylesheet.ImportPrecedence;
end;
{--------}
function TXpXSLTemplate.IsUnionPattern : Boolean;
begin
  Result := (FPattern.IsUnionPattern);
end;
{--------}
function TXpXSLTemplate.Matches(oNode : TXpElement) : Boolean;
begin
  { Match attribute specified? }
  if FPattern <> nil then
    { Yes. See if the node matches the pattern. }
    Result := FPattern.Matches(oNode, nil)
  else
    Result := False;
end;
{--------}
procedure TXpXSLTemplate.Preprocess(oContext : TXpXSLContext);
var
  oMediator : TXpElementMediator;
  sTerm : DOMString;
begin
  if not FInitialized then begin
    inherited;
    { Has a mode? }
    FMode := GetAttribute(XpsMode);

    { Has a name? }
    FName := GetAttribute(XpsName);

    oMediator := ParentStylesheet.Mediator;

    { Has a match attribute? }
    sTerm := GetAttribute(XpsMatch);
    if sTerm <> '' then begin
      { Yes. Compile the match attribute. }
      FMatch := sTerm;
      FPattern := oMediator.PatternMaker.GeneratePattern(Self, FMatch);
    end;

    { Has a priority? }
    if HasAttribute(XpsPriority) then
      FPriority := XpStrToFloat(GetAttribute(XpsPriority))             {!!.57}
    else if FPattern <> nil then
      FPriority := FPattern.Priority
    else
      FPriority := 0.5;
      { Note: The previous IF statement is meaningless when a union pattern is
        involved. The union pattern's priority must be gathered at
        match-time based upon which of the two patterns actually matched the
        node. }

    { Add self to template manager. }
    oMediator.AddTemplate(Self);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLTemplate.Valid(oContext : TXpXSLContext;
                          var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is this a top-level element? }
    Result := IsTopLevelElement;
    if Result then begin
      { Does it have either a name or match attribute? }
      Result := ((GetAttributeNode(XpsMatch) <> nil) or
                 (GetAttributeNode(XpsName) <> nil));
      if not Result then
        ErrMsg := sXSLMatchOrNameAttrib;
    end
    else
      ErrMsg := Format(sXSLNotTopLevel, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLText========================================================}
procedure TXpXSLText.Execute(oContext : TXpXSLContext);
begin
  { Copy the child text nodes to the result tree. }
  ExecuteChildren(oContext);
end;
{--------}
procedure TXpXSLText.Preprocess(oContext : TXpXSLContext);
var
  OutputEscaping : DOMString;
  DisableOutputEsc : Boolean;
begin
  if not FInitialized then begin
    inherited;
    { Obtain disable-output-escaping attribute. }
    OutputEscaping := GetAttribute(XpsDisOutEsc);
    { Attribute set to Yes? }
    DisableOutputEsc := (OutputEscaping = XpsYes);
    if (not DisableOutputEsc) and
       (OutputEscaping <> '') and
       (OutputEscaping <> XpsNo) then
      raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                       [QuotedStr(OutputEscaping),
                                        XpsDisOutEsc, Self.NodeName]));
    { Set the value on the child text node. By this time, the Valid method
      has verified the content of the xsl:text element. }
    if ChildNodes.Length = 1 then
      TXpText(ChildNodes.Item(0)).OutputEscaping := (not DisableOutputEsc);

    FInitialized := True;
  end;
end;
{--------}
function TXpXSLText.Valid(oContext : TXpXSLContext;
                      var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Verify that it is empty or contains one child text node. }
      if (ChildNodes.Length = 0) then
        Exit
      else if (ChildNodes.Length > 1) then begin
        Result := False;
        ErrMsg := sXSLTextOneChild;
      end
      else if not (ChildNodes.Item(0) is TXpText) then begin
        ErrMsg := sXSLTextNotText;
        Result := False;
      end;
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLValueOf=====================================================}
procedure TXpXSLValueOf.Execute(oContext : TXpXSLContext);
var
  oNode : TXpText;
  sText : DOMString;
begin
  { Evaluate the select attribute. }
  sText := oContext.CurrentNode.SelectStringContext
                                  (FSelect, oContext.CurrentNodeList);
  oNode := OwnerDocument.CreateTextNode(sText);
  oNode.OutputEscaping := noOutputEscaping;
  ParentStylesheet.Mediator.CopyTextNode(oNode);
  oNode.Release;
end;
{--------}
procedure TXpXSLValueOf.Preprocess(oContext : TXpXSLContext);
var
  OutputEscaping : DOMString;
  DisableOutputEsc : Boolean;
begin
  if not FInitialized then begin
    inherited;
    { Obtain the select attribute. }
    FSelect := GetAttribute(XpsSelect);

    { Obtain disable-output-escaping attribute. }
    OutputEscaping := GetAttribute(XpsDisOutEsc);
    { Attribute set to Yes? }
    DisableOutputEsc := (OutputEscaping = XpsYes);
    if (not DisableOutputEsc) and
       (OutputEscaping <> '') and
       (OutputEscaping <> XpsNo) then
      raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                       [QuotedStr(OutputEscaping),
                                        XpsDisOutEsc, Self.NodeName]));
    { Set the value on this node. It will be transferred to the text node that
      is sent to the result tree. }
    noOutputEscaping := (not DisableOutputEsc);

    FInitialized := True;
  end;
end;
{--------}
function TXpXSLValueOf.Valid(oContext : TXpXSLContext;
                         var ErrMsg : string) : Boolean;
var
  oNode : TXpNode;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it within a template body? }
    Result := InTemplateBody;
    if Result then begin
      { Select attribute is mandatory. }
      oNode := GetAttributeNode(XpsSelect);
      Result := (oNode <> nil) and (oNode.NodeValue <> '');
      if (not Result) then
        ErrMsg := MissingAttrib(XpsSelect);
    end
    else
      ErrMsg := Format(sXSLNotInBody, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLVariable====================================================}
procedure TXpXSLVariable.Execute(oContext : TXpXSLContext);
var
  oValue : TXpValue;
begin
  if (not IsGlobal) then begin
    oValue := Evaluate(oContext);
    if oValue <> nil then
      oContext.SetVar(FName, oValue);
  end;
end;
{--------}
procedure TXpXSLVariable.Preprocess(oContext : TXpXSLContext);
var
  oValue : TXpValue;
begin
  if not FInitialized then begin
    inherited;
    { Expand the namespace prefix (if present) of the name attribute. }
    FName := ExpandName(FName);

    { If global then evaluate and send value to the processor.
      Local variables are evaluated in the Execute method. }
    if IsGlobal then begin
      oValue := Evaluate(oContext);
      ParentStylesheet.Mediator.SetGlobalVar(FName, oValue);
      oValue.Free;
    end;
    FInitialized := True;
  end;
end;
{=====================================================================}

{===TXpXSLWhen========================================================}
constructor TXpXSLWhen.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
end;
{--------}
function TXpXSLWhen.ConditionMet(oContext : TxpXSLContext) : Boolean;
begin
  Result := oContext.CurrentNode.SelectBooleanContext
                                  (FTest, oContext.CurrentNodeList);
end;
{--------}
procedure TXpXSLWhen.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Obtain test attribute. }
    FTest := GetAttribute(XpsTest);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLWhen.Valid(oContext : TXpXSLContext;
                      var ErrMsg : string) : Boolean;
begin
  Result := inherited Valid(oContext, ErrMsg);
  if Result then begin
    { Is it a child of an xsl:choose element? }
    Result := (ParentNode is TXpXSLChoose);
    if Result then begin
      { Test attribute is mandatory. }
      Result := (GetAttributeNode(XpsTest) <> nil);
      if not Result then
        ErrMsg := MissingAttrib(XpsTest);
    end
    else
      ErrMsg := Format(sXSLNotInChoose, [QuotedStr(NodeName)]);
  end;
end;
{=====================================================================}

{===TXpXSLWithParam===================================================}
function TXpXSLWithParam.Evaluate(oContext : TXpXSLContext) : TXpValue;
begin
  oContext.FUsePrevVars := True;
  try
    Result := inherited Evaluate(oContext);
  finally
    oContext.FUsePrevVars := False;
  end;
end;
{--------}
procedure TXpXSLWithParam.Execute(oContext : TXpXSLContext);
var
  oValue : TXpValue;
begin
  UpdateNSMgr(oContext);
  oValue := Evaluate(oContext);
  if oValue <> nil then
    oContext.SetVar(FName, oValue);
end;
{--------}
procedure TXpXSLWithParam.Preprocess(oContext : TXpXSLContext);
begin
  if not FInitialized then begin
    inherited;
    { Expand the namespace prefix (if present) of the name attribute. }
    FName := ExpandName(FName);
    FInitialized := True;
  end;
end;
{--------}
function TXpXSLWithParam.Valid(oContext : TXpXSLContext;
                           var ErrMsg : string) : Boolean;
begin
  { Requirement: Must have xsl:apply-templates or xsl:call-template as parent
    node. }
  Result := (ParentNode is TXpXSLApplyTemplates) or
            (ParentNode is TXpXSLCallTemplate);
  if Result then
    Result := inherited Valid(oContext, ErrMsg)
  else
    ErrMsg := sXSLWithParamParent;
end;
{=====================================================================}

{====================================================================}
procedure InitializeUnit;
begin

  xpoFOPropertyHash := TXpStrHash.Create(xpc_Size59);
    { This table is used to quickly detect a Formatting Object property
      & obtain its tag code. }

  { Add the Formatting Object properties to the hash table. }
  with xpoFOPropertyHash do begin
    Add(XpsAlignmentAdjust, Pointer(FOP_ALIGNMENT_ADJUST));
    Add(XpsBackgroundAttachment, Pointer(FOP_BACKGROUND_ATTACHMENT));
    Add(XpsBackgroundColor, Pointer(FOP_BACKGROUND_COLOR));
    Add(XpsBackgroundImage, Pointer(FOP_BACKGROUND_IMAGE));
    Add(XpsBackgroundPositionHorizontal, Pointer(FOP_BACKGROUND_POSITION_H));
    Add(XpsBackgroundPositionVertical, Pointer(FOP_BACKGROUND_POSITION_V));
    Add(XpsBlankOrNotBlank, Pointer(FOP_BLANK_OR_NOT_BLANK));
    Add(XpsBorderAfterColor, Pointer(FOP_BORDER_AFTER_COLOR));
    Add(XpsBorderAfterWidth, Pointer(FOP_BORDER_AFTER_WIDTH));
    Add(XpsBorderBeforeColor, Pointer(FOP_BORDER_BEFORE_COLOR));
    Add(XpsBorderBeforeWidth, Pointer(FOP_BORDER_BEFORE_WIDTH));
    Add(XpsBorderBottomColor, Pointer(FOP_BORDER_BOTTOM_COLOR));
    Add(XpsBorderBottomWidth, Pointer(FOP_BORDER_BOTTOM_WIDTH));
    Add(XpsBorderEndColor, Pointer(FOP_BORDER_END_COLOR));
    Add(XpsBorderEndWidth, Pointer(FOP_BORDER_END_WIDTH));
    Add(XpsBorderLeftColor, Pointer(FOP_BORDER_LEFT_COLOR));
    Add(XpsBorderLeftWidth, Pointer(FOP_BORDER_LEFT_WIDTH));
    Add(XpsBorderRightColor, Pointer(FOP_BORDER_RIGHT_COLOR));
    Add(XpsBorderRightWidth, Pointer(FOP_BORDER_RIGHT_WIDTH));
    Add(XpsBorderStartColor, Pointer(FOP_BORDER_START_COLOR));
    Add(XpsBorderStartWidth, Pointer(FOP_BORDER_START_WIDTH));
    Add(XpsBorderTopColor, Pointer(FOP_BORDER_TOP_COLOR));
    Add(XpsBorderTopWidth, Pointer(FOP_BORDER_TOP_WIDTH));
    Add(XpsBreakAfter, Pointer(FOP_BREAK_AFTER));
    Add(XpsBreakBefore, Pointer(FOP_BREAK_BEFORE));
    Add(XpsColor, Pointer(FOP_COLOR));
    Add(XpsColumnNumber, Pointer(FOP_COLUMN_NUMBER));
    Add(XpsColumnWidth, Pointer(FOP_COLUMN_WIDTH));
    Add(XpsContentHeight, Pointer(FOP_CONTENT_HEIGHT));
    Add(XpsContentWidth, Pointer(FOP_CONTENT_WIDTH));
    Add(XpsDisplayAlign, Pointer(FOP_DISPLAY_ALIGN));
    Add(XpsEmptyCells, Pointer(FOP_EMPTY_CELLS));
    Add(XpsEndIndent, Pointer(FOP_END_INDENT));
    Add(XpsEndsRow, Pointer(FOP_ENDS_ROW));
    Add(XpsExternalDestination, Pointer(FOP_EXTERNAL_DESTINATION));
    Add(XpsExtent, Pointer(FOP_EXTENT));
    Add(XpsFlowName, Pointer(FOP_FLOW_NAME));
    Add(XpsFontFamily, Pointer(FOP_FONT_FAMILY));
    Add(XpsFontSize, Pointer(FOP_FONT_SIZE));
    Add(XpsFontSizeAdjust, Pointer(FOP_FONT_SIZE_ADJUST));
    Add(XpsFontStretch, Pointer(FOP_FONT_STRETCH));
    Add(XpsFontStyle, Pointer(FOP_FONT_STYLE));
    Add(XpsFontVariant, Pointer(FOP_FONT_VARIANT));
    Add(XpsFontWeight, Pointer(FOP_FONT_WEIGHT));
    Add(XpsHeight, Pointer(FOP_HEIGHT));
    Add(XpsHyphenate, Pointer(FOP_HYPHENATE));
    Add(XpsHyphenationCharacter, Pointer(FOP_HYPHENATE_CHAR));
    Add(XpsHyphenationKeep, Pointer(FOP_HYPHENATE_KEEP));
    Add(XpsHyphenationPushCharacterCount, Pointer(FOP_HYPHENATE_PUSH_CHARACTER_COUNT));
    Add(XpsHyphenationRemainCharacterCount, Pointer(FOP_HYPHENATE_REMAIN_CHARACTER_COUNT));
    Add(XpsId, Pointer(FOP_ID));
    Add(XpsKeepTogether, Pointer(FOP_KEEP_TOGETHER));
    Add(XpsKeepWithNext, Pointer(FOP_KEEP_WITH_NEXT));
    Add(XpsKeepWithPrevious, Pointer(FOP_KEEP_WITH_PREVIOUS));
    Add(XpsLanguage, Pointer(FOP_LANGUAGE));
    Add(XpsLeaderLength, Pointer(FOP_LEADER_LENGTH));
    Add(XpsLeaderPattern, Pointer(FOP_LEADER_PATTERN));
    Add(XpsLetterSpacing, Pointer(FOP_LETTER_SPACING));
    Add(XpsLineHeight, Pointer(FOP_LINE_HEIGHT));
    Add(XpsLineHeightShiftAdjustment, Pointer(FOP_LINE_HEIGHT_SHIFT_ADJUSTMENT));
    Add(XpsMarginBottom, Pointer(FOP_MARGIN_BOTTOM));
    Add(XpsMarginLeft, Pointer(FOP_MARGIN_LEFT));
    Add(XpsMarginRight, Pointer(FOP_MARGIN_RIGHT));
    Add(XpsMarginTop, Pointer(FOP_MARGIN_TOP));
    Add(XpsMasterName, Pointer(FOP_MASTER_NAME));
    Add(XpsNumberColumnsRepeated, Pointer(FOP_NUMBER_COLUMNS_REPEATED));
    Add(XpsOddOrEven, Pointer(FOP_ODD_OR_EVEN));
    Add(XpsOrphans, Pointer(FOP_ORPHANS));
    Add(XpsOverflow, Pointer(FOP_OVERFLOW));
    Add(XpsPaddingAfter, Pointer(FOP_PADDING_AFTER));
    Add(XpsPaddingBefore, Pointer(FOP_PADDING_BEFORE));
    Add(XpsPaddingBottom, Pointer(FOP_PADDING_BOTTOM));
    Add(XpsPaddingEnd, Pointer(FOP_PADDING_END));
    Add(XpsPaddingLeft, Pointer(FOP_PADDING_LEFT));
    Add(XpsPaddingRight, Pointer(FOP_PADDING_RIGHT));
    Add(XpsPaddingStart, Pointer(FOP_PADDING_START));
    Add(XpsPaddingTop, Pointer(FOP_PADDING_TOP));
    Add(XpsPageHeight, Pointer(FOP_PAGE_HEIGHT));
    Add(XpsPagePosition, Pointer(FOP_PAGE_POSITION));
    Add(XpsPageWidth, Pointer(FOP_PAGE_WIDTH));
    Add(XpsPrecedence, Pointer(FOP_PRECEDENCE));
    Add(XpsProvisionalDistanceBetweenStarts, Pointer(FOP_PROVISIONAL_DISTANCE_BETWEEN_STARTS));
    Add(XpsProvisionalLabelSeparation, Pointer(FOP_PROVISIONAL_LABEL_SEPARATION));
    Add(XpsRegionName, Pointer(FOP_REGION_NAME));
    Add(XpsRuleThickness, Pointer(FOP_RULE_THICKNESS));
    Add(XpsScaling, Pointer(FOP_SCALING));
    Add(XpsScalingMethod, Pointer(FOP_SCALING_METHOD));
    Add(XpsShowDestination, Pointer(FOP_SHOW_DESTINATION));
    Add(XpsSpaceAfterMaximum, Pointer(FOP_SPACE_AFTER_MAX));
    Add(XpsSpaceAfterMinimum, Pointer(FOP_SPACE_AFTER_MIN));
    Add(XpsSpaceAfterOptimum, Pointer(FOP_SPACE_AFTER_OPT));
    Add(XpsSpaceBeforeMaximum, Pointer(FOP_SPACE_BEFORE_MAX));
    Add(XpsSpaceBeforeMinimum, Pointer(FOP_SPACE_BEFORE_MIN));
    Add(XpsSpaceBeforeOptimum, Pointer(FOP_SPACE_BEFORE_OPT));
    Add(XpsSpaceEnd, Pointer(FOP_SPACE_END));
    Add(XpsSpaceStart, Pointer(FOP_SPACE_START));
    Add(XpsSrc, Pointer(FOP_SRC));
    Add(XpsStartIndent, Pointer(FOP_START_INDENT));
    Add(XpsStartsRow, Pointer(FOP_STARTS_ROW));
    Add(XpsTextAlign, Pointer(FOP_TEXT_ALIGN));
    Add(XpsTextAlignLast, Pointer(FOP_TEXT_ALIGN_LAST));
    Add(XpsTextIndent, Pointer(FOP_TEXT_INDENT));
    Add(XpsWidows, Pointer(FOP_WIDOWS));
    Add(XpsWidth, Pointer(FOP_WIDTH));
    Add(XpsWordSpacing, Pointer(FOP_WORD_SPACING));
    Add(XpsWrapOption, Pointer(FOP_WRAP_OPTION));
  end;  { with  }
end;
{--------}
procedure FinalizeUnit;
begin
  xpoFOPropertyHash.Free;
end;
{====================================================================}
initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

