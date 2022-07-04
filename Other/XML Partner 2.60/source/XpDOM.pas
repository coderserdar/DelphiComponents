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
{* XMLPartner: XpDOM.PAS                                 *}
{*********************************************************}
{* XMLPartner: Document object model implementation      *}
{*********************************************************}

{$I XpDefine.inc}

unit XpDOM;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  SysUtils,
  Classes,
  XpParser,
  XpBase,
  XpChrFlt,                                                            {!!.52}
  XpExcept;

const
  ELEMENT_NODE                = 1;
  ATTRIBUTE_NODE              = 2;
  TEXT_NODE                   = 3;
  CDATA_SECTION_NODE          = 4;
  ENTITY_REFERENCE_NODE       = 5;
  ENTITY_NODE                 = 6; { Unused, uses ENTITY_DECL_NODE instead}
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE                = 8;
  DOCUMENT_NODE               = 9;
  DOCUMENT_TYPE_NODE          = 10;
  DOCUMENT_FRAGMENT_NODE      = 11;
  NOTATION_NODE               = 12; { Unused, uses NOTATION_DECL_NODE instead}

  ELEMENT_DECL_NODE           = 50;
  ELEMENT_DECL_GROUP_NODE     = 51;
  ELEMENT_DECL_CONTENT_NODE   = 52;
  ATTLIST_DECL_NODE           = 53;
  ATT_DEF_DECL_NODE           = 54;
  ENTITY_DECL_NODE            = 55;
  NOTATION_DECL_NODE          = 56;
  CONDITIONAL_DECL_NODE       = 57;

  XPATH_TOKEN                 = 99;                                    {!!.52}

  { Validation errors }
  V_NODTD                   = 1;  { No DTD for the document}
  V_BADDTD                  = 2;  { Bad DTD structure}
  V_NODOCUMENT              = 3;  { No document to check}
  V_MISMATCH                = 4;  { Document name does not match DTD name}
  V_ELEMENTNOTDEFINED       = 5;  { Element name not defined in DTD}
  V_ELEMENTNOTEMPTY         = 6;  { Element declared as empty but not empty}
  V_ELEMENTNOTCONTENT       = 7;  { Element not allowed as content}
  V_ELEMENTCHILDMISSING     = 8;  { Required child element missing from element}
  V_ELEMENTCHILDNOTEXPECTED = 9;  { Encountered child that was not expected}
  V_ELEMENTATTRNOTDEFINED   = 10; { Attributes for element are not defined}
  V_ATTRIBUTEFIXED          = 11; { Cannot change a fixed attribute}
  V_ATTRIBUTENOTENUM        = 12; { Attribute not in enumeration}
  V_ATTRIBUTEREQUIRED       = 13; { Attribute is required}
  V_STANDALONEVIOLATED      = 14; { XML declaration has standalone = yes,
                                    but external requirement found}
  V_ATTRIBUTENOTEXPECTED    = 15; { Encountered an attribute that was not expected } {!!.55}

  { Expression tokens }
  TOK_NUMBER                  = 1;
  TOK_STRING_TYPE             = 2;
  TOK_ELEMENT                 = 3;
  TOK_ATTRIBUTE               = 4;
  TOK_OPSUM                   = 5;
  TOK_OPDIFF                  = 6;
  TOK_OPMUL                   = 7;
  TOK_OPDIV                   = 8;
  TOK_OPCONCAT                = 9;
  TOK_OPMOD                   = 10;
  TOK_WILD                    = 20;
  TOK_COMMA                   = 21;
  TOK_LPAREN                  = 22;
  TOK_RPAREN                  = 23;
  TOK_BANG                    = 24;
  TOK_SLASH                   = 25;
  TOK_2SLASH                  = 26;
  TOK_DOT                     = 27;
  TOK_2DOT                    = 28;
  TOK_LFRAME                  = 29;
  TOK_RFRAME                  = 30;
  TOK_LBRACK                  = 31;
  TOK_RBRACK                  = 32;
  TOK_TO                      = 40;
  TOK_AND                     = 41;
  TOK_OR                      = 42;
  TOK_NOT                     = 43;
  TOK_EQ                      = 44;
  TOK_NE                      = 45;
  TOK_LT                      = 46;
  TOK_LE                      = 47;
  TOK_GT                      = 48;
  TOK_GE                      = 49;
  TOK_UNION                   = 50;
  TOK_INTERSECT               = 51;
  TOK_CIEQ                    = 52;
  TOK_CINE                    = 53;
  TOK_CILT                    = 54;
  TOK_CILE                    = 55;
  TOK_CIGT                    = 56;
  TOK_CIGE                    = 57;
  TOK_ANCESTOR                = 65;
  TOK_ANCESTOR_OR_SELF        = 66;
  TOK_ID                      = 71;
  TOK_TRUE                    = 72;
  TOK_FALSE                   = 73;
  TOK_DATE                    = 74;
  TOK_IVALUE                  = 76;
  TOK_INODETYPE               = 77;
  TOK_INODENAME               = 78;
  TOK_IINDEX                  = 79;
  TOK_ICOUNT                  = 80;
  TOK_INODETYPESTRING         = 81;
  TOK_CURRENT                 = 82;
  TOK_CMTEXTNODE              = 100;
  TOK_CMCOMMENT               = 101;
  TOK_CMPI                    = 102;
  TOK_CMELEMENT               = 103;
  TOK_CMATTRIBUTE             = 104;
  TOK_CMNODE                  = 105;
  TOK_FARG                    = 120;
  TOK_FCONSTANT               = 130;
  TOK_FVAR                    = 131;
  TOK_FIRSTOFTYPE             = 150;
  TOK_NOTFIRSTOFTYPE          = 151;
  TOK_LASTOFTYPE              = 152;
  TOK_NOTLASTOFTYPE           = 153;
  TOK_ONLYOFTYPE              = 154;
  TOK_NOTONLYOFTYPE           = 155;
  TOK_FIRSTOFANY              = 156;
  TOK_NOTFIRSTOFANY           = 157;
  TOK_LASTOFANY               = 158;
  TOK_NOTLASTOFANY            = 159;
  TOK_ONLYOFANY               = 160;
  TOK_NOTONLYOFANY            = 161;
  TOK_AXIS_ANCESTOR           = 170;
  TOK_AXIS_ANCESTOR_OR_SELF   = 171;
  TOK_AXIS_ATTRIBUTE          = 172;
  TOK_AXIS_CHILD              = 173;
  TOK_AXIS_DESCENDANT         = 174;
  TOK_AXIS_DESCENDANT_OR_SELF = 175;
  TOK_AXIS_FOLLOWING          = 176;
  TOK_AXIS_FOLLOWING_SIBLING  = 177;
  TOK_AXIS_NAMESPACE          = 178;
  TOK_AXIS_PARENT             = 179;
  TOK_AXIS_PRECEDING          = 180;
  TOK_AXIS_PRECEDING_SIBLING  = 181;
  TOK_AXIS_SELF               = 182;
  TOK_UNPARSED_ENTITY_URI     = 189;                                   {!!.53}
  TOK_LAST                    = 190;
  TOK_POSITION                = 191;
  TOK_KEY                     = 192;
  TOK_DOCUMENT                = 193;
  TOK_DOCREF                  = 194;
  TOK_LOCAL_PART              = 195;
  TOK_NAMESPACE               = 196;
  TOK_QNAME                   = 197;
  TOK_NAMESPACEURI            = 198;                                   {!!.52}
  TOK_ELEMENT_AVAILABLE       = 199;                                   {!!.53}
  TOK_GENERATEID              = 200;
  TOK_BOOLEAN                 = 201;
  TOK_LANG                    = 202;
  TOK_OPQUO                   = 203;
  TOK_FLOOR                   = 204;
  TOK_CEILING                 = 205;
  TOK_ROUND                   = 206;
  TOK_STRING_CAST             = 207;
  TOK_STARTS_WITH             = 208;
  TOK_CONTAINS                = 209;
  TOK_STRING_LENGTH           = 210;
  TOK_SUBSTRING               = 211;
  TOK_SUBSTRING_BEFORE        = 212;
  TOK_SUBSTRING_AFTER         = 213;
  TOK_NORMALIZE               = 214;
  TOK_TRANSLATE               = 215;
  TOK_FORMAT_NUMBER           = 216;
  TOK_FUNCTION_AVAILABLE      = 217;
  TOK_OPUNARY                 = 218;
  TOK_VARIABLE                = 219;
  TOK_NUMBER_CAST             = 220;
  TOK_SUM                     = 221;
  TOK_FCONCAT                 = 222;
  TOK_SYSTEM_PROPERTY         = 223;
  TOK_FUNCTION                = 224;

type
  EXpExceptionCode = (ecNone, ecIndexSizeErr, ecWStringSizeErr,
                      ecHierarchyRequestErr, ecWrongDocumentErr,
                      ecInvalidNameErr, ecNoDataAllowedErr,
                      ecNoModificationAllowedErr, ecNotFoundErr,
                      ecNotSupportedErr, ecInuseAttributeErr,
                      ecInvalidParamErr, ecExpectedToken);
  TXpExpressionType = (xpetUnknown, xpetNodeSet, xpetBoolean,          {!!.52}
                       xpetNumber, xpetString);                        {!!.52}
  TXpAnswer = (xpanUnknown, xpanYes, xpanNo);                          {!!.53}

  { Forward declarations }
  TXpDocument = class;
  TXpDocumentType = class;
  TXpElement = class;
  TXpNodeList = class;
  TXpValue = class;
  TXpXPathParser = class;
  TXpNode = class;
  TXpNamedNodeMap = class;

  TXpCharTestMethod = function(const aChar : DOMChar) : Boolean;

{Begin !!.52}
  TXpSystemLookupEvent = procedure(oOwner    : TObject;
                                   sVarName  : DOMString;
                               var sVarValue : DOMString) of object;

  TXpCurrentNodeEvent = procedure(oOwner : TObject;
                              var oCurrentNode : TXpNode) of object;
    { The XPath engine uses this event to obtain the current node from
      the XSL processor. }

  TXpNodeEvent = procedure(oNode : TXpNode) of object;
    { The DOM uses this event for protected notifications that are
      node-specific. }

  TXpNodeCancelEvent = procedure(oNode   : TXpNode;
                             var bCancel : Boolean) of object;
    { The DOM uses this event for protected notifications that are
      node-specific and which may be canceled. }

  TXpFormatNumberEvent = procedure(oOwner : TXpNode;
                             const sNumber, sPattern, sFormat : DOMString;
                               var sFormattedNumber : DOMString) of object;
    { Raised by XPath engine when format-number() function encountered. }

  TXpKeyLookupEvent = procedure(oDoc : TXpDocument;
                          const sKeyName, sKeyValue : DOMString;
                                oList : TXpNodeList) of object;
    { Raised by XPath engine when key() function is encountered.
      oDoc is the instance of TXpDocument representing the XML document
      loaded by the DOM. sKeyName is the name of the key. sKeyValue is the
      key value being sought. oList is the node list into which those nodes
      in document oDoc matching sKeyValue are placed. }

{End !!.52}

  TXpVariableLookupEvent = procedure(oOwner    : TObject;
                               const sVarName  : DOMString;            {!!.57}
                                 var oVarValue : TXpValue) of object;  {!!.52}

  TXpFunctionAvailableEvent = procedure(oOwner        : TObject;
                                        sFunctionName : DOMString;
                                    var bAvailable    : Boolean) of object;

  TXpFunctionEvent = procedure(oOwner        : TObject;
                               sFunctionName : DOMString;
                               oArguments    : TStringList;
                           var sResult       : DOMString) of object;

{Begin !!.53}
  TXpElementAvailableEvent = procedure(oNode : TXpNode;
                                 const sElementName : DOMString;
                                   var bAvailable : Boolean) of object;

  TXpResolveDocumentEvent = procedure(oNode : TXpNode;
                                const sHref, sBaseURI : DOMString;
                                  var oRoot : TXpDocument) of object;
    { Raised by the XPath engine when the document() function is
      encountered. The handler must load the specified document and return
      the document's root node in parameter oRoot.

      Parameter oNode is the instance of TXpNode from which the
      XPath expression is being evaluated. Parameter sHref is URI reference
      for the document to be loaded. sHref may be an absolute or relative
      reference. If it is a relative reference then sBaseURI is the base URI
      used to resolve the relative reference. }
{End !!.53}

{Begin !!.52}
  TXpStopTestMethod = function(const aValue : DOMString; const wPos : Integer) : Boolean;
    { Used by GetDelimitedTokenF to determine when the parser should stop
      evaluating a string. aValue is the result string being returned to the
      parser. wPos is the current position within that string (i.e., the result
      string is sized to fit and the test cannot depend upon its length). }
{End !!.52}

  TXpInvalidHalt = class(EXpException);

  TXpDOMException = class(EXpException)
    protected
      deCode  : EXpExceptionCode;
      deValue : Integer;
    public
      constructor CreateCode(oCode : EXpExceptionCode);
      constructor CreateValue(oCode  : EXpExceptionCode;
                              wValue : Integer);
      property Code : EXpExceptionCode
         read deCode;
      property Value : Integer
         read deValue;
  end;

{Begin !!.52}
  TXpXPathException = class(EXpException);

  TXpValue = class
    { This class represents a value returned for an expression, where the
      value can take the form of a nodeset, boolean, number, or string.
      It is used by the XSL processor when dealing with such things as
      variables and parameters. }
  protected
    FValueType : TXpExpressionType;
    FValueBoolean : Boolean;
    FValueNodeSet : TXpNodeList;
    FValueNumber : Double;
    FValueString : DOMString;

    procedure Clear;

    function GetAsBoolean : Boolean;
    function GetAsNodeSet : TXpNodeList;
    function GetAsNumber : Double;
    function GetAsString : DOMString;

    procedure SetBoolean(const aValue : Boolean);
    procedure SetNodeSet(const aValue : TXpNodeList);
    procedure SetNumber(const aValue : Double);
    procedure SetString(const aValue : DOMString);

  public
    constructor Create;
    destructor Destroy; override;

    function Clone : TXpValue;
      { Clone a value. }

    procedure SelectValue(const sExpr : DOMString;
                                oNode : TXpNode;
                                oContext : TXpNodeList);
      { Calculate the value of an expression based upon a certain node &
        current node list. }

    property AsBoolean : Boolean
      read GetAsBoolean write SetBoolean;
      { Return or set the value as a boolean. }

    property AsNodeSet : TXpNodeList
      read GetAsNodeSet write SetNodeSet;
      { Return or set the value as a nodeset. }

    property AsNumber : Double
      read GetAsNumber write SetNumber;
      { Return or set the value as a double. }

    property AsString : DOMString
      read GetAsString write SetString;
      { Return or set the value as a string. }

    property ValueType : TXpExpressionType
      read FValueType;
      { Returns the type of value (e.g., boolean, string). }
  end;
{End !!.52}


  TXpDomImplementation = class(TObject)
    public
      function CreateDocument(const sNamespaceURI,                     {!!.57}
                              sQualifiedName : DOMString;
                              oDocumentType  : TXpDocumentType)
                                             : TXpDocument;
      function CreateDocumentType(const sQualified,                    {!!.57}
                                  sPublicID,
                                  sSystemID : DOMString)
                                            : TXpDocumentType;
      function HasFeature(const sFeature,                              {!!.57}
                          sVersion : DOMString) : Boolean;
  end;

  TXpNodeList = class(TObject)
    protected
      nlList : TList;
      nlTag : Integer;                                                 {!!.52}

      function nlGetLength : Integer;
      function nlomGetXpDocument : DOMString;
      procedure nlSortList(const dwSize     : Integer;
                           const dwFirst    : Integer;
                           const dwLast     : Integer;
                           const sAttribute : DOMString);
    public
      procedure Add(oNode : TXpNode);
      procedure CopyList(oNodeList : TXpNodeList);
      procedure CopyNamedNodeMap(oNamedNodeMap : TXpNamedNodeMap);
      constructor Create;
      procedure Delete(wIndex : Integer);
      destructor Destroy; override;
      procedure Empty;
      function Exchange(wSrc,
                        wDest : Integer) : Boolean;
      function IndexOf(oNode : TXpNode) : Integer;
      procedure Insert(wIndex : Integer;
                       oNode  : TXpNode);
      function Item(wIndex : Integer) : TXpNode;
      function Last : TXpNode;                                         {!!.52}
{$IFDEF NodeMemoryUsageEnabled}
      function MemoryUsed : Longint; virtual;
{$ENDIF}
      function Move(wSrc,
                    wDest : Integer) : Boolean;
      procedure Replace(wIndex : Integer; oNode: TXpNode);
      procedure Sort(const sAttribute : DOMString                      {!!.57}
        {$IFNDEF VER100}
        = ''
        {$ENDIF}
        ; wOrder : Integer
        {$IFNDEF VER100}
        = 0
        {$ENDIF}
        );
      property Length : Integer
         read nlGetLength;
      property Tag : Integer                                           {!!.52}
         read nlTag write nlTag;                                       {!!.52}
      property XmlDocument : DOMString
         read nlomGetXpDocument;
  end;

  {forward declarations}
  TXpElementClasses = class of TXpElement;                             {!!.52}
  TXpNodeClasses = class of TXpNode;                                   {!!.52}

  EXpAddNodeType = (antSelf, antChild, antDescend, antFollowSibs,
                    antPrecedSibs, antNamespace, antPreceding,         {!!.52}
                    antFollowing);                                     {!!.52}
  TXpAddNodeTypeSet = set of EXpAddNodeType;

  TXpNode = class(TObject)
    protected                                                          {!!.51}
      noAttributes       : TXpNamedNodeMap;
      noBaseURI          : DOMString;                                  {!!.53}
      noChildNodes       : TXpNodeList;
      noDefaultNameSpace : DOMString;
{Begin !!.52}
      noOutputEscaping : Boolean;
        { If True then special characters such as '<' and '&' are
          not escaped during output. This facility was built in for
          the sake of the xsl:text element. }
{End !!.52}
      noNamespaceList    : TXpNamedNodeMap;
      noNodeId           : Integer;
      noNodeName         : DOMString;
      noNodeType         : Integer;
      noNodeValue        : DOMString;
      fnoOwnerDocument    : TXpDocument;                               {!!.57}
      noParentNode       : TXpNode;
      noRefCount         : Integer;
      noTag              : Longint;

      procedure SetnoOwnerDocument(const Value: TXpDocument);
      property noOwnerDocument : TXpDocument read fnoOwnerDocument write SetnoOwnerDocument;

      procedure noCheckForLineBreaks(const OldText : DOMString;        {!!.56}{!!.57 - Start}
                                       var NewText : DOMString;
                                       var OldPos,
                                           NewPos : Integer);          {!!.57 - End}
      procedure noCheckToken(wToken      : Integer;
                             oTokenList  : TXpNodeList;
                         var wTokenIndex : Integer);
      procedure noFindMatchingRFRAME(oTokenList  : TXpNodeList;
                                 var wTokenIndex : Integer);
      procedure noFindMatchingRPAREN(oTokenList  : TXpNodeList;        {!!.53}
                                 var wTokenIndex : Integer);           {!!.53}
      function noGetAllChildText(oNode : TXpNode;                      {!!.53}
                           const bAddSpace : Boolean) : DOMString;     {!!.53}
      function noGetBaseURI : DOMString; virtual;                      {!!.53}
      function noGetChildNodesByNodeTypeName(wType : Integer;
                                       const sName : DOMString) : TXpNodeList;
      function noGetFirstChild : TXpNode;
      function noGetLastChild : TXpNode;
      function noGetLevelCode : DOMString;
      function noGetLocalName : DOMString;
      function noGetNamespace : DOMString;
      function noGetNextInDocument : TXpNode;                          {!!.52}
        { Get the next node in the document. }                         {!!.52}
      function noGetNextSibling : TXpNode;
      function noGetNodesByNodeTypeName(wType : Integer;
                                  const sName : DOMString) : TXpNodeList; virtual; {!!.53}
      function noGetNodeStringType : DOMString; virtual;
      function noGetOwnerDocument : TXpDocument;
      function noGetPrefix : DOMString;
      function noGetPreviousInDocument : TXpNode;                      {!!.52}
        { Get the previous node in the document. }                     {!!.52}
      function noGetPreviousSibling : TXpNode;
      function noGetStringValue : DOMString; virtual;                  {!!.53}{!!.55}
      function noGetText : DOMString;
      function noGetLastDescendantOrSelf : TXpNode;                    {!!.52}
      function noNameEquals(const aName : DOMString) : boolean;
      function noNameTest(oNode : TXpNode; const args : array of const) : Boolean; {!!.52}
      procedure noNextRParen(oTokenList : TXpNodeList;                 {!!.52}
                         var wTokenIndex : Integer);                   {!!.52}
      function noNormalize(const aValue : DOMString) : DOMString;
      function noomGetXpDocument : DOMString;
{Begin !!.53}
      procedure noOutput(oMem : TMemoryStream;
                         wLevel : Integer); virtual;
        { Output a human-readable version of the node to the
          specified memory stream. }
{End !!.53}
      function noOutputDTDAttDefinitions(oNode : TXpNode) : DOMString;
      function noOutputDTDElementContent(oNode : TXpNode) : DOMString;
      procedure noOutputIndent(oMem : TMemoryStream;                   {!!.52}
                         const wLevel : Integer);                      {!!.52}
      procedure noOutputXpAttributes(oMem   : TMemoryStream;
                                     oAttrs : TXpNamedNodeMap);
      function noOutputXpSiblings(oMem   : TMemoryStream;
                                  oNode  : TXpNode;
                            const wLevel : Integer) : Boolean;
      procedure noOutputXpText(oMem       : TMemoryStream;
                         const sText      : DOMString;                 {!!.57}
                               bAttribute : Boolean);
      function noParseAdditiveExpr(var oElemList       : TXpNodeList;
                                       oTokenList      : TXpNodeList;
                                   var wTokenIndex     : Integer;
                                   var oExpressionType : TXpExpressionType;
                                       oCurrentNode    : TXpNode)
                                                       : DOMString;
      function noParseAndExpr(var oElemList       : TXpNodeList;
                                  oTokenList      : TXpNodeList;
                              var wTokenIndex     : Integer;
                              var oExpressionType : TXpExpressionType;
                                  oCurrentNode    : TXpNode)
                                                  : DOMString;
      function noParseEqualityExpr(var oElemList       : TXpNodeList;
                                       oTokenList      : TXpNodeList;
                                   var wTokenIndex     : Integer;
                                   var oExpressionType : TXpExpressionType;
                                       oCurrentNode    : TXpNode)
                                                       : DOMString;
      function noParseGetStartElement(oTokenList      : TXpNodeList;
                                  var wTokenIndex     : Integer;
                                      oNode           : TXpNode;
                                  var oExpressionType : TXpExpressionType)
                                                      : TXpNode;
      function noParseMultiplicativeExpr(var oElemList       : TXpNodeList;
                                             oTokenList      : TXpNodeList;
                                         var wTokenIndex     : Integer;
                                         var oExpressionType : TXpExpressionType;
                                             oCurrentNode    : TXpNode)
                                                             : DOMString;
      function noParseNewAddNodes(var oElemList       : TXpNodeList;
                                      oNode           : TXpNode;
                                      oTokenList      : TXpNodeList;
                                  var wTokenIndex     : Integer;
                                  var oExpressionType : TXpExpressionType;
                                      oAddType        : TXpAddNodeTypeSet
                                      {$IFNDEF VER100}
                                             = [antChild]
                                      {$ENDIF}
                                      )  : DOMString;

      function noParseOrExpr(var oElemList       : TXpNodeList;
                                 oTokenList      : TXpNodeList;
                             var wTokenIndex     : Integer;
                             var oExpressionType : TXpExpressionType;
                                 oCurrentNode    : TXpNode)
                                                 : DOMString;
      function noParsePathExpr(var oElemList       : TXpNodeList;
                                   oTokenList      : TXpNodeList;
                               var wTokenIndex     : Integer;
                               var oExpressionType : TXpExpressionType;
                                   oCurrentNode    : TXpNode)
                                                   : DOMString;

{Begin !!.52}
      function noParsePathContinue(var oElemList       : TXpNodeList;
                                       oTokenList      : TXpNodeList;
                                   var wTokenIndex     : Integer;
                                   var oExpressionType : TXpExpressionType;
                                       oCurrentNode    : TXpNode)
                                                       : DOMString;
{End !!.52}

      function noParseRelationalExpr(var oElemList       : TXpNodeList;
                                         oTokenList      : TXpNodeList;
                                     var wTokenIndex     : Integer;
                                     var oExpressionType : TXpExpressionType;
                                         oCurrentNode    : TXpNode)
                                                         : DOMString;
      function noParseUnaryExpr(var oElemList       : TXpNodeList;
                                    oTokenList      : TXpNodeList;
                                var wTokenIndex     : Integer;
                                var oExpressionType : TXpExpressionType;
                                    oCurrentNode    : TXpNode)
                                                    : DOMString;
      function noParseUnionExpr(var oElemList       : TXpNodeList;
                                    oTokenList      : TXpNodeList;
                                var wTokenIndex     : Integer;
                                var oExpressionType : TXpExpressionType;
                                    oCurrentNode    : TXpNode)
                                                  : DOMString;

{Begin !!.53}
      function noParseUnpEntityURI(var oElemList       : TXpNodeList;
                                       oTokenList      : TXpNodeList;
                                   var wTokenIndex     : Integer;
                                   var oExpressionType : TXpExpressionType;
                                       oCurrentNode    : TXpNode) : DOMString;
{End !!.53}

{Begin !!.52}
      function noPredicate(var oElemList       : TXpNodeList;
                               oTokenList      : TXpNodeList;
                           var wTokenIndex     : Integer;
                           var oExpressionType : TXpExpressionType;
                               oCurrentNode    : TXpNode;
                               bPosWithinParent : Boolean)
                                             : DOMString;
{End !!.52}

{Begin !!.53}
      procedure noResolveDocument(var oElemList       : TXpNodeList;
                                      oTokenList      : TXpNodeList;
                                  var wTokenIndex     : Integer;
                                  var oExpressionType : TXpExpressionType;
                                      oCurrentNode    : TXpNode);
{End !!.53}

{Begin !!.52}
      function noTestToken(wToken      : Integer;
                           oTokenList  : TXpNodeList;
                       var wTokenIndex : Integer) : Boolean;
{End !!.52}
      function noTranslate(const sValue, sFrom, sTo : DOMString) : DOMString;
      procedure noTraverseTreeForNodes(oList : TXpNodeList;
                                       oNode : TXpNode;
                                 const wType : Integer);
{Begin !!.52}
      procedure noTraverseTreeForNamedNodes(oList : TXpNodeList;
                                            oNode : TXpNode;
                                      const sName : DOMString;
                                      const wType : Integer); virtual; {!!.53}
{End !!.52}
      procedure noTraverseTreeToNormalize(oNode     : TXpNode;
                                          bAddSpace : Boolean);
    public
      procedure AddRef;
      procedure AppendChild(oNewChild : TXpNode);
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; virtual;                                          {!!.52}
      constructor Create; virtual;                                     {!!.52}
      destructor Destroy; override;

{$IFDEF NodeMemoryUsageEnabled}
      function MemoryUsed : Longint; virtual;
{$ENDIF}
      procedure ForceOwnerDocument(oNode : TXpNode);
      function GetChildNodesByNodeType(wType : Integer) : TXpNodeList;
      function GetNodesByNodeType(wType : Integer) : TXpNodeList;
      function HasAttributes : Boolean;
      function HasChildNodes : Boolean;
      procedure InsertBefore(oNewChild, oRefChild : TXpNode);
      function IsAfter(oNode : TXpNode) : Boolean;
      function IsSupported(const sFeature, sVersion : DOMString) : Boolean; {!!.57}
      function LocationPath : DOMString; virtual;                      {!!.53}
      procedure Normalize(bAddSpace : Boolean
        {$IFNDEF VER100}
        = False
        {$ENDIF}
        );
      procedure Release;
      procedure RemoveAll; virtual;
      function RemoveChild(oRefChild : TXpNode) : TXpNode;
      function ReplaceChild(oNewChild, oRefChild : TXpNode) : TXpNode;
      function ResolveNSPrefix(const sPrefix : DOMString) : DOMString;
{!!.52 - Moved Select functionality from TXpElement to TXpNode due to
  required functionality in XSL processor. }
      function Select(const sQuery : DOMString;                 {!!.52}{!!.57}
                      oContext : TXpNodeList) : TXpValue;              {!!.52}
      function SelectBoolean(const sQuery : DOMString) : Boolean;      {!!.57}
      function SelectBooleanContext(const sQuery : DOMString;   {!!.52}{!!.57}
                                    oContext : TXpNodeList) : Boolean; {!!.52}
      function SelectInteger(const sQuery : DOMString) : Integer;      {!!.57}
      function SelectNodes(const sQuery : DOMString) : TXpNodeList;    {!!.57}
      function SelectNumber(const sQuery : DOMString) : Double;        {!!.57}
      function SelectNumberContext(const sQuery : DOMString;    {!!.52}{!!.57}
                                   oContext : TXpNodeList) : Double;   {!!.52}
      function SelectSingleNode(const sQuery : DOMString) : TXpElement; {!!.57}
      function SelectString(const sQuery : DOMString) : DOMString;     {!!.57}
      function SelectStringContext(const sQuery : DOMString;    {!!.52}{!!.57}
                                   oContext : TXpNodeList) : DOMString;{!!.52}

      property Attributes : TXpNamedNodeMap
         read noAttributes;
      property BaseURI : DOMString                                     {!!.53}
         read noGetBaseURI;                                            {!!.53}
      property ChildNodes : TXpNodeList
         read noChildNodes;
      property Defaultnamespace : DOMString                            {!!.52}
         read noDefaultNamespace;                                      {!!.52}
      property FirstChild : TXpNode
         read noGetFirstChild;
      property LastChild : TXpNode
         read noGetLastChild;
      property LastDescendantOrSelf : TXpNode                          {!!.52}
         read noGetLastDescendantOrSelf;                               {!!.52}
      property LevelCode : DOMString
         read noGetLevelCode;
      property LocalName : DOMString
         read noGetLocalName;
      property NamespaceURI : DOMString
         read noGetNamespace;
      property NamespaceList : TXpNamedNodeMap                         {!!.52}
         read noNamespaceList;                                         {!!.52}
      property NextInDocument : TXpNode                                {!!.52}
         read noGetNextInDocument;                                     {!!.52}
      property NextSibling : TXpNode
         read noGetNextSibling;
      property NodeId : Integer
         read noNodeId
         write noNodeId;
      property NodeName : DOMString
         read noNodeName
         write noNodeName;
      property NodeStringType : DOMString
         read noGetNodeStringType;
      property NodeType : Integer
         read noNodeType;
      property NodeValue : DOMString
         read noNodeValue
         write noNodeValue;
      property OwnerDocument : TXpDocument
         read noGetOwnerDocument;
      property ParentNode : TXpNode
         read noParentNode;
      property Prefix : DOMString
         read noGetPrefix;
{Begin !!.52}
      property PreviousInDocument : TXpNode
         read noGetPreviousInDocument;
{End !!.52}
      property PreviousSibling : TXpNode
         read noGetPreviousSibling;
      property StringValue : DOMString                                 {!!.53}
         read noGetStringValue;                                        {!!.53}
      property Tag : Longint
         read noTag write noTag;
      property Text : DOMString
         read noGetText;
      property XmlDocument : DOMString
         read noomGetXpDocument;
  end;


  TXpCharacterData = class(TXpNode)
    protected
      function cdGetLength : Integer;

    public
      procedure AppendData(const sData : DOMString);                   {!!.57}
      procedure DeleteData(wOffset, wCount : Integer);
      procedure InsertData(wOffset : Integer; const sData : DOMString); {!!.57}
      procedure ReplaceData(wOffset, wCount : Integer; const sData : DOMString); {!!.57}
      function SubStringData(wOffset, wCount : Integer ) : DOMString;

      property Data : DOMString
         read noNodeValue
         write noNodeValue;
      property Length : Integer
         read cdGetLength;
  end;


  TXpDocumentFragment = class(TXpNode)
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;
  end;


  TXpComment = class(TXpCharacterData)
    protected
      procedure noOutput(oMem : TMemoryStream;
                         wLevel : Integer); override;

      function noGetStringValue : DOMString; override;                 {!!.55}
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;
  end;


  TXpProcessingInstruction = class(TXpNode)
{Begin !!.55}
    protected
      function noGetStringValue : DOMString; override;
{End !!.55}
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = true
        {$ENDIF}
        ) : TXpNode; override;

      constructor Create; override;
      property Data : DOMString
         read noNodeValue
         write noNodeValue;
      property Target : DOMString
         read noNodeName
         write noNodeName;
  end;


  TXpEntityReference = class(TXpNode)
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
  end;


  TXpNamedNodeMap = class(TObject)
    protected
      nmList : TList;
      fnoOwnerDocument    : TXpDocument;                               {!!.57}
      function nmCloneNamedNodeMap : TXpNamedNodeMap;
      function nmGetLength : Integer;
      procedure SetnoOwnerDocument(const Value: TXpDocument);
      property noOwnerDocument : TXpDocument read fnoOwnerDocument write SetnoOwnerDocument;
    public
      procedure Add(oNode : TXpNode);
      constructor Create;
      procedure Delete(wIndex : Integer);
      destructor Destroy; override;
      procedure Empty;
      function GetNamedItem(const sName : DOMString) : TXpNode;        {!!.57}
      function GetNamedItemNS(const sNamespaceURI,
                              sLocalName    : DOMString) : TXpNode;    {!!.57}
      function IndexOf(oNode : TXpNode) : Integer;
      procedure Insert(wIndex : Integer; oNode : TXpNode);
      function Item(wIndex : Integer) : TXpNode;
{$IFDEF NodeMemoryUsageEnabled}
      function MemoryUsed : Longint; virtual;
{$ENDIF}
      function RemoveNamedItem(const sName : DOMString) : TXpNode;     {!!.57}
      function RemoveNamedItemNS(const sNamespaceURI,                  {!!.57}
                                 sName         : DOMString) : TXpNode;
      procedure Replace(wIndex : Integer; oNode : TXpNode);
      function SetNamedItem(oNode : TXpNode) : TXpNode;

      property Length : Integer
         read nmGetLength;
  end;

  TXpAttribute = class(TXpNode)
    protected
      atSpecified    : Boolean;

      function atGetOwnerElement : TXpElement;
      function noGetStringValue : DOMString; override;                 {!!.55}

    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property Name : DOMString
         read noNodeName
         write noNodeName;
      property OwnerElement : TXpElement
         read atGetOwnerElement;
      property Specified : Boolean
         read atSpecified
         write atSpecified;
      property Value : DOMString
         read noNodeValue
         write noNodeValue;
  end;


  {forward declarations}
  TXpText = class;
  TXpCDataSection = class;

  TXpElement = class(TXpNode)
    protected
      elFullEndTag   : Boolean;
      elIgnoreEndTag : Boolean;

      function elAttribValueEquals(const sValue,
                                         sValueSought : DOMString) : Boolean;
      function elAttribValueIn(const sValue : DOMString;
                                     oValues : TStringList) : Boolean;
      function elGetElementText : DOMString;
      function elGetImmediateChildText(oNode : TXpNode) : DOMString;
      function elIsNthOccurrence(N : Integer) : Boolean;
        { Determine if the element is the Nth occurrence of that element type
          within its parent. }
//      function elIsSystemFunction(aName : DOMString) : Boolean;      {Deleted !!.52}
      procedure elSetAttributePrim(oAttr : TXpAttribute);
      procedure elStripWhitespaceNodesPrim(bPreserve : Boolean);
      function elTraverseTreeForElement(oNode : TXpNode;
                                        const sName : DOMString)       {!!.57}
                                              : TXpElement;
      procedure elTraverseTreeForElements(oList         : TXpNodeList;
                                          oNode         : TXpNode;
                                          const sName,                 {!!.57}
                                          sNamespaceURI : DOMString);
      procedure elTraverseTreeForElementsWithAttribute(oList  : TXpNodeList;
                                                       oNode  : TXpNode;
                                                       const sName,       {!!.57}
                                                       sAttr,
                                                       sValue : DOMString);
      procedure elTraverseTreeForElementsWithIDs(oList  : TXpNodeList;
                                                 oNode : TXpNode;
                                                 const sAttr : DOMString; {!!.57}
                                                 oIDs  : TStringList);
      function noGetBaseURI : DOMString; override;                     {!!.53}
    public
      function CloneNode(bDeep: Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function CreateChildCDataSection(const sText : DOMString)        {!!.57}
                                             : TXpCDataSection;
      function CreateChildElement(const sElem : DOMString) : TXpElement; {!!.57}
      function CreateChildText(const sText : DOMString) : TXpText;     {!!.57}
      function FindElement(const sName : DOMString) : TXpElement;      {!!.57}
      function GetAttribute(const sName : DOMString) : DOMString;      {!!.57}
      function GetAttributeInt(const sName : DOMString) : Integer;     {!!.52}
      function GetAttributeNode(const sName : DOMString) : TXpAttribute; {!!.57}
      function GetAttributeNodeNS(const sNamespaceURI,                 {!!.57}
                                  sLocalName   : DOMString)
                                               : TXpAttribute;
      function GetAttributeNS(const sNamespaceURI, sLocalName : DOMString) {!!.57}
                                                        : DOMString;
      function GetChildElementsByTagName(const sName: DOMString) : TXpNodeList; {!!.57}
      function GetElementsByTagName(const sName: DOMString) : TXpNodeList; {!!.57}
      function GetElementsByTagNameNS(const sNamespaceURI,             {!!.57}
                                      sLocalName     : DOMString)
                                                     : TXpNodeList;
      function GetElementsByTagNameWithAttribute(const sName,
                                                 sAttr,
                                                 sValue: DOMString) : TXpNodeList; {!!.57}
      function GetElementsWithIDs(const sAttr,
                                     sValue: DOMString) : TXpNodeList; {!!.57}
      function HasAttribute(const sName : DOMString) : Boolean;        {!!.57}
      procedure RemoveAttribute(const sName : DOMString);              {!!.57}
      function RemoveAttributeNode(oOldAttr : TXpAttribute)
                                            : TXpAttribute;
      procedure RemoveAttributeNS(const sNamespaceURI,                 {!!.57}
                                  sLocalName    : DOMString);
      procedure SetAttribute(const sName, sValue : DOMString);         {!!.57}
      function SetAttributeNode(oNewAttr : TXpAttribute)
                                         : TXpAttribute;
      function SetAttributeNodeNS(oNewAttr : TXpAttribute)
                                           : TXpAttribute;
      procedure SetAttributeNS(const sNamespaceURI,                    {!!.57}
                               sLocalName,
                               sValue       : DOMString);
      procedure StripWhitespaceNodes(bDeep : Boolean);                 {!!.53}

      property ElementText: DOMString
         read elGetElementText;
      property FullEndTag: Boolean
         read elFullEndTag
         write elFullEndTag;
      property IgnoreEndTag: Boolean
         read elIgnoreEndTag
         write elIgnoreEndTag;
      property TagName: DOMString
         read noNodeName
         write noNodeName;
  end;


  TXpText = class(TXpCharacterData)
    protected
      procedure noOutput(oMem : TMemoryStream;
                         wLevel : Integer); override;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function IsWhitespaceNode : Boolean;                             {!!.53}
      function SplitText(wOffset : Integer) : TXpText;
{Begin !!.52}
      property OutputEscaping : Boolean
        read noOutputEscaping write noOutputEscaping;
        { If True then text being output is escaped (e.g., '<'
          is output as '&lt;'). }
{End !!.52}
  end;


  TXpCDATASection = class(TXpText)
    protected
      procedure noOutput(oMem : TMemoryStream;
                         wLevel : Integer); override;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;
    end;


  TXpDTDElementContent = class(TXpNode)
    protected
      ecOccurs   : Integer;
      ecRelation : Integer;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property Name : DOMString
         read noNodeName
         write noNodeName;
      property Occurs : Integer
         read ecOccurs
         write ecOccurs;
      property Relation : Integer
         read ecRelation
         write ecRelation;
  end;


  TXpDTDElementGroup = class(TXpNode)
    protected
      egOccurs: Integer;
      egRelation: Integer;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = true
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function CreateDTDElementContent(const sName     : DOMString;    {!!.57}
                                       wOccurs,
                                       wRelation : Integer)
                                                 : TXpDTDElementContent;
      function CreateDTDElementGroup : TXpDTDElementGroup;
      destructor Destroy; override;

      property Occurs: Integer
         read egOccurs
         write egOccurs;
      property Relation: Integer
         read egRelation
         write egRelation;
  end;


  TXpDTDElement = class(TXpNode)
    protected
      elContentType : Integer;
      procedure elSetContentType(aType : Integer);
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;
      function CreateDTDElementGroup : TXpDTDElementGroup;

      property ContentType : Integer
         read elContentType
         write elSetContentType
         default CONTENT_ANY;
      property Name : DOMString
         read noNodeName
         write noNodeName;
  end;


  TXpDTDAttDefinition = class(TXpNode)
    private
      adAttType     : Integer;
      adDefaultType : Integer;
      adEnumeration : TStringList;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property AttType : Integer
        read adAttType
        write adAttType;
      property DefaultType : Integer
        read adDefaultType
        write adDefaultType;
      property DefaultValue : DOMString
         read noNodeValue
         write noNodeValue;
      property Enumeration : TStringList
        read adEnumeration;
      property Name : DOMString
         read noNodeName
         write noNodeName;
  end;


  TXpDTDAttlist = class(TXpNode)
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function CreateDTDAttDefinition(const sName        : DOMString;  {!!.57}
                                      wAttrType    : Integer;
                                      oEnumeration : TStringList;
                                      wValueType   : Integer;
                                      const sValue       : DOMString)  {!!.57}
                                                   : TXpDTDAttDefinition;
      destructor Destroy; override;

      property Name : DOMString
         read noNodeName
         write noNodeName;
  end;


  TXpDTDEntity = class(TXpNode)
    protected
      enIsPE         : Boolean;
      enNotationName : DOMString;
      enPublicID     : DOMString;
      enSystemID     : DOMString;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property IsPE : Boolean
         read enIsPE
         write enIsPE;
      property Name : DOMString
         read noNodeName
         write noNodeName;
      property NotationName : DOMString
         read enNotationName
         write enNotationName;
      property PublicID : DOMString
         read enPublicID
         write enPublicID;
      property SystemID : DOMString
         read enSystemID
         write enSystemID;
      property Value : DOMString
         read noNodeValue
         write noNodeValue;
  end;

  TXpDTDNotation = class(TXpNode)
    protected
      noPublicID : DOMString;
      noSystemID : DOMString;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
         {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property Name : DOMString
         read noNodeName
         write noNodeName;
      property PublicID : DOMString
        read noPublicID
        write noPublicID;
      property SystemID : DOMString
        read noSystemID
        write noSystemID;
  end;

  {forward declaration}
  TXpDTDConditional = class;


  TXpDocumentType = class(TXpNode)
    protected
      dtExternalDTD   : TXpDocumentType;
      dtIsExternalDTD : Boolean;
      dtPublicID      : DOMString;
      dtSystemID      : DOMString;

      function noGetNodesByNodeTypeName(wType : Integer;                            {!!.53}
                                  const sName : DOMString) : TXpNodeList; override; {!!.53}

    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function CreateDTDAttlist(const sName : DOMString) : TXpDTDAttlist; {!!.57}
      function CreateDTDConditional(wType : Integer) : TXpDTDConditional;
      function CreateDTDElement(sName        : DOMString;
                                wContentType : Integer) : TXpDTDElement;
      function CreateDTDEntity(const sName : DOMString) : TXpDTDEntity; {!!.57}
      function CreateDTDNotation(const sName : DOMString) : TXpDTDNotation; {!!.57}
      destructor Destroy; override;

      property ExternalDTD : TXpDocumentType
        read dtExternalDTD
        write dtExternalDTD;
      property IsExternalDTD : Boolean
        read dtIsExternalDTD;
      property Name : DOMString
         read noNodeName
         write noNodeName;
      property PublicID : DOMString
        read dtPublicID
        write dtPublicID;
      property SystemID : DOMString
        read dtSystemID
        write dtSystemID;
  end;


  TXpDTDConditional = class(TXpDocumentType)
    protected
      dcCondType : Integer;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      destructor Destroy; override;

      property CondType : Integer
        read dcCondType
        write dcCondType;
  end;

{Begin !!.52}
  TXpElementFactory = class
  protected
    FID : DOMString;
  public
    function CreateElement(const sName : DOMString) : TXpElement; virtual;

    procedure Initialize; virtual;
      { Called when factory is instantiated. }

    class procedure Register(const sID : DOMString); virtual;
      { Creates an instance of this object and adds it to the list of
        registered factories. }

    class procedure Unregister;
      { Removes all instances of this class type from the list of
        registered factories. }

    class function FindFactory(const sID : DOMString) : TXpElementFactory;
      { Searchs the list of registered factories for a factory that matches
        the specified ID. }

    property ID : DOMString
      read FID;

  end;

  TXpElementFactories = class of TXpElementFactory;
{End !!.52}

{Begin !!.55}
  TXpLineBreakMode = (lbmDefault, lbmCR, lbmCRLF, lbmLF);
    { Controls translation of linefeeds when saving document.
      lbmDefault - Convert #xA to linebreak based upon operating system
      lbmCR - Convert #xA to #xD
      lbmCRLF - Convert #xA to #xD#xA
      lbmLF - Leave #xA as is }
{End !!.55}

  { TXpDocument }
  TXpDocument = class(TXpNode)
    protected                                                          {!!.52}
      docActualCDATA         : Boolean;
      docBaseURI             : DOMString;                              {!!.53}
      docDomImplementation   : TXpDomImplementation;
      docElementFactory      : TXpElementFactory;                      {!!.52}
      docFormattedOutput     : Boolean;
      docIdAttribute         : DOMString;
      docIgnoreCase          : Boolean;
      docLineBreakCharReplace : Boolean;                               {!!.55}
      docLineBreakMode       : TXpLineBreakMode;                       {!!.55}
      docNoCharReplacement   : Boolean;
      docOnCurrentNode       : TXpCurrentNodeEvent;                    {!!.52}
      docOnElementAvailable  : TXpElementAvailableEvent;               {!!.53}
      docOnFormatNumber      : TXpFormatNumberEvent;                   {!!.52}
      docOnFunction          : TXpFunctionEvent;
      docOnFunctionAvailable : TXpFunctionAvailableEvent;
      docOnKeyLookup         : TXpKeyLookupEvent;                      {!!.52}
      docOnOutputAttrib      : TXpNodeCancelEvent;                     {!!.52}
      docOnOutputAttribEnd   : TXpNodeEvent;                           {!!.52}
      docOnOutputAttribStart : TXpNodeEvent;                           {!!.52}
      docOnOutputElementEnd  : TXpNodeEvent;                           {!!.52}
      docOnOutputElementStart : TXpNodeEvent;                          {!!.52}
      docOnResolveDocument   : TXpResolveDocumentEvent;                {!!.53}
      docOnSystemProperty    : TXpSystemLookupEvent;                   {!!.52}
      docOnVariableLookup    : TXpVariableLookupEvent;
      docXPathParser         : TXpXPathParser;                         {!!.52}

      function docGetDocType : TXpDocumentType;
      function docGetDocumentElement : TXpElement;
      function docGetXpDTD : DOMString;
{Begin !!.55}
      function docLineBreakChars : DOMString;
        { Returns the character(s) specific to the current line break mode. }
      function docMustReplaceLineBreaks : Boolean;
        { Returns True if line break mode is different than what is native
          to operating system. }
{End !!.55}
      procedure docTraverseTreeForElements(oList         : TXpNodeList;
                                           oNode         : TXpNode;
                                           const sName,                {!!.57}
                                           sNamespaceURI : DOMString);
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      function CreateAttribute(const sName : DOMString) : TXpAttribute;{!!.52}
      function CreateAttributeNS(const sNamespaceURI,                  {!!.57}
                                 sQualifiedName : DOMString)
                                                : TXpAttribute;
      function CreateCDATASection(const sData : DOMString              {!!.57}
        {$IFNDEF VER100}
        = ''
        {$ENDIF}
        ) : TXpCDATASection;
      function CreateComment(const sData : DOMString                   {!!.57}
        {$IFNDEF VER100}
        = ''
        {$ENDIF}
        ) : TXpComment;
      function CreateDocumentFragment : TXpDocumentFragment;
      function CreateDocumentType(const sName     : DOMString;         {!!.57}
                                  bExternal : Boolean
                                  {$IFNDEF VER100}
                                  = False
                                  {$ENDIF}
                                  ) : TXpDocumentType;
{Begin !!.52}
      function CreateElement(const sTagName : DOMString) : TXpElement;
      function CreateElementNS(const sNamespaceURI,
                                     sQualifiedName : DOMString) : TXpElement;
{End !!.52}
      function CreateEntityReference(const sName : DOMString)          {!!.57}
                                           : TXpEntityReference;
      function CreateProcessingInstruction(const sTarget : DOMString;  {!!.57}
                                           const sData   : DOMString   {!!.57}
                                           {$IFNDEF VER100}
                                           = ''
                                           {$ENDIF}
                                           ) : TXpProcessingInstruction;
      function CreateTextNode(const sData : DOMString
        {$IFNDEF VER100}
        = ''
        {$ENDIF}
        ) : TXpText;
      destructor Destroy; override;
      function GetElementsByTagName(const sName : DOMString) : TXpNodeList; {!!.57}
      function GetElementsByTagNameNS(const sNamespaceURI,             {!!.57}
                                      sLocalName    : DOMString)
                                                    : TXpNodeList;
      function ImportNode(oNode : TXpNode;
                          bDeep : Boolean) : TXpNode;
      function LocationPath : DOMString; override;                     {!!.53}

      property ActualCDATA : Boolean
         read docActualCDATA
         write docActualCDATA;
      property BaseURI : DOMString                                     {!!.53}
         read docBaseURI;                                              {!!.53}
      property DocType : TXpDocumentType
         read docGetDocType;
      property DocumentElement : TXpElement
         read docGetDocumentElement;
      property DomImplementation : TXpDomImplementation
         read docDomImplementation;
      property FormattedOutput : Boolean
         read docFormattedOutput
         write docFormattedOutput;
      property IdAttribute : DOMString
         read docIdAttribute
         write docIdAttribute;
      property IgnoreCase : Boolean
         read docIgnoreCase
         write docIgnoreCase;
      property NoCharReplacement : Boolean
         read docNoCharReplacement
         write docNoCharReplacement;
{Begin !!.55}
      property LineBreakCharReplace : Boolean
         read docLineBreakCharReplace
         write docLineBreakCharReplace
         default False;

      property LineBreakMode : TXpLineBreakMode
         read docLineBreakMode
         write docLineBreakMode
         default lbmDefault;
{End !!.55}
      property OnCurrentNode : TXpCurrentNodeEvent                     {!!.52}
         read docOnCurrentNode                                         {!!.52}
         write docOnCurrentNode;                                       {!!.52}
      property OnElementAvailable : TXpElementAvailableEvent           {!!.53}
         read docOnElementAvailable                                    {!!.53}
         write docOnElementAvailable;                                  {!!.53}
      property OnFormatNumber : TXpFormatNumberEvent                   {!!.52}
         read docOnFormatNumber                                        {!!.52}
         write docOnFormatNumber;                                      {!!.52}
      property OnFunction: TXpFunctionEvent
         read docOnFunction
         write docOnFunction;
      property OnFunctionAvailable : TXpFunctionAvailableEvent
         read docOnFunctionAvailable
         write docOnFunctionAvailable;
      property OnKeyLookup : TXpKeyLookupEvent                         {!!.52}
         read docOnKeyLookup                                           {!!.52}
         write docOnKeyLookup;                                         {!!.52}
      property OnResolveDocument : TXpResolveDocumentEvent             {!!.53}
         read docOnResolveDocument                                     {!!.53}
         write docOnResolveDocument;                                   {!!.53}
      property OnSystemProperty : TXpSystemLookupEvent                 {!!.52}
         read docOnSystemProperty
         write docOnSystemProperty;
      property OnVariableLookup : TXpVariableLookupEvent
         read docOnVariableLookup
         write docOnVariableLookup;
      property XmlDTD : DOMString
         read docGetXpDTD;
  end;

  TXpInvalidDocumentEvent = procedure(oOwner : TObject;
                                      wCode  : Integer;
                                      oNode  : TXpNode;
                                  var bStop  : Boolean) of object;
  EXpContentCode = (ccNone, ccBad, ccOk);

  TXpObjModel = class(TXpComponent)
    protected
      omAttrList      : TList;
      omDocument      : TXpDocument;
      omErrors        : TStringList;
      omInDTD         : Boolean;
      omNormalizeData : Boolean;
      omOnDTDExternal : TXpDTDExternalStartEvent;                      {!!.57}
      omOnInvalidDocument : TXpInvalidDocumentEvent;
      omOnResolveEntity   : TXpResolveEvent;
      omOutCharSet        : TXpCharEncoding;                           {!!.51}
      omParser            : TXpParser;
      omPreserveSpace     : TXpPreserveSpaceEvent;                     {!!.52}
      omRaiseErrors       : Boolean;
      omSetUTF8Sig        : Boolean;                                   {!!.51}
      omStack             : TXpNodeList;                               {!!.53}
{$IFDEF XPDPRO}                                                        {!!.52}
      omStylePIs          : TxpNodeList;                               {!!.52}
{$ENDIF}                                                               {!!.52}
      omTag               : Longint;                                   {!!.53}
      omTrackAttributes   : Boolean;
      omTrackedAttributes : TStringList;
      omTrackedElements   : TStringList;
      omTrackElements     : Boolean;

      function omCheckAttlistDefinition(oElem    : TXpElement;
                                        oAttlist : TXpDTDAttlist)
                                                 : Boolean;
      function omCheckElementAttribute(oElem    : TXpElement;
                                       oAttr    : TXpAttribute;
                                       oAttlist : TXpDTDAttlist)
                                                : Boolean;
      function omCheckElementInContent(oElem     : TXpElement;
                                       oDTDGroup : TXpDTDElementGroup)
                                                 : Boolean;
      function omCheckElementsContent(var wIndex    : Integer;
                                          oElem     : TXpElement;
                                          oDTDGroup : TXpDTDElementGroup)
                                                    : EXpContentCode;
      function omCheckElementsGroup(var wIndex    : Integer;
                                        oElem     : TXpElement;
                                        oDTDGroup : TXpDTDElementGroup;
                                        bRoot     : Boolean
                                        {$IFNDEF VER100}
                                        = False
                                        {$ENDIF}
                                        ;
                                        bDoError  : Boolean
                                        {$IFNDEF VER100}
                                        = True
                                        {$ENDIF}
                                        ) : Boolean;
      function omCheckElementsMoreChildren(const oElem  : TXpElement;  {!!.57}
                                                 wIndex : Integer)     {!!.57}
                                                        : Boolean;     {!!.57}
      function omGetBufferSize : Integer;
      function omGetDocLocation : string;                              {!!.52}
      function omGetDocName : string;                                  {!!.52}
      function omGetDocumentElement : TXpElement;
      function omGetFormattedOutput : Boolean;
      function omGetIdAttribute : DOMString;
      function omGetIgnoreCase : Boolean;
      function omGetLineBreakCharReplace : Boolean;                    {!!.55}
      function omGetLineBreakMode : TXpLineBreakMode;                  {!!.55}
      function omGetPassword : string;
      function omGetUserName : string;
      function omGetXpDocument : DOMString;
      procedure omFreeAttrList;                                        {!!.55}
      procedure omHandleErr;                                           {!!.53}
      function omLoadInContext(oContext : TXpBaseDirContext;           {!!.52}
                         const sSource : string) : Boolean;            {!!.52}
      procedure omLoadInit;                                            {!!.52}
      procedure omOnPreserveSpace(oOwner       : TObject;
                                  sElementName : DOMString;
                              var bPreserve    : Boolean);
      procedure omParseAttribute(oOwner     : TObject;
                                 sName,
                                 sValue     : DOMString;
                                 bSpecified : Boolean);
      procedure omParseCDATASection(oOwner : TObject;
                                    sValue : DOMString);
      procedure omParseCharData(oOwner : TObject;
                                sValue : DOMString);
      procedure omParseComment(oOwner : TObject;
                               sValue : DOMString);
      procedure omParseDTDAttDefinition(oOwner       : TObject;
                                        sName        : DOMString;
                                        wAttrType    : Integer;
                                        oEnumeration : TStringList;
                                        wValueType   : Integer;
                                        sValue       : DOMString);
      procedure omParseDTDAttlistEnd(oOwner: TObject);
      procedure omParseDTDAttlistStart(oOwner : TObject;
                                       sName : DOMString);
      procedure omParseDTDConditionalEnd(oOwner : TObject);
      procedure omParseDTDConditionalStart(oOwner : TObject;
                                           wType  : Integer);
      procedure omParseDTDElementContent(oOwner      : TObject;
                                         sName       : DOMString;
                                         wOccurrence,
                                         wRelation   : Integer);
      procedure omParseDTDElementEnd(oOwner : TObject;
                                     sName  : DOMString);
      procedure omParseDTDElementGroupEnd(oOwner      : TObject;
                                          wOccurrence,
                                          wRelation   : Integer);
      procedure omParseDTDElementGroupStart(oOwner : TObject);
      procedure omParseDTDElementStart(oOwner       : TObject;
                                       sName        : DOMString;
                                       wContentType : Integer);
      procedure omParseDTDEnd(oOwner : TObject);
      procedure omParseDTDEntityExternal(oOwner        : TObject;
                                         bPeFlag       : Boolean;
                                         sName,
                                         sPublicID,
                                         sSystemID,
                                         sNotationName : DOMString);
      procedure omParseDTDEntityInternal(oOwner  : TObject;
                                         bPeFlag : Boolean;
                                         sName,
                                         sValue  : DOMString);
      procedure omParseDTDExternalEnd(oOwner : TObject);
      procedure omParseDTDExternalLoaded(oOwner : TObject);            {!!.53}
      procedure omParseDTDExternalStart(oOwner : TObject;
                                    var sFile  : DOMString);
      procedure omParseDTDNotation(oOwner        : TObject;
                                   sNotationName,
                                   sPublicID,
                                   sSystemID     : DOMString);
      procedure omParseDTDStart(oOwner : TObject;
                                sDecl,
                                sId0,
                                sId1   : DOMString);
      procedure omParseEndElement(oOwner : TObject;
                                  sValue : DOMString);
      procedure omParseNonXMLEntity(oOwner        : TObject;
                                    sEntityName,
                                    sPublicID,
                                    sSystemID,
                                    sNotationName : DOMString);
      procedure omParseProcessingInstruction(oOwner : TObject;
                                             sName,
                                             sValue : DOMString);
      procedure omParseStartElement(oOwner : TObject;
                                    sValue : DOMString);
      procedure omSetBufferSize(aSize : Integer);
      procedure omSetFormattedOutput(bValue : Boolean);
      procedure omSetIdAttribute(const sAttr : DOMString);             {!!.57}
      procedure omSetIgnoreCase(bValue : Boolean);
      procedure omSetLineBreakCharReplace(bValue : Boolean);           {!!.55}
      procedure omSetLineBreakMode(Value : TXpLineBreakMode);          {!!.55}
      procedure omSetOnResolveEntity(const Value : TXpResolveEvent);
      procedure omSetPassword(const sPassword : string);               {!!.57}
      procedure omSetUserName(const sUserName : string);               {!!.57}
      function omSubValidateElements(oElem : TXpElement;
                                     oDTD  : TXpDocumentType) : Boolean;
    protected
      function omGetErrorCount : Integer;
      function omGetFilter : TXpInCharFilter;                          {!!.52}
      function omGetInCharSet : TXpCharEncoding;                       {!!.51}
      function omGetOnCurrentNode : TXpCurrentNodeEvent;               {!!.53}
      function omGetOnDTDExternal : TXpDTDExternalStartEvent;          {!!.57}
      function omGetOnElementAvailable : TXpElementAvailableEvent;     {!!.53}
      function omGetOnFormatNumber : TXpFormatNumberEvent;             {!!.53}
      function omGetOnFunction : TXpFunctionEvent;
      function omGetOnFunctionAvailable : TXpFunctionAvailableEvent;
      function omGetOnKeyLookup : TXpKeyLookupEvent;                   {!!.53}
      function omGetOnPreserveSpace : TXpPreserveSpaceEvent;
      function omGetOnResolveDocument : TXpResolveDocumentEvent;       {!!.53}
      function omGetOnSystemProperty : TXpSystemLookupEvent;           {!!.52}
      function omGetOnVariableLookup : TXpVariableLookupEvent;
      procedure omSetDocument(oDoc : TXpDocument);
      procedure omSetOnCurrentNode(oFunc : TXpCurrentNodeEvent);       {!!.53}
      procedure omSetOnDTDExternal(oFunc : TXpDTDExternalStartEvent);  {!!.57}
      procedure omSetOnElementAvailable(oFunc : TXpElementAvailableEvent); {!!.53}
      procedure omSetOnFormatNumber(oFunc : TXpFormatNumberEvent);     {!!.53}
      procedure omSetOnFunction(oFunc : TXpFunctionEvent);
      procedure omSetOnFunctionAvailable(oFunc : TXpFunctionAvailableEvent);
      procedure omSetOnKeyLookup(oFunc : TXpKeyLookupEvent);           {!!.53}
      procedure omSetOnPreserveSpace(oFunc : TXpPreserveSpaceEvent);
      procedure omSetOnResolveDocument(oFunc : TXpResolveDocumentEvent); {!!.53}
      procedure omSetOnSystemProperty(oFunc : TXpSystemLookupEvent);   {!!.52}
      procedure omSetOnVariableLookup(oFunc : TXpVariableLookupEvent);
    public
      procedure ClearDocument;
      constructor Create(oOwner : TComponent); override;
      destructor Destroy; override;
      function GetErrorMsg(wIdx : Integer) : DOMString;
      function LoadDataSource(const sSource : string) : Boolean;       {!!.57}
      function LoadMemory(var Buffer; aSize : Longint) : Boolean;
      function LoadStream(oStream : TStream) : Boolean;                {!!.52}
      function SaveToFile(sFile : string) : Boolean;
      function SaveToStream(oStream : TStream) : Boolean;              {!!.52}
      function ValidateDocument : Boolean;
      function ValidateElement(oElem : TXpElement;                     {!!.55}
                               oDTD  : TXpDocumentType) : Boolean;     {!!.55}

{Begin !!.52}
      property DocLocation : string
        read omGetDocLocation;

      property DocName : string
        read omGetDocName;
{End !!.52}
      property Document : TXpDocument
         read omDocument
         write omSetDocument;
      property DocumentElement : TXpElement
         read omGetDocumentElement;
      property ErrorCount : Integer
         read omGetErrorCount;
      property Errors : TStringList
         read omErrors;
{Begin !!.51}
      property InCharSet : TXpCharEncoding
         read omGetInCharSet;
{End !!.51}
{$IFDEF XPDPRO}                                                        {!!.52}
      property StylePIs : TXpNodeList                                  {!!.52}
         read omStylePIs;                                              {!!.52}
{$ENDIF}                                                               {!!.52}
      property TrackedAttributes : TStringList
         read omTrackedAttributes;
      property TrackedElements : TStringList
         read omTrackedElements;
      property XmlDocument : DOMString
         read omGetXpDocument;

    published

      property BufferSize : Integer
        read omGetBufferSize
        write omSetBufferSize;

      property FormattedOutput : Boolean
         read omGetFormattedOutput
         write omSetFormattedOutput
         default False;

      property IdAttribute : DOMString
         read omGetIdAttribute
         write omSetIdAttribute;

      property IgnoreCase : Boolean
         read omGetIgnoreCase
         write omSetIgnoreCase
         default False;

{Begin !!.55}
      property LineBreakCharReplace : Boolean
         read omGetLineBreakCharReplace
         write omSetLineBreakCharReplace;
         
      property LineBreakMode : TXpLineBreakMode
         read omGetLineBreakMode
         write omSetLineBreakMode;
{End !!.55}
      property NormalizeData : Boolean
         read omNormalizeData
         write omNormalizeData
         default True;

{Begin !!.53}
      property OnCurrentNode : TXpCurrentNodeEvent
         read omGetOnCurrentNode
         write omSetOnCurrentNode;

      property OnDTDExternal : TXpDTDExternalStartEvent                {!!.57}
         read omGetOnDTDExternal                                       {!!.57}
         write omSetOnDTDExternal;                                     {!!.57}

      property OnElementAvailable : TXpElementAvailableEvent
         read omGetOnElementAvailable
         write omSetOnElementAvailable;

      property OnFormatNumber : TXpFormatNumberEvent
         read omGetOnFormatNumber
         write omSetOnFormatNumber;
{End !!.53}

      property OnFunction : TXpFunctionEvent
         read omGetOnFunction
         write omSetOnFunction;

      property OnFunctionAvailable : TXpFunctionAvailableEvent
         read omGetOnFunctionAvailable
         write omSetOnFunctionAvailable;

      property OnInvalidDocument : TXpInvalidDocumentEvent
         read omOnInvalidDocument
         write omOnInvalidDocument;

{Begin !!.53}
      property OnKeyLookup : TXpKeyLookupEvent
         read omGetOnKeyLookup
         write omSetOnKeyLookup;
{End !!.53}

      property OnPreserveSpace : TXpPreserveSpaceEvent
         read omGetOnPreserveSpace
         write omSetOnPreserveSpace;

{Begin !!.53}
      property OnResolveDocument : TXpResolveDocumentEvent
         read omGetOnResolveDocument
         write omSetOnResolveDocument;
{End !!.53}

      property OnResolveEntity : TXpResolveEvent
         read omOnResolveEntity
         write omSetOnResolveEntity;

      property OnSystemProperty : TXpSystemLookupEvent                 {!!.52}
         read omGetOnSystemProperty
         write omSetOnSystemProperty;

      property OnVariableLookup : TXpVariableLookupEvent
         read omGetOnVariableLookup
         write omSetOnVariableLookup;

{Published !!.53}
      property OutCharSet : TXpCharEncoding
         read omOutCharSet write omOutCharSet;

      property Password : string
         read omGetPassword
         write omSetPassword;

      property RaiseErrors : Boolean
         read omRaiseErrors
         write omRaiseErrors
         default False;

{Begin !!.53}
      property Tag : Longint
         read omTag
         write omTag
         default 0;
{End !!.53}

      property TrackAttributes : Boolean
         read omTrackAttributes
         write omTrackAttributes
         default False;

      property TrackElements : Boolean
         read omTrackElements
         write omTrackElements
         default False;

      property UserName : string
         read omGetUserName
         write omSetUserName;

{Published !!.53}
{Begin !!.51}
      property WriteUTF8Signature : Boolean
         read omSetUTF8Sig
         write omSetUTF8Sig;
{End !!.51}
  end;


  TXpXqlToken = class(TXpNode)
    protected
      qtTokenId : Integer;
    public
      function CloneNode(bDeep : Boolean
        {$IFNDEF VER100}
        = True
        {$ENDIF}
        ) : TXpNode; override;
      constructor Create; override;                                    {!!.52}
      constructor CreateToken(wTokenId : Integer);
      constructor CreateTokenValue(wTokenId : Integer;
                                   const sValue   : DOMString);        {!!.57}

      property TokenId: Integer
         read qtTokenId
         write qtTokenId;
  end;

{Begin !!.52}
  TXpXPathParser = class
  protected
    function GetDelimitedToken(const anExpr : DOMString;
                               const anEndChar : DOMChar;
                               var   wPos : Integer) : DOMString;
    function GetDelimitedTokenF(const anExpr : DOMString;
                                      aCharTest : TXpCharTestMethod;
                                      aStopTest : TXpStopTestMethod;
                                  var wPos : Integer) : DOMString;
  public
    function Tokenize(const Expr : DOMString) : TXpNodeList;
  end;
{End !!.52}

  function IsNCChar(const ch : DOMChar) : Boolean;

{Begin !!.52}
  { Utility functions }
  function XpConvertBoolean(const sTerm           : DOMString;
                                  oElemList       : TXpNodeList;
                                  oExpressionType : TXpExpressionType) : Boolean;

  function XpConvertInteger(const sTerm           : DOMString;
                                  oElemList       : TXpNodeList;
                                  oExpressionType : TXpExpressionType) : Integer;

  function XpConvertNumber(const sTerm           : DOMString;
                                 oElemList       : TXpNodeList;
                                 oExpressionType : TXpExpressionType) : Double;

  function XpConvertString(const sTerm           : DOMString;
                                 oElemList       : TXpNodeList;
                                 oExpressionType : TXpExpressionType;
                                 bAll            : Boolean) : DOMString;

  function XpIsWhiteSpace(const ch : DOMChar) : Boolean;

const
  XpsDefaultFactoryID = 'default';
  XpsXSLFactoryID = 'xproxsl';
{End !!.52}

implementation

{Begin !!.51}
uses
{$IFDEF XpUseInet}
  XpInet,
{$ENDIF}
{$IFDEF XPTrialRun}
{$IFDEF UsingCLX}
  XpQTrial,
{$ELSE}
  XpTrial,
{$ENDIF}
{$ENDIF}
  XpHash;                                                              {!!.52}
{End !!.51}

{Begin !!.52}
var
  _Factories : TXpPointerList;
{End !!.52}

{Begin !!.57}
threadvar
  xptvReverseAxis : Boolean;
    { Used by the XPath engine while evaluating predicates. Allows the predicate
      to detect the case where the current axis is a reverse axis. }
{End !!.57}

{===Misc constants===================================================}
const
  xpsIDPrefix = 'x';
    { Used by generate-id function. First character must be alphanumeric. }

{Begin !!.52}
var
  xpoXPathFuncHash : TXpStrHash;
    { Used by TXpXPathParser to identify XPath functions. }
  xpoXPathAxisHash : TXpStrHash;
    { Used by TXpXPathParser to identify XPath axis identifiers. }
{End !!.52}

{====================================================================}

{!!.52 - System functions moved to XpBase. }

{===Global methods===================================================}
{Begin !!.52}
function XpConvertBoolean(const sTerm           : DOMString;
                              oElemList       : TXpNodeList;
                              oExpressionType : TXpExpressionType) : Boolean;
begin
  case oExpressionType of
    xpetNodeSet : Result := oElemList.Length > 0;
    xpetBoolean : Result := sTerm = xpsTrue;
    xpetNumber  : Result := StrToIntDef(sTerm, 0) <> 0;
    xpetString  : Result := Length(sTerm) > 0;
  else
    Result := False;
  end;
end;
{--------}
function XpConvertInteger(const sTerm           : DOMString;
                              oElemList       : TXpNodeList;
                              oExpressionType : TXpExpressionType) : Integer;
begin
  Result := 0;
  try
    case oExpressionType of
      xpetNodeSet :
        Result := StrToIntDef(XpConvertString(sTerm,
                                            oElemList,
                                            xpetNodeSet,
                                            False), 0);
      xpetBoolean :
        if sTerm = xpsTrue then
          Result := 1;
      xpetNumber, xpetString :
        Result := StrToIntDef(sTerm, 0);
    end;
  except
  end;
end;
{--------}
function XpConvertNumber(const sTerm           : DOMString;
                               oElemList       : TXpNodeList;
                               oExpressionType : TXpExpressionType) : Double;
var
  sTmp : DOMString;
begin
  Result := 0;
  try
    case oExpressionType of
      xpetNodeSet:
        try
          sTmp := XpConvertString(sTerm, oElemList, xpetNodeSet, False);
          Result := XpStrToFloat(sTmp);                                {!!.56}
        except
          raise EXpException.Create(Format(sStringNaN, [QuotedStr(sTmp)]));
        end;
      xpetBoolean:
        if sTerm = xpsTrue then
          Result := 1;
      xpetNumber, xpetString:
        Result := XpStrToFloat(sTerm);                                 {!!.56}
    end;
  except
  end;
end;
{--------}
function XpConvertString(const sTerm           : DOMString;
                               oElemList       : TXpNodeList;
                               oExpressionType : TXpExpressionType;
                               bAll            : Boolean) : DOMString;
var
  i : Integer;
begin
  Result := '';
  case oExpressionType of
    xpetNodeSet:
      if oElemList.Length > 0 then
        if not bAll then
          if oElemList.Item(0).NodeType = ATTRIBUTE_NODE then
            Result := oElemList.Item(0).NodeValue
          else
            Result := oElemList.Item(0).StringValue                    {!!.53}
        else for i := 0 to oElemList.Length - 1 do begin
          if oElemList.Item(i).NodeType = ATTRIBUTE_NODE then
            Result := Result + oElemList.Item(i).NodeValue
          else
            Result := Result + oElemList.Item(i).StringValue;          {!!.53}
        end;
    xpetBoolean, xpetNumber, xpetString:
      Result := sTerm;
  end;
end;
{--------}
{End !!.52}
function IsPartOfNumber(const ch : DOMChar) : Boolean;
begin
  Result := ((ch >= '0') and (ch <= '9')) or
            (ch = '-') or (ch = '.');
end;
{--------}
{Begin !!.52}
function IsDoubleColon(const aValue : DOMString; const wPos : Integer) : Boolean;
begin
  Result := (aValue[wPos] = ':') and (aValue[wPos - 1] = ':');
end;
{End !!.52}
{--------}
function IsNCChar(const ch : DOMChar) : Boolean;
begin
  {!!.57 - Rewritten}
  Result := XpValidNameChar(False, ch);
end;
{--------}
//function IsValidNCName(const name : DOMString) : Boolean;            {!!.57 - No longer used}
//var
//  Idx : Integer;
//begin
//  Result := True;
//  for Idx := 1 to Length(name) do
//    if not IsNCChar(name[Idx]) then begin
//      Result := False;
//      Exit;
//  end;
//end;
{--------}
function XpIsWhiteSpace(const ch : DOMChar) : Boolean;
begin
  Result := (ch = #9) or
            (ch = #10) or
            (ch = #13) or
            (ch = #32);
end;
{====================================================================}

{Begin !!.52}
{===TXpXPathParser===================================================}
function TXpXPathParser.GetDelimitedToken(const anExpr : DOMString;
                                          const anEndChar : DOMChar;
                                            var wPos : Integer) : DOMString;
var
  anInx : Integer;
  aLen : Integer;
begin
  { Scan to determine the length of the token. }
  aLen := 0;
  while (anExpr[wPos + aLen] <> anEndChar) do
    inc(aLen);

  { Set the length of the Result. }
  SetLength(Result, aLen);

  { Grab characters from the expression & place into Result. }
  for anInx := 1 to aLen do
    Result[anInx] := anExpr[wPos + anInx - 1];

  inc(wPos, aLen);
end;
{--------}
function TXpXPathParser.GetDelimitedTokenF(const anExpr : DOMString;
                                                 aCharTest : TXpCharTestMethod;
                                                 aStopTest : TXpStopTestMethod;
                                             var wPos : Integer) : DOMString;
var
  anInx : Integer;
  aLen : Integer;
begin
  { Scan to determine the length of the token. }
  aLen := 0;
  while aCharTest(anExpr[aLen + wPos]) do
    inc(aLen);

  { Set the length of the Result. }
  SetLength(Result, aLen);

  { Grab characters from the expression & place into Result. }
  for anInx := 1 to aLen do begin
    Result[anInx] := anExpr[anInx + wPos - 1];
    if assigned(aStopTest) and aStopTest(Result, anInx) then
      Break;
  end;

  { Do we need to reduce the length of the string? }
  if anInx < aLen then begin
    { Yes. The StopTest told us to stop before we reached the end of what
      we thought was the token. }
    SetLength(Result, anInx);
    inc(wPos, anInx);
  end
  else
    inc(wPos, aLen);

end;
{--------}
function TXpXPathParser.Tokenize(const Expr : DOMString) : TXpNodeList;
var
  i         : Integer;
  oList     : TXpNodeList;
  oToken    : TXpXqlToken;
  pTokenID  : Pointer;
  sTmp      : DOMString;
  wLen,
  wPos      : Integer;
begin
  oToken := nil;
  oList := TXpNodeList.Create;
  try
    wPos := 1;
    wLen := Length(Expr);
    while wPos <= wLen do begin
      oToken := nil;
      case Expr[wPos] of
        ' ': inc(wPos);
        '*': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateTokenValue(TOK_WILD, '*'));
             end;
        ',': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_COMMA));
             end;
        '=': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_EQ));
             end;
        '+': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_OPSUM));
             end;
        '-': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_OPDIFF));
             end;
        '(': begin
               if (oList.Length > 0) and
                  (TXpXqlToken(oList.Item(oList.Length - 1)).TokenId = TOK_ELEMENT) then
                 TXpXqlToken(oList.Item(oList.Length - 1)).TokenId := TOK_FUNCTION;
               oList.Add(TXpXqlToken.CreateToken(TOK_LPAREN));
               inc(wPos);
             end;
        ')': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_RPAREN));
             end;
        '|': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_UNION));
             end;
        '[': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_LFRAME));
             end;
        ']': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_RFRAME));
             end;
        '{': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_LBRACK));
             end;
        '}': begin
               inc(wPos);
               oList.Add(TXpXqlToken.CreateToken(TOK_RBRACK));
             end;
        '!': begin
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_SLASH);
               if (wPos <= wLen) and (Expr[wPos] = '=') then begin
                 inc(wPos);
                 oToken.TokenId := TOK_NE;
               end;
               oList.Add(oToken);
             end;
        '<': begin
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_LT);
               if (wPos <= wLen) then begin
                 if (Expr[wPos] = '=') then begin
                   inc(wPos);
                   oToken.TokenId := TOK_LE;
                 end
                 else if (Expr[wPos] = '>') then begin
                   inc(wPos);
                   oToken.TokenId := TOK_NE;
                 end;
               end;
               oList.Add(oToken);
             end;
        '>': begin
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_GT);
               if (wPos <= wLen) and (Expr[wPos] = '=') then begin
                 inc(wPos);
                 oToken.TokenId := TOK_GE;
               end;
               oList.Add(oToken);
             end;
        '@': begin
               oList.Add(TXpXqlToken.CreateToken(TOK_ATTRIBUTE));
               inc(wPos);
             end;
        '''': begin
                inc(wPos);
                sTmp := GetDelimitedToken(Expr, '''', wPos);
                inc(wPos);
                oToken := TXpXqlToken.CreateToken(TOK_STRING_TYPE);
                oToken.NodeName := sTmp;
                oList.Add(oToken);
              end;
        '"': begin
               inc(wPos);
               sTmp := GetDelimitedToken(Expr, '"', wPos);
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_STRING_TYPE);
               oToken.NodeName := sTmp;
               oList.Add(oToken);
             end;
        '~': begin
               inc(wPos);
               sTmp := GetDelimitedToken(Expr, '~', wPos);
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_STRING_TYPE);
               oToken.NodeName := sTmp;
               oList.Add(oToken);
             end;
        '.': begin
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_DOT);
               if (wPos <= wLen) and (Expr[wPos] = '.') then begin
                 inc(wPos);
                 oToken.TokenId := TOK_2DOT;
               end;
               oList.Add(oToken);
             end;
        '/': begin
               inc(wPos);
               oToken := TXpXqlToken.CreateToken(TOK_SLASH);
               if (wPos <= wLen) and (Expr[wPos] = '/') then begin
                 inc(wPos);
                 oToken.TokenId := TOK_2SLASH;
               end;
               oList.Add(oToken);
             end;
        '$': begin
               inc(wPos);
               sTmp := GetDelimitedTokenF(Expr, IsNCChar, nil, wPos);
               if (wPos > wLen) or (Expr[wPos] <> '$') then begin      {!!.55}
                 oToken := TXpXqlToken.CreateToken(TOK_VARIABLE);
                 oToken.NodeName := sTmp;
                 oList.Add(oToken);
               end else begin
                 { Must be an XQL token. }
                 inc(wPos);
                 if sTmp = 'eq' then oList.Add(TXpXqlToken.CreateToken(TOK_EQ))
                 else if sTmp = 'ne' then oList.Add(TXpXqlToken.CreateToken(TOK_NE))
                 else if sTmp = 'lt' then oList.Add(TXpXqlToken.CreateToken(TOK_LT))
                 else if sTmp = 'le' then oList.Add(TXpXqlToken.CreateToken(TOK_LE))
                 else if sTmp = 'gt' then oList.Add(TXpXqlToken.CreateToken(TOK_GT))
                 else if sTmp = 'ge' then oList.Add(TXpXqlToken.CreateToken(TOK_GE))
                 else if sTmp = 'ieq' then oList.Add(TXpXqlToken.CreateToken(TOK_CIEQ))
                 else if sTmp = 'ine' then oList.Add(TXpXqlToken.CreateToken(TOK_CINE))
                 else if sTmp = 'ilt' then oList.Add(TXpXqlToken.CreateToken(TOK_CILT))
                 else if sTmp = 'ile' then oList.Add(TXpXqlToken.CreateToken(TOK_CILE))
                 else if sTmp = 'igt' then oList.Add(TXpXqlToken.CreateToken(TOK_CIGT))
                 else if sTmp = 'ige' then oList.Add(TXpXqlToken.CreateToken(TOK_CIGE))
                 else if sTmp = 'intersect' then oList.Add(TXpXqlToken.CreateToken(TOK_INTERSECT))
                 else if sTmp = 'union' then oList.Add(TXpXqlToken.CreateToken(TOK_UNION))
                 else if sTmp = xpsNot then oList.Add(TXpXqlToken.CreateToken(TOK_NOT))
                 else if sTmp = 'and' then oList.Add(TXpXqlToken.CreateToken(TOK_AND))
                 else if sTmp = 'or' then oList.Add(TXpXqlToken.CreateToken(TOK_OR))
                 else if sTmp = 'to' then oList.Add(TXpXqlToken.CreateToken(TOK_TO))
                 else if sTmp = xpsSum then oList.Add(TXpXqlToken.CreateToken(TOK_OPSUM))
                 else if sTmp = 'diff' then oList.Add(TXpXqlToken.CreateToken(TOK_OPDIFF))
                 else if sTmp = 'mul' then oList.Add(TXpXqlToken.CreateToken(TOK_OPMUL))
                 else if sTmp = xpsConcat then oList.Add(TXpXqlToken.CreateToken(TOK_OPCONCAT))
                 else if sTmp = 'mod' then oList.Add(TXpXqlToken.CreateToken(TOK_OPMOD))
                 else if sTmp = 'div' then oList.Add(TXpXqlToken.CreateToken(TOK_OPDIV));
               end;
             end;
        '0'..'9':
             begin
               oToken := TXpXqlToken.CreateToken(TOK_NUMBER);
               sTmp := GetDelimitedTokenF(Expr, IsPartOfNumber, nil, wPos);
               oToken.NodeName := sTmp;
               oList.Add(oToken);
             end;
        'a'..'z', 'A'..'Z', '_', '€'..'ÿ':                             {!!.57}
             begin
               oToken := TXpXqlToken.CreateToken(TOK_ELEMENT);
               sTmp := GetDelimitedTokenF(Expr, IsNCChar, IsDoubleColon, wPos);
               oToken.NodeName := sTmp;
               if (wPos <= wLen) then begin
                 if (sTmp[Length(sTmp)] = ':') then begin
                   { Check for an axis. }
                   pTokenID := xpoXPathAxisHash.Get(sTmp);
                   if pTokenID <> nil then
                     oToken.TokenID := Integer(pTokenID)
                   else
                     { Invalid axis specifier. }
                     raise TXpXPathException.Create
                             (Format(sUnknownAxis, [QuotedStr(oToken.NodeName)]));
                 end
                 else if (Expr[wPos] = '(') then begin
                   { Check for a function. }
                   pTokenID := xpoXPathFuncHash.Get(sTmp);
                   if pTokenID <> nil then
                     oToken.TokenID := Integer(pTokenID)
                   else begin
                     { Not a function or axis. Might be a node test. }
                     if (sTmp = 'pi') or (sTmp = 'processing-instruction') then
                       oToken.TokenId := TOK_CMPI
                     else if sTmp = 'comment' then
                       oToken.TokenId := TOK_CMCOMMENT
                     else if sTmp = 'text' then
                       oToken.TokenId := TOK_CMTEXTNODE
                     else if sTmp = 'node' then
                       oToken.TokenId := TOK_CMNODE;
                   end
                 end
                 else begin
                   if sTmp = 'and' then
                     oToken.TokenId := TOK_AND
                   else if sTmp = 'or' then
                      oToken.TokenId := TOK_OR
                   else if sTmp = 'div' then
                      oToken.TokenId := TOK_OPDIV
                   else if sTmp = 'mod' then
                      oToken.TokenId := TOK_OPMOD
                   else if sTmp = 'quo' then
                      oToken.TokenId := TOK_OPQUO;
                 end;
               end;  { if }
               oList.Add(oToken);
             end;
      else
        inc(wPos);
      end;
    end;

    { Decrease counter }
    for i := 0 to Pred(oList.Length) do
      oList.Item(i).Release;
    Result := oList;
  except
    { If exception raised as token was being created, free the token. }
    if oToken <> nil then
      oToken.Free;
    { Decrease counter of those tokens already added to the list. }
    for i := 0 to Pred(oList.Length) do
      oList.Item(i).Release;
    oList.Free;
    raise;
  end;
end;
{====================================================================}
{End !!.52}

{===TXpDomException====================================================}
constructor TXpDomException.CreateCode(oCode : EXpExceptionCode);
begin
  inherited Create('');
  deCode := oCode;
end;
{--------}
constructor TXpDomException.CreateValue(oCode  : EXpExceptionCode;
                                        wValue : Integer);
begin
  inherited Create('');
  deCode := oCode;
  deValue := wValue;
end;
{====================================================================}

{===TXpValue=====================================================}
constructor TXpValue.Create;
begin
  inherited;
  FValueType := xpetUnknown;
end;
{--------}
destructor TXpValue.Destroy;
begin
  Clear;
  inherited;
end;
{--------}
procedure TXpValue.Clear;
begin
{Begin !!.53}
  if FValueType = xpetNodeSet then begin
    FValueNodeSet.Free;
    FValueNodeSet := nil;
  end;
{End !!.53}
end;
{--------}
function TXpValue.Clone : TXpValue;
begin
  Result := TXpValue.Create;
  try
    Result.FValueType := FValueType;
    case FValueType of
      xpetNodeSet :
        begin
          Result.FValueNodeSet := TXpNodeList.Create;
          Result.FValueNodeSet.CopyList(FValueNodeSet);
        end;
      xpetBoolean :
        Result.FValueBoolean := FValueBoolean;
      xpetNumber :
        Result.FValueNumber := FValueNumber;
      xpetString :
        Result.FValueString := FValueString;
    end;  { case }
  except
    Result.Free;
    raise;
  end;
end;
{--------}
function TXpValue.GetAsBoolean : Boolean;
begin
  Result := False;
  case FValueType of
    xpetNodeSet :
      Result := XpConvertBoolean('', FValueNodeSet, FValueType);
    xpetBoolean :
      Result := FValueBoolean;
    xpetNumber :
      Result := FValueNumber <> 0.0;
    xpetString :
      Result := Length(FValueString) > 0;
  end;
end;
{--------}
function TXpValue.GetAsNodeSet : TXpNodeList;
begin
  case FValueType of
    xpetNodeSet :
      Result := FValueNodeSet;
    else
      Result := nil;
  end;
end;
{--------}
function TXpValue.GetAsNumber : Double;
begin
  Result := 0.0;
  case FValueType of
    xpetNodeSet :
      Result := XpConvertNumber('', FValueNodeSet, FValueType);
    xpetBoolean :
      if FValueBoolean then
        Result := 1;
    xpetNumber :
      Result := FValueNumber;
    xpetString :
      begin
        try
          Result := XpStrToFloat(FValueString);                        {!!.56}
        except
          raise EXpException.Create(Format(sStringNaN, [QuotedStr(FValueString)]));
        end;
        if FValueString = xpsTrue then
          Result := 1;
     end;
  end;
end;
{--------}
function TXpValue.GetAsString : DOMString;
begin
  case FValueType of
    xpetUnknown :
      Result := '';
    xpetNodeSet :
      Result := XpConvertString('', FValueNodeSet, FValueType, True);
    xpetBoolean :
      if FValueBoolean then
        Result := xpsTrue
      else
        Result := xpsFalse;
    xpetNumber :
      Result :=  XpFloatToStr(FValueNumber);                           {!!.56}
    xpetString :
      Result := FValueString;
  end;
end;
{--------}
procedure TXpValue.SelectValue(const sExpr : DOMString;
                                     oNode : TXpNode;
                                     oContext : TXpNodeList);
begin
  oNode.Select(sExpr, oContext);
end;
{--------}
procedure TXpValue.SetBoolean(const aValue : Boolean);
begin
  Clear;
  FValueBoolean := aValue;
  FValueType := xpetBoolean;
end;
{--------}
procedure TXpValue.SetNodeSet(const aValue : TXpNodeList);
begin
  Clear;
  FValueNodeSet := TXpNodeList.Create;
  FValueNodeSet.CopyList(aValue);
  FValueType := xpetNodeSet;
end;
{--------}
procedure TXpValue.SetNumber(const aValue : Double);
begin
  Clear;
  FValueNumber := aValue;
  FValueType := xpetNumber;
end;
{--------}
procedure TXpValue.SetString(const aValue : DOMString);
begin
  Clear;
  FValueString := aValue;
  FValueType := xpetString;
end;
{====================================================================}

{===TXpDomImplementation============================================}
function TXpDomImplementation.HasFeature(const sFeature,               {!!.57}
                                         sVersion : DOMString)
                                                  : Boolean;
begin
  Result := False;
  if (Uppercase(sFeature) = 'XML') and
     ((Uppercase(sVersion) = '1.0') or
      (Uppercase(sVersion) = '2.0')) then
    Result := True
  else if (Uppercase(sFeature) = 'CORE') and
          (Uppercase(sVersion) = '2.0') then
    Result := True;
end;
{--------}
function TXpDomImplementation.CreateDocumentType(const sQualified,     {!!.57}
                                                 sPublicID,
                                                 sSystemID : DOMString)
                                                           : TXpDocumentType;
begin
  Result := TXpDocumentType.Create;
  Result.NodeName := sQualified;
  Result.PublicID := sPublicID;
  Result.SystemID := sSystemID;
  Result.noOwnerDocument := nil;
end;
{--------}
function TXpDomImplementation.CreateDocument(const sNamespaceURI,      {!!.57}
                                             sQualifiedName : DOMString;
                                             oDocumentType  : TXpDocumentType)
                                                            : TXpDocument;
var
  oElem : TXpElement;
begin
  Result := TXpDocument.Create;
  Result.noDefaultNameSpace := sNamespaceURI;
  Result.NodeName := sQualifiedName;
  if Assigned(oDocumentType) then begin
    oDocumentType.noOwnerDocument := Result;
    Result.AppendChild(oDocumentType);
  end;

  if sQualifiedName <> '' then begin                                   {!!.52}
    oElem := Result.CreateElement(sQualifiedName);
    Result.AppendChild(oElem);
    oElem.Release;
  end;                                                                 {!!.52}
end;
{====================================================================}


{===TXpNodeList=====================================================}
function TXpNodeList.nlGetLength : Integer;
begin
{Rewritten !!.52}
  if nlList = nil then
    Result := 0
  else
    Result := nlList.Count;
end;
{--------}
function TXpNodeList.nlomGetXpDocument : DOMString;
var
  Idx : Integer;
begin
  Result := '';
  for Idx := 0 to Pred(nlList.Count) do
    Result := Result + TXpNode(nlList[Idx]).XmlDocument;
end;
{--------}
procedure TXpNodeList.nlSortList(const dwSize     : Integer;
                                 const dwFirst    : Integer;
                                 const dwLast     : Integer;
                                 const sAttribute : DOMString);
var
  i     : Integer;
  j     : Integer;
  x     : Integer;
  sVal  : DomString;
  sVal2 : DOMString;
begin
  i := dwFirst;
  j := dwLast;
  x := (dwFirst + dwLast) shr 1;
  if sAttribute <> '' then begin
    sVal := '';
    if TXpNode(nlList[x]).HasAttributes then
      sVal := TXpElement(nlList[x]).GetAttribute(sAttribute);
  end else
    sVal := TXpNode(nlList[x]).Text;

  repeat
    while true do  begin
      if sAttribute <> '' then begin
        sVal2 := '';
        if TXpNode(nlList[i]).HasAttributes then
          sVal2 := TXpElement(nlList[i]).GetAttribute(sAttribute);
      end else
        sVal2 := TXpNode(nlList[i]).Text;
      if not ((sVal2 < sVal) and (i < dwLast)) then
        break;
      Inc(i);
    end;

    while true do begin
      if sAttribute <> '' then begin
        sVal2 := '';
        if TXpNode(nlList[j]).HasAttributes then
          sVal2 := TXpElement(nlList[j]).GetAttribute(sAttribute);
      end else
        sVal2 := TXpNode(nlList[j]).Text;
      if not ((sVal < sVal2) and (j > dwFirst)) then
        break;
      Dec(j);
    end;

    if i <= j then begin
      nlList.Exchange(i, j);
      Inc(i); Dec(j);
    end;
  until i > j;

  if (dwFirst < j) then
    nlSortList(dwSize, dwFirst, j, sAttribute);
  if (i < dwLast) then
    nlSortList(dwSize, i, dwLast, sAttribute);
end;
{--------}
procedure TXpNodeList.Add(oNode : TXpNode);
begin
  oNode.AddRef;
  nlList.Add(oNode);
end;
{--------}
procedure TXpNodeList.CopyList(oNodeList : TXpNodeList);
var
  Idx : Integer;
begin
  if oNodeList <> nil then                                             {!!.52}
    for Idx := 0 to Pred(oNodeList.Length) do
      Add(oNodeList.Item(Idx));
end;
{--------}
procedure TXpNodeList.CopyNamedNodeMap(oNamedNodeMap : TXpNamedNodeMap);
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(oNamedNodeMap.Length) do
    Add(oNamedNodeMap.Item(Idx));
end;
{--------}
constructor TXpNodeList.Create;
begin
  inherited Create;
  nlList := TList.Create;
  nlTag := 0;                                                          {!!.52}
end;
{--------}
procedure TXpNodeList.Delete(wIndex : Integer);
begin
  if wIndex < nlList.Count then begin
    TXpNode(nlList[wIndex]).Release;
    nlList.Delete(wIndex);
  end;
end;
{--------}
destructor TXpNodeList.Destroy;
begin
  Empty;
  nlList.Free;
  inherited Destroy;
end;
{--------}
procedure TXpNodeList.Empty;
var
  Idx : Integer;
begin
  if nlList <> nil then begin
    for Idx := 0 to Pred(nlList.Count) do
      TXpNode(nlList.Items[Idx]).Release;
    nlList.Clear;
  end;
end;
{--------}
function TXpNodeList.Exchange(wSrc, wDest : Integer) : Boolean;
begin
  Result := (wSrc < nlList.Count) and (wDest < nlList.Count);
  if not Result then
    Exit;
  nlList.Exchange(wSrc, wDest);
end;
{--------}
function TXpNodeList.IndexOf(oNode : TXpNode) : Integer;
begin
  Result := nlList.IndexOf(oNode);
end;
{--------}
procedure TXpNodeList.Insert(wIndex : Integer;
                             oNode  : TXpNode);
begin
  oNode.AddRef;
  nlList.Insert(wIndex, oNode);
end;
{--------}
function TXpNodeList.Item(wIndex : Integer) : TXpNode;
begin
  Result := nil;
  if (wIndex >= 0) and (wIndex < nlList.Count) then
    Result := nlList[wIndex];
end;
{Begin !!.52}
{--------}
function TXpNodeList.Last : TXpNode;
begin
  if nlList.Count = 0 then
    Result := nil
  else
    Result := nlList.Items[Pred(nlList.Count)];
end;
{End !!.52}
{$IFDEF NodeMemoryUsageEnabled}
{--------}
function TXpNodeList.MemoryUsed : Longint;
begin
  Result := SizeOf(nlList) + SizeOf(nlTag);
end;
{$ENDIF}
{--------}
function TXpNodeList.Move(wSrc,
                          wDest : Integer) : Boolean;
begin
  Result := (wSrc < nlList.Count) and (wDest < nlList.Count);
  if not Result then
    Exit;
  nlList.Move(wSrc, wDest);
end;
{--------}
procedure TXpNodeList.Replace(wIndex : Integer;
                              oNode  : TXpNode);
begin
  if wIndex < nlList.Count then begin
    TXpNode(nlList[wIndex]).Release;
    oNode.AddRef;
    nlList[wIndex] := oNode;
  end;
end;
{--------}
procedure TXpNodeList.Sort(const sAttribute : DOMString;               {!!.57}
                           wOrder     : Integer);
var
  i : Integer;
  j : Integer;
begin
  { Quick sort nodes }
  nlSortList(nlList.Count, 0, Pred(nlList.Count), sAttribute);

  if wOrder = 1 then begin
    { Reverse nodes }
    j := nlList.Count div 2;
    for i := 0 to Pred(J) do
      nlList.Exchange(i, nlList.Count - 2);
  end;
end;
{====================================================================}

{Begin !!.52}
{===TXpElementFactory================================================}
function TXpElementFactory.CreateElement(const sName : DOMString) : TXpElement;
begin
  Result := TXpElement.Create;
  Result.TagName := sName;
end;
{--------}
procedure TXpElementFactory.Initialize;
begin
  { Do nothing }
end;
{--------}
class procedure TXpElementFactory.Register(const sID : DOMString);
var
  oFactory : TXpElementFactory;
begin
  oFactory := Create;
  try
    oFactory.Initialize;
    _Factories.Append(oFactory);
  except
    oFactory.Free;
  end;
  oFactory.FID := sID;
end;
{--------}
class procedure TXpElementFactory.Unregister;
var
  wInx : Integer;
begin
  if _Factories = nil then
    Exit;
  { Free every instance of this class. }
  for wInx := Pred(_Factories.Count) downto 0 do
    with TXpElementFactory(_Factories.Pointers[wInx]) do
{Begin !!.55}
      if (ClassType = Self) then begin
        Free;
        _Factories.RemoveAt(wInx);
      end;
{End !!.55}
end;
{--------}
class function TXpElementFactory.FindFactory(const sID : DOMString) : TXpElementFactory;
var
  wInx : Integer;
begin
  Result := nil;
  for wInx := 0 to Pred(_Factories.Count) do
    with TXpElementFactory(_Factories.Pointers[wInx]) do
      if (FID = sID) then begin
        Result := _Factories.Pointers[wInx];
        Break;
      end;
end;
{====================================================================}

{===TXpBaseAxis======================================================}
type
  TXpBaseAxis = class
    FStartNode : TXpNode;
  public
    constructor Create(oStartNode : TXpNode); virtual;
    function NextNode(oNode : TXpNode) : TXpNode; virtual; abstract;
    property StartNode : TXpNode
      read FStartNode;
  end;
{--------}
constructor TXpBaseAxis.Create(oStartNode : TXpNode);
begin
  inherited Create;
  FStartNode := oStartNode;
end;
{====================================================================}

{===TXpPrecedingAxis=================================================}
type
  TXpPrecedingAxis = class(TXpBaseAxis)
    { This axis includes all nodes whose end tag comes before the start
      tag of the start node. This excludes the start node's ancestors. }
    FAncestor : TXpNode;
  public
    constructor Create(oStartNode : TXpNode); override;
    function NextNode(oNode : TXpNode) : TXpNode; override;
  end;
{--------}
constructor TXpPrecedingAxis.Create(oStartNode : TXpNode);
begin
  inherited Create(oStartNode);
  FAncestor := oStartNode;
end;
{--------}
function TXpPrecedingAxis.NextNode(oNode : TxpNode) : TXpNode;
begin
  Result := oNode.PreviousInDocument;
  { Is this an ancestor of the starting node? }
  if Result = FAncestor then begin
    { Yes. Get the next ancestor and skip this node. }
    FAncestor := Result.ParentNode;
    Result := Result.PreviousInDocument;
  end;
end;
{====================================================================}

{===TXpFollowingAxis=================================================}
type
  TXpFollowingAxis = class(TXpBaseAxis)
    { This axis includes all nodes whose start tags come after the end
      tag of the start node. This excludes all of the start node's
      descendants. }
  public
    function NextNode(oNode : TXpNode) : TXpNode; override;
  end;
{--------}
function TXpFollowingAxis.NextNode(oNode : TxpNode) : TXpNode;
var
  oCurNode : TXpNode;
begin
  { Is this the starting node? }
  if oNode = FStartNode then begin
    { Yes. Skip the descendants. Need to take into account the situation
      where the specified node is the last sibling. In that case, move back
      up until we find an ancestor node with a next sibling. }
    Result := oNode.NextSibling;
    oCurNode := oNode;
    while (Result = nil) and (oCurNode <> nil) do begin
      oCurNode := oCurNode.ParentNode;
      if oCurNode <> nil then
        Result := oCurNode.NextSibling;
    end;
  end
  else
    Result := oNode.NextInDocument;
end;
{====================================================================}

{===TXpTreeWalker====================================================}
type
  TXpWalkTest = function(oNode : TXpNode;
                   const args : array of const) : Boolean of object;
  TXpTreeWalker = class
  public
    procedure Gather(oAxis : TXpBaseAxis; oTest : TXpWalkTest;
               const oTestArgs : array of const;
                     oList : TXpNodeList);
      { Starting with the current node of the specified axis, walks the axis
        finding nodes that match the specified test. oTestArgs is passed to the
        function specified by parameter oTest. Each node that matches is added
        to parameter oList. }
  end;
{--------}
procedure TXpTreeWalker.Gather(oAxis : TXpBaseAxis; oTest : TXpWalkTest;
                         const oTestArgs : array of const;
                               oList : TXpNodeList);
var
  oNode : TXpNode;
begin
  if (oAxis <> nil) and (oList <> nil) then begin
    oNode := oAxis.StartNode;
    { Was a test assigned? }
    if Assigned(oTest) then
      { Yes. Add each node in the axis that passes the test. }
      while oNode <> nil do begin
        oNode := oAxis.NextNode(oNode);
        if (oNode <> nil) and oTest(oNode, oTestArgs) then
            oList.Add(oNode);
      end  { while }
    else
      { No test. Add each node in the axis. }
      while oNode <> nil do begin
        oNode := oAxis.NextNode(oNode);
        if (oNode <> nil) then
          oList.Add(oNode);
      end;  { while }
  end;  { if }
end;
{=====================================================================}
{End !!.52}

{== TXpNode ==========================================================}
procedure TXpNode.noCheckForLineBreaks(const OldText : DOMString;      {!!.56}{!!.57 - Start}
                                         var NewText : DOMString;
                                         var OldPos,
                                             NewPos : Integer);        {!!.57 - End}
var
  s,
  s2         : DOMString;
  wInx       : Integer;
  c          : DOMChar;
begin
  c := OldText[OldPos];                                                {!!.57}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  if noOwnerDocument.docMustReplaceLineBreaks then begin
    { If we are replacing line breaks and the source text contains
      #13#10 (CRLF) we are going to handle both of these characters at
      the same time so we will just delete the #10 now for this
      situation. }
    if ((noOwnerDocument.LineBreakCharReplace) and
        (c = #13) and
        (OldText[OldPos + 1] = #10)) then begin                        {!!.57 - Start}
      OldPos := OldPos + 1;
    end;                                                               {!!.57 - End}

    s := noOwnerDocument.docLineBreakChars;
    { Insert as is or get textual equivalent? }
    if (s <> XpsLineBreak) and
       noOwnerDocument.LineBreakCharReplace then begin
      { Textual equivalent. }
      s2 := '';
      for wInx := 1 to Length(s) do
        s2 := s2 + '&#' + IntToStr(Ord(s[wInx])) + ';';
      XpDOMBufferAppend(s2, NewText, NewPos);                          {!!.57 - Start}
      Inc(OldPos, 1);
    end
    else if (noOwnerDocument.LineBreakCharReplace) then begin
      { Insert as is. }
      XpDOMBufferAppend(s, NewText, NewPos);
      Inc(OldPos, 1);
    end else begin
      XpDOMBufferAppend(c, NewText, NewPos);
      Inc(OldPos, 1);
    end;
  end
  else if (s <> XpsLineBreak) and
          noOwnerDocument.LineBreakCharReplace then begin
    s := '&#' + IntToStr(Ord(c)) + ';';
    XpDOMBufferAppend(s, NewText, NewPos);
    Inc(OldPos, 1);
  end else begin
    XpDOMBufferAppend(s, NewText, NewPos);
    Inc(OldPos, 1);
  end;                                                                 {!!.57 - End}
end;
{--------}
procedure TXpNode.noCheckToken(wToken      : Integer;
                               oTokenList  : TXpNodeList;
                           var wTokenIndex : Integer);
begin
  Inc(wTokenIndex);
  if wTokenIndex >= oTokenList.Length then
    raise TXpDomException.CreateValue(ecExpectedToken, wToken);

  if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId <> wToken then
    raise TXpDomException.CreateValue(ecExpectedToken, wToken);
end;
{--------}
procedure TXpNode.noFindMatchingRFRAME(oTokenList  : TXpNodeList;
                                   var wTokenIndex : Integer);
var
  aCount : Integer;
  aLevel : Integer;
begin
  aCount := Pred(oTokenList.Length);                                   {!!.55}
  aLevel := 0;
  while wTokenIndex < aCount do begin
    inc(wTokenIndex);
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenID of
      TOK_RFRAME :
        if aLevel = 0 then
          Exit
        else
          dec(aLevel);
      TOK_LFRAME :
        inc(aLevel);
    end;  { case }
  end;  { while }

  { Could not find matching ']'. }
  raise TXpDomException.CreateValue(ecExpectedToken, TOK_RFRAME);
end;
{Begin !!.53}
{--------}
procedure TXpNode.noFindMatchingRPAREN(oTokenList  : TXpNodeList;
                                   var wTokenIndex : Integer);
var
  aCount : Integer;
  aLevel : Integer;
begin
  aCount := Pred(oTokenList.Length);                                   {!!.55}
  aLevel := 0;
  while wTokenIndex < aCount do begin
    inc(wTokenIndex);
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenID of
      TOK_RPAREN :
        if aLevel = 0 then
          Exit
        else
          dec(aLevel);
      TOK_LPAREN :
        inc(aLevel);
    end;  { case }
  end;  { while }

  { Could not find matching ']'. }
  raise TXpDomException.CreateValue(ecExpectedToken, TOK_RFRAME);
end;
{End !!.53}
{--------}
{Modified !!.53}
function TXpNode.noGetAllChildText(oNode : TXpNode;
                             const bAddSpace : Boolean) : DOMString;
var
  sTerm : DOMString;
  i     : Integer;
begin
  Result := '';
  if Assigned(oNode) then begin
    if oNode is TXpText then
      Result := TXpText(oNode).Data;
    if oNode.HasChildNodes then begin
      for i := 0 to Pred(oNode.ChildNodes.Length) do begin
        sTerm := noGetAllChildText(oNode.ChildNodes.Item(i), bAddSpace);
        if Result = '' then
          Result := sTerm
        else if sTerm <> '' then begin
          if bAddSpace then
            Result := Result + ' ' + sTerm
          else
            Result := Result + sTerm;
        end;  { if }
      end;  { for }
    end;  { if }
  end;  { if }
end;
{Begin !!.53}
{--------}
function TXpNode.noGetBaseURI : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  { Was this node's base URI explicitly set? }
  if noBaseURI = '' then
    { No. Retrieve from owner document. }
    Result := noOwnerDocument.BaseURI
  else
    { Yes. Return it. }
    Result := noBaseURI;
end;
{--------}
{End !!.53}
{Begin !!.52}
{--------}
function TXpNode.noGetChildNodesByNodeTypeName(wType : Integer;
                                         const sName : DOMString) : TXpNodeList;
var
  oNode : TXpNode;
  i     : Integer;
begin
  Result := TXpNodeList.Create;
  oNode := FirstChild;
  while Assigned(oNode) do begin
    if wType = ATTRIBUTE_NODE then begin
      if oNode.HasAttributes then
        for i := 0 to oNode.Attributes.Length - 1 do
          if oNode.Attributes.Item(i).noNameEquals(sName) then
            Result.Add(oNode.Attributes.Item(i));
    end
    else if ((wType = 0) or (oNode.NodeType = wType)) and
            oNode.noNameEquals(sName) then
      Result.Add(oNode);
    oNode := oNode.NextSibling;
  end;
end;
{End !!.52}
{--------}
function TXpNode.noGetFirstChild : TXpNode;
begin
  Result := noChildNodes.Item(0);
end;
{--------}
function TXpNode.noGetLastChild : TXpNode;
begin
  Result := ChildNodes.Item(Pred(noChildNodes.Length));
end;
{Begin !!.52}
{--------}
function TXpNode.noGetLastDescendantOrSelf : TXpNode;
begin
  Result := LastChild;
  if Result = nil then
    Result := Self
  else
    Result := Result.LastDescendantOrSelf;
end;
{End !!.52}
{--------}
function TXpNode.noGetLevelCode : DOMString;
var
  oNode : TXpNode;
begin
  oNode := Self;
  Result := '';
  while (Assigned(oNode.ParentNode)) do begin
    if Result <> '' then
      Result := '.' + Result;
    Result := IntToStr(oNode.ParentNode.ChildNodes.IndexOf(oNode) + 1) + Result;
    oNode := oNode.ParentNode;
  end;
end;
{--------}
function TXpNode.noGetLocalName : DOMString;
var
  wPos : Integer;
begin
  Result := noNodeName;
  wPos := XpPos(':', noNodeName);
  if wPos > 0 then
    Result := XpCopy(noNodeName, Succ(wPos), Length(noNodeName));
end;
{--------}
function TXpNode.noGetNamespace : DOMString;
{Rewritten !!.52}
begin
  Result := ResolveNSPrefix(Prefix);
end;
{Begin !!.52}
{--------}
function TXpNode.noGetNextInDocument : TXpNode;
var
  oCurNode : TXpNode;
begin
  { Has a child? }
  Result := noGetFirstChild;
  if Result = nil then begin
    { No. Has a sibling? }
    Result := noGetNextSibling;
    if Result = nil then begin
      { No. Navigate up the ancestors until we find one that has a next
        sibling. }
      oCurNode := Self;
      while (Result = nil) and (oCurNode <> nil) do begin
        oCurNode := oCurNode.ParentNode;
        if oCurNode <> nil then
          Result := oCurNode.NextSibling;
      end;  { while }
    end;  { if }
  end;  { if }
end;
{End !!.52}
{--------}
function TXpNode.noGetNextSibling : TXpNode;
var
  wMyIndex : Integer;
begin
  Result := nil;
  wMyIndex := - 1;
  if Assigned(noParentNode) then
    wMyIndex := noParentNode.ChildNodes.IndexOf(self);

  if (wMyIndex >= 0) and
     (wMyIndex < Pred(ParentNode.noChildNodes.Length)) then
    Result := ParentNode.ChildNodes.Item(Succ(wMyIndex));
end;
{Begin !!.52}
{--------}
function TXpNode.noGetNodesByNodeTypeName(wType : Integer;
                                    const sName : DOMString) : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  try
    noTraverseTreeForNamedNodes(Result, FirstChild, sName, wType);
  except
    Result.Free;
    raise;
  end;
end;
{End !!.52}
{--------}
function TXpNode.noGetOwnerDocument : TXpDocument;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := noOwnerDocument;
end;
{--------}
function TXpNode.noGetPrefix : DOMString;
var
  wPos : Integer;
begin
  Result := '';
  wPos := XpPos(':', noNodeName);
  if wPos > 0 then
    Result := XpCopy(noNodeName, 1, Pred(wPos));
end;
{Begin !!.52}
{--------}
function TXpNode.noGetPreviousInDocument : TXpNode;
begin
  Result := PreviousSibling;
  if (Result = nil) then
    Result := ParentNode
  else
    Result := Result.LastDescendantOrSelf;
end;
{End !!.52}
{--------}
function TXpNode.noGetPreviousSibling : TXpNode;
var
  Idx : Integer;
begin
  Result := nil;
  Idx := - 1;
  if Assigned(noParentNode) then
    Idx := ParentNode.ChildNodes.IndexOf(self);

  if Idx > 0 then
    Result := ParentNode.ChildNodes.Item(Pred(Idx));
end;
{Begin !!.53}
{--------}
function TXpNode.noGetStringValue : DOMString;
begin
  Result := noGetAllChildText(Self, False);
end;
{End !!.53}
{--------}
function TXpNode.noGetText : DOMString;
begin
  Result := noGetAllChildText(Self, True);                             {!!.53}
end;
{--------}
function TXpNode.noNameEquals(const aName : DOMString) : boolean;
begin
  assert(Pointer(Self) <> nil);
  assert(TObject(Self) is TXpNode);
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := (noNodeName = aName) or
            (Assigned(noOwnerDocument) and
             noOwnerDocument.docIgnoreCase and
             (UpperCase(noNodeName) = UpperCase(aName)));
end;
{Begin !!.52}
{--------}
function TXpNode.noNameTest(oNode : TXpNode; const args : array of const) : Boolean;
begin
  Result := oNode.noNameEquals(args[0].VPWideChar);
end;
{--------}
procedure TXpNode.noNextRParen(oTokenList : TXpNodeList;
                           var wTokenIndex : Integer);
begin
  repeat
    Inc(wTokenIndex);
  until (TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_RPAREN) or
        (wTokenIndex >= oTokenList.Length);
  if wTokenIndex >= oTokenList.Length then
    raise TXpDomException.CreateValue(ecExpectedToken, TOK_RPAREN);
end;
{--------}
function TXpNode.noNormalize(const aValue : DOMString) : DOMString;
var
  anInx       : Integer;
  aLen        : Integer;
  aResultInx  : Integer;
  aRightBound : Integer;
  aTrimming   : Boolean;
begin
  aLen := Length(aValue);
  if aLen = 0 then
    Exit;

  { Find the right boundary. }
  aRightBound := aLen;
  while (aRightBound > 0) and XpIsWhitespace(aValue[aRightBound]) do   {!!.52}
    dec(aRightBound);
  if aRightBound = 0 then begin
    Result := '';
    Exit;
  end;

  { Obtain some space for the result. Note that we have to do this because
    Delphi 3 doesn't allow concatenation of a widechar to a widestring.
    May need to chop off trailing characters at end of scan. }
  SetLength(Result, aRightBound);

  { Now move through the string, trimming and normalizing as necessary. }
  aTrimming := True;
  anInx := 1;
  aResultInx := 0;
  while anInx <= aRightBound do begin
    { Are we trimming? }
    if aTrimming then begin
      { Yes. Is this a non-whitespace character? }
      if not XpIsWhiteSpace(aValue[anInx]) then begin                  {!!.52}
        { Yes. Copy it to the result string. Disable trimming. }
        inc(aResultInx);
        Result[aResultInx] := aValue[anInx];
        aTrimming := False;
      end;
      inc(anInx)
    end
    else begin
      inc(aResultInx);
      { No we are not trimming. Is this a whitespace character? }
      if XpIsWhiteSpace(aValue[anInx]) then begin                      {!!.52}
        { Yes. Copy a space to the result string and enable trimming. }
        Result[aResultInx] := ' ';
        aTrimming := True;
      end
      else
        { Not a whitespace character. Copy it to the result string. }
        Result[aResultInx] := aValue[anInx];
      inc(anInx);
    end;
  end;

  dec(anInx);
  if aResultInx < anInx then
    Delete(Result, aResultInx + 1, anInx - aResultInx);

end;
{End !!.52}
{--------}
function TXpNode.noomGetXpDocument : DOMString;
var
  sText    : DOMString;
  bHadText : Boolean;
  oMem     : TMemoryStream;
  oDTD     : TXpDocumentType;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  oMem := TMemoryStream.Create;
  case NodeType of
    ELEMENT_NODE:
      begin
        if (Assigned(noOwnerDocument)) and
           noOwnerDocument.docFormattedOutput and
           (oMem.Size <> 0) then begin
          sText := noOwnerDocument.docLineBreakChars;                  {!!.55}
          oMem.Write(PWideChar(sText)^, 4);                            {!!.58}
        end;
        if Assigned(noOwnerDocument.docOnOutputElementStart) then      {!!.52}
          noOwnerDocument.docOnOutputElementStart(Self);               {!!.52}
        sText := '<' + noNodeName;
        oMem.write(PWideChar(sText)^, (Length(noNodeName) + 1) * 2);   {!!.58}
        if HasAttributes then
          noOutputXpAttributes(oMem, Attributes);
        if HasChildNodes then begin
          sText := '>';
          oMem.write(PWideChar(sText)^, 2);                            {!!.58}
          bHadText := noOutputXpSiblings(oMem, Self, 1);
          sText := '';
          if not bHadText and
            (Assigned(noOwnerDocument)) and
            noOwnerDocument.docFormattedOutput then
            sText := Chr(13) + Chr(10);
          sText := sText + '</' + noNodeName + '>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end else begin
          if not TXpElement(self).IgnoreEndTag then begin
            if TXpElement(self).FullEndTag then
              sText := '></' + noNodeName + '>'
            else
              sText := '/>';
            oMem.write(PWideChar(sText)^, Length(sText) * 2);          {!!.58}
          end else begin
            sText := '>';
            oMem.write(PWideChar(sText)^, 2);                          {!!.58}
          end;
        end;
        if Assigned(noOwnerDocument.docOnOutputElementEnd) then        {!!.52}
          noOwnerDocument.docOnOutputElementEnd(Self);                 {!!.52}
      end;
    TEXT_NODE:
      begin
        noOutputXpText(oMem, noNodeValue, False);
      end;
    CDATA_SECTION_NODE:
      if noOwnerDocument.ActualCDATA then
        oMem.write(PWideChar(noNodeValue)^, Length(noNodeValue) * 2)   {!!.51}{!!.58}
      else begin
        sText := '<![CDATA[' + noNodeValue + ']]>';
        oMem.write(PWideChar(sText)^, Length(sText) * 2);              {!!.58}
      end;
    COMMENT_NODE:
      begin
        if (Assigned(noOwnerDocument)) and
           noOwnerDocument.docFormattedOutput then begin
          sText := Chr(13) + Chr(10);
          oMem.write(PWideChar(sText)^, 4);                            {!!.58}
        end;
        sText := '<!--' + noNodeValue + '-->';
        oMem.write(PWideChar(sText)^, Length(sText) * 2);              {!!.58}
      end;
    PROCESSING_INSTRUCTION_NODE:
      begin
        if (Assigned(noOwnerDocument)) and
           noOwnerDocument.docFormattedOutput and
           (oMem.Size <> 0) then begin
          sText := Chr(13) + Chr(10);
          oMem.write(PWideChar(sText)^, 4);                            {!!.58}
        end;
        sText := '<?' + noNodeName + ' ' + noNodeValue + '?>';
        oMem.write(PWideChar(sText)^, Length(sText) * 2);              {!!.58}
      end;
    DOCUMENT_TYPE_NODE:
      begin
        oDTD := TXpDocumentType(Self);
        if oDTD.IsExternalDTD then
          noOutputXpSiblings(oMem, Self, 1)
        else begin
          sText := '<!DOCTYPE ' + noNodeName + ' ';
          if Assigned(oDTD.ExternalDTD) then
            sText := sText + 'SYSTEM "' + oDTD.ExternalDTD.name + '" '
          else if (oDTD.PublicID <> '') or (oDTD.SystemID <> '') then begin
            if oDTD.PublicID <> '' then begin
              sText := sText + 'PUBLIC "' + oDTD.PublicID + '"';
              if oDTD.SystemID <> '' then
                sText := sText + ' "' + oDTD.SystemID + '"';
            end else if oDTD.SystemID <> '' then
              sText := sText + 'SYSTEM "' + oDTD.SystemID + '"';
          end;
{Begin !!.53}
          if HasChildNodes then begin
            sText := sText + '[';
            oMem.write(PWideChar(sText)^, Length(sText) * 2);          {!!.58}
            noOutputXpSiblings(oMem, Self, 1);
            sText := ']';
          end;
          sText := sText + '>';
{End !!.53}
        end;
        oMem.write(PWideChar(sText)^, Length(sText) * 2);              {!!.58}
      end;
    XPATH_TOKEN :                                                      {!!.52}
      noOutputXpText(oMem, noNodeValue, False);                        {!!.52}
  end;

  sText := Chr(0);
  oMem.write(PWideChar(sText)^, 2);                                    {!!.58}
  oMem.Position := 0;
  Result := DOMString(PWideChar(oMem.Memory));
  oMem.Free;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.noOutputDTDAttDefinitions(oNode : TXpNode) : DOMString;
var
  oNewNode : TXpNode;
  oAtt     : TXpDTDAttDefinition;
  i        : Integer;
begin
  Result := '';
  oNewNode := oNode.noGetFirstChild;
  while Assigned(oNewNode) do begin
    case oNewNode.NodeType of
      ATT_DEF_DECL_NODE:
        begin
          oAtt := TXpDTDAttDefinition(oNewNode);
          Result := Result + ' ' + oAtt.name;
          case oAtt.AttType of
            ATTRIBUTE_CDATA    : Result := Result + ' CDATA';
            ATTRIBUTE_ID       : Result := Result + ' ID';
            ATTRIBUTE_IDREF    : Result := Result + ' IDREF';
            ATTRIBUTE_IDREFS   : Result := Result + ' IDREFS';
            ATTRIBUTE_ENTITY   : Result := Result + ' ENTITY';
            ATTRIBUTE_ENTITIES : Result := Result + ' ENTITIES';
            ATTRIBUTE_NMTOKEN  : Result := Result + ' NMTOKEN';
            ATTRIBUTE_NMTOKENS : Result := Result + ' NMTOKENS';
            ATTRIBUTE_ENUMERATED:
              begin
                Result := Result + ' (';
                for i := 0 to oAtt.Enumeration.Count - 1 do begin
                  if i > 0 then
                    Result := Result + ' | ';
                  Result := Result + oAtt.Enumeration[i];
                end;
                Result := Result + ')';
              end;
            ATTRIBUTE_NOTATION:
              begin
                Result := Result + ' NOTATION (';
                for i := 0 to oAtt.Enumeration.Count - 1 do begin
                  if i > 0 then
                    Result := Result + ' | ';
                  Result := Result + oAtt.Enumeration[i];
                end;
                Result := Result + ')';
              end;

          end;

          case oAtt.DefaultType of
            ATTRIBUTE_DEFAULT_IMPLIED  : Result := Result + ' #IMPLIED';
            ATTRIBUTE_DEFAULT_REQUIRED : Result := Result + ' #REQUIRED';
            ATTRIBUTE_DEFAULT_FIXED    : Result := Result + ' #FIXED';
          end;

          if oAtt.DefaultValue <> '' then
            Result := Result + ' "' + oAtt.DefaultValue + '"';
        end;
    end;
    oNewNode := oNewNode.noGetNextSibling;
  end;
end;
{--------}
function TXpNode.noOutputDTDElementContent(oNode : TXpNode) : DOMString;
var
  oNewNode : TXpNode;
begin
  Result := '';
  oNewNode := oNode.noGetFirstChild;
                                                                       {!!.55 - Start}
  { Mark the element node as PCDATA if it is mixed and doesn't have
    children. }
  if ((oNewNode = nil) and
      (oNode is TXpDTDElement) and
      (TXpDTDElement(oNode).ContentType = CONTENT_MIXED)) then
    Result := '(#PCDATA)'
  else
    while Assigned(oNewNode) do begin
      case oNewNode.NodeType of
        ELEMENT_DECL_GROUP_NODE:
          begin
            Result := Result + '(' + noOutputDTDElementContent(oNewNode) + ')';
            case TXpDTDElementGroup(oNewNode).Occurs of
              OCCURS_OPT_REPEAT   : Result := Result + '*';
              OCCURS_REQ_REPEAT   : Result := Result + '+';
              OCCURS_OPT_NOREPEAT : Result := Result + '?';
            end;
            if (oNewNode.noGetNextSibling <> nil) then
              case TXpDTDElementGroup(oNewNode).Relation of
                REL_OR: Result := Result + ' | ';
                REL_AND: Result := Result + ', ';
              end;
          end;
        ELEMENT_DECL_CONTENT_NODE:
          begin
            Result := Result + oNewNode.NodeName;
            case TXpDTDElementContent(oNewNode).Occurs of
              OCCURS_OPT_REPEAT: Result := Result + '*';
              OCCURS_REQ_REPEAT: Result := Result + '+';
              OCCURS_OPT_NOREPEAT: Result := Result + '?';
            end;
            if (oNewNode.noGetNextSibling <> nil) then
              case TXpDTDElementContent(oNewNode).Relation of
                REL_OR: Result := Result + ' | ';
                REL_AND: Result := Result + ', ';
              end;
          end;
      end;
      oNewNode := oNewNode.noGetNextSibling;
    end;                                                               {!!.55 - End}
end;
{--------}
procedure TXpNode.noOutputXpAttributes(oMem   : TMemoryStream;
                                       oAttrs : TXpNamedNodeMap);
{Begin !!.52}
var
  bCancel : Boolean;
  i     : Integer;
  oAttr : TXpAttribute;
  sText : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  if Assigned(noOwnerDocument.docOnOutputAttribStart) then
    noOwnerDocument.docOnOutputAttribStart(Self);
  for i := 0 to Pred(oAttrs.Length) do begin
    bCancel := False;                                                  {!!.53}
    oAttr := TXpAttribute(oAttrs.Item(i));
    if Assigned(noOwnerDocument.docOnOutputAttrib) then
      noOwnerDocument.docOnOutputAttrib(oAttr, bCancel);
    if not bCancel then begin
      sText := ' ' + oAttr.name + '="';
      oMem.Write(PWideChar(sText)^, Length(sText) * 2);                {!!.58}
      noOutputXpText(oMem, oAttr.Value, True);
      sText := '"';
      oMem.Write(PWideChar(sText)^, 2);                                {!!.58}
    end;
  end;
  if Assigned(noOwnerDocument.docOnOutputAttribEnd) then
    noOwnerDocument.docOnOutputAttribEnd(Self);
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
{End !!.52}
end;
{Begin !!.52}
{--------}
procedure TXpNode.noOutputIndent(oMem : TMemoryStream;
                           const wLevel : Integer);
var
  i : Integer;
  sText : DOMString;
begin
  sText := Chr(13) + Chr(10);
  for i := 1 to wLevel do
    sText := sText + '  ';
  oMem.write(PWideChar(sText)^, Length(sText) * 2);                    {!!.58}
end;
{End !!.52}
{--------}
function TXpNode.noOutputXpSiblings(oMem   : TMemoryStream;
                                    oNode  : TXpNode;
                              const wLevel : Integer) : Boolean;
var
  oNewNode    : TXpNode;
  oElemNode   : TXpElement;
  oDTDElement : TXpDTDElement;
  oEntity     : TXpDTDEntity;
  oNotation   : TXpDTDNotation;
  bHadText    : Boolean;
//  bFirstChild : Boolean;                                             {!!.52}{Deleted !!.53}
  bLastWasText : Boolean;                                              {!!.52}
  sText       : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := False;
  oNewNode := oNode.noGetFirstChild;
//  bFirstChild := True;                                               {!!.52}{Deleted !!.53}
  bLastWasText := False;                                               {!!.52}
  while Assigned(oNewNode) do begin
    case oNewNode.NodeType of
      ELEMENT_NODE:
        begin
          oElemNode := TXpElement(oNewNode);
          sText := '';
          if (Assigned(noOwnerDocument)) and
             noOwnerDocument.docFormattedOutput
             {and not Result} then                                     {!!.52}
            noOutputIndent(oMem, wLevel);                              {!!.52}
          if Assigned(noOwnerDocument.docOnOutputElementStart) then    {!!.52}
            noOwnerDocument.docOnOutputElementStart(oElemNode);        {!!.52}
          sText := sText + '<' + oElemNode.TagName;
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
          if oElemNode.HasAttributes then
            noOutputXpAttributes(oMem, oElemNode.Attributes);
          if oNewNode.HasChildNodes then begin
            sText := '>';
            oMem.write(PWideChar(sText)^, 2);                          {!!.58}
            bHadText := noOutputXpSiblings(oMem, oNewNode, wLevel + 1);
            sText := '';
            if not bHadText and (Assigned(noOwnerDocument)) and
               noOwnerDocument.docFormattedOutput then begin
              noOutputIndent(oMem, wLevel);                            {!!.52}
            end;
            sText := sText + '</' + oElemNode.TagName + '>';
            oMem.write(PWideChar(sText)^, Length(sText) * 2);          {!!.58}
          end else
            if not oElemNode.IgnoreEndTag then
              if oElemNode.FullEndTag then begin
                sText := '></' + oElemNode.noNodeName + '>';
                oMem.write(PWideChar(sText)^, Length(sText) * 2);      {!!.58}
              end else begin
                sText := '/>';
                oMem.write(PWideChar(sText)^, 4);                      {!!.58}
              end
            else begin
              sText := '>';
              oMem.write(PWideChar(sText)^, 2);                        {!!.58}
            end;
          if Assigned(noOwnerDocument.docOnOutputElementEnd) then      {!!.52}
            noOwnerDocument.docOnOutputElementEnd(oElemNode);          {!!.52}
        end;
      TEXT_NODE:
{Rewritten !!.53}
        oNewNode.noOutput(oMem, wLevel);
      CDATA_SECTION_NODE:
{Rewritten !!.53}
        oNewNode.noOutput(oMem, wLevel);
      COMMENT_NODE:
{Rewritten !!.53}
        oNewNode.noOutput(oMem, wLevel);
      PROCESSING_INSTRUCTION_NODE:
        begin
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then                                       {!!.52}
            noOutputIndent(oMem, wLevel);                              {!!.52}
          sText := sText + '<?' + oNewNode.NodeName + ' ' + oNewNode.NodeValue + '?>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
      ELEMENT_DECL_NODE:
        begin
          oDTDElement := TXpDTDElement(oNewNode);
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10) + '  ';
          sText := sText + '<!ELEMENT ' + oDTDElement.name + ' ';
          case oDTDElement.ContentType of
            CONTENT_ANY      :
              sText := sText + 'ANY';
            CONTENT_EMPTY    :
              sText := sText + 'EMPTY';
            CONTENT_MIXED    :
              sText := sText + noOutputDTDElementContent(oDTDElement);
            CONTENT_ELEMENTS :
              sText := sText + noOutputDTDElementContent(oDTDElement);
          end;
          sText := sText + '>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
      ATTLIST_DECL_NODE:
        begin
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10) + '  ';
          sText := sText + '<!ATTLIST ' + oNewNode.NodeName;
          sText := sText + noOutputDTDAttDefinitions(oNewNode);
          sText := sText + '>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
      ENTITY_DECL_NODE:
        begin
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10) + '  ';
          oEntity := TXpDTDEntity(oNewNode);
          sText := sText + '<!ENTITY ';
          if oEntity.IsPE then
            sText := sText + '% ';
          sText := sText + oEntity.name;
          if oEntity.Value <> '' then begin
            sText := sText + ' "';
            oMem.write(PWideChar(sText)^, Length(sText) * 2);          {!!.58}
            noOutputXpText(oMem, oEntity.Value, True);
            sText := '"';
          end else begin
            { public or system}
            if oEntity.PublicID <> '' then begin
              sText := sText + ' PUBLIC "' + oEntity.PublicID + '"';
              if oEntity.SystemID <> '' then
                sText := sText + ' "' + oEntity.SystemID + '"';

            end else if oEntity.SystemID <> '' then
              sText := sText + ' SYSTEM "' + oEntity.SystemID + '"';
            if oEntity.NotationName <> '' then
              sText := sText + ' NDATA ' + oEntity.NotationName;
          end;
          sText := sText + '>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
      NOTATION_DECL_NODE:
        begin
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10) + '  ';
          oNotation := TXpDTDNotation(oNewNode);
          sText := sText + '<!NOTATION ' + oNotation.name;
          { public or system}
          if oNotation.PublicID <> '' then begin
            sText := sText + ' PUBLIC "' + oNotation.PublicID + '"';
            if oNotation.SystemID <> '' then
              sText := sText + ' "' + oNotation.SystemID + '"';
          end else if oNotation.SystemID <> '' then
              sText := sText + ' SYSTEM "' + oNotation.SystemID + '"';
          sText := sText + '>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
      CONDITIONAL_DECL_NODE:
        begin
          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10);
          if TXpDTDConditional(oNewNode).CondType = CONDITIONAL_INCLUDE then
            sText := sText + '<![INCLUDE['
          else
            sText := sText + '<![IGNORE[';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}

          if oNewNode.HasChildNodes then
            noOutputXpSiblings(oMem, oNewNode, wLevel + 1);

          sText := '';
          if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput
             and not Result then
            sText := Chr(13) + Chr(10);
          sText := sText + ']]>';
          oMem.write(PWideChar(sText)^, Length(sText) * 2);            {!!.58}
        end;
    end;

    bLastWasText := (oNewNode.NodeType = TEXT_NODE);                   {!!.52}

    oNewNode := oNewNode.noGetNextSibling;
//    bFirstChild := False;                                            {!!.52}{Deleted !!.53}
  end;
  Result := bLastWasText;                                              {!!.52}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
procedure TXpNode.noOutputXpText(oMem       : TMemoryStream;
                           const sText      : DOMString;
                                 bAttribute : Boolean);
{Rewritten !!.57}
var
  NewLen,
  BufLen,
  TxtLen,
  x       : Integer;
  c       : DOMChar;
  s       : DOMString;
  Handled : Boolean;
  NewText : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  x := 1;
  NewLen := 0;
  TxtLen := Length(sText);
  BufLen := TxtLen * 2;
  SetLength(NewText, BufLen);
  if noOutputEscaping then begin
    while (x <= TxtLen) do begin
      Handled := False;
      c := sText[x];
      case c of
        '&':
          begin
            XpDOMBufferAppend('&amp;', NewText, NewLen);
            x := x + 1;
            Handled := True;
          end;
        '<':
          begin
            XpDOMBufferAppend('&lt;', NewText, NewLen);
            x := x + 1;
            Handled := True;
          end;
        '"':
          if bAttribute then begin
            XpDOMBufferAppend('&#34;', NewText, NewLen);
            x := x + 1;
            Handled := True;
          end;
        #10 :
          begin
            if bAttribute then begin
              x := x + 1;
              Handled := True;
              XpDOMBufferAppend('&#xA;', NewText, NewLen);
  { Cases:
  MSWindows output - #xA, #xD translated to text equivalent
  Linux output - #xA should be left as is
  }
            end
            else begin
              { See if linebreak needs to be replaced with other characters. }
              Handled := True;
              {!!.56 - Old section of code moved to noCheckForLineBreaks}
              noCheckForLineBreaks(sText, NewText, x, NewLen);
            end;
          end;
        #13 :
          begin
            Handled := True;
            noCheckForLineBreaks(sText, NewText, x, NewLen);
          end;
      end;

      if not Handled then begin
        if ((c <> #9) and ((c < #32) or (c > #126))) and
           (not noOwnerDocument.NoCharReplacement) then begin
          s := '&#' + IntToStr(Ord(c)) + ';';
          XpDOMBufferAppend(s, NewText, NewLen);
        end else begin
          XpDOMBufferAppend(c, NewText, NewLen);
        end;
        x := x + 1;
      end;
    end;  { while }
    oMem.Write(PWideChar(NewText)^, NewLen * 2);                       {!!.58}
  end
  else
    oMem.Write(PWideChar(sText)^, TxtLen * 2);                         {!!.58}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.noParseAdditiveExpr(var oElemList       : TXpNodeList;
                                         oTokenList      : TXpNodeList;
                                     var wTokenIndex     : Integer;
                                     var oExpressionType : TXpExpressionType;
                                         oCurrentNode    : TXpNode)
                                                         : DOMString;
var
  sTmp             : DOMString;
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
begin
  Result := noParseMultiplicativeExpr(oElemList, oTokenList, wTokenIndex,
                                      oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_OPCONCAT:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseMultiplicativeExpr(oNewList, oTokenList, wTokenIndex,
                                            oExpressionType2, oCurrentNode);
          Result := XpConvertString(Result, oElemList, oExpressionType, False) +
                    XpConvertString(sTmp, oNewList, oExpressionType2, False);
          oNewList.Free;
          oExpressionType := xpetString;
        end;
      TOK_OPSUM:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseMultiplicativeExpr(oNewList, oTokenList, wTokenIndex,
                                            oExpressionType2, oCurrentNode);
          Result := XpFloatToStr(XpConvertNumber(Result,               {!!.56}
                                                 oElemList,
                                                 oExpressionType) +
                                 XpConvertNumber(sTmp,
                                                 oNewList,
                                                 oExpressionType2));
          oNewList.Free;
          oExpressionType := xpetNumber;
        end;
      TOK_OPDIFF:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseMultiplicativeExpr(oNewList, oTokenList, wTokenIndex,
                                            oExpressionType2, oCurrentNode);
          Result := XpFloatToStr(XpConvertNumber(Result,               {!!.56}
                                                 oElemList,
                                                 oExpressionType) -
                                 XpConvertNumber(sTmp,
                                                 oNewList,
                                                 oExpressionType2));
          oNewList.Free;
          oExpressionType := xpetNumber;
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TXpNode.noParseAndExpr(var oElemList       : TXpNodeList;
                                    oTokenList      : TXpNodeList;
                                var wTokenIndex     : Integer;
                                var oExpressionType : TXpExpressionType;
                                    oCurrentNode    : TXpNode)
                                                    : DOMString;
var
  sTmp             : DOMString;
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
begin
  Result := noParseEqualityExpr(oElemList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_AND:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseEqualityExpr(oNewList, oTokenList, wTokenIndex,
                                      oExpressionType2, oCurrentNode);
          if XpConvertBoolean(Result, oElemList, oExpressionType) and
             XpConvertBoolean(sTmp, oNewList, oExpressionType2) then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TXpNode.noParseEqualityExpr(var oElemList       : TXpNodeList;
                                         oTokenList      : TXpNodeList;
                                     var wTokenIndex     : Integer;
                                     var oExpressionType : TXpExpressionType;
                                         oCurrentNode    : TXpNode)
                                                         : DOMString;
var
  sTmp,
  s1,
  s2               : DOMString;
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
  i                : Integer;
begin
  Result := noParseRelationalExpr(oElemList, oTokenList, wTokenIndex,
                                  oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_EQ:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseRelationalExpr(oNewList, oTokenList, wTokenIndex,
                                        oExpressionType2, oCurrentNode);
          s1 := XpConvertString(Result, oElemList, oExpressionType, true);
          s2 := XpConvertString(sTmp, oNewList, oExpressionType2, true);
          Result := xpsFalse;
          if (oExpressionType = xpetNodeSet) and
             (oExpressionType2 = xpetNodeSet) then begin
            if (oElemList.Length > 0) and (oNewList.Length > 0) and
               (oNewList.Length = oElemList.Length) then begin
              Result := xpsTrue;
              for i := 0 to oElemList.Length - 1 do
                if oNewList.IndexOf(oElemList.Item(i)) < 0 then begin
                  Result := xpsFalse;
                  break;
                end;
            end;
            if (Result = xpsFalse) and
               not ((s1 = '') and
               (s2 = '')) then begin
              if (s1 = s2) then
                Result := xpsTrue;
              if (Result = xpsFalse) and
                 (oElemList.Length > 1) then
                if XpPos(s2, s1) > 0 then
                  Result := xpsTrue;
              if (Result = xpsFalse) and
                 (oNewList.Length > 1) then
                if XpPos(s1, s2) > 0 then
                  Result := xpsTrue;
            end;
          end else begin
            if s1 = s2 then
              Result := xpsTrue;
            if (Result = xpsFalse) and
               (oExpressionType = xpetNodeSet) and
{Begin !!.57}
               (oElemList.Length > 1) then begin
//              if XpPos(s2, s1) > 0 then
//                Result := xpsTrue;
              for i := 0 to Pred(oElemList.Length) do begin
                if oElemList.Item(i).StringValue = s2 then begin
                  Result := xpsTrue;
                  Break;
                end;
              end;
            end;
{End !!.57}
            if (Result = xpsFalse) and
               (oExpressionType2 = xpetNodeSet) and
               (oNewList.Length > 1) then
              if XpPos(s1, s2) > 0 then
                Result := xpsTrue;
          end;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      TOK_NE:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseRelationalExpr(oNewList, oTokenList, wTokenIndex,
                                        oExpressionType2, oCurrentNode);
          s1 := XpConvertString(Result, oElemList, oExpressionType, True);
          s2 := XpConvertString(sTmp, oNewList, oExpressionType2, True);
          if s1 <> s2 then
            Result := xpsTrue
          else
            Result := xpsFalse;
            {        if (Result = xpsFalse) and (oExpressionType = xpetNodeSet) and (oElemList.Length > 1) then
            begin
            if XpPos(s2, s1)  0 then
            Result := xpsTrue;
            end;
            if (Result = xpsFalse) and (oExpressionType2 = xpetNodeSet) and (oNewList.Length > 1) then
            begin
            if XpPos(s1, s2) > 0 then
            Result := xpsTrue;
            end;}
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TXpNode.noParseGetStartElement(oTokenList      : TXpNodeList;
                                    var wTokenIndex     : Integer;
                                        oNode           : TXpNode;
                                    var oExpressionType : TXpExpressionType)
                                                        : TXpNode;
var
  sTmp  : DOMString;
  oList : TXpNodeList;
begin
  Result := oNode;
  Inc(wTokenIndex);
  if wTokenIndex >= oTokenList.Length then begin
    Dec(wTokenIndex);
    Exit;
  end;

  { Check for absolute references }
  case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
    TOK_AXIS_SELF:
      begin
        noParseNewAddNodes(oList,
                           oNode,
                           oTokenList,
                           wTokenIndex,
                           oExpressionType,
                           [antSelf]);                             
        if (oList = nil) or (oList.Length = 0) then                    {!!.56}
          Result := nil;
        oList.Free;
        Exit;
      end;
    TOK_SLASH:
      begin
        Result := oNode.OwnerDocument;
        Exit;
      end;
    TOK_2SLASH:
      begin
        Result := oNode.OwnerDocument;
        Dec(wTokenIndex);
        Exit;
      end;
    TOK_ANCESTOR, TOK_ANCESTOR_or_SELF:
      begin
        sTmp := oTokenList.Item(wTokenIndex + 2).NodeName;
        while (Assigned(Result)) and Result.noNameEquals(sTmp) do
          Result := TXpElement(Result.ParentNode);
        if (TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_ANCESTOR_OR_SELF) and
           (Result = nil) then
          Result := oNode;
        Inc(wTokenIndex, 3);
        Exit;
      end;
  end;

  { Handle relative locations }
  while True do begin
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_2DOT:
        begin
          Result := TXpElement(Result.ParentNode);
          if TXpNode(Result).NodeType = DOCUMENT_NODE then
            Result := nil;
          if Result = nil then
            Exit;
        end;
      TOK_SLASH, TOK_DOT:;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
   end;
  end ;
end;
{--------}
function TXpNode.noParseMultiplicativeExpr(var oElemList       : TXpNodeList;
                                               oTokenList      : TXpNodeList;
                                           var wTokenIndex     : Integer;
                                           var oExpressionType : TXpExpressionType;
                                               oCurrentNode    : TXpNode)
                                                               : DOMString;
var
  sTmp, sTmp2      : DOMString;                                        {!!.52}
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
  wTmp             : Double;                                           {!!.53}
begin
  Result := noParseUnaryExpr(oElemList, oTokenList, wTokenIndex,
                             oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_WILD, TOK_OPMUL:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseUnaryExpr(oNewList, oTokenList, wTokenIndex,
                                   oExpressionType2, oCurrentNode);
          Result :=
            XpFloatToStr(XpConvertNumber(Result,                       {!!.56}
                                         oElemList,
                                         oExpressionType) *
                         XpConvertNumber(sTmp,
                                         oNewList,
                                         oExpressionType2));
          oExpressionType := xpetNumber;
          oNewList.Free;
        end;
      TOK_OPQUO:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseUnaryExpr(oNewList, oTokenList, wTokenIndex,
                                   oExpressionType2, oCurrentNode);
          try
            Result :=
              IntToStr(XpConvertInteger(Result, oElemList, oExpressionType) div
                       XpConvertInteger(sTmp, oNewList, oExpressionType2));
          except
            Result := '';
          end;
          oExpressionType := xpetNumber;
          oNewList.Free;
        end;
      TOK_OPMOD:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseUnaryExpr(oNewList, oTokenList, wTokenIndex,
                                   oExpressionType2, oCurrentNode);
          try
            Result :=
              IntToStr(XpConvertInteger(Result, oElemList, oExpressionType) mod
                       XpConvertInteger(sTmp, oNewList, oExpressionType2));
          except
            Result := '';
          end;
          oExpressionType := xpetNumber;
          oNewList.Free;
        end;
      TOK_OPDIV:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseUnaryExpr(oNewList, oTokenList, wTokenIndex,
                                   oExpressionType2, oCurrentNode);
          oNewList.Free;
          try
            sTmp2 := XpConvertString(Result, oElemList, oExpressionType,  {!!.52}
                                     False);                              {!!.52}
{Begin !!.53}
            wTmp := XpStrToFloat(sTmp);                                {!!.56}
            if wTmp = 0.0 then begin
              Result := XpsNaN;
              oExpressionType := xpetString;
            end
            else begin
              Result := XpFloatToStr(XpStrToFloat(sTmp2) / wTmp);      {!!.56}
              oExpressionType := xpetNumber;
            end;
          except
            Result := XpsNaN;
            oExpressionType := xpetString;
          end;
{End !!.53}
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TXpNode.noParseNewAddNodes(var oElemList       : TXpNodeList;
                                        oNode           : TXpNode;
                                        oTokenList      : TXpNodeList;
                                    var wTokenIndex     : Integer;
                                    var oExpressionType : TXpExpressionType;
                                        oAddType        : TXpAddNodeTypeSet)
                                                        : DOMString;
var
  oJnkList      : TXpNodeList;
  oAttr         : TXpAttribute;
  oTmpNode      : TXpNode;
  sTmp          : DOMString;
  i, j          : Integer;
  oToken        : TXpXQLToken;                                         {!!.52}
  PosWithinParent : Boolean;
  oWalker       : TXpTreeWalker;                                       {!!.52}
  oAxis         : TXpBaseAxis;                                         {!!.52}
begin
  Result := '';
  PosWithinParent := False;
  oElemList := TXpNodeList.Create;
  oElemList.Add(oNode);
  Inc(wTokenIndex);
  if wTokenIndex >= oTokenList.Length then begin
    Dec(wTokenIndex);
    Exit;
  end;

  if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_2SLASH then begin
    oAddType := [antDescend];                                     
    PosWithinParent := True;
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
  end;

  if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_DOT then  {!!.57}
    include(oAddType, antSelf);                                        {!!.57}

  case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
    TOK_IVALUE:
      begin
        Inc(wTokenIndex, 2);
        Exit;
      end;
  end;

  oElemList.Free;
  oElemList := nil;                                                    {!!.56}
  case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
    TOK_ID:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        Inc(wTokenIndex);
{Begin !!.52}
        oJnkList := TXpNodeList.Create;
        try
          if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId in
             [TOK_NUMBER, TOK_STRING_TYPE] then
            sTmp := oTokenList.Item(wTokenIndex).NodeName
          else begin
            dec(wTokenIndex);
            sTmp := noParseOrExpr(oJnkList,
                                  oTokenList,
                                  wTokenIndex,
                                  oExpressionType,
                                  oNode);
            case oExpressionType of
              xpetBoolean, xpetNumber :
                sTmp := XpConvertString(sTmp, oJnkList, oExpressionType, True);
              xpetNodeSet :
                begin
                  { Convert the nodeset to a set of whitespace-separated list of
                    IDs. }
                  sTmp := '';
                  for i := 0 to Pred(oJnkList.Length) do begin
                    if i > 0 then
                      sTmp := sTmp + ' ';
                    if oJnkList.Item(i).NodeType = ATTRIBUTE_NODE then
                      sTmp := sTmp + oJnkList.Item(i).NodeValue
                    else
                      sTmp := sTmp + oJnkList.Item(i).Text;
                  end;  { for }
                end;
            end;  { case }
          end;
        finally
          oJnkList.Free;
        end;
{End !!.52}
        oElemList := oNode.OwnerDocument.DocumentElement.GetElementsWithIDs(oNode.noOwnerDocument.IdAttribute, sTmp);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
      end;
    TOK_CMTEXTNODE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        if antChild in oAddType then
          oElemList := oNode.GetChildNodesByNodeType(TEXT_NODE)
{Begin !!.56}
        else if antSelf in oAddType then begin                 
          if oNode.NodeType = TEXT_NODE then begin
            oElemList := TXpNodeList.Create;
            oElemList.Add(oNode);
          end;
        end
{End !!.56}
        else
          oElemList := oNode.GetNodesByNodeType(TEXT_NODE);
      end;
    TOK_CMCOMMENT:
      begin
        if antChild in oAddType then                            
          oElemList := oNode.GetChildNodesByNodeType(COMMENT_NODE)
{Begin !!.56}
        else if antSelf in oAddType then begin                    
          if oNode.NodeType = COMMENT_NODE then begin
            oElemList := TXpNodeList.Create;
            oElemList.Add(oNode);
          end;
        end
{End !!.56}
        else
          oElemList := oNode.GetNodesByNodeType(COMMENT_NODE);
        Inc(wTokenIndex, 2);
      end;
    TOK_CMPI:
      begin
{Begin !!.52}
        { The processing-instruction() test may have an argument that is Literal
          in which case the test is true for any processing instruction that has
          a name equal to the value of the literal. }
        { Is current token a left parenthesis?}
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        { Yes. Move to next. Is it a string literal? }
        inc(wTokenIndex);
        oToken := TXpXqlToken(oTokenList.Item(wTokenIndex));
        if oToken.TokenID = TOK_STRING_TYPE then begin
          { Yes. Search for PI nodes bearing the specified name. }
          if antChild in oAddType then                             
            oElemList:= oNode.noGetChildNodesByNodeTypeName
                                (PROCESSING_INSTRUCTION_NODE,
                                 oToken.NodeName)
          else
            oElemList := oNode.noGetNodesByNodeTypeName
                                (PROCESSING_INSTRUCTION_NODE,
                                 oToken.NodeName);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        end
        else if oToken.TokenID = TOK_RPAREN then begin
          { Token is a right parenthesis. }
          inc(wTokenIndex);
          if antChild in oAddType then                             
            oElemList := oNode.GetChildNodesByNodeType
                                 (PROCESSING_INSTRUCTION_NODE)
{Begin !!.56}
        else if antSelf in oAddType then begin                    
          if oNode.NodeType = PROCESSING_INSTRUCTION_NODE then begin
            oElemList := TXpNodeList.Create;
            oElemList.Add(oNode);
          end;
        end
{End !!.56}
          else
            oElemList := oNode.GetNodesByNodeType
                                 (PROCESSING_INSTRUCTION_NODE);
        end
        else
          { Token was not a string literal or right parenthesis. Raise error. }
          raise TXpDomException.CreateValue(ecExpectedToken, TOK_RPAREN);
{End !!.52}
      end;
    TOK_CMELEMENT:
      begin
        if antChild in oAddType then                              
          oElemList := oNode.GetChildNodesByNodeType(ELEMENT_NODE)
{Begin !!.56}
        else if antSelf in oAddType then begin                    
          if oNode.NodeType = ELEMENT_NODE then begin
            oElemList := TXpNodeList.Create;
            oElemList.Add(oNode);
          end;
        end
{End !!.56}
        else
          oElemList := oNode.GetNodesByNodeType(ELEMENT_NODE);
        Inc(wTokenIndex, 2);
      end;
    TOK_CMNODE, TOK_DOT :                                              {!!.57}
      begin
        oElemList := TXpNodeList.Create;
        if antSelf in oAddType then                                    {!!.57}
          oElemList.Add(oNode);                                        {!!.57}
        if antChild in oAddType then begin                             
          oJnkList := oNode.GetChildNodesByNodeType(0);
          oElemList.CopyList(oJnkList);
          oJnkList.Free;
        end;
        if antDescend in oAddType then begin                       
          oJnkList := oNode.GetNodesByNodeType(0);
          oElemList.CopyList(oJnkList);
          oJnkList.Free;
        end;
//      if antSelf in oAddType then                                    {Deleted !!.57}
//        oElemList.Add(oNode);                                        {Deleted !!.57}
{Begin !!.57}
        if antFollowSibs in oAddType then begin                     
          oTmpNode := oNode.NextSibling;
          while Assigned(oTmpNode) do begin
            oElemList.Add(oTmpNode);
            oTmpNode := oTmpNode.NextSibling;
          end;
        end;
        if antPrecedSibs in oAddType then begin                    
          oTmpNode := oNode.PreviousSibling;
          while Assigned(oTmpNode) do begin
            oElemList.Add(oTmpNode);
            oTmpNode := oTmpNode.PreviousSibling;
          end;
        end;
{End !!.57}
        Inc(wTokenIndex, 2);
      end;
    TOK_AXIS_ATTRIBUTE:
      begin
        Inc(wTokenIndex);
        oElemList := TXpNodeList.Create;
        if oNode.HasAttributes then
          if TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName = '*' then
            for i := 0 to oNode.Attributes.Length - 1 do
              oElemList.Add(oNode.Attributes.Item(i))
          else begin
            oTmpNode :=
              TXpElement(oNode).GetAttributeNode(TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName);
            if Assigned(oTmpNode) then
              oElemList.Add(oTmpNode);
          end;
      end;
    TOK_CMATTRIBUTE:
      begin
        oElemList := oNode.GetChildNodesByNodeType(ATTRIBUTE_NODE);
        Inc(wTokenIndex, 2);
      end;
    TOK_AXIS_CHILD:
      begin
        oElemList := TXpNodeList.Create;
        Result := noParseOrExpr(oElemList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oNode);
      end;
    TOK_AXIS_NAMESPACE:
        Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex, oExpressionType, [antNamespace]);
    TOK_AXIS_DESCENDANT:
        Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex, oExpressionType, [antDescend]);
    TOK_AXIS_DESCENDANT_OR_SELF:
        Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex, oExpressionType, [antDescend, antSelf]);
{Begin !!.52}
    TOK_AXIS_PARENT:
      begin
        Inc(wTokenIndex);
        sTmp := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        oElemList := TXpNodeList.Create;
        oTmpNode := oNode.ParentNode;
        if (sTmp = '*') or (sTmp = oTmpNode.NodeName) then
          oElemList.Insert(0, oTmpNode);
      end;
{End !!.52}
    TOK_AXIS_ANCESTOR:
      begin
        Inc(wTokenIndex);
        sTmp := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        oElemList := TXpNodeList.Create;
        oTmpNode := oNode.ParentNode;
        while (oTmpNode <> nil) and                                    {!!.53}
              (oTmpNode.NodeType <> DOCUMENT_NODE) do begin            {!!.53}
          if (sTmp = '*') or (sTmp = oTmpNode.NodeName) then
            { Add item in document order. }                            {!!.52}
            oElemList.Insert(0, oTmpNode);                             {!!.52}
          oTmpNode := oTmpNode.ParentNode;
        end;
        xptvReverseAxis := True;                                       {!!.57}
      end;
    TOK_AXIS_ANCESTOR_or_SELF:
      begin
        Inc(wTokenIndex);
        sTmp := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        oElemList := TXpNodeList.Create;
        oTmpNode := oNode;
        while oTmpNode.NodeType <> DOCUMENT_NODE do begin
          if (sTmp = '*') or (sTmp = oTmpNode.NodeName) then
            oElemList.Insert(0, oTmpNode);                             {!!.52}
          oTmpNode := oTmpNode.ParentNode;
        end;
        xptvReverseAxis := True;                                       {!!.57}
      end;
{Begin !!.52}
    TOK_AXIS_FOLLOWING:
        Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex,
                                     oExpressionType, [antFollowing]);
{End !!.52}
    TOK_AXIS_FOLLOWING_SIBLING:
        Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex,
                                     oExpressionType, [antFollowSibs]);
{Begin !!.52}
    TOK_AXIS_PRECEDING:
{Begin !!.57}
        begin
          Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex,
                                       oExpressionType, [antPreceding]);
          xptvReverseAxis := True;
        end;
{End !!.57}
{End !!.52}
    TOK_AXIS_PRECEDING_SIBLING:
{Begin !!.57}
        begin
          Result := noParseNewAddNodes(oElemList, oNode, oTokenList, wTokenIndex,
                                       oExpressionType, [antPrecedSibs]);
          xptvReverseAxis := True;
        end;
{End !!.57}
    TOK_ELEMENT, TOK_WILD:
      begin
        if oNode is TXpDocument then
          if antChild in oAddType then begin                       
            oElemList := TXpNodeList.Create;
{Begin !!.53}
            if (oTokenList.Item(wTokenIndex).NodeName = '*') or
               (oTokenList.Item(wTokenIndex).NodeName =
                TXpDocument(oNode).DocumentElement.NodeName) then
{End !!.53}
              oElemList.Add(TXpDocument(oNode).DocumentElement);
          end else
            oElemList := TXpDocument(oNode).GetElementsByTagName(oTokenList.Item(wTokenIndex).NodeName)
        else begin
          oElemList := TXpNodeList.Create;
          if antChild in oAddType then begin                       
            oJnkList := TXpElement(oNode).GetChildElementsByTagName(oTokenList.Item(wTokenIndex).NodeName);
            oElemList.CopyList(oJnkList);
            oJnkList.Free;
          end;
{Begin !!.52}
          if antSelf in oAddType then begin                        
            sTmp := oTokenList.Item(wTokenIndex).NodeName;
            if (sTmp = '*') or (sTmp = oNode.NodeName) then
              oElemList.Add(oNode);
          end;
{End !!.52}
          if antDescend in oAddType then begin                    
            oJnkList := TXpElement(oNode).GetElementsByTagName(oTokenList.Item(wTokenIndex).NodeName);
            oElemList.CopyList(oJnkList);
            oJnkList.Free;
          end;
          if antNamespace in oAddType then begin                  
            oJnkList := TXpElement(oNode).GetElementsByTagNameNS(oTokenList.Item(wTokenIndex).NodeName, '*');
            oElemList.CopyList(oJnkList);
            oJnkList.Free;
          end;
{Moved !!.52}
{          if antSelf in oAddType then begin
            sTmp := oTokenList.Item(wTokenIndex).NodeName;
            if (sTmp = '*') or (sTmp = oNode.NodeName) then
              oElemList.Add(oNode);
          end;}
          if antFollowSibs in oAddType then begin                
            sTmp := oTokenList.Item(wTokenIndex).NodeName;
            oTmpNode := oNode.NextSibling;
            while Assigned(oTmpNode) do begin
              if (sTmp = '*') or (sTmp = oTmpNode.NodeName) then
                oElemList.Add(oTmpNode);
              oTmpNode := oTmpNode.NextSibling;
            end;
          end;
          if antPrecedSibs in oAddType then begin                
            sTmp := oTokenList.Item(wTokenIndex).NodeName;
            oTmpNode := oNode.PreviousSibling;
            while Assigned(oTmpNode) do begin
              if (sTmp = '*') or (sTmp = oTmpNode.NodeName) then
                oElemList.Add(oTmpNode);
              oTmpNode := oTmpNode.PreviousSibling;
            end;
          end;
{Begin !!.52}
          if antPreceding in oAddType then begin                  
            oWalker := TXpTreeWalker.Create;
            oAxis := TXpPrecedingAxis.Create(oNode);
            try
              oWalker.Gather(oAxis, noNameTest,
                             [oTokenList.Item(wTokenIndex).NodeName],
                             oElemList);
            finally
              oAxis.Free;
              oWalker.Free;
            end;
          end;
          if antFollowing in oAddType then begin                  
            oWalker := TXpTreeWalker.Create;
            oAxis := TXpFollowingAxis.Create(oNode);
            try
              oWalker.Gather(oAxis, noNameTest,
                             [oTokenList.Item(wTokenIndex).NodeName],
                             oElemList);
            finally
              oAxis.Free;
              oWalker.Free;
            end;
          end;
{End !!.52}
        end;
      end;
    TOK_LFRAME:
      begin
        oElemList := TXpNodeList.Create;
        oElemList.Add(oNode);
        Dec(wTokenIndex);
      end;
    TOK_ATTRIBUTE:
      begin
        oElemList := TXpNodeList.Create;
        Inc(wTokenIndex);
        if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_LPAREN then begin
          sTmp := oTokenList.Item(wTokenIndex + 1).NodeName;
          Inc(wTokenIndex, 2);
        end else
          sTmp := oTokenList.Item(wTokenIndex).NodeName;
        if oNode.HasAttributes then begin
          if sTmp <> '*' then begin
            oAttr := TXpAttribute(oNode.Attributes.GetNamedItem(sTmp));
            if Assigned(oAttr) then
              oElemList.Add(oAttr);
          end else
            { Add all attributes }
            for i := 0 to oNode.Attributes.Length - 1 do
              oElemList.Add(oNode.Attributes.Item(i));
        end;
        { Handle descendants if necessary. }
        if antDescend in oAddType then begin                       
          oJnkList := TXpNodeList.Create;
          try
            oNode.noTraverseTreeForNodes(oJnkList, oNode, ELEMENT_NODE);
            for i := 0 to Pred(oJnkList.Length) do
              if oJnkList.Item(i).HasAttributes then
                if sTmp <> '*' then begin
                  oAttr := TXpAttribute(oJnkList.Item(i).Attributes.GetNamedItem(sTmp));
                  if Assigned(oAttr) then
                    oElemList.Add(oAttr);
                end
                else
                  { Add all attributes }
                  for j := 0 to Pred(oJnkList.Item(i).Attributes.Length) do
                    oElemList.Add(oJnkList.Item(i).Attributes.Item(j));
          finally
            oJnkList.Free;
          end;
        end;  { if }
      end;
    else begin
      oElemList := TXpNodeList.Create;
      oElemList.Add(oNode);
      Dec(wTokenIndex);
      Exit;
    end;
  end;

{Rewritten !!.52}
  Result := noPredicate(oElemList, oTokenList, wTokenIndex, oExpressionType,
                        oNode, PosWithinParent)

end;
{--------}
function TXpNode.noParseOrExpr(var oElemList       : TXpNodeList;
                                   oTokenList      : TXpNodeList;
                               var wTokenIndex     : Integer;
                               var oExpressionType : TXpExpressionType;
                                   oCurrentNode    : TXpNode)
                                                   : DOMString;
var
  sTmp             : DOMString;       
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
begin
  Result := noParseAndExpr(oElemList, oTokenList, wTokenIndex,
                           oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_OR:
        begin
{Begin !!.53}
          if XpConvertBoolean(Result, oElemList, oExpressionType) then begin
            Result := xpsTrue;
            oExpressionType := xpetBoolean;
            Break;
          end
          else begin
            oNewList := TXpNodeList.Create;
            try
              oNewList.CopyList(oElemList);
              sTmp := noParseAndExpr(oNewList, oTokenList, wTokenIndex,
                                     oExpressionType2, oCurrentNode);
              if XpConvertBoolean(sTmp, oNewList, oExpressionType2) then begin
                Result := xpsTrue;
                Break;
              end
              else
                Result := xpsFalse;
            finally
              oExpressionType := xpetBoolean;
              oNewList.Free;
            end;
{End !!.53}
          end;  { if..else }
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
procedure TXpNode.noResolveDocument(var oElemList       : TXpNodeList;
                                        oTokenList      : TXpNodeList;
                                    var wTokenIndex     : Integer;
                                    var oExpressionType : TXpExpressionType;
                                        oCurrentNode    : TXpNode);
var
  bBaseURISet : Boolean;
  oExpressionType2 : TXpExpressionType;
  oNode : TXpDocument;
  oTmpList,
  oTmpList2 : TXpNodeList;
  sBaseURI,
  sTmp,
  sTmp2 : DOMString;
  wInx : Integer;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  if Assigned(noOwnerDocument.OnResolveDocument) then begin
    { parameters: uri [, base-uri] }
    noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
    sBaseURI := '';
    bBaseURISet := False;
    oElemList.Free;
    oElemList := TXpNodeList.Create;
    oTmpList := TXpNodeList.Create;
    try
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, oCurrentNode);
      oExpressionType2 := xpetUnknown;

      { Second parameter is optional. }
      if noTestToken(TOK_COMMA, oTokenList, wTokenIndex) then begin
        oTmpList2 := TXpNodeList.Create;
        try
          oExpressionType2 := xpetNodeSet;
          noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                        oExpressionType2, oCurrentNode);
          { Parameter 2 must be a node set. }
          if oExpressionType2 <> xpetNodeSet then
            raise TXpXPathException.Create
                    (Format(sXPathExpectedNodeSet,
                            [sXPathForDocumentParm2]));
          sBaseURI := XpConvertString('', oTmpList2, oExpressionType2, False);
          bBaseURISet := True;
        finally
          oTmpList2.Free;
        end;
      end;  { if }

      { Is the 1st parameter a nodeset? }
      if oExpressionType = xpetNodeSet then
        { Yes. Need to treat each node as a URI reference. }
        for wInx := 0 to Pred(oTmpList.Length) do begin
          if oTmpList.Item(wInx).NodeType = ATTRIBUTE_NODE then
            sTmp2 := oTmpList.Item(wInx).NodeValue
          else
            sTmp2 := oTmpList.Item(wInx).Text;
          if not bBaseURISet then
            sBaseURI := oTmpList.Item(wInx).BaseURI;
          noOwnerDocument.docOnResolveDocument(Self, sTmp2, sBaseURI, oNode);
          if oNode <> nil then
            oElemList.Add(oNode);
        end  { for }
      else begin
        { No. Variable sTmp contains the string value. }
        noOwnerDocument.docOnResolveDocument(Self, sTmp, sBaseURI, oNode);
        if oNode <> nil then
          oElemList.Add(oNode);
      end;  { if }
    finally
      oTmpList.Free;
    end;
    noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
    oExpressionType := xpetNodeSet;
  end  { if }
  else
    raise TXpXPathException.Create(Format(sXPathNoHandler, [XpsDocument]));
end;
{--------}
function TXpNode.noParseUnpEntityURI(var oElemList       : TXpNodeList;
                                         oTokenList      : TXpNodeList;
                                     var wTokenIndex     : Integer;
                                     var oExpressionType : TXpExpressionType;
                                         oCurrentNode    : TXpNode) : DOMString;
var
  i : Integer;
  oBaseContext,
  oContext : TXpDirContext;
  oNode : TXpNode;
  oTmpList : TXpNodeList;
  sTmp : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  { First parameter is the entity name. }
  noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
  oTmpList := TXpNodeList.Create;
  try
    sTmp := noParseOrExpr(oTmpList,
                          oTokenList,
                          wTokenIndex,
                          oExpressionType,
                          oCurrentNode);
    sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
  finally
    oTmpList.Free;
  end;
  noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);

  { Find the DTD for the document. }
  oNode := nil;
  for i := 0 to Pred(noOwnerDocument.ChildNodes.Length) do begin
    if noOwnerDocument.ChildNodes.Item(i).NodeType =
       DOCUMENT_TYPE_NODE then begin
      oNode := noOwnerDocument.ChildNodes.Item(i);
      Break;
    end;  { if }
  end;

  if oNode = nil then
    raise TXpXPathException.Create
             (Format(sXPathNoDTD,[QuotedStr(sTmp)]))
  else begin
    { Find the entity declaration in the DTD. }
    oTmpList := oNode.noGetNodesByNodeTypeName(ENTITY_DECL_NODE, sTmp);
    try
      if oTmpList.Length <> 1 then begin
        raise TXpXPathException.Create
                (Format(sXPathUnparsedEntCount, [QuotedStr(sTmp)]));
      end
      else if TXpDTDEntity(oTmpList.Item(0)).NotationName = '' then
        raise TXpXPathException.Create
                (Format(sXPathNotUnparsedEnt, [QuotedStr(sTmp)]))
      else begin
        { Expand the system ID using the base URI of the DTD. }
        oContext := nil;
        oBaseContext := TXpDirContext.Make
                          (nil,
                           oTmpList.Item(0).BaseURI + 'dummy.xml');
        try
          { Now create a context for the reference passing the base context
            in case the reference is relative. }
          oContext := TXpDirContext.Make(oBaseContext,
                                         TXpDTDEntity(oTmpList.Item(0)).SystemID);

          Result := oContext.URL;
          oExpressionType := xpetString;
        finally
          oBaseContext.Free;
          oContext.Free;
        end;
      end;
    finally
      oTmpList.Free;
    end;
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.noParsePathExpr(var oElemList       : TXpNodeList;
                                     oTokenList      : TXpNodeList;
                                 var wTokenIndex     : Integer;
                                 var oExpressionType : TXpExpressionType;
                                     oCurrentNode    : TXpNode)
                                                     : DOMString;
var
  oNode     : TXpNode;
  oNodeXQL  : TXpXQLToken;
  oTmpList  : TXpNodeList;                                             {!!.52}
  oValue    : TXpValue;                                                {!!.52}
  i,
  j         : Integer;                                                 {!!.52}
  sTmp,
  sTmp2,
  sTmp3     : DOMString;
  bTmp      : Boolean;
  oArgsList : TStringList;
  TempDouble: Double;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := '';
  Inc(wTokenIndex);
  if wTokenIndex >= oTokenList.Length then begin
    Dec(wTokenIndex);
    Exit;
  end;

  case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
    TOK_LPAREN:
      begin { ( Expr )}
        Result := noParseOrExpr(oElemList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
        noFindMatchingRPAREN(oTokenList, wTokenIndex);
//        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);           {!!.53}
      end;
{Begin !!.53}
    TOK_DOCUMENT:
      noResolveDocument(oElemList, oTokenList, wTokenIndex,
                        oExpressionType, oCurrentNode);
{End !!.53}
    TOK_VARIABLE:
      begin { variable $var}
        if (Assigned(OwnerDocument)) and
           Assigned(OwnerDocument.OnVariableLookup) then begin
          oValue := nil;
          OwnerDocument.OnVariableLookup(Self,
                                         TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName,
{Begin !!.52}
                                         oValue);
          try
            if oValue = nil then begin
              oExpressionType := xpetString;
              Result := '';
            end
            else begin
              oExpressionType := oValue.ValueType;
              if oExpressionType = xpetNodeSet then begin
                oElemList.Free;
                oElemList := TXpNodeList.Create;
                oElemList.CopyList(oValue.AsNodeSet);
              end
              else
                Result := oValue.AsString;
            end;
          finally
            oValue.Free;
          end;  { try }
        end;  { if }
{End !!.52}
      end;
    TOK_NUMBER:
      begin { numeric value}
        Result := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        oExpressionType := xpetNumber;
      end;
{Begin !!.52}
    TOK_KEY:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        { Get key name parameter. }
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
          noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);

          { Get key value(s) parameter. This could be a single key value or
            multiple key values (i.e., a nodeset). }
          oTmpList.Empty;
          oExpressionType := xpetNodeSet;                              {!!.55}
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          if oExpressionType <> xpetNodeset then
            sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);

          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);

          oElemList.Free;
          oElemList := TXpNodeList.Create;
          if Assigned(noOwnerDocument.OnKeyLookup) then
            { Were key values specified as a nodeset? }
            if oExpressionType = xpetNodeSet then begin
              { Yes. Do a key lookup for the string value of each node in the
                nodeset. }
              for i := 0 to Pred(oTmpList.Length) do begin
                oNode := oTmpList.Item(i);
                if oNode.NodeType = ATTRIBUTE_NODE then
                  sTmp3 := oNode.NodeValue
                else
                  sTmp3 := oNode.Text;
                noOwnerDocument.OnKeyLookup(noOwnerDocument, sTmp,
                                            sTmp3, oElemList);
              end;  { for }
            end
            else
              noOwnerDocument.OnKeyLookup(noOwnerDocument, sTmp, sTmp2,
                                          oElemList);
          oExpressionType := xpetNodeset;
        finally
          oTmpList.Free;
        end;

      end;
    TOK_FORMAT_NUMBER:
      begin
        { parameters: value, format pattern[, decimal format name] }
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        { Third parameter is optional. }
        if noTestToken(TOK_COMMA, oTokenList, wTokenIndex) then begin
          oTmpList := TXpNodeList.Create;
          try
            sTmp3 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                   oExpressionType, oCurrentNode);
          finally
            oTmpList.Free;
          end;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        if Assigned(noOwnerDocument.OnFormatNumber) then
          noOwnerDocument.OnFormatNumber(Self, sTmp, sTmp2, sTmp3, Result)
        else
          Result := sTmp;
        oExpressionType := xpetString;
      end;
{End !!.52}
    TOK_STRING_TYPE:
      begin { string value}
        Result := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        oExpressionType := xpetString;
      end;
    TOK_ATTRIBUTE:
      begin
        Inc(wTokenIndex);
        oElemList.Free;
        oElemList := TXpNodeList.Create;
        if (oCurrentNode.NodeType = ELEMENT_NODE) and                  {!!.52}
           oCurrentNode.HasAttributes then begin                       {!!.52}
          oNodeXQL := TXpXqlToken(oTokenList.Item(wTokenIndex));
          if oNodeXQL.TokenID = TOK_WILD then
            oElemList.CopyNamedNodeMap(oCurrentNode.Attributes)
          else begin
            oNode := oCurrentNode.Attributes.GetNamedItem(oNodeXQL.NodeName);
            if Assigned(oNode) then
              oElemList.Add(oNode);
          end;
        end;
        oExpressionType := xpetNodeSet;
      end;
    { Boolean functions}
    TOK_TRUE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := xpsTrue;
        oExpressionType := xpetBoolean;
      end;
    TOK_FALSE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := xpsFalse;
        oExpressionType := xpetBoolean;
      end;
{Begin !!.52}
    TOK_CURRENT:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        oExpressionType := xpetNodeset;
        oElemList.Free;
        oElemList := TXpNodeList.Create;
        if Assigned(noOwnerDocument.OnCurrentNode) then begin
          noOwnerDocument.OnCurrentNode(Self, oNode);
          oElemList.Add(oNode);
        end;
      end;
{End !!.52}
    TOK_NOT:
      begin
        oTmpList := TXpNodeList.Create;
        try
          if oElemList <> nil then                                     {!!.52}
            oTmpList.CopyList(oElemList);                              {!!.52}
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          if XpConvertBoolean(sTmp, oTmpList, oExpressionType) then
            Result := xpsFalse
          else
            Result := xpsTrue;
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetBoolean;
      end;
    TOK_BOOLEAN:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          if XpConvertBoolean(sTmp, oTmpList, oExpressionType) then
            Result := xpsTrue
          else
            Result := xpsFalse;
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetBoolean;
      end;
    TOK_LANG:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          { Grab the language identifier. }
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := xpsFalse;
        oExpressionType := xpetBoolean;
        if sTmp <> '' then begin
          i := Length(sTmp);
          oNode := oCurrentNode;
          while (Assigned(oNode)) and
                (oNode.NodeType = ELEMENT_NODE) do begin
            sTmp2 := TXpElement(oNode).GetAttribute(XpsXMLLang);       {!!.53}
            if sTmp2 <> '' then begin
              if XpStrIEquals(sTmp, sTmp2, i) then
                Result := xpsTrue
              else
                Result := xpsFalse;
              break;
            end;
            oNode := oNode.ParentNode;
          end;
        end;
      end;
    { Numeric functions}
    TOK_NUMBER_CAST:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
{Begin !!.53}
          try
            Result := XpFloatToStr(XpConvertNumber(sTmp,               {!!.56}
                                                   oTmpList,
                                                   oExpressionType));
          except
            Result := XpsNaN;
            oExpressionType := xpetNumber;
          end;
{End !!.53}
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetNumber;
      end;
    TOK_SUM:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        if TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId <> TOK_RPAREN then begin
          oTmpList := TXpNodeList.Create;
          try
            sTmp := noParseOrExpr(oTmpList,
                                  oTokenList,
                                  wTokenIndex,
                                  oExpressionType,
                                  oCurrentNode);
            TempDouble := 0;
{Begin !!.53}
            try
              for i := 0 to Pred(oTmpList.Length) do begin             {!!.55 - Start}
                oNode := oTmpList.Item(i);
                { Add its numerical value to the sum. }
                TempDouble := TempDouble + XpStrToFloat(oNode.StringValue); {!!.56}
              end;                                                     {!!.55 - End}
              Result := XpFloatToStr(TempDouble);                      {!!.56}
              oExpressionType := xpetNumber;
            except
              Result := XpsNaN;
              oExpressionType := xpetString;
            end;
{End !!.53}
          finally
            oTmpList.Free;
          end;
          noNextRParen(oTokenList, wTokenIndex);                       {!!.52}
        end else begin
          TempDouble := 0;
{Begin !!.53}
          try
            for i := 0 to Pred(oCurrentNode.ParentNode.ChildNodes.Length) do
              TempDouble :=
                TempDouble +
                XpStrToFloat(oCurrentNode.ParentNode.ChildNodes.Item(i).NodeValue); {!!.56}
            Result := XpFloatToStr(TempDouble);                        {!!.56}
            oExpressionType := xpetNumber;
          except
            Result := XpsNaN;
            oExpressionType := xpetString;
          end;
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        end;
{End !!.53}
      end;
    TOK_FLOOR:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := IntToStr(XpFloor(XpConvertNumber(sTmp, oTmpList,
                                                     oExpressionType)));
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetNumber;
      end;
    TOK_CEILING:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := IntToStr(XpCeiling(XpConvertNumber(sTmp, oTmpList,
                                                       oExpressionType)));
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetNumber;
      end;
    TOK_ROUND:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := IntToStr(Round(XpConvertNumber(sTmp, oTmpList, oExpressionType)));
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetNumber;
      end;
    { String functions}
    TOK_STRING_CAST:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetString;
      end;
    TOK_FCONCAT:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        while TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId = TOK_COMMA do begin
          Inc(wTokenIndex);
          oExpressionType := xpetNodeSet;                              {!!.52}
          oTmpList := TXpNodeList.Create;
          oTmpList.CopyList(oElemList);                                {!!.52}
          try
            sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                   oExpressionType, oCurrentNode);
            sTmp := sTmp + XpConvertString(sTmp2, oTmpList, oExpressionType, False);
          finally
            oTmpList.Free;
          end;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := sTmp;
        oExpressionType := xpetString;
      end;
    TOK_STARTS_WITH:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);

        { 1st string empty? }
        if sTmp = '' then begin
          { Yes. 2nd string empty? }
          if sTmp2 = '' then
            Result := xpsTrue
          else
            Result := xpsFalse;
        end
        else if sTmp2 = '' then
          Result := xpsTrue
        else if Length(sTmp2) > Length(sTmp) then
          Result := xpsFalse
        else if XpStrEquals(sTmp, sTmp2, Length(sTmp2)) then
          Result := xpsTrue
        else
          Result := xpsFalse;
        oExpressionType := xpetBoolean;
      end;
    TOK_CONTAINS:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        try
          noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
          oTmpList := TXpNodeList.Create;
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        if XpPos(sTmp2, sTmp) > 0 then
          Result := xpsTrue
        else
          Result := xpsFalse;
        oExpressionType := xpetBoolean;
      end;
    TOK_SUBSTRING:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          i := XpConvertInteger(sTmp2, oTmpList, oExpressionType);
        finally
          oTmpList.Free;
        end;
        { Third parameter is optional. }
{Begin !!.53}
        if noTestToken(TOK_COMMA, oTokenList, wTokenIndex) then begin
          oTmpList := TXpNodeList.Create;
          try
            sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                   oExpressionType, oCurrentNode);
            j := XpConvertInteger(sTmp2, oTmpList, oExpressionType)
          except
            dec(wTokenIndex);
            j := Length(sTmp) - i + 1;
          end;  { try..except }
          oTmpList.Free;
        end  { if }
        else
          j := Length(sTmp) - i + 1;
{End !!.53}
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := XpCopy(sTmp, i, j);
        oExpressionType := xpetString;
      end;
    TOK_SUBSTRING_BEFORE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                              oExpressionType, oCurrentNode);
        sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        oTmpList.Free;
        try
          noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
          oTmpList := TXpNodeList.Create;
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := '';
        i := XpPos(sTmp2, sTmp);
        if i > 0 then
          Result := XpCopy(sTmp, 1, i - 1);
        oExpressionType := xpetString;
      end;
    TOK_STRING_LENGTH:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := IntToStr(Length(XpConvertString(sTmp, oTmpList,
                                                    oExpressionType, False)));
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetNumber;
      end;
    TOK_SUBSTRING_AFTER:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := '';
        i := XpPos(sTmp2, sTmp);
        if i > 0 then
          Result := XpCopy(sTmp, i + Length(sTmp2), Length(sTmp));
        oExpressionType := xpetString;
      end;
    TOK_NORMALIZE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := noNormalize(sTmp);
        oExpressionType := xpetString;
      end;
    TOK_TRANSLATE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp2 := XpConvertString(sTmp2, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_COMMA, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp3 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                 oExpressionType, oCurrentNode);
          sTmp3 := XpConvertString(sTmp3, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := noTranslate(sTmp, sTmp2, sTmp3);
        oExpressionType := xpetString;
      end;
    { System functions}
    TOK_SYSTEM_PROPERTY:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        if sTmp = XpsXSLVersion then begin                             {!!.52}
          Result := XpXSLImplementation;                               {!!.51}
          oExpressionType := xpetNumber;
        end else if sTmp = 'xsl:vendor' then begin
          Result := XpVendor;
         oExpressionType := xpetString;
        end else if sTmp = 'xsl:vendor-url' then begin
          Result := XpVendorURL;
          oExpressionType := xpetString;
        end else begin
          Result := '';
          if (Assigned(OwnerDocument)) and
             (Assigned(OwnerDocument.OnSystemProperty)) then
            OwnerDocument.OnSystemProperty(self, sTmp, Result);
          oExpressionType := xpetString;
        end;
      end;
{Begin !!.53}
    TOK_UNPARSED_ENTITY_URI:
       Result := noParseUnpEntityURI(oElemList, oTokenList, wTokenIndex,
                                     oExpressionType, oCurrentNode);
    TOK_ELEMENT_AVAILABLE:
      begin
        if Assigned(noOwnerDocument.OnElementAvailable) then begin
          { First parameter is the element name. }
          noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
          oTmpList := TXpNodeList.Create;
          try
            sTmp := noParseOrExpr(oTmpList,
                                  oTokenList,
                                  wTokenIndex,
                                  oExpressionType,
                                  oCurrentNode);
            sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
          finally
            oTmpList.Free;
          end;
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          noOwnerDocument.OnElementAvailable(Self, sTmp, bTmp);
          if bTmp then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
        end
        else
          raise TXpXPathException.Create(Format(sXPathNoHandler,
                                                [XpsElementAvail]));
      end;
{End !!.53}
    TOK_FUNCTION_AVAILABLE:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          sTmp := noParseOrExpr(oTmpList,
                                oTokenList,
                                wTokenIndex,
                                oExpressionType,
                                oCurrentNode);
          sTmp := XpConvertString(sTmp, oTmpList, oExpressionType, False);
        finally
          oTmpList.Free;
        end;
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := '';
        bTmp := xpoXPathFuncHash.Exists(sTmp);                         {!!.52}
        if not bTmp then begin
          if (Assigned(OwnerDocument)) and
             (Assigned(OwnerDocument.OnFunctionAvailable)) then
            OwnerDocument.OnFunctionAvailable(Self, sTmp, bTmp);
        end;
        if bTmp then
          Result := xpsTrue
        else
          Result := xpsFalse;
        oExpressionType := xpetBoolean;
      end;
    { Extension functions}
    TOK_FUNCTION:
      begin
        sTmp := TXpXqlToken(oTokenList.Item(wTokenIndex)).NodeName;
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oArgsList := TStringList.Create;
        try
          while TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId <> TOK_RPAREN do begin
            if TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId = TOK_COMMA then
              Inc(wTokenIndex);
            oTmpList := TXpNodeList.Create;
            try
              sTmp2 := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                                     oExpressionType, oCurrentNode);
              oArgsList.Add(XpConvertString(sTmp2, oTmpList, oExpressionType,
                                            False));
            finally
              oTmpList.Free;
            end;
          end;
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := '';
          if (Assigned(OwnerDocument)) and Assigned(OwnerDocument.OnFunction) then
            OwnerDocument.OnFunction(self, sTmp, oArgsList, Result);
        finally
          oArgsList.Free;
        end;
        oExpressionType := xpetString;
      end;
    { Node set functions}
    TOK_LAST:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        Result := IntToStr(oElemList.Length);
        oExpressionType := xpetNumber;
      end;
    TOK_POSITION:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
{Begin !!.57}
        if xptvReverseAxis then
          Result := IntToStr(oElemList.Length -
                             oElemList.IndexOf(oCurrentNode))
        else
          Result := IntToStr(oElemList.IndexOf(oCurrentNode) + 1);
{End !!.57}
        oExpressionType := xpetNumber;
      end;
    TOK_ICOUNT:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        if TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId <> TOK_RPAREN then begin
          oTmpList := TXpNodeList.Create;
          try
            sTmp := noParseOrExpr(oTmpList,
                                  oTokenList,
                                  wTokenIndex,
                                  oExpressionType,
                                  oCurrentNode);
            Result := IntToStr(oTmpList.Length);
          finally
            oTmpList.Free;
          end;
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        end else begin
          Result := IntToStr(oCurrentNode.ParentNode.ChildNodes.Length);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
        end;
        oExpressionType := xpetNumber;
      end;
    TOK_LOCAL_PART:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                        oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := '';
          if (oTmpList.Length > 0) then begin
            oNode := oTmpList.Item(0);
            case oNode.NodeType of
              ELEMENT_NODE :
                Result := oNode.LocalName;
              ATTRIBUTE_NODE :
                { If this is the default namespace then return an empty string
                  otherwise return the attribute's name after the prefix. }
                if oNode.LocalName <> xpsXmlns then
                  Result := oNode.LocalName;
              PROCESSING_INSTRUCTION_NODE :
                Result := TXpProcessingInstruction(oNode).Target;
            end; { case }
          end;  { if }
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetString;
      end;
    TOK_NAMESPACEURI:                                                  {!!.52}
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                        oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := '';
          if (oTmpList.Length > 0) then begin
            oNode := oTmpList.Item(0);
            case oNode.NodeType of
              ELEMENT_NODE :
                Result := oNode.NamespaceURI;
              ATTRIBUTE_NODE :
                { If this is not a namespace declaration then return the
                  namespace URI of the attribute. }
                if (oNode.Prefix <> xpsXmlns) and
                   (oNode.LocalName <> xpsXmlns) then
                  Result := oNode.NamespaceURI;
            end; { case }
          end;  { if }
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetString;
      end;
    TOK_QNAME:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                        oExpressionType, oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := '';
          if (oTmpList.Length > 0) then begin
            oNode := oTmpList.Item(0);
            case oNode.NodeType of
              ELEMENT_NODE :
                Result := oNode.NodeName;
              ATTRIBUTE_NODE :
                { If this is the default namespace then return an empty string
                  otherwise return the attribute's name after the prefix. }
                if oNode.LocalName <> xpsXmlns then
                  if oNode.Prefix = xpsXmlns then
                    Result := oNode.LocalName
                  else
                    Result := oNode.NodeName;
              PROCESSING_INSTRUCTION_NODE :
                Result := TXpProcessingInstruction(oNode).Target;
            end; { case }
          end;  { if }
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetString;
      end;
    TOK_GENERATEID:
      begin
        noCheckToken(TOK_LPAREN, oTokenList, wTokenIndex);
        oTmpList := TXpNodeList.Create;
        try
          noParseOrExpr(oTmpList,
                        oTokenList,
                        wTokenIndex,
                        oExpressionType,
                        oCurrentNode);
          noCheckToken(TOK_RPAREN, oTokenList, wTokenIndex);
          Result := '';
          if (oTmpList.Length > 0) and (oTmpList.Item(0).NodeType = ELEMENT_NODE) then
            Result := xpsIDPrefix + oTmpList.Item(0).noGetLevelCode;
        finally
          oTmpList.Free;
        end;
        oExpressionType := xpetString;
      end;
    else begin
      { Find starting node }
      Dec(wTokenIndex);
      oNode := noParseGetStartElement(oTokenList, wTokenIndex, oCurrentNode,
                                      oExpressionType);
      if oNode = nil then begin
        oElemList.Empty;
        Exit;
      end;

      oElemList.Free;
      Result := noParseNewAddNodes(oElemList,
                                   oNode,
                                   oTokenList,
                                   wTokenIndex,
                                   oExpressionType,
                                   [antChild]);

      if oElemList.Length = 0 then
        Exit;
      Result := noParsePathContinue(oElemList, oTokenList, wTokenIndex,
                                    oExpressionType, oNode);
    end;  { case..else }
  end;  { case }

{Begin !!.52}
  { Can we continue parsing? }
  if (oExpressionType = xpetNodeSet) and
     (wTokenIndex < Pred(oTokenList.Length)) then begin
    oNodeXQL := TXpXQLToken(oTokenList.Item(wTokenIndex + 1));
    case oNodeXQL.TokenID of
      TOK_LFRAME :
        begin
          Result := noPredicate(oElemList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode, False);
          if wTokenIndex < Pred(oTokenList.Length) then
            Result := noParsePathContinue(oElemList, oTokenList,
                                          wTokenIndex, oExpressionType,
                                          oCurrentNode);

        end;
      TOK_RFRAME :;
      else
        Result := noParsePathContinue(oElemList, oTokenList,
                                      wTokenIndex, oExpressionType,
                                      oCurrentNode);
    end;  { case }
  end;  { if continuable }
{End !!.52}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{Begin !!.52}
{--------}
function TXpNode.noParsePathContinue(var oElemList       : TXpNodeList;
                                         oTokenList      : TXpNodeList;
                                     var wTokenIndex     : Integer;
                                     var oExpressionType : TXpExpressionType;
                                         oCurrentNode    : TXpNode)
                                                         : DOMString;
var
  i, j, k : Integer;
  oTmpList, oTmpList2 : TXpNodeList;
begin
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_SLASH:
        begin
          oTmpList := TXpNodeList.Create;
          k := wTokenIndex;
          if oElemList.Length > 0 then begin
            for i := 0 to oElemList.Length - 1 do begin
              k := wTokenIndex;
              Result := noParseNewAddNodes(oTmpList2,
                                           oElemList.Item(i),
                                           oTokenList,
                                           k,
                                           oExpressionType,
                                           [antChild]);
              for j := 0 to oTmpList2.Length - 1 do
                oTmpList.Add(oTmpList2.Item(j));
              oTmpList2.Free;
            end;
            wTokenIndex := k;
          end else begin
            Result := noParseNewAddNodes(oTmpList2,
                                         Self,
                                         oTokenList,
                                         wTokenIndex,
                                         oExpressionType,
                                         [antChild]);
            oTmpList2.Free;
          end;
          oElemList.Free;
          oElemList := oTmpList;
          if oElemList.Length = 0 then                                 {!!.57}
            Exit;                                                      {!!.57}
        end;
      TOK_2SLASH:
        begin
          oTmpList := TXpNodeList.Create;
          Dec(wTokenIndex);
          k := wTokenIndex;
          for i := 0 to oElemList.Length - 1 do begin
            k := wTokenIndex;
            Result := noParseNewAddNodes(oTmpList2, oElemList.Item(i),
                                         oTokenList, k, oExpressionType,
                                         [antChild]);
            for j := 0 to oTmpList2.Length - 1 do
              oTmpList.Add(oTmpList2.Item(j));
            oTmpList2.Free;
          end;
          wTokenIndex := k;
          oElemList.Free;
          oElemList := oTmpList;
          if oElemList.Length = 0 then                                 {!!.57}
            Exit;                                                      {!!.57}
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;  { case..else }
    end;  { case }
  end;  { while }
end;
{--------}
function TXpNode.noParseRelationalExpr(var oElemList       : TXpNodeList;
                                           oTokenList      : TXpNodeList;
                                       var wTokenIndex     : Integer;
                                       var oExpressionType : TXpExpressionType;
                                           oCurrentNode    : TXpNode)
                                                           : DOMString;
var
  sTmp             : DOMString;
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
begin
  Result := noParseAdditiveExpr(oElemList, oTokenList, wTokenIndex,
                                oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_LT:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseAdditiveExpr(oNewList, oTokenList, wTokenIndex,
                                      oExpressionType2, oCurrentNode);
          if XpConvertNumber(Result, oElemList, oExpressionType) <
             XpConvertNumber(sTmp, oNewList, oExpressionType2) then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      TOK_GT:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseAdditiveExpr(oNewList, oTokenList, wTokenIndex,
                                      oExpressionType2, oCurrentNode);
          if XpConvertNumber(Result, oElemList, oExpressionType) >
             XpConvertNumber(sTmp, oNewList, oExpressionType2) then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      TOK_LE:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseAdditiveExpr(oNewList, oTokenList, wTokenIndex,
                                      oExpressionType2, oCurrentNode);
          if XpConvertNumber(Result, oElemList, oExpressionType) <=
             XpConvertNumber(sTmp, oNewList, oExpressionType2) then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      TOK_GE:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          sTmp := noParseAdditiveExpr(oNewList, oTokenList, wTokenIndex,
                                      oExpressionType2, oCurrentNode);
          if XpConvertNumber(Result, oElemList, oExpressionType) >=
             XpConvertNumber(sTmp, oNewList, oExpressionType2) then
            Result := xpsTrue
          else
            Result := xpsFalse;
          oExpressionType := xpetBoolean;
          oNewList.Free;
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TXpNode.noParseUnaryExpr(var oElemList       : TXpNodeList;
                                      oTokenList      : TXpNodeList;
                                  var wTokenIndex     : Integer;
                                  var oExpressionType : TXpExpressionType;
                                      oCurrentNode    : TXpNode)
                                                      : DOMString;
var
  aStr : DOMString;
begin
  Inc(wTokenIndex);
  if wTokenIndex >= oTokenList.Length then begin
    Dec(wTokenIndex);
    Exit;
  end;

  if TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId = TOK_OPDIFF then begin
    aStr := noParseUnionExpr(oElemList, oTokenList, wTokenIndex,
                             oExpressionType, oCurrentNode);
    Result := XpFloatToStr( - XpConvertNumber(aStr,                    {!!.56}
                                              oElemList,
                                              oExpressionType))
  end
  else begin
    Dec(wTokenIndex);
    Result := noParseUnionExpr(oElemList, oTokenList, wTokenIndex,
                               oExpressionType, oCurrentNode);
  end;
end;
{--------}
function TXpNode.noParseUnionExpr(var oElemList       : TXpNodeList;
                                      oTokenList      : TXpNodeList;
                                  var wTokenIndex     : Integer;
                                  var oExpressionType : TXpExpressionType;
                                      oCurrentNode    : TXpNode)
                                                      : DOMString;
var
  i                : Integer;
  oNewList         : TXpNodeList;
  oExpressionType2 : TXpExpressionType;
begin
  Result := noParsePathExpr(oElemList, oTokenList, wTokenIndex,
                            oExpressionType, oCurrentNode);
  while True do begin
    Inc(wTokenIndex);
    if wTokenIndex >= oTokenList.Length then begin
      Dec(wTokenIndex);
      Exit;
    end;
    oExpressionType2 := xpetNodeSet;                                   {!!.52}
    case TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId of
      TOK_UNION:
        begin
          oNewList := TXpNodeList.Create;
          oNewList.CopyList(oElemList);
          Result := noParsePathExpr(oNewList, oTokenList, wTokenIndex,
                                    oExpressionType2, oCurrentNode);
          for i := 0 to oNewList.Length - 1 do
            if oElemList.IndexOf(oNewList.Item(i)) < 0 then
              oElemList.Add(oNewList.Item(i));
          oNewList.Free;
          oExpressionType := xpetNodeSet;
        end;
      else begin
        Dec(wTokenIndex);
        Exit;
      end;
    end;
  end;
end;
{Begin !!.52}
{--------}
function TXpNode.noPredicate(var oElemList       : TXpNodeList;
                                 oTokenList      : TXpNodeList;
                             var wTokenIndex     : Integer;
                             var oExpressionType : TXpExpressionType;
                                 oCurrentNode    : TXpNode;
                                 bPosWithinParent : Boolean)
                                                 : DOMString;
var
  i, k, NodeCnt : Integer;                                             {!!.57}
  oJnkList : TXpNodeList;
  oLocalExpType : TXpExpressionType;
  Value : Integer;                                                     {!!.57}
begin
  { Check to qualify elements }
  while True do begin
    Inc(wTokenIndex);
    if (wTokenIndex >= oTokenList.Length) or
       (TXpXqlToken(oTokenList.Item(wTokenIndex)).TokenId <>
        TOK_LFRAME) then begin
      Dec(wTokenIndex);
      Exit;
    end;
    k := wTokenIndex;
    if oElemList.Length > 0 then begin
      oJnkList := TXpNodeList.Create;
      oJnkList.CopyList(oElemList);
      NodeCnt := oElemList.Length;
      for i := Pred(NodeCnt) downto 0 do begin
        k := wTokenIndex;
        oLocalExpType := xpetNodeSet;
        Result := noParseOrExpr(oJnkList, oTokenList, k, oLocalExpType,
                                TXpElement(oElemList.Item(i)));
        case oLocalExpType of
          xpetNumber:
            { Is this case like '//para[1]'? }
            if bPosWithinParent then begin
              { Yes. Select those nodes that are the nth occurrence of that
                node within their parent. }
              if not TXpElement(oElemList.Item(i)).elIsNthOccurrence
                                                   (StrToIntDef(Result, -1)) then
                oElemList.Delete(i);
            end
{Begin !!.57}
            else begin
              Value := StrToIntDef(Result, - 1);
              if xptvReverseAxis then begin
                if Value <> (NodeCnt - i) then
                  oElemList.Delete(i);
              end
              else if Value <> i + 1 then
                oElemList.Delete(i);
            end;
{End !!.57}
          else
              if not XpConvertBoolean(Result, oJnkList, oLocalExpType) then
                oElemList.Delete(i);
        end;
      end;
      oJnkList.Free;
      wTokenIndex := k;
    end else begin
      oJnkList := TXpNodeList.Create;
      oLocalExpType := xpetNodeSet;
      Result := noParseOrExpr(oJnkList, oTokenList, wTokenIndex,
                              oLocalExpType, oCurrentNode);
      oJnkList.Free;
    end;
    noFindMatchingRFrame(oTokenList, wTokenIndex);
  end;
end;
{--------}
procedure TXpNode.Normalize(bAddSpace : Boolean);
begin
  noTraverseTreeToNormalize(FirstChild, bAddSpace);
end;
{Begin !!.52}
{--------}
function TXpNode.noTestToken(wToken      : Integer;
                             oTokenList  : TXpNodeList;
                         var wTokenIndex : Integer) : Boolean;
begin
  Result := (wTokenIndex + 1 < oTokenList.Length) and
            (TXpXqlToken(oTokenList.Item(wTokenIndex + 1)).TokenId = wToken);
  if Result then
    inc(wTokenIndex);
end;
{End !!.52}
{--------}
function TXpNode.noTranslate(const sValue, sFrom, sTo : DOMString) : DOMString;
var
  anInx : Integer;
  aPos : Integer;
  aResultInx : Integer;
  aSrcLen : Integer;
  aToLen : Integer;
begin
  aSrcLen := Length(sValue);
  SetLength(Result, aSrcLen);
  aResultInx := 0;
  aToLen := Length(sTo);
  for anInx := 1 to Length(sValue) do begin
    { Does the character exist within the sFrom parameter? }
    aPos := XpPos(sValue[anInx], sFrom);
    if aPos > 0 then begin
      { Yes. Is there a corresponding character in the sTo parameter? }
      if aPos <= aToLen then begin
        { Yes. Copy the substitute character to the result. }
        inc(aResultInx);
        Result[aResultInx] := sTo[aPos];
      end;
    end
    else begin
      { No. Copy character as is. }
      inc(aResultInx);
      Result[aResultInx] := sValue[anInx];
    end;
  end;
  if aResultInx < aSrcLen then
    Delete(Result, aResultInx + 1, aSrcLen - aResultInx);
end;
{--------}
procedure TXpNode.noTraverseTreeForNodes(oList : TXpNodeList;
                                         oNode : TXpNode;
                                   const wType : Integer);
var
  i : Integer;
begin
  while Assigned(oNode) do begin
    if wType = ATTRIBUTE_NODE then begin
      if oNode.HasAttributes then
        for i := 0 to oNode.Attributes.Length - 1 do
          oList.Add(oNode.Attributes.Item(i))
    end else if (wType = 0) or
                (oNode.NodeType = wType) then begin
      oList.Add(oNode);
    end; {if..else}
    if oNode.HasChildNodes then
      noTraverseTreeForNodes(oList, oNode.FirstChild, wType);
    oNode := oNode.NextSibling;
  end; {while}
end;
{Begin !!.52}
{--------}
procedure TXpNode.noTraverseTreeForNamedNodes(oList : TXpNodeList;
                                              oNode : TXpNode;
                                        const sName : DOMString;
                                        const wType : Integer);
var
  i : Integer;
begin
  while Assigned(oNode) do begin
    if wType = ATTRIBUTE_NODE then begin
      if oNode.HasAttributes then
        for i := 0 to oNode.Attributes.Length - 1 do
          if oNode.Attributes.Item(i).noNameEquals(sName) then
            oList.Add(oNode.Attributes.Item(i))
    end else if ((wType = 0) or (oNode.NodeType = wType)) and
                oNode.noNameEquals(sName) then begin
      oList.Add(oNode);
    end; {if..else}
    if oNode.HasChildNodes then
      noTraverseTreeForNamedNodes(oList, oNode.FirstChild, sName, wType);
    oNode := oNode.NextSibling;
  end; {while}
end;
{End !!.52}
{--------}
function TXpNode.noGetNodeStringType : DOMString;
begin
  case noNodeType of
    ELEMENT_NODE                : Result := 'ELEMENT_NODE';
    ATTRIBUTE_NODE              : Result := 'ATTRIBUTE_NODE';
    TEXT_NODE                   : Result := 'TEXT_NODE';
    CDATA_SECTION_NODE          : Result := 'CDATA_SECTION_NODE';
    ENTITY_REFERENCE_NODE       : Result := 'ENTITY_REFERENCE_NODE';
    ENTITY_NODE                 : Result := 'ENTITY_NODE';
    PROCESSING_INSTRUCTION_NODE : Result := 'PROCESSING_INSTRUCTION_NODE';
    COMMENT_NODE                : Result := 'COMMENT_NODE';
    DOCUMENT_NODE               : Result := 'DOCUMENT_NODE';
    DOCUMENT_type_NODE          : Result := 'DOCUMENT_TYPE_NODE';
    DOCUMENT_FRAGMENT_NODE      : Result := 'DOCUMENT_FRAGMENT_NODE';
    NOTATION_NODE               : Result := 'NOTATION_NODE';
  else
    Result := 'UNKNOWN';
  end;
end;
{--------}
procedure TXpNode.noTraverseTreeToNormalize(oNode     : TXpNode;
                                            bAddSpace : Boolean);
var
  oNode2 : TXpNode;
begin
  while Assigned(oNode) do begin
    if oNode.NodeType = TEXT_NODE then  begin
      oNode2 := oNode.NextSibling;
      while (Assigned(oNode2)) and (oNode2.NodeType = TEXT_NODE) do begin
        if bAddSpace then
          TXpText(oNode).AppendData(' ');
        TXpText(oNode).AppendData(TXpText(oNode2).Data);
        oNode2.ParentNode.RemoveChild(oNode2);
        oNode2.Release;
        oNode2 := oNode.NextSibling;
      end;
    end;
    if oNode.HasChildNodes then
      noTraverseTreeToNormalize(oNode.FirstChild, bAddSpace);
    oNode := oNode.NextSibling;
  end;
end;
{--------}
procedure TXpNode.AddRef;
begin
  Inc(noRefCount);
end;
{--------}
procedure TXpNode.AppendChild(oNewChild : TXpNode);
var
  oNode    : TXpNode;
  oNewNode : TXpNode;
begin
  if OwnerDocument <> oNewChild.OwnerDocument then
    raise TXpDomException.CreateCode(ecWrongDocumentErr);
  case noNodeType of
    TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE,
    PROCESSING_INSTRUCTION_NODE, ENTITY_NODE, NOTATION_NODE:
      raise TXpDomException.CreateCode(ecHierarchyRequestErr);
  end;

  if Assigned(oNewChild) then
    if oNewChild.noNodeType = DOCUMENT_FRAGMENT_NODE then begin
      { Document fragment }
      oNode := oNewChild.FirstChild;
      while Assigned(oNode) do begin
        oNewNode := oNode.CloneNode(True);
        oNewNode.noParentNode := Self;
        noChildNodes.Add(oNewNode);
        oNewNode.Release;
        oNode := oNode.NextSibling;
      end;
    end else begin
      oNewChild.noParentNode := Self;
      noChildNodes.Add(oNewChild);
    end
  else
    raise TXpDomException.CreateCode(ecInvalidParamErr);
end;
{--------}
constructor TXpNode.Create;
begin
  inherited Create;
  noOutputEscaping := True;                                            {!!.52}
  noOwnerDocument := nil;
  noParentNode := nil;
  noAttributes := nil;
  noNamespaceList := nil;
  noNodeID := 0;
  noChildNodes := TXpNodeList.Create;
  AddRef;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
destructor TXpNode.Destroy;
begin
  Dec(noRefCount);
  if noRefCount <= 0 then begin
    RemoveAll;
    noAttributes.Free;
    noNamespaceList.Free;
    noChildNodes.Free;
    inherited Destroy;
  end;
end;
{Begin !!.52}
{--------}
function TXpNode.CloneNode(bDeep : Boolean) : TXpNode;
begin
  { Do nothing }
  Result := nil;
end;
{End !!.52}
{--------}
procedure TXpNode.ForceOwnerDocument(oNode : TXpNode);
var
  Idx : Integer;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  oNode.noOwnerDocument := noOwnerDocument;
  for Idx := 0 to Pred(oNode.noChildNodes.Length) do
    ForceOwnerDocument(oNode.noChildNodes.Item(Idx));
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.GetChildNodesByNodeType(wType : Integer) : TXpNodeList;
var
  oNode : TXpNode;
  i     : Integer;
begin
  Result := TXpNodeList.Create;
  oNode := FirstChild;
  while Assigned(oNode) do begin
    if wType = ATTRIBUTE_NODE then begin
      if oNode.HasAttributes then
        for i := 0 to oNode.Attributes.Length - 1 do
          Result.Add(oNode.Attributes.Item(i));
    end
    else if (wType = 0) or (oNode.NodeType = wType) then
      Result.Add(oNode);
    oNode := oNode.NextSibling;
  end;
end;
{--------}
function TXpNode.GetNodesByNodeType(wType : Integer) : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  try
    noTraverseTreeForNodes(Result, FirstChild, wType);
  except
    Result.Free;
    raise;
  end;
end;
{--------}
function TXpNode.HasAttributes : Boolean;
begin
  Result := (Assigned(noAttributes)) and (noAttributes.Length > 0);
end;
{--------}
function TXpNode.HasChildNodes : Boolean;
begin
  Result := noChildNodes.Length > 0;
end;
{--------}
procedure TXpNode.InsertBefore(oNewChild, oRefChild : TXpNode);
var
  i        : Integer;
  oNode    : TXpNode;
  oNewNode : TXpNode;
begin
  if OwnerDocument <> oNewChild.OwnerDocument then
    raise TXpDomException.CreateCode(ecWrongDocumentErr);
  case noNodeType of
    TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE,
    PROCESSING_INSTRUCTION_NODE, ENTITY_NODE, NOTATION_NODE:
    raise TXpDomException.CreateCode(ecHierarchyRequestErr);
  end;

  if Assigned(oNewChild) then
    if oNewChild.noNodeType = DOCUMENT_FRAGMENT_NODE then
      { Document fragment }
      if oRefChild = nil then begin
        oNode := oNewChild.FirstChild;
        while Assigned(oNode) do begin
          oNewNode := oNode.CloneNode(true);
          oNewNode.noParentNode := Self;
          noChildNodes.Add(oNewNode);
          oNewNode.Release;
          oNode := oNode.NextSibling;
        end;
      end else begin
        i := noChildNodes.IndexOf(oRefChild);
        if i >= 0 then begin
          oNode := oNewChild.FirstChild;
          while Assigned(oNode) do begin
            oNewNode := oNode.CloneNode(true);
            oNewNode.noParentNode := Self;
            noChildNodes.Insert(i, oNewNode);
            oNewNode.Release;
            Inc(i);
            oNode := oNode.NextSibling;
          end;
        end else
          raise TXpDomException.CreateCode(ecNotFoundErr);
      end
    else
      if oRefChild = nil then begin
        oNewChild.noParentNode := Self;
        noChildNodes.Add(oNewChild);
      end else begin
        i := noChildNodes.IndexOf(oRefChild);
        if i >= 0 then begin
          oNewChild.noParentNode := Self;
          noChildNodes.Insert(i, oNewChild);
        end else
          raise TXpDomException.CreateCode(ecNotFoundErr);
      end
  else
    raise TXpDomException.CreateCode(ecInvalidParamErr);
end;
{--------}
function TXpNode.IsAfter(oNode : TXpNode) : Boolean;
var
  sLine1 : DOMString;
  sLine2 : DOMString;
  wPos   : Integer;
  wVal1  : Integer;
  wVal2  : Integer;
begin
  Result := false;
  sLine1 := LevelCode;
  sLine2 := oNode.LevelCode;
  if (sLine1 = '') or (sLine2 = '') then
    Exit;
  if oNode = Self then
    Exit;

  while Length(sLine2) > 0 do begin
    wPos := XpPos('.', sLine1);
    if wPos > 0 then begin
      wVal1 := StrToIntDef(XpCopy(sLine1, 1, wPos - 1), 0);
      Delete(sLine1, 1, wPos);
    end else begin
      wVal1 := StrToIntDef(sLine1, 0);
      sLine1 := '';
    end;
    wPos := XpPos('.', sLine2);
    if wPos > 0 then begin
      wVal2 := StrToIntDef(XpCopy(sLine2, 1, wPos - 1), 0);
      Delete(sLine2, 1, wPos);
    end else begin
      wVal2 := StrToIntDef(sLine2, 0);
      sLine2 := '';
    end;
    if wVal1 > wVal2 then begin
      Result := true;
      Exit;
{Begin !!.57}
    end
    else if wVal1 < wVal2 then begin
      Result := False;
      Exit;
    end;
{End !!.57}
    if sLine1 = '' then
      Exit;
  end;
  Result := true;
end;
{--------}
function TXpNode.IsSupported(const sFeature, sVersion : DOMString) : Boolean; {!!.57}
begin
  Result := (Uppercase(sFeature) = 'XML') and
            ((Uppercase(sVersion) = '1.0') or (Uppercase(sVersion) = '2.0'));
end;
{Begin !!.53}
{--------}
function TXpNode.LocationPath : DOMString;
begin
  if noParentNode <> nil then
    Result := noParentNode.LocationPath + '/' +
              Format('%s[%d]',
                     [noNodeName,
                      noParentNode.ChildNodes.IndexOf(Self) + 1])
  else
    Result := noNodeName;
end;
{$IFDEF NodeMemoryUsageEnabled}
{--------}
function TXpNode.MemoryUsed : Longint;
begin
  Result := SizeOf(noBaseURI) +
            (Length(noBaseURI) * 2) +
            SizeOf(noDefaultnamespace) +
            (Length(noDefaultNamespace) * 2) +
            SizeOf(noOutputEscaping) +
            SizeOf(noNodeID) +
            SizeOf(noNodeName) +
            (Length(noNodeName) * 2) +
            SizeOf(noNodeType) +
            SizeOf(noNodeValue) +
            (Length(noNodeValue) * 2) +
            SizeOf(fNoOwnerDocument) +
            SizeOf(noParentNode) +
            SizeOf(noRefCount) +
            SizeOf(noTag);

  if noAttributes <> nil then
    Result := Result + noAttributes.MemoryUsed;

  if noChildNodes <> nil then
    Result := Result + noChildNodes.MemoryUsed;

  if noNameSpaceList <> nil then
    Result := Result + noNameSpaceList.MemoryUsed;

end;
{$ENDIF}
{--------}
procedure TXpNode.noOutput(oMem   : TMemoryStream;
                           wLevel : Integer);
begin
  { Do nothing }
end;
{End !!.53}
{--------}
procedure TXpNode.Release;
begin
  Dec(noRefCount);
  if noRefCount <= 0 then
    Free;
end;
{--------}
procedure TXpNode.RemoveAll;
begin
  if noChildNodes <> nil then                                          {!!.52}
    noChildNodes.Empty;
end;
{--------}
function TXpNode.RemoveChild(oRefChild : TXpNode) : TXpNode;
var
  Idx : Integer;
begin
  Idx := noChildNodes.IndexOf(oRefChild);
  if Idx >= 0 then begin
    Result := noChildNodes.Item(Idx);
    Result.noParentNode := nil;
    Result.AddRef;
    noChildNodes.Delete(Idx);
  end else
    raise TXpDomException.CreateCode(ecNotFoundErr);
end;
{--------}
function TXpNode.ReplaceChild(oNewChild, oRefChild : TXpNode) : TXpNode;
var
  Idx : Integer;
begin
  if OwnerDocument <> oNewChild.OwnerDocument then
    raise TXpDomException.CreateCode(ecWrongDocumentErr);
  case noNodeType of
    TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE,
    PROCESSING_INSTRUCTION_NODE, ENTITY_NODE, NOTATION_NODE:
    raise TXpDomException.CreateCode(ecHierarchyRequestErr);
  end;

  Idx := noChildNodes.IndexOf(oRefChild);
  if Idx >= 0 then begin
    Result := noChildNodes.Item(Idx);
    Result.noParentNode := nil;
    Result.AddRef;
    oNewChild.noParentNode := Self;
    noChildNodes.Replace(Idx, oNewChild);
  end else
    raise TXpDomException.CreateCode(ecNotFoundErr);
end;
{Begin !!.52}
{--------}
function TXpNode.ResolveNSPrefix(const sPrefix : DOMString) : DOMString;
var
  oNode   : TXpNode;
  oNode2  : TXpNode;
begin
  Result := '';
  if sPrefix = '' then begin
    { Get default namespace }
    oNode := Self;
    while (Assigned(oNode)) and
          (oNode.NodeType in [ELEMENT_NODE, ATTRIBUTE_NODE]) do begin
      if TXpElement(oNode).noDefaultNamespace <> '' then begin
        Result := TXpElement(oNode).noDefaultNamespace;
        Exit;
      end;
      oNode := oNode.ParentNode;
    end;
  end else begin
    { Lookup namespace }
    oNode := Self;
    while (Assigned(oNode)) and
          (oNode.NodeType in [ELEMENT_NODE, ATTRIBUTE_NODE]) do begin
      if Assigned(TXpElement(oNode).noNamespaceList) then begin
        oNode2 := TXpElement(oNode).noNamespaceList.GetNamedItem(sPrefix);
        if Assigned(oNode2) then begin
          Result := oNode2.NodeValue;
          Exit;
        end;
      end;
      oNode := oNode.ParentNode;
    end;
  end;
end;
{End !!.52}
{Begin !!.52}
{--------}
function TXpNode.Select(const sQuery : DOMString;                      {!!.57}
                        oContext : TXpNodeList) : TXpValue;
var
  oTokenList      : TXpNodeList;
  oTmpList        : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
  sTmp            : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := nil;
  if sQuery = '' then
    Exit;

  xptvReverseAxis := False;                                            {!!.57}

  Result := TXpValue.Create;
  try
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);
    oTmpList := TXpNodeList.Create;
    try
      if oContext <> nil then
        oTmpList.CopyList(oContext);
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, Self);
      Result.FValueType := oExpressionType;
      case oExpressionType of
        xpetNodeSet :
          Result.AsNodeSet := oTmpList;
        xpetBoolean :
          Result.AsBoolean := XpConvertBoolean(sTmp, oTmpList, oExpressionType);
        xpetNumber :
          Result.AsNumber := XpConvertNumber(sTmp, oTmpList, oExpressionType);
        xpetString :
          Result.AsString := XpConvertString(sTmp, oTmpList, oExpressionType, False);
      end;
    finally
      oTokenList.Free;
      oTmpList.Free;
    end;
  except
    Result.Free;
    raise;
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.SelectBoolean(const sQuery : DOMString) : Boolean;    {!!.57}
begin
  Result := False;
  if sQuery = '' then
    Exit;

  try
    Result := SelectBooleanContext(sQuery, nil);
  except
  end;
end;
{--------}
function TXpNode.SelectBooleanContext(const sQuery : DOMString;        {!!.57}
                                      oContext : TXpNodeList) : Boolean;
var
  oTmpList        : TxpNodeList;
  oTokenList      : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
  sTmp            : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := False;
  if sQuery = '' then
    Exit;

  try
    xptvReverseAxis := False;                                          {!!.57}
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);     {!!.52}
    oTmpList := TXpNodeList.Create;
    try
      if oContext <> nil then
        oTmpList.CopyList(oContext);
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, Self);
      Result := XpConvertBoolean(sTmp, oTmpList, oExpressionType);
    finally
      oTmpList.Free;
      oTokenList.Free;
    end;
  except
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{End !!.52}
{--------}
function TXpNode.SelectInteger(const sQuery : DOMString) : Integer;    {!!.57}
var
  oTokenList      : TXpNodeList;
  oTmpList        : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
  sTmp            : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := 0;
  if sQuery = '' then
    Exit;

  try
    xptvReverseAxis := False;                                          {!!.57}
    oTmpList := TXpNodeList.Create;
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);     {!!.52}
    try
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, Self);
      Result := XpConvertInteger(sTmp, oTmpList, oExpressionType);
    finally
      oTokenList.Free;
      oTmpList.Free;
    end;
  except
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.SelectNodes(const sQuery : DOMString) : TXpNodeList;  {!!.57}
var
  oTokenList      : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpNodeList.Create;
  if sQuery = '' then
    Exit;

  try
    xptvReverseAxis := False;                                          {!!.57}
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);     {!!.52}
    try
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      if oTokenList.Length = 1 then begin
        case TXpXqlToken(oTokenList.Item(0)).TokenId of
          TOK_ELEMENT:
            begin
              Result.Free;
              Result := noGetChildNodesByNodeTypeName
                          (ELEMENT_NODE, TXpXqlToken(oTokenList.Item(0)).NodeName);
            end;
          TOK_SLASH:
            Result.Add(noOwnerDocument);                               {!!.51}
          else
            noParseOrExpr(Result, oTokenList, wTokenIndex,
                          oExpressionType, Self);
        end;
      end else
        noParseOrExpr(Result, oTokenList, wTokenIndex, oExpressionType, Self);
    finally
      oTokenList.Free;
    end;
  except
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.SelectNumber(const sQuery : DOMString) : Double;      {!!.57}
begin
  Result := 0;
  if sQuery = '' then
    Exit;

  try
    Result := SelectNumberContext(sQuery, nil);
  except
  end;
end;
{--------}
function TXpNode.SelectNumberContext(const sQuery : DOMString;         {!!.57}
                                        oContext : TXpNodeList) : Double;
var
  oTokenList      : TXpNodeList;
  oTmpList        : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
  sTmp            : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := 0;
  if sQuery = '' then
    Exit;

  try
    xptvReverseAxis := False;                                          {!!.57}
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);     {!!.52}
    oTmpList := TXpNodeList.Create;
    try
      if oContext <> nil then
        oTmpList.CopyList(oContext);
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, Self);
      Result := XpConvertNumber(sTmp, oTmpList, oExpressionType);
    finally
      oTokenList.Free;
      oTmpList.Free;
    end;
  except
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpNode.SelectSingleNode(const sQuery : DOMString) : TXpElement; {!!.57}
var
  oList : TXpNodeList;
begin
  Result := nil;
  oList := SelectNodes(sQuery);
  if oList.Length > 0 then
    Result := TXpElement(oList.Item(0));
  oList.Free;
end;
{Begin !!.52}
{--------}
function TXpNode.SelectString(const sQuery : DOMString) : DOMString;   {!!.57}
begin
  Result := '';
  if sQuery = '' then
    Exit;

  try
    Result := SelectStringContext(sQuery, nil);
  except
  end;
end;
{--------}
function TXpNode.SelectStringContext(const sQuery : DOMString;         {!!.57}
                                     oContext : TXpNodeList) : DOMString;
var
  oTmpList        : TXpNodeList;
  oTokenList      : TXpNodeList;
  wTokenIndex     : Integer;
  oExpressionType : TXpExpressionType;
  sTmp            : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := '';
  if sQuery = '' then
    Exit;

  try
    xptvReverseAxis := False;                                          {!!.57}
    oTokenList := noOwnerDocument.docXPathParser.Tokenize(sQuery);     {!!.52}
    oTmpList := TXpNodeList.Create;
    try
      if oContext <> nil then
        oTmpList.CopyList(oContext);
      wTokenIndex := - 1;
      oExpressionType := xpetNodeSet;
      sTmp := noParseOrExpr(oTmpList, oTokenList, wTokenIndex,
                            oExpressionType, Self);
      Result := XpConvertString(sTmp, oTmpList, oExpressionType, False);
    finally
      oTmpList.Free;
      oTokenList.Free;
    end;
  except
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{End !!.52}
procedure TXpNode.SetnoOwnerDocument(const Value: TXpDocument);
begin
  fnoOwnerDocument := Value;
  if assigned(noAttributes) then
    noAttributes.noOwnerDocument := Value;
  if assigned(noNamespaceList) then
    noNameSpaceList.noOwnerDocument := Value;
end;

{=====================================================================}

{== TXpCharacterData =================================================}
procedure TXpCharacterData.AppendData(const sData : DOMString);        {!!.57}
begin
  noNodeValue := noNodeValue + sData;
end;
{--------}
procedure TXpCharacterData.DeleteData(wOffset, wCount : Integer);
begin
  Delete(noNodeValue, Succ(wOffset), wCount);
end;
{--------}
function TXpCharacterData.cdGetLength : Integer;
begin
  Result := System.Length(noNodeValue);                                {!!.51}
end;
{--------}
procedure TXpCharacterData.InsertData(wOffset : Integer;
                                      const sData   : DOMString);      {!!.57}
begin
  Insert(sData, noNodeValue, Succ(wOffset));
end;
{--------}
procedure TXpCharacterData.ReplaceData(wOffset,
                                       wCount  : Integer;
                                       const sData   : DOMString);     {!!.57}
begin
  Delete(noNodeValue, Succ(wOffset), wCount);
  Insert(sData, noNodeValue, Succ(wOffset));
end;
{--------}
function TXpCharacterData.SubStringData(wOffset,
                                        wCount  : Integer) : DOMString;
begin
  Result := XpCopy(noNodeValue, Succ(wOffset), wCount);
end;
{=====================================================================}


{== TXpDocumentFragment ==============================================}
function TXpDocumentFragment.CloneNode(bDeep : Boolean) : TXpNode;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpDocumentFragment.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpDocumentFragment.Create;
begin
  inherited Create;
  noNodeType := DOCUMENT_FRAGMENT_NODE;
end;
{====================================================================}


{===TXpComment======================================================}
constructor TXpComment.Create;
begin
  inherited Create;
  noNodeType := COMMENT_NODE;
end;
{--------}
function TXpComment.CloneNode(bDeep : Boolean) : TXpNode;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpComment.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{Begin !!.53}
{--------}
procedure TXpComment.noOutput(oMem   : TMemoryStream;
                              wLevel : Integer);
var
  sText : DOMString;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  if (Assigned(noOwnerDocument)) and noOwnerDocument.docFormattedOutput then
    noOutputIndent(oMem, wLevel);
{Begin !!.55}
  { Need to replace line breaks? }
  if noOwnerDocument.docMustReplaceLineBreaks then
    sText := '<!--' +
             XpStringReplaceAll(NodeValue, #10, noOwnerDocument.docLineBreakChars) +
             '-->'
  else
   sText := '<!--' + NodeValue + '-->';
{End !!.55}
  oMem.write(PWideChar(sText)^, System.Length(sText) * 2);             {!!.58}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{End !!.53}
{Begin !!.55}
{--------}
function TXpComment.noGetStringValue : DOMString;
begin
  Result := noNodeValue;
end;
{End !!.55}
{====================================================================}


{===TXpProcessingInstructioon=======================================}
function TXpProcessingInstruction.CloneNode(bDeep : Boolean) : TXpNode;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpProcessingInstruction.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  if bDeep and HasChildNodes then
    for i := 0 to PRed(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpProcessingInstruction.Create;
begin
  inherited Create;
  noNodeType := PROCESSING_INSTRUCTION_NODE;
end;
{Begin !!.55}
{--------}
function TXpProcessingInstruction.noGetStringValue : DOMString;
begin
  Result := noNodeValue;
end;
{End !!.55}
{=====================================================================}

{== TXpEntityReference ===============================================}
function TXpEntityReference.CloneNode(bDeep : Boolean) : TXpNode;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpEntityReference.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpEntityReference.Create;
begin
  inherited Create;
  noNodeType := ENTITY_REFERENCE_NODE;
end;
{=====================================================================}

{== TXpNamedNodeMap ==================================================}
procedure TXpNamedNodeMap.SetnoOwnerDocument(const Value: TXpDocument);
var
  i : Integer;
begin
  inherited;
  for i := 0 to nmList.Count - 1 do
    TXpNode(nmList[i]).noOwnerDocument := Value;
end;

function TXpNamedNodeMap.nmCloneNamedNodeMap : TXpNamedNodeMap;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpNamedNodeMap.Create;
  for i := 0 to Pred(nmList.Count) do begin
    oNewNode := TXpNode(nmList[i]).CloneNode(True);
    Result.Add(oNewNode);
    oNewNode.Release;
  end;
end;
{--------}
function TXpNamedNodeMap.nmGetLength : Integer;
begin
  Result := nmList.Count;
end;
{--------}
procedure TXpNamedNodeMap.Add(oNode : TXpNode);
begin
  Assert((oNode.noOwnerDocument = nil)
    or (TObject(oNode.noOwnerDocument) is TXpDocument));
  oNode.AddRef;
  nmList.Add(oNode);
  Assert((oNode.noOwnerDocument = nil)
    or (TObject(oNode.noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpNamedNodeMap.Create;
begin
  inherited Create;
  nmList := TList.Create;
end;
{--------}
procedure TXpNamedNodeMap.Delete(wIndex : Integer);
begin
  if wIndex < nmList.Count then begin
    TXpNode(nmList[wIndex]).Release;
    nmList.Delete(wIndex);
  end;
end;
{--------}
destructor TXpNamedNodeMap.Destroy;
begin
  Empty;
  nmList.Free;
  inherited Destroy;
end;
{--------}
procedure TXpNamedNodeMap.Empty;
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(nmList.Count) do
    TXpNode(nmList[Idx]).Release;
  nmList.Clear;
end;
{--------}
function TXpNamedNodeMap.GetNamedItem(const sName : DOMString) : TXpNode; {!!.57}
var
  i     : Integer;
  oNode : TXpNode;
begin
  assert(Pointer(Self) <> nil);
  assert(TObject(Self) is TXpNamedNodeMap);
  Result := nil;
  for i := 0 to Pred(nmList.Count) do begin
    oNode := TXpNode(nmList[i]);
    if (sName = '*') or
       oNode.noNameEquals(sName) then begin
      Result := nmList[i];
      Exit;
    end;
  end;
end;
{--------}
function TXpNamedNodeMap.GetNamedItemNS(const sNamespaceURI,
                                        sLocalName     : DOMString)    {!!.57}
                                                       : TXpNode;
var
  bIgnoreCase : Boolean;
  i     : Integer;
  oNode : TXpNode;
  sUNamespaceURI,
  sULocalName : DOMString;
begin
  Result := nil;
  bIgnoreCase := False;
  if (sNamespaceURI <> '') and
     (nmList.Count > 0) then begin
    bIgnoreCase := TXpNode(nmList[0]).noOwnerDocument.docIgnoreCase;
    if bIgnoreCase then begin
      sUNamespaceURI := UpperCase(sNamespaceURI);
      sULocalName := UpperCase(sLocalName);
    end;
  end;

  for i := 0 to Pred(nmList.Count) do begin
    oNode := TXpNode(nmList[i]);
    if (sNamespaceURI <> '') then begin
      if ((sNamespaceURI = oNode.NamespaceURI) and
          (sLocalName = oNode.LocalName))
         or (bIgnoreCase and
             (sUNamespaceURI = UpperCase(oNode.NamespaceURI)) and
             (sULocalName = UpperCase(oNode.LocalName))) then begin
        Result := nmList[i];
        Result.AddRef;
        Break;
      end
    end
    else if oNode.noNameEquals(sLocalName) then begin
        Result := nmList[i];
        Result.AddRef;
        Break;
    end;
  end;
end;
{--------}
function TXpNamedNodeMap.IndexOf(oNode : TXpNode) : Integer;
begin
  Result := nmList.IndexOf(oNode);
end;
{--------}
procedure TXpNamedNodeMap.Insert(wIndex : Integer; oNode : TXpNode);
begin
  oNode.AddRef;
  nmList.Insert(wIndex, oNode);
end;
{--------}
function TXpNamedNodeMap.Item(wIndex : Integer) : TXpNode;
begin
  Result := nil;
  if (wIndex >= 0) and (wIndex < nmList.Count) then
    Result := TXpNode(nmList[wIndex]);
  Assert((Result.noOwnerDocument = nil)
    or (TObject(Result.noOwnerDocument) is TXpDocument));
end;
{$IFDEF NodeMemoryUsageEnabled}
{--------}
function TXpNamedNodeMap.MemoryUsed : Longint;
var
  Inx : Longint;
begin
  Result := SizeOf(nmList) + SizeOf(fNoOwnerDocument);
  for Inx := 0 to Pred(nmList.Count) do
    Result := Result + TXpNode(nmList[Inx]).MemoryUsed;
end;
{$ENDIF}
{--------}
function TXpNamedNodeMap.RemoveNamedItem(const sName : DOMString) : TXpNode; {!!.57}
var
  i     : Integer;
  oNode : TXpNode;
begin
  Result := nil;
  for i := 0 to Pred(nmList.Count) do begin
    oNode := TXpNode(nmList[i]);
    if oNode.noNameEquals(sName) then begin
      Result := nmList[i];
      Result.AddRef;
      Delete(i);
      Exit;
    end;
  end;
end;
{--------}
function TXpNamedNodeMap.RemoveNamedItemNS(const sNamespaceURI,        {!!.57}
                                           sName : DOMString) : TXpNode;
var
  i     : Integer;
  oNode : TXpNode;
begin
  Result := nil;
  for i := 0 to Pred(nmList.Count) do begin
    oNode := TXpNode(nmList[i]);
    if (sNamespaceURI <> '') then
      if (sNamespaceURI = oNode.NamespaceURI) and
         (sName = oNode.LocalName) then begin
        Result := nmList[i];
        Result.AddRef;
        Delete(i);
        Exit;
      end
    else if oNode.noNameEquals(sName) then begin
        Result := nmList[i];
        Result.AddRef;
        Delete(i);
        Exit;
    end;
  end;
end;
{--------}
procedure TXpNamedNodeMap.Replace(wIndex : Integer; oNode : TXpNode);
begin
  TXpNode(nmList[wIndex]).Release;
  oNode.AddRef;
  nmList[wIndex] := oNode;
end;
{--------}
function TXpNamedNodeMap.SetNamedItem(oNode : TXpNode) : TXpNode;
begin
  Result := RemoveNamedItem(oNode.NodeName);
  Add(oNode);
end;
{====================================================================}



{===TXpAttribute====================================================}
function TXpAttribute.CloneNode(bDeep : Boolean) : TXpNode;
var
  oNode    : TXpAttribute;
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  oNode := TXpAttribute.Create;
  oNode.noNodeType := noNodeType;
  oNode.noNodeName := noNodeName;
  oNode.noNodeValue := noNodeValue;
  oNode.noNodeID := noNodeID;
  oNode.noOwnerDocument := noOwnerDocument;
  oNode.noDefaultNameSpace := noDefaultNameSpace;
  oNode.atSpecified := atSpecified;
  Result := oNode;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpAttribute.Create;
begin
  inherited Create;
  noNodeType := ATTRIBUTE_NODE;
  atSpecified := True;
end;
{--------}
destructor TXpAttribute.Destroy;
begin
  inherited Destroy;
end;
{--------}
function TXpAttribute.atGetOwnerElement : TXpElement;
begin
  Result := TXpElement(noParentNode);
end;
{Begin !!.55}
{--------}
function TXpAttribute.noGetStringValue : DOMString;
begin
  Result := noNodeValue;
end;
{End !!.55}
{====================================================================}


{===TXpElement======================================================}
function TXpElement.elAttribValueEquals(const sValue,
                                              sValueSought : DOMString) : boolean;
begin
  Result := (sValue = sValueSought) or
            ( assigned(noOwnerDocument) and
              noOwnerDocument.docIgnoreCase and
              (UpperCase(sValue) = UpperCase(sValueSought)));
end;
{--------}
function TXpElement.elAttribValueIn(const sValue : DOMString;
                                          oValues : TStringList) : Boolean;
begin
  { Note: ID search is case-sensitive. We do not take document's IgnoreCase
    property into account. }
  Result := (oValues.IndexOf(sValue) > -1);
end;
{--------}
function TXpElement.elGetElementText : DOMString;
begin
  Result := elGetImmediateChildText(FirstChild);
end;
{--------}
function TXpElement.elGetImmediateChildText(oNode : TXpNode)
                                                  : DOMString;
var
  sTerm : DOMString;
begin
  Result := '';
  while Assigned(oNode) do begin
    if oNode is TXpText then begin
      sTerm := TXpText(oNode).Data;
      if Result <> '' then
        Result := Result + ' ' + sTerm
      else
        Result := sTerm;
    end;
    oNode := oNode.noGetNextSibling;
  end;
end;
{--------}
function TXpElement.elIsNthOccurrence(N : Integer) : Boolean;
var
  anInx : Integer;
  aNode : TXpNode;
begin
  Result := False;
  if N < 1 then
    Exit;
  anInx := 0;
  aNode := noParentNode.FirstChild;
  while aNode <> nil do begin
    { Is this the same node or does it have the same node name? }
    if (aNode = Self) or
       aNode.noNameEquals(noNodeName) then
      { Yes. Increment the counter. }
      inc(anInx);

    { Is this the same node? }
    if (aNode = Self) then begin
      { Yes. Does the counter match the expected position? }
      Result := (anInx = N);
      exit;
    end
    else
      { Not the same node. See if we have passed the expected position. }
      if (anInx > N) then
        Exit;
    { Move to the next sibling. }
    aNode := aNode.NextSibling;
  end;  { while }
end;
{Begin !!.53}
{--------}
procedure TXpElement.elStripWhitespaceNodesPrim(bPreserve : Boolean);
var
  oNode : TXpNode;
  sAttrValue : DOMString;
  wInx : Integer;
begin
  { Is there an xml:space attribute on this element? }
  sAttrValue := GetAttribute(XpsXMLSpace);
  if sAttrValue <> '' then
    bPreserve := (sAttrValue = XpsPreserve);

  for wInx := Pred(ChildNodes.Length) downto 0 do begin
    oNode := ChildNodes.Item(wInx);
    { Is this a text node containing whitespace only? }
    if (not bPreserve) and
       (oNode.NodeType = TEXT_NODE) and
       TXpText(oNode).IsWhitespaceNode then
      { Yes. Eliminate this node. }
      ChildNodes.Delete(wInx)
    { No. Are we to strip recursively and we have just encountered
      another element? }
    else if (oNode.NodeType = ELEMENT_NODE) and
            (oNode.NodeName <> XpsXSLText) then
      { Yes. Tell it to strip its whitespace nodes. }
      TXpElement(oNode).elStripWhitespaceNodesPrim(bPreserve);
  end;  { for }
end;
{End !!.53}
{--------}
function TXpElement.elTraverseTreeForElement(oNode : TXpNode;
                                             const sName : DOMString)  {!!.57}
                                                   : TXpElement;
begin
  Result := nil;
  while Assigned(oNode) do begin
    if (oNode.NodeType = ELEMENT_NODE) and oNode.noNameEquals(sName) then begin
      Result := TXpElement(oNode);
      Exit;
    end;
    if oNode.HasChildNodes then begin
      Result := elTraverseTreeForElement(oNode.FirstChild, sName);
      if Assigned(Result) then
        Exit;
    end;
    oNode := oNode.NextSibling;
  end;
end;
{--------}
procedure TXpElement.elTraverseTreeForElements(oList         : TXpNodeList;
                                               oNode         : TXpNode;
                                               const sName,            {!!.57}
                                               sNamespaceURI : DOMString);
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  while Assigned(oNode) do begin
    if ((oNode.NodeType = ELEMENT_NODE) and
        (((sName = '*') or
          (oNode.LocalName = sName)) and                               {!!.57 - Start}
         ((sNamespaceURI = '*') or
          (sNamespaceURI = oNode.NamespaceURI))) or                    {!!.57 - End}
       ((Assigned(noOwnerDocument)) and
        (noOwnerDocument.docIgnoreCase) and
        (UpperCase(oNode.LocalName) = UpperCase(sName))) or
       ((XpPos(':*', sName) > 0) and
        (XpCopy(sName, 1, XpPos(':*', sName) - 1) = oNode.Prefix))) then
      oList.Add(oNode);
    if oNode.HasChildNodes then
      elTraverseTreeForElements(oList,
                                oNode.FirstChild,
                                sName,
                                sNamespaceURI);
    oNode := oNode.NextSibling;
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
procedure TXpElement.elTraverseTreeForElementsWithAttribute(oList  : TXpNodeList;
                                                            oNode  : TXpNode;
                                                            const sName,  {!!.57}
                                                            sAttr,
                                                            sValue : DOMString);
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  while Assigned(oNode) do begin
    if (oNode.NodeType = ELEMENT_NODE) and
       ((sName = '*') or oNode.noNameEquals(sName)) then begin
      if (TXpElement(oNode).GetAttribute(sAttr) = sValue) or
         ((Assigned(noOwnerDocument)) and
          noOwnerDocument.docIgnoreCase and
          (UpperCase(TXpElement(oNode).GetAttribute(sAttr)) = UpperCase(sValue))) then
        oList.Add(oNode);
    end;
    if oNode.HasChildNodes then
      elTraverseTreeForElementsWithAttribute(oList,
                                             oNode.FirstChild,
                                             sName,
                                             sAttr,
                                             sValue);
    oNode := oNode.NextSibling;
  end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
procedure TXpElement.elTraverseTreeForElementsWithIDs(oList : TXpNodeList;
                                                      oNode : TXpNode;
                                                      const sAttr : DOMString; {!!.57}
                                                      oIDs  : TStringList);
begin
  while Assigned(oNode) do begin
    if (oNode.NodeType = ELEMENT_NODE) then begin
      if elAttribValueIn(TXpElement(oNode).GetAttribute(sAttr), oIDs) then
        oList.Add(oNode);
    end;
    if (oList.Length < oIDs.Count) and oNode.HasChildNodes then
      elTraverseTreeForElementsWithIDs(oList, oNode.FirstChild,
                                       sAttr, oIDs);
    { Have we found all that was sought? }
    if (oList.Length = oIDs.Count) then
      { Yes. Break out of here. }
      break;
    oNode := oNode.NextSibling;
  end;
end;
{--------}
function TXpElement.CloneNode(bDeep : Boolean) : TXpNode;
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := noOwnerDocument.docElementFactory.CreateElement(noNodeName); {!!.52}
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpElement(Result).elIgnoreEndTag := elIgnoreEndTag;
  TXpElement(Result).elFullEndTag := elFullEndTag;
  if Assigned(noAttributes) then
    Result.noAttributes := noAttributes.nmCloneNamedNodeMap;
  if Assigned(noNamespaceList) then
    Result.noNamespaceList := noNamespaceList.nmCloneNamedNodeMap;
  if bDeep and HasChildNodes then
    for i := 0 to noChildNodes.Length - 1 do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpElement.Create;
begin
  inherited Create;
  noNodeType := ELEMENT_NODE;
  elIgnoreEndTag := False;
  elFullEndTag := False;
end;
{--------}
function TXpElement.CreateChildCDataSection(const sText: DOMString) : TXpCDataSection; {!!.57}
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := noOwnerDocument.CreateCDataSection(sText);
  AppendChild(Result);
  Result.Release;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpElement.CreateChildElement(const sElem: DOMString) : TXpElement; {!!.57}
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := noOwnerDocument.CreateElement(sElem);
  AppendChild(Result);
  Result.Release;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpElement.CreateChildText(const sText: DOMString) : TXpText; {!!.57}
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := noOwnerDocument.CreateTextNode(sText);
  AppendChild(Result);
  Result.Release;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpElement.FindElement(const sName : DOMString) : TXpElement; {!!.57}
begin
  Result := elTraverseTreeForElement(FirstChild, sName);
end;
{--------}
function TXpElement.GetAttribute(const sName : DOMString) : DOMString; {!!.57}
var
  oNode : TXpNode;
  i     : Integer;
begin
  assert(Pointer(Self) <> nil);
  assert(TObject(Self) is TXpElement);
  Result := '';
  if Assigned(noAttributes) then begin
    oNode := noAttributes.GetNamedItem(sName);
    if Assigned(oNode) then begin
      Result := oNode.NodeValue;
      if Result = '' then
        for i := 0 to Pred(oNode.noChildNodes.nlList.Count) do begin
          if TXpNode(oNode.noChildNodes.nlList[i]).noNodeName = sName then
            Result := TXpNode(oNode.noChildNodes.nlList[i]).noNodeValue;
        end;
    end;
  end;
end;
{Begin !!.52}
{--------}
function TXpElement.GetAttributeInt(const sName : DOMString) : Integer;
var
  sTmp : DOMString;
begin
  Result := 0;
  sTmp := GetAttribute(sName);
  if sTmp <> '' then
    try
      Result := StrToInt(sTmp);
    except
      raise EXpException.Create(sAttrNotNum);
    end;
end;
{End !!.52}
{--------}
function TXpElement.GetAttributeNode(const sName : DOMString) : TXpAttribute; {!!.57}
begin
  assert(TObject(self) is TXpElement);
  Result := nil;
  if Assigned(noAttributes) then begin
    Result := TXpAttribute(noAttributes.GetNamedItem(sName));
    assert((Result = nil) or (TObject(Result) is TXpAttribute));
  end;
end;
{--------}
function TXpElement.GetAttributeNodeNS(const sNamespaceURI,            {!!.57}
                                       sLocalName     : DOMString)
                                                      : TXpAttribute;
begin
  Result := nil;
  if Assigned(noAttributes) then
    Result := TXpAttribute(noAttributes.GetNamedItemNS(sNamespaceURI,
                                                       sLocalName));
end;
{--------}
function TXpElement.GetAttributeNS(const sNamespaceURI,                {!!.57}
                                   sLocalName    : DOMString) : DOMString;
var
  oNode : TXpNode;
begin
  Result := '';
  if Assigned(noAttributes) then begin
    oNode := noAttributes.GetNamedItemNS(sNamespaceURI, sLocalName);
    if Assigned(oNode) then
      Result := oNode.NodeValue;
  end;
end;
{--------}
function TXpElement.GetChildElementsByTagName(const sName : DOMString) : TXpNodeList; {!!.57}
var
  oNode : TXpNode;
begin
  Result := TXpNodeList.Create;
  oNode := FirstChild;
  while Assigned(oNode) do begin
    if (oNode.NodeType = ELEMENT_NODE) and
       ((sName = '*') or oNode.noNameEquals(sName) or
       ((XpPos(':*', sName) > 0) and
        (XpCopy(sName, 1, XpPos(':*', sName) - 1) = oNode.Prefix))) then
      Result.Add(oNode);
    oNode := oNode.NextSibling;
  end;
end;
{--------}
function TXpElement.GetElementsByTagName(const sName : DOMString) : TXpNodeList; {!!.57}
begin
  Result := TXpNodeList.Create;
  elTraverseTreeForElements(Result, FirstChild, sName, '*');           {!!.57}
end;
{--------}
function TXpElement.GetElementsByTagNameNS(const sNamespaceURI,        {!!.57}
                                           sLocalName    : DOMString)
                                                         : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  elTraverseTreeForElements(Result,
                            FirstChild,
                            sLocalName,
                            sNamespaceURI);
end;
{--------}
function TXpElement.GetElementsByTagNameWithAttribute(const sName,          {!!.57}
                                                      sAttr,
                                                      sValue : DOMString)
                                                             : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  elTraverseTreeForElementsWithAttribute(Result,
                                         FirstChild,
                                         sName,
                                         sAttr,
                                         sValue);
end;
{--------}
function TXpElement.GetElementsWithIDs(const sAttr,
                                       sValue : DOMString) : TXpNodeList; {!!.57}
var
  bInWhite : boolean;
  sID  : string;
  iInx : Integer;
  iLen : Integer;
  oIDs : TStringList;
begin
  Result := TXpNodeList.Create;
  oIDs := nil;
  try
    oIDs := TStringList.Create;

    { The ID function accepts a whitespace-separated list of IDs. Break the
      string down into its IDs. }
    iLen := Length(sValue);
    iInx := 1;
    sID := '';
    bInWhite := False;
    while iInx <= iLen do begin
      if not XpIsWhiteSpace(sValue[iInx]) then begin                   {!!.52}
        bInWhite := False;
        sID := sID + sValue[iInx];
      end
      else if (not bInWhite) then begin
        if sID <> '' then begin
          oIDs.Add(sID);
          sID := '';
        end;
        bInWhite := True;
      end;
      inc(iInx);
    end;
    if sID <> '' then
      oIDs.Add(sID);

    elTraverseTreeForElementsWithIDs(Result,
                                     FirstChild,
                                     sAttr,
                                     oIDs);
  finally
    oIDs.Free;
  end;
end;
{--------}
function TXpElement.HasAttribute(const sName : DOMString) : Boolean;   {!!.57}
begin
  Result := (Assigned(noAttributes)) and
            (Assigned(noAttributes.GetNamedItem(sName)));
end;
{Begin !!.53}
{--------}
function TXpElement.noGetBaseURI : DOMString;
begin
  Result := TXpElement(Self).GetAttribute(XpsXMLBase);
  if Result = '' then
      Result := inherited noGetBaseURI;
end;
{--------}
{End !!.53}
{--------}
procedure TXpElement.RemoveAttribute(const sName : DOMString);         {!!.57}
var
  oNode: TXpNode;
begin
  if Assigned(noAttributes) then begin
    oNode := noAttributes.RemoveNamedItem(sName);
    if Assigned(oNode) then
      oNode.Release;
  end;
end;
{--------}
function TXpElement.RemoveAttributeNode(oOldAttr : TXpAttribute) : TXpAttribute;
begin
  Result := nil;
  if Assigned(noAttributes) then
    Result := TXpAttribute(noAttributes.RemoveNamedItem(oOldAttr.name));
end;
{--------}
procedure TXpElement.RemoveAttributeNS(const sNamespaceURI, sLocalName : DOMString); {!!.57}
var
  oNode: TXpNode;
begin
  if Assigned(noAttributes) then begin
    oNode := noAttributes.RemoveNamedItemNS(sNamespaceURI, sLocalName);
    if Assigned(oNode) then
      oNode.Release;
  end;
end;
{--------}
procedure TXpElement.elSetAttributePrim(oAttr : TXpAttribute);
var
  oAttr2 : TXpAttribute;
begin

  { Check for/add namespace }
  if oAttr.Prefix = xpsXmlns then begin
    if noNamespaceList = nil then
      noNamespaceList := TXpNamedNodeMap.Create;
    oAttr2 := TXpAttribute.Create;
    oAttr2.name := oAttr.LocalName;
    oAttr2.Value := oAttr.NodeValue;
    noNamespaceList.Add(oAttr2);
    oAttr2.Release;
  end else if oAttr.NodeName = xpsXmlns then
    noDefaultNameSpace := oAttr.NodeValue;

end;
{--------}
procedure TXpElement.SetAttribute(const sName, sValue : DOMString);    {!!.57}
var
  oAttr    : TXpAttribute;
  oOldNode : TXpNode;
begin
  if noAttributes = nil then
    noAttributes := TXpNamedNodeMap.Create
  else begin
    oOldNode := noAttributes.RemoveNamedItem(sName);
    oOldNode.Free;
  end;

//  if sValue <> '' then begin                                         {!!.53}
    oAttr := TXpAttribute.Create;
    oAttr.noParentNode := Self;
    oAttr.name := sName;
    oAttr.Value := sValue;
    ForceOwnerDocument(oAttr);
    noAttributes.Add(oAttr);
    oAttr.Release;
    elSetAttributePrim(oAttr);
//  end;                                                               {!!.53}
end;
{--------}
function TXpElement.SetAttributeNode(oNewAttr : TXpAttribute) : TXpAttribute;
begin
  if noAttributes = nil then
    noAttributes := TXpNamedNodeMap.Create;

  Result := TXpAttribute(noAttributes.RemoveNamedItem(oNewAttr.name));

  oNewAttr.noParentNode := Self;
  oNewAttr.noOwnerDocument := noOwnerDocument;                         {!!.57}
  noAttributes.Add(oNewAttr);
  elSetAttributePrim(oNewAttr);
end;
{--------}
function TXpElement.SetAttributeNodeNS(oNewAttr : TXpAttribute) : TXpAttribute;
begin
  if noAttributes = nil then
    noAttributes := TXpNamedNodeMap.Create;

  Result := TXpAttribute(noAttributes.RemoveNamedItemNS
                               (oNewAttr.noDefaultNameSpace, oNewAttr.name));

  oNewAttr.noParentNode := Self;
  noAttributes.Add(oNewAttr);
  elSetAttributePrim(oNewAttr);
end;
{--------}
procedure TXpElement.SetAttributeNS(const sNamespaceURI,               {!!.57}
                                    sLocalName,
                                    sValue        : DOMString);
var
  oAttr    : TXpAttribute;
  oOldNode : TXpNode;
begin
  if noAttributes = nil then
    noAttributes := TXpNamedNodeMap.Create;

  oOldNode := noAttributes.RemoveNamedItemNS(sNamespaceURI, sLocalName);
  oOldNode.Free;
//  if sValue <> '' then begin                                         {!!.53}
    oAttr := TXpAttribute.Create;
    oAttr.noParentNode := Self;
    oAttr.noDefaultNameSpace := sNamespaceURI;
    oAttr.name := sLocalName;
    oAttr.Value := sValue;
    ForceOwnerDocument(oAttr);
    noAttributes.Add(oAttr);
    oAttr.Release;
    elSetAttributePrim(oAttr);
//  end;                                                               {!!.53}
end;
{Begin !!.53}
{--------}
procedure TXpElement.StripWhitespaceNodes(bDeep : Boolean);
var
  bPreserve : Boolean;
  oNode : TXpNode;
  sAttrValue : DOMString;
  wInx : Integer;
begin
  { Does this element or one of its ancestors have an xml:space
    attribute with value 'preserve'? }
  oNode := Self;
  bPreserve := False;
  while (oNode <> nil) and (oNode.NodeType <> DOCUMENT_NODE) do begin
    sAttrValue := TXpElement(oNode).GetAttribute(XpsXMLSpace);
    if sAttrValue <> '' then begin
      bPreserve := (sAttrValue = XpsPreserve);
      Break;
    end;
    oNode := oNode.ParentNode;
  end;  { while }

  for wInx := Pred(ChildNodes.Length) downto 0 do begin
    oNode := ChildNodes.Item(wInx);
    { Is this a text node containing whitespace only? }
    if (not bPreserve) and
       (oNode.NodeType = TEXT_NODE) and
       TXpText(oNode).IsWhitespaceNode then
      { Yes. Eliminate this node. }
      ChildNodes.Delete(wInx)
    { No. Are we to strip recursively and we have just encountered
      another element? }
    else if bDeep and
         (oNode.NodeType = ELEMENT_NODE) and
         (oNode.NodeName <> XpsXSLText) then
      { Yes. Tell it to strip its whitespace nodes. }
      TXpElement(oNode).elStripWhitespaceNodesPrim(bPreserve);
  end;  { for }
end;
{End !!.53}
{====================================================================}



{===TXpText=========================================================}
constructor TXpText.Create;
begin
  inherited Create;
  noNodeType := TEXT_NODE;
end;
{--------}
function TXpText.CloneNode(bDeep : Boolean) : TXpNode;
var
  oNode    : TXpText;
  i        : Integer;
  oNewNode : TXpNode;
begin
  oNode := TXpText.Create;
  oNode.noNodeType := noNodeType;
  oNode.noNodeName := noNodeName;
  oNode.noNodeValue := noNodeValue;
  oNode.noNodeID := noNodeID;
  oNode.noOutputEscaping := noOutputEscaping;                          {!!.52}
  oNode.noOwnerDocument := noOwnerDocument;
  oNode.noDefaultNameSpace := noDefaultNameSpace;
  Result := oNode;
  if bDeep and HasChildNodes then
    for i := 0 to noChildNodes.Length - 1 do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
end;
{Begin !!.53}
{--------}
function TXpText.IsWhitespaceNode : Boolean;
var
  wInx : Integer;
begin
  Result := True;
  for wInx := 1 to System.Length(noNodeValue) do begin
    Result := XpIsWhitespace(noNodeValue[wInx]);
    if not Result then
      Break;
  end;  { for }
end;
{--------}
procedure TXpText.noOutput(oMem : TMemoryStream;
                           wLevel : Integer);
var
  sText : DOMString;
begin
  if PreviousSibling is TXpComment then begin
    sText := Chr(13) + Chr(10);
    oMem.write(PWideChar(sText)^, System.Length(sText) * 2);           {!!.58}
  end;
  noOutputXpText(oMem, Data, False);
end;
{End !!.53}
{--------}
function TXpText.SplitText(wOffset : Integer) : TXpText;
var
  oNewText : TXpText;
  oNode : TXpNode;
  sTextA,
  sTextB : DOMString;
  wLen : Integer;
begin
  wLen := System.Length(Data);
  if (wOffset < 0) or (wOffset >= wLen) then
    raise TXpDomException.CreateCode(ecIndexSizeErr);
  oNewText := TXpText.Create;
  ForceOwnerDocument(oNewText);
  sTextA := SubStringData(0, wOffset);
  sTextB := SubStringData(wOffset, wLen - wOffset + 1);
  Data := sTextA;
  oNewText.Data := sTextB;
  oNode := NextSibling;
  if oNode = nil then
    noParentNode.AppendChild(oNewText)
  else
    noParentNode.InsertBefore(oNewText, Self);
  Result := oNewText;
end;
{====================================================================}



{===TXpCDATASection=================================================}
constructor TXpCDATASection.Create;
begin
  inherited Create;
  noNodeType := CDATA_SECTION_NODE;
end;
{--------}
function TXpCDATASection.CloneNode(bDeep : Boolean) : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpCDATASection.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{Begin !!.53}
{--------}
procedure TXpCDATASection.noOutput(oMem   : TMemoryStream;
                                   wLevel : Integer);
var
  NewText : DOMString;                                                 {!!.57 - Start}
  i,
  CurrLen : Integer;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
{Begin !!.55}
  SetLength(NewText, Trunc(System.Length(NodeValue) * 1.25));
  CurrLen := 0;
  { Need to replace line breaks? }
  if noOwnerDocument.docMustReplaceLineBreaks then begin
    i := 1;
    while i <= System.Length(NodeValue) do begin
      if (Char(NodeValue[i]) in [#13, #10]) then begin
        noCheckForLineBreaks(NodeValue, NewText, i, CurrLen);
      end else begin
        XpDOMBufferAppend(NodeValue[i], NewText, CurrLen);
        i := i + 1;
      end;
    end;
  end;
{End !!.55}

  if (Assigned(noOwnerDocument)) and
     (not noOwnerDocument.ActualCDATA) then begin
    XpDOMBufferInsert('<![CDATA[', NewText, 1, CurrLen);
    XpDOMBufferAppend(']]>', NewText, CurrLen);
  end;

  oMem.Write(PWideChar(NewText)^, CurrLen * 2);                      {!!.57 - End}{!!.58}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{End !!.53}
{=====================================================================}

{== TXpDTDElementContent =============================================}
function TXpDTDElementContent.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpDTDElementContent.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDElementContent(Result).ecOccurs := ecOccurs;
  TXpDTDElementContent(Result).ecRelation := ecRelation;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpDTDElementContent.Create;
begin
  inherited Create;
  noNodeType := ELEMENT_DECL_CONTENT_NODE;
end;
{--------}
destructor TXpDTDElementContent.Destroy;
begin
  inherited Destroy;;
end;
{--------}
procedure TXpDTDElement.elSetContentType(aType : Integer);
begin
  if (aType >= CONTENT_UNDECLARED) and
     (aType <= CONTENT_ELEMENTS) then
    elContentType := aType
  else
    raise TXpDomException.CreateCode(ecInvalidParamErr);
end;
{====================================================================}



{===TXpDTDElementGroup==============================================}
function TXpDTDElementGroup.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpDTDElementGroup.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDElementGroup(Result).egOccurs := egOccurs;
  TXpDTDElementGroup(Result).egRelation := egRelation;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpDTDElementGroup.Create;
begin
  inherited Create;
  noNodeType := ELEMENT_DECL_GROUP_NODE;
end;
{--------}
function TXpDTDElementGroup.CreateDTDElementContent(const sName     : DOMString; {!!.57}
                                                    wOccurs,
                                                    wRelation : Integer)
                                                              : TXpDTDElementContent;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpDTDElementContent.Create;
  Result.noOwnerDocument := noOwnerDocument;
  Result.name := sName;
  Result.Occurs := wOccurs;
  Result.Relation := wRelation;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpDTDElementGroup.CreateDTDElementGroup : TXpDTDElementGroup;
begin
  Result := TXpDTDElementGroup.Create;
  Result.noOwnerDocument := noOwnerDocument;
end;
{--------}
destructor TXpDTDElementGroup.Destroy;
begin
  inherited Destroy;;
end;
{====================================================================}



{===TXpDTDElement====================================================}
function TXpDTDElement.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDElement.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDElement(Result).elContentType := elContentType;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDElement.Create;
begin
  inherited Create;
  noNodeType := ELEMENT_DECL_NODE;
end;
{--------}
function TXpDTDElement.CreateDTDElementGroup : TXpDTDElementGroup;
begin
  Result := TXpDTDElementGroup.Create;
  Result.noOwnerDocument := noOwnerDocument;
end;
{--------}
destructor TXpDTDElement.Destroy;
begin
  inherited Destroy;
end;
{====================================================================}



{===TXpDTDAttDefinition=============================================}
function TXpDTDAttDefinition.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDAttDefinition.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDAttDefinition(Result).adAttType := adAttType;
  TXpDTDAttDefinition(Result).adDefaultType := adDefaultType;
  TXpDTDAttDefinition(Result).adEnumeration.Assign(adEnumeration);
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDAttDefinition.Create;
begin
  inherited Create;
  adEnumeration := TStringList.Create;
  noNodeType := ATT_DEF_DECL_NODE;
end;
{--------}
destructor TXpDTDAttDefinition.Destroy;
begin
  adEnumeration.Free;
  inherited Destroy;
end;
{====================================================================}



{===TXpDTDAttList===================================================}
function TXpDTDAttlist.CloneNode(bDeep: Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDAttList.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDAttlist.Create;
begin
  inherited Create;
  noNodeType := ATTLIST_DECL_NODE;
end;
{--------}
function TXpDTDAttlist.CreateDTDAttDefinition(const sName        : DOMString; {!!.57}
                                              wAttrType    : Integer;
                                              oEnumeration : TStringList;
                                              wValueType   : Integer;
                                              const sValue       : DOMString) {!!.57}
                                                           : TXpDTDAttDefinition;
begin
  if sName <> '' then begin
    Result := TXpDTDAttDefinition.Create;
    Result.Name := sName;
    Result.AttType := wAttrType;
    if Assigned(oEnumeration) then
      Result.Enumeration.Assign(oEnumeration)
    else begin                                                         {!!.55 - Start}
      Result.adEnumeration.Free;
      Result.adEnumeration := nil;
    end;                                                               {!!.55 - End}
    Result.DefaultType := wValueType;
    Result.DefaultValue := sValue;
    if Assigned(Result) then
      Result.noOwnerDocument := noOwnerDocument;
    if Assigned(noOwnerDocument) then
      noOwnerDocument.docIdAttribute := sName;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
destructor TXpDTDAttlist.Destroy;
begin
  inherited Destroy;
end;
{====================================================================}



{===TXpDTDEntity====================================================}
function TXpDTDEntity.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDEntity.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDEntity(Result).enIsPE := enIsPE;
  TXpDTDEntity(Result).enNotationName := enNotationName;
  TXpDTDEntity(Result).enPublicID := enPublicID;
  TXpDTDEntity(Result).enSystemID := enSystemID;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDEntity.Create;
begin
  inherited Create;
  noNodeType := ENTITY_DECL_NODE;
end;
{--------}
destructor TXpDTDEntity.Destroy;
begin
  inherited Destroy;
end;
{====================================================================}



{===TXpDTDNotation==================================================}
function TXpDTDNotation.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDNotation.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDNotation(Result).noPublicID := noPublicID;
  TXpDTDNotation(Result).noSystemID := noSystemID;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDNotation.Create;
begin
  inherited Create;
  noNodeType := NOTATION_DECL_NODE;
end;
{--------}
destructor TXpDTDNotation.Destroy;
begin
  inherited Destroy;
end;
{====================================================================}



{===TXpDocumentType=================================================}
function TXpDocumentType.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  Result := TXpDocumentType.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDocumentType(Result).dtIsExternalDTD := dtIsExternalDTD;
  if Assigned(dtExternalDTD) and bDeep then
    TXpDocumentType(Result).dtExternalDTD :=
      TXpDocumentType(dtExternalDTD.CloneNode(True));
  TXpDocumentType(Result).dtPublicID := dtPublicID;
  TXpDocumentType(Result).dtSystemID := dtSystemID;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpDocumentType.Create;
begin
  inherited Create;
  noNodeType := DOCUMENT_type_NODE;
  dtExternalDTD := nil;
  dtIsExternalDTD := false;
end;
{--------}
function TXpDocumentType.CreateDTDAttlist(const sName : DOMString) : TXpDTDAttlist; {!!.57}
begin
  if sName <> '' then begin
    Result := TXpDTDAttlist.Create;
    Result.name := sName;
    Result.noOwnerDocument := noOwnerDocument;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocumentType.CreateDTDConditional(wType : Integer)
                                                    : TXpDTDConditional;
begin
  Result := TXpDTDConditional.Create;
  Result.CondType := wType;
  Result.noOwnerDocument := noOwnerDocument;
end;
{--------}
function TXpDocumentType.CreateDTDElement(sName        : DOMString;
                                          wContentType : Integer)
                                                       : TXpDTDElement;
begin
  if sName <> '' then begin
    Result := TXpDTDElement.Create;
    Result.name := sName;
    Result.ContentType := wContentType;
    Result.noOwnerDocument := noOwnerDocument;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocumentType.CreateDTDEntity(const sName : DOMString)      {!!.57}
                                               : TXpDTDEntity;
begin
  if sName <> '' then begin
    Result := TXpDTDEntity.Create;
    Result.name := sName;
    Result.noOwnerDocument := noOwnerDocument;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocumentType.CreateDTDNotation(const sName : DOMString) : TXpDTDNotation; {!!.57}
begin
  if sName <> '' then begin
    Result := TXpDTDNotation.Create;
    Result.name := sName;
    Result.noOwnerDocument := noOwnerDocument;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
destructor TXpDocumentType.Destroy;
begin
  dtExternalDTD.Free;
  inherited Destroy;
end;
{Begin !!.53}
{--------}
function TXpDocumentType.noGetNodesByNodeTypeName(wType : Integer;
                                            const sName : DOMString) : TXpNodeList;
var
  oList : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  try
    noTraverseTreeForNamedNodes(Result, FirstChild, sName, wType);
    if dtExternalDTD <> nil then begin
      oList := dtExternalDTD.noGetNodesByNodeTypeName(wType, sName);
      Result.CopyList(oList);
      oList.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{End !!.53}
{====================================================================}



{===TXpDTDConditional===============================================}
function TXpDTDConditional.CloneNode(bDeep : Boolean) : TXpNode;
{Begin !!.57}
var
  i        : Integer;
  oNewNode : TXpNode;
begin
  Result := TXpDTDConditional.Create;
  Result.noNodeType := noNodeType;
  Result.noNodeName := noNodeName;
  Result.noNodeValue := noNodeValue;
  Result.noNodeID := noNodeID;
  Result.noOwnerDocument := noOwnerDocument;
  Result.noDefaultNameSpace := noDefaultNameSpace;
  TXpDTDConditional(Result).dcCondType := dcCondType;
  if bDeep and HasChildNodes then
    for i := 0 to Pred(noChildNodes.Length) do begin
      oNewNode := noChildNodes.Item(i).CloneNode(true);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
{End !!.57}
end;
{--------}
constructor TXpDTDConditional.Create;
begin
  inherited Create;
  noNodeType := CONDITIONAL_DECL_NODE;
end;
{--------}
destructor TXpDTDConditional.Destroy;
begin
  inherited Destroy;
end;
{====================================================================}

type
  TXpParserCracker = class(TXpParser);

{===TXpDocument=====================================================}
function TXpDocument.docGetDocType : TXpDocumentType;
var
  oNode: TXpNode;
begin
  oNode := FirstChild;
  while (Assigned(oNode)) and (oNode.NodeType <> DOCUMENT_TYPE_NODE) do
    oNode := oNode.NextSibling;
  if oNode = nil then
    Result := nil
  else
    Result := TXpDocumentType(oNode);
end;
{--------}
function TXpDocument.docGetDocumentElement : TXpElement;
{Rewritten !!.53}
begin
  Result := TXpElement(FirstChild);
  while Assigned(Result) and (Result.NodeType <> ELEMENT_NODE) do
    Result := TXpElement(Result.NextSibling);
end;
{--------}
function TXpDocument.docGetXpDTD : DOMString;
begin
  Result := '';
  if Assigned(DocType) then
    Result := DocType.XmlDocument;
end;
{Begin !!.55}
{--------}
function TXpDocument.docLineBreakChars : DOMString;
begin
  case docLineBreakMode of
    lbmDefault : Result := XpsLineBreak;
    lbmCR : Result := #13;
    lbmCRLF : Result := #13#10;
    lbmLF : Result := #10;
  end;  { case }
end;
{--------}
function TXpDocument.docMustReplaceLineBreaks : Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (docLineBreakMode <> lbmLF);
{$ELSE}
  Result := (docLineBreakMode <> lbmDefault) and
            (docLineBreakMode <> lbmLF);
{$ENDIF}
end;
{End !!.55}
{--------}
procedure TXpDocument.docTraverseTreeForElements(oList         : TXpNodeList;
                                                 oNode         : TXpNode;
                                                 const sName,          {!!.57}
                                                 sNamespaceURI : DOMString);
{Rewritten !!.52}
var
  bAdd,
  bHasURI,
  bWildcardName : Boolean;
begin
  bAdd := False;
  bHasURI := (sNamespaceURI <> '');
  bWildcardName := (sName = '*');

  while Assigned(oNode) do begin
    if (oNode.NodeType = ELEMENT_NODE) then begin
      { URI specified? }
      if bHasURI then begin
        { Yes. See if matches requirements. }
        bAdd := ((sNamespaceURI = '*') or (sNamespaceURI =  oNode.NamespaceURI))
                and
                (bWildcardName or (oNode.LocalName = sName));
      end
      else begin
        { No URI. Does sName contain a wildcarded URI? }
        if XpPos(':*', sName) > 0 then
          { Yes. See if prefixes match. }
          bAdd := (XpCopy(sName, 1, XpPos(':*', sName) - 1) = oNode.Prefix)
        else
          { No. See if name is wildcarded or matches. }
          bAdd := bWildcardName or oNode.noNameEquals(sName);          {!!.55}
      end;
    end;
    if bAdd then begin
      oList.Add(oNode);
      bAdd := False;
    end;

    if oNode.HasChildNodes then
      docTraverseTreeForElements(oList,
                                 oNode.FirstChild,
                                 sName,
                                 sNameSpaceURI);
    oNode := oNode.NextSibling;
  end;  { while }
end;
{--------}
function TXpDocument.CloneNode(bDeep : Boolean) : TXpNode;
begin
  Result := nil;
end;
{--------}
constructor TXpDocument.Create;
begin
  inherited Create;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  noNodeType := DOCUMENT_NODE;
  docDomImplementation := TXpDomImplementation.Create;
{Begin !!.52}
{$IFDEF XPDPRO}
  docElementFactory := TXpElementFactory.FindFactory(XpsXSLFactoryID);
  if docElementFactory = nil then
    docElementFactory := TXpElementFactory.FindFactory(XpsDefaultFactoryID);
{$ELSE}
  docElementFactory := TXpElementFactory.FindFactory(XpsDefaultFactoryID);
{$ENDIF}
{End !!.52}
  noOwnerDocument := Self;
  docActualCDATA := False;
  docIdAttribute := xpsID;
  docIgnoreCase := False;
  docNoCharReplacement := False;
  docXPathParser := TXpXPathParser.Create;                             {!!.52}
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
function TXpDocument.CreateAttribute(const sName : DOMString)          {!!.52}
                                                 : TXpAttribute;
begin
  if ((sName <> '') and
      (XpValidName(sName))) then begin                                 {!!.57}
    Result := TXpAttribute.Create;
    Result.name := sName;
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocument.CreateAttributeNS(const sNamespaceURI,            {!!.57}
                                       sQualifiedName : DOMString)
                                                      : TXpAttribute;
begin
  if ((sQualifiedName <> '') and
      (XpValidName(sQualifiedName))) then begin                        {!!.57}
    Result := TXpAttribute.Create;
    Result.name := sQualifiedName;
    Result.noDefaultNameSpace := sNamespaceURI;
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocument.CreateCDATASection(const sData : DOMString) : TXpCDATASection;
begin
  Result := TXpCDATASection.Create;
  Result.Data := sData;
  Result.noOwnerDocument := Self;
end;
{--------}
function TXpDocument.CreateComment(const sData : DOMString) : TXpComment; {!!.57}
begin
  Result := TXpComment.Create;
  Result.Data := sData;
  Result.noOwnerDocument := Self;
end;
{--------}
function TXpDocument.CreateDocumentFragment : TXpDocumentFragment;
begin
  Result := TXpDocumentFragment.Create;
  Result.noOwnerDocument := Self;
end;
{--------}
function TXpDocument.CreateDocumentType(const sName     : DOMString;   {!!.57}
                                        bExternal : Boolean) : TXpDocumentType;
begin
  Result := TXpDocumentType.Create;
  Result.NodeName := sName;
  Result.dtIsExternalDTD := bExternal;
  Result.noOwnerDocument := Self;
end;
{--------}
function TXpDocument.CreateElement(const sTagName : DOMString)
                                                  : TXpElement; {!!.52}
begin
  if ((sTagName <> '') and
      (XpValidName(sTagName))) then begin                              {!!.57}
    Result := docElementFactory.CreateElement(sTagName);               {!!.52}
//    Result.TagName := sTagName;                                      {Deleted !!.52}
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
{Begin !!.52}
function TXpDocument.CreateElementNS(const sNamespaceURI,
                                           sQualifiedName : DOMString)
                                                          : TXpElement;
{End !!.52}
begin
  if ((sQualifiedName <> '') and
      (XpValidName(sQualifiedName))) then begin                        {!!.57}
    Result := TXpElement.Create;
    Result.TagName := sQualifiedName;
    Result.noDefaultNameSpace := sNamespaceURI;
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocument.CreateEntityReference(const sName : DOMString) : TXpEntityReference; {!!.57}
begin
  if ((sName <> '') and
      (XpValidName(sName))) then begin                                 {!!.57}
    Result := TXpEntityReference.Create;
    Result.NodeName := sName;
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocument.CreateProcessingInstruction(const sTarget,
                                                 sData   : DOMString)  {!!.57}
                                                         : TXpProcessingInstruction;
begin
  if (sTarget <> '') then begin                                        {!!.57}
    Result := TXpProcessingInstruction.Create;
    Result.Target := sTarget;
    Result.Data := sData;
    Result.noOwnerDocument := Self;
  end else
    raise TXpDomException.CreateCode(ecInvalidNameErr);
end;
{--------}
function TXpDocument.CreateTextNode(const sData : DOMString) : TXpText;
begin
  Result := TXpText.Create;
  Result.Data := sData;
  Result.noOwnerDocument := Self;
end;
{--------}
destructor TXpDocument.Destroy;
begin
  RemoveAll;
  { NOTE: Do not free docElementFactory as it points to a global
          instance that may be used by multiple DOMs. }
  docDomImplementation.Free;
  docXPathParser.Free;                                                 {!!.52}
  inherited Destroy;
end;
{--------}
function TXpDocument.GetElementsByTagName(const sName : DOMString)     {!!.57}
                                                : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  docTraverseTreeForElements(Result, DocumentElement, sName, '');
end;
{--------}
function TXpDocument.GetElementsByTagNameNS(const sNamespaceURI,       {!!.57}
                                            sLocalName    : DOMString)
                                                          : TXpNodeList;
begin
  Result := TXpNodeList.Create;
  docTraverseTreeForElements(Result,
                             DocumentElement,
                             sLocalName,
                             sNamespaceURI);
end;
{--------}
function TXpDocument.ImportNode(oNode : TXpNode; bDeep : Boolean) : TXpNode;
begin
  Result := oNode.CloneNode(bDeep);
  if Assigned(Result) then begin
    ForceOwnerDocument(Result);                                        {!!.58}
    Result.noDefaultNameSpace := noDefaultNameSpace;
  end;
end;
{Begin !!.53}
{--------}
function TXpDocument.LocationPath : DOMString;
begin
  Result := '/';
end;
{End !!.53}
{====================================================================}



{===TXpObjModel=====================================================}
function TXpObjModel.omCheckAttlistDefinition(oElem    : TXpElement;
                                              oAttlist : TXpDTDAttlist)
                                                       : Boolean;
var
  oAttDef : TXpDTDAttDefinition;
  j       : Integer;
  bStop   : Boolean;
begin
  Result := true;
  bStop := false;
  for j := 0 to oAttlist.ChildNodes.Length - 1 do begin
    if oAttList.ChildNodes.Item(j).NodeType <> ATT_DEF_DECL_NODE then
      raise TXpInvalidHalt.Create('Improperly formed DTD');

    oAttDef := TXpDTDAttDefinition(oAttList.ChildNodes.Item(j));
    if (oAttDef.DefaultType = ATTRIBUTE_DEFAULT_REQUIRED)
       and not oElem.HasAttribute(oAttDef.name) then begin
      if Assigned(omOnInvalidDocument) then
        omOnInvalidDocument(self, V_ATTRIBUTEREQUIRED, oElem, bStop);
      if bStop then
        raise TXpInvalidHalt.Create('Cannot change fixed attribute');
      Result := false;
    end;
  end;
end;
{--------}
function TXpObjModel.omCheckElementAttribute(oElem    : TXpElement;
                                             oAttr    : TXpAttribute;
                                             oAttlist : TXpDTDAttlist) : Boolean;
var
  oAttDef : TXpDTDAttDefinition;
  j       : Integer;
  i       : Integer;
  Matched,                                                             {!!.56}
  bStop   : Boolean;
begin
  Result := False;
  bStop := False;
  Matched := False;                                                    {!!.56}
  for j := 0 to oAttlist.ChildNodes.Length - 1 do begin
    if oAttList.ChildNodes.Item(j).NodeType <> ATT_DEF_DECL_NODE then
      raise TXpInvalidHalt.Create('Improperly formed DTD');

    oAttDef := TXpDTDAttDefinition(oAttList.ChildNodes.Item(j));
    if oAttDef.name = oAttr.name then begin
      Matched := True;                                                 {!!.56}
      case oAttDef.AttType of
        ATTRIBUTE_ENUMERATED:
          begin
            for i := 0 to oAttDef.Enumeration.Count - 1 do
              if oAttr.Value = oAttDef.Enumeration[i] then begin
                Result := True;
                Exit;
              end;
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_ATTRIBUTENOTENUM, oAttr, bStop); {!!.52}
            if bStop then
              raise TXpInvalidHalt.Create('Attribute not defined in enumeration');
          end;
        else begin
          Result := True;
          if (oAttDef.DefaultType = ATTRIBUTE_DEFAULT_FIXED) and
             (oAttDef.DefaultValue <> oAttr.Value) then begin
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_ATTRIBUTEFIXED, oAttr, bStop); {!!.52}
            if bStop then
              raise TXpInvalidHalt.Create('Cannot change fixed attribute');
            Result := False;
          end;
          Break;
        end;
      end;
    end; {else begin}                                                  {!!.55 - Start}{!!.56 - Start}
  end;
  if (not Matched) and
     (Assigned(omOnInvalidDocument)) then begin
    omOnInvalidDocument(Self, V_ATTRIBUTENOTEXPECTED, oAttr, bStop);
    if (bStop) then
      raise TXpInvalidHalt.Create(oAttr.Name + ' attribute not defined in DTD');
  end;                                                                 {!!.55 - End}{!!.56 - End}
end;
{--------}
function TXpObjModel.omCheckElementInContent(oElem     : TXpElement;
                                             oDTDGroup : TXpDTDElementGroup) : Boolean;
var
  i     : Integer;
  oNode : TXpNode;
begin
  Result := false;
  if not oDTDGroup.HasChildNodes then
    raise TXpInvalidHalt.Create('Improperly formed DTD');

  for i := 0 to oDTDGroup.ChildNodes.Length - 1 do begin
    oNode := oDTDGroup.ChildNodes.Item(i);
    case oNode.NodeType of
      ELEMENT_DECL_GROUP_NODE:
        begin
          if omCheckElementInContent(oElem, TXpDTDElementGroup(oNode)) then begin
            Result := true;
            Exit;
          end;
        end;
      ELEMENT_DECL_CONTENT_NODE:
        begin
          if TXpDTDElementContent(oNode).name = oElem.TagName then begin
            Result := true;
            Exit;
          end;
        end;
      else
        raise TXpInvalidHalt.Create('Improperly formed DTD');
    end;
  end;
end;
{--------}
function TXpObjModel.omCheckElementsContent(var wIndex    : Integer;
                                                oElem     : TXpElement;
                                                oDTDGroup : TXpDTDElementGroup)
                                                          : EXpContentCode;
var
  oDTDNode     : TXpNode;
  oContentNode : TXpNode;
  oDTDContent  : TXpDTDElementContent;
  oDTDGroup2   : TXpDTDElementGroup;
  bRet         : Boolean;
  i            : Integer;
begin
//  if not oElem.HasChildNodes or                                      {!!.57 - Removed}
//     (wIndex >= oElem.ChildNodes.Length) then begin
//    Result := ccNone;
//    Exit;
//  end;

  oDTDNode := oDTDGroup.FirstChild;
  if oDTDNode = nil then
    raise TXpInvalidHalt.Create('Improperly formed DTD');


  Result := ccBad;
  while Assigned(oDTDNode) do begin
    case oDTDNode.NodeType of
      ELEMENT_DECL_GROUP_NODE:
        begin
          oDTDGroup2 := TXpDTDElementGroup(oDTDNode);
          bRet := omCheckElementsGroup(wIndex,
                                       oElem,
                                       oDTDGroup2,
                                       False,
                                       True);
{Begin !!.55}
          if not bRet then begin
            if (oDTDGroup2.Occurs = OCCURS_REQ_NOREPEAT) and
               (oDTDGroup2.Relation = REL_AND) then
              Exit;
          end
          else begin
            if oDTDGroup2.Occurs in
                 [OCCURS_OPT_REPEAT, OCCURS_REQ_REPEAT] then begin
              while bRet do begin
                i := wIndex;
                bRet := omCheckElementsGroup(wIndex,
                                             oElem,
                                             oDTDGroup2,
                                             False,
                                             True);
                if i = wIndex then
                  Break;
              end;  { while }
            end;  { if }
            if oDTDGroup2.Relation = REL_OR then
              Break;
          end;
{End !!.55}
        end;
      ELEMENT_DECL_CONTENT_NODE:
        begin
          oDTDContent := TXpDTDElementContent(oDTDNode);
{Begin !!.52}
          if wIndex >= oElem.ChildNodes.Length then begin
            { Are the remaining DTD nodes optional? }
            repeat
              if oDTDContent.Occurs in
                [OCCURS_REQ_NOREPEAT, OCCURS_REQ_REPEAT] then
                { Found a required element. Exit. }
                Exit;
              oDTDNode := oDTDNode.NextSibling;
              oDTDContent := TXpDTDElementContent(oDTDNode);
            until oDTDNode = nil;
            Result := ccOK;
            Exit;
          end;
{End !!.52}
          oContentNode := oElem.ChildNodes.Item(wIndex);
          { Process "AND" }
          if oDTDContent.Relation = REL_AND then begin
            case oDTDContent.Occurs of
              OCCURS_REQ_NOREPEAT:
                begin
{Begin !!.55}
                  while Assigned(oContentNode) and
                        ((oContentNode is TXpComment) or
                         (oContentNode is TXpProcessingInstruction)) do begin
                    Inc(wIndex);
                    oContentNode := oElem.ChildNodes.Item(wIndex);
                  end;
{End !!.55}
                  if oDTDContent.name <> oContentNode.NodeName then
                    Exit
                  else
                    Inc(wIndex);
                end;
              OCCURS_OPT_NOREPEAT:
                begin
                  if oDTDContent.name = oContentNode.NodeName then
                    Inc(wIndex);
                end;
              OCCURS_OPT_REPEAT:
                begin
                  while (Assigned(oContentNode)) and
                        ((oDTDContent.Name = oContentNode.NodeName) or  {!!.52}
                         (oContentNode is TXpComment) or                {!!.55}
                         (oContentNode is TXpProcessingInstruction)) do begin {!!.55}
                    Inc(wIndex);
                    oContentNode := oElem.ChildNodes.Item(wIndex);
                  end;
                end;
              OCCURS_REQ_REPEAT:
                begin
{Begin !!.55}
                  while Assigned(oContentNode) and
                        ((oContentNode is TXpComment) or
                         (oContentNode is TXpProcessingInstruction)) do begin
                    Inc(wIndex);
                    oContentNode := oElem.ChildNodes.Item(wIndex);
                  end;
{End !!.55}
                  if oDTDContent.Name <> oContentNode.NodeName then
                    Exit
                  else
                    while (Assigned(oContentNode)) and
                          ((oDTDContent.Name = oContentNode.NodeName) or  {!!.52}
                           (oContentNode is TXpComment) or                {!!.55}
                           (oContentNode is TXpProcessingInstruction)) do begin {!.55}
                      Inc(wIndex);
                      oContentNode := oElem.ChildNodes.Item(wIndex);
                    end;
                end;
            end;
          end
          { Process "OR" }
          else begin
{Begin !!.55}
//            while (Assigned(oDTDNode)) and
//                  (oDTDNode.NodeName <> oContentNode.NodeName) do begin
//              oDTDNode := oDTDNode.NextSibling;
//            end;
//            if Assigned(oDTDNode) then begin
//              Inc(wIndex);
//              Result := ccOk;
//            end;
            while Assigned(oDTDNode) and Assigned(oContentNode) and
                  (oDTDNode.NodeName <> oContentNode.NodeName) do begin
              if (oContentNode is TXpComment) or
                 (oContentNode is TXpProcessingInstruction) then begin
                Inc(wIndex);
                oContentNode := oElem.ChildNodes.Item(wIndex);
              end
              else if (wIndex > 0) and
                      (oDTDGroup.Occurs in
                       [OCCURS_REQ_NOREPEAT, OCCURS_OPT_NOREPEAT]) then begin
                i := Pred(wIndex);
                while (oElem.ChildNodes.Item(i) is TXpComment) and (i > 0) do
                  Dec(i);
                if ((i = 0) or                                         {!!.57}
                    (oDTDContent.Relation = REL_OR)) then              {!!.57}
                  oDTDNode := oDTDNode.NextSibling
                else
                  oDTDNode := nil;
              end
              else
                oDTDNode := oDTDNode.NextSibling
            end;  { while }
            if Assigned(oDTDNode) then begin
              oDTDContent := TXpDTDElementContent(oDTDNode);
              if oDTDContent.Occurs in
                [OCCURS_OPT_REPEAT, OCCURS_REQ_REPEAT] then begin
                while (Assigned(oContentNode)) and
                      ((oDTDContent.Name = oContentNode.NodeName) or
                       (oContentNode is TXpComment) or
                       (oContentNode is TXpProcessingInstruction)) do begin
                  Inc(wIndex);
                  oContentNode := oElem.ChildNodes.Item(wIndex);
                end;
                Result := ccOk;
              end
              else begin
                Inc(wIndex);
                Result := ccOk;
              end;
            end;
{End !!.55}
            Exit;
          end;
        end;
      else
        raise TXpInvalidHalt.Create('Improperly formed DTD');
    end;
    oDTDNode := oDTDNode.NextSibling;
  end;
  Result := ccOk;
end;
{--------}
function TXpObjModel.omCheckElementsGroup(var wIndex    : Integer;
                                              oElem     : TXpElement;
                                              oDTDGroup : TXpDTDElementGroup;
                                              bRoot,
                                              bDoError  : Boolean) : Boolean;
var
//  i        : Integer;                                                Deleted !!.58}
  ccResult : EXpContentCode;
  bStop    : Boolean;
begin
  Result := True;
  bStop := False;
  ccResult := omCheckElementsContent(wIndex, oElem, oDTDGroup);

  case oDTDGroup.Occurs of
    OCCURS_REQ_NOREPEAT:
      begin
        if ccResult <> ccOk then begin
{Begin !!.55}
          Result := False;
          if (oDTDGroup.Relation = REL_AND) then begin
            if Assigned(omOnInvalidDocument) then
               omOnInvalidDocument(self, V_ELEMENTCHILDMISSING, oElem, bStop);
            if bStop then
              raise TXpInvalidHalt.Create('Required child missing');
          end;  { if }
{End !!.55}
        end else if ((bRoot) and
                     (omCheckElementsMoreChildren(oElem, wIndex))) then begin {!!.57}
          if Assigned(omOnInvalidDocument) then
            omOnInvalidDocument(self,
                                V_ELEMENTCHILDNOTEXPECTED,
                                oElem,
                                bStop);
          Result := False;
          if bStop then
            raise TXpInvalidHalt.Create('Element child not expected');
        end;
      end;
    OCCURS_OPT_NOREPEAT:
      begin
        if bRoot then begin
          if ccResult = ccBad then begin
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_ELEMENTCHILDNOTEXPECTED, oElem, bStop);
            Result := false;
            if bStop then
              raise TXpInvalidHalt.Create('Element child not expected');
          end else if ((ccResult = ccOk) and
                       (omCheckElementsMoreChildren(oElem, wIndex))) then begin {!!.57}
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_ELEMENTCHILDNOTEXPECTED, oElem, bStop);
            Result := false;
            if bStop then
              raise TXpInvalidHalt.Create('Element child not expected');
          end;
        end;
      end;
    OCCURS_OPT_REPEAT:
      begin
        if bRoot and (ccResult = ccBad) then begin
          if Assigned(omOnInvalidDocument) then
            omOnInvalidDocument(self, V_ELEMENTCHILDNOTEXPECTED, oElem, bStop);
          Result := false;
          if bStop then
            raise TXpInvalidHalt.Create('Element child not expected');
        end else begin
{Begin !!.58}
          while ((ccResult = ccOk) and
                 (omCheckElementsMoreChildren(oElem, wIndex))) do begin
            ccResult := omCheckElementsContent(wIndex, oElem, oDTDGroup);
            if bRoot and (ccResult = ccBad) then begin
              if Assigned(omOnInvalidDocument) then
                omOnInvalidDocument(self, V_ELEMENTCHILDNOTEXPECTED, oElem, bStop);
              Result := false;
              if bStop then
                raise TXpInvalidHalt.Create('Element child not expected');
            end;
{End !!.58}
          end;  { while }
        end;  { if }
      end;
    OCCURS_REQ_REPEAT:
      begin
        if bDoError and (ccResult <> ccOK) then begin
          if Assigned(omOnInvalidDocument) then
            omOnInvalidDocument(self, V_ELEMENTCHILDMISSING, oElem, bStop);
          Result := False;
          if bStop then
            raise TXpInvalidHalt.Create('Element child not expected');
        end else begin
          while ((ccResult = ccOk) and                                  {!!.57}
                 (omCheckElementsMoreChildren(oElem, wIndex))) do begin {!!.57}
            //i := wIndex;                                              {!!.57}
            ccResult := omCheckElementsContent(wIndex, oElem, oDTDGroup);
            if bRoot and (ccResult = ccBad) then begin
              if Assigned(omOnInvalidDocument) then
                omOnInvalidDocument(Self, V_ELEMENTCHILDNOTEXPECTED, oElem, bStop);
              Result := False;
              if bStop then
                raise TXpInvalidHalt.Create('Element child not expected');
            end;
            //if i = wIndex then                                       {!!.57}
            //  Break;                                                 {!!.57}
          end;
        end;
      end;
  end;  { case }
end;
{--------}
{!!.57 - New}
function TXpObjModel.omCheckElementsMoreChildren(const oElem  : TXpElement;
                                                       wIndex : Integer)
                                                              : Boolean;
begin
  Result := ((oElem.HasChildNodes) and
             (wIndex < oElem.ChildNodes.Length));                      {!!.58}
end;
{--------}
function TXpObjModel.omGetBufferSize : Integer;
begin
  Result := omParser.BufferSize;
end;
{Begin !!.52}
{--------}
function TXpObjModel.omGetDocLocation : string;
{Rewritten !!.53}
begin
  if omDocument <> nil then
    Result := omDocument.BaseURI
  else
    Result := '';
end;
{--------}
function TXpObjModel.omGetDocName : string;
begin
  if omParser <> nil then
    Result := omParser.DocName
  else
    Result := '';
end;
{End !!.52}
{--------}
function TXpObjModel.omGetDocumentElement : TXpElement;
begin
  Result := omDocument.DocumentElement;
end;
{--------}
function TXpObjModel.omGetFormattedOutput : Boolean;
begin
  Result := omDocument.FormattedOutput;
end;
{--------}
function TXpObjModel.omGetIdAttribute : DOMString;
begin
  Result := omDocument.IdAttribute;
end;
{--------}
function TXpObjModel.omGetIgnoreCase : Boolean;
begin
  Result := omDocument.IgnoreCase;
end;
{Begin !!.55}
{--------}
function TXpObjModel.omGetLineBreakCharReplace : Boolean;
begin
  Result := omDocument.LineBreakCharReplace;
end;
{--------}
function TXpObjModel.omGetLineBreakMode : TXpLineBreakMode;
begin
  Result := omDocument.LineBreakMode;
end;
{End !!.55}
{--------}
function TXpObjModel.omGetPassword : string;
begin
  Result := omParser.Password;
end;
{--------}
function TXpObjModel.omGetUserName : string;
begin
  Result := omParser.UserName;
end;
{--------}
function TXpObjModel.omGetXpDocument : DOMString;
var
  Idx : Integer;
begin
  Result := '';

  { Output nodes for the document }
  for Idx := 0 to Pred(omDocument.ChildNodes.Length) do begin
    if (Idx > 0) then
      Result := Result + XpsLineBreak;
    Result := Result + omDocument.ChildNodes.Item(Idx).XmlDocument;
  end;
end;
{--------}
procedure TXpObjModel.omFreeAttrList;                                  {!!.55 - Added}
var
  i     : Integer;
begin
  for i := 0 to (omAttrList.Count - 1) do
    TXpAttribute(omAttrList[i]).Release;
  omAttrList.Clear;
end;
{--------}                                                             {!!.55 - End added}
{Begin !!.53}
{--------}
procedure TXpObjModel.omHandleErr;
var
  wInx : Integer;
begin
  omErrors.Assign(omParser.Errors);
  omFreeAttrList;                                                      {!!.55}
  omAttrList.Clear;
  { So that all the in-work nodes will be freed, we must remove them from
    the stack & clear the document. }
  for wInx := Pred(omStack.Length) downto 1 do begin
    omStack.Item(wInx).Release;
    omStack.Delete(wInx);
  end;
  omStack.Delete(0);
  ClearDocument;
end;
{End !!.53}
{--------}
procedure TXpObjModel.omParseAttribute(oOwner     : TObject;
                                       sName,
                                       sValue     : DOMString;
                                       bSpecified : Boolean);
var
  oNewAttr : TXpAttribute;
begin
  if sName <> '' then begin                                            {!!.53}
    if omTrackAttributes then
      omTrackedAttributes.Add(sName);
    oNewAttr := omDocument.CreateAttribute(sName);
    try                                                                {!!.55 - Start}
      oNewAttr.Value := sValue;
      oNewAttr.Specified := bSpecified;
      omAttrList.Add(oNewAttr);
    except
      oNewAttr.Free;
      raise;
    end;                                                               {!!.55 - End}
  end;                                                                 {!!.53}
end;
{--------}
procedure TXpObjModel.omParseCDATASection(oOwner : TObject;
                                          sValue : DOMString);
var
  oNewText  : TXpCDATASection;
  oLastNode : TXpNode;
begin
  oNewText := omDocument.CreateCDATASection(sValue);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oNewText);
  oNewText.Release;
end;
{--------}
procedure TXpObjModel.omParseCharData(oOwner : TObject;
                                      sValue : DOMString);
var
  oNewText  : TXpText;
  oLastNode : TXpNode;
begin
  oNewText := omDocument.CreateTextNode(sValue);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oNewText);
  oNewText.Release;
end;
{--------}
procedure TXpObjModel.omParseComment(oOwner : TObject;
                                     sValue : DOMString);
var
  oNewComment : TXpComment;
  oLastNode   : TXpNode;
begin
  oNewComment := omDocument.CreateComment(sValue);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oNewComment);
  oNewComment.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDAttDefinition(oOwner       : TObject;
                                              sName  : DOMString;
                                              wAttrType    : Integer;
                                              oEnumeration : TStringList;
                                              wValueType   : Integer;
                                              sValue       : DOMString);
var
  oNewDTDAttDef : TXpDTDAttDefinition;
  oLastNode     : TXpDTDAttlist;
begin
  oLastNode := TXpDTDAttlist(omStack.Item(Pred(omStack.Length)));
  oNewDTDAttDef := oLastNode.CreateDTDAttDefinition(sName,
                                                    wAttrType,
                                                    oEnumeration,
                                                    wValueType,
                                                    sValue);
  oLastNode.AppendChild(oNewDTDAttDef);
  oNewDTDAttDef.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDAttlistEnd(oOwner : TObject);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{--------}
procedure TXpObjModel.omParseDTDAttlistStart(oOwner : TObject;
                                             sName  : DOMString);
var
  oNewDTDAttlist : TXpDTDAttlist;
  oLastNode      : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));

  oNewDTDAttlist := oLastNode.CreateDTDAttlist(sName);
  oLastNode.AppendChild(oNewDTDAttlist);
  omStack.Add(oNewDTDAttlist);
end;
{--------}
procedure TXpObjModel.omParseDTDConditionalEnd(oOwner : TObject);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{--------}
procedure TXpObjModel.omParseDTDConditionalStart(oOwner : TObject;
                                                 wType  : Integer);
var
  oNewDTDCond : TXpDTDConditional;
  oLastNode   : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));
  oNewDTDCond := oLastNode.CreateDTDConditional(wType);
  oLastNode.AppendChild(oNewDTDCond);
  omStack.Add(oNewDTDCond);
end;
{--------}
procedure TXpObjModel.omParseDTDElementContent(oOwner      : TObject;
                                               sName       : DOMString;
                                               wOccurrence,
                                               wRelation   : Integer);
var
  oLastNode             : TXpDTDElementGroup;
  oNewDTDElementContent : TXpDTDElementContent;
begin
  oLastNode := TXpDTDElementGroup(omStack.Item(Pred(omStack.Length)));
  oNewDTDElementContent := oLastNode.CreateDTDElementContent(sName, wOccurrence, wRelation);
  oLastNode.AppendChild(oNewDTDElementContent);
  oNewDTDElementContent.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDElementEnd(oOwner : TObject; sName : DOMString);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{--------}
procedure TXpObjModel.omParseDTDElementGroupEnd(oOwner    : TObject;
                                                wOccurrence,
                                                wRelation : Integer);
begin
  TXpDTDElementGroup(omStack.Item(Pred(omStack.Length))).Occurs := wOccurrence;
  TXpDTDElementGroup(omStack.Item(Pred(omStack.Length))).Relation := wRelation;
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{--------}
procedure TXpObjModel.omParseDTDElementGroupStart(oOwner : TObject);
var
  oLastNode           : TXpNode;
  oNewDTDElementGroup : TXpDTDElementGroup;
begin
  oLastNode := omStack.Item(Pred(omStack.Length));

  if TXpNode(omStack.Item(Pred(omStack.Length))) is TXpDTDElement then
    oNewDTDElementGroup := TXpDTDElement(oLastNode).CreateDTDElementGroup
  else
    oNewDTDElementGroup := TXpDTDElementGroup(oLastNode).CreateDTDElementGroup;

  oLastNode.AppendChild(oNewDTDElementGroup);
  omStack.Add(oNewDTDElementGroup);
end;
{--------}
procedure TXpObjModel.omParseDTDElementStart(oOwner       : TObject;
                                             sName        : DOMString;
                                             wContentType : Integer);
var
  oNewDTDElement : TXpDTDElement;
  oLastNode      : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));

  oNewDTDElement := oLastNode.CreateDTDElement(sName, wContentType);
  oLastNode.AppendChild(oNewDTDElement);
  omStack.Add(oNewDTDElement);
end;
{--------}
procedure TXpObjModel.omParseDTDEnd(oOwner : TObject);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
  omInDTD := false;
end;
{--------}
procedure TXpObjModel.omParseDTDEntityExternal(oOwner        : TObject;
                                               bPeFlag       : Boolean;
                                               sName,
                                               sPublicID,
                                               sSystemID,
                                               sNotationName : DOMString);
var
  oNewDTDEntity : TXpDTDEntity;
  oLastNode     : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));

  oNewDTDEntity := oLastNode.CreateDTDEntity(sName);
  oNewDTDEntity.IsPE := bPeFlag;
  oNewDTDEntity.PublicID := sPublicID;
  oNewDTDEntity.SystemID := sSystemID;
  oNewDTDEntity.NotationName := sNotationName;
  oLastNode.AppendChild(oNewDTDEntity);
  oNewDTDEntity.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDEntityInternal(oOwner  : TObject;
                                               bPeFlag : Boolean;
                                               sName,
                                               sValue  : DOMString);
var
  oNewDTDEntity : TXpDTDEntity;
  oLastNode     : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));

  oNewDTDEntity := oLastNode.CreateDTDEntity(sName);
  oNewDTDEntity.IsPE := bPeFlag;
  oNewDTDEntity.Value := sValue;
  oLastNode.AppendChild(oNewDTDEntity);
  oNewDTDEntity.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDExternalEnd(oOwner : TObject);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{Begin !!.53}
{--------}
procedure TXpObjModel.omParseDTDExternalLoaded(oOwner : TObject);
var
  oNode : TXpNode;
begin
  oNode := omStack.Item(Pred(omStack.Length));
  oNode.noBaseURI := TXpParserCracker(omParser).FFilter.DirContext.Dir;
end;
{End !!.53}
{--------}
procedure TXpObjModel.omParseDTDExternalStart(oOwner : TObject;
                                          var sFile  : DOMString);
var
  oDTD : TXpDocumentType;
begin
  if (Assigned(omOnDTDExternal)) then                                  {!!.57}
    omOnDTDExternal(Self, sFile);                                      {!!.57}

  oDTD := omDocument.CreateDocumentType(sFile, True);

  if omDocument.DocType.ExternalDTD <> nil then
    omDocument.DocType.ExternalDTD.Release;
  omDocument.DocType.ExternalDTD := oDTD;
  oDTD.AddRef;
  omStack.Add(oDTD);
end;
{--------}
procedure TXpObjModel.omParseDTDNotation(oOwner        : TObject;
                                         sNotationName,
                                         sPublicID,
                                         sSystemID     : DOMString);
var
  oNewDTDNotation : TXpDTDNotation;
  oLastNode       : TXpDocumentType;
begin
  oLastNode := TXpDocumentType(omStack.Item(Pred(omStack.Length)));

  oNewDTDNotation := oLastNode.CreateDTDNotation(sNotationName);
  oNewDTDNotation.PublicID := sPublicID;
  oNewDTDNotation.SystemID := sSystemID;
  oLastNode.AppendChild(oNewDTDNotation);
  oNewDTDNotation.Release;
end;
{--------}
procedure TXpObjModel.omParseDTDStart(oOwner : TObject;
                                      sDecl,
                                      sId0,
                                      sId1   :DOMString);
var
  oDTD      : TXpDocumentType;
  oLastNode : TXpNode;
begin
  omInDTD := True;

  oDTD := omDocument.CreateDocumentType(sDecl, False);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oDTD);
  omStack.Add(oDTD);
end;
{--------}
procedure TXpObjModel.omParseEndElement(oOwner : TObject;
                                        sValue : DOMString);
begin
  omStack.Item(Pred(omStack.Length)).Release;
  omStack.Delete(Pred(omStack.Length));
end;
{--------}
procedure TXpObjModel.omParseNonXMLEntity(oOwner        : TObject;
                                          sEntityName,
                                          sPublicID,
                                          sSystemID,
                                          sNotationName : DOMString);
var
  oNew      : TXpEntityReference;
  oLastNode : TXpNode;
begin
  oNew := omDocument.CreateEntityReference(sEntityName);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oNew);
  oNew.Release;
end;
{--------}
procedure TXpObjModel.omParseProcessingInstruction(oOwner : TObject;
                                                   sName,
                                                   sValue : DOMString);
var
  oNewProcess : TXpProcessingInstruction;
  oLastNode   : TXpNode;
begin
  oNewProcess := omDocument.CreateProcessingInstruction(sName, sValue);

  oLastNode := TXpNode(omStack.Item(Pred(omStack.Length)));
  oLastNode.AppendChild(oNewProcess);
{Begin !!.52}
{$IFDEF XPDPRO}
  { Is this an xml-stylesheet processing instruction (PI)? }
  if oNewProcess.noNameEquals(XpsStylePI) then
    { Yes. Add it to our list of xml-stylesheet PIs for later use by the
      XSL processor. }
    omStylePIs.Add(oNewProcess);
{$ENDIF}
{End !!.52}
  oNewProcess.Release;
end;
{--------}
procedure TXpObjModel.omParseStartElement(oOwner : TObject;
                                          sValue : DOMString);
var
  oNewElement : TXpElement;
  oLastNode   : TXpNode;
  oAttr       : TXpAttribute;
  i           : Integer;
begin
  if omTrackElements then
    omTrackedElements.Add(sValue);

  oNewElement := omDocument.CreateElement(sValue);

  oLastNode := TXpNode(omStack.Item(omStack.Length - 1));

  for i := 0 to (omAttrList.Count - 1) do begin                        {!!.55 - Start}
    oAttr := oNewElement.SetAttributeNode(omAttrList[i]);
    if (oAttr <> nil) then
      oAttr.Release;
  end;
  {omAttrList.Clear;}
  omFreeAttrList;                                                      {!!.55 - End}

  oLastNode.AppendChild(oNewElement);
  omStack.Add(oNewElement);
end;
{--------}
procedure TXpObjModel.omSetBufferSize(aSize : Integer);
begin
  omParser.BufferSize := aSize;
end;
{--------}
procedure TXpObjModel.omSetFormattedOutput(bValue : Boolean);
begin
  omDocument.FormattedOutput := bValue;
end;
{--------}
procedure TXpObjModel.omSetIdAttribute(const sAttr : DOMString);       {!!.57}
begin
  omDocument.IdAttribute := sAttr;
end;
{--------}
procedure TXpObjModel.omSetIgnoreCase(bValue : Boolean);
begin
  omDocument.IgnoreCase := bValue;
end;
{Begin !!.55}
{--------}
procedure TXpObjModel.omSetLineBreakCharReplace(bValue : Boolean);
begin
  omDocument.LineBreakCharReplace := bValue;
end;
{--------}
procedure TXpObjModel.omSetLineBreakMode(Value : TXpLineBreakMode);
begin
  omDocument.LineBreakMode := Value;
end;
{End !!.55}
{--------}
procedure TXpObjModel.omSetOnResolveEntity(const Value : TXpResolveEvent);
begin
  omOnResolveEntity := Value;
end;
{--------}
procedure TXpObjModel.omSetPassword(const sPassword : string);         {!!.57}
begin
  omParser.Password := sPassword;
end;
{--------}
procedure TXpObjModel.omSetUserName(const sUserName : string);         {!!.57}
begin
  omParser.UserName := sUserName;
end;
{--------}
function TXpObjModel.omSubValidateElements(oElem : TXpElement;
                                           oDTD  : TXpDocumentType)
                                                 : Boolean;
var
  i            : Integer;
  wIndex       : Integer;
  bFound       : Boolean;
  bStop        : Boolean;
  oTmpNode     :  TXpNode;
  oFoundNode   : TXpNode;
  oAttlistNode : TXpNode;
  oList        : TXpNodeList;
  oNodes       : TXpNodeList;
begin
  Result := True;
  bFound := False;
  bStop := False;
  oFoundNode := nil;
  oAttlistNode := nil;

  { Loop through DTD nodes }
  oNodes := oDTD.GetNodesByNodeType(ELEMENT_DECL_NODE);
  for i := 0 to oNodes.Length - 1 do begin
    oTmpNode := oNodes.Item(i);
    if oTmpNode.NodeName = oElem.NodeName then begin
      bFound := True;
      oFoundNode := oTmpNode;
      Break;
    end;
  end;
  oNodes.Free;

  { Check external DTD }
  if not bFound and (Assigned(oDTD.ExternalDTD)) then begin
    oNodes := oDTD.ExternalDTD.GetNodesByNodeType(ELEMENT_DECL_NODE);
    for i := 0 to oNodes.Length - 1 do begin
      oTmpNode := oNodes.Item(i);
      if oTmpNode.NodeName = oElem.NodeName then begin
        bFound := True;
        oFoundNode := oTmpNode;
        Break;
      end;
    end;
    oNodes.Free;
  end;

  { Report not found error }
  if not bFound then begin
    if Assigned(omOnInvalidDocument) then
      omOnInvalidDocument(self, V_ELEMENTNOTDEFINED, oElem, bStop);
    Result := False;
    if bStop then
      raise TXpInvalidHalt.Create('Element not defined');
  end;

  { Validate attributes }
  bFound := False;                                                     {!!.52}
  oNodes := oDTD.GetNodesByNodeType(ATTLIST_DECL_NODE);
  for i := 0 to oNodes.Length - 1 do begin
    oTmpNode := oNodes.Item(i);
    if oTmpNode.NodeName = oElem.NodeName then begin
      bFound := True;
      oAttlistNode := oTmpNode;
      break;
    end;
  end;
  oNodes.Free;

  { Check external DTD }
  if not bFound and (Assigned(oDTD.ExternalDTD)) then begin
    oNodes := oDTD.ExternalDTD.GetNodesByNodeType(ATTLIST_DECL_NODE);  {!!.52}
    for i := 0 to oNodes.Length - 1 do begin
      oTmpNode := oNodes.Item(i);
      if oTmpNode.NodeName = oElem.NodeName then begin
        bFound := True;
        oAttlistNode := oTmpNode;
        Break;
      end;
    end;
    oNodes.Free;
  end;

  if oElem.HasAttributes then begin
    { Report not found error }
    if not bFound then begin
      if Assigned(omOnInvalidDocument) then
        omOnInvalidDocument(self, V_ELEMENTATTRNOTDEFINED, oElem, bStop);
      Result := False;
      if bStop then
        raise TXpInvalidHalt.Create('Element attribute list not defined');
    end;

    { Check attributes against definition }
    if Assigned(oAttlistNode) then
      for i := 0 to oElem.Attributes.Length - 1 do
        if not omCheckElementAttribute(oElem,
                                       TXpAttribute(oElem.Attributes.Item(i)),
                                       TXpDTDAttlist(oAttlistNode)) then
          Result := False;
  end;

  if Assigned(oAttlistNode) then
    if oAttlistNode is TXpDTDAttList then
      if not omCheckAttlistDefinition(oElem, TXpDTDAttlist(oAttlistNode)) then
        Result := False;

  { Check for required child elements }
  if Assigned(oFoundNode) then begin
    case TXpDTDElement(oFoundNode).ContentType of
      CONTENT_EMPTY:
        begin
          { Check for no child nodes }
          if oElem.HasChildNodes then begin
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_ELEMENTNOTEMPTY, oElem, bStop);
            Result := False;
            if bStop then
              raise TXpInvalidHalt.Create('Element not empty');
          end;
        end;
      CONTENT_MIXED:
        begin
          if not oFoundNode.HasChildNodes or
            (oFoundNode.FirstChild.NodeType <> ELEMENT_DECL_GROUP_NODE) then begin
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_BADDTD, oElem, bStop);
            raise TXpInvalidHalt.Create('Improperly formed DTD');
          end;
          { Check for declaration of child }
          oList := oElem.GetChildElementsByTagName('*');
          for i := 0 to oList.Length - 1 do begin
            if not omCheckElementInContent(TXpElement(oList.Item(i)),
                       TXpDTDElementGroup(oFoundNode.FirstChild)) then begin
              if Assigned(omOnInvalidDocument) then
                omOnInvalidDocument(self, V_ELEMENTNOTCONTENT, oList.Item(i), bStop);
              Result := False;
              if bStop then
                raise TXpInvalidHalt.Create('Element not allowed as content');
            end;
          end;
          oList.Free;
        end;
      CONTENT_ELEMENTS:
        begin
          if not oFoundNode.HasChildNodes or
             (oFoundNode.FirstChild.NodeType <> ELEMENT_DECL_GROUP_NODE) then begin
            if Assigned(omOnInvalidDocument) then
              omOnInvalidDocument(self, V_BADDTD, oElem, bStop);
            raise TXpInvalidHalt.Create('Improperly formed DTD');
          end;
          { Check for declaration of child }
          oList := oElem.GetChildElementsByTagName('*');
          for i := 0 to oList.Length - 1 do begin
            if not omCheckElementInContent(TXpElement(oList.Item(i)),
                      TXpDTDElementGroup(oFoundNode.FirstChild)) then begin
              if Assigned(omOnInvalidDocument) then
                omOnInvalidDocument(self,
                                    V_ELEMENTNOTCONTENT,
                                    oList.Item(i),
                                    bStop);
              Result := False;
              if bStop then
                raise TXpInvalidHalt.Create('Element not allowed as content');
            end;
          end;
          oList.Free;
          { Check for order of children }
          wIndex := 0;
          if not omCheckElementsGroup(wIndex,
                                      oElem,
                                      TXpDTDElementGroup(oFoundNode.FirstChild), {!!.52}
                                      True,
                                      True) then
            Result := False;
        end;
    end;
  end;

  { Process child elements }
  oList := oElem.GetChildElementsByTagName('*');
  for i := 0 to oList.Length - 1 do begin
    if not omSubValidateElements(TXpElement(oList.Item(i)), oDTD) then
      Result := False;
  end;
  oList.Free;
end;
{--------}
function TXpObjModel.omGetErrorCount : Integer;
begin
  Result := omErrors.Count;
end;
{Begin !!.52}
{--------}
function TXpObjModel.omGetFilter : TXpInCharFilter;
begin
  Result := TXpParserCracker(omParser).FFilter;
end;
{End !!.52}
{Begin !!.51}
{--------}
function TXpObjModel.omGetInCharSet : TXpCharEncoding;
begin
  if omParser <> nil then
    Result := omParser.InCharSet
  else
    Result := ceUnknown;
end;
{End !!.51}
{Begin !!.53}
{--------}
function TXpObjModel.omGetOnCurrentNode : TXpCurrentNodeEvent;
begin
  Result := omDocument.OnCurrentNode;
end;
{--------}
function TXpObjModel.omGetOnDTDExternal : TXpDTDExternalStartEvent;    {!!.57 - Added}
begin
  Result := omOnDTDExternal;
end;
{--------}
function TXpObjModel.omGetOnElementAvailable : TXpElementAvailableEvent;
begin
  Result := Document.OnElementAvailable;
end;
{--------}
function TXpObjModel.omGetOnFormatNumber : TXpFormatNumberEvent;
begin
  Result := omDocument.OnFormatNumber;
end;
{End !!.53}
{--------}
function TXpObjModel.omGetOnFunction : TXpFunctionEvent;
begin
  Result := Document.OnFunction;
end;
{--------}
function TXpObjModel.omGetOnFunctionAvailable : TXpFunctionAvailableEvent;
begin
  Result := Document.OnFunctionAvailable;
end;
{Begin !!.53}
{--------}
function TXpObjModel.omGetOnKeyLookup : TXpKeyLookupEvent;
begin
  Result := omDocument.OnKeyLookup;
end;
{End !!.53}
{--------}
function TXpObjModel.omGetOnPreserveSpace : TXpPreserveSpaceEvent;
begin
  Result := omPreserveSpace;                                           {!!.52}
end;
{Begin !!.53}
{--------}
function TXpObjModel.omGetOnResolveDocument : TXpResolveDocumentEvent;
begin
  Result := omDocument.OnResolveDocument;
end;
{End !!.53}
{--------}
function TXpObjModel.omGetOnSystemProperty : TXpSystemLookupEvent;     {!!.52}
begin
  Result := Document.OnSystemProperty;
end;
{--------}
function TXpObjModel.omGetOnVariableLookup : TXpVariableLookupEvent;
begin
  Result := Document.OnVariableLookup;
end;
{Begin !!.52}
{--------}
procedure TXpObjModel.omLoadInit;
begin
  ClearDocument;
  omStack.Empty;
  omStack.Add(omDocument);
  omErrors.Clear;
  omParser.NormalizeData := omNormalizeData;
end;
{--------}
procedure TXpObjModel.omOnPreserveSpace(oOwner       : TObject;
                                        sElementName : DOMString;
                                    var bPreserve    : Boolean);
begin
  { If this is an xsl:text element then space is always to be preserved. }
  if sElementName = XpsXSLText then
    bPreserve := True
  else if assigned(omPreserveSpace) then
    omPreserveSpace(oOwner, sElementName, bPreserve);
end;
{End !!.52}
{--------}
procedure TXpObjModel.omSetDocument(oDoc : TXpDocument);
begin
  if Assigned(oDoc) then begin
    omDocument.Free;
    omDocument := oDoc;
  end;
end;
{Begin !!.53)
{--------}
procedure TXpObjModel.omSetOnCurrentNode(oFunc : TXpCurrentNodeEvent);
begin
  Document.OnCurrentNode := oFunc;
end;
{--------}
procedure TXpObjModel.omSetOnDTDExternal(oFunc : TXpDTDExternalStartEvent); {!!.57 - Added}
begin
  omOnDTDExternal := oFunc;
end;
{--------}
procedure TXpObjModel.omSetOnElementAvailable(oFunc : TXpElementAvailableEvent);
begin
  Document.OnElementAvailable := oFunc;
end;
{--------}
procedure TXpObjModel.omSetOnFormatNumber(oFunc : TXpFormatNumberEvent);
begin
  Document.OnFormatNumber := oFunc;
end;
{End !!.53}
{--------}
procedure TXpObjModel.omSetOnFunction(oFunc : TXpFunctionEvent);
begin
  Document.OnFunction := oFunc;
end;
{--------}
procedure TXpObjModel.omSetOnFunctionAvailable(oFunc : TXpFunctionAvailableEvent);
begin
  Document.OnFunctionAvailable := oFunc;
end;
{Begin !!.53)
{--------}
procedure TXpObjModel.omSetOnKeyLookup(oFunc : TXpKeyLookupEvent);
begin
  Document.OnKeyLookup := oFunc;
end;
{End !!.53}
{--------}
procedure TXpObjModel.omSetOnPreserveSpace(oFunc : TXpPreserveSpaceEvent);
begin
  omPreserveSpace := oFunc;                                            {!!.52}
end;
{--------}
procedure TXpObjModel.omSetOnSystemProperty(oFunc : TXpSystemLookupEvent); {!!.52}
begin
  Document.OnSystemProperty := oFunc;
end;
{Begin !!.53)
{--------}
procedure TXpObjModel.omSetOnResolveDocument(oFunc : TXpResolveDocumentEvent);
begin
  Document.OnResolveDocument := oFunc;
end;
{End !!.53}
{--------}
procedure TXpObjModel.omSetOnVariableLookup(oFunc : TXpVariableLookupEvent);
begin
  Document.OnVariableLookup := oFunc;
end;
{--------}
procedure TXpObjModel.ClearDocument;
begin
  omDocument.RemoveAll;
end;
{--------}
constructor TXpObjModel.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);

  {$IFDEF XPTrialRun}
  _CC_;
  _VC_;
  {$ENDIF}

  omRaiseErrors := False;
  omNormalizeData := True;
  omDocument := TXpDocument.Create;
  omPreserveSpace := nil;                                              {!!.52}
  omSetUTF8Sig := True;                                                {!!.51}
  omStack := TXpNodeList.Create;                                       {!!.53}
{$IFDEF XPDPRO}                                                        {!!.52}
  omStylePIs := TXpNodeList.Create;                                    {!!.52}
{$ENDIF}                                                               {!!.52}
  omAttrList := TList.Create;
  omErrors := TStringList.Create;
  omTrackedAttributes := TStringList.Create;
  omTrackedAttributes.Sorted := True;
  omTrackedAttributes.Duplicates := dupIgnore;
  omTrackedElements := TStringList.Create;
  omTrackedElements.Sorted := True;
  omTrackedElements.Duplicates := dupIgnore;
  omTrackAttributes := False;
  omTrackElements := False;
  omInDTD := False;
  omParser := TXpParser.Create(self);
  omParser.RaiseErrors := True;
  omParser.OnStartElement := omParseStartElement;
  omParser.OnEndElement := omParseEndElement;
  omParser.OnCharData := omParseCharData;
  omParser.OnCDATASection := omParseCDATASection;
  omParser.OnAttribute := omParseAttribute;
  omParser.OnComment := omParseComment;
  omParser.OnProcessingInstruction := omParseProcessingInstruction;
  omParser.OnResolveEntity := omOnResolveEntity;
  omParser.OnNonXMLEntity := omParseNonXMLEntity;
  omParser.OnDTDStart := omParseDTDStart;
  omParser.OnDTDEnd := omParseDTDEnd;
  omParser.OnDTDAttlistStart := omParseDTDAttlistStart;
  omParser.OnDTDAttlistEnd := omParseDTDAttlistEnd;
  omParser.OnDTDAttDefinition := omParseDTDAttDefinition;
  omParser.OnDTDConditionalStart := omParseDTDConditionalStart;
  omParser.OnDTDConditionalEnd := omParseDTDConditionalEnd;
  omParser.OnDTDElementStart := omParseDTDElementStart;
  omParser.OnDTDElementEnd := omParseDTDElementEnd;
  omParser.OnDTDElementContent := omParseDTDElementContent;
  omParser.OnDTDElementGroupEnd := omParseDTDElementGroupEnd;
  omParser.OnDTDElementGroupStart := omParseDTDElementGroupStart;
  omParser.OnDTDEntityInternal := omParseDTDEntityInternal;
  omParser.OnDTDEntityExternal := omParseDTDEntityExternal;
  omParser.OnDTDExternalStart := omParseDTDExternalStart;
  omParser.OnDTDExternalEnd := omParseDTDExternalEnd;
  TXpParserCracker(omParser).OnDTDExternalLoaded := omParseDTDExternalLoaded; {!!.53}
  omParser.OnDTDNotation := omParseDTDNotation;
  omParser.OnPreserveSpace := omOnPreserveSpace;                       {!!.52}
  omOnDTDExternal := nil;                                              {!!.57}
end;
{--------}
destructor TXpObjModel.Destroy;
begin
  omTrackedElements.Free;
  omTrackedAttributes.Free;
  omErrors.Free;
  omFreeAttrList;                                                      {!!.55}
  omAttrList.Free;
  omStack.Free;
{$IFDEF XPDPRO}                                                        {!!.52}
  omStylePIs.Free;                                                     {!!.52}
{$ENDIF}                                                               {!!.52}
  omParser.Free;
  omDocument.Release;                                                  {!!.53}
  inherited Destroy;
end;
{--------}
function TXpObjModel.GetErrorMsg(wIdx : Integer) : DOMString;
begin
  Result := sIndexOutOfBounds;
  if (wIdx >= 0) and (wIdx < omErrors.Count) then
    Result := omErrors[wIdx];
end;
{Begin !!.52}
{--------}
function TXpObjModel.omLoadInContext(oContext : TXpBaseDirContext;
                               const sSource : string) : Boolean;
begin
  Result := False;
  omLoadInit;
  try
    Result := TXpParserCracker(omParser).ParseInContext(oContext, sSource);
    omDocument.docBaseURI := omParser.DocLocation;                     {!!.53}
    omOutCharSet := omParser.InCharSet;
    if not Result then
      omErrors.Assign(omParser.Errors);
  except
    omErrors.Assign(omParser.Errors);
    omAttrList.Clear;
    if omRaiseErrors then
      raise;
  end;
end;
{End !!.52}
{--------}
function TXpObjModel.LoadDataSource(const sSource : string) : Boolean; {!!.57}
begin
  Result := False;
  omLoadInit;                                                          {!!.52}
  try
    Result := omParser.ParseDataSource(sSource);
    omOutCharSet := omParser.InCharSet;                                {!!.51}
    if not Result then
      omErrors.Assign(omParser.Errors);
    omDocument.docBaseURI := omParser.DocLocation;                     {!!.53}
  except
    omHandleErr;                                                       {!!.53}
    if omRaiseErrors then
      raise;
  end;
end;
{--------}
function TXpObjModel.LoadMemory(var Buffer; aSize : Longint) : Boolean;
begin
  Result := False;
  omLoadInit;                                                          {!!.52}
  try
    Result := omParser.ParseMemory(Buffer, aSize);
    omDocument.docBaseURI := '';                                       {!!.53}
  except
    omHandleErr;                                                       {!!.53}
    if omRaiseErrors then
      raise;
  end;
end;
{Begin !!.52}
{--------}
function TXpObjModel.LoadStream(oStream : TStream) : Boolean;
begin
  Result := False;
  omLoadInit;
  try
    Result := omParser.ParseStream(oStream);
    omDocument.docBaseURI := '';                                       {!!.53}
  except
    omHandleErr;                                                       {!!.53}
    if omRaiseErrors then
      raise;
  end;
end;
{End !!.52}
{--------}
function TXpObjModel.SaveToFile(sFile : string) : Boolean;
var
  aStream : TFileStream;                                               {!!.51}
  aFilter : TXpOutCharFilter;                                          {!!.51}
  sTempFile : string;                                                  {!!.51}
  sText    : DOMString;
begin
  sText := XmlDocument;
  {$IFDEF XpUseInet}
  { Saving via FTP? }
  if XpPos('FTP://', UpperCase(sFile)) > 0 then begin
    Delete(sFile, 1, 6);
{Begin !!.51}
    sTempFile := XpGenTemporaryFile;
    aStream := TFileStream.Create(sTempFile, fmOpenWrite);
    aFilter := TXpOutCharFilter.Create(aStream, Length(sText) * 2);
    try
      aFilter.WriteUTF8Signature := omSetUTF8Sig;
      aFilter.Format := MapCharEncToStreamFormat(omOutCharSet);
      aFilter.PutString(sText);
    finally
      aFilter.Free;
      aStream.Free;
    end;
    Result := XpSaveToFTP(sFile, UserName, Password, sTempFile, omErrors);
{End !!.51}
  end else
  {$ENDIF}
  begin
    { No. Must be saving to local or network hard drive. }
    if XpPos('FILE://', UpperCase(sFile)) = 1 then
      Delete(sFile, 1, 7);
{Begin !!.51}
    aStream := TFileStream.Create(sFile, fmCreate);
    aFilter := TXpOutCharFilter.Create(aStream, Length(sText) * 2);
    try
      aFilter.WriteUTF8Signature := omSetUTF8Sig;
      aFilter.Format := MapCharEncToStreamFormat(omOutCharSet);
      aFilter.PutString(sText);
      Result := True;
    finally
      aFilter.Free;
      aStream.Free;
    end;
{End !!.51}
  end;
end;
{Begin !!.52}
{--------}
function TXpObjModel.SaveToStream(oStream : TStream) : Boolean;
var
  aFilter : TXpOutCharFilter;
  sText    : DOMString;
begin
  sText := XmlDocument;
  aFilter := TXpOutCharFilter.Create(oStream, Length(sText) * 2);
  try
    aFilter.WriteUTF8Signature := omSetUTF8Sig;
    aFilter.Format := MapCharEncToStreamFormat(omOutCharSet);
    aFilter.PutString(sText);
    Result := True;
  finally
    aFilter.Free;
  end;
end;
{End !!.52}
{--------}
function TXpObjModel.ValidateDocument : Boolean;
var
  bStop : Boolean;
  oDTD  : TXpDocumentType;
  oRoot : TXpElement;
begin
  Result := True;
  bStop := False;

  { Check for DTD }
  oDTD := omDocument.DocType;
  if oDTD = nil then begin
    if Assigned(omOnInvalidDocument) then
      omOnInvalidDocument(self, V_NODTD, nil, bStop);
    Result := False;
    Exit;
  end;

  { Check for document }
  oRoot := omDocument.DocumentElement;
  if oRoot = nil then begin
    if Assigned(omOnInvalidDocument) then
      omOnInvalidDocument(self, V_NODOCUMENT, nil, bStop);
    Result := False;
    Exit;
  end;

  { Check names of DTD doc and root element }
  if oDTD.Name <> oRoot.NodeName then begin
    if Assigned(omOnInvalidDocument) then
      omOnInvalidDocument(self, V_MISMATCH, nil, bStop);
    Result := False;
    if bStop then
      Exit;
  end;

  { Standalone validity constraints }
  { needs to be implemented...}
  if omParser.IsStandAlone and
     omParser.HasExternals then begin
    if Assigned(omOnInvalidDocument) then
      omOnInvalidDocument(self, V_STANDALONEVIOLATED, nil, bStop);
    Result := False;
    if bStop then
      Exit;
  end;

  { Check elements against DTD }
  try
    if not omSubValidateElements(oRoot, oDTD) then
      Result := False;
  except
    Result := False;
  end;

end;
{Begin !!.55}
{--------}
function TXpObjModel.ValidateElement(oElem : TXpElement;
                                     oDTD  : TXpDocumentType) : Boolean;
begin
  Result := omSubValidateElements(oElem, oDTD);
end;
{End !!.55}
{=====================================================================}



{== TXpXqlToken ======================================================}
function TXpXqlToken.CloneNode(bDeep : Boolean) : TXpNode;
var
  oNode    : TXpXqlToken;
  i        : Integer;
  oNewNode : TXpNode;
begin
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
  oNode := TXpXqlToken.Create;
  oNode.noNodeType := noNodeType;
  oNode.noNodeName := noNodeName;
  oNode.noNodeValue := noNodeValue;
  oNode.qtTokenId := qtTokenId;
  oNode.noOwnerDocument := noOwnerDocument;
  Result := oNode;
  if bDeep and HasChildNodes then
    for i := 0 to noChildNodes.Length - 1 do begin
      oNewNode := noChildNodes.Item(i).CloneNode(True);
      Result.AppendChild(oNewNode);
      oNewNode.Release;
    end;
  Assert((noOwnerDocument = nil)
    or (TObject(noOwnerDocument) is TXpDocument));
end;
{--------}
constructor TXpXqlToken.Create;
begin
  inherited Create;
  noNodeType := XPATH_TOKEN;                                           {!!.52}
  noOutputEscaping := False;                                           {!!.52}
end;
{--------}
constructor TXpXqlToken.CreateToken(wTokenId : Integer);
begin
  Create;
  qtTokenId := wTokenId;
end;
{--------}
constructor TXpXqlToken.CreateTokenValue(wTokenId : Integer; const sValue : DOMString); {!!.57}
begin
  Create;
  qtTokenId := wTokenId;
  noNodeName := sValue;
end;
{====================================================================}

{====================================================================}
procedure InitializeUnit;
begin
  _Factories := TXpPointerList.Create;

  { Register default factory. }
  TXpElementFactory.Register(XpsDefaultFactoryID);

  xpoXPathFuncHash := TXpStrHash.Create(xpc_Size59);
  xpoXPathAxisHash := TXpStrHash.Create(xpc_Size59);

  { Add the functions to the XPath function hash table. }
  with xpoXPathFuncHash do begin
    Add(xpsBoolean, Pointer(TOK_BOOLEAN));
    Add(xpsCeiling, Pointer(TOK_CEILING));
    Add(xpsConcat, Pointer(TOK_FCONCAT));
    Add(xpsContains, Pointer(TOK_CONTAINS));
    Add(xpsCount, Pointer(TOK_ICOUNT));
    Add(xpsCurrent, Pointer(TOK_CURRENT));
    Add(xpsDocument, Pointer(TOK_DOCUMENT));
    Add(xpsFalse, Pointer(TOK_FALSE));
    Add(xpsFloor, Pointer(TOK_FLOOR));
    Add(xpsFormatNumber, Pointer(TOK_FORMAT_NUMBER));
    Add(xpsFuncAvail, Pointer(TOK_FUNCTION_AVAILABLE));
    Add(xpsGenID, Pointer(TOK_GENERATEID));
    Add(xpsID, Pointer(TOK_ID));
    Add(xpsKey, Pointer(TOK_KEY));
    Add(xpsLang, Pointer(TOK_LANG));
    Add(xpsLast, Pointer(TOK_LAST));
    Add(xpsLocalName, Pointer(TOK_LOCAL_PART));
    Add(xpsName, Pointer(TOK_QNAME));
    Add(xpsNamespURI, Pointer(TOK_NAMESPACEURI));
    Add(xpsElementAvail, Pointer(TOK_ELEMENT_AVAILABLE));
    Add(xpsNormSpace, Pointer(TOK_NORMALIZE));
    Add(xpsNot, Pointer(TOK_NOT));
    Add(xpsNumber, Pointer(TOK_NUMBER_CAST));
    Add(xpsPosition, Pointer(TOK_POSITION));
    Add(xpsRound, Pointer(TOK_ROUND));
    Add(xpsStartsWith, Pointer(TOK_STARTS_WITH));
    Add(xpsString, Pointer(TOK_STRING_CAST));
    Add(xpsStringLen, Pointer(TOK_STRING_LENGTH));
    Add(xpsSubstring, Pointer(TOK_SUBSTRING));
    Add(xpsSubstrAfter, Pointer(TOK_SUBSTRING_AFTER));
    Add(xpsSubstrBefore, Pointer(TOK_SUBSTRING_BEFORE));
    Add(xpsSum, Pointer(TOK_SUM));
    Add(xpsSystemProp, Pointer(TOK_SYSTEM_PROPERTY));
    Add(xpsTranslate, Pointer(TOK_TRANSLATE));
    Add(xpsTrue, Pointer(TOK_TRUE));
    Add(xpsUnprseEntURI, Pointer(TOK_UNPARSED_ENTITY_URI));
  end;  { with  }

  with xpoXPathAxisHash do begin
    { Future:: Turn these into constants }
    Add('ancestor::', pointer(TOK_AXIS_ANCESTOR));
    Add('ancestor-or-self::', pointer(TOK_AXIS_ANCESTOR_OR_SELF));
    Add('attribute::', pointer(TOK_AXIS_ATTRIBUTE));
    Add('child::', pointer(TOK_AXIS_CHILD));
    Add('descendant::', pointer(TOK_AXIS_DESCENDANT));
    Add('descendant-or-self::', pointer(TOK_AXIS_DESCENDANT_OR_SELF));
    Add('following::', pointer(TOK_AXIS_FOLLOWING));
    Add('following-sibling::', pointer(TOK_AXIS_FOLLOWING_SIBLING));
    Add('namespace::', pointer(TOK_AXIS_NAMESPACE));
    Add('parent::', pointer(TOK_AXIS_PARENT));
    Add('preceding::', pointer(TOK_AXIS_PRECEDING));
    Add('preceding-sibling::', pointer(TOK_AXIS_PRECEDING_SIBLING));
    Add('self::', pointer(TOK_AXIS_SELF));
  end;  { with }
end;
{--------}
procedure FinalizeUnit;
begin
  TXpElementFactory.Unregister;
  _Factories.Free;
    { Assumption: Units registering factory classes will also unregister
      them. }
  _Factories := nil;
  xpoXPathFuncHash.Free;
  xpoXPathAxisHash.Free;
end;
{====================================================================}

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
