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
{* XMLPartner Pro: XpXSLPrc.INC                          *}
{*********************************************************}
{* XMLPartner Pro: XSL Processor engine                  *}
{*********************************************************}

{$I XpDefine.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UsingCLX}
  QForms,
  QGraphics,
  XpQFlBas,
{$ELSE}
  Forms,
  Graphics,
  XpvFlBas,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  SysUtils,
  Classes,
  XpDOM,
  XpXSLCon,
  XpBase,
  XpHash,
{$IFDEF UsingCLX}
  XpQXSLFO,
  XpQXSLT;
{$ELSE}
  XpvXSLFO,
  XpvXSLT;
{$ENDIF}

type
  EXpXSL = class(Exception);

{== Event types ======================================================}
  TXpCDATAEvent = procedure(oOwner   : TObject;
                            sValue   : DOMString;
                            bReplace : Boolean) of object;
  TXpChooseStyleEvent = procedure(oOwner : TObject;
                                  oStylePIs : TXpNodeList;
                              var oChosenStyleIndex : Integer) of object;
  TXpFormatObjectEvent = procedure(oOwner  : TObject;
                                   aFormatObject : TXpFormattingObject) of object;

{Begin !!.55}
//  TXpFunctionEvent = procedure(oOwner : TObject;
//                               sName,
//                               sArgs  : DOMString;
//                           var sValue : DOMString) of object;
{End !!.55}
  TXpTextNodeEvent = procedure(oOwner : TObject; oNode : TXpText) of object;
  TXpPropertyEvent = procedure(oOwner : TObject;
                               wCode  : Integer;
                               sName,
                               sValue : DOMString) of object;
  TXpQueryFilterEvent = procedure(oOwner : TObject;
                              var sName  : string) of object;

{=====================================================================}

{===TXpXSLElementFactory==============================================}
  { This is a replacement factory for the DOM in XMLPartner Professional.
    It recognizes XSLT elements and instantiates the appropriate class. }
  TXpXSLElementFactory = class(TXpElementFactory)
  protected
  public
    function CreateElement(const sName : DOMString) : TXpElement; override;
  end;
{=====================================================================}

{===TXpDocumentCache==================================================}
  { This class caches documents loaded via the document() function.
    Speeds up processing by loading the documents once instead of loading
    them once each time they are referenced via document(). However, this
    means that a large amount of memory may be used. To avoid this
    problem, the cache is limited to 25 items. The limit may be changed via
    the MaxDocs property. The default value for MaxDocs is set in constant
    XpcMaxDocs at the end of the interface section. }
  TXpDocumentCache = class
  protected
    FCache : TXpStrHash;
      { Hash table containing the loaded documents. Hash on a Base URI and
        document name (e.g., d:\docs\mydoc.xml,
        http://www.patooey.com/spit.xml) and get back an instance of
        TXpObjModel. }
    FIndexHead : TXpDOMStringListItem;
      { Start of index on cached documents. The index is sorted from
        least used to most used document in the cache. The sString value of
        each item is the base URI and name of the XML document. The Tag value
        of each item is the # of times used since placed in the cache. A
        newly-cached document has Tag = 0.

        Note: The algorithm for maintaining the index does not use the
        Tag value. Instead, when a document is used then it moves it to the
        tail of the list. Documents that are frequently used will tend to be
        at the tail of the list. }
    FIndexTail : TXpDOMStringListItem;
      { The tail of the index. }
    FMaxDocs : Integer;
      { The maximum number of documents that may be stored in the cache. }
    FOwner : TObject;
      { The object that created this document cache. This is typically an
        instance of a class inheriting from TXpBaseXSLProcessor. }

    function AddToIndex(const sDocName : DOMString) : TXpDOMStringListItem;
      { Adds an entry to the index for the specified document name. The
        usage count is set to zero and the index entry is returned to the
        caller so that it may be stored on the DOM's Tag property. }

    procedure OnDisposeDoc(Sender : TXpStrHash; aData : Pointer);
      { Used to dispose of documents stored in the cache. }

    procedure RemoveLeastUsed;
      { Remove the least-used document from the cache. }

    procedure UpdateIndex(oItem : TXpDOMStringListItem);
      { Increments the usage count of a cached TXpObjModel via the specified
        TXpDOMStringListItem. The Tag property of a cached TXpObjModel is a
        reference to the DOM's associated TXpDOMStringListItem in the list
        pointed to by FUsageHead. }
  public
    constructor Create(oOwner : TObject);
    destructor Destroy; override;

    procedure Clear;
      { Empties the document cache. }

    function Count : Integer;
      { Returns the number of documents in the cache. }

    function Get(const sDocName : DOMString) : TXpObjModel;
      { Retrieves the document with the specified base URI and name. If the
        document is already in the cache then this method increments the
        documents use count and returns the root element of the document.
        If the document is not in the cache then this method loads the
        document and stores it in the cache. If the maximum number of documents
        has been reached then the least used document is removed from the
        cache. }

    function InCache(const sDocName : DOMString) : Boolean;
      { Returns True if the specified document is currently in the cache.
        Note that sDocName must include the base URI of the document as well
        as the name of the document. }

    property MaxDocs : Integer
      read FMaxDocs write FMaxDocs;
      { The maximum number of documents that may be stored in the cache.
        Defaults to 25. }
  end;
{=====================================================================}

{===TXpOutputManager==================================================}
  TXpOutputManager = class(TXpBaseOutputMgr)
  public
    function BuildFilter : TObject; override;
  end;
{=====================================================================}

{===TXpStripManager===================================================}

  TXpStripItem = class
    { Used by TXpStripManager to hold a pattern. }
  protected
    FStylesheetNode : TXpBaseXSLElement;
      { The xsl:strip-space or xsl:preserve-space element from which this
        pattern was obtained. }

    procedure SetStylesheetNode(oNode : TXpBaseXSLElement);

  public
    Pattern : TXpPattern;
      { The name pattern specified in the xsl:strip-space or xsl:preserve-space
        element. }
    Precedence : Integer;
    Strip : Boolean;
      { If True then the item is to be stripped of whitespace nodes otherwise
        the whitespace nodes are to be preserved. }
    destructor Destroy; override;

    property StylesheetNode : TXpBaseXSLElement
      read FStylesheetNode
      write SetStylesheetNode;
  end;

  { This class manages the information found in the xsl:strip-space
    & xsl:preserve-space elements. }
  TXpStripManager = class
  protected
    FItems : TXpPointerList;
      { Contains zero or more instances of TXpStripItem. Each item contains
        a pattern obtained from an xsl:strip-space or xsl:preserve-space
        element, sorted by import precedence & priority. }
    FLastElementName : DOMString;
    FLastResult : Boolean;
{$IFDEF UsingCLX}
    FProcessor : TXpQBaseXSLProcessor;
{$ELSE}
    FProcessor : TXpBaseXSLProcessor;
{$ENDIF}
    FStripping : Boolean;

    function CompareItems(oLItem, oRItem : TXpStripItem) : Integer;
    function GetInsertionPoint(oNewItem : TXpStripItem) : Longint;
    procedure StripNodesPrim(oElem : TXpElement);

  public
{$IFDEF UsingCLX}
    constructor Create(oProcessor : TXpQBaseXSLProcessor);
{$ELSE}
    constructor Create(oProcessor : TXpBaseXSLProcessor);
{$ENDIF}
    destructor Destroy; override;

    procedure Add(oContext : TXpXSLContext;
                  oSLNode : TXpXSLWhitespaceControl);
      { Use this to add one or more element name patterns to the strip manager's
        cache. Parameter oSLNode is the xsl:strip-space or xsl:preserve-space
        element from which the necessary information was obtained. }

    procedure Clear;

    function ShouldStrip(oElement : TXpElement) : Boolean;
      { Use this method to determine if whitespace nodes should be stripped
        for a particular element. Specify the element via parameter oElement.
        This method returns True if the element's whitespace nodes should be
        stripped (non-recursively). This method does not take into account
        any xml:space attributes on the element or its ancestors. }

    procedure StripNodes(oDoc : TXpDocument);
      { Use this method to perform whitespace node stripping on the specified
        document. This method traverses the tree, checking for stripping on
        each node. It assumes the TXpElement.StripWhitespaceNodes function
        will handle checking for xml:space attributes. }

    property Stripping : Boolean
      read FStripping;
      { Returns True if one or more xsl:strip-space elements containing
        one or more name patterns were encountered in the stylesheet. }
  end;
{=====================================================================}

{===TXpTemplateManager================================================}
  { This class manages a set of templates within a stylesheet (and
    all the stylesheets it imports and includes). }
  TXpTemplateManager = class(TXpBaseTemplateManager)
  protected
    FNamedTemplates : TXpStrHash;
      { Named templates organized by name. Hash on a mode and get back an
        instance of TXpPointerList containing templates sorted by
        import precedence. According to XSLT spec, Section 6, it is an
        error for a stylesheet to contain more than one template with the
        same name & import precedence.  Note that a named template having a
        match attribute may be in both the FNamedTemplates and FTemplates
        structures.}

    FTemplates : TXpStrHash;
      { Nameless templates organized by mode. Hash on a mode and get back
        an instance of TXpPointerList containing templates sorted by import
        precedence & priority. Note that a named template having a match
        attribute may be in both the FNamedTemplates and FTemplates structures.
      }

    FBuiltInModeTemplates : TXpStrHash;
      { Built-in templates that are dynamically created for specific modes. }

    FBuiltInElementDOM : TXpObjModel;
      { Holds the built-in template for element nodes. }
    FBuiltInElementTemplate : TXpXSLTemplate;

    FBuiltInTextAttribDOM : TxpObjModel;
      { Holds the built-in template for text & attribute nodes. }
    FBuiltInTextAttribTemplate : TXpXSLTemplate;

    FBuiltInPICommentDOM : TXpObjModel;
      { Holds the built-in template for processing instruction & comment
        nodes. }
    FBuiltInPICommentTemplate : TXpXSLTemplate;

    {$IFDEF UsingCLX}
    FProcessor : TXpQBaseXSLProcessor;
    {$ELSE}
    FProcessor : TXpBaseXSLProcessor;
    {$ENDIF}

    procedure AddPrim(oTemplate : TXpXSLTemplate;
                      oHash : TXpStrHash;
                      const sKey : DOMString);                         {!!.57}

    function CompareTemplates(oLTemp, oRTemp : TXpXSLTemplate) : Integer;

    function GetBuiltInTemplate(oNode : TXpNode;
                          const sMode : DOMString) : TXpXSLTemplate;
      { Returns the built-in template for a specific type of node. }

    function GetInsertionPoint(oTemplate : TXpXSLTemplate;
                               oList : TXpPointerList) : Longint;
      { Insert a template into a list of templates based upon import
        precedence & priority. }

    function GetModeTemplate(const sMode : DOMString) : TXpXSLTemplate;
      { Construct a built-in template to support a specific mode. }

    procedure OnDisposeDOM(Sender : TXpStrHash; aData : Pointer);
      { Used to dispose of dynamically-created DOMs for mode templates. }

    procedure OnDisposeHashEntry(Sender : TXpStrHash; aData : Pointer);

  public
    {$IFDEF UsingCLX}
    constructor Create(oProcessor : TXpQBaseXSLProcessor); virtual;
    {$ELSE}
    constructor Create(oProcessor : TXpBaseXSLProcessor); virtual;
    {$ENDIF}
    destructor Destroy; override;

    procedure Add(oTemplate : TXpXSLTemplate); override;
      { Adds a template to the manager's store of templates. }

    procedure Clear; override;

    function Match(oNode : TXpElement;
             const sMode : DOMString) : TXpXSLTemplate; override;
      { Use this method to find the template that matches a node. }

    function MatchImport(oTemplate : TXpXSLTemplate) : TXpXSLTemplate; override;
      { Find a template matching the specified template but having a lower
        import precedence. }

    function MatchName(const sName : DOMString) : TXpXSLTemplate; override;
      { Use this method to find a named template. }

    procedure SetMediator(oMediator : TXpElementMediator;
                          bUsingNS : Boolean); override;
      { Set the mediator for the built-in templates. }

  end;
{=====================================================================}

{=====================================================================}
  TXpFilterStackItem = class
    { Represents a set of filter information placed on a stack. Used
      when an element or processor needs to temporarily redirect the output of
      the stylesheet (i.e., xsl:param or xsl:variable elements that have a
      template body. }
  public
    Filter : TXpFilterBase;
    PrevItem : TXpFilterStackItem;
  end;
{=====================================================================}

{== TXpXSLProcessor ==================================================}
{$IFDEF UsingCLX}
  TXpQXSLProcessor = class(TXpQBaseXSLProcessor)
{$ELSE}
  TXpXSLProcessor = class(TXpBaseXSLProcessor)
{$ENDIF}
  protected
    FAppParms : TXpStrHash;
      { Parameters defined by the outside application. }
    FAppVars : TXpStrHash;
      { Variables defined by the outside application. }
    FDocMgr : TXpDocumentCache;
      { Manages documents cached via document() function. }
    FFilter : TXpFilterBase;
      { Static filter. May be replaced by a dynamic filter. }
    FFilterSav : TXpFilterBase;
      { If the static filter is overridden by a dynamic filter then the static
        filter is preserved via this variable. }
    FFilterStackTail : TXpFilterStackItem;
    FForEachCount : Integer;
    FGlobalBindings : TXpStrHash;
      { Variables bound at run-time. }
    FIdTag : DOMString;
    FIgnoreCase : Boolean;
    FNormalizeStylesheet : Boolean;
    FOnApplyStyleEnd : TNotifyEvent;
    FOnApplyStyleStart : TNotifyEvent;
    FOnAttribute : TXpAttributeEvent;
    FOnCDATASection : TXpCDATAEvent;
    FOnChooseStyle : TXpChooseStyleEvent;
    FOnElementEnd : TXpValueEvent;
    FOnElementStart : TXpValueEvent;
    FOnExternalStyle : TXpImportEvent;                                 {!!.57}
    FOnFormatObject : TXpFormatObjectEvent;
    FOnFormatObjectEnd : TNotifyEvent;
    FOnFunction : TXpFunctionEvent;
    FOnQueryFilter : TXpQueryFilterEvent;
    FOnText : TXpValueEvent;
    FOnTextNode : TXpTextNodeEvent;
    FPatternMaker : TXpPatternMaker;
    FStripMgr : TXpStripManager;
    FStyleDOM : TXpObjModel;
    FStyleURL : DOMString;
    FTextValue : DOMString;
    FUsingDynamicFilter : Boolean;
      { Set to True if using a dynamic filter created via xsl:output }
    FDOM : TXpObjModel;

    procedure AddStrip(oContext: TXpXSLContext;
                       oSLNode : TXpXSLWhitespaceControl); override;
    procedure ApplyImports(Context : TXpXSLContext); override;
    procedure Clear;
    procedure CopyAttribute(const sName, sValue : DOMString;
                                  oCurrentSLNode : TXpNode); override;
    procedure CopyCDATANode(oNode : TXpCDATASection); override;
    procedure CopyText(const sValue : DOMString); override;
    procedure CopyTextNode(oNode : TXpText); override;
    procedure EndElement(oNode : TxpNode); override;
    procedure FOEnd; override;
      { End the copy of a formatting object to the result tree. }

    procedure CopyFO(oNode : TXpNode); override;
      { Copy of a formatting object to the result tree. }
    function GetCurrentResultTreeElement : TXpElement; override;
      { Get the current element in the result tree. Used by xsl:attribute. }
    function GetEmbeddedStylesheet : TXpXSLStylesheet;
    function GetOnImport : TXpImportEvent;                             {!!.57}
    function GetPatternMaker : TXpPatternMaker; override;
    function GetPsuedoAttr(const sName, sValue : DOMString) : DOMString;
      { Retrieve an attribute value from an xml-stylesheet PI. }
    function GetStyleData : DOMString;
    function GetStyleURL : DOMString; override;
      { Return the URL of the stylesheet to an XSLT element. }
    function Ignore(oNode : TXpNode) : Boolean; virtual;
      { Determines if the node should be ignored (e.g., the <?xsl?> prolog). }
    function HasFilter : Boolean; override;
      { Returns True if a filter is attached to the XSL processor. For use
        by element mediator. }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnCurrentNode(oOwner : TObject; var oCurrentNode : TXpNode);
    procedure OnDisposeVar(Sender : TXpStrHash; aData : Pointer);
    procedure OnElementAvailable(oNode : TXpNode;
                           const sElementName : DOMString;
                             var bAvailable : Boolean);
      { Handler passed to XmlObjModel. Used to determine if an xsl/fo element
        or extension element is available. }
    procedure OnKeyLookup(oDoc : TXpDocument;
                    const sKeyName, sKeyValue : DOMString;
                          oList : TXpNodeList);
      { Handler passed to XmlObjModel. Used to lookup key values during
        expression evaluation. }
    procedure OnResolveDocument(oNode : TXpNode;
                          const sHref, sBaseURI : DOMString;
                            var oRoot : TXpDocument);
      { Handler passed to XmlObjModel. Used to resolve document() functions
        during expression evaluation. }
    procedure OnVariableLookup(oOwner    : TObject;
                        const sVarName  : DOMString;                   {!!.57}
                          var oVarValue : TXpValue);
      { Handler passed to XmlObjModel. Used to lookup variable values during
        expression evaluation. }

    procedure PopDynamicFilter;
      { Replaces a dynamic filter with the save static filter. }

    function PopFilter : TObject; override;
      { Returns the current filter, replacing it with the top filter on the
        filter stack. Calling routine is responsible for freeing the
        returned filter. }

    procedure ProcessDocument(oDoc : TXpDocument);
    procedure ProcessSimpleStylesheet(oNode : TXpNode);
    procedure ProcPreserveSpace(oOwner       : TObject;
                                sElementName : DOMString;
                            var bPreserve    : Boolean);
    procedure PushDynamicFilter(oFilter : TXpFilterBase);
      { Replaces the static filter with a dynamic filter. }
    procedure PushFilter(oFilter : TObject); override;
      { Pushes the current filter onto the filter stack, replacing it with
        another filter. }
    procedure ResetGlobals(bVars : Boolean);
      { Reset all global xsl:variable or xsl:param elements so that they will
        preprocess the next time the stylesheet is applied to an XML document.
        If bVars is True then all xsl:variable elements are reset.
        If bVars is False then all xsl:parameter elements are reset. }
    procedure ResetGlobalVar(const sName : DOMString);
      { Reset a global xsl:variable or xsl:param element so that it will
        preprocess the next time the stylesheet is applied to an XML document. }
    procedure SetFilter(oFilter : TXpFilterBase);
    procedure SetGlobalParm(const sName : DOMString;
                                 oValue : TXpValue); override;
      { Set a global parameter. }

    procedure SetGlobalVar(const sName : DOMString;
                                 oValue : TXpValue); override;
      { Set a global variable. }
    procedure SetIgnoreCase(const Value : Boolean);
    procedure SetOnImport(const ImportEvent : TXpImportEvent);         {!!.57}
    procedure SetStyleData(Value : DOMString);
    procedure SetStyleURL(const Value : DOMString);
    procedure StartElement(oNode : TxpNode); override;
    procedure StripWhitespace(var sTerm : DOMString);
    procedure TemplateToText(oOwner : TObject; const sValue : DOMString); {!!.57}

{$IFDEF DACTIVEX}
    function GetHeight : Integer;
    function GetWidth : Integer;
    procedure SetHeight(const Value : Integer);
    procedure SetWidth(const Value : Integer);
{$ENDIF}

  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

    function ApplyStyle : Boolean;

    procedure ClearParameters;
    function GetParameter(const sName : DOMString) : TXpValue;
    procedure SetParameter(const sName : DOMString; oValue : TXpValue);

    procedure ClearVariables;
    function GetVariable(const sName : DOMString) : TXpValue;
    procedure SetVariable(const sName : DOMString; oValue : TXpValue);

    property StyleData : DOMString
      read GetStyleData
      write SetStyleData;

{$IFDEF DACTIVEX}
    procedure SetBounds(aLeft,
                        aTop,
                        aWidth,
                        aHeight : Integer); override;
{$ENDIF}

  published
    { Published declarations }
    property Filter : TXpFilterBase
      read FFilter
      write SetFilter;

    property IgnoreCase : Boolean
      read FIgnoreCase
      write SetIgnoreCase
      default True;

    property NormalizeStylesheet : Boolean
      read FNormalizeStylesheet
      write FNormalizeStylesheet
      default False;
      { If set to True then the stylesheet's whitespace is normalized when
        the stylesheet is read into memory. Normally, only the stylesheet's
        whitespace nodes are stripped. With this property set to True, the
        text nodes containing non-whitespace characters also have their
        whitespace normalized. }

    property OnApplyStyleEnd : TNotifyEvent
      read FOnApplyStyleEnd
      write FOnApplyStyleEnd;

    property OnApplyStyleStart : TNotifyEvent
      read FOnApplyStyleStart
      write FOnApplyStyleStart;

    property OnAttribute : TXpAttributeEvent
      read FOnAttribute
      write FOnAttribute;

    property OnCDATASection : TXpCDATAEvent
      read FOnCDATASection
      write FOnCDATASection;

    property OnChooseStylesheet : TXpChooseStyleEvent
      read FOnChooseStyle
      write FOnChooseStyle;
      { Raised when no stylesheet has been specified & the XML document
        contains more than one reference (e.g., xml-stylesheet processing
        instruction) to a stylesheet. }

    property OnElementEnd : TXpValueEvent
      read FOnElementEnd
      write FOnElementEnd;

    property OnElementStart : TXpValueEvent
      read FOnElementStart
      write FOnElementStart;

    property OnExternalStyle : TXpImportEvent                          {!!.57}
      read FOnExternalStyle                                            {!!.57}
      write FOnExternalStyle;                                          {!!.57}

    property OnFormatObject : TXpFormatObjectEvent
      read FOnFormatObject
      write FOnFormatObject;

    property OnFormatObjectEnd : TNotifyEvent
      read FOnFormatObjectEnd
      write FOnFormatObjectEnd;

    property OnFunction : TXpFunctionEvent
      read FOnFunction
      write FOnFunction;

    property OnImport : TXpImportEvent                                 {!!.57}
      read GetOnImport                                                 {!!.57}
      write SetOnImport;                                               {!!.57}

    property OnQueryFilter : TXpQueryFilterEvent
      read FOnQueryFilter
      write FOnQueryFilter;

    property OnText : TXpValueEvent
      read FOnText
      write FOnText;

    property OnTextNode : TXpTextNodeEvent
      read FOnTextNode
      write FOnTextNode;

    property StyleURL : DOMString
      read FStyleURL
      write SetStyleURL;

    property XmlObjModel : TXpObjModel
      read FDOM
      write FDOM;

{$IFDEF DACTIVEX}
    property Height : Integer
      read GetHeight
      write SetHeight;

    property Width : Integer
      read GetWidth
      write SetWidth;
{$ENDIF}
  end;

const
  XpcMaxDocs : Integer = 25;
    { The default maximum number of documents that may be maintained by an
      instance of TXpDocumentCache. }

implementation

uses
  XpChrFlt,                                                            {!!.58}
  XpExcept,
  XpParser,
{$IFDEF UsingCLX}
  XpQFlHTM,
  XpQFlXML,
{$IFDEF MSWindows}
  XpQFlRTF,
{$ENDIF}
  XpQFOHsh;
{$ELSE}
  XpvFlHTM,
  XpvFlPrt,
  XpvFlRTF,
  XpvFlXML,
  XpvFOHsh;
{$ENDIF}

//{$R *.RES}

type
  EFormatType = (ftNumeric,
                 ftAlphaLower,
                 ftAlphaUpper,
                 ftRomanLower,
                 ftRomanUpper);

var
  xpoElementHash : TXpStrHash;
      { Used to map an XSLT element name to the TXpXSLxxx class that must
        be instantiated for that element within the DOM. Used by instances of
        TXpXSLElementFactory. }

{== TXpXSLProcessor ====================================================}
type
  TXpDOMCracker = class(TXpObjModel);

{$IFDEF UsingCLX}
constructor TXpQXSLProcessor.Create(oOwner : TComponent);
{$ELSE}
constructor TXpXSLProcessor.Create(oOwner : TComponent);
{$ENDIF}
begin
  inherited Create(oOwner);
  FAppParms := TXpStrHash.Create(xpc_Size59);
  FAppParms.OnDisposeData := OnDisposeVar;
  FAppVars := TXpStrHash.Create(xpc_Size59);
  FAppVars.OnDisposeData := OnDisposeVar;
  FDocMgr := TXpDocumentCache.Create(Self);
  FDOM := nil;
  FGlobalBindings := TXpStrHash.Create(xpc_Size59);
  FGlobalBindings.OnDisposeData := OnDisposeVar;
  FMediator := TXpElementMediator.Create(Self);
  FNormalizeStylesheet := False;
  FOutputMgr := TXpOutputManager.Create;
  FPatternMaker := TXpPatternMaker.Create;
  FStripMgr := TXpStripManager.Create(Self);
  FState := xppsIdle;
  FStyleDOM := nil;
  FTemplateMgr := TXpTemplateManager.Create(Self);
  FUsingDynamicFilter := False;
  FOnExternalStyle := nil;                                             {!!.57}
end;
{--------}
{$IFDEF UsingCLX}
destructor TXpQXSLProcessor.Destroy;
{$ELSE}
destructor TXpXSLProcessor.Destroy;
{$ENDIF}
begin
  Clear;
  FAppVars.Free;
  FAppParms.Free;
  FDocMgr.Free;
  FGlobalBindings.Free;
  FMediator.Free;
  FOutputMgr.Free;
  FPatternMaker.Free;
  FStripMgr.Free;
  FStyleDOM.Free;
  FTemplateMgr.Free;
  inherited Destroy;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.ApplyStyle : Boolean;
{$ELSE}
function TXpXSLProcessor.ApplyStyle : Boolean;
{$ENDIF}
var
  oNode : TXpDocument;
  oSavCurNode : TXpCurrentNodeEvent;
  oSavElAvail : TXpElementAvailableEvent;
  oSavFormatNum : TXpFormatNumberEvent;
  oSavFunction : TXpFunctionEvent;                                     {!!.55}
  oSavKeyLookup : TXpKeyLookupEvent;
  oSavResolveDoc : TXpResolveDocumentEvent;
  oSavVarLookup : TXpVariableLookupEvent;
  oValue : TXpValue;
  sName : string;
begin
  Result := False;
  FIdTag := '';

  FState := xppsExecuting;
  try
    FErrors.Clear;
    try
      { If no DOM specified then raise an error. }
      if FDOM = nil then
        raise EXpXSLException.Create(sXSLDOMRequired);

      { Preserve the old events & substitute our own. }
      oSavCurNode := FDOM.OnCurrentNode;
      oSavElAvail := FDOM.OnElementAvailable;
      oSavFormatNum := FDOM.OnFormatNumber;
      oSavFunction := FDOM.OnFunction;                                 {!!.55}
      oSavKeyLookup := FDOM.OnKeyLookup;
      oSavResolveDoc := FDOM.OnResolveDocument;
      oSavVarLookup := FDOM.OnVariableLookup;

      FDOM.OnCurrentNode := OnCurrentNode;
      FDOM.OnElementAvailable := OnElementAvailable;
      FDOM.OnFormatNumber := OnFormatNumber;
      FDOM.OnFunction := OnFunction;                                   {!!.55}
      FDOM.OnKeyLookup := OnKeyLookup;
      FDOM.OnResolveDocument := OnResolveDocument;
      FDOM.OnVariableLookup := OnVariableLookup;
      try
        oNode := FDOM.Document;
        if Assigned(FOnQueryFilter) then begin
          FOnQueryFilter(self, sName);
          oValue := TXpValue.Create;
          oValue.AsString := sName;
          FAppVars.Remove(XpsFilter);
          FAppVars.Add(XpsFilter, oValue);
        end;

        if Assigned(FOnApplyStyleStart) then
          FOnApplyStyleStart(Self);
        ProcessDocument(oNode);
        if Assigned(FOnApplyStyleEnd) then
          FOnApplyStyleEnd(Self);
        Result := True;
      finally
        FDOM.OnCurrentNode := oSavCurNode;
        FDOM.OnElementAvailable := oSavElAvail;
        FDOM.OnKeyLookup := oSavKeyLookup;
        FDOM.OnFormatNumber := oSavFormatNum;
        FDOM.OnFunction := oSavFunction;                               {!!.55}
        FDOM.OnResolveDocument := oSavResolveDoc;
        FDOM.OnVariableLookup := oSavVarLookup;
      end;
    except
      on E:Exception do begin
        FErrors.Add(E.Message);
        if FRaiseErrors then
          raise;
      end;
    end;
  finally
    FState := xppsIdle;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.AddStrip(oContext: TXpXSLContext;
                                    oSLNode : TXpXSLWhitespaceControl);
{$ELSE}
procedure TXpXSLProcessor.AddStrip(oContext: TXpXSLContext;
                                   oSLNode : TXpXSLWhitespaceControl);
{$ENDIF}
begin
  FStripMgr.Add(oContext, oSLNode);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ApplyImports(Context : TXpXSLContext);
{$ELSE}
procedure TXpXSLProcessor.ApplyImports(Context : TXpXSLContext);
{$ENDIF}
var
  oSavTemplate, oTemplate : TXpXSLTemplate;
begin
  { Find a template that matches the current template's mode and
    match attributes, but of lower import precedence. }
  oTemplate := FTemplateMgr.MatchImport(Context.CurrentTemplate);
  if oTemplate <> nil then begin
    oSavTemplate := Context.CurrentTemplate;
    try
      { Found a template. Apply it to the current node. }
      Context.CurrentTemplate := oTemplate;
      Context.MakeVarBinding;
      oTemplate.Execute(Context);
    finally
      Context.CurrentTemplate := oSavTemplate;
      Context.DropVarBinding;
    end;
  end;  { if }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.Clear;
{$ELSE}
procedure TXpXSLProcessor.Clear;
{$ENDIF}
begin
  { Get rid of a dynamic filter if we have one. }
  PopDynamicFilter;
  { Reset the managers. }
  try
    FStyleDOM.Free;
  finally
    FStyleDOM := nil;
  end;
  FDecimalFmtMgr.Clear;
  FDocMgr.Clear;
  FStripMgr.Clear;
  FTemplateMgr.Clear;
  FAttrSetMgr.Clear;
  FGlobalBindings.Clear;
  FKeyMgr.Clear;
  FNSAliasMgr.Clear;
  FOutputMgr.Clear;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ClearParameters;
{$ELSE}
procedure TXpXSLProcessor.ClearParameters;
{$ENDIF}
begin
  if FState <> xppsIdle then
    raise EXpXSLException.Create(sXSLCannotClearVars);
  FAppParms.Clear;
  ResetGlobals(False);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ClearVariables;
{$ELSE}
procedure TXpXSLProcessor.ClearVariables;
{$ENDIF}
begin
  if FState <> xppsIdle then
    raise EXpXSLException.Create(sXSLCannotClearVars);
  FAppVars.Clear;
  ResetGlobals(True);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.CopyAttribute(const sName, sValue : DOMString;
{$ELSE}
procedure TXpXSLProcessor.CopyAttribute(const sName, sValue : DOMString;
{$ENDIF}
                                              oCurrentSLNode : TXpNode);
var
  sNewName,
  sURI : DOMString;
begin
  if Assigned(FOnAttribute) then begin
    { Any namespace aliases in effect? }
    if FNSAliasMgr.HaveAliases then begin
      { Yes. Is this a namespace node? }
      if (sName = XpsXMLNS) or
         XpStartsWith(XpsXMLNS + ':', sName) then begin
        { Yes. Map the prefix and the URI. }
        sNewName := sName;
        sURI := sValue;
        FNSAliasMgr.MapNSNode(sNewName, sURI, oCurrentSLNode);
      end
      else
        { No. Map the prefix. }
        sNewName := FNSAliasMgr.MapName(sName);
      FOnAttribute(Self, sNewName, sValue);
    end
    else
      FOnAttribute(Self, sName, sValue);
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.CopyCDATANode(oNode : TXpCDATASection);
{$ELSE}
procedure TXpXSLProcessor.CopyCDATANode(oNode : TXpCDATASection);
{$ENDIF}
begin
  if Assigned(FOnCDATASection) then
    FOnCDATASection(Self, oNode.NodeValue, False);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.CopyFO(oNode : TXpNode);
{$ELSE}
procedure TXpXSLProcessor.CopyFO(oNode : TXpNode);
{$ENDIF}
begin
  if Assigned(FOnFormatObject) then
    FOnFormatObject(Self, TXpFormattingObject(oNode));
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.CopyText(const sValue : DOMString);
{$ELSE}
procedure TXpXSLProcessor.CopyText(const sValue : DOMString);
{$ENDIF}
begin
  if assigned(FOnText) then
    FOnText(Self, sValue);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.CopyTextNode(oNode : TXpText);
{$ELSE}
procedure TXpXSLProcessor.CopyTextNode(oNode : TXpText);
{$ENDIF}
begin
  if assigned(FOnTextNode) then
    FOnTextNode(Self, oNode)
  else if assigned(FOnText) then
    FOnText(Self, oNode.NodeValue);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.EndElement(oNode : TxpNode);
{$ELSE}
procedure TXpXSLProcessor.EndElement(oNode : TxpNode);
{$ENDIF}
begin
  if Assigned(FOnElementEnd) then
    FOnElementEnd(Self, oNode.NodeName);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.FOEnd;
{$ELSE}
procedure TXpXSLProcessor.FOEnd;
{$ENDIF}
begin
  if Assigned(FOnFormatObjectEnd) then
    FOnFormatObjectEnd(Self);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetCurrentResultTreeElement : TXpElement;
{$ELSE}
function TXpXSLProcessor.GetCurrentResultTreeElement : TXpElement;
{$ENDIF}
begin
  Result := FFilter.CurrentElement;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetEmbeddedStylesheet : TXpXSLStylesheet;
{$ELSE}
function TXpXSLProcessor.GetEmbeddedStylesheet : TXpXSLStylesheet;
{$ENDIF}
var
  oDirContext : TXpDirContext;
  oList       : TXpNodeList;
  oPI         : TXpProcessingInstruction;
  sHRef,                                                               {!!.57}
  sHRefPath   : DOMString;                                             {!!.57}
  wInx        : Integer;
begin
  Result := nil;
  { Any xml-stylesheet PIs found? }
  if FDOM.StylePIs.Length > 0 then begin
    { Yes. More than one found? }
    wInx := 0;
    if FDOM.StylePIs.Length > 1 then begin
      { Yes. Raise an event so that the application can determine which one
        to use. }
      if Assigned(FOnChooseStyle) then
        FOnChooseStyle(Self, FDOM.StylePIs, wInx);
    end;
    if wInx >= 0 then begin
      oPI := TXpProcessingInstruction(FDOM.StylePIs.Item(wInx));
      { Obtain the reference to the stylesheet. }
      sHRef := GetPsuedoAttr(XpsHref, oPI.NodeValue);
      { Is this a reference to an internal stylesheet? }
      if (sHRef <> '') then begin
        if sHRef[1] = '#' then begin
          { Yes. Refers to an internal stylesheet. Find it. }
          sHRef := XpCopy(sHRef, 2, Length(sHRef) - 1);
          oList := FDOM.Document.DocumentElement.SelectNodes
                     ('//' + XpsXSLStylesheet + '[@id=' + QuotedStr(sHRef) +
                      ']');
          if oList.Length > 0 then
            Result := TXpXSLStylesheet(oList.Item(0));
          oList.Free;
        end
        else begin
          { No. Load the external stylesheet. }
          FStyleDOM := TXpObjModel.Create(nil);
          FStyleDOM.NormalizeData := FNormalizeStylesheet;
          FStyleDOM.FormattedOutput := False;
          FStyleDOM.OnPreserveSpace := ProcPreserveSpace;
          FStyleDOM.RaiseErrors := True;

          if (XpIsRelativePath(sHRef)) then                            {!!.57}
            sHRefPath := FDOM.DocLocation + sHRef                      {!!.57}
          else                                                         {!!.57}
            sHRefPath := sHRef;                                        {!!.57}

          if (Assigned(FOnExternalStyle)) then                         {!!.57}
            FOnExternalStyle(Self, sHRefPath);                         {!!.57}

          oDirContext := TXpDirContext.Make(nil,
                                            sHRefPath);                {!!.57}
          try
            TXpDOMCracker(FStyleDOM).omLoadInContext(oDirContext,
                                                     sHRefPath);           
            if not FNormalizeStylesheet then begin
              FStyleDOM.Document.Normalize(False);
              FStyleDOM.Document.DocumentElement.StripWhitespaceNodes(True);
            end;
            Result := TXpXSLStylesheet(FStyleDOM.DocumentElement);
{Begin !!.54}
            { Increment the reference count of the temporary stylesheet's owner
              document. This prevents them from being freed when we free the
              temporary DOM. }
            Result.OwnerDocument.AddRef;
//          except
          finally
            oDirContext.Free;
            FStyleDOM.Free;
            FStyleDOM := nil;
//            raise;
{End !!.54}
          end;
        end;
      end;
    end;
  end
  else begin
    { No xml-stylesheet PIs found. Search for an TXpXSLStylesheet element. }
    oList := FDOM.Document.DocumentElement.SelectNodes('//' + XpsXSLStylesheet);
    if oList.Length > 0 then
      Result := TXpXSLStylesheet(oList.Item(0));
    oList.Free;
  end;
end;
{--------}
{!!.57 - Begin}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetOnImport : TXpImportEvent;
{$ELSE}
function TXpXSLProcessor.GetOnImport : TXpImportEvent;
{$ENDIF}
begin
  Result := Mediator.OnImport;
end;
{!!.57 - End}
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetParameter(const sName : DOMString) : TXpValue;
{$ELSE}
function TXpXSLProcessor.GetParameter(const sName : DOMString) : TXpValue;
{$ENDIF}
begin
  { Is the stylesheet executing? }
  if FState = xppsExecuting then
    { Yes. Obtain the value from the current context. }
    Result := FContextTail.GlobalVars.Get(sName)
  else
    { No. Obtain the value from the store. }
    Result := FAppParms.Get(sName);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetPatternMaker : TXpPatternMaker;
{$ELSE}
function TXpXSLProcessor.GetPatternMaker : TXpPatternMaker;
{$ENDIF}
begin
  Result := FPatternMaker;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetStyleData : DOMString;
{$ELSE}
function TXpXSLProcessor.GetStyleData : DOMString;
{$ENDIF}
begin
  { Get first style sheet }
  Result := '';
  if FStyleDOM <> nil then
    Result := FStyleDOM.XMLDocument;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetPsuedoAttr(const sName, sValue : DOMString) : DOMString;
{$ELSE}
function TXpXSLProcessor.GetPsuedoAttr(const sName, sValue : DOMString) : DOMString;
{$ENDIF}
var
  bDone : Boolean;
  wInx,
  wPos,
  wValLen : Integer;
begin
  { Is the attribute in the PI's value string? }
  wInx := XpPos(sName, sValue);
  if wInx > 0 then begin
    { Yes. Move to just past the attribute name. }
    inc(wInx, Length(sName));
    wValLen := Length(sValue);
    SetLength(Result, wValLen);
    bDone := False;
    wPos := 0;
    { Run through the characters preceding the attribute value. Note that
      we are not doing any validation so something like
      attrName = = = 'attrValue' would be acceptable. }
    while (sValue[wInx] = ' ') or (sValue[wInx] = '=') do
      inc(wInx);
    { Next token should be a single or double quote. }
    if (sValue[wInx] = '''') or (sValue[wInx] = '"') then
      inc(wInx)
    else
      raise EXpXSLException.CreateFmt(sXSLInvalidXSLStyleRef,
                                   [sValue]);
    while (not bDone) and (wInx <= wValLen) do begin
      bDone := (sValue[wInx] = '''') or (sValue[wInx] = '"');
      if not bDone then begin
        inc(wPos);
        Result[wPos] := sValue[wInx];
        inc(wInx);
      end;
    end;
    if bDone then
      SetLength(Result, wPos)
    else
      raise EXpXSLException.CreateFmt(sXSLInvalidXSLStyleRef,
                                   [sValue]);
  end
  else
    Result := '';
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetStyleURL : DOMString;
{$ELSE}
function TXpXSLProcessor.GetStyleURL : DOMString;
{$ENDIF}
begin
  Result := FStyleURL;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetVariable(const sName : DOMString) : TXpValue;
{$ELSE}
function TXpXSLProcessor.GetVariable(const sName : DOMString) : TXpValue;
{$ENDIF}
begin
  { Is the stylesheet executing? }
  if FState = xppsExecuting then
    { Yes. Obtain the value from the current context. }
    Result := FContextTail.GlobalVars.Get(sName)
  else
    { No. Obtain the value from the store. }
    Result := FAppVars.Get(sName);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.Ignore(oNode : TXpNode) : Boolean;
{$ELSE}
function TXpXSLProcessor.Ignore(oNode : TXpNode) : Boolean;
{$ENDIF}
begin
  { Need to ignore the xsl prolog & embedded stylesheet commands. }
  Result := (oNode.NodeName = XpsXML) or
            (oNode.NodeName = XpsXSLStylesheet);
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.HasFilter : Boolean;
{$ELSE}
function TXpXSLProcessor.HasFilter : Boolean;
{$ENDIF}
begin
  Result := (FFilter <> nil);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.Notification(AComponent: TComponent;
                                        Operation: TOperation);
{$ELSE}
procedure TXpXSLProcessor.Notification(AComponent: TComponent;
                                       Operation: TOperation);
{$ENDIF}
begin
  if Operation = opRemove then
    if (FFilter = AComponent) then
      FFilter := nil
    else if (FFilterSav = AComponent) then
      FFilterSav := nil
    else if (FDOM = AComponent) then
      FDOM := nil;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnCurrentNode(oOwner : TObject;
{$ELSE}
procedure TXpXSLProcessor.OnCurrentNode(oOwner : TObject;
{$ENDIF}
                                    var oCurrentNode : TXpNode);
begin
  oCurrentNode := FContextTail.CurrentNode;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnDisposeVar(Sender : TXpStrHash; aData : Pointer);
{$ELSE}
procedure TXpXSLProcessor.OnDisposeVar(Sender : TXpStrHash; aData : Pointer);
{$ENDIF}
begin
  TXpValue(aData).Free;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnElementAvailable(oNode : TXpNode;
                                        const sElementName : DOMString;
                                          var bAvailable : Boolean);
{$ELSE}
procedure TXpXSLProcessor.OnElementAvailable(oNode : TXpNode;
                                       const sElementName : DOMString;
                                         var bAvailable : Boolean);
{$ENDIF}
begin
  if XpStartsWith(XpsXSLColon, sElementName) then
    bAvailable := (XpoElementHash.Get(sElementName) <> nil)
  else if XpStartsWith(XpsFO, sElementName) then
    bAvailable := (XpoFOHash.Get(sElementName) <> nil)
  else
    bAvailable := False;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnKeyLookup(oDoc : TXpDocument;
{$ELSE}
procedure TXpXSLProcessor.OnKeyLookup(oDoc : TXpDocument;
{$ENDIF}
                                const sKeyName, sKeyValue : DOMString;
                                      oList : TXpNodeList);
var
  oIndex : TXpStrHash;
  oNodes : TXpNodeList;
begin
  { Get the list of nodes for the specified document and key. }
  oIndex := FKeyMgr.KeysForDoc(sKeyName, oDoc, FContextTail);
  if oIndex <> nil then begin
    { Get the nodes matching a specific key value. }
    oNodes := oIndex.Get(sKeyValue);
    if oNodes <> nil then
      { Copy the nodes into the output list. }
      oList.CopyList(oNodes);
  end;  { if }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnResolveDocument(oNode : TXpNode;
{$ELSE}
procedure TXpXSLProcessor.OnResolveDocument(oNode : TXpNode;
{$ENDIF}
                                      const sHref, sBaseURI : DOMString;
                                        var oRoot : TXpDocument);
var
  oBaseContext,
  oContext : TXpDirContext;
  sTmpURI : DOMString;
begin
  { If the referenced document parameter is an empty string then return the
    current stylesheet. }
  if sHref = '' then
    oRoot := FContextTail.CurrentStylesheetNode.OwnerDocument
  else if FileExists(sHref) then
    oRoot := FDocMgr.Get(sHref).Document
  else begin
    { If the base URI is an empty string then use the base URI of the current
      stylesheet node. }
    if sBaseURI = '' then
      sTmpURI := FContextTail.CurrentStylesheetNode.BaseURI
    else
      sTmpURI := sBaseURI;

    { Create a context for the base URI.
      Requirement: base URI has trailing backslash or forwardslash. }
    oContext := nil;
    oBaseContext := TXpDirContext.Make(nil, sTmpURI + 'dummy.xml');
    try
      { Now create a context for the reference passing the base context
        in case the reference is relative. }
      oContext := TXpDirContext.Make(oBaseContext, sHref);

      { Get the document via the document cache. }
{Begin !!.58}
      if oContext.ContextType = xpdcLocal then
        oRoot := FDocMgr.Get(oContext.URL).Document
      else
        oRoot := FDocMgr.Get(sHref).Document;
{End !!.58}
    finally
      oBaseContext.Free;
      oContext.Free;
    end;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.OnVariableLookup(oOwner    : TObject;
{$ELSE}
procedure TXpXSLProcessor.OnVariableLookup(oOwner    : TObject;
{$ENDIF}
                                     const sVarName  : DOMString; {!!.57}
                                       var oVarValue : TXpValue);
var
  oValue : TXpValue;
begin
  oValue := FContextTail.GetVar(sVarName);
  if oValue <> nil then
    oVarValue := oValue.Clone;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.PopDynamicFilter;
{$ELSE}
procedure TXpXSLProcessor.PopDynamicFilter;
{$ENDIF}
begin
  if FUsingDynamicFilter then begin
    FFilter.Free;
    SetFilter(FFilterSav);
    FUsingDynamicFilter := False;
    FFilterSav := nil;
  end;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.PopFilter : TObject;
{$ELSE}
function TXpXSLProcessor.PopFilter : TObject;
{$ENDIF}
var
  oStackItem : TXpFilterStackItem;
begin
  Result := FFilter;
  SetFilter(FFilterStackTail.Filter);
  oStackItem := FFilterStackTail;
  FFilterStackTail := FFilterStackTail.PrevItem;
  oStackItem.Free;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ProcessDocument(oDoc : TXpDocument);
{$ELSE}
procedure TXpXSLProcessor.ProcessDocument(oDoc : TXpDocument);
{$ENDIF}
var
  oContext : TXpXSLContext;
  oList : TXpNodeList;
  oStyleSheet : TXpXSLStylesheet;
  oTemplate : TXpXSLTemplate;
begin
  // Kylix 1 raises a warning that NewSize is never initialized.       {!!.55}
  // This warning is invalid. }                                        {!!.55}
  { Stylesheet specified? }
  if FStyleDOM = nil then
    { No. Look for a reference to an embedded stylesheet. }
    oStylesheet := GetEmbeddedStylesheet
  else begin
    if FStyleDOM.DocumentElement is TXpXSLStylesheet then
      oStyleSheet := TXpXSLStylesheet(FStyleDOM.DocumentElement)
    else if FStyleDOM.DocumentElement is TXpElement then begin
      ProcessSimpleStylesheet(oDoc);
      Exit;
    end
    else
      raise EXpXSLException.Create
              (Format(sXSLInvalidStylesheet,
                      [QuotedStr(FStyleDOM.DocumentElement.NodeName)]));
  end;

  if oStylesheet <> nil then begin
    oStylesheet.Mediator := FMediator;
    { If a static XML filter is attached to the processor OR there is the
      possibility that an xsl:output statement will be invoked then
      turn on namespace management.
      FUTURE:: It would be nice if we could determine what kind of filter
      is created via xsl:output and turn on namespace management only if
      it happens to be an XML filter. }
    oStylesheet.NSMgr.Using := FOverrideFilter or
                               (FFilter <> nil) and (FFilter is TXpFilterXML);
    { Assign the mediator to the built-in templates. }
    FTemplateMgr.SetMediator(FMediator, oStylesheet.NSMgr.Using);
    oStylesheet.StartNSLevel;
    oList := TXpNodeList.Create;
    oContext := TXpXSLContext.Create;
    try
      PushContext(oContext);
      oContext.GlobalVars := FGlobalBindings;
      oContext.CurrentNode := oDoc;
      oContext.MakeVarBinding;                                         {!!.58}
      oList.Add(oDoc);
        { Note: oList will be freed when the context is freed. }
      oContext.CurrentNodeList := oList;
      { Preprocess the stylesheet's top-level nodes. Note that if the stylesheet
        includes or imports other stylesheets (e.g., xsl:include & xsl:import)
        then those stylesheets will be automatically preprocessed. }
      oStyleSheet.Preprocess(oContext);

      { Do we need to create a dynamic filter in response to xsl:output
        elements? }
      if (FOverrideFilter or (FFilter = nil)) and
         FOutputMgr.OutputSet then
        PushDynamicFilter(TXpFilterBase(FOutputMgr.BuildFilter));

      { Process the whitespace nodes within the source document.
        By this time, the xsl:strip-space & xsl:preserve-space
        elements have been preprocessed. }
      oDoc.Normalize(False);

      { Strip whitespace-only nodes as necessary. }
      FStripMgr.StripNodes(oDoc);

      { Is there a template that matches the document element? }
      oTemplate := FTemplateMgr.Match(TXpElement(oDoc), '');
      if oTemplate <> nil then
        { Yes. Execute the template. Note that other nodes in the XML document
          are processed only when xsl:apply-templates is called. }
        try
          oContext.MakeVarBinding;
          oTemplate.Execute(oContext);
        finally
          oContext.DropVarBinding;
        end;
    finally
      PopContext;
      oList.Free;
      oContext.Free;
      oStylesheet.EndNSLevel;
      oStylesheet.NSMgr.Clear;
{Begin !!.54}
      { Did we create a temporary stylesheet for this document? }
      if (FStyleDOM = nil) and (oStyleSheet <> nil) and
         (oStyleSheet.OwnerDocument <> FDOM.Document) then
        { Yes. Release its owner document. }
        oStyleSheet.OwnerDocument.Release;
{End !!.54}
    end;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ProcessSimpleStylesheet(oNode : TXpNode);
{$ELSE}
procedure TXpXSLProcessor.ProcessSimpleStylesheet(oNode : TXpNode);
{$ENDIF}
var
  oContext : TXpXSLContext;
  oList : TXpNodeList;
  oOldRoot : TXpNode;
  oStylesheet : TXpXSLStylesheet;
begin
  { Temporarily insert an xsl:stylesheet node into the stylesheet.
    We need it to hold the mediator. }
  oStylesheet := TXpXSLStylesheet(FStyleDOM.Document.CreateElement(XpsXSLStylesheet));
  { Set the version and xmlns:xsl attributes. }
  oStylesheet.SetAttribute(XpsVersion, XpXSLImplementation);
  oStylesheet.SetAttribute(XpsXSLNS, XpsXSLTURI);
  FStyleDOM.Document.ForceOwnerDocument(oStylesheet);
  try
    oOldRoot := FStyleDOM.Document.ReplaceChild
                  (oStylesheet, FStyleDOM.Document.DocumentElement);
    oStylesheet.AppendChild(oOldRoot);
    try
      oStylesheet.Mediator := FMediator;
      oStylesheet.NSMgr.Using := (FFilter <> nil) and (FFilter is TXpFilterXML);
      { Assign the mediator to the built-in templates. }
      FTemplateMgr.SetMediator(FMediator, oStylesheet.NSMgr.Using);
      oStylesheet.StartNSLevel;
      oList := TXpNodeList.Create;
      oContext := TXpXSLContext.Create;
      try
        PushContext(oContext);
        oContext.GlobalVars := FGlobalBindings;
        oContext.CurrentNode := oNode;
        oList.Add(oNode);
          { Note: oList will be freed when the context is freed. }
        oContext.CurrentNodeList := oList;
        oStyleSheet.Preprocess(oContext);

        try
          oContext.MakeVarBinding;
          oStylesheet.Execute(oContext);
        finally
          oContext.DropVarBinding;
        end;
      finally
        PopContext;
        oList.Free;
        oContext.Free;
        oStylesheet.EndNSLevel;
        oStylesheet.NSMgr.Clear;
      end;
    finally
      { Remove the temporary stylesheet. }
{Begin !!.54}
      oStylesheet.RemoveChild(oOldRoot);
      FStyleDOM.Document.ReplaceChild(oOldRoot, oStylesheet);
      oStylesheet.Release;
      oOldRoot.Release;
      oOldRoot.Release;
    end;
  finally
    oStylesheet.Free;
  end;
{End !!.54}
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ProcPreserveSpace(oOwner       : TObject;
{$ELSE}
procedure TXpXSLProcessor.ProcPreserveSpace(oOwner       : TObject;
{$ENDIF}
                                            sElementName : DOMString;
                                        var bPreserve    : Boolean);
begin
  if sElementName = XpsXSLText then
    bPreserve := True;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.PushDynamicFilter(oFilter : TXpFilterBase);
{$ELSE}
procedure TXpXSLProcessor.PushDynamicFilter(oFilter : TXpFilterBase);
{$ENDIF}
begin
  { Static filter saved already? }
  if FFilterSav = nil then
    { No. Save it. }
    FFilterSav := FFilter;

  FUsingDynamicFilter := True;
  SetFilter(oFilter);
  oFilter.Reset;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.PushFilter(oFilter : TObject);
{$ELSE}
procedure TXpXSLProcessor.PushFilter(oFilter : TObject);
{$ENDIF}
var
  oStackItem : TXpFilterStackItem;
begin
  { Save the current filter information. }
  oStackItem := TXpFilterStackItem.Create;
  oStackItem.Filter := FFilter;
  oStackItem.PrevItem := FFilterStackTail;
  FFilterStackTail := oStackItem;
  SetFilter(TXpFilterBase(oFilter));
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ResetGlobals(bVars : Boolean);
{$ELSE}
procedure TXpXSLProcessor.ResetGlobals(bVars : Boolean);
{$ENDIF}
var
  wInx : Integer;
  oList : TXpNodeList;
  oNode : TXpElement;
begin
  if FStyleDOM <> nil then begin
    oList := FStyleDOM.Document.DocumentElement.ChildNodes;
    for wInx := 0 to Pred(oList.Length) do begin
      oNode := TXpElement(oList.Item(wInx));
      if ((oNode is TXpXSLVariable) and bVars) or
         ((oNode is TXpXSLParam) and (not bVars)) then begin
        TXpXSLBaseVar(oNode).Reset;
        FGlobalBindings.Remove(oNode.GetAttribute(XpsName));
      end
    end;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.ResetGlobalVar(const sName : DOMString);
{$ELSE}
procedure TXpXSLProcessor.ResetGlobalVar(const sName : DOMString);
{$ENDIF}
var
  wInx : Integer;
  oList : TXpNodeList;
  oNode : TXpElement;
  sVarName : DOMString;                                                {!!.55}
begin
  if FStyleDOM <> nil then begin
    oList := FStyleDOM.Document.DocumentElement.ChildNodes;
    for wInx := 0 to Pred(oList.Length) do begin
      oNode := TXpElement(oList.Item(wInx));
{Begin !!.55}
      if (oNode is TXpXSLBaseVar) then begin
        sVarName := oNode.GetAttribute(XpsName);
        if sName = sVarName then begin
          TXpXSLBaseVar(oNode).Reset;
          FGlobalBindings.Remove(oNode.GetAttribute(XpsName));
          Break;
        end;  { if }
{End !!.55}
      end;  { if }
    end;  { for }
  end;  { if }
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetFilter(oFilter : TXpFilterBase);
{$ELSE}
procedure TXpXSLProcessor.SetFilter(oFilter : TXpFilterBase);
{$ENDIF}
begin
  FFilter := oFilter;
  if FFilter <> nil then begin
    FOnApplyStyleEnd := FFilter.XslProcessorApplyStyleEnd;
    FOnApplyStyleStart := FFilter.XslProcessorApplyStyleStart;
    FOnAttribute := FFilter.XslProcessorAttribute;
    FOnCDATASection := FFilter.XslProcessorCDATASection;
    FOnComment := FFilter.XslProcessorComment;
    FOnElementEnd := FFilter.XslProcessorElementEnd;
    FOnElementStart := FFilter.XslProcessorElementStart;
    FOnFormatObject := FFilter.XslProcessorFormatObject;
    FOnFormatObjectEnd := FFilter.XslProcessorFormatObjectEnd;
    FOnProcessingInstruction := FFilter.XslProcessorProcessingInstruction;
    FOnQueryFilter := FFilter.XslProcessorQueryFilter;
    FOnText := FFilter.XslProcessorText;
    FOnTextNode := FFilter.XslProcessorTextNode;
  end
  else begin
    FOnApplyStyleEnd := nil;
    FOnApplyStyleStart := nil;
    FOnAttribute := nil;
    FOnCDATASection := nil;
    FOnComment := nil;
    FOnElementEnd := nil;
    FOnElementStart := nil;
    FOnFormatObject := nil;
    FOnFormatObjectEnd := nil;
    FOnProcessingInstruction := nil;
    FOnQueryFilter := nil;
    FOnText := nil;
    FOnTextNode := nil;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetGlobalParm(const sName : DOMString;
{$ELSE}
procedure TXpXSLProcessor.SetGlobalParm(const sName : DOMString;
{$ENDIF}
                                              oValue : TXpValue);
var
  oTmpValue, oCloneValue : TXpValue;
begin
  { Has this value already been set (i.e., by a stylesheet with higher
    import precedence)? }
  if FGlobalBindings.Exists(sName) then
    Exit;

  { Was a value specified by the outside application? }
  oTmpValue := FAppParms.Get(sName);
  if oTmpValue = nil then
    { No. Clone the specified value. }
    oCloneValue := oValue.Clone
  else
    { Yes. Add the app-specified value to the bindings. }
    oCloneValue := oTmpValue.Clone;

  { Add the clone to the variable bindings. }
  FGlobalBindings.Add(sName, oCloneValue);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetGlobalVar(const sName : DOMString;
{$ELSE}
procedure TXpXSLProcessor.SetGlobalVar(const sName : DOMString;
{$ENDIF}
                                             oValue : TXpValue);
var
  oTmpValue, oCloneValue : TXpValue;
begin
  { Has this value already been set (i.e., by a stylesheet with higher
    import precedence)? }
  if FGlobalBindings.Exists(sName) then
    Exit;

  { Was a value specified by the outside application? }
  oTmpValue := FAppVars.Get(sName);
  if oTmpValue = nil then
    { No. Clone the specified value. }
    oCloneValue := oValue.Clone
  else
    { Yes. Add the app-specified value to the bindings. }
    oCloneValue := oTmpValue.Clone;

  { Add the clone to the variable bindings. }
  FGlobalBindings.Add(sName, oCloneValue);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetIgnoreCase(const Value : Boolean);
{$ELSE}
procedure TXpXSLProcessor.SetIgnoreCase(const Value : Boolean);
{$ENDIF}
begin
  FIgnoreCase := Value;
end;
{--------}
{!!.57 - Begin}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetOnImport(const ImportEvent : TXpImportEvent);
{$ELSE}
procedure TXpXSLProcessor.SetOnImport(const ImportEvent : TXpImportEvent);
{$ENDIF}
begin
  Mediator.OnImport := ImportEvent;
end;
{!!.57 - End}
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetStyleData(Value : DOMString);
{$ELSE}
procedure TXpXSLProcessor.SetStyleData(Value : DOMString);
{$ENDIF}
var
  bIsBEorLE : Boolean;
  sModValue : DOMString;
begin
  FStyleURL := '';
  Clear;
  if Value <> '' then begin
    FStyleDOM := TXpObjModel.Create(nil);
    try
      FStyleDOM.NormalizeData := FNormalizeStylesheet;
      FStyleDOM.OnPreserveSpace := ProcPreserveSpace;
      FStyleDOM.RaiseErrors := True;
      { Are there any marks to indicate it is big endian or little endian? }
      bIsBEorLE := ( (Value[1] = #$FEFF) or
                     (Value[1] = #$FFFE) );
      if bIsBEorLE then
        { Yes. Load it as is. }
        FStyleDOM.LoadMemory(Value[1], Length(Value) * 2)
      else begin
        { No. Force it to be recognized as little endian. }
        sModValue := DOMString(#$FEFF) + Value;
        FStyleDOM.LoadMemory(sModValue[1], Length(sModValue) * 2);
      end;
      if not FNormalizeStylesheet then begin
        FStyleDOM.Document.Normalize(False);
        FStyleDOM.Document.DocumentElement.StripWhitespaceNodes(True);
      end;
    except
      FStyleDOM.Free;
      FStyleDOM := nil;
      raise;
    end;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetStyleURL(const Value : DOMString);
{$ELSE}
procedure TXpXSLProcessor.SetStyleURL(const Value : DOMString);
{$ENDIF}
begin
  FStyleURL := '';
  Clear;
  if Value <> '' then begin
    FStyleDOM := TXpObjModel.Create(nil);
    try
      FStyleDOM.NormalizeData := FNormalizeStylesheet;
      FStyleDOM.OnPreserveSpace := ProcPreserveSpace;
      FStyleDOM.RaiseErrors := True;
      if FStyleDOM.LoadDataSource(Value) then begin
        if not FNormalizeStylesheet then begin
          FStyleDOM.Document.Normalize(False);
          FStyleDOM.Document.DocumentElement.StripWhitespaceNodes(True);
        end;
        FStyleURL := Value;
      end;
    except
      FStyleDOM.Free;
      FStyleDOM := nil;
      raise;
    end;
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetParameter(const sName : DOMString; oValue : TXpValue);
{$ELSE}
procedure TXpXSLProcessor.SetParameter(const sName : DOMString; oValue : TXpValue);
{$ENDIF}
begin
  { Is the stylesheet idle or has the global yet to be bound? }
  if (FState = xppsIdle) or (not FGlobalBindings.Exists(sName)) then begin
    { Yes. Add to store. }
    if FAppParms.Exists(sName) then
      FAppParms.Remove(sName);                                         {!!.55}
    FAppParms.Add(sName, oValue);
    ResetGlobalVar(sName);
  end
  else begin
    { No. The variable has been bound. Replace its value. }
    FGlobalBindings.Remove(sName);
    FGlobalBindings.Add(sName, oValue);
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetVariable(const sName : DOMString; oValue : TXpValue);
{$ELSE}
procedure TXpXSLProcessor.SetVariable(const sName : DOMString; oValue : TXpValue);
{$ENDIF}
begin
  { Is the stylesheet idle or has the global yet to be bound? }
  if (FState = xppsIdle) or (not FGlobalBindings.Exists(sName)) then begin
    { Yes. Add to store. }
    if FAppVars.Exists(sName) then
      FAppVars.Remove(sName);
    FAppVars.Add(sName, oValue);
    ResetGlobalVar(sName);
  end
  else begin
    { No. The variable has been bound. Replace its value. }
    FGlobalBindings.Remove(sName);
    FGlobalBindings.Add(sName, oValue);
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.StartElement(oNode : TxpNode);
{$ELSE}
procedure TXpXSLProcessor.StartElement(oNode : TxpNode);
{$ENDIF}
var
  sNewPrefix : DOMString;
begin
  if Assigned(FOnElementStart) then begin
    { Any namespace aliases in effect? }
    if FNSAliasMgr.HaveAliases then begin
      { Yes. Map as necessary. }
      sNewPrefix := FNSAliasMgr.MapPrefix(oNode);
      FOnElementStart(Self, sNewPrefix + oNode.LocalName);
    end
    else
      FOnElementStart(Self, oNode.NodeName);
  end;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.StripWhitespace(var sTerm : DOMString);
{$ELSE}
procedure TXpXSLProcessor.StripWhitespace(var sTerm : DOMString);
{$ENDIF}
begin
  while (Length(sTerm) > 0) and
        (sTerm[1] = ' ') do
    Delete(sTerm, 1, 1);
  while (Length(sTerm) > 0) and
        (sTerm[Length(sTerm)] = ' ') do
    Delete(sTerm, Length(sTerm), 1);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.TemplateToText(oOwner : TObject;
{$ELSE}
procedure TXpXSLProcessor.TemplateToText(oOwner : TObject;
{$ENDIF}
                                         const sValue : DOMString);    {!!.57}
begin
  FTextValue := FTextValue + sValue;
end;
{--------}

{$IFDEF DACTIVEX}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetHeight : Integer;
{$ELSE}
function TXpXSLProcessor.GetHeight : Integer;
{$ENDIF}
begin
  Result := inherited Height;
end;
{--------}
{$IFDEF UsingCLX}
function TXpQXSLProcessor.GetWidth : Integer;
{$ELSE}
function TXpXSLProcessor.GetWidth : Integer;
{$ENDIF}
begin
  Result := inherited Width;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetBounds(aLeft,
{$ELSE}
procedure TXpXSLProcessor.SetBounds(aLeft,
{$ENDIF}
                                    aTop,
                                    aWidth,
                                    aHeight : Integer);
begin
  if (csDesigning in ComponentState) then begin
    if (aWidth <> 28) then
      aWidth := 28;
    if (aHeight <> 28) then
      aHeight := 28;
  end else begin
    if (aWidth <> 0) then
      aWidth := 0;
    if (aHeight <> 0) then
      aHeight := 0;
  end;
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetHeight(const Value : Integer);
{$ELSE}
procedure TXpXSLProcessor.SetHeight(const Value : Integer);
{$ENDIF}
begin
  if csDesigning in ComponentState then
    inherited Height := 28
  else
    inherited Height := 0;
end;
{--------}
{$IFDEF UsingCLX}
procedure TXpQXSLProcessor.SetWidth(const Value : Integer);
{$ELSE}
procedure TXpXSLProcessor.SetWidth(const Value : Integer);
{$ENDIF}
begin
  if csDesigning in ComponentState then
    inherited Width := 28
  else
    inherited Width := 0;
end;
{$ENDIF}
{=====================================================================}

{===TXpXSLTElementFactory============================================}
function TXpXSLElementFactory.CreateElement(const sName : DOMString) : TXpElement;
var
  anElementClass : TXpElementClasses;
begin
  { Does the name contain the prefix 'xsl:'? }
  if XpStartsWith(XpsXSLColon, sName) then begin
    { Yes. Do we have a corresponding entry in our XSL element hash table? }
    anElementClass := xpoElementHash.Get(sName);
    if anElementClass <> nil then begin
      { Yes. Create the element class returned from the hash table. }
      Result := anElementClass.Create;
      Result.TagName := sName;
    end
    else begin
      { No. We don't recognize this XSLT element but we need to instantiate
        it anyway since it may have fallback processing. }
      Result := TXpElement.Create;
      Result.TagName := sName;
    end;
  end
  else if XpStartsWith(XpsFO, sName) then begin
    { If it starts with 'fo:' then see if it is a recognized object. }
    anElementClass := XpoFOHash.Get(sName);
    if anElementClass <> nil then begin
      Result := anElementClass.Create;
      Result.TagName := sName;
    end
    else begin
      { No. We don't recognize this XSFO element but we need to instantiate
        it anyway since it may have fallback processing. }
      Result := TXpElement.Create;
      Result.TagName := sName;
    end;
  end
  else begin
    { No. Create a regular element. }
    Result := TXpElement.Create;
    Result.TagName := sName;
  end;
end;
{=====================================================================}

{===TXpDocumentCache==================================================}
constructor TXpDocumentCache.Create(oOwner : TObject);
begin
  inherited Create;
  FCache := TXpStrHash.Create(xpc_Size59);
  FCache.OnDisposeData := OnDisposeDoc;
  FMaxDocs := XpcMaxDocs;
  FOwner := oOwner;
end;
{--------}
destructor TXpDocumentCache.Destroy;
begin
  Clear;
  FCache.Free;
  inherited;
end;
{--------}
function TXpDocumentCache.AddToIndex(const sDocName : DOMString) : TXpDOMStringListItem;
begin
  Result := TXpDOMStringListItem.Create;
  Result.sString := sDocName;
  Result.Tag := 0;
  { Is this the first document in the index? }
  if FIndexHead = nil then begin
    { Yes. }
    FIndexHead := Result;
    FIndexTail := Result;
  end
  else begin
    { No. Insert this one at the end.  }
    FIndexTail.NextItem := Result;
    Result.PrevItem := FIndexTail;
    FIndexTail := Result;
  end;
end;
{--------}
procedure TXpDocumentCache.Clear;
var
  oItem,
  oNextItem : TXpDOMStringListItem;
begin
  { Purge the index. }
  oItem := FIndexHead;
  while oItem <> nil do begin
    oNextItem := oItem.NextItem;
    oItem.Free;
    oItem := oNextItem;
  end;
  FIndexHead := nil;
  FIndexTail := nil;

  { Clear the cache. }
  FCache.Clear;
end;
{--------}
function TXpDocumentCache.Count : Integer;
begin
  Result := FCache.Count;
end;
{--------}
type
{$IFDEF UsingCLX}
  TXpXSLProcessorCracker = class(TXpQXSLProcessor);
{$ELSE}
  TXpXSLProcessorCracker = class(TXpXSLProcessor);
{$ENDIF}

function TXpDocumentCache.Get(const sDocName : DOMString) : TXpObjModel;
begin
  { Is the document already in the cache? }
  Result := FCache.Get(sDocName);
  if Result = nil then begin
    { No. Have we reached the maximum # of docs in the cache? }
    if FCache.Count = FMaxDocs then
      { Yes. Remove the least used document. }
      RemoveLeastUsed;

    { Load it and put it in the cache. }
    Result := TXpObjModel.Create(nil);
    Result.NormalizeData := False;
    Result.RaiseErrors := True;
    Result.LoadDataSource(sDocName);
    { Perform whitespace processing. }
    Result.Document.Normalize(False);

{$IFDEF UsingCLX}
    if FOwner is TXpQXSLProcessor then
{$ELSE}
    if FOwner is TXpXSLProcessor then
{$ENDIF}
      { Strip whitespace nodes as necessary. }
      TXpXSLProcessorCracker(FOwner).FStripMgr.StripNodes(Result.Document);
      { NOTE:: In the future, we could make this specific to class
        TXpBaseXSLProcessor. }
      with TXpXSLProcessorCracker(FOwner) do begin
        Result.OnCurrentNode := OnCurrentNode;
        Result.OnFormatNumber := OnFormatNumber;
        Result.OnKeyLookup := OnKeyLookup;
        Result.OnResolveDocument := OnResolveDocument;
        Result.OnVariableLookup := OnVariableLookup;
      end;  { with }

    { Add it to the usage index. }
    Result.Tag := Longint(AddToIndex(sDocName));

    { Add it to the cache. }
    FCache.Add(sDocName, Result);
  end
  else
    { Yes. Update the usage count. }
    UpdateIndex(TXpDOMStringListItem(Result.Tag));
end;
{--------}
function TXpDocumentCache.InCache(const sDocName : DOMString) : Boolean;
begin
  Result := FCache.Exists(sDocName);
end;
{--------}
procedure TXpDocumentCache.OnDisposeDoc(Sender : TXpStrHash; aData : Pointer);
begin
  TXpObjModel(aData).Free;
end;
{--------}
procedure TXpDocumentCache.RemoveLeastUsed;
var
  oItem : TXpDOMStringListItem;
begin
  { The least used document is at the head of the index. Remove it from the
    head. }
  oItem := FIndexHead;
  FIndexHead := oItem.NextItem;
  if FIndexHead = nil then
    FIndexTail := nil
  else
    FIndexHead.PrevItem := nil;

  { Remove the document from the cache. }
  FCache.Remove(oItem.sString);
  oItem.Free;
end;
{--------}
procedure TXpDocumentCache.UpdateIndex(oItem : TXpDOMStringListItem);
begin
  inc(oItem.Tag);
  { Is this the tail? }
  if FIndexTail <> oItem then begin
    { No. Pull it out of the list. Is it the head? }
    if oItem.PrevItem = nil then
      { Yes. The head is the next item. }
      FIndexHead := oItem.NextItem
    else
      oItem.PrevItem.NextItem := oItem.NextItem;

    oItem.NextItem.PrevItem := oItem.PrevItem;

    { Move this item to the end of the list. }
    FIndexTail.NextItem := oItem;
    oItem.PrevItem := FIndexTail;
    oItem.NextItem := nil;
    FIndexTail := oItem;
  end;
end;
{=====================================================================}

{===TXpStripItem======================================================}
destructor TXpStripItem.Destroy;
begin
  Pattern.Free;
  FStylesheetNode.Release;
  inherited;
end;
{--------}
procedure TXpStripItem.SetStylesheetNode(oNode : TXpBaseXSLElement);
begin
 FStylesheetNode := oNode;
 FStylesheetNode.AddRef;
end;
{=====================================================================}

{===TXpOutputManager==================================================}
function TXpOutputManager.BuildFilter : TObject;
var
  oFilterClass : TXpFilterClass;
  oItem : TXpDOMStringItem;
  sValue : DOMString;
{$IFDEF MSWINDOWS}
  oOutputNode : TXpXSLOutput;
  sPrefix : DOMString;
  sURI : DOMString;
  wInx : Integer;
{$ENDIF}
begin
  { What kind of filter should be created? }
  oItem := TXpDOMStringItem(FAttribs.Get(XpsMethod));
  if oItem = nil then begin
{$IFDEF MSWINDOWS}
    oOutputNode := nil;
{$ENDIF}
    sValue := ''
  end
  else begin
{$IFDEF MSWINDOWS}
    oOutputNode := TXpXSLOutput(oItem.Tag);
{$ENDIF}
    sValue := oItem.sString;
  end;

  { Default value or 'xml' specified? }
  if (sValue = '') or (sValue = XpsXML) then
    { Yes. Prepare to create an XML filter.
      NOTE: The specification lists several conditions for defaulting
      to HTML. However, those conditions are verifiable only after the result
      tree has been created. With our architecture we must create the filter
      first. }
    oFilterClass := TXpFilterXML
  else if sValue = XpsHTML then
    oFilterClass := TXpFilterHTML
  else if sValue = XpsText then
    oFilterClass := TXpFilterText
  else
{$IFDEF LINUX}
    raise EXpXSLException.Create(Format(sXSLUnrecognizedOutput,
                                        [sValue, XpsXSLOutput]));
{$ELSE}
  begin
    { Has a namespace prefix? }
    wInx := XpPos(':', sValue);
    if wInx > 0 then begin
      { Yes. Resolve the prefix. }
      sPrefix := XpCopy(sValue, 1, Pred(wInx));
      sURI := oOutputNode.ResolveNSPrefix(sPrefix);
      if sURI = '' then
        raise EXpXSLException.Create(Format(sXSLUnresolvedNSPrefix,
                                            [QuotedStr(sPrefix),
                                             XpsXSLOutput + ' element having ' +
                                             ' method attribute = ' +
                                             QuotedStr(sValue)]));

      { Is it a TurboPower URI (i.e., an XMLPartern-specific output method) ? }
      if sURI <> XpsTurboPowerURI then
        raise EXpXSLException.Create(Format(sXSLUnrecognizedOutput,
                                            [sValue, XpsXSLOutput]));

      { Get the XMLPartner-specific method. }
      sURI := XpCopy(sValue, Succ(wInx), Length(sValue) - wInx);
      if sURI = XpsRTF then
        oFilterClass := TXpFilterRTF
{$IFNDEF UsingCLX}
      else if sURI = XpsPrint then
        oFilterClass := TXpFilterPrint
{$ENDIF}
      else
        raise EXpXSLException.Create(Format(sXSLUnrecognizedOutput,
                                            [sValue, XpsXSLOutput]));
    end
    else
      raise EXpXSLException.Create(Format(sXSLUnrecognizedOutput,
                                          [sValue, XpsXSLOutput]));
  end;
{$ENDIF}

  Result := oFilterClass.Create(nil);

  { Set the filter properties.
    Note: No attributes apply to Print or RTF filters. }
  if oFilterClass = TXpFilterHTML then begin
    { doctype-public - future }

    { doctype-system - future }

    { encoding }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsEncoding));
    if oItem <> nil then begin
      sValue := oItem.sString;
      if sValue <> '' then
        TXpFilterHTML(Result).OutputEncoding := XpMapStringToCharEnc(sValue);
    end;  { if }

    { indent }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsIndent));
    if oItem <> nil then begin
      if oItem.sString = XpsYes then
        TXpFilterHTML(Result).FormattedOutput := True
      else if oItem.sString = XpsNo then
        TXpFilterHTML(Result).FormattedOutput := False
      else
        raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                            [sValue, XpsIndent,
                                             XpsXSLOutput]));
    end;  { if }

    { media-type: ignored }

    { version - future }

  end
  else if oFilterClass = TXpFilterText then begin
    { Set attributes specific to text output. }
    { encoding }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsEncoding));
    if oItem <> nil then begin
      sValue := oItem.sString;
      if sValue <> '' then
        TXpFilterXML(Result).OutputEncoding := XpMapStringToCharEnc(sValue);
    end;  { if }

    { media-type: ignored }

  end
  else if oFilterClass = TXpFilterXML then begin
    { Set attributes specific to XML output. }
    { doctype-public }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsDocTypePub));
    if (oItem <> nil) and (oItem.sString <> '') then
        TXpFilterXML(Result).DocTypePublic := oItem.sString;

    { doctype-system }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsDocTypeSys));
    if (oItem <> nil) and (oItem.sString <> '') then
        TXpFilterXML(Result).DocTypeSystem := oItem.sString;

    { encoding }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsEncoding));
    if oItem <> nil then begin
      sValue := oItem.sString;
      if sValue <> '' then
        TXpFilterXML(Result).OutputEncoding := XpMapStringToCharEnc(sValue);
    end;  { if }

    { indent }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsIndent));
    if oItem <> nil then begin
      if oItem.sString = XpsYes then
        TXpFilterXML(Result).FormattedOutput := True
      else if oItem.sString = XpsNo then
        TXpFilterXML(Result).FormattedOutput := False
      else
        raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                            [sValue, XpsIndent,
                                             XpsXSLOutput]));
    end;  { if }

    { media-type: ignored }

    { omit-xml-declaration }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsOmitXMLDecl));
    if oItem <> nil then begin
      if oItem.sString = XpsYes then
        TXpFilterXML(Result).OmitXMLDeclaration := True
      else if oItem.sString = XpsNo then
        TXpFilterXML(Result).OmitXMLDeclaration := False
      else
        raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                            [sValue, XpsOmitXMLDecl,
                                             XpsXSLOutput]));
    end;  { if }

    { standalone }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsStandalone));
    if oItem <> nil then begin
      TXpFilterXML(Result).OmitStandalone := not (oItem.sString <> '');
      if oItem.sString = XpsYes then
        TXpFilterXML(Result).Standalone := True
      else if oItem.sString = XpsNo then
        TXpFilterXML(Result).Standalone := False
      else
        raise EXpXSLException.Create(Format(sXSLInvalidAttrib,
                                            [sValue, XpsStandalone,
                                             XpsXSLOutput]));
    end;  { if }

    { version }
    oItem := TXpDOMStringItem(FAttribs.Get(XpsVersion));
    if oItem <> nil then begin
      sValue := oItem.sString;
      if sValue <> '' then
        TXpFilterXML(Result).XMLVersion := sValue;
    end;  { if }

  end;  { xml filter }

end;
{=====================================================================}

{===TXpStripManager===================================================}
{$IFDEF UsingCLX}
constructor TXpStripManager.Create(oProcessor : TXpQBaseXSLProcessor);
{$ELSE}
constructor TXpStripManager.Create(oProcessor : TXpBaseXSLProcessor);
{$ENDIF}
begin
  inherited Create;
  FItems := TXpPointerList.Create;
  FLastElementName := '';
  FLastResult := False;
  FProcessor := oProcessor;
  FStripping := False;
end;
{--------}
destructor TXpStripManager.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;
{--------}
procedure TXpStripManager.Add(oContext : TXpXSLContext;
                              oSLNode : TXpXSLWhitespaceControl);
var
  bInWhite : Boolean;
  oItem : TXpStripItem;
  oPatternMaker : TXpPatternMaker;
  sNames : DOMString;
  wInx,
  wLen,
  wStart : Integer;
begin
  sNames := oSLNode.ElementNames;
  if sNames = '' then
    Exit;

  { Once FStripping is set to True, never reset to False. }
  if oSLNode.ShouldStrip then
    FStripping := True;

  oPatternMaker := TXpPatternMaker.Create;
  try
    wLen := Length(sNames);
    wInx := 1;
    wStart := 1;
    bInWhite := True;
    while wInx <= wLen do begin
      { Find the start of a name. }
      while bInWhite and (wInx <= wLen) do begin
        bInWhite := XpIsWhiteSpace(sNames[wInx]);
        if not bInWhite then
          wStart := wInx
        else
          inc(wInx);
      end;  { while }

      { Find the end of the name. }
      while (not bInWhite) and (wInx <= wLen) do begin
        bInWhite := XpIsWhiteSpace(sNames[wInx]);
        if not bInWhite then
          inc(wInx);
      end;  { while }

      if Pred(wInx) >= wStart then begin
        oItem := TXpStripItem.Create;
        oItem.Strip := oSLNode.ShouldStrip;
        oItem.StylesheetNode := oSLNode;
        oItem.Precedence := oSLNode.ParentStylesheet.ImportPrecedence;
        oItem.Pattern := oPatternMaker.GeneratePattern
                           (oContext.CurrentStylesheetNode,
                            XpCopy(sNames, wStart, wInx - wStart));
        FItems.Insert(Pointer(oItem), GetInsertionPoint(oItem));
      end;
    end;  { while }
  finally
    oPatternMaker.Free;
  end;
end;
{--------}
procedure TXpStripManager.Clear;
var
  wInx : Integer;
begin
  for wInx := Pred(FItems.Count) downto 0 do begin
    TXpStripItem(FItems.Pointers[wInx]).Free;
    FItems.RemoveAt(wInx);
  end;
end;
{--------}
function TXpStripManager.CompareItems(oLItem, oRItem : TXpStripItem) : Integer;
begin
  if (oLItem.Precedence = oRItem.Precedence) then begin
    { Import precedence is equal. Test priority. }
    if (oLItem.Pattern.Priority = oRItem.Pattern.Priority) then
      Result := 0
    else if (oLItem.Pattern.Priority < oRItem.Pattern.Priority) then
      Result := -1
    else
      Result := 1;
  end
  else if (oLItem.Precedence < oRItem.Precedence) then
    Result := -1
  else
    Result := 1;
end;
{--------}
function TXpStripManager.GetInsertionPoint(oNewItem : TXpStripItem) : Longint;
var
  L, R, M : Longint;
  oItem : TXpStripItem;
  wResult : Integer;
begin
  if FItems.Count = 0 then
    Result := 0
  else begin
    { The item needs to be positioned in the list based upon its import
      precedence & priority. }
    L := 0;
    R := Pred(FItems.Count);
    repeat
      M := (L + R) div 2;
      oItem := TXpStripItem(FItems.Pointers[M]);
      wResult := CompareItems(oItem, oNewItem);
      if wResult = 0 then begin
        { Equivalent. Put the new item after the item already in the
          list. }
        L := Succ(M);
        break;
      end
      else if wResult < 0 then
        R := M - 1
      else
        L := M + 1;
    until L > R;
    Result := L;
  end;
end;
{--------}
function TXpStripManager.ShouldStrip(oElement : TXpElement) : Boolean;
var
  bMatched, bRaiseError : Boolean;
  wInx : Integer;
  wCount : Longint;
  oConflictItem,
  oItem,
  oMatchItem : TXpStripItem;
  oConflictList : TXpPointerList;
begin
  { No items in the cache? }
  if FItems.Count = 0 then
    Result := False
  { Is this the same element name as last time? }
  else if oElement.NodeName = FLastElementName then
    Result := FLastResult
  else begin
    Result := False;
    { Find the matching pattern with highest import precedence & import
      priority that matches. Watch out for situation where multiple patterns
      match. }
    { For now do a simple sequential search from the beginning of the
      list until we encounter a match. }
    wInx := 0;
    wCount := FItems.Count;
    oConflictList := nil;
    oMatchItem := nil;
    repeat
      oItem := TXpStripItem(FItems.Pointers[wInx]);
      bMatched := oItem.Pattern.Matches(oElement, nil);
      { Found a match? }
      if bMatched then
        oMatchItem := oItem
      else
        inc(wInx);
    until (wInx = wCount) or bMatched;

    try
      { Did we find a matching pattern & it is not the last in the list? }
      if (oMatchItem <> nil) and (wInx < Pred(wCount)) then begin
        { We have a match. We need to see if any of the subsequent pattern(s)
          match this element & if they have equivalent import precedence & priority.
          If so then we need to resolve the situation. }
        inc(wInx);
        repeat
          oConflictItem := TXpStripItem(FItems.Pointers[wInx]);
          { Do they have the same import precedence & priority? }
          if CompareItems(oConflictItem, oMatchItem) < 0 then
            { No. Break out of this loop. }
            Break
          else begin
            { Yes. Does the subsequent item match the node? }
            if oConflictItem.Pattern.Matches(oElement, nil) then begin
              { Yes. Add it to the conflict list. }
              if oConflictList = nil then
                oConflictList := TXpPointerList.Create;
              oConflictList.Append(oConflictItem);
            end;
          end;
          inc(wInx);
        until (wInx = wCount);

        { Do we have a conflict? }
        if oConflictList <> nil then begin
          { Yes. Ask the XSL processor how the situation should be resolved. }
          bRaiseError := True;
          if Assigned(FProcessor.OnResolveConflict) then
            FProcessor.OnResolveConflict(FProcessor, oElement,
                                         xpctStripPatterns, bRaiseError);
          if bRaiseError then begin
              raise EXpXSLException.Create(Format(sXSLStripMatch,
                                                  [QuotedStr(oElement.NodeName)]));
          end
          else begin
            { Return the last item in the conflict list. }
            for wInx := 0 to Pred(oConflictList.Count) do begin
              oConflictItem := TXpStripItem(oConflictList.Pointers[wInx]);
              if oConflictItem.StylesheetNode.IsAfter(oMatchItem.StylesheetNode) then
                oMatchItem := oConflictItem;
            end;
          end;
        end;  { if }
      end;  { if }
    finally
      oConflictList.Free;
    end;  { try..finally }

    if oMatchItem <> nil then
      Result := oMatchItem.Strip;
  end;  { if }
end;
{--------}
procedure TXpStripManager.StripNodes(oDoc : TXpDocument);
begin
  if FStripping then
    StripNodesPrim(oDoc.DocumentElement);
end;
{--------}
procedure TXpStripManager.StripNodesPrim(oElem : TXpElement);
var
  oNode : TXpNode;
  wInx : Integer;
begin
  if ShouldStrip(oElem) then
    oElem.StripWhitespaceNodes(False);
  for wInx := 0 to Pred(oElem.ChildNodes.Length) do begin
    oNode := oElem.ChildNodes.Item(wInx);
    if oNode.NodeType = ELEMENT_NODE then
      StripNodesPrim(TXpElement(oNode));
  end;
end;
{=====================================================================}

{===TXpTemplateManager===============================================}
{$IFDEF UsingCLX}
constructor TXpTemplateManager.Create(oProcessor : TXpQBaseXSLProcessor);
{$ELSE}
constructor TXpTemplateManager.Create(oProcessor : TXpBaseXSLProcessor);
{$ENDIF}
var
  sTmp : string;
begin
  inherited Create;
  FTemplates := TXpStrHash.Create(xpc_Size59);
  FNamedTemplates := TXpStrHash.Create(xpc_Size59);
  FBuiltInModeTemplates := TXpStrHash.Create(xpc_Size59);

  FTemplates.OnDisposeData := OnDisposeHashEntry;
  FNamedTemplates.OnDisposeData := OnDisposeHashEntry;
  FBuiltInModeTemplates.OnDisposeData := OnDisposeDOM;

  FProcessor := oProcessor;

  { Load the built-in templates. }
  { Element }
  FBuiltInElementDOM := TXpObjModel.Create(nil);
  sTmp := XpsBuiltInElement;
  FBuiltInElementDOM.LoadMemory(sTmp[1], Length(sTmp));
  FBuiltInElementTemplate :=
    TXpXSLTemplate(FBuiltInElementDOM.Document.DocumentElement.ChildNodes.Item(0));

  { Text & Attrib }
  FBuiltInTextAttribDOM := TXpObjModel.Create(nil);
  sTmp := XpsBuiltInTextAttrib;
  FBuiltInTextAttribDOM.LoadMemory(sTmp[1], Length(sTmp));
  FBuiltInTextAttribTemplate :=
    TXpXSLTemplate(FBuiltInTextAttribDOM.Document.DocumentElement.ChildNodes.Item(0));

  { PI & Comment }
  FBuiltInPICommentDOM := TXpObjModel.Create(nil);
  sTmp := XpsBuiltInPIComment;
  FBuiltInPICommentDOM.LoadMemory(sTmp[1], Length(sTmp));
  FBuiltInPICommentTemplate :=
    TXpXSLTemplate(FBuiltInPICommentDOM.Document.DocumentElement.ChildNodes.Item(0));
end;
{--------}
destructor TXpTemplateManager.Destroy;
begin
  { Note: The templates referenced by FNamedTemplates and FTemplates are
    destroyed when their respective stylesheets are destroyed. The template
    manager is responsible only for freeing the hash tables. }
  FNamedTemplates.Free;
  FTemplates.Free;
  FBuiltInModeTemplates.Free;
  FBuiltInElementDOM.Free;
  FBuiltInTextAttribDOM.Free;
  FBuiltInPICommentDOM.Free;
  inherited Destroy;
end;
{--------}
procedure TXpTemplateManager.Add(oTemplate : TXpXSLTemplate);
begin
  { Does the template have a name attribute? }
  if oTemplate.Name <> '' then
    AddPrim(oTemplate, FNamedTemplates, oTemplate.Name);

  { Does the template have a match attribute? }
  if oTemplate.Match <> '' then
    AddPrim(oTemplate, FTemplates, oTemplate.Mode);

end;
{--------}
procedure TXpTemplateManager.AddPrim(oTemplate : TXpXSLTemplate;
                                     oHash : TXpStrHash;
                                     const sKey : DOMString);          {!!.57}
var
  oList : TXpPointerList;
  wInx : Longint;
begin
  { Do we have an entry for the template's mode? }
  oList := oHash.Get(sKey);
  if oList = nil then begin
    { No. Create an entry for this mode. }
    oList := TXpPointerList.Create;
    oList.Append(Pointer(oTemplate));
    oHash.Add(sKey, oList);
  end
  else begin
    { Yes. The templates are sorted by import precedence & priority. Add this
      template into the correct position within the list. }
    wInx := GetInsertionPoint(oTemplate, oList);
    oList.Insert(Pointer(oTemplate), wInx);
  end;
end;
{--------}
procedure TXpTemplateManager.Clear;
begin
  FNamedTemplates.Clear;
  FTemplates.Clear;
  FBuiltInModeTemplates.Clear;
end;
{--------}
function TXpTemplateManager.CompareTemplates(oLTemp, oRTemp : TXpXSLTemplate) : Integer;
begin
  if (oLTemp.Precedence = oRTemp.Precedence) then begin
    { Import precedence is equal. Test priority. }
    if (oLTemp.Priority = oRTemp.Priority) then
      Result := 0
    else if (oLTemp.Priority < oRTemp.Priority) then
      Result := -1
    else
      Result := 1;
  end
  else if (oLTemp.Precedence < oRTemp.Precedence) then
    Result := -1
  else
    Result := 1;
end;
{--------}
function TXpTemplateManager.GetBuiltInTemplate(oNode : TXpNode;
                                         const sMode : DOMString) : TXpXSLTemplate;
begin
  case oNode.NodeType of
    DOCUMENT_NODE, ELEMENT_NODE :
      if sMode = '' then
        Result := FBuiltInElementTemplate
      else begin
        Result := GetModeTemplate(sMode);
      end;
    TEXT_NODE, ATTRIBUTE_NODE :
      Result := FBuiltInTextAttribTemplate;
    COMMENT_NODE, PROCESSING_INSTRUCTION_NODE :
      Result := FBuiltInPICommentTemplate;
    else
      Result := nil;
  end;  { case }
end;
{--------}
function TXpTemplateManager.GetInsertionPoint(oTemplate : TXpXSLTemplate;
                                              oList : TXpPointerList) : Longint;
var
  L, R, M : Longint;
  oItem : TXpXSLTemplate;
  wResult : Integer;
begin
  if oList.Count = 0 then
    Result := 0
  else begin
    { The template needs to be positioned in the list based upon its import
      precedence & priority. }
    L := 0;
    R := Pred(oList.Count);
    repeat
      M := (L + R) div 2;
      oItem := TXpXSLTemplate(oList.Pointers[M]);
      wResult := CompareTemplates(oItem, oTemplate);
      if wResult = 0 then begin
        { Equivalent. Put the new template after the template already in the
          list. }
        L := Succ(M);
        break;
      end
      else if wResult < 0 then
        R := M - 1
      else
        L := M + 1;
    until L > R;
    Result := L;
  end;
end;
{--------}
function TXpTemplateManager.GetModeTemplate(const sMode : DOMString) : TXpXSLTemplate;
var
  oDOM : TXpObjModel;
  oStylesheet : TXpXSLStylesheet;
  sDoc : string;
begin
  oDOM := FBuiltInModeTemplates.Get(sMode);
  if oDOM = nil then begin
    sDoc := Format(XpsBuiltInMode, [sMode, sMode]);
    oDOM :=TXpObjModel.Create(nil);
    oDOM.LoadMemory(sDoc[1], Length(sDoc));
    FBuiltInModeTemplates.Add(sMode, Pointer(oDOM));
    oStylesheet := TXpXSLStylesheet(oDOM.Document.DocumentElement);
    oStylesheet.Mediator := FProcessor.Mediator;
    oStylesheet.NSMgr.Using :=
      TXpXSLStylesheet(FBuiltInElementDOM.Document.DocumentElement).NSMgr.Using;
  end;
  Result := TXpXSLTemplate(oDOM.Document.DocumentElement.ChildNodes.Item(0));
end;
{--------}
function TXpTemplateManager.Match(oNode : TXpElement;
                            const sMode : DOMString) : TXpXSLTemplate;
var
  bMatched, bRaiseError : Boolean;
  oConflictTemplate,
  oTemplate : TXpXSLTemplate;
  oList : TXpPointerList;
  oConflictList : TXpPointerList;
  sLocPath : DOMString;
  wCount : Longint;
  wInx : Integer;
begin
  Result := nil;
  oConflictList := nil;

  { Find a list of templates for the specified mode. }
  oList := FTemplates.Get(sMode);
  { Any found? }
  if oList <> nil then begin
    { Yes. Find the one with highest import precedence & import priority
      that matches. Watch out for situation where multiple templates
      match. }
    { For now do a simple sequential search from the beginning of the
      list until we encounter a match. }
    wInx := 0;
    wCount := oList.Count;
    repeat
      oTemplate := TXpXSLTemplate(oList.Pointers[wInx]);
      bMatched := oTemplate.Matches(oNode);
      { Found a match? }
      { Note: At this point in time, we do not treat union patterns as having
        a set of priorities. We just take the highest priority and go with it. }
      if bMatched then
        Result := oTemplate
      else
        inc(wInx);
    until (wInx = wCount) or bMatched;

    try
      { Did we find a matching template & it is not the last in the list? }
      if (Result <> nil) and (wInx < Pred(wCount)) then begin
        { We have a match. We need to see if any of the subsequent pattern(s)
          match this node & if they have equivalent import precedence & priority.
          If so then we need to resolve the situation. }
        inc(wInx);
        repeat
          oConflictTemplate := TXpXSLTemplate(oList.Pointers[wInx]);
          { Do they have the same import precedence & priority? }
          if CompareTemplates(oConflictTemplate, Result) < 0 then
            { No. Break out of this loop. }
            Break
          else begin
            { Yes. Does the subsequent template match the node? }
            if oConflictTemplate.Matches(oNode) then begin
              { Yes. Add it to the conflict list. }
              if oConflictList = nil then
                oConflictList := TXpPointerList.Create;
              oConflictList.Append(Pointer(oConflictTemplate));
            end;
          end;
          inc(wInx);
        until (wInx = wCount);

        { Do we have a conflict? }
        if oConflictList <> nil then begin
          { Yes. Ask the XSL processor how the situation should be resolved. }
          bRaiseError := False;
          if Assigned(FProcessor.OnResolveConflict) then
            FProcessor.OnResolveConflict(FProcessor, oNode, xpctTemplateMatch,
                                         bRaiseError);
          if bRaiseError then begin
            sLocPath := QuotedStr(oNode.LocationPath);
            if TXpNode(oNode) is TXpDocument then
              sLocPath := sLocPath + ' (i.e., document root)';
            raise EXpXSLException.Create
                    (Format(sXSLTemplateMatch,
                            [sLocPath,
                             QuotedStr(Result.Match),
                             Result.BaseURI,
                             QuotedStr(TXpXSLTemplate(oConflictList.Pointers[0]).Match),
                             TXpXSLTemplate(oConflictList.Pointers[0]).BaseURI
                            ]));
          end
          else begin
            { Return the last template in the stylesheet. }
            for wInx := 0 to Pred(oConflictList.Count) do begin
              oConflictTemplate := TXpXSLTemplate(oConflictList.Pointers[wInx]);
              if oConflictTemplate.IsAfter(Result) then
                Result := oConflictTemplate;
            end;
          end;
        end;  { if }
      end;  { if }
    finally
      oConflictList.Free;
    end;
  end;

  { Did we find a template? }
  if Result = nil then
    { No. Return the default template for this node type. }
    Result := GetBuiltInTemplate(oNode, sMode);

end;
{--------}
function TXpTemplateManager.MatchImport(oTemplate : TXpXSLTemplate) : TXpXSLTemplate;
var
  bCanCompare, bMatched : Boolean;
  wCount : Longint;
  wInx : Integer;
  oList : TXpPointerList;
  oListTemplate : TXpXSLTemplate;
begin
  Result := nil;
  bMatched := False;

  { Find the list of templates having the same mode as the specified template. }
  oList := FTemplates.Get(oTemplate.Mode);
  { Any found? }
  if oList <> nil then begin
    { Yes. Find the template with the next lowest import priority. Note that
      a template with the same name but declared in the same stylesheet
      will not be found by xsl:apply-imports. }
    wInx := 0;
    wCount := oList.Count;
    bCanCompare := False;
    repeat
      oListTemplate := TXpXSLTemplate(oList.Pointers[wInx]);
      { Have we reached the point where we can start checking for a template
        with same match but lower import precedence? }
      if bCanCompare then begin
        { Yes. }
        bMatched := (oListTemplate.Match = oTemplate.Match) and
                  (oListTemplate.Precedence < oTemplate.Precedence);
        if bMatched then
          Result := oListTemplate;
      end
      else
        { No. See if this is the specified template. }
        bCanCompare := (oTemplate = oListTemplate);
      inc(wInx);
    until (wInx = wCount) or bMatched;
  end;  { if }
end;
{--------}
function TXpTemplateManager.MatchName(const sName : DOMString) : TXpXSLTemplate;
var
  oList : TXpPointerList;
begin
  Result := nil;
  oList := FNamedTemplates.Get(sName);
  { Do we have a list of templates? }
  if (oList <> nil) and (oList.Count > 0) then
    { Yes. Return the first template in the list. This is a template matching
      the specified name and having the highest import precedence. }
    Result := TXpXSLTemplate(oList.Pointers[0]);
end;
{--------}
procedure TXpTemplateManager.OnDisposeDOM(Sender : TXpStrHash; aData : Pointer);
begin
  TXpObjModel(aData).Free;
end;
{--------}
procedure TXpTemplateManager.OnDisposeHashEntry(Sender : TXpStrHash; aData : Pointer);
begin
  TXpPointerList(aData).Free;
end;
{--------}
procedure TXpTemplateManager.SetMediator(oMediator : TXpElementMediator;
                                         bUsingNS : Boolean);
begin
  { Set the mediator for the built-in templates. }
  TXpXSLStylesheet(FBuiltInElementDOM.Document.DocumentElement).Mediator := oMediator;
  TXpXSLStylesheet(FBuiltInElementDOM.Document.DocumentElement).NSMgr.Using := bUsingNS;
  TXpXSLStylesheet(FBuiltInTextAttribDOM.Document.DocumentElement).Mediator := oMediator;
  TXpXSLStylesheet(FBuiltInTextAttribDOM.Document.DocumentElement).NSMgr.Using := bUsingNS;
  TXpXSLStylesheet(FBuiltInPICommentDOM.Document.DocumentElement).Mediator := oMediator;
  TXpXSLStylesheet(FBuiltInPICommentDOM.Document.DocumentElement).NSMgr.Using := bUsingNS;
end;
{====================================================================}

{====================================================================}
procedure InitializeUnit;
begin

  { Register the XSLT element factory. }
  TXpXSLElementFactory.Register(XpsXSLFactoryID);

  xpoElementHash := TXpStrHash.Create(xpc_Size59);

  { Add the elements to the hash table. }
  with xpoElementHash do begin
    Add(XpsXSLApplyImports, Pointer(TXpXSLApplyImports));
    Add(XpsXSLApplyTemplates, Pointer(TXpXSLApplyTemplates));
    Add(XpsXSLAttribute, Pointer(TXpXSLAttribute));
    Add(XpsXSLAttributeSet, Pointer(TXpXSLAttributeSet));
    Add(XpsXSLCallTemplate, Pointer(TXpXSLCallTemplate));
    Add(XpsXSLChoose, Pointer(TXpXSLChoose));
    Add(XpsXSLComment, Pointer(TXpXSLComment));
    Add(XpsXSLCopy, Pointer(TXpXSLCopy));
    Add(XpsXSLCopyOf, Pointer(TXpXSLCopyOf));
    Add(XpsXSLDecimalFormat, Pointer(TXpXSLDecimalFormat));
    Add(XpsXSLElement, Pointer(TXpXSLElement));
    Add(XpsXSLFallback, Pointer(TXpXSLFallback));
    Add(XpsXSLForEach, Pointer(TXpXSLForEach));
    Add(XpsXSLIf, Pointer(TXpXSLIf));
    Add(XpsXSLImport, Pointer(TXpXSLImport));
    Add(XpsXSLInclude, Pointer(TXpXSLInclude));
    Add(XpsXSLKey, Pointer(TXpXSLKey));
    Add(XpsXSLMessage, Pointer(TXpXSLMessage));
    Add(XpsXSLNamespaceAlias, Pointer(TXpXSLNamespaceAlias));
    Add(XpsXSLNumber, Pointer(TXpXSLNumber));
    Add(XpsXSLOtherwise, Pointer(TXpXSLOtherwise));
    Add(XpsXSLOutput, Pointer(TXpXSLOutput));
    Add(XpsXSLParam, Pointer(TXpXSLParam));
    Add(XpsXSLPreserveSpace, Pointer(TXpXSLPreserveSpace));
    Add(XpsXSLProcessingInstruction, Pointer(TXpXSLProcessingInstruction));
    Add(XpsXSLSort, Pointer(TXpXSLSort));
    Add(XpsXSLStripSpace, Pointer(TXpXSLStripSpace));
    Add(XpsXSLStylesheet, Pointer(TXpXSLStylesheet));
    Add(XpsXSLTemplate, Pointer(TXpXSLTemplate));
    Add(XpsXSLText, Pointer(TXpXSLText));
    Add(XpsXSLTransform, Pointer(TXpXSLTransform));
    Add(XpsXSLValueOf, Pointer(TXpXSLValueOf));
    Add(XpsXSLVariable, Pointer(TXpXSLVariable));
    Add(XpsXSLWhen, Pointer(TXpXSLWhen));
    Add(XpsXSLWithParam, Pointer(TXpXSLWithParam));
  end;  { with  }
end;
{--------}
procedure FinalizeUnit;
begin
  TXpXSLElementFactory.Unregister;
  xpoElementHash.Free;
end;
{====================================================================}
initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
