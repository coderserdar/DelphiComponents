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
 * Thorsten Engler
 *
 * Portions created by the Initial Developer are Copyright (C) 2001
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*******************************************************}
{ Turbopower XML Partner DOM Implementation Wrapper     }
{*******************************************************}

unit XpXmlDom;

interface

uses Variants, ComObj, XpDOM, xmldom, Classes;

const

  SXpXML                      = 'XpXML';                    { Do not localize }

type

  { IXpNodeRef }

  IXpNodeRef = interface
    ['{A8532FFF-90A5-4807-BAAC-4039F4D9FE2C}']
    function GetRealXpNode: TXpNode;
  end;

  { TXpDOMInterface }

  TXpDOMInterface = class(TInterfacedObject)
  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;
  end;

  { TXpDOMImplementation }

  TXpDOMImplementation = class(TXpDOMInterface, IDOMImplementation)
  private
    FXpDOMImpl: XpDOM.TXpDomImplementation;
    FMustFree: Boolean;
  protected
    { IDOMImplementation }
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;
      doctype: IDOMDocumentType): IDOMDocument; safecall;
  public
    constructor Create(DOMImpl: XpDOM.TXpDomImplementation; aMustFree: Boolean);
    destructor Destroy;
    property XpDOMImpl: XpDOM.TXpDomImplementation read FXpDOMImpl;
  end;

  { TXpDOMNode }

  TXpDOMNodeClass = class of TXpDOMNode;

  TXpDOMNode = class(TXpDOMInterface, IXpNodeRef, IDOMNode, IDOMNodeEx)
  private
    FXpNode: TXpNode;
    FChildNodes: IDOMNodeList;
    FAttributes: IDOMNamedNodeMap;
    FOwnerDocument: IDOMDocument;
  protected
    { IXpNodeRef }
    function GetRealXpNode: TXpNode;
    { IDOMNode }
    function get_nodeName: DOMString; safecall;
    function get_nodeValue: DOMString; safecall;
    procedure set_nodeValue(value: DOMString);
    function get_nodeType: DOMNodeType; safecall;
    function get_parentNode: IDOMNode; safecall;
    function get_childNodes: IDOMNodeList; safecall;
    function get_firstChild: IDOMNode; safecall;
    function get_lastChild: IDOMNode; safecall;
    function get_previousSibling: IDOMNode; safecall;
    function get_nextSibling: IDOMNode; safecall;
    function get_attributes: IDOMNamedNodeMap; safecall;
    function get_ownerDocument: IDOMDocument; safecall;
    function get_namespaceURI: DOMString; safecall;
    function get_prefix: DOMString; safecall;
    function get_localName: DOMString; safecall;
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    procedure normalize;
    function supports(const feature, version: DOMString): WordBool;
    { IDOMNodeEx }
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure transformNode(const stylesheet: IDOMNode; var output: WideString); overload;
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); overload;
    procedure set_text(const Value: DOMString); safecall;
  public
    constructor Create(aNode: TXpNode); virtual;
    destructor Destroy; override;
    property XpNode: TXpNode read FXpNode;
  end;

  { TXpDOMNodeList }

  TXpDOMNodeList = class(TXpDOMInterface, IDOMNodeList)
  private
    FXpNodeList: TXpNodeList;
    FMustFree: Boolean;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(aNodeList: TXpNodeList; aMustFree: Boolean);
    destructor Destroy; override;
    property XpNodeList: TXpNodeList read FXpNodeList;
  end;

  { TXpDOMNamedNodeMap }

  TXpDOMNamedNodeMap = class(TXpDOMInterface, IDOMNamedNodeMap)
  private
    FXpNamedNodeMap: TXpNamedNodeMap;
    FMustFree: Boolean;
  protected
    { IDOMNamedNodeMap }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer;
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function setNamedItem(const newItem: IDOMNode): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
  public
    constructor Create(ANamedNodeMap: TXpNamedNodeMap; aMustFree: Boolean);
    destructor Destroy; override;
    property XpNamedNodeMap: TXpNamedNodeMap read FXpNamedNodeMap;
  end;

  { TXpDOMCharacterData }

  TXpDOMCharacterData = class(TXpDOMNode, IDOMCharacterData)
  private
    function GetXpCharacterData: TXpCharacterData;
  protected
    { IDOMCharacterData }
    function get_data: DOMString;
    procedure set_data(const data: DOMString);
    function get_length: Integer;
    function substringData(offset, count: Integer): DOMString;
    procedure appendData(const data: DOMString);
    procedure insertData(offset: Integer; const data: DOMString);
    procedure deleteData(offset, count: Integer);
    procedure replaceData(offset, count: Integer; const data: DOMString);
  public
    property XpCharacterData: TXpCharacterData read GetXpCharacterData;
  end;

  { TXpDOMAttr }

  TXpDOMAttr = class(TXpDOMNode, IDOMAttr)
  private
    function GetXpAttribute: TXpAttribute;
  protected
    { Property Get/Set }
    function get_name: DOMString;
    function get_specified: WordBool;
    function get_value: DOMString;
    procedure set_value(const attributeValue: DOMString);
    function get_ownerElement: IDOMElement;
    { Properties }
    property name: DOMString read get_name;
    property specified: WordBool read get_specified;
    property value: DOMString read get_value write set_value;
    property ownerElement: IDOMElement read get_ownerElement;
  public
    property XpAttribute: TXpAttribute read GetXpAttribute;
  end;

  { TXpDOMElement }

  TXpDOMElement = class(TXpDOMNode, IDOMElement, IDOMNodeSelect)
  private
    function GetXpElement: TXpElement;
  protected
    { IDOMElement }
    function get_tagName: DOMString; safecall;
    function getAttribute(const name: DOMString): DOMString; safecall;
    procedure setAttribute(const name, value: DOMString);
    procedure removeAttribute(const name: DOMString);
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;
    procedure setAttributeNS(const namespaceURI, qualifiedName, value: DOMString);
    procedure removeAttributeNS(const namespaceURI, localName: DOMString);
    function getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall;
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function hasAttribute(const name: DOMString): WordBool; safecall;
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool;
    procedure normalize;
    { IDOMNodeSelect }
    function selectNode(const nodePath: WideString): IDOMNode; safecall;
    function selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
  public
    property XpElement: TXpElement read GetXpElement;
  end;

  { TXpDOMText }

  TXpDOMText = class(TXpDOMCharacterData, IDOMText)
  protected
    function splitText(offset: Integer): IDOMText; safecall;
  end;

  { TXpDOMComment }

  TXpDOMComment = class(TXpDOMCharacterData, IDOMComment)
  end;

  { TXpDOMCDATASection }

  TXpDOMCDATASection = class(TXpDOMText, IDOMCDATASection)
  end;

  { TXpDOMDocumentType }

  TXpDOMDocumentType = class(TXpDOMNode, IDOMDocumentType)
  private
    //FEntities: IDOMNamedNodeMap;
    //FNotations: IDOMNamedNodeMap;
    function GetXpDocumentType: TXpDocumentType;
  protected
    { IDOMDocumentType }
    function get_name: DOMString; safecall;
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_internalSubset: DOMString; safecall;
  public
    property XpDocumentType: TXpDocumentType read GetXpDocumentType;
  end;

  { TXpDOMNotation }

  TXpDOMNotation = class(TXpDOMNode, IDOMNotation)
  private
    function GetXpNotation: TXpDTDNotation;
  protected
    { IDOMNotation }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  public
    property XpNotation: TXpDTDNotation read GetXpNotation;
  end;

  { TXpDOMEntity }

  TXpDOMEntity = class(TXpDOMNode, IDOMEntity)
  private
    function GetXpEntity: TXpDTDEntity;
  protected
    { IDOMEntity }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_notationName: DOMString; safecall;
  public
    property XpEntity: TXpDTDEntity read GetXpEntity;
  end;

  { TXpDOMEntityReference }

  TXpDOMEntityReference = class(TXpDOMNode, IDOMEntityReference)
  end;

  { TXpDOMProcessingInstruction }

  TXpDOMProcessingInstruction = class(TXpDOMNode, IDOMProcessingInstruction)
  private
    function GetXpProcessingInstruction: TXpProcessingInstruction;
  protected
    { IDOMProcessingInstruction }
    function get_target: DOMString; safecall;
    function get_data: DOMString; safecall;
    procedure set_data(const value: DOMString);
  public
    property XpProcessingInstruction: TXpProcessingInstruction read GetXpProcessingInstruction;
  end;

  { TXpDOMDocumentFragment }

  TXpDOMDocumentFragment = class(TXpDOMNode, IDOMDocumentFragment)
  end;

  TXpDOMDocument = class;

  { TXpDOMDocument }

  TXpDOMDocument = class(TXpDOMNode, IDOMDocument, IDOMParseOptions, IDOMPersist,
      IDOMParseError)
  private
    FXpObjModel: TXpObjModel;
    function GetXpDocument: TXpDocument;
  protected
    { IDOMDocument }
    function get_doctype: IDOMDocumentType; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function get_documentElement: IDOMElement; safecall;
    procedure set_documentElement(const IDOMElement: IDOMElement);
    function createElement(const tagName: DOMString): IDOMElement; safecall;
    function createDocumentFragment: IDOMDocumentFragment; safecall;
    function createTextNode(const data: DOMString): IDOMText; safecall;
    function createComment(const data: DOMString): IDOMComment; safecall;
    function createCDATASection(const data: DOMString): IDOMCDATASection; safecall;
    function createProcessingInstruction(const target,
      data: DOMString): IDOMProcessingInstruction; safecall;
    function createAttribute(const name: DOMString): IDOMAttr; safecall;
    function createEntityReference(const name: DOMString): IDOMEntityReference; safecall;
    function getElementsByTagName(const tagName: DOMString): IDOMNodeList; safecall;
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall;
    function createElementNS(const namespaceURI,
      qualifiedName: DOMString): IDOMElement; safecall;
    function createAttributeNS(const namespaceURI,
      qualifiedName: DOMString): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function getElementById(const elementId: DOMString): IDOMElement;
    { IDOMParseOptions }
    function get_async: Boolean;
    function get_preserveWhiteSpace: Boolean;
    function get_resolveExternals: Boolean;
    function get_validate: Boolean;
    procedure set_async(Value: Boolean);
    procedure set_preserveWhiteSpace(Value: Boolean);
    procedure set_resolveExternals(Value: Boolean);
    procedure set_validate(Value: Boolean);
    { IDOMPersist }
    function get_xml: DOMString; safecall;
    function asyncLoadState: Integer; safecall;
    function load(source: OleVariant): WordBool; safecall;
    function loadFromStream(const stream: TStream): WordBool; safecall;
    function loadxml(const Value: DOMString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    procedure saveToStream(const stream: TStream); safecall;
    procedure set_OnAsyncLoad(const Sender: TObject;
      EventHandler: TAsyncEventHandler); safecall;
    { IDOMParseError }
    function get_errorCode: Integer;
    function get_url: WideString; safecall;
    function get_reason: WideString; safecall;
    function get_srcText: WideString; safecall;
    function get_line: Integer;
    function get_linepos: Integer;
    function get_filepos: Integer;
  public
    constructor Create(aNode: TXpNode); override;
    destructor Destroy; override;
    property XpDocument: TXpDocument read GetXpDocument;
  end;

  { TXpDOMImplementationFactory }

  TXpDOMImplementationFactory = class(TDOMVendor)
  public
    function DOMImplementation: IDOMImplementation; override;
    function Description: string; override;
  end;

var
  XpXML_DOM                   : TXpDOMImplementationFactory;

implementation

resourcestring
  SNodeExpected               = 'Node cannot be null';

  { Utility Functions }

function MakeNode(Node: TXpNode): IDOMNode;
const
  NodeClasses                 : array[ELEMENT_NODE..NOTATION_NODE] of TXpDOMNodeClass =
    (TXpDOMElement, TXpDOMAttr, TXpDOMText, TXpDOMCDataSection,
    TXpDOMEntityReference, TXpDOMEntity, TXpDOMProcessingInstruction,
    TXpDOMComment, TXpDOMDocument, TXpDOMDocumentType, TXpDOMDocumentFragment,
    TXpDOMNotation);
begin
  if Node <> nil then
    Result := NodeClasses[Node.nodeType].Create(Node) else
    Result := nil;
end;

function MakeNodeList(const NodeList: TXpNodeList; aMustFree: Boolean): IDOMNodeList;
begin
  Result := TXpDOMNodeList.Create(NodeList, aMustFree);
end;

function MakeNamedNodeMap(const NamedNodeMap: TXpNamedNodeMap; aMustFree: Boolean): IDOMNamedNodeMap;
begin
  Result := TXpDOMNamedNodeMap.Create(NamedNodeMap, aMustFree);
end;

function GetXpNode(const Node: IDOMNode): TXpNode;
begin
  if not Assigned(Node) then
    raise DOMException.Create(SNodeExpected);
  Result := (Node as IXpNodeRef).GetRealXpNode;
end;

{ TXpDOMInterface }

function TXpDOMInterface.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
var
  HelpFile                    : string;
begin
  if ExceptObject is EOleException then
    HelpFile := (ExceptObject as EOleException).HelpFile;
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IDOMNode, '', Helpfile);
end;

{ TXpDOMImplementation }

constructor TXpDOMImplementation.Create(DOMImpl: XpDOM.TXpDomImplementation; aMustFree: Boolean);
begin
  inherited Create;
  FXpDOMImpl := DOMImpl;
  FMustFree := aMustFree;
end;

destructor TXpDOMImplementation.Destroy;
begin
  inherited;
  if FMustFree then
    FXpDOMImpl.Free;
end;

function TXpDOMImplementation.createDocument(const namespaceURI,
  qualifiedName: DOMString; doctype: IDOMDocumentType): IDOMDocument;
var
  DocumentType                : TXpDocumentType;
begin
  if Assigned(doctype) then
    DocumentType := GetXpNode(doctype) as TXpDocumentType
  else
    DocumentType := nil;
  Result := TXpDOMDocument.Create(XpDOMImpl.CreateDocument(namespaceURI,
    qualifiedName, DocumentType));
end;

function TXpDOMImplementation.createDocumentType(const qualifiedName,
  publicId, systemId: DOMString): IDOMDocumentType;
begin
  Result := TXpDOMDocumentType.Create(XpDOMImpl.CreateDocumentType(qualifiedName,
    publicId, systemId));
end;

function TXpDOMImplementation.hasFeature(const feature,
  version: DOMString): WordBool;
begin
  Result := XpDOMImpl.hasFeature(feature, version);
end;

{ TXpDOMNode }

constructor TXpDOMNode.Create(aNode: TXpNode);
begin
  Assert(Assigned(aNode));
  FXpNode := aNode;
  FXpNode.AddRef;
  inherited Create;
end;

destructor TXpDOMNode.Destroy;
begin
  FXpNode.Release;
  inherited;
end;

function TXpDOMNode.appendChild(const newChild: IDOMNode): IDOMNode;
begin
  XpNode.appendChild(GetXpNode(newChild));
  Result := newChild;
end;

function TXpDOMNode.cloneNode(deep: WordBool): IDOMNode;
begin
  Result := MakeNode(XpNode.cloneNode(deep));
end;

function TXpDOMNode.get_attributes: IDOMNamedNodeMap;
begin
  if not Assigned(FAttributes) and Assigned(XpNode.attributes) then
    FAttributes := MakeNamedNodeMap(XpNode.attributes, false);
  Result := FAttributes;
end;

function TXpDOMNode.get_childNodes: IDOMNodeList;
begin
  if not Assigned(FChildNodes) then
    FChildNodes := MakeNodeList(XpNode.childNodes, false);
  Result := FChildNodes;
end;

function TXpDOMNode.get_firstChild: IDOMNode;
begin
  Result := MakeNode(XpNode.firstChild);
end;

function TXpDOMNode.get_lastChild: IDOMNode;
begin
  Result := MakeNode(XpNode.lastChild);
end;

function TXpDOMNode.get_localName: DOMString;
begin
  Result := XpNode.localName;
end;

function TXpDOMNode.get_namespaceURI: DOMString;
begin
  Result := XpNode.namespaceURI;
end;

function TXpDOMNode.get_nextSibling: IDOMNode;
begin
  Result := MakeNode(XpNode.nextSibling);
end;

function TXpDOMNode.get_nodeName: DOMString;
begin
  Result := XpNode.nodeName;
end;

function TXpDOMNode.get_nodeType: DOMNodeType;
begin
  Result := XpNode.nodeType;
end;

function TXpDOMNode.get_nodeValue: DOMString;
var
  NodeValue                   : OleVariant;
begin
  NodeValue := XpNode.nodeValue;
  if not VarIsNull(NodeValue) then
    Result := NodeValue
  else
    Result := '';
end;

function TXpDOMNode.get_ownerDocument: IDOMDocument;
begin
  if not Assigned(FOwnerDocument) then
    FOwnerDocument := TXpDOMDocument.Create(XpNode.ownerDocument);
  Result := FOwnerDocument;
end;

function TXpDOMNode.get_parentNode: IDOMNode;
begin
  Result := MakeNode(XpNode.parentNode);
end;

function TXpDOMNode.get_prefix: DOMString;
begin
  Result := XpNode.prefix;
end;

function TXpDOMNode.get_previousSibling: IDOMNode;
begin
  Result := MakeNode(XpNode.previousSibling);
end;

function TXpDOMNode.hasChildNodes: WordBool;
begin
  Result := XpNode.hasChildNodes;
end;

function TXpDOMNode.insertBefore(const newChild,
  refChild: IDOMNode): IDOMNode;
begin
  XpNode.insertBefore(GetXpNode(newChild), GetXpNode(refChild));
  Result := newChild;
end;

procedure TXpDOMNode.normalize;
begin
  DOMVendorNotSupported('normalize', SXpXML);               { Do not localize }
end;

function TXpDOMNode.removeChild(const childNode: IDOMNode): IDOMNode;
var
  Node                        : TXpNode;
begin
  Node := XpNode.removeChild(GetXpNode(childNode));
  try
    Result := MakeNode(Node);
  finally
    Node.Release;
  end;
end;

function TXpDOMNode.replaceChild(const newChild,
  oldChild: IDOMNode): IDOMNode;
var
  Node                        : TXpNode;
begin
  Node := XpNode.replaceChild(GetXpNode(newChild), GetXpNode(oldChild));
  try
    Result := MakeNode(Node);
  finally
    Node.Release;
  end;
end;

procedure TXpDOMNode.set_nodeValue(value: DOMString);
begin
  XpNode.nodeValue := value;
end;

function TXpDOMNode.supports(const feature, version: DOMString): WordBool;
begin
  DOMVendorNotSupported('supports', SXpXML);                { Do not localize }
  Result := False;
end;

function TXpDOMNode.GetRealXpNode: TXpNode;
begin
  Result := XpNode;
end;

{ IDOMNodeEx Interface }

function TXpDOMNode.get_text: DOMString;
begin
  Result := XpNode.text;
end;

procedure TXpDOMNode.set_text(const Value: DOMString);
begin
  DOMVendorNotSupported('set_text', SXpXML);                { Do not localize }
end;

function TXpDOMNode.get_xml: DOMString;
begin
  DOMVendorNotSupported('get_xml', SXpXML);                 { Do not localize }
  //Result := XpNode.xml;
end;

procedure TXpDOMNode.transformNode(const stylesheet: IDOMNode; var output: WideString);
begin
  DOMVendorNotSupported('transformNode', SXpXML);           { Do not localize }
  //output := XpNode.transformNode(GetXpNode(stylesheet));
end;

procedure TXpDOMNode.transformNode(const stylesheet: IDOMNode; const output: IDOMDocument);
begin
  DOMVendorNotSupported('transformNode', SXpXML);           { Do not localize }
  //XpNode.transformNodeToObject(GetXpNode(stylesheet), GetXpNode(output));
end;

{ TXpDOMNodeList }

constructor TXpDOMNodeList.Create(aNodeList: TXpNodeList; aMustFree: Boolean);
begin
  inherited Create;
  FXpNodeList := aNodeList;
  FMustFree := aMustFree;
end;

destructor TXpDOMNodeList.Destroy;
begin
  inherited;
  if FMustFree then
    FXpNodeList.Free;
end;

function TXpDOMNodeList.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(XpNodeList.Item(index));
end;

function TXpDOMNodeList.get_length: Integer;
begin
  Result := XpNodeList.Length;
end;

{ TXpDOMNamedNodeMap }

constructor TXpDOMNamedNodeMap.Create(ANamedNodeMap: TXpNamedNodeMap; aMustFree: Boolean);
begin
  inherited Create;
  FXpNamedNodeMap := ANamedNodeMap;
  FMustFree := aMustFree;
end;

destructor TXpDOMNamedNodeMap.Destroy;
begin
  inherited;
  if FMustFree then
    FXpNamedNodeMap.Free;
end;

function TXpDOMNamedNodeMap.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(XpNamedNodeMap.item(index));
end;

function TXpDOMNamedNodeMap.get_length: Integer;
begin
  Result := XpNamedNodeMap.length;
end;

function TXpDOMNamedNodeMap.getNamedItem(const name: DOMString): IDOMNode;
begin
  Result := MakeNode(XpNamedNodeMap.getNamedItem(name));
end;

function TXpDOMNamedNodeMap.getNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  Result := MakeNode(XpNamedNodeMap.GetNamedItemNS(namespaceURI, localName));
end;

function TXpDOMNamedNodeMap.removeNamedItem(const name: DOMString): IDOMNode;
var
  Node                        : TXpNode;
begin
  Node := XpNamedNodeMap.removeNamedItem(name);
  try
    Result := MakeNode(Node);
  finally
    Node.Release;
  end;
end;

function TXpDOMNamedNodeMap.removeNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
var
  Node                        : TXpNode;
begin
  Node := XpNamedNodeMap.RemoveNamedItemNS(namespaceURI, localName);
  try
    Result := MakeNode(Node);
  finally
    Node.Release;
  end;
end;

function TXpDOMNamedNodeMap.setNamedItem(const newItem: IDOMNode): IDOMNode;
begin
  Result := MakeNode(XpNamedNodeMap.setNamedItem(GetXpNode(newItem)));
end;

function TXpDOMNamedNodeMap.setNamedItemNS(const arg: IDOMNode): IDOMNode;
begin
  DOMVendorNotSupported('setNamedItemNS', SXpXML);          { Do not localize }
end;

{ TXpDOMCharacterData }

function TXpDOMCharacterData.GetXpCharacterData: TXpCharacterData;
begin
  Result := XpNode as TXpCharacterData;
end;

procedure TXpDOMCharacterData.appendData(const data: DOMString);
begin
  XpCharacterData.appendData(data);
end;

procedure TXpDOMCharacterData.deleteData(offset, count: Integer);
begin
  XpCharacterData.deleteData(offset, count);
end;

function TXpDOMCharacterData.get_data: DOMString;
begin
  Result := XpCharacterData.data;
end;

function TXpDOMCharacterData.get_length: Integer;
begin
  Result := XpCharacterData.length;
end;

procedure TXpDOMCharacterData.insertData(offset: Integer;
  const data: DOMString);
begin
  XpCharacterData.insertData(offset, data);
end;

procedure TXpDOMCharacterData.replaceData(offset, count: Integer;
  const data: DOMString);
begin
  XpCharacterData.replaceData(offset, count, data);
end;

procedure TXpDOMCharacterData.set_data(const data: DOMString);
begin
  XpCharacterData.data := data;
end;

function TXpDOMCharacterData.substringData(offset,
  count: Integer): DOMString;
begin
  Result := XpCharacterData.substringData(offset, count);
end;

{ TXpDOMAttr }

function TXpDOMAttr.GetXpAttribute: TXpAttribute;
begin
  Result := XpNode as TXpAttribute;
end;

function TXpDOMAttr.get_name: DOMString;
begin
  Result := XpAttribute.name;
end;

function TXpDOMAttr.get_ownerElement: IDOMElement;
begin
  DOMVendorNotSupported('get_ownerElement', SXpXML);        { Do not localize }
  Result := nil;
end;

function TXpDOMAttr.get_specified: WordBool;
begin
  Result := XpAttribute.specified;
end;

function TXpDOMAttr.get_value: DOMString;
begin
  Result := XpAttribute.value;
end;

procedure TXpDOMAttr.set_value(const attributeValue: DOMString);
begin
  XpAttribute.value := attributeValue;
end;

{ TXpDOMElement }

function TXpDOMElement.GetXpElement: TXpElement;
begin
  Result := XpNode as TXpElement;
end;

function TXpDOMElement.get_tagName: DOMString;
begin
  Result := XpElement.tagName;
end;

function TXpDOMElement.getAttribute(const name: DOMString): DOMString;
begin
  Result := VarToWideStr(XpElement.getAttribute(name));
end;

function TXpDOMElement.getAttributeNS(const namespaceURI,
  localName: DOMString): DOMString;
begin
  Result := VarToWideStr(getAttributeNodeNS(namespaceURI, localName).NodeValue);
end;

function TXpDOMElement.getAttributeNode(const name: DOMString): IDOMAttr;
begin
  Result := MakeNode(XpElement.getAttributeNode(name)) as IDOMAttr;
end;

function TXpDOMElement.getAttributeNodeNS(const namespaceURI,
  localName: DOMString): IDOMAttr;
begin
  Result := MakeNode(XpElement.getAttributeNodeNS(namespaceURI, localName)) as IDOMAttr;
end;

function TXpDOMElement.getElementsByTagName(const name: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(XpElement.getElementsByTagName(name), true);
end;

function TXpDOMElement.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
begin
  DOMVendorNotSupported('getElementsByTagNameNS', SXpXML);  { Do not localize }
end;

function TXpDOMElement.hasAttribute(const name: DOMString): WordBool;
begin
  Result := XpElement.hasAttribute(name);
end;

function TXpDOMElement.hasAttributeNS(const namespaceURI,
  localName: DOMString): WordBool;
begin
  { Note: use Xp DOM function when/if it is added. }
  try
    Result := getAttributeNodeNS(namespaceURI, localName) <> nil;
  except
    Result := False;
  end;
end;

procedure TXpDOMElement.removeAttribute(const name: DOMString);
begin
  XpElement.removeAttribute(name);
end;

function TXpDOMElement.removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr;
begin
  Result := MakeNode(XpElement.removeAttributeNode(
    GetXpNode(oldAttr) as TXpAttribute)) as IDOMAttr;
end;

procedure TXpDOMElement.removeAttributeNS(const namespaceURI,
  localName: DOMString);
begin
  XpElement.RemoveAttributeNS(namespaceURI, localName);
end;

procedure TXpDOMElement.setAttribute(const name, value: DOMString);
begin
  XpElement.setAttribute(name, value);
end;

function TXpDOMElement.setAttributeNode(const newAttr: IDOMAttr): IDOMAttr;
begin
  Result := MakeNode(XpElement.setAttributeNode(
    GetXpNode(newAttr) as TXpAttribute)) as IDOMAttr;
end;

function TXpDOMElement.setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr;
begin
  Result := setAttributeNode(newAttr);
end;

procedure TXpDOMElement.setAttributeNS(const namespaceURI, qualifiedName,
  value: DOMString);
var
  AttrNode                    : TXpAttribute;
begin
  AttrNode := XpNode.ownerDocument.CreateAttributeNS(namespaceURI,
    qualifiedName);
  AttrNode.nodeValue := value;
  XpElement.setAttributeNode(AttrNode);
end;

procedure TXpDOMElement.normalize;
begin
  XpElement.normalize;
end;

function TXpDOMElement.selectNode(const nodePath: WideString): IDOMNode;
var
  Node                        : TXpNode;
begin
  Node := XpElement.selectSingleNode(nodePath);
  if Assigned(Node) then
    Result := MakeNode(Node) else
    Result := nil;
end;

function TXpDOMElement.selectNodes(const nodePath: WideString): IDOMNodeList;
var
  NodeList                    : TXpNodeList;
begin
  NodeList := XpElement.selectNodes(nodePath);
  if Assigned(NodeList) then
    Result := MakeNodeList(NodeList, true) else
    Result := nil;
end;

{ TXpDOMText }

function TXpDOMText.splitText(offset: Integer): IDOMText;
var
  Node                        : TXpNode;
begin
  Node := (XpNode as TXpText).splitText(offset);
  try
    Result := MakeNode(Node) as IDOMText;
  finally
    Node.Release;
  end;
end;

{ TXpDOMDocumentType }

function TXpDOMDocumentType.GetXpDocumentType: TXpDocumentType;
begin
  Result := XpNode as TXpDocumentType;
end;

function TXpDOMDocumentType.get_entities: IDOMNamedNodeMap;
begin
  DOMVendorNotSupported('get_entities', SXpXML);            { Do not localize }
  (*
  if not Assigned(FEntities) then
    FEntities := MakeNamedNodeMap(XpDocumentType.get_entities);
  Result := FEntities;
  *)
end;

function TXpDOMDocumentType.get_internalSubset: DOMString;
begin
  DOMVendorNotSupported('get_internalSubset', SXpXML);      { Do not localize }
end;

function TXpDOMDocumentType.get_name: DOMString;
begin
  Result := XpDocumentType.name;
end;

function TXpDOMDocumentType.get_notations: IDOMNamedNodeMap;
begin
  DOMVendorNotSupported('get_notations', SXpXML);           { Do not localize }
  (*
  if not Assigned(FNotations) then
    FNotations := MakeNamedNodeMap(XpDocumentType.notations);
  Result := FNotations;
  *)
end;

function TXpDOMDocumentType.get_publicId: DOMString;
begin
  Result := XpDocumentType.PublicID;
end;

function TXpDOMDocumentType.get_systemId: DOMString;
begin
  Result := XpDocumentType.SystemID;
end;

{ TXpDOMNotation }

function TXpDOMNotation.GetXpNotation: TXpDTDNotation;
begin
  Result := XpNode as TXpDTDNotation;
end;

function TXpDOMNotation.get_publicId: DOMString;
begin
  Result := XpNotation.publicId;
end;

function TXpDOMNotation.get_systemId: DOMString;
begin
  Result := XpNotation.systemId;
end;

{ TXpDOMEntity }

function TXpDOMEntity.GetXpEntity: TXpDTDEntity;
begin
  Result := XpNode as TXpDTDEntity;
end;

function TXpDOMEntity.get_notationName: DOMString;
begin
  Result := XpEntity.notationName;
end;

function TXpDOMEntity.get_publicId: DOMString;
begin
  Result := XpEntity.publicId;
end;

function TXpDOMEntity.get_systemId: DOMString;
begin
  Result := XpEntity.systemId;
end;

{ TXpDOMProcessingInstruction }

function TXpDOMProcessingInstruction.GetXpProcessingInstruction: TXpProcessingInstruction;
begin
  Result := XpNode as TXpProcessingInstruction;
end;

function TXpDOMProcessingInstruction.get_data: DOMString;
begin
  Result := XpProcessingInstruction.data;
end;

function TXpDOMProcessingInstruction.get_target: DOMString;
begin
  Result := XpProcessingInstruction.target;
end;

procedure TXpDOMProcessingInstruction.set_data(const value: DOMString);
begin
  XpProcessingInstruction.data := value;
end;

{ TXpDOMDocument }

constructor TXpDOMDocument.Create(aNode: TXpNode);
begin
  inherited;
  FXpObjModel := TXpObjModel.Create(nil);
  FXpObjModel.Document := XpDocument;
end;

destructor TXpDOMDocument.Destroy;
begin
  XpDocument.AddRef;
  inherited;
  FXpObjModel.Free;
end;

function TXpDOMDocument.GetXpDocument: TXpDocument;
begin
  Result := XpNode as TXpDocument;
end;

function TXpDOMDocument.createAttribute(const name: DOMString): IDOMAttr;
var
  XpAttribute                 : TXpAttribute;
begin
  XpAttribute := XpDocument.createAttribute(name);
  try
    Result := MakeNode(XpAttribute) as IDOMAttr;
  finally
    XpAttribute.Release;
  end;
end;

function TXpDOMDocument.createAttributeNS(const namespaceURI,
  qualifiedName: DOMString): IDOMAttr;
var
  XpAttribute                 : TXpAttribute;
begin
  XpAttribute := XpDocument.CreateAttributeNS(namespaceURI, qualifiedName);
  try
    Result := MakeNode(XpAttribute) as IDOMAttr;
  finally
    XpAttribute.Release;
  end;
end;

function TXpDOMDocument.createCDATASection(const data: DOMString): IDOMCDATASection;
var
  XpCDATASection              : TXpCDATASection;
begin
  XpCDATASection := XpDocument.createCDATASection(data);
  try
    Result := IDOMCDATASection(TXpDOMCDATASection.Create(XpCDATASection));
  finally
    XpCDATASection.Release;
  end;
end;

function TXpDOMDocument.createComment(const data: DOMString): IDOMComment;
var
  XpComment                   : TXpComment;
begin
  XpComment := XpDocument.createComment(data);
  try
    Result := IDOMComment(TXpDOMComment.Create(XpComment));
  finally
    XpComment.Release;
  end;
end;

function TXpDOMDocument.createDocumentFragment: IDOMDocumentFragment;
var
  XpDocumentFragment          : TXpDocumentFragment;
begin
  XpDocumentFragment := XpDocument.createDocumentFragment;
  try
    Result := IDOMDocumentFragment(TXpDOMDocumentFragment.Create(XpDocumentFragment));
  finally
    XpDocumentFragment.Release;
  end;
end;

function TXpDOMDocument.createElement(const tagName: DOMString): IDOMElement;
var
  XpElement                   : TXpElement;
begin
  XpElement := XpDocument.createElement(tagName);
  try
    Result := MakeNode(XpElement) as IDOMElement;
  finally
    XpElement.Release;
  end;
end;

function TXpDOMDocument.createElementNS(const namespaceURI, qualifiedName: DOMString): IDOMElement;
var
  XpElement                   : TXpElement;
begin
  XpElement := XpDocument.createElementNS(namespaceURI, qualifiedName);
  try
    Result := MakeNode(XpElement) as IDOMElement;
  finally
    XpElement.Release;
  end;
end;

function TXpDOMDocument.createEntityReference(const name: DOMString): IDOMEntityReference;
var
  XpEntityReference           : TXpEntityReference;
begin
  XpEntityReference := XpDocument.createEntityReference(name);
  try
    Result := IDOMEntityReference(TXpDOMEntityReference.Create(XpEntityReference));
  finally
    XpEntityReference.Release;
  end;
end;

function TXpDOMDocument.createProcessingInstruction(const target, data: DOMString): IDOMProcessingInstruction;
var
  XpProcessingInstruction     : TXpProcessingInstruction;
begin
  XpProcessingInstruction := XpDocument.createProcessingInstruction(target, data);
  try
    Result := IDOMProcessingInstruction(TXpDOMProcessingInstruction.Create(XpProcessingInstruction));
  finally
    XpProcessingInstruction.Release;
  end;
end;

function TXpDOMDocument.createTextNode(const data: DOMString): IDOMText;
var
  XpText                      : TXpText;
begin
  XpText := XpDocument.createTextNode(data);
  try
    Result := MakeNode(XpText) as IDOMText;
  finally
    XpText.Release;
  end;
end;

function TXpDOMDocument.get_doctype: IDOMDocumentType;
begin
  Result := IDOMDocumentType(TXpDOMDocumentType.Create(XpDocument.docType));
end;

function TXpDOMDocument.get_documentElement: IDOMElement;
begin
  Result := MakeNode(XpDocument.documentElement) as IDOMElement;
end;

function TXpDOMDocument.get_domImplementation: IDOMImplementation;
begin
  Result := IDOMImplementation(TXpDOMImplementation.Create(XpDocument.DomImplementation, false));
end;

function TXpDOMDocument.getElementById(const elementId: DOMString): IDOMElement;
begin
  DOMVendorNotSupported('getElementById', SXpXML);          { Do not localize }
end;

function TXpDOMDocument.getElementsByTagName(const tagName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(XpDocument.getElementsByTagName(tagName), true);
end;

function TXpDOMDocument.getElementsByTagNameNS(const namespaceURI, localName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(XpDocument.GetElementsByTagNameNS(namespaceURI, localName), true);
end;

function TXpDOMDocument.importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode;
begin
  Result := MakeNode(XpDocument.ImportNode(GetXpNode(importedNode), deep));
end;

procedure TXpDOMDocument.set_documentElement(const IDOMElement: IDOMElement);
begin
  DOMVendorNotSupported('set_documentElement', SXpXML);     { Do not localize }
  (*
  XpDocument.documentElement := GetXpNode(IDOMElement) as TXpElement;
  *)
end;

{ IDOMParseOptions Interface }

function TXpDOMDocument.get_async: Boolean;
begin
  Result := False;
end;

procedure TXpDOMDocument.set_async(Value: Boolean);
begin
end;

function TXpDOMDocument.get_preserveWhiteSpace: Boolean;
begin
  Result := not FXpObjModel.NormalizeData;
end;

function TXpDOMDocument.get_resolveExternals: Boolean;
begin
  Result := True;
  //DOMVendorNotSupported('get_resolveExternals', SXpXML);    { Do not localize }
end;

function TXpDOMDocument.get_validate: Boolean;
begin
  Result := False;
end;

procedure TXpDOMDocument.set_preserveWhiteSpace(Value: Boolean);
begin
  FXpObjModel.NormalizeData := not Value;
end;

procedure TXpDOMDocument.set_resolveExternals(Value: Boolean);
begin
  //DOMVendorNotSupported('set_resolveExternals', SXpXML);    { Do not localize }
end;

procedure TXpDOMDocument.set_validate(Value: Boolean);
begin
  //DOMVendorNotSupported('set_validate', SXpXML);            { Do not localize }
end;

{ IDOMPersist interface }

function TXpDOMDocument.asyncLoadState: Integer;
begin
  Result := -1;                                             // async mode not supported
end;

function TXpDOMDocument.get_xml: DOMString;
begin
  Result := XpDocument.Text;
end;

function TXpDOMDocument.load(source: OleVariant): WordBool;
begin
  Result := FXpObjModel.LoadDataSource(source);
end;

function TXpDOMDocument.loadFromStream(const stream: TStream): WordBool;
begin
  Result := FXpObjModel.LoadStream(stream);
end;

procedure TXpDOMDocument.save(destination: OleVariant);
begin
  FXpObjModel.SaveToFile(destination);
end;

procedure TXpDOMDocument.saveToStream(const stream: TStream);
begin
  FXpObjModel.SaveToStream(stream);
end;

function TXpDOMDocument.loadxml(const Value: DOMString): WordBool;
begin
  FXpObjModel.LoadMemory(Pointer(Value)^, Length(Value) * 2);
end;

procedure TXpDOMDocument.set_OnAsyncLoad(const Sender: TObject;
  EventHandler: TAsyncEventHandler);
begin
  DOMVendorNotSupported('set_OnAsyncLoad', SXpXML);         { Do not localize }
end;

{ IDOMParseError }

function TXpDOMDocument.get_errorCode: Integer;
begin
  DOMVendorNotSupported('get_errorCode', SXpXML);           { Do not localize }
  //Result := XpDocument.parseError.get_errorCode;
end;

function TXpDOMDocument.get_filepos: Integer;
begin
  DOMVendorNotSupported('get_filepos', SXpXML);             { Do not localize }
  //Result := XpDocument.parseError.get_filepos;
end;

function TXpDOMDocument.get_line: Integer;
begin
  DOMVendorNotSupported('get_line', SXpXML);                { Do not localize }
  //Result := XpDocument.parseError.get_line;
end;

function TXpDOMDocument.get_linepos: Integer;
begin
  DOMVendorNotSupported('get_linepos', SXpXML);             { Do not localize }
  //Result := XpDocument.parseError.get_linepos;
end;

function TXpDOMDocument.get_reason: WideString;
begin
  DOMVendorNotSupported('get_reason', SXpXML);              { Do not localize }
  //Result := XpDocument.parseError.get_reason;
end;

function TXpDOMDocument.get_srcText: WideString;
begin
  DOMVendorNotSupported('get_srcText', SXpXML);             { Do not localize }
  //Result := XpDocument.parseError.get_srcText;
end;

function TXpDOMDocument.get_url: WideString;
begin
  DOMVendorNotSupported('get_url', SXpXML);                 { Do not localize }
  //Result := XpDocument.parseError.get_url;
end;

{ TXpDOMImplementationFactory }

function TXpDOMImplementationFactory.DOMImplementation: IDOMImplementation;
begin
  Result := TXpDOMImplementation.Create(XpDOM.TXpDomImplementation.Create, true);
end;

function TXpDOMImplementationFactory.Description: string;
begin
  Result := SXpXML;
end;

initialization
  XpXML_DOM := TXpDOMImplementationFactory.Create;
  RegisterDOMVendor(XpXML_DOM);
finalization
  UnRegisterDOMVendor(XpXML_DOM);
  XpXML_DOM.Free;
end.

