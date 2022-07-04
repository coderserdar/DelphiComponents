unit XMLDOM;

interface

uses
  ActiveX, ComObj, Classes, SysUtils, Windows;

{ Forward declarations }

type
  INodes = interface;
  INode = interface;
  IAttributes = interface;
  IAttribute = interface;

{ IDocumentInterface }

  IDocumentInterface = interface(IUnknown)
    function GetController: IUnknown;
    property Controller: IUnknown read GetController;
  end;

{ ITextInterface }

  ITextInterface = interface(IDocumentInterface)
    function GetText: string;
    procedure SetText(Value: string);
    property Text: string read GetText write SetText;
  end;

{ IDocument }

  IDocument = interface(ITextInterface)
    function GetNodes: INodes;
    function GetRoot: INode;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property Nodes: INodes read GetNodes;
    property Root: INode read GetRoot;
  end;

{ INodes }

  INodes = interface(IDocumentInterface)
    function GetCount: Integer;
    function GetNode(Index: integer): INode;
    function Add(const Name: string): INode;
    procedure Clear;
    procedure Remove(Node: INode);
    property Count: Integer read GetCount;
    property Node[Index: Integer]: INode read GetNode; default;
  end;

{ IElement }

  IElement = interface(ITextInterface)
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetName: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Document: IDocument read GetDocument;
    property Parent: INode read GetParent;
    property Name: string read GetName;
    property Value: string read GetValue write SetValue;
  end;

{ INode }

  INode = interface(IElement)
    function GetAttributes: IAttributes;
    function GetNodes: INodes;
    property Attributes: IAttributes read GetAttributes;
    property Nodes: INodes read GetNodes;
    function FindAttribute(const Name: string): IAttribute;
    function FindNode(const Name: string): INode;
    function FindNodes(const Name: string): INodes;
  end;

{ IAttributes }

  IAttributes = interface(IDocumentInterface)
    function GetCount: Integer;
    function GetAttribute(Index: Integer): IAttribute;
    function Add(const Name: string): IAttribute;
    procedure Clear;
    procedure Remove(Node: IAttribute);
    property Count: Integer read GetCount;
    property Attribute[Index: Integer]: IAttribute read GetAttribute; default;
  end;

{ IAttribute }

  IAttribute = interface(IElement)
  end;

function CreateDocument: IDocument;

implementation

uses
  MSXML;

{ TDocument }

type
  TDocument = class(TInterfacedObject, IDocumentInterface, ITextInterface, IDocument)
  private
    FDocument: IXMLDOMDocument;
    { IDocumentInterface }
    function GetController: IUnknown;
    { ITextInterface }
    function GetText: string;
    procedure SetText(Value: string);
    { IDocument }
    function GetNodes: INodes;
    function GetRoot: INode;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  public
    constructor Create(Document: IXMLDOMDocument);
  end;

{ TNodes }

  TNodes = class(TInterfacedObject, IDocumentInterface, INodes)
  private
    FParent: IXMLDOMNode;
    FNodes: IXMLDOMNodeList;
    { IDocumentInterface }
    function GetController: IUnknown;
    { INodes }
    function GetCount: Integer;
    function GetNode(Index: integer): INode;
    function Add(const Name: string): INode;
    procedure Clear;
    procedure Remove(Node: INode);
  public
    constructor Create(Parent: IXMLDOMNode; Nodes: IXMLDOMNodeList);
  end;

  TElement = class(TInterfacedObject)
  private
    FNode: IXMLDOMNode;
    { IDocumentInterface }
    function GetController: IUnknown;
    { ITextInterface }
    function GetText: string;
    procedure SetText(Value: string);
    { IElement }
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetName: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(Node: IXMLDOMNode);
  end;

{ TNode }

  TNode = class(TElement, IDocumentInterface, ITextInterface, IElement, INode)
  private
    { INode }
    function GetAttributes: IAttributes;
    function GetNodes: INodes;
    function FindAttribute(const Name: string): IAttribute;
    function FindNode(const Name: string): INode;
    function FindNodes(const Name: string): INodes;
  end;

{ TAttributes }

  TAttributes = class(TInterfacedObject, IDocumentInterface, IAttributes)
  private
    FParent: IXMLDOMNode;
    FAttributes: IXMLDOMNamedNodeMap;
    { IDocumentInterface }
    function GetController: IUnknown;
    { IAttributes }
    function GetCount: Integer;
    function GetAttribute(Index: Integer): IAttribute;
    function Add(const Name: string): IAttribute;
    procedure Clear;
    procedure Remove(Attribute: IAttribute);
  public
    constructor Create(Parent: IXMLDOMNode;
      Attributes: IXMLDOMNamedNodeMap);
  end;

  TAttribute = class(TElement, IDocumentInterface, ITextInterface, IElement,
    IAttribute)
  end;

{ TDocument }

constructor TDocument.Create(Document: IXMLDOMDocument);
begin
  inherited Create;
  FDocument := Document;
end;

{ TDocument.IDocumentController }

function TDocument.GetController: IUnknown;
begin
  Result := FDocument;
end;

{ TDocument.ITextInterface }

function TDocument.GetText: string;
begin
  Result := FDocument.xml;
end;

procedure TDocument.SetText(Value: string);
begin
  FDocument.loadXML(Value);
end;

function TDocument.GetNodes: INodes;
begin
  Result := nil;
  Result := TNodes.Create(FDocument, FDocument.childNodes);
end;

function TDocument.GetRoot: INode;
begin
  Result := TNode.Create(FDocument.firstChild);
end;

procedure TDocument.LoadFromFile(const FileName: string);
begin
  FDocument.load(FileName);
end;

procedure TDocument.SaveToFile(const FileName: string);
begin
  FDocument.save(FileName);
end;

{ TNodes }

constructor TNodes.Create(Parent: IXMLDOMNode; Nodes: IXMLDOMNodeList);
begin
  inherited Create;
  FParent := Parent;
  FNodes := Nodes;
end;

{ TNodes.IDocumentController }

function TNodes.GetController: IUnknown;
begin
  Result := FNodes;
end;

function TNodes.GetCount: Integer;
begin
  Result := FNodes.length;
end;

function TNodes.GetNode(Index: Integer): INode;
begin
  Result := TNode.Create(FNodes.item[Index]);
end;

function TNodes.Add(const Name: string): INode;
var
  Document: IXMLDomDocument;
  Node: IXMLDOMNode;
begin
  if FParent.QueryInterface(IXMLDomDocument, Document) <> S_OK then
    Document := FParent.ownerDocument;
  Node := Document.createNode(NODE_ELEMENT, Name, '');
  Result := TNode.Create(FParent.appendChild(Node))
end;

procedure TNodes.Clear;
begin
  FParent.text := '';
end;

procedure TNodes.Remove(Node: INode);
begin
  FParent.removeChild(FParent);
end;

{ TElement }

constructor TElement.Create(Node: IXMLDOMNode);
begin
  inherited Create;
  FNode := Node;
end;

{ TElement.IDocumentInterface }

function TElement.GetController: IUnknown;
begin
  Result := FNode;
end;

{ TElement.ITextInterface }

function TElement.GetText: string;
begin
  Result := FNode.xml;
end;

procedure TElement.SetText(Value: string);
begin
end;

{ TElement.IElement }

function TElement.GetDocument: IDocument;
begin
  Result := TDocument.Create(FNode.ownerDocument);
end;

function TElement.GetParent: INode;
begin
  if FNode.parentNode <> nil then
    Result := TNode.Create(FNode.parentNode);
end;

function TElement.GetName: string;
begin
  Result := FNode.nodeName;
end;

function TElement.GetValue: string;
begin
  Result := FNode.text;
end;

procedure TElement.SetValue(const Value: string);
begin
  FNode.text := Value;
end;

{ TNode.INode }

function TNode.GetAttributes: IAttributes;
begin
  if FNode.attributes <> nil then
    Result := TAttributes.Create(FNode, FNode.attributes)
  else
    Result := nil;
end;

function TNode.GetNodes: INodes;
begin
  Result := TNodes.Create(FNode, FNode.childNodes);
end;

function TNode.FindAttribute(const Name: string): IAttribute;
var
  Node: IXMLDOMNode;
begin
  Node := FNode.attributes.getNamedItem(Name);
  if Node <> nil then
    Result := TAttribute.Create(Node)
  else
    Result := nil;
end;

function TNode.FindNode(const Name: string): INode;
var
  Node: IXMLDOMNode;
begin
  Node := FNode.selectSingleNode(Name);
  if Node <> nil then
    Result := TNode.Create(Node)
  else
    Result := nil;
end;

function TNode.FindNodes(const Name: string): INodes;
var
  NodeList: IXMLDOMNodeList;
begin
  NodeList := FNode.selectNodes(Name);
  Result := TNodes.Create(FNode, NodeList)
end;

{ TAttributes }

constructor TAttributes.Create(Parent: IXMLDOMNode;
   Attributes: IXMLDOMNamedNodeMap);
begin
  inherited Create;
  FParent := Parent;
  FAttributes := Attributes;
end;

{ TAttributes.IDocumentInterface }

function TAttributes.GetController: IUnknown;
begin
  Result := FAttributes;
end;

{ TAttributes.IAttributes }

function TAttributes.GetCount: Integer;
begin
  Result := FAttributes.length;
end;

function TAttributes.GetAttribute(Index: Integer): IAttribute;
begin
  Result := TAttribute.Create(FAttributes.item[Index]);
end;

function TAttributes.Add(const Name: string): IAttribute;
var
  Document: IXMLDOMDocument;
  Attribute: IXMLDOMAttribute;
begin
  Document := FParent.ownerDocument;
  Attribute := Document.createAttribute(Name);
  Result := TAttribute.Create(FAttributes.setNamedItem(Attribute));
end;

procedure TAttributes.Clear;
var
  I: Integer;
begin
  for I := FAttributes.length - 1 downto 0 do
    FAttributes.removeNamedItem(FAttributes.item[I].nodeName);
end;

procedure TAttributes.Remove(Attribute: IAttribute);
begin
  FAttributes.removeNamedItem(Attribute.Name);
end;

function CreateDocument: IDocument;
begin
  Result := TDocument.Create(CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument);
end;

end.
