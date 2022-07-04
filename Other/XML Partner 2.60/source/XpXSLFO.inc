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
{* XMLPartner Pro: XpXSLFO.INC                           *}
{*********************************************************}
{* XMLPartner Pro: XSL-FO Type Definitions               *}
{*********************************************************}

interface

uses
  Classes,
  SysUtils,
  XpBase,
  XpDOM,
{$IFDEF UsingCLX}
  XpQXSLT;
{$ELSE}
  XpvXSLT;
{$ENDIF}

const
  FO_PROPERTY_NODE = 50;

type
{== TXpFoProperty ====================================================}
  TXpFoProperty = class(TXpNode)
  private
    FIsInherited   : Boolean;
    FIsInheritable : Boolean;
    procedure prSetName(const aName : DOMString);
  public
    constructor Create; override;
    function CloneNode(bDeep : Boolean
                        {$IFNDEF VER100}
                        = True
                        {$ENDIF}
                        ) : TXpNode; override;

    property IsInheritable : Boolean
      read FIsInheritable
      write FIsInheritable;

    property IsInherited : Boolean
      read FIsInherited
      write FIsInherited;

    property Name : DOMString
      read noNodeName
      write prSetName;

    property Value : DOMString
      read noNodeValue
      write noNodeValue;
  end;
{=====================================================================}

{== TXpFoPropertyGroup ===============================================}
  TXpFoPropertyGroup = class(TXpNamedNodeMap)
  public
    procedure DoInherit(oFoGroup : TXpNamedNodeMap);

    function GetProperty(const sName : string) : string;
    function IsInheritable(const aPropName : string) : Boolean;
    procedure SetProperty(const sName,
                                sValue         : string;
                                bIsInheritable : Boolean
                                                 {$IFNDEF VER100}
                                                 = True
                                                 {$ENDIF}
                                                 );
  end;
{=====================================================================}

{== TXpFormattingObject ==============================================}
  PXpFormattingobject = ^TXpFormattingObject;
  TXpFormattingObject = class(TXpBaseXSLElement)
  protected
    FParentElement : TXpFormattingObject;
    FType          : Integer;
  public
    constructor Create; override;

    procedure Execute(Context : TXpXSLContext); override;
    function  GetPropertyValue(const aPropName : DOMString) : DOMString;
    procedure PreProcess(oContext: TXpXSLContext); override;
    procedure ProcessAttribs(oContext       : TXpXSLContext;
                             oNode          : TXpElement;
                             bAttribValTemp : Boolean); override;
    procedure SetPropertyValue(const aPropName,
                                     aPropValue : DOMString;
                                     aOverride  : Boolean);

    property FormatObjectType : Integer
      read FType
      write FType;

    property ParentFO : TXpFormattingObject
      read FParentElement
      write FParentElement;
  end;
{=====================================================================}

{== TXpPageRefCondition ==============================================}
  TXpFOPageRefCondition = class(TXpFormattingObject)
  protected
    FMasterName : string;
  public
    constructor Create; override;

    function IsMatch(const aPagePos : string;
                           aIsBlank,
                           aIsOdd   : Boolean) : Boolean;
    function Valid(oContext: TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property MasterName : string
      read FMasterName
      write FMasterName;
  end;
{=====================================================================}

{== TXpFORegion ========================================================}
  TXpFORegion = class(TXpFormattingObject)
  protected
    FContents   : TXpObjModel;
    FRegionName : string;
    FStatic     : TXpObjModel;

    function rgGetContentObject : TXpObjModel;
    function rgGetDocument : TXpDocument;
    function rgGetStaticObject : TXpObjModel;
    function rgGetStaticText : DOMString;
    function rgGetXMLDocument : DOMString;
    function rgHasContent : Boolean;
    function rgHasStaticContent : Boolean;
    procedure rgSetDocument(const aDoc : TXpDocument);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Initialize;
    procedure Reset; override;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property ContentObject : TXpObjModel
      read rgGetContentObject;

    property Document : TXpDocument
      read rgGetDocument
      write rgSetDocument;

    property HasContent : Boolean
      read rgHasContent;

    property HasStaticContent : Boolean
      read rgHasStaticContent;

    property RegionName : string
      read FRegionName
      write FRegionName;

    property StaticObject : TXpObjModel
      read rgGetStaticObject;

    property StaticText : DOMString
      read rgGetStaticText;

    property XMLDocument : DOMString
      read rgGetXMLDocument;
  end;
{=====================================================================}

{=====================================================================}
  TXpFORegionAfter = class(TXpFORegion)
  public
    constructor Create; override;
  end;
{=====================================================================}

{=====================================================================}
  TXpFORegionBefore = class(TXpFORegion)
  public
    constructor Create; override;
  end;
{=====================================================================}

{=====================================================================}
  TXpFORegionBody = class(TXpFORegion)
  public
    constructor Create; override;
  end;
{=====================================================================}

{=====================================================================}
  TXpFORegionEnd = class(TXpFORegion)
  public
    constructor Create; override;
  end;
{=====================================================================}

{== TXpFORegionStart =================================================}
  TXpFORegionStart = class(TXpFORegion)
  public
    constructor Create; override;
  end;
{=====================================================================}

{== TXpFORetrieveMarker ==============================================}
  TXpFORetrieveMarker  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpPageSequence ==================================================}
  PXpFOPageSequence = ^TXpFOPageSequence;
  TXpFOPageSequence = class(TXpFormattingObject)
  protected
  public
    constructor Create; override;

    procedure Reset; override;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpSimplePageMaster ==============================================}
  PXpFOSimplePageMaster = ^TXpFOSimplePageMaster;
  TXpFOSimplePageMaster = class(TXpFormattingObject)
  protected
    FMasterName  : string;
    FPropsAfter  : TXpFormattingObject;
    FPropsBefore : TXpFormattingObject;
    FPropsBody   : TXpFormattingObject;
    FPropsEnd    : TXpFormattingObject;
    FPropsStart  : TXpFormattingObject;
    function spmGetRegion(const aRegionName : string) : TXpFormattingObject;
    function spmGetRegionAfter : TXpFormattingObject;
    function spmGetRegionBefore : TXpFormattingObject;
    function spmGetRegionBody : TXpFormattingObject;
    function spmGetRegionEnd : TXpFormattingObject;
    function spmGetRegionStart : TXpFormattingObject;
    function spmHasRegionAfter : Boolean;
    function spmHasRegionBefore : Boolean;
    function spmHasRegionBody : Boolean;
    function spmHasRegionEnd : Boolean;
    function spmHasRegionStart : Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property MasterName : string
      read FMasterName
      write FMasterName;

    property HasRegionAfter : Boolean
      read spmHasRegionAfter;

    property HasRegionBefore : Boolean
      read spmHasRegionBefore;

    property HasRegionBody : Boolean
      read spmHasRegionBody;

    property HasRegionEnd : Boolean
      read spmHasRegionEnd;

    property HasRegionStart : Boolean
      read spmHasRegionStart;

    property RegionAfter : TXpFormattingObject
      read spmGetRegionAfter;

    property RegionBefore : TXpFormattingObject
      read spmGetRegionBefore;

    property RegionBody : TXpFormattingObject
      read spmGetRegionBody;

    property RegionEnd : TXpFormattingObject
      read spmGetRegionEnd;

    property RegionStart : TXpFormattingObject
      read spmGetRegionStart;
  end;
{=====================================================================}

{== TXpFOSinglePageRef =================================================}
  TXpFOSinglePageRef = class(TXpFormattingObject)
  protected
    FMasterName : string;
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property MasterName : string
      read FMasterName
      write FMasterName;
  end;
{=====================================================================}

{== TXpRepeatingPageRef ==============================================}
  TXpFORepeatingPageRef = class(TXpFOSinglePageRef)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpAlternatingPageRef ============================================}
  TXpFOAlternatingPageRef = class(TXpFormattingObject)
  protected
    function aprGetCondRefs : TXpNodeList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property ConditionalReferences : TXpNodeList
      read aprGetCondRefs;
  end;
{=====================================================================}

{== TXpPageSeqMaster =================================================}
  TXpFOPageSeqMaster = class(TXpFormattingObject)
  protected
    FMasterName     : string;
    FPageReferences : TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetPageMaster(const aMasterName : string)
                                             : TXpFormattingObject;
    function GetPageMasterByPageRefIdx(aPageRefIdx : Integer)
                                                   : TXpFOSimplePageMaster;
    function GetMaxRepeatsByPageRefIdx(aPageRefIdx : Integer)
                                                   : Integer;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;

    property MasterName : string
      read FMasterName
      write FMasterName;

    property PageReferences : TList
      read FPageReferences
      write FPageReferences;
  end;
{=====================================================================}

{== TXpFOLayoutMaster ================================================}
  TXpFOLayoutMaster = class(TXpFormattingObject)
  protected
//    FPageSeqMasters : TXpNodeList;
//    function lmGetPageSequenceMasters : TXpNodeList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetPageMaster(const aMasterName : string)
                                             : TXpFOSimplePageMaster;
    function GetPageSequenceMaster(const aMasterName : string)
                                                     : TXpFOPageSeqMaster;
    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;

//    property PageSequenceMasters : TXpNodeList
//      read lmGetPageSequenceMasters;
  end;
{=====================================================================}

{== TXpFORoot ========================================================}
  TXpFORoot = class(TXpFormattingObject)
  private
    FPageSeqs                   : TXpNodeList;
    function rtGetLayoutMaster  : TXpFOLayoutMaster;
    function rtGetPageSequences : TXpNodeList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;

    property LayoutMaster : TXpFOLayoutMaster
      read rtGetLayoutMaster;

    property PageSequences : TXpNodeList
      read rtGetPageSequences;
  end;
{=====================================================================}

{== TXpFOStaticContent ===============================================}
  TXpFOStaticContent = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTitle =======================================================}
  TXpFOTitle = class(TXpFormattingObject)
  public
    constructor Create; override;
  end;
{=====================================================================}

{== TXpFOBasicLink ===================================================}
  TXpFOBasicLink = class(TXpFormattingObject)
  protected
    FIsExternal : Boolean;
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOBlock =======================================================}
  TXpFOBlock = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOExternalGraphic =============================================}
  TXpFOExternalGraphic = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOFlow ========================================================}
  TXpFOFlow  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOInline ======================================================}
  TXpFOInline  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOInlineContainer =============================================}
  TXpFOInlineContainer  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOInstreamForeignObject =======================================}
  TXpFOInstreamForeignObject  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOLeader ======================================================}
  TXpFOLeader  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOListBlock ===================================================}
  TXpFOListBlock  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOListItem ====================================================}
  TXpFOListItem  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOListItemBody ================================================}
  TXpFOListItemBody = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOListItemLabel ===============================================}
  TXpFOListItemLabel = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMarker ======================================================}
  TXpFOMarker = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMultiCase ===================================================}
  TXpFOMultiCase = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMultiProperties =============================================}
  TXpFOMultiProperties = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMultiPropertySet ============================================}
  TXpFOMultiPropertySet = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMultiSwitch =================================================}
  TXpFOMultiSwitch = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOMultiToggle =================================================}
  TXpFOMultiToggle = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOPageNumber ==================================================}
  TXpFOPageNumber = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOBidiOverride ================================================}
  TXpFOBidiOverride = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOBlockContainer ==============================================}
  TXpFOBlockContainer = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOCharacter ===================================================}
  TXpFOCharacter = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOColorProfile ================================================}
  TXpFOColorProfile = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFODeclarations ================================================}
  TXpFODeclarations = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOFloat =======================================================}
  TXpFOFloat  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOFootnote ====================================================}
  TXpFOFootnote  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOFootnoteBody ================================================}
  TXpFOFootnoteBody  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOInitialPropertySet ==========================================}
  TXpFOInitialPropertySet  = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOPageNumberCitation ==========================================}
  TXpFOPageNumberCitation = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTable =======================================================}
  TXpFOTable = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableAndCaption =============================================}
  TXpFOTableAndCaption = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableBody ===================================================}
  TXpFOTableBody = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableCaption ================================================}
  TXpFOTableCaption = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableCell ===================================================}
  TXpFOTableCell = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableColumn =================================================}
  TXpFOTableColumn = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableFooter =================================================}
  TXpFOTableFooter = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableHeader =================================================}
  TXpFOTableHeader = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOTableRow ====================================================}
  TXpFOTableRow = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

{== TXpFOWrapper =====================================================}
  TXpFOWrapper = class(TXpFormattingObject)
  public
    constructor Create; override;

    function Valid(oContext : TXpXSLContext;
               var ErrMsg   : string) : Boolean; override;
  end;
{=====================================================================}

  TXpXSLFOException = class(Exception);

{=====================================================================}

implementation

uses
  XpXSLCon;

{== TXpFoProperty ====================================================}
constructor TXpFoProperty.Create;
begin
  inherited Create;
  noNodeType := FO_PROPERTY_NODE;
  FIsInherited := False;
  FIsInheritable := True;
end;
{--------}
function TXpFoProperty.CloneNode(bDeep : Boolean) : TXpNode;
var
  oNode : TXpFoProperty;
begin
  oNode := TXpFoProperty.Create;
  oNode.noNodeType := NodeType;
  oNode.NodeName := noNodeName;
  oNode.NodeValue := noNodeValue;
  oNode.IsInheritable := FIsInheritable;
  oNode.IsInherited := FIsInherited;
  oNode.NodeId := noNodeId;
  Result := oNode;
end;
{=====================================================================}

{== TXpFoPropertyGroupSimplePageMaster ===============================}
constructor TXpFORegion.Create;
begin
  inherited Create;
  FContents := nil;
  FStatic := nil;
  Initialize;
end;
{--------}
destructor TXpFORegion.Destroy;
begin
  FContents.Free;
  FStatic.Free;
  inherited;
end;
{--------}
procedure TXpFORegion.Initialize;
var
  oElem : TXpElement;
begin
  if (FContents <> nil) then begin
    FContents.ClearDocument;
    oElem := FContents.Document.CreateElement(FRegionName);
    FContents.Document.AppendChild(oElem);
    oElem.Release;
  end;
end;
{--------}
procedure TXpFORegion.Reset;
begin
  FContents.ClearDocument;
//  Empty;
  SetPropertyValue('background-attachment', 'scroll', False);
  SetPropertyValue('background-color', 'transparent', False);
  SetPropertyValue('background-image', 'none', False);
  SetPropertyValue('background-position-horizontal', '0pt', False);
  SetPropertyValue('background-position-vertical', '0pt', False);
  SetPropertyValue('background-repeat', 'repeat', False);
  SetPropertyValue('border-after-color', 'transparent', False);
  SetPropertyValue('border-after-width', 'medium', False);
  SetPropertyValue('border-before-color', 'transparent', False);
  SetPropertyValue('border-before-width', 'medium', False);
  SetPropertyValue('border-bottom-color', 'transparent', False);
  SetPropertyValue('border-bottom-width', 'medium', False);
  SetPropertyValue('border-end-color', 'transparent', False);
  SetPropertyValue('border-end-width', 'medium', False);
  SetPropertyValue('border-left-color', 'transparent', False);
  SetPropertyValue('border-left-width', 'medium', False);
  SetPropertyValue('border-right-color', 'transparent', False);
  SetPropertyValue('border-right-width', 'medium', False);
  SetPropertyValue('border-start-color', 'transparent', False);
  SetPropertyValue('border-start-width', 'medium', False);
  SetPropertyValue('border-top-color', 'transparent', False);
  SetPropertyValue('border-top-width', 'medium', False);
  SetPropertyValue('display-align', 'auto', False);
  SetPropertyValue('overflow', 'auto', False);
  SetPropertyValue('padding-after', '0pt', False);
  SetPropertyValue('padding-before', '0pt', False);
  SetPropertyValue('padding-bottom', '0pt', False);
  SetPropertyValue('padding-end', '0pt', False);
  SetPropertyValue('padding-left', '0pt', False);
  SetPropertyValue('padding-right', '0pt', False);
  SetPropertyValue('padding-start', '0pt', False);
  SetPropertyValue('padding-top', '0pt', False);
  SetPropertyValue('region-name', FRegionName, False);
  if (FRegionName = 'xsl-region-body') then begin
    SetPropertyValue('end-indent', '0pt', False);
    SetPropertyValue('margin-bottom', '0pt', False);
    SetPropertyValue('margin-left', '0pt', False);
    SetPropertyValue('margin-right', '0pt', False);
    SetPropertyValue('margin-top', '0pt', False);
    SetPropertyValue('space-after', '0pt', False);
    SetPropertyValue('space-before', '0pt', False);
    SetPropertyValue('start-indent', '0pt', False);
  end;
end;
{--------}
function TXpFORegion.rgGetContentObject: TXpObjModel;
//var
//  aElem : TXpElement;
begin
  if (FContents = nil) then begin
    FContents := TXpObjModel.Create(nil);
//    aElem := FContents.Document.CreateElement('root');
//    FContents.Document.AppendChild(aElem);
//    aElem.Release;
  end;
  Result := FContents;
end;

function TXpFORegion.rgGetDocument : TXpDocument;
begin
  Result := ContentObject.Document;
end;
{--------}
function TXpFORegion.rgGetStaticObject: TXpObjModel;
//var
//  aElem : TXpElement;
begin
  if (FStatic = nil) then begin
    FStatic := TXpObjModel.Create(nil);
//    aElem := FStatic.Document.CreateElement('root');
//    FStatic.Document.AppendChild(aElem);
//    aElem.Free;
  end;
  Result := FStatic;
end;
{--------}
function TXpFORegion.rgGetStaticText : DOMString;
begin
  if (FStatic <> nil) then
    Result := FStatic.XmlDocument
  else
    Result := '';
end;
{--------}
function TXpFORegion.rgGetXMLDocument : DOMString;
begin
  Result := FContents.XmlDocument;
end;
{--------}
function TXpFORegion.rgHasContent : Boolean;
begin
  if (FContents <> nil) then
    Result := True
  else
    Result := False;
end;
{--------}
function TXpFORegion.rgHasStaticContent : Boolean;
begin
  if (FStatic <> nil) then
    Result := True
  else
    Result := False;
end;
{--------}
procedure TXpFORegion.rgSetDocument(const aDoc : TXpDocument);
begin
  FContents.Document := aDoc;
end;
{=====================================================================}

{== TXpLayoutMaster ==================================================}
constructor TXpFOLayoutMaster.Create;
begin
  inherited;
//  FPageSeqMasters := nil;
  FormatObjectType := FO_LAYOUT_MASTER_SET;
end;
{--------}
destructor TXpFOLayoutMaster.Destroy;
begin
//  FPageSeqMasters.Free;
  inherited;
end;
{--------}
function TXpFOLayoutMaster.GetPageMaster(const aMasterName : string)
                                                           : TXpFOSimplePageMaster;
var
  TempFO, TempFO2 : TXpFormattingObject;
  i, j, k   : Integer;
begin
  Result := nil;
  for i := 0 to (ChildNodes.Length - 1) do
    if (TXpFormattingObject(ChildNodes.Item(i)).GetPropertyValue(XpsMasterName) = aMasterName) then begin
      TempFO := TXpFormattingObject(ChildNodes.Item(i));
      if (TempFO is TXpFOSimplePageMaster) then
        Result := TXpFOSimplePageMaster(ChildNodes.Item(i))
      else if (TempFO is TXpFOPageSeqMaster) then begin
        for j := 0 to TempFO.ChildNodes.Length - 1 do begin
          TempFO2 := TXpFormattingObject(TempFO.ChildNodes.Item(j));
          if TempFO2 is TXpFORepeatingPageRef then begin
            for k := 0 to ChildNodes.Length - 1 do
              if (TXpFormattingObject(ChildNodes.Item(k)) is TXpFOSimplePageMaster) then
                if (TXpFormattingObject(ChildNodes.Item(k)).
                  GetPropertyValue(XpsMasterName) =
                    TempFO2.GetPropertyValue('master-reference')) then begin
                      Result := TXpFOSimplePageMaster(ChildNodes.Item(k));
                      exit;
                    end;
          end;
        end;
      end;

        (*
        for j := 0 to (ChildNodes.Length - 1) do
          if (j <> i) then                                     {!!.57}
            if (ChildNodes.Item(j) is TXpFOPageSeqMaster) then begin{!!.57}
              if (TempFO.GetPropertyValue(XpsMasterName) =
                  TXpFOPageSeqMaster(ChildNodes.Item(j)).GetPropertyValue(XpsMasterName)) then
                Result := TXpFOSimplePageMaster(ChildNodes.Item(j));
            end else
            {!!!! note done here}
            if (ChildNodes.Item(j) is TXpFORepeatingPageRef) then begin{!!.57}
              if (TempFO.GetPropertyValue(XpsMasterName) =
                  TXpFOPageSeqMaster(ChildNodes.Item(j)).GetPropertyValue('master-reference')) then
                Result := TXpFOSimplePageMaster(ChildNodes.Item(j));
            end;
        *)
      Break;
    end;
end;
{--------}
function TXpFOLayoutMaster.GetPageSequenceMaster(const aMasterName : string)
                                                                   : TXpFOPageSeqMaster;
var
  TempElement : TXpElement;
  i           : Integer;
begin
  Result := nil;
  for i := 0 to (ChildNodes.Length - 1) do begin
    TempElement := TXpElement(ChildNodes.Item(i));
    if (TempElement is TXpFOPageSeqMaster) and
       (
       (TempElement.GetAttribute(XpsMasterName) = aMasterName)
     or (TempElement.GetAttribute('master-reference') = aMasterName) {!!.57}
       )then begin
      Result := TXpFOPageSeqMaster(TempElement);
      Break;
    end;
  end;
end;
{--------}
{function TXpFOLayoutMaster.lmGetPageSequenceMasters : TXpNodeList;
var
  i : Integer;
begin
  if (FPageSeqMasters = nil) then begin
    FPageSeqMasters := TXpNodeList.Create;
    for i := 0 to (ChildNodes.Length - 1) do begin
      if (TXpFormattingObject(ChildNodes.Item(i)).FormatObjectType =
          FO_PAGE_SEQUENCE_MASTER) then
        FPageSeqMasters.Add(ChildNodes.Item(i));
    end;
  end;
end; }
{--------}
function TXpFOLayoutMaster.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string)
                                          : Boolean;
var
  i           : Integer;
  HasGoodKids : Boolean;
begin
  assert(Pointer(Self) <> nil);
  assert(TObject(Self) is TXpFOLayoutMaster);
  inherited Valid(oContext, ErrMsg);

  Result := True;
  HasGoodKids := False;

  { Must have at least 1 Simple-Page-Master or Page-Sequence-Master
    children.}
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOSimplePageMaster) then begin
      HasGoodKids := True;
      if (not TXpFOSimplePageMaster(ChildNodes.Item(i)).Valid(oContext, ErrMsg)) then begin
        Result := False;
        Break;
      end;
    end else if (ChildNodes.Item(i).NodeName = XpsFOPageSequenceMaster) then begin
      HasGoodKids := True;
      if (not TXpFOPageSeqMaster(ChildNodes.Item(i)).Valid(oContext, ErrMsg)) then begin
        Result := False;
        Break;
      end;
    end else begin
      ErrMsg := format(sXSLFOInvalidChild, [ChildNodes.Item(i).NodeName,
                                            NodeName]);
      Result := False;
      Break;
    end;
  end;
  if (not HasGoodKids) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                          XpsFOSimplePageMaster + ' or ' +
                                          XpsFOSinglePageMasterReference]);
end;
{=====================================================================}

{== TXpSimplePageMaster ==============================================}
constructor TXpFOSimplePageMaster.Create;
begin
  inherited Create;
  FPropsAfter := nil;
  FPropsBefore := nil;
  FPropsBody := nil;
  FPropsEnd := nil;
  FPropsStart := nil;
  FormatObjectType := FO_SIMPLE_PAGE_MASTER;
end;
{--------}
destructor TXpFOSimplePageMaster.Destroy;
begin
  inherited;
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegion(const aRegionName : string)
                                                              : TXpFormattingObject;
var
  i : Integer;
begin
  if (aRegionName = XpsFORegAfter) then begin
    if (FPropsAfter = nil) then begin
      for i := 0 to (ChildNodes.Length - 1) do begin
        if (ChildNodes.Item(i).NodeName = XpsFORegionAfter) then
          FPropsAfter := TXpFormattingObject(ChildNodes.Item(i));
      end;
    end;
    Result := FPropsAfter;
  end else if (aRegionName = XpsFORegBefore) then begin
    if (FPropsBefore = nil) then begin
      for i := 0 to (ChildNodes.Length - 1) do begin
        if (ChildNodes.Item(i).NodeName = XpsFORegionBefore) then
          FPropsBefore := TXpFormattingObject(ChildNodes.Item(i));
      end;
    end;
    Result := FPropsBefore
  end else if (aRegionName = XpsFORegBody) then begin
    if (FPropsBody = nil) then begin
      for i := 0 to (ChildNodes.Length - 1) do begin
        if (ChildNodes.Item(i).NodeName = XpsFORegionBody) then
          FPropsBody := TXpFormattingObject(ChildNodes.Item(i));
      end;
    end;
    Result := FPropsBody
  end else if (aRegionName = XpsFORegEnd) then begin
    if (FPropsEnd = nil) then begin
      for i := 0 to (ChildNodes.Length - 1) do begin
        if (ChildNodes.Item(i).NodeName = XpsFORegionEnd) then
          FPropsEnd := TXpFormattingObject(ChildNodes.Item(i));
      end;
    end;
    Result := FPropsEnd
  end else if (aRegionName = XpsFORegStart) then begin
    if (FPropsStart = nil) then begin
      for i := 0 to (ChildNodes.Length - 1) do begin
        if (ChildNodes.Item(i).NodeName = XpsFORegionStart) then
          FPropsStart := TXpFormattingObject(ChildNodes.Item(i));
      end;
    end;
    Result := FPropsStart
  end else
    Result := nil;
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegionAfter : TXpFormattingObject;
begin
  Result := spmGetRegion(XpsFORegAfter);
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegionBefore : TXpFormattingObject;
begin
  Result := spmGetRegion(XpsFORegBefore);
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegionBody : TXpFormattingObject;
begin
  Result := spmGetRegion(XpsFORegBody);
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegionEnd : TXpFormattingObject;
begin
  Result := spmGetRegion(XpsFORegEnd);
end;
{--------}
function TXpFOSimplePageMaster.spmGetRegionStart : TXpFormattingObject;
begin
  Result := spmGetRegion(XpsFORegStart);
end;
{--------}
function TXpFOSimplePageMaster.spmHasRegionAfter : Boolean;
begin
  { FPropsAfter will be nil until it's accessed the first time, but
    that doesn't mean there isn't an after region there...}
  if (FPropsAfter = nil) then
    {...we have to go look to be sure.}
    spmGetRegionAfter;
    { If a region after was found, FPropsAfter is now assigned.}
  Result := (FPropsAfter <> nil);
end;
{--------}
function TXpFOSimplePageMaster.spmHasRegionBefore : Boolean;
begin
  { See spmHasRegionAfter for notes.}
  if (FPropsBefore = nil) then
    spmGetRegionBefore;
  Result := (FPropsBefore <> nil);
end;
{--------}
function TXpFOSimplePageMaster.spmHasRegionBody : Boolean;
begin
  { See spmHasRegionAfter for notes.}
  if (FPropsBody = nil) then
    spmGetRegionBody;
  Result := (FPropsBody <> nil);
end;
{--------}
function TXpFOSimplePageMaster.spmHasRegionEnd : Boolean;
begin
  { See spmHasRegionAfter for notes.}
  if (FPropsEnd = nil) then
    spmGetRegionEnd;
  Result := (FPropsEnd <> nil);
end;
{--------}
function TXpFOSimplePageMaster.spmHasRegionStart : Boolean;
begin
  { See spmHasRegionAfter for notes.}
  if (FPropsStart = nil) then
    spmGetRegionStart;
  Result := (FPropsStart <> nil);
end;
{--------}
procedure TXpFOSimplePageMaster.Reset;
begin
  SetPropertyValue('end-indent', '0pt', False);
  SetPropertyValue('margin-bottom', '0pt', False);
  SetPropertyValue('margin-left', '0pt', False);
  SetPropertyValue('margin-right', '0pt', False);
  SetPropertyValue('margin-top', '0pt', False);
  SetPropertyValue('master-name', FMasterName, False);
  SetPropertyValue('page-height', 'auto', False);
  SetPropertyValue('page-width', 'auto', False);
  SetPropertyValue('space-after', '0pt', False);
  SetPropertyValue('space-before', '0pt', False);
  SetPropertyValue('start-indent', '0pt', False);
end;
{=====================================================================}

{== TXpFORetrieveMarker ==============================================}
constructor TXpFORetrieveMarker.Create;
begin
  inherited;
  FormatObjectType := FO_RETRIEVE_MARKER;
end;
{--------}
function TXpFORetrieveMarker.Valid(oContext: TXpXSLContext;
                               var ErrMsg  : string)
                                           : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpPageRefCondition ==============================================}
constructor TXpFOPageRefCondition.Create;
begin
  inherited;
  FormatObjectType := FO_CONDITIONAL_PAGE_MASTER_REFERENCE;
end;
{--------}
function TXpFOPageRefCondition.IsMatch(const aPagePos : string;
                                             aIsBlank,
                                             aIsOdd   : Boolean) : Boolean;
var
  PagePosProp,
  IsBlankProp,
  IsOddProp    : string;
begin
  Result := False;

  PagePosProp := GetPropertyValue(XpsPagePosition);
  IsBlankProp := GetPropertyValue(XpsBlankOrNotBlank);
  IsOddProp := GetPropertyValue(XpsOddOrEven);

  { Do we have a PagePos match? }
  if (PagePosProp = 'any') or
     (PagePosProp = '') or
     (PagePosProp = aPagePos) or
     ((PagePosProp = 'rest') and
      ((aPagePos <> 'first') and
       (aPagePos <> 'last'))) then
    { Do we have a Blank page match? }
    if (IsBlankProp = 'any') or
       (IsBlankProp = '') or
       ((aIsBlank) and
        (IsBlankProp = 'blank')) or
       ((not aIsBlank) and
        (IsBlankProp = 'not-blank')) then
      { Do we have an Odd page match? }
      if (IsOddProp = 'any') or
         (IsOddProp = '') or
         ((aIsOdd) and
          (IsOddProp = 'odd')) or
         ((not aIsOdd) and
          (IsOddProp = 'even')) then
        Result := True;
end;
{=====================================================================}

{== TXpPageSeqMaster =================================================}
constructor TXpFOPageSeqMaster.Create;
begin
  inherited;
  FormatObjectType := FO_PAGE_SEQUENCE_MASTER;
end;
{--------}
destructor TXpFOPageSeqMaster.Destroy;
begin
  FPageReferences.Free;
  inherited;
end;
{--------}
function TXpFOPageSeqMaster.GetMaxRepeatsByPageRefIdx(aPageRefIdx : Integer)
                                                                  : Integer;
begin
  Result := TXpElement(ChildNodes.Item(aPageRefIdx)).GetAttributeInt(XpsMaximumRepeats);
end;
{--------}
function TXpFOPageSeqMaster.GetPageMaster(const aMasterName : string)
                                                            : TXpFormattingObject;
begin
  Result := TXpFOLayoutMaster(FParentElement).GetPageMaster(aMasterName);
end;
{=====================================================================}

{== TXpAlternatingPageRef ============================================}
function TXpFOAlternatingPageRef.aprGetCondRefs : TXpNodeList;
begin
  if (ChildNodes = nil) then
    Result := nil
  else
    Result := ChildNodes;
end;
{--------}
constructor TXpFOAlternatingPageRef.Create;
begin
  inherited;
  FormatObjectType := FO_REPEATABLE_PAGE_MASTER_ALTERNATIVES;
end;
{--------}
destructor TXpFOAlternatingPageRef.Destroy;
begin
  inherited;
end;
{=====================================================================}

{== TXpRepeatingPageRef ==============================================}
constructor TXpFORepeatingPageRef.Create;
begin
  inherited;
  FormatObjectType := FO_REPEATABLE_PAGE_MASTER_REFERENCE;
end;
{=====================================================================}

{== TXpSinglePageRef =================================================}
constructor TXpFOSinglePageRef.Create;
begin
  inherited;
  FormatObjectType := FO_SINGLE_PAGE_MASTER_REFERENCE;
end;
{=====================================================================}


{== TXpFormattingObject ==============================================}
constructor TXpFormattingObject.Create;
begin
  inherited;
  FAllowsTemplateBody := True;
  ParentFO := nil;
end;
{--------}
procedure TXpFoPropertyGroup.DoInherit(oFoGroup : TXpNamedNodeMap);
var
  oProp    : TXpFoProperty;
  oOldNode : TXpNode;
  i        : Integer;
begin
  for i := 0 to oFoGroup.Length - 1 do begin
    if (TXpFoProperty(oFoGroup.Item(i)).IsInheritable) then begin
      oProp := TXpFoProperty(oFoGroup.Item(i).CloneNode(True));
      oOldNode := RemoveNamedItem(oProp.Name);
      if (oOldNode <> nil) then
        oOldNode.Free;
      oProp.FIsInherited := True;
      Add(oProp);
      oProp.Release;
    end;
  end;
end;
{--------}
procedure TXpFormattingObject.Execute(Context : TXpXSLContext);
var
  oMediator : TXpElementMediator;
  oClone    : TXpElement;
begin
  { Start copying the element to the result tree. }
  oMediator := ParentStyleSheet.Mediator;

  oClone := TXpElement(CloneNode(False));
  try
    ProcessAttribs(Context, oClone, True);
    oMediator.CopyFO(oClone);
  finally
    oClone.Free;
  end;

  { Execute the child nodes. }
  ExecuteChildren(Context);

  oMediator.FOEnd;
end;
{--------}
function TXpFormattingObject.GetPropertyValue(const aPropName : DOMString)
                                                              : DOMString;
begin
  assert(Pointer(Self) <> nil);
  assert(TObject(Self) is TXpFormattingObject);
  Result := GetAttribute(aPropName);
end;
{--------}
procedure TXpFormattingObject.PreProcess;
begin
  { No processing is done here. The result tree is validated after it
    is constructed. The base filter class validates the root element
    of the formatting tree in its ApplyStyleEnd handler. This validates
    the complete formatting object tree.}
end;
{--------}
procedure TXpFormattingObject.ProcessAttribs(oContext       : TXpXSLContext;
                                             oNode          : TXpElement;
                                             bAttribValTemp : Boolean);
var
  anInx      : Integer;
  oAttrib    : TXpAttribute;
  oMediator  : TXpElementMediator;
begin
  oMediator := ParentStyleSheet.Mediator;

  if (oNode.HasAttributes) then
    for anInx := 0 to Pred(oNode.Attributes.Length) do begin
      oAttrib := TXpAttribute(oNode.Attributes.Item(anInx));
      { Evaluate it as an attribute value template? }
      { Note: bAttribValTemp should always be True.}
      oAttrib.Value := EvaluateAttrib(oContext, oAttrib.Value);

      if (not (oNode is TXpFormattingObject)) then
        { Copy to result tree. }
        oMediator.CopyAttribute(oAttrib.Name, oAttrib.Value, Self)
    end;
end;
{--------}
procedure TXpFormattingObject.SetPropertyValue(const aPropName,
                                                     aPropValue : DOMString;
                                                     aOverride  : Boolean);
begin
  if ((aOverride) or
      (GetAttribute(aPropName) = '')) then
    SetAttribute(aPropName, aPropValue)
end;
{=====================================================================}

{=====================================================================}
function TXpFoPropertyGroup.GetProperty(const sName : string) : string;
var
  oNode : TXpNode;
begin
  Result := '';
  oNode := GetNamedItem(sName);
  if (oNode <> nil) then
    Result := oNode.NodeValue;
end;
{--------}
function TXpFoPropertyGroup.IsInheritable(const aPropName : string)
                                                          : Boolean;
begin
  Result := (aPropName <> XpsMasterName) and
            (aPropName <> XpsRegionName);
end;
{--------}
procedure TXpFoPropertyGroup.SetProperty(const sName,
                                               sValue         : string;
                                               bIsInheritable : Boolean);
var
  oProp    : TXpFoProperty;
  oOldNode : TXpNode;
begin
  oOldNode := RemoveNamedItem(sName);
  if oOldNode <> nil then
    oOldNode.Free;
  if sValue <> '' then begin
    oProp := TXpFoProperty.Create;
    oProp.Name := sName;
    oProp.Value := sValue;
    if (not bIsInheritable) then
      oProp.IsInheritable := False
    else
      oProp.IsInheritable := ((sName <> XpsBackgroundColor) and
                              (sName <> XpsBreakAfter) and
                              (sName <> XpsBreakBefore) and
                              (sName <> XpsMarginLeft) and
                              (sName <> XpsMarginRight) and
                              (sName <> XpsSpaceAfter) and
                              (sName <> XpsSpaceAfterMaximum) and
                              (sName <> XpsSpaceAfterMinimum) and
                              (sName <> XpsSpaceAfterOptimum) and
                              (sName <> XpsSpaceBefore) and
                              (sName <> XpsSpaceBeforeMaximum) and
                              (sName <> XpsSpaceBeforeMinimum) and
                              (sName <> XpsSpaceBeforeOptimum));
    Add(oProp);
    oProp.Release;
  end;
end;
{=====================================================================}

{== TXpPageSequence ==================================================}
constructor TXpFOPageSequence.Create;
begin
  inherited;
  FormatObjectType := FO_PAGE_SEQUENCE;
end;
{--------}
procedure TXpFOPageSequence.Reset;
begin
end;
{=====================================================================}

{=====================================================================}
{ TXpFORegionBefore }
constructor TXpFORegionBefore.Create;
begin
  FRegionName := XpsFORegBefore;
  inherited;
  noNodeName := XpsFORegionBefore;
  FormatObjectType := FO_REGION_BEFORE;
end;
{=====================================================================}

{=====================================================================}
{ TXpFORegionAfter }
constructor TXpFORegionAfter.Create;
begin
  FRegionName := XpsFORegAfter;
  inherited;
  noNodeName := XpsFORegionAfter;
  FormatObjectType := FO_REGION_AFTER;
end;
{=====================================================================}
{ TXpFORegionBody }
constructor TXpFORegionBody.Create;
begin
  FRegionName := XpsFORegBody;
  inherited;
  noNodeName := XpsFORegionBody;
  FormatObjectType := FO_REGION_BODY;
end;
{=====================================================================}

{=====================================================================}
{ TXpFORegionEnd }
constructor TXpFORegionEnd.Create;
begin
  FRegionName := XpsFORegEnd;
  inherited;
  noNodeName := XpsFORegionEnd;
  FormatObjectType := FO_REGION_END;
end;
{=====================================================================}

{=====================================================================}
{ TXpFORegionStart }
constructor TXpFORegionStart.Create;
begin
  FRegionName := XpsFORegStart;
  inherited;
  noNodeName := XpsFORegionStart;
  FormatObjectType := FO_REGION_START;
end;
{=====================================================================}

{=====================================================================}
{ TXpFORoot }
constructor TXpFORoot.Create;
begin
  inherited;
  FormatObjectType := FO_ROOT;
  FPageSeqs := nil;
end;
{--------}
destructor TXpFORoot.Destroy;
begin
  FPageSeqs.Free;
  inherited;
end;
{--------}
function TXpFORoot.rtGetLayoutMaster : TXpFOLayoutMaster;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (TXpFormattingObject(ChildNodes.Item(i)).FormatObjectType =
        FO_LAYOUT_MASTER_SET) then begin
      Result := TXpFOLayoutMaster(ChildNodes.Item(i));
      Break;
    end;
  end;
end;
{--------}
function TXpFORoot.rtGetPageSequences : TXpNodeList;
var
  i : Integer;
begin
  if (FPageSeqs = nil) then begin
    FPageSeqs := TXpNodeList.Create;
    for i := 0 to (ChildNodes.Length - 1) do
      if (TXpFormattingObject(ChildNodes.Item(i)).FormatObjectType = FO_PAGE_SEQUENCE) then
        FPageSeqs.Add(TXpFOPageSequence(ChildNodes.Item(i)));
  end;
  Result := FPageSeqs;
end;
{--------}
function TXpFORoot.Valid(oContext : TXpXSLContext;
                     var ErrMsg   : string) : Boolean;
var
  HasLayoutMaster : Boolean;
  HasPageSequence : Boolean;
  i               : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  if (FType <> FO_ROOT) then begin 
    Result := True;
    Exit;
  end else
    Result := False;

  HasLayoutMaster := False;
  HasPageSequence := False;

  for i := 0 to (ChildNodes.Length - 1) do begin
    { Root must have 1 fo:layout-master-set child }
    if (ChildNodes.Item(i).NodeName = XpsFOLayoutMasterSet) then begin
      HasLayoutmaster := True;
      Result := TXpFOLayoutMaster(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end else if (ChildNodes.Item(i).NodeName = XpsFOPageSequence) then begin
      { Root must have 1 or more fo:page-sequence children.}
      HasPageSequence := True;
      Result := TXpFOPageSequence(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end else begin
      ErrMsg := format(sXSLFOInvalidChild, [ChildNodes.Item(i).NodeName,
                                            NodeName]);
      Result := False;
      Break;
    end;
  end;

  if (Result) then
    if (not HasLayoutMaster) then begin
      ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                            XpsFOLayoutMasterSet]);
      Result := False;
    end else if (not HasPageSequence) then begin
      ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                            XpsFOPageSequence]);
      Result := False;
    end;
end;
{=====================================================================}

{== TXpFOFlow ========================================================}
constructor TXpFOFlow.Create;
begin
  inherited;
  FormatObjectType := FO_FLOW;
end;
{--------}
function TXpFOFlow.Valid(oContext : TXpXSLContext;
                     var ErrMsg   : string)
                                  : Boolean;
var
  ChildNodeName : string;
  i             : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  Result := False;
  for i := 0 to (ChildNodes.Length - 1) do begin
    ChildNodeName := TXpFormattingObject(ChildNodes.Item(i)).NodeName;
    if (ChildNodeName = XpsFOBlock) or
       (ChildNodeName = XpsFOTable) or
       (ChildNodeName = XpsFOListBlock) then begin
      Result := True;
      Break;
    end;
  end;
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFOBlock + ' or ' +
                                                    XpsFOTable + ' or ' +
                                                    XpsFOListBlock]);
end;
{=====================================================================}
function TXpFOPageSequence.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string)
                                          : Boolean;
var
  HasGoodKids : Boolean;
  i           : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  HasGoodKids := False;

  { Must have a master-name property.}
  Result := (GetAttributeNode(XpsMasterName) <> nil);

  { Must have atleast 1 flow child }
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOFlow) then
      HasGoodKids := True;
    Result := TXpFormattingObject(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
    if (not Result) then
      Break;
  end;
  if (not HasGoodKids) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFOFlow]);
end;
{--------}
function TXpFOPageSeqMaster.GetPageMasterByPageRefIdx(aPageRefIdx : Integer)
                                                                  : TXpFOSimplePageMaster;
var
  MasterName : string;
begin
  Result := nil;
  MasterName := TXpElement(ChildNodes.Item(aPageRefIdx)).GetAttribute(XpsMasterName);
  if (MasterName = '') then
    MasterName := TXpElement(ChildNodes.Item(aPageRefIdx)).GetAttribute('master-reference');
  if (MasterName <> '') then
    Result := TXpFOSimplePageMaster(GetPageMaster(MasterName));
end;
{--------}
function TXpFOPageSeqMaster.Valid(oContext : TXpXSLContext;
                              var ErrMsg   : string)
                                           : Boolean;
var
  ChildNodeName : string;
  i             : Integer;
  HasGoodKids   : Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  HasGoodKids := False;

  { Must have a master-name property.}
  Result := (GetAttributeNode(XpsMasterName) <> nil);
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsMasterName])
  else begin
    Result := False;
    { Must have one of the following children nodes.}
    for i := 0 to (ChildNodes.Length - 1) do begin
      ChildNodeName := ChildNodes.Item(i).NodeName;
      if (ChildNodeName = XpsFOSinglePageMasterReference) or
         (ChildNodeName = XpsFORepeatablePageMasterAlternatives) or
         (ChildNodeName = XpsFORepeatablePageMasterReference) then begin
        HasGoodKids := True;
        Result := TXpFormattingObject(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
        if (not Result) then
          Break;
      end;
    end;
    if (not HasGoodKids) then
      ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                            XpsFOSinglePageMasterReference + ' or ' +
                                            XpsFORepeatablePageMasterAlternatives + ' or ' +
                                            XpsFORepeatablePageMasterReference]);
  end;
end;
{--------}
function TXpFORegion.Valid(oContext : TXpXSLContext;
                       var ErrMsg   : string)
                                    : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { No requirements.}
  Result := True;
end;
{=====================================================================}
function TXpFOAlternatingPageRef.Valid(oContext : TXpXSLContext;
                                   var ErrMsg   : string)
                                                : Boolean;
var
  HasGoodKids : Boolean;
  i           : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  Result := False;
  HasGoodKids := False;
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOConditionalPageMasterReference) then begin
      HasGoodKids := True;
      Result := TXpFOPageRefCondition(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end;
  end;
  if (not HasGoodKids) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                          XpsFOConditionalPageMasterReference]);
end;
{=====================================================================}

{=====================================================================}
function TXpFORepeatingPageRef.Valid(oContext : TXpXSLContext;
                                 var ErrMsg: string): Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  Result := (GetAttributeNode(XpsMasterName) <> nil);
  if (not Result) then
    Result := (GetAttributeNode('master-reference') <> nil);           {!!.57}
  if (not Result) then                                                 {!!.57}
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsMasterName]);
end;
{=====================================================================}

{=====================================================================}
function TXpFOSimplePageMaster.Valid(oContext : TXpXSLContext;
                                 var ErrMsg   : string)
                                              : Boolean;
var
  i           : Integer;
  HasGoodKids : Boolean;
begin
  assert(TObject(Self) is TXpFOSimplePageMaster);
  inherited Valid(oContext, ErrMsg);

  HasGoodKids := False;

  { Must have a master-name property }
  Result := (GetAttributeNode(XpsMasterName) <> nil);

  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsMasterName])
  else begin
    Result := False;
    { Must have a region-body child}
    for i := 0 to (ChildNodes.Length - 1) do begin
      if (ChildNodes.Item(i) is TXpFORegion) then begin
        Result := TXpFORegion(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
        if (not Result) then
          Break
        else if (ChildNodes.Item(i).NodeName = XpsFORegionBody) then
          HasGoodKids := True;
      end else begin
        { Only fo:region-* are allowed.}
        ErrMsg := Format(sXSLFOInvalidChild, [ChildNodes.Item(i).NodeName,
                                              NodeName]);
        Break;
      end;
    end;
    if (not HasGoodKids) then begin
      ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFORegionBody]);
      Result := False;
    end;
  end;
end;
{=====================================================================}

{=====================================================================}
function TXpFOSinglePageRef.Valid(oContext : TXpXSLContext;
                              var ErrMsg : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  { Must have a master-name property.}
  Result := (GetAttributeNode(XpsMasterName) <> nil);
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsMasterName]);
end;
{=====================================================================}

{== TXpFOStaticContent ===============================================}
constructor TXpFOStaticContent.Create;
begin
  inherited;
  FormatObjectType := FO_STATIC_CONTENT;
end;
{--------}
function TXpFOStaticContent.Valid(oContext : TXpXSLContext;
                              var ErrMsg   : string)
                                           : Boolean;
var
  i           : Integer;
  HasGoodKids : Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  HasGoodKids := False;

  { Must have a flow-name property.}
  Result := (GetAttributeNode(XpsFlowName) <> nil);
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsFlowName])
  else begin
    for i := 0 to (ChildNodes.Length - 1) do begin
      if (ChildNodes.Item(i).NodeName = XpsFOBlock) or
         (ChildNodes.Item(i).NodeName = XpsFOTable) or
         (ChildNodes.Item(i).NodeName = XpsFOListBlock) then
        HasGoodKids := True;
      if (ChildNodes.Item(i) is TXpFormattingObject) then
        Result := TXpFormattingObject(ChildNodes.Item(i)).Valid(oContext, ErrMsg)
      else
        Result := True;
      if (not Result) then
        Break;
    end;
    if (not HasGoodKids) then
      ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFOBlock + ' or ' +
                                                      XpsFOTable + ' or ' +
                                                      XpsFOListBlock]);
  end;
end;
{--------}
function TXpFOPageRefCondition.Valid(oContext : TXpXSLContext;
                                 var ErrMsg   : string)
                                              : Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  { Must have a master-name property.}
  Result := (GetAttributeNode(XpsMasterName) <> nil);
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsMasterName]);
end;

{== TXpFOTitle =======================================================}
constructor TXpFOTitle.Create;
begin
  inherited;
  FormatObjectType := FO_TITLE;
end;
{=====================================================================}

{== TXpFOBasicLink ===================================================}
constructor TXpFOBasicLink.Create;
begin
  inherited;
  FormatObjectType := FO_BASIC_LINK;
  FIsExternal := False;
end;
{--------}
function TXpFOBasicLink.Valid(oContext : TXpXSLContext;
                          var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);

  { Must have an internal-destination or external-destination
    property.}
  Result := (GetAttributeNode(XpsExternalDestination) <> nil);
  if (not Result) then begin
    Result := (GetAttributeNode(XpsInternalDestination) <> nil);
    if (not Result) then
      ErrMsg := Format(sXSLFOMissingProperty, [NodeName,
                                               XpsExternalDestination + ' or ' +
                                               XpsInternalDestination]);
  end;
end;
{=====================================================================}

{== TXpFOBlock =======================================================}
constructor TXpFOBlock.Create;
begin
  inherited;
  FormatObjectType := FO_BLOCK;
end;
{--------}
function TXpFOBlock.Valid(oContext : TXpXSLContext;
                      var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Nothing required.}
  Result := True;
end;
{=====================================================================}

{== TXpFOExternalGraphic =============================================}
constructor TXpFOExternalGraphic.Create;
begin
  inherited;
  FormatObjectType := FO_EXTERNAL_GRAPHIC;
end;
{--------}
function TXpFOExternalGraphic.Valid(oContext : TXpXSLContext;
                                var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Must have a Src property.}
  Result := (GetAttributeNode(XpsSrc) <> nil);
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingProperty, [NodeName, XpsSrc]);
end;
{=====================================================================}

{== TXpFOInline ======================================================}
constructor TXpFOInline.Create;
begin
  inherited;
  FormatObjectType := FO_INLINE;
end;
{--------}
function TXpFOInline.Valid(oContext : TXpXSLContext;
                       var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Nothing required.}
  Result := True;
end;
{=====================================================================}

{== TXpFOLeader ======================================================}
constructor TXpFOLeader.Create;
begin
  inherited;
  FormatObjectType := FO_LEADER;
end;
{--------}
function TXpFOLeader.Valid(oContext : TXpXSLContext;
                       var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Nothing required.}
  Result := True;
end;
{=====================================================================}

{== TXpFOListBlock ===================================================}
constructor TXpFOListBlock.Create;
begin
  inherited;
  FormatObjectType := FO_LIST_BLOCK;
end;
{--------}
function TXpFOListBlock.Valid(oContext : TXpXSLContext;
                          var ErrMsg   : string) : Boolean;
var
  HasGoodKids : Boolean;
  i           : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  Result := False;
  HasGoodKids := False;
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOListItem) then begin
      HasGoodKids := True;
      Result := TXpFOListItem(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end;
  end;
  if (not HasGoodKids) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                          XpsFOListItem]);
end;
{=====================================================================}

{== TXpFOListItem ====================================================}
constructor TXpFOListItem.Create;
begin
  inherited;
  FormatObjectType := FO_LIST_ITEM;
end;
{--------}
function TXpFOListItem.Valid(oContext : TXpXSLContext;
                         var ErrMsg   : string) : Boolean;
var
  HasLabel : Boolean;
  HasBody  : Boolean;
  i        : Integer;
begin
  inherited Valid(oContext, ErrMsg);

  Result := False;
  HasLabel := False;
  HasBody := False;

  { Must have 1 list-item-label and 1 list-item-body children.}

  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOListItemLabel) then begin
      HasLabel := True;
      Result := TXpFOListItemLabel(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end else if (ChildNodes.Item(i).NodeName = XpsFOListItemBody) then begin
      HasBody := True;
      Result := TXpFOListItemBody(ChildNodes.Item(i)).Valid(oContext, ErrMsg);
      if (not Result) then
        Break;
    end else begin
      ErrMsg := format(sXSLFOInvalidChild, [ChildNodes.Item(i).NodeName,
                                            NodeName]);
      Result := False;
      Break;
    end;
  end;

  if (Result) then
    if (not HasLabel) then begin
      ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                            XpsFOListItemLabel]);
      Result := False;
    end else if (not HasBody) then begin
      ErrMsg := Format(sXSLFOMissingChild, [NodeName,
                                            XpsFOListItemBody]);
      Result := False;
    end;
end;
{=====================================================================}

{== TXpFOListItemBody ================================================}
constructor TXpFOListItemBody.Create;
begin
  inherited;
  FormatObjectType := FO_LIST_ITEM_BODY;
end;
{--------}
function TXpFOListItemBody.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
var
  i : Integer;
begin
  inherited Valid(oContext, ErrMsg);
  Result := False;

  { Must have 1 or more block children.}
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOBlock) then begin
      Result := True;
      Break;
    end;
  end;
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFOBlock]);
end;
{=====================================================================}

{== TXpFOListItemLabel ===============================================}
constructor TXpFOListItemLabel.Create;
begin
  inherited;
  FormatObjectType := FO_LIST_ITEM_LABEL;
end;
{--------}
function TXpFOListItemLabel.Valid(oContext : TXpXSLContext;
                              var ErrMsg : string) : Boolean;
var
  i : Integer;
begin
  inherited Valid(oContext, ErrMsg);
  Result := False;

  { Must have 1 or more block children.}
  for i := 0 to (ChildNodes.Length - 1) do begin
    if (ChildNodes.Item(i).NodeName = XpsFOBlock) then begin
      Result := True;
      Break;
    end;
  end;
  if (not Result) then
    ErrMsg := Format(sXSLFOMissingChild, [NodeName, XpsFOBlock]);
end;
{=====================================================================}

{== TXpFOPageNumber ==================================================}
constructor TXpFOPageNumber.Create;
begin
  inherited;
  FormatObjectType := FO_PAGE_NUMBER;
end;
{--------}
function TXpFOPageNumber.Valid(oContext : TXpXSLContext;
                           var ErrMsg : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Nothing required.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTable =======================================================}
constructor TXpFOTable.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE;
end;
{--------}
function TXpFOTable.Valid(oContext : TXpXSLContext;
                      var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableBody ===================================================}
constructor TXpFOTableBody.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_BODY;
end;
{--------}
function TXpFOTableBody.Valid(oContext : TXpXSLContext;
                          var ErrMsg : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableCell ===================================================}
constructor TXpFOTableCell.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_CELL;
end;
{--------}
function TXpFOTableCell.Valid(oContext : TXpXSLContext;
                          var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableColumn =================================================}
constructor TXpFOTableColumn.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_COLUMN;
end;
{--------}
function TXpFOTableColumn.Valid(oContext : TXpXSLContext;
                            var ErrMsg   : string) : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableRow ====================================================}
constructor TXpFOTableRow.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_ROW;
end;
{--------}
function TXpFOTableRow.Valid(oContext : TXpXSLContext;
                         var ErrMsg   : string)
                                      : Boolean;
begin
  inherited Valid(oContext, ErrMsg);
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOInlineContainer =============================================}
constructor TXpFOInlineContainer.Create;
begin
  inherited;
  FormatObjectType := FO_INLINE_CONTAINER;
end;
{--------}
function TXpFOInlineContainer.Valid(oContext : TXpXSLContext;
                                var ErrMsg   : string)
                                             : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOInstreamForeignObject =======================================}
constructor TXpFOInstreamForeignObject.Create;
begin
  inherited;
  FormatObjectType := FO_INSTREAM_FOREIGN_OBJECT;
end;
{--------}
function TXpFOInstreamForeignObject.Valid(oContext : TXpXSLContext;
                                      var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMarker ======================================================}
constructor TXpFOMarker.Create;
begin
  inherited;
  FormatObjectType := FO_MARKER;
end;
{--------}
function TXpFOMarker.Valid(oContext : TXpXSLContext;
                       var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMultiCase ===================================================}
constructor TXpFOMultiCase.Create;
begin
  inherited;
  FormatObjectType := FO_MULTI_CASE;
end;
{--------}
function TXpFOMultiCase.Valid(oContext : TXpXSLContext;
                          var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMultiProperties =============================================}
constructor TXpFOMultiProperties.Create;
begin
  inherited;
  FormatObjectType := FO_MULTI_PROPERTIES;
end;
{--------}
function TXpFOMultiProperties.Valid(oContext : TXpXSLContext;
                                var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMultiPropertySet ============================================}
constructor TXpFOMultiPropertySet.Create;
begin
  inherited;
  FormatObjectType := FO_MULTI_PROPERTY_SET;
end;
{--------}
function TXpFOMultiPropertySet.Valid(oContext : TXpXSLContext;
                                 var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMultiSwitch =================================================}
constructor TXpFOMultiSwitch.Create;
begin
  inherited;
  FormatObjectType := FO_MULTI_SWITCH;
end;
{--------}
function TXpFOMultiSwitch.Valid(oContext : TXpXSLContext;
                            var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOMultiToggle =================================================}
constructor TXpFOMultiToggle.Create;
begin
  inherited;
  FormatObjectType := FO_MULTI_TOGGLE;
end;
{--------}
function TXpFOMultiToggle.Valid(oContext : TXpXSLContext;
                            var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOBidiOverride ================================================}
constructor TXpFOBidiOverride.Create;
begin
  inherited;
  FormatObjectType := FO_BIDI_OVERRIDE;
end;
{--------}
function TXpFOBidiOverride.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOBlockContainer ==============================================}
constructor TXpFOBlockContainer.Create;
begin
  inherited;
  FormatObjectType := FO_BLOCK_CONTAINER;
end;
{--------}
function TXpFOBlockContainer.Valid(oContext : TXpXSLContext;
                               var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOCharacter ===================================================}
constructor TXpFOCharacter.Create;
begin
  inherited;
  FormatObjectType := FO_CHARACTER;
end;
{--------}
function TXpFOCharacter.Valid(oContext : TXpXSLContext;
                          var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOColorProfile ================================================}
constructor TXpFOColorProfile.Create;
begin
  inherited;
  FormatObjectType := FO_COLOR_PROFILE;
end;
{--------}
function TXpFOColorProfile.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFODeclarations ================================================}
constructor TXpFODeclarations.Create;
begin
  inherited;
  FormatObjectType := FO_DECLARATIONS;
end;
{--------}
function TXpFODeclarations.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOFloat =======================================================}
constructor TXpFOFloat.Create;
begin
  inherited;
  FormatObjectType := FO_FLOAT;
end;
{--------}
function TXpFOFloat.Valid(oContext : TXpXSLContext;
                      var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOFootnote ====================================================}
constructor TXpFOFootnote.Create;
begin
  inherited;
  FormatObjectType := FO_FOOTNOTE;
end;
{--------}
function TXpFOFootnote.Valid(oContext : TXpXSLContext;
                         var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOFootnoteBody ================================================}
constructor TXpFOFootnoteBody.Create;
begin
  inherited;
  FormatObjectType := FO_FOOTNOTE_BODY;
end;
{--------}
function TXpFOFootnoteBody.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOInitialPropertySet ==========================================}
constructor TXpFOInitialPropertySet.Create;
begin
  inherited;
  FormatObjectType := FO_INITIAL_PROPERTY_SET;
end;
{--------}
function TXpFOInitialPropertySet.Valid(oContext : TXpXSLContext;
                                   var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOPageNumberCitation ==========================================}
constructor TXpFOPageNumberCitation.Create;
begin
  inherited;
  FormatObjectType := FO_PAGE_NUMBER_CITATION;
end;
{--------}
function TXpFOPageNumberCitation.Valid(oContext : TXpXSLContext;
                                   var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableAndCaption =============================================}
constructor TXpFOTableAndCaption.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_AND_CAPTION;
end;
{--------}
function TXpFOTableAndCaption.Valid(oContext : TXpXSLContext;
                                var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{== TXpFOTableCaption ================================================}
constructor TXpFOTableCaption.Create;
begin
  inherited;
  FormatObjectType := FO_TABLE_CAPTION;
end;
{--------}
function TXpFOTableCaption.Valid(oContext : TXpXSLContext;
                             var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableFooter =================================================}
constructor TXpFOTableFooter.Create;
begin
  inherited;
  FormatObjectType := FO_FOOTER;
end;
{--------}
function TXpFOTableFooter.Valid(oContext : TXpXSLContext;
                            var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOTableHeader =================================================}
constructor TXpFOTableHeader.Create;
begin
  inherited;
  FormatObjectType := FO_HEADER;
end;
{--------}
function TXpFOTableHeader.Valid(oContext : TXpXSLContext;
                            var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}

{== TXpFOWrapper =====================================================}
constructor TXpFOWrapper.Create;
begin
  inherited;
  FormatObjectType := FO_WRAPPER;
end;
{--------}
function TXpFOWrapper.Valid(oContext : TXpXSLContext;
                        var ErrMsg   : string) : Boolean;
begin
  { Not supported at this time.}
  Result := True;
end;
{=====================================================================}
procedure TXpFoProperty.prSetName(const aName : DOMString);
begin
  noNodeName := aName;
  FIsInheritable := ((NodeName <> XpsBackgroundColor) and
                     (NodeName <> XpsBreakAfter) and
                     (NodeName <> XpsBreakBefore) and
                     (NodeName <> XpsMarginLeft) and
                     (NodeName <> XpsMarginRight) and
                     (NodeName <> XpsSpaceAfterMaximum) and
                     (NodeName <> XpsSpaceAfterMinimum) and
                     (NodeName <> XpsSpaceAfterOptimum) and
                     (NodeName <> XpsSpaceBeforeMaximum) and
                     (NodeName <> XpsSpaceBeforeMinimum) and
                     (NodeName <> XpsSpaceBeforeOptimum));
end;

end.
