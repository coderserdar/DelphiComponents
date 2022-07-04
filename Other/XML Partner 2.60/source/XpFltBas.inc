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
{* XMLPartner Pro: XpFltBas.INC                          *}
{*********************************************************}
{* XMLPartner Pro: Base Filter Implementation            *}
{*********************************************************}

{!!.57 numerous changes}

{$I XpDefine.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UsingCLX}
  QControls,
  QGraphics,
  XpQXSLFO,
{$ELSE}
  Controls,
  Graphics,
  XpvXSLFO,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  Classes,
  Sysutils,
  XpDOM,
  XpBase,
  XpHash;

const
  MILLIMETERSPERINCH = 25.4;
  CENTIMETERSPERINCH = 2.54;
  POINTSPERINCH = 72;
  POINTSPERCENTIMETER = POINTSPERINCH / CENTIMETERSPERINCH;
  POINTSPERMILLIMETER = POINTSPERINCH / MILLIMETERSPERINCH;
  POINTSPERPICA = 12;
  TWIPSPERINCH = 1440;
  TWIPSPERPOINT = 20;
  TWIPSPER_RTFPOINT = TWIPSPERPOINT div 2;
  TWIPSPERCENTIMETER = TWIPSPERINCH / CENTIMETERSPERINCH;
  STANDARDPAGEWIDTH = 816;

type
{== Formatting units =================================================}
  EFormatUnits = (fuDefault,     fuEmpty,        fuPoints, fuPercent,
                  fuPixels,      fuTwips,        fuInches, fuCentimeters,
                  fuMillimeters, fuFontFraction, fuPica);
{=====================================================================}

{== TXpFoPropertyGroupText ===========================================}
  TXpFoPropertyGroupText = class(TXpFOPropertyGroup)
  public
    constructor Create;
    procedure Reset;
  end;
{=====================================================================}

{== TXpTableCellArea =================================================}
  TXpCellBorders = record
    TW, RW, BW, LW: Integer;
  end;
  TXpTableCellArea = class(TObject)
  public
    HeightSpc  : Integer; {specified}
    HeightClc  : Integer; {calculated}
    Left    : Integer;
    Top     : Integer;
    Width   : Integer;
    NoLeft, NoRight,
    NoTop, NoBottom: Boolean;
    Column : Integer;
    Borders, Padding: TXpCellBorders;
  end;
{=====================================================================}

{== TXpDrawArea ======================================================}

  {Forward Declaration}
  TXpTableArea = class;

  TXpDrawArea = class(TObject)
  public
    Bottom : Integer;
    Left   : Integer;
    Right  : Integer;
    Table  : TXpTableArea;
    Top    : Integer;
    XPos   : Integer;
    YIncr  : Integer;
    YPos   : Integer;

    constructor Create;
    destructor Destroy; override;
  end;
{=====================================================================}

{== TXpTableRowArea ==================================================}
  TXpTableRowArea = class(TObject)
  private
    FRowElement : TXpElement;
    function GetCell(Index: Integer): TXpTableCellArea;
  public
    Height     : Integer;
    SelectCell : Integer;
    TableCells : TList;
    Top        : Integer;

    constructor Create(AElement: TXpElement);
    destructor Destroy; override;

    function CurrentCell: TXpTableCellArea;
    function FirstAvailColumn(C: Integer): Integer;
    procedure AddOrdered(C: TXpTableCellArea);
    property Cell[Index: Integer]: TXpTableCellArea
      read GetCell;
    property RowElement: TXpElement read FRowElement;
  end;
{=====================================================================}

{== TXpTableArea =====================================================}
  TXpTableArea = class(TObject)
  private
    function GetCurrentRow : TXpTableRowArea;
    function GetRow(Index: Integer): TXpTableRowArea;
  public
    SelectRow : Integer;
    TableRows : TList;
    Width     : Integer;
    constructor Create;
    destructor Destroy; override;

    property CurrentRow : TXpTableRowArea
      read GetCurrentRow;
    property Row[Index: Integer]: TXpTableRowArea read GetRow;
  end;
{=====================================================================}

{== RPageInfo ========================================================}
  RPageInfo = packed record
    PageHeight : Integer;
    PageWidth  : Integer;
  end;
{=====================================================================}

{== TXpFilterBase ====================================================}
  TXpFilterBase = class(TCustomControl)
    { Private declarations }
    function NormalizeDecimalSeparator(const sVal : DOMString)
                                                  : DOMString;
  protected
    { Protected declarations }
    FBlockLevel : Integer;
    FCurAttr : TXpElement;
    FCurCanvas : TCanvas;
    FCurElement : TXpElement;
    FCurFormatObject : TXpFormattingObject;
    FCurPageMaster : TXpFOSimplePageMaster;
    FCurPageSequence : TXpFOPageSequence;
    FCurPageSeqMaster : TXpFOPageSeqMaster;
    FDisplayName : string;
    FLastBlockElem : TXpElement;
    FLastInlineElem : TXpElement;
    FLastTDElem : TXpElement;
    FLastTRElem : TXpElement;
    FLastTABLEANDCAPTIONElem : TXpElement;
    FLastTABLEElem : TXpElement;
    FLastTBODYElem : TXpElement;
    FLastTHEADElem : TXpElement;
    FLastTFOOTElem : TXpElement;
    FOutRegionAfter : TXpObjModel;
    FOutRegionAfterRoot : string;
    FOutRegionBefore : TXpObjModel;
    FOutRegionBeforeRoot : string;
    FOutRegionBody : TXpObjModel;
    FOutRegionBodyRoot : string;
    FOutRegionCurrent : TXpObjModel;
    FOutRegionEnd : TXpObjModel;
    FOutRegionEndRoot : string;
    FOutRegionStart : TXpObjModel;
    FOutRegionStartRoot : string;
    FOutTitle : TXpObjModel;
    FOutTitleRoot : string;
    FPageCount      : Integer;
    FPageCurrent    : Integer;
    FPageSequenceCount : Integer;
    FResultTree : TXpObjModel;
    FHPixelsPerInch : Integer;
    FVPixelsPerInch : Integer;
    function CreateElement(const sTagName : string) : TXpElement; virtual;
    function EvalProp(sVal      : string;
                      oUnits    : EFormatUnits
                                  {$IFNDEF VER100}
                                  = fuDefault
                                  {$ENDIF}
                                  ;
                      wStandard : Integer
                                  {$IFNDEF VER100}
                                  = 1
                                  {$ENDIF}
                                  ) : string; virtual;
    function EvalPropAsNum(const sVal      : string;
                                 oUnits    : EFormatUnits
                                             {$IFNDEF VER100}
                                             = fuDefault
                                             {$ENDIF};
                                 wStandard : Integer
                                             {$IFNDEF VER100}
                                             = 1
                                             {$ENDIF})
                                           : Integer; virtual;
    procedure fbCopyAttrsToCurrent(const aSourceElem : TXpElement);
    procedure fbAddNodesToPage(oPage : TXpElement; oNode : TXpNode);
    procedure fbFreeOutRegions;
    function fbGetOutputEncoding : TXpCharEncoding;
    function fbGetOutRegionAfter : TXpObjModel;
    function fbGetOutRegionBefore : TXpObjModel;
    function fbGetOutRegionBody : TXpObjModel;
    function fbGetOutRegionEnd : TXpObjModel;
    function fbGetOutRegionStart : TXpObjModel;
    function fbGetOutTitle : TXpObjModel;
    function fbGetPassword : string;
    function fbGetTreeRoot : TXpElement;
    function fbGetUserName : string;
    function fbGetVersion : string;
    function fbGetWriteUTF8Sig : Boolean;
    function fbHasOutRegionAfter : Boolean;
    function fbHasOutRegionBefore : Boolean;
    function fbHasOutRegionBody : Boolean;
    function fbHasOutRegionEnd : Boolean;
    function fbHasOutRegionStart : Boolean;
    function fbHasOutTitle : Boolean;
    function fbIsResultFO : Boolean;
    function foProcessFOChildren(aFOElem : TXpElement) : Boolean;
    function fbProcessFONode(aFONode : TXpNode) : Boolean;
    function fbSelectPageMaster(aPageRefIdx,
                                aPageNum    : Integer)
                                            : Integer;
    { Sets the appropriate SimplePageMaster based on the current
      PageSequenceMaster's PageRef index and current page number. This
      assumes the current page sequence master (FCurPageSeqMaster) has
      been assigned before this function is called. This function
      returns the maximum repeats for a repeatable page reference or
      -1 for a non-repeatable page reference.}
    procedure fbSetOutputEncoding(aEncoding : TXpCharEncoding);
    procedure fbSetVersion(const Value : string);
    procedure InitRegion(aRegion      : TXpObjModel;
                   const sRootTagName : string); virtual;

    procedure fbSetPassword(const aPassword : string);
    procedure fbSetUserName(const aUserName : string);
    procedure fbSetWriteUTF8Sig(const aValue : Boolean);
    procedure SetParentToCurrent; virtual;

    property OutputEncoding : TXpCharEncoding
      read fbGetOutputEncoding
      write fbSetOutputEncoding
      default ceISO88591;

    property ResultIsFO : Boolean
      read fbIsResultFO;

    property ResultTreeRoot : TXpElement
      read fbGetTreeRoot;

    property WriteUTF8Signature : Boolean
      read fbGetWriteUTF8Sig
      write fbSetWriteUTF8Sig
      default False;
      
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

    function RenderFile(const aFileName : string) : Boolean; virtual;
    function RenderMemory(var Buffer; aSize : Integer) : Boolean;
                                                         virtual;
    function RenderStream(aStream : TStream) : Boolean; virtual;

    procedure Reset; virtual;
    function SaveToFile(const sFile : string) : Boolean; virtual;      {!!.57}
    function SaveToStream(aStream : TStream) : Boolean; virtual;
    procedure SetCurrentRegion(const aRegionName : string);

    procedure XslProcessorApplyStyleEnd(Sender : TObject); virtual;
    procedure XslProcessorApplyStyleStart(Sender : TObject); virtual;
    procedure XslProcessorAttribute(oOwner : TObject;
                                    sName,
                                    sValue : DOMString); virtual;
    procedure XslProcessorCDATASection(oOwner   : TObject;
                                       sValue   : DOMString;
                                       bReplace : Boolean); virtual;
    procedure XslProcessorComment(oOwner : TObject;
                                  sValue : DOMString); virtual;
    procedure XslProcessorElementEnd(oOwner : TObject;
                                     sValue : DOMString); virtual;
    procedure XslProcessorElementStart(oOwner : TObject;
                                       sValue : DOMString); virtual;
    procedure XslProcessorFormatObject(Sender        : TObject;
                                       aFormatObject : TXpFormattingObject); virtual;
    procedure XslProcessorFormatObjectEnd(Sender : TObject); virtual;
    procedure XslProcessorProcessingInstruction(oOwner : TObject;
                                                sName,         
                                                sValue : DOMString); virtual;
    procedure XslProcessorQueryFilter(Sender : TObject;
                                  var sName  : string); virtual;
    procedure XslProcessorText(oOwner : TObject;
                               sValue : DOMString); virtual;

    procedure XslProcessorTextNode(oOwner : TObject;
                                   oNode : TXpText); virtual;

    property CurrentElement : TXpElement
      read FCurElement;

    property DisplayName : string
      read FDisplayName;

    property HPixelsPerInch : Integer
      read FHPixelsPerInch
      write FHPixelsPerInch;

    property VPixelsPerInch : Integer
      read FVPixelsPerInch
      write FVPixelsPerInch;

    property HasOutRegionAfter : Boolean
      read fbHasOutRegionAfter;

    property HasOutRegionBefore : Boolean
      read fbHasOutRegionBefore;

    property HasOutRegionBody : Boolean
      read fbHasOutRegionBody;

    property HasOutRegionEnd : Boolean
      read fbHasOutRegionEnd;

    property HasOutRegionStart : Boolean
      read fbHasOutRegionStart;

    property HasOutTitle : Boolean
      read fbHasOutTitle;

    property OutRegionAfter : TXpObjModel
      read fbGetOutRegionAfter;

    property OutRegionBefore : TXpObjModel
      read fbGetOutRegionBefore;

    property OutRegionBody : TXpObjModel
      read fbGetOutRegionBody;

    property OutRegionEnd : TXpObjModel
      read fbGetOutRegionEnd;

    property OutRegionStart : TXpObjModel
      read fbGetOutRegionStart;

    property OutTitle : TXpObjModel
      read fbGetOutTitle;

  published
    property Password : string
      read fbGetPassword
      write fbSetPassword;

    property UserName : string
      read fbGetUserName
      write fbSetUserName;

    property Version : string
      read fbGetVersion
      write fbSetVersion
      stored False;
      
  end;

  TXpFilterClass = class of TXpFilterBase;
  
{=====================================================================}

implementation

uses
{$IFDEF XPTrialRun}
{$IFDEF UsingCLX}
  XpQTrial,
{$ELSE}
  XpTrial,
{$ENDIF}
{$ENDIF}
  XpExcept,
  XpXSLCon,
{$IFDEF UsingCLX}
  XpQFOHsh;
{$ELSE}
  XpvFOHsh;
{$ENDIF}

{$IFNDEF Delphi6}
function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;
{$ENDIF}

{== TXpXslFilterBase =================================================}
constructor TXpFilterBase.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);
  {$IFDEF XPTrialRun}
  _CC_;
  _VC_;
  {$ENDIF}
  {!!.57 redundant:
  FCurAttr := nil;
  FCurElement := nil;
  FCurFormatObject := nil;
  FCurPageMaster := nil;
  FCurPageSequence := nil;
  FCurPageSeqMaster := nil;
  FOutRegionAfter := nil;
  FOutRegionBefore := nil;
  FOutRegionBody := nil;
  FOutRegionCurrent := nil;
  FOutRegionEnd := nil;
  FOutRegionStart := nil;
  FDisplayName := '';
  FPageSequenceCount := 0;
  }
  FHPixelsPerInch := 96;
  FVPixelsPerInch := 96;
  FResultTree := TXpObjModel.Create(nil);
  FResultTree.OutCharSet := ceISO88591;
  FResultTree.WriteUTF8Signature := False;
end;
{--------}
destructor TXpFilterBase.Destroy;
begin
  fbFreeOutRegions;
  FResultTree.Free;
  inherited Destroy;
end;
{--------}
function TXpFilterBase.CreateElement(const sTagName : string)
                                                    : TXpElement;
begin
  Result := nil;
  if (FCurElement <> nil) then begin
    Result := FCurElement.CreateChildElement(sTagName);
    FResultTree.Document.ForceOwnerDocument(Result);
    FCurElement := Result;
    FCurAttr := FCurElement;
  end;
end;
{--------}
function TXpFilterBase.EvalProp(sVal      : string;
                                oUnits    : EFormatUnits;
                                wStandard : Integer) : string;
var
  oCurUnits : EFormatUnits;
  nPoints   : Single;
  sTmp      : string;
begin
  sVal := Trim(sVal);

  if (sVal = 'auto') then
    sVal := '100%'
  else if (sVal = '*') then
    sVal := '50%'
  else if (sVal = 'none') then
    sVal := '';
  Result := sVal;

  sTmp := LowerCase(Copy(sVal, Length(sVal) - 1, 2));
  if (sVal = '') then
    oCurUnits := fuEmpty
  else if (sVal[Length(sVal)] = '%') then
    oCurUnits := fuPercent
  else if (sTmp = 'pt') then
    oCurUnits := fuPoints
  else if (sTmp = 'cm') then
    oCurUnits := fuCentimeters
  else if (sTmp = 'mm') then
    oCurUnits := fuMillimeters
  else if (sTmp = 'px') then
    oCurUnits := fuPixels
  else if (sTmp = 'in') then
    oCurUnits := fuInches
  else if (sTmp = 'tw') then
    oCurUnits := fuTwips
  else if (sTmp = 'pc') then
    oCurUnits := fuPica
  else if (sTmp = 'em') or
          (sTmp = 'ex') then
    oCurUnits := fuFontFraction
  else
    oCurUnits := fuPixels;

  if (oUnits = fuDefault) or
     (oCurUnits = fuEmpty) or
     (oCurUnits = oUnits) then
    Exit;

  sVal := NormalizeDecimalSeparator(sVal);

  { Convert to standard unit...points }
  nPoints := 0;
  case oCurUnits of
    fuPoints :
      nPoints := StrToFloatDef(Copy(sVal, 1, Length(sVal) - 2), wStandard); {!!.57}
    fuPixels :
      nPoints := (StrToIntDef(sVal, 0) * POINTSPERINCH) /
                 FHPixelsPerInch;
    fuPercent :
      nPoints := (((StrToIntDef(Copy(sVal, 1, Length(sVal) - 1), 0) *
                    wStandard) / 100) * POINTSPERINCH) / FHPixelsPerInch;
    fuTwips :
      nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) /
                 TWIPSPERPOINT;
    fuInches :
      nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) *
                 POINTSPERINCH;
    fuCentimeters :
      nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) *
                 POINTSPERCENTIMETER;
    fuMillimeters :
      nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) *
                 POINTSPERMILLIMETER;
    fuPica :
      nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) *
                 POINTSPERPICA;
    fuFontFraction :
      begin
        if (FCurCanvas <> nil) then
          nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) *
                     FCurCanvas.Font.Size
        else
          nPoints := StrToFloat(Copy(sVal, 1, Length(sVal) - 2)) * 10;
      end;
  end;

  { Convert back to requested unit }
  case oUnits of
    fuPoints :
      Result := FloatToStrF(nPoints, ffFixed, 7, 0) + 'pt';
    fuPixels :
      Result := FloatToStrF((nPoints * FHPixelsPerInch) /
                            POINTSPERINCH, ffFixed, 7, 0);
    fuPercent :
      Result := FloatToStrF((nPoints * 100) /
                            wStandard, ffFixed, 7, 0) + '%';
    fuTwips :
      Result := FloatToStrF(nPoints * TWIPSPERPOINT, ffFixed, 7, 0) +
                'tw';
    fuInches :
      Result := FloatToStrF(nPoints / POINTSPERINCH, ffFixed, 7, 0) +
                'in';
    fuCentimeters :
      Result := FloatToStrF(nPoints /
                            POINTSPERCENTIMETER, ffFixed, 7, 0) + 'cm';
    fuMillimeters :
      Result := FloatToStrF(nPoints /
                            POINTSPERMILLIMETER, ffFixed, 7, 0) + 'mm';
    fuPica :
      Result := FloatToStrF(nPoints / POINTSPERPICA, ffFixed, 7, 0) +
                'pc';
    fuFontFraction :
      begin
        if (FCurCanvas <> nil) then
          Result := FloatToStrF(nPoints /
                                FCurCanvas.Font.Size, ffFixed, 7, 0) +
                    'em'
        else
          Result := FloatToStrF(nPoints / 10, ffFixed, 7, 0) + 'em'
      end;
  end;
end;
{--------}
function TXpFilterBase.EvalPropAsNum(const sVal      : string;
                                           oUnits    : EFormatUnits;
                                           wStandard : Integer) : Integer;
var
  sTerm : string;
begin
  sTerm := EvalProp(sVal, oUnits, wStandard);
  case oUnits of
    fuPoints,
    fuTwips,
    fuPica,
    fuInches,
    fuCentimeters,
    fuMillimeters :
      begin
        Delete(sTerm, Length(sTerm), 1);
        Delete(sTerm, Length(sTerm), 1);
        if Pos('.', sTerm) > 0 then
          Delete(sTerm, Pos('.', sTerm), Length(sTerm));
        Result := StrToIntDef(sTerm, 0);
      end;
    fuPercent :
      begin
        Delete(sTerm, Length(sTerm), 1);
        Result := StrToIntDef(sTerm, 0);
      end;
  else
    Result := StrToIntDef(sTerm, wStandard);                           {!!.57}
  end;
end;
{--------}
procedure TXpFilterBase.fbAddNodesToPage(oPage : TXpElement;
                                          oNode : TXpNode);
var
  oNew,
  oWalk,
  oOldNew : TXpElement;
  oTmp,
  oTmp2,
  oRef,
  oClone  : TXpNode;
begin
  {
  1. Clone parent node (without children)
  2. Copy all previous siblings with children
  3. Copy current node?
  4. If parent is not oParent, goto 1
  }
  oWalk := TXpElement(oNode.ParentNode);
  oRef := oNode;
  oOldNew := nil;
  while (True) do begin
    { Clone parent node }
    if (oWalk.ParentNode.NodeType = DOCUMENT_NODE) then
      oNew := oPage
    else begin
      oNew := TXpElement(oWalk.CloneNode(False));
    end;

    { Copy all previous siblings }
    oTmp := oWalk.FirstChild;
    while (oTmp <> oRef) do begin
      oClone := oTmp.CloneNode(True);
      oNew.ForceOwnerDocument(oClone);
      oNew.AppendChild(oClone);
      oClone.Release;
      oTmp2 := oTmp;
      oTmp := oTmp.NextSibling;
      oTmp2.ParentNode.RemoveChild(oTmp2);
      oTmp2.Release;
    end;

    if (oOldNew <> nil) then begin
      oNew.ForceOwnerDocument(oOldNew);
      oNew.AppendChild(oOldNew);
      oOldNew.Release;
    end;

    if (oNew = oPage) then
      Break;

    oOldNew := oNew;
    oWalk := TXpElement(oWalk.ParentNode);
    oRef := TXpElement(oRef.ParentNode);
  end;
end;
{--------}
procedure TXpFilterBase.fbCopyAttrsToCurrent(const aSourceElem : TXpElement);
var
  TempAttr : TXpAttribute;
  i        : Integer;
begin
  if (aSourceElem.Attributes <> nil) then begin
    for i := 0 to (aSourceElem.Attributes.Length - 1) do begin
      TempAttr := TXpAttribute(aSourceElem.Attributes.Item(i));
      FCurElement.SetAttribute(TempAttr.Name, TempAttr.Value);
    end;
  end;
end;
{--------}
procedure TXpFilterBase.fbFreeOutRegions;
begin
  FOutRegionAfter.Free;
  FOutRegionAfter := nil;

  FOutRegionBefore.Free;
  FOutRegionBefore := nil;

  FOutRegionBody.Free;
  FOutRegionBody := nil;

  FOutRegionEnd.Free;
  FOutRegionEnd := nil;

  FOutRegionStart.Free;
  FOutRegionStart := nil;

  FOutTitle.Free;
  FOutTitle := nil;
end;
{--------}
function TXpFilterBase.fbGetOutputEncoding : TXpCharEncoding;
begin
  Result := FResultTree.OutCharSet;
end;
{--------}
function TXpFilterBase.fbGetOutRegionAfter : TXpObjModel;
begin
  if (FOutRegionAfter = nil) then begin
    FOutRegionAfter := TXpObjModel.Create(nil);
    InitRegion(FOutRegionAfter, FOutRegionAfterRoot);
  end;
  Result := FOutRegionAfter;
end;
{--------}
function TXpFilterBase.fbGetOutRegionBefore : TXpObjModel;
begin
  if (FOutRegionBefore = nil) then begin
    FOutRegionBefore := TXpObjModel.Create(nil);
    InitRegion(FOutRegionBefore, FOutRegionBeforeRoot);
  end;
  Result := FOutRegionBefore;
end;
{--------}
function TXpFilterBase.fbGetOutRegionBody : TXpObjModel;
begin
  if (FOutRegionBody = nil) then begin
    FOutRegionBody := TXpObjModel.Create(nil);
    InitRegion(FOutRegionBody, FOutRegionBodyRoot);
  end;
  Result := FOutRegionBody;
end;
{--------}
function TXpFilterBase.fbGetOutRegionEnd : TXpObjModel;
begin
  if (FOutRegionEnd = nil) then begin
    FOutRegionEnd := TXpObjModel.Create(nil);
    InitRegion(FOutRegionEnd, FOutRegionEndRoot);
  end;
  Result := FOutRegionEnd;
end;
{--------}
function TXpFilterBase.fbGetOutRegionStart : TXpObjModel;
begin
  if (FOutRegionStart = nil) then begin
    FOutRegionStart := TXpObjModel.Create(nil);
    InitRegion(FOutRegionStart, FOutRegionStartRoot);
  end;
  Result := FOutRegionStart;
end;
{--------}
function TXpFilterBase.fbGetOutTitle : TXpObjModel;
begin
  if (FOutTitle = nil) then begin
    FOutTitle := TXpObjModel.Create(nil);
    InitRegion(FOutTitle, FOutTitleRoot);
  end;
  Result := FOutTitle;
end;
{--------}
function TXpFilterBase.fbGetPassword : string;
begin
  Result := FResultTree.Password;
end;
{--------}
function TXpFilterBase.fbGetTreeRoot : TXpElement;
begin
  Result := FResultTree.DocumentElement;
end;
{--------}
function TXpFilterBase.fbGetUserName : string;
begin
  Result := FResultTree.UserName;
end;
{--------}
function TXpFilterBase.fbGetVersion : string;
begin
  Result := Format('%5.4f', [XpVersionNumber / 10000.0]);
end;
{--------}
function TXpFilterBase.fbGetWriteUTF8Sig : Boolean;
begin
  Result := FResultTree.WriteUTF8Signature;
end;
{--------}
function TXpFilterBase.fbHasOutRegionAfter : Boolean;
begin
  Result := (FOutRegionAfter <> nil);
end;
{--------}
function TXpFilterBase.fbHasOutRegionBefore : Boolean;
begin
  Result := (FOutRegionBefore <> nil);
end;
{--------}
function TXpFilterBase.fbHasOutRegionBody : Boolean;
begin
  Result := (FOutRegionBody <> nil);
end;
{--------}
function TXpFilterBase.fbHasOutRegionEnd : Boolean;
begin
  Result := (FOutRegionEnd <> nil)
end;
{--------}
function TXpFilterBase.fbHasOutRegionStart : Boolean;
begin
  Result := (FOutRegionStart <> nil);
end;
{--------}
function TXpFilterBase.fbHasOutTitle : Boolean;
begin
  Result := (FOutTitle <> nil);
end;
{--------}
function TXpFilterBase.fbIsResultFO : Boolean;
begin
  Result := (ResultTreeRoot is TXpFORoot);
end;
{--------}
function TXpFilterBase.foProcessFOChildren(aFOElem : TXpElement) : Boolean;
var
  TmpNode : TXpNode;
  i       : Integer;
begin
  Result := True;
  for i := 0 to (aFOElem.ChildNodes.Length - 1) do begin
    TmpNode := aFOElem.ChildNodes.Item(i);
    Result := fbProcessFONode(TmpNode);
    if (not Result) then
      Break;
  end;
end;
{--------}
function TXpFilterBase.fbProcessFONode(aFONode : TXpNode) : Boolean;
begin
  Result := True;
  if (XpStartsWith(XpsFO, aFONode.NodeName)) then begin
    XslProcessorFormatObject(self, TXpFormattingObject(aFONode));
    Result := foProcessFOChildren(TXpFormattingObject(aFONode));
    XslProcessorFormatObjectEnd(self);
  end else if (aFONode is TXpText) then
    XslProcessorTextNode(self, aFONode as TXpText);
end;
{--------}
function TXpFilterBase.fbSelectPageMaster(aPageRefIdx, aPageNum : Integer)
                                                                : Integer;
var
  PageRef  : TXpFormattingObject;
  CondRef  : TXpFOPageRefCondition;
  PagePos  : string;
  i        : Integer;
begin
  if (FCurPageSeqMaster <> nil) then begin
    { We need to loop through each page reference as long as there
      is content in the current page sequence.}
    FCurPageMaster := FCurPageSeqMaster.GetPageMasterByPageRefIdx(aPageRefIdx);
    PageRef := TXpFormattingObject(FCurPageSeqMaster.ChildNodes.Item(aPageRefIdx));
    if (not (PageRef is TXpFOSinglePageRef)) then begin
      Result := FCurPageSeqMaster.GetMaxRepeatsByPageRefIdx(aPageRefIdx);
      if (PageRef.FormatObjectType = FO_REPEATABLE_PAGE_MASTER_ALTERNATIVES) then begin
        if (aPageNum = 1) then
          PagePos := 'first'
        else if (aPageNum = FPageCount) then
          PagePos := 'last'
        else
          PagePos := 'rest';

        { Search the conditional page references for a match. }
        for i := 0 to (TXpFOAlternatingPageRef(PageRef).ConditionalReferences.Length - 1) do begin
          CondRef := TXpFOPageRefCondition(TXpFOAlternatingPageRef(PageRef).ConditionalReferences.Item(i));
          if (CondRef.IsMatch(PagePos, False, (aPageNum mod 2 <> 0))) then begin
            FCurPageMaster :=
              TXpFOSimplePageMaster(FCurPageSeqMaster.GetPageMaster(CondRef.GetPropertyValue(XpsMasterName)));
            Break;
          end;
        end;
      end;
    end else
      Result := 1;
  end else begin
    FCurPageMaster := TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(FCurPageSequence.GetPropertyValue(XpsMasterName));
    if FCurPageMaster = nil then
      FCurPageMaster := TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(FCurPageSequence.GetPropertyValue('master-reference'));
    Result := -1;
  end;
end;
{--------}
procedure TXpFilterBase.fbSetOutputEncoding(aEncoding : TXpCharEncoding);
begin
  if aEncoding = ceUnknown then
    raise EXpException.Create(sInvalidCharEncoding)
  else
    FResultTree.OutCharSet := aEncoding;
end;
{--------}
procedure TXpFilterBase.fbSetPassword(const aPassword : string);
begin
  FResultTree.Password := aPassword;
end;
{--------}
procedure TXpFilterBase.fbSetUserName(const aUserName : string);
begin
  FResultTree.UserName := aUserName;
end;
{--------}
procedure TXpFilterBase.fbSetVersion(const Value : string);
begin
  {do nothing}
end;
{--------}
procedure TXpFilterBase.fbSetWriteUTF8Sig(const aValue : Boolean);
begin
  FResultTree.WriteUTF8Signature := aValue;
end;
{--------}
procedure TXpFilterBase.InitRegion(aRegion      : TXpObjModel;
                             const sRootTagName : string);
var
  oElem : TXpElement;
begin
  if (aRegion <> nil) then begin
    aRegion.ClearDocument;
    if (sRootTagName <> '') then begin
      oElem := aRegion.Document.CreateElement(sRootTagName);
      aRegion.Document.AppendChild(oElem);
      oElem.Release;
    end;
  end;
end;
{--------}
function TXpFilterBase.NormalizeDecimalSeparator(const sVal : DOMString)
                                                            : DOMString;
var
  ds,
  old : DOMChar;
  idx : Integer;
begin
  Result := sVal;
  ds := DOMChar(DecimalSeparator); //System Default
  if (ds = ',') then
    old := '.'
  else
    old := ',';
  for idx := 1 to Length(Result) do
    if (Result[idx] = old) then
      Result[idx] := ds;
end;
{--------}
function TXpFilterBase.RenderFile(const aFileName : string) : Boolean;
var
  FODoc : TXpObjModel;
begin
  FODoc := TXpObjModel.Create(nil);
  try
    Reset;
    FODoc.LoadDataSource(aFileName);
    Result := fbProcessFONode(FODoc.Document.DocumentElement);
  finally
    FODoc.Free;
  end;
end;
{--------}
function TXpFilterBase.RenderMemory(var Buffer;
                                        aSize : Integer)
                                              : Boolean;
var
  FODoc : TXpObjModel;
begin
  FODoc := TXpObjModel.Create(nil);
  try
    FODoc.LoadMemory(Buffer, aSize);
    Result := fbProcessFONode(FODoc.Document.DocumentElement);
  finally
    FODoc.Free;
  end;
end;
{--------}
function TXpFilterBase.RenderStream(aStream : TStream) : Boolean;
var
  FODoc : TXpObjModel;
begin
  FODoc := TXpObjModel.Create(nil);
  try
    FODoc.LoadStream(aStream);
    Result := fbProcessFONode(FODoc.Document.DocumentElement);
  finally
    FODoc.Free;
  end;
end;
{--------}
procedure TXpFilterBase.Reset;
begin
  FResultTree.ClearDocument;
  fbFreeOutRegions;
  FPageSequenceCount := 0;
  FCurPageMaster := nil
end;
{--------}
function TXpFilterBase.SaveToFile(const sFile : string) : Boolean;     {!!.57}
begin
  Result := False;
end;
{--------}
function TXpFilterBase.SaveToStream(aStream : TStream) : Boolean;
begin
  Result := False;
end;
{--------}
procedure TXpFilterBase.SetParentToCurrent;
begin
  if (FCurElement <> nil) then begin
    FCurElement := TXpElement(FCurElement.ParentNode);
    FCurAttr := FCurElement;
  end;
end;
{--------}
procedure TXpFilterBase.SetCurrentRegion(const aRegionName : string);
begin
  if (aRegionName = XpsFORegAfter) then
    FOutRegionCurrent := OutRegionAfter
  else if (aRegionName = XpsFORegBefore) then
    FOutRegionCurrent := OutRegionBefore
  else if ((aRegionName = XpsFORegBody) or
           (aRegionName = '')) then
    FOutRegionCurrent := OutRegionBody
  else if (aRegionName = XpsFORegEnd) then
    FOutRegionCurrent := OutRegionEnd
  else if (aRegionName = XpsFORegStart) then
    FOutRegionCurrent := OutRegionStart
  else if (aRegionName = XpsFORegTitle) then
    FOutRegionCurrent := OutTitle;
  FCurElement := FOutRegionCurrent.Document.DocumentElement;
end;
{--------}
procedure TXpFilterBase.XslProcessorApplyStyleEnd(Sender : TObject);
var
  ErrMsg : string;
begin
  { Validate the FO tree. }
  if (ResultIsFO) and
     (not TXpFORoot(ResultTreeRoot).Valid(nil, ErrMsg)) then
    raise TXpXSLFOException.Create(ErrMsg)
  else
    Exit;

  if (TXpFORoot(ResultTreeRoot).PageSequences <> nil) then begin
    FCurPageSequence :=
      TXpFOPageSequence(TXpFORoot(ResultTreeRoot).PageSequences.Item(0));
    FCurPageMaster :=
      TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(FCurPageSequence.GetPropertyValue(XpsMasterName));
  end;
end;
{--------}
procedure TXpFilterBase.XslProcessorApplyStyleStart(Sender : TObject);
begin
  Reset;
end;
{--------}
procedure TXpFilterBase.XslProcessorAttribute(oOwner : TObject;
                                              sName,          
                                              sValue : DOMString);
begin
//Do nothing
end;
{--------}
procedure TXpFilterBase.XslProcessorCDATASection(oOwner   : TObject;
                                                 sValue   : DOMString;
                                                 bReplace : Boolean);
begin
//Do nothing
end;
{--------}
procedure TXpFilterBase.XslProcessorComment(oOwner : TObject;
                                            sValue : DOMString);
begin
//Do nothing
end;
{--------}
procedure TXpFilterBase.XslProcessorElementEnd(oOwner : TObject;
                                               sValue : DOMString);
begin
  SetParentToCurrent;
end;
{--------}
procedure TXpFilterBase.XslProcessorElementStart(oOwner : TObject;
                                                 sValue : DOMString);
begin
  CreateElement(sValue);
end;
{--------}
procedure TXpFilterBase.XslProcessorFormatObject(Sender        : TObject;
                                                 aFormatObject : TXpFormattingObject);
var
  NewFO       : TXpFormattingObject;
  TempPropVal : string;
begin
  if (((FResultTree <> nil) and
       (aFormatObject.FormatObjectType <> FO_ROOT)) and
      (not ResultIsFO)) then
    raise TXpXSLFOException.Create(sXSLFORootMustBeRoot);

  NewFO := TXpFormattingObject(aFormatObject.CloneNode(False));
  FResultTree.Document.ForceOwnerDocument(NewFO);
  NewFO.FormatObjectType := aFormatObject.FormatObjectType;

  case (NewFO.FormatObjectType) of
    FO_BLOCK :
      begin
        FCurFormatObject.ForceOwnerDocument(NewFO);
        FCurFormatObject.AppendChild(NewFO);
        NewFO.ParentFO := FCurFormatObject;
        FCurFormatObject := NewFO;
        NewFO.Release;
      end;
    FO_INLINE :
      begin
        FCurElement.ForceOwnerDocument(NewFO);
        FCurElement.AppendChild(NewFO);
        NewFO.ParentFO := FCurFormatObject;
        FCurFormatObject := NewFO;
        NewFO.Release;
      end;
    FO_ROOT :
      begin
        if (ResultTreeRoot = nil) then begin
          FResultTree.Document.AppendChild(NewFO);
          NewFO.ParentFO := FCurFormatObject;
          FCurFormatObject := NewFO;
          NewFO.Release;
        end else
          raise TXpXSLFOException.Create(sXSLFOMultipleRoots);
      end;
    FO_PAGE_SEQUENCE :
      begin
        if (ResultTreeRoot is TXpFORoot) then begin
          FCurFormatObject := TXpFORoot(ResultTreeRoot);
          FCurFormatObject.AppendChild(NewFO);
          FCurPageSequence := TXpFOPageSequence(NewFO);
          TempPropVal := aFormatObject.GetAttribute(XpsMasterName);
          if TempPropVal = '' then                                         {!!.57}
            TempPropVal := aFormatObject.GetAttribute(XpsMasterReference); {!!.57}
          FCurPageMaster :=
            TXpFOSimplePageMaster(TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(TempPropVal));
          NewFO.ParentFO := FCurFormatObject;
          FCurFormatObject := NewFO;
          FPageSequenceCount := FPageSequenceCount + 1;
          NewFO.Release;
        end;
      end;
    FO_FLOW :
      begin
        FCurFormatObject.AppendChild(NewFO);
        SetCurrentRegion(aFormatObject.GetAttribute(XpsFlowName));
        FCurElement := FOutRegionCurrent.DocumentElement;
        NewFO.ParentFO := FCurFormatObject;
        FCurFormatObject := NewFO;
        NewFO.Release;
      end;
    FO_PAGE_SEQUENCE_MASTER :
      begin
        if (ResultTreeRoot is TXpFORoot) then begin
          FCurFormatObject := TXpFORoot(ResultTreeRoot).LayoutMaster;
          FCurFormatObject.AppendChild(NewFO);
          NewFO.ParentFO := FCurFormatObject;
          FCurFormatObject := NewFO;
          NewFO.Release;
        end;
      end;
    FO_LAYOUT_MASTER_SET :
      begin
        if (ResultTreeRoot is TXpFORoot) then begin
          FCurFormatObject := TXpFORoot(ResultTreeRoot);
          FCurFormatObject.AppendChild(NewFO);
          NewFO.ParentFO := FCurFormatObject;
          FCurFormatObject := NewFO;
          NewFO.Release;
        end;
      end;
    FO_SIMPLE_PAGE_MASTER :
      begin
        if (ResultTreeRoot is TXpFORoot) then begin
          FCurFormatObject := TXpFORoot(ResultTreeRoot).LayoutMaster;
          FCurFormatObject.AppendChild(NewFO);
          NewFO.ParentFO := FCurFormatObject;
          FCurFormatObject := NewFO;
          NewFO.Release;
        end;
      end;
    FO_STATIC_CONTENT :
      begin
        FCurFormatObject.AppendChild(NewFO);
        SetCurrentRegion(aFormatObject.GetAttribute(XpsFlowName));
        NewFO.ParentFO := FCurFormatObject;
        FCurFormatObject := NewFO;
        NewFO.Release;
      end;
    FO_TITLE :
      begin
        FCurFormatObject.ForceOwnerDocument(NewFO);
        FCurFormatObject.AppendChild(NewFO);
        SetCurrentRegion(XpsFORegTitle);
        NewFO.ParentFO := FCurFormatObject;
        FCurFormatObject := NewFO;
        NewFO.Release;
      end;
  else
    if (FCurFormatObject <> nil) then begin
      FCurFormatObject.AppendChild(NewFO);
      NewFO.ParentFO := FCurFormatObject;
      FCurFormatObject := NewFO;
      NewFO.Release;
    end;
  end;
end;
{--------}
procedure TXpFilterBase.XslProcessorFormatObjectEnd;
var                                                                    {!!.53 - rewritten}
  TempNode : TXpNode;
begin
  TempNode := FCurFormatObject.ParentFO;
  while ((TempNode <> nil) and
         (not (TempNode is TXpFormattingObject)) and
         (TempNode.ParentNode <> nil)) do
    TempNode := TempNode.ParentNode;
  FCurFormatObject := TXpFormattingObject(TempNode);
end;
{--------}                                                             {!!.53 - end}
procedure TXpFilterBase.XslProcessorProcessingInstruction(oOwner : TObject;
                                                          sName,
                                                          sValue : DOMString);
begin
//Do nothing
end;
{--------}
procedure TXpFilterBase.XslProcessorQueryFilter(Sender : TObject;
                                            var sName  : string);
begin
  sName := FDisplayName;
end;
{--------}
procedure TXpFilterBase.XslProcessorText(oOwner : TObject;
                                         sValue : DOMString);  
begin
//!!!!
end;
{--------}
procedure TXpFilterBase.XslProcessorTextNode(oOwner : TObject;
                                             oNode  : TXpText);
begin
  FCurElement.ForceOwnerDocument(oNode);
  FCurElement.AppendChild(oNode);
end;
{=====================================================================}

{== TXpTableRowArea ==================================================}
constructor TXpTableRowArea.Create;
begin
  inherited Create;
  FRowElement := AElement;                                             {!!.57}
  TableCells := TList.Create;
end;
{--------}
destructor TXpTableRowArea.Destroy;
var
  i : Integer;
begin
  for i := 0 to (TableCells.Count - 1) do
    TXpTableCellArea(TableCells[i]).Free;
  TableCells.Free;
  inherited Destroy;
end;
{--------}
function TXpTableRowArea.CurrentCell: TXpTableCellArea;
begin
  if (SelectCell < TableCells.Count) then
    Result := TXpTableCellArea(TableCells[SelectCell])
  else
    Result := nil;
end;

function TXpTableRowArea.GetCell(Index: Integer): TXpTableCellArea;
begin
  Result := TXpTableCellArea(TableCells[Index]);
end;

function TXpTableRowArea.FirstAvailColumn(C: Integer): Integer;
var
  i: Integer;
begin
  repeat
    for i := 0 to TableCells.Count - 1 do
      if TXpTableCellArea(TableCells[i]).Column = C then begin
        inc(C);
        continue;
      end;
  until True;
  Result := C;
end;

procedure TXpTableRowArea.AddOrdered(C: TXpTableCellArea);
var
  i: Integer;
begin
  if (TableCells.Count = 0) then
    TableCells.Add(C)
  else
  if (C.Column < Cell[0].Column) then
    TableCells.Insert(0, C)
  else begin
    for i := 0 to TableCells.Count - 1 do begin
      if Cell[i].Column = C.Column then
        raise Exception.Create('duplicate column');
      if Cell[i].Column > C.Column then begin
        TableCells.Insert(i, C);
        exit;
      end;
    end;
    TableCells.Add(C);
  end;
end;

{=====================================================================}

{== TXpTableArea =====================================================}
constructor TXpTableArea.Create;
begin
  inherited Create;
  TableRows := TList.Create;
end;
{--------}
destructor TXpTableArea.Destroy;
var
  i : Integer;
begin
  for i := 0 to (TableRows.Count - 1) do
    TXpTableRowArea(TableRows[i]).Free;
  TableRows.Free;
  inherited Destroy;
end;
{--------}
function TXpTableArea.GetCurrentRow : TXpTableRowArea;
begin
  Result := nil;
  if (SelectRow < TableRows.Count) then
    Result := TXpTableRowArea(TableRows[SelectRow]);
end;

function TXpTableArea.GetRow(Index: Integer): TXpTableRowArea;
begin
  Result := TXpTableRowArea(TableRows[Index]);
end;

{=====================================================================}

{== TXpDrawArea ======================================================}
constructor TXpDrawArea.Create;
begin
  inherited Create;
  Table := TXpTableArea.Create;
end;
{--------}
destructor TXpDrawArea.Destroy;
begin
  Table.Free;
  inherited Destroy;
end;
{=====================================================================}

{== TXpFoPropertyGroupText ===========================================}
constructor TXpFoPropertyGroupText.Create;
begin
  inherited Create;
  Reset;
end;
{--------}
procedure TXpFoPropertyGroupText.Reset;
begin
  Empty;
  SetProperty(XpsFontFamily, 'Arial', True);
  SetProperty(XpsFontSize, '10pt', True);
  SetProperty(XpsColor, 'black', True);
  SetProperty(XpsBackgroundColor, 'transparent', True);
end;
{=====================================================================}

{== Global methods ===================================================}
procedure FreeFOP(Sender : TXpBaseHash; aData : Pointer);
begin
  TXpIntHash(aData).Free;
end;
{--------}
procedure InitializeUnit;
//var
//  /TempHash : TXpIntHash;
//  Idx      : Integer;
begin
end;
 { xpoFOPHash := TXpIntHash.Create(xpc_Size127);
  xpoFOPHash.OnDisposeDataEx := FreeFOP;
  for Idx := FOP_ALIGNMENT_ADJUST to FOP_WRAP_OPTION do begin
    TempHash := TXpIntHash.Create(xpc_Size59);
    case Idx of
      FOP_ALIGNMENT_ADJUST :
        begin
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_page_number, nil);
        end;
      FOP_BACKGROUND_ATTACHMENT,
      FOP_BACKGROUND_COLOR,
      FOP_BACKGROUND_IMAGE,
      FOP_BACKGROUND_POSITION_H,
      FOP_BACKGROUND_POSITION_V :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_body, nil);
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_table_column, nil);
          TempHash.Add(FO_table_row, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_BLANK_OR_NOT_BLANK,
      FOP_ODD_OR_EVEN,
      FOP_PAGE_POSITION :
        begin
          TempHash.Add(FO_conditional_page_master_reference, nil);
        end;
      FOP_BORDER_AFTER_COLOR,
      FOP_BORDER_AFTER_WIDTH,
      FOP_BORDER_BEFORE_COLOR,
      FOP_BORDER_BEFORE_WIDTH,
      FOP_BORDER_BOTTOM_COLOR,
      FOP_BORDER_BOTTOM_WIDTH,
      FOP_BORDER_END_COLOR,
      FOP_BORDER_END_WIDTH,
      FOP_BORDER_LEFT_COLOR,
      FOP_BORDER_LEFT_WIDTH,
      FOP_BORDER_RIGHT_COLOR,
      FOP_BORDER_RIGHT_WIDTH,
      FOP_BORDER_START_COLOR,
      FOP_BORDER_START_WIDTH,
      FOP_BORDER_TOP_COLOR,
      FOP_BORDER_TOP_WIDTH :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_body, nil);
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_BREAK_AFTER,
      FOP_BREAK_BEFORE :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_row, nil);
        end;
      FOP_COLOR :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_COLUMN_NUMBER :
        begin
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_table_column, nil);
        end;
      FOP_COLUMN_WIDTH,
      FOP_NUMBER_COLUMNS_REPEATED :
        begin
          TempHash.Add(FO_table_column, nil);
        end;
      FOP_CONTENT_HEIGHT,
      FOP_CONTENT_WIDTH,
      FOP_SCALING,
      FOP_SCALING_METHOD,
      FOP_SRC :
        begin
          TempHash.Add(FO_external_graphic, nil);
        end;
      FOP_DISPLAY_ALIGN :
        begin
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
          TempHash.Add(FO_table_cell, nil);
        end;
      FOP_EMPTY_CELLS,
      FOP_ENDS_ROW,
      FOP_STARTS_ROW :
        begin
          TempHash.Add(FO_table_cell, nil);
        end;
      FOP_END_INDENT,
      FOP_MARGIN_BOTTOM,
      FOP_MARGIN_LEFT,
      FOP_MARGIN_RIGHT,
      FOP_MARGIN_TOP,
      FOP_SPACE_AFTER,
      FOP_SPACE_BEFORE :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_simple_page_master, nil);
          TempHash.Add(FO_table, nil);
        end;
      FOP_EXTENT,
      FOP_PRECEDENCE :
        begin
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
        end;
      FOP_EXTERNAL_DESTINATION,
      FOP_SHOW_DESTINATION :
        begin
          TempHash.Add(FO_basic_link, nil);
        end;
      FOP_FLOW_NAME :
        begin
          TempHash.Add(FO_flow, nil);
          TempHash.Add(FO_static_content, nil);
        end;
      FOP_FONT_FAMILY,
      FOP_FONT_SIZE,
      FOP_FONT_SIZE_ADJUST,
      FOP_FONT_STRETCH,
      FOP_FONT_STYLE,
      FOP_FONT_VARIANT,
      FOP_FONT_WEIGHT :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_HEIGHT :
        begin
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_table_row, nil);
        end;
      FOP_HYPHENATE,
      FOP_HYPHENATE_CHAR,
      FOP_HYPHENATE_KEEP,
      FOP_HYPHENATE_PUSH_CHARACTER_COUNT,
      FOP_HYPHENATE_REMAIN_CHARACTER_COUNT,
      FOP_LANGUAGE,
      FOP_ORPHANS,
      FOP_TEXT_ALIGN_LAST,
      FOP_TEXT_INDENT,
      FOP_WIDOWS,
      FOP_WRAP_OPTION :
        begin
          TempHash.Add(FO_BLOCK, nil);
        end;
      FOP_ID :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_list_item_body, nil);
          TempHash.Add(FO_list_item_label, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_page_sequence, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_body, nil);
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_table_row, nil);
        end;
      FOP_KEEP_TOGETHER :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_list_item_body, nil);
          TempHash.Add(FO_list_item_label, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_row, nil);
        end;
      FOP_KEEP_WITH_NEXT,
      FOP_KEEP_WITH_PREVIOUS :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_row, nil);
        end;
      FOP_LEADER_LENGTH,
      FOP_LEADER_PATTERN,
      FOP_RULE_THICKNESS :
        begin
          TempHash.Add(FO_LEADER, nil);
        end;
      FOP_LETTER_SPACING,
      FOP_WORD_SPACING :
        begin
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_page_number, nil);
        end;
      FOP_LINE_HEIGHT,
      FOP_LINE_HEIGHT_SHIFT_ADJUSTMENT :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_MASTER_NAME :
        begin
          TempHash.Add(FO_page_sequence, nil);
          TempHash.Add(FO_page_sequence_master, nil);
          TempHash.Add(FO_repeatable_page_master_reference, nil);
          TempHash.Add(FO_simple_page_master, nil);
          TempHash.Add(FO_single_page_master_reference, nil);
        end;
      FOP_OVERFLOW :
        begin
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
        end;
      FOP_PADDING_AFTER,
      FOP_PADDING_BEFORE,
      FOP_PADDING_BOTTOM,
      FOP_PADDING_END,
      FOP_PADDING_LEFT,
      FOP_PADDING_RIGHT,
      FOP_PADDING_START,
      FOP_PADDING_TOP :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_list_block, nil);
          TempHash.Add(FO_list_item, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_body, nil);
          TempHash.Add(FO_table_cell, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_PAGE_HEIGHT,
      FOP_PAGE_WIDTH :
        begin
          TempHash.Add(FO_simple_page_master, nil);
        end;
      FOP_PROVISIONAL_DISTANCE_BETWEEN_STARTS,
      FOP_PROVISIONAL_LABEL_SEPARATION :
        begin
          TempHash.Add(FO_list_block, nil);
        end;
      FOP_REGION_NAME :
        begin
          TempHash.Add(FO_region_after, nil);
          TempHash.Add(FO_region_before, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_region_end, nil);
          TempHash.Add(FO_region_start, nil);
        end;
      FOP_SPACE_END,
      FOP_SPACE_START :
        begin
          TempHash.Add(FO_basic_link, nil);
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_inline, nil);
          TempHash.Add(FO_leader, nil);
          TempHash.Add(FO_page_number, nil);
          TempHash.Add(FO_title, nil);
        end;
      FOP_START_INDENT :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_region_body, nil);
          TempHash.Add(FO_simple_page_master, nil);
        end;
      FOP_TEXT_ALIGN :
        begin
          TempHash.Add(FO_block, nil);
          TempHash.Add(FO_external_graphic, nil);
        end;
      FOP_WIDTH :
        begin
          TempHash.Add(FO_external_graphic, nil);
          TempHash.Add(FO_table, nil);
          TempHash.Add(FO_table_cell, nil);
        end;
    end;
    xpoFOPHash.Add(Idx, TempHash);
  end;
end;  }
{--------}
procedure FinalizeUnit;
begin
//  xpoFOPHash.Free;
end;
{=====================================================================}

{=====================================================================}
initialization
  InitializeUnit;
{--------}
finalization
  FinalizeUnit;
{=====================================================================}
end.
