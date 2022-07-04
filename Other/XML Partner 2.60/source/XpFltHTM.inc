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
{* XMLPartner Pro: XpFltHTM.INC                          *}
{*********************************************************}
{* XMLPartner Pro: HTML Filter for XSL                   *}
{*********************************************************}

{$I XpDefine.inc}

{!!.57 numerous changes}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UsingCLX}
  QControls,
  QGraphics,
  XpQFlBas,
  XpQXSLFO,
{$ELSE}
  Controls,
  Graphics,
  XpvFlBas,
  XpvXSLFO,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  SysUtils,
  Classes,
  XpBase,
  XpChrFlt,
  XpDOM,
  XpXSLCon;

type
  TXpHTMLOutputMode = (xphomBodyOnly, xphomFragment, xphomFull);

  TXpColEntry = class
  protected
    FCol: Integer;
    FWidth: Integer;
    FColor: Integer;
  public
    constructor Create(ACol, AWidth, AColor: Integer);
    property Col: Integer read FCol write FCol;
    property Width: Integer read FWidth write FWidth;
    property Color: Integer read FColor write FColor;
  end;

  TXpContextStackEntry = class
  protected
    FFormatObjectType: Integer;
    FColumns: TList;
    FCurCol: Integer;
    FImplicitRow: Boolean;
    FInRow: Boolean;
    FIsTable: Boolean;
    function GetColumnWidth(Index: Integer): Integer;
    function GetColumnColor(Index: Integer): Integer;
  public
    constructor Create(aFormatObject : TXpFormattingObject);
    destructor Destroy; override;
    property FormatObjectType: Integer read FFormatObjectType write FFormatObjectType;
    procedure AddColumn(ColIndex, ColWidth, ColColor: Integer);
    property ColumnWidth[Index: Integer]: Integer read GetColumnWidth;
    property ColumnColor[Index: Integer]: Integer read GetColumnColor;
    property CurCol: Integer read FCurCol write FCurCol;
    property ImplicitRow: Boolean read FImplicitRow write FImplicitRow;
    property InRow: Boolean read FInRow write FInRow;
    property IsTable: Boolean read FIsTable write FIsTable;
  end;

  TXpContextStack = class
  protected
    FStack: TList;
    function GetTOS: TXpContextStackEntry;
    function GetCurTable: TXpContextStackEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(aFormatObject : TXpFormattingObject);
    {$IFOPT C+}
    procedure PopAssert(ObjectType: Integer);
    {$ELSE}
    procedure Pop;
    {$ENDIF}
    property TOS: TXpContextStackEntry read GetTOS;
    property CurTable: TXpContextStackEntry read GetCurTable;
  end;

  TXpFilterHTML = class(TXpFilterBase)
  protected
    { Private declarations }
    FCSS                : Boolean;
    FFormattedOutput    : Boolean;
    {FInPageMaster       : Boolean;}                                   {!!.57}
    FListLabelWidth     : string;
    FOutputMode         : TXpHTMLOutputMode;
    {FPointsWidth        : Single;}                                    {!!.57}
    FXmlValidHTML       : Boolean;

    Context             : TXpContextStack;

    procedure fhCopyAttrsToCurrent(const aSourceElem : TXpElement);
    procedure fhCreateNonFOResultTree;
    function fhGetBodyDef : DOMString;
    function fhGetEndSideDef : DOMString;
    function fhGetFooterDef : DOMString;
    function fhGetHeaderDef : DOMString;
    function fhGetHtmlDocument : DOMString;
    function fhGetStartSideDef : DOMString;
    procedure fhSetCSS(oNode : TXpElement; const sCss, sValue : string);
    procedure fhSaveColumnAttrs(const aSourceElem : TXpElement);
  protected
    { Protected declarations }
    procedure Paint; override;
    function Render : DOMString; {virtual;}                            {!!.57}
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;
    procedure InitRegion(aRegion      : TXpObjModel;
                   const aRootTagName : string); override;
    procedure Reset; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight : Integer); override;
    function SaveToFile(const sFile : string) : Boolean; override;     {!!.57}
    function SaveToStream(aInStream : TStream) : Boolean; override;
    procedure XslProcessorApplyStyleStart(Sender : TObject); override;
    procedure XslProcessorApplyStyleEnd(Sender : TObject); override;
    procedure XslProcessorAttribute(oOwner : TObject;
                                    sName,
                                    sValue : DOMString); override;
    procedure XslProcessorCDATASection(oOwner   : TObject;
                                       sValue   : DOMString;
                                       bReplace : Boolean); override;
    procedure XslProcessorComment(oOwner : TObject;
                                  sValue : DOMString); override;
    procedure XslProcessorElementStart(oOwner : TObject;
                                       sValue : DOMString); override;
    procedure XslProcessorFormatObjectEnd(Sender : TObject); override;
    procedure XslProcessorFormatObject(Sender        : TObject;
                                       aFormatObject : TXpFormattingObject); override;

    procedure XslProcessorProcessingInstruction(oOwner : TObject;
                                                sName,        
                                                sValue : DOMString); override;

    procedure XslProcessorText(oOwner : TObject;
                               sValue : DOMString); override;  
    procedure XslProcessorTextNode(oOwner : TObject;
                                   oNode  : TXpText); override;

    property HTMLDocument : DOMString
      read fhGetHTMLDocument;

  published
    property CSS : Boolean
      read FCSS
      write FCSS
      default False;

    property FormattedOutput : Boolean
      read FFormattedOutput
      write FFormattedOutput
      default True;

    property OutputEncoding;

    property OutputMode : TXpHTMLOutputMode
      read FOutputMode
      write FOutputMode
      default XphomFragment;

    property WriteUTF8Signature;

    property XmlValidHtml : Boolean
      read FXmlValidHtml
      write FXmlValidHtml
      default True;
  end;

implementation

uses
  XpExcept,
{$IFDEF UsingCLX}
  XpQXSLPr;
{$ELSE}
  XpvXSLPr;
{$ENDIF}

//{$R *.RES}

const
  { Markup constants }
  XpsBody = '<body>';
  XpsBodyEnd = '</body>';
  XpsHTML = '<html>';
  XpsHTMLEnd = '</html>';
  XpsHTMLU = 'HTML';
  XpsCell = '<td';
  XpsCellEnd = '</td>';
  XpsRow = '<tr>';
  XpsRowEnd = '</tr>';
//  XpsStyle = '<style>';
  XpsStyleEnd = '</style>';
  {XpsTable = '<table';}                                               {!!.57}
  XpsTableEnd = '</table>';
  XpsTitle = '<title>';
  XpsTitleEnd = '</title>';

{== TXpFilterHTML =================================================}
constructor TXpFilterHTML.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);
  FDisplayName := XpsHTMLU;
  FFormattedOutput := True;
  {FInPageMaster := False;}                                            {!!.57}
  FXmlValidHtml := True;
  FOutRegionAfterRoot := XpsSPAN;
  FOutRegionBeforeRoot := XpsSPAN;
  FOutRegionBodyRoot := XpsSPAN;
  FOutRegionEndRoot := XpsSPAN;
  FOutRegionStartRoot := XpsSPAN;
  FOutTitleRoot := XpsTITLE;
  FOutputMode := XphomFragment;
  Context := TxpContextStack.Create;
end;
{--------}
destructor TXpFilterHTML.Destroy;
begin
  Context.Free;
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;
 inherited;
end;
{--------}
procedure TXpFilterHTML.fhCopyAttrsToCurrent(const aSourceElem : TXpElement);
var
  TempAttr : TXpAttribute;
  i        : Integer;
begin
  if (aSourceElem.Attributes <> nil) then begin
    for i := 0 to (aSourceElem.Attributes.Length - 1) do begin
      TempAttr := TXpAttribute(aSourceElem.Attributes.Item(i));
      fhSetCSS(FCurElement, TempAttr.Name, TempAttr.Value);
    end;
  end;
end;
{--------}
procedure TXpFilterHTML.fhCreateNonFOResultTree;
var
  oRoot : TXpElement;
begin
  oRoot := FResultTree.Document.CreateElement('html');
  FResultTree.Document.AppendChild(oRoot);
  FCurElement := oRoot;
  oRoot.Release;
end;
{--------}
function TXpFilterHTML.fhGetBodyDef : DOMString;
begin
  Result := 'width="*"';
end;
{--------}
function TXpFilterHTML.fhGetEndSideDef : DOMString;
begin
  if (FCurPageMaster.HasRegionEnd) then begin
    Result := 'rowspan="2" ';
    if (FCurPageMaster.HasRegionAfter) and
       (FCurPageMaster.HasRegionBefore) then begin
      if (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> xpsTrue) and
         (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> xpsTrue) then
        Result := 'rowspan="3" '
      else if (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = xpsTrue) and
              (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = xpsTrue) then
        Result := '';
    end;
    Result := ' width="' + EvalProp(FCurPageMaster.RegionEnd.GetAttribute(XpsExtent), fuDefault, 1)
              {, fuPercent, STANDARDPAGEWIDTH} + '"';
  end;
end;
{--------}
function TXpFilterHTML.fhGetFooterDef : DOMString;
begin
  Result := 'width="*"';
  if (FCurPageMaster.HasRegionAfter) then begin
    if (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = xpsTrue) then
      Result := 'width="100%" colspan="3" height="' +
                EvalProp(FCurPageMaster.RegionAfter.GetAttribute(XpsExtent),
                         fuPixels, 1) +
                '"';
  end;
end;
{--------}
function TXpFilterHTML.fhGetHeaderDef : DOMString;
begin
  if FCurPageMaster <> nil then                                        {!!.57}
    if (FCurPageMaster.HasRegionBefore) then
      Result := 'height="' +
                EvalProp(FCurPageMaster.RegionBefore.GetAttribute(XpsExtent),
                         fuPixels, 1) +
                '"';
end;
{--------}
function TXpFilterHTML.fhGetHtmlDocument : DOMString;
begin
  Result := Render;
end;
{--------}
function TXpFilterHTML.fhGetStartSideDef : DOMString;
begin
  if (FCurPageMaster.HasRegionStart) then begin
    Result := 'rowspan="2" ';
    if (FCurPageMaster.HasRegionAfter) and
       (FCurPageMaster.HasRegionBefore) then begin
      if (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> xpsTrue) and
         (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> xpsTrue) then
        Result := 'rowspan="3" '
      else if (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = xpsTrue) and
              (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = xpsTrue) then
        Result := '';
    end;
    Result := ' width="' + OutRegionStart.DocumentElement.GetAttribute(XpsExtent)
      {, fuPercent, STANDARDPAGEWIDTH} + '"'
  end;
end;
{--------}
procedure TXpFilterHTML.InitRegion(aRegion      : TXpObjModel;
                             const aRootTagName : string);
var
  oElem : TXpElement;
begin
  {Note: Intentionally not calling the inherited InitRegion.}
  aRegion.ClearDocument;
  if (aRootTagName <> '') then begin
    oElem := aRegion.Document.CreateElement(aRootTagName);
    oElem.FullEndTag := True;
    aRegion.Document.AppendChild(oElem);
    oElem.Release;
  end;
end;
{--------}
procedure TXpFilterHTML.Paint;
var
  oBit : TBitmap;
begin
  if csDesigning in ComponentState then begin
    oBit := TBitmap.Create;
    try
      oBit.Transparent := True;
      oBit.TransparentMode := tmAuto;
      oBit.LoadFromResourceName(HInstance, 'TXPFILTERHTML');
      Canvas.Draw(2, 2, oBit);
    finally
      oBit.Free;
    end;
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(0, 28);
    Canvas.LineTo(0, 0);
    Canvas.LineTo(28, 0);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(27, 0);
    Canvas.LineTo(27, 27);
    Canvas.LineTo(0, 27);
  end;
end;
{--------}
function TXpFilterHTML.Render : DOMString;
var
  TempRegBody   : TXpFORegionBody;
  sBodyDef,
  sHeaderDef,
  sFooterDef,
  sStartSideDef,
  sEndSideDef,
  sTmp          : DOMString;
  {nTmp          : Single;}                                            {!!.57}
  i             : Integer;
begin

  if (ResultIsFO) then begin
    { !!!! which page master should we use???}
    { !!!! is there a way to give the user a choice? -- an event??}
    for i := 0 to (TXpFORoot(ResultTreeRoot).PageSequences.Length - 1) do begin
      FCurPageSequence := TXpFOPageSequence(TXpFORoot(ResultTreeRoot).PageSequences.Item(i));
      FCurPageMaster := TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(
        FCurPageSequence.GetPropertyValue(XpsMasterName));
      if FCurPageMaster = nil then
        FCurPageMaster := TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(
          FCurPageSequence.GetPropertyValue(XpsMasterReference));
      Assert(FCurPageMaster <> nil);

      if (HasOutTitle) then begin
        with OutTitle do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;

      if (HasOutRegionBefore) then begin
        with OutRegionBefore do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;

      if (HasOutRegionStart) then begin
        with OutRegionStart do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;

      if (HasOutRegionBody) then begin
        with OutRegionBody do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;

      if (HasOutRegionEnd) then begin
        with OutRegionEnd do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;

      if (HasOutRegionAfter) then begin
        with OutRegionAfter do begin
          Document.DocumentElement.Normalize(False);
          Document.ActualCDATA := True;
          Document.FormattedOutput := FFormattedOutput;
        end;
      end;
    end;
  end else begin
    { Create a default PageMaster with only a body region.}
    FCurPageMaster := TXpFOSimplePageMaster.Create;
    TempRegBody := TXpFORegionBody.Create;
    FCurPageMaster.AppendChild(TempRegBody);
    TempRegBody.Release;
    FOutRegionBody.Free;
    FOutRegionBody := FResultTree;
    with FOutRegionBody do begin
      Document.DocumentElement.Normalize(False);
      Document.ActualCDATA := True;
      Document.FormattedOutput := FFormattedOutput;
    end;
  end;

  if (OutputMode = XphomFragment) then begin
    if (HasOutRegionBody) then
      Result := OutRegionBody.XMLDocument;
  end else if (OutputMode = XphomBodyOnly) then begin
    Result := XpsHTML + XpsCR;
    if (HasOutTitle) then begin
      Result := Result + XpsTitle;
      Result := Result + FOutTitle.XMLDocument;
      Result := Result + XpsTitleEnd + XpsCR;
    end;
    if (FCSS) then begin
      Result := Result + '<' + XpsStyle + '>' + XpsCR + '{';
      if FCurPageSequence <> nil then
        sTmp := FCurPageSequence.GetPropertyValue(XpsBackgroundImage)
      else
        sTmp := '';
      if (sTmp <> '') and
         (sTmp <> 'none') then
        Result := Result + 'background: ' + sTmp + ';';
      if FCurPageMaster <> nil then begin
        sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPoints, 1);
        if (sTmp <> '') then
          Result := Result + ' ' + XpsMarginLeft + ': ' + sTmp + ';';
        sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginTop), fuPoints, 1);
        if (sTmp <> '') then
          Result := Result + ' ' + XpsMarginTop + ': ' + sTmp + ';';
        sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPoints, 1);
        if sTmp <> '' then
          Result := Result + ' ' + XpsMarginRight + ': ' + sTmp + ';';
        sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginBottom), fuPoints, 1);
        if (sTmp <> '') then
          Result := Result + ' ' + XpsMarginBottom + ': ' + sTmp + ';';
        sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsBackgroundColor),
                         fuDefault, 1);
        if (sTmp <> '') and (sTmp <> 'transparent') then
          Result := Result + ' bgcolor: ' + sTmp + ';';
      end;
      Result := Result + '}' + XpsCR + XpsStyleEnd + XpsCR;
    end; { if FCSS }
    Result := Result + XpsBody + XpsCR;
    sTmp := '';
    if FCurPageMaster <> nil then
      sTmp := FCurPageMaster.GetPropertyValue(XpsPageWidth);
    if (sTmp = '') then
      sTmp := 'auto';
    sTmp := EvalProp(sTmp, fuPoints, 1);
    {nTmp := StrToFloat(Copy(sTmp, 1, Length(sTmp) - 2));}             {!!.57}
    {FPointsWidth := nTmp;}                                            {!!.57}

    if (HasOutRegionBody) then
      { Did the filter stub in a default HTML root element? }
      if FOutRegionBody.Document.DocumentElement.NodeName = 'html' then
        { Yes. Skip it since we've already output one. }
        Result := Result +
                  FOutRegionBody.Document.DocumentElement.ChildNodes.XmlDocument
      else
        { No. Output the full document. }
        Result := Result + FOutRegionBody.XmlDocument;

    Result := Result + XpsCR + XpsBodyEnd + XpsCR + XpsHTMLEnd;
  end else begin
    sHeaderDef := fhGetHeaderDef;
    sFooterDef := fhGetFooterDef;
    sStartSideDef := fhGetStartSideDef;
    sEndSideDef := fhGetEndSideDef;
    sBodyDef := fhGetBodyDef;

    Result := XpsHTML + XpsCR;
    if (HasOutTitle) then
      Result := Result + OutTitle.XMLDocument;

    if (FCSS) then begin
      Result := Result + '<' + XpsStyle + '>' + XpsCR + '{';

      sTmp := FCurPageMaster.RegionBody.GetPropertyValue(XpsBackgroundImage);
      if ((sTmp <> '') and
          (sTmp <> 'none')) then
        Result := Result + 'background: ' + sTmp + ';';
      sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPoints, 1);
      if (sTmp <> '') then
        Result := Result + ' ' + XpsMarginLeft + ': ' + sTmp + ';';
      sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginTop), fuPoints, 1);
      if (sTmp <> '') then
        Result := Result + ' ' + XpsMarginTop + ': ' + sTmp + ';';
      sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPoints, 1);
      if (sTmp <> '') then
        Result := Result + ' ' + XpsMarginRight + ': ' + sTmp + ';';
      sTmp := EvalProp(FCurPageMaster.GetPropertyValue(XpsMarginBottom), fuPoints, 1);
      if (sTmp <> '') then
        Result := Result + ' ' + XpsMarginBottom + ': ' + sTmp + ';';
      sTmp := EvalProp(FCurPageMaster.RegionBody.GetPropertyValue(XpsBackgroundColor),
                       fuDefault, 1);
      if (sTmp <> '') and (sTmp <> 'transparent') then
        Result := Result + ' bgcolor: ' + sTmp + ';';
      Result := Result + '}' + XpsCR + XpsStyleEnd + XpsCR;
    end; { if FCSS }
    Result := Result + XpsBody + XpsCR;
    sTmp := FCurPageMaster.GetPropertyValue(XpsPageWidth);
    if (sTmp = '') then
      sTmp := 'auto';
    sTmp := EvalProp(sTmp, fuPoints, 1);
    {nTmp := StrToFloat(Copy(sTmp, 1, Length(sTmp) - 2));}             {!!.57}
    {FPointsWidth := nTmp;}                                            {!!.57}

    Result := Result + '<' + XpsTable + ' border="0" width="' +        {!!.57}
              EvalProp(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                       fuPixels, 1) + '">' + XpsCR;
    if (FCurPageMaster.HasRegionBefore) and
       (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = xpsTrue) then begin
      if (HasOutRegionBefore) then begin
        Result :=
          Result + XpsRow + XpsCell + ' valign="top" colspan="3"' + sHeaderDef + '>' + XpsCR;
        Result := Result + FOutRegionBefore.XMLDocument;
        Result :=
          Result + XpsCellEnd + XpsRowEnd + XpsCR;
      end;
      Result := Result + XpsRow;
      if (HasOutRegionStart) then begin
        Result :=
          Result + XpsCell + ' valign="top" ' + sStartSideDef + '>' +
          FOutRegionStart.XMLDocument + XpsCellEnd + XpsCR;
      end;
      if (HasOutRegionBody) then begin
        Result :=
          Result + XpsCell + ' valign="top" ' + sBodyDef + '>' +
          FOutRegionBody.XMLDocument + XpsCellEnd + XpsCR;
      end;
      if (HasOutRegionEnd) then begin
        Result :=
          Result + XpsCell + ' valign="top" ' + sEndSideDef + '>' +
          FOutRegionEnd.XMLDocument + XpsCellEnd + XpsCR;
      end;
      Result := Result + XpsRowEnd + XpsCR;
    end else begin
      Result := Result + XpsRow + XpsCR;
      if (HasOutRegionStart) then
        Result := Result + XpsCell + ' valign="top" ' + sStartSideDef + '>' +
                  OutRegionStart.XmlDocument + XpsCellEnd + XpsCR;
      if (HasOutRegionBefore) then
        Result := Result + XpsCell + ' valign="top" width="*" ' + sHeaderDef +
                  '>' + OutRegionBefore.XmlDocument + XpsCellEnd + XpsCR;
      if (HasOutRegionEnd) then
        Result := Result + XpsCell + ' ' + sEndSideDef + '>' +
                  OutRegionEnd.Xmldocument + XpsCellEnd + XpsCR;
      Result := Result + XpsRowEnd + XpsCR;
      if (HasOutRegionBody) then
        Result := Result + XpsRow + XpsCell + ' valign="top" ' + sBodyDef + '>';
      { Did the filter stub in a default HTML root element? }
      if (FOutRegionBody.Document.DocumentElement.NodeName = 'html') then
        { Yes. Skip it since we've already output one. }
        Result := Result +
                  FOutRegionBody.Document.DocumentElement.ChildNodes.XmlDocument
      else
        { No. Output the full document. }
        Result := Result + FOutRegionBody.XmlDocument;
      Result := Result + XpsCellEnd + XpsCR;
      Result := Result + XpsRowEnd + XpsCR;
    end;
    if (HasOutRegionAfter) then
      Result := Result + XpsRow + XpsCell + ' valign="top" ' + sFooterDef +
                '>' + OutRegionAfter.XmlDocument + XpsCellEnd +
                XpsCR +XpsRowEnd + XpsCR;
    Result := Result + XpsTableEnd + XpsCR + XpsBodyEnd +
              XpsCR + XpsHTMLEnd;
  end;
  if (not ResultIsFO) then
    FOutRegionBody := nil;
  sTmp := '';
end;
{--------}
procedure TXpFilterHTML.Reset;
begin
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;
  inherited;
  FOutRegionCurrent := OutRegionBody;
  FCurElement := FOutRegionCurrent.Document.DocumentElement;
  FCurAttr := FCurElement;
  {FPointsWidth := 504;}                                               {!!.57}
  FCurPageSequence := nil;
end;
{--------}
{!!.57 new}
function StripFile(const S: string): string;
begin
  if StrLIComp(PChar(S), 'FILE://', 7) = 0 then
    Result := copy(S, 8, MaxInt)
  else
    Result := S;
end;

function TXpFilterHTML.SaveToFile(const sFile : string) : Boolean;     {!!.57}
var
  aStream : TFileStream;
  aFilter : TXpOutCharFilter;
  sText   : DOMString;
begin
{Begin !!.57}
  if Assigned(FResultTree.DocumentElement) then
    sText := Render
  else
    sText := '';
{End !!.57}
  {!!.57
  if (Pos('FILE://', UpperCase(sFile)) = 1) then
    Delete(sFile, 1, 7);
  }
  aStream := TFileStream.Create(StripFile(sFile), fmCreate);           {!!.57}
  aFilter := TXpOutCharFilter.Create(aStream, Length(sText) * 2);
  try
    aFilter.WriteUTF8Signature := False;
    aFilter.Format := MapCharEncToStreamFormat(FResultTree.OutCharSet);
    aFilter.PutString(sText);
    Result := True;
  finally
    aFilter.Free;
    aStream.Free;
  end;
  sText := '';
end;
{--------}
function TXpFilterHTML.SaveToStream(aInStream : TStream) : Boolean;
var
  OutFilter : TXpOutCharFilter;
  OutText   : DOMString;
  OutLength : Integer;
begin
  Result := True;
  try
{Begin !!.57}
    if Assigned(FResultTree.DocumentElement) then
      OutText := Render
    else
      OutText := '';
{End !!.57}
    OutLength := Length(OutText) * 2;
    OutFilter := TXpOutCharFilter.Create(aInStream, OutLength);
    try
      OutFilter.WriteUTF8Signature := False;
      OutFilter.Format := MapCharEncToStreamFormat(FResultTree.OutCharSet);
      OutFilter.PutString(OutText);
    finally
      OutFilter.Free;
    end;
  except
    Result := False;
  end;
  OutText := '';
end;
{--------}
procedure TXpFilterHTML.SetBounds(aLeft, aTop, aWidth, aHeight : Integer);
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
procedure TXpFilterHTML.XslProcessorApplyStyleStart(Sender : TObject);
begin
  Reset;
end;
{--------}
procedure TXpFilterHTML.XslProcessorApplyStyleEnd(Sender : TObject);
begin
  inherited;
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;
end;
{--------}
procedure TXpFilterHTML.XslProcessorAttribute(oOwner : TObject;
                                              sName,
                                              sValue : DOMString);
begin
  inherited;
  if (FCurAttr <> nil) then
    FCurAttr.SetAttribute(sName, sValue);
end;
{--------}
procedure TXpFilterHTML.XslProcessorCDATASection(oOwner   : TObject;
                                                 sValue   : DOMstring; 
                                                 bReplace : Boolean);
var
  oText : TXpCDATASection;
begin
  if (ResultTreeRoot = nil) then
    fhCreateNonFOResultTree;

  inherited;

  if (FCurElement <> nil) then begin
    { Replace special chars with entities in sValue }
    if bReplace then begin
      sValue := XpStringReplaceAll(sValue, '&', '&amp;');
      sValue := XpStringReplaceAll(sValue, '<', '&lt;');
    end;

    oText := FOutRegionCurrent.Document.CreateCDATASection(sValue);
    FResultTree.Document.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
    oText.Release;
  end;
end;
{--------}
procedure TXpFilterHTML.XslProcessorComment(oOwner : TObject;
                                            sValue : DOMString);
var
  oComment : TXpComment;
begin

  { Create the comment node. }
  oComment := FResultTree.Document.CreateComment(sValue);
  try
    FResultTree.Document.ForceOwnerDocument(oComment);

    { Do we lack the document root? }
    if (ResultTreeRoot = nil) then
      fhCreateNonFOResultTree;

    { Add it as a child of the current node. }
    FCurElement.AppendChild(oComment);
  finally
    oComment.Release;
  end;
end;
{--------}
procedure TXpFilterHTML.XslProcessorElementStart(oOwner : TObject;
                                                 sValue : DOMString);
var
  sTerm : string;
begin
  if (ResultTreeRoot = nil) then begin
    fhCreateNonFOResultTree;
    if AnsiUppercase(sValue) = XpsHTMLU then
      Exit;
  end;

  inherited XslProcessorElementStart(oOwner, sValue);

  sTerm := AnsiUpperCase(sValue);
  if (sTerm = 'AREA')    or (sTerm = 'BASE')   or (sTerm = 'BASEFONT') or
     (sTerm = 'BGSOUND') or (sTerm = 'BR')     or (sTerm = 'COL') or
     (sTerm = 'FRAME')   or (sTerm = 'HR')     or (sTerm = 'IMG') or
     (sTerm = 'INPUT')   or (sTerm = 'KEYGEN') or (sTerm = 'META') or
     (sTerm = 'PARAM')   or (sTerm = 'SPACER') or (sTerm = 'WBR') then begin
    FCurElement.IgnoreEndTag := not FXmlValidHTML;
  end else
    FCurElement.FullEndTag := True;

  sTerm := '';
end;
{--------}
procedure TXpFilterHTML.XslProcessorFormatObject(Sender        : TObject;
                                                 aFormatObject : TXpFormattingObject);
var
  SpanStr: string;
  W, W0,
  i, Span, Err: Integer;
  TempElement : TXpElement;
begin
  inherited;

  Context.Push(aFormatObject);

  case (aFormatObject.FormatObjectType) of
    FO_BLOCK :
      begin
        XslProcessorElementStart(Sender, XpsDIV);
        FLastBlockElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_BASIC_LINK :
      begin
        XslProcessorElementStart(Sender, 'a');
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_EXTERNAL_GRAPHIC :
      begin
        XslProcessorElementStart(Sender, XpsIMG);
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_INLINE :
      begin
        XslProcessorElementStart(Sender, XpsSPAN);
        FLastInlineElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_LIST_BLOCK :
      begin
        XslProcessorElementStart(Sender, 'table');
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, '100%');
        fhCopyAttrsToCurrent(aFormatObject);
        if (FCurElement.HasAttribute(XpsProvisionalLabelSeparation)) then
          FListLabelWidth := FCurElement.GetAttribute(XpsProvisionalLabelSeparation)
        else
          FListLabelWidth := '10%';
      end;
    FO_LIST_ITEM :
      begin
        XslProcessorElementStart(Sender, XpsTR);
        FLastTRElem := FCurElement;
        FCurElement.FullEndTag := True;
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_LIST_ITEM_BODY :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, '*');
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_LIST_ITEM_LABEL :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, FListLabelWidth);
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_PAGE_NUMBER :
      begin
        XslProcessorText(Sender, '1');
      end;
    FO_TABLE_AND_CAPTION :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEANDCAPTIONELem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, '100%');
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEELem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        //FCurElement.SetAttribute(XpsWidth, '100%');
        fhCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE_BODY :
      begin
        XslProcessorElementStart(Sender, 'tbody');
        FLastTBODYELem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_HEADER :
      begin
        XslProcessorElementStart(Sender, 'thead');
        FLastTHEADElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_FOOTER :
      begin
        XslProcessorElementStart(Sender, 'tfoot');
        FLastTFOOTElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_TABLE_ROW :
      begin
        XslProcessorElementStart(Sender, 'tr');
        FLastTRElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        Context.CurTable.CurCol := 0;
        Context.CurTable.InRow := True;
      end;
    FO_TABLE_CELL :
      begin
        Assert(aFormatObject is TXpFOTableCell);
        SpanStr := TXpFOTableCell(aFormatObject).GetPropertyValue('starts-row');
        if LowerCase(SpanStr) = 'true' then begin
          if Context.CurTable.ImplicitRow then begin
            FCurElement := FLastTRElem;
            TempElement := TXpElement(FCurElement.ParentNode);
            while ((TempElement <> nil) and
                   (TempElement.NodeName <> XpsTR)) do
              TempElement := TXpElement(TempElement.ParentNode);
            FLastTRElem := TXpElement(TempElement);
            SetParentToCurrent;
          end;
          XslProcessorElementStart(Sender, 'tr');
          FLastTRElem := FCurElement;
          FCurElement.FullEndTag := True;
          Context.CurTable.CurCol := 0;
          Context.CurTable.ImplicitRow := True;
        end else
        if not Context.CurTable.InRow and not Context.CurTable.ImplicitRow then begin
          XslProcessorElementStart(Sender, 'tr');
          FLastTRElem := FCurElement;
          FCurElement.FullEndTag := True;
          Context.CurTable.CurCol := 0;
          Context.CurTable.ImplicitRow := True;
        end;
        XslProcessorElementStart(Sender, 'td');
        FLastTDElem := FCurElement;
        fhCopyAttrsToCurrent(aFormatObject);
        SpanStr := TXpFOTableCell(aFormatObject).GetPropertyValue('number-columns-spanned');
        val(SpanStr, Span, Err);
        if Err = 0 then begin
          W := 0;
          for i := 1 to Span do begin
            W0 := Context.CurTable.ColumnWidth[Context.CurTable.CurCol + i];
            if W0 <> -1 then
              inc(W, W0);
          end;
          Context.CurTable.CurCol := Context.CurTable.CurCol + Span;
          if W <> 0 then
            fCurElement.SetAttribute('width', IntToStr(W));
          W := Context.CurTable.ColumnColor[Context.CurTable.CurCol + 1];
          if W <> -1 then
            fCurElement.SetAttribute('bgcolor', '#' + IntToHex(W,0));
          fCurElement.SetAttribute('colspan', IntToStr(Span));
        end else begin
          Context.CurTable.CurCol := Context.CurTable.CurCol + 1;
          W := Context.CurTable.ColumnWidth[Context.CurTable.CurCol];
          if W <> -1 then
            fCurElement.SetAttribute('width', IntToStr(W));
          W := Context.CurTable.ColumnColor[Context.CurTable.CurCol];
          if W <> -1 then
            fCurElement.SetAttribute('bgcolor', '#' + IntToHex(W,0));
          Context.CurTable.CurCol := Context.CurTable.CurCol + Span;
        end;
        SpanStr := TXpFOTableCell(aFormatObject).GetPropertyValue('number-rows-spanned');
        val(SpanStr, Span, Err);
        if Err = 0 then begin
          fCurElement.SetAttribute('rowspan', IntToStr(Span));
        end;
      end;
    FO_TABLE_COLUMN :
      fhSaveColumnAttrs(aFormatObject);
    end;
end;

procedure TXpFilterHTML.fhSaveColumnAttrs(const aSourceElem : TXpElement);
var
  ColStr, PixStr: string;
  Col, Pix, Err, i, Color, R, G, B: Integer;
begin
  Assert(aSourceElem is TXpFOTableColumn);
  ColStr := TXpFOTableColumn(aSourceElem).GetPropertyValue('background-color');
  if ColStr <> '' then begin
    XpEvaluateColor(ColStr, R, G, B);
    Color := ((R shl 8) + G) shl 8 + B;
  end else
    Color := -1;

  PixStr := EvalProp(TXpFOTableColumn(aSourceElem).GetPropertyValue('column-width'),
                     fuPixels, 1);
  val(PixStr, Pix, Err);
  if Err <> 0 then
    Pix := -1;

  ColStr := TXpFOTableColumn(aSourceElem).GetPropertyValue('column-number');
  val(ColStr, Col, Err);
  if Err = 0 then
    Context.CurTable.AddColumn(Col, Pix, Color)
  else begin
    ColStr := TXpFOTableColumn(aSourceElem).GetPropertyValue('number-columns-repeated');
    val(ColStr, Col, Err);
    if Err = 0 then begin
      for i := 1 to Col do
        Context.CurTable.AddColumn(I, Pix, Color);
    end else begin
      Context.CurTable.CurCol := Context.CurTable.CurCol + 1;
      Context.CurTable.AddColumn(Context.CurTable.CurCol, Pix, Color);
    end;
  end;
end;

{--------}
procedure TXpFilterHTML.XslProcessorFormatObjectEnd(Sender : TObject);
var
  TempElement : TXpElement;
  SpanStr: string;
begin
  {$IFOPT C+}
  Context.PopAssert(FCurFormatObject.FormatObjectType);
  {$ELSE}
  Context.Pop;
  {$ENDIF}
  case (FCurFormatObject.FormatObjectType) of
    FO_BLOCK :
      begin
        FCurElement := FLastBlockElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsDIV)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastBlockElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_INLINE :
      begin
        FCurElement := FLastInlineElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsSPAN)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastInlineElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM :
      begin
        FCurElement := FLastTRElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTR)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTRElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM_BODY :
      begin
        FCurElement := FLastTDElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTD)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTDElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM_LABEL :
      begin
        FCurElement := FLastTDElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTD)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTDElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_TABLE_AND_CAPTION :
      begin
        FCurElement := FLastTABLEANDCAPTIONElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTABLE)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTABLEANDCAPTIONElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_TABLE :
      begin
        FCurElement := FLastTABLEElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTABLE)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTABLEElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_TABLE_BODY :
      begin
        FCurElement := FLastTBODYElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTBODY)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTBODYElem := TXpElement(TempElement);
        SetParentToCurrent;
        Context.CurTable.ImplicitRow := False;
      end;
    FO_HEADER :
      begin
        FCurElement := FLastTHEADElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> 'thead')) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTHEADElem := TXpElement(TempElement);
        SetParentToCurrent;
        Context.CurTable.ImplicitRow := False;
      end;
    FO_FOOTER :
      begin
        FCurElement := FLastTFOOTElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> 'tfoot')) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTFOOTElem := TXpElement(TempElement);
        SetParentToCurrent;
        Context.CurTable.ImplicitRow := False;
      end;
    FO_TABLE_ROW :
      begin
        FCurElement := FLastTRElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTR)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTRElem := TXpElement(TempElement);
        SetParentToCurrent;
        Context.CurTable.InRow := False;
      end;
    FO_TABLE_CELL :
      begin
        Assert(FCurFormatObject is TXpFOTableCell);
        SpanStr := TXpFOTableCell(FCurFormatObject).GetPropertyValue('ends-row');
        FCurElement := FLastTDElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while ((TempElement <> nil) and
               (TempElement.NodeName <> XpsTD)) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTDElem := TXpElement(TempElement);
        SetParentToCurrent;
        if LowerCase(SpanStr) = 'true' then begin
          if Context.CurTable.ImplicitRow then begin
            FCurElement := FLastTRElem;
            TempElement := TXpElement(FCurElement.ParentNode);
            while ((TempElement <> nil) and
                   (TempElement.NodeName <> XpsTR)) do
              TempElement := TXpElement(TempElement.ParentNode);
            FLastTRElem := TXpElement(TempElement);
            SetParentToCurrent;
            Context.CurTable.ImplicitRow := False;
          end;
        end;
      end;
  end;
  inherited;
end;
{--------}
procedure TXpFilterHTML.fhSetCSS(oNode  : TXpElement;
                           const sCSS,
                                 sValue : string);
var
  TempAttr  : TXpAttribute;
  TempCSS   : string;
  TempValue : string;
  sStyle    : string;
  wPos      : Integer;
  i         : Integer;

  function PointToSize(Pt: Integer): Integer;
  begin
    if Pt <= 8 then
      Result := 1
    else
    if Pt <= 10 then
      Result := 2
    else
    if Pt <= 12 then
      Result := 3
    else
    if Pt <= 14 then
      Result := 4
    else
    if Pt <= 18 then
      Result := 5
    else
    if Pt <= 24 then
      Result := 6
    else
      Result := 7;
  end;

begin
  if (sCss = 'display-align') then begin
    if CSS then begin
      if (sValue = 'before') then
        TempValue := 'top'
      else if (sValue = 'after') then
        TempValue := 'bottom'
      else if (sValue = 'center') then
        TempValue := 'middle'
      else
        TempValue := sValue;
      TempCSS := 'vertical-align';
    end else begin
      if (sValue = 'before') then
        TempValue := 'top'
      else if (sValue = 'after') then
        TempValue := 'bottom'
      else if (sValue = 'center') then
        TempValue := 'middle'
      else
        TempValue := sValue;
      oNode.SetAttribute('valign', TempValue);
      exit;
    end;
  end else   if (sCss = XpsTextAlign) then begin
    if (sValue = 'end') then
      TempValue := 'right'
    else if (sValue = 'start') then
      TempValue := 'left'
    else
      TempValue := sValue;
    TempCSS := sCSS;
  end else if (Pos(XpsSpaceAfter, sCSS) > 0) then begin
    if CSS then
      TempCSS := XpsPaddingBottom
    else
      exit;
    TempValue := sValue;
  end else if (Pos(XpsSpaceBefore, sCSS) > 0) then begin
    if CSS then
      TempCss := XpsPaddingTop
    else
      exit;
    TempValue := sValue;
  end else if (sCSS = XpsExternalDestination) then begin
    TempCss := XpsHref;
    TempValue := sValue;
  end else if (sCSS = XpsInternalDestination) then begin
    TempCss := XpsHref;
    TempValue := '#' + sValue;
  end else if (sCSS = XpsLeaderLength) then begin
    TempCss := XpsWidth;
    TempValue := sValue;
  end else if (sCSS = XpsShowDestination) then begin
    if (AnsiUpperCase(sValue) = 'NEW') then begin
      TempCss := 'TARGET';
      TempValue := '..blank';
    end else
      { No attribute is required if the new page is displayed
        in the same window.}
      Exit;
  end else if (sCSS = XpsHeight) then begin
    if (FCurElement.NodeName = XpsIMG) or
       (FCurElement.NodeName = XpsTable) or
       (FCurElement.NodeName = XpsTD) or
       (FCurElement.NodeName = XpsTH) or
       (FCurElement.NodeName = 'APPLET') or
       (FCurElement.NodeName = 'FRAME') or
       (FCurElement.NodeName = 'LAYER')  or
       (FCurElement.NodeName = 'OBJECT') or
       (FCurElement.NodeName = 'SPACER') or
       (FCurElement.NodeName = 'IFRAME') then begin
      if CSS then
        FCurAttr.SetAttribute(XpsHeight, EvalProp(sValue, fuPixels, 1))
      else begin
        oNode.SetAttribute('height',
          IntToStr(EvalPropAsNum(sValue, fuPixels, 1)));               {!!.57}
      end;

      Exit;
    end else begin
      if CSS then begin
        TempCss := sCSS;
        TempValue := sValue;
      end else begin
        oNode.SetAttribute('height',
          IntToStr(EvalPropAsNum(sValue, fuPixels, 1)));               {!!.57}
        exit;
      end;
    end;
  end else if (sCSS = XpsId) then begin
    XslProcessorElementStart(Self, 'a');
    oNode := FCurElement;
    TempCss := XpsName;
    TempValue := sValue;
  end else if (sCSS = XpsBackgroundImage) then begin
    TempCss := sCSS;
    TempValue := 'url(' + sValue + ')';
  end else if (sCSS = XpsBackgroundPositionHorizontal) then begin
    { Are we using an HTML stylesheet?}
    if (not FCSS) then begin
      { Nope. The style is stored as an attribute.}

      { If this attribute already exist we'll have to update it.}
      if (oNode.HasAttribute('backgound-position')) then begin
        { It exists.}
        TempAttr := oNode.GetAttributeNode('backgound-position');
        { Retrieve vertical value and add to new value. The vertical
          value is the second token in the two-token string value.}
        for wPos := 1 to Length(TempAttr.NodeValue) do begin
          sStyle := sStyle + TempAttr.NodeValue[wPos];
          if (TempAttr.NodeValue[wPos] = ' ') then
            sStyle := '';
        end;
        oNode.SetAttribute('backgound-position', sValue + ' ' + sStyle);
      end else begin
        { Doesn't exist so we can just add it with the default
          vertical value.}
        oNode.SetAttribute('backgound-position', sValue + ' 0%');
      end;
    end else begin
      { Yes. This style is (will?) stored as part of the STYLE attribute.}

      { Is there already a STYLE attribute?}
      if (oNode.HasAttribute(XpsSTYLE)) then begin
        { Yes.}
        TempAttr := oNode.GetAttributeNode(XpsSTYLE);
        { Does it have an entry for background-position?}
        wPos := Pos('backgound-position', TempAttr.NodeValue);
        if (wPos > 0) then begin
          { Yes. We have to update it.}

          { Where is the entry in the old string?}
          wPos := wPos + Length('backgound-position') + 2;
          SetLength(TempValue, wPos);
          { Copy the stuff we need to preserve to a temp string.}
          for i := 1 to wPos do
            TempValue[i] := Char(TempAttr.NodeValue[i]);
          { Copy the new horizontal value to the temp string.}
          TempValue := TempValue + sValue;
          { Skip past the old horizontal value.}
          while (TempAttr.NodeValue[wPos] <> ' ') do
            wPos := wPos + 1;
          { Copy the rest of the style string to our temp string.}
          for i := wPos to Length(TempAttr.NodeValue) do
            TempValue := TempAttr.NodeValue[i];
          oNode.SetAttribute(XpsStyle, TempValue);
        end else begin
          { No. We can just tack it on the end.}
          TempValue := TempAttr.NodeValue + ' ' + sCSS + ': ';
          oNode.SetAttribute(XpsStyle, sValue + ' 0%;');
        end;
      end else begin
        { No. Just add the STYLE attribute with the
          background-position entry.}
          TempValue := sCSS + ': ';
          oNode.SetAttribute(XpsStyle, sValue + ' 0%;');
      end;
      Exit;
    end;
  end else if (sCSS = XpsBackgroundPositionVertical) then begin
    { Are we using an HTML stylesheet?}
    if (not FCSS) then begin
      { Nope. The style is stored as an attribute.}

      { If this attribute already exist we'll have to update it.}
      if (oNode.HasAttribute('backgound-position')) then begin
        { It exists.}
        TempAttr := oNode.GetAttributeNode('backgound-position');
        { Retrieve vertical value and add to new value. The vertical
          value is the second token in the two-token string value.}
        for wPos := 1 to Length(TempAttr.NodeValue) do begin
          sStyle := sStyle + TempAttr.NodeValue[wPos];
          if (TempAttr.NodeValue[wPos] = ' ') then
            sStyle := '';
        end;
        oNode.SetAttribute('backgound-position', sValue + ' ' + sStyle);
      end else begin
        { Doesn't exist so we can just add it with the default
          vertical value.}
        oNode.SetAttribute('backgound-position', sValue + ' 0%');
      end;
    end else begin
      { Yes. This style is (will?) stored as part of the STYLE attribute.}

      { Is there already a STYLE attribute?}
      if (oNode.HasAttribute(XpsSTYLE)) then begin
        { Yes.}
        TempAttr := oNode.GetAttributeNode(XpsSTYLE);
        { Does it have an entry for background-position?}
        wPos := Pos('backgound-position', TempAttr.NodeValue);
        if (wPos > 0) then begin
          { Yes. We have to update it.}

          { Where is the entry in the old string?}
          wPos := wPos + Length('backgound-position') + 2;
          SetLength(TempValue, wPos);
          for i := 1 to wPos do
            TempValue[i] := Char(TempAttr.NodeValue[i]);
          { Copy the horizontal value to the temp string.}
          while (TempAttr.NodeValue[wPos] <> ' ') do begin
            TempValue[wPos] := Char(TempAttr.NodeValue[wPos]);
            wPos := wPos + 1;
          end;
          { Skip past that last space in the old value.}
          wPos := wPos + 1;
          { Add the new vertical value to the string.}
          TempValue := TempValue + ' ' + sValue;
          { Skip past the old vertical value.}
          while (TempAttr.NodeValue[wPos] <> ';') do begin
            TempValue[wPos] := Char(TempAttr.NodeValue[wPos]);
            wPos := wPos + 1;
          end;
          { Copy the rest of the style string to our temp string.}
          for i := wPos to Length(TempAttr.NodeValue) do
            TempValue := TempAttr.NodeValue[i];
          oNode.SetAttribute(XpsStyle, TempValue);
        end else begin
          { No. We can just tack it on the end.}
          TempValue := TempAttr.NodeValue + ' ' + sCSS + ': ';
          oNode.SetAttribute(XpsStyle, '0% ' + sValue + ';');
        end;
      end else begin
        { No. Just add the STYLE attribute with the
          background-position entry.}
          TempValue := sCSS + ': ';
          oNode.SetAttribute(XpsStyle, sValue + ' 0%;');
      end;
      Exit;
    end;
  end else if (sCSS = XpsWidth) then begin
    if (FCurElement.NodeName = XpsIMG) or
       (FCurElement.NodeName = XpsTable) or
       (FCurElement.NodeName = XpsTD) or
       (FCurElement.NodeName = XpsTH) or
       (FCurElement.NodeName = 'APPLET') or
       (FCurElement.NodeName = 'FRAME') or
       (FCurElement.NodeName = 'LAYER') or
       (FCurElement.NodeName = 'OBJECT') or
       (FCurElement.NodeName = 'SPACER') or
       (FCurElement.NodeName = 'IFRAME') then begin
      FCurAttr.SetAttribute(XpsWidth, EvalProp(sValue, fuPixels, 1));
      Exit;
    end else begin
      TempCSS := sCSS;
      TempValue := sValue;
    end;
  end else if (sCSS = XpsWrapOption) then begin
    if ((FCurElement.NodeName = XpsDIV) and
        (sValue = 'nowrap')) then begin
      FCurAttr.NodeName := 'PRE';
      Exit;
    end;
  end else begin
    TempCSS := sCSS;
    TempValue := sValue;
  end;

  if not CSS and (sCSS = 'font-weight') then begin
    if sValue = 'bold' then begin
      XslProcessorElementStart(nil, 'b');
      exit;
    end;
  end;

  if not CSS and (sCSS = 'background-color') then begin
    oNode.SetAttribute('bgcolor', sValue);
    exit;
  end;

  if not CSS and (sCSS = 'font-size') then begin
    XslProcessorElementStart(nil, 'font');
    FCurElement.SetAttribute('size',
      IntToStr(PointToSize(EvalPropAsNum(sValue, fuPoints, 1))));      {!!.57}
    exit;
  end;

  if not CSS and (sCSS = 'font-family') then begin
    XslProcessorElementStart(nil, 'font');
    FCurElement.SetAttribute('face', sValue);
    exit;
  end;

  if not CSS and (sCSS = 'border-style') then begin
    FLastTABLEElem.SetAttribute('border', '1');
    exit;
  end;

  if not CSS and (sCSS = 'border-width') then begin
    exit;
  end;

  if not CSS and (sCSS = 'text-indent') then begin
    exit;
  end;

  if not CSS and (sCSS = 'padding') then begin
    exit;
  end;

  if not CSS and (sCSS = 'text-align') then begin
    oNode.SetAttribute('align', sValue);
    exit;
  end;

  if ((not FCSS) or
      (sCSS = XpsExternalDestination) or
      (sCSS = XpsInternalDestination) or
      (TempCss = XpsName) or
      (sCSS = XpsSrc)) then begin
    oNode.SetAttribute(TempCSS, TempValue);
    Exit;
  end;

  sStyle := oNode.GetAttribute(XpsSTYLE);
  if (sStyle = '') then
    sStyle := TempCss + ': ' + TempValue
  else begin
    wPos := Pos(' ' + TempCss + ':', sStyle);
    if (wPos > 0) then begin
      wPos := wPos + 1;
      while (wPos < Length(sStyle)) and
            (sStyle[wPos] <> ';') do
        Delete(sStyle, wPos, 1);
      if (wPos < Length(sStyle)) and
         (sStyle[wPos] = ';') then
        Delete(sStyle, wPos, 1);
    end;

    sStyle := sStyle + '; ' + TempCss + ': ' + TempValue;
  end;
  oNode.SetAttribute(XpsSTYLE, sStyle);

  TempCSS   := '';
  TempValue := '';
  sStyle    := '';
end;
{--------}
procedure TXpFilterHTML.XslProcessorProcessingInstruction(oOwner : TObject;
                                                          sName,
                                                          sValue : DOMString);
var
  oPI : TXpProcessingInstruction;
begin

  { Create the comment node. }
  oPI := FResultTree.Document.CreateProcessingInstruction(sName, sValue);
  try
    FResultTree.Document.ForceOwnerDocument(oPI);

    { Do we lack the document root? }
    if (ResultTreeRoot = nil) then
      fhCreateNonFOResultTree;

    { Add it as a child of the current node. }
    FCurElement.AppendChild(oPI);
  finally
    oPI.Release;
  end;
end;
{--------}
procedure TXpFilterHTML.XslProcessorText(oOwner : TObject;
                                         sValue : DOMString);
begin
  if (ResultTreeRoot = nil) then
    fhCreateNonFOResultTree;

  FCurElement.CreateChildText(sValue);
end;
{--------}
procedure TXpFilterHTML.XslProcessorTextNode(oOwner : TObject;         {!!.54 - Rewritten}
                                             oNode  : TXpText);
var
  oText : TXpNode;
begin
  if (ResultTreeRoot = nil) then
    fhCreateNonFOResultTree;

  if (oNode.NodeValue <> '') then begin
    { Create the text node. }
    oText := oNode.CloneNode(False);
    try
      FResultTree.Document.ForceOwnerDocument(oText);

      { Add it as a child of the current node. }
      FCurElement.AppendChild(oText);
    finally
      oText.Release;
    end;
  end;
end;                                                                   {!!.54 - End rewritten}

{=====================================================================}

constructor TXpContextStackEntry.Create(aFormatObject : TXpFormattingObject);
begin
  //FFormatObject := AFormatObject;
  FFormatObjectType := AFormatObject.FormatObjectType;
end;

destructor TXpContextStackEntry.Destroy;
var
  i: Integer;
begin
  if FColumns <> nil then begin
    for i := 0 to FColumns.Count - 1 do
      TObject(FColumns[i]).Free;
    FColumns.Free;
  end;
  inherited;
end;

procedure TXpContextStackEntry.AddColumn(ColIndex, ColWidth, ColColor: Integer);
begin
  if FColumns = nil then
    FColumns := TList.Create;
  FColumns.Add(TXpColEntry.Create(ColIndex, ColWidth, ColColor));
end;

function TXpContextStackEntry.GetColumnWidth(Index: Integer): Integer;
var
  i: Integer;
begin
  if (FColumns <> nil) then begin
    for i := 0 to FColumns.Count - 1 do
      if TXpColEntry(FColumns[i]).Col = Index then begin
        Result := TXpColEntry(FColumns[i]).Width;
        exit;
      end;
  end;
  Result := -1;
end;

function TXpContextStackEntry.GetColumnColor(Index: Integer): Integer;
var
  i: Integer;
begin
  if (FColumns <> nil) then begin
    for i := 0 to FColumns.Count - 1 do
      if TXpColEntry(FColumns[i]).Col = Index then begin
        Result := TXpColEntry(FColumns[i]).Color;
        exit;
      end;
  end;
  Result := -1;
end;

{=====================================================================}

constructor TXpColEntry.Create(ACol, AWidth, AColor: Integer);
begin
  FCol := ACol;
  FWidth := AWidth;
  FColor := AColor;
end;

{=====================================================================}

constructor TXpContextStack.Create;
begin
  FStack := TList.Create;
end;

destructor TXpContextStack.Destroy;
begin
  Assert(FStack.Count = 0);
  FStack.Free;
end;

procedure TXpContextStack.Push(aFormatObject : TXpFormattingObject);
begin
  FStack.Add(TXpContextStackEntry.Create(aFormatObject));
  if aFormatObject is TXpFOTable then
    TOS.IsTable := True;
end;

{$IFOPT C+}
procedure TXpContextStack.PopAssert(ObjectType: Integer);
begin
  Assert(FStack.Count > 0);
  Assert(TOS.{FormatObject.}FormatObjectType = ObjectType);
  TOS.Free;
  FStack.Delete(FStack.Count - 1);
end;

{$ELSE}
procedure TXpContextStack.Pop;
begin
  TOS.Free;
  FStack.Delete(FStack.Count - 1);
end;
{$ENDIF}

function TXpContextStack.GetTOS: TXpContextStackEntry;
begin
  Assert(FStack.Count > 0);
  Result := TXpContextStackEntry(FStack[FStack.Count - 1]);
  Assert(TObject(Result) is TXpContextStackEntry);
end;

function TXpContextStack.GetCurTable: TXpContextStackEntry;
var
  i : Integer;
begin
  for i := FStack.Count - 1 downto 0 do
    if TXpContextStackEntry(FStack[i]).IsTable then begin
      Result := TXpContextStackEntry(FStack[i]);
      exit;
    end;
  raise Exception.Create('Error: Table reference outside table object');
end;

end.
