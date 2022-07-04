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
{* XMLPartner Pro: XpFltXML.INC                          *}
{*********************************************************}
{* XMLPartner Pro: Implements XML Filter for XSL         *}
{*********************************************************}

{$I XpDefine.inc}

interface

uses
  Classes,
{$IFDEF UsingCLX}
  XpQFlBas,
  XpQXSLFO,
{$ELSE}
  XpvFlBas,
  XpvXSLFO,
{$ENDIF}
  XpBase,
  XpChrFlt,
  XpDOM;

type
  TXpFilterText = class(TXpFilterBase)
  protected
    function ftGetStringValue : DOMString;
    function ftGetText : DOMString;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;

    function RenderFile(const aFileName : string) : Boolean; override;
    function RenderMemory(var Buffer; aSize : Integer) : Boolean; override;
    function RenderStream(aStream : TStream) : Boolean; override;
    procedure Reset; override;
    function SaveToFile(const sFile : string) : Boolean; override;     {!!.57}
    function SaveToStream(aInStream : TStream) : Boolean; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight : Integer); override;

    procedure XslProcessorApplyStyleEnd(Sender : TObject); override;

    procedure XslProcessorApplyStyleStart(Sender : TObject); override;

    procedure XslProcessorAttribute(oOwner : TObject;
                                    sName,                     
                                    sValue : DOMString); override;

    procedure XslProcessorCDATASection(oOwner   : TObject;
                                       sValue   : DOMString;
                                       bReplace : Boolean); override;

    procedure XslProcessorElementEnd(oOwner : TObject;
                                     sValue : DOMString); override;

    procedure XslProcessorElementStart(oOwner : TObject;
                                       sValue : DOMString); override;

    procedure XslProcessorFormatObject(Sender        : TObject;
                                       aFormatObject : TXpFormattingObject); override;

    procedure XslProcessorFormatObjectEnd(Sender : TObject); override;

    procedure XslProcessorText(oOwner : TObject;
                               sValue : DOMString); override;

    procedure XslProcessorTextNode(oOwner : TObject;
                                   oNode  : TXpText); override;

    property StringValue : DOMString read ftGetStringValue;

    property Text : DOMString read ftGetText;

  published

    property OutputEncoding;

    property WriteUTF8Signature;

  end;

  TXpFilterXML = class(TXpFilterText)
  protected
    FDocTypePublicID : DOMString;
    FDocTypeSystemID : DOMString;
    FOmitStandalone : Boolean;
    FOmitXMLDecl : Boolean;
    FStandalone : Boolean;
    FVersion : DOMString;

    procedure ConfigureProlog;
    function GetFormattedOutput : Boolean;
    function GetXMLDocument : DOMString;
    procedure Paint; override;
    procedure SetFormattedOutput(const bFormatted : Boolean);
    property Text;
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;

    function RenderFile(const aFileName : string) : Boolean; override;
    function RenderMemory(var Buffer; aSize : Integer) : Boolean; override;
    function RenderStream(aStream : TStream) : Boolean; override;
    function SaveToFile(const sFile : string) : Boolean; override;     {!!.57}
    function SaveToStream(aInStream : TStream) : Boolean; override;

    procedure XslProcessorComment(oOwner : TObject;
                                  sValue : DOMString); override;

    procedure XslProcessorProcessingInstruction(oOwner : TObject;
                                                sName,         
                                                sValue : DOMString); override;

    property ResultTreeRoot;

    property XMLDocument : DOMString
      read GetXMLDocument;

  published

    property DocTypePublic : DOMString
      read FDocTypePublicID
      write FDocTypePublicID;

    property DocTypeSystem : DOMString
      read FDocTypeSystemID
      write FDocTypeSystemID;

    property FormattedOutput : Boolean
      read GetFormattedOutput
      write SetFormattedOutput
      default True;

    property OmitStandalone : Boolean
      read FOmitStandalone
      write FOmitStandalone
      default True;
      { Omit the standalone attribute? }

    property OmitXMLDeclaration : Boolean
      read FOmitXMLDecl
      write FOmitXMLDecl
      default False;
      { Omit the XML declaration when output? }

    property Standalone : Boolean
      read FStandalone
      write FStandalone
      default False;
      { Determines what value is output for the standalone attribute
        in the XML declaration. If set to True then 'standalone="yes"'
        is output otherwise 'standalone="no"' is output. To omit the
        standalone attribute from the prolog, set OmitStandalone to
        False. }

    property XMLVersion : DOMString
      read FVersion
      write FVersion;

  end;

implementation

uses
{$IFDEF UsingCLX}
  QGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  SysUtils,
{$IFDEF XpUseInet}
  XpInet,
{$ENDIF}
  XpExcept,
  XpXSLCon;

const
  XpsRoot = 'root';

{===TXpFilterText=====================================================}
constructor TXpFilterText.Create(oOwner : TComponent);
begin
  inherited;
  FDisplayName := 'TEXT';
  FResultTree.OutCharSet := ceISO88591;
end;
{--------}
function TXpFilterText.ftGetStringValue : DOMString;
{Rewritten !!.55}
begin
  if FResultTree.DocumentElement = nil then
    Result := ''
  else
    Result := FResultTree.DocumentElement.StringValue;
end;
{--------}
function TXpFilterText.ftGetText : DOMString;
{Rewritten !!.55}
begin
  if FResultTree.DocumentElement = nil then
    Result := ''
  else
    Result := FResultTree.DocumentElement.Text;
end;
{--------}
procedure TXpFilterText.Paint;
var
  oBit : TBitmap;
begin
  if csDesigning in ComponentState then begin
    oBit := TBitmap.Create;
    oBit.Transparent := True;
    oBit.TransparentMode := tmAuto;
    oBit.LoadFromResourceName(HInstance, 'TXpFilterText');
    Canvas.Draw(2, 2, oBit);
    oBit.Free;
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
function TXpFilterText.RenderFile(const aFileName : string) : Boolean;
begin
  { Do nothing - Text filter can't render FO.}
  Result := False;
end;
{--------}
function TXpFilterText.RenderMemory(var Buffer; aSize : Integer) : Boolean;
begin
  { Do nothing - Text filter can't render FO.}
  Result := False;
end;
{--------}
function TXpFilterText.RenderStream(aStream : TStream) : Boolean;
begin
  { Do nothing - Text filter can't render FO.}
  Result := False;
end;
{--------}
procedure TXpFilterText.Reset;
begin
  InitRegion(FResultTree, '');
  FCurElement := nil;
  FCurAttr := FCurElement;
end;
{--------}
function TXpFilterText.SaveToFile(const sFile : string) : Boolean;
var
  aStream : TFileStream;
  aFilter : TXpOutCharFilter;
{$IFDEF XpUseInet}
  sTempFile : string;
{$ENDIF}
  sText    : DOMString;
  sFileLocal: string;                                                  {!!.57}
begin
  sFileLocal := sFile;
//  sText := FResultTree.DocumentElement.StringValue;                  {Deleted !!.57}
  sText := ftGetStringValue;                                           {!!.57}
  {$IFDEF XpUseInet}
  { Saving via FTP? }
  if XpPos('FTP://', UpperCase(sFileLocal)) > 0 then begin             {!!.57}
    Delete(sFileLocal, 1, 6);                                          {!!.57}
    sTempFile := XpGenTemporaryFile;
    aStream := TFileStream.Create(sTempFile, fmOpenWrite);
    aFilter := TXpOutCharFilter.Create(aStream, Length(sText) * 2);
    try
      aFilter.WriteUTF8Signature := False;
      aFilter.Format := MapCharEncToStreamFormat(FResultTree.OutCharSet);
      aFilter.PutString(sText);
    finally
      aFilter.Free;
      aStream.Free;
    end;
    Result := XpSaveToFTP(sFileLocal, FResultTree.UserName, FResultTree.Password, {!!.57}
                          sTempFile, FResultTree.Errors);
  end else
  {$ENDIF}
  begin
    { No. Must be saving to local or network hard drive. }
    if XpPos('FILE://', UpperCase(sFileLocal)) = 1 then                {!!.57}
      Delete(sFileLocal, 1, 7);                                        {!!.57}
    aStream := TFileStream.Create(sFileLocal, fmCreate);               {!!.57}
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
  end;
end;
{--------}
function TXpFilterText.SaveToStream(aInStream : TStream) : Boolean;
var
  OutFilter : TXpOutCharFilter;
  OutText   : DOMString;
  OutLength : Integer;
begin
  Result := True;
  try
//    OutText := FResultTree.DocumentElement.StringValue;              {Deleted !!.57}
    OutText := ftGetStringValue;                                       {!!.57}
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
procedure TXpFilterText.SetBounds(aLeft, aTop, aWidth, aHeight : Integer);
begin
  if csDesigning in ComponentState then begin
    if aWidth <> 28 then
      aWidth := 28;
    if aHeight <> 28 then
      aHeight := 28;
  end else begin
    if aWidth <> 0 then
      aWidth := 0;
    if aHeight <> 0 then
      aHeight := 0;
  end;
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;
{--------}
procedure TXpFilterText.XslProcessorApplyStyleEnd(Sender: TObject);
begin
  { Do nothing }
end;
{--------}
procedure TXpFilterText.XslProcessorApplyStyleStart(Sender : TObject);
begin
  Reset;
end;
{--------}
procedure TXpFilterText.XslProcessorAttribute(oOwner        : TObject;
                                              sName, sValue : DOMString);
begin
  if FCurAttr <> nil then
    FCurAttr.SetAttribute(sName, sValue);
end;
{--------}
procedure TXpFilterText.XslProcessorCDATASection(oOwner   : TObject;
                                                 sValue   : DOMString;
                                                 bReplace : Boolean);
var
  oText : TXpCDATASection;
begin
  if (FCurElement <> nil) then begin
    { Replace special chars with entities in sValue }
    if (bReplace) then begin
      sValue := XpStringReplaceAll(sValue, '&', '&amp;');
      sValue := XpStringReplaceAll(sValue, '<', '&lt;');
    end;

    oText := FResultTree.Document.CreateCDATASection(sValue);
    FResultTree.Document.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
    oText.Release;
  end;
end;
{--------}
procedure TXpFilterText.XslProcessorElementEnd(oOwner : TObject;
                                               sValue : DOMString);
begin
  SetParentToCurrent;
end;
{--------}
procedure TXpFilterText.XslProcessorElementStart(oOwner : TObject;
                                                 sValue : DOMString);
var
  oElem : TXpElement;
begin
  oElem := FResultTree.Document.CreateElement(sValue);

  { Are we creating the root element? }
  if (FCurElement = nil) then
    { Yes. Make it a child of the document root. }
    FResultTree.Document.AppendChild(oElem)
  else
    { No. Make it a child of the current element. }
    FCurElement.AppendChild(oElem);

  FCurElement := oElem;
  FCurAttr := FCurElement;
  oElem.Release;
end;
{--------}
procedure TXpFilterText.XslProcessorFormatObject(Sender        : TObject;
                                                 aFormatObject : TXpFormattingObject);
var
  oRoot : TXpElement;
  oNode : TXpNode;
begin
  { Create the new node. }
  oNode := aFormatObject.CloneNode(False);
  try
    FResultTree.Document.ForceOwnerDocument(oNode);

    if (aFormatObject.FormatObjectType = FO_ROOT) then begin
      FResultTree.Document.AppendChild(oNode);
      FCurElement := TXpElement(oNode)
    end else
    { Are we creating the root element? }
    if (FCurElement = nil) then begin
      { Yes. Create it & make it to the document element. }
      oRoot := FResultTree.Document.CreateElement(XpsRoot);
      FResultTree.Document.AppendChild(oRoot);
      FCurElement := oRoot;
      oRoot.Release;
    end else begin
      FCurElement.AppendChild(oNode);
      FCurElement := TXpElement(oNode);
    end;
    FCurAttr := FCurElement;                                           {!!.57}
  finally
    oNode.Release;
  end;
end;
{--------}
procedure TXpFilterText.XslProcessorFormatObjectEnd(Sender: TObject);
begin
  SetParentToCurrent;
end;
{--------}
procedure TXpFilterText.XslProcessorText(oOwner : TObject;
                                         sValue : DOMString);
var
  oRoot : TXpElement;
  oText : TXpText;
begin

  { Create the text node. }
  oText := FResultTree.Document.CreateTextNode(sValue);
  try
    { Do we lack the document root? }
    if (FCurElement = nil) then begin
      { Yes. Create the document root & make it the current element. }
      oRoot := FResultTree.Document.CreateElement(XpsRoot);
      FResultTree.Document.AppendChild(oRoot);
      FCurElement := oRoot;
      oRoot.Release;
    end;

    { Add it as a child of the current node. }
    FResultTree.DocumentElement.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
  finally
    oText.Release;
  end;
end;
{--------}
procedure TXpFilterText.XslProcessorTextNode(oOwner : TObject;
                                            oNode  : TXpText);
var
  oRoot : TXpElement;
  oText : TXpNode;
begin

  { Create the text node. }
  oText := oNode.CloneNode(False);
  try
    FResultTree.Document.ForceOwnerDocument(oText);

    { Do we lack the document root? }
    if (FCurElement = nil) then begin
      { Yes. Create the document root & make it the current element. }
      oRoot := FResultTree.Document.CreateElement(XpsRoot);
      FResultTree.Document.AppendChild(oRoot);
      FCurElement := oRoot;
      oRoot.Release;
    end;

    { Add it as a child of the current node. }
    FCurElement.AppendChild(oText);
  finally
    oText.Release;
  end;
end;
{=====================================================================}

{===TXpFilterXML=======================================================}
constructor TXpFilterXML.Create(oOwner : TComponent);
begin
  inherited;
  FDisplayName := 'XML';
  {FDocTypePublicID := '';
  FDocTypeSystemID := '';}
  FOmitStandalone := True;
  {FOmitXMLDecl := False;}
  FResultTree.FormattedOutput := True;
  {FStandalone := False;}
  FVersion := XpXMLSpecification;
end;
{--------}
procedure TXpFilterXML.ConfigureProlog;
var
  bHasProlog : Boolean;
  oDocType : TXpDocumentType;
  oNode : TXpNode;
  oPI : TXpProcessingInstruction;
  wInx,
  wTargetInx : Integer;
begin
  { Look for a prolog in the result tree. }
  // Kylix 1 raises a warning that NewSize is never initialized.       {!!.55}
  // This warning is invalid. }                                        {!!.55}
  bHasProlog := False;
  wTargetInx := -1;
  for wInx := 0 to Pred(FResultTree.Document.ChildNodes.Length) do begin
    bHasProlog := (FResultTree.Document.ChildNodes.Item(wInx).NodeType =
                   PROCESSING_INSTRUCTION_NODE);
    if bHasProlog then begin
      wTargetInx := wInx;
      Break;
    end;
  end;

  { Is the prolog to be omitted? }
  if FOmitXMLDecl then begin
    { Yes. Has a prolog? }
    if bHasProlog then
      { Yes. Remove it from the result tree. }
      FResultTree.Document.ChildNodes.Delete(wTargetInx);
    Exit;
  end
  else begin
    { No. Prolog to be included. Has a prolog? }
    if bHasProlog then begin
      { Yes. Obtain a reference to the prolog. }
      oPI := TXpProcessingInstruction
               (FResultTree.Document.ChildNodes.Item(wTargetInx));
    end
    else begin
      { No. Insert a prolog. }
      oPI := FResultTree.Document.CreateProcessingInstruction(XpsXML,'');
      FResultTree.Document.ChildNodes.Insert(0, oPI);
      oPI.Release;
    end;
  end;

  { Configure the properties on the prolog. }
  { Version number }
  oPI.Data := XpsVersion + '="' + FVersion + '"';

  { Encoding }
  oPI.Data := oPI.Data + ' ' + XpsEncoding + '="' +
              XpMapCharEncToString(FResultTree.OutCharSet) + '"';

  { Standalone }
  if not FOmitStandAlone then begin
    if FStandAlone then
      oPI.Data := oPI.Data + ' ' + XpsStandAlone + '="' + XpsYes + '"'
    else
      oPI.Data := oPI.Data + ' ' + XpsStandAlone + '="' + XpsNo + '"'
  end;

  { Add a document type? }
  if (FResultTree.Document.ChildNodes.Length > 0) and
     (FDocTypeSystemID <> '') then begin
      { Yes. Locate the insertion point. }
      wTargetInx := -1;
      for wInx := 0 to Pred(FResultTree.Document.ChildNodes.Length) do begin
        oNode := FResultTree.Document.ChildNodes.Item(wInx);
        { Already has a document type? }
        if oNode.NodeType = DOCUMENT_TYPE_NODE then
          { Yes. Exit. }
          Break
        else if oNode.NodeType = ELEMENT_NODE then begin
          wTargetInx := wInx;
          Break;
        end;
      end;  { for }

      { Insertion point found? }
      if wTargetInx >= 0 then begin
        { Yes. Create a document type node & insert it. }
        oDocType := FResultTree.Document.CreateDocumentType
                      (FResultTree.Document.ChildNodes.Item(wTargetInx).NodeName,
                       False);
        oDocType.PublicID := FDocTypePublicID;
        oDocType.SystemID := FDocTypeSystemID;
        FResultTree.Document.ChildNodes.Insert(wTargetInx, oDocType);
        oDocType.Release;
      end;  { if }
    end;

end;
{--------}
function TXpFilterXML.GetFormattedOutput : Boolean;
begin
  Result := FResultTree.FormattedOutput;
end;
{--------}
function TXpFilterXML.GetXMLDocument : DOMString;
begin
  ConfigureProlog;
  Result := FResultTree.XmlDocument;
end;
{--------}
procedure TXpFilterXML.Paint;
var
  oBit : TBitmap;
begin
  if csDesigning in ComponentState then begin
    oBit := TBitmap.Create;
    oBit.Transparent := True;
    oBit.TransparentMode := tmAuto;
    oBit.LoadFromResourceName(HInstance, 'TXPFILTERXML');
    Canvas.Draw(2, 2, oBit);
    oBit.Free;
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
function TXpFilterXML.RenderFile(const aFileName : string) : Boolean;
begin
  { Do nothing - XML filter can't render FO. }
  Result := False;
end;
{--------}
function TXpFilterXML.RenderMemory(var Buffer; aSize : Integer) : Boolean;
begin
  { Do nothing - XML filter can't render FO. }
  Result := False;
end;
{--------}
function TXpFilterXML.RenderStream(aStream : TStream) : Boolean;
begin
  { Do nothing - XML filter can't render FO. }
  Result := False;
end;
{--------}
function TXpFilterXML.SaveToFile(const sFile : string) : Boolean;      {!!.57}
begin
  ConfigureProlog;
  Result := FResultTree.SaveToFile(sFile);
end;
{--------}
function TXpFilterXML.SaveToStream(aInStream : TStream) : Boolean;
begin
  ConfigureProlog;
  Result := FResultTree.SaveToStream(aInStream);
end;
{--------}
procedure TXpFilterXML.SetFormattedOutput(const bFormatted : Boolean);
begin
  FResultTree.FormattedOutput := bFormatted;
end;
{--------}
procedure TXpFilterXML.XslProcessorComment(oOwner : TObject;
                                           sValue : DOMString);
var
  oRoot : TXpElement;
  oComment : TXpComment;
begin

  { Create the comment node. }
  oComment := FResultTree.Document.CreateComment(sValue);
  try
    FResultTree.Document.ForceOwnerDocument(oComment);

    { Do we lack the document root? }
    if (FCurElement = nil) then begin
      { Yes. Create the document root & make it the current element. }
      oRoot := FResultTree.Document.CreateElement(XpsRoot);
      FResultTree.Document.AppendChild(oRoot);
      FCurElement := oRoot;
      oRoot.Release;
    end;

    { Add it as a child of the current node. }
    FCurElement.AppendChild(oComment);
  finally
    oComment.Release;
  end;
end;
{--------}
procedure TXpFilterXML.XslProcessorProcessingInstruction(oOwner : TObject;
                                                         sName,
                                                         sValue : DOMString);
var
  oRoot : TXpElement;
  oPI : TXpProcessingInstruction;
begin

  { Create the comment node. }
  oPI := FResultTree.Document.CreateProcessingInstruction(sName, sValue);
  try
    FResultTree.Document.ForceOwnerDocument(oPI);

    { Do we lack the document root? }
    if (FCurElement = nil) then begin
      { Yes. Create the document root & make it the current element. }
      oRoot := FResultTree.Document.CreateElement(XpsRoot);
      FResultTree.Document.AppendChild(oRoot);
      FCurElement := oRoot;
      oRoot.Release;
    end;

    { Add it as a child of the current node. }
    FCurElement.AppendChild(oPI);
  finally
    oPI.Release;
  end;
end;
{=====================================================================}
end.
