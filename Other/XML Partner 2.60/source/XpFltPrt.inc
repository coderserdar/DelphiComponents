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
{* XMLPartner Pro: XpFltPrt.INC                          *}
{*********************************************************}
{* XMLPartner Pro: Implements Printer Filter for XSL     *}
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
  Classes,
  XpBase,
  XpDOM;

type
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

{== TXpXSLFilterPrint ================================================}
  TXpFilterPrint = class(TXpFilterBase)
  protected {private}                                                  {!!.55}
    { Private declarations }
    FBitmap         : TBitmap;
    FHardwareHeight : Integer;
    FHardwareWidth  : Integer;
    FHSlideFactor   : Integer;
    FHSlidePosition : Integer;
    FInText         : Boolean;
    {FOrient         : Integer;}
    {FPaper          : Integer;}
    FPrinting       : Boolean;
    FRefreshSettings : Boolean;                                        {!!.58}
    FRenderedDoc    : TXpObjModel;
    FRightText      : DOMString;
    FVSlideFactor   : Integer;
    FVSlidePosition : Integer;

    Context             : TXpContextStack;

    procedure fpSaveColumnAttrs(const aSourceElem : TXpElement);
    procedure RenderHeader(const oPageInfo : RPageInfo);
    procedure RenderFooter(const oPageInfo : RPageInfo);
    procedure RenderStartSide(const oPageInfo : RPageInfo);
    procedure RenderEndSide(const oPageInfo : RPageInfo);
    procedure SetPageCurrent(const Value : Integer);
    function RenderTree(oDrawArea : TXpDrawArea;
                        oTree     : TXpElement;
                        oAttrs    : TXpFOPropertyGroup;
                        oPage     : TXpElement;
                        bTop      : Boolean) : Boolean;                {!!.55}
    procedure RenderText(oDrawArea : TXpDrawArea;
                         oAttrs    : TXpFOPropertyGroup;
                         sText     : string);
    procedure RenderNewline(oDrawArea : TXpDrawArea;
                            oAttrs    : TXpFOPropertyGroup);
    procedure RenderImage(oDrawArea : TXpDrawArea;
                          oAttrs    : TXpFOPropertyGroup); virtual;    {!!.55}
    procedure SetVSlidePosition(const Value : Integer);
    procedure RenderGraphicLine(oDrawArea : TXpDrawArea;
                                oAttrs    : TXpFOPropertyGroup);
    procedure FigureTableInfo(oDrawArea : TXpDrawArea;
                              oTree     : TXpElement;
                              oAttrs    : TXpFOPropertyGroup);
    procedure PageinateDocument;
    function GetRange(var sPages : string;
                      var wStart,
                          wEnd   : Integer) : Boolean;
    procedure FigureFont(oFont : TFont; oAttrs : TXpFOPropertyGroup);
    procedure SetHSlidePosition(const Value : Integer);
  protected
    { Protected declarations }
    procedure fpCopyAttrsToCurrent(const aSourceElem : TXpElement);
    procedure fpCreateNonFOResultTree;
    procedure Paint; override;
    procedure RenderPage(wPage : Integer); virtual;
    {$IFDEF Delphi3OrBCB3}
    procedure Resize;
    {$ELSE}
    procedure Resize; override;
    {$ENDIF}
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;
    procedure Print(sPages : string);
    function RenderFile(const aFileName : string) : Boolean; override;
    function RenderMemory(var Buffer; aSize : Integer) : Boolean; override;
    function RenderStream(aStream : TStream) : Boolean; override;
    procedure Reset; override;
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

    property HSlideFactor : Integer
      read FHSlideFactor;

    property PageCount : Integer
      read FPageCount;

    property VSlideFactor : Integer
      read FVSlideFactor;
  published
    { Published declarations }
    property Align;
    {$IFDEF DCC4OrLater}
    {$IFNDEF UsingCLX}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    {$ENDIF}
    {$ENDIF}
    property HSlidePosition : Integer
      read FHSlidePosition
      write SetHSlidePosition;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DCC4OrLater}
    property OnResize;
    {$ENDIF}
    property PageCurrent : Integer
      read FPageCurrent
      write SetPageCurrent;
{Begin !!.58}
    property RefreshSettings : Boolean
      read FRefreshSettings write FRefreshSettings default False;
      { By default, the print filter uses the printer settings (e.g.,
        orientation, page size) of the default printer at the time the
        print filter was created. If the printer settings are changed via
        the operating system printer properties dialog then the new settings
        are not used by the print filter. However, changes made via the
        TPrinterSetupDialog are reflected in the print filter.

        To have the print filter update itself to use the latest printer
        settings as controlled via the operating system printer properties
        dialog, set this property to the value True. }
{End !!.58}
    property VSlidePosition : Integer
      read FVSlidePosition
      write SetVSlidePosition;
  end;
{=====================================================================}
  TDrawImageFunc = function(cpFile    : PChar;
                            oCanvas   : TCanvas;
                            oParent   : TWinControl;
                            wXPos,
                            wYPos     : Integer;
                        var awWidth,
                            awHeight  : Integer;
                            bPrinting : Boolean) : Boolean; stdcall;
{=====================================================================}

implementation

uses
  Sysutils,
  Winspool,
  Math,
{$IFDEF UsingCLX}
  QPrinters,
  XpQXSLPr,
{$ELSE}
  Printers,
  XpvXSLPr,
{$ENDIF}
  XPXSLCon;


{== TXpFilterPrint ===================================================}
constructor TXpFilterPrint.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);
  FDisplayName := 'PRINT';
  FRightText := '';

  FOutRegionAfterRoot := XpsSPAN;
  FOutRegionBeforeRoot := XpsSPAN;
  FOutRegionBodyRoot := XpsSPAN;
  FOutRegionEndRoot := XpsSPAN;
  FOutRegionStartRoot := XpsSPAN;
  FOutTitleRoot := XpsSPAN;

  FCurElement := nil;
  FRefreshSettings := False;                                           {!!.58}
  FPageCurrent := 1;
  {FPageCount := 0;}
  FBitmap := TBitmap.Create;
  FRenderedDoc := nil;
  {
  FVSlidePosition := 0;
  FVSlideFactor := 0;
  FHSlidePosition := 0;
  FHSlideFactor := 0;
  FPrinting := False;
  }
  Align := alClient;
  Context := TxpContextStack.Create;
end;
{--------}
destructor TXpFilterPrint.Destroy;
begin
  Context.Free;
  FBitmap.Free;
  FBitmap := nil;

  FRenderedDoc.Free;
  FRenderedDoc := nil;

  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;

  inherited Destroy;
end;
{--------}
function TXpFilterPrint.GetRange(var sPages : string;
                                 var wStart,
                                     wEnd   : Integer) : Boolean;
var
  sTerm : string;
begin
  wStart := -1;
  while (Length(sPages) > 0) and
        not XpIsNumeric(sPages[1]) do
    Delete(sPages, 1, 1);

  if (sPages = '') then begin
    Result := False;
    Exit;
  end;

  sTerm := '';
  while ((Length(sPages) > 0) and
         (XpIsNumeric(sPages[1]))) do begin
    sTerm := (sTerm + sPages[1]);
    Delete(sPages, 1, 1);
  end;
  wStart := StrToIntDef(sTerm, -1);
  wEnd := wStart;

  while (Length(sPages) > 0) and
        (sPages[1] = ' ') do
    Delete(sPages, 1, 1);

  if (Length(sPages) > 0) then begin
    if (sPages[1] = '-') then begin
      while ((Length(sPages) > 0) and
             (not XpIsNumeric(sPages[1]))) do
        Delete(sPages, 1, 1);
      sTerm := '';
      while ((Length(sPages) > 0) and
             (XpIsNumeric(sPages[1]))) do begin
        sTerm := sTerm + sPages[1];
        Delete(sPages, 1, 1);
      end;
      wEnd := StrToIntDef(sTerm, wStart);
    end;
  end;
  Result := (wStart <> -1);
end;
{--------}
procedure TXpFilterPrint.FigureFont(oFont  : TFont;
                                    oAttrs : TXpFoPropertyGroup);
var
  wRed,
  wGreen,
  wBlue  : Integer;
begin
  oFont.Name := oAttrs.GetProperty(XpsFontFamily);
  oFont.Size := EvalPropAsNum(oAttrs.GetProperty(XpsFontSize), fuPoints, 1);
  oFont.Color :=
    XpEvaluateColor(oAttrs.GetProperty(XpsColor), wRed, wGreen, wBlue);

  if (oAttrs.GetProperty(XpsFontWeight) = 'bold') then
    oFont.Style := oFont.Style + [fsBold]
  else
    oFont.Style := oFont.Style - [fsBold];

  if (oAttrs.GetProperty(XpsFontStyle) = 'italic') then
    oFont.Style := oFont.Style + [fsItalic]
  else
    oFont.Style := oFont.Style - [fsItalic];
end;
{--------}
procedure TXpFilterPrint.FigureTableInfo(oDrawArea : TXpDrawArea;
                                         oTree     : TXpElement;
                                         oAttrs    : TXpFoPropertyGroup);
var
  oList,
  oRowsList,
  oCellsList : TXpNodeList;
  oTmp,
  oRow,
  oCell      : TXpElement;
  i, j, k, l,
  wDiv,
  wWidth,
  wSum,
  wZeros,
  wCurCols   : Integer;
  sTmp       : string;
  xWidth,
  ColSpan,
  RowSpan    : Integer;
  CA, CA2: TXpTableCellArea;
  R, R2: TXpTableRowArea;
  Borders, Padding: TXpCellBorders;
begin
  { Set table information }
  xWidth :=
    EvalPropAsNum(oAttrs.GetProperty(XpsWidth), fuPixels, 0);
  if xWidth <> 0 then
    oDrawArea.Table.Width := xWidth
  else
    oDrawArea.Table.Width := oDrawArea.Right - oDrawArea.Left;

  { Enumerate through child rows first to add rows}
  oRowsList := oTree.SelectNodes(XpsTR);
  if oRowsList.Length <> 0 then begin
    for i := 0 to (oRowsList.Length - 1) do begin
      oDrawArea.Table.TableRows.Add(TXpTableRowArea.Create(TXpElement(oRowsList.Item(i))));
    end;
    oRowsList.Free;
  end else begin
    oRowsList.Free;
    {no rows immediately below the table node}
    {see if we have head, body, or footer nodes}
    oList := oTree.SelectNodes('thead');
    if oList.Length <> 0 then
      for i := 0 to (oList.Length - 1) do begin
        oTmp := TXpElement(oList.Item(i));
        oRowsList := oTmp.SelectNodes(XpsTR);
        for j := 0 to (oRowsList.Length - 1) do
          oDrawArea.Table.TableRows.Add(TXpTableRowArea.
            Create(TXpElement(oRowsList.Item(j))));
        oRowsList.Free;
      end;
    oList.Free;
    oList := oTree.SelectNodes('tbody');
    if oList.Length <> 0 then
      for i := 0 to (oList.Length - 1) do begin
        oTmp := TXpElement(oList.Item(i));
        oRowsList := oTmp.SelectNodes(XpsTR);
        for j := 0 to (oRowsList.Length - 1) do
          oDrawArea.Table.TableRows.Add(TXpTableRowArea.
            Create(TXpElement(oRowsList.Item(j))));
        oRowsList.Free;
      end;
    oList.Free;
    oList := oTree.SelectNodes('tfoot');
    if oList.Length <> 0 then
      for i := 0 to (oList.Length - 1) do begin
        oTmp := TXpElement(oList.Item(i));
        oRowsList := oTmp.SelectNodes(XpsTR);
        for j := 0 to (oRowsList.Length - 1) do
          oDrawArea.Table.TableRows.Add(TXpTableRowArea.
            Create(TXpElement(oRowsList.Item(j))));
        oRowsList.Free;
      end;
    oList.Free;
  end;

  { Enumerate through child rows now to add table cells}
  for i := 0 to oDrawArea.Table.TableRows.Count - 1 do begin
    oDrawArea.Table.SelectRow := i;
    oRow := oDrawArea.Table.CurrentRow.RowElement;
    R := oDrawArea.Table.CurrentRow;
    R.Height := EvalPropAsNum(oRow.GetAttribute('height'), fuPixels, 0);
    oCellsList := oRow.SelectNodes(XpsTD);
    { Enumerate through child cells }
    for j := 0 to (oCellsList.Length - 1) do begin
      oCell := TXpElement(oCellsList.Item(j));
      RowSpan := StrToIntDef(oCell.GetAttribute('rowspan'), 1);
      ColSpan :=
        StrToIntDef(oCell.GetAttribute('colspan'), 1);
      wWidth :=
        EvalPropAsNum(oCell.GetAttribute('border-width'), fuPixels, 0);
      if (wWidth > 0) then begin
        Borders.TW := wWidth;
        Borders.LW := wWidth;
        Borders.RW := wWidth;
        Borders.BW := wWidth;
      end else begin
        Borders.TW := EvalPropAsNum(oCell.GetAttribute(XpsBorderTopWidth), fuPixels, 0);
        Borders.LW := EvalPropAsNum(oCell.GetAttribute(XpsBorderLeftWidth), fuPixels, 0);
        Borders.RW := EvalPropAsNum(oCell.GetAttribute(XpsBorderRightWidth), fuPixels, 0);
        Borders.BW := EvalPropAsNum(oCell.GetAttribute(XpsBorderBottomWidth), fuPixels, 0);
      end;
      wWidth := EvalPropAsNum(oCell.GetAttribute('padding'),
                              fuPixels, 1);
      if (wWidth > 0) then begin
        Padding.TW := wWidth;
        Padding.LW := wWidth;
        Padding.RW := wWidth;
        Padding.BW := wWidth;
      end else begin
        Padding.TW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingTop), fuPixels, 0);
        Padding.LW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingLeft), fuPixels, 0);
        Padding.RW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingRight), fuPixels, 0);
        Padding.BW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingBottom), fuPixels, 0);
      end;
      for l := 1 to ColSpan do begin
        CA := TXpTableCellArea.Create;
        CA.NoLeft := l > 1;
        CA.NoRight := l < ColSpan;
        CA.Column := R.FirstAvailColumn(j);
        R.AddOrdered(CA);
        { Cell borders }
        CA.Borders := Borders;
        CA.Padding := Padding;
        for k := 2 to RowSpan do begin
          CA2 := TXpTableCellArea.Create;
          CA2.NoTop := True;
          CA2.NoBottom := k < RowSpan;
          R2 := oDrawArea.Table.Row[i + k - 1];
          CA2.Column := R2.FirstAvailColumn(CA.Column);
          CA2.Borders := Borders;
          CA2.Padding := Padding;
          R2.AddOrdered(CA2);
        end;
        if RowSpan > 1 then
          CA.NoBottom := True;
        sTmp := oCell.GetAttribute(XpsWidth);
        if ((sTmp = '') or
            (sTmp = '*')) then
          CA.Width := 0
        else
          CA.Width :=
            EvalPropAsNum(sTmp,
                          fuPixels,
                          oDrawArea.Right - oDrawArea.Left + 1);
        if R.Height <> 0 then
          CA.HeightSpc := R.Height
        else
          CA.HeightSpc :=
            EvalPropAsNum(oCell.GetAttribute(XpsHeight), fuPixels, 0);
        if (CA.HeightSpc >
            R.Height) then
          R.Height := CA.HeightSpc;
      end;
    end;
    oCellsList.Free;
  end;

  { Go back through rows and cells and adjust cell widths and positions }
  for i := 0 to (oDrawArea.Table.TableRows.Count - 1) do begin
    oDrawArea.Table.SelectRow := i;
    oDrawArea.Table.CurrentRow.Top :=
      oDrawArea.Top + i * FCurCanvas.TextHeight(' ');
    oDrawArea.Table.CurrentRow.Height := FCurCanvas.TextHeight(' ');
    wWidth := oDrawArea.Table.Width;
    wSum := 0;
    wZeros := 0;
    wCurCols := oDrawArea.Table.CurrentRow.TableCells.Count;
    if (wCurCols > 0) then begin
      { Figure sizes of columns }
      for j := 0 to (wCurCols - 1) do begin
        oDrawArea.Table.CurrentRow.SelectCell := j;
        CA := oDrawArea.Table.CurrentRow.CurrentCell;
        if (CA.Width = 0) then
          Inc(wZeros)
        else
          Inc(wSum, CA.Width);
      end;
      if (wSum > wWidth + 2) then begin
        wWidth := wSum + 2;
        { All cells are the same }
        wDiv := wWidth div oDrawArea.Table.CurrentRow.TableCells.Count;
        wSum := 0;
        for j := 0 to
            (oDrawArea.Table.CurrentRow.TableCells.Count - 2) do begin
          oDrawArea.Table.CurrentRow.SelectCell := j;
          oDrawArea.Table.CurrentRow.CurrentCell.Width := wDiv;
          Inc(wSum, wDiv);
        end;
        oDrawArea.Table.CurrentRow.SelectCell :=
          (oDrawArea.Table.CurrentRow.TableCells.Count - 1);
        oDrawArea.Table.CurrentRow.CurrentCell.Width := (wWidth - wSum);
        wSum := wWidth;
      end else if (wZeros > 0) then begin
        wDiv := (wWidth - wSum) div wZeros;
        wSum := 0;
        for j := 0 to
            (oDrawArea.Table.CurrentRow.TableCells.Count - 1) do begin
          oDrawArea.Table.CurrentRow.SelectCell := j;
          CA := oDrawArea.Table.CurrentRow.CurrentCell;
          if (CA.Width = 0) then begin
            CA.Width := wDiv;
            Inc(wSum, wDiv);
          end else
            Inc(wSum, CA.Width);
        end;
      end;
      if (wSum < xWidth) then begin                                    {!!.57}
        oDrawArea.Table.CurrentRow.SelectCell :=
          oDrawArea.Table.CurrentRow.TableCells.Count - 1;
        CA := oDrawArea.Table.CurrentRow.CurrentCell;
        CA.Width := CA.Width + (wWidth - wSum);
      end;

      { Loop through and set positions }
      wSum := oDrawArea.Left;
      for j := 0 to (wCurCols - 1) do begin
        oDrawArea.Table.CurrentRow.SelectCell := j;
        CA := oDrawArea.Table.CurrentRow.CurrentCell;
        CA.Left := wSum;
        Inc(wSum, CA.Width);
      end;
    end;
  end;
  if oDrawArea.Table.TableRows.Count > 0 then begin
    {fix up sizes and offsets}
    R := oDrawArea.Table.Row[0];
    for i := 1 to oDrawArea.Table.TableRows.Count - 1 do begin
      R2 := oDrawArea.Table.Row[i];
      for j := 0 to R2.TableCells.Count - 1 do begin
        if j < R.TableCells.Count then begin
          R2.Cell[j].Left := R.Cell[j].Left;
          R2.Cell[j].Width := R.Cell[j].Width;
        end else
        if j > 0 then begin
          R2.Cell[j].Left := R2.Cell[j-1].Left + R2.Cell[j-1].Width;
          R2.Cell[j].Width := R2.Cell[j-1].Width;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TXpFilterPrint.fpCopyAttrsToCurrent(const aSourceElem : TXpElement);
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
procedure TXpFilterPrint.fpCreateNonFOResultTree;
var
  oRoot : TXpElement;
begin
  oRoot := FResultTree.Document.CreateElement(XpsSPAN);
  FResultTree.Document.AppendChild(oRoot);
  FCurElement := oRoot;
  oRoot.Release;
end;
{--------}
procedure TXpFilterPrint.PageinateDocument;
var
  wPage,
  wPageWidth,
  wPageHeight,
  i,
  CurPageRefIdx,
  RepeatCount    : Integer;
  bLoop          : Boolean;
  oDrawArea      : TXpDrawArea;
  oNewAttrs      : TXpFoPropertyGroupText;
  oPage          : TXpElement;
  oDocFrag       : TXpDocumentFragment;
  TempRegBody    : TXpFORegionBody;
begin
  wPage := 0;
  oDocFrag := FRenderedDoc.Document.CreateDocumentFragment;

  if (HasOutRegionBefore) then
    OutRegionBefore.IgnoreCase := True;
  if (HasOutRegionAfter) then
    OutRegionAfter.IgnoreCase := True;
  if (HasOutRegionBody) then
    OutRegionBody.IgnoreCase := True;
  if (HasOutRegionStart) then
    OutRegionStart.IgnoreCase := True;
  if (HasOutRegionEnd) then
    OutRegionEnd.IgnoreCase := True;
  if (HasOutRegionEnd) then
    OutTitle.IgnoreCase := True;

  if (ResultIsFO) then begin
    for i := 0 to (TXpFORoot(ResultTreeRoot).PageSequences.Length - 1) do begin
      FPageSequenceCount := i + 1;
      { Each root may have many page sequences.}
      FCurPageSequence := TXpFOPageSequence(TXpFORoot(ResultTreeRoot).PageSequences.Item(i));
      FCurPageSeqMaster :=
        TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageSequenceMaster(FCurPageSequence.GetPropertyValue(XpsMasterName));
      CurPageRefIdx := 0;
      RepeatCount := -1;
      bLoop := True;

      while bLoop do begin
        oPage := FRenderedDoc.Document.CreateElement('page');
        Inc(wPage);

        fbSelectPageMaster(CurPageRefIdx, wPage);
        { If we couldn't find a page master, there's nothing more we
          can do. }
        if (FCurPageMaster = nil) then                                 {!!.57}
          Exit;                                                        {!!.57}

        { Store the page master used with this page so we don't have to
          look it up again later.}
        oPage.SetAttribute('PageMaster',
                           FCurPageMaster.GetPropertyValue(XpsMasterName));
        oPage.SetAttribute('XpPageSeqNum', IntToStr(FPageSequenceCount));

        wPageWidth :=
          EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                        fuPixels,
                        FBitmap.Width);
        wPageWidth := XpMin(wPageWidth, FHardwareWidth);

        wPageHeight :=
          EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                        fuPixels,
                        FBitmap.Height);
        wPageHeight := XpMin(wPageHeight, FHardwareHeight);

        oDrawArea := TXpDrawArea.Create;
        try
          { Get the top margin.}
          oDrawArea.Top := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop),
                                         fuPixels, 1);

          if (FCurPageMaster.HasRegionBefore) then
            oDrawArea.Top := oDrawArea.Top +
                             EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                                           fuPixels, 1);

          { Get the bottom margin.}
          oDrawArea.Bottom :=
            wPageHeight -
            EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                          fuPixels, 1);
          if (FCurPageMaster.HasRegionAfter) then
            oDrawArea.Bottom := oDrawArea.Bottom -
                                EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                              fuPixels, 1);

          { Get the right margin.}
          oDrawArea.Right :=
            wPageWidth -
            EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                          fuPixels, 1);
           if (FCurPageMaster.HasRegionEnd) then
             oDrawArea.Right := oDrawArea.Right -
                                EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                              fuPixels, 1);

          { Get the left margin.}
          oDrawArea.Left :=
            EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPixels, 1);
          if (FCurPageMaster.HasRegionStart) then
            oDrawArea.Left := oDrawArea.Left +
                              EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                              fuPixels, 1);

          oDrawArea.XPos := oDrawArea.Left;
          oDrawArea.YPos := oDrawArea.Top;
          oDrawArea.YIncr := 0;

          oNewAttrs := TXpFoPropertyGroupText.Create;
          try
            oNewAttrs.DoInherit(FCurPageMaster.Attributes);
            bLoop := RenderTree(oDrawArea,
                                FOutRegionBody.DocumentElement,
                                oNewAttrs,
                                oPage,
                                True);
          finally
            oNewAttrs.Free;
          end;
        finally
          oDrawArea.Free;
        end;

        oDocFrag.AppendChild(oPage);
        oPage.Release;

        { If we're using a page sequence master, decrement the number
          of pages the current page master can be used for.}
        if (FCurPageSeqMaster <> nil) then begin
          RepeatCount := RepeatCount - 1;
          { Have we reached the limit for this page master? }
          if (RepeatCount = 0) then begin
            { Yes. Let's get the next page master.}
            RepeatCount := fbSelectPageMaster(CurPageRefIdx, wPage);
            CurPageRefIdx := CurPageRefIdx + 1;
          end;
        end;
      end;
    end;
  end else begin
    { Create a default PageMaster with only a body region.}
    FCurPageMaster := TXpFOSimplePageMaster.Create;
    bLoop := True;
    try
      TempRegBody := TXpFORegionBody.Create;
      try
        FCurPageMaster.AppendChild(TempRegBody);

        while bLoop do begin
          oPage := FRenderedDoc.Document.CreateElement('page');
          Inc(wPage);

          wPageWidth := FHardwareWidth;
          wPageHeight := FHardwareHeight;

          oDrawArea := TXpDrawArea.Create;
          try
            oDrawArea.Top := 94;
            oDrawArea.Bottom := wPageHeight - 94;
            oDrawArea.Right := wPageWidth - 94;
            oDrawArea.Left := 94;
            oDrawArea.XPos := oDrawArea.Left;
            oDrawArea.YPos := oDrawArea.Top;
            oDrawArea.YIncr := 0;

            { Default page size and margins.}
            FCurPageMaster.SetPropertyValue(XpsPageWidth, '300mm', True);
            FCurPageMaster.SetPropertyValue(XpsPageHeight, '400mm', True);
            FCurPageMaster.SetPropertyValue(XpsMarginTop, '25mm', True);
            FCurPageMaster.SetPropertyValue(XpsMarginBottom, '25mm', True);
            FCurPageMaster.SetPropertyValue(XpsMarginRight, '25mm', True);
            FCurPageMaster.SetPropertyValue(XpsMarginLeft, '25mm', True);

            oNewAttrs := TXpFoPropertyGroupText.Create;
            try
              oNewAttrs.DoInherit(FCurPageMaster.Attributes);
              bLoop := RenderTree(oDrawArea,
                                  FOutRegionBody.DocumentElement,
                                  oNewAttrs,
                                  oPage,
                                  True);
            finally
              oNewAttrs.Free;
            end;
          finally
            oDrawArea.Free;
          end;
          oDocFrag.AppendChild(oPage);
          oPage.Release;
        end;
      except
        TempRegBody.Free;
      end;
    except
      FCurPageMaster.Free;
      FCurPageMaster := nil;
    end;
  end;

  FRenderedDoc.Document.DocumentElement.AppendChild(oDocFrag);
  oDocFrag.Free;

  FPageCount := wPage;
  FPageCurrent := 1;
end;
{--------}
procedure TXpFilterPrint.Paint;
begin
  inherited;
  if (FBitmap <> nil) then
    BitBlt(Canvas.Handle,
           0,
           0,
           Width,
           Height,
           FBitmap.Canvas.Handle,
           FHSlidePosition,
           FVSlidePosition,
           SRCCOPY);
end;
{--------}
procedure TXpFilterPrint.Print(sPages : string);
var
  Device, Name, Port: array[0..100] of Char;                           {!!.58}
  DevMode: THandle;                                                    {!!.58}
  i,
  wStart,
  wEnd          : Integer;
  oPageList     : TStringList;
begin
  if (PageCount > 0) then begin
{Begin !!.58}
    if FRefreshSettings then begin
      Printer.GetPrinter(Device, Name, Port, DevMode);
      Printer.SetPrinter(Device, Name, Port, 0);
    end;
{End !!.58}
    Printer.BeginDoc;
    try
      FPrinting := True;
      FHPixelsPerInch := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
      FVPixelsPerInch := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY);
      FCurCanvas := Printer.Canvas;
      FHardwareWidth := Printer.PageWidth;
      FHardwareHeight := Printer.PageHeight;
      oPageList := TStringList.Create;
      try
        if (sPages = '') then begin
          for i := 1 to FPageCount do
            oPageList.Add(IntToStr(i));
        end else begin
          while GetRange(sPages, wStart, wEnd) do begin
            for i := wStart to wEnd do begin
              if (i > 0) and (i <= FPageCount) then
                oPageList.Add(IntToStr(i));
            end;
          end;
        end;

        for i := 0 to (oPageList.Count - 1) do begin
          FPageCurrent := i + 1;
          RenderPage(StrToInt(oPageList[i]));
          if (i <> oPageList.Count - 1) then
            Printer.NewPage;
        end;
        if (oPageList.Count > 0) then
          FPageCurrent := StrToInt(oPageList[0]);
        Printer.EndDoc;
      finally
        oPageList.Free;
      end;
    except
      Printer.Abort;
    end;

    FPrinting := False;
    FHPixelsPerInch := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
    FVPixelsPerInch := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
    FCurCanvas := FBitmap.Canvas;
    FHardwareWidth := FBitmap.Width;
    FHardwareHeight := FBitmap.Height;
  end;
end;
{--------}
procedure TXpFilterPrint.RenderEndSide(const oPageInfo : RPageInfo);
var
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oElem     : TXpElement;
  j         : Integer;
begin
  if (HasOutRegionEnd) then begin
    FBlockLevel := 0;
    oDrawArea := TXpDrawArea.Create;
    try
      oDrawArea.Top :=
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop), fuPixels, 1);
      oDrawArea.Bottom :=
        oPageInfo.PageHeight -
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom), fuPixels, 1);
      oDrawArea.Right :=
        oPageInfo.PageWidth -
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPixels, 1);
      oDrawArea.Left :=
        oDrawArea.Right -
        EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent), fuPixels, 1);
      if ((FCurPageMaster.HasRegionBefore) and
          ((FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) <> 'true') or
           (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'false'))) then
        Inc(oDrawArea.Top,
            EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                          fuPixels, 1));
      if ((FCurPageMaster.HasRegionAfter) and
          ((FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) <> 'true') or
           (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'false'))) then
        Dec(oDrawArea.Bottom,
            EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                          fuPixels, 1));
      oDrawArea.XPos := oDrawArea.Left;
      oDrawArea.YPos := oDrawArea.Top;
      oDrawArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionEnd.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to oElem.Attributes.Length - 1 do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        RenderTree(oDrawArea,
                   FOutRegionEnd.DocumentElement,
                   oNewAttrs, nil, False);
      finally
        oNewAttrs.Free;
      end;
    finally
      oDrawArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
function TXpFilterPrint.RenderFile(const aFileName : string) : Boolean;
begin
  Result := inherited RenderFile(aFileName);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
function TXpFilterPrint.RenderMemory(var Buffer; aSize : Integer) : Boolean;
begin
  Result := inherited RenderMemory(Buffer, aSize);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
function TXpFilterPrint.RenderStream(aStream : TStream) : Boolean;
begin
  Result := inherited RenderStream(aStream);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
procedure TXpFilterPrint.RenderFooter(const oPageInfo : RPageInfo);
var
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oElem     : TXpElement;
  j         : Integer;
begin
  if (HasOutRegionAfter) then begin
    FBlockLevel := 0;
    oDrawArea := TXpDrawArea.Create;
    try
      { Get bottom margin.}
      oDrawArea.Bottom :=
        oPageInfo.PageHeight -
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom), fuPixels, 1);

      { Get top margin.}
      { Note: We know there is a FCurPageMaster.RegionAfter. The next
              line just ensures it's loaded.}
      if (FCurPageMaster.HasRegionAfter) then
        oDrawArea.Top := oDrawArea.Bottom -
                         EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                       fuPixels, 1);

      { Get left margin.}
      oDrawArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                      fuPixels, 1);

      { Get right margin.}
      oDrawArea.Right :=
        oPageInfo.PageWidth -
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPixels, 1);

      if (FCurPageMaster.HasRegionBefore) and
         (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = 'false') then begin
        if (FCurPageMaster.HasRegionStart) then
          oDrawArea.Left := oDrawArea.Left +
                            EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                          fuPixels, 1);
        if (FCurPageMaster.HasRegionEnd) then
          oDrawArea.Right := oDrawArea.Right -
                             EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                           fuPixels, 1);
      end;

      oDrawArea.XPos := oDrawArea.Left;
      oDrawArea.YPos := oDrawArea.Top;
      oDrawArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(OutRegionAfter.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        RenderTree(oDrawArea,
                   FOutRegionAfter.DocumentElement,
                   oNewAttrs, nil, False);
      finally
        oNewAttrs.Free;
      end;
    finally
      oDrawArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterPrint.RenderGraphicLine(oDrawArea : TXpDrawArea;
                                           oAttrs    : TXpFoPropertyGroup);
var
  wWidth,
  wRed,
  wGreen,
  wBlue  : Integer;
begin
  FCurCanvas.Pen.Style := psSolid;
  FCurCanvas.Pen.Width :=
    EvalPropAsNum(oAttrs.GetProperty(XpsRuleThickness), fuPixels, 1);
  FCurCanvas.Pen.Color :=
    XpEvaluateColor(oAttrs.GetProperty(XpsColor), wRed, wGreen, wBlue);

  wWidth := EvalPropAsNum(oAttrs.GetProperty(XpsLeaderLength), fuPixels, 1);
  if (wWidth = 0) then
    wWidth := oDrawArea.Right - oDrawArea.Left;

  oDrawArea.YIncr := FCurCanvas.TextHeight(' ');
  FCurCanvas.MoveTo(oDrawArea.XPos,
                    oDrawArea.YPos + (oDrawArea.YIncr shr 1));
  FCurCanvas.LineTo(oDrawArea.XPos + wWidth,
                    oDrawArea.YPos + (oDrawArea.YIncr shr 1));
  Inc(oDrawArea.XPos, wWidth);
end;
{--------}
procedure TXpFilterPrint.RenderHeader(const oPageInfo : RPageInfo);
var
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oElem     : TXpElement;
  j         : Integer;
begin
  if (HasOutRegionBefore) then begin
    FBlockLevel := 0;
    oDrawArea := TXpDrawArea.Create;
    try
      { Get top margin.}
      oDrawArea.Top :=
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop),
                      fuPixels, 1);

      { Get bottom margin.}
      oDrawArea.Bottom := oDrawArea.Top;
      if (FCurPageMaster.HasRegionBefore) then
        oDrawArea.Bottom := oDrawArea.Bottom  +
                            EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                                          fuPixels, 1);

      { Get left margin.}
      oDrawArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                      fuPixels, 1);

      { Get right margin.}
      oDrawArea.Right := oPageInfo.PageWidth -
                         EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                         fuPixels, 1);

      if (FCurPageMaster.HasRegionBefore) and
         (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = 'false') then begin
        oDrawArea.Left := oDrawArea.Left +
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                        fuPixels, 1);
        if (FCurPageMaster.HasRegionStart) then
          oDrawArea.Left := oDrawArea.Left +
                            EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                          fuPixels, 1);
        if (FCurPageMaster.HasRegionEnd) then
          oDrawArea.Right := oDrawArea.Right -
                             EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                           fuPixels, 1);
        if (FCurPageMaster.HasRegionBody) then
          oDrawArea.Right := oDrawArea.Right -
                             EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                             fuPixels, 1);
      end;
      oDrawArea.XPos := oDrawArea.Left;
      oDrawArea.YPos := oDrawArea.Top;
      oDrawArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionBefore.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        RenderTree(oDrawArea,
                   FOutRegionBefore.DocumentElement,
                   oNewAttrs, nil, False);
      finally
        oNewAttrs.Free;
      end;
    finally
      oDrawArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterPrint.RenderImage(oDrawArea : TXpDrawArea;
                                     oAttrs    : TXpFoPropertyGroup);
{var
  sFile    : string;
  wWidth,
  wHeight  : Integer;
  hLib     : THandle;
  P        : Pointer;
  DrawFunc : TDrawImageFunc;  }
begin
  { NOTE: Not supported at this time
  sFile := oAttrs.GetProperty('image');
  if FileExists(sFile) then begin
    wWidth := 0;
    wHeight := 0;
    if oAttrs.GetProperty('max-width') <> '' then
      wWidth := EvalPropAsNum(oAttrs.GetProperty('max-width'), fuPixels, 1);
    if oAttrs.GetProperty('max-height') <> '' then
      wHeight := EvalPropAsNum(oAttrs.GetProperty('max-height'), fuPixels, 1);
      hLib := LoadLibrary('CSImageP.DLL');
    if hLib <> 0 then begin
      P := GetProcAddress(hLib, 'DrawImage');
      DrawFunc := P;
      DrawFunc(PChar(sFile),
               FCurCanvas,
               self,
               oDrawArea.XPos,
               oDrawArea.YPos,
               wWidth,
               wHeight,
               FPrinting);
      FreeLibrary(hLib);
    end;
    oDrawArea.XPos := oDrawArea.XPos + wWidth;
    if wHeight > oDrawArea.YIncr then
      oDrawArea.YIncr := wHeight;
  end;                              }
end;
{--------}
procedure TXpFilterPrint.RenderNewline(oDrawArea : TXpDrawArea;
                                       oAttrs    : TXpFoPropertyGroup);
begin
  Inc(oDrawArea.YPos, oDrawArea.YIncr);
  oDrawArea.XPos := oDrawArea.Left +
                    EvalPropAsNum(oAttrs.GetProperty(XpsStartIndent), fuPixels, 1);
end;
{--------}
procedure TXpFilterPrint.RenderPage(wPage : Integer);
var
  oPageInfo : RPageInfo;
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oPageList : TXpNodeList;
  oElem     : TXpElement;
  ThePage   : TXpElement;
  j         : Integer;
begin
  { Clear the bitmap page }
  if (not FPrinting) then begin
    FCurCanvas.Pen.Style := psClear;
    FCurCanvas.Brush.Color := clWhite;
    FCurCanvas.Rectangle(0, 0, FHardwareWidth, FHardwareHeight);
  end;

  { Select correct page master }
  oPageList := FRenderedDoc.Document.GetElementsByTagName('page');
  ThePage := TXpElement(oPageList.Item(wPage - 1));
  assert(ThePage <> nil);
  FPageSequenceCount := ThePage.GetAttributeInt('XpPageSeqNum');
  if (ResultIsFO) then
    FCurPageMaster :=
      TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(ThePage.GetAttribute('PageMaster'));

  oPageInfo.PageWidth :=
    EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                  fuPixels,
                  FHardwareWidth);
  oPageInfo.PageWidth := XpMin(oPageInfo.PageWidth, FHardwareWidth);

  oPageInfo.PageHeight :=
    EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                  fuPixels,
                  FHardwareHeight);
  oPageInfo.PageHeight := XpMin(oPageInfo.PageHeight, FHardwareHeight);

  RenderHeader(oPageInfo);
  RenderFooter(oPageInfo);
  RenderStartSide(oPageInfo);
  RenderEndSide(oPageInfo);

  { Render body }
  oDrawArea := TXpDrawArea.Create;
  try
    { Get top margin.}
    oDrawArea.Top :=
      EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop), fuPixels, 1);
    if (FCurPageMaster.HasRegionBefore) then
      oDrawArea.Top := oDrawArea.Top +
                       EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                                     fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Top := oDrawArea.Top +
                       EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBefore),
                                     fuPixels, 1);
    { Get bottom margin.}
    oDrawArea.Bottom :=
      oPageInfo.PageHeight -
      EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom), fuPixels, 1);
    if (FCurPageMaster.HasRegionAfter) then
      oDrawArea.Bottom := oDrawArea.Bottom -
                          EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                        fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Bottom := oDrawArea.Bottom -
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingAfter),
                                        fuPixels, 1);

    { Get right margin.}
    oDrawArea.Right :=
      oPageInfo.PageWidth -
      EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                    fuPixels, 1);
    if (FCurPageMaster.HasRegionEnd) then
      oDrawArea.Right := oDrawArea.Right -
                         EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                       fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Right := oDrawArea.Right -
                         EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                                       fuPixels, 1);

    { Get the left margin.}
    oDrawArea.Left :=
      EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPixels, 1);
    if (FCurPageMaster.HasRegionStart) then
      oDrawArea.Left := oDrawArea.Left +
                        EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                      fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Left := oDrawArea.Left +
                        EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                      fuPixels, 1);

    oDrawArea.XPos := oDrawArea.Left;
    oDrawArea.YPos := oDrawArea.Top;
    oDrawArea.YIncr := 0;

    oNewAttrs := TXpFoPropertyGroupText.Create;
    try
      if (FCurPageMaster.Attributes <> nil) then
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
      oElem := TXpElement(FRenderedDoc.DocumentElement);
      if (oElem.HasAttributes) then begin
        for j := 0 to oElem.Attributes.Length - 1 do
          oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                oElem.Attributes.Item(j).NodeValue, True);
      end;

      FBlockLevel := 0;
      try
        if ((wPage > 0) and
            (wPage <= oPageList.Length)) then
          RenderTree(oDrawArea, ThePage, oNewAttrs, nil, False);
      finally
        oPageList.Free;
      end;
    finally
      oNewAttrs.Free;
    end;
  finally
    oDrawArea.Free;
  end;
end;
{--------}
procedure TXpFilterPrint.RenderStartSide(const oPageInfo : RPageInfo);
var
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oElem     : TXpElement;
  j         : Integer;
begin
  if (HasOutRegionStart) then begin
    FBlockLevel := 0;
    oDrawArea := TXpDrawArea.Create;
    try
      oDrawArea.Top :=
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop), fuPixels, 1);
      oDrawArea.Bottom :=
        oPageInfo.PageHeight -
        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                      fuPixels, 1);
      oDrawArea.Left :=
        EvalPropAsNum(FcurPageMaster.GetPropertyValue(XpsMarginLeft), fuPixels, 1);
      oDrawArea.Right :=
        oDrawArea.Left +
        EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                      fuPixels, 1);
      if ((FCurPageMaster.HasRegionBefore) and
          ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true') or
           (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'false'))) then
        Inc(oDrawArea.Top,
            EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                          fuPixels, 1));
      if ((FCurPageMaster.HasRegionAfter) and
          ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true') or
           (FCurPagemaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'false'))) then
        Dec(oDrawArea.Bottom,
            EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                          fuPixels, 1));
      oDrawArea.XPos := oDrawArea.Left;
      oDrawArea.YPos := oDrawArea.Top;
      oDrawArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionStart.DocumentElement);
        if oElem.HasAttributes then begin
          for j := 0 to oElem.Attributes.Length - 1 do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        RenderTree(oDrawArea,
                   FOutRegionStart.DocumentElement,
                   oNewAttrs, nil, False);
      finally
        oNewAttrs.Free;
      end;
    finally
      oDrawArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterPrint.RenderText(oDrawArea : TXpDrawArea;
                                    oAttrs    : TXpFoPropertyGroup;
                                    sText     : string);
var
  wTextHeight,
  wRed,
  wGreen,
  wBlue,
  al, h,
  wLen         : Integer;
  Loop, l: Integer;
  sWord,
  sSave,
  sTmp         : string;
  oRect        : TRect;
  TmpDrawArea  : TxpDrawArea;
begin
  { Set font and color }
  FCurCanvas.Font.PixelsPerInch := FHPixelsPerInch;
  FigureFont(FCurCanvas.Font, oAttrs);

  if (not FInText) then begin
    FBlockLevel := FBlockLevel + 1;
    FInText := True;
  end;

  sTmp := oAttrs.GetProperty(XpsBackgroundColor);
  if ((sTmp <> 'transparent') and
      (sTmp <> '')) then begin
    SetBkMode(FCurCanvas.Handle, OPAQUE);
    FCurCanvas.Brush.Color :=
      XpEvaluateColor(oAttrs.GetProperty(XpsBackgroundColor),
                    wRed,
                    wGreen,
                    wBlue);
  end else begin
    SetBkMode(FCurCanvas.Handle, TRANSPARENT);
    FCurCanvas.Brush.Color := clWhite;
  end;

  { Draw centered text }
  FCurCanvas.Pen.Style := psClear;
  if (oAttrs.GetProperty(XpsTextAlign) = 'center') then begin
    oRect.Left := oDrawArea.Left;
    oRect.Right := oDrawArea.Right;
    oRect.Top := oDrawArea.YPos;
    {oRect.Bottom := oRect.Top + FCurCanvas.TextHeight(' ');}
    oRect.Bottom := oDrawArea.Bottom; {!!!}
    FCurCanvas.Rectangle(oRect.Left - 1,
                         oRect.Top - 1,
                         oRect.Right + 2,
                         oRect.Bottom + 2);
    wTextHeight :=
      DrawText(FCurCanvas.Handle,
               PChar(sText),
               Length(sText),
               oRect,
               DT_CENTER{ or DT_SINGLELINE});                          {!!.57}
    if (wTextHeight > oDrawArea.YIncr) then
      oDrawArea.YIncr := wTextHeight;
  end else if (oAttrs.GetProperty(XpsTextAlign) = 'end') or
              (oAttrs.GetProperty(XpsTextAlign) = 'right') then begin
    oRect.Left := oDrawArea.Left;
    oRect.Right := oDrawArea.Right;
    oRect.Top := oDrawArea.YPos;
    {oRect.Bottom := oRect.Top + FCurCanvas.TextHeight(' ');}
    oRect.Bottom := oDrawArea.Bottom; {!!!}
    FCurCanvas.Rectangle(oRect.Left - 1,
                         oRect.Top - 1,
                         oRect.Right + 2,
                         oRect.Bottom + 2);
    wTextHeight :=
      DrawText(FCurCanvas.Handle,
               PChar(sText),
               Length(sText),
               oRect,
               DT_RIGHT{ or DT_SINGLELINE});
    if (wTextHeight > oDrawArea.YIncr) then
      oDrawArea.YIncr := wTextHeight;
  end else begin
    { Draw text }
    sTmp := oAttrs.GetProperty('display-align');
    if CompareText(sTmp, 'center') = 0 then begin
      Loop := 2;
      Al := 1;
    end else
    if CompareText(sTmp, 'after') = 0 then begin
      Loop := 2;
      Al := 2;
    end else begin
      Loop := 1;
      Al := 0;
    end;

    sSave := sText;
    TmpDrawArea := TxpDrawArea.Create;
    TmpDrawArea.Bottom := oDrawArea.Bottom;
    TmpDrawArea.Left := oDrawArea.Left;
    TmpDrawArea.Right := oDrawArea.Right;
    TmpDrawArea.Top := oDrawArea.Top;
    TmpDrawArea.XPos := oDrawArea.XPos;
    TmpDrawArea.YIncr := oDrawArea.YIncr;
    TmpDrawArea.YPos := oDrawArea.YPos;

    for l := 1 to Loop do begin
      sText := sSave;
      h := oDrawArea.YPos - TmpDrawArea.YPos;
      oDrawArea.Bottom := TmpDrawArea.Bottom;
      oDrawArea.Left := TmpDrawArea.Left;
      oDrawArea.Right := TmpDrawArea.Right;
      oDrawArea.Top := TmpDrawArea.Top;
      oDrawArea.XPos := TmpDrawArea.XPos;
      oDrawArea.YIncr := TmpDrawArea.YIncr;
      oDrawArea.YPos := TmpDrawArea.YPos;
      if l = 2 then
        case Al of
        1 :
          begin
            inc(oDrawArea.YPos,
              ((oDrawArea.Bottom - oDrawArea.Top) - h) div 2 - FCurCanvas.TextHeight(' '));
          end;
        2 :
          begin
            inc(oDrawArea.YPos,
              ((oDrawArea.Bottom - oDrawArea.Top) - h) - 2 * FCurCanvas.TextHeight(' '));
          end;
        end;
      while (sText <> '') do begin
        case sText[1] of
          #$0d :
            begin
              sTmp := oAttrs.GetProperty(XpsWhiteSpace);
              if (sTmp = 'no-wrap') or
                 (sTmp = 'pre') then
                RenderNewline(oDrawArea, oAttrs);
              Delete(sText, 1, 1);
            end;
          #$0a :
            Delete(sText, 1, 1);
          ' ' :
            begin
              sTmp := oAttrs.GetProperty(XpsWhiteSpace);
              if (sTmp = 'no-wrap') or
                 (sTmp = 'pre') or
                 (oDrawArea.XPos > oDrawArea.Left) then begin
                FCurCanvas.TextOut(oDrawArea.XPos, oDrawArea.YPos, ' ');
                Inc(oDrawArea.XPos, FCurCanvas.TextWidth(' '));
              end;
              Delete(sText, 1, 1);
            end;
          else begin
            sWord := XpGetWord(sText);
            Delete(sText, 1, Length(sWord));
            wLen := FCurCanvas.TextWidth(sWord);
            sTmp := oAttrs.GetProperty(XpsWhiteSpace);
            if not ((sTmp = 'no-wrap') or
                    (sTmp = 'pre')) then begin
              if ((oDrawArea.XPos + wLen) > oDrawArea.Right) then begin
                RenderNewLine(oDrawArea, oAttrs);
                Inc(oDrawArea.XPos,
                    EvalPropAsNum(oAttrs.GetProperty(XpsTextIndent),
                                  fuPixels, 1));
              end;
            end;
            if ((oDrawArea.XPos < oDrawArea.Right) and
                ((oDrawArea.YPos + FCurCanvas.TextHeight(' ')) <
                  oDrawArea.Bottom)) then
              if l = Loop then
                FCurCanvas.TextOut(oDrawArea.XPos, oDrawArea.YPos, sWord);
            wTextHeight := FCurCanvas.TextHeight(' ');
            if (wTextHeight > oDrawArea.YIncr) then
              oDrawArea.YIncr := wTextHeight;
            Inc(oDrawArea.XPos, wLen);
          end;
        end;
      end;
    end;
    TmpDrawArea.Free;
  end;
end;
{--------}
function TXpFilterPrint.RenderTree(oDrawArea : TXpDrawArea;
                                   oTree     : TXpElement;
                                   oAttrs    : TXpFoPropertyGroup;
                                   oPage     : TXpElement;
                                   bTop      : Boolean) : Boolean;
var
  i,
  j,
  x,
  y,
  k, l, h0, t0,
  wRed,
  wGreen,
  wBlue{,
  wWidth}       : Integer;
  oElem        : TXpElement;
  oNewAttrs    : TXpFoPropertyGroup;
  sCommand,
  sBreakAfter,
  sBreakBefore,
  sMarginStart,
  sMarginEnd   : string;
  TmpDrawArea,
  oOldDrawArea : TXpDrawArea;
  bInheritable : Boolean;
  oTmpNode     : TXpNode;
  CS, CS2: TXpTableCellArea;

  procedure Reorder(TableElem: TXpElement);
  var
    i, FootIndex : Integer;
    oElem        : TXpElement;
    AtEnd: Boolean;
  begin
    AtEnd := True;
    FootIndex := 0;
    for i := TableElem.ChildNodes.Length - 1 downto 0 do begin
      oElem := TXpElement(TableElem.ChildNodes.Item(i));
      if oElem.NodeName = 'tfoot' then begin
        if not AtEnd then
          if i < FootIndex - 1 then
            TableElem.ChildNodes.Move(i, FootIndex - 1);
      end else begin
        AtEnd := False;
        FootIndex := i + 1;
      end;
    end;
    AtEnd := True;
    for i := 0 to TableElem.ChildNodes.Length - 1 do begin
      oElem := TXpElement(TableElem.ChildNodes.Item(i));
      if oElem.NodeName = 'thead' then begin
        if not AtEnd then
          if i > FootIndex + 1 then
            TableElem.ChildNodes.Move(i, FootIndex + 1);
      end else begin
        AtEnd := False;
        FootIndex := i - 1;
      end;
    end;
  end;

begin
  oOldDrawArea := nil;
  Result := False;
  for i := 0 to (oTree.ChildNodes.Length - 1) do begin
    sBreakAfter := '';
    sBreakBefore := '';
    { Skip this node if:
      - not a text node
      - not a SPAN node
      - its XpPageSeqNum property doesn't match the curr page sequence}
    if ((not (oTree.ChildNodes.Item(i) is TXpText)) and
        (oTree.ChildNodes.Item(i).NodeName <> XpsSpan) and
        (TXpElement(oTree.ChildNodes.Item(i)).GetAttributeInt('XpPageSeqNum') <>
         FPageSequenceCount)) then
      Continue;
    case oTree.ChildNodes.Item(i).NodeType of
      ELEMENT_NODE :
        begin
          { Process attributes }
          oNewAttrs := TXpFoPropertyGroup.Create;
          try
            oNewAttrs.DoInherit(oAttrs);
            oElem := TXpElement(oTree.ChildNodes.Item(i));
            if (oElem.HasAttributes) then begin
              for j := 0 to (oElem.Attributes.Length - 1) do begin
                bInheritable := True;
                case oElem.Attributes.Item(j).NodeId of
                  FOP_MARGIN_RIGHT,
                  FOP_MARGIN_LEFT,
                  FOP_BREAK_BEFORE,
                  FOP_BREAK_AFTER :
                    bInheritable := False;
                end;
                oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                      oElem.Attributes.Item(j).NodeValue,
                                      bInheritable);
              end;
              sBreakAfter := oElem.GetAttribute(XpsBreakAfter);
              sBreakBefore := oElem.GetAttribute(XpsBreakBefore);
              oElem.SetAttribute(XpsBreakBefore, '');
            end;

            sCommand := oElem.NodeName;
            if (sCommand = XpsIMG) then
              RenderImage(oDrawArea, oNewAttrs)
            else if (sCommand = 'imgp') then begin
              RenderNewline(oDrawArea, oNewAttrs);
              RenderImage(oDrawArea, oNewAttrs);
              RenderNewline(oDrawArea, oNewAttrs);
            end else if (sCommand = XpsDiv) then begin
              if ((FInText) and
                  (FBlockLevel > 0)) then begin
                oDrawArea.YIncr := EvalPropAsNum(oAttrs.GetProperty(XpsFontSize),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oAttrs);
                FBlockLevel := FBlockLevel - 1;
              end;
              FInText := False;
              if (oNewAttrs.GetProperty(XpsSpaceBeforeOptimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeOptimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBefore) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBefore),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMinimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMinimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMaximum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMaximum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsP) then begin
              if ((FInText) and
                  (FBlockLevel > 0)) then begin
                oDrawArea.YIncr := EvalPropAsNum(oAttrs.GetProperty(XpsFontSize),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oAttrs);
                FBlockLevel := FBlockLevel - 1;
              end;
              FInText := False;
              { If there, handle the SpaceBefore property.}
              if (oNewAttrs.GetProperty(XpsSpaceBeforeOptimum) <> '') then begin
                oDrawArea.YIncr := EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeOptimum),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMinimum) <> '') then begin
                oDrawArea.YIncr := EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMinimum),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMaximum) <> '') then begin
                oDrawArea.YIncr := EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMaximum),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsBR) then begin
              if oDrawArea.YIncr = 0 then
                oDrawArea.YIncr := FCurCanvas.TextHeight(' ');
              RenderNewline(oDrawArea, oNewAttrs);
            end else if (sCommand = XpsHR) then begin
              if oDrawArea.XPos <> oDrawArea.Left then
                RenderNewline(oDrawArea, oNewAttrs);
              RenderGraphicLine(oDrawArea, oNewAttrs);
              RenderNewline(oDrawArea, oNewAttrs);
            end else if (sCommand = XpsPAGENUM) then begin
              if ((oAttrs.GetProperty(XpsTextAlign) = 'end') or
                  (oAttrs.GetProperty(XpsTextAlign) = 'right')) then
                FRightText := FRightText + IntToStr(FPageCurrent)
              else
                RenderText(oDrawArea, oNewAttrs, IntToStr(FPageCurrent));
            end else if (sCommand = XpsTABLE) then begin
              RenderNewLine(oDrawArea, oNewAttrs);
              oDrawArea.YIncr := FCurCanvas.TextHeight(' ') shr 1;
              if (oNewAttrs.GetProperty(XpsSpaceBeforeOptimum) <> '') then
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeOptimum),
                                fuPixels, 1)
              else if (oNewAttrs.GetProperty(XpsSpaceBefore) <> '') then
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBefore),
                                fuPixels, 1)
              else if (oNewAttrs.GetProperty(XpsSpaceBeforeMinimum) <> '') then
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMinimum),
                                fuPixels, 1)
              else if (oNewAttrs.GetProperty(XpsSpaceBeforeMaximum) <> '') then
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMaximum),
                                fuPixels, 1);
              RenderNewLine(oDrawArea, oNewAttrs);

              ReOrder(oElem);

              FigureTableInfo(oDrawArea, oElem, oNewAttrs);
              oDrawArea.Table.SelectRow := 0;
              if (oDrawArea.Table.CurrentRow <> nil) then begin
                oDrawArea.Table.CurrentRow.Top := oDrawArea.YPos;
                oDrawArea.YIncr := FCurCanvas.TextHeight(' ') shr 1;
                RenderNewLine(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTR) then begin
              oDrawArea.Table.CurrentRow.SelectCell := 0;
            end else if (sCommand = XpsTD) then begin
              CS := oDrawArea.Table.CurrentRow.CurrentCell;
              CS.Top := oDrawArea.Table.CurrentRow.Top;
              oOldDrawArea := oDrawArea;
              oDrawArea := TXpDrawArea.Create;
              oDrawArea.Top :=
                oOldDrawArea.Table.CurrentRow.Top +
                CS.Padding.TW;

              oDrawArea.Left :=
                CS.Left +
                CS.Padding.LW;

              oDrawArea.Right :=
                oDrawArea.Left +
                CS.Width -
                 CS.Padding.RW;

              if CS.NoRight then begin
                k := oOldDrawArea.Table.CurrentRow.SelectCell + 1;
                repeat
                  CS2 := oOldDrawArea.Table.CurrentRow.Cell[k];
                  inc(oDrawArea.Right, CS2.Width);
                  if not CS2.NoRight then
                    break;
                  inc(k);
                until False;
              end;

              oDrawArea.Bottom :=
                oOldDrawArea.Bottom -
                CS.Padding.BW;

              if CS.HeightSpc <> 0 then
                if oDrawArea.Bottom > oDrawArea.Top + CS.HeightSpc + CS.Padding.TW + CS.Padding.BW then
                  oDrawArea.Bottom := oDrawArea.Top + CS.HeightSpc + CS.Padding.TW + CS.Padding.BW;

              inc(oDrawArea.Top, 2 * CS.Borders.TW);

              oDrawArea.YPos := oDrawArea.Top;
              oDrawArea.XPos := oDrawArea.Left;
            end;

            { Make adjustments for attributes }
            sMarginStart := oNewAttrs.GetProperty(XpsMarginLeft);
            if (sMarginStart <> '') then begin
              Inc(oDrawArea.Left, EvalPropAsNum(sMarginStart, fuPixels, 1));
              if (oDrawArea.XPos < oDrawArea.Left) then
                oDrawArea.XPos := oDrawArea.Left;
            end;
            sMarginEnd := oNewAttrs.GetProperty(XpsMarginRight);
            if (sMarginEnd <> '') then
              Dec(oDrawArea.Right, EvalPropAsNum(sMarginEnd, fuPixels, 1));

            { Render node }
            Result := RenderTree(oDrawArea, oElem, oNewAttrs, oPage, False);

            if (sMarginStart <> '') then
              Dec(oDrawArea.Left, EvalPropAsNum(sMarginStart, fuPixels, 1));
            if (sMarginEnd <> '') then
              Inc(oDrawArea.Right, EvalPropAsNum(sMarginEnd, fuPixels, 1));

            if (sCommand = XpsP) then begin
              { Is there right justified text that needs to be rendered? }
              if (FRightText <> '') then begin
                RenderText(oDrawArea, oNewAttrs, FRightText);
                FRightText := ''
              end;
              { If this element doesn't have an DIV children, then we
                haven't skiped to the next line yet.}
              if (oElem.FindElement(XpsP) = nil) then begin
                oDrawArea.YIncr := EvalPropAsNum(oNewAttrs.GetProperty(XpsFontSize),
                                                 fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
                FBlockLevel := FBlockLevel - 1;
              end else
                FBlockLevel := FBlockLevel - 1;
              { Do we need some space after this block? }
              if (oNewAttrs.GetProperty(XpsSpaceAfterOptimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterOptimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfter) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfter),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMinimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMinimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMaximum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMaximum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTABLE) then begin
              FCurCanvas.Pen.Style := psSolid;
              {fix up cell borders}
              for k := 0 to oDrawArea.Table.TableRows.Count - 1 do
                with oDrawArea.Table.Row[k] do begin
                  H0 := 0;
                  T0 := 0;
                  for l := 0 to TableCells.Count - 1 do begin
                    if not Cell[l].NoBottom and (Cell[l].HeightClc > H0) then
                      H0 := Cell[l].HeightClc;
                    if Cell[l].Top > T0 then
                      T0 := Cell[l].Top;
                  end;
                  for l := 0 to TableCells.Count - 1 do begin
                    Cell[l].HeightClc := H0;
                    Cell[l].Top := T0;
                  end;
                end;
              TmpDrawArea := TXpDrawArea.Create;
              for k := 0 to oDrawArea.Table.TableRows.Count - 1 do
                with oDrawArea.Table.Row[k] do begin
                  for l := 0 to TableCells.Count - 1 do begin
                    CS := Cell[l];
                    TmpDrawArea.Top := CS.Top;
                    TmpDrawArea.Bottom := TmpDrawArea.Top + CS.HeightClc;
                    TmpDrawArea.Left := CS.Left;
                    TmpDrawArea.Right := TmpDrawArea.Left + CS.Width;

                    inc(TmpDrawArea.Left, CS.Borders.LW);
                    dec(TmpDrawArea.Right, CS.Borders.RW);
                    inc(TmpDrawArea.Top, CS.Borders.TW);


                    FCurCanvas.Pen.Style := psSolid;
                    FCurCanvas.Pen.Width := CS.Borders.TW;
                    FCurCanvas.Pen.Color :=
                      XpEvaluateColor(oElem.GetAttribute(XpsBorderTopColor),
                                    wRed, wGreen, wBlue);
                    FCurCanvas.MoveTo(TmpDrawArea.Left, TmpDrawArea.Top);
                    if (FCurCanvas.Pen.Width > 0) and not CS.NoTop then
                      if CS.NoRight then
                        FCurCanvas.LineTo(TmpDrawArea.Right + 4*CS.Borders.RW, TmpDrawArea.Top)
                      else
                        FCurCanvas.LineTo(TmpDrawArea.Right, TmpDrawArea.Top)
                    else
                      FCurCanvas.MoveTo(TmpDrawArea.Right, TmpDrawArea.Top);
                    FCurCanvas.Pen.Color :=
                      XpEvaluateColor(oElem.GetAttribute(XpsBorderRightColor),
                                    wRed, wGreen, wBlue);
                    FCurCanvas.Pen.Width := CS.Borders.RW;
                    if (FCurCanvas.Pen.Width > 0) and not CS.NoRight then
                      if CS.NoBottom then
                        FCurCanvas.LineTo(TmpDrawArea.Right, TmpDrawArea.Bottom + 2 * CS.Borders.BW)
                      else
                        FCurCanvas.LineTo(TmpDrawArea.Right, TmpDrawArea.Bottom)
                    else
                      FCurCanvas.MoveTo(TmpDrawArea.Right, TmpDrawArea.Bottom);
                    FCurCanvas.Pen.Color :=
                      XpEvaluateColor(oElem.GetAttribute(XpsBorderBottomColor),
                                      wRed, wGreen, wBlue);
                    FCurCanvas.Pen.Width := CS.Borders.BW;
                    if (FCurCanvas.Pen.Width > 0) and not CS.NoBottom then
                      if CS.NoLeft then
                        FCurCanvas.LineTo(TmpDrawArea.Left - 4*CS.Borders.LW, TmpDrawArea.Bottom)
                      else
                        FCurCanvas.LineTo(TmpDrawArea.Left, TmpDrawArea.Bottom)
                    else
                      if CS.NoBottom then
                        FCurCanvas.MoveTo(TmpDrawArea.Left, TmpDrawArea.Bottom + 2 * CS.Borders.BW)
                      else
                        FCurCanvas.MoveTo(TmpDrawArea.Left, TmpDrawArea.Bottom);
                    FCurCanvas.Pen.Color :=
                      XpEvaluateColor(oElem.GetAttribute(XpsBorderLeftColor),
                                    wRed, wGreen, wBlue);
                    FCurCanvas.Pen.Width := CS.Borders.LW;
                    if (FCurCanvas.Pen.Width > 0) and not CS.NoLeft then
                      FCurCanvas.LineTo(TmpDrawArea.Left, TmpDrawArea.Top-1)
                    else
                      FCurCanvas.MoveTo(TmpDrawArea.Left, TmpDrawArea.Top-1);

                  end;
                end;
              TmpDrawArea.Free;
              oDrawArea.Table.Free;
              oDrawArea.Table := TXpTableArea.Create;
              oDrawArea.YIncr := FCurCanvas.TextHeight(' ') shr 1;
              RenderNewLine(oDrawArea, oNewAttrs);
              if (oNewAttrs.GetProperty(XpsSpaceAfterOptimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterOptimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMinimum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMinimum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMaximum) <> '') then begin
                oDrawArea.YIncr :=
                  EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMaximum),
                                fuPixels, 1);
                RenderNewline(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTR) then begin
              { Compute row size based on largest cell }
              x := 0;
              for y := 0 to
                  (oDrawArea.Table.CurrentRow.TableCells.Count - 1) do begin
                oDrawArea.Table.CurrentRow.SelectCell := y;
                CS := oDrawArea.Table.CurrentRow.CurrentCell;
                if not CS.NoBottom and (CS.HeightClc > x) then
                  x := CS.HeightClc + CS.Borders.BW;
              end;
              if (x > oDrawArea.Table.CurrentRow.Height) then
                oDrawArea.Table.CurrentRow.Height := x;
              oDrawArea.YPos := oDrawArea.Table.CurrentRow.Top +
                                oDrawArea.Table.CurrentRow.Height;
              Inc(oDrawArea.Table.SelectRow);
              if oDrawArea.Table.CurrentRow <> nil then begin
                oDrawArea.Table.CurrentRow.Top := oDrawArea.YPos;
                oDrawArea.YIncr := FCurCanvas.TextHeight(' ') shr 1;
                RenderNewLine(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTD) then begin
              RenderNewLine(oDrawArea, oNewAttrs);
              Inc(oDrawArea.YPos, FCurCanvas.TextHeight(' ') shr 2);
              CS := oOldDrawArea.Table.CurrentRow.CurrentCell;
              if (oDrawArea.YPos - CS.Top > CS.HeightClc) then
                CS.HeightClc := oDrawArea.YPos - CS.Top;

              oDrawArea.Free;
              if not CS.NoBottom then
                inc(CS.HeightClc, 2 * CS.Borders.BW);

              oDrawArea := oOldDrawArea;
              repeat
                inc(oDrawArea.Table.CurrentRow.SelectCell);
                if oDrawArea.Table.CurrentRow.CurrentCell = nil then
                  break;
              until not oDrawArea.Table.CurrentRow.CurrentCell.NoTop
               and not oDrawArea.Table.CurrentRow.CurrentCell.NoLeft;
              while oDrawArea.Table.CurrentRow.CurrentCell = nil do
                dec(oDrawArea.Table.CurrentRow.SelectCell);
            end;
          finally
            oNewAttrs.Free;
          end;
        end;
      TEXT_NODE, CDATA_SECTION_NODE :
        begin
          if ((oAttrs.GetProperty(XpsTextAlign) = 'end') or
              (oAttrs.GetProperty(XpsTextAlign) = 'right')) then
            FRightText := FRightText + oTree.ChildNodes.Item(i).NodeValue
          else
            RenderText(oDrawArea,
                       oAttrs,
                       oTree.ChildNodes.Item(i).NodeValue);
        end;
    end;

    { Check for page breaks }
    if Result then
      Exit;

    if (oPage <> nil) then begin
      { Go to next page...}

      {...if we've filled this one up.}
      if (oDrawArea.YPos > oDrawArea.Bottom) and
         (i + 1 < oTree.Childnodes.Length) then begin
        fbAddNodesToPage(oPage, oTree.ChildNodes.Item(i));
        Result := True;
        Exit;
      end;
      {...or if we're told to break before the next page.}
      if ((sBreakBefore = 'page') or
          ((sBreakBefore = 'even-page') and
           (FPageCurrent mod 2 = 1)) or
          ((sBreakBefore = 'odd-page') and
           (FPageCurrent mod 2 = 0))) then begin
        fbAddNodesToPage(oPage, oTree.ChildNodes.Item(i));
        Result := True;
        Exit;
      end;
      {...or if we're told to break after this page.}
      if ((i + 1 < oTree.ChildNodes.Length) and
          ((sBreakAfter = 'page') or
           ((sBreakAfter = 'even-page') and
            (FPageCurrent mod 2 = 0)) or
           ((sBreakAfter = 'odd-page') and
            (FPageCurrent mod 2 = 1)))) then begin
          fbAddNodesToPage(oPage, oTree.ChildNodes.Item(i + 1));
          Result := True;
          Exit;
      end
    end;
  end;

  if ((oPage <> nil) and
      (bTop)) then begin
    for j := 0 to (oTree.ChildNodes.Length - 1) do begin
      oTmpNode := oTree.ChildNodes.Item(j).CloneNode(True);
      oPage.OwnerDocument.ForceOwnerDocument(oTmpNode);
      oPage.AppendChild(oTmpNode);
      oTmpNode.Release;
    end;
    oTree.RemoveAll;
  end;
end;
{--------}
procedure TXpFilterPrint.Reset;
{ Revised !!.58}
var
  Device, Name, Port: array[0..100] of Char;
  DevMode: THandle;
  wWidth,
  wHeight,
  wTmp         : Integer;
  pDevMode     : PDeviceMode;
begin
  inherited;
  { Query default printer }
  FBlockLevel := 0;
  FInText := False;
  FHPixelsPerInch := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
  FVPixelsPerInch := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  if FRefreshSettings then begin
    Printer.GetPrinter(Device, Name, Port, DevMode);
    Printer.SetPrinter(Device, Name, Port, 0);
  end;
  Printer.GetPrinter(Device, Name, Port, DevMode);
  if DevMode <> 0 then begin
    pDevMode := GlobalLock(DevMode);
    try
      case pDevMode.dmPaperSize of
        DMPAPER_LETTER, DMPAPER_LETTERSMALL, DMPAPER_NOTE :
          begin
            wWidth := 612;
            wHeight := 792;
          end;
        DMPAPER_LEGAL :
          begin
            wWidth := 612;
            wHeight := 1008;
          end;
        DMPAPER_A4, DMPAPER_A4SMALL :
          begin
            wWidth := 605;
            wHeight := 856;
          end;
        DMPAPER_CSHEET :
          begin
            wWidth := 1224;
            wHeight := 1728;
          end;
        DMPAPER_DSHEET :
          begin
            wWidth := 1584;
            wHeight := 2448;
          end;
        DMPAPER_ESHEET :
          begin
            wWidth := 2448;
            wHeight := 3168;
          end;
        DMPAPER_TABLOID :
          begin
            wWidth := 792;
            wHeight := 1224;
          end;
        DMPAPER_LEDGER :
          begin
            wWidth := 1224;
            wHeight := 792;
          end;
        DMPAPER_STATEMENT :
          begin
            wWidth := 396;
            wHeight := 612;
          end;
        DMPAPER_EXECUTIVE :
          begin
            wWidth := 522;
            wHeight := 756;
          end;
        DMPAPER_A3 :
          begin
            wWidth := 856;
            wHeight := 1210;
          end;
        DMPAPER_A5 :
          begin
            wWidth := 427;
            wHeight := 605;
          end;
        DMPAPER_B4 :
          begin
            wWidth := 720;
            wHeight := 1019;
          end;
        DMPAPER_B5 :
          begin
            wWidth := 525;
            wHeight := 741;
          end;
        DMPAPER_FOLIO :
          begin
            wWidth := 612;
            wHeight := 936;
          end;
        DMPAPER_QUARTO :
          begin
            wWidth := 620;
            wHeight := 792;
          end;
        DMPAPER_10X14 :
          begin
            wWidth := 720;
            wHeight := 1008;
          end;
        DMPAPER_11X17 :
          begin
            wWidth := 792;
            wHeight := 1224;
          end;
        DMPAPER_ENV_9 :
          begin
            wWidth := 279;
            wHeight := 639;
          end;
        DMPAPER_ENV_10 :
          begin
            wWidth := 297;
            wHeight := 684;
          end;
        DMPAPER_ENV_11 :
          begin
            wWidth := 324;
            wHeight := 747;
          end;
        DMPAPER_ENV_12 :
          begin
            wWidth := 342;
            wHeight := 792;
          end;
        DMPAPER_ENV_14 :
          begin
            wWidth := 360;
            wHeight := 828;
          end;
        DMPAPER_ENV_DL :
          begin
            wWidth := 317;
            wHeight := 634;
          end;
        DMPAPER_ENV_C5 :
          begin
            wWidth := 467;
            wHeight := 660;
          end;
        DMPAPER_ENV_C3 :
          begin
            wWidth := 934;
            wHeight := 1319;
          end;
        DMPAPER_ENV_C4 :
          begin
            wWidth := 660;
            wHeight := 934;
          end;
        DMPAPER_ENV_C6 :
          begin
            wWidth := 329;
            wHeight := 467;
          end;
        DMPAPER_ENV_C65 :
          begin
            wWidth := 328;
            wHeight := 660;
          end;
        DMPAPER_ENV_B4 :
          begin
            wWidth := 720;
            wHeight := 1017;
          end;
        DMPAPER_ENV_B5 :
          begin
            wWidth := 507;
            wHeight := 720;
          end;
        DMPAPER_ENV_B6 :
          begin
            wWidth := 507;
            wHeight := 360;
          end;
        DMPAPER_ENV_MONARCH :
          begin
            wWidth := 279;
            wHeight := 540;
          end;
        DMPAPER_ENV_PERSONAL :
          begin
            wWidth := 261;
            wHeight := 468;
          end;
        DMPAPER_FANFOLD_US :
          begin
            wWidth := 1071;
            wHeight := 792;
          end;
        DMPAPER_FANFOLD_STD_GERMAN :
          begin
            wWidth := 612;
            wHeight := 864;
          end;
        DMPAPER_FANFOLD_LGL_GERMAN :
          begin
            wWidth := 612;
            wHeight := 936;
          end;
        else begin
          wWidth := 612;
          wHeight := 792;
        end;
      end;
      if (pDevMode.dmOrientation = 2) then begin
        wTmp := wWidth;
        wWidth := wHeight;
        wHeight := wTmp;
      end;

      FBitmap.Width := FHPixelsPerInch * wWidth div 72;
      FBitmap.Height := FVPixelsPerInch * wHeight div 72;
    finally
      GlobalUnlock(DevMode);
    end;
  end
  else begin
    FBitmap.Width := (FHPixelsPerInch * 17) shr 1;
    FBitmap.Height := FVPixelsPerInch * 11;
  end;

  FCurCanvas := FBitmap.Canvas;
  FHardwareWidth := FBitmap.Width;
  FHardwareHeight := FBitmap.Height;

  InitRegion(FOutRegionBefore, XpsSPAN);
  InitRegion(FOutRegionAfter, XpsSPAN);
  InitRegion(FOutRegionBody, XpsSPAN);
  InitRegion(FOutRegionStart, XpsSPAN);
  InitRegion(FOutRegionEnd, XpsSPAN);
  InitRegion(FOutTitle, XpsSPAN);

  FVSlideFactor := (FBitmap.Height - Height);
  if (FVSlideFactor < 0) then
    FVSlideFactor := 0;
  if (FVSlidePosition > FVSlideFactor) then
    VSlidePosition := FVSlideFactor;

  FHSlideFactor := FBitmap.Width - Width;
  if (FHSlideFactor < 0) then
    FHSlideFactor := 0;
  if (FHSlidePosition > FHSlideFactor) then
    HSlidePosition := FHSlideFactor;

  FOutRegionCurrent := OutRegionBody;
  FCurElement := FOutRegionCurrent.Document.DocumentElement;
  FCurAttr := FCurElement;
end;
{--------}
procedure TXpFilterPrint.Resize;
begin
  FVSlideFactor := (FBitmap.Height - Height);
  if (FVSlideFactor < 0) then
    FVSlideFactor := 0;
  if (FVSlidePosition > FVSlideFactor) then
    FVSlidePosition := FVSlideFactor;
  FHSlideFactor := (FBitmap.Width - Width);
  if (FHSlideFactor < 0) then
    FHSlideFactor := 0;
  if (FHSlidePosition > FHSlideFactor) then
    FHSlidePosition := FHSlideFactor;
  inherited;
  Invalidate;
end;
{--------}
procedure TXpFilterPrint.SetHSlidePosition(const Value : Integer);
begin
  if (Value < FHSlideFactor) then
    FHSlidePosition := Value
  else
    FHSlidePosition := FHSlideFactor;
  InvalidateRect(Handle, nil, False);
end;
{--------}
procedure TXpFilterPrint.SetPageCurrent(const Value : Integer);
begin
  FPageCurrent := Value;
  if (FPageCurrent <= FPageCount) and
     (FPageCurrent > 0) then begin
    RenderPage(FPageCurrent);
    Invalidate;
  end else
    FPageCurrent := 1;
end;
{--------}
procedure TXpFilterPrint.SetVSlidePosition(const Value : Integer);
begin
  if (Value < FVSlideFactor) then
    FVSlidePosition := Value
  else
    FVSlidePosition := FVSlideFactor;
  InvalidateRect(Handle, nil, False);
end;
{=====================================================================}
procedure TXpFilterPrint.XslProcessorApplyStyleEnd(Sender : TObject);
begin
  inherited;

  { Now that the formatting object tree has been validated, we know
    we're gonna be rendering this document so we'll create a DOM to
    hold the rendered document now.}
  FRenderedDoc := TXpObjModel.Create(nil);
  InitRegion(FRenderedDoc, XpsSPAN);

{  FQueueBody.FormattedOutput := True;
  FQueueBody.SaveToFile('c:\jnk\out1.xml');}

  PageinateDocument;
{  FQueueBody.FormattedOutput := True;
  FQueueBody.SaveToFile('c:\jnk\out2.xml');}

  SetPageCurrent(FPageCurrent);
  Resize;
end;
{--------}
procedure TXpFilterPrint.XslProcessorApplyStyleStart(Sender : TObject);
begin
  Reset;
end;
{--------}
procedure TXpFilterPrint.XslProcessorAttribute(oOwner : TObject;
                                               sName,
                                               sValue : DOMString);
begin
  if (FCurAttr <> nil) then
    FCurAttr.SetAttribute(sName, sValue);
end;
{--------}
procedure TXpFilterPrint.XslProcessorCDATASection(oOwner   : TObject;
                                                  sValue   : DOMString;
                                                  bReplace : Boolean);
var
  oText : TXpCDATASection;
begin
  if (FCurElement <> nil) then begin
    oText := FOutRegionCurrent.Document.CreateCDATASection(sValue);
    FResultTree.Document.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
    oText.Release;
  end;
end;
{--------}
procedure TXpFilterPrint.XslProcessorElementEnd(oOwner : TObject;
                                                sValue : DOMString);
begin
  inherited;
end;
{--------}
procedure TXpFilterPrint.XslProcessorElementStart(oOwner : TObject;
                                                  sValue : DOMString);
begin
  inherited XslProcessorElementStart(oOwner, sValue);
  FCurElement.SetAttribute('XpPageSeqNum', IntToStr(FPageSequenceCount));
end;
{--------}
procedure TXpFilterPrint.XslProcessorText(oOwner : TObject;
                                          sValue : DOMString);  
begin
  TXpElement(FCurElement.CreateChildText(sValue)).SetAttribute('XpPageSeqNum',
                                                               IntToStr(FPageSequenceCount));
end;
{--------}
procedure TXpFilterPrint.XslProcessorTextNode(oOwner : TObject;
                                              oNode  : TXpText);
begin
  if (oNode.NodeValue <> '') then
    TXpElement(FCurElement.CreateChildText(oNode.NodeValue)).SetAttribute('XpPageSeqNum',
                                                                          IntToStr(FPageSequenceCount));
end;
{--------}
procedure TXpFilterPrint.XslProcessorFormatObject(Sender        : TObject;
                                                  aFormatObject : TXpFormattingObject);
var
  SpanStr: string;
  TempElement : TXpElement;
  Span, Err, W, i, W0: Integer;
begin
  inherited;

  Context.Push(aFormatObject);

  case (aFormatObject.FormatObjectType) of
    FO_BLOCK :
      begin
        XslProcessorElementStart(Sender, XpsP);
        FLastBlockElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
      end;
    FO_CHARACTER :
      begin
        { Not supported at this time.}
      end;
    FO_EXTERNAL_GRAPHIC :
      begin
        { Not supported at this time.}
      end;
    FO_INLINE :
      begin
        XslProcessorElementStart(Sender, XpsSPAN);
        FLastInlineElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LEADER :
      begin
        XslProcessorElementStart(Sender, XpsHR);
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_BLOCK :
      begin
        XslProcessorElementStart(Sender, XpsTABLE);
        FCurElement.SetAttribute(XpsWidth, '100%');
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_ITEM :
      begin
        XslProcessorElementStart(Sender, XpsTR);
        FLastTRElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_ITEM_BODY :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.SetAttribute(XpsWidth, '*');
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_ITEM_LABEL :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.SetAttribute(XpsWidth, '10%');
        fpCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_PAGE_NUMBER :
      begin
        XslProcessorElementStart(Sender, XpsPAGENUM);
        fpCopyAttrsToCurrent(aFormatObject);
      end;
    FO_STATIC_CONTENT :
      begin
        XslProcessorElementStart(Sender, 'STATIC');
        fpCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE_AND_CAPTION :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEANDCAPTIONELem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, '100%');
        fpCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEELem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_TABLE_BODY :
      begin
        XslProcessorElementStart(Sender, 'tbody');
        FLastTBODYELem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_HEADER :
      begin
        XslProcessorElementStart(Sender, 'thead');
        FLastTHEADElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_FOOTER :
      begin
        XslProcessorElementStart(Sender, 'tfoot');
        FLastTFOOTElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_TABLE_ROW :
      begin
        XslProcessorElementStart(Sender, 'tr');
        FLastTRElem := FCurElement;
        fpCopyAttrsToCurrent(aFormatObject);
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
        fpCopyAttrsToCurrent(aFormatObject);
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
            fCurElement.SetAttribute('width', IntToStr(W) + 'tw');
          W := Context.CurTable.ColumnColor[Context.CurTable.CurCol + 1];
          if W <> -1 then
            fCurElement.SetAttribute('bgcolor', '#' + IntToHex(W,0));
          fCurElement.SetAttribute('colspan', IntToStr(Span));
        end else begin
          Context.CurTable.CurCol := Context.CurTable.CurCol + 1;
          W := Context.CurTable.ColumnWidth[Context.CurTable.CurCol];
          if W <> -1 then
            fCurElement.SetAttribute('width', IntToStr(W) + 'tw');
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
      fpSaveColumnAttrs(aFormatObject);
  end;
end;

procedure TXpFilterPrint.fpSaveColumnAttrs(const aSourceElem : TXpElement);
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
                     fuTwips, 1);
  val(copy(PixStr, 1, length(PixStr) - 2), Pix, Err);
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

procedure TXpFilterPrint.XslProcessorFormatObjectEnd(Sender : TObject);
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
        while (TempElement <> nil) and
              (TempElement.NodeName <> XpsP) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastBlockElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_INLINE :
      begin
        FCurElement := FLastInlineElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while (TempElement <> nil) and
              (TempElement.NodeName <> XpsSpan) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastInlineElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM :
      begin
        FCurElement := FLastTRElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while (TempElement <> nil) and
              (TempElement.NodeName <> XpsTR) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTRElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM_BODY :
      begin
        FCurElement := FLastTDElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while (TempElement <> nil) and
              (TempElement.NodeName <> XpsTD) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastTDElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM_LABEL :
      begin
        FCurElement := FLastTDElem;
        TempElement := TXpElement(FCurElement.ParentNode);
        while (TempElement <> nil) and
              (TempElement.NodeName <> XpsTD) do
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

{=====================================================================}

constructor TXpContextStackEntry.Create(aFormatObject : TXpFormattingObject);
begin
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
  Assert(TOS.FormatObjectType = ObjectType);
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


