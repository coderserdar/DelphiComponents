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
{* XMLPartner Pro: XpFltRTF.INC                          *}
{*********************************************************}
{* XMLPartner Pro: Implements RTF filter for XSL         *}
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
    //procedure DumpStack(const Caption: string);
  end;

{== TXpFilterRTF =====================================================}
  TXpFilterRTF = class(TXpFilterBase)
  private
    { Private declarations }
    FBitmap         : TBitmap;
    FCellCnt        : Integer;
    FCellWidthList  : TStringList;
    FCentered       : Boolean;
    FColorList      : TStringList;
    {FCountCells     : Boolean;}
    FFontFamilyList : TStringList;
    FFontList       : TStringList;
    FInText         : Boolean;
    FInTable        : Boolean;
    FPrinting       : Boolean;
    FRenderedDoc    : TXpObjModel;
    FRightAligned   : Boolean;
    FRTFData        : DOMString;
    FTableElem      : TXpElement;
    FTableWidth     : Integer;
    function AddToColorTable(const cl : string) : string;              {!!.57}
    function AddToFontTable(const aFont : string) : string;
    procedure CreateColorDefs;
    procedure CreateColorTable;
    procedure CreateFontInfos;
    procedure CreateFontTable;
    procedure CreateListTable;
    procedure CreateRevisionMarks;
    procedure CreateRTFHeader;
    procedure CreateStyleSheet;
    function EvaluateColor(sColor : string) : string;
    function GetFontFamily(const aFontName : string) : string;
    function GetRTFDocument : DOMString;
    function GetRTFText(aTextNode : TXpText) : DOMString;
    procedure InitColorTable;
    procedure InitFontTable;
  protected
  
    Context             : TXpContextStack;

    { Protected declarations }
    function  frCalcYInc(aTextHeight : Integer) : Integer;
    procedure frFigureFont(oAttrs : TXpFOPropertyGroup);
    procedure frFigureTableInfo(oDrawArea : TXpDrawArea;
                                oTree     : TXpElement;
                                oAttrs    : TXpFOPropertyGroup);
    procedure frPageinateDocument;
    procedure frRenderAfterRegion(aBodyArea : TXpDrawArea;
                                  const aPageInfo : RPageInfo);
    procedure frRenderBeforeRegion(aBodyArea : TXpDrawArea;
                                   const aPageInfo : RPageInfo);
    procedure frRenderDocFormat;
    procedure frRenderEndRegion(aBodyArea : TXpDrawArea;
                                const aPageInfo : RPageInfo);
    procedure frRenderGraphicLine(oDrawArea : TXpDrawArea;
                                  oAttrs    : TXpFOPropertyGroup);
    procedure frRenderNewLine(oDrawArea : TXpDrawArea;
                              oAttrs    : TXpFOPropertyGroup);
    procedure frRenderPage(aPageNumber : Integer);
    procedure frRenderStartRegion(aBodyArea : TXpDrawArea;
                                  const aPageInfo : RPageInfo);
    procedure frRenderText(oDrawArea : TXpDrawArea;
                           oAttrs    : TXpFOPropertyGroup;
                           sText     : string);
    function frRenderTree(oDrawArea : TXpDrawArea;
                          oTree     : TXpElement;
                          oAttrs    : TXpFOPropertyGroup;
                          oPage     : TXpElement;
                          bTop      : Boolean) : Boolean;
    procedure fbSaveColumnAttrs(const aSourceElem : TXpElement);
    procedure Paint; override;
    function Render : string; virtual;
  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

    function RenderFile(const aFileName : string) : Boolean; override;
    function RenderMemory(var Buffer; aSize : Integer) : Boolean; override;
    function RenderStream(aStream : TStream) : Boolean; override;
    procedure Reset; override;
    function SaveToFile(const sFile : string) : Boolean; override;     {!!.57}
    function SaveToStream(aStream : TStream) : Boolean; override;
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

    property RTFDocument : DOMString
      read GetRTFDocument;
  end;

const
  XpsRTFLeadFactor = 1.15;
{=====================================================================}

implementation

uses
  Dialogs,
  Sysutils,
{$IFDEF UsingCLX}
  XpQXSLPr,
{$ELSE}
  XpvXSLPr,
{$ENDIF}
  XpXSLCon;

//{$R *.RES}

{== TXpFilterRTF =====================================================}
constructor TXpFilterRTF.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);
  FDisplayName := 'RTF';
  {FRTFData := '';}
  FBitmap := TBitmap.Create;

  FColorList := TStringList.Create;
  FFontList := TStringList.Create;
  FFontFamilyList := TStringList.Create;
  FCellWidthList := TStringList.Create;

  with FFontFamilyList do begin
    Add('Times New Roman=roman');
    Add('Palatino=roman');
    Add('Arial=swiss');
    Add('Courier New=modern');
    Add('Pica=modern');
    Add('Cursive=script');
    Add('Old English=decor');
    Add('ITC Zapf Chancery=decor');
    Add('Symbol=tech');
    Add('Miriam=bidi');
  end;
  FOutRegionAfterRoot := XpsFOOTER;
  FOutRegionBeforeRoot := XpsHEADER;
  FOutRegionBodyRoot := XpsBODY;
  FOutRegionEndRoot := XpsSPAN;
  FOutRegionStartRoot := XpsSPAN;
  FOutTitleRoot := XpsTITLE;
  Context := TxpContextStack.Create;
end;
{--------}
destructor TXpFilterRTF.Destroy;
begin
  FBitmap.Free;
  FColorList.Free;
  FFontList.Free;
  FFontFamilyList.Free;
  FCellWidthList.Free;
  FRenderedDoc.Free;
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then
    FCurPageMaster.Free;
  Context.Free;
  inherited Destroy;
end;
{--------}
function TXpFilterRTF.AddToColorTable(const cl : string) : string;     {!!.57}
var
 Idx : Integer;
begin
  Result := EvaluateColor(cl);
  Idx := FColorList.IndexOf(Result);
  if (Idx = -1) then
    Idx := FColorList.Add(Result);
  Result := IntToStr(Idx);
end;
{--------}
function TXpFilterRTF.AddToFontTable(const aFont : string) : string;
var
  Idx : Integer;
begin
  Idx := FFontList.IndexOf(aFont);
  if (Idx = -1) then
    Idx := FFontList.Add(aFont);
  Result := IntToStr(Idx);
end;
{--------}
procedure TXpFilterRTF.CreateFontInfos;
var
  i        : Integer;
  fontName : string;
begin
  for i := 0 to (FFontList.Count - 1) do begin
    fontName := FFontList[i];
    FRTFData := FRTFData + '{\f' + IntToStr(i);
    FRTFData := FRTFData + '\f' + GetFontFamily(fontName) + ' ';
    FRTFData := FRTFData + fontName + ';}';
  end;
end;
{--------}
procedure TXpFilterRTF.CreateRTFHeader;
begin
  FRTFData := '{\rtf\ansi' +
              FCurPageMaster.GetPropertyValue('destination') +
              '\deff0';
  CreateFontTable;
  CreateColorTable;
  CreateStyleSheet;
  CreateListTable;
  CreateRevisionMarks;
end;
{--------}
procedure TXpFilterRTF.CreateFontTable;
begin
  FRTFData := FRTFData + '{\fonttbl';
  CreateFontInfos;
  FRTFData := FRTFData + '}';
end;
{--------}
procedure TXpFilterRTF.CreateColorTable;
begin
  FRTFData := FRTFData + '{\colortbl';
  CreateColorDefs;
  FRTFData := FRTFData + '}';
end;
{--------}
procedure TXpFilterRTF.CreateColorDefs;
var
  i : Integer;
begin
  for i := 0 to (FColorList.Count - 1) do
    FRTFData := FRTFData + FColorList[i];
end;
{--------}
procedure TXpFilterRTF.CreateStyleSheet;
begin
  FRTFData := FRTFData + '{\stylesheet' + '}';
end;
{--------}
procedure TXpFilterRTF.CreateListTable;
begin
  { Not implemented yet}
end;
{--------}
procedure TXpFilterRTF.CreateRevisionMarks;
begin
  { Not implemented yet}
end;
{--------}
function TXpFilterRTF.EvaluateColor(sColor : string) : string;
var
  wRed,
  wGreen,
  wBlue  : Integer;
begin
  wRed := 0;
  wGreen := 0;
  wBlue := 0;

  Result := '\red0\green0\blue0;';

  if (sColor <> '') then begin
    sColor := LowerCase(sColor);
    if sColor[1] = '#' then begin
      Delete(sColor, 1, 1);
      wRed := StrToIntDef(Copy(sColor, 1, 2), 0);
      wGreen := StrToIntDef(Copy(sColor, 3, 2), 0);
      wBlue := StrToIntDef(Copy(sColor, 5, 2), 0);
    end else if sColor[1] < 'd' then begin
      if sColor = 'aliceblue' then begin
        wRed := $f0;
        wGreen := $f8;
        wBlue := $ff;
      end else if sColor = 'antiquewhite' then begin
        wRed := $fa;
        wGreen := $eb;
        wBlue := $d7;
      end else if sColor = 'auqa' then begin
        wRed := $00;
        wGreen := $ff;
        wBlue := $ff;
      end else if sColor = 'auqamarine' then begin
        wRed := $7f;
        wGreen := $ff;
        wBlue := $d4;
      end else if sColor = 'azure' then begin
        wRed := $f0;
        wGreen := $ff;
        wBlue := $ff;
      end else if sColor = 'beige' then begin
        wRed := $f5;
        wGreen := $f5;
        wBlue := $dc;
      end else if sColor = 'bisque' then begin
        wRed := $ff;
        wGreen := $e4;
        wBlue := $c4;
      end else if sColor = 'blanchdalmond' then begin
        wRed := $ff;
        wGreen := $eb;
        wBlue := $cd;
      end else if sColor = 'blue' then begin
        wRed := $00;
        wGreen := $00;
        wBlue := $ff;
      end else if sColor = 'blueviolet' then begin
        wRed := $8a;
        wGreen := $2b;
        wBlue := $e2;
      end else if sColor = 'brown' then begin
        wRed := $a5;
        wGreen := $2a;
        wBlue := $2a;
      end else if sColor = 'burlywood' then begin
        wRed := $de;
        wGreen := $b8;
        wBlue := $87;
      end else if sColor = 'cadetblue' then begin
        wRed := $5f;
        wGreen := $9e;
        wBlue := $a0;
      end else if sColor = 'chartreuse' then begin
        wRed := $7f;
        wGreen := $ff;
        wBlue := $00;
      end else if sColor = 'chocolate' then begin
        wRed := $d2;
        wGreen := $69;
        wBlue := $1e;
      end else if sColor = 'coral' then begin
        wRed := $ff;
        wGreen := $7f;
        wBlue := $50;
      end else if sColor = 'cornflowerblue' then begin
        wRed := $64;
        wGreen := $95;
        wBlue := $ed;
      end else if sColor = 'cornsilk' then begin
        wRed := $ff;
        wGreen := $f8;
        wBlue := $dc;
      end else if sColor = 'crimson' then begin
        wRed := $dc;
        wGreen := $14;
        wBlue := $3c;
      end else if sColor = 'cyan' then begin
        wRed := $00;
        wGreen := $ff;
        wBlue := $ff;
      end;
    end else if sColor[1] < 'm' then begin
      if sColor = 'darkblue' then begin
        wRed := $00;
        wGreen := $00;
        wBlue := $8b;
      end else if sColor = 'darkcyan' then begin
        wRed := $00;
        wGreen := $8b;
        wBlue := $8b;
      end else if sColor = 'darkgoldenrod' then begin
        wRed := $b8;
        wGreen := $86;
        wBlue := $0b;
      end else if sColor = 'darkgray' then begin
        wRed := $a9;
        wGreen := $a9;
        wBlue := $a9;
      end else if sColor = 'darkgreen' then begin
        wRed := $00;
        wGreen := $64;
        wBlue := $00;
      end else if sColor = 'darkkhaki' then begin
        wRed := $bd;
        wGreen := $b7;
        wBlue := $6b;
      end else if sColor = 'darkmagenta' then begin
        wRed := $8b;
        wGreen := $00;
        wBlue := $8b;
      end else if sColor = 'darkolivegreen' then begin
        wRed := $55;
        wGreen := $6b;
        wBlue := $2f;
      end else if sColor = 'darkorange' then begin
        wRed := $ff;
        wGreen := $8c;
        wBlue := $00;
      end else if sColor = 'darkorchid' then begin
        wRed := $99;
        wGreen := $32;
        wBlue := $cc;
      end else if sColor = 'darkred' then begin
        wRed := $8b;
        wGreen := $00;
        wBlue := $00;
      end else if sColor = 'darksalmon' then begin
        wRed := $e9;
        wGreen := $96;
        wBlue := $7a;
      end else if sColor = 'darkseagreen' then begin
        wRed := $8f;
        wGreen := $bc;
        wBlue := $8f;
      end else if sColor = 'darkslateblue' then begin
        wRed := $48;
        wGreen := $3d;
        wBlue := $8b;
      end else if sColor = 'darkslategray' then begin
        wRed := $2f;
        wGreen := $4f;
        wBlue := $4f;
      end else if sColor = 'darkturquoise' then begin
        wRed := $00;
        wGreen := $ce;
        wBlue := $d1;
      end else if sColor = 'darkviolet' then begin
        wRed := $94;
        wGreen := $00;
        wBlue := $d3;
      end else if sColor = 'deeppink' then begin
        wRed := $ff;
        wGreen := $14;
        wBlue := $93;
      end else if sColor = 'deepskyblue' then begin
        wRed := $00;
        wGreen := $bf;
        wBlue := $ff;
      end else if sColor = 'dimgray' then begin
        wRed := $69;
        wGreen := $69;
        wBlue := $69;
      end else if sColor = 'dodgerblue' then begin
        wRed := $1e;
        wGreen := $90;
        wBlue := $ff;
      end else if sColor = 'firebrick' then begin
        wRed := $b2;
        wGreen := $22;
        wBlue := $22;
      end else if sColor = 'floralwhite' then begin
        wRed := $ff;
        wGreen := $fa;
        wBlue := $f0;
      end else if sColor = 'forestgreen' then begin
        wRed := $22;
        wGreen := $8b;
        wBlue := $22;
      end else if sColor = 'fuchsia' then begin
        wRed := $ff;
        wGreen := $00;
        wBlue := $ff;
      end else if sColor = 'gainsboro' then begin
        wRed := $dc;
        wGreen := $dc;
        wBlue := $dc;
      end else if sColor = 'ghostwhite' then begin
        wRed := $f8;
        wGreen := $f8;
        wBlue := $ff;
      end else if sColor = 'gold' then begin
        wRed := $ff;
        wGreen := $d7;
        wBlue := $00;
      end else if sColor = 'goldenrod' then begin
        wRed := $da;
        wGreen := $a5;
        wBlue := $20;
      end else if sColor = 'gray' then begin
        wRed := $80;
        wGreen := $80;
        wBlue := $80;
      end else if sColor = 'green' then begin
        wRed := $00;
        wGreen := $80;
        wBlue := $00;
      end else if sColor = 'greenyellow' then begin
        wRed := $ad;
        wGreen := $ff;
        wBlue := $2f;
      end else if sColor = 'honeydew' then begin
        wRed := $f0;
        wGreen := $ff;
        wBlue := $f0;
      end else if sColor = 'hotpink' then begin
        wRed := $ff;
        wGreen := $69;
        wBlue := $b4;
      end else if sColor = 'indianred' then begin
        wRed := $cd;
        wGreen := $5c;
        wBlue := $5c;
      end else if sColor = 'indigo' then begin
        wRed := $4b;
        wGreen := $00;
        wBlue := $82;
      end else if sColor = 'ivory' then begin
        wRed := $ff;
        wGreen := $ff;
        wBlue := $f0;
      end else if sColor = 'khaki' then begin
        wRed := $f0;
        wGreen := $e6;
        wBlue := $8c;
      end else if sColor = 'lavender' then begin
        wRed := $e6;
        wGreen := $e6;
        wBlue := $fa;
      end else if sColor = 'lavenderblush' then begin
        wRed := $ff;
        wGreen := $f0;
        wBlue := $f5;
      end else if sColor = 'lawngreen' then begin
        wRed := $7c;
        wGreen := $fc;
        wBlue := $00;
      end else if sColor = 'lemonchiffon' then begin
        wRed := $ff;
        wGreen := $fa;
        wBlue := $cd;
      end else if sColor = 'lightblue' then begin
        wRed := $ad;
        wGreen := $d8;
        wBlue := $e6;
      end else if sColor = 'lightcoral' then begin
        wRed := $f0;
        wGreen := $80;
        wBlue := $80;
      end else if sColor = 'lightcyan' then begin
        wRed := $e0;
        wGreen := $ff;
        wBlue := $ff;
      end else if sColor = 'lightgoldenrodyellow' then begin
        wRed := $fa;
        wGreen := $fa;
        wBlue := $d2;
      end else if sColor = 'lightgreen' then begin
        wRed := $90;
        wGreen := $ee;
        wBlue := $90;
      end else if sColor = 'lightgray' then begin
        wRed := $d3;
        wGreen := $d3;
        wBlue := $d3;
      end else if sColor = 'lightpink' then begin
        wRed := $ff;
        wGreen := $b6;
        wBlue := $c1;
      end else if sColor = 'lightsalmon' then begin
        wRed := $ff;
        wGreen := $a0;
        wBlue := $7a;
      end else if sColor = 'lightseagreen' then begin
        wRed := $20;
        wGreen := $b2;
        wBlue := $aa;
      end else if sColor = 'lightskyblue' then begin
        wRed := $87;
        wGreen := $ce;
        wBlue := $fa;
      end else if sColor = 'lightslategray' then begin
        wRed := $77;
        wGreen := $88;
        wBlue := $99;
      end else if sColor = 'lightsteelblue' then begin
        wRed := $b0;
        wGreen := $c4;
        wBlue := $de;
      end else if sColor = 'lightyellow' then begin
        wRed := $ff;
        wGreen := $ff;
        wBlue := $e0;
      end else if sColor = 'lime' then begin
        wRed := $00;
        wGreen := $ff;
        wBlue := $00;
      end else if sColor = 'limegreen' then begin
        wRed := $32;
        wGreen := $cd;
        wBlue := $32;
      end else if sColor = 'linen' then begin
        wRed := $fa;
        wGreen := $f0;
        wBlue := $e6;
      end;
    end else if sColor[1] < 's' then begin
      if sColor = 'magenta' then begin
        wRed := $ff;
        wGreen := $00;
        wBlue := $ff;
      end else if sColor = 'maroon' then begin
        wRed := $80;
        wGreen := $00;
        wBlue := $00;
      end else if sColor = 'mediumaquamarine' then begin
        wRed := $66;
        wGreen := $cd;
        wBlue := $aa;
      end else if sColor = 'mediumblue' then begin
        wRed := $00;
        wGreen := $00;
        wBlue := $cd;
      end else if sColor = 'mediumorchid' then begin
        wRed := $ba;
        wGreen := $55;
        wBlue := $d3;
      end else if sColor = 'mediumpurple' then begin
        wRed := $93;
        wGreen := $70;
        wBlue := $db;
      end else if sColor = 'mediumseagreen' then begin
        wRed := $3c;
        wGreen := $b3;
        wBlue := $71;
      end else if sColor = 'mediumslateblue' then begin
        wRed := $7b;
        wGreen := $68;
        wBlue := $ee;
      end else if sColor = 'mediumspringgreen' then begin
        wRed := $00;
        wGreen := $fa;
        wBlue := $9a;
      end else if sColor = 'mediumturquoise' then begin
        wRed := $48;
        wGreen := $d1;
        wBlue := $cc;
      end else if sColor = 'mediumvioletred' then begin
        wRed := $c7;
        wGreen := $15;
        wBlue := $85;
      end else if sColor = 'midnightblue' then begin
        wRed := $19;
        wGreen := $19;
        wBlue := $70;
      end else if sColor = 'mintcream' then begin
        wRed := $f5;
        wGreen := $ff;
        wBlue := $fa;
      end else if sColor = 'mistyrose' then begin
        wRed := $ff;
        wGreen := $e4;
        wBlue := $e1;
      end else if sColor = 'moccasin' then begin
        wRed := $ff;
        wGreen := $e4;
        wBlue := $b5;
      end else if sColor = 'navajowhite' then begin
        wRed := $ff;
        wGreen := $de;
        wBlue := $ad;
      end else if sColor = 'navy' then begin
        wRed := $00;
        wGreen := $00;
        wBlue := $80;
      end else if sColor = 'oldlace' then begin
        wRed := $fd;
        wGreen := $f5;
        wBlue := $e6;
      end else if sColor = 'olive' then begin
        wRed := $80;
        wGreen := $80;
        wBlue := $00;
      end else if sColor = 'olivedrab' then begin
        wRed := $6b;
        wGreen := $8e;
        wBlue := $23;
      end else if sColor = 'orange' then begin
        wRed := $ff;
        wGreen := $a5;
        wBlue := $00;
      end else if sColor = 'orangered' then begin
        wRed := $ff;
        wGreen := $45;
        wBlue := $00;
      end else if sColor = 'orchid' then begin
        wRed := $da;
        wGreen := $70;
        wBlue := $d6;
      end else if sColor = 'palegoldenrod' then begin
        wRed := $ee;
        wGreen := $e8;
        wBlue := $aa;
      end else if sColor = 'palegreen' then begin
        wRed := $98;
        wGreen := $fb;
        wBlue := $98;
      end else if sColor = 'paleturquoise' then begin
        wRed := $af;
        wGreen := $ee;
        wBlue := $ee;
      end else if sColor = 'palevioletred' then begin
        wRed := $db;
        wGreen := $70;
        wBlue := $93;
      end else if sColor = 'papayawhip' then begin
        wRed := $ff;
        wGreen := $ef;
        wBlue := $d5;
      end else if sColor = 'peachpuff' then begin
        wRed := $ff;
        wGreen := $da;
        wBlue := $b9;
      end else if sColor = 'peru' then begin
        wRed := $cd;
        wGreen := $85;
        wBlue := $3f;
      end else if sColor = 'pink' then begin
        wRed := $ff;
        wGreen := $c0;
        wBlue := $cb;
      end else if sColor = 'plum' then begin
        wRed := $dd;
        wGreen := $a0;
        wBlue := $dd;
      end else if sColor = 'powderblue' then begin
        wRed := $b0;
        wGreen := $e0;
        wBlue := $e6;
      end else if sColor = 'purple' then begin
        wRed := $80;
        wGreen := $00;
        wBlue := $80;
      end else if sColor = 'red' then begin
        wRed := $ff;
        wGreen := $00;
        wBlue := $00;
      end else if sColor = 'rosybrown' then begin
        wRed := $bc;
        wGreen := $8f;
        wBlue := $8f;
      end else if sColor = 'royalblue' then begin
        wRed := $41;
        wGreen := $69;
        wBlue := $e1;
      end;
    end else begin
      if sColor = 'saddlebrown' then begin
        wRed := $8b;
        wGreen := $45;
        wBlue := $13;
      end else if sColor = 'salmon' then begin
        wRed := $fa;
        wGreen := $80;
        wBlue := $72;
      end else if sColor = 'sandybrown' then begin
        wRed := $f4;
        wGreen := $a4;
        wBlue := $60;
      end else if sColor = 'seagreen' then begin
        wRed := $2e;
        wGreen := $8b;
        wBlue := $57;
      end else if sColor = 'seashell' then begin
        wRed := $ff;
        wGreen := $f5;
        wBlue := $ee;
      end else if sColor = 'sienna' then begin
        wRed := $a0;
        wGreen := $52;
        wBlue := $2d;
      end else if sColor = 'silver' then begin
        wRed := $c0;
        wGreen := $c0;
        wBlue := $c0;
      end else if sColor = 'skyblue' then begin
        wRed := $87;
        wGreen := $ce;
        wBlue := $eb;
      end else if sColor = 'slateblue' then begin
        wRed := $6a;
        wGreen := $5a;
        wBlue := $cd;
      end else if sColor = 'slategray' then begin
        wRed := $70;
        wGreen := $80;
        wBlue := $90;
      end else if sColor = 'snow' then begin
        wRed := $ff;
        wGreen := $fa;
        wBlue := $fa;
      end else if sColor = 'springgreen' then begin
        wRed := $00;
        wGreen := $ff;
        wBlue := $7f;
      end else if sColor = 'steelblue' then begin
        wRed := $46;
        wGreen := $82;
        wBlue := $b4;
      end else if sColor = 'tan' then begin
        wRed := $d2;
        wGreen := $b4;
        wBlue := $8c;
      end else if sColor = 'teal' then begin
        wRed := $00;
        wGreen := $80;
        wBlue := $80;
      end else if sColor = 'thistle' then begin
        wRed := $d8;
        wGreen := $bf;
        wBlue := $d8;
      end else if sColor = 'tomato' then begin
        wRed := $ff;
        wGreen := $63;
        wBlue := $47;
      end else if sColor = 'turquiose' then begin
        wRed := $40;
        wGreen := $e0;
        wBlue := $d0;
      end else if sColor = 'violet' then begin
        wRed := $ee;
        wGreen := $82;
        wBlue := $ee;
      end else if sColor = 'wheat' then begin
        wRed := $f5;
        wGreen := $de;
        wBlue := $b3;
      end else if sColor = 'white' then begin
        wRed := $ff;
        wGreen := $ff;
        wBlue := $ff;
      end else if sColor = 'whitesmoke' then begin
        wRed := $f5;
        wGreen := $f5;
        wBlue := $f5;
      end else if sColor = 'yellow' then begin
        wRed := $ff;
        wGreen := $ff;
        wBlue := $00;
      end else if sColor = 'yellowgreen' then begin
        wRed := $9a;
        wGreen := $cd;
        wBlue := $32;
      end;
    end;
    Result := '\red' + IntToStr(wRed) +
              '\green' + IntToStr(wGreen)+
              '\blue' + IntToStr(wBlue) + ';';
  end;
end;
{--------}
function TXpFilterRTF.frCalcYInc(aTextHeight : Integer) : Integer;
begin
  Result := Round(aTextHeight * XpsRTFLeadFactor);
end;
{--------}
procedure TXpFilterRTF.frFigureFont(oAttrs : TXpFOPropertyGroup);
var
  TempStr : string;
begin
  TempStr := oAttrs.GetProperty(XpsFontFamily);
  TempStr := AddToFontTable(TempStr);
  if ((TempStr <> '') and
      FPrinting) then
    FRTFData := FRTFData + '\f' + TempStr;

  TempStr := IntToStr(EvalPropAsNum(oAttrs.GetProperty(XpsFontSize), fuPoints, 1) * 2);
  if ((FPrinting) and
      (TempStr <> '')) then
    FRTFData := FRTFData + '\fs' + TempStr;

  TempStr := oAttrs.GetProperty(XpsBackgroundColor);
  if (TempStr <> '') and
     (TempStr <> 'transparent') then
    TempStr := AddToColorTable(TempStr)
  else
    TempStr := AddToColorTable('white');
  if (FPrinting) then
    FRTFData := FRTFData + '\cbpat' + TempStr;

  TempStr := oAttrs.GetProperty(XpsColor);
  if (TempStr <> '') and
     (TempStr <> 'transparent') then
    TempStr := AddToColorTable(TempStr)
  else
    TempStr := AddToColorTable('black');
  if (FPrinting) then
    FRTFData := FRTFData + '\cf' + TempStr;

  if (oAttrs.GetProperty(XpsFontWeight) = 'bold') then
    FRTFData := FRTFData + '\b '
  else
    FRTFData := FRTFData + '\b0 ';

  if (oAttrs.GetProperty(XpsFontWeight) = 'italic') then
    FRTFData := FRTFData + '\i '
  else
    FRTFData := FRTFData + '\i0 ';
end;
{--------}
procedure TXpFilterRTF.frFigureTableInfo(oDrawArea : TXpDrawArea;
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
  {oDrawArea.Table.Top := oDrawArea.YPos;}
  {oDrawArea.Table.Left := oDrawArea.Left;}
  xWidth :=
    EvalPropAsNum(oAttrs.GetProperty(XpsWidth), fuTwips, 0);
  if xWidth <> 0 then
    oDrawArea.Table.Width := xWidth
  else
    oDrawArea.Table.Width := oDrawArea.Right - oDrawArea.Left;
  {oDrawArea.Table.Height :=
    EvalPropAsNum(oAttrs.GetProperty(XpsHeight), fuTwips, 1);}
  {if (oDrawArea.Table.Width < 75) then
    oDrawArea.Table.Width := 75;}

  { Enumerate through child rows first to add rows}
  oRowsList := oTree.SelectNodes(XpsTR);
  if oRowsList.Length <> 0 then begin
    for i := 0 to (oRowsList.Length - 1) do begin
      oDrawArea.Table.TableRows.Add(TXpTableRowArea.
        Create(TXpElement(oRowsList.Item(i))));
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
    R.Height := EvalPropAsNum(oRow.GetAttribute('height'), fuTwips, 0);
    oCellsList := oRow.SelectNodes(XpsTD);
    { Enumerate through child cells }
    for j := 0 to (oCellsList.Length - 1) do begin
      oCell := TXpElement(oCellsList.Item(j));
      RowSpan := StrToIntDef(oCell.GetAttribute('rowspan'), 1);
      ColSpan :=
        StrToIntDef(oCell.GetAttribute('colspan'), 1);
      wWidth :=
        EvalPropAsNum(oCell.GetAttribute('border-width'), fuTwips, 0);
      if (wWidth > 0) then begin
        Borders.TW := wWidth;
        Borders.LW := wWidth;
        Borders.RW := wWidth;
        Borders.BW := wWidth;
      end else begin
        Borders.TW := EvalPropAsNum(oCell.GetAttribute(XpsBorderTopWidth), fuTwips, 0);
        Borders.LW := EvalPropAsNum(oCell.GetAttribute(XpsBorderLeftWidth), fuTwips, 0);
        Borders.RW := EvalPropAsNum(oCell.GetAttribute(XpsBorderRightWidth), fuTwips, 0);
        Borders.BW := EvalPropAsNum(oCell.GetAttribute(XpsBorderBottomWidth), fuTwips, 0);
      end;
      wWidth := EvalPropAsNum(oCell.GetAttribute('padding'),
                              fuTwips, 1);
      if (wWidth > 0) then begin
        Padding.TW := wWidth;
        Padding.LW := wWidth;
        Padding.RW := wWidth;
        Padding.BW := wWidth;
      end else begin
        Padding.TW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingTop), fuTwips, 0);
        Padding.LW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingLeft), fuTwips, 0);
        Padding.RW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingRight), fuTwips, 0);
        Padding.BW := EvalPropAsNum(oCell.GetAttribute(XpsPaddingBottom), fuTwips, 0);
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
                          fuTwips,
                          oDrawArea.Right - oDrawArea.Left + 1);
        if R.Height <> 0 then
          CA.HeightSpc := R.Height
        else
          CA.HeightSpc :=
            EvalPropAsNum(oCell.GetAttribute(XpsHeight), fuTwips, 0);
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
      oDrawArea.Top + i * FBitmap.Canvas.TextHeight(' ');
    oDrawArea.Table.CurrentRow.Height := FBitmap.Canvas.TextHeight(' ');
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
      if (wSum < xWidth) then begin
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
    {fixup sizes and offsets}
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
procedure TXpFilterRTF.frPageinateDocument;
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
  FPrinting := False;

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

        wPageWidth := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                                    fuPixels, 0);
                                                                       {!!.57 - Start}
        { If there wasn't an page-width property we are going to use a
          default for a 8.5 inch wide page. }
        if (wPageWidth = 0) then begin
          wPageWidth := EvalPropAsNum('8.5in', fuPixels, 1);
          FCurPageMaster.SetPropertyValue(XpsPageWidth,
                                          IntToStr(wPageWidth),
                                          True);
        end;                                                           {!!.57 - End}

        wPageHeight := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                                     fuPixels, 0);
                                                                       {!!.57 - Start}
        { If there wasn't an page-heigth property we are going to use a
          default for a 11.5 inch high page. }
        if (wPageHeight = 0) then begin
          wPageHeight := EvalPropAsNum('11in', fuPixels, 1);
          FCurPageMaster.SetPropertyValue(XpsPageHeight,
                                          IntToStr(wPageHeight),
                                          True);
        end;                                                           {!!.57 - End}

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
          oDrawArea.Bottom := wPageHeight -
                              EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                                            fuPixels, 1);
          if (FCurPageMaster.HasRegionAfter) then
            oDrawArea.Bottom := oDrawArea.Bottom -
                                EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                              fuPixels, 1);

          { Get the right margin.}
          oDrawArea.Right := wPageWidth -
                             EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                                           fuPixels, 1);
           if (FCurPageMaster.HasRegionEnd) then
             oDrawArea.Right := oDrawArea.Right -
                                EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                              fuPixels, 1);

          { Get the left margin.}
          oDrawArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                          fuPixels, 1);
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
            bLoop := frRenderTree(oDrawArea,
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
    wPageWidth := EvalPropAsNum('8.5in', fuPixels, 1);
    wPageHeight := EvalPropAsNum('11in', fuPixels, 1);
    bLoop := True;
    try
      TempRegBody := TXpFORegionBody.Create;
      FCurPageMaster.AppendChild(TempRegBody);
      TempRegBody.Release;
      while bLoop do begin
        oPage := FRenderedDoc.Document.CreateElement('page');
        Inc(wPage);

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
          FCurPageMaster.SetPropertyValue(XpsPageWidth, IntToStr(wPageWidth), True);
          FCurPageMaster.SetPropertyValue(XpsPageHeight, IntToStr(wPageHeight), True);
          FCurPageMaster.SetPropertyValue(XpsMarginTop, '25mm', True);
          FCurPageMaster.SetPropertyValue(XpsMarginBottom, '25mm', True);
          FCurPageMaster.SetPropertyValue(XpsMarginRight, '25mm', True);
          FCurPageMaster.SetPropertyValue(XpsMarginLeft, '25mm', True);

          oNewAttrs := TXpFoPropertyGroupText.Create;
          try
            oNewAttrs.DoInherit(FCurPageMaster.Attributes);
            bLoop := frRenderTree(oDrawArea,
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
      FCurPageMaster.Free;
      FCurPageMaster := nil;
    end;
  end;

  FRenderedDoc.Document.DocumentElement.AppendChild(oDocFrag);
  oDocFrag.Release;

  FPageCount := wPage;
  FPageCurrent := 1;
end;
{--------}
procedure TXpFilterRTF.frRenderAfterRegion(aBodyArea : TXpDrawArea;
                                           const aPageInfo : RPageInfo);
var
  oAfterArea    : TXpDrawArea;
  oNewAttrs     : TXpFoPropertyGroupText;
  oElem         : TXpElement;
  TwipsToTop,
  TwipsToRight,
  TwipsToLeft,
  TwipsToBottom,
  j             : Integer;
begin
  if (HasOutRegionAfter) then begin
    FBlockLevel := 0;
    FOutRegionAfter.Document.DocumentElement.Normalize(False);
    FOutRegionAfter.Document.ActualCDATA := True;

    oAfterArea := TXpDrawArea.Create;
    try
      { Get top margin.}
      oAfterArea.Top := aBodyArea.Bottom;
      if (FCurPageMaster.HasRegionBody) then
        oAfterArea.Top := oAfterArea.Top +
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBottom),
                                        fuPixels, 1);
      { Get bottom margin.}
      oAfterArea.Bottom := oAfterArea.Top;
      if (FCurPageMaster.HasRegionAfter) then
        oAfterArea.Bottom := oAfterArea.Bottom +
                             EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                           fuPixels, 1);
      { Get left margin.}
      oAfterArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                       fuPixels, 1);
      if ((FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'true') and
          (FCurPageMaster.HasRegionStart) and
           (FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true')) then begin
        oAfterArea.Left := oAfterArea.Left +
                           EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                         fuPixels, 1);
        if (FCurPageMaster.HasRegionBody) then
          oAfterArea.Left := oAfterArea.Left +
                             EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                           fuPixels, 1);
      end;
      { Get right margin.}
      oAfterArea.Right := oAfterArea.Left + aPageInfo.PageWidth -
                          EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                                        fuPixels, 1);
      if ((FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'true') and
          (FCurPageMaster.HasRegionEnd) and
           (FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) = 'true')) then begin
        oAfterArea.Right := oAfterArea.Right -
                            EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                          fuPixels, 1);
        if (FCurPageMaster.HasRegionBody) then
          oAfterArea.Right := oAfterArea.Right -
                              EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                                            fuPixels, 1);
      end;

      oAfterArea.XPos := oAfterArea.Left;
      oAfterArea.YPos := oAfterArea.Top;
      oAfterArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionAfter.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        if (FPrinting) then begin
          { Create text frame for the region }
          FRTFData := FRTFData + '{\shp{\*\shpinst';

          { How far below the body region's top margin is the after
            region's top margin? }
          TwipsToTop := EvalPropAsNum(IntToStr(aBodyArea.Bottom - aBodyArea.Top),
                                      fuTwips, 1);
          if (FCurPageMaster.HasRegionBody) then
            TwipsToTop := TwipsToTop +
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingAfter),
                                        fuTwips, 1);
          FRTFData := FRTFData + '\shptop' + IntToStr(TwipsToTop);

          { How far to the left of the left margin is the before region's
            left margin? }
          TwipsToLeft := 0;
          if ((FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = 'true') or
              (not FCurPageMaster.HasRegionStart) or
              ((FCurPageMaster.HasRegionStart) and
               (FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true'))) then begin
            if (FCurPageMaster.HasRegionStart) then
              TwipsToLeft := TwipsToLeft -
                             EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                           fuTwips, 1);
            if (FCurPageMaster.HasRegionBody) then
              TwipsToLeft := TwipsToLeft -
                             EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                           fuTwips, 1);
          end;
          FRTFData := FRTFData + '\shpleft' + IntToStr(TwipsToLeft);

          { How far to the right of the left margin is the before region's
            right margin? }
          TwipsToRight := EvalPropAsNum(IntToStr(aBodyArea.Right -
                                                 EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPixels, 1)),
                                        fuTwips, 1);
          if ((FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = 'true') or
              (not FCurPageMaster.HasRegionEnd) or
              ((FCurPageMaster.HasRegionEnd) and
               (FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) <> 'true'))) then begin
            if (FCurPageMaster.HasRegionEnd) then
              TwipsToRight := TwipsToRight +
                              EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                            fuTwips, 1);
            if (FCurPageMaster.HasRegionBody) then
              TwipsToRight := TwipsToRight +
                              EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                                            fuTwips, 1);
          end;
          FRTFData := FRTFData + '\shpright' + IntToStr(TwipsToRight);

          { How far above the top of the body region's top margin is the
            bottom of the before region? }
          TwipsToBottom := TwipsToTop +
                           EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                         fuTwips, 1);
          if (FCurPageMaster.HasRegionBody) then
            TwipsToBottom := TwipsToBottom +
                             EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBottom),
                                           fuTwips, 1) * -1;
          FRTFData := FRTFData + '\shpbottom' + IntToStr(TwipsToBottom);

          FRTFData := FRTFData + '\shpfhdr0\shpbxcolumn\shpbxignore\shpbypara' +
                      '\shpbyignore\shpwr3\shpwrk0\shpfblwtxt0\shpz0\shplid1028' +
                      '{\sp{\sn shapeType}{\sv 202}}{\sp{\sn fFlipH}{\sv 0}}' +
                      '{\sp{\sn fFlipV}{\sv 0}}{\sp{\sn lTxid}{\sv 65536}}' +
                      '{\sp{\sn fLine}{\sv 0}}{\sp{\sn fLayoutInCell}{\sv 1}}' +
                      '{\sp{\sn dxTextLeft}{\sv 0}}{\sp{\sn dyTextTop}{\sv 0}}' +
                      '{\sp{\sn dxTextRight}{\sv 0}}{\sp{\sn dyTextBottom}' +
                      '{\sv 0}}{\sp{\sn fLine}{\sv 0}}' +
                      '{\shptxt \pard\plain \li0\ri0\widctlpar' +
                      '\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0';
          frRenderTree(oAfterArea,
                       FOutRegionAfter.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}}}';
          FRTFData := FRTFData + '{\shprslt{\*\do\dobxcolumn\dobypara' +
                      '\dodhgt8192\dptxbx\dptxlrtb{\dptxbxtext\pard\plain ' +
                      '\li0\ri0\widctlpar\aspalpha\aspnum\faauto' +
                      '\adjustright\rin0\lin0\itap0 ';
          frRenderTree(oAfterArea,
                       FOutRegionAfter.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}';

          { left }
          FRTFData := FRTFData + '\dpx' + IntToStr(TwipsToLeft);
          { top }
          FRTFData := FRTFData + '\dpy' + IntToStr(TwipsToTop);
          { length }
          FRTFData := FRTFData + '\dpxsize' + IntToStr(TwipsToRight + TwipsToLeft);
          { height }
          FRTFData := FRTFData + '\dpysize' + IntToStr(TwipsToTop - TwipsToBottom);

          FRTFData := FRTFData + '\dpfillfgcr255\dpfillfgcg255' +
                      '\dpfillfgcb255\dpfillbgcr255\dpfillbgcg255' +
                      '\dpfillbgcb255\dpfillpat1\dplinehollow}}';
        end;
      finally
        oNewAttrs.Free;
      end;
    finally
      oAfterArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterRTF.frRenderBeforeRegion(aBodyArea : TXpDrawArea;
                                            const aPageInfo : RPageInfo);
var
  oBeforeArea   : TXpDrawArea;
  oNewAttrs     : TXpFoPropertyGroupText;
  oElem         : TXpElement;
  TwipsToTop,
  TwipsToRight,
  TwipsToLeft,
  TwipsToBottom,
  j             : Integer;
begin
  if (HasOutRegionBefore) then begin
    FBlockLevel := 0;
    FOutRegionBefore.Document.DocumentElement.Normalize(False);
    FOutRegionBefore.Document.ActualCDATA := True;

    oBeforeArea := TXpDrawArea.Create;
    try
      { Get top margin.}
      oBeforeArea.Top := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop),
                                       fuPixels, 1);
      { Get bottom margin.}
      oBeforeArea.Bottom := oBeforeArea.Top;
      if (FCurPageMaster.HasRegionBefore) then
        oBeforeArea.Bottom := oBeforeArea.Bottom  +
                              EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                                            fuPixels, 1);
      { Get left margin.}
      oBeforeArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                        fuPixels, 1);
      if ((FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true') and
          (FCurPageMaster.HasRegionStart) and
           (FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true')) then begin
        oBeforeArea.Left := oBeforeArea.Left +
                            EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                          fuPixels, 1);
        if (FCurPageMaster.HasRegionBody) then
          oBeforeArea.Left := oBeforeArea.Left +
                              EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                            fuPixels, 1);
      end;
      { Get right margin.}
      oBeforeArea.Right := oBeforeArea.Left + aPageInfo.PageWidth -
                           EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                                         fuPixels, 1);
      if ((FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true') and
          (FCurPageMaster.HasRegionEnd) and
           (FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) = 'true')) then begin
        oBeforeArea.Right := oBeforeArea.Right -
                             EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                           fuPixels, 1);
        if (FCurPageMaster.HasRegionBody) then
          oBeforeArea.Right := oBeforeArea.Right -
                               EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                                             fuPixels, 1);
      end;

      oBeforeArea.XPos := oBeforeArea.Left;
      oBeforeArea.YPos := oBeforeArea.Top;
      oBeforeArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionBefore.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        if (FPrinting) then begin
          { Create text frame for the region }
          FRTFData := FRTFData + '{\shp{\*\shpinst';

          { How far above the top margin are we? }
          TwipsToTop := EvalPropAsNum(FCurPageMaster.RegionBefore.GetPropertyValue(XpsExtent),
                                      fuTwips, 1);
          if (FCurPageMaster.HasRegionBody) then
            TwipsToTop := TwipsToTop +
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBefore),
                                        fuTwips, 1);
          FRTFData := FRTFData + '\shptop' + IntToStr(TwipsToTop * -1);

          { How far to the left of the left margin is the before region's
            left margin? }
          TwipsToLeft := 0;
          if ((FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = 'true') or
              (not FCurPageMaster.HasRegionStart) or
              ((FCurPageMaster.HasRegionStart) and
               (FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true'))) then
            TwipsToLeft := TwipsToLeft - EvalPropAsNum(IntToStr(aBodyArea.Left -
                                                                EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPixels, 1)),
                                                       fuTwips, 1);
          FRTFData := FRTFData + '\shpleft' + IntToStr(TwipsToLeft);

          { How far to the right of the left margin is the before region's
            right margin? }
          if ((not FCurPageMaster.HasRegionEnd) or
              (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) = 'true') or
              (FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) <> 'true')) then
            TwipsToRight := EvalPropAsNum(IntToStr(aPageInfo.PageWidth - aBodyArea.Left),
                                          fuTwips, 1) -
                            EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                          fuTwips, 1)
          else
            TwipsToRight := EvalPropAsNum(IntToStr(aBodyArea.Right - aBodyArea.Left),
                                          fuTwips, 1);

          FRTFData := FRTFData + '\shpright' + IntToStr(TwipsToRight);

          { How far above the top of the body region's top margin is the
            bottom of the before region? }
          TwipsToBottom := EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingTop),
                                         fuTwips, 1) * -1;
          FRTFData := FRTFData + '\shpbottom' + IntToStr(TwipsToBottom);

          FRTFData := FRTFData + '\shpfhdr0\shpbxcolumn\shpbxignore\shpbypara' +
                      '\shpbyignore\shpwr3\shpwrk0\shpfblwtxt0\shpz0\shplid1028' +
                      '{\sp{\sn shapeType}{\sv 202}}{\sp{\sn fFlipH}{\sv 0}}' +
                      '{\sp{\sn fFlipV}{\sv 0}}{\sp{\sn lTxid}{\sv 65536}}' +
                      '{\sp{\sn fLine}{\sv 0}}{\sp{\sn fLayoutInCell}{\sv 1}}' +
                      '{\sp{\sn dxTextLeft}{\sv 0}}{\sp{\sn dyTextTop}{\sv 0}}' +
                      '{\sp{\sn dxTextRight}{\sv 0}}{\sp{\sn dyTextBottom}{\sv 0}}' +
                      '{\sp{\sn fLine}{\sv 0}}' +
                      '{\shptxt \pard\plain \li0\ri0\widctlpar' +
                      '\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0';
          frRenderTree(oBeforeArea,
                       FOutRegionBefore.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}}}';
          FRTFData := FRTFData + '{\shprslt{\*\do\dobxcolumn\dobypara' +
                      '\dodhgt8192\dptxbx\dptxlrtb{\dptxbxtext\pard\plain ' +
                      '\li0\ri0\widctlpar\aspalpha\aspnum\faauto' +
                      '\adjustright\rin0\lin0\itap0 \dptxbxmar0';
          frRenderTree(oBeforeArea,
                       FOutRegionBefore.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}';

          { left }
          FRTFData := FRTFData + '\dpx' + IntToStr(TwipsToLeft);
          { top }
          FRTFData := FRTFData + '\dpy' + IntToStr(TwipsToTop);
          { length }
          FRTFData := FRTFData + '\dpxsize' + IntToStr(TwipsToRight + TwipsToLeft);
          { height }
          FRTFData := FRTFData + '\dpysize' + IntToStr(TwipsToTop - TwipsToBottom);

          FRTFData := FRTFData + '\dpfillfgcr255\dpfillfgcg255' +
                      '\dpfillfgcb255\dpfillbgcr255\dpfillbgcg255' +
                      '\dpfillbgcb255\dpfillpat1\dplinehollow}}';
        end;
      finally
        oNewAttrs.Free;
      end;
    finally
      oBeforeArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterRTF.frRenderDocFormat;
begin
  FRTFData := FRTFData + #13#10;
  FRTFData := FRTFData + '\margb' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\margr' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\margt' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\margl' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\paperh' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\paperw' +
              IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                                     fuTwips, 1));
  FRTFData := FRTFData + '\nolead';
end;
{--------}
procedure TXpFilterRTF.frRenderEndRegion(aBodyArea : TXpDrawArea;
                                         const aPageInfo : RPageInfo);
var
  oEndArea      : TXpDrawArea;
  oNewAttrs     : TXpFoPropertyGroupText;
  oElem         : TXpElement;
  TwipsToTop,
  TwipsToRight,
  TwipsToLeft,
  TwipsToBottom,
  j             : Integer;
begin
  if (HasOutRegionEnd) then begin
    FBlockLevel := 0;
    FOutRegionEnd.Document.DocumentElement.Normalize(False);
    FOutRegionEnd.Document.ActualCDATA := True;

    oEndArea := TXpDrawArea.Create;
    try
      { Get top margin.}
      if ((not FCurPageMaster.HasRegionBefore) or
          ((FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) = 'true') and
           (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true'))) then
        oEndArea.Top := EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsMarginTop),
                                        fuPixels, 1)
      else
        oEndArea.Top := aBodyArea.Top;

      { Get bottom margin.}
      if ((not FCurPageMaster.HasRegionAfter) or
          ((FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) = 'true') and
           (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'true'))) then
        oEndArea.Bottom := aPageInfo.PageHeight -
                             EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                                           fuPixels, 1)
      else
        oEndArea.Bottom := aBodyArea.Bottom;

      { Get left margin.}
      oEndArea.Left := aBodyArea.Left;
      oEndArea.Left := oEndArea.Left +
                       EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                       fuPixels, 1);
      if (FCurPageMaster.HasRegionBody) then
        oEndArea.Left := oEndArea.Left +
                         EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingLeft),
                                       fuPixels, 1);
      { Get right margin.}
      oEndArea.Right := oEndArea.Left +
                          EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                        fuPixels, 1);

      oEndArea.XPos := oEndArea.Left;
      oEndArea.YPos := oEndArea.Top;
      oEndArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionEnd.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        if (FPrinting) then begin
          { Create text frame for the region }
          FRTFData := FRTFData + '{\shp{\*\shpinst';

          { How far below the body region's top margin is the after
            region's top margin? }
          if ((not FCurPageMaster.HasRegionBefore) or
              ((FCurPageMaster.RegionEnd.GetPropertyValue(XpsPrecedence) = 'true') and
               (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true'))) then begin
            TwipsToTop := EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                        fuTwips, 1) * -1;
            if (FCurPageMaster.HasRegionBody) then
              TwipsToTop := TwipsToTop + EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingTop),
                                                       fuTwips, 1)
          end else
            TwipsToTop := 0;
          FRTFData := FRTFData + '\shptop' + IntToStr(TwipsToTop);

          { How far to the left of the left margin is the before region's
            left margin? }
          TwipsToLeft := EvalPropAsNum(IntToStr(aBodyArea.Right - aBodyArea.Left),
                                       fuTwips, 1);
          if (FCurPageMaster.HasRegionBody) then
            TwipsToLeft := TwipsToLeft +
                           EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingEnd),
                                         fuTwips, 1);
          FRTFData := FRTFData + '\shpleft' + IntToStr(TwipsToLeft);

          { How far to the right of the left margin is the before region's
            right margin? }
          TwipsToRight := TwipsToLeft +
                          EvalPropAsNum(FCurPageMaster.RegionEnd.GetPropertyValue(XpsExtent),
                                        fuTwips, 1);
          FRTFData := FRTFData + '\shpright' + IntToStr(TwipsToRight);

          { How far above the top of the body region's top margin is the
            bottom of the before region? }
          TwipsToBottom := EvalPropAsNum(IntToStr(aBodyArea.Bottom - aBodyArea.Top),
                                         fuTwips, 1);
          if ((not FCurPageMaster.HasRegionAfter) or
              ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true') and
               (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = 'true'))) then begin
            TwipsToBottom := TwipsToBottom +
                             EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                           fuTwips, 1);
            if (FCurPageMaster.HasRegionBody) then
              TwipsToBottom := TwipsToBottom +
                               EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBottom),
                                             fuTwips, 1);
          end;
          FRTFData := FRTFData + '\shpbottom' + IntToStr(TwipsToBottom);

          FRTFData := FRTFData + '\shpfhdr0\shpbxcolumn\shpbxignore\shpbypara' +
                      '\shpbyignore\shpwr3\shpwrk0\shpfblwtxt0\shpz0\shplid1028' +
                      '{\sp{\sn shapeType}{\sv 202}}{\sp{\sn fFlipH}{\sv 0}}' +
                      '{\sp{\sn fFlipV}{\sv 0}}{\sp{\sn lTxid}{\sv 65536}}' +
                      '{\sp{\sn fLine}{\sv 0}}{\sp{\sn fLayoutInCell}{\sv 1}}' +
                      '{\sp{\sn dxTextLeft}{\sv 0}}{\sp{\sn dyTextTop}{\sv 0}}' +
                      '{\sp{\sn dxTextRight}{\sv 0}}{\sp{\sn dyTextBottom}' +
                      '{\sv 0}}{\sp{\sn fLine}{\sv 0}}' +
                      '{\shptxt \pard\plain \li0\ri0\widctlpar' +
                      '\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0';
          frRenderTree(oEndArea,
                       FOutRegionEnd.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}}}';
          FRTFData := FRTFData + '{\shprslt{\*\do\dobxcolumn\dobypara' +
                      '\dodhgt8192\dptxbx\dptxlrtb{\dptxbxtext\pard\plain ' +
                      '\li0\ri0\widctlpar\aspalpha\aspnum\faauto' +
                      '\adjustright\rin0\lin0\itap0 ';
          frRenderTree(oEndArea,
                       FOutRegionEnd.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}';

          { left }
          FRTFData := FRTFData + '\dpx' + IntToStr(TwipsToLeft);
          { top }
          FRTFData := FRTFData + '\dpy' + IntToStr(TwipsToTop);
          { length }
          FRTFData := FRTFData + '\dpxsize' + IntToStr(TwipsToRight - TwipsToLeft);
          { height }
          FRTFData := FRTFData + '\dpysize' + IntToStr(TwipsToTop + TwipsToBottom);

          FRTFData := FRTFData + '\dpfillfgcr255\dpfillfgcg255' +
                      '\dpfillfgcb255\dpfillbgcr255\dpfillbgcg255' +
                      '\dpfillbgcb255\dpfillpat1\dplinehollow}}';
        end;
      finally
        oNewAttrs.Free;
      end;
    finally
      oEndArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterRTF.frRenderGraphicLine(oDrawArea : TXpDrawArea;
                                           oAttrs    : TXpFOPropertyGroup);
begin
  if (FPrinting) then
    FRTFData := FRTFData + '\brdrs\brdrb\brdrl\brdrr\par';
  Inc(oDrawArea.YPos, EvalPropAsNum(oAttrs.GetProperty(XpsRuleThickness),
                                    fuPixels, 1));
end;
{--------}
procedure TXpFilterRTF.frRenderNewLine(oDrawArea : TXpDrawArea;
                                       oAttrs    : TXpFOPropertyGroup);
begin
  if ((FRTFData <> '') and
      (oDrawArea.YIncr <> 0)) then begin
    Inc(oDrawArea.YPos, oDrawArea.YIncr);
    oDrawArea.XPos := oDrawArea.Left +
                      EvalPropAsNum(oAttrs.GetProperty(XpsStartIndent), fuPixels, 1);
    if (FPrinting) then begin
      frFigureFont(oAttrs);
      FRTFData := FRTFData + Chr(13) + Chr(10) + '{\fs' +
                  IntToStr(EvalPropAsNum(IntToStr(oDrawArea.YIncr), fuPoints, 1) * 2) +
                  '\par}';
    end;
  end;
end;
{--------}
procedure TXpFilterRTF.frRenderPage(aPageNumber : Integer);
var
  oPageInfo : RPageInfo;
  oDrawArea : TXpDrawArea;
  oNewAttrs : TXpFoPropertyGroupText;
  oPageList : TXpNodeList;
  oElem     : TXpElement;
  ThePage   : TXpElement;
  j         : Integer;
begin
  { Select correct page master }
  oPageList := FRenderedDoc.Document.GetElementsByTagName('page');
  ThePage := TXpElement(oPageList.Item(aPageNumber - 1));
  FPageSequenceCount := ThePage.GetAttributeInt('XpPageSeqNum');
  if (ResultIsFO) then
    FCurPageMaster :=
      TXpFORoot(ResultTreeRoot).LayoutMaster.GetPageMaster(ThePage.GetAttribute('PageMaster'));

  oPageInfo.PageWidth := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                                       fuPixels, 1);
  oPageInfo.PageHeight := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                                        fuPixels, 1);

  { Render body }
  oDrawArea := TXpDrawArea.Create;
  try
    { Get top margin.}
    oDrawArea.Top := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginTop),
                                   fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Top := oDrawArea.Top +
                       EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsMarginTop),
                                     fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Top := oDrawArea.Top +
                       EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBefore),
                                     fuPixels, 1);
    { Get bottom margin.}
    oDrawArea.Bottom := oPageInfo.PageHeight -
                        EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                        fuPixels, 1);
    if (FCurPageMaster.HasRegionAfter) then
      oDrawArea.Bottom := oDrawArea.Bottom -
                          EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                        fuPixels, 1);
    if (FCurPageMaster.HasRegionBody) then
      oDrawArea.Bottom := oDrawArea.Bottom -
                          EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingAfter),
                                        fuPixels, 1);

    { Get right margin.}
    oDrawArea.Right := oPageInfo.PageWidth -
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
    oDrawArea.Left := EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                    fuPixels, 1);
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

    FRTFData := FRTFData + '\paperh' +
                IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageHeight),
                                       fuTwips, 1));
    FRTFData := FRTFData + '\paperw' +
                IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsPageWidth),
                                       fuTwips, 1));
    FRTFData := FRTFData + '\margb' +
                IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                                       fuTwips, 1));
    FRTFData := FRTFData + '\margr' +
                IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight),
                                       fuTwips, 1));
    FRTFData := FRTFData + '\margt' +
                IntToStr(EvalPropAsNum(IntToStr(oDrawArea.Top), fuTwips, 1));
    FRTFData := FRTFData + '\margl' +
                IntToStr(EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft),
                                       fuTwips, 1));

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
      FRTFData := FRTFData + '\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn' +
                  '\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade' +
                  '\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin1800' +
                  '\dgvorigin1440\dghshow1\dgvshow1 \jexpand\viewkind1\viewscale75' +
                  '\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp' +
                  '\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr' +
                  '\lnbrkrule \fet0\sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl' +
                  '{\*\pnseclvl1 \pnucrm\pnstart1\pnindent720\pnhang{\pntxta .}}' +
                  '{\*\pnseclvl2\pnucltr\pnstart1\pnindent720\pnhang{\pntxta .}}' +
                  '{\*\pnseclvl3\pndec\pnstart1\pnindent720\pnhang{\pntxta .}}' +
                  '{\*\pnseclvl4\pnlcltr\pnstart1\pnindent720\pnhang{\pntxta )}}' +
                  '{\*\pnseclvl5 \pndec\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}' +
                  '{\*\pnseclvl6\pnlcltr\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}' +
                  '{\*\pnseclvl7\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}' +
                  '{\*\pnseclvl8\pnlcltr\pnstart1\pnindent720\pnhang {\pntxtb (}{\pntxta )}}' +
                  '{\*\pnseclvl9\pnlcrm\pnstart1\pnindent720\pnhang{\pntxtb (}{\pntxta )}}' +
                  '\pard\plain ';
      FRTFData := FRTFData + '\li0\ri0\widctlpar\aspalpha\aspnum\faauto' +
                             '\adjustright\rin0\lin0\itap0 ' +
                             '\fs24\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 ' +
                             '{\fs20\lang1024\langfe1024\noproof';

      frRenderBeforeRegion(oDrawArea, oPageInfo);
      frRenderAfterRegion(oDrawArea, oPageInfo);
      frRenderStartRegion(oDrawArea, oPageInfo);
      frRenderEndRegion(oDrawArea, oPageInfo);
      FRTFData := FRTFData + '}';
      FBlockLevel := 0;
      try
        if ((aPageNumber > 0) and
            (aPageNumber <= oPageList.Length)) then
          frRenderTree(oDrawArea, ThePage, oNewAttrs, nil, False);
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
procedure TXpFilterRTF.frRenderStartRegion(aBodyArea : TXpDrawArea;
                                           const aPageInfo : RPageInfo);
var
  oStartArea    : TXpDrawArea;
  oNewAttrs     : TXpFoPropertyGroupText;
  oElem         : TXpElement;
  TwipsToTop,
  TwipsToRight,
  TwipsToLeft,
  TwipsToBottom,
  j             : Integer;
begin
  if (HasOutRegionStart) then begin
    FBlockLevel := 0;
    FOutRegionStart.Document.DocumentElement.Normalize(False);
    FOutRegionStart.Document.ActualCDATA := True;

    oStartArea := TXpDrawArea.Create;
    try
      { Get top margin.}
      if ((not FCurPageMaster.HasRegionBefore) or
          ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true') and
           (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true'))) then
        oStartArea.Top := EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsMarginTop),
                                        fuPixels, 1)
      else
        oStartArea.Top := aBodyArea.Top;

      { Get bottom margin.}
      if ((not FCurPageMaster.HasRegionAfter) or
          ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true') and
           (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) <> 'true'))) then
        oStartArea.Bottom := aPageInfo.PageHeight -
                             EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginBottom),
                                           fuPixels, 1)
      else
        oStartArea.Bottom := aBodyArea.Bottom;

      { Get left margin.}
      oStartArea.Left := aBodyArea.Left;
      oStartArea.Left := oStartArea.Left +
                         EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                       fuPixels, 1);
      if (FCurPageMaster.HasRegionBody) then
        oStartArea.Left := oStartArea.Left +
                         EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingLeft),
                                       fuPixels, 1);
      { Get right margin.}
      oStartArea.Right := oStartArea.Left +
                          EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                        fuPixels, 1);

      oStartArea.XPos := oStartArea.Left;
      oStartArea.YPos := oStartArea.Top;
      oStartArea.YIncr := 0;

      oNewAttrs := TXpFoPropertyGroupText.Create;
      try
        oNewAttrs.DoInherit(FCurPageMaster.Attributes);
        oElem := TXpElement(FOutRegionStart.DocumentElement);
        if (oElem.HasAttributes) then begin
          for j := 0 to (oElem.Attributes.Length - 1) do
            oNewAttrs.SetProperty(LowerCase(oElem.Attributes.Item(j).NodeName),
                                  oElem.Attributes.Item(j).NodeValue, True);
        end;
        if (FPrinting) then begin
          { Create text frame for the region }
          FRTFData := FRTFData + '{\shp{\*\shpinst';

          { How far below the body region's top margin is the after
            region's top margin? }
          if ((not FCurPageMaster.HasRegionBefore) or
              ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true') and
               (FCurPageMaster.RegionBefore.GetPropertyValue(XpsPrecedence) <> 'true'))) then begin
            TwipsToTop := EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                        fuTwips, 1) * -1;
            if (FCurPageMaster.HasRegionBody) then
              TwipsToTop := TwipsToTop + EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingTop),
                                                       fuTwips, 1)
          end else
            TwipsToTop := 0;
          FRTFData := FRTFData + '\shptop' + IntToStr(TwipsToTop);

          { How far to the left of the left margin is the before region's
            left margin? }
          TwipsToLeft := 0;
          if ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) = 'true') or
              (not FCurPageMaster.HasRegionStart) or
              ((FCurPageMaster.HasRegionStart) and
               (FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true'))) then begin
            TwipsToLeft := TwipsToLeft -
                           EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                         fuTwips, 1);
            if (FCurPageMaster.HasRegionBody) then
              TwipsToLeft := TwipsToLeft -
                             EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingStart),
                                           fuTwips, 1);
          end;
          FRTFData := FRTFData + '\shpleft' + IntToStr(TwipsToLeft);

          { How far to the right of the left margin is the before region's
            right margin? }
          TwipsToRight := TwipsToLeft +
                          EvalPropAsNum(FCurPageMaster.RegionStart.GetPropertyValue(XpsExtent),
                                        fuTwips, 1);
          FRTFData := FRTFData + '\shpright' + IntToStr(TwipsToRight);

          { How far above the top of the body region's top margin is the
            bottom of the before region? }
          TwipsToBottom := EvalPropAsNum(IntToStr(aBodyArea.Bottom - aBodyArea.Top),
                                         fuTwips, 1);
          if ((not FCurPageMaster.HasRegionAfter) or
              ((FCurPageMaster.RegionStart.GetPropertyValue(XpsPrecedence) <> 'true') and
               (FCurPageMaster.RegionAfter.GetPropertyValue(XpsPrecedence) = 'true'))) then begin
            TwipsToBottom := TwipsToBottom +
                             EvalPropAsNum(FCurPageMaster.RegionAfter.GetPropertyValue(XpsExtent),
                                           fuTwips, 1);
            if (FCurPageMaster.HasRegionBody) then
              TwipsToBottom := TwipsToBottom +
                               EvalPropAsNum(FCurPageMaster.RegionBody.GetPropertyValue(XpsPaddingBottom),
                                             fuTwips, 1);
          end;
          FRTFData := FRTFData + '\shpbottom' + IntToStr(TwipsToBottom);

          FRTFData := FRTFData + '\shpfhdr0\shpbxcolumn\shpbxignore\shpbypara' +
                      '\shpbyignore\shpwr3\shpwrk0\shpfblwtxt0\shpz0\shplid1028' +
                      '{\sp{\sn shapeType}{\sv 202}}{\sp{\sn fFlipH}{\sv 0}}' +
                      '{\sp{\sn fFlipV}{\sv 0}}{\sp{\sn lTxid}{\sv 65536}}' +
                      '{\sp{\sn fLine}{\sv 0}}{\sp{\sn fLayoutInCell}{\sv 1}}' +
                      '{\sp{\sn dxTextLeft}{\sv 0}}{\sp{\sn dyTextTop}{\sv 0}}' +
                      '{\sp{\sn dxTextRight}{\sv 0}}{\sp{\sn dyTextBottom}' +
                      '{\sv 0}}{\sp{\sn fLine}{\sv 0}}' +
                      '{\shptxt \pard\plain \li0\ri0\widctlpar' +
                      '\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0';
          frRenderTree(oStartArea,
                       FOutRegionStart.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}}}';
          FRTFData := FRTFData + '{\shprslt{\*\do\dobxcolumn\dobypara' +
                      '\dodhgt8192\dptxbx\dptxlrtb{\dptxbxtext\pard\plain ' +
                      '\li0\ri0\widctlpar\aspalpha\aspnum\faauto' +
                      '\adjustright\rin0\lin0\itap0 ';
          frRenderTree(oStartArea,
                       FOutRegionStart.DocumentElement,
                       oNewAttrs, nil, False);
          FRTFData := FRTFData + '}';

          { left }
          FRTFData := FRTFData + '\dpx' + IntToStr(TwipsToLeft);
          { top }
          FRTFData := FRTFData + '\dpy' + IntToStr(TwipsToTop);
          { length }
          FRTFData := FRTFData + '\dpxsize' + IntToStr(TwipsToRight + TwipsToLeft);
          { height }
          FRTFData := FRTFData + '\dpysize' + IntToStr(TwipsToTop - TwipsToBottom);

          FRTFData := FRTFData + '\dpfillfgcr255\dpfillfgcg255' +
                      '\dpfillfgcb255\dpfillbgcr255\dpfillbgcg255' +
                      '\dpfillbgcb255\dpfillpat1\dplinehollow}}';
        end;
      finally
        oNewAttrs.Free;
      end;
    finally
      oStartArea.Free;
    end;
  end;
  FInText := False;
end;
{--------}
procedure TXpFilterRTF.frRenderText(oDrawArea : TXpDrawArea;
                                    oAttrs    : TXpFOPropertyGroup;
                                    sText     : string);
var
  wTextHeight,
  wLen         : Integer;
  sWord,
  sTmp         : string;
begin
  { Set font and color }
  FBitmap.Canvas.Font.PixelsPerInch := FHPixelsPerInch;
  frFigureFont(oAttrs);
  if FPrinting then begin
    if (not FInText) then begin
      FBlockLevel := FBlockLevel + 1;
      FInText := True;
    end;
    if ((oAttrs.GetProperty(XpsTextAlign) = 'end') or
        (oAttrs.GetProperty(XpsTextAlign) = 'right')) then begin
      if (FRightAligned <> True) then begin
        FRightAligned := True;
        FRTFData := FRTFData + '\qr ';
      end;
    end else if (oAttrs.GetProperty(XpsTextAlign) = 'center') then begin
      if (FCentered <> True) then begin
        FCentered := True;
        FRTFData := FRTFData + '\qc ';
      end;
    end else begin
      FRightAligned := False;
      FCentered := False;
      FRTFData := FRTFData + '\ql ';
    end;
  end;

  { Draw text }
  while (sText <> '') do begin
    case sText[1] of
      #$0d :
        begin
          sTmp := oAttrs.GetProperty(XpsWhiteSpace);
          if (sTmp = 'no-wrap') or
             (sTmp = 'pre') then
            frRenderNewline(oDrawArea, oAttrs);
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
            Inc(oDrawArea.XPos, FBitmap.Canvas.TextWidth(' '));
            FRTFData := FRTFData + ' ';
          end;
          Delete(sText, 1, 1);
        end;
      else begin
        sWord := XpGetWord(sText);
        Delete(sText, 1, Length(sWord));
        wLen := FBitmap.Canvas.TextWidth(sWord);
        sTmp := oAttrs.GetProperty(XpsWhiteSpace);
        if not ((sTmp = 'no-wrap') or
                (sTmp = 'pre')) then begin
          if ((oDrawArea.XPos + wLen) > oDrawArea.Right) then begin
            frRenderNewLine(oDrawArea, oAttrs);
            Inc(oDrawArea.XPos,
                EvalPropAsNum(oAttrs.GetProperty(XpsTextIndent),
                              fuPixels, 1));
          end;
        end;
        if ((oDrawArea.XPos < oDrawArea.Right) and
            ((oDrawArea.YPos + FBitmap.Canvas.TextHeight(' ')) <
              oDrawArea.Bottom)) then
          if FPrinting then
            FRTFData := FRTFData + sWord;
        wTextHeight := frCalcYInc(FBitmap.Canvas.TextHeight(' '));
        if (wTextHeight > oDrawArea.YIncr) then
          oDrawArea.YIncr := wTextHeight;
        Inc(oDrawArea.XPos, wLen);
      end;
    end;
  end;
end;
{--------}
function TXpFilterRTF.frRenderTree(oDrawArea : TXpDrawArea;
                                   oTree     : TXpElement;
                                   oAttrs    : TXpFOPropertyGroup;
                                   oPage     : TXpElement;
                                   bTop      : Boolean) : Boolean;
var
  i,
  j,
  x,
  y,
  wWidth       : Integer;
  oElem        : TXpElement;
  oNewAttrs    : TXpFoPropertyGroup;
  sCommand,
  sBreakAfter,
  sBreakBefore,
  sMarginStart,
  sMarginEnd   : string;
  oOldDrawArea : TXpDrawArea;
  bInheritable : Boolean;
  oTmpNode     : TXpNode;
  CS: TXpTableCellArea;
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
                case oElem.Attributes.Item(j).NodeId of
                  FOP_MARGIN_RIGHT,
                  FOP_MARGIN_LEFT,
                  FOP_BREAK_BEFORE,
                  FOP_BREAK_AFTER,
                  FOP_BACKGROUND_COLOR,
                  FOP_SPACE_AFTER_MAX,
                  FOP_SPACE_AFTER_MIN,
                  FOP_SPACE_AFTER_OPT,
                  FOP_SPACE_BEFORE_MAX,
                  FOP_SPACE_BEFORE_MIN,
                  FOP_SPACE_BEFORE_OPT :
                    bInheritable := False;
                else
                  bInheritable := True;
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
            if ((sCommand = XpsDiv) or
                (sCommand = XpsP)) then begin
              if ((FInText) and
                  (FBlockLevel > 0) and
                  (oElem.ParentNode.NodeName <> XpsTD)) then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oAttrs.GetProperty(XpsFontSize),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
                FBlockLevel := FBlockLevel - 1;
              end;
              FInText := False;
              { If there, handle the SpaceBefore property.}
              if (oNewAttrs.GetProperty(XpsSpaceBeforeOptimum) <> '') then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeOptimum),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBefore) <> '') then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBefore),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMinimum) <> '') then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMinimum),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceBeforeMaximum) <> '') then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMaximum),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end;
            end else if (sCommand = XpsBR) then begin
              if (oDrawArea.YIncr = 0) then
                oDrawArea.YIncr := frCalcYInc(FBitmap.Canvas.TextHeight(' '));
              frRenderNewline(oDrawArea, oNewAttrs);
            end else if (sCommand = XpsHR) then begin
              if oDrawArea.XPos <> oDrawArea.Left then
                frRenderNewline(oDrawArea, oNewAttrs);
              if ((oElem.PreviousSibling <> nil) and
                  (oElem.PreviousSibling.NodeType = TEXT_NODE)) then begin
                if (FPrinting) then
                  FRTFData := FRTFData + '\par';
                Inc(oDrawArea.YPos, oDrawArea.YIncr);
                FBlockLevel := FBlockLevel - 1;
              end;
              frRenderGraphicLine(oDrawArea, oNewAttrs);
              frRenderNewline(oDrawArea, oNewAttrs);
            end else if (sCommand = XpsPAGENUM) then begin
              frRenderText(oDrawArea, oNewAttrs, IntToStr(FPageCurrent));
            end else if (sCommand = XpsTABLE) then begin
              frRenderNewLine(oDrawArea, oNewAttrs);
              oDrawArea.YIncr := frCalcYInc(FBitmap.Canvas.TextHeight(' ')) shr 1;
              if (oNewAttrs.GetProperty(XpsSpaceBeforeOptimum) <> '') then
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeOptimum),
                                           fuPixels, 1))
              else if (oNewAttrs.GetProperty(XpsSpaceBefore) <> '') then
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBefore),
                                           fuPixels, 1))
              else if (oNewAttrs.GetProperty(XpsSpaceBeforeMinimum) <> '') then
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMinimum),
                                           fuPixels, 1))
              else if (oNewAttrs.GetProperty(XpsSpaceBeforeMaximum) <> '') then
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceBeforeMaximum),
                                           fuPixels, 1));
              frRenderNewLine(oDrawArea, oNewAttrs);

              frFigureTableInfo(oDrawArea, oElem, oNewAttrs);
              oDrawArea.Table.SelectRow := 0;
              if (oDrawArea.Table.CurrentRow <> nil) then begin
                oDrawArea.Table.CurrentRow.Top := oDrawArea.YPos;
                oDrawArea.YIncr := frCalcYInc(FBitmap.Canvas.TextHeight(' ')) shr 1;
                frRenderNewLine(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTR) then begin
              if FPrinting then
                begin
                  FRTFData := FRTFData +
                    '\trowd\trgaph108\trleft-108 ' +
                    '\trbrdrt\brdrs\brdrw10 ' +
                    '\trbrdrl\brdrs\brdrw10 ' +
                    '\trbrdrb\brdrs\brdrw10 ' +
                    '\trbrdrr\brdrs\brdrw10 '#13#10;
                  x := 0;
                  for y := 0 to
                    oDrawArea.Table.CurrentRow.TableCells.Count - 1 do begin
                    oDrawArea.Table.CurrentRow.SelectCell := y;
                    CS := oDrawArea.Table.CurrentRow.Cell[y];
                    inc(x, CS.Width);
                    FRTFData := FRTFData +
                      '\clbrdrt\brdrw15\brdrs ' +
                      '\clbrdrl\brdrw15\brdrs ' +
                      '\clbrdrb\brdrw15\brdrs ' +
                      '\clbrdrr\brdrw15\brdrs ' +
                      '\cellx' + IntToStr(x) + ' '#13#10;
                  end;
                  FRTFData := FRTFData + '\pard '#13#10;
                end;
              oDrawArea.Table.CurrentRow.SelectCell := 0;
            end else if (sCommand = XpsTD) then begin
              if (FPrinting) then
                FRTFData := FRTFData + '\intbl ';

              oDrawArea.Table.CurrentRow.CurrentCell.Top :=
                oDrawArea.Table.CurrentRow.Top;
              oOldDrawArea := oDrawArea;
              oDrawArea := TXpDrawArea.Create;
              oDrawArea.Top :=
                oOldDrawArea.Table.CurrentRow.Top +
                EvalPropAsNum(oNewAttrs.GetProperty(XpsPaddingTop),
                              fuPixels, 1);
              oDrawArea.Left :=
                oOldDrawArea.Table.CurrentRow.CurrentCell.Left +
                EvalPropAsNum(oNewAttrs.GetProperty(XpsPaddingLeft),
                              fuPixels, 1);
              oDrawArea.Right :=
                oDrawArea.Left +
                oOldDrawArea.Table.CurrentRow.CurrentCell.Width -
                (EvalPropAsNum(oNewAttrs.GetProperty(XpsPaddingLeft), fuPixels, 1) +
                 EvalPropAsNum(oNewAttrs.GetProperty(XpsPaddingRight), fuPixels, 1));
              oDrawArea.Bottom :=
                oOldDrawArea.Bottom -
                EvalPropAsNum(oNewAttrs.GetProperty(XpsPaddingBottom),
                              fuPixels, 1);
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
            Result := frRenderTree(oDrawArea, oElem, oNewAttrs, oPage, False);

            if (sMarginStart <> '') then
              Dec(oDrawArea.Left, EvalPropAsNum(sMarginStart, fuPixels, 1));
            if (sMarginEnd <> '') then
              Inc(oDrawArea.Right, EvalPropAsNum(sMarginEnd, fuPixels, 1));

            if ((sCommand = XpsDiv) or
                (sCommand = XpsP)) then begin
              { If this element doesn't have an DIV children, then we
                haven't skiped to the next line yet.}
              if ((oElem.FindElement(XpsDiv) = nil) and
                  (oElem.ParentNode.NodeName <> XpsTD))  then begin
                oDrawArea.YIncr := frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsFontSize),
                                                            fuPixels, 1));
                frRenderNewline(oDrawArea, oNewAttrs);
                FBlockLevel := FBlockLevel - 1;
              end else
                FBlockLevel := FBlockLevel - 1;
              if (oNewAttrs.GetProperty(XpsSpaceAfterOptimum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterOptimum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfter) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfter),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMinimum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMinimum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMaximum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMaximum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oAttrs);
              end;
            end else if (sCommand = XpsTABLE) then begin
              { Draw table border }
              if FPrinting then
                FRTFData := FRTFData + '\pard '#13#10;
              wWidth := EvalPropAsNum(oElem.GetAttribute('border-width'),
                                      fuPixels, 1);
              for x := 0 to (oDrawArea.Table.TableRows.Count - 1) do begin
                oDrawArea.Table.SelectRow := x;
                for y := 0 to
                    oDrawArea.Table.CurrentRow.TableCells.Count - 1 do begin
                  oDrawArea.Table.CurrentRow.SelectCell := y;
                  if (wWidth > 0) then
                    FBitmap.Canvas.Pen.Width := wWidth
                  else
                    FBitmap.Canvas.Pen.Width :=
                      EvalPropAsNum(oElem.GetAttribute(XpsBorderTopWidth),
                                    fuPixels, 1);
                  if (wWidth > 0) then
                    FBitmap.Canvas.Pen.Width := wWidth
                  else
                    FBitmap.Canvas.Pen.Width := EvalPropAsNum(oElem.GetAttribute(XpsBorderRightWidth),
                                                              fuPixels, 1);
                  if (wWidth > 0) then
                    FBitmap.Canvas.Pen.Width := wWidth
                  else
                    FBitmap.Canvas.Pen.Width := EvalPropAsNum(oElem.GetAttribute(XpsBorderBottomWidth),
                                                              fuPixels, 1);
                  if (wWidth > 0) then
                    FBitmap.Canvas.Pen.Width := wWidth
                  else
                    FBitmap.Canvas.Pen.Width := EvalPropAsNum(oElem.GetAttribute(XpsBorderLeftWidth),
                                                              fuPixels, 1);
                end;
              end;
              oDrawArea.Table.Free;
              oDrawArea.Table := TXpTableArea.Create;
              oDrawArea.YIncr := frCalcYInc(FBitmap.Canvas.TextHeight(' ')) shr 1;
              frRenderNewLine(oDrawArea, oNewAttrs);
              if (oNewAttrs.GetProperty(XpsSpaceAfterOptimum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterOptimum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMinimum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMinimum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oNewAttrs);
              end else if (oNewAttrs.GetProperty(XpsSpaceAfterMaximum) <> '') then begin
                oDrawArea.YIncr :=
                  frCalcYInc(EvalPropAsNum(oNewAttrs.GetProperty(XpsSpaceAfterMaximum),
                                           fuPixels, 1));
                frRenderNewline(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTR) then begin
              if FPrinting then
              FRTFData := FRTFData + '\row '#13#10;
              { Compute row size based on largest cell }
              x := 0;
              for y := 0 to
                  (oDrawArea.Table.CurrentRow.TableCells.Count - 1) do begin
                oDrawArea.Table.CurrentRow.SelectCell := y;
                if oDrawArea.Table.CurrentRow.CurrentCell.HeightClc > x then
                  x := oDrawArea.Table.CurrentRow.CurrentCell.HeightClc;
              end;
              if (x > oDrawArea.Table.CurrentRow.Height) then
                oDrawArea.Table.CurrentRow.Height := x;
              oDrawArea.YPos := oDrawArea.Table.CurrentRow.Top +
                                oDrawArea.Table.CurrentRow.Height;
              Inc(oDrawArea.Table.SelectRow);
              if oDrawArea.Table.CurrentRow <> nil then begin
                oDrawArea.Table.CurrentRow.Top := oDrawArea.YPos;
                oDrawArea.YIncr := frCalcYInc(FBitmap.Canvas.TextHeight(' ')) shr 1;
                frRenderNewLine(oDrawArea, oNewAttrs);
              end;
            end else if (sCommand = XpsTD) then begin
              if FPrinting then
                FRTFData := FRTFData + '\cell ' + Chr(13) + Chr(10);
              Inc(oDrawArea.YPos, FBitmap.Canvas.TextHeight(' ') shr 2);
              if (oDrawArea.YPos - oOldDrawArea.Table.CurrentRow.CurrentCell.Top >
                  oOldDrawArea.Table.CurrentRow.CurrentCell.HeightClc) then
                oOldDrawArea.Table.CurrentRow.CurrentCell.HeightClc :=
                  oDrawArea.YPos - oOldDrawArea.Table.CurrentRow.CurrentCell.Top;
              oDrawArea.Free;
              oDrawArea := oOldDrawArea;
              Inc(oDrawArea.Table.CurrentRow.SelectCell);
            end;
          finally
            oNewAttrs.Free;
          end;
        end;
      TEXT_NODE, CDATA_SECTION_NODE :
        begin
          frRenderText(oDrawArea,
                       oAttrs,
                       GetRTFText(TXpText(oTree.ChildNodes.Item(i))));
        end;
    end;

    { Check for page breaks }
    if Result then
      Exit;

    if (oPage <> nil) then begin
      { Go to next page...}

      {...if we've filled this one up.}
      if ((oDrawArea.YPos > oDrawArea.Bottom) and
          (i + 1 < oTree.Childnodes.Length)) then begin
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
function TXpFilterRTF.GetFontFamily(const aFontName : string) : string;
var
  Idx : Integer;
begin
  Idx := FFontFamilyList.IndexOfName(aFontName);
  if (Idx <> -1) then
    Result := FFontFamilyList.Values[FFontFamilyList.Names[Idx]]
  else
    Result := 'nil';
end;
{--------}
procedure TXpFilterRTF.InitColorTable;
begin
  FColorList.Clear;
  FColorList.Add('\red0\green0\blue0;') //Black
end;
{--------}
procedure TXpFilterRTF.InitFontTable;
begin
  FFontList.Clear;
  FFontList.Add('Times New Roman')
end;
{--------}
function TXpFilterRTF.GetRtfDocument : DOMString;
begin
  Result := Render;
end;
{--------}
function TXpFilterRTF.GetRTFText(aTextNode : TXpText) : DOMString;
var
  TmpStr : DOMString;
begin
  Result := '';
  TmpStr := aTextNode.Data;
  { Since we havn't normalized textdata, we have to throw away "empty strings". }
  if (TmpStr <> '') then begin
    if (not (aTextNode is TXpCDATASection)) then begin
      TmpStr := XpStringReplaceAll(TmpStr, '\', '\\');
      TmpStr := XpStringReplaceAll(TmpStr, '{', '\{');
      TmpStr := XpStringReplaceAll(TmpStr, '}', '\}');
    end else if (aTextNode is TXpCDATASection) then begin
      if (FInTable) then
        TmpStr := XpStringReplaceAll(TmpStr, Chr(13) + Chr(10), '\par\intbl ')
      else
        TmpStr := XpStringReplaceAll(TmpStr, Chr(13) + Chr(10), '\par ');
    end;
    Result := TmpStr;
  end;
end;
{--------}
procedure TXpFilterRTF.Paint;
var
  oBit : TBitmap;
begin
  if (csDesigning in ComponentState) then begin
    oBit := TBitmap.Create;
    oBit.Transparent := True;
    oBit.TransparentMode := tmAuto;
    oBit.LoadFromResourceName(HInstance, 'TXPFILTERRTF');
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
function TXpFilterRTF.Render : string;
var
  i : Integer;
begin
  if (FCurPageMaster = nil) then begin                                 {!!.57 - Start}
    fbSelectPageMaster(0, 1);
    if (FCurPageMaster = nil) then
      Exit;
  end;                                                                 {!!.57 - End}

  FPrinting := True;

  CreateRTFHeader;
  frRenderDocFormat;

  for i := 1 to FPageCount do begin
    if (i <> 1) then
      FRTFData := FRTFData + #13#10 + '{\page}' + #13#10;
    FPageCurrent := i;
    frRenderPage(i);
  end;
  FRTFData := FRTFData + '}';
  Result := FRTFData;
  FPrinting := False;
end;
{--------}
function TXpFilterRTF.RenderFile(const aFileName : string) : Boolean;
begin
  Result := inherited RenderFile(aFileName);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
function TXpFilterRTF.RenderMemory(var Buffer; aSize : Integer) : Boolean;
begin
  Result := inherited RenderMemory(Buffer, aSize);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
function TXpFilterRTF.RenderStream(aStream : TStream) : Boolean;
begin
  Result := inherited RenderStream(aStream);
  XslProcessorApplyStyleEnd(self);
end;
{--------}
procedure TXpFilterRTF.Reset;
begin
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;
  inherited;
  InitColorTable;
  InitFontTable;
  FOutRegionCurrent := OutRegionBody;
  FCurElement := FOutRegionCurrent.Document.DocumentElement;
  FCurAttr := FCurElement;
  FCentered := False;
  FRightAligned := False;
  FInText := False;
  FBlockLevel := 0;
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

function TXpFilterRTF.SaveToFile(const sFile : string) : Boolean;      {!!.57}
var
  hf    : THandle;
  sText : string;
  dwTmp : DWord;
begin
  sText := Render;
  hF := CreateFile(PChar(StripFile(sFile)),                            {!!.57}
                   GENERIC_WRITE,
                   0,
                   nil,
                   CREATE_ALWAYS,
                   FILE_ATTRIBUTE_NORMAL,
                   0);
  Result := (hF <> INVALID_HANDLE_VALUE);
  if (Result) then begin
    WriteFile(hF, PChar(sText)^, Length(sText), dwTmp, nil);
    CloseHandle(hF);
  end;
end;
{--------}
function TXpFilterRTF.SaveToStream(aStream : TStream) : Boolean;
var
  sText : string;                                                      {!!.57}
begin
  Result := True;
  sText := Render;
  try
    aStream.WriteBuffer(sText[1], Length(sText));
  except
    Result := False;
  end;
end;
{--------}
procedure TXpFilterRTF.SetBounds(aLeft, aTop, aWidth, aHeight : Integer);
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
procedure TXpFilterRTF.XslProcessorApplyStyleEnd(Sender: TObject);
begin
  inherited;

  if (FRenderedDoc <> nil) then
    FRenderedDoc.ClearDocument
  else
    FRenderedDoc := TXpObjModel.Create(nil);
    
  if ((not ResultIsFO) and
      (FCurPageMaster <> nil)) then begin
    FCurPageMaster.Free;
    FCurPageMaster := nil;
  end;

  InitRegion(FRenderedDoc, XpsSPAN);
  frPageinateDocument;
end;
{--------}
procedure TXpFilterRTF.XslProcessorApplyStyleStart(Sender : TObject);
begin
  Reset;
end;
{--------}
procedure TXpFilterRTF.XslProcessorAttribute(oOwner : TObject;
                                             sName,
                                             sValue : DOMString);
begin
  {sName := LowerCase(sName);}                                         {!!.57}
  if ((FCurAttr <> nil) and
      (sValue <> '')) then
    FCurAttr.SetAttribute(LowerCase(sName), sValue);                   {!!.57}
end;
{--------}
procedure TXpFilterRTF.XslProcessorCDATASection(oOwner   : TObject;
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
procedure TXpFilterRTF.XslProcessorElementEnd(oOwner : TObject;
                                              sValue : DOMString);
begin
  SetParentToCurrent;
end;
{--------}
procedure TXpFilterRTF.XslProcessorElementStart(oOwner : TObject;
                                                sValue : DOMString);
begin
  inherited XslProcessorElementStart(oOwner, sValue);

  FCurElement.SetAttribute('XpPageSeqNum', IntToStr(FPageSequenceCount));

  if (FCurElement.NodeName = XpsBR) then
    FCurElement.SetAttribute('line', '\line')
  else if (FCurElement.NodeName = 'U') then
    FCurElement.SetAttribute('u', '\ul');
end;
{--------}
procedure TXpFilterRTF.XslProcessorText(oOwner : TObject;
                                        sValue : DOMString);    
var
  oText : TXpText;
begin
  if (FCurElement <> nil) then begin
    oText := FOutRegionCurrent.Document.CreateTextNode(sValue);
    FCurElement.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
    oText.Release;
  end;
end;
{--------}
procedure TXpFilterRTF.XslProcessorTextNode(oOwner : TObject;
                                            oNode  : TXpText);
var
  oText : TXpText;
begin
  if (FCurElement <> nil) then begin
    oText := TXpText(oNode.CloneNode(True));
    FCurElement.ForceOwnerDocument(oText);
    FCurElement.AppendChild(oText);
    oText.Release;
  end;
end;
procedure TXpFilterRTF.fbSaveColumnAttrs(const aSourceElem : TXpElement);
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

{=====================================================================}
procedure TXpFilterRTF.XslProcessorFormatObject(Sender        : TObject;
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
        XslProcessorElementStart(Sender, XpsDIV);
        FCurElement.SetAttribute('par', '\par' {\par});
        FLastBlockElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
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
        XslProcessorElementStart(Sender, 'Sequence');
        FLastInlineElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
      end;
    FO_LEADER :
      begin
        XslProcessorElementStart(Sender, XpsHR);
        fbCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_BLOCK :
      begin
        XslProcessorElementStart(Sender, XpsTABLE);
        FInTable := True;
        FTableElem := FCurElement;
        FTableWidth := STANDARDPAGEWIDTH -
                       EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginRight), fuPixels, 1) -
                       EvalPropAsNum(FCurPageMaster.GetPropertyValue(XpsMarginLeft), fuPixels, 1);
        FCurElement.SetAttribute(XpsWidth, '100%');
        fbCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_ITEM :
      begin
        XslProcessorElementStart(Sender, XpsTR);
        FLastTRElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
      end;
    FO_LIST_ITEM_BODY :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.SetAttribute(XpsWidth, '90%');
        fbCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
        Inc(FCellCnt);
      end;
    FO_LIST_ITEM_LABEL :
      begin
        XslProcessorElementStart(Sender, XpsTD);
        FLastTDElem := FCurElement;
        FCurElement.SetAttribute(XpsWidth, '10%');
        fbCopyAttrsToCurrent(aFormatObject);
        FCurAttr := FCurElement;
        Inc(FCellCnt);
      end;
    FO_PAGE_NUMBER :
      begin
        XslProcessorElementStart(Sender, XpsPAGENUM);
        fbCopyAttrsToCurrent(aFormatObject);
      end;
    FO_STATIC_CONTENT :
      begin
        XslProcessorElementStart(Sender, 'STATIC');
        fbCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE_AND_CAPTION :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEANDCAPTIONELem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
        FCurElement.SetAttribute(XpsWidth, '100%');
        fbCopyAttrsToCurrent(aFormatObject);
      end;
    FO_TABLE :
      begin
        XslProcessorElementStart(Sender, 'table');
        FLastTABLEELem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_TABLE_BODY :
      begin
        XslProcessorElementStart(Sender, 'tbody');
        FLastTBODYELem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_HEADER :
      begin
        XslProcessorElementStart(Sender, 'thead');
        FLastTHEADElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_FOOTER :
      begin
        XslProcessorElementStart(Sender, 'tfoot');
        FLastTFOOTElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
        FCurElement.FullEndTag := True;
      end;
    FO_TABLE_ROW :
      begin
        XslProcessorElementStart(Sender, 'tr');
        FLastTRElem := FCurElement;
        fbCopyAttrsToCurrent(aFormatObject);
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
        fbCopyAttrsToCurrent(aFormatObject);
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
      fbSaveColumnAttrs(aFormatObject);
  end;
end;
{--------}
procedure TXpFilterRTF.XslProcessorFormatObjectEnd(Sender : TObject);
var
  TempElement : TXpElement;
  TempString  : string;
  TempInt     : Integer;
  i           : Integer;
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
               (TempElement.NodeName <> 'Sequence')) do
          TempElement := TXpElement(TempElement.ParentNode);
        FLastInlineElem := TXpElement(TempElement);
        SetParentToCurrent;
      end;
    FO_LIST_ITEM :
      begin
        TempInt := 0;
        TempString := '';
        for i := 1 to FCellCnt do begin
          if (i <= FCellWidthList.Count) then
            TempInt := TempInt + StrToInt(FCellWidthList[i - 1])
          else
            TempInt := TempInt +
                       EvalPropAsNum('100%', fuTwips, FTableWidth);
          TempString := TempString +
                        FTableElem.GetAttribute('border') +
                        '\cellx' + IntToStr(TempInt);
        end;
        FCurAttr.SetAttribute(XpsTable, TempString);
        FCurAttr.SetAttribute('border', '');
        FCellCnt := 0;
        FCellWidthList.Clear;

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
