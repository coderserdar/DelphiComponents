{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: OfficeIntf.pas,v 1.30 2002/12/27 15:08:05 laa Exp $ }

{-----------------------------------------------------------------------------
  OfficeIntf      Wrappers around Delphi's Office components. Note that these
                  require
                   a) Delphi 5
                   b) The wanted Office product installed on the *client*
                      machine

  What:           TOfficeInterface    a somewhat "general" interface
                  TExcelInterface     wrapper around Delphi's TExcelApplication,
                                      i.e. a wrapper-wrapper

  Company:        Polycon Ab
  Authors:        LAA
-----------------------------------------------------------------------------}

unit OfficeIntf;
{$i common.inc}

interface

{$ifdef D5_OR_HIGHER}
uses
  Excel97, OleServer,
  DataType, Graphics;

type


  TOfficeTypeClass = class of TOleServer;

  {/** Abstract base class to commect to any of the Micro$oft Office
       COM servers. */}
  TOfficeInterface = class
  private
    fSuccessful : Boolean;
    fOfficeServer : TOleServer;
  protected
    constructor Create(OfficeType : TOfficeTypeClass; ConnectKind:TConnectKind = ckNewInstance);
    property OfficeServer : TOleServer read fOfficeServer;

    function GetSubject : string; virtual;
    function GetTitle : string; virtual;
    function GetVisible : Boolean; virtual; abstract;
    function GetHasChanged : Boolean; virtual; abstract;
    procedure SetSubject(const ASubject : string); virtual;
    procedure SetTitle(const ATitle : string); virtual;
    procedure SetVisible(Visibility : Boolean); virtual; abstract;
    procedure SetHasChanged(HasChanged : Boolean); virtual; abstract;
  public
    destructor Destroy; override;
    procedure SaveAs(const FileName : string); virtual;

    // do not continue work if not successful (couldn't connect, Office not installed?)
    property Successful : Boolean read fSuccessful;
    property Subject : string read GetSubject write SetSubject;
    property Title : string read GetTitle write SetTitle;
    // Shows the Office Application which until then is invisuble
    property Visible : Boolean read GetVisible write SetVisible;
    // If HasChanged=False then closing the Office Application won't ask "Save changes?"
    property HasChanged : Boolean read GetHasChanged write SetHasChanged;
  end;

  {/** An easy to use interface to the Excel COM-object. The implemantion is
       far from optimal but for the purposes used (so far) it's good enough.
       Anyone who feels like it may implement a better one, as long as it
       support all flavours of Delphi, Windows and Excel.

       Note that rows and columns are index from zero, to the Excel column A1
       (in the upper left corner) is referenced as Column[0,0].
  */}
  TExcelInterface = class(TOfficeInterface)
  private
    fAutoQuit : Boolean;
    WB : _WorkBook;
    fActiveWorksheetIndex : integer;
    LCID : integer;

    function GetExcelApplication : TExcelApplication;

    function GetCellString(Col, Row : integer) : string;
    procedure SetCellString(Col, Row : integer; const AValue : string);
    procedure SetCellValue(Col, Row : integer; const AValue : TValue);
    procedure SetCellRangeString(Left,Top,Right,Bottom:integer; const AValue : string);

    function GetWorkSheet(index: integer): _WorkSheet;
    function GetWorkSheetCount: integer;
    function GetWorkSheetName(index: integer): string;
    procedure SetWorkSheetName(index: integer; const Value: string);
    function GetActiveWorksheetIndex: integer;

    procedure SetCellBold(Col,Row:integer; IsBold : Boolean);

    procedure SetAutoQuit(const Value: Boolean);
  protected
    function GetSubject : string; override;
    function GetTitle : string; override;
    function GetVisible : Boolean; override;
    function GetHasChanged : Boolean; override;
    procedure SetSubject(const ASubject : string); override;
    procedure SetTitle(const ATitle : string); override;
    procedure SetVisible(Visibility : Boolean); override;
    procedure SetHasChanged(HasChanged : Boolean); override;
  public
    constructor Create(AutoQuit:Boolean = True);
    constructor CreateFromFile(const AFileName : string);
    destructor Destroy; override;

    function CellName(Col, Row : integer) : string;
    procedure SaveAs(const FileName : string); override;
    procedure MergeHorizontally(Left, Right, Row : integer);
    procedure MergeVertically(Col, Top, Bottom : integer);
    procedure SetActiveSheet(index : integer);


    property AutoQuit : Boolean read FAutoQuit write SetAutoQuit;
    property ExcelApplication : TExcelApplication read GetExcelApplication;
    property WorkBook : _WorkBook read WB;
    property WorkSheet[index:integer] : _WorkSheet read GetWorkSheet;
    property WorkSheetCount : integer read GetWorkSheetCount;
    property WorkSheetName[index:integer] : string read GetWorkSheetName write SetWorkSheetName;
    property ActiveWorksheetIndex : integer read GetActiveWorksheetIndex;

    property CellString[Col,Row : integer] : string read GetCellString write SetCellString;
    property CellValue[Col,Row : integer] : TValue write SetCellValue;
    property CellRangeString[Left,Top,Right,Bottom : integer] : string write SetCellRangeString;
    property CellBold[Col,Row:integer] : Boolean write SetCellBold;

  end;

{$endif D5_OR_HIGHER}

implementation

{$ifdef D5_OR_HIGHER}
uses
{$ifdef D6_OR_HIGHER}
  Variants,
{$endif D6_OR_HIGHER}
  ComObj, ActiveX,
  Windows, Sysutils;


{ TOfficeInterface }

constructor TOfficeInterface.Create(OfficeType:TOfficeTypeClass; ConnectKind:TConnectKind);
begin
  fOfficeServer := OfficeType.Create( nil );
  fOfficeServer.ConnectKind := ConnectKind;

  fSuccessful := True;
  try
    fOfficeServer.Connect;
  except
    fSuccessful := False;
  end;
end;

destructor TOfficeInterface.Destroy;
begin
  try
    fOfficeServer.Disconnect;
  except
  end;
  fOfficeServer.Free;
end;

function TOfficeInterface.GetSubject : string;
begin
  result := '';
end;

function TOfficeInterface.GetTitle : string;
begin
  result := '';
end;

procedure TOfficeInterface.SetSubject(const ASubject : string);
begin
end;

procedure TOfficeInterface.SetTitle(const ATitle : string);
begin
end;

procedure TOfficeInterface.SaveAs(const FileName : string);
begin
  DeleteFile( FileName );
end;

{ TExcelInterface }

constructor TExcelInterface.Create(AutoQuit:Boolean = True);
var
  DoDisplayAlerts : Boolean;
  iSheet : integer;
begin
  fAutoQuit := AutoQuit;
//  LCID := GetUserDefaultLCID;
  LCID := LOCALE_USER_DEFAULT;
  inherited Create( TExcelApplication );

  if fSuccessful then
  begin
    // add a workbook to wotk with
    WB := ExcelApplication.Workbooks.Add(EmptyParam, LCID);

    // delete all except one worksheet (this is really optional)
    DoDisplayAlerts := ExcelApplication.Application.DisplayAlerts[LCID];
    ExcelApplication.DisplayAlerts[LCID] := False;
    for iSheet := WorkSheetCount-1 downto 1 do
      WorkSheet[iSheet].Delete(LCID);
    ExcelApplication.DisplayAlerts[LCID] := DoDisplayAlerts;
 end;

  // set the first to active
  fActiveWorksheetIndex := 0;
end;

constructor TExcelInterface.CreateFromFile(const AFileName : string);
begin
  fAutoQuit := True;
  inherited Create( TExcelApplication );

  if fSuccessful then
    try
      WB := ExcelApplication.Workbooks.Open(AFileName,false,true,xlNoChange,EmptyParam, EmptyParam,
                    True, EmptyParam,EmptyParam, False, EmptyParam,EmptyParam,EmptyParam,0);
    except
      WB := nil;
      fSuccessful := False;
    end;

  // set the first to active
  fActiveWorksheetIndex := 0;
end;

destructor TExcelInterface.Destroy;
begin
  // Always close invisible Excels, because those will be left hanging
  try
    if fAutoQuit or (not Visible) then
    begin
      if WB<>nil then
        WB.Close(False, EmptyParam, EmptyParam, 0);
      WB := nil;
      ExcelApplication.Workbooks.Close(0);
      ExcelApplication.Quit;
    end;
  except
  end;

  inherited Destroy;
end;

function TExcelInterface.GetWorkSheet(index: integer): _WorkSheet;
begin
  Result := WB.Worksheets[index+1] as _WorkSheet;
end;

function TExcelInterface.GetWorkSheetCount: integer;
begin
  Result := WB.Worksheets.Get_Count;
end;

procedure TExcelInterface.SetActiveSheet(index : integer);
var
  WS : _WorkSheet;
begin
  // add the missing amount of sheets, at the end of the list
  while ( index >= GetWorkSheetCount ) do
    WB.Worksheets.Add(EmptyParam,WorkSheet[WorkSheetCount-1],EmptyParam,EmptyParam,0);

  fActiveWorksheetIndex := index;
  WS := WorkSheet[index];
  WS.Activate(0);
end;

function TExcelInterface.CellName(Col, Row : integer) : string;
const
 LengthOfAlphabet = 26;
var
 Char1, Char2 : integer;
begin
  Char1 := col div LengthOfAlphabet;
  Char2 := col mod LengthOfAlphabet;
  if Char1 > 0 then
    Result := Char( ord('A')+Char1-1)
  else
    Result := '';
  Result := result + Char ( ord('A')+Char2) + IntToStr(Row+1);
end;

function TExcelInterface.GetCellString(Col, Row: integer): string;
begin
  result := ExcelApplication.Range[ CellName(Col,Row),CellName(Col,Row) ].Value;
end;

function TExcelInterface.GetExcelApplication : TExcelApplication;
begin
  result := TExcelApplication( OfficeServer );
end;

procedure TExcelInterface.SetCellBold(Col,Row:integer; IsBold : Boolean);
begin
  ExcelApplication.Range[CellName(Col,Row),CellName(Col,Row)].Font.Bold := IsBold;
end;

function TExcelInterface.GetSubject: string;
begin
  result := WB.Subject[0];
end;

function TExcelInterface.GetTitle: string;
begin
  result := WB.Title[0];
end;

procedure TExcelInterface.SaveAs(const FileName : string);
begin
  inherited;
  WB.SaveAs(FileName, EmptyParam, EmptyParam, EmptyParam, False, False,
            xlNoChange, EmptyParam, EmptyParam, EmptyParam, EmptyParam, 0);
end;

procedure TExcelInterface.SetCellRangeString(Left,Top,Right,Bottom:integer; const AValue : string);
begin
  ExcelApplication.Range[ CellName(Left,Top),CellName(Right,Bottom) ].Value := AValue;
end;

procedure TExcelInterface.SetCellString(Col, Row: integer; const AValue: string);
begin
  ExcelApplication.Range[ CellName(Col,Row), EmptyParam ].Value :=  AValue;
end;

procedure TExcelInterface.SetCellValue(Col, Row: integer; const AValue: TValue);
var
{$ifndef D7_OR_HIGHER}
  ARange : Range;
{$else}
  ARange : ExcelRange;
{$endif D7_OR_HIGHER}
begin
//  if (AValue.DataType is TCurrencyType) or  (AValue.DataType is TDoubleType) then
  ARange := ExcelApplication.Range[ CellName(Col,Row), EmptyParam ];
  if AValue.DataType.IsNumeric then
    ARange.Value := AsDouble(AValue)
  else
    ARange.Value := AsString(AValue);
end;

procedure TExcelInterface.SetSubject(const ASubject: string);
begin
  WB.Subject[0] := ASubject;
end;

procedure TExcelInterface.SetTitle(const ATitle: string);
begin
  WB.Title[0] := ATitle;
end;

function TExcelInterface.GetVisible: Boolean;
begin
  result := ExcelApplication.Application.Visible[LCID];
end;

procedure TExcelInterface.SetVisible(Visibility: Boolean);
begin
  ExcelApplication.Visible[LCID] := Visibility;
end;

function TExcelInterface.GetHasChanged: Boolean;
begin
  result := not WorkBook.Saved[0];
end;

procedure TExcelInterface.SetHasChanged(HasChanged: Boolean);
begin
  WorkBook.Saved[0] := not HasChanged;
end;

procedure TExcelInterface.MergeHorizontally(Left, Right, Row : integer);
begin
  ExcelApplication.Range[ CellName(Left,Row),CellName(Right,Row) ].Merge( True );
end;

procedure TExcelInterface.MergeVertically(Col, Top, Bottom : integer);
begin
  ExcelApplication.Range[ CellName(Col,Top),CellName(Col,Bottom) ].Merge( False );
end;































procedure TExcelInterface.SetAutoQuit(const Value: Boolean);
begin
  FAutoQuit := Value;
end;

function TExcelInterface.GetWorkSheetName(index: integer): string;
begin
  result := WorkSheet[index].Name;
end;

procedure TExcelInterface.SetWorkSheetName(index: integer; const Value: string);
begin
  WorkSheet[index].Name := Value;
end;

function TExcelInterface.GetActiveWorksheetIndex: integer;
begin
  Result := fActiveWorksheetIndex;
end;

initialization
  OleCheck( CoInitialize(nil) );

{$endif D5_OR_HIGHER}

end.

