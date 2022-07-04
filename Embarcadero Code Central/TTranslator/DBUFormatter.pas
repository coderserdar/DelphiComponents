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

{ $Id: DBUFormatter.pas,v 1.11 2002/10/23 13:13:09 laa Exp $}

unit DBUFormatter;

interface

uses
{$ifndef LINUX}
  Windows, Graphics, Grids,
{$else LINUX}
  QGraphics, QGrids, Types,
{$endif LINUX}
  DataInterfaces, DBUTypes, Classes, DataType;

type

  TDBUTextStyle = record
    Font : TFont;
    BGColor : TColor;
  end;

  TDBUFormatter = class(TDataFormatter, ILabel)
  private
    FFont : TFont;
    FBGColor : TColor;

    FVAlignment : TVerticalAlignment;
    FHAlignment : TAlignment;
    FVMargin : Integer;
    FHMargin : Integer;

    procedure SetHAlignment(const Value: TAlignment);
    procedure SetVAlignment(const Value: TVerticalAlignment);
    procedure SetHMargin(const Value: Integer);
    procedure SetVMargin(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetBGColor(AColor : TColor);
    function GetDBUAlign: TDBUAlign;
    function GetDBUMargin: TDBUMargin;

    function GetFontSize : integer;
    procedure SetFontSize(ASize : integer);
    procedure SetFGColor(AColor : TColor);
    procedure SetFontStyles(FontStyles : TFontStyles);
    procedure Clear;

    // plain puckoness of delphi, need to define this crap for interfaces
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    {/** Constructor */}
    constructor Create;
    {/** Destructor */}
    destructor Destroy; override;

    function FormatValue(SrcValue: TValue; DataType: TDataType): TValue; virtual;
    function ParseValue(SrcValue: TValue; var ParsedValue: TValue;
      DataType: TDataType): Boolean; virtual;

    procedure DrawBackground( Canvas : TCanvas; Rect: TRect );
    procedure PrepareCanvas( Canvas : TCanvas ); virtual;
    procedure Reset;
    procedure AssignTextStyle( ATextStyle : TDBUTextStyle );
    procedure SetTextAlignment(AnAlignment : TTextAlignment);
    function GetTextAlignment : TTextAlignment;

    property Font : TFont read FFont write SetFont;
    property BGColor : TColor read FBGColor write SetBGColor;
    property VAlignment : TVerticalAlignment read FVAlignment write SetVAlignment;
    property HAlignment : TAlignment read FHAlignment write SetHAlignment;
    property Align : TDBUAlign read GetDBUAlign;
    property VMargin : Integer read FVMargin write SetVMargin;
    property HMargin : Integer read FHMargin write SetHMargin;
    property Margin : TDBUMargin read GetDBUMargin;
  end;

{  TPictureType = class(TObjectType)
  protected
    function GetAsPicture(AValue : TValue) : TPicture; virtual;
  public
    function Value(Val : TPicture) : TValue;

    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

function AsPicture(Value : TValue) : TPicture;
function ValueFromPicture(Val : TPicture) : TValue;
}
function DBUTextStyle( AFont : TFont; ABGColor : TColor ) : TDBUTextStyle;
function AssignCanvasToTextStyle( Canvas : TCanvas ) : TDBUTextStyle;
procedure CopyToTextStyle( SrcStyle : TDBUTextStyle; var DestStyle : TDBUTextStyle );
procedure ApplyTextStyleToCanvas( TextStyle : TDBUTextStyle; Canvas : TCanvas;
  State : TGridDrawState; IsNegative : Boolean);

  // Predefined BGColors
const
  StripeBGColor = clYellow or clLtGray;     // Very light yellow
  InvalidBGColor = $00DDDDDD;               // Very light gray
  InvalidStripeBGColor = $00CCCCCC;         // Light gray

var
//  PictureType : TPictureType;

  // Predefined fonts
  NormalFont : TFont;
  NegFont : TFont;
  ReadOnlyFont : TFont;
  NegReadOnlyFont : TFont;
  FixedFont : TFont;
  SelectFont : TFont;
  DisabledFont : TFont;
  DisabledHeaderFont : TFont;

  // Predefined text styles
  NormalStyle : TDBUTextStyle;
  StripeStyle : TDBUTextStyle;
  InvalidStyle : TDBUTextStyle;
  InvalidStripeStyle : TDBUTextStyle;
  FixedStyle : TDBUTextStyle;
  ActiveStyle : TDBUTextStyle;
//  SelectStyle : TDBUTextStyle;

implementation

uses
  SysUtils;

type
  TDummyExceptionEvent = class
    procedure OnChange( Sender : TObject );
  end;

var
  DummyEvent : TDummyExceptionEvent;
{
function AsPicture(Value : TValue) : TPicture;
begin
  Result := PictureType.GetAsPicture(Value);
end;

function ValueFromPicture(Val : TPicture) : TValue;
begin
  Result := PictureType.Value(Val);
end;
}
function DBUTextStyle( AFont : TFont; ABGColor : TColor ) : TDBUTextStyle;
begin
  with Result do
  begin
    Font := AFont;
    BGColor := ABGColor;
  end;
end;

procedure ApplyTextStyleToCanvas( TextStyle : TDBUTextStyle; Canvas : TCanvas;
  State : TGridDrawState; IsNegative : Boolean);
begin
  with TextStyle do
  begin
{    if Assigned( Pen ) then
      Canvas.Pen.Assign( Pen );
}
    Canvas.Brush.Color := BGColor;

    if IsNegative and Assigned( NegFont ) then
      Canvas.Font.Assign( NegFont )
    else if Assigned( Font ) then
      Canvas.Font.Assign( Font );
  end;
end;

function AssignCanvasToTextStyle( Canvas : TCanvas ) : TDBUTextStyle;
begin
  with Canvas do
    Result := DBUTextStyle( Font, Brush.Color );
end;

procedure CopyToTextStyle( SrcStyle : TDBUTextStyle; var DestStyle : TDBUTextStyle );
begin
  with SrcStyle do
  begin
    if Assigned( Font ) then
      DestStyle.Font.Assign( Font );
    DestStyle.BGColor := BGColor;
  end;
end;

procedure CreateTextStyles;
begin
  DummyEvent := nil;

  // Predefined fonts
  NormalFont := TFont.Create;
  NegFont := TFont.Create;
  NegFont.Color := clRed;
//  NegFont.Name := 'Batang';
  ReadOnlyFont := TFont.Create;
  ReadOnlyFont.Color := clBlue;
  NegReadOnlyFont := TFont.Create;
  NegReadOnlyFont.Color := clRed;
  FixedFont := TFont.Create;
//  FixedFont.Name := 'Courier';
  FixedFont.Style := [fsBold];
  SelectFont := TFont.Create;
  SelectFont.Color := clHighlightText;
  DisabledFont := TFont.Create;
  DisabledFont.Color := clGray;
  DisabledHeaderFont := TFont.Create;
  DisabledHeaderFont.Color := clGray;

  // Predefined text styles
  NormalStyle := DBUTextStyle( NormalFont,  clWindow{, NormalPen} );
  StripeStyle := DBUTextStyle( NormalFont, StripeBGColor{, NormalPen} );
  InvalidStyle := DBUTextStyle( NormalFont, InvalidBGColor{, NormalPen} );
  InvalidStripeStyle := DBUTextStyle( NormalFont, InvalidStripeBGColor{, NormalPen} );
  FixedStyle := DBUTextStyle( FixedFont, clBtnFace{, NormalPen} );
//  ActiveStyle := DBUTextStyle( SelectFont, clHighLight );
//  SelectStyle := DBUTextStyle( SelectFont, ActiveBGColor );

  // Except if somebody tries to change the fonts
  NormalFont.OnChange := DummyEvent.OnChange;
  NormalFont.OnChange := DummyEvent.OnChange;
  NegFont.OnChange := DummyEvent.OnChange;
  ReadOnlyFont.OnChange := DummyEvent.OnChange;
  NegReadOnlyFont.OnChange := DummyEvent.OnChange;
  FixedFont.OnChange := DummyEvent.OnChange;
  SelectFont.OnChange := DummyEvent.OnChange;
  DisabledFont.OnChange := DummyEvent.OnChange;
  DisabledHeaderFont.OnChange := DummyEvent.OnChange;
end;

procedure DestroyTextStyles;
begin
  NormalFont.Free;
  NegFont.Free;
  ReadOnlyFont.Free;
  NegReadOnlyFont.Free;
  FixedFont.Free;
  SelectFont.Free;
  DisabledFont.Free;
  DisabledHeaderFont.Free;
end;
{ TDummyExceptionEvent }

procedure TDummyExceptionEvent.OnChange(Sender: TObject);
begin
  raise Exception.Create( 'This text style is read only!' );
end;

{ TDBUFormatter }

constructor TDBUFormatter.Create;
begin
  inherited;

  FFont := TFont.Create;
  FBGColor := clWindow;
  Reset;
end;

destructor TDBUFormatter.Destroy;
begin
  inherited;
  FFont.Free;
end;

procedure TDBUFormatter.Reset;
begin
  FVAlignment := vaMiddle;
  FHAlignment := taLeftJustify;
  FHMargin := 2;
  FVMargin := 1;
end;

procedure TDBUFormatter.PrepareCanvas( Canvas : TCanvas );
begin
  Canvas.Font.Assign( Font );
  Canvas.Brush.Color := BGColor;
end;

procedure TDBUFormatter.DrawBackground(Canvas: TCanvas; Rect: TRect);
begin
  Canvas.FillRect(Rect);
end;

function TDBUFormatter.FormatValue(SrcValue: TValue; DataType: TDataType): TValue;
begin
  if (DataType = nil) and
     (SrcValue.DataType = nil) then
    DataType := AnyStringType;

  Result := DataType.FormattedValue( SrcValue, Self );
end;

function TDBUFormatter.ParseValue(SrcValue: TValue; var ParsedValue : TValue;
  DataType: TDataType): Boolean;
begin
  if DataType = nil then
    DataType := AnyStringType;

  Result := DataType.ParseText( AsString( SrcValue ), ParsedValue, Self)
end;

procedure TDBUFormatter.SetHAlignment(const Value: TAlignment);
begin
  FHAlignment := Value;
end;

procedure TDBUFormatter.SetVAlignment(const Value: TVerticalAlignment);
begin
  FVAlignment := Value;
end;

procedure TDBUFormatter.SetHMargin(const Value: Integer);
begin
  FHMargin := Value;
end;

procedure TDBUFormatter.SetVMargin(const Value: Integer);
begin
  FVMargin := Value;
end;

procedure TDBUFormatter.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

procedure TDBUFormatter.SetBGColor(AColor: TColor);
begin
  FBGColor := AColor;
end;

procedure TDBUFormatter.AssignTextStyle(ATextStyle: TDBUTextStyle);
begin
  if ATextStyle.Font <> nil then
    Font.Assign( ATextStyle.Font );
  BGColor := ATextStyle.BGColor;
end;

procedure TDBUFormatter.SetTextAlignment(AnAlignment : TTextAlignment);
begin
  // I(LAA) know this is a bit ugly, but I happen to know that the types match
  HAlignment := TAlignment( AnAlignment );
end;

function TDBUFormatter.GetTextAlignment: TTextAlignment;
begin
  // I(LAA) know this is a bit ugly, but I happen to know that the types match
  Result := TTextAlignment( HAlignment );
end;


{ TPictureType }
{
function TPictureType.GetAsPicture(AValue : TValue) : TPicture;
var
  AObject : TObject;
begin
  AObject := AsObject(AValue);
  if (AObject <> nil) and
     not (AObject is TPicture) then
    raise Exception.Create(Self.ClassName + '.GetAsPicture: You can not get Pictures from this DataType!')
  else
    Result := TPicture(AObject);
end;

function TPictureType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := Inherited FormattedValue(AValue, DataFormatter);
end;

function TPictureType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := Inherited ParseText(AString, ParsedValue, DataFormatter);
end;

function TPictureType.Value(Val : TPicture) : TValue;
begin
  Result := Inherited Value(Val);
end;
}
function TDBUFormatter.GetDBUAlign: TDBUAlign;
begin
  Result := DBUAlign( VAlignment, HAlignment );
end;

function TDBUFormatter.GetDBUMargin: TDBUMargin;
begin
  Result := DBUMargin( VMargin, HMargin );
end;

procedure TDBUFormatter.Clear;
begin
  Reset;
end;

function TDBUFormatter.GetFontSize: integer;
begin
  Result := Font.Size;
end;

procedure TDBUFormatter.SetFGColor(AColor: TColor);
begin
  Font.Color := AColor;
end;

procedure TDBUFormatter.SetFontSize(ASize: integer);
begin
  Font.Size := ASize;
end;

procedure TDBUFormatter.SetFontStyles(FontStyles: TFontStyles);
begin
  Font.Style := Font.Style + FontStyles;
end;

function TDBUFormatter._AddRef: Integer;
begin
  Result := 1;
end;

function TDBUFormatter._Release: Integer;
begin
  Result := 1;
end;

function TDBUFormatter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

initialization
//  PictureType := TPictureType.Create;
  CreateTextStyles;
finalization
//  PictureType.Free;
  DestroyTextStyles;

end.

