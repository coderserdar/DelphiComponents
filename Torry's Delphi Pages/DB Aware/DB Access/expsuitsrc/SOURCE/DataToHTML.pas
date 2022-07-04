{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit DataToHTML;

{$I DELPHI.VER}

interface

uses
  SysUtils, Classes, DataExport, Graphics, Windows;

type
  THTMLSource = TStrings;  {Para poner en el TDataToHTML}
  THTMLFontSize = (fsNormal, fs8, fs10, fs12, fs14, fs18, fs24, fs36);
  THTMLFontStyle = (hfBold, hfItalic, hfUnderline, hfStrikeOut, hfBlink, hfSup,
                    hfSub, hfDfn, hfStrong, hfEm, hfCite, hfVariable,
                    hfKeyboard, hfCode);

  THTMLFontStyles = set of THTMLFontStyle;

  THTMLObject = class(TPersistent)
  protected
    //function GetBegin: string; virtual; abstract;
    //function GetEnd: string; virtual; abstract;
    function GetCode(AInside: string): string; virtual; abstract;
  end;

  THTMLFont = class(THTMLObject)
  private
    FColor: TColor;
    FName: TFontName;
    FSize: THTMLFontSize;
    FStyle: THTMLFontStyles;
  protected
    function GetCode(AInside: string): string; override;
    function GetStyle(AInside: string): string;
  public
    constructor Create;
  published
    property Color: TColor read FColor write FColor default clNone;
    property Name: TFontName read FName write FName;
    property Size: THTMLFontSize read FSize write FSize default fsNormal;
    property Style: THTMLFontStyles read FStyle write FStyle default [];
  end;

  THTMLBand = class(THTMLObject) // THTMLObject
  private
    FBackColor: TColor;
    FFont: THTMLFont;
  protected
    function GetCode(AInside: string): string; override;
    function ColorValue: string;
    function PropertyCode: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Font: THTMLFont read FFont write FFont;
    property BackColor: TColor read FBackColor write FBackColor default clNone;
  end;

  THTMLPage = class(THTMLBand)
  private
    FTopMargin: Integer;
    FLeftMargin: Integer;
    FTitle: string;
  protected
    function GetCode: string; {$IFNDEF LESS110}reintroduce;{$ENDIF}
  published
    property Title: string read FTitle write FTitle;
    property TopMargin: Integer read FTopMargin write FTopMargin default 0;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default 0;
  end;

  THTMLTitle = class(THTMLBand)
  private
    TText: string;
  protected
    function GetCode: string; {$IFNDEF LESS110}reintroduce;{$ENDIF}
  published
    property Text: string read TText write TText;
  end;

  THTMLHeader = class(THTMLBand)
  private
    FVisible: Boolean;
  protected
  public
    constructor Create;
  published
    property Visible: Boolean read FVisible write FVisible default True;
  end;

  THTMLDetail = class(THTMLBand)
  private
    FHeaders: THTMLHeader;
    FBorderWidth: Integer;
    FBorderColor: TColor;
    FCellSpacing: Integer;
    FCellPadding: Integer;
  protected
    function PropertyCode: string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Headers: THTMLHeader read FHeaders write FHeaders;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth default 0;
    property BorderColor: TColor  read FBorderColor write FBorderColor default clNone;
    property CellSpacing: Integer read FCellSpacing write FCellSpacing default 2;
    property CellPadding: Integer read FCellPadding write FCellPadding default 1;
  end;

  THTMLField = class(TExportField)
  private
    FTitle: string;
    FAlignment: TAlignment;
  protected
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
  published
    property Title: string read FTitle write FTitle;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
  end;

  THTMLFields = class(TExportFields)
  private
    function GetItem(Index: Integer): THTMLField;
    procedure SetItem(Index: Integer; const Value: THTMLField);
  protected
  public
    function Add: THTMLField;
    property Items[Index: Integer]: THTMLField read GetItem write SetItem; default;
  end;

  TDataToHTML = class(TDataExport)
  private
    FFields: THTMLFields;
    FPageOptions: THTMLPage;
    FTitle: THTMLTitle;
    FDetail: THTMLDetail;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure WriteRecord; override;
    procedure WriteHeaders;
  public
    function GetFields: TExportFields; override;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSet;
    property Fields: THTMLFields read FFields write FFields;
    property PageOptions: THTMLPage read FPageOptions write FPageOptions;
    property Title: THTMLTitle read FTitle write FTitle;
    property Detail: THTMLDetail read FDetail write FDetail;
{   property FetchFirst;
    property SaveIfEmpty;
    property MaxRecords;}
    property OnBeginExport;
    property OnEndExport;
    property BeforeWriteRecord;
    property AfterWriteRecord;
  end;

implementation

uses
  DB, DBGrids;

const
  HTML_FONTSTYLETAGS: array[0..14] of string = ('b', 'i', 'u', 'strike', 'blink',
                                                'sup', 'sub', 'sub', 'dfn',
                                                'strong', 'em', 'cite', 'var',
                                                'kdb', 'code');
  HTML_FONTSIZEPOINT: array[0..7] of Integer = (0, 8, 10, 12, 14, 18, 24, 36);
  HTML_INDENT = '  ';

function HTMLSimpleTag(AName, AInside: string): string; {overload;}
begin
  Result := '<'  + AName + '>' + AInside +
            '</' + AName + '>';
end;

function HTMLTag(AName, APropertys, AInside: string): string; {overload;}
begin
  Result := '<'  + AName + ' ' + APropertys + '>' + AInside +
            '</' + AName + '>';
end;

function HTMLProperty(APropName, APropValue: string): string; {overload;}
begin
  Result := APropName + '="' + APropValue + '"';
end;

function HTMLIntProperty(APropName: string; APropValue: Integer): string; {overload;}
begin
  Result := HTMLProperty(APropName, IntToStr(APropValue));
end;

function HTMLQuotedProperty(APropName, APropValue: string): string;
begin
  Result := APropName + ': ' + APropValue;
end;

function ColorToHtml(AColor: TColor): string;
var
  tmpRGB: TColorRef;
begin
  tmpRGB := ColorToRGB(AColor);
  Result:=Format('#%.2x%.2x%.2x', [GetRValue(tmpRGB),
                                   GetGValue(tmpRGB),
                                   GetBValue(tmpRGB)]);
end;

{ THTMLFont }

constructor THTMLFont.Create;
begin
  inherited;
  FColor := clNone;
  FName := '';
  FSize := fsNormal;
  FStyle := [];
end;

{ THTMLBand }

function THTMLBand.ColorValue: string;
begin
  Result := ColorToHtml(FBackColor);
end;

function THTMLBand.PropertyCode: string;
begin
  if FBackColor <> clNone then
    Result := ' ' + HTMLProperty('bgcolor', ColorValue);
end;

constructor THTMLBand.Create;
begin
  inherited;

  FFont := THTMLFont.Create;
  FBackColor := clNone;
end;

destructor THTMLBand.Destroy;
begin
  FFont.Free;
  inherited;
end;

function THTMLBand.GetCode(AInside: string): string;
begin
  Result := FFont.GetCode(AInside);
  if FBackColor <> clNone then
    Result := HTMLTag('span', HTMLProperty('style',
      HTMLQuotedProperty('background-color', ColorValue)), Result);
end;

{function THTMLBand.GetCode: string;
begin
  Result := FFont.GetCode(AInside);
  if FBackColor <> clNone then
    Result := HTMLTag('span', HTMLProperty('style', HTMLQuotedProperty('background-color', ColorValue)), Result);
end;}

{ THTMLTitle }

function THTMLTitle.GetCode: string;
begin
  Result := inherited GetCode(TText);
end;

{ THTMLDetail }

constructor THTMLDetail.Create;
begin
  inherited;
  FHeaders := THTMLHeader.Create;
  FBorderWidth := 0;
  FBorderColor := clNone;
  FCellSpacing := 2;
  FCellPadding := 1;
end;

destructor THTMLDetail.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function THTMLDetail.PropertyCode: string;
begin
  Result := inherited PropertyCode;
  if FBorderWidth <> 0 then
    Result := Result + ' ' + HTMLIntProperty('border', FBorderWidth);

  if FBorderColor <> clNone then
    Result := Result + ' ' + HTMLProperty('bordercolor', ColorToHtml(FBorderColor));

  if FCellSpacing <> 2 then
    Result := Result + ' ' + HTMLIntProperty('cellspacing', FCellSpacing);

  if FCellPadding <> 1 then
    Result := Result + ' ' + HTMLIntProperty('cellpadding', FCellPadding);
end;

{ THTMLHeader }

constructor THTMLHeader.Create;
begin
  inherited;
  FVisible := True;
end;

{ THTMLField }

procedure THTMLField.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is THTMLField then
    with Source as THTMLField do
    begin
      FTitle := Title;
      FAlignment := Alignment;
    end
  else
  if Source is TField then
  begin
    with Source as TField do
    begin
      FTitle := DisplayLabel ;
      FAlignment := Alignment;
    end;
  end else
  if Source is TColumn then
  begin
    with Source as TColumn do
    begin
      FTitle := Title.Caption;
      FAlignment := Alignment;
    end
  end;
end;

constructor THTMLField.Create(Collection: TCollection);
begin
  inherited;
  FAlignment := taLeftJustify;
end;

{ THTMLFields }

function THTMLFields.Add: THTMLField;
begin
  Result := THTMLField(inherited Add);
end;

function THTMLFields.GetItem(Index: Integer): THTMLField;
begin
  Result := THTMLField(inherited GetItem(Index));
end;

procedure THTMLFields.SetItem(Index: Integer; const Value: THTMLField);
begin
  inherited SetItem(Index, Value);
end;

{ TDataToHTML }

constructor TDataToHTML.Create(AOwner: TComponent);
begin
  inherited;
  FFields := THTMLFields.Create(Self, THTMLField);
  FPageOptions := THTMLPage.Create;
  FTitle := THTMLTitle.Create;
  FDetail := THTMLDetail.Create;
end;

destructor TDataToHTML.Destroy;
begin
  FDetail.Free;
  FTitle.Free;
  FPageOptions.Free;
  FFields.Free;
  inherited;
end;

function TDataToHTML.GetFields: TExportFields;
begin
  Result := FFields ;
end;

procedure TDataToHTML.OpenFile;
begin
  inherited;
  WriteString('<html>'#13#10'<head>'#13#10'<title>' + FPageOptions.Title + '</title>'#13#10'</head>'#13#10{$IFDEF LESS110}, 0{$ENDIF});
  WriteString('<body' + FPageOptions.GetCode + '>'#13#10{$IFDEF LESS110}, 0{$ENDIF});
  WriteString('<!-- FireSoft Export Suite for Delphi -->'#13#10{$IFDEF LESS110}, 0{$ENDIF});

  { Escribe el Título }
  if FTitle.Text <> '' then
    WriteLine(FTitle.GetCode);

  WriteString('<table' + FDetail.PropertyCode + '>'#13#10{$IFDEF LESS110}, 0{$ENDIF});
  WriteHeaders;
end;

procedure TDataToHTML.CloseFile;
begin
  WriteString('</table>'#13#10'</body>'#13#10'</html>'{$IFDEF LESS110}, 0{$ENDIF});
  inherited;
end;

procedure TDataToHTML.WriteHeaders;
var
  iField: Integer;
begin
  WriteLine(HTML_INDENT + '<tr>');


  for iField := 0 to FFields.Count - 1 do
    if FFields[iField].Save then
      WriteLine(HTML_INDENT + HTML_INDENT +  '<td' + FDetail.Headers.PropertyCode + '>' + FDetail.Headers.Font.GetCode(FFields[iField].Title) + '</td>');

  WriteLine(HTML_INDENT + '</tr>');
end;

procedure TDataToHTML.WriteRecord;
var
  iField: Integer;
begin
  WriteLine(HTML_INDENT + '<tr>');

  for iField := 0 to FFields.Count - 1 do
    if FFields[iField].Save then
      WriteLine(HTML_INDENT + HTML_INDENT + '<td>' + FFields[iField].Field.AsString + '</td>');

  WriteLine(HTML_INDENT + '</tr>');
end;

function THTMLFont.GetCode(AInside: string): string;
var
  sProp: string;
begin
  Result := GetStyle(AInside);
  sProp := '';
  if FSize <> fsNormal then
    sProp := sProp + ' ' + HTMLIntProperty('size', Integer(FSize));

  if FName <> '' then
    sProp := sProp + ' ' + HTMLProperty('face', FName);

  if FColor <> clNone then
    sProp := sProp + ' ' + HTMLProperty('color', ColorToHtml(FColor));

  if sProp <> '' then
    Result := HTMLTag('font', sProp, Result)
  else
    Result := Result;
end;

function THTMLFont.GetStyle(AInside: string): string;
var
  i: Integer;
begin
  Result := AInside;
  for i := 0 to High(HTML_FONTSTYLETAGS) do
    if THTMLFontStyle(i) in FStyle then
      Result := HTMLSimpleTag(HTML_FONTSTYLETAGS[i], Result);

{  if fsBold in FStyle then
    Result := HTMLTag('n', AInside);

  if fsItalic in FStyle then
    Result := HTMLTag('i', AInside);

  if fsUnderline in FStyle then
    Result := HTMLTag('u', AInside);

  if fsStrikeOut in FStyle then
    Result := HTMLTag('strike', AInside);}
end;

{ THTMLPage }

function THTMLPage.GetCode: string;
var
  sStyle: string;
begin
  Result := '';
  if FBackColor <> clNone then
    Result := Result + ' ' +  HTMLProperty('bgcolor', ColorValue);

  if FTopMargin <> 0 then
    Result := Result + ' ' + HTMLIntProperty('topmargin', FTopMargin);

  if FLeftMargin <> 0 then
    Result := Result + ' ' + HTMLIntProperty('leftmargin', FLeftMargin);

  { --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- }
  if Font.Name <> '' then
    sStyle := 'font-family: ' + Font.Name;

  if Font.Size <> fsNormal then
    sStyle := 'font-size: ' + IntToStr(HTML_FONTSIZEPOINT[Integer(Font.Size)]) + ' pt';

{  if Font.Style <> [] then
    sStyle := 'font-weight: ' + Font.s IntToStr(Font.Size);}

  if Font.Color <> clNone then
    sStyle := 'color: ' + ColorToHtml(Font.Color);

  if sStyle <> '' then
    Result := Result + ' ' + HTMLProperty('style', sStyle);
end;

end.
