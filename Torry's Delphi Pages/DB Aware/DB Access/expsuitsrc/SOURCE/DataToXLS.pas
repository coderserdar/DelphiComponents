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

unit DataToXLS;

{$I DELPHI.VER}

interface

uses
  Classes, Graphics, DataExport, Db, BiffFile;

type
  // Para que no tenga que incluír la unit BiffFile en todos lados
  TCellType = BiffFile.TCellType;
  TCellAttributes = BiffFile.TCellAttributes;

  TxlBand = class(TPersistent)
  private
    FRowHeight: Integer;
    FFont: TFont;
    procedure SetFont(const Value: TFont);
    function SaveFont: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    {property Color: TColor read FColor write FColor default clSilver;}
    property Font: TFont read FFont write SetFont stored  SaveFont;
    property RowHeight: Integer read FRowHeight write FRowHeight default 0;
  end;

  TxlTitle = class(TxlBand)
  private
    FText: String;
  published
    property Text: string read FText write FText;
  end;

  TxlDetail = class(TxlBand);
  TxlHeader = class(TxlBand);

  TxlColumn = class(TExportField)
  private
    FTitle: string;
    FAlignment: TAlignment;
    FColWidth: Integer;
    FColumnType: TCellType;
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string  read FTitle write FTitle;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property ColWidth: Integer read FColWidth write FColWidth default 0;
    property DataType: TCellType read FColumnType write FColumnType default ctLabel;
  end;

  TxlColumns = class(TExportFields)
  private
    function GetItem(Index: Integer): TxlColumn;
    procedure SetItem(Index: Integer; const Value: TxlColumn);
  protected
  public
    function Add: TxlColumn;
    property Items[Index: Integer]: TxlColumn read GetItem write SetItem; default;
  end;

  TGetDetailCellParams = procedure(ARow, ACol: Integer; AColumn: TxlColumn;
    var AValue: Variant; var AColumnType: TCellType;
    var AAttributes: TCellAttributes) of object;

  TDataToXLS = class(TDataExport)
  private
    VPos: Integer; { Keeps vertical position when is drawing the file }
    FColumns: TxlColumns;
    FTitle: TxlTitle;
    FDetail: TxlDetail;
    FHeader: TxlHeader;
    FOnGetDetailParams: TGetDetailCellParams;
    FComment: String;
  {Excel}
    procedure Internal_SaveFonts;
    procedure Internal_SaveTitle;
    procedure Internal_SaveHeaders;
  protected
    FBiffFile: TBiffFile;
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure WriteRecord; override;
  public
    function GetFields: TExportFields; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSet;
    property OnBeginExport;
    property OnEndExport;
    property BeforeWriteRecord;
    property AfterWriteRecord;
    property Columns: TxlColumns read FColumns write FColumns;
    property Title: TxlTitle read FTitle write FTitle;
    property Detail: TxlDetail read FDetail write FDetail;
    property Header: TxlHeader read FHeader write FHeader;
    property OnGetDetailParams: TGetDetailCellParams read FOnGetDetailParams
      write FOnGetDetailParams;
    property Comment: string read FComment write FComment;
  end;

function FieldToCellType(AFieldType: TFieldType): TCellType;

implementation

uses
  DBGrids;

function FieldToCellType(AFieldType: TFieldType): TCellType;
begin
  Case AFieldType of
    ftBytes, ftSmallint, ftInteger, ftWord, ftAutoInc:
      Result := ctInteger;
    ftFloat, ftCurrency, ftBCD:
      Result := ctDouble;
    ftBoolean:
      Result := ctBoolean;
  else
    Result := ctLabel;
  end;
end;

{ TxlBand }

constructor TxlBand.Create;
begin
  inherited;
  FFont := TFont.Create ;
  FFont.Name := 'Arial';
  FFont.Size := 10;
  FFont.Style := [];
end;

destructor TxlBand.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TxlBand.SaveFont: Boolean;
begin
  { Arial 10 is the default font of Biff5 }
  Result := not ((FFont.Name = 'Arial') and
                 (FFont.Size = 10) and
                 (FFont.Style = []));
end;

procedure TxlBand.SetFont(const Value: TFont);
begin
  FFont.Assign( Value );
end;

{ TxlColumn }

procedure TxlColumn.Assign(Source: TPersistent);
begin
  inherited Assign( Source );
  if Source is TxlColumn then
    with Source as TxlColumn do
    begin
      FTitle := Title;
      FAlignment := Alignment;
      FColWidth := ColWidth;
      FColumnType := DataType;
    end
  else if Source is TField then
  begin
    with Source as TField do
    begin
      FTitle := DisplayLabel;
      FAlignment := Alignment;
      FColWidth := 0;
      FColumnType := FieldToCellType(DataType);
    end;
  end else if Source is TColumn then begin
    {if Assigned(TColumn(Source).Field) Then
      Assign(TColumn(Source).Field);} { Asigna todas las propiedades propias del Campo }
    with Source as TColumn do
    begin
      FTitle := Title.Caption;
      FAlignment := Alignment;
      FColWidth := Width * 37;

      if Assigned(TColumn(Source).Field) Then
        FColumnType := FieldToCellType(TColumn(Source).Field.DataType);
    end
  end;
end;

constructor TxlColumn.Create(Collection: TCollection);
begin
  inherited;
  FAlignment := taLeftJustify;
  FColWidth := 0;
  FColumnType := ctLabel;
end;

{ TxlColumns }

function TxlColumns.Add: TxlColumn;
begin
  Result := TxlColumn(inherited Add);
end;

function TxlColumns.GetItem(Index: Integer): TxlColumn;
begin
  Result := TxlColumn(inherited GetItem(Index));
end;

procedure TxlColumns.SetItem(Index: Integer; const Value: TxlColumn);
begin
  inherited SetItem(Index, Value);
end;

{ TDataToXLS }

constructor TDataToXLS.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TxlColumns.Create(Self, TxlColumn);

  FTitle := TxlTitle.Create;
  FTitle.Font.Style := [fsBold];

  FHeader := TxlHeader.Create ;
  FHeader.Font.Style := [fsBold];

  FDetail := TxlDetail.Create;
end;

destructor TDataToXLS.Destroy;
begin
  FColumns.Free;
  FHeader.Free;
  FDetail.Free;
  FTitle.Free;
  inherited;
end;

procedure TDataToXLS.Internal_SaveFonts;
begin
  FBiffFile.WriteFont(FDetail.Font); { Default Font }
  FBiffFile.WriteFont(FTitle.Font);
  FBiffFile.WriteFont(FHeader.Font);
end;

procedure TDataToXLS.Internal_SaveHeaders;
var
  iCol,
  hPos: Integer;
begin
  hPos := 0;
  for iCol := 0 to FColumns.Count -1 do
    if FColumns.Items[iCol].Save Then
    begin
      if FColumns.Items[iCol].ColWidth > 0 Then
        FBiffFile.SetColWidth(hPos, FColumns.Items[iCol].ColWidth);

      FBiffFile.WriteStringCell(VPos, hPos, FColumns.Items[iCol].Title, [caFont3]);
      Inc(hPos);
    end;
  Inc(VPos);
end;

procedure TDataToXLS.Internal_SaveTitle;
begin
  if FTitle.Text <> '' Then
  begin
    FBiffFile.WriteStringCell(0, 0, FTitle.Text, [caFont2]);
   {if FTitle.RowHeight = 0 Then
     FBiffFile.SetRowAttributes(0, 0, 5, FTitle.Font.Size * 24, True, [caFont2,
       caCenter, caBottomBorder, caTopBorder, caRightBorder, caLeftBorder], 2);}
    Inc(VPos);
  end;
end;

procedure TDataToXLS.CloseFile;
begin
  FBiffFile.Destroy ;
  inherited;
end;

procedure TDataToXLS.OpenFile;
begin
  inherited;
  FBiffFile := TBiffFile.Create( FStream );

  VPos := 0;
  //ExceFile.SetDefaultRowHeight(233);
  Internal_SaveFonts;
  FBiffFile.SetDimensions(0, 1025, 0, 4);
  Internal_SaveTitle;
  if FComment <> '' Then
    FBiffFile.WriteCellNote(0, 0, FComment);

  Internal_SaveHeaders;
end;

procedure TDataToXLS.WriteRecord;
var
  iCol,
  hPos: Integer;
  AValue: Variant;
  AAttributes: TCellAttributes;
  ADataType: TCellType;
begin
  hPos := 0;
  for iCol := 0 to FColumns.Count -1 do
    if FColumns.Items[iCol].Save then
    begin
      AValue := FColumns.Items[iCol].Field.AsVariant ;
      ADataType := FColumns.Items[iCol].DataType ;

      case FColumns.Items[iCol].Alignment of
        taLeftJustify:
          AAttributes := [caLeft];
        taRightJustify:
          AAttributes := [caRight];
        taCenter:
          AAttributes := [caCenter];
      end;

      if Assigned(FOnGetDetailParams) Then
        FOnGetDetailParams(iCol, hPos, FColumns.Items[iCol], AValue,
          ADataType, AAttributes);

      FBiffFile.WriteCell(VPos, hPos, AValue, ADataType, AAttributes);

      Inc(hPos);
    end;
  Inc(VPos);
end;

function TDataToXLS.GetFields: TExportFields;
begin
  Result := FColumns;
end;

end.
