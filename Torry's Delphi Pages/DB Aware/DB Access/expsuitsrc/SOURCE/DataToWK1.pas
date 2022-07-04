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

unit DataToWK1;

{$I DELPHI.VER}

interface

uses
  Classes, Graphics, DataExport, Db, WKFile;

type
  TwkColumn = class(TExportField)
  private
    FColWidth: Integer;
    FTitle: string;
    FColumnType: TwkCellType;
  protected
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write FTitle;
    property ColWidth: Integer read FColWidth write FColWidth default 0;
    property DataType: TwkCellType read FColumnType write FColumnType
      default wtLabel;
  end;

  TwkColumns = class(TExportFields)
  private
    function GetItem(Index: Integer): TwkColumn;
    procedure SetItem(Index: Integer; const Value: TwkColumn);
  protected
  public
    function Add: TwkColumn;
    property Items[Index: Integer]: TwkColumn read GetItem write SetItem; default;
  end;

  {TGetDetailCellParams = procedure(ARow, ACol: Integer; AColumn: TwkColumn;
    var AValue: Variant; var AColumnType: TCellType;
    var AAttributes: TCellAttributes) of object;}

  TDataToWK1 = class(TDataExport)
  private
    VPos: Integer; { Keeps vertical position when is drawing the file }
    FColumns: TwkColumns;
    FTitle: string;
    FSaveHeaders: Boolean;
  {Excel}
    procedure Internal_SaveTitle;
    procedure Internal_SaveHeaders;
  protected
    FWKFile: TWKFile;
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
    property Columns: TwkColumns read FColumns write FColumns;
    property Title: string read FTitle write FTitle;
    property SaveHeaders: Boolean read FSaveHeaders write FSaveHeaders default True;
  end;

implementation

uses
  DBGrids, SysUtils;

function FieldToCellType(AFieldType: TFieldType): TwkCellType;
begin
  Case AFieldType of
    ftBytes, ftSmallint, ftInteger, ftWord, ftAutoInc, ftBoolean:
      Result := wtInteger;
    ftFloat, ftCurrency, ftBCD:
      Result := wtDouble;
  else
    Result := wtLabel;
  end;
end;

{ TwkColumn }

procedure TwkColumn.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TwkColumn then
    with Source as TwkColumn do
    begin
      FTitle := Title;
      FColWidth := ColWidth;
      FColumnType := DataType;
    end
  else
  if Source is TField then
  begin
    with Source as TField do
    begin
      FTitle := DisplayLabel ;
      FColWidth := 0;
      FColumnType := FieldToCellType(DataType);
    end;
  end else if Source is TColumn then
  begin
    with Source as TColumn do
    begin
      FTitle := Title.Caption;

      if Assigned(TColumn(Source).Field) then
        FColumnType := FieldToCellType(TColumn(Source).Field.DataType);
    end
  end;
end;

constructor TwkColumn.Create(Collection: TCollection);
begin
  inherited;
  {FAlignment := taLeftJustify;}
  FColWidth := 0;
  FColumnType := wtLabel;
end;

{ TwkColumns }

function TwkColumns.Add: TwkColumn;
begin
  Result := TwkColumn(inherited Add);
end;

function TwkColumns.GetItem(Index: Integer): TwkColumn;
begin
  Result := TwkColumn(inherited GetItem(Index));
end;

procedure TwkColumns.SetItem(Index: Integer; const Value: TwkColumn);
begin
  inherited SetItem(Index, Value);
end;

{ TDataToWK1 }

procedure TDataToWK1.CloseFile;
begin
  FWKFile.Destroy;
  inherited;
end;

constructor TDataToWK1.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TwkColumns.Create(Self, TwkColumn);
  FTitle := '';
  FSaveHeaders := True;
end;

destructor TDataToWK1.Destroy;
begin
  FColumns.Free;
  inherited;
end;

function TDataToWK1.GetFields: TExportFields;
begin
  Result := FColumns;
end;

procedure TDataToWK1.Internal_SaveHeaders;
var
  iCol,
  hPos: Integer;
begin
  if FSaveHeaders then
  begin
    hPos := 0;
    for iCol := 0 to FColumns.Count -1 do
      if FColumns.Items[iCol].Save then
      begin
        if FColumns.Items[iCol].ColWidth > 0 then
          FWKFile.SetColWidth(hPos, FColumns.Items[iCol].ColWidth);

        FWKFile.WriteStringCell(VPos, hPos, FColumns.Items[iCol].Title, 255); {D3}
        Inc(hPos);
      end;
    Inc(VPos);
  end;
end;

procedure TDataToWK1.Internal_SaveTitle;
begin
  if FTitle <> '' then
  begin
    FWKFile.WriteStringCell(0, 0, FTitle, 255); {D3}
    Inc(VPos);
  end;
end;

procedure TDataToWK1.OpenFile;
begin
  inherited;
  FWKFile := TWKFile.Create(FStream);

  VPos := 0;
  Internal_SaveTitle;
  Internal_SaveHeaders;
end;

procedure TDataToWK1.WriteRecord;
var
  iCol,
  hPos: Integer;
  AValue: Variant;
  ADataType: TwkCellType;
begin
  hPos := 0;
  for iCol := 0 to FColumns.Count -1 do
    if FColumns.Items[iCol].Save then
    begin
      AValue := FColumns.Items[iCol].Field.AsVariant;
      ADataType := FColumns.Items[iCol].DataType;

      FWKFile.WriteCell(VPos, hPos, AValue, ADataType, 255); {D3}
      Inc(hPos);
    end;
  Inc(VPos);
end;

end.
