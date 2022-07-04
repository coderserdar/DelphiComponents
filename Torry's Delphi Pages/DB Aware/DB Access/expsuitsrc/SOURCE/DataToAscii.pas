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

unit DataToAscii;

{$I DELPHI.VER}

interface

uses
  Classes, DataExport, Db;

type
  TDTAOptions = set of (qaSaveHeaders, qaRemoveCR, qaRemoveLF);

  TCancelEvent = procedure (Sender: TObject; var Cancel: Boolean) of object;

  TAsciiField = class(TExportField)
  private
    FTitle: string;
    FFixedLength: Integer;
    FAlignment: TAlignment;
    FFillChar: Char;
  protected
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
  published
    property Title: string read FTitle write FTitle;
    property FixedLength: Integer read FFixedLength write FFixedLength default 0;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property FillChar: Char read FFillChar write FFillChar default #32;
  end;

  TAsciiFields = class(TExportFields)
  private
    function GetItem(Index: Integer): TAsciiField;
    procedure SetItem(Index: Integer; const Value: TAsciiField);
  protected
  public
    function Add: TAsciiField;
    property Items[Index: Integer]: TAsciiField read GetItem write SetItem; default;
  end;


  TDataToAscii = class(TDataExport)
  private
    FQuote: Char;
    FSeparator: Char;
    FEOFChar: Char;
    FEOLChar: Char;
    FFields: TAsciiFields;
    FOptions: TDTAOptions;
    {function FieldString(Campo: TField; Texto: string): string; overload;}
    function FieldString(AField: TAsciiField; Texto: string): string; {overload;}
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure WriteRecord; override;
    procedure WriteEOL;
    procedure WriteHeader;
  public
    function GetFields: TExportFields; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property RecNo;
  published
    property DataSet;
    property Separator: Char read FSeparator write FSeparator default #9;
    property Quote: Char read FQuote write FQuote default #34;
    property EolChar: Char read FEOLChar write FEOLChar default #0;
    property EofChar: Char read FEOFChar write FEOFChar default #0;
    property Fields: TAsciiFields read FFields write FFields;
    property Options: TDTAOptions  read FOptions write FOptions
      default [qaRemoveCR, qaRemoveLF];
    property FetchFirst;
    property SaveIfEmpty;
    property MaxRecords;
    property OnBeginExport;
    property OnEndExport;
    property BeforeWriteRecord;
    property AfterWriteRecord;
  end;

implementation

Uses
  DBGrids, SysUtils;

{$IFDEF LESS110}
procedure CharDelete(var S: string; const C: Char);
var
  i: Integer;
begin
  for i := Length(S) downto 1 do
    if S[i] = C then
      Delete(S, i, 1);
end;
{$ENDIF}


{ TAsciiField }

procedure TAsciiField.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAsciiField then
    with Source as TAsciiField do
    begin
      FTitle := Title;
      FFixedLength := FixedLength;
      FAlignment := Alignment;
      FFillChar := FillChar;
      Self.DataField := DataField;
    end
  else
  if Source is TColumn then
    with Source as TColumn do
    begin
      FTitle := Title.Caption ;
      Save := {$IFnDEF LESS110}Visible{$ELSE}True{$ENDIF};
      FFixedLength := 0;
      FAlignment := Alignment;
      FFillChar := #32;
      DataField := FieldName;
    end
  else
  if Source is TField then
    with Source as TField do
    begin
      FTitle := DisplayLabel ;
      FFixedLength := DisplayWidth ;
      FAlignment := Alignment;
      FFillChar := #32;
    end
end;

constructor TAsciiField.Create(Collection: TCollection);
begin
  inherited;
  FFixedLength := 0;
  FAlignment := taLeftJustify;
  FFillChar := #32;
end;

{ TAsciiFields }

function TAsciiFields.Add: TAsciiField;
begin
  Result := TAsciiField(inherited Add);
end;

function TAsciiFields.GetItem(Index: Integer): TAsciiField;
begin
  Result := TAsciiField(inherited GetItem(Index));
end;

procedure TAsciiFields.SetItem(Index: Integer; const Value: TAsciiField);
begin
  inherited SetItem(Index, Value);
end;

function TDataToAscii.GetFields: TExportFields;
begin
  Result := FFields ;
end;

{ TDataToAscii }

procedure TDataToAscii.CloseFile;
begin
  if FEofChar <> #0 then
    WriteChar(FEofChar);

  inherited;
end;

constructor TDataToAscii.Create(AOwner: TComponent);
begin
  inherited;
  FEofChar := #0;
  FEolChar := #0;
  FSeparator := #9;
  FQuote := #34;
  FFields := TAsciiFields.Create(Self, TAsciiField);
  FOptions := [qaRemoveCR, qaRemoveLF];
end;

destructor TDataToAscii.Destroy;
begin
  FFields.Free;
  inherited;
end;

{
function TDataToAscii.FieldString(Campo: TField; Texto: string): string ;
begin
  if qaRemoveCR in FOptions then
    Texto := StringReplace(Texto, #13, '', [rfReplaceAll]);

  if qaRemoveLF in FOptions then
    Texto := StringReplace(Texto, #10, '', [rfReplaceAll]);

  if FQuote <> #0 then
    Result := FQuote + Result + FQuote;

  if (FSeparator <> #0) and ((Campo.Index <> DataSet.FieldCount-1)) then
    Result := Result + FSeparator;
end;
}

function TDataToAscii.FieldString(AField: TAsciiField; Texto: string): string ;
var
  ASize: Integer;
begin
  if qaRemoveCR in FOptions then
    {$IFDEF LESS110}
    CharDelete(Texto, #13);
    {$ELSE}
    Texto := StringReplace(Texto, #13, '', [rfReplaceAll]);
    {$ENDIF}

  if qaRemoveLF in FOptions then
    {$IFDEF LESS110}
    CharDelete(Texto, #10);
    {$ELSE}
    Texto := StringReplace(Texto, #10, '', [rfReplaceAll]);
    {$ENDIF}

  ASize := AField.FixedLength;
  if (ASize > 0) then
    Case AField.Alignment of
      taLeftJustify:
        Result := Copy(Texto + StringOfChar(AField.FillChar, ASize - Length(Texto)), 1, ASize);
      taRightJustify:
        Result := Copy(StringOfChar(AField.FillChar, ASize - Length(Texto)) + Texto, 1, ASize);
      taCenter:
        Result := Copy(Texto + StringOfChar(AField.FillChar, ASize - Length(Texto)), 1, ASize);
    end
  else
    Result := Texto ;

  if FQuote <> #0 then
    Result := FQuote + Result + FQuote;

  if FSeparator <> #0 then
    if ((FFields.Count > 0) and (AField.Index < FFields.Count-1)) or
       ((FFields.Count = 0) and (AField.Field.Index < DataSet.FieldCount-1)) then
      Result := Result + FSeparator;
end;


procedure TDataToAscii.OpenFile;
begin
  inherited;
  WriteHeader;
end;

procedure TDataToAscii.WriteEOL;
begin
  if FEolChar = #0 then
    WriteString(#13#10, 2)
  else
    WriteString(FEolChar + #13#10 {$IFDEF LESS110}, 0{$ENDIF});
end;

procedure TDataToAscii.WriteHeader;
var
  iField: Integer;
begin
  if qaSaveHeaders in FOptions then
  begin
    for iField := 0 to FFields.Count - 1 do
      if FFields[iField].Save then
        WriteString(FieldString(FFields[iField], FFields[iField].Title)
                    {$IFDEF LESS110}, 0{$ENDIF});

    WriteEol;
  end;
end;

procedure TDataToAscii.WriteRecord;
var
  iField: Integer;
begin
  for iField := 0 to FFields.Count - 1 do
    if FFields[iField].Save then
      WriteString(FieldString(FFields[iField],
                              FFields[iField].Field.AsString)
                  {$IFDEF LESS110}, 0{$ENDIF});

  WriteEol;
end;

end.
