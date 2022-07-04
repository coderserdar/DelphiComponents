{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLHtmlExport.pas
   Copyright (c) 2003-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}
{$I fbl.inc}
unit FBLHtmlExport;

interface

uses
  SysUtils, Classes, FBLDsql, ibase_h;

type
  THtmlPageTypes = (hptOnlyTable, hptComplete);

  TFBLHtmlExport = class(TObject)
  private
    FQuery: TFBLdsql;
    FShowTableCaption: boolean;
    FShowTableTitle: boolean;
    FPrepared: boolean;
    FEOF: boolean;
    FHtmlPageType: THtmlPageTypes;
    FPageTitle: string;
    FTagBodyOption: string;
    FTableCaption: string;
    FTagTableCaptionOption: string;
    FTagTableOption: string;
    FTagThOption: string;
    FTagTrOption: string;
    FTagTdOption: string;
    FHtmlHeader: TStringList;
    FHtmlCurrentRecord: TStringList;
    FHtmlFooter: TStringList;
    FRecordCount: integer;
    function GetTagBody: string;
    function GetTagTable: string;
    function GetTagTableCaption: string;
    function GetTagTh: string;
    function GetTagTr: string;
    function GetTagTd: string;
    function GetHtmlHeader: TStringList;
    function GetHtmlCurrentRecord: TStringList;
    procedure Prepare;
  public
    constructor Create(AQuery: TFBLDsql);
    destructor Destroy; override;
    procedure NextRecord;
    procedure BeginFetch;
    property ShowTableCaption: boolean read FShowTableCaption write FShowTableCaption;
    property ShowTableTitle: boolean read FShowTableTitle write FShowTableTitle;
    property TagBodyOption: string read FTagBodyOption write FTagBodyOption;
    property TableCaption: string read FTableCaption write FTableCaption;
    property TagTableOption: string read FTagTableOption write FTagTableOption;
    property TagTableCaptionOption: string read FTagTableCaptionOption  
      write FTagTableCaptionOption;
    property TagThOption: string read FTagThOption write FTagThOption;
    property TagTrOption: string read FTagTrOption write FTagTrOption;
    property TagTdOption: string read FTagTdOption write FTagTdOption;
    property HtmlHeader: TStringList read GetHtmlHeader;
    property HtmlCurrentRecord: TStringList read GetHtmlCurrentRecord;
    property HtmlFooter: TStringList read FHtmlFooter;
    property EOF: boolean read FEOF;
    property FetchCount: integer read FRecordCount;
    property HtmlPageType: THtmlPageTypes read FHtmlPageType write FHtmlPageType;
    property PageTitle: string read FPageTitle write FPageTitle;
  end;

implementation

constructor TFBLHtmlExport.Create(AQuery: TFBLDsql);
begin
  FQuery := AQuery;
  FShowTableCaption := False;
  FShowTableTitle := True;
  FEOF := False;
  FPrepared := False;
  FRecordCount := 0;
  FHtmlPageType := hptOnlyTable;
  FHtmlHeader := TStringList.Create;
  FHtmlCurrentRecord := TStringList.Create;
  FHtmlFooter := TStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TFBLHtmlExport.Destroy;
begin
  FHtmlHeader.Free;
  FHtmlCurrentRecord.Free;
  FHtmlFooter.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagBody: string;
begin
  if FTagBodyOption = '' then
    Result := '<body>'
  else
    Result := '<body ' + FTagBodyOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagTable: string;
begin
  if FTagTableOption = '' then
    Result := '  <table>'
  else
    Result := '  <table ' + FTagTableOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagTableCaption: string;
begin
  if FTagTableCaptionOption = '' then
    Result := '    <caption>'
  else
    Result := '    <caption ' + FTagTableCaptionOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagTh: string;
begin
  if FTagThOption = '' then
    Result := '        <th>'
  else
    Result := '        <th ' + FTagThOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagTr: string;
begin
  if FTagTrOption = '' then
    Result := '      <tr>'
  else
    Result := '      <tr ' + FTagTrOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetTagTd: string;
begin
  if FTagTdOption = '' then
    Result := '        <td>'
  else
    Result := '        <td ' + FTagTdOption + '>';
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetHtmlHeader: TStringList;
begin
  Result := FHtmlHeader;
end;

//------------------------------------------------------------------------------

function TFBLHtmlExport.GetHtmlCurrentRecord: TStringList;
var
  i: integer;
begin
  FHtmlCurrentRecord.Clear;
  if not FQuery.EOF then
  begin
    FHtmlCurrentRecord.Add(GetTagTr);
    for i := 0 to FQuery.FieldCount - 1 do
    begin
      case FQuery.FieldType(i) of
        SQL_BLOB:
          FHtmlCurrentRecord.Add(GetTagTd + 'BLOB' + '</td>');
        SQL_ARRAY:
          FHtmlCurrentRecord.Add(GetTagTd + 'ARRAY' + '</td>');
        else
          FHtmlCurrentRecord.Add(GetTagTd + FQuery.FieldAsString(i) + '</td>');
      end;
    end;
    FHtmlCurrentRecord.Add('      ' + '</tr>');
  end;
  Result := FHtmlCurrentRecord;
end;

//------------------------------------------------------------------------------

procedure TFBLHtmlExport.Prepare;
var
  i: integer;
begin
  FHtmlHeader.Clear;
  if FHtmlPageType = hptComplete then
  begin
    FHtmlHeader.Add('<html>');
    if FPageTitle <> '' then
    begin
      FHtmlHeader.Add('  <head>');
      FHtmlHeader.Add('  <title>' + FPageTitle + '</title>');
      FHtmlHeader.Add('  </head>')
    end;
    FHtmlHeader.Add(GetTagBody);
  end;

  FHtmlHeader.Add(GetTagTable);
  if FShowTableCaption then
  begin
    FHtmlHeader.Add(GetTagTableCaption);
    FHtmlHeader.Add('    ' + FTableCaption);
    FHtmlHeader.Add('    </caption>');
  end;
  if FShowTableTitle then
  begin
    FHtmlHeader.Add(GetTagTr);
    for i := 0 to FQuery.FieldCount - 1 do
      FHtmlHeader.Add(GetTagTh + FQuery.FieldName(i) + '</th>');
    FHtmlHeader.Add('      ' + '</tr>');
  end;
  FPrepared := True;
  FHtmlFooter.Clear;
  FHtmlFooter.Add('  </table>');
  if FHtmlPageType = hptComplete then
  begin
    FHtmlFooter.Add('</body>');
    FHtmlFooter.Add('</html>');
  end;
  if FQuery.BOF then NextRecord;
end;

//------------------------------------------------------------------------------

procedure TFBLHtmlExport.BeginFetch;
begin
  if not FPrepared then Prepare;
  if FQuery.BOF then NextRecord;
  GetHtmlCurrentRecord;
end;


//------------------------------------------------------------------------------

procedure TFBLHtmlExport.NextRecord;
begin
  FQuery.Next;
  Inc(FRecordCount);
  if not FQuery.EOF then
  begin
    GetHtmlCurrentRecord;
  end
  else
    FEOF := True;
end;

end.
