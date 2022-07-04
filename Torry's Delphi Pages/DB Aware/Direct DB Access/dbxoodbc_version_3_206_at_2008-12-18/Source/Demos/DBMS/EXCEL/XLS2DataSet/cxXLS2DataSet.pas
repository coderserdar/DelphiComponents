{***********************************************}
{  Delphi Free Open Source                      }
{  Version 2008.11.04                           }
{                                               }
{  Copyright (c) 2007-2009 Vadim V.Lopushansky  }
{                Kyiv, Ukraine                  }
{                e-mail: pult@ukr.net           }
{***********************************************}
//
// Required Libraries:
//   - kbmMemTable Library
//   - Developer Express SpreadSheet Library

unit cxXLS2DataSet;
{$B-,O-,D+,L+}

interface

uses
  Windows, SysUtils, Classes, DB, Controls, Forms, Dialogs,
  kbmMemTable, StrUtils;

type
  TXLS2DataSet = class
  protected
    function DoLoadXLSFile(const FileName: string; const SheetName: string = ''; SheetNum: Integer = -1): TDataSet;
  public
    IsHeader: Boolean;
    HeaderRowNo: Integer;
    SkipFirstRowCount: Integer;
    SkipLastRowCount: Integer;
    FieldsTrim: Boolean;
    FieldsRemoveCLR: Boolean;

    constructor Create();

    function LoadXLSFile(const FileName: string; const SheetName: string): TDataSet; overload;
    function LoadXLSFile(const FileName: string; SheetNum: Integer = -1): TDataSet; overload;
  end;

implementation

uses
  //
  // cx Excel
  //
  cxSSTypes, cxSSStyles, cxSSHeaders, cxSSData, cxSSheet, cxSSPainterWrapper,
  cxExcelConst, ShellApi;

type
  TcxSpreadSheetBookPro = class(TcxSpreadSheetBook);
  TcxSSBookSheetPro = class(TcxSSBookSheet);
  TcxSSCellObjectPro = class(TcxSSCellObject);
  TcxSSDataStoragePro = class(TcxSSDataStorage);

function ExcelTrim(const S: string): string;
var
  I, L: Integer;
const
  chNoBreakSpace = #160;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and ( (S[I] <= ' ') or (S[I] = chNoBreakSpace) )
    do Inc(I);
  if I > L then Result := '' else
  begin
    while ( (S[L] <= ' ') or (S[L] = chNoBreakSpace) )
      do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

procedure xls_fix_date(var Date: TDateTime);
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  if Year > 1900 then
    Exit;
  Year := CurrentYear;
  Date := EncodeDate(Year, Month, Day);
end;

{ TLoadXLSFileOption }

constructor TXLS2DataSet.Create();
begin
  inherited Create();
  IsHeader := True;
  HeaderRowNo := 0;
  SkipFirstRowCount := 1;
  SkipLastRowCount := 0;
end;

function TXLS2DataSet.LoadXLSFile(const FileName: string; const SheetName: string): TDataSet;
begin
  Result := DoLoadXLSFile(FileName, SheetName, -1);
end;

function TXLS2DataSet.LoadXLSFile(const FileName: string; SheetNum: Integer = -1): TDataSet;
begin
  Result := DoLoadXLSFile(FileName, '', SheetNum);
end;

function TXLS2DataSet.DoLoadXLSFile;//(const FileName: string; const SheetName: string = ''; SheetNum: Integer = -1): TDataSet;
var
  cxSpreadSheetBook: TcxSpreadSheetBookPro;
  cxSpreadSheet: TcxSpreadSheet;
  cxBookSheet: TcxSSBookSheetPro;
  vCursor: TCursor;
  vDataStorage: TcxSSDataStoragePro;
  iRow, iCol, iCollCount: Integer;
  S: string;
  D: TkbmMemTable;
  F: TFieldDef;
  vDateTime: TDateTime;
begin
  Result := nil;
  if (not FileExists(FileName)) then
    Exit;
  cxSpreadSheetBook := nil;
  cxSpreadSheet := nil;
  vCursor := Screen.Cursor;
  D := TkbmMemTable.Create(nil);
  try
    Screen.Cursor := crHourGlass;
    //
    // 1) Read XLS File
    //
    if (SheetNum >= 0) or (SheetName <> '') then
    begin
      cxSpreadSheetBook := TcxSpreadSheetBookPro(TcxSpreadSheetBook.Create(nil));
      cxSpreadSheetBook.LoadFromFile(FileName);
      //
      // Select ActiveSheet
      //
      if SheetNum >= 0 then
      begin
        if SheetNum >= cxSpreadSheetBook.PageCount then
          raise Exception.CreateFmt('Excel file not contained sheet num: %d', [SheetNum]);
        cxSpreadSheetBook.ActivePage := SheetNum;
      end
      else if SheetName <> '' then
      begin
        iCol := 0;
        for iRow := 0 to cxSpreadSheetBook.PageCount - 1 do
        begin
          if SameText(cxSpreadSheetBook.Pages[iRow].Caption, SheetName) then
          begin
            cxSpreadSheetBook.ActivePage := iRow;
            iCol := 1;
            Break;
          end;
        end;
        if iCol = 0 then
          raise Exception.CreateFmt('Excel file not contained sheet "%s"', [SheetName]);
      end;

      cxBookSheet := TcxSSBookSheetPro(cxSpreadSheetBook.ActiveSheet);
    end
    else
    begin
      cxSpreadSheet := TcxSpreadSheet.Create(nil);
      cxSpreadSheet.LoadFromFile(FileName);

      cxBookSheet := TcxSSBookSheetPro(cxSpreadSheet.Sheet);
    end;

    //
    // Convert Excel File to DataSet
    //
    if (IsHeader and (cxBookSheet.RowCount < 2)) or
      ((not IsHeader) and (cxBookSheet.RowCount < 1)) then
    begin
      MessageDlg('No rows in excel file', mtError, [mbOk], 0);
      Exit;
    end;
    vDataStorage := TcxSSDataStoragePro(cxBookSheet.DataStorage);
    //
    // make fields
    //
    iRow := HeaderRowNo;
    if (iRow < 0) or (iRow > cxBookSheet.RowCount - 1) then
    begin
     iRow := 0;
     IsHeader := False;
    end;
    iCollCount := cxBookSheet.ColumnCount;
    F := D.FieldDefs.AddFieldDef;
    F.Name := '#RowNo#';
    F.DisplayName := 'Row Num';
    F.DataType := ftInteger;
    for iCol := 0 to  iCollCount - 1 do
    begin
      F := D.FieldDefs.AddFieldDef;
      if IsHeader then
      begin
        S := ExcelTrim(vDataStorage[iCol, iRow].Text);
        if S <> '' then
        begin
          S := ExcelTrim(StringReplace(S, #13#10, ' ', [rfReplaceAll]));
          S := ExcelTrim(StringReplace(S, #10, ' ', [rfReplaceAll]));
        end;
      end
      else
        S := '';
      if S = '' then
      begin
        S := '#FIELD#_' + IntToStr(iCol + 1);
        //F.DisplayName := S;
      end;
      F.Name := S;
      F.DataType := ftString;
      F.Size := 250;
    end;
    D.Open;
    D.DisableControls;
    //
    // load records
    //
    for iRow := SkipFirstRowCount to (cxBookSheet.RowCount - 1) - SkipLastRowCount do
    begin
      D.Append;
      D.Fields[0].AsInteger := iRow;
      for iCol := 0 to iCollCount - 1 do
      begin
        // todo: case datatype: datetime: ... convert raw to formated string ???
        case vDataStorage[iCol, iRow].DataType of
         dtDateTime:
           begin
             vDateTime := vDataStorage[iCol, iRow].DateTime; // test: DateTimeToStr(vDataStorage[iCol, iRow].DateTime)
             xls_fix_date(vDateTime);
             S := DateToStr(vDateTime);
           end;
         else
         begin
           S := vDataStorage[iCol, iRow].Text;
           if (S <> '') then
           begin
             if FieldsTrim then
               S := ExcelTrim(S);
             if FieldsRemoveCLR and (S <> '') then
             begin
               S := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
               S := StringReplace(S, #10, ' ', [rfReplaceAll]);
               if FieldsTrim and (S <> '') then
                 S := ExcelTrim(S);
             end;
           end;
         end;
        end;
        D.Fields[iCol+1].AsString := S;
      end;
    end;
    D.First;
    D.EnableControls;
    Result := D;
    D := nil;
  finally
    Screen.Cursor := vCursor;
    D.Free;
    cxSpreadSheetBook.Free;
    cxSpreadSheet.Free;
  end;
end;

end.
