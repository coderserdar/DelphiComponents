unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DirectOffice;

type
  TFormMain = class(TForm)
    ButtonCreateDocument: TButton;
    ButtonCreateWorkbook: TButton;
    procedure ButtonCreateDocumentClick(Sender: TObject);
    procedure ButtonCreateWorkbookClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses ShellApi;

procedure TFormMain.ButtonCreateDocumentClick(Sender: TObject);
var
  WordDocument: WordprocessingDocument;
  MainPart: MainDocumentPart;
  Document: _Document;
  Body: _Body;
  Paragraph: _DocumentFormat_OpenXml_Wordprocessing_Paragraph;
  Run: _DocumentFormat_OpenXml_Wordprocessing_Run;
  Text: _DocumentFormat_OpenXml_Wordprocessing_Text;
begin
  WordDocument := CoWordprocessingDocumentClass.Create.Create('hello.docx', WordprocessingDocumentType_Document);
  try
    MainPart := WordDocument.AddMainDocumentPart;

     // create document
    Document := CoDocument.Create;
    MainPart.Document := Document;

    // create body
    Body := CoBody.Create;
    Document.AppendChild(Body as _OpenXmlElement);

    // create paragraph
    Paragraph := CoDocumentFormat_OpenXml_Wordprocessing_Paragraph.Create;
    Body.AppendChild(Paragraph as _OpenXmlElement);

    // create run
    Run := CoDocumentFormat_OpenXml_Wordprocessing_Run.Create;
    Paragraph.AppendChild(Run as _OpenXmlElement);

    // create text
    Text := CoDocumentFormat_OpenXml_Wordprocessing_Text.Create;
    Text.Text := 'Hello, world!';
    Run.AppendChild(Text as _OpenXmlElement);
  finally
    WordDocument.Dispose;
  end;

  ShellExecute(Handle, 'open', 'hello.docx', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.ButtonCreateWorkbookClick(Sender: TObject);
var
  Spreadsheet: SpreadsheetDocument;
  WorkbookPart: _WorkbookPart;
  Workbook: _Workbook;
  WorksheetPart: _WorksheetPart;
  Worksheet: _Worksheet;
  SheetData: _SheetData;
  Sheets: _Sheets;
  Sheet: _Sheet;
  Row: _Row;
  Cell: _Cell;
begin
  Spreadsheet := CoSpreadsheetDocumentClass.Create.Create('hello.xlsx', SpreadsheetDocumentType_Workbook);
  try
    // create workbook
    WorkbookPart := Spreadsheet.AddWorkbookPart;
    Workbook := CoWorkbook.Create;
    WorkbookPart.Workbook := Workbook;

    // create sheet data
    SheetData := CoSheetData.Create;

    // create worksheet
    WorksheetPart := WorkbookPart.AddNewPart_WorksheetPart;
    Worksheet := CoWorksheet.Create;
    Worksheet.AppendChild(SheetData as OpenXmlElement);
    WorksheetPart.Worksheet := Worksheet;

    // create sheets
    Sheets := CoSheets.Create;
    Workbook.AppendChild(Sheets as OpenXmlElement);

    // create sheet
    Sheet := CoSheet.Create;
    Sheet.Id := CoStringValueClass.Create.Value(WorkbookPart.GetIdOfPart(WorksheetPart as OpenXmlPart));

    Sheet.SheetId := CoUInt32ValueClass.Create.Value(1);
    Sheet.Name := CoStringValueClass.Create.Value('HelloSheet');
    Sheets.AppendChild(Sheet as OpenXmlElement);

    // create row
    Row := CoRow.Create;
    Row.RowIndex := CoUInt32ValueClass.Create.Value(3);
    SheetData.AppendChild(Row as OpenXmlElement);

    // create cells
    Cell := CoCell.Create;
    Cell.CellReference := CoStringValueClass.Create.Value('C3');
    Cell.DataTypeValue := CellValues_String;
    Cell.CellValue := CoCellValue.Create;
    Cell.CellValue.Text := 'Hello';
    Row.AppendChild(Cell as OpenXmlElement);

    Cell := CoCell.Create;
    Cell.CellReference := CoStringValueClass.Create.Value('D3');
    Cell.DataTypeValue := CellValues_String;
    Cell.CellValue := CoCellValue.Create;
    Cell.CellValue.Text := '1 + 1 = ';
    Row.AppendChild(Cell as OpenXmlElement);

    Cell := CoCell.Create;
    Cell.CellReference := CoStringValueClass.Create.Value('E3');
    Cell.CellFormula := CoCellFormula.Create;
    Cell.CellFormula.Text := '1+1';
    Row.AppendChild(Cell as OpenXmlElement);
  finally
    Spreadsheet.Dispose;
  end;

  ShellExecute(Handle, 'open', 'hello.xlsx', nil, nil, SW_SHOWNORMAL);
end;

end.
