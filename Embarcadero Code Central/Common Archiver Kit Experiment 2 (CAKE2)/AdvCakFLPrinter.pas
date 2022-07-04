unit AdvCakFLPrinter;

interface

uses
  SysUtils, Classes, CakFileListPrinter, QzMiniHtml2, QzPDFPrinter2, QzHtmlLabel2;

type
  TAdvCakFLPrinter = class(TCakFileListPrinter)
  private
    { Private declarations }
    MiniHtml : TQzMiniHtml2;
    PDFPrinter : TQzPDFPrinter2;
    procedure PDFPrint;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure PdfFileList;
    procedure CreateFileList; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure TAdvCakFLPrinter.PDFPrint;
var i : integer;
begin
  with PDFPrinter do
  if Assigned(MiniHtml) then
  begin
  if Assigned(MiniHtml.HtmlPrinter) then
    MiniHtml.HtmlPrinter.Free;
  Printer := TPdfPrinter.Create(MiniHtml);
  if Filename <> '' then
    Printer.OutputFilename := Filename;

  if (MiniHtml.TagList.Count = 0) and (MiniHtml.Owner is TQzHtmlLabel2) then
    TQzHtmlLabel2(MiniHtml.Owner).Paint;

  MiniHtml.HtmlPrinter := Printer;
  for i := 0 to MiniHtml.TotalMiniHtml -1 do
      MiniHtml.Subminihtml[i].HtmlPrinter := Printer; 
  MiniHtml.HtmlPrinter.Init(MiniHtml.CacheBitmap.Height);
  //UpdateMiniHtml;
  //MiniHtml.width := 100;
  MiniHtml.Update(Printer);
  MiniHtml.Print;

  MiniHtml.HtmlPrinter.Save;
  if Assigned(MiniHtml.HtmlPrinter) then
    MiniHtml.HtmlPrinter.Free;
  MiniHtml.HtmlPrinter := nil;
  for i := 0 to MiniHtml.TotalMiniHtml -1 do
      MiniHtml.Subminihtml[i].HtmlPrinter := nil;
  MiniHtml.Update;

  end;
end;

procedure Register;
begin
  RegisterComponents('QZip', [TAdvCakFLPrinter]);
end;

end.
