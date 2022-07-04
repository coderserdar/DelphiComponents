program K2DCUs;

uses
  QForms,
  RLReport in 'RLReport.pas',
  RLUtils in 'RLUtils.pas',
  RLPreviewForm in 'RLPreviewForm.pas',
  RLPreview in 'RLPreview.pas',
  RLFeedBack in 'RLFeedBack.pas',
  RLMetaFile in 'RLMetaFile.pas',
  RLParser in 'RLParser.pas',
  RLConsts in 'RLConsts.pas',
  RLTypes in 'RLTypes.pas',
  RLFilters in 'RLFilters.pas',
  RLDraftFilter in 'RLDraftFilter.pas',
  RLPrintDialog in 'RLPrintDialog.pas',                  
  RLSaveDialog in 'RLSaveDialog.pas',                  
  RLRichFilter in 'RLRichFilter.pas',
  RLHTMLFilter in 'RLHTMLFilter.pas',
  RLPrinters in 'RLPrinters.pas',
  RLBarcode in 'RLBarcode.pas',
  RLPDFFilter in 'RLPDFFilter.pas',
  RLXLSFilter in 'RLXLSFilter.pas',
  RLRichText in 'RLRichText.pas',
  RLSpoolFilter in 'RLSpoolFilter.pas',
  RLMetaCLX in 'RLMetaCLX.pas',
  RLFindDialog in 'RLFindDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
