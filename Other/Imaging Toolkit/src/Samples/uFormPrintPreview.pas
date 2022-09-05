// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17613: uFormPrintPreview.pas 
//
//    Rev 1.3    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    07-01-2005 12:15:04  mcm
// Fix error in seScaleImageChanged.

//
//   Rev 1.1    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.0    27-05-2002 16:22:38  mcm

unit uFormPrintPreview;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin, ComCtrls, ToolWin, ExtCtrls,
      {$IFNDEF DCB3} ImgList, {$ENDIF}
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
      Vcl.Samples.Spin, Vcl.ToolWin, Vcl.ImgList,
     {$ENDIF}
     mcmPrinter,
     umcmIntE;


type
  TFormPrintPreview = class(TForm)
    mcmPrinter      : TmcmPrinter;
    mcmPrintPreview : TmcmPrintPreview;

    StatusBar       : TStatusBar;
    ToolBar         : TToolBar;
    tbClose         : TToolButton;
    tbPageMargin    : TToolButton;
    tbPrint         : TToolButton;
    tbSetupPrinter  : TToolButton;
    ToolButton5     : TToolButton;
    tbFirstPage     : TToolButton;
    tbPreviousPage  : TToolButton;
    tbNextPage      : TToolButton;
    tbLastPage      : TToolButton;
    ToolButton3     : TToolButton;
    lZoom           : TLabel;
    cbZoom          : TComboBox;
    ToolButton2     : TToolButton;
    tbCentre        : TToolButton;
    tbFitToPage     : TToolButton;
    ImageList       : TImageList;
    PrinterSetupDialog : TPrinterSetupDialog;
    eScaleImage     : TmcmIntSpin;
    ToolButton1: TToolButton;

    procedure tbCloseClick(Sender: TObject);
    procedure cbZoomChange(Sender: TObject);
    procedure tbFirstPageClick(Sender: TObject);
    procedure tbPreviousPageClick(Sender: TObject);
    procedure tbNextPageClick(Sender: TObject);
    procedure tbLastPageClick(Sender: TObject);
    procedure tbPrintClick(Sender: TObject);
    procedure tbSetupPrinterClick(Sender: TObject);
    procedure PrinterSetupDialogClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mcmPrinterNewPage(Sender: TObject);
    procedure tbCentreClick(Sender: TObject);
    procedure tbFitToPageClick(Sender: TObject);
    procedure seScaleImageChange(Sender: TObject);
    procedure tbPageMarginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FormPrintPreview : TFormPrintPreview;

implementation

uses uFormMargin;

{$R *.DFM}

procedure TFormPrintPreview.FormCreate(Sender : TObject);
begin
  cbZoom.ItemIndex := 0;
  mcmPrintPreview.ZoomToFit;

  // Initialise browser buttons.
  tbFirstPage.Enabled    := False;
  tbPreviousPage.Enabled := False;
  tbNextPage.Enabled     := (mcmPrinter.PageCount > 1);
  tbLastPage.Enabled     := (mcmPrinter.PageCount > 1);
  StatusBar.SimpleText   := 'Page ' + IntToStr(mcmPrintPreview.PageIndex) +
                            ' of ' + IntToStr(mcmPrinter.PageCount);
end;

procedure TFormPrintPreview.tbCloseClick(Sender : TObject);
begin
  Close;
end; // TFormPrintPreview.tbCloseClick.

procedure TFormPrintPreview.cbZoomChange(Sender : TObject);
begin
  // Change zoom factor for preview window.
  case cbZoom.ItemIndex of
  0 : mcmPrintPreview.ZoomToFit;
  1 : mcmPrintPreview.ZoomToWidth;
  2 : mcmPrintPreview.ZoomToHeight;
  3 : mcmPrintPreview.ZoomPercent := 25;
  4 : mcmPrintPreview.ZoomPercent := 50;
  5 : mcmPrintPreview.ZoomPercent := 75;
  6 : mcmPrintPreview.ZoomPercent := 100;
  7 : mcmPrintPreview.ZoomPercent := 150;
  8 : mcmPrintPreview.ZoomPercent := 200;
  end;
end;


procedure TFormPrintPreview.tbFirstPageClick(Sender: TObject);
begin
  // Go to first page.
  mcmPrintPreview.PageIndex := 1;
  tbFirstPage.Enabled    := False;
  tbPreviousPage.Enabled := False;
  tbNextPage.Enabled     := (mcmPrinter.PageCount > 1);
  tbLastPage.Enabled     := (mcmPrinter.PageCount > 1);
end;


procedure TFormPrintPreview.tbPreviousPageClick(Sender: TObject);
begin
  // Got to previous page.
  if (mcmPrintPreview.PageIndex > 1)
  then begin
       mcmPrintPreview.PageIndex := mcmPrintPreview.PageIndex - 1;

       tbNextPage.Enabled := True;
       tbLastPage.Enabled := True;
       if (mcmPrintPreview.PageIndex = 1)
       then begin
            tbFirstPage.Enabled    := False;
            tbPreviousPage.Enabled := False;
       end;
  end;
  StatusBar.SimpleText   := 'Page ' + IntToStr(mcmPrintPreview.PageIndex) +
                            ' of ' + IntToStr(mcmPrinter.PageCount);
end;

procedure TFormPrintPreview.tbNextPageClick(Sender: TObject);
begin
  // Go to next page.
  if (mcmPrintPreview.PageIndex <= mcmPrinter.PageCount)
  then begin
       mcmPrintPreview.PageIndex := mcmPrintPreview.PageIndex + 1;

       tbFirstPage.Enabled    := True;
       tbPreviousPage.Enabled := True;
       if (mcmPrintPreview.PageIndex = mcmPrinter.PageCount)
       then begin
            tbNextPage.Enabled := False;
            tbLastPage.Enabled := False;
       end;
  end;
  StatusBar.SimpleText   := 'Page ' + IntToStr(mcmPrintPreview.PageIndex) +
                            ' of ' + IntToStr(mcmPrinter.PageCount);
end;

procedure TFormPrintPreview.tbLastPageClick(Sender: TObject);
begin
  // Go to last page.
  mcmPrintPreview.PageIndex := mcmPrinter.PageCount;
  tbFirstPage.Enabled    := (mcmPrintPreview.PageIndex > 1);
  tbPreviousPage.Enabled := (mcmPrintPreview.PageIndex > 1);
  tbNextPage.Enabled := False;
  tbLastPage.Enabled := False;
  StatusBar.SimpleText   := 'Page ' + IntToStr(mcmPrintPreview.PageIndex) +
                            ' of ' + IntToStr(mcmPrinter.PageCount);
end;

procedure TFormPrintPreview.tbPrintClick(Sender: TObject);
begin
  // Print pages.
  if (mcmPrinter.PageCount > 0)
  then mcmPrinter.Print;
end;

procedure TFormPrintPreview.tbSetupPrinterClick(Sender: TObject);
begin
  // Access printer set-up dialogue.
  if PrinterSetupDialog.Execute
  then mcmPrinter.RefreshProperties;
  InvalidateRect(Handle, Nil, True);
end;

procedure TFormPrintPreview.PrinterSetupDialogClose(Sender: TObject);
begin
//  mcmPrinter.
end;

procedure TFormPrintPreview.mcmPrinterNewPage(Sender : TObject);
begin
  tbNextPage.Enabled := (mcmPrinter.PageCount > 1);
  tbLastPage.Enabled := (mcmPrinter.PageCount > 1);
  StatusBar.SimpleText   := 'Page ' + IntToStr(mcmPrintPreview.PageIndex) +
                            ' of ' + IntToStr(mcmPrinter.PageCount);
end;

procedure TFormPrintPreview.tbCentreClick(Sender : TObject);
begin
{$IFDEF DCB3_6}
  tbCentre.Down := Not tbCentre.Down;
{$ENDIF}
  mcmPrinter.ImageCenter := tbCentre.Down;
end;

procedure TFormPrintPreview.tbFitToPageClick(Sender : TObject);
begin
{$IFDEF DCB3_6}
  tbFitToPage.Down := Not tbFitToPage.Down;
{$ENDIF}
  mcmPrinter.ImageFitToPage := tbFitToPage.Down;
end;

procedure TFormPrintPreview.seScaleImageChange(Sender : TObject);
begin
  try
    if (eScaleImage.Text <> '') and Assigned(mcmPrinter)
    then mcmPrinter.ImageScale := eScaleImage.Value;
  except
  end;
end;

procedure TFormPrintPreview.tbPageMarginClick(Sender: TObject);
begin
  // Set page margin.
  FormPageMargin := TFormPageMargin.Create(Self);
  FormPageMargin.ForceMargin  := mcmPrinter.ForceMargin;
  FormPageMargin.MarginLeft   := mcmPrinter.MarginLeft;
  FormPageMargin.MarginTop    := mcmPrinter.MarginTop;
  FormPageMargin.MarginRight  := mcmPrinter.MarginRight;
  FormPageMargin.MarginBottom := mcmPrinter.MarginBottom;

  if (FormPageMargin.ShowModal = mrOK)
  then begin
       mcmPrinter.ForceMargin  := FormPageMargin.ForceMargin;
       mcmPrinter.MarginLeft   := FormPageMargin.MarginLeft;
       mcmPrinter.MarginTop    := FormPageMargin.MarginTop;
       mcmPrinter.MarginRight  := FormPageMargin.MarginRight;
       mcmPrinter.MarginBottom := FormPageMargin.MarginBottom;
  end;
  FormPageMargin.Free;
end;

{$IFDEF DCB3_6}
  {$UNDEF DCB3_6}
{$ENDIF}


end.
