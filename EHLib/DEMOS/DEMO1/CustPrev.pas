unit CustPrev;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, PrViewEh, StdCtrls, ExtCtrls;

type
  TfCustomPreview = class(TForm)
    Panel1: TPanel;
    bPrint: TButton;
    bPrinterSetupDialog: TButton;
    bPrevPage: TButton;
    bNextPage: TButton;
    bStop: TButton;
    bClose: TButton;
    PreviewBox1: TPreviewBox;
    StatusBar1: TStatusBar;
    procedure bPrintClick(Sender: TObject);
    procedure bPrinterSetupDialogClick(Sender: TObject);
    procedure bPrevPageClick(Sender: TObject);
    procedure bNextPageClick(Sender: TObject);
    procedure PreviewBox1PrinterPreviewChanged(Sender: TObject);
    procedure PreviewBox1OpenPreviewer(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fCustomPreview: TfCustomPreview;

implementation

{$R *.DFM}

procedure TfCustomPreview.bPrintClick(Sender: TObject);
begin
  PreviewBox1.PrintDialog;
end;

procedure TfCustomPreview.bPrinterSetupDialogClick(Sender: TObject);
begin
  PreviewBox1.PrinterSetupDialog;
end;

procedure TfCustomPreview.bPrevPageClick(Sender: TObject);
begin
  PreviewBox1.PageIndex := Pred(PreviewBox1.PageIndex);
end;

procedure TfCustomPreview.bNextPageClick(Sender: TObject);
begin
  PreviewBox1.PageIndex:=Succ(PreviewBox1.PageIndex);
end;

procedure TfCustomPreview.PreviewBox1PrinterPreviewChanged(
  Sender: TObject);
begin
  bStop.Enabled := PreviewBox1.Printer.Printing;
  bClose.Enabled := not PreviewBox1.Printer.Printing;
  bPrint.Enabled := not PreviewBox1.Printer.Printing;
  bPrinterSetupDialog.Enabled := not PreviewBox1.Printer.Printing and
    (Assigned(PreviewBox1.OnPrinterSetupDialog) or Assigned(PreviewBox1.OnPrinterSetupChanged)) and
    Assigned(PreviewBox1.PrinterSetupOwner);
  bPrevPage.Enabled:=PreviewBox1.PageIndex>1;
  bNextPage.Enabled:=PreviewBox1.PageIndex<PreviewBox1.PageCount;
  StatusBar1.SimpleText:='Page '+IntToStr(PreviewBox1.PageIndex)+' of '+IntToStr(PreviewBox1.PageCount);
  Caption := 'Custom preview form - ' + PreviewBox1.Printer.Title;
end;

procedure TfCustomPreview.PreviewBox1OpenPreviewer(Sender: TObject);
begin
  if IsIconic(Handle) then ShowWindow(Handle,sw_Restore);
  BringWindowToTop(Handle);
  if not Visible then Show;
end;

procedure TfCustomPreview.bCloseClick(Sender: TObject);
begin
  Close;
end;

end.
