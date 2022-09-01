unit bvTabPrintPreview;

interface

uses
  Windows,
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Buttons, DB, Printers,bvLocalization,dbgrids, QExtCtrls,
  QControls, QForms, QButtons;

type
  TdbPrintPreviewForm = class(TForm)
    Panel1: TPanel;
    Printbtn: TSpeedButton;
    ScrollBox: TScrollBox;
    PageDec: TSpeedButton;
    PageInc: TSpeedButton;
    Image: TImage;
    Bevel3: TBevel;
    ExitButton: TSpeedButton;
    BtnOptions: TSpeedButton;
    BtnColumns: TSpeedButton;
    PageLab: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PageDecClick(Sender: TObject);
    procedure PageIncClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure BtnOptionsClick(Sender: TObject);
    procedure BtnColumnsClick(Sender: TObject);
    procedure PrintbtnClick(Sender: TObject);
  protected
  private
    { Private declarations }
    PageCount:integer;
  public
    { Public declarations }
    PageNo: integer;

    Grid:TCustomDBGrid;
    procedure PaintPreview;
    procedure UpdateCTrls;
  end;

var
  dbPrintPreviewForm : TdbPrintPreviewForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

uses SetupPanelUnit,Columnsview,bvDBTablePrinter;


procedure TdbPrintPreviewForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
end;

{/////////////////////////////////////////////////////////////////////////////}

procedure TdbPrintPreviewForm.FormCreate(Sender: TObject);
begin
  ExitButton.caption:=StrExit;
  BtnColumns.caption:=StrColumns;
  BtnOptions.caption:=StrOptions;
  self.caption:=strQuickPrint;

  Grid:=nil;

  PageNo:= 1;
  PageCount:=0;
  updatectrls;
end;

procedure TdbPrintPreviewForm.UpdateCTRLS;
begin
  PageDec.Enabled:= PageNo>1;
  PageInc.Enabled:= PageNo<PageCount;

  PageLab.Caption:= IntToStr(pageno)+'/'+IntToStr(Pagecount);
end;

procedure TdbPrintPreviewForm.PageDecClick(Sender: TObject);
begin
  if pageno<=1 then Exit;
  dec(PageNO);
  PaintPreview;
  UpdateCTRLS;
end;

procedure TdbPrintPreviewForm.PageIncClick(Sender: TObject);
begin
  if PageNO>=Pagecount then Exit;

  inc(PageNO);

  PaintPreview;

  UpdateCTRLS;
end;

procedure TdbPrintPreviewForm.PaintPreview;
begin
  with Image.Canvas do
  begin
    Brush.Style:= bsClear;
    Brush.Color:= clWhite;
  end;
end;

procedure TdbPrintPreviewForm.ExitButtonClick(Sender: TObject);
begin
  Close;
  self.print;
end;

procedure TdbPrintPreviewForm.BtnOptionsClick(Sender: TObject);
begin
  {
  with TSetupPanelForm.Create(self) do
  try
     DataFontDialog.Font:=DSP.DataFont;

     case dsp.DataHeaderAlignment of
       taLeftJustify: HeaderLeftAlign.Down:=true;
       taRightJustify: HeaderRightAlign.Down:=true;
       taCenter: HeaderCenterAlign.Down:=true;
     end;

     EditFitToheader.Checked:=dsp.FitCellsByHeader;

     CheckNPagesToFit.Checked:=not (dsp.NPagesToFit=0);
     if CheckNPagesToFit.Checked then NPagesToFit.AsInteger:=dsp.NPagesToFit;

     if dsp.PageOrientation=poPortrait then PageOrientation.ItemIndex:=0
     else PageOrientation.ItemIndex:=1;
//     PageOrientation.itemIndex:=ord(PageOrientation);

     if dsp.Restartdata then REstartDAta.ItemIndex:=0
     else RestartdAta.ItemIndex:=1;

     HorzLines.Checked := ddoHorzGridLines in dsp.rpDataOptions;
     VertLines.Checked := ddoVertGridLines in dsp.rpDataOptions;


     case dsp.rptitleAlignment of
       taLeftJustify: titleLeft.Down:=true;
       taRightJustify: titleRight.Down:=true;
       taCenter: titleCenter.Down:=true;
     end;

     TitleEdit.Lines:=dsp.rpPageHeader;

     WordWrapCells.Checked:=dsp.WordWrapCells;

     if showModal=mrOk then begin
       DSP.DataFont:=DataFontDialog.Font;

       if HeaderLeftAlign.Down
       then dsp.DataHeaderAlignment:=taLeftJustify
       else if HeaderRightAlign.Down
       then dsp.DataHeaderAlignment:=taRightJustify
       else dsp.DataHeaderAlignment:=taCenter;

       dsp.FitCellsByHeader:=EditFitToheader.Checked;

       if not CheckNPagesToFit.Checked
       then dsp.NPagesToFit:=0
       else dsp.NPagesToFit:=NPagesToFit.asInteger;

       dsp.PageOrientation:=TPrinterOrientation(PageOrientation.ItemIndex);


       dsp.Restartdata :=(REstartDAta.ItemIndex=0);

       if HorzLines.Checked
       then
          dsp.rpDataOptions:=dsp.rpDataOptions+[ddoHorzGridLines]
       else
          dsp.rpDataOptions:=dsp.rpDataOptions-
          [ddoHorzGridLines];

       if VertLines.Checked
       then
          dsp.rpDataOptions:=dsp.rpDataOptions+
          [ddoVertGridLines]
       else
          dsp.rpDataOptions:=dsp.rpDataOptions-
          [ddoVertGridLines];

       if TitleLeft.Down
       then dsp.rpTitleAlignment:=taLeftJustify
       else if TitleRight.Down
       then dsp.rpTitleAlignment:=taRightJustify
       else dsp.rptitleAlignment:=taCenter;


       dsp.rpTitle:=TitleEdit.Lines;

       dsp.WordWrapCells:=WordWrapCells.Checked;


       paint;
     end;
  finally
     free;
  end;
  }
end;

procedure TdbPrintPreviewForm.BtnColumnsClick(Sender: TObject);
//var thForm:tColumnsViewForm;
begin
  {
  thForm:=TColumnsViewForm.Create(Self);
  try
    thForm.Grid:=Self.DSP.Grid;
    thForm.DSP:=Self.DSP;
    thForm.ShowModal;
    Paint;
  finally
    thForm.Free;
  end;
  }
end;

procedure TdbPrintPreviewForm.PrintbtnClick(Sender: TObject);
begin
;
end;

end.
