unit pmpprev;

{ Copyright (c) Electro-Concept Mauricie, 1999-2005 }
{ TPlusMemoPrinter accessory component:
    This unit contains the Preview form and display processing

 }

{$IFDEF VER130} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER140} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D5New} {$ENDIF}
{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, pmprint, PlusMemo, ComCtrls;

type
  TPreviewMouseState = (pmsNormal, pmsLeft, pmsRight, pmsTop, pmsBottom);
  TFrmPMPrintPreview = class(TForm)
    Panel1: TPanel;
    BtnClose: TBitBtn;
    LblPage: TLabel;
    sbPreview: TScrollBox;
    PBPreview: TPaintBox;
    cbZoom: TComboBox;
    btnPrinterSetup: TSpeedButton;
    btnPrint: TSpeedButton;
    btnMargins: TSpeedButton;
    btnNextPage: TSpeedButton;
    btnPrevious: TSpeedButton;
    btnNumbers: TSpeedButton;
    udColumns: TUpDown;
    lblColumns: TLabel;
    Label1: TLabel;
    procedure sbPreviewResize(Sender: TObject);
    procedure PBPreviewPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnNextPageClick(Sender: TObject);
    procedure BtnPreviousClick(Sender: TObject);
    procedure BtnPrinterSetupClick(Sender: TObject);
    procedure PBPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PBPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnPrintClick(Sender: TObject);
    procedure BtnMarginsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbZoomChange(Sender: TObject);
    procedure btnNumbersClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }

    { position of margins in the PBPreview paint box }
    fLeftMargin, fRightMargin, fTopMargin, fBottomMargin: Integer;

    fMouseState    :  TPreviewMouseState;
    fLastPos       : TPoint;    { last position where a dragging margin has been drawn }
    fCurrentPage   : Integer;
    fBufCanvas     : TCanvas;   { a canvas attached to a printer device context used when
                                  painting the preview lines in the PBPreview paint box }
    fPrintDialog   : TPrintDialog;  { created as needed, when user presses Print Setup button the first time }

    procedure SetLblPage;       { update the label that indicates the current page number }
    procedure setCurrentPage(cp: Integer);
    procedure PaintMargins;     { paint the dashed margin lines on the PBPreview paint box }

  public
    { Public declarations }
    ZoomFact: Single;           { zoom factor between printer pixels and PBPreview pixels }
    PMPrinter: TPlusMemoPrinter;
    property CurrentPage: Integer read fCurrentPage write setCurrentPage;  { current page in preview }
  end;

var FrmPMPrintPreview: TFrmPMPrintPreview;

implementation

{$R *.DFM}
uses Printers  {$IFNDEF WIN32} , PlusSup  {$ENDIF} ;

procedure ClearFPUExceptions;
  // clears pending FPU exceptions.
  // useful on wrong "invalid floating point operation" exceptions
  asm
    FNCLEX
  end;


procedure TFrmPMPrintPreview.sbPreviewResize(Sender: TObject);
var ph, pw, havail, ppi, nleft: Integer;
begin
  ClearFPUExceptions;
  PMPrinter.FormatPrint;
  { determine the new zoom factor, set the paint box and margins position }
  ph:= PMPrinter.PageHeight;
  pw:= PMPrinter.PageWidth;
  ppi:= PMPrinter.YPPI;
  havail:= sbPreview.ClientHeight;

  try
    case cbZoom.ItemIndex of
      0..5:
          begin
            if PBPreview.Width<sbPreview.ClientWidth-20 then
              nleft:= (sbPreview.ClientWidth-pbPreview.Width) div 2
            else nleft:= 10
          end;
      6:     { fit page width }
          begin
            ZoomFact:= (sbPreview.ClientWidth-20)/pw;
            nleft:= 10
          end;

      else    { fit page }
        if ph/pw>(havail-20)/(sbPreview.ClientWidth-20) then
          begin
            ZoomFact:= (havail-20)/ph;
            nleft:= Round((sbPreview.ClientWidth-(pw*ZoomFact))/2);
          end
        else
          begin
            ZoomFact:= (sbPreview.ClientWidth-20)/pw;
            nleft:= 10;
          end
      end;

    fLeftMargin  := Round(PMPrinter.MarginLeft*ppi*ZoomFact);
    fRightMargin := Round((pw-PMPrinter.MarginRight*ppi)*ZoomFact);
    fTopMargin   := Round(PMPrinter.MarginTop*ppi*ZoomFact);
    fBottomMargin:= Round((ph-PMPrinter.MarginBottom*ppi)*ZoomFact);
    PBPreview.SetBounds(nleft, 10, Round(PMPrinter.PageWidth*ZoomFact), Round(PMPrinter.PageHeight*ZoomFact))

  except
    fLeftMargin  := 0;
    fRightMargin := 0;
    fTopMargin   := 0;
    fBottomMargin:= 0
  end;

end;

procedure TFrmPMPrintPreview.PBPreviewPaint(Sender: TObject);
var clrect: TRect;    { clipping rectangle of PBPreview }
    firstline, lastline, ppi: Integer;
    linespacingpix          : Integer;
    topmarginpix,
    slinespacing            : Single;
    xstart, y, j            : Integer;
    xinc                    : Single;
begin
  ClearFPUExceptions;
  with PMPrinter do
    begin
      //FormatPrint;  // it may not be formatted after changing a view property
      ppi:= YPPI;
      slinespacing:= LineSpacing;
      if sLineSpacing<0 then linespacingpix:= -Round(LineHeight*sLineSpacing)
                        else linespacingpix:=  Round(sLineSpacing*PPI);
      topmarginpix:= MarginTop*ppi
    end;

  if linespacingpix=0 then linespacingpix:=1;

  with PBPreview, Canvas do
    begin
      clrect:= ClipRect;
      FillRect(clrect);
      if BtnMargins.Down then PaintMargins;
      { determine which lines need be painted }
      firstline:= Trunc((clrect.Top/ZoomFact-topmarginpix)/linespacingpix);
      if firstline<0 then firstline:= 0;
      lastline:= Trunc((clrect.Bottom/ZoomFact-topmarginpix)/LineSpacingPix) + 1;
      if lastline>=PMPrinter.LinesPerPage then lastline:= PMPrinter.LinesPerPage-1;

      { clip the unprintable area }
      ExcludeClipRect(Handle, 0, 0, Trunc(PMPrinter.PageXStart*ZoomFact), ClientHeight);
      ExcludeClipRect(Handle, 0, 0, ClientWidth, Trunc(PMPrinter.PageYStart*ZoomFact));
      ExcludeClipRect(Handle, 0, Trunc((PMPrinter.PrintHeight+PMPrinter.PageYStart)*ZoomFact)+1,
                                                 ClientWidth, ClientHeight);

      ExcludeClipRect(Handle, Trunc((PMPrinter.PrintWidth+PMPrinter.PageXStart)*ZoomFact)+1, 0,
                                              ClientWidth, ClientHeight)
    end;

  { get the printer device context, attach fBufCanvas to it }
  fBufCanvas.Handle:= Printer.Handle;
  fBufCanvas.Font.PixelsPerInch:= PMPrinter.YPPI;

  y:= Round(topmarginpix + firstline*LineSpacingPix);
  with PMPrinter do
    begin
      xstart:= Round(MarginLeft*XPPI);
      xinc:= (PageWidth - xstart - Round(MarginRight*XPPI) + Round(GutterWidth*XPPI)) div Columns
    end;

  for j:=0 to PMPrinter.Columns-1 do
    begin
    PMPrinter.DrawLinesSpecial(pbPreview.Canvas,
                               fBufCanvas,
                               PMPrinter.LinesPerPage*(CurrentPage*PMPrinter.Columns)+firstline+PMPrinter.LinesPerPage*j,
                               PMPrinter.LinesPerPage*(CurrentPage*PMPrinter.Columns)+lastline+PMPrinter.LinesPerPage*j,
                               xstart+j*xinc, y, LineSpacingPix, ZoomFact)
    end;

  if (firstline=0) and (PMPrinter.Header<>'') then
      PMPrinter.DrawHeaderFooter(pbPreview.Canvas, fBufCanvas, CurrentPage, ZoomFact, False, True);
  if (lastline>=PMPrinter.LinesPerPage-1) and (PMPrinter.Footer<>'') then
      PMPrinter.DrawHeaderFooter(pbPreview.Canvas, fBufCanvas, CurrentPage, ZoomFact, False, False);

  { finished with the printer device context }
  fBufCanvas.Handle:= 0;
end;     { PBPreviewPaint }

procedure TFrmPMPrintPreview.SetLblPage;
begin
  LblPage.Caption:= 'Page ' + IntToStr(CurrentPage+1) + '/' + IntToStr(PMPrinter.PageCount)
end;

procedure TFrmPMPrintPreview.setCurrentPage(cp: Integer);
begin
  if cp>=PMPrinter.PageCount then cp:= PMPrinter.PageCount-1;
  fCurrentPage:= cp;
  SetLblPage;
  PBPreview.Invalidate;
  BtnNextPage.Enabled:= fCurrentPage<PMPrinter.PageCount-1;
  BtnPrevious.Enabled:= fCurrentPage>0
end;


procedure TFrmPMPrintPreview.FormShow(Sender: TObject);
begin
  PMPrinter.FormatPrint;
  SetLblPage;
  btnNextPage.Enabled:= PMPrinter.PageCount>1;
  btnNumbers.Down:= PMPrinter.LineNumbers;
  udColumns.Position:= PMPrinter.Columns;
  lblColumns.Caption:= IntToStr(PMPrinter.Columns)
end;



procedure TFrmPMPrintPreview.FormCreate(Sender: TObject);
begin
  fBufCanvas:= TCanvas.Create;
  cbZoom.ItemIndex:= 6
end;

procedure TFrmPMPrintPreview.FormDestroy(Sender: TObject);
begin
  fBufCanvas.Free
end;


procedure TFrmPMPrintPreview.BtnNextPageClick(Sender: TObject);
begin
  CurrentPage:= CurrentPage+1
end;

procedure TFrmPMPrintPreview.BtnPreviousClick(Sender: TObject);
begin
  CurrentPage:= CurrentPage-1
end;

procedure TFrmPMPrintPreview.BtnPrinterSetupClick(Sender: TObject);
var oldpw, oldph, oldyppi, oldxppi: Integer; doprint: Boolean;
begin
  oldpw:= PMPrinter.PageWidth;
  oldph:= PMPrinter.PageHeight;
  oldyppi:= PMPrinter.YPPI;
  oldxppi:= PMPrinter.XPPI;

  if fPrintDialog=nil then
    begin
      fPrintDialog:= TPrintDialog.Create(Self);
      fPrintDialog.Options:= [poPageNums, poWarning];
      fPrintDialog.MinPage:= 1;
      fPrintDialog.MaxPage:= PMPrinter.PageCount;
      fPrintDialog.FromPage :=1;
      fPrintDialog.ToPage := PMPrinter.PageCount
    end;

  doprint:= fPrintDialog.Execute;
  if ((oldpw<>GetDeviceCaps(Printer.Handle, PHYSICALWIDTH)) or
      (oldph<>GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT)) or
      (oldyppi<>GetDeviceCaps(Printer.Handle, LOGPIXELSY)) or
      (oldxppi<>GetDeviceCaps(Printer.Handle, LOGPIXELSX))) then
      begin
        PMPrinter.FormatPrint;
        cbZoomChange(Self)  { update the display }
      end;
  if doprint then PMPrinter.PrintFromDialog(fPrintDialog)
end;

function InBounds(i, pos: Integer): Boolean;
begin
  Result:= (i>=pos-2) and (i<=pos+2)
end;


procedure TFrmPMPrintPreview.PBPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not BtnMargins.Down then
    begin
      Cursor:= crDefault;
      Exit
    end;

  with PBPreview.Canvas do
    begin
      { prepare to drag a margin }
      Pen.Color:= clGray;
      Pen.Style:= psDot;
      Pen.Mode:= pmXOR;
      if X<0 then X:= 0;
      if X>ClientWidth-1 then X:= ClientWidth-1;
      if Y<0 then Y:= 0;
      if Y>ClientHeight-1 then Y:= ClientHeight-1;

      case fMouseState of
        pmsNormal: begin
                     if InBounds(X,fLeftMargin) or InBounds(X, fRightMargin) then Cursor:= crHSplit
                     else
                       if InBounds(Y, fTopMargin) or InBounds(Y, fBottomMargin) then Cursor:= crVSplit
                                                                                else Cursor:= crDefault;
                     WinProcs.SetCursor(Screen.Cursors[Cursor])
                   end;

        pmsLeft, pmsRight:
                 begin
                   if fMouseState=pmsLeft then
                     begin
                       if X>fRightMargin-3 then X:= fRightMargin-3
                     end
                   else
                     if X<fLeftMargin+3 then X:= fLeftMargin+3;

                   MoveTo(fLastPos.X, 0);
                   LineTo(fLastPos.X, PBPreview.Height);
                   MoveTo(X, 0);
                   LineTo(X, PBPreview.Height)
                 end;

        pmsTop, pmsBottom:
                 begin
                   if fMouseState=pmsTop then
                     begin
                       if Y>fBottomMargin-3 then Y:= fBottomMargin-3
                     end
                   else
                     if Y<fTopMargin+3 then Y:= fTopMargin+3;
                   MoveTo(0, fLastPos.Y);
                   LineTo(PBPreview.Width, fLastPos.Y);
                   MoveTo(0, Y);
                   LineTo(PBPreview.Width, Y)
                  end;

        end;   { case fMouseState }
      fLastPos.X:= X;
      fLastPos.Y:= Y
    end;    { with PBPreview }
end;  { PBPreviewMouseMove }

procedure TFrmPMPrintPreview.PBPreviewMouseDown(Sender:  TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not BtnMargins.Down then Exit;
  if InBounds(X, fLeftMargin) then fMouseState:= pmsLeft
  else
    if InBounds(X, fRightMargin) then fMouseState:= pmsRight
    else
      if InBounds(Y, fTopMargin) then fMouseState:= pmsTop
      else if InBounds(Y, fBottomMargin) then fMouseState:= pmsBottom;

  with PBPreview.Canvas do
    begin
      Pen.Color:= clGray;
      Pen.Style:= psDot;
      Pen.Mode:= pmXOR;
      fLastPos.X:= X;
      fLastPos.Y:= Y;

      case fMouseState of
        pmsLeft: begin
                   MoveTo(X, 0);
                   LineTo(X, PBPreview.Height)
                 end;
        pmsRight: begin
                    MoveTo(X, 0);
                    LineTo(X, PBPreview.Height)
                  end;
        pmsTop:  begin
                   MoveTo(0, Y);
                   LineTo(PBPreview.Width, Y)
                 end;
        pmsBottom: begin
                     MoveTo(0, Y);
                     LineTo(PBPreview.Width, Y)
                   end
        end
    end
end;

procedure TFrmPMPrintPreview.PBPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ppi       : Integer;
  repaginate: Boolean;

begin
  if fMouseState<>pmsNormal then
    with PBPreview.Canvas do
      begin
        Pen.Color:= clGray;
        Pen.Style:= psDot;
        Pen.Mode:= pmXOR;
        if X<0 then X:= 0;
        if X>PBPreview.Width-1 then X:= PBPreview.Width-1;
        if Y<0 then Y:= 0;
        if Y>PBPreview.Height-1 then Y:= PBPreview.Height-1;

        case fMouseState of
          pmsLeft, pmsRight:
                   begin
                     MoveTo(fLastPos.X, 0);
                     LineTo(fLastPos.X, PBPreview.Height);
                   end;
          pmsTop, pmsBottom:
                   begin
                     MoveTo(0, fLastPos.Y);
                     LineTo(PBPreview.ClientWidth, fLastPos.Y);
                    end;
          end;

        ppi:= PMPrinter.YPPI;

        repaginate:= False;
        case fMouseState of
          pmsLeft: begin
                     if X>fRightMargin-10 then X:= fRightMargin-10;
                     if X<>fLeftMargin then
                       begin
                         PMPrinter.MarginLeft:= X/(ZoomFact*ppi);
                         repaginate:= True
                       end
                     end;
          pmsRight: begin
                     if X<fLeftMargin+10 then X:= fLeftMargin+10;
                     if X<>fRightMargin then
                       begin
                         PMPrinter.MarginRight:= (PBPreview.ClientWidth-X)/(ZoomFact*ppi);
                         repaginate:= True
                       end
                     end;
          pmsTop: begin
                     if Y>fBottomMargin-10 then Y:= fBottomMargin-10;
                     if Y<>fTopMargin then
                       begin
                         PMPrinter.MarginTop:= Y/(ZoomFact*ppi);
                         repaginate:= True
                       end
                     end;

          pmsBottom: begin
                     if Y<fTopMargin+10 then Y:= fTopMargin+10;
                     if Y<>fBottomMargin then
                       begin
                         PMPrinter.MarginBottom:= (PBPreview.ClientHeight-Y)/(ZoomFact*ppi);
                         repaginate:= True
                       end
                     end
          end;   { case fMouseState, updating margins }

        fMouseState:= pmsNormal;
        if repaginate then
          begin
            cbZoomChange(Self);
            //PBPreview.Invalidate;
            //SetLblPage
          end
      end;   { with PBPreview.Canvas }

end;    { PBPreviewMouseUp }

procedure TFrmPMPrintPreview.BtnPrintClick(Sender: TObject);
begin
  PMPrinter.Print;
end;

procedure TFrmPMPrintPreview.BtnMarginsClick(Sender: TObject);
begin
  PBPreview.Invalidate;
  PBPreviewMouseMove(Self, [], 0, 0)  { update the cursor and mouse state if needed }
end;

procedure TFrmPMPrintPreview.PaintMargins;
begin
  with PBPreview, Canvas do
    begin
      Pen.Color:= clBlack;
      Pen.Width:= 1;
      Pen.Style:= psDash;
      Pen.Mode:= pmXOR;
      MoveTo(fLeftMargin, 0);
      LineTo(fLeftMargin, ClientHeight);

      MoveTo(fRightMargin, 0);
      LineTo(fRightMargin, ClientHeight);

      MoveTo(0, fTopMargin);
      LineTo(ClientWidth, fTopMargin);

      MoveTo(0, fBottomMargin);
      LineTo(ClientWidth, fBottomMargin)
    end
end;

procedure TFrmPMPrintPreview.cbZoomChange(Sender: TObject);
begin
  ClearFPUExceptions;
  sbPreview.HorzScrollBar.Position:= 0;
  sbPreview.VertScrollBar.Position:= 0;
  ZoomFact:= Screen.PixelsPerInch/PMPrinter.YPPI;
  case cbZoom.ItemIndex of
    0: ZoomFact:= 5*ZoomFact;
    1: ZoomFact:= 2*ZoomFact;
    2: ZoomFact:= 1.5*ZoomFact;
    4: ZoomFact:= 0.75*ZoomFact;
    5: ZoomFact:= 0.5*ZoomFact;
    {6, 7: ZoomFact will be set in resizing the scroll box }
    end;
  sbPreviewResize(cbZoom)
end;

procedure TFrmPMPrintPreview.btnNumbersClick(Sender: TObject);
begin
  PMPrinter.LineNumbers:= btnNumbers.Down;
  PBPreview.Invalidate
end;

procedure TFrmPMPrintPreview.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
                                            var Handled: Boolean);
var msg: Cardinal; i, code: Integer; sc: TControl;
begin
  sc:= ControlAtPos(ScreenToClient(MousePos), False {$IFDEF D5New}, True {$ENDIF});
  if (sc=sbPreview) or (sc=PBPreview) then
    begin
      Handled := true;
      if ssShift in Shift then msg := WM_HSCROLL
                          else msg := WM_VSCROLL;

      if WheelDelta < 0 then code := SB_LINEDOWN
                        else code := SB_LINEUP;

      for i:= 1 to Mouse.WheelScrollLines do sbPreview.Perform(msg, code, 0);
    end
end;

procedure TFrmPMPrintPreview.udColumnsClick(Sender: TObject;
  Button: TUDBtnType);
begin
  PMPrinter.Columns:= udColumns.Position;
  lblColumns.Caption:= IntToStr(PMPrinter.Columns)
end;

end.

