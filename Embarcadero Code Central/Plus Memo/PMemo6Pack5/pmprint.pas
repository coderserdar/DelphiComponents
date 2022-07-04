unit pmprint;

{ Copyright (c) Electro-Concept Mauricie, 1999-2004 }

{ TPlusMemoPrinter accessory component: see PlusMemo help file for usage
  See at end of file for history }

{$IFDEF VER150} {$DEFINE D7New} {$ENDIF}
{$IFDEF VER170} {$DEFINE D7New} {$ENDIF}

{$IFDEF D7New}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$DEFINE pmprint}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PlusMemo, PMSupport;

type
  TPlusMemoPrinter = class(TComponent, IpmsNotify)
    private
      { Fields corresponding to properties }
      fPlusMemo                 : TPlusMemo;
      fLeftMargin, fRightMargin,   { in inches }
      fTopMargin, fBottomMargin,
      fLineSpacing, fGutterWidth,
      fHeaderYPos, fFooterYPos  : Single;
      fColumns                  : Integer;

      fHeaderAlign, fFooterAlign: TAlignment;
      fPlainText, fWordWrap     : Boolean;
      fLineNumbers              : Boolean;
      fParagraphNumbers         : Boolean;
      fStartingPageNumber       : Integer;
      fStartingLineNumber       : Integer;
      fHeader, fFooter          : string;
      FHeaderFont, FFooterFont  : TFont;
      fPreviewTitle             : string;

      fPageCount, fCurrentPage  : Integer;
      fLinesPerPage             : Integer;
      fOnProgress               : TNotifyEvent;
      fOnPreview                : TNotifyEvent;
      fOnFormatPrint            : TNotifyEvent;

      { Internal working fields }
      fPrintAborted             : Boolean;
      fBufferMemo               : TPlusMemo;  { used as a copy of fPlusMemo to do the formatting and printing }
      fPreviewForm              : TForm;
      fBufMemoFormatted         : Boolean;
      fSelection                : Boolean;    { True if fBufferMemo contains only the selected part of MemoToPrint }
      fStartingParNumber        : Integer;    { contains value of SelStartNav.ParNumber, affects number display if WordWrap is False }
      fSavedLastCar             : Char;
      fPageBreaks               : TList;      { list of line numbers starting with a page break }
      fPageBreakLines           : TList;      { list of number of lines added for each page break }
      fTotalPageBreakLines      : Integer;    { total number of lines added due to page breaks }
      fPagesCalculated          : Boolean;    { indicates if page break locations are calculated }

      fLineHeight,
      fXSize, fYSize,
      fPageWidth, fPageHeight,
      fXStart, fYStart,
      fXPPI, fYPPI              : Integer;    { in pixels }

      procedure setPlusMemo(pm: TPlusMemo);
      procedure setLeftMargin(lm: Single);
      procedure setRightMargin(rm: Single);
      procedure setTopMargin(tm: Single);
      procedure setBottomMargin(bm: Single);
      procedure setLineSpacing(ls: Single);
      procedure setPlainText(pt: Boolean);
      procedure SetFooterFont(const Value: TFont);
      procedure SetHeaderFont(const Value: TFont);
      procedure SetColumns(Value: Integer);
      procedure SetGutterWidth(Value: Single);
      procedure SetWordWrap(Value: Boolean);

      procedure CleanupBufferMemo;
      procedure CalcPageBoundaries;
      procedure RefreshPreview;
      procedure Notify(Sender: TComponent; Events: TpmEvents);

    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure AbortPrint;
      procedure DrawLinesSpecial(sCanvas, bufCanvas: TCanvas; fl, ll: Longint;
                                 sLeft, sTop, LineSpace, sZoom: Single);
      procedure DrawHeaderFooter(sCanvas, bufCanvas: TCanvas; PageNo: Integer; sZoom: Single; UseOffsets, sHeader: Boolean);

      procedure FormatPrint;
      procedure Preview;
      procedure Print;
      procedure PrintFromDialog(PrintDialog: TPrintDialog);
      function  PrintLineToMemoLine(Line: Longint): Longint;
      procedure PrintRange(StartPage, StopPage: Integer);
      procedure PrintSelection;

      property PageCount   : Integer read fPageCount;
      property LinesPerPage: Integer read fLinesPerPage;
      property BufferMemo  : TPlusMemo read fBufferMemo;
      property CurrentPage : Integer read fCurrentPage;

      property PageHeight  : Integer read fPageHeight;
      property PageWidth   : Integer read fPageWidth;
      property PageXStart  : Integer read fXStart;
      property PageYStart  : Integer read fYStart;
      property PrintWidth  : Integer read fXSize;
      property PrintHeight : Integer read fYSize;
      property YPPI        : Integer read fYPPI;
      property XPPI        : Integer read fXPPI;
      property LineHeight  : Integer read fLineHeight;

    published
      { Published declarations }
      property MemoToPrint : TPlusMemo read fPlusMemo write setPlusMemo;
      property Columns     : Integer read fColumns write SetColumns default 1;
      property GutterWidth : Single read fGutterWidth write SetGutterWidth;

      property MarginLeft  : Single read fLeftMargin   write setLeftMargin;
      property MarginRight : Single read fRightMargin  write setRightMargin;
      property MarginTop   : Single read fTopMargin    write setTopMargin;
      property MarginBottom: Single read fBottomMargin write setBottomMargin;

      property LineNumbers : Boolean read fLineNumbers write fLineNumbers default False;
      property ParagraphNumbers: Boolean read fParagraphNumbers write fParagraphNumbers default False;

      property LineSpacing : Single read fLineSpacing write setLineSpacing;

      property Header      : string read fHeader      write fHeader;
      property Footer      : string read fFooter      write fFooter;

      property HeaderYPos  : Single read fHeaderYPos  write fHeaderYPos;
      property FooterYPos  : Single read fFooterYPos  write fFooterYPos;

      property HeaderAlign : TAlignment read fHeaderAlign write fHeaderAlign default taLeftJustify;
      property FooterAlign : TAlignment read fFooterAlign write fFooterAlign default taLeftJustify;

      property HeaderFont  : TFont read FHeaderFont write SetHeaderFont;
      property FooterFont  : TFont read FFooterFont write SetFooterFont;

      property PlainText   : Boolean read fPlainText    write setPlainText default False;
      property PreviewTitle: string  read fPreviewTitle write fPreviewTitle;
      property WordWrap    : Boolean read fWordWrap     write SetWordWrap default True;
      property StartingLineNumber: Integer read fStartingLineNumber write fStartingLineNumber default 1;
      property StartingPageNumber: Integer read fStartingPageNumber write fStartingPageNumber default 1;

      property OnFormatPrint: TNotifyEvent read fOnFormatPrint write fOnFormatPrint;
      property OnProgress   : TNotifyEvent read fOnProgress write fOnProgress;
      property OnPreview    : TNotifyEvent read fOnPreview  write fOnPreview;

    end;

procedure Register;

implementation

uses StdCtrls, Printers, pmpprev;

const AlignToFlag: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

function AttrToExtFontStyles(Memo: TPlusMemo; StaticAttrib: TFontStyles; DynAttrib: Word): TFontStyles;
var newattr: Byte; fontstyle: TFontStyles;
begin
  if DynAttrib and $80<>0 then
    if DynAttrib and $18=$18 then Byte(Result):= DynAttrib and $a7
                             else Byte(Result):= DynAttrib and $3f

  else
    begin
      fontstyle:= Memo.Font.Style;
      newattr:= Byte(StaticAttrib) xor Byte(fontstyle);
      Result:= TFontStyles(newattr)
    end
end;


constructor TPlusMemoPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPageBreaks:= TList.Create;
  fPageBreakLines:= TList.Create;
  fLeftMargin:= 0.75;
  fRightMargin:= 0.5;
  fTopMargin:= 0.75;
  fBottomMargin:= 0.75;
  fLineSpacing:= - 1.5;
  fHeaderYPos:= 0.4;
  fFooterYPos:= 0.4;
  fHeaderFont:= TFont.Create;
  fFooterFont:= TFont.Create;
  fWordWrap:= True;
  fStartingPageNumber:= 1;
  fStartingLineNumber:= 1;
  fColumns:= 1;
  fGutterWidth:= 0.5
end;

destructor TPlusMemoPrinter.Destroy;
begin
  CleanupBufferMemo;
  fPageBreaks.Free;
  fPageBreakLines.Free;
  fHeaderFont.Free;
  fFooterFont.Free;
  fPlusMemo:= nil;
  inherited Destroy
end;

procedure TPlusMemoPrinter.setPlusMemo(pm: TPlusMemo);
begin
  if pm=fPlusMemo then Exit;
  CleanupBufferMemo;
  fPlusMemo:= pm;
  if pm<>nil then pm.FreeNotification(Self);
end;

procedure TPlusMemoPrinter.setLeftMargin(lm: Single);
begin
  if lm<>fLeftMargin then
    begin
      fLeftMargin:= lm;
      fBufMemoFormatted:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.setRightMargin(rm: Single);
begin
  if rm<>fRightMargin then
    begin
      fRightMargin:= rm;
      fBufMemoFormatted:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.setTopMargin(tm: Single);
begin
  if tm<>fTopMargin then
    begin
      fTopMargin:= tm;
      fPagesCalculated:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.setBottomMargin(bm: Single);
begin
  if bm<>fBottomMargin then
    begin
      fBottomMargin:= bm;
      fPagesCalculated:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.setLineSpacing(ls: Single);
begin
  if fLineSpacing<>ls then
    begin
      fLineSpacing:= ls;
      fPagesCalculated:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
end;

procedure TPlusMemoPrinter.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TPlusMemoPrinter.setPlainText(pt: Boolean);
begin
  if pt<>fPlainText then
    begin
      fPlainText:= pt;
      CleanupBufferMemo;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.SetColumns(Value: Integer);
begin
  if Value<1 then Value:= 1;
  if Value<>fColumns then
    begin
      fColumns:= Value;
      fBufMemoFormatted:= False;
      RefreshPreview
    end
end;

procedure TPlusMemoPrinter.SetGutterWidth(Value: Single);
begin
  if Value<>fGutterWidth then
    begin
      fGutterWidth:= Value;
      if fColumns>1 then
        begin
          fBufMemoFormatted:= False;
          RefreshPreview
        end
    end
end;

procedure TPlusMemoPrinter.SetWordWrap(Value: Boolean);
begin
  if Value<>fWordWrap then
    begin
      fWordWrap:= Value;
      fBufMemoFormatted:= False;
      RefreshPreview
    end
end;


procedure TPlusMemoPrinter.CleanupBufferMemo;
var i: Integer; snotify: IpmsNotify;
begin
  fPageCount:= 0;
  if fBufferMemo=nil then Exit;
  fBufMemoFormatted:= False;
  if fPlusMemo<>nil then
    begin    // remove notification
      snotify:= Self;
      i:= fPlusMemo.NotifyList.IndexOf(Pointer(snotify));
      if i>=0 then fPlusMemo.NotifyList[i]:= nil
    end;
  FreeAndNil(fBufferMemo);
end;

procedure ClearFPUExceptions;
  // clears pending FPU exceptions.
  // useful on wrong "invalid floating point operation" exceptions
  asm
    FNCLEX
  end;


procedure TPlusMemoPrinter.CalcPageBoundaries;
var i, j, add, linespacingpix: Integer;
begin
  ClearFPUExceptions;
  if (fBufferMemo<>nil) and fBufMemoFormatted then
    begin
      if fLineSpacing<0 then linespacingpix:= -Round(fLineHeight*fLineSpacing)
                        else linespacingpix:= Round(fLineSpacing*fYPPI);

      if linespacingpix=0 then linespacingpix:=1;

      fLinesPerPage:= Trunc((fPageHeight-(MarginTop+MarginBottom)*fYPPI)/linespacingpix);
      if fLinesPerPage<=0 then fLinesPerPage:= 1;

      { add lines to make the page breaks }
      fTotalPageBreakLines:= 0;
      for i:= 0 to fPageBreaks.Count-1 do
        begin
          j:= Integer(fPageBreaks[i]) + fTotalPageBreakLines;
          add:= fLinesPerPage - j mod fLinesPerPage;
          Inc(fTotalPageBreakLines, add);
          fPageBreakLines[i]:= Pointer(add)
        end;
      fPageCount:= ((fBufferMemo.LineCount + fTotalPageBreakLines) div fLinesPerPage) div fColumns + 1;
      fPagesCalculated:= True
    end
end;


procedure TPlusMemoPrinter.RefreshPreview;
begin
  if Assigned(fPreviewForm) then
    begin
      FormatPrint;
      with TfrmPMPrintPreview(fPreviewForm) do
          if CurrentPage>=PageCount then CurrentPage:= PageCount-1
                                    else CurrentPage:= CurrentPage;
    end
end;

procedure TPlusMemoPrinter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (Assigned(fPlusMemo)) and (AComponent = fPlusMemo) then
    begin
      fPlusMemo:= nil;
      CleanupBufferMemo
    end
end;

procedure TPlusMemoPrinter.Notify(Sender: TComponent; Events: TpmEvents);
begin
  if (pmeChange in Events) and (Sender=fPlusMemo) then CleanupBufferMemo
end;

procedure TPlusMemoPrinter.AbortPrint;
begin
  fPrintAborted:= True
end;

procedure TPlusMemoPrinter.Preview;
begin
  Printer.Handle;  // just check for the presence of a default printer
  fPreviewForm:= TFrmPMPrintPreview.Create(nil);
  try
    fPreviewForm.Caption:= fPreviewTitle;
    TfrmPMPrintPreview(fPreviewForm).PMPrinter:= Self;
    if Assigned(fOnPreview) then OnPreview(fPreviewForm);
    fPreviewForm.ShowModal;
    fPreviewForm.Free
  finally
      fPreviewForm:= nil
    end
end;

procedure TPlusMemoPrinter.Print;
begin
  if (fBufferMemo<>nil) and (fBufMemoFormatted) and (fSelection) then CleanupBufferMemo;
  fSelection:= False;
  FormatPrint;
  PrintRange(0, fPageCount-1)
end;

procedure TPlusMemoPrinter.PrintFromDialog(PrintDialog: TPrintDialog);
begin
  if (fBufferMemo<>nil) and (fBufMemoFormatted) and (fSelection) then CleanupBufferMemo;
  fSelection:= False;
  case PrintDialog.PrintRange of
    prAllPages: Print;
    prPageNums: PrintRange(PrintDialog.FromPage-1, PrintDialog.ToPage-1);
    prSelection: PrintSelection
    end
end;

procedure TPlusMemoPrinter.PrintSelection;
begin
  if (fBufferMemo<>nil) and (fBufMemoFormatted) and (not fSelection) then CleanupBufferMemo;
  fSelection:= True;
  try
    FormatPrint;
    PrintRange(0, fPageCount-1);
    CleanupBufferMemo;
    FreeAndNil(fBufferMemo);
  finally
      fSelection:= False
    end
end;

procedure TPlusMemoPrinter.PrintRange(StartPage, StopPage: Integer);
var
  pagenb, j       : Integer;
  linespacingpix  : Single;
  xstart, xinc, y : Integer;

begin      { PrintRange }
  ClearFPUExceptions;
  fPrintAborted:= False;
  FormatPrint;
  Printer.Title:= Application.Title;
  Printer.BeginDoc;

  try
    { determine the line spacing in pixels }
    if LineSpacing<0 then
      begin
        Printer.Canvas.Font:= fBufferMemo.Font;
        linespacingpix:= -Printer.Canvas.TextHeight('A')*LineSpacing
      end
    else linespacingpix:= LineSpacing*YPPI;

    if linespacingpix=0 then linespacingpix:=1;

    y:= Round(MarginTop*YPPI) - fYStart;
    xstart:= Round(MarginLeft*XPPI)-fXStart;
    xinc:= ((fPageWidth- xstart - Round(MarginRight*XPPI)-fXStart)) div fColumns;

    for pagenb:= StartPage to StopPage do
      begin
        fCurrentPage:= pagenb;
        if Assigned(Self.fOnProgress) then Self.OnProgress(Self);
        if fPrintAborted then Break;

        if pagenb>=fPageCount then Continue;
        if pagenb>StartPage then Printer.NewPage;
        if Header<>'' then DrawHeaderFooter(Printer.Canvas, nil, pagenb, 1, True, True);
        if Footer<>'' then DrawHeaderFooter(Printer.Canvas, nil, pagenb, 1, True, False);

        for j:=0 to fColumns-1 do
          begin
            DrawLinesSpecial(Printer.Canvas, nil, j*LinesPerPage+(fCurrentPage*LinesPerPage)*fColumns,
                             (j+1)*LinesPerPage+(fCurrentPage*LinesPerPage)*fColumns-1, xstart+j*xinc, y, linespacingpix, 1)
          end;
      end;

  finally
    Printer.EndDoc;
    fCurrentPage:= -1;
    if Assigned(Self.fOnProgress) then Self.OnProgress(Self);
  end;
end;  { PrintRange }


procedure TPlusMemoPrinter.FormatPrint;
var
  sBufCanvas  : TCanvas;   { a canvas whose handle is the printer device context }
  i, j        : Integer;
  sp, spb     : pParInfo;
  spars       : TParagraphsList;
  pstart, pstop: Integer;
  srunoffset  : Integer;
  oldum       : TpmUpdateMode;
  snotify     : IpmsNotify;

begin
  if fBufferMemo=nil then
    begin
      fBufMemoFormatted:= False;
      fBufferMemo:= TPlusMemo.Create(nil);

      if MemoToPrint<>nil then
        begin
          with MemoToPrint do
            begin
              fBufferMemo.Alignment         := Alignment;
              fBufferMemo.AltFont           := AltFont;
              fBufferMemo.Delimiters        := Delimiters;
              fBufferMemo.Justified         := Justified;
              fBufferMemo.Font              := Font;
              fBufferMemo.LeftMargin        := 0;
              fBufferMemo.RightMargin       := 0;
              fBufferMemo.TabStops          := TabStops;
              fBufferMemo.UpdateMode        := umOnNeed;
              fBufferMemo.Visible           := False;
              fBufferMemo.WordWrap          := fWordWrap;
              fBufferMemo.ScrollBars        := ssNone;
              fBufferMemo.StaticFormat      := StaticFormat;
              oldum                         := UpdateMode;
              UpdateMode                    := umImmediate
            end;

          if fSelection then
            begin
              pstart:= MemoToPrint.SelStartNav.ParNumber;
              pstop:= MemoToPrint.SelStopNav.ParNumber;
            end
          else
            begin
              pstart:= 0;
              pstop:= MemoToPrint.ParagraphCount-1
            end;

          fStartingParNumber:= pstart;
          spars:= fBufferMemo.IParList;
          spars.Count:= pstop-pstart+1;
          srunoffset:= 0;
          sp:= nil;  // to avoid a warning
          for i:= 0 to spars.Count-1 do
            begin
              spb:= spars.Pointers[i];
              sp:=  MemoToPrint.IParList.Pointers[i+pstart];
              spb^.ParText:= sp^.ParText;
              spb^.StartOffset:= srunoffset;
              spb^.StartLine:= i;
              spb^.ParState:= [pmpKeywDone, pmpSSDone];
              SetParLength(spb^, GetParLength(sp^));
              SetStartDynAttrib(spb^, GetStartDynAttrib(sp^), True);
              Inc(srunoffset, GetParLength(spb^)+2);
              if fSelection and (i=0) then Dec(srunoffset, MemoToPrint.SelStartNav.ParOffset);
              if not fPlainText then
                begin
                  SetDynCount(spb^, GetDynCount(sp^));
                  for j:= 0 to GetDynCount(sp^)-1 do spb^.ParExtra.DynCodes[j]:= sp^.ParExtra.DynCodes[j]
                end
            end;

          spars.fTrueLineCount:= spars.Count;
          spars.fVisibleLineCount:= spars.Count;

          if fSelection then
            begin    // trim off starting and ending paragraphs
              spb:= spars.Pointers[0];    
              Inc(spb.ParText, MemoToPrint.SelStartNav.ParOffset);
              SetParLength(spb^, GetParLength(spb^)- MemoToPrint.SelStartNav.ParOffset);
              SetStartAttrib(spb^, MemoToPrint.SelStartNav.Style);
              if (not fPlainText) and (MemoToPrint.SelStartNav.ParOffset>0) then
                begin
                  if MemoToPrint.SelStartNav.DynNb>0 then
                    begin
                      for j:= MemoToPrint.SelStartNav.DynNb to GetDynCount(spb^)-1 do
                          spb^.ParExtra.DynCodes[j-MemoToPrint.SelStartNav.fDynNb]:= sp^.ParExtra.DynCodes[j];
                      SetDyncount(spb^, GetDynCount(spb^)-MemoToPrint.SelStartNav.fDynNb);
                      SetStartDynAttrib(spb^, @sp^.ParExtra.DynCodes[MemoToPrint.SelStartNav.fDynNb-1], True)
                    end;
                  for i:= 0 to GetDynCount(spb^)-1 do Dec(spb.ParExtra.DynCodes[i].DynOffset, MemoToPrint.SelStartNav.fOffset);
                end;
              spb:= spars.Pointers[spars.Count-1];
              Dec(srunoffset, GetParLength(spb^) - MemoToPrint.SelStopNav.ParOffset);
              SetParLength(spb^, MemoToPrint.SelStopNav.ParOffset);
              if spb.ParText<>nil then
                begin
                  fSavedLastCar:= spb.ParText[GetParLength(spb^)];
                  spb.ParText[GetParLength(spb^)]:= #0
                end
            end;

          snotify:= Self;
          if MemoToPrint.NotifyList.IndexOf(Pointer(snotify))<0 then MemoToPrint.NotifyList.Add(Pointer(snotify));
          MemoToPrint.UpdateMode:= oldum;
          spars.fTextLen:= srunoffset-2;
          spars.fUpdateStartPar:= 0;
          spars.fUpdateStopPar:= spars.fTrueLineCount-1
        end;      { MemoToPrint<>nil }
    end;  { fBufferMemo=nil }

  if (not fBufMemoFormatted) or
     (fPageWidth<>GetDeviceCaps(Printer.Handle, PHYSICALWIDTH)) or
     (fPageHeight<>GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT)) then
    begin
      { Determine the paragraph location of all page breaks, removing this character at the same time.
        We will transform these values in line numbers after formatting is done }
      fPageBreaks.Clear;
      fPageBreakLines.Clear;
      fTotalPageBreakLines:= 0;
      fPagesCalculated:= False;
      srunoffset:= 0;
      for i:= 0 to fBufferMemo.ParagraphCount-1 do
        begin
          sp:= fBufferMemo.IParList.ParPointers[i];
          Dec(sp.StartOffset, srunoffset);
          if (GetParLength(sp^)>=1) and (sp^.ParText[0]=#12) then
            begin
              fPageBreaks.Add(Pointer(i));
              Inc(sp.ParText);
              SetParLength(sp^, GetParLength(sp^)-1);
              Dec(fBufferMemo.IParList.fTextLen);
              Inc(srunoffset);
              if pmpHasExtra in sp.ParState then Dec(sp.ParExtra.FirstLine.Stop)
            end
        end;
      fPageBreakLines.Count:= fPageBreaks.Count;   // The individual values are initialized in SetLineSpacing

      if Assigned(fOnFormatPrint) then fOnFormatPrint(fBufferMemo);

      { get the page characteristics }
      with Printer do
        begin
          fPageWidth := GetDeviceCaps(Handle, PHYSICALWIDTH);
          fPageHeight:= GetDeviceCaps(Handle, PHYSICALHEIGHT);
          {$IFDEF WIN32}
          fXStart    := GetDeviceCaps(Handle, PHYSICALOFFSETX);
          fYStart    := GetDeviceCaps(Handle, PHYSICALOFFSETY);
          {$ELSE}
          fXStart    := 0;
          fYStart    := 0;
          {$ENDIF}
          fXSize     := GetDeviceCaps(Handle, HORZRES);
          fYSize     := GetDeviceCaps(Handle, VERTRES);
          fXPPI      := GetDeviceCaps(Handle, LOGPIXELSX);
          fYPPI      := GetDeviceCaps(Handle, LOGPIXELSY);

        end;

      { get the printer device context in sBufCanvas.Handle }
      sBufCanvas:= TCanvas.Create;
      sBufCanvas.Handle:= Printer.Handle;
      sBufCanvas.Font.PixelsPerInch:= fYPPI;
      sBufCanvas.Font:= fBufferMemo.Font;
      fLineHeight:= sBufCanvas.TextHeight('A');

      fBufferMemo.OnProgress:= OnFormatPrint;
      fBufferMemo.FormatText(sBufCanvas, (fPageWidth-Round(MarginLeft*fXPPI) - Round(MarginRight*fXPPI) -
                                          (fColumns-1)*Round(fGutterWidth*fXPPI)) div fColumns);

      sBufCanvas.Free;
      fBufMemoFormatted:= True;

      { Change page break paragraph numbers to page numbers }
      for i:= 0 to fPageBreaks.Count-1 do
        begin
          j:= Integer(fPageBreaks[i]);
          j:= pParInfo(fBufferMemo.IParList.Pointers[j]).StartLine;
          fPageBreaks[i]:= Pointer(j)
        end
    end;     { not fBufMemoFormatted }

  if not fPagesCalculated then CalcPageBoundaries
end;    { FormatPrint }

function TPlusMemoPrinter.PrintLineToMemoLine(Line: Longint): Longint;
var i: Integer;
begin
  Result:= Line;
  i:= 0;
  while (i<fPageBreaks.Count) and (Result>=Integer(fPageBreaks[i])) do
    begin
      Dec(Result, Integer(fPageBreakLines[i]));
      if Result<Integer(fPageBreaks[i]) then
        begin
          Result:= High(Result);
          Break
        end;
      Inc(i)
    end
end;

type cwarray = array[Char] of Integer;
     pCharWidth = ^cwarray;

procedure TPlusMemoPrinter.DrawLinesSpecial(sCanvas, bufCanvas: TCanvas; fl, ll: Longint;
                                            sLeft, sTop, LineSpace, sZoom: Single);
var dc: hdc;
    i: Longint;
    j, b: Integer;
    t: PChar;
    tlen: Integer;
    par: ParInfo;
    pardyncount: Integer;
    lin: LineInfo;
    sstyle: TFontStyles;
    currentattr: TExtFontStyles;
    currentdyn : DynInfoRec;
    curdynnb   : Integer;
    logfont:  TLogFont;
    smemo: TPlusMemo;
    charwidths: pCharWidth;

  procedure SetupLogfontStyle(style: TFontStyles);
    var i: Char; f: TFont; bstyle: Byte;  scolor: TColor; h: THandle;
    begin
      if TPlusFontStyle(fsAltFont) in TPlusFontStyles(style) then f:= smemo.AltFont
                                                             else f:= smemo.Font;
      bstyle:= Byte(style);
      bstyle:= bstyle and $0f;
      scolor:= sCanvas.Font.Color;
      sCanvas.Font:= f;
      sCanvas.Font.Style:= TFontStyles(bstyle);
      sCanvas.Font.Color:= scolor;
      if bufCanvas<>nil then
        begin
          bufCanvas.Font:= f;
          bufCanvas.Font.PixelsPerInch:= YPPI;
          if f.Height<0 then bufCanvas.Font.Height:= (f.Height*YPPI) div smemo.Font.PixelsPerInch ;
          bufCanvas.Font.Style:= TFontStyles(bstyle);
          h:= bufCanvas.Handle
        end
      else h:= sCanvas.Handle;
      if not GetCharWidth32(h, 0, 255, charwidths^) then
        for i:= #0 to #255 do charwidths[i]:= 0;
      {$IFDEF PMPrintU}
      for i:= #256 to High(i) do charwidths[i]:= 0;
      {$ENDIF}

      if f.Height>0 then sCanvas.Font.Height:= Round(f.Height*sZoom)
      else
        if bufCanvas<>nil then
            sCanvas.Font.Size:= Round(f.Size*sZoom*bufCanvas.Font.PixelsPerInch/sCanvas.Font.PixelsPerInch)
        else
            sCanvas.Font.Size:= Round(f.Size*sZoom);

      dc:= sCanvas.Handle
    end;

   procedure setColors;
     var t, b: Longint;
     begin
       t:= pmsGetParForegnd(par);
       if (t=-1) or (t=clNone) then t:= smemo.Font.Color;
       b:= -1;  { Transparent }

       if fsHighlight in TPlusFontStyles(sstyle) then
         begin
           t:=smemo.HighlightColor;
           b:=smemo.HighlightBackgnd
         end;

       with currentdyn do
         if (DynStyle and $80 <>0) and ((Foregnd<>0) or (Backgnd<>0)) then
           begin
             if (pmsGetParForegnd(par)=-1) and (Foregnd<>-1) and (Foregnd<>clNone) then t:= Foregnd;
             if (pmsGetParBackgnd(par)=-1) and (Backgnd<>-1) and (Backgnd<>clNone) then b:= Backgnd
           end;

       sCanvas.Font.Color:= t;
       sCanvas.Brush.Color:= b;
       if b<>-1 then sCanvas.Brush.Style:= bsSolid
                else sCanvas.Brush.Style:= bsClear;
       dc:= sCanvas.Handle
     end;

  procedure SetDC(var offset: Integer; maxoffset: Integer);
    var changed: Boolean; ss: TExtFontStyles; sdnb: Integer;
    begin
      changed:= False;
      if fBufferMemo.StaticFormat then
        while (offset<maxoffset) and (t[offset]<=#26) and (AnsiChar(t[offset]) in CtrlCodesSet) do
          begin
            changed:= True;
            XORStyleCode(currentattr, t[offset]);
            Inc(offset)
            end;

      sdnb:= curdynnb;
      while (curdynnb<pardyncount) and (par.ParExtra.DynCodes[curdynnb].DynOffset<=offset) do
          Inc(curdynnb);

      if sdnb<>curdynnb then
        begin
          changed:= True;
          currentdyn:= par.ParExtra.DynCodes[curdynnb-1];
        end;

      if changed then
        begin
          ss:= sstyle;
          sstyle:= AttrToExtFontStyles(smemo, currentattr, currentdyn.DynStyle);
          if (TPlusFontStyles(ss)*[TPlusFontStyle(fsHighlight)]<>
              TPlusFontStyles(sstyle)*[TPlusFontStyle(fsHighlight)]) or (sdnb<>curdynnb) then SetColors;

          if (TPlusFontStyles(ss)-[TPlusFontStyle(fsHighlight)])<>
             (TPlusFontStyles(sstyle)-[TPlusFontStyle(fsHighlight)]) then
                                                             SetupLogFontStyle(sstyle);
          end;
    end;


  procedure getwords(var offset: Integer; maxoffset: Integer);
    var offlim: Integer;
    begin
      if curdynnb<pardyncount then offlim:= par.ParExtra.DynCodes[curdynnb].DynOffset
                              else offlim:= High(offlim);

      if maxoffset>offlim then maxoffset:= offlim;
      if smemo.StaticFormat then
        while (offset<maxoffset) and
              ((t[offset]>#26) or (not (AnsiChar(t[offset]) in (CtrlCodesSet+[#9])))) do Inc(offset)
      else
        while (offset<maxoffset) and (t[offset]<>#9) do Inc(offset)
    end;

type IntArray = array[0..maxint div SizeOf(Integer)-1] of Integer;
     pIntArray = ^IntArray;

var r: TRect; currentpar: Longint;
    tm: TTextMetric;
    runningX,
    sLineHeight, sLineBase: Integer;
    runningerror, breakerror, breakadd,
    newx, xtoreach, rdispx, k, numchar: Integer;
    rline: Longint;
    oldpar: pParInfo;
    pardx: pIntArray;
    leftmarg, enddisp: Integer;
    cw: TSize;
    sline: string;
    ssize: TSize;
    ssaveddc: HDC;
    sbackgnd: TColor;

begin
  ClearFPUExceptions;
  fl:= PrintLineToMemoLine(fl);
  repeat
    rline:= PrintLineToMemoLine(ll);
    if rline=High(ll) then Dec(ll);
  until (ll<0) or (rline<High(ll));
  if ll>=0 then ll:= rline;

  New(charwidths);
  smemo:= fBufferMemo;
  smemo.MouseNav.TrueLineNumber:= fl;
  currentpar:= smemo.MouseNav.ParNumber;

  if fl<smemo.LineCount then
    begin
      sCanvas.Font:= smemo.Font;
      if smemo.Font.Height>0 then sCanvas.Font.Height:= Round(smemo.Font.Height*sZoom)
      else
        if bufCanvas<>nil then
            sCanvas.Font.Size:= Round(smemo.Font.Size*sZoom*bufCanvas.Font.PixelsPerInch/sCanvas.Font.PixelsPerInch)
        else
            sCanvas.Font.Size:= Round(smemo.Font.Size*sZoom);

      dc:= sCanvas.Handle;
      ssaveddc:= SaveDC(dc);
      SetTextAlign(dc, ta_updatecp or ta_baseline);
      GetTextMetrics(dc, tm);
      sLineHeight:= tm.tmHeight+1;
      sLineBase:= sLineHeight - tm.tmDescent-1;
      leftmarg:= Round(sLeft);

      par:= smemo.MouseNav.NavLines.LLPar^;
      pardyncount:= GetDynCount(par);
      lin:= smemo.MouseNav.fNavLines[fl-par.StartLine];
      t:= par.ParText;
      tlen:= GetParLength(par);
      sbackgnd:= pmsGetParBackgnd(par);

      if fLineNumbers or fParagraphNumbers then
        begin
          r.Left:= Round(leftmarg*sZoom);
          if fParagraphNumbers then
            begin
              for i:= fl to ll do
                if i=par.StartLine then
                  begin
                    sline:= IntToStr(fStartingParNumber + fStartingLineNumber + currentpar) + ': ';
                    GetTextExtentPoint32(dc, Pointer(sline), Length(sline), ssize);
                    r.Top:= Round((sTop+(i-fl)*LineSpace)*sZoom);
                    MoveToEx(dc, r.Left-ssize.cx, sLineBase+r.Top, nil);
                    ExtTextOut(dc, 0, 0, 0, @r, Pointer(sline), Length(sline), nil);
                    Inc(currentpar);
                    if currentpar<smemo.IParList.Count then par:= smemo.IParList[currentpar]
                  end
                else
                  while (i<smemo.LineCount) and (i>par.StartLine) do
                    begin
                      Inc(currentpar);
                      if currentpar<smemo.IParList.Count then par:= smemo.IParList[currentpar]
                                                         else Break
                    end;

              // replace local vars used
              par:= smemo.MouseNav.fNavLines.LLPar^;
              currentpar:= smemo.MouseNav.ParNumber
            end

          else   // just fLineNumbers
            for i:= fl to ll do
              if i<smemo.LineCount then
                begin
                  sline:= IntToStr(i+fStartingLineNumber) + ': ';
                  GetTextExtentPoint32(dc, Pointer(sline), Length(sline), ssize);
                  r.Top:= Round((sTop+(i-fl)*LineSpace)*sZoom);
                  MoveToEx(dc, r.Left-ssize.cx, sLineBase+r.Top, nil);
                  ExtTextOut(dc, 0, 0, 0, @r, Pointer(sline), Length(sline), nil)
                end;
        end;

      {set font attributes and starting value in buffer }
      currentattr:= lin.StartAttrib;
      curdynnb:= 0;
      while (curdynnb<pardyncount) and (par.ParExtra.DynCodes[curdynnb].DynOffset<=lin.Start) do Inc(curdynnb);
      if curdynnb=0 then currentdyn:= GetStartDynAttrib(par)^
                    else currentdyn:= par.ParExtra.DynCodes[curdynnb-1];
      sstyle:= AttrToExtFontStyles(smemo, currentattr, currentdyn.DynStyle);
      j:= lin.Start;

      SetUpLogFontStyle(sstyle);
      SetColors;

      for i:= fl to ll do
        if i<smemo.LineCount then
          begin
            runningX:= leftmarg;
            r.Left:= Round(runningX*sZoom);
            rdispX:= r.Left;
            r.Top:= Round((sTop+(i-fl)*LineSpace)*sZoom);
            r.Right:= Round((PageWidth-MarginRight*XPPI));
            r.Bottom:= r.Top+sLineHeight;

            if (sbackgnd<>-1) and (sbackgnd<>clNone) then
              with sCanvas do
                begin
                  smemo.Brush.Assign(Brush);    { use smemo.Brush as temporary store for brush attributes }
                  Brush.Color:= sbackgnd;
                  Brush.Style:= bsSolid;
                  FillRect(r);
                  Brush.Assign(smemo.Brush)
                end;

            if (not smemo.Justified) or (lin.JustifyStart>=lin.Stop) then
              case smemo.Alignment of
                taRightJustify: r.Left:= Round((r.Right - lin.LineWidth)*sZoom);
                taCenter: r.Left:= Round(((leftmarg+r.Right-lin.LineWidth) div 2)*sZoom)
                end;
            MoveToEx(dc, r.Left, sLineBase+r.Top, nil);

            j:= lin.Start;
            if smemo.Justified and (lin.Spaces>0) then
              begin
                runningerror:= (PageWidth-Round((MarginLeft+MarginRight+(Columns-1)*GutterWidth)*XPPI)) div Columns - lin.LineWidth;
                breakadd:= runningerror div lin.Spaces;
                breakerror:= runningerror mod lin.Spaces
              end
            else
              begin
                breakadd:= 0;
                breakerror:= 0
              end;
            runningerror:= breakerror div 2;

            { draw the text }
            repeat
              setDC(j, lin.Stop);
              if j<lin.Stop then
                begin
                  b:= j;
                  getWords(j, lin.Stop);

                  enddisp:= j;
                  if (j=lin.Stop) and (i<par.StartLine+smemo.MouseNav.fNavLines.Count-1) then
                      while (enddisp>b) and (t[enddisp-1]=' ') do Dec(enddisp);

                  numchar:= enddisp-b;
                  if numchar>0 then
                    begin
                      GetMem(pardx, numchar*SizeOf(Integer));
                      for k:=0 to numchar-1 do
                         begin
                           cw.cX:= charwidths[t[b+k]];
                           if cw.cX=0 then
                             begin
                               if bufCanvas<>nil then GetTextExtentPoint32(bufCanvas.Handle, t+b+k, 1, cw)
                                                 else GetTextExtentPoint32(dc, t+b+k, 1, cw);
                               charwidths[t[b+k]]:= cw.cX
                             end;
                           newx:= runningX+cw.cX;
                           if (t[b+k]=' ') and smemo.Justified and (j>lin.JustifyStart) then
                             begin
                               Inc(newx, breakadd);
                               Inc(runningerror, breakerror);
                               if runningerror>=lin.Spaces then
                                 begin
                                   Inc(newx);
                                   runningerror:= runningerror-lin.Spaces
                                 end
                             end;
                           xtoreach:= Round(newx*sZoom);
                           pardx^[k]:= xtoreach-rdispX;
                           rdispX:= xtoreach;
                           runningX:= newx
                         end;

                      ExtTextOut(dc, 0, 0, 0, @r, t+b, numchar, PInteger(pardx));
                      FreeMem(pardx, numchar*SizeOf(Integer))
                    end;

                  if (j<lin.Stop) and (t[j]=#9) then
                    begin
                      cw.cx:= charwidths[Char(Ord(' '))];
                      if cw.cx=0 then
                        begin
                          if bufCanvas<>nil then GetTextExtentPoint32(bufCanvas.Handle, ' ', 1, cw)
                                            else GetTextExtentPoint32(dc, ' ', 1, cw);
                          charwidths[Char(Ord(' '))]:= cw.cX
                        end;
                      runningX:= leftmarg + ((runningX-leftmarg) div
                                             (smemo.TabStops*cw.cx)+1)*(smemo.TabStops*cw.cx);
                      rdispX:= Round(runningX*sZoom);

                      MoveToEx(dc, rdispX, sLineBase+r.Top, nil);
                      Inc(j);
                    end;
                end

            until (j>=lin.Stop) or (j>= tlen);


            { finished for this line, go on with next one }
            if (i>=par.StartLine+smemo.MouseNav.fNavLines.Count-1) and (i<smemo.LineCount-1) then
              begin
                if curdynnb<pardyncount then SetDC(j, lin.Stop);
                Inc(currentpar);
                oldpar:= smemo.MouseNav.fNavLines.LLPar;
                smemo.MouseNav.fNavLines.LLPar:= smemo.IParList.Pointers[currentpar];
                par:= smemo.MouseNav.fNavLines.LLPar^;
                if pmsGetParForegnd(oldpar^)<>pmsGetParForegnd(par) then SetColors;

                t:= par.ParText;
                tlen:= GetParLength(par);
                pardyncount:= GetDynCount(par);
                curdynnb:= 0;
              end;
            if (i<smemo.LineCount-1) and (i<ll) then
              lin:= smemo.MouseNav.fNavLines.Items[i+1-par.StartLine]
          end;

        // restore state before exiting
        RestoreDC(dc, ssaveddc);
        smemo.MouseNav.fNavLines.LLPar:= smemo.MouseNav.fPar
      end;
      
  Dispose(charwidths)
end;      { DrawLinesSpecial }

procedure TPlusMemoPrinter.DrawHeaderFooter(sCanvas, bufCanvas: TCanvas; PageNo: Integer; sZoom: Single;
                                            UseOffsets, sHeader: Boolean);
var r: TRect; s: AnsiString; pagepos: Integer; salign: TAlignment; soffx, soffy: Integer;
begin
  if UseOffsets then
    begin
      soffx:= fXStart;
      soffy:= fYStart
    end
  else
    begin
      soffx:= 0;
      soffy:= 0
    end;

  if sHeader then
    begin
      r.Top:= Round((HeaderYPos*YPPI-soffy)*sZoom);
      r.Bottom:= Round((MarginTop*YPPI-soffy)*sZoom);
      sCanvas.Font:= HeaderFont;
      salign:= HeaderAlign;
      s:= Header
    end
  else
    begin
      r.Top:= Round((PageHeight - FooterYPos*YPPI-soffy)*sZoom);
      r.Bottom:= Round((PageHeight-soffy)*sZoom);
      sCanvas.Font:= FooterFont;
      salign:= FooterAlign;
      s:= Footer
    end;

  if bufCanvas<>nil then
      sCanvas.Font.Size:= Round(sCanvas.Font.Size * sZoom * bufCanvas.Font.PixelsPerInch/sCanvas.Font.PixelsPerInch);
  r.Left:= Round((MarginLeft*XPPI-soffx)*sZoom);
  r.Right:= Round((PageWidth - Round(MarginRight*XPPI) - soffx)*sZoom);

  pagepos:= Pos('{p}', s);
  if pagepos>0 then
      s:= Copy(s, 1, pagepos-1) +
          IntToStr(PageNo+fStartingPageNumber) + Copy(s, pagepos+3, Length(s)-pagepos-2);

  SetTextAlign(sCanvas.Handle, ta_noupdatecp or ta_top);
  DrawText(sCanvas.Handle, PAnsiChar(s), -1, r, AlignToFlag[salign] or DT_WORDBREAK or DT_NOCLIP)
end;


procedure Register;
begin
  RegisterComponents({UCONVERT}'PlusMemo'{/UCONVERT}, [TPlusMemoPrinter]);
end;

end.
