unit ZRPrev;

interface

{$I ZRDefine.inc}

uses Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
     Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ZRPrntr, ZRSearch;

type
  TZRPreviewForm = class(TForm)
    Toolbar: TPanel;
    StatusPanel: TPanel;
    FirstPage: TSpeedButton;
    PrevPage: TSpeedButton;
    PageNo: TSpeedButton;
    NextPage: TSpeedButton;
    LastPage: TSpeedButton;
    Setup: TSpeedButton;
    Print: TSpeedButton;
    Save: TSpeedButton;
    Load: TSpeedButton;
    InfoPanel: TPanel;
    Status: TLabel;
    Preview: TRichEdit;
    ProgressBar: TProgressBar;
    QuitPanel: TPanel;
    Exit: TBitBtn;
    Search: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FirstPageClick(Sender: TObject);
    procedure PrevPageClick(Sender: TObject);
    procedure NextPageClick(Sender: TObject);
    procedure LastPageClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SetupClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PreviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageNoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure PreviewSelectionChange(Sender: TObject);
  private
    fSearch     : String;
    fContinue   : Boolean;
    fmSearch    : TfmSearch;
    fPrinter    : TZRPrinter;
    fPageNumber : Integer;
    fPageCount  : Integer;
    PageIndex   : array of Integer;
    procedure SetPageNumber(Value: Integer);
    procedure SetPrinter(Value: TZRPrinter);
  protected
    procedure UpdateInfo;
    procedure AddPage(Index: Integer);
    procedure UpdateText;
    procedure CMZRProgressUpdate(var Message: TCMZRProgressUpdate); message CM_ZRPROGRESSUPDATE;
    procedure CMZRPageFinished(var Message: TCMZRPageFinished); message CM_ZRPAGEFINISHED;
  public
    constructor CreatePreview(AOwner : TComponent; aPrinter : TZRPrinter); virtual;
    property PageNumber: Integer    read fPageNumber write SetPageNumber;
    property PageCount : Integer    read fPageCount;
    property Printer   : TZRPrinter read fPrinter   write SetPrinter;
  end;

implementation

{$R *.DFM}

uses Printers, ZRConst, ZREscape, ZReport, ZRCtrls;

{ TZRPreviewForm }

constructor TZRPreviewForm.CreatePreview(AOwner : TComponent; aPrinter : TZRPrinter);
begin
  inherited Create(AOwner);
  fPageNumber := 1;
  Printer     := aPrinter;
  WindowState := wsMaximized;
end;

procedure TZRPreviewForm.FormCreate(Sender: TObject);
begin
  Preview.Lines.Clear;
  Status .Caption:= LoadStr(szrProcessing);
  Exit   .Caption:= LoadStr(szrExit);
  Print  .Caption:= LoadStr(szrPrintReport);
  Search .Caption:= LoadStr(szrSearchReport);
  FirstPage .Hint:= LoadStr(szrFirstPageHint);
  PrevPage  .Hint:= LoadStr(szrPrevPageHint);
  NextPage  .Hint:= LoadStr(szrNextPageHint);
  LastPage  .Hint:= LoadStr(szrLastPageHint);
  PageNo    .Hint:= LoadStr(szrPageNoHint);
  Setup     .Hint:= LoadStr(szrPrinterSetup);
  Save      .Hint:= LoadStr(szrSaveReportHint);
  Load      .Hint:= LoadStr(szrLoadReportHint);
  Print     .Hint:= LoadStr(szrPrintReportHint);
  Search    .Hint:= LoadStr(szrSearchHint);
end;

procedure TZRPreviewForm.FormDestroy(Sender: TObject);
begin
  Printer   := nil;
  PageIndex := nil;
end;

procedure TZRPreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TZRPreviewForm.SetPrinter(Value: TZRPrinter);
begin
  if Printer <> Value then begin
    if Assigned(Printer) then Printer.PreviewForm := nil;
    fPrinter := Value;
    if Assigned(Printer) then begin
      Printer.PreviewForm := Self;
      Preview.Font := Printer.Report.Font;
      Preview.DefAttributes.Charset := Preview.Font.Charset;
      Preview.DefAttributes.Name    := Preview.Font.Name;
      Preview.DefAttributes.Pitch   := Preview.Font.Pitch;
      Preview.DefAttributes.Size    := Preview.Font.Size;
      Preview.DefAttributes.Height  := Preview.Font.Height;
      Preview.DefAttributes.Style   := [];
      //Preview.DefAttributes.ConsistentAttributes := [caFace,caSize,];
      Caption      := Printer.Report.Title;
    end;
  end;
end;

procedure TZRPreviewForm.CMZRProgressUpdate(var Message: TCMZRProgressUpdate);
begin
  with Message do begin
    ProgressBar.Position := Position;
    ProgressBar.Visible  := (ProgressBar.Position > 0) and (ProgressBar.Position < 100);
    if ProgressBar.Visible then
      Preview.Cursor := crAppStart
    else
      Preview.Cursor := crDefault;
  end;
end;

procedure TZRPreviewForm.UpdateInfo;
begin
  Status.Caption := Format('%s %d %s %d', [
                      ''{LoadStr(szrPage)}, PageNumber,
                      LoadStr(szrOf),   Printer.PageCount] );
end;

procedure TZRPreviewForm.AddPage(Index: Integer);
var
  T: TZREscapeTokenizer;

  procedure FormatLine(const S: String);
  var
    f: TZRFontStyle;
    l: Integer;
    V: String;
    p: Integer;
    Y: TFontStyles;
  begin
    V := EscapeDeformat(S);
    p := Preview.GetTextLen;
    Preview.SelStart := p;
    Preview.Lines.Add(V);

    T.Line := S;
    while not T.EOL do begin
      l := length(T.Token);
      Preview.SelStart := p;
      Preview.SelLength:= l;
      Inc(p, l);
      Y := [];
      for f := Low(TZRFontStyle) to High(TZRFontStyle) do begin
        if ZRFontStyleToEscapeMap[f] in T.Styles then
          Y := Y + ZRFontStyleMap[f]
        else
          Y := Y - ZRFontStyleMap[f];
      end;
      Preview.SelAttributes.Style := Y;
      T.NextToken;
    end;
  end;

var
  Page : TZRPage;
  C    : TCursor;
  i    : Integer;
  L    : Integer;
  EOP  : String;
begin
  T    := TZREscapeTokenizer.Create;
  Page := Printer.Page[Pred(Index)];
  with Preview do begin
    C := Screen.Cursor;
    {if Self.Visible then }Screen.Cursor := crAppStart;
    try
      Lines.BeginUpdate;
      if Printer.Report.Options.PreviewMode = zpmSinglePage then begin
        Lines.Clear;
      end else begin
        PageIndex[Pred(Index)] := GetTextLen;
        SelStart := GetTextLen;
      end;
      {if Printer.Options.PaperType = zptContinuous then
        L := Page.Strings.Count
      else}
        L := Printer.PageHeight;
      for i := 0 to pred(L) do FormatLine(Page[i]);
      if Printer.Options.PreviewMode = zpmWholeReport then begin
        EOP := Printer.EndOfPage(True);
        if length(EOP) > 0 then Lines.Add(EOP);
      end;
      SelStart := 0;
      SelLength:= 0;
    finally
      Lines.EndUpdate;
      {if Self.Visible then }Screen.Cursor := C;
      T.Free;
    end;
  end;
end;

procedure TZRPreviewForm.CMZRPageFinished(var Message: TCMZRPageFinished);
begin
  UpdateInfo;
  if PageCount < Message.PageCount then begin

    fPageCount := Message.PageCount;
    SetLength(PageIndex, PageCount);

    if (Printer.Report.Options.PreviewMode = zpmSinglePage) then begin
      if (PageNumber = Message.PageCount) then AddPage(PageNumber);
    end else begin
      AddPage(PageCount);
    end;
  end;
end;

procedure TZRPreviewForm.SetPageNumber(Value: Integer);
begin
  if (PageNumber <> Value) and Assigned(Printer) and
     (Value > 0) and (Value <= Printer.PageCount) then begin
    fPageNumber := Value;
    UpdateText;
    UpdateInfo;
  end{ else
    MessageBeep(Cardinal(-1))};
end;

procedure TZRPreviewForm.UpdateText;
begin
  if (Printer.Report.Options.PreviewMode = zpmSinglePage) then
    AddPage(PageNumber)
  else begin
    Preview.Lines.BeginUpdate;
    Preview.SelStart := PageIndex[Pred(PageNumber)];
    if PageNumber = PageCount then
      Preview.SelLength:= Preview.GetTextLen - Preview.SelStart
    else
      Preview.SelLength:= PageIndex[PageNumber] - Preview.SelStart;
    Preview.SelLength:= 0;
    Preview.Lines.EndUpdate;
  end;
end;

procedure TZRPreviewForm.FirstPageClick(Sender: TObject);
begin
  PageNumber := 1;
end;

procedure TZRPreviewForm.PrevPageClick(Sender: TObject);
begin
  PageNumber := PageNumber - 1;
end;

procedure TZRPreviewForm.PageNoClick(Sender: TObject);
var
  S : String;
begin
  S := InputBox(LoadStr(szrGotoPageNo), '', IntToStr(PageNumber));
  PageNumber := StrToIntDef(S, PageNumber);
end;

procedure TZRPreviewForm.NextPageClick(Sender: TObject);
begin
  PageNumber := PageNumber + 1;
end;

procedure TZRPreviewForm.LastPageClick(Sender: TObject);
begin
  PageNumber := PageCount;
end;

procedure TZRPreviewForm.PrintClick(Sender: TObject);
begin
  with Printer do if Setup then Print;
end;

procedure TZRPreviewForm.ExitClick(Sender: TObject);
begin
  if Assigned(Printer) and (Printer.Status = zpsBusy) then Printer.Cancel else Close;
end;

procedure TZRPreviewForm.SaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(Application) do
    try
      Filter   := LoadStr(szrFileFilter);
      Options  := Options + [ofOverwritePrompt,ofPathMustExist];
      //FileName := Printer.Report.Options.FileName;
      FileName := Printer.Report.FileName;
      if Execute then Printer.SaveToFile(FileName);
    finally
      Free;
    end;
end;

procedure TZRPreviewForm.SetupClick(Sender: TObject);
begin
  Printer.Setup;
end;

procedure TZRPreviewForm.LoadClick(Sender: TObject);
begin
{
  with TOpenDialog.Create(Application) do
    try
      if Execute then
        if FileExists(FileName) then
          begin
            ZRPrinter.Load(FileName);
            ZRPreview.PageNumber := 1;
            ZRPreview.PreviewImage.PageNumber := 1;
            UpdateInfo;
          end
        else
          ShowMessage(LoadStr(SzrFileNotExist));
    finally
      free;
    end;
}
end;

procedure TZRPreviewForm.SearchClick(Sender: TObject);

(*
  procedure DoSearch;
  var
    Pos   : Integer;
    Start : Integer;
  begin
    if fSearch <> '' then with Preview do begin
      Start := SelStart;
      if fContinue then Inc(Start, SelLength);
      Pos := FindText(fSearch, Start, GetTextLen-Start, []);
      if Pos <> -1 then begin
        SelStart := Pos;
        SelLength:= length(fSearch);
      end;
    end;
  end;
*)

  function GotoXY(x,y:Integer):integer;
  var
    i, pos: integer;
  begin
    pos := 0;
    for i:= 0 to y - 1 do
      begin
        pos := pos + length(Preview.Lines[i]) + 2;
      end;
    pos := pos + x;
    result := pos ;
  end;

  procedure DoSearchDlg;
  var
    s         : string;
    ignCase   : boolean;
    fromCur   : boolean;
    isCont    : boolean;
    frPage,frLine,frPos : Integer;
  begin
    if not fContinue then
      begin
        if not Assigned(fmSearch) then fmSearch := TfmSearch.Create(Self);
        case fmSearch.ShowModal of
          mrOK:
            begin
              s       := fmSearch.edSearchStr.Text;
              ignCase := not fmSearch.cbUseCase.Checked;
              fromCur := fmSearch.cbFromCurrent.Checked;
              if fromCur then
                begin
                  frPage := PageNumber-1;
                  frLine := Preview.CaretPos.y;
                  frPos  := Preview.CaretPos.x+1;
                end
              else
                begin
                  frPage := 0;
                  frLine := 0;
                  frPos  := 1;
                end;
              isCont  := false;
            end;
          mrRetry:
            begin
              isCont  := true;
            end;
          mrCancel:
            begin
               System.exit;
            end;
        end;
      end
    else
      isCont := true;

    if isCont then
      begin
        s := fSearch;
        Printer.ContinueSearch;
      end
    else
      begin
        Printer.StartSearch(s,ignCase,frPage,frLine,frPos);
      end;

    if Printer.Found then
      begin
        PageNumber         := Printer.FoundPage+1;
        Preview.SelStart   := GotoXY(Printer.FoundPos-1,Printer.FoundLine);
        Preview.SelLength  := length(s);
      end
    else
      begin
        ShowMessageFmt(LoadStr(szrSearchNotFound),[s]);
      end;
    fSearch := s;
  end;

{var
  S : String;}
begin
  DoSearchDlg;
(*
  if fContinue then
    S := fSearch
  else
    S := InputBox(LoadStr(szrSearchReport), '', fSearch);
  if fContinue or (S <> fSearch) then begin
    fSearch := S;
    DoSearch;
  end;
*)
end;

procedure TZRPreviewForm.PreviewSelectionChange(Sender: TObject);
var
  s : Integer;
  i : Integer;
begin
  if Assigned(Printer) and (Printer.Report.Options.PreviewMode = zpmWholeReport) then begin
    s := Preview.SelStart;
    i := 0;
    while (i < PageCount) and (PageIndex[i] <= s) do Inc(i);
    fPageNumber := i;
    UpdateInfo;
  end;
end;

procedure TZRPreviewForm.PreviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    $50{VK_P}: if Shift=[ssCtrl] then
                 begin
                   Print.Click;
                   Key := 0;
                 end;
    VK_Next  : if Printer.Report.Options.PreviewMode=zpmSinglePage then
                 if Shift=[] then
                   begin
                     NextPage.Click;
                     Key:= 0;
                   end else
                 if Shift=[ssCtrl] then
                   begin
                     LastPage.Click;
                     Key:= 0;
                   end;
    VK_Prior : if Printer.Report.Options.PreviewMode=zpmSinglePage then
                 if Shift=[] then
                   begin
                     PrevPage.Click;
                     Key:= 0;
                   end else
                 if Shift=[ssCtrl] then
                   begin
                     FirstPage.Click;
                     Key:= 0;
                   end;
    VK_Up    : if Printer.Report.Options.PreviewMode=zpmSinglePage then
                 if (Shift=[]) and (PageNumber > 1) and (Preview.SelStart = 0) then
                   begin
                     PrevPage.Click;
                     Preview.SelStart := Preview.GetTextLen;
                     Preview.SelLength:= 0;
                     Key:= 0;
                   end;
    VK_Down  : if Printer.Report.Options.PreviewMode=zpmSinglePage then
                 if (Shift=[]) and (Preview.SelStart + Preview.SelLength = Preview.GetTextLen) then
                   begin
                     NextPage.Click;
                     Key:= 0;
                   end;
    VK_Escape: if (Shift=[]) then begin
                 Exit.Click;
                 Key:= 0;
               end;
    VK_F2    : if (Shift=[]) then begin
                 Save.Click;
                 Key := 0;
               end;
    $53{VK_S}: if (ssCtrl in Shift) then begin
                 Save.Click;
                 Key := 0;
               end;
    VK_F7    : if (Shift - [ssShift] = []) then begin
                 fContinue := (ssShift in Shift);
                 Search.Click;
                 Key := 0;
               end;
    $46{VK_F}: if (ssCtrl in Shift) then begin
                 fContinue := False;
                 Search.Click;
                 Key := 0;
               end;
  end;
end;

end.

