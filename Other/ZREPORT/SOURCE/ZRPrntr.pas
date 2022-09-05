unit ZRPrntr;

interface

{$I ZRDefine.inc}

uses
  Windows, Messages, WinSpool,                     // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes, Controls, Graphics, Printers,           // Delphi VCL
  ZRStream, ZREscape;                              // ZReport

const
  CM_ZRPROGRESSUPDATE = WM_USER + 0;
  CM_ZRPAGEFINISHED   = WM_USER + 1;

type
  TZRPrinter = class;

  { TZReportOptions }

  TZRDestination = (zrdFile, zrdPrinter);
  TZRPaperType   = (zptSheet, zptFolio, zptContinuous);
  TZRPreviewMode = (zpmSinglePage, zpmWholeReport);

  TZRPrinterFontStyle = (zfsPica, zfsElite);
  TZRPrinterFont = class(TPersistent)
  private
    fStyle     : TZRPrinterFontStyle;
    fCondensed : Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Style     : TZRPrinterFontStyle read fStyle write fStyle default zfsPica;
    property Condensed : Boolean read fCondensed write fCondensed default False;
  end;

  TZReportOptions = class(TPersistent)
  private
    fDestination : TZRDestination;
    fEscapes     : TZREscapes;
    fFileName    : String;
    fFont        : TZRPrinterFont;
    fOEMConvert  : Boolean;
    fPageFrom,
    fPageTo      : Integer;
    fCopies      : Integer;
    fPaperType   : TZRPaperType;
    fPreviewMode : TZRPreviewMode;
    fIgnoreStyles: Boolean;
    fScissors    : Boolean;
    fFirstPageHeader: Boolean;
    fLastPageFooter : Boolean;
    procedure SetFont(Value: TZRPrinterFont);
    procedure SetEscapes(const Value: TZREscapes);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Copies      : Integer read fCopies write fCopies default 1;
    property Destination : TZRDestination read fDestination write fDestination default zrdPrinter;
    property Escapes     : TZREscapes read fEscapes write SetEscapes;
    property FileName    : String read fFileName write fFileName;
    property Font        : TZRPrinterFont read fFont write SetFont;
    property IgnoreStyles: Boolean read fIgnoreStyles write fIgnoreStyles default False;
    property OEMConvert  : Boolean read fOEMConvert write fOEMConvert default True;
    property PageFrom    : Integer read fPageFrom write fPageFrom default 0;
    property PageTo      : Integer read fPageTo   write fPageTo   default 0;
    property PaperType   : TZRPaperType read fPaperType write fPaperType default zptFolio;
    property PreviewMode : TZRPreviewMode read fPreviewMode write fPreviewMode default zpmSinglePage;
    property Scissors    : Boolean read fScissors write fScissors default True;
    property FirstPageHeader : Boolean read fFirstPageHeader write fFirstPageHeader default True;
    property LastPageFooter  : Boolean read fLastPageFooter  write fLastPageFooter  default True;
  end;

  { TZRPage }
  TZRPage = class(TObject)
  private
    fOwner   : TZRPrinter;
    fStrings : TStringList;
    function  GetCount : Integer;
    function  GetLine(Index: Integer): String;
    procedure SetLine(Index: Integer; const Value: String);
  protected
    property Owner: TZRPrinter read fOwner;
  public
    constructor Create(aOwner: TZRPrinter);
    destructor  Destroy; override;
    procedure BeginPage;
    procedure SaveToStream(Stream: TZStringStream);
    property Count : Integer read GetCount;
    property Line[Index: Integer]: String read GetLine write SetLine; default;
    property Strings: TStringList read fStrings;
  end;

  { IZReport }
  IZReport = interface
    function  GetOptions: TZReportOptions;
    procedure SetOptions(Value: TZReportOptions);
    function  GetTitle: String;
    procedure SetTitle(Value: String);
    function  GetFont: TFont;
    procedure SetFont(Value: TFont);

    function  GetFileName  : String;
    function  GetPageWidth : Integer;
    function  GetPageHeight: Integer;

    function  GetPrinter: TZRPrinter;
    procedure SetPrinter(Value: TZRPrinter);
    function  IsPrinting   : Boolean;

    procedure Preview;
    procedure Print;

    procedure PrintStart;
    procedure PrintFinish;

    property Font      : TFont           read GetFont    write SetFont;
    property Options   : TZReportOptions read GetOptions write SetOptions;
    property Title     : String          read GetTitle   write SetTitle;
    property FileName  : String     read GetFileName;
    property Printer   : TZRPrinter read GetPrinter;
    property PageWidth : Integer    read GetPageWidth;
    property PageHeight: Integer    read GetPageHeight;
  end;

  { TZRPrinter }

  TZRPrinterStatus = (zpsReady, zpsBusy, zpsFinished, zpsCancelled);

  TZRPrinter = class
  private
    fOwner       : IZReport;
    fPages       : TList;
    fPageCount   : Integer;
    fStatus      : TZRPrinterStatus;
    fCurrentX,
    fCurrentY    : Integer;
    fProgress    : Integer;
    fPreviewForm : TWinControl;
    fProgressForm: TWinControl;

    function GetPageCountX(Y: Integer) : Integer;
    function GetPageCountY : Integer;
    function  GetPage(Index: Integer): TZRPage;
    function  GetLine(Index: Integer): String;
    procedure SetLine(Index: Integer; const Value: String);
    procedure CheckStatus(Value: TZRPrinterStatus);

    function GetOptions: TZReportOptions;
    function GetPageWidth  : Integer;
    function GetPageHeight : Integer;

    procedure SetProgress(const Value: Integer);
  protected
    procedure Cleanup;
    procedure SaveToStream(Stream: TStream);

    function Scissors: String;

    property Pages: TList read fPages;
    property CurrentX : Integer read fCurrentX;
    property CurrentY : Integer read fCurrentY;
  public
    constructor Create(aOwner: IZReport);
    destructor  Destroy; override;

    procedure AbortDoc;
    procedure BeginDoc;
    procedure EndDoc;
    procedure Cancel;
    function  Cancelled: Boolean;
    procedure BeginPageX;
    procedure BeginPageY;
    procedure EndPage;

    procedure Finished;
    procedure Print;
    procedure Preview;
    procedure SaveToFile(FileName: String);
    procedure SaveToPrinter(Printer: TPrinter);
    function  Setup: Boolean;

    function InPreview  : Boolean;
    function EndOfPage(Preview: Boolean) : String;

    property Options: TZReportOptions read GetOptions;
    property PageWidth  : Integer read GetPageWidth;
    property PageHeight : Integer read GetPageHeight;

    property PreviewForm : TWinControl read fPreviewForm  write fPreviewForm;
    property ProgressForm: TWinControl read fProgressForm write fProgressForm;
    property Progress : Integer     read fProgress  write SetProgress stored False;
    property Report   : IZReport    read fOwner;

    property Page[Index: Integer]: TZRPage read GetPage;
    property Line[Index: Integer]: String read GetLine write SetLine;
    property PageCountX[Y: Integer] : Integer read GetPageCountX;
    property PageCountY : Integer read GetPageCountY;
    property PageCount  : Integer read fPageCount;
    property Status: TZRPrinterStatus read fStatus;

  private
    FFound          : boolean;
    fSearchString   : string;
    FFoundPage      : Integer;
    FFoundLine      : Integer;
    FFoundPos       : Integer;
    fIgnoreCase     : boolean;
    fSearchStarted  : boolean;
    procedure DoSearch;
  public
    procedure StartSearch(What      : string;
                          aIgnCase  : boolean = true;  // Ignore Case
                          aFromPage : integer = 0;     // От страницы (С 0)
                          aFromLine : integer = 0;     // От линии (С 0)
                          aFromPos  : integer = 1      // от Позиции (С 1)
                          );
    procedure ContinueSearch;
    property  Found          : boolean read FFound;
    property  FoundPage      : Integer read FFoundPage;
    property  FoundLine      : Integer read FFoundLine;
    property  FoundPos       : Integer read FFoundPos;
  end;

type
  TCMZRProgressUpdate = packed record
    Msg       : Cardinal;
    Position  : Longint;
    Sender    : TZRPrinter;
    Result    : Longint;
  end;

  TCMZRPageFinished = packed record
    Msg       : Cardinal;
    PageCount : Longint;
    Sender    : TZRPrinter;
    Result    : Longint;
  end;

implementation

uses
  Dialogs, Forms,
  ZRConst, ZRUtils, ZRStrUtl, ZReport, ZRPrev,
  agPrnDlg;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZReportOptions                          !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRPrinterFont.Create;
begin
  inherited;
  fStyle     := zfsPica;
  fCondensed := False;
end;

procedure TZRPrinterFont.Assign(Source: TPersistent);
begin
  if Source is TZRPrinterFont then begin
    Style     := TZRPrinterFont(Source).Style;
    Condensed := TZRPrinterFont(Source).Condensed;
  end else
    inherited;
end;

constructor TZReportOptions.Create;
begin
  inherited;
  fCopies     := 1;
  fDestination:= zrdPrinter;
  fFont       := TZRPrinterFont.Create;
  fEscapes    := TZREscapes.Create;
  fOEMConvert := True;
  fPaperType  := zptFolio;
  fPreviewMode:= zpmSinglePage;
  fScissors   := True;
  fFirstPageHeader := True;
  fLastPageFooter  := True;
end;

destructor TZReportOptions.Destroy;
begin
  fEscapes.Free;
  fFont.Free;
  inherited;
end;

procedure TZReportOptions.SetFont(Value: TZRPrinterFont);
begin
  fFont.Assign(Value);
end;

procedure TZReportOptions.SetEscapes(const Value: TZREscapes);
begin
  fEscapes.Assign(Value);
end;

procedure TZReportOptions.Assign(Source: TPersistent);
begin
  if Source is TZReportOptions then begin
    Font        := TZReportOptions(Source).Font;
    Escapes     := TZReportOptions(Source).Escapes;
    Copies      := TZReportOptions(Source).Copies;
    Destination := TZReportOptions(Source).Destination;
    FileName    := TZReportOptions(Source).FileName;
    IgnoreStyles:= TZReportOptions(Source).IgnoreStyles;
    OEMConvert  := TZReportOptions(Source).OEMConvert;
    PageFrom    := TZReportOptions(Source).PageFrom;
    PageTo      := TZReportOptions(Source).PageTo;
    PaperType   := TZReportOptions(Source).PaperType;
    PreviewMode := TZReportOptions(Source).PreviewMode;
    FirstPageHeader := FirstPageHeader;
    LastPageFooter  := LastPageFooter;
  end else
    inherited;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                              TZRPrinterStream                           !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

type
  TZRPrinterStream = class(TStream)
  private
    fPrinter: TPrinter;
    fHandle : THandle;
    fTitle  : String;
    procedure CreateHandle;
    procedure FreeHandle;
  public
    constructor Create(aPrinter: TPrinter; aTitle: String);
    destructor  Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Handle: THandle read fHandle;
  end;

constructor TZRPrinterStream.Create(aPrinter: TPrinter; aTitle: String);
begin
  inherited Create;
  fPrinter:= aPrinter;
  fTitle  := aTitle;
  CreateHandle;
end;

destructor TZRPrinterStream.Destroy;
begin
  FreeHandle;
  inherited;
end;

procedure TZRPrinterStream.FreeHandle;
begin
  if fHandle <> 0 then begin
    EndPagePrinter(fHandle);
    EndDocPrinter(fHandle);
    ClosePrinter(Handle);
    fHandle:= 0;
  end;
end;

procedure TZRPrinterStream.CreateHandle;
type
  DOC_INFO_1 = packed record
    pDocName   : PChar;
    pOutputFile: PChar;
    pDataType  : PChar;
  end;
const
  DocInfo: DOC_INFO_1 = (
    pDocName   : nil;
    pOutputFile: nil;
    pDataType  : 'RAW');
var
  aDevice,
  aDriver,
  aPort   : array[0..255] of Char;
  aMode   : Cardinal;
begin
  FreeHandle;
  if fHandle = 0 then begin
    fPrinter.GetPrinter(aDevice, aDriver, aPort, aMode);
    if OpenPrinter(aDevice, fHandle, nil) then begin
      DocInfo.pDocName   := PChar(fTitle);
      if StartDocPrinter(fHandle, 1, @DocInfo) = 0 then begin
        ClosePrinter(fHandle);
        fHandle:= 0;
      end else
      if not StartPagePrinter(fHandle) then begin
        EndDocPrinter(fHandle);
        ClosePrinter(fHandle);
        fHandle:= 0;
      end;
    end;
  end;
end;

function TZRPrinterStream.Write(const Buffer; Count: Integer): Longint;
var
  Bytes: Cardinal;
begin
  WritePrinter(Handle, @Buffer, Count, Bytes);
  Result:= Bytes;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                  TZRPage                               !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRPage.Create(aOwner: TZRPrinter);
begin
  inherited Create;
  fStrings := TStringList.Create;
  fOwner   := aOwner;
  Owner.fCurrentY := Owner.Pages.Add(Self);
  Owner.fCurrentX := -1;
  BeginPage;
end;

destructor TZRPage.Destroy;
begin
  Dec(Owner.fPageCount, Count);
  Strings.Free;
  inherited;
end;

function TZRPage.GetCount : Integer;
begin
  Result := succ(pred(Strings.Count) div Owner.PageHeight);
end;

function TZRPage.GetLine(Index: Integer): String;
{
var
  i : Integer;
}
begin
  Result := Strings[(Count-1) * Owner.PageHeight + Index];
{
  i := (Count-1) * Owner.PageHeight + Index;
  if (Owner.Options.PaperType = zptContinuous) and (i >= Strings.Count) then
    Result := ''
  else
  Result := Strings[i];
}
end;

procedure TZRPage.SetLine(Index: Integer; const Value: String);
{
var
  i : Integer;
}
begin
  Strings[(Count-1) * Owner.PageHeight + Index] := Value;
{
  i := (Count-1) * Owner.PageHeight + Index;
  if (Owner.Options.PaperType = zptContinuous) then
    while i >= Strings.Count do Strings.Add('');
  Strings[i] := Value;
}
end;

procedure TZRPage.BeginPage;
var
  i : Integer;
begin
  //if (Owner.Options.PaperType <> zptContinuous) then
    for i:= 0 to Owner.PageHeight-1 do Strings.Add('');
  Inc(Owner.fPageCount);
  Owner.fCurrentY := Owner.Pages.IndexOf(Self);
  Owner.fCurrentX := Count - 1;
end;

procedure TZRPage.SaveToStream(Stream: TZStringStream);
var
  Buffer: array[0..1023] of Char;
  Break : Boolean;
  i     : Integer;
begin
  Break := True;
  if (Owner.Pages.Last = Self) or (Owner.Options.PaperType = zptContinuous) then begin
    i := Strings.Count-1;
    while (i >= 0) and (Trim(Strings[i]) = '') do begin
      Strings.Delete(i);
      Dec(i);
      Break := False;
    end;
    if (Owner.Options.PaperType = zptContinuous) then
      while (Strings.Count > 0) and (Trim(Strings[0]) = '') do Strings.Delete(0);
  end;
  for i:= 0 to Strings.Count-1 do Stream.WriteString(Strings[i]);
  if Break then begin
    FillChar(Buffer, SizeOf(Buffer), 0);
    StrCopy(Buffer, PChar(Owner.EndOfPage(False)));
    Stream.Write(Buffer, StrLen(Buffer));       //Stream.WriteString(fOwner.EndOfPage);
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZRPrinter                              !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRPrinter.Create(aOwner: IZReport);
begin
  inherited Create;
  fOwner := aOwner;
  fPages := TList.Create;
  Cleanup;
  Report.SetPrinter(Self);
end;

destructor TZRPrinter.Destroy;
begin
  Cleanup;
  Report.SetPrinter(nil);
  inherited;
  fPages.Free;
end;

procedure TZRPrinter.CheckStatus(Value: TZRPrinterStatus);
const
  MessageMap: array[TZRPrinterStatus] of Integer = (
    szrPrinterNotReady, szrPrinterNotBusy, szrPrinterNotFinished, -1 );
begin
  if Status <> Value then ZRError(MessageMap[Value], []);
end;

function TZRPrinter.GetLine(Index: Integer): String;
var
  Page: TZRPage;
begin
  if {(Options.PaperType <> zptContinuous) and}
     (Index < 0) or (Index >= PageHeight) then ZRError(szrPrinterPageIndex, [CurrentY, CurrentX, Index]);
  Page := TZRPage(fPages[CurrentY]);
  Result := Page[Index];
end;

procedure TZRPrinter.SetLine(Index: Integer; const Value: String);
var
  Page: TZRPage;
begin
  if {(Options.PaperType <> zptContinuous) and}
     (Index < 0) or (Index >= PageHeight) then ZRError(szrPrinterPageIndex, [CurrentY, CurrentX, Index]);
  Page := TZRPage(fPages[CurrentY]);
  Page[Index] := Value;
end;

function TZRPrinter.GetPageCountX(Y: Integer) : Integer;
begin
  Result := TZRPage(fPages[Y]).Count;
end;

function TZRPrinter.GetPageCountY : Integer;
begin
  Result := fPages.Count;
end;

function TZRPrinter.GetPage(Index: Integer): TZRPage;
begin
  Result := TZRPage(fPages[Index]);
end;

procedure TZRPrinter.Cancel;
begin
  fStatus:= zpsCancelled;
  if Assigned(PreviewForm ) then TCustomForm(PreviewForm ).Close;
  if Assigned(ProgressForm) then TCustomForm(ProgressForm).Close;
end;

procedure TZRPrinter.Cleanup;
var
  p : TZRPage;
begin
  Cancel;
  while PageCountY > 0 do begin
    p := TZRPage(fPages.Last);
    fPages.Remove(p);
    p.Free;
  end;
  fPageCount :=  0;
  fCurrentY  := -1;
  fCurrentX  := -1;
  fStatus    := zpsReady;
end;

procedure TZRPrinter.BeginDoc;
begin
  CheckStatus(zpsReady);
  fStatus  := zpsBusy;
  Progress := 0;
end;

procedure TZRPrinter.EndDoc;
begin
  EndPage;
  fStatus := zpsFinished;
  Options.PageFrom := 1;
  Options.PageTo   := PageCount;
  Progress := 100;
end;

procedure TZRPrinter.AbortDoc;
begin
  Cancel;
end;

function TZRPrinter.Cancelled: Boolean;
begin
  Result := Status = zpsCancelled;
end;

procedure TZRPrinter.BeginPageX;
begin
  CheckStatus(zpsBusy);
  if PageCount = 0 then
    BeginPageY
  else
    TZRPage(fPages[fCurrentY]).BeginPage;
end;

procedure TZRPrinter.BeginPageY;
begin
  CheckStatus(zpsBusy);
  if PageCount > 0 then EndPage;
  TZRPage.Create(Self);
end;

procedure TZRPrinter.EndPage;
begin
  CheckStatus(zpsBusy);
  if Assigned(ProgressForm) then
    ProgressForm.Perform(CM_ZRPAGEFINISHED, PageCount, Integer(Self));
  if Assigned(PreviewForm) then
    PreviewForm .Perform(CM_ZRPAGEFINISHED, PageCount, Integer(Self));
end;

function TZRPrinter.Scissors: String;
begin
  if Options.Scissors then
    Result := ':<' + Replicate(' -', (Report.PageWidth - 2) div 2) + #13
  else
    Result := '';
end;

function TZRPrinter.EndOfPage(Preview: Boolean): String;
begin
  case Options.PaperType of
    zptFolio : Result := Scissors;
    zptSheet : if Preview then Result := '' else Result := EscapeFormat('', [ecFormFeed], []);
    else       Result := '';
  end;
end;

procedure TZRPrinter.SetProgress(const Value: Integer);
begin
  fProgress := Value;
  if Assigned(ProgressForm) then
    ProgressForm.Perform(CM_ZRPROGRESSUPDATE, Progress, Integer(Self));
  if Assigned(PreviewForm) then
    PreviewForm .Perform(CM_ZRPROGRESSUPDATE, Progress, Integer(Self));
end;

procedure TZRPrinter.SaveToStream(Stream: TStream);

var
  EscapeStream : TZREscapeStream;
  i, j         : Integer;

  function InitCodes: TZREscapeCodes;
  const
    CondensedMap : array[Boolean] of TZREscapeCode = (ecCondensedOff, ecCondensedOn);
    StyleMap     : array[TZRPrinterFontStyle] of TZREscapeCode = (ecPica, ecElite);
  begin
    Result :=  [ CondensedMap[Options.Font.Condensed], StyleMap[Options.Font.Style] ];
  end;

  procedure StreamStartReport;
  var
    S: String;
  begin
    S := EscapeFormat('', [ecReset, ecReportStart] + InitCodes, []);
    EscapeStream.WriteString(S);
  end;

  procedure StreamStartPage(IsFirst: Boolean);
  var
    S: String;
  begin
    if IsFirst or (Options.PaperType <> zptContinuous) then begin
      S := EscapeFormat('', [ecPageStart] + InitCodes, []);
      EscapeStream.WriteString(S);
    end;
  end;

  procedure StreamFinishPage(IsLast : Boolean);
  var
    S: String;
  begin
    if IsLast or (Options.PaperType <> zptContinuous) then begin
      S := EscapeFormat('', [ecPageFinish], []);
      EscapeStream.WriteString(S);
    end;
  end;

  procedure StreamFinishReport;
  var
    S: String;
  begin
    S := EscapeFormat('', [ecReportFinish], []);
    EscapeStream.WriteString(S);
  end;


var
  StartPage,
  EndPage : Integer;
begin
  CheckStatus(zpsFinished);

  StartPage := Options.PageFrom;
  EndPage   := Options.PageTo;

  if (StartPage > EndPage  ) or
     (StartPage < 0        ) or
     (EndPage   > PageCount) then ZRError(szrIllegalPageRange, [StartPage, EndPage]);

  if EndPage < StartPage then begin
    i := EndPage;  EndPage := StartPage;  StartPage := i;
  end;
  if StartPage < 1         then StartPage := 1;
  if EndPage   > PageCount then EndPage   := PageCount;

  try
    EscapeStream := TZREscapeStream.Create(Stream, Options.Escapes.Values);
    EscapeStream.OEMConvert:= Options.OEMConvert;
    for i := 1 to Options.Copies do
      try
        StreamStartReport;
        for j := StartPage-1 to EndPage-1 do begin
          StreamStartPage(j = StartPage-1);
          TZRPage(fPages[j]).SaveToStream(EscapeStream);
          StreamFinishPage(j = EndPage-1);
        end;
      finally
        StreamFinishReport;
      end;
  finally
    EscapeStream.Free;
  end;
end;

procedure TZRPrinter.SaveToFile(FileName: String);
var
  Stream  : TStream;
begin
  CheckStatus(zpsFinished);
  if not ValidFileName(FileName) then
    ZRError(szrInvalidFileName, [FileName]);
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TZRPrinter.GetOptions: TZReportOptions;
begin
  Result := Report.Options;
end;

function TZRPrinter.GetPageWidth: Integer;
begin
  Result := Report.GetPageWidth;
end;
function TZRPrinter.GetPageHeight: Integer;
begin
  Result := Report.GetPageHeight;
end;

procedure TZRPrinter.SaveToPrinter(Printer: TPrinter);
var
  Stream  : TStream;
begin
  CheckStatus(zpsFinished);
  Stream:= TZRPrinterStream.Create(Printer, Report.Title);
  try
    Report.PrintStart;
    SaveToStream(Stream);
    Report.PrintFinish;
  finally
    Stream.Free;
  end;
end;

procedure TZRPrinter.Print;
begin
  case Options.Destination of
    zrdFile   : SaveToFile(Options.FileName);
    zrdPrinter: SaveToPrinter(Printer);
  end;
end;

function TZRPrinter.InPreview : Boolean;
begin
  Result := Assigned(PreviewForm);
end;

procedure TZRPrinter.Finished;
begin
  if Assigned(ProgressForm) then ProgressForm.Free;
  if not Cancelled and Assigned(PreviewForm) then PreviewForm.Show;
end;

procedure TZRPrinter.Preview;
begin
  TZRPreviewForm.CreatePreview(Application, Self);
  if Assigned(PreviewForm ) then PreviewForm .Visible := (Options.PreviewMode =  zpmSinglePage);
  if Assigned(ProgressForm) then ProgressForm.Visible := (Options.PreviewMode <> zpmSinglePage);
end;

function TZRPrinter.Setup: Boolean;
begin
  with TfmPrinterSetup.Create(Application) do
    try

      with FormPrintCommonDialog do begin
        Options  := [poPrintToFile,poPageNums,poWarning];
        MinPage  := 1;
        MaxPage  := PageCount;
        Copies   := Self.Options.Copies;
        Collate  := True;
        FromPage := Self.Options.PageFrom;
        ToPage   := Self.Options.PageTo;
        if (FromPage > MinPage) or
           (ToPage   < MaxPage) then
          PrintRange := prPageNums
        else
          PrintRange := prAllPages;
        PrintToFile := Self.Options.Destination = zrdFile;
      end;

      cbConvertToOEM.Checked     := Self.Options.OEMConvert;
      cbIgnoreFontStyles.Checked := Self.Options.IgnoreStyles;
      cbEscapeModel.ItemIndex    := Integer(Self.Options.Escapes.Model);

      Result := FormPrintCommonDialog.Execute;

      if Result then begin
        with FormPrintCommonDialog do begin
          Self.Options.PageFrom   := FromPage;
          Self.Options.PageTo     := ToPage;
          Self.Options.Copies     := Copies;
        end;
        Self.Options.OEMConvert    := cbConvertToOEM.Checked;
        Self.Options.IgnoreStyles  := cbIgnoreFontStyles.Checked;
        Self.Options.Escapes.Model := TZREscapeModel(cbEscapeModel.ItemIndex);

        if FormPrintCommonDialog.PrintToFile then begin
          Self.Options.Destination := zrdFile;
          with TSaveDialog.Create(Application) do
            try
              Filter   := LoadStr(szrFileFilter);
              Options  := Options + [ofOverwritePrompt,ofPathMustExist];
              //FileName := Self.Options.FileName;
              FileName := Self.Report.FileName;
              Result   := Execute;
              if Result then Self.Options.FileName := FileName;
            finally
              Free;
            end;
        end else
          Self.Options.Destination := zrdPrinter;
      end;

    finally
      Free;
    end;
(*
  with TPrintDialog.Create(Application) do
    try
      Options := [poPrintToFile,poPageNums,poWarning];
      MinPage := 1;
      MaxPage := PageCount;
      FromPage:= Self.Options.PageFrom;
      ToPage  := Self.Options.PageTo;
      if (FromPage > MinPage) or
         (ToPage   < MaxPage) then
        PrintRange := prPageNums
      else
        PrintRange := prAllPages;
      Copies  := Self.Options.Copies;
      Collate := True;
      PrintToFile := Self.Options.Destination = zrdFile;
      Result := Execute;
      if Result then begin
        Self.Options.PageFrom := FromPage;
        Self.Options.PageTo   := ToPage;
        Self.Options.Copies   := Copies;
        if PrintToFile then begin
          Self.Options.Destination := zrdFile;
          with TSaveDialog.Create(Application) do
            try
              Filter   := LoadStr(szrFileFilter);
              Options  := Options + [ofOverwritePrompt,ofPathMustExist];
              //FileName := Self.Options.FileName;
              FileName := Self.Report.FileName;
              Result := Execute;
              if Result then Self.Options.FileName := FileName;
            finally
              Free;
            end;
        end else
          Self.Options.Destination := zrdPrinter;
      end;
    finally
      Free;
    end;
*)
end;

// Добавления от AlGo
procedure TZRPrinter.ContinueSearch;
begin
  if not fSearchStarted then exit;
  if not Found then
    exit;
  if fSearchString ='' then
    exit;
  FFound := false;
  inc (FFoundPos,length(fSearchString));
  DoSearch;
end;

procedure TZRPrinter.StartSearch( What      : string;
                                  aIgnCase  : boolean = true;  // Ignore Case
                                  aFromPage : integer = 0; // От страницы (С 0)
                                  aFromLine : integer = 0; // От линии (С 0)
                                  aFromPos  : integer = 1   // от Позиции (С 1)
                                  );
begin
  fSearchString  := what;
  fIgnoreCase    := aIgnCase;
  fFoundPage     := aFromPage;
  fFoundLine     := aFromLine;
  fFoundPos      := aFromPos;
  fSearchStarted := true;
  DoSearch;
end;

procedure TZRPrinter.DoSearch;
var
  i,j,k  : Integer;
  fr     : integer;
  page   : TZRPage;
  line   : String;
  sLine  : string;
  found  : boolean;
  toFind : string;
  ps     : integer;
begin
  found  := false;
  // do Igore case
  if fIgnoreCase then
    toFind := AnsiUpperCase(fSearchString)
  else
    toFind := fSearchString;

  for i := FFoundPage to pagecount-1 do
    begin
      page := Self.Page[i];
      if i = FFoundPage then
        fr := FFoundLine
      else
        fr := 0;
      for j := fr to Report.GetPageHeight-1 do
        begin
          line := EscapeDeformat(page.Line[j]);
          if (i = fFoundPage) and (j = fFoundLine) then
            begin
              line := copy(line,FoundPos,length(line));
            end;
            if fIgnoreCase then
              line := AnsiUpperCase(line);
            ps   := AnsiPos(toFind,line);
            if ps  <> 0 then
              begin
                // Ну вот и нашли вхождение!!!
                FFound     := true;
                if (i = fFoundPage) and (j = fFoundLine) then
                  begin
                    FFoundPos  := ps + FoundPos - 1;
                  end
                else
                  FFoundPos  := ps;
                FFoundPage := i;
                FFoundLine := j;
                exit;
              end;
        end;
    end;
  FFound := false;
end;

end.

