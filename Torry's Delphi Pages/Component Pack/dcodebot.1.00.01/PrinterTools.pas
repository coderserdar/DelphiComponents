
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit PrinterTools;

interface

{$I STD.INC}

uses
  Classes, SysUtils, Windows, Graphics, WinSpool, Printers, MathTools,
  GraphTools;

{ TAdvancedPrinter class }

type
  TChar = array[0..MAX_PATH] of Char;

  TPaperSize = (psLetter, psLegal, psLedger, psStatement, psExecutive, psA, psB,
    psC, psD, psE, psUser, psOther);

  TFloatCanvas = class(TPersistent);

  TAdvancedPrinter = class(TPrinter)
  private
    FBuffer: TBitmap;
    FCookie: Integer;
    FFilePath: string;
    FHorzMargin: Double;
    FVertMargin: Double;
    FDeviceHandle: THandle;
    FPageNumber: Integer;
    FPapers: TStrings;
    FX: Double;
    FY: Double;
    FOnFooter: TNotifyEvent;
    FOnHeader: TNotifyEvent;
    procedure BitmapChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetHeight: Double;
    function GetPapers: TStrings;
    function GetPaperSize: TPaperSize;
    procedure SetPaperSize(Value: TPaperSize);
    function GetPenWidth: Double;
    procedure SetPenWidth(const Value: Double);
    function GetPhysicalSize: TFloatPoint;
    procedure SetPhysicalSize(const Value: TFloatPoint);
    function GetPrinterIndex: Integer;
    procedure SetPrinterIndex(Value: Integer);
    function GetRaggedLeft: Double;
    function GetRaggedRight: Double;
    function GetWidth: Double;
  protected
    function AllocDevice: PDevMode;
    procedure FreeDevice;
    procedure CheckPrinting(Value: Boolean);
  public
    destructor Destroy; override;
    procedure EndDoc;
    procedure BeginDoc;
    procedure StretchDraw(const Rect: TFloatRect; Graphic: TGraphic);
    procedure LineTo(const X, Y: Double);
    procedure MoveTo(const X, Y: Double);
    procedure Rectangle(const ALeft, ATop, ARight, ABottom: Double); overload;
    procedure Rectangle(const Rect: TFloatRect); overload;
    procedure Polygon(const P: TFloatPolygon);
    procedure NewLine;
    procedure NewPage;
    procedure UpdateFont(Size: Integer; Style: TFontStyles = []);
    function TextWidth(const S: string): Double;
    function TextHeight(const S: string): Double;
    procedure Write(const S: string); overload;
    procedure Write(const S: string; const Width: Double; Direction: TDirection = drLeft); overload;
    function Write(const S: string; const Width, Height: Double): Double; overload;
    procedure WriteLine(const S: string);
    property Canvas: TCanvas read GetCanvas;
    property FilePath: string read FFilePath write FFilePath;
    property Height: Double read GetHeight;
    property HorzMargin: Double read FHorzMargin write FHorzMargin;
    property PageNumber: Integer read FPageNumber write FPageNumber;
    property Papers: TStrings read GetPapers;
    property PaperSize: TPaperSize read GetPaperSize write SetPaperSize;
    property PenWidth: Double read GetPenWidth write SetPenWidth;
    property PhysicalSize: TFloatPoint read GetPhysicalSize write SetPhysicalSize;
    property PrinterIndex: Integer read GetPrinterIndex write SetPrinterIndex;
    property RaggedLeft: Double read GetRaggedLeft;
    property RaggedRight: Double read GetRaggedRight;
    property VertMargin: Double read FVertMargin write FVertMargin;
    property Width: Double read GetWidth;
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property OnFooter: TNotifyEvent read FOnFooter write FOnFooter;
    property OnHeader: TNotifyEvent read FOnHeader write FOnHeader;
  end;

function AdvancedPrinter: TAdvancedPrinter;

procedure PrintRawText(const Printer, Name, Data: string);

implementation

uses
  Consts;

const
  BufferedPages: Integer = 0;

{ TAdvancedPrinter }

type
  TPaperSizeMap = record
    PaperSize: SHORT;
    Width: Double;
    Height: Double;
  end;

const
  PaperSizeMap: array[TPaperSize] of TPaperSizeMap = (
    (PaperSize: DMPAPER_LETTER; Width: 8.5; Height: 11.0),
    (PaperSize: DMPAPER_LEGAL; Width: 8.5; Height: 14.0),
    (PaperSize: DMPAPER_LEDGER; Width: 11.0; Height: 17.0),
    (PaperSize: DMPAPER_STATEMENT; Width: 5.5; Height: 8.5),
    (PaperSize: DMPAPER_EXECUTIVE; Width: 7.25; Height: 10.5),
    (PaperSize: DMPAPER_LETTER; Width: 8.5; Height: 11.0),
    (PaperSize: DMPAPER_11X17; Width: 11.0; Height: 17.0),
    (PaperSize: DMPAPER_CSHEET; Width: 17.0; Height: 22.0),
    (PaperSize: DMPAPER_DSHEET; Width: 22.0; Height: 34.0),
    (PaperSize: DMPAPER_ESHEET; Width: 34.0; Height: 44.0),
    (PaperSize: DMPAPER_USER; Width: 0.0; Height: 0.0),
    (PaperSize: 0; Width: 0.0; Height: 0.0));
  MetricRatio = 10000 / (3.2808333 * 12);

function PaperFromSize(const Size: TFloatPoint): SHORT;
var
  I: TPaperSize;
begin
  Result := DMPAPER_USER;
  with Size do
    for I := psLetter to psE do
      if (X = PaperSizeMap[I].Width) and (Y = PaperSizeMap[I].Height) then
      begin
        Result := PaperSizeMap[I].PaperSize;
        Break;
      end
end;

destructor TAdvancedPrinter.Destroy;
begin
  FPapers.Free;
  inherited Destroy;
end;

function TAdvancedPrinter.AllocDevice: PDevMode;
var
  Device, Driver, Port: TChar;
begin
  Result := nil;
  GetPrinter(Device, Driver, Port, FDeviceHandle);
  if FDeviceHandle <> 0 then
    Result := GlobalLock(FDeviceHandle);
end;

procedure TAdvancedPrinter.BitmapChange(Sender: TObject);
begin
  InitializeDevice(FBuffer.Canvas.Handle);
end;

procedure TAdvancedPrinter.BeginDoc;
begin
  if FFilePath <> '' then
    FBuffer := TBitmap.Create
  else
    inherited BeginDoc;
  with Canvas.Font do
  begin
    Name := 'Arial';
    Size := 10;
    Style := [];
  end;
  FCookie := InitializeDevice(Canvas.Handle);
  if FBuffer <> nil then
    with GetPhysicalSize do
    begin
      FBuffer.OnChange := BitmapChange;
      FBuffer.Width := Round(DeviceToInt(1) * X);
      FBuffer.Height := Round(DeviceToInt(1, 90) * Y);
    end;
  MoveTo(FHorzMargin, FVertMargin);
  FPageNumber := 1;
  if Assigned(FOnHeader) then
    FOnHeader(Self);
end;

procedure TAdvancedPrinter.CheckPrinting(Value: Boolean);
begin
  if Printing <> Value then
    if Value then
      raise EPrinter.Create(SNotPrinting)
    else
      raise EPrinter.Create(SPrinting);
end;

procedure TAdvancedPrinter.EndDoc;
begin
  try
    if Assigned(FOnFooter) then
      FOnFooter(Self);
  finally
    FinalizeDevice(FCookie);
    FCookie := 0;
    if FBuffer <> nil then
    begin
      FBuffer.SaveToFile(FFilePath + '\job' + IntToStr(BufferedPages) + '.bmp');
      Inc(BufferedPages);
      FBuffer.Free;
      FBuffer := nil;
    end
    else
      inherited EndDoc;
  end;
end;

procedure TAdvancedPrinter.FreeDevice;
begin
  if FDeviceHandle <> 0 then
     GlobalUnlock(FDeviceHandle);
   FDeviceHandle := 0;
end;

procedure TAdvancedPrinter.MoveTo(const X, Y: Double);
begin
  FX := X;
  FY := Y;
  with DeviceToPoint(GetFloatPoint(X, Y)) do
    Canvas.MoveTo(X, Y);
end;

procedure TAdvancedPrinter.LineTo(const X, Y: Double);
begin
  FX := X;
  FY := Y;
  with DeviceToPoint(GetFloatPoint(X, Y)) do
    Canvas.LineTo(x, y);
end;

procedure TAdvancedPrinter.Rectangle(const ALeft, ATop, ARight, ABottom: Double);
var
  IntegerRect: TRect;
begin
  IntegerRect := DeviceToRect(ALeft, ATop, ARight, ABottom);
  Canvas.Rectangle(IntegerRect);
end;

procedure TAdvancedPrinter.Rectangle(const Rect: TFloatRect);
var
  IntegerRect: TRect;
begin
  IntegerRect := DeviceToRect(Rect);
  Canvas.Rectangle(IntegerRect);
end;

procedure TAdvancedPrinter.Polygon(const P: TFloatPolygon);
begin
  DrawPolygon(Canvas.Handle, P);
end;

procedure TAdvancedPrinter.StretchDraw(const Rect: TFloatRect; Graphic: TGraphic);
begin
  Canvas.StretchDraw(DeviceToRect(Rect), Graphic);
end;

procedure TAdvancedPrinter.NewLine;
var
  Baseline: Double;
begin
  Baseline := GetTextBaseline;
  FY := FY + Baseline;
  if FY + Baseline > IntToDevice(PageHeight, 90) - FVertMargin then
    NewPage;
end;

procedure TAdvancedPrinter.NewPage;
begin
  if Assigned(FOnFooter) then
    FOnFooter(Self);
  if FBuffer <> nil then
    with FBuffer do
    begin
      FinalizeDevice(FCookie);
      FBuffer.SaveToFile(FFilePath + '\job' + IntToStr(BufferedPages) + '.bmp');
      FCookie := InitializeDevice(FBuffer.Canvas.Handle);
      Inc(BufferedPages);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, 0, 0, 0, WHITENESS);
    end
  else
    inherited NewPage;
  MoveTo(FHorzMargin, FVertMargin);
  Inc(FPageNumber);
  if Assigned(FOnHeader) then
    FOnHeader(Self);
end;

function TAdvancedPrinter.TextWidth(const S: string): Double;
begin
  Result := IntToDevice(Canvas.TextWidth(S));
end;

function TAdvancedPrinter.TextHeight(const S: string): Double;
begin
  Result := IntToDevice(Canvas.TextWidth(S), 90);
end;

procedure TAdvancedPrinter.UpdateFont(Size: Integer; Style: TFontStyles = []);
begin
  with Canvas do
  begin
    Font.Size := Size;
    Font.Style := Style;
    Handle;
  end;
end;

procedure TAdvancedPrinter.Write(const S: string);
const
  DT_NORMAL = DR_FORMAT or DT_TOP or DT_LEFT or DT_SINGLELINE;
var
  DC: HDC;
  Rect: TRect;
  Size: TSize;
  PriorMode: Integer;
begin
  if S = '' then Exit;
  DC := Canvas.handle;
  Rect := DeviceToRect(GetFloatRect(FX, FY - GetTextAscent, FX + Width, FY));
  GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
  Inc(Rect.Bottom, Size.cy);
  PriorMode := SetBkMode(DC, TRANSPARENT);
  DrawText(DC, PChar(S), -1, Rect, DT_NORMAL);
  SetBkMode(DC, PriorMode);
  FX := IntToDevice(Rect.Left + Size.cx);
end;

procedure TAdvancedPrinter.Write(const S: string; const Width: Double;
  Direction: TDirection = drLeft);
var
  Rect: TRect;
  Size: TSize;
  PriorMode: Integer;
  Format: Integer;
begin
  if S = '' then Exit;
  Rect := DeviceToRect(GetFloatRect(FX, FY - GetTextAscent, FX + Width, FY));
  GetTextExtentPoint32(Canvas.Handle, PChar(S), Length(S), Size);
  Inc(Rect.Bottom, Size.cy);
  PriorMode := SetBkMode(Canvas.Handle, TRANSPARENT);
  Format := DR_FORMAT or DT_TOP;
  case Direction of
    drLeft, drUp, drDown: Format := Format or DT_LEFT;
    drRight: Format := Format or DT_RIGHT;
    drCenter: Format := Format or DT_CENTER;
  end;
  DrawText(Canvas.Handle, PChar(S), -1, Rect, Format);
  SetBkMode(Canvas.Handle, PriorMode);
end;

function TAdvancedPrinter.Write(const S: string; const Width, Height: Double): Double;
var
  Rect: TRect;
begin
  Result := 0.0;
  if S <> '' then
  begin
    Rect := DeviceToRect(GetFloatRect(FX, FY, FX + Width, FY + Height));
    Result := IntToDevice(DrawText(Canvas.Handle, PChar(S), -1, Rect, DR_WRAP), 90);
  end;
end;

procedure TAdvancedPrinter.WriteLine(const S: string);
begin
  Write(S, 24);
  NewLine;
end;

function TAdvancedPrinter.GetCanvas: TCanvas;
begin
  if FBuffer <> nil then
    Result := FBuffer.Canvas
  else
    Result := inherited Canvas;
end;

function TAdvancedPrinter.GetHeight: Double;
begin
  Result := IntToDevice(PageHeight, 90);
end;

function TAdvancedPrinter.GetPapers: TStrings;
const
  PaperNames: array[TPaperSize] of string = ('Letter - 8.5 x 11 in.',
    'Legal - 8.5 x 14 in.', 'Ledger - 11 x 17 in.', 'Statement - 5.5 x 8.5 in.',
    'Executive - 7 1/4 x 10.5 in.', 'A Sheet - 8.5 x 11 in.',
    'B Sheet - 11 x 17 in.', 'C Sheet - 17 x 22 in.', 'D Sheet - 22 x 34 in.',
    'E Sheet - 34 x 44 in.', 'User Defined', 'Other');
var
  I: TPaperSize;
begin
  if FPapers = nil then
    FPapers := TStringList.Create;
  FPapers.Clear;
  for I := Low(TPaperSize) to High(TPaperSize) do
    FPapers.Add(PaperNames[I]);
  I := PaperSize;
  case I of
    psUser, psOther:
      with PhysicalSize do
        FPapers[Ord(I)] := PaperNames[I] + Format(' - %f x %f in.', [X, Y]);
  end;
  Result := FPapers;
end;

function TAdvancedPrinter.GetPaperSize: TPaperSize;
var
  DevMode: PDevMode;
  I: TPaperSize;
begin
  CheckPrinting(False);
  DevMode := AllocDevice;
  try
    Result := psOther;
    if DevMode <> nil then
      for I := Low(TPaperSize) to High(TPaperSize) do
        if DevMode.dmPaperSize = PaperSizeMap[I].PaperSize then
        begin
          Result := I;
          Break;
        end;
        
  finally
    FreeDevice;
  end;
end;

procedure TAdvancedPrinter.SetPaperSize(Value: TPaperSize);
var
  DevMode: PDevMode;
begin
  CheckPrinting(False);
  DevMode := AllocDevice;
  try
    if DevMode <> nil then
    begin
      DevMode.dmPaperSize := PaperSizeMap[Value].PaperSize;
      if Value < psUser then
      begin
        DevMode.dmPaperWidth := Round(PaperSizeMap[Value].Width * MetricRatio);
        DevMode.dmPaperLength := Round(PaperSizeMap[Value].Height * MetricRatio);
      end;
    end;
  finally
    FreeDevice;
  end;
end;

function TAdvancedPrinter.GetPenWidth: Double;
begin
  Result := IntToDevice(Canvas.Pen.Width) * 72;
end;

procedure TAdvancedPrinter.SetPenWidth(const Value: Double);
begin
  Canvas.Pen.Width := DeviceToInt(Value / 72 + IntToDevice(0));
end;

function TAdvancedPrinter.GetPhysicalSize: TFloatPoint;
var
  DevMode: PDevMode;
  I: TPaperSize;
begin
  DevMode := AllocDevice;
  try
     with Result do
      if DevMode <> nil then
        for I := Low(TPaperSize) to High(TPaperSize) do
          if I > psE then
          begin
            X := Round(DevMode.dmPaperWidth / MetricRatio * 1000) / 1000;
            Y := Round(DevMode.dmPaperLength / MetricRatio * 1000) / 1000;
            Break;
          end
          else
          if DevMode.dmPaperSize = PaperSizeMap[I].PaperSize then
          begin
            X := PaperSizeMap[I].Width;
            Y := PaperSizeMap[I].Height;
            Break;
          end
      else
        Result := GetFloatPoint(0, 0);
  finally
    FreeDevice;
  end;
end;

procedure TAdvancedPrinter.SetPhysicalSize(const Value: TFloatPoint);
var
  DevMode: PDevMode;
begin
  CheckPrinting(False);
  DevMode := AllocDevice;
  try
    if DevMode <> nil then
    begin
      DevMode.dmPaperSize := PaperFromSize(Value);
      DevMode.dmPaperWidth := Round(Value.X * MetricRatio);
      DevMode.dmPaperLength := Round(Value.Y * MetricRatio);
      DevMode.dmFields := DevMode.dmFields or DM_PAPERSIZE;
    end;
  finally
    FreeDevice;
  end;
end;

function TAdvancedPrinter.GetPrinterIndex: Integer;
begin
  Result := inherited PrinterIndex;
end;

procedure TAdvancedPrinter.SetPrinterIndex(Value: Integer);
var
  Device, Driver, Port: TChar;
begin
  inherited PrinterIndex := Value;
  GetPrinter(Device, Driver, Port, FDeviceHandle);
  SetPrinter(Device, Driver, Port, 0);
end;

function TAdvancedPrinter.GetRaggedLeft: Double;
begin
  Result := FX - FHorzMargin;
end;

function TAdvancedPrinter.GetRaggedRight: Double;
begin
  Result := Width - FX - FHorzMargin;
end;

function TAdvancedPrinter.GetWidth: Double;
begin
  Result := IntToDevice(PageWidth);
end;

{ InternalAdvancedPrinter variable }

var
  InternalAdvancedPrinter: TObject;
  PreviousPrinter: TPrinter;

function AdvancedPrinter: TAdvancedPrinter;
begin
  if InternalAdvancedPrinter = nil then
  begin
    PreviousPrinter := Printer;
    InternalAdvancedPrinter := TAdvancedPrinter.Create;
    SetPrinter(InternalAdvancedPrinter as TPrinter);
  end;
  Result := TAdvancedPrinter(InternalAdvancedPrinter);
end;

procedure PrintRawText(const Printer, Name, Data: string);
var
  PrintHandle: THandle;
  Doc: TDocInfo1;
  Written: Cardinal;
begin
  OpenPrinter(PChar(Printer), PrintHandle, nil);
  if PrintHandle <> 0 then
  begin
    Doc.pDocName := PChar(Name);
    Doc.pOutputFile := nil;
    Doc.pDatatype := nil;
    StartDocPrinter(PrintHandle, 1, @Doc);
    StartPagePrinter(PrintHandle);
    WritePrinter(PrintHandle, Pointer(Data), Length(Data), Written);
    EndPagePrinter(PrintHandle);
    EndDocPrinter(PrintHandle);
    ClosePrinter(PrintHandle);
  end;
end;

initialization
  InternalAdvancedPrinter := nil;
  PreviousPrinter := nil;
finalization
  if PreviousPrinter <> nil then
    SetPrinter(PreviousPrinter as TPrinter);
  InternalAdvancedPrinter.Free;
end.
