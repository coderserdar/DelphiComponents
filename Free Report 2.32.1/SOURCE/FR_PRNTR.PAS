
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Printer info               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Prntr;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Printers, WinSpool, FR_Class, FR_Const;

type
  TfrPrinter = class
  private
    FDevice: PChar;
    FDriver: PChar;
    FPort: PChar;
    FDeviceMode: THandle;
    FMode: PDeviceMode;
    FPrinter: TPrinter;
    FPaperNames: TStringList;
    FPrinters: TStringList;
    FPrinterIndex: Integer;
    FDefaultPrinter: Integer;
    procedure GetSettings;
    procedure SetSettings;
    procedure SetPrinter(Value: TPrinter);
    procedure SetPrinterIndex(Value: Integer);
  public
    Orientation: TPrinterOrientation;
    PaperSize: Integer;
    PaperWidth: Integer;
    PaperHeight: Integer;
    PaperSizes: Array[0..255] of Word;
    PaperSizesNum: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure FillPrnInfo(var p: TfrPrnInfo);
    procedure SetPrinterInfo(pgSize, pgWidth, pgHeight: Integer;
      pgOr: TPrinterOrientation);
    function IsEqual(pgSize, pgWidth, pgHeight: Integer;
      pgOr: TPrinterOrientation): Boolean;
    function GetArrayPos(pgSize: Integer): Integer;
    property PaperNames: TStringList read FPaperNames;
    property Printer: TPrinter read FPrinter write SetPrinter;
    property Printers: TStringList read FPrinters;
    property PrinterIndex: Integer read FPrinterIndex write SetPrinterIndex;
  end;


var
  Prn: TfrPrinter;

implementation

type
  TPaperInfo = record
    Typ: Integer;
    Name: String;
    X, Y: Integer;
  end;

const
  PAPERCOUNT = 67;
  PaperInfo: Array[0..PAPERCOUNT - 1] of TPaperInfo = (
    (Typ:1;  Name: ''; X:2159; Y:2794),
    (Typ:2;  Name: ''; X:2159; Y:2794),
    (Typ:3;  Name: ''; X:2794; Y:4318),
    (Typ:4;  Name: ''; X:4318; Y:2794),
    (Typ:5;  Name: ''; X:2159; Y:3556),
    (Typ:6;  Name: ''; X:1397; Y:2159),
    (Typ:7;  Name: ''; X:1842; Y:2667),
    (Typ:8;  Name: ''; X:2970; Y:4200),
    (Typ:9;  Name: ''; X:2100; Y:2970),
    (Typ:10; Name: ''; X:2100; Y:2970),
    (Typ:11; Name: ''; X:1480; Y:2100),
    (Typ:12; Name: ''; X:2500; Y:3540),
    (Typ:13; Name: ''; X:1820; Y:2570),
    (Typ:14; Name: ''; X:2159; Y:3302),
    (Typ:15; Name: ''; X:2150; Y:2750),
    (Typ:16; Name: ''; X:2540; Y:3556),
    (Typ:17; Name: ''; X:2794; Y:4318),
    (Typ:18; Name: ''; X:2159; Y:2794),
    (Typ:19; Name: ''; X:984;  Y:2254),
    (Typ:20; Name: ''; X:1048; Y:2413),
    (Typ:21; Name: ''; X:1143; Y:2635),
    (Typ:22; Name: ''; X:1207; Y:2794),
    (Typ:23; Name: ''; X:1270; Y:2921),
    (Typ:24; Name: ''; X:4318; Y:5588),
    (Typ:25; Name: ''; X:5588; Y:8636),
    (Typ:26; Name: ''; X:8636; Y:11176),
    (Typ:27; Name: ''; X:1100; Y:2200),
    (Typ:28; Name: ''; X:1620; Y:2290),
    (Typ:29; Name: ''; X:3240; Y:4580),
    (Typ:30; Name: ''; X:2290; Y:3240),
    (Typ:31; Name: ''; X:1140; Y:1620),
    (Typ:32; Name: ''; X:1140; Y:2290),
    (Typ:33; Name: ''; X:2500; Y:3530),
    (Typ:34; Name: ''; X:1760; Y:2500),
    (Typ:35; Name: ''; X:1760; Y:1250),
    (Typ:36; Name: ''; X:1100; Y:2300),
    (Typ:37; Name: ''; X:984;  Y:1905),
    (Typ:38; Name: ''; X:920;  Y:1651),
    (Typ:39; Name: ''; X:3778; Y:2794),
    (Typ:40; Name: ''; X:2159; Y:3048),
    (Typ:41; Name: ''; X:2159; Y:3302),
    (Typ:42; Name: ''; X:2500; Y:3530),
    (Typ:43; Name: ''; X:1000; Y:1480),
    (Typ:44; Name: ''; X:2286; Y:2794),
    (Typ:45; Name: ''; X:2540; Y:2794),
    (Typ:46; Name: ''; X:3810; Y:2794),
    (Typ:47; Name: ''; X:2200; Y:2200),
    (Typ:50; Name: ''; X:2355; Y:3048),
    (Typ:51; Name: ''; X:2355; Y:3810),
    (Typ:52; Name: ''; X:2969; Y:4572),
    (Typ:53; Name: ''; X:2354; Y:3223),
    (Typ:54; Name: ''; X:2101; Y:2794),
    (Typ:55; Name: ''; X:2100; Y:2970),
    (Typ:56; Name: ''; X:2355; Y:3048),
    (Typ:57; Name: ''; X:2270; Y:3560),
    (Typ:58; Name: ''; X:3050; Y:4870),
    (Typ:59; Name: ''; X:2159; Y:3223),
    (Typ:60; Name: ''; X:2100; Y:3300),
    (Typ:61; Name: ''; X:1480; Y:2100),
    (Typ:62; Name: ''; X:1820; Y:2570),
    (Typ:63; Name: ''; X:3220; Y:4450),
    (Typ:64; Name: ''; X:1740; Y:2350),
    (Typ:65; Name: ''; X:2010; Y:2760),
    (Typ:66; Name: ''; X:4200; Y:5940),
    (Typ:67; Name: ''; X:2970; Y:4200),
    (Typ:68; Name: ''; X:3220; Y:4450),
    (Typ:256;Name: ''; X:0;    Y:0));


function DeviceCapabilities(pDevice, pPort: PChar; fwCapability: Word; pOutput: PChar;
  DevMode: PDeviceMode): Integer; stdcall; external winspl name 'DeviceCapabilitiesA';

{----------------------------------------------------------------------------}
constructor TfrPrinter.Create;
var
  i: Integer;
begin
  inherited Create;
  GetMem(FDevice, 128);
  GetMem(FDriver, 128);
  GetMem(FPort, 128);
  FPaperNames := TStringList.Create;
  FPrinters := TStringList.Create;
  for i := 0 to PAPERCOUNT - 1 do
    PaperInfo[i].Name := LoadStr(SPaper1 + i);
end;

destructor TfrPrinter.Destroy;
begin
  FreeMem(FDevice, 128);
  FreeMem(FDriver, 128);
  FreeMem(FPort, 128);
  FPaperNames.Free;
  FPrinters.Free;
  inherited Destroy;
end;

procedure TfrPrinter.GetSettings;
var
  i: Integer;
  PaperNames: PChar;
  Size: TPoint;
begin
  FPrinter.GetPrinter(FDevice, FDriver, FPort, FDeviceMode);
  try
    FMode := GlobalLock(FDeviceMode);

    PaperSize := FMode.dmPaperSize;

    Escape(FPrinter.Handle, GetPhysPageSize, 0, nil, @Size);
    PaperWidth := Round(Size.X / GetDeviceCaps(FPrinter.Handle, LOGPIXELSX) * 254);
    PaperHeight := Round(Size.Y / GetDeviceCaps(FPrinter.Handle, LOGPIXELSY) * 254);

    FillChar(PaperSizes, SizeOf(PaperSizes), 0);
    PaperSizesNum := DeviceCapabilities(FDevice, FPort, DC_PAPERS, @PaperSizes, FMode);

    GetMem(PaperNames, PaperSizesNum * 64);
    DeviceCapabilities(FDevice, FPort, DC_PAPERNAMES, PaperNames, FMode);
    FPaperNames.Clear;
    for i := 0 to PaperSizesNum - 1 do
      FPaperNames.Add(StrPas(PaperNames + i * 64));
    FreeMem(PaperNames, PaperSizesNum * 64);
  finally
    GlobalUnlock(FDeviceMode);
  end;
end;

procedure TfrPrinter.SetSettings;
var
  i, n: Integer;
begin
  if FPrinterIndex = FDefaultPrinter then
  begin
    FPaperNames.Clear;
    for i := 0 to PAPERCOUNT - 1 do
    begin
      FPaperNames.Add(PaperInfo[i].Name);
      PaperSizes[i] := PaperInfo[i].Typ;
      if (PaperSize <> $100) and (PaperSize = PaperInfo[i].Typ) then
      begin
        PaperWidth := PaperInfo[i].X;
        PaperHeight := PaperInfo[i].Y;
        if Orientation = poLandscape then
        begin
          n := PaperWidth; PaperWidth := PaperHeight; PaperHeight := n;
        end;
      end;
    end;
    PaperSizesNum := PAPERCOUNT;
    Exit;
  end;

  FPrinter.GetPrinter(FDevice, FDriver, FPort, FDeviceMode);
  try
    FMode := GlobalLock(FDeviceMode);
    if PaperSize = $100 then
    begin
      FMode.dmFields := FMode.dmFields or DM_PAPERLENGTH or DM_PAPERWIDTH;
      FMode.dmPaperLength := PaperHeight;
      FMode.dmPaperWidth := PaperWidth;
    end;

    if (FMode.dmFields and DM_PAPERSIZE) <> 0 then
      FMode.dmPaperSize := PaperSize;

    if (FMode.dmFields and DM_ORIENTATION) <> 0 then
      if Orientation = poPortrait then
        FMode.dmOrientation := DMORIENT_PORTRAIT else
        FMode.dmOrientation := DMORIENT_LANDSCAPE;

    if (FMode.dmFields and DM_COPIES) <> 0 then
      FMode.dmCopies := 1;

    FPrinter.SetPrinter(FDevice, FDriver, FPort, FDeviceMode);
  finally
    GlobalUnlock(FDeviceMode);
  end;
  GetSettings;
end;

procedure TfrPrinter.FillPrnInfo(var p: TfrPrnInfo);
var
  kx, ky: Double;
begin
  kx := 93 / 1.022;
  ky := 93 / 1.015;
  if FPrinterIndex = FDefaultPrinter then
    with p do
    begin
      Pgw := Round(PaperWidth * kx / 254);
      Pgh := Round(PaperHeight * ky / 254);
      Ofx := Round(50 * kx / 254);
      Ofy := Round(50 * ky / 254);
      Pw := Pgw - Ofx * 2;
      Ph := Pgh - Ofy * 2;
    end
  else
    with p, FPrinter do
    begin
      kx := kx / GetDeviceCaps(Handle, LOGPIXELSX);
      ky := ky / GetDeviceCaps(Handle, LOGPIXELSY);
      PPgw := GetDeviceCaps(Handle, PHYSICALWIDTH); Pgw := Round(PPgw * kx);
      PPgh := GetDeviceCaps(Handle, PHYSICALHEIGHT); Pgh := Round(PPgh * ky);
      POfx := GetDeviceCaps(Handle, PHYSICALOFFSETX); Ofx := Round(POfx * kx);
      POfy := GetDeviceCaps(Handle, PHYSICALOFFSETY); Ofy := Round(POfy * ky);
      PPw := PageWidth; Pw := Round(PPw * kx);
      PPh := PageHeight; Ph := Round(PPh * ky);
    end;
end;

function TfrPrinter.IsEqual(pgSize, pgWidth, pgHeight: Integer;
  pgOr: TPrinterOrientation): Boolean;
begin
  if (PaperSize = pgSize) and (pgSize = $100) then
    Result := (PaperSize = pgSize) and (PaperWidth = pgWidth) and
     (PaperHeight = pgHeight) and (Orientation = pgOr)
  else
    Result := (PaperSize = pgSize) and (Orientation = pgOr);
end;

procedure TfrPrinter.SetPrinterInfo(pgSize, pgWidth, pgHeight: Integer;
  pgOr: TPrinterOrientation);
begin
  if IsEqual(pgSize, pgWidth, pgHeight, pgOr) then Exit;
  PaperSize := pgSize;
  PaperWidth := pgWidth;
  PaperHeight := pgHeight;
  Orientation := pgOr;
  SetSettings;
end;

function TfrPrinter.GetArrayPos(pgSize: Integer): Integer;
var
  i: Integer;
begin
  Result := PaperSizesNum - 1;
  for i := 0 to PaperSizesNum - 1 do
    if PaperSizes[i] = pgSize then
    begin
      Result := i;
      break;
    end;
end;

procedure TfrPrinter.SetPrinterIndex(Value: Integer);
begin
  FPrinterIndex := Value;
  if Value = FDefaultPrinter then
    SetSettings
  else if FPrinter.Printers.Count > 0 then
  begin
    FPrinter.PrinterIndex := Value;
    GetSettings;
  end;
end;

procedure TfrPrinter.SetPrinter(Value: TPrinter);
begin
  FPrinters.Clear;
  FPrinterIndex := 0;
  FPrinter := Value;
  if FPrinter.Printers.Count > 0 then
  begin
    FPrinters.Assign(FPrinter.Printers);
    FPrinterIndex := FPrinter.PrinterIndex;
    GetSettings;
  end;
  FPrinters.Add(LoadStr(SDefaultPrinter));
  FDefaultPrinter := FPrinters.Count - 1;
end;


{----------------------------------------------------------------------------}

initialization
  Prn := TfrPrinter.Create;
  Prn.Printer := Printer;

finalization
  Prn.Free;

end.
