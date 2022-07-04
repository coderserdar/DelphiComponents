{   Unit cyImageEn

    Description:
    Unit with functions to use with ImageEn5.X/ImageEn6.X components.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyImageEn;

interface

uses Windows, Classes, SysUtils, Variants, Graphics, imageenio, iemio, iewia, imscan, iexAcquire, imageenview, iemview, imageenproc, hyiedefs;

// Twain functions :
function TWAIN_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): Boolean;
function TWAIN_SilentAcquireFromFeeder(Handler: TImageEnMIO; Pages: Integer): Boolean;

// * WIA functions * //
function UseWIACommands_ForVistaAndUpper: boolean;

// INFO :
function WIA_Connected(aWIAParams: TIEWia): Boolean;        // Device connected?
function WIA_FeederSupported(aWIAParams: TIEWia): Boolean;    // Scanner with Feeder capability ?
function WIA_FeederDetectPage(aWIAParams: TIEWia): Boolean;
function WIA_DuplexSupported(aWIAParams: TIEWia): Boolean;  // Duplex feature?

// STATUS :
function WIA_FeedPageReady(aWIAParams: TIEWia): Boolean;    // Feeder with sheets?
function WIA_paperJam(aWIAParams: TIEWia): Boolean;         // Paper jam?

// OTHERS :
function WIA_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): boolean;
function WIA_SilentAcquireFromFeeder(Handler: TImageEnMIO; DuplexMode: Boolean; Pages: Integer): boolean;

// Other functions :
procedure ConvertTiff2Pdf(ImageEnMView: TImageEnMView; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                          const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true); overload;  // Diference between 80 and 90 is not big ...
procedure ConvertTiff2Pdf(FileTif, FilePdf: String; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                          const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true); overload;    // Diference between 80 and 90 is not big ...

procedure ConvertImage2Pdf(ImageEnMView: TImageEnMView; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                           const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true); overload;   // Diference between 80 and 90 is not big ...
procedure ConvertImage2Pdf(ImageFile, FilePdf: String; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                           const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true); overload;   // Diference between 80 and 90 is not big ...

procedure ConvertImage2Tiff(ImageEnMView: TImageEnMView; const TIFF_Compression: TIOTIFFCompression = ioTIFF_G4FAX; const ResampleDpis: Integer = 0); overload;
procedure ConvertImage2Tiff(ImageFile, FileTiff: String; const TIFF_Compression: TIOTIFFCompression = ioTIFF_G4FAX; const ResampleDpis: Integer = 0); overload;

function EncodeTiffToG4FAX(const SrcFileTIF, DestFileTIF: String; var Modified: Boolean; const BWLocalThresholdParam: Integer = 16; const RemoveNoiseIteration: Integer = 4): Boolean;
function ReEncodeTiff(const SrcFileTIF, DestFileTIF: String; const NewCompression: TIOTIFFCompression; const CompressionQuality: Integer; const BitsPerSample, SamplesPerPixel: Integer; var Modified: Boolean; const RemoveNoiseIteration: Integer = 0): Boolean;
function TIFFRemoveNoise(const SrcFileTIF, DestFileTIF: String; const Iteration: Integer = 4): Boolean;

function Save(ImageEnMView: TImageEnMView; const DestFile: String): Boolean;
function SaveToTiff(ImageEnMView: TImageEnMView; const DestFileTiff: String): Boolean;
function SaveToPdf(ImageEnMView: TImageEnMView; const DestFilePdf: String): Boolean;

procedure PrintPreviewDialog(aBitmap: Graphics.TBitmap; const DialogType: TIEDialogType = iedtDialog; const PrintSize: TIOPrintPreviewSize = psFitToPage;
                              const DialogsMeasureUnit: TIEDialogsMeasureUnit = ieduCm; const MarginTop: Double = 0.5; const MarginLeft: double = 0.5;
                              const MarginRight: Double = 0.5; const MarginBottom: double = 0.5);

const
  TWAIN_CAP_XFERCOUNT = $0001; // number of pages (default is -1)

  // For Windows Vista and upper (from Microsoft SDK wiadef.h header file)
  WIA_IPS_PAGES = 3096;                 // Page number to scan from feeder
  WIA_IPS_PAGE_SIZE = 3097;             // Page definition
  WIA_IPS_PAGE_WIDTH = 3098;            // A4 =  8267 inches
  WIA_IPS_PAGE_HEIGHT = 3099;           // A4 = 11692 inches
  WIA_IPS_MAX_HORIZONTAL_SIZE = 6165;   // Maximum Horizontal Scan Size

  WIA_IPS_MAX_VERTICAL_SIZE = 6166;     // Maximum Vertical Scan Size
  WIA_IPS_MIN_HORIZONTAL_SIZE = 6167;   // Minimum Horizontal Scan Size
  WIA_IPS_MIN_VERTICAL_SIZE = 6168;     // Minimum Vertical Scan Size

  // 2017-01-11 Scan handlig :
  WIA_IPS_AUTO_DESKEW = 3107;
  WIA_IPS_BLANK_PAGES = 4167;
  WIA_IPS_AUTO_CROP = 4170;
  WIA_IPS_BLANK_PAGES_SENSITIVITY = 4192;

  // 2017-01-11 WIA_IPS_BLANK_PAGES constants
  WIA_BLANK_PAGE_DETECTION_DISABLED = 0;
  WIA_BLANK_PAGE_DISCARD = 1;
  WIA_BLANK_PAGE_JOB_SEPARATOR = 2;

  // 2017-01-11 WIA_IPS_AUTO_CROP constants
  WIA_AUTO_CROP_DISABLED = 0;
  WIA_AUTO_CROP_SINGLE = 1;
  WIA_AUTO_CROP_MULTI = 2;

  // 2017-01-11 WIA_IPS_AUTO_CROP constants
  WIA_AUTO_DESKEW_ON = 0;
  WIA_AUTO_DESKEW_OFF = 1;

  // 2017-01-11 Set page size exemple : WIAParams.SetItemProperty(WIA_IPS_PAGE_SIZE, WIA_PAGE_A4, nil)
  WIA_PAGE_ISO_A3 = 10; // 11692 x 16535 inches
  WIA_PAGE_ISO_A4 = 0;  // same as WIA_PAGE_A4 in iewia.pas ...
  WIA_PAGE_ISO_A5 = 11; // 5826 x 8267 inches

implementation

// Twain functions :
// RHR - Does not work if pages are in the feeder!
function TWAIN_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): Boolean;
begin
  Result := true;
  Handler.TWainParams.VisibleDialog := false;
  Handler.TWainParams.ProgressIndicators:= false;
  Handler.TWainParams.FeederEnabled := false;
  Handler.TWainParams.AutoFeed := false;
  Handler.TWainParams.AutoScan := false;
  Handler.TWainParams.AcceptedImages := -2;  // Default scanner value

  Handler.TWainParams.Update;   // 2017-01-03 ... Determines if the current parameters are valid. If not, it restores value to a value supported by the scanner.


  // Digitalize once :
  if Handler.Acquire then // ImageEn3    if Handler.Acquire(ieaTWain) then
    begin
      if Assigned(AfterScanSheet) then
        AfterScanSheet(Handler);
    end
    else
      Result := false;
end;

function TWAIN_SilentAcquireFromFeeder(Handler: TImageEnMIO; Pages: Integer): Boolean;
begin
  Handler.TWainParams.VisibleDialog := false;
  Handler.TWainParams.ProgressIndicators := false;
  Handler.TWainParams.FeederEnabled := true;
  Handler.TWainParams.AutoFeed := (Pages = 0) or (Pages > 1);
  // 2017-10-20 Handler.TWainParams.AutoScan := Pages > 0;
  Handler.TWainParams.AutoScan := (Pages = 0) or (Pages > 1);  // 2017-10-20 Increase speed performance !

  if pages > 0
  then Handler.TWainParams.AcceptedImages := Pages
  else Handler.TWainParams.AcceptedImages := -1;    // Multiple pages

  Handler.TWainParams.Update;   // Will determine if the current parameters are valid. If not, it restores value to a value supported by the scanner.

  Result := Handler.Acquire;
end;

// WIA functions :
function UseWIACommands_ForVistaAndUpper: boolean;
var
  VersionInfo: TOSVersionInfo;
begin
  Result := false;
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  if VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT  // Win 2000, XP, Vista and 7 ...
  then Result := VersionInfo.dwMajorVersion >= 6;
end;

function WIA_Connected(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
begin
  // WIA_DPA_CONNECT_STATUS "is intended for serial devices or other non-Plug and Play devices where the operating system cannot determine if the device is connected."
  // So, if WIA device in list, we assume that is connected !
  RESULT := aWIAParams.ConnectedDeviceIndex <> -1;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_DPA_CONNECT_STATUS);          // Not working ...
    // ov := aWIAParams.GetItemProperty(WIA_DPA_CONNECT_STATUS, aWIAParams.Device);    // Not working ...


    if ov <> unassigned then
      if ov = WIA_DEVICE_NOT_CONNECTED then   // ov = unassigned = WIA_DEVICE_NOT_CONNECTED ...
        RESULT := false;
  except
  end;
end;


function WIA_FeederSupported(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
  Valor: Integer;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES);
    // Ov returns : WIA_FEED = 1, WIA_FLAT = 2, WIA_DETECT_FLAT = 8, WIA_DETECT_SCAN = 16, WIA_DETECT_FEED = 32, WIP_DUP = 64, WIA_DETECT_FEED_AVAIL = 128, WIA_DETECT_DUP_AVAIL  = 256 !
    // ov = 3, so : WIA_FEED + WIA_DUP ...

    valor := ov;    // = 3 ...

    if ov <> 0 then
      if Valor and WIA_FEED = Valor then
        RESULT := true;
  except

  end;
end;

function WIA_FeederDetectPage(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
  Valor: Integer;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES);
    // Ov returns : WIA_FEED = 1, WIA_FLAT = 2, WIA_DETECT_FLAT = 8, WIA_DETECT_SCAN = 16, WIA_DETECT_FEED = 32, WIP_DUP = 64, WIA_DETECT_FEED_AVAIL = 128, WIA_DETECT_DUP_AVAIL  = 256 !
    // ov = 3, so : WIA_FEED + WIA_DUP ...

    valor := ov;    // = 3 ...

    if ov <> 0 then
      if Valor and WIA_DETECT_FEED = Valor then
        RESULT := true;
  except

  end;
end;

function WIA_DuplexSupported(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
  Valor: Integer;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES);
    // Ov returns : WIA_FEED = 1, WIA_FLAT = 2, WIA_DETECT_FLAT = 8, WIA_DETECT_SCAN = 16, WIA_DETECT_FEED = 32, WIP_DUP = 64, WIA_DETECT_FEED_AVAIL = 128, WIA_DETECT_DUP_AVAIL  = 256 !
    // ov = 3, so : WIA_FEED + WIA_DUP ...

    valor := ov;    // = 3 ...

    if ov <> 0 then
      if Valor and WIA_DUP = Valor then
        RESULT := true;
  except

  end;
end;

function WIA_FeedPageReady(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
  Valor: Integer;
begin
  RESULT := false;

  try
    // * CHECK IF SCANNER CAN DETECT PAGE ON FEEDER * //
    // !!! WIA_DETECT_FEED_AVAIL is Result of WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES !!!
    // ov := aWIAParams.GetDeviceProperty(WIA_DETECT_FEED_AVAIL);

    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_CAPABILITIES);
    valor := ov;

    if Valor and WIA_DETECT_FEED = Valor then
    begin
      ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_STATUS);
      // Ov returns : WIA_FEED_READY = 1, WIA_FLAT_READY = 2, WIA_DUP_READY = 4, WIA_FLAT_COVER_UP = 8, WIA_PATH_COVER_UP = 16, WIA_PAPER_JAM = 32 !
      // ov = 3, so : WIA_FEED_READY + WIA_DUP_READY ...

      valor := ov;

      if ov <> 0 then
        if Valor and WIA_FEED_READY = Valor then
          RESULT := true;
    end;
  except
  end;
end;

function WIA_paperJam(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
  Valor: Integer;
begin
  RESULT := false;

  try
    // !!! WIA_PAPER_JAM is Result of WIA_DPS_DOCUMENT_HANDLING_STATUS !!!
    // ov := aWIAParams.GetDeviceProperty(WIA_PAPER_JAM);
    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_STATUS);

    // Ov returns : WIA_FEED_READY = 1, WIA_FLAT_READY = 2, WIA_DUP_READY = 4, WIA_FLAT_COVER_UP = 8, WIA_PATH_COVER_UP = 16, WIA_PAPER_JAM = 32 !
    // ov = 3, so : WIA_FEED_READY + WIA_DUP_READY ...

    valor := ov;

    if ov <> 0 then
      if Valor and WIA_PAPER_JAM = Valor then
        RESULT := true;
  except

  end;
end;

function WIA_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): boolean;
var
  Flag: Integer;
begin
  Result := true;
  Handler.AcquireParams.VisibleDialog := false;
  Flag   := WIA_FLATBED;
  Handler.WIAParams.SetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_SELECT, Flag);

  if Handler.Acquire then
  begin
    if Assigned(AfterScanSheet) then
      AfterScanSheet(Handler);
  end
  else
    Result := false;
end;

function WIA_SilentAcquireFromFeeder(Handler: TImageEnMIO; DuplexMode: Boolean; Pages: Integer): boolean;
var Flag: Integer;
begin
  Handler.AcquireParams.VisibleDialog := false;

  if Pages < 0
  then Pages := 0; // Multipages ...

  if DuplexMode
  then Flag := WIA_FEEDER or WIA_DUPLEX
  else Flag := WIA_FEEDER;

  Handler.WIAParams.SetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_SELECT, Flag);

  if UseWIACommands_ForVistaAndUpper
  then Handler.WIAParams.SetDeviceProperty(WIA_IPS_PAGES, Pages)
  else Handler.WIAParams.SetDeviceProperty(WIA_DPS_PAGES, Pages);

  Result := Handler.Acquire;
end;

procedure ConvertTiff2Pdf(ImageEnMView: TImageEnMView; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                          const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true);   // Diference between 80 and 90 is not big ...
var
  p, Sav: Integer;
begin
  for p := 1 to ImageEnMView.ImageCount do
  begin
    ImageEnMView.SelectedImage := p-1;

    if ImageEnMView.IEBitmap.Width <> 0 then    // 2016-07-29 Force to load IEBitmap reading any property !!!
    begin
      if ResampleDpis <> 0 then
        if ImageEnMView.MIO.Params[p-1].Dpi <> ResampleDpis then
          ImageEnMView.Proc.Resample( Round(ImageEnMView.MIO.Params[p-1].Width * ResampleDpis / ImageEnMView.MIO.Params[p-1].Dpi),    // New width
                                     -1,   // New height (Auto) ...
                                     rfFastLinear, True);

      if ImageEnMView.MIO.Params[p-1].BitsPerSample = 1 then
      begin
        ImageEnMView.MIO.Params[p-1].JPEG_Quality := JpegQuality;
        ImageEnMView.MIO.Params[p-1].PDF_Compression := ioPDF_G4FAX;
      end
      else begin
        ImageEnMView.MIO.Params[p-1].JPEG_Quality := ImageEnMView.MIO.Params[p-1].TIFF_JPEGQuality;
        ImageEnMView.MIO.Params[p-1].PDF_Compression := ioPDF_JPEG;
      end;
    end;

    ImageEnMView.MIO.Params[p-1].JPEG_Smooth     := 100;
    ImageEnMView.MIO.Params[p-1].PDF_PaperSize   := PDFPaperSize;

    // 2017-04-11 Adjust page orientation automatically :
    if AutoAdjustPageOrientation then
      if ImageEnMView.MIO.Params[p-1].Width > ImageEnMView.MIO.Params[p-1].Height then
      begin
        Sav := ImageEnMView.MIO.Params[p-1].PDF_PaperHeight;
        ImageEnMView.MIO.Params[p-1].PDF_PaperHeight := ImageEnMView.MIO.Params[p-1].PDF_PaperWidth;
        ImageEnMView.MIO.Params[p-1].PDF_PaperWidth  := Sav;
      end;

    (* 2016-07-29 ImageEnMView.MIO.Params[j].JPEG_Quality := JpegQuality;
    ImageEnMView.MIO.Params[j].PDF_Compression := ioPDF_JPEG;  *)
  end;
end;

procedure ConvertTiff2Pdf(FileTif, FilePdf: String; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                          const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true);   // Diference between 80 and 90 is not big ...
var
  ImageEnMView: TImageEnMView;
begin
  ImageEnMView := TImageEnMView.Create(Nil);

  (* 2016-07-29 Error Out of memory on large tif (16 MO) ...
  ImageEnMView.EnableImageCaching := True;
  ImageEnMView.ImageCacheUseDisk := False;          // !!! On ne peut pas utiliser ImageCacheUseDisk quand on utilise le LogonUser !!!
  ImageEnMView.MIO.LoadFromFileTIFF(FileTif); *)

  ImageEnMView.LoadFromFileOnDemand(FileTif, false);
  ConvertTiff2Pdf(ImageEnMView, JpegQuality, ResampleDpis, PDFPaperSize, AutoAdjustPageOrientation);

  ImageEnMView.MIO.SaveToFilePDF(FilePdf);
  ImageEnMView.Free;
end;

// !!! 2017-09-22 Problem with jpeg at 300 dpis: pdf page too big !!!
procedure ConvertImage2Pdf(ImageEnMView: TImageEnMView; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                           const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true);   // Diference between 80 and 90 is not big ...
var
  p, Sav: Integer;
  InchesX, InchesY: Extended;
begin
  for p := 1 to ImageEnMView.ImageCount do
  begin
    ImageEnMView.SelectedImage := p-1;

    if ImageEnMView.IEBitmap.Width <> 0 then    // 2016-07-29 Force to load IEBitmap reading any property !!!
    begin
      if ResampleDpis <> 0 then
        if ImageEnMView.MIO.Params[p-1].Dpi <> ResampleDpis then
          ImageEnMView.Proc.Resample( Round(ImageEnMView.MIO.Params[p-1].Width * ResampleDpis / ImageEnMView.MIO.Params[p-1].Dpi),    // New width
                                     -1,   // New height (Auto) ...
                                     rfFastLinear, True);

      ImageEnMView.MIO.Params[p-1].JPEG_Quality    := JpegQuality;
      ImageEnMView.MIO.Params[p-1].PDF_Compression := ioPDF_JPEG;
      ImageEnMView.MIO.Params[p-1].JPEG_Smooth     := 100;
      ImageEnMView.MIO.Params[p-1].PDF_PaperSize   := PDFPaperSize;

      // 2017-09-22   Adobe PDF points (1 point=1/72 of inch).
      // ImageEnMView.MIO.Params[p-1].Dpi  := Already set !
      // ImageEnMView.MIO.Params[p-1].DpiX := Already set !
      // ImageEnMView.MIO.Params[p-1].DpiY := Already set !

      InchesX := ImageEnMView.MIO.Params[p-1].Width / ImageEnMView.MIO.Params[p-1].DpiX;
      InchesY := ImageEnMView.MIO.Params[p-1].Height / ImageEnMView.MIO.Params[p-1].DpiY;

      ImageEnMView.MIO.Params[p-1].PDF_PaperWidth := Round(InchesX * 72);    // A4 = 595 => 595 / 72 * 2.5 = 20.66 cm
      ImageEnMView.MIO.Params[p-1].PDF_PaperHeight := Round(InchesY * 72);    // A4 = 842 => 842 / 72 * 2.5 = 29.4 cm

      // 2017-04-11 Adjust page orientation automatically :
      if AutoAdjustPageOrientation then
        if ImageEnMView.MIO.Params[p-1].Width > ImageEnMView.MIO.Params[p-1].Height then
        begin
          Sav := ImageEnMView.MIO.Params[p-1].PDF_PaperHeight;
          ImageEnMView.MIO.Params[p-1].PDF_PaperHeight := ImageEnMView.MIO.Params[p-1].PDF_PaperWidth;
          ImageEnMView.MIO.Params[p-1].PDF_PaperWidth  := Sav;
        end;
    end;
  end;
end;

procedure ConvertImage2Pdf(ImageFile, FilePdf: String; const JpegQuality: Integer = 90; const ResampleDpis: Integer = 0;
                           const PDFPaperSize: TIOPDFPaperSize = iepA4; const AutoAdjustPageOrientation: Boolean = true);   // Diference between 80 and 90 is not big ...
var
  ImageEnMView: TImageEnMView;
begin
  ImageEnMView := TImageEnMView.Create(Nil);

  ImageEnMView.LoadFromFileOnDemand(ImageFile, false);
  ConvertImage2Pdf(ImageEnMView, JpegQuality, ResampleDpis, PDFPaperSize, AutoAdjustPageOrientation);

  ImageEnMView.MIO.SaveToFilePDF(FilePdf);
  ImageEnMView.Free;
end;

procedure ConvertImage2Tiff(ImageEnMView: TImageEnMView; const TIFF_Compression: TIOTIFFCompression = ioTIFF_G4FAX; const ResampleDpis: Integer = 0);
var
  p: Integer;
begin
  for p := 1 to ImageEnMView.ImageCount do
  begin
    ImageEnMView.SelectedImage := p-1;

    if ImageEnMView.IEBitmap.Width <> 0 then    // 2016-07-29 Force to load IEBitmap reading any property !!!
    begin
      if ResampleDpis <> 0 then
        if ImageEnMView.MIO.Params[p-1].Dpi <> ResampleDpis then
          ImageEnMView.Proc.Resample( Round(ImageEnMView.MIO.Params[p-1].Width * ResampleDpis / ImageEnMView.MIO.Params[p-1].Dpi),    // New width
                                     -1,   // New height (Auto) ...
                                     rfFastLinear, True);

      if TIFF_Compression = ioTIFF_G4FAX then
      begin
        ImageEnMView.MIO.Params[p-1].TIFF_Compression := ioTIFF_G4FAX;
        ImageEnMView.MIO.Params[p-1].BitsPerSample := 1;
        ImageEnMView.MIO.Params[p-1].SamplesPerPixel := 1;
      end
      else begin
        ImageEnMView.MIO.Params[p-1].BitsPerSample := 8;
        ImageEnMView.MIO.Params[p-1].SamplesPerPixel := 3;
        ImageEnMView.MIO.Params[p-1].TIFF_Compression := TIFF_Compression; // Old ... ioTIFF_LZW;
      end;
    end;
  end;
end;

procedure ConvertImage2Tiff(ImageFile, FileTiff: String; const TIFF_Compression: TIOTIFFCompression = ioTIFF_G4FAX; const ResampleDpis: Integer = 0);
var
  ImageEnMView: TImageEnMView;
begin
  ImageEnMView := TImageEnMView.Create(Nil);

  ImageEnMView.LoadFromFileOnDemand(ImageFile, false);
  ConvertImage2Tiff(ImageEnMView, TIFF_Compression, ResampleDpis);

  ImageEnMView.MIO.SaveToFileTIFF(FileTiff);
  ImageEnMView.Free;
end;

// Because imageen components don' t save always in first try if file exists :
// On va limiter la tentative de sauvegarde à 100 fois
function Save(ImageEnMView: TImageEnMView; const DestFile: String): Boolean;
var
  TryCount: Integer;
begin
  Result := false;
  TryCount := 0;

  repeat
    try
      ImageEnMView.MIO.SaveToFile(DestFile);
      Result := true;
    except
      Inc(TryCount);
      Sleep(100);
    end;
  until Result or (TryCount >= 100);
end;

function SaveToTiff(ImageEnMView: TImageEnMView; const DestFileTiff: String): Boolean;
var
  TryCount: Integer;
begin
  Result := false;
  TryCount := 0;

  repeat
    try
      ImageEnMView.MIO.SaveToFileTIFF(DestFileTiff);
      Result := true;
    except
      Inc(TryCount);
      Sleep(100);
    end;
  until Result or (TryCount >= 100);
end;

function SaveToPdf(ImageEnMView: TImageEnMView; const DestFilePdf: String): Boolean;
var
  TryCount: Integer;
begin
  Result := false;
  TryCount := 0;

  repeat
    try
      ImageEnMView.MIO.SaveToFilePDF(DestFilePdf);
      Result := true;
    except
      Inc(TryCount);
      Sleep(100);
    end;
  until Result or (TryCount >= 100);
end;

(* OLD function ConvertTiffToTIFFG4FAX(SrcFileTIF, DestFileTIF: String; var Modified: Boolean): Boolean;
var
  ImageEnMView, ImageEnMViewDest: TImageEnMView;
  i: Integer;
begin
  Result := false;
  Modified := false;
  ImageEnMView := TImageEnMView.Create(Nil);
  ImageEnMViewDest := TImageEnMView.Create(Nil);

  try
    ImageEnMView.MIO.LoadFromFile(SrcFileTIF);

    if ImageEnMView.MIO.ParamsCount > 0 then
      if ImageEnMView.MIO.Params[0].TIFF_Compression <> ioTIFF_G4FAX then
        for i := 0 to ImageEnMView.MIO.ParamsCount-1 do
        begin
          Modified := True;
          ImageEnMView.SelectedImage := i;
          ImageEnMView.Proc.ConvertToBWThreshold;
          // BUG On peut pas modifier TIFF_Compression := ioTIFF_G4FAX et BitsPerSample := 1 en même temps !!!
          //ImageEnMView.MIO.Params[i].TIFF_Compression := ioTIFF_G4FAX;
          ImageEnMView.MIO.Params[i].BitsPerSample := 1;
          ImageEnMView.MIO.Params[i].SamplesPerPixel := 1;

          ImageEnMViewDest.AppendImage;
          ImageEnMViewDest.SetImage(i, ImageEnMView.Bitmap);
          ImageEnMViewDest.MIO.Params[i].TIFF_Compression := ioTIFF_G4FAX;
          ImageEnMViewDest.MIO.Params[i].BitsPerSample := 1;
          ImageEnMViewDest.MIO.Params[i].SamplesPerPixel := 1;
        end;

    if Modified then
      SaveToTiff(ImageEnMViewDest, DestFileTIF);

    Result := True;
  except
  end;

  ImageEnMView.Free;
  ImageEnMViewDest.Free;
end;    *)


function EncodeTiffToG4FAX(const SrcFileTIF, DestFileTIF: String; var Modified: Boolean; const BWLocalThresholdParam: Integer = 16; const RemoveNoiseIteration: Integer = 4): Boolean;
var
  ImageEnMView, ImageEnMViewDest: TImageEnMView;
  i: Integer;
begin
  Result := false;
  Modified := false;
  ImageEnMView := TImageEnMView.Create(Nil);
  ImageEnMViewDest := TImageEnMView.Create(Nil);

  try
    ImageEnMView.MIO.LoadFromFileTiff(SrcFileTIF);

    if ImageEnMView.MIO.ParamsCount > 0 then
      for i := 0 to ImageEnMView.MIO.ParamsCount-1 do
        if ImageEnMView.MIO.Params[i].TIFF_Compression <> ioTIFF_G4FAX then  // Threshold and RemoveNoiseIteration has no effect if TIFF_Compression is already ioTIFF_G4FAX !!!
        begin
          Modified := True;
          ImageEnMView.SelectedImage := i;

          if BWLocalThresholdParam = 0
          then ImageEnMView.Proc.ConvertToBWThreshold
          else ImageEnMView.Proc.ConvertToBWLocalThreshold(BWLocalThresholdParam, ietMean, BWLocalThresholdParam);   //       (ietMean, ietMedian, ietMeanMinMax);

          if RemoveNoiseIteration <> 0 then
            ImageEnMView.Proc.RemoveNoise(RemoveNoiseIteration, false);

          // BUG On peut pas modifier TIFF_Compression := ioTIFF_G4FAX et BitsPerSample := 1 en même temps DANS LE FICHIER SOURCE !!!
          //ImageEnMView.MIO.Params[i].TIFF_Compression := ioTIFF_G4FAX;
          // ImageEnMView.MIO.Params[i].BitsPerSample := 1;
          // ImageEnMView.MIO.Params[i].SamplesPerPixel := 1;

          ImageEnMViewDest.AppendImage;
          ImageEnMViewDest.SetImage(i, ImageEnMView.Bitmap);

          ImageEnMViewDest.MIO.Params[i].TIFF_Compression := ioTIFF_G4FAX;
          ImageEnMViewDest.MIO.Params[i].BitsPerSample := 1;
          ImageEnMViewDest.MIO.Params[i].SamplesPerPixel := 1;
        end
        else begin
          ImageEnMView.SelectedImage := i;

          ImageEnMViewDest.AppendImage;
          ImageEnMViewDest.SetImage(i, ImageEnMView.Bitmap);

          if ImageEnMViewDest.MIO.Params[i].TIFF_Compression <> ioTIFF_G4FAX then   // Occurs if first page added !
          begin
            ImageEnMViewDest.MIO.Params[i].TIFF_Compression := ioTIFF_G4FAX;
            ImageEnMViewDest.MIO.Params[i].BitsPerSample := 1;
            ImageEnMViewDest.MIO.Params[i].SamplesPerPixel := 1;
          end;
        end;

    Result := True;
  except
    // Le fichier n' a pas pu être ouvert/sauvegardé ...
  end;

  ImageEnMView.Free;

  if Result then
    if Modified then
      Result := cyImageEn.SaveToTiff(ImageEnMViewDest, DestFileTIF)
    else
      // 2017-10-25 ...
      if AnsiUppercase(SrcFileTIF) <> AnsiUppercase(DestFileTIF) then
        Result := CopyFile(PChar(SrcFileTIF), PChar(DestFileTIF), false);

  ImageEnMViewDest.Free;
end;

function ReEncodeTiff(const SrcFileTIF, DestFileTIF: String; const NewCompression: TIOTIFFCompression; const CompressionQuality: Integer; const BitsPerSample, SamplesPerPixel: Integer; var Modified: Boolean; const RemoveNoiseIteration: Integer = 0): Boolean;
var
  ImageEnMView, ImageEnMViewDest: TImageEnMView;
  i: Integer;
begin
  Result := false;
  Modified := false;
  ImageEnMView := TImageEnMView.Create(Nil);
  ImageEnMViewDest := TImageEnMView.Create(Nil);

  try
    ImageEnMView.MIO.LoadFromFileTiff(SrcFileTIF);

    if ImageEnMView.MIO.ParamsCount > 0 then
      for i := 0 to ImageEnMView.MIO.ParamsCount-1 do
        if (ImageEnMView.MIO.Params[i].TIFF_Compression <> NewCompression)
        or ((RemoveNoiseIteration <> 0) and (ImageEnMView.MIO.Params[i].TIFF_Compression <> ioTIFF_G4FAX)) then  // RemoveNoiseIteration has no effect if TIFF_Compression is already ioTIFF_G4FAX !!!
        begin
          Modified := True;
          ImageEnMView.SelectedImage := i;

          if RemoveNoiseIteration <> 0 then
            ImageEnMView.Proc.RemoveNoise(RemoveNoiseIteration, false);

          ImageEnMViewDest.AppendImage;
          ImageEnMViewDest.SetImage(i, ImageEnMView.Bitmap);
          ImageEnMViewDest.MIO.Params[i].TIFF_Compression := NewCompression;

          if BitsPerSample <> 0 then
            ImageEnMViewDest.MIO.Params[i].BitsPerSample := BitsPerSample;

          if SamplesPerPixel <> 0 then
            ImageEnMViewDest.MIO.Params[i].SamplesPerPixel := SamplesPerPixel;

          if NewCompression = ioTIFF_JPEG then
          begin
            ImageEnMViewDest.MIO.Params[i].TIFF_JPEGColorSpace := ioJPEG_YCbCr;        // ioJPEG_YCbCr or ioJPEG_GRAYLEV generate bigger files ...

            if CompressionQuality <> 0 then
              ImageEnMViewDest.MIO.Params[i].TIFF_JPEGQuality := CompressionQuality;
          end;
        end
        else begin
          ImageEnMView.SelectedImage := i;

          ImageEnMViewDest.AppendImage;
          ImageEnMViewDest.SetImage(i, ImageEnMView.Bitmap);

          if ImageEnMViewDest.MIO.Params[i].TIFF_Compression <> NewCompression then   // Occurs if first page added !
            ImageEnMViewDest.MIO.Params[i].TIFF_Compression := NewCompression;

          if BitsPerSample <> 0 then
            if ImageEnMViewDest.MIO.Params[i].BitsPerSample <> BitsPerSample then
              ImageEnMViewDest.MIO.Params[i].BitsPerSample := BitsPerSample;

          if SamplesPerPixel <> 0 then
            if ImageEnMViewDest.MIO.Params[i].SamplesPerPixel <> SamplesPerPixel then
              ImageEnMViewDest.MIO.Params[i].SamplesPerPixel := SamplesPerPixel;
        end;

    Result := True;
  except
    // Le fichier n' a pas pu être ouvert/sauvegardé ...
  end;

  ImageEnMView.Free;

  if Result then
    if Modified then
      Result := cyImageEn.SaveToTiff(ImageEnMViewDest, DestFileTIF)
    else
      // 2017-10-25 ...
      if AnsiUppercase(SrcFileTIF) <> AnsiUppercase(DestFileTIF) then
        Result := CopyFile(PChar(SrcFileTIF), PChar(DestFileTIF), false);

  ImageEnMViewDest.Free;
end;

function TIFFRemoveNoise(const SrcFileTIF, DestFileTIF: String; const Iteration: Integer = 4): Boolean;
var
  ImageEnMView: TImageEnMView;
  i: Integer;
begin
  Result := false;

  ImageEnMView := TImageEnMView.Create(Nil);

  try
    ImageEnMView.MIO.LoadFromFileTiff(SrcFileTIF);
    ImageEnMView.Proc.AutoUndo := false;

    for i := 0 to ImageEnMView.MIO.ParamsCount-1 do
    begin
      ImageEnMView.SelectedImage := i;
      ImageEnMView.Proc.RemoveNoise(Iteration, false);
      //ImageEnMView.Proc.RemoveIsolatedPixels(0, 1);        // Black / 1 ...
    end;

    cyImageEn.SaveToTiff(ImageEnMView, DestFileTIF);

    Result := True;
  except
    // Le fichier n' a pas pu être ouvert/sauvegardé ...
  end;

  ImageEnMView.Free;
end;

procedure PrintPreviewDialog(aBitmap: Graphics.TBitmap; const DialogType: TIEDialogType = iedtDialog; const PrintSize: TIOPrintPreviewSize = psFitToPage;
                              const DialogsMeasureUnit: TIEDialogsMeasureUnit = ieduCm; const MarginTop: Double = 0.5; const MarginLeft: double = 0.5;
                              const MarginRight: Double = 0.5; const MarginBottom: double = 0.5);
var
  ImageEnView: TImageEnView;
begin
  ImageEnView := TImageEnView.Create(Nil);
  ImageEnView.IEBitmap.Assign(aBitmap);
  ImageEnView.IO.DialogsMeasureUnit := DialogsMeasureUnit;
  ImageEnView.IO.PrintPreviewParams.Gamma := 0;
  // Specify print size :
  //ImageEnView.IO.PrintPreviewParams.Width := 800;
  //ImageEnView.IO.PrintPreviewParams.Height := 600;

  // Margins :
  ImageEnView.IO.PrintPreviewParams.MarginTop := 0.5;
  ImageEnView.IO.PrintPreviewParams.MarginLeft := 0.5;
  ImageEnView.IO.PrintPreviewParams.MarginRight := 0.5;
  ImageEnView.IO.PrintPreviewParams.MarginBottom := 0.5;

  // Fit/Stretch/Normal etc ...
  ImageEnView.IO.PrintPreviewParams.Size := PrintSize;

  ImageEnView.IO.DoPrintPreviewDialog(DialogType, 'Imprimir imagem', false, 'Imprimir ...');
  ImageEnView.Free;
end;

end.
