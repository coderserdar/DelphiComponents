{   Unit cyImageEn4

    Description:
    Unit with functions to use with ImageEn4.X components.

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

unit cyImageEn4;

interface

uses Windows, Classes, imageenio, iemio, iewia, imscan, iexAcquire, iemview;

// Twain functions :
function TWAIN_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): Boolean;
function TWAIN_SilentAcquireFromFeeder(Handler: TImageEnMIO; Pages: Integer): Boolean;

// WIA functions:
function UseWIACommands_ForVistaAndUpper: boolean;
function WIA_Connected(aWIAParams: TIEWia): Boolean;        // Device connected?
function WIA_FeedPageReady(aWIAParams: TIEWia): Boolean;    // Feeder with sheets?
function WIA_DuplexSupported(aWIAParams: TIEWia): Boolean;  // Duplex feature?
function WIA_paperJam(aWIAParams: TIEWia): Boolean;         // Paper jam?
//function WIA_SilentAcquire(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): boolean;
function WIA_SilentAcquireFromFlatbed(Handler: TImageEnIO; AfterScanSheet: TNotifyEvent): boolean;
function WIA_SilentAcquireFromFeeder(Handler: TImageEnMIO; DuplexMode: Boolean; Pages: Integer): boolean;

// Other functions :
procedure ConvertTiff2Pdf(FileTif, FilePdf: String; const JpegQuality: Integer = 90);
function SaveToTiff(ImageEnMView: TImageEnMView; DestFileTiff: String): Boolean;

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

  // ImageEn3   imscan.FeederScanPages := -1;
  Handler.TWainParams.AcceptedImages := -2;  // Default scanner value

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
  Handler.TWainParams.ProgressIndicators:= false;
  Handler.TWainParams.FeederEnabled := true;

{ ImageEn3
  Handler.TWainParams.AutoFeed := Pages <> 1;

  if pages > 0
  then imscan.FeederScanPages := Pages
  else imscan.FeederScanPages := -1;    }

  // Não faz diferênça ...    Handler.TWainParams.AutoScan := true;
  Handler.TWainParams.AutoFeed := Pages = 0;  // Scan all pages

  if pages > 0
  then Handler.TWainParams.AcceptedImages := Pages
  else Handler.TWainParams.AcceptedImages := -1;    // Multiple pages


  Result := Handler.Acquire;   // ImageEn3  Handler.Acquire(ieaTWain);
end;

// WIA functions :
function UseWIACommands_ForVistaAndUpper: boolean;
var
  VersionInfo: TOSVersionInfo;
begin
  Result := false;
  VersionInfo.dwOSVersionInfoSize:=SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  if VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT  // Win 2000, XP, Vista and 7 ...
  then Result := VersionInfo.dwMajorVersion >= 6;
end;

function WIA_Connected(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetItemProperty(WIA_DPA_CONNECT_STATUS, nil);

    if ov = WIA_DEVICE_CONNECTED then
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
    ov := aWIAParams.GetDeviceProperty(WIA_DPS_DOCUMENT_HANDLING_STATUS);
    valor := ov;

    if ov <> 0 then
      if Valor and WIA_FEED_READY = Valor then
        RESULT := true;
  except

  end;
end;

function WIA_DuplexSupported(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_DETECT_DUP_AVAIL);

    if ov <> 0 then
      RESULT := true;
  except

  end;
end;

function WIA_paperJam(aWIAParams: TIEWia): Boolean;
var
  ov: Variant;
begin
  RESULT := false;

  try
    ov := aWIAParams.GetDeviceProperty(WIA_PAPER_JAM);

    if ov <> 0 then
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

  if Handler.Acquire then // ImageEn3   if Handler.Acquire(ieaWIA) then
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

  Result := Handler.Acquire; // ImageEn3  Result := Handler.Acquire(ieaWIA);
end;

procedure ConvertTiff2Pdf(FileTif, FilePdf: String; const JpegQuality: Integer = 90);
var
  ImageEnMView: TImageEnMView;
  j: Integer;
begin
  ImageEnMView := TImageEnMView.Create(Nil);
  // !!! On ne peut pas utiliser ImageCacheUseDisk quand on utilise le LogonUser !!!
  ImageEnMView.EnableImageCaching := True;
  ImageEnMView.ImageCacheUseDisk := False;
  ImageEnMView.MIO.LoadFromFileTIFF(FileTif);

  for j := 0 to ImageEnMView.ImageCount-1 do
  begin
    ImageEnMView.MIO.Params[j].JPEG_Quality := JpegQuality;
    ImageEnMView.MIO.Params[j].PDF_Compression := ioPDF_JPEG;
  end;

  ImageEnMView.MIO.SaveToFilePDF(FilePdf);
  ImageEnMView.Free;
end;

// Because imageen components don' t save always in first try if file exists :
// On va limiter la tentative de sauvegarde à 100 fois
function SaveToTiff(ImageEnMView: TImageEnMView; DestFileTiff: String): Boolean;
var TryCount: Integer;
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

end.
