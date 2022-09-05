// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  21056: DocScanForm.pas 
//
//    Rev 1.13    26-08-2009 22:46:44  mcm    Version: IMG 3.2
// Fixed unicode issues (PChar -> PAnsiChar)
//
//    Rev 1.12    22-05-2006 20:21:34  mcm
// Fixed problem when scanning image unchecking the ADF.
//
//    Rev 1.11    02-04-2006 19:06:14  mcm
//
//   Rev 1.10    30-06-2005 22:06:04  mcm

//
//   Rev 1.9    11-06-2005 10:06:08  mcm    Version: IMG 2.9
// Modified the fix for Canon scanners. 

//
//   Rev 1.8    29-05-2005 23:38:14  mcm    Version: IMG 2.9
// Solved a problem when acquiring multiple pages from Canon Scanners (MPC600F). 

//
//   Rev 1.7    15-05-2005 22:07:06  mcm    Version: IMG 2.9
// Added additionl color parameters to the user interface.

//
//   Rev 1.6    09-04-2005 17:34:06  mcm    Version: IMG 2.9
// Corrected a problem with data sources being unable to negotiate resolution.
// Enabled scan of multiple pages from non ADF scanners (Correction).

//
//   Rev 1.5    19-02-2005 00:37:26  mcm
// Added brightness checkbox.

//
//   Rev 1.4    07-03-2004 14:33:18  mcm
// Modified to handle change in OnNegotiation.

//
//   Rev 1.3    17-11-2003 00:45:28  mcm    Version: IMG 2.0
// Added call to SetStretchMode to optimize B/W view.

//
//   Rev 1.2    04-11-2003 23:35:06  mcm

//
//   Rev 1.1    25-09-2003 23:31:18  mcm    Version: IMG 1.5

//
//   Rev 1.0    29-07-2003 11:42:38  mcm

unit DocScanForm;

//------------------------------------------------------------------------------
// DOCSCAN Sample project.
//
// This example shows how to acquire a document using the Native or Memory
// transfer mechanism and store the image to TIFF.
//
// The sample project works with flatbed scanners and document scanners, i.e.
// scanners with an ADF (Automatic Document Feeder).
//
// The acquired images are saved to either a multi paged TIFF or in single TIFF
// image files post-indexed by continues numbers.
// We've added code that stores the image scanned in a TMemoryStream, for use
// with a database Blob field (see SaveImage below).
//
// If the acquired image is BW, CCITT Group 4 compression is used. With other
// color resolutions 4-32 bit i.e. grey/color images, the Packbits (or if
// available LZW) compression scheme is used (see SaveImage below).
//
// When this application is started or a new TWAIN driver is selected, the TWAIN
// driver is interogated, to determin a few of its capabilities
// (see GetScannerInfo and GetNegotiate below).
//
// The acquisition:
// In this example we will not use the function Acquire, but instead
//  - OpenSourceMgr to open the TWAIN manager.
//  - OpenSource - to open and enable us to communicate with the TWAIN driver.
//  - EnableSource - to initiate scanning.
//
//    With ADF EnableSource causes the number of pages to be scanned (note that
//    we do keep track of the number of images scanned and we will terminate the
//    acquisition (see mcmTWAINXferNext and WMNextPage) if the scanner doesn't
//    stop after the requested number of images are scanned.
//    If the ADF doesn't contain the requested number of pages, we'll use the
//    same trick as for non-ADF scanners below - we disable and then re-enable
//    the TWAIN driver. 
//
//    Without an ADF the scanner will only scan one page at a time, i.e. one
//    image per call to EnableSource, we will therefore in WMNextPage call
//
//  - DisableSource - to disable the TWAIN driver.
//  - EnableSource - to initiate the next acquisition.
//
//    When all requested pages have been scanned we call (see WMNextPage)
//  - DisableSource - to disable the TWAIN driver.
//  - CloseSource - to close and unload the TWAIN driver.
//  - CloseSourceMgr - close and unload the TWAIN manager.
//
// Make sure that the "Search Path" in "Project, Options, Directories/
// Conditionals" points to the directory that the Imaging Toolkit for Delphi was
// installed to: d:\Program files\Borland\DelphiX\MCM DESIGN.
// Where "d" is the drive, and "X" is the Delphi version.
// Also make sure that "Output directory" and "Unit output directory" points to
// valid directories.
//------------------------------------------------------------------------------

interface

{$IFDEF VER100} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER110} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER120} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER150} {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Buttons, StdCtrls, Spin,
  TWAIN, mcmTWAINKernel, mcmTWAINIntf, mcmTWAIN, mcmImage, mcmImageFile, umcmIntE;

const
  WM_NEXTPAGE  = WM_USER + 1;
  WM_SAVEIMAGE = WM_USER + 2;

type
  TFormDocScan = class(TForm)
    ImageCtrl       : TmcmImageCtrl;
    mcmTWAIN        : TmcmTWAIN;
    StatusBar       : TStatusBar;
    SaveDialog      : TSaveDialog;

    PanelMain       : TPanel;
    PanelButton     : TPanel;
    sbScan          : TSpeedButton;
    sbSelect        : TSpeedButton;
    sbClose         : TSpeedButton;
    PageControl     : TPageControl;
    tsMain          : TTabSheet;
    gbMain          : TGroupBox;

    lPagesToScan    : TLabel;
    sePagesToScan   : TSpinEdit;
    lDesc1          : TLabel;

    gbSaveImage     : TGroupBox;
    lFileName       : TLabel;
    eFileName       : TEdit;
    sbFileName      : TSpeedButton;
    cbMultiTIFF     : TCheckBox;
    cbSaveImage     : TCheckBox;

    tsFormat        : TTabSheet;
    gbFormat        : TGroupBox;
    lUnit           : TLabel;
    cbUnits         : TComboBox;
    lResolution     : TLabel;
    cbResolution    : TComboBox;
    seResolution    : TSpinEdit;
    lPaperSize      : TLabel;
    cbPaperSize     : TComboBox;
    lLeft           : TLabel;
    lRight          : TLabel;
    lTop            : TLabel;
    lBottom         : TLabel;
    rsLeft          : TmcmRealSpin;
    rsRight         : TmcmRealSpin;
    rsTop           : TmcmRealSpin;
    rsBottom        : TmcmRealSpin;

    tsColor         : TTabSheet;
    gbColor         : TGroupBox;
    lPixelType      : TLabel;
    cbColor         : TComboBox;
    cbAutoBright    : TCheckBox;
    lBitReduce      : TLabel;
    cbBitReduce     : TComboBox;
    lThreshold      : TLabel;
    isThreshold     : TmcmIntSpin;

    tsADF           : TTabSheet;
    gbADF           : TGroupBox;
    cbEnableADF     : TCheckBox;
    cbEnableDuplex  : TCheckBox;
    lBrightness     : TLabel;
    isBrightness    : TmcmIntSpin;
    lContrast       : TLabel;
    isContrast      : TmcmIntSpin;

    tsTransfer      : TTabSheet;
    gbTransfer      : TGroupBox;
    lMechanism      : TLabel;
    cbXferMechanism : TComboBox;
    lFileFormat     : TLabel;
    cbFileFormat    : TComboBox;
    lCompression    : TLabel;
    cbCompression   : TComboBox;  

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbSelectClick(Sender: TObject);
    procedure sbScanClick(Sender: TObject);
    procedure mcmTWAINDisableMenus(Sender: TObject);
    procedure mcmTWAINEnableMenus(Sender: TObject);
    procedure mcmTWAINCloseSource(Sender: TObject);
    procedure mcmTWAINNegotiation(Sender: TObject; var DoCancel : boolean);
    procedure mcmTWAINXferNext(Sender: TObject; var NumImages: Integer; var SkipNext: Boolean);
    procedure mcmTWAINDeviceNotReady(Sender: TObject; var DoOpenSource: Boolean);
    {$IFNDEF VER100}
     // Delphi 4 - 7
    procedure mcmTWAINImageReady(Sender: TObject; pBmp: Pointer; pBmpInfo: PBitmapInfo; hImage : hBitmap; FilePath: String);
    {$ELSE}
     // Delphi 3
    procedure mcmTWAINImageReady(Sender: TObject; pBmp: Pointer; pBmpInfo: PBitmapInfo; hImage : Integer; FilePath: String);
    {$ENDIF}
    procedure sePagesToScanChange(Sender: TObject);
    procedure seResolutionChange(Sender: TObject);
    procedure mcmTWAINMemXferSize(Sender: TObject; MinSize, MaxSize: Integer; var BufSize: Integer; pBmpInfo: PBitmapInfo);
    procedure mcmTWAINMemXferBuffer(Sender: TObject; pBmpInfo: PBitmapInfo; BytesPerRow, Rows, DataSize: Integer; var pData: Pointer);
    procedure mcmTWAINXferReady(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure cbBitReduceChange(Sender: TObject);

    procedure cbPaperSizeChange(Sender: TObject);
    procedure cbUnitsChange(Sender: TObject);
    procedure rsLeftChange(Sender: TObject);
    procedure rsRightChange(Sender: TObject);
    procedure rsTopChange(Sender: TObject);
    procedure rsBottomChange(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
    procedure cbResolutionChange(Sender: TObject);
    procedure cbEnableADFClick(Sender : TObject);

    procedure mcmTWAINDeviceEvent(Sender: TObject; Event: TTwnDeviceEvent;
      DeviceName: String; A, B, C: Variant);
    procedure sbFileNameClick(Sender: TObject);
    procedure cbSaveImageClick(Sender: TObject);
    procedure cbXferMechanismChange(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
  private
    { Private declarations }
    FSourceName     : string;
    FSaveImage      : TmcmImage;
    FImagesToScan   : integer; // Number of images to scan.
    FImagesScanned  : integer; // Images scanned during current session.
    FADFAvailable   : boolean; // Indicates if ADF is available.
    FDuplex         : boolean;
    FAutoFeed       : boolean; // Indicates if auto feed is performed by device.
    FAutoScan       : boolean;
    FColorFormat    : integer; // Color format of acquired images
    FPaperSize      : integer; // Size of image acquired if not "None".
    FUnit           : integer; // Unit of measurement (Inch or Centimeter).
    FResolution     : double;  // Resolution used when acquiring images.
    FBitDepth       : integer; // Pixel bit depth.
    FBitReduction   : integer; // Bit depth reduction method.

    FFileName       : string;  // Base file name used for storing images.
                               // If Multi-page TIFF is selected the images are
                               // stored under this file name.
                               // Otherwise the images are stored using
                               // d:\path\FileName(N).tif
                               // where (N) is an incremented integer.
    FThisLine       : integer; // Number of lines acquired when using the Memory
                               // transfer mechanisme.
    FBuffer         : Pointer; // Temporary storage for incoming image data.
    FxMin, FxMax    : double;  // Max x & y image size/coordinates.
    FyMin, FyMax    : double;  // Min x & y image size.

    procedure WMSaveImage(var Msg : TMessage); message WM_SAVEIMAGE;
    procedure SaveImage;
    procedure GetScannerInfo;
    procedure GetNegotiate(Sender: TObject; var DoCancel : boolean);
    procedure ConvertToInch(Value : TmcmRealSpin);
    procedure ConvertToCm(Value : TmcmRealSpin);
    procedure WMNextPage(var Msg : TMessage); message WM_NEXTPAGE;
    procedure SetNextFilename;
  public
    { Public declarations }
  end;

var FormDocScan : TFormDocScan;

implementation

{$R *.DFM}

uses mcmTWAINContainer, mcmImageTypeDef;

procedure TFormDocScan.FormCreate(Sender : TObject);
begin
  PageControl.ActivePage := tsMain;

  // Initialisation for TWAIN
  FBuffer := Nil;
  FxMin := 0;
  FxMax := 10000;
  FyMin := 0;
  FyMax := 10000;
  FColorFormat    := TWPT_BW;
  FBitReduction   := TWBR_THRESHOLD;
  FBitDepth       := 1;
  FUnit           := TWUN_INCHES;
  FResolution     := 150.0;
  FPaperSize      := TWSS_A4LETTER;
  FFileName       := '.\docscan.tif';
  FImagesToScan   := 1;

  // Initialise the TmcmTWAIN control.
  mcmTWAIN.ShowIndicators := False;
  mcmTWAIN.ShowUI := False;
  mcmTWAIN.DisableAfterAcquire := False;
  mcmTWAIN.XferMech := TWFX_MEMORY;
  if (mcmTWAIN.XferMech = TWFX_MEMORY)
  then mcmTWAIN.DIBHandleType := THDT_MEMPTR
  else mcmTWAIN.DIBHandleType := THDT_DIBSEC;

  // Set-up our image view.
  ImageCtrl.Center := True;
  ImageCtrl.ScaleToFit := True;

  FSaveImage := TmcmImage.Create;

  // Initialise user interface controls.
  seResolution.Value  := Round(FResolution);
  sePagesToScan.Value := FImagesToScan;
  eFileName.Text      := FFilename;
  seResolution.Width  := cbResolution.Width;
  cbXferMechanism.ItemIndex := integer(mcmTWAIN.XferMech);
end; // TFormDocScan.FormCreate.


procedure TFormDocScan.FormDestroy(Sender : TObject);
begin
  if Assigned(FSaveImage)
  then FSaveImage.Free;
  if (FBuffer <> Nil)
  then FreeMem(FBuffer);
end; // TFormDocScan.FormDestroy.


procedure TFormDocScan.FormActivate(Sender : TObject);
begin
  cbSaveImageClick(Self); // Update Main, file save section.
  
  Update;
  GetScannerInfo;
  Caption := 'DocScan [' + FSourceName + ']';
end; // TFormDocScan.FormActivate.


procedure TFormDocScan.sbCloseClick(Sender : TObject);
begin
  Close;
end; // TFormDocScan.sbCloseClick.


procedure TFormDocScan.sePagesToScanChange(Sender : TObject);
begin
  if (sePagesToScan.Text <> '')
  then FImagesToScan := sePagesToScan.Value;
end; // TFormDocScan.sePagesToScanChange.


procedure TFormDocScan.sbFileNameClick(Sender : TObject);
begin
  // Get file name from Save dialogue.
  SaveDialog.FileName := eFileName.Text;
  if SaveDialog.Execute
  then eFileName.Text := ChangeFileExt(SaveDialog.FileName, '.tif');
end; // TFormDocScan.sbFileNameClick.


procedure TFormDocScan.cbSaveImageClick(Sender : TObject);
begin
  lFileName.Enabled := cbSaveImage.Checked;
  eFileName.Enabled := cbSaveImage.Checked;
  sbFileName.Enabled := cbSaveImage.Checked;
  cbMultiTIFF.Enabled := cbSaveImage.Checked;
end; // TFormDocScan.cbSaveImageClick.


procedure TFormDocScan.seResolutionChange(Sender : TObject);
begin
  if (seResolution.Text <> '')
  then FResolution := seResolution.Value;
end; // TFormDocScan.seResolutionChange.


procedure TFormDocScan.cbResolutionChange(Sender : TObject);
begin
  if (cbResolution.Items.Count > cbResolution.ItemIndex)
  then FResolution := StrToInt(cbResolution.Items[cbResolution.ItemIndex]);
end; // TFormDocScan.cbResolutionChange.


procedure TFormDocScan.sbSelectClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
  GetScannerInfo;
  Caption := 'DocScan [' + FSourceName + ']';
end; // TFormDocScan.sbSelectClick.


procedure TFormDocScan.sbScanClick(Sender : TObject);
var Failed : boolean;
begin
  // Start acquisition.
  // In this example we will not use the function Acquire, but instead:
  //  - OpenSourceMgr to open the TWAIN manager.
  //  - OpenSource - to open and enable us to communicate with the TWAIN driver.
  //  - EnableSource - to initiate scanning.

  FImagesScanned := 0; // Zero image have been scanned.

  // Special case: Transfer to file. We'll update the file name by adding an
  // index at the end of the file name.
  if (mcmTWAIN.XferMech = TWFX_FILES)
  then SetNextFilename;

  Failed := True;
  if mcmTWAIN.OpenSourceMgr
  then if mcmTWAIN.OpenSource('')
       then if mcmTWAIN.EnableSource(mcmTWAIN.ShowUI)
            then Failed := False;

  if Failed
  then begin
       // If any part of opening/enabling the data source failed, we'll
       // close down the data source and manager.
       mcmTWAIN.DisableSource;
       mcmTWAIN.CloseSource;
       mcmTWAIN.CloseSourceMgr;
       StatusBar.Panels[0].Text := 'Status: Could not open source';
  end
  else begin
       // The data source was opened and enabled sucessfully - we're scanning.
       StatusBar.Panels[0].Text := 'Status: Scanning';
  end;
end; // TFormDocScan.sbScanClick.


procedure TFormDocScan.cbColorChange(Sender : TObject);
var Index : integer;
begin
  // User interface: User has changed the color format.
  if (0 <= cbColor.ItemIndex) and (cbColor.ItemIndex < cbColor.Items.Count)
  then begin
       FColorFormat := smallint(cbColor.Items.Objects[cbColor.ItemIndex]);
       Index := cbBitReduce.Items.IndexOf('None');
       if (FColorFormat <> TWPT_BW)
       then begin
            if (Index = -1)
            then begin
                 cbBitReduce.Items.AddObject('None', Pointer($FFFF));
                 cbBitReduce.ItemIndex := cbBitReduce.Items.Count - 1;
            end;
       end
       else begin
            if (Index <> -1)
            then begin
                 cbBitReduce.Items.Delete(Index);
                 cbBitReduce.ItemIndex := 0;
            end;
       end;
       cbBitReduceChange(Self);
  end;
end; // TFormDocScan.cbColorChange.


procedure TFormDocScan.cbBitReduceChange(Sender : TObject);
begin
  // User interface: User has changed the bit reduction method.
  if (0 <= cbBitReduce.ItemIndex) and (cbBitReduce.ItemIndex < cbBitReduce.Items.Count)
  then begin
       FBitReduction := smallint(cbBitReduce.Items.Objects[cbBitReduce.ItemIndex]);
       lThreshold.Enabled  := (FBitReduction = TWBR_THRESHOLD) and (isThreshold.Tag <> 0);
       isThreshold.Enabled := lThreshold.Enabled;
  end;
end; // TFormDocScan.cbBitReduceChange.


procedure TFormDocScan.cbPaperSizeChange(Sender : TObject);
begin
  // User interface: User has changed the paper format.
  if (0 <= cbPaperSize.ItemIndex) and (cbPaperSize.ItemIndex < cbPaperSize.Items.Count)
  then FPaperSize := integer(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex])
  else FPaperSize := -1;
  if (FPaperSize = 0) // i.e. None
  then begin
       lLeft.Enabled := True;
       lRight.Enabled := True;
       lTop.Enabled := True;
       lBottom.Enabled := True;
       rsLeft.Enabled := True;
       rsRight.Enabled := True;
       rsTop.Enabled := True;
       rsBottom.Enabled := True;
  end
  else begin
       lLeft.Enabled := False;
       lRight.Enabled := False;
       lTop.Enabled := False;
       lBottom.Enabled := False;
       rsLeft.Enabled := False;
       rsRight.Enabled := False;
       rsTop.Enabled := False;
       rsBottom.Enabled := False;
  end;
end; // TFormDocScan.cbPaperSizeChange.


procedure TFormDocScan.ConvertToInch(Value : TmcmRealSpin);
var TempValue : double;
begin
  // Convert cemtimeter value to inch
  if Assigned(Value)
  then begin
       TempValue := Value.Value;
       Value.MinValue := Value.MinValue / 2.54;
       Value.MaxValue := Value.MaxValue / 2.54;
       Value.Value := TempValue / 2.54;
  end;
end; // TFormDocScan.ConvertToInch.


procedure TFormDocScan.ConvertToCm(Value : TmcmRealSpin);
var TempValue : double;
begin
  // Convert inch value cemtimeter. 
  if Assigned(Value)
  then begin
       TempValue := Value.Value;
       Value.MinValue := Value.MinValue * 2.54;
       Value.MaxValue := Value.MaxValue * 2.54;
       Value.Value := TempValue * 2.54;
  end;
end; // TFormDocScan.ConvertToCm.


procedure TFormDocScan.cbUnitsChange(Sender : TObject);
begin
  // User interface: User has changed the unit of resolution.
  if (FUnit <> integer(cbUnits.Items.Objects[cbUnits.ItemIndex]))
  then begin
       FUnit := integer(cbUnits.Items.Objects[cbUnits.ItemIndex]);
       if (FUnit = TWUN_INCHES)
       then begin
            ConvertToInch(rsLeft);
            ConvertToInch(rsTop);
            ConvertToInch(rsRight);
            ConvertToInch(rsBottom);
       end
       else begin
            ConvertToCm(rsLeft);
            ConvertToCm(rsTop);
            ConvertToCm(rsRight);
            ConvertToCm(rsBottom);
       end;
  end;
end; // TFormDocScan.cbUnitsChange.


procedure TFormDocScan.rsLeftChange(Sender : TObject);
var Value : double;
begin
  // User interface: User has changed the Left border.
  if (rsLeft.Text <> '') and Assigned(rsRight)
  then begin
       try
         Value := rsLeft.Value;
         rsRight.MinValue := Value + FxMin;
       except
       end;
  end;
end; // TFormDocScan.rsLeftChange.


procedure TFormDocScan.rsRightChange(Sender : TObject);
var Value : double;
begin
  // User interface: User has changed the Right border.
  if (rsRight.Text <> '') and Assigned(rsLeft)
  then begin
       try
         Value := rsRight.Value;
         rsLeft.MaxValue := Value - FxMin;
       except
       end;
  end;
end; // TFormDocScan.rsRightChange.


procedure TFormDocScan.rsTopChange(Sender : TObject);
var Value : double;
begin
  // User interface: User has changed the Top border.
  if (rsTop.Text <> '') and Assigned(rsBottom)
  then begin
       try
         Value := rsTop.Value;
         rsBottom.MinValue := Value + FyMin;
       except
       end;
  end;
end; // TFormDocScan.rsTopChange.


procedure TFormDocScan.rsBottomChange(Sender : TObject);
var Value : double;
begin
  // User interface: User has changed the Bottom border.
  if (rsBottom.Text <> '') and Assigned(rsTop)
  then begin
       try
         Value := rsBottom.Value;
         rsTop.MaxValue := Value - FyMin;
       except
       end;
  end;
end; // TFormDocScan.rsBottomChange.


procedure TFormDocScan.cbEnableADFClick(Sender : TObject);
begin
  // User interface: User has enabled/disabled the ADF.
  if cbEnableADF.Checked
  then cbEnableDuplex.Enabled := FDuplex // Enable Duplex checkbox if supported.
  else cbEnableDuplex.Enabled := False;
end; // TFormDocScan.cbEnableADFClick.


procedure TFormDocScan.cbXferMechanismChange(Sender : TObject);
begin
  // User interface: User has changed the transfer mechanism.
  if Assigned(mcmTWAIN) and Assigned(cbXferMechanism)
  then if (0 <= cbXferMechanism.ItemIndex)
       then mcmTWAIN.XferMech := TTwnXferType(cbXferMechanism.ItemIndex);
  
  case mcmTWAIN.XferMech of
  TWFX_NATIVE : mcmTWAIN.DIBHandleType := THDT_DIBSEC;
  TWFX_FILES  : begin
                  mcmTWAIN.FileFormat := TWFF_TIFFMULTI;
                  mcmTWAIN.FileFormats := [TWFF_TIFFMULTI, TWFF_TIFF];
                  mcmTWAIN.DIBHandleType := THDT_DIBSEC;
                end;
  TWFX_MEMORY : mcmTWAIN.DIBHandleType := THDT_MEMPTR;
  end;
end; // TFormDocScan.cbXferMechanismChange.


procedure TFormDocScan.WMSaveImage(var Msg : TMessage);
//var Msg : TMessage;
begin
  (*
  // When using AutoScan we need to return the synchronized call to
  // mcmTWAINMemXferBuffer or mcmTWAINImageReady from TmcmTWAIN and theirby the
  // TWAIN driver to be ready to receive new image data.
  // If we do not return the synchronized call as fast as possible the TWAIN
  // drivers buffer may over-run causing the acquisition of additional pages to
  // stop.
  // Therefore, we copy the image and post a message to our self (this thread)
  // which actually saves he image.
  WMSaveImage(Msg);
  //  PostMessage(Handle, WM_SAVEIMAGE, 0, 0);
  *)
  SaveImage;
end; // TFormDocScan.WMSaveImage.


procedure TFormDocScan.SaveImage;
var Name : string;
    Ext  : string;
    // Used with example below storing to a TMemoryStream.
    // MemoryStream : TMemoryStream;
    // Stream       : TFileStream;
begin
  FSaveImage.Assign(ImageCtrl.Image);
  
  // Set TIFF compression.
  case FSaveImage.BitCount of
  1  : FSaveImage.Compression := CP_CCITTGROUP4;
  4,
  8,
  24,
  32 : FSaveImage.Compression := CP_LZW;
       // FSaveImage.Compression := CP_PACKBITS; // Could be CP_NONE
  else ;
  end;

  // Add information about image - NOTE THIS is not necessary.
  FSaveImage.ImageInfo.Software := 'DocScan';
  FSaveImage.ImageInfo.Copyright := 'MCM DESIGN';
  FSaveImage.ImageInfo.DocumentName := FFileName;
  FSaveImage.ImageInfo.PageName     := 'Page ' + IntToStr(FImagesScanned);
  FSaveImage.ImageInfo.Description  := mcmTWAIN.SourceInfo.Manufacturer + chr($0D) +
                                       mcmTWAIN.SourceInfo.ProductName + ', ' +
                                       mcmTWAIN.SourceInfo.Version + chr($0D) +
                                       mcmTWAIN.SourceInfo.ProductFamily + chr($0D) +
                                       'Protocol ' + mcmTWAIN.SourceInfo.Protocol + chr($0D) +
                                       mcmTWAIN.SourceInfo.Info;

  if cbMultiTIFF.Checked
  then begin
       // Save document in a Multi paged TIF file.
       StatusBar.Panels[0].Text := 'Status: Saving image to ' + FFilename;
       if (FImagesScanned = 1)
       then FSaveImage.FileSave(FFileName)
       else FSaveImage.FileAppend(FFileName);
  end
  else begin
       // Save each scanned page in individual TIF files.
       // Change the filename, but re-use the extension.
       Ext  := ExtractFileExt(FFilename);
       Name := ChangeFileExt(FFilename, '');
       Name := Name + IntToStr(FImagesScanned) + Ext;
       StatusBar.Panels[0].Text := 'Status: Saving image to ' + Name;
       FSaveImage.FileSave(Name);
  end;

  //----------------------------------------------------------------------------
  // If you want to store the image in a database rather than in a file,
  // the code below show how to write to a memory stream.
  // We simply write the content of the TMemoryStream to disk just to complete
  // this example!
(*
  Stream := TFileStream.Create('.\memstream.tif', fmCreate);
  MemoryStream := TMemoryStream.Create;
  try
    // Write first image
    ImageFileManager.Compression := FSaveImage.Compression;
    ImageFileManager.SaveToStream(MemoryStream, FF_TIFF, FSaveImage.DibHandle);
    if (ImageFileManager.Error <> EC_OK)
    then Exit;

    MemoryStream.SaveToStream(Stream);
  finally
  end;
  Stream.Free;
  MemoryStream.Free;
*)

  FSaveImage.Clear;
end; // TFormDocScan.SaveImage.


procedure TFormDocScan.mcmTWAINDisableMenus(Sender : TObject);
begin
  PanelMain.Enabled := False;
  sbScan.Enabled := False;
  sbSelect.Enabled := False;
  sbClose.Enabled := False;
end; // TFormDocScan.mcmTWAINDisableMenus.


procedure TFormDocScan.mcmTWAINEnableMenus(Sender : TObject);
begin
  PanelMain.Enabled := True;
  sbScan.Enabled := True;
  sbSelect.Enabled := True;
  sbClose.Enabled := True;
  StatusBar.Panels[0].Text := 'Status: Ready';
end; // TFormDocScan.mcmTWAINEnableMenus.


procedure TFormDocScan.mcmTWAINCloseSource(Sender : TObject);
begin
  PanelMain.Enabled := True;
  sbScan.Enabled := True;
  sbSelect.Enabled := True;
  sbClose.Enabled := True;
  StatusBar.Panels[0].Text := 'Status: Source was closed.';
end; // TFormDocScan.mcmTWAINCloseSource.


procedure TFormDocScan.mcmTWAINDeviceNotReady(    Sender       : TObject;
                                              var DoOpenSource : Boolean);
begin
  StatusBar.Panels[0].Text := 'Status: Device not ready';

  // First we check if the device is on-line.
  case MessageDlg('Device is not attached, powered on or communicating! ' + chr($0D) +
                  'Check that your scanner is turned on and connected.'  + chr($0D) +
                  'Would you like to retry ?', mtConfirmation, [mbYes, mbNo], 0) of
  mrYes   : Sleep(500);
  mrNo    : DoOpenSource := False;
  end;
end; // TFormDocScan.mcmTWAINDeviceNotReady.


procedure TFormDocScan.GetScannerInfo;
begin
  StatusBar.Panels[0].Text := 'Status: Checking scanner';
  // This procedure will open the data source without showing the user interface
  // or scanning.
  // The OnNegotiation event procedure is exchanged with GetNegotiate that only
  // obtains the required information from the data source.
  Screen.Cursor := crHourglass;
  try
    mcmTWAIN.OnNegotiation := GetNegotiate;
    mcmTWAIN.OpenSourceMgr;
    mcmTWAIN.OpenSource('');
    Update;
    mcmTWAIN.CloseSource;
    mcmTWAIN.GetDefaultSource(FSourceName);
    mcmTWAIN.CloseSourceMgr;
    mcmTWAIN.OnNegotiation := mcmTWAINNegotiation;
  except
  end;
  Screen.Cursor := crArrow;
  StatusBar.Panels[0].Text := 'Status: Ready';
end; // TFormDocScan.GetScannerInfo.


procedure TFormDocScan.GetNegotiate(Sender : TObject; var DoCancel : boolean);
var Container   : TtwnContainer;
    i           : integer;
    DefaultInt  : integer;
    ImageLayout : TImageLayout;
    TempRes     : double;
begin
  // Check ADF
  if mcmTWAIN.FeederEnabled(True)
  then begin
       cbEnableADF.Enabled := True;
       cbEnableADF.Checked := True;
  end
  else begin
       Container := mcmTWAIN.Containers.Items[CAP_FEEDERENABLED];
       if Assigned(Container)
       then cbEnableADF.Enabled := Container.CurrentValue
       else cbEnableADF.Enabled := False;
  end;

  // Check duplex mode.
  cbEnableDuplex.Enabled := False;
  FDuplex := False;
  if cbEnableADF.Enabled
  then begin
       if (mcmTWAIN.Duplex <> TWDX_NONE)
       then begin
            FDuplex := True; // Save this info for use with user interface only.
            cbEnableDuplex.Enabled := True;
            cbEnableDuplex.Checked := True;
       end;
  end;

  // Check color modes
  cbBitReduce.Clear;
  cbColor.Clear;
  cbColor.Tag := 0;
  if mcmTWAIN.IsCapSupported(ICAP_PIXELTYPE)
  then begin
       mcmTWAIN.PixelType := FColorFormat;
       FColorFormat := mcmTWAIN.PixelType;
       Container := mcmTWAIN.Containers.Items[ICAP_PIXELTYPE];
       if Assigned(Container)
       then begin
            cbColor.Tag := 1;
            for i := 0 to (Container.NumItems - 1)
            do begin
               case Container.Items[i] of
               TWPT_BW      : cbColor.Items.AddObject('BW', Pointer(TWPT_BW));
               TWPT_GRAY    : cbColor.Items.AddObject('Grey', Pointer(TWPT_GRAY));
               TWPT_RGB     : cbColor.Items.AddObject('RGB', Pointer(TWPT_RGB));
               TWPT_PALETTE : cbColor.Items.AddObject('Palette', Pointer(TWPT_PALETTE));
               end;
               if (Container.Items[i] = FColorFormat)
               then cbColor.ItemIndex := i;
            end;
       end;
       cbColor.Enabled := True;
       lPixelType.Enabled := cbColor.Enabled;
       cbColorChange(Self);
  end
  else begin
       cbColor.Items.AddObject('None', Pointer($FFFF));
       cbColor.ItemIndex := 0;
       cbColor.Enabled := False;
       lPixelType.Enabled := cbColor.Enabled;
  end;

  // Get/Set current pixel flavor.
  if (mcmTWAIN.PixelFlavor <> TWPF_CHOCOLATE)
  then mcmTWAIN.PixelFlavor := TWPF_CHOCOLATE;

  // Check & Get bit depth reduction modes.
  cbBitReduce.Tag := 0;
  if mcmTWAIN.IsCapSupported(ICAP_BITDEPTHREDUCTION)
  then begin
       Container := Nil;
       mcmTWAIN.GetCapabilityMsg(ICAP_BITDEPTHREDUCTION, MSG_GET, Container);
       if Assigned(Container)
       then FBitReduction := Container.CurrentValue;

       Container := mcmTWAIN.Containers.Items[ICAP_BITDEPTHREDUCTION];
       if Assigned(Container)
       then begin
            cbBitReduce.Tag := 1;
            for i := 0 to (Container.NumItems - 1)
            do begin
               case Container.Items[i] of
               TWBR_THRESHOLD    : cbBitReduce.Items.AddObject('Threshold', Pointer(TWBR_THRESHOLD));
               TWBR_HALFTONE     : cbBitReduce.Items.AddObject('Halftones', Pointer(TWBR_HALFTONE));
               //TWBR_CUSTHALFTONE : cbBitReduce.Items.AddObject('Custom Halftones', Pointer(TWBR_CUSTHALFTONE));
               TWBR_DIFFUSION    : cbBitReduce.Items.AddObject('Diffusion', Pointer(TWBR_DIFFUSION));
               end;
               if (Container.Items[i] = FBitReduction)
               then cbBitReduce.ItemIndex := i;
            end;
       end;
       if (FColorFormat <> TWPT_BW)
       then begin
            cbBitReduce.Items.AddObject('None', Pointer($FFFF));
            cbBitReduce.ItemIndex := cbBitReduce.Items.Count - 1;
       end;
       cbBitReduceChange(Self);
  end
  else begin
       FBitReduction := -1;
       cbBitReduce.Enabled := False;
       lBitReduce.Enabled := cbBitReduce.Enabled;
       cbBitReduce.Items.AddObject('None', Pointer($FFFF));
       cbBitReduce.ItemIndex := 0;
  end;

  // Get Threshold value.
  isThreshold.Tag := 0;
  if mcmTWAIN.IsCapSupported(ICAP_THRESHOLD)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_THRESHOLD, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := isThreshold.Value;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
            then begin
                 mcmTWAIN.GetCapabilityMsg(ICAP_THRESHOLD, MSG_GET, Container);
                 isThreshold.MaxValue := Container.MaxValue;
                 isThreshold.MinValue := Container.MinValue;
                 isThreshold.Value := Container.CurrentValue;
                 isThreshold.Tag := 1;
            end;
       end;
  end;
  if (FBitReduction = TWBR_THRESHOLD)
  then cbBitReduceChange(Self);

  // Check Auto-brightness
  if mcmTWAIN.IsCapSupported(ICAP_AUTOBRIGHT)
  then begin
       cbAutoBright.Enabled := True;
       cbAutoBright.Checked := mcmTWAIN.AutoBrightness;
  end
  else cbAutoBright.Enabled := False;

  // Check Brightness
  isBrightness.Enabled := False;
  if mcmTWAIN.IsCapSupported(ICAP_BRIGHTNESS)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_BRIGHTNESS, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := isBrightness.Value;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
            then begin
                 mcmTWAIN.GetCapabilityMsg(ICAP_BRIGHTNESS, MSG_GET, Container);
                 isBrightness.MaxValue := Round(Container.MaxValue);
                 isBrightness.MinValue := Round(Container.MinValue);
                 isBrightness.Value := Container.CurrentValue;
                 isBrightness.Tag := 1;
                 isBrightness.Enabled := True;
            end;
       end;
  end;
  lBrightness.Enabled := isBrightness.Enabled;

  // Check Contrast
  isContrast.Enabled := False;
  if mcmTWAIN.IsCapSupported(ICAP_CONTRAST)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_CONTRAST, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := isContrast.Value;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
            then begin
                 mcmTWAIN.GetCapabilityMsg(ICAP_CONTRAST, MSG_GET, Container);
                 isContrast.MaxValue := Round(Container.MaxValue);
                 isContrast.MinValue := Round(Container.MinValue);
                 isContrast.Value    := Container.CurrentValue;
                 isContrast.Tag := 1;
                 isContrast.Enabled := True;
            end;
       end;
  end;
  lContrast.Enabled := isContrast.Enabled;

  // Get supported page sizes.
  cbPaperSize.Clear;
  cbPaperSize.Sorted := False;
  cbPaperSize.Items.AddObject('None', Pointer(TWSS_NONE));
  cbPaperSize.ItemIndex := 0;
  cbPaperSize.Enabled := False;
  if mcmTWAIN.IsCapSupported(ICAP_SUPPORTEDSIZES)
  then begin
       DefaultInt := mcmTWAIN.PageType;
       Container := mcmTWAIN.Containers.Items[ICAP_SUPPORTEDSIZES];
       if Assigned(Container)
       then begin
            cbPaperSize.Enabled := True;
            for i := 0 to (Container.NumItems - 1)
            do begin
               case Container.Items[i] of
               //TWSS_NONE         : cbPaperSize.Items.AddObject('None', Pointer(TWSS_NONE));
               TWSS_USLETTER     : cbPaperSize.Items.AddObject('US Letter', Pointer(TWSS_USLETTER));
               TWSS_USLEGAL      : cbPaperSize.Items.AddObject('US Legal', Pointer(TWSS_USLEGAL));
               TWSS_A5           : cbPaperSize.Items.AddObject('A5', Pointer(TWSS_A5));
               TWSS_USLEDGER     : cbPaperSize.Items.AddObject('US Ledger', Pointer(TWSS_USLEDGER));
               TWSS_USEXECUTIVE  : cbPaperSize.Items.AddObject('US Executive', Pointer(TWSS_USEXECUTIVE));
               TWSS_A3           : cbPaperSize.Items.AddObject('A3', Pointer(TWSS_A3));
               TWSS_A6           : cbPaperSize.Items.AddObject('A6', Pointer(TWSS_A6));
               TWSS_C4           : cbPaperSize.Items.AddObject('C4', Pointer(TWSS_C4));
               TWSS_C5           : cbPaperSize.Items.AddObject('C5', Pointer(TWSS_C5));
               TWSS_C6           : cbPaperSize.Items.AddObject('C6', Pointer(TWSS_C6));
               TWSS_4A0          : cbPaperSize.Items.AddObject('4A0', Pointer(TWSS_4A0));
               TWSS_2A0          : cbPaperSize.Items.AddObject('2A0', Pointer(TWSS_2A0));
               TWSS_A0           : cbPaperSize.Items.AddObject('A0', Pointer(TWSS_A0));
               TWSS_A1           : cbPaperSize.Items.AddObject('A1', Pointer(TWSS_A1));
               TWSS_A2           : cbPaperSize.Items.AddObject('A2', Pointer(TWSS_A2));
               TWSS_A4           : cbPaperSize.Items.AddObject('A4', Pointer(TWSS_A4));
               TWSS_A7           : cbPaperSize.Items.AddObject('A7', Pointer(TWSS_A7));
               TWSS_A8           : cbPaperSize.Items.AddObject('A8', Pointer(TWSS_A8));
               TWSS_A9           : cbPaperSize.Items.AddObject('A9', Pointer(TWSS_A9));
               TWSS_A10          : cbPaperSize.Items.AddObject('A10', Pointer(TWSS_A10));
               TWSS_ISOB0        : cbPaperSize.Items.AddObject('ISO B0', Pointer(TWSS_ISOB0));
               TWSS_ISOB1        : cbPaperSize.Items.AddObject('ISO B1', Pointer(TWSS_ISOB1));
               TWSS_ISOB2        : cbPaperSize.Items.AddObject('ISO B2', Pointer(TWSS_ISOB2));
               TWSS_ISOB3        : cbPaperSize.Items.AddObject('ISO B3', Pointer(TWSS_ISOB3));
               TWSS_ISOB4        : cbPaperSize.Items.AddObject('ISO B4', Pointer(TWSS_ISOB4));
               TWSS_ISOB5        : cbPaperSize.Items.AddObject('ISO B5', Pointer(TWSS_ISOB5));
               TWSS_ISOB6        : cbPaperSize.Items.AddObject('ISO B6', Pointer(TWSS_ISOB6));
               TWSS_ISOB7        : cbPaperSize.Items.AddObject('ISO B7', Pointer(TWSS_ISOB7));
               TWSS_ISOB8        : cbPaperSize.Items.AddObject('ISO B8', Pointer(TWSS_ISOB8));
               TWSS_ISOB9        : cbPaperSize.Items.AddObject('ISO B9', Pointer(TWSS_ISOB9));
               TWSS_ISOB10       : cbPaperSize.Items.AddObject('ISO B10', Pointer(TWSS_ISOB10));
               TWSS_JISB0        : cbPaperSize.Items.AddObject('JIS B0', Pointer(TWSS_JISB0));
               TWSS_JISB1        : cbPaperSize.Items.AddObject('JIS B1', Pointer(TWSS_JISB1));
               TWSS_JISB2        : cbPaperSize.Items.AddObject('JIS B2', Pointer(TWSS_JISB2));
               TWSS_JISB3        : cbPaperSize.Items.AddObject('JIS B3', Pointer(TWSS_JISB3));
               TWSS_JISB4        : cbPaperSize.Items.AddObject('JIS B4', Pointer(TWSS_JISB4));
               TWSS_JISB5        : cbPaperSize.Items.AddObject('B5 Letter, JISB5', Pointer(TWSS_JISB5)); // B5 Letter
               TWSS_JISB6        : cbPaperSize.Items.AddObject('JIS B6', Pointer(TWSS_JISB6));
               TWSS_JISB7        : cbPaperSize.Items.AddObject('JIS B7', Pointer(TWSS_JISB7));
               TWSS_JISB8        : cbPaperSize.Items.AddObject('JIS B8', Pointer(TWSS_JISB8));
               TWSS_JISB9        : cbPaperSize.Items.AddObject('JIS B9', Pointer(TWSS_JISB9));
               TWSS_JISB10       : cbPaperSize.Items.AddObject('JIS B10', Pointer(TWSS_JISB10));
               TWSS_C0           : cbPaperSize.Items.AddObject('C0', Pointer(TWSS_C0));
               TWSS_C1           : cbPaperSize.Items.AddObject('C1', Pointer(TWSS_C1));
               TWSS_C2           : cbPaperSize.Items.AddObject('C2', Pointer(TWSS_C2));
               TWSS_C3           : cbPaperSize.Items.AddObject('C3', Pointer(TWSS_C3));
               TWSS_C7           : cbPaperSize.Items.AddObject('C7', Pointer(TWSS_C7));
               TWSS_C8           : cbPaperSize.Items.AddObject('C8', Pointer(TWSS_C8));
               TWSS_C9           : cbPaperSize.Items.AddObject('C9', Pointer(TWSS_C9));
               TWSS_C10          : cbPaperSize.Items.AddObject('C10', Pointer(TWSS_C10));
               TWSS_USSTATEMENT  : cbPaperSize.Items.AddObject('US Statement', Pointer(TWSS_USSTATEMENT));
               TWSS_BUSINESSCARD : cbPaperSize.Items.AddObject('Business Card', Pointer(TWSS_BUSINESSCARD));
               end;
               if (Container.Items[i] = DefaultInt)
               then cbPaperSize.ItemIndex := i;
            end;
       end;
  end;

  mcmTWAIN.Units := TWUN_INCHES;
  cbUnits.Clear;
  FUnit := mcmTWAIN.Units;
  if mcmTWAIN.IsCapSupported(ICAP_UNITS)
  then begin
       Container := mcmTWAIN.Containers.Items[ICAP_UNITS];
       if Assigned(Container)
       then begin
            for i := 0 to (Container.NumItems - 1)
            do begin
               case Container.Items[i] of
               TWUN_INCHES      : cbUnits.Items.AddObject('Inch', Pointer(TWUN_INCHES));
               TWUN_CENTIMETERS : cbUnits.Items.AddObject('Centimeter', Pointer(TWUN_CENTIMETERS));
               //TWUN_PIXELS     : cbUnits.Items.AddObject('Pixels', Pointer(TWUN_PIXELS));
               end;
               if (FUnit = Container.Items[i])
               then cbUnits.ItemIndex := i;
            end;
       end;
  end;

  mcmTWAIN.XResolution := FResolution;
  mcmTWAIN.YResolution := FResolution;
  
  cbResolution.Clear;
  cbResolution.Enabled := False;
  seResolution.Enabled := False;
  if mcmTWAIN.IsCapSupported(ICAP_XRESOLUTION)
  then begin
       TempRes := mcmTWAIN.XResolution;
       if (TempRes > 0.0)
       then FResolution := TempRes;
       Container := mcmTWAIN.Containers.Items[ICAP_XRESOLUTION];
       if Assigned(Container)
       then begin
            if (Container.ContainerType = TWON_ENUMERATION)
            then begin
                 lResolution.FocusControl := cbResolution;
                 seResolution.Visible := False;
                 cbResolution.Visible := True;
                 cbResolution.Enabled := True;
                 for i := 0 to (Container.NumItems - 1)
                 do begin
                    cbResolution.Items.Add(IntToStr(Container.Items[i]));
                    if (FResolution = Container.Items[i])
                    then cbResolution.ItemIndex := i;
                 end;
            end
            else begin
                 lResolution.FocusControl := seResolution;
                 cbResolution.Visible := False;
                 seResolution.Visible := True;
                 seResolution.Enabled := True;
                 if (Container.MinValue >= 0)
                 then seResolution.MinValue := Round(Container.MinValue)
                 else seResolution.MinValue := 0;
                 if (Container.MaxValue > 0)
                 then seResolution.MaxValue := Round(Container.MaxValue)
                 else seResolution.MaxValue := 1000;
                 if (FResolution > 0)
                 then seResolution.Value := Round(FResolution);
            end;
       end;
  end;

  // Get physical min and max size.
  FyMin := mcmTWAIN.MinimumHeight;
  if (FyMin < 0)
  then FyMin := 0;
  FxMin := mcmTWAIN.MinimumWidth;
  if (FxMin < 0)
  then FxMin := 0;
  FxMax := mcmTWAIN.PhysicalWidth;
  FyMax := mcmTWAIN.PhysicalHeight;

  // Get image layout.
  mcmTWAIN.ResetImageLayout;
  ImageLayout := mcmTWAIN.GetImageLayout;
  with ImageLayout
  do begin
     rsLeft.MinValue   := 0;
     rsTop.MinValue    := 0;
     rsRight.MinValue  := 0;
     rsBottom.MinValue := 0;
     rsLeft.MaxValue   := 10000;
     rsTop.MaxValue    := 10000;
     rsRight.MaxValue  := 10000;
     rsBottom.MaxValue := 10000;

     rsLeft.Value   := Frame.Left;
     rsTop.Value    := Frame.Top;
     rsRight.Value  := Frame.Right;
     rsBottom.Value := Frame.Bottom;
     if (FxMax <= 0.0)
     then FxMax := Frame.Right;
     if (FyMax <= 0.0)
     then FyMax := Frame.Bottom;
  end;
  rsRight.MaxValue  := FxMax;
  rsBottom.MaxValue := FyMax;
end; // TFormDocScan.GetNegotiate.


procedure TFormDocScan.mcmTWAINNegotiation(Sender : TObject; var DoCancel : boolean);
var i             : integer;
    r             : double;
    TempRes       : double;
    xRes, yRes    : double;
    ImageLayout   : TImageLayout;
    Container     : TtwnContainer;
begin
  StatusBar.Panels[0].Text := 'Status: Preparing';

//------------------------------------------------------------------------------
// Negotiate capabilities.
// -----------------------
// This event is fired when the Data Source is opened, but before it becomes
// enabled.
// When you negotiate capabilities, you should alway check that the capability
// is supported by calling IsCapSupported(CAP_xxxx).
// Unfortunatly some data sources promis just a bit more than they realy can
// live up. Therefore it's a fairly good idear to GET the capability before
// SETting it.
//
// Below, you'll find many examples on how to change the data source settings.
// You do not have to implement all or any of these negotiations, just the ones
// required by your application. But remember that some capabilities do depend
// on others.
//------------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // Feeder capabilities.

  // Is Feeder selected in the menu, then enable it.
  // Call FeederEnabled to enable/disable the feeder.
  if mcmTWAIN.FeederEnabled(cbEnableADF.Checked)
  then FADFAvailable := True
  else begin
       Container := mcmTWAIN.Containers.Items[CAP_FEEDERENABLED];
       if Assigned(Container)
       then FADFAvailable := Container.CurrentValue
       else FADFAvailable := False;
  end;

  if cbEnableADF.Enabled and cbEnableADF.Checked
  then begin
       if mcmTWAIN.PaperDetectable or Not(mcmTWAIN.IsCapSupported(CAP_PAPERDETECTABLE))
       then if Not(mcmTWAIN.FeederLoaded)
            then begin
                 // Inform user that feeder is not loaded.
                 ShowMessage('Could not detect paper in feeder.');
                 DoCancel := True;
                 // Could loop until paper is inserted or quit
                 // acquisition!
                 Exit;
            end;

       // Enable auto-feed.
       FAutoFeed := mcmTWAIN.AutoFeed(True);

       // Check for Duplex scan unit, if found then enable it.
       if (mcmTWAIN.Duplex <> TWDX_NONE)
       then mcmTWAIN.DuplexEnabled := cbEnableDuplex.Checked;
  end;


  //----------------------------------------------------------------------------
  // Negotiate dimension unit.

  // Get/Set current unit.
  if (mcmTWAIN.Units <> FUnit)
  then mcmTWAIN.Units := FUnit;

  if (mcmTWAIN.Units = TWUN_INCHES)
  then TempRes := FResolution
  else TempRes := FResolution / 2.54;

  // Get native resolution (Devices optical resolution).
  // Use this and set to XResolution & YResolution if f. ex. you want an image
  // recorded with the scanners native/optical resolution.
  //xRes := mcmTWAIN.NativeXResolution;
  //yRes := mcmTWAIN.NativeYResolution;

  // Get physical max size.
  FxMax := mcmTWAIN.PhysicalWidth;
  FyMax := mcmTWAIN.PhysicalHeight;

  // Get scanners/cameras minimum scan height and width.
  if mcmTWAIN.IsCapSupported(ICAP_MINIMUMHEIGHT)
  then FyMin := mcmTWAIN.MinimumHeight
  else FyMin := 0;
  if (FyMin < 0)
  then FyMin := 0;

  if mcmTWAIN.IsCapSupported(ICAP_MINIMUMWIDTH)
  then FxMin := mcmTWAIN.MinimumWidth
  else FxMin := 0;
  if (FxMin < 0)
  then FxMin := 0;

  //----------------------------------------------------------------------------
  // Negotiate Color.

  // Get/Set current bit order.
  if (mcmTWAIN.BitOrder <> TWBO_MSBFIRST)
  then mcmTWAIN.BitOrder := TWBO_MSBFIRST;

  // Get/Set current pixel type.
  // FColorFormat := TWPT_RGB; //TWPT_BW; // TWPT_RGB; Value set via menu!
  if mcmTWAIN.IsCapSupported(ICAP_PIXELTYPE)
  then begin
       if (mcmTWAIN.PixelType <> FColorFormat)
       then begin
            mcmTWAIN.PixelType := FColorFormat;
            FColorFormat := mcmTWAIN.PixelType;
       end;
  end;

  //----------------------------------------------------------------------------
  // Get/Set current bit depth.
  // The number of bits per channel, are the total bits for a pixel (color).
  // TWPC_BW      -> ICAP_BITDEPTH = 1
  // TWPC_GRAY    -> ICAP_BITDEPTH = 4, 8 (maybe higher, if source supports this).
  // TWPC_PALETTE -> ICAP_BITDEPTH = 4 or 8.
  // TWPC_RGB     -> ICAP_BITDEPTH = 8 (maybe higher, if source supports this).
  //                                 There are some drivers that expect 24
  FBitDepth := mcmTWAIN.BitDepth;
  case FColorFormat of
  TWPT_BW      : begin
                   if (FBitDepth <> 1)
                   then begin
                        mcmTWAIN.BitDepth := 1;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  TWPT_GRAY,
  TWPT_PALETTE : begin
                   if (FBitDepth <> 8)
                   then begin
                        mcmTWAIN.BitDepth := 8;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  TWPT_RGB     : begin
                   if (FBitDepth = 24)
                   then FBitDepth := 8; // Should be 8 (bits per channel).
                   if (FBitDepth <> 8)
                   then begin
                        mcmTWAIN.BitDepth := 8;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  end;

  // Did we get the requested bit depth ?
  if (FBitDepth in [1,8])
  then ; //

  // Get/Set current pixel flavor.
  if (mcmTWAIN.PixelFlavor <> TWPF_CHOCOLATE)
  then mcmTWAIN.PixelFlavor := TWPF_CHOCOLATE;

  // RGB Pixel data arrangement.
  if (FColorFormat = TWPT_RGB)
  then begin
       // We want the bitmap data returned as "RGBRGB..." not "RRR..GGG..BBB..".
       if (mcmTWAIN.PlanarChunky <> TWPC_CHUNKY)
       then mcmTWAIN.PlanarChunky := TWPC_CHUNKY;
  end;

  if (FColorFormat in [TWPT_BW{,TWPT_GRAY,TWPT_PALETTE}])
  then begin
       // Most commenly used with Black & White images.
       // Therefore, support for TWPT_GRAY and TWPT_PALETTE may not be available!
       if mcmTWAIN.IsCapSupported(ICAP_BITDEPTHREDUCTION)
       then begin
            Container := Nil;
            if (mcmTWAIN.GetCapabilityMsg(ICAP_BITDEPTHREDUCTION, MSG_GET, Container) = TWRC_SUCCESS)
            then begin
                 if (FBitReduction <> Container.CurrentValue)
                 then begin
                      Container.CurrentValue := FBitReduction;
                      if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                      then mcmTWAIN.GetCapabilityMsg(ICAP_BITDEPTHREDUCTION, MSG_GET, Container);
                      FBitReduction := Container.CurrentValue;
                 end;
            end;
       end;

       // Threshold or Half-tones? Above Threshold was selected!
       case FBitReduction of
       TWBR_THRESHOLD    : if mcmTWAIN.IsCapSupported(ICAP_THRESHOLD)
                           then begin // Threshold method was selected.
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_THRESHOLD, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     if (Container.CurrentValue <> isThreshold.Value)
                                     then begin
                                          Container.CurrentValue := isThreshold.Value;
                                          if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                          then ; // Current value was changed to default item in the list.
                                     end;
                                end;
                           end;
       TWBR_HALFTONE     : if mcmTWAIN.IsCapSupported(ICAP_HALFTONES)
                           then begin // Half tone method was selected.
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_HALFTONES, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     // The current halftone is available through Container.CurrentValue.
                                     if (Container.NumItems >= 1)
                                     then begin
                                          // To iterate through items in a container,
                                          //for i := 0 to (Container.NumItems - 1)
                                          //do ShowMessage('Item[' + IntToStr(i) + '] := ' + Container.Items[i]);
                                     end;
                                     // We'll selected the source default.
                                     if (Container.CurrentIndex <> Container.DefaultIndex)
                                     then begin
                                          Container.CurrentIndex := Container.DefaultIndex;
                                          if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                          then ; // Current value was changed to default item in the list.
                                     end;
                                end;
                           end;
       TWBR_CUSTHALFTONE : ;
       TWBR_DIFFUSION    : ;
       end;
  end;

  //----------------------------------------------------------------------------
  // Negotiate dimensions.

  // Get X resolution.
  mcmTWAIN.XResolution := TempRes;
  xRes := mcmTWAIN.XResolution;
  if (xRes <> TempRes)
  then begin
       // Lets check that the resolution is in range.
       Container := mcmTWAIN.Containers.Items[ICAP_XRESOLUTION];
       if Assigned(Container)
       then if (Container.ContainerType <> TWON_ONEVALUE)
            then begin
                 if (Container.MinValue <= TempRes) and (TempRes <= Container.MaxValue)
                 then mcmTWAIN.XResolution := TempRes // Our choice was OK
                 else if (Container.MinValue > TempRes)
                      then mcmTWAIN.XResolution := Container.MinValue // Nop, it's too small
                      else if (TempRes > Container.MaxValue)
                           then mcmTWAIN.XResolution := Container.MaxValue; // Nop, it's too big
            end
            else mcmTWAIN.XResolution := TempRes;
       xRes := mcmTwain.XResolution;
       if (xRes = -1)
       then begin
            Container := Nil;
            mcmTWAIN.GetCapabilityMsg(ICAP_XRESOLUTION, MSG_RESET, Container);
            //if (Container <> Nil)
            //then xRes := Container.CurrentValue;
       end;

       // Had we just set "Resolution" to mcmTWAIN.XResolution and this value was
       // outside the range, mcmTWAIN would override the choice and use the
       // CurrentValue. Note that the same goes of all capabilities negotiated.
  end;

  // Get Y resolution.
  mcmTWAIN.YResolution := TempRes;
  yRes := mcmTWAIN.YResolution;
  if (yRes <> TempRes)
  then begin
       // Lets check that the resolution is in range.
       Container := mcmTWAIN.Containers.Items[ICAP_YRESOLUTION];
       if Assigned(Container)
       then if (Container.ContainerType <> TWON_ONEVALUE)
            then begin
                 if (Container.MinValue <= TempRes) and (TempRes <= Container.MaxValue)
                 then mcmTWAIN.YResolution := TempRes // Our choice was OK
                 else if (Container.MinValue > TempRes)
                      then mcmTWAIN.YResolution := Container.MinValue // Nop, it's too small
                      else if (TempRes > Container.MaxValue)
                           then mcmTWAIN.YResolution := Container.MaxValue; // Nop, it's too big
            end
            else mcmTWAIN.YResolution := TempRes;
       yRes := mcmTwain.YResolution;
       if (yRes = -1)
       then begin
            Container := Nil;
            mcmTWAIN.GetCapabilityMsg(ICAP_YRESOLUTION, MSG_RESET, Container);
            //if (Container <> Nil)
            //then yRes := Container.CurrentValue;
       end;
  end;
  Container := Nil;

  // Negotiate X & Y Scaling to default 1.0.
  if (mcmTWAIN.XScaling <> 1.0)
  then mcmTWAIN.XScaling := 1.0;
  if (mcmTWAIN.YScaling <> 1.0)
  then mcmTWAIN.YScaling := 1.0;

  //----------------------------------------------------------------------------
  // NOTE: If required negotiate Zoom factor here.


  //----------------------------------------------------------------------------
  // Negotiate Auto Brightness.
  if cbAutoBright.Enabled
  then mcmTWAIN.AutoBrightness := cbAutoBright.Checked;

  //----------------------------------------------------------------------------
  // NOTE: If required negotiate Brightness, Contract etc. here
  if Not(mcmTWAIN.AutoBrightness) and isBrightness.Enabled
  then mcmTWAIN.Brightness := isBrightness.Value;

  if isContrast.Enabled
  then mcmTWAIN.Contrast := isContrast.Value;

  //----------------------------------------------------------------------------
  // We'll disable undefined images size, automatic boarder detection, rotate
  // and deskew!

  if mcmTwain.IsCapSupported(ICAP_UNDEFINEDIMAGESIZE)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_UNDEFINEDIMAGESIZE, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := False;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
            then { An Error occured. }
            else begin
                 if (mcmTWAIN.GetCapabilityMsg(ICAP_UNDEFINEDIMAGESIZE, MSG_GET, Container) = TWRC_SUCCESS)
                 then begin
                      if (Container.CurrentValue = True)
                      then begin
                           if mcmTwain.IsCapSupported(ICAP_AUTOMATICBORDERDETECTION)
                           then begin
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICBORDERDETECTION, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     Container.CurrentValue := False;
                                     if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                     then { An Error occured. } ;
                                end;
                           end;
                      end;
                 end;
            end;
       end;
  end;

  if mcmTwain.IsCapSupported(ICAP_AUTOMATICDESKEW)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICDESKEW, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> False)
            then begin
                 Container.CurrentValue := False;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
  end;

  if mcmTwain.IsCapSupported(ICAP_AUTOMATICROTATE)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICROTATE, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> False)
            then begin
                 Container.CurrentValue := False;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
  end;

  //----------------------------------------------------------------------------
  // Negotiate paper size to an A4.
  if (mcmTWAIN.PageType <> FPaperSize)  // See TWAIN.PAS for other fixed sizes.
  then mcmTWAIN.PageType := FPaperSize; // f.ex. TWSS_USLETTER, TWSS_USLEGAL
  i := mcmTWAIN.PageType;

  //----------------------------------------------------------------------------
  // Negotiate the Image layout her !
  // Remember to negotiate Unit and Resolution first, as above.

  // You should do this only after negotiating ADF, Units and resolution and
  // color.
  // We are requesting the a quater of the maximum area.

  if (i = 0) or (i = -1)
  then begin
       ImageLayout := mcmTWAIN.GetImageLayout;
       with ImageLayout
       do begin
          // Set frame layout to an A4 size
          Frame.Left   := rsLeft.Value;
          Frame.Top    := rsTop.Value;
          Frame.Right  := rsRight.Value;
          Frame.Bottom := rsBottom.Value;

          if (FyMin < (Frame.Bottom - Frame.Top))
          then ; // Correct for scanners minimum scan height!

          if (FxMin < (Frame.Right - Frame.Left))
          then ; // Correct for scanners minimum scan height!

          // You could use xMax and yMax inquired earlier to ensure that the
          // requested frame is within limits.
          if (FxMax > 0.0)
          then if (Frame.Right > FxMax)
               then Frame.Right := FxMax;
          if (FyMax > 0)
          then if (Frame.Bottom > FyMax)
               then Frame.Bottom := FyMax;
       end;
       mcmTWAIN.SetImageLayout(ImageLayout);
  end
  else ImageLayout := mcmTWAIN.GetImageLayout;

  //----------------------------------------------------------------------------
  // Negotiate Orientation / Rotation.

  // Negotiating Orientation and/or Rotation shall be done after considering
  // the DAT_IMAGELAYOUT and ICAP_FRAMES.
  i := mcmTWAIN.Orientation;
  if (i <> TWOR_ROT0)
  then mcmTWAIN.Orientation := TWOR_ROT0;

  if mcmTwain.IsCapSupported(ICAP_FLIPROTATION)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_FLIPROTATION, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> TWFR_BOOK) // Alternativ to TWFR_BOOK is TWFR_FANFOLD
            then begin
                 Container.CurrentValue := TWFR_BOOK;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
       Container := Nil;
  end;

  r := mcmTWAIN.Rotation;
  if (r <> 0.0)
  then mcmTWAIN.Rotation := 0.0;

  if FADFAvailable
  then begin
        // Control the number of pages to transfer. In the case transfer
        // we want all available images use "-1".
        mcmTWAIN.NumImagesToScan(FImagesToScan);

        FAutoScan := mcmTWAIN.AutoScan(True);
        if FAutoScan
        then begin
             mcmTWAIN.MaxBatchBuffers := 1;
             if (mcmTWAIN.MaxBatchBuffers > 0)
             then mcmTWAIN.ClearBatchBuffers := TWCB_CLEAR;
        end;
  end;
end; // TFormDocScan.mcmTWAINNegotiation.


procedure TFormDocScan.mcmTWAINImageReady(Sender   : TObject;
                                          pBmp     : Pointer;
                                          pBmpInfo : PBitmapInfo;
                                          {$IFNDEF VER100}
                                          hImage   : hBitmap;
                                          {$ELSE}
                                          hImage   : Integer;
                                          {$ENDIF}
                                          FilePath : String);
begin
  if (hImage <> 0)
  then begin
       ImageCtrl.Image.DibHandle := hImage;
       if Assigned(pBmpInfo)
       then begin // Only if TmcmTWAIN.DIBHandleType = THDT_DIBSEC.
            ImageCtrl.Image.XResolution := pBmpInfo^.bmiHeader.biXPelsPerMeter;
            ImageCtrl.Image.YResolution := pBmpInfo^.bmiHeader.biYPelsPerMeter;
       end;

       inc(FImagesScanned);

       // Let's save the image.
       if cbSaveImage.Checked
       then begin
            if (StrPos(PChar(mcmTWAIN.SourceInfo.ProductName), 'Canon') <> Nil)
            then PostMessage(Handle, WM_SAVEIMAGE, 0, 0) // Fix for Canon Scanners.
            else SaveImage;
       end;
  end
  else if (FilePath <> '')
       then inc(FImagesScanned);

  if (FImagesScanned < FImagesToScan) or (FImagesToScan = -1)
  then StatusBar.Panels[0].Text := 'Status: Page ' + IntToStr(FImagesScanned) +
                                   ' of ' + IntToStr(FImagesToScan)
  else StatusBar.Panels[0].Text := 'Status: Scan complete';
end; // TFormDocScan.mcmTWAINImageReady.


procedure TFormDocScan.mcmTWAINXferReady(Sender : TObject);
var ImageInfo : TImageInfo;
begin
//------------------------------------------------------------------------------
// Image transfer is ready.
// ------------------------
// This event is fired just before the data source begins to transfer an image.
// Use this event to obtain information on the image about to be transferred.
//------------------------------------------------------------------------------
  if (mcmTWAIN.XferMech = TWFX_MEMORY)
  then begin
       // Get and set image information.
       ImageInfo := mcmTWAIN.GetImageInfo;
       if (ImageCtrl.Image.Width <> ImageInfo.ImageWidth)
       then ImageCtrl.Image.Width := ImageInfo.ImageWidth;
       if (ImageCtrl.Image.Height <> ImageInfo.ImageLength)
       then ImageCtrl.Image.Height := ImageInfo.ImageLength;
       if (ImageCtrl.Image.BitCount <> word(ImageInfo.BitsPerPixel))
       then ImageCtrl.Image.BitCount := word(ImageInfo.BitsPerPixel);
  end;
end; // TFormDocScan.mcmTWAINXferReady.


procedure TFormDocScan.mcmTWAINMemXferSize(    Sender   : TObject;
                                               MinSize,
                                               MaxSize  : Integer;
                                           var BufSize  : Integer;
                                               pBmpInfo : PBitmapInfo);
var LongWidth  : integer;
    BufferSize : integer;
    FNoLines   : integer;
begin
  // Set-up number of lines/bytes to (Memory) transfer in each image chunk.
  with pBmpInfo^.bmiHeader
  do LongWidth := (((Longint(biWidth * biBitCount) + 31) div 32) * 4);
  FNoLines  := pBmpInfo^.bmiHeader.biHeight div 4; // Number of lines to transfer.
  BufferSize := FNoLines * LongWidth;

  // Validate that BufferSize satisfy the condition specified by the data source,
  // i.e. MinSize <= BufferSize <= MaxSize.
  if (BufferSize < MinSize)
  then begin
       FNoLines := Trunc(0.9999 + MinSize / LongWidth);
       BufferSize := FNoLines * LongWidth;
  end;

  if (BufferSize > MaxSize) and (MaxSize <> integer(TWON_DONTCARE32))
  then begin
       FNoLines := Trunc(MaxSize / LongWidth);
       BufferSize := FNoLines * LongWidth;
  end;

  // Create a transfer buffer.
  if (FBuffer <> Nil)
  then FreeMem(FBuffer);
  GetMem(FBuffer, BufferSize);

  BufSize := BufferSize;
end; // TFormDocScan.mcmTWAINMemXferSize.


procedure TFormDocScan.mcmTWAINMemXferBuffer(    Sender   : TObject;
                                                 pBmpInfo : PBitmapInfo;
                                                 BytesPerRow,
                                                 Rows,
                                                 DataSize : Integer;
                                             var pData    : Pointer);
var i          : integer;
    pSource    : PAnsiChar;
    pDest      : PAnsiChar;
    NoColors   : integer;
    LogPalette : PLogPalette;
begin
//------------------------------------------------------------------------------
// Memory buffer ready.
// --------------------
// This event is fired when the data source has filled the buffer with image
// data. This event is also called once before each new image transfer begins to
// obtain the initial pointer to the allocated buffer (see above).
//
// When pData equals NIL this event marks the beginning of the transfer. It is
// your responsibility to assign pData to point to a valid memory buffer.
//
// When DataSize = 0 no more chucks from the current image is returned.
//
// During the transfer you can change the buffer into which the next chunk is
// returned. Simply change where pData points to. Make sure that the new memory
// area is valid and large enough to receive the negotiated number of bytes.
//
// If pData is set to NIL during an image transfer, the current transfer is
// skipped. If additional "images" are available to the data source, the next
// "image" will be scanned.
//------------------------------------------------------------------------------
  if (pData = Nil)
  then begin // Start of image transfer. Assign initial memory area to pData
       if (ImageCtrl.Image.Width <> pBmpInfo^.bmiHeader.biWidth)
       then ImageCtrl.Image.Width := pBmpInfo^.bmiHeader.biWidth;
       if (ImageCtrl.Image.Height <> pBmpInfo^.bmiHeader.biHeight)
       then ImageCtrl.Image.Height := pBmpInfo^.bmiHeader.biHeight;
       if (ImageCtrl.Image.BitCount <> pBmpInfo^.bmiHeader.biBitCount)
       then ImageCtrl.Image.BitCount := pBmpInfo^.bmiHeader.biBitCount;

       if (ImageCtrl.Image.BitCount <= 8)
       then begin
            NoColors := 1 shl ImageCtrl.Image.BitCount;
            GetMem(LogPalette, NoColors * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
            LogPalette^.palNumEntries := NoColors;
            if Assigned(LogPalette)
            then begin
                 for i := 0 to (NoColors - 1)
                 do begin
                    LogPalette^.palPalEntry[i].peRed   := pBmpInfo^.bmiColors[i].rgbRed;
                    LogPalette^.palPalEntry[i].peGreen := pBmpInfo^.bmiColors[i].rgbGreen;
                    LogPalette^.palPalEntry[i].peBlue  := pBmpInfo^.bmiColors[i].rgbBlue;
                    LogPalette^.palPalEntry[i].peFlags := 0;
                 end;
                 ImageCtrl.Image.SetPaletteEntries(LogPalette);
                 FreeMem(LogPalette);
            end;
       end;
       if (ImageCtrl.Image.ImageFormat = IF_BW)
       then ImageCtrl.Image.SetStretchMode(HALFTONE);
       ImageCtrl.Image.FillAll($FF);

       FThisLine := 0;
       pData := FBuffer;

       StatusBar.Panels[0].Text := 'Status: Page ' + IntToStr(FImagesScanned + 1) +
                                   ' of ' + IntToStr(FImagesToScan);
  end
  else begin
       if (DataSize <> 0)
       then begin // Incomming image data.
            // Image data is up-side-down, we therefore need to flip the image
            // lines.
            pSource := pData;
            for i := 0 to (Rows - 1)
            do begin
               pDest := ImageCtrl.Image.ScanLine[i+FThisLine];
               CopyMemory(pDest, pSource, BytesPerRow);
               // Increment pSource to point to the next line.
               pSource := PAnsiChar(integer(pData) + i * BytesPerRow);
            end;
            inc(FThisLine, Rows); // Number of image lines processed so far.

            ImageCtrl.Repaint;
            Update;
            StatusBar.Panels[0].Text := 'Status: Page ' + IntToStr(FImagesScanned + 1) +
                                        ' of ' + IntToStr(FImagesToScan) +
                                        ', ' + IntToStr(Round(100 * FThisLine / pBmpInfo^.bmiHeader.biHeight)) +
                                        '%';
       end
       else begin // DataSize = 0 specifies the end of image data.
            ImageCtrl.DrawImage;
            Update;
            ImageCtrl.Image.XResolution := pBmpInfo^.bmiHeader.biXPelsPerMeter;
            ImageCtrl.Image.YResolution := pBmpInfo^.bmiHeader.biYPelsPerMeter;

            // Make sure that the image was transferred in full.
            if (FThisLine = pBmpInfo^.bmiHeader.biHeight) or True
            then begin
                 inc(FImagesScanned);
                 // Now save the image.
                 if cbSaveImage.Checked
                 then begin
                      if (StrPos(PChar(mcmTWAIN.SourceInfo.ProductName), 'Canon') <> Nil)
                      then PostMessage(Handle, WM_SAVEIMAGE, 0, 0) // Fix for Canon Scanners.
                      else SaveImage;
                 end;
            end;
       end;

       if (FImagesScanned = FImagesToScan)
       then StatusBar.Panels[0].Text := 'Status: Scan complete';
  end;
end; // TFormDocScan.mcmTWAINMemXferBuffer.


procedure TFormDocScan.SetNextFilename;
var FImageFileName : string;
    FileExt        : string;
begin
   // Change the filename, but re-use the extension.
   FImageFileName := eFileName.Text;
   FileExt := ExtractFileExt(FImageFileName);
   FImageFileName := ChangeFileExt(FImageFileName, '');
   FImageFileName := FImageFileName + IntToStr(FImagesScanned  + 1) + FileExt;
   mcmTWAIN.Filename := FImageFileName;
end; // TFormDocScan.SetNextFilename.


procedure TFormDocScan.mcmTWAINXferNext(    Sender    : TObject;
                                        var NumImages : Integer;
                                        var SkipNext  : Boolean);
begin
  if (FImagesToScan = FImagesScanned)
  then NumImages := 0
  else begin
       //----------------------------------------------------------------------------
       // When transferring multiple images in file mode, change the file name here.
       // You should only do so, if the transfer is done without user interaction.
       // mcmTWAIN.Filename := '.\newname.ext';
       if (mcmTWAIN.XferMech = TWFX_FILES)  and ((NumImages > 0) or (NumImages = -1))
       then begin
            if (mcmTWAIN.FileFormat <> TWFF_TIFFMULTI)
            then SetNextFilename;
       end;
  end;
  PostMessage(Handle, WM_NEXTPAGE, integer(NumImages = 0), 0);
end; // TFormDocScan.mcmTWAINXferNext.


procedure TFormDocScan.WMNextPage(var Msg : TMessage);
var Reply : integer;
begin
  Msg.Result := 0;
  if (Msg.wParam <> 0) and (FImagesScanned < FImagesToScan)
  then begin
       Reply := mrAbort;
       if FADFAvailable and cbEnableADF.Checked
       then begin // Scanner has ADF
            repeat
              if mcmTWAIN.PaperDetectable or Not(mcmTWAIN.IsCapSupported(CAP_PAPERDETECTABLE))
              then begin
                   if Not(mcmTWAIN.FeederLoaded) or ((FThisLine = 0) and (Reply <> mrRetry))
                   then Reply := MessageDlg('Feeder is empty. Place the missing pages in the ADF and click Retry, or click abort to cancel', mtConfirmation, [mbRetry, mbAbort], 0)
                   else Reply := mrOk;
              end;
            until (Reply = mrOk) or (Reply = mrAbort);
       end
       else begin // Flatbed scanner
            Reply := MessageDlg('Place next page on the platen and click OK, or click abort to cancel', mtConfirmation, [mbOk, mbAbort], 0);
       end;
       if (Reply = mrOk)
       then begin
            mcmTWAIN.DisableSource;
            // sbScan.Enabled is enabled if the data source has been closed.
            // sbScan.Enabled is enabled in event mcmTWAINEnableMenus.
            if (sbScan.Enabled)
            then begin
                 // If the scannner/data source requested to be closed down,
                 // we'll have to open the data source again.
                 if mcmTWAIN.OpenSourceMgr
                 then mcmTWAIN.OpenSource('');
            end;
            mcmTWAIN.EnableSource(mcmTWAIN.ShowUI);
            Exit;
       end;
  end;

  if (FImagesScanned = FImagesToScan) or (Msg.wParam <> 0)
  then begin
       mcmTWAIN.DisableSource;
       mcmTWAIN.CloseSource;
       mcmTWAIN.CloseSourceMgr;
  end;
end; // TFormDocScan.WMNextPage.


procedure TFormDocScan.mcmTWAINDeviceEvent(Sender     : TObject;
                                           Event      : TTwnDeviceEvent;
                                           DeviceName : String;
                                           A, B, C    : Variant);
var MsgStr    : string;
    Container : TtwnContainer;
begin
//------------------------------------------------------------------------------
// Device Event.
// --------------------
// Depending on the "Event" that triggered mcmTWAINDeviceEvent to fire
// A, B and C represents different field of the original TW_DEVICEEVENT
// structure.
// Only TWDE_CHECKAUTOMATICCAPTURE, TWDE_CHECKBATTERY, TWDE_CHECKFLASH,
// TWDE_CHECKPOWERSUPPLY and TWDE_CHECKRESOLUTION returns values in A, B and C.
// For in-depth information please refer to the TWAIN Specifications (Look for
// CAP_DEVICEEVENT, TW_DEVICEEVENT and TWAIN Articals - Device Events) and view
// example code below.
//
// The Events which you want your application to receive from the source are
// specified in the property mcmTWAIN.DeviceEventTypes. By default this set
// is empty = [] resulting in no events.
//
// Note that all device events only occurres when the device status changes or
// as a result of user interaction.
// An event does(should) never occure as a result of the application changing
// properties in the TWAIN driver.
//------------------------------------------------------------------------------

  case Event of
  TWDE_CHECKDEVICEONLINE     : begin
                                 MsgStr := 'Device has been turned ';
                                 // Use CAP_DEVICEONLINE for more information.
                                 Container := Nil;
                                 if (mcmTWAIN.GetCapabilityMsg(CAP_DEVICEONLINE, MSG_GET, Container) = TWRC_SUCCESS)
                                 then begin
                                      if Container.CurrentValue
                                      then MsgStr := MsgStr + 'ON'
                                      else MsgStr := MsgStr + 'OFF';
                                 end
                                 else MsgStr := MsgStr + 'ON/OFF ?';
                               end;
  TWDE_CHECKRESOLUTION       : begin
                                 // A = double - Current X resolution.
                                 // B = double - Current Y resolution.
                                 MsgStr := 'Current resolution has been changed to' + chr($0D) +
                                           'X: ' + FloatToStrF(A, ffFixed, 8, 4) + chr($0D) +
                                           'Y: ' + FloatToStrF(B, ffFixed, 8, 4);
                                 // If necessary use ICAP_XRESOLUTION and
                                 // ICAP_XRESOLUTION.
                               end;
  TWDE_DEVICEADDED           : MsgStr := 'Device added (Memory card, etc.)';
  TWDE_DEVICEOFFLINE         : MsgStr := 'Device is unavailable';
  TWDE_DEVICEREADY           : MsgStr := 'Device is ready to acquire an image';
  TWDE_DEVICEREMOVED         : MsgStr := 'Device has been removed';
  TWDE_PAPERDOUBLEFEED       : MsgStr := 'Paper double feed';
  TWDE_PAPERJAM              : MsgStr := 'Paper jam.';
  TWDE_LAMPFAILURE           : MsgStr := 'Light source failed.';
  else MsgStr := 'Unknown Device Event!';
  end;

  // In this example it has been chosen to display the event-message.
  // In most cases you should not display a message but use the information
  // to obtain the desired functionality.
  MessageDlg(DeviceName + chr($0D) + 'fired the following event:' + chr($0D) +
             MsgStr, mtInformation, [mbOK], 0);
end; // TFormDocScan.mcmTWAINDeviceEvent.


procedure TFormDocScan.eFileNameChange(Sender : TObject);
begin
  FFileName := eFileName.Text;
end; // TFormDocScan.eFileNameChange.

end.
