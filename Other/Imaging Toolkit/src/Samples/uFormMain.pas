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
// $Log:  17605: uFormMain.pas 
//
//    Rev 1.47    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.46    01-03-2011 20:40:32  mcm    Version: IMG 3.4
//
//    Rev 1.45    25-10-2009 17:23:50  mcm    Version: IMG 3.3
// Support for Delphi 2010
//
//    Rev 1.44    11-08-2009 10:25:10  mcm
// Modified registry keys.
//
//    Rev 1.43    30-11-2008 12:49:18  mcm
// Delphi 2009
//
//    Rev 1.42    31-10-2007 20:22:12  mcm    Version: IMG 3.2
// Added inclusion for WMF file import - external file format.
//
//    Rev 1.41    20-08-2007 20:28:34  mcm
// Added support for Delphi 2007
//
//    Rev 1.40    05-11-2006 18:57:48  mcm    Version: IMG 3.1
// Added support for showing all images in a multi-paged file, in the image
// browser.
//
//    Rev 1.39    05-06-2006 22:47:00  mcm    Version: IMG 3.0
// Added EDM - Euclidean Distance Map.
//
//    Rev 1.38    19-03-2006 10:19:52  mcm    Version: IMG 2.16
// Fixed Help path.
//
//    Rev 1.37    04-03-2006 18:31:08  mcm    Version: IMG 2.16
// Added TFormCanvasSize.
//
//    Rev 1.36    22/02/2006 00:11:08  mcm
// Added FormPaintBox - paint tools window.
//
//    Rev 1.35    19/02/2006 11:56:46  mcm    Version: IMG 2.15
// Added New Image dialogue
//
//    Rev 1.34    29-01-2006 12:11:14  mcm    Version: IMG 2.14
// Improved support for transparency and alpha channel in 32 bit images.
// Improved Append image example.
//
//    Rev 1.33    22-12-2005 20:52:12  mcm    Version: IMG 2.12
// Delphi 2006 support.
//
//    Rev 1.32    22/11/2005 20:42:42  mcm    Version: IMG 2.10
// Added support for Shen-Castan edge filter.
//
//   Rev 1.31    23-08-2005 22:27:14  mcm    Version: IMG 2.9
// Modified to use UnregisterFileFormats instead of terminated method
// SetSupportedFormats.

//
//   Rev 1.30    04-08-2005 18:55:16  mcm    Version: IMG 2.9
// Completed drap & drop support using both the Delphi and Windows methods.

//
//   Rev 1.29    31-07-2005 21:34:30  mcm
// New Icons.
// Additional File drag & drop.

//
//   Rev 1.28    30/07/2005 12:27:04  mcm
// Drag & Drop continued.

//
//   Rev 1.27    24-07-2005 19:01:40  mcm    Version: IMG 2.9
// Added Drap & Drop support.

//
//   Rev 1.26    23-05-2005 22:07:20  mcm    Version: IMG 2.9
// Added Canny, Despeckle and Marr-Hildreth filters, 
// Added Trace threshold method.

//
//   Rev 1.25    19-02-2005 00:42:02  mcm
// Deskew modifications.

//
//   Rev 1.24    03-02-2005 21:20:22  mcm
// Added Deskew method.
// Added full support for BW images in TmcmImageTransform.

//
//   Rev 1.23    03-01-2005 18:31:30  mcm    Version: IMG 2.7
// Added support for Delphi 2005.

//
//   Rev 1.22    28-10-2004 19:44:58  mcm    Version: IMG 2.6
// Minor changes in Filter method.

//
//   Rev 1.21    02-09-2004 21:42:58  mcm    Version: IMG 2.6
// Added use of TmcmImage.MaxIntScale with Zoom edit control.
// Corrected a Child window zoom/focusing problem introduced in version 2.5.

//
//   Rev 1.20    27-07-2004 15:13:24  mcm    Version: IMG 2.5
// Corrected situations where Zooming variable (FScale) wasn't updated
// correctly, leading to incorrect reported mouse coordinates in statusbar.

//
//   Rev 1.19    09-07-2004 20:29:28  mcm

//
//   Rev 1.18    08-07-2004 23:05:28  mcm    Version: IMG 2.5
// Introduced the new TmcmOpenDialog & TmcmSaveDialog.

//
//   Rev 1.17    30-01-2004 20:49:06  mcm    Version: IMG 2.3
// Added Edit palette menu and dialogue.
// Added Profile speed button. 

//
//   Rev 1.16    24-11-2003 20:17:10  mcm

//
//   Rev 1.15    17-11-2003 10:12:06  mcm    Version: IMG 2.0
// Included file association support.

//
//   Rev 1.14    16-10-2003 11:35:06  mcm    Version: IMG 1.6

//
//   Rev 1.13    29-09-2003 18:54:04  mcm    Version: IMG 1.6
// Added support for file association.

//
//   Rev 1.12    25-09-2003 23:38:30  mcm    Version: IMG 1.5
// Included the Affine, Brightness & Contrast and Gamma dialogues.

//
//   Rev 1.11    15-06-2003 13:25:34  mcm    Version: IMG 1.3.4
// Included the TmcmImageMorph component.
// Improved the Rotate example with fixed 90, 180 and 270 degrees and the
// uFormRotate dialogue.

//
//   Rev 1.10    27-03-2003 16:22:40  mcm    Version: IMG 1.3.3

//
//   Rev 1.9    26-03-2003 09:20:46  mcm    Version: IMG 1.3.3
// Added code example to read and write multiple TIFF images.

//
//   Rev 1.8    11-03-2003 00:55:42  mcm    Version: IMG 1.3.2
// Added STI (Still Imaging)
// Modified the Threshold example to use the TmcmImageDualView.
// Modified the ConvertTo method to include Windows palette colors.

//
//   Rev 1.7    17-02-2003 10:33:20  mcm    Version: IMG 1.3
// Added the Luminance histogram equalize method.

//
//   Rev 1.6    05-02-03 16:53:00  mcm
// Disabled certain ImageFormats in some functions to avoid errors. 

//
//   Rev 1.5    29-01-2003 16:03:50  mcm
// Added Math dialogue
// 

//
//   Rev 1.4    27-01-2003 14:24:10  mcm
// Included the new mcmImageMath unit.
// Added new Copy & Past functions.
// Added support for 32K color image conversion.
// Added missing palette copy in RotateItemClick.

//
//   Rev 1.3    08-10-2002 12:58:56  mcm    Version: IMG 1.2

//
//   Rev 1.2    27-09-2002 13:34:08  mcm    Version: IMG 1.2
// Added Bayer filter.
// Added
// - SGI Image file support.
// - User defined filters.
// - more matrix filters: Degrain, Median max min, Median min max.   Unsharp
// mask.
// General improvement of this sample project.
// - Copy Section example.
//
//

//
//   Rev 1.1    08-08-2002 15:34:20  mcm    Version: IMG 1.1

//
//   Rev 1.0    27-05-2002 16:22:34  mcm

unit uFormMain;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, Dialogs,
      Menus, ToolWin, ComCtrls, StdCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ToolWin, Vcl.StdCtrls,
      Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ImgList,
     {$ENDIF}
     mcmFileDialogues,
     uChildWin,
     umcmIntE,
     uImgAbout,
     {$IFDEF VER200}   ImgList,      {$ENDIF} // DELPHI 2009  = VER200
     {$IFDEF VER210}   ImgList,      {$ENDIF} // DELPHI 2010  = VER210
     {$IFDEF VER220}   ImgList,      {$ENDIF} // DELPHI XE    = VER220
     mcmSTI, mcmTWAIN, mcmTWAINKernel, mcmTWAINIntf,
     mcmImageTypeDef,
     mcmImageResStr,
     mcmDragDrop,
     mcmImage,
     mcmImageFile,
     mcmPrinter,
     mcmRegion,
     mcmImageColor,
     mcmImageFilter,
     mcmImageTransform,
     mcmImageMath,
     mcmImageMorph,
     mcmWMFFile; // The unit containing the added WMF file format.

const WM_STIACQUIRE = WM_USER + 1;

var   MessageID : Integer;         

type
  TFormMain = class(TForm)//, IUnknown, IDropTarget)
  // NOTE: IUnknown and IDropTarget is used to enable objects being dragged from
  // other applications (Explorer etc.) onto this.  
    MainMenu           : TMainMenu;
    // File menu
    FileMenu           : TMenuItem;
    NewItem            : TMenuItem;
    OpenItem           : TMenuItem;
    SaveItem           : TMenuItem;
    AppendItem         : TMenuItem;
    BrowseItem         : TMenuItem;
    N1                 : TMenuItem;
    AcquireItem        : TMenuItem;
    SourceItem         : TMenuItem;
    N2                 : TMenuItem;
    PrintItem          : TMenuItem;
    PrintPreviewItem   : TMenuItem;
    PrinterSetupItem   : TMenuItem;
    N3                 : TMenuItem;
    PreferencesItem    : TMenuItem;
    AssociatefileItem  : TMenuItem;
    N8                 : TMenuItem;
    ExitItem           : TMenuItem;

    // Edit menu
    EditMenu           : TMenuItem;
    CopyItem           : TMenuItem;
    CopySectionItem    : TMenuItem;
    PasteItem          : TMenuItem;
    PasteNewImageItem  : TMenuItem;
    PasteAsSectionItem : TMenuItem;
    DuplicateItem      : TMenuItem;
    EmptyClipboardItem : TMenuItem;

    // Image menu
    ImageMenu          : TMenuItem;
    CanvasSizeItem     : TMenuItem;
    InformationItem    : TMenuItem;
    N9                 : TMenuItem;
    TransparentViewItem: TMenuItem;

    // Color menu
    ColorMenu          : TMenuItem;
    UsedColorsItem     : TMenuItem;
    PaletteMenu        : TMenuItem;
    EditPaletteItem    : TMenuItem;
    InvertPaletteItem  : TMenuItem;
    InvertItem         : TMenuItem;
    ThresholdItem      : TMenuItem;
    TuneMenu           : TMenuItem;
    BrightnessContrastItem: TMenuItem;
    GammaCorrectionItem: TMenuItem;
    N4                 : TMenuItem;
    ConvertToMenu      : TMenuItem;
    GreyScaleItem      : TMenuItem;
    N5                 : TMenuItem;
    InclWindowsPalItem : TMenuItem;
    DecColorDepthMenu  : TMenuItem;
    Dec2Colors         : TMenuItem;
    Dec16Colors        : TMenuItem;
    Dec256Colors       : TMenuItem;
    Dec32KColors       : TMenuItem;
    Dec24MColors       : TMenuItem;
    IncColorDepthMenu  : TMenuItem;
    Inc16Colors        : TMenuItem;
    Inc256Colors       : TMenuItem;
    Inc32KColors       : TMenuItem;
    Inc16MillColors    : TMenuItem;
    Inc16MillColors32  : TMenuItem;
    N6                 : TMenuItem;
    SplitColorMenu     : TMenuItem;
    CIEMenu            : TMenuItem;
    CIEXD65Item        : TMenuItem;
    CIEYD65Item        : TMenuItem;
    CIEZD65Item        : TMenuItem;
    CMYKMenu           : TMenuItem;
    CyanItem           : TMenuItem;
    MagentaItem        : TMenuItem;
    YellowItem         : TMenuItem;
    BlackItem          : TMenuItem;
    HSVMenu            : TMenuItem;
    HueItem            : TMenuItem;
    SaturationItem     : TMenuItem;
    ValueItem          : TMenuItem;
    RGBMenu            : TMenuItem;
    RedItem            : TMenuItem;
    GreenItem          : TMenuItem;
    BlueItem           : TMenuItem;
    AlphaItem          : TMenuItem;
    YCbCrMenu          : TMenuItem;
    YChannelItem       : TMenuItem;
    CbChannelItem      : TMenuItem;
    CrChannelItem      : TMenuItem;
    YIQMenu            : TMenuItem;
    NTSCYItem          : TMenuItem;
    IChannelItem       : TMenuItem;
    QChannelItem       : TMenuItem;
    YUVMenu            : TMenuItem;
    PALYItem           : TMenuItem;
    UChannelItem       : TMenuItem;
    VChannelItem       : TMenuItem;
    CombineColorMenu   : TMenuItem;
    CombineCIEItem     : TMenuItem;
    CombineCMYKItem    : TMenuItem;
    CombineHSVItem     : TMenuItem;
    CombineRGBItem     : TMenuItem;
    CombineYCbCrItem   : TMenuItem;
    CombineYIQCItem    : TMenuItem;
    CombineYUVItem     : TMenuItem;

    GreyToBayerItem    : TMenuItem;
    OddOddItem         : TMenuItem;
    OddEvenItem        : TMenuItem;
    EvenOddItem        : TMenuItem;
    EvenEvenItem       : TMenuItem;

    // Histogram menu
    HistogramMenu      : TMenuItem;
    HistogramItem      : TMenuItem;
    EqualizeItem       : TMenuItem;
    EqualizeLumItem    : TMenuItem;
    StretchColorItem   : TMenuItem;

    // Filter menu
    FilterMenu         : TMenuItem;
    FilterBrowser      : TMenuItem;
    UserDefFilterItem  : TMenuItem;
    SmoothingMenu      : TMenuItem;
    AverageItem        : TMenuItem;
    AverageHeavyItem   : TMenuItem;
    BlurItem           : TMenuItem;
    BlurHeavyItem      : TMenuItem;
    DegrainItem        : TMenuItem;
    GaussBlurItem      : TMenuItem;
    SmoothItem         : TMenuItem;
    SmoothCircluarItem : TMenuItem;
    SmoothConeItem     : TMenuItem;
    SmoothPyramidalItem: TMenuItem;
    SharpeningMenu     : TMenuItem;
    EdgeItem           : TMenuItem;
    EdgePrewittItem    : TMenuItem;
    HighpassItem       : TMenuItem;
    LaplacianItem      : TMenuItem;
    PrewittNWItem      : TMenuItem;
    PrewittSEItem      : TMenuItem;
    SharpenItem        : TMenuItem;
    SharpenheavyItem   : TMenuItem;
    SobelNWItem        : TMenuItem;
    SobelSEItem        : TMenuItem;
    // Filter, Special
    SpecialMenu        : TMenuItem;
    EmbossItem         : TMenuItem;
    MaximumItem        : TMenuItem;
    MaxMinItem         : TMenuItem;
    MedianItem         : TMenuItem;
    MinimumItem        : TMenuItem;
    MosaicItem         : TMenuItem;
    LineMaskHorzItem   : TMenuItem;
    LineMaskVertItem   : TMenuItem;
    UnsharpMaskItem    : TMenuItem;
    // Filter, documents
    DocumentMenu       : TMenuItem;
    DespecklelightItem : TMenuItem;
    DespeckleItem      : TMenuItem;
    DespeckleAltItem   : TMenuItem;
    // Specialised Edge filters
    EdgeMenu           : TMenuItem;
    MarrHildreth       : TMenuItem;
    Canny              : TMenuItem;
    ShenCastan         : TMenuItem;

    // Transform menu
    TransformMenu      : TMenuItem;
    AffineItem         : TMenuItem;
    StretchItem        : TMenuItem;
    FlipItem           : TMenuItem;
    MirrorItem         : TMenuItem;
    RotateItem         : TMenuItem;
    RotateFixItem      : TMenuItem;
    Rotate180Item      : TMenuItem;
    Rotate270Item      : TMenuItem;
    DeskewItem         : TMenuItem;
    DeskewAndAutoCrop  : TMenuItem;

    // Math menu
    MathMenu           : TMenuItem;

    // Morphing menu
    MorphingMenu       : TMenuItem;
    MorphWizardItem    : TMenuItem;
    N7                 : TMenuItem;
    CloseMorphItem     : TMenuItem;
    DilateMorphItem    : TMenuItem;
    ErodeMorphItem     : TMenuItem;
    OpenMorphItem      : TMenuItem;
    OutlineMorhpItem   : TMenuItem;

    // Help - About menu
    HelpMenu           : TMenuItem;
    AboutItem          : TMenuItem;

    WindowsMenu        : TMenuItem;
    WindowCascadeItem  : TMenuItem;
    WindowTileItem     : TMenuItem;
    CloseAllItem       : TMenuItem;

    // Components drop on this form.
    mcmSTI             : TmcmSTI;        // Windows STI interface (Enables this
                                         // application to be activated when a
                                         // button on a scanner is pressed).
    mcmTWAIN           : TmcmTWAIN;      // Interface to TWAIN
    mcmDropTarget      : TmcmDropTarget; // Drag & Drop interface from other app's.
    mcmOpenDialog      : TmcmOpenDialog; // Image file open dialogue.
    mcmSaveDialog      : TmcmSaveDialog; // Image file save dialogue.
    mcmAppendDialog    : TmcmSaveDialog; // do.
    PrintDialog        : TPrintDialog;
    PrinterSetupDialog : TPrinterSetupDialog;

    ToolBar            : TToolBar;
    tbNew              : TToolButton;
    tbOpen             : TToolButton;
    tbSave             : TToolButton;
    tbPrint            : TToolButton;
    tbPrintPreview     : TToolButton;
    ToolButton7        : TToolButton;
    tbSelect           : TToolButton;
    tbScan             : TToolButton;
    ToolButton10       : TToolButton;
    lZoom              : TLabel;
    rsZoom             : TmcmRealSpin;
    tbFitInWindow      : TToolButton;
    ToolButton11       : TToolButton;
    tbText             : TToolButton;

    MenuImageList      : TImageList;
    StatusBar          : TStatusBar;
    HelpItem            : TMenuItem;
    EDMItem             : TMenuItem;

    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormShow(Sender : TObject);

    // Toolbar button click.
    procedure ImageToolClick(Sender : TObject);

    // Zoom methods.
    procedure tbFitInWindowClick(Sender : TObject);
    procedure rsZoomChange(Sender : TObject);

    // Drag & Drop methods.
    procedure FormDragOver(Sender, Source : TObject; X, Y : Integer; State : TDragState; var Accept : Boolean);
    procedure FormDragDrop(Sender, Source : TObject; X, Y : Integer);

    // File menu.
    procedure FileMenuClick(Sender : TObject);
    procedure NewItemClick(Sender : TObject);
    procedure OpenItemClick(Sender : TObject);
    procedure SaveItemClick(Sender : TObject);
    procedure AppendItemClick(Sender : TObject);
    procedure BrowseItemClick(Sender : TObject);
    procedure AcquireItemClick(Sender : TObject);
    procedure SourceItemClick(Sender : TObject);
    procedure PrintItemClick(Sender : TObject);
    procedure PrintPreviewItemClick(Sender : TObject);
    procedure PrinterSetupItemClick(Sender : TObject);
    procedure AssociatefileItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);

    // Remove function call and IFDEF below which does not match your Delphi
    // version. Code Insight in Delphi fails with this IFDEF
 {$IFNDEF DCB3}
    // Delphi 4, 5 and 6
    procedure mcmTWAINImageReady(Sender : TObject; pBmp : Pointer;
      pBmpInfo : PBitmapInfo; hImage : hBitmap; FilePath : String);
 {$ELSE}
    // Delphi 3
    procedure mcmTWAINImageReady(Sender : TObject; pBmp : Pointer;
      pBmpInfo: PBitmapInfo; hImage : Integer; FilePath : String);
 {$ENDIF}

    // Edit menu.
    procedure EditMenuClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure CopySectionItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
    procedure PasteSectionItemClick(Sender : TObject);
    procedure DuplicateItemClick(Sender : TObject);
    procedure EmptyClipboardItemClick(Sender : TObject);

    // Image menu.
    procedure ImageMenuClick(Sender : TObject);
    procedure InformationItemClick(Sender : TObject);
    procedure CanvasSizeItemClick(Sender : TObject);
    procedure TransparentViewItemClick(Sender : TObject);

    // Color menu.
    procedure ColorMenuClick(Sender : TObject);
    procedure UsedColorsItemClick(Sender : TObject);
    procedure EditPaletteItemClick(Sender: TObject);
    procedure InvertPaletteItemClick(Sender : TObject);
    procedure InvertItemClick(Sender : TObject);
    procedure ThresholdItemClick(Sender : TObject);
    procedure BrightnessContrastItemClick(Sender : TObject);
    procedure GammaCorrectionItemClick(Sender : TObject);

    procedure GreyScaleItemClick(Sender : TObject);

    procedure DecColorDepthMenuClick(Sender : TObject);
    procedure InclWindowsPalItemClick(Sender : TObject);
    procedure Dec2ColorsClick(Sender : TObject);
    procedure Dec16ColorsClick(Sender : TObject);
    procedure Dec256ColorsClick(Sender : TObject);
    procedure Dec32KColorsClick(Sender : TObject);
    procedure Dec24MColorsClick(Sender : TObject);
    procedure IncColorDepthMenuClick(Sender : TObject);
    procedure Inc16ColorsClick(Sender : TObject);
    procedure Inc256ColorsClick(Sender : TObject);
    procedure Inc32KColorsClick(Sender : TObject);
    procedure Inc16MillColorsClick(Sender : TObject);
    procedure Inc16MillColors32Click(Sender : TObject);

    procedure CIEXD65ItemClick(Sender : TObject);
    procedure CIEYD65ItemClick(Sender : TObject);
    procedure CIEZD65ItemClick(Sender : TObject);
    procedure CyanItemClick(Sender : TObject);
    procedure MagentaItemClick(Sender : TObject);
    procedure YellowItemClick(Sender : TObject);
    procedure BlackItemClick(Sender : TObject);
    procedure HueItemClick(Sender : TObject);
    procedure SaturationItemClick(Sender : TObject);
    procedure ValueItemClick(Sender : TObject);
    procedure RGBMenuClick(Sender : TObject);
    procedure RedChannelItemClick(Sender : TObject);
    procedure GreenChannelItemClick(Sender : TObject);
    procedure BlueChannelItemClick(Sender : TObject);
    procedure AlphaItemClick(Sender : TObject);
    procedure YChannelItemClick(Sender : TObject);
    procedure CbChannelItemClick(Sender : TObject);
    procedure CrChannelItemClick(Sender : TObject);
    procedure IChannelItemClick(Sender : TObject);
    procedure QChannelItemClick(Sender : TObject);
    procedure UChannelItemClick(Sender : TObject);
    procedure VChannelItemClick(Sender : TObject);

    procedure CombineCIEItemClick(Sender : TObject);
    procedure CombineCMYKItemClick(Sender : TObject);
    procedure CombineHSVItemClick(Sender: TObject);
    procedure CombineRGBItemClick(Sender : TObject);
    procedure CombineYCbCrItemClick(Sender : TObject);
    procedure CombineYIQCItemClick(Sender : TObject);
    procedure CombineYUVItemClick(Sender : TObject);

    procedure GreyToBayerItemClick(Sender : TObject);

    // Histogram menu.
    procedure HistogramMenuClick(Sender : TObject);
    procedure HistogramItemClick(Sender : TObject);
    procedure EqualizeItemClick(Sender : TObject);
    procedure EqualizeLumItemClick(Sender : TObject);
    procedure StretchColorItemClick(Sender : TObject);

    // Filter menu.
    procedure FilterMenuClick(Sender : TObject);
    procedure FilterBrowserClick(Sender : TObject);
    procedure UserDefFilterItemClick(Sender : TObject);
    procedure FilterItemClick(Sender : TObject);
    procedure MarrHildrethClick(Sender : TObject);
    procedure CannyClick(Sender : TObject);
    procedure ShenCastanClick(Sender : TObject);

    // Document filters.
    procedure DespecklelightItemClick(Sender : TObject);
    procedure DespeckleItemClick(Sender : TObject);
    procedure DespeckleAltItemClick(Sender : TObject);

    // Transform menu.
    procedure TransformMenuClick(Sender : TObject);
    procedure FlipItemClick(Sender : TObject);
    procedure MirrorItemClick(Sender : TObject);
    procedure AffineItemClick(Sender : TObject);
    procedure StretchItemClick(Sender : TObject);
    procedure RotateItemClick(Sender : TObject);
    procedure RotateFixItemClick(Sender : TObject);
    procedure DeskewItemClick(Sender : TObject);
    procedure DeskewAndAutoCropClick(Sender : TObject);

    // Math menu.
    procedure MathMenuClick(Sender : TObject);

    // Morph menu.
    procedure MorphWizardItemClick(Sender : TObject);
    procedure CloseMorphItemClick(Sender : TObject);
    procedure DilateMorphItemClick(Sender : TObject);
    procedure ErodeMorphItemClick(Sender : TObject);
    procedure OpenMorphItemClick(Sender : TObject);
    procedure OutlineMorhpItemClick(Sender : TObject);

    // Window menu.
    procedure WindowsMenuClick(Sender : TObject);
    procedure WindowCascadeItemClick(Sender : TObject);
    procedure WindowTileItemClick(Sender : TObject);
    procedure CloseAllItemClick(Sender : TObject);

    // Help menu.
    procedure AboutItemClick(Sender : TObject);
    procedure HelpItemClick(Sender : TObject);

    procedure tbTextClick(Sender : TObject);
    procedure tbProfileClick(Sender : TObject);
    procedure EDMItemClick(Sender: TObject);
  private
    { Private declarations }
    AppPath          : string;  // Path to this applications exe file.
    FNextImageWin    : integer; // Next Image window index.
    FScale           : double;  // Zoom factor of the active image window.

    // Timing variables.
    {$IFDEF DCB3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    FPerformanceFreq : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    FPerformanceFreq : int64;
    {$ENDIF}

    FImageIndex      : cardinal; // Number of created image windows.

    // Image file save variables.
    FCompress        : TmcmCompress; // Compression method.
    FInterlaced      : boolean;      // Interlaced or non-interlaced.
    FQuality         : word;         // Quality of image when saved.
    FSaveCursor      : TCursor;

    mcmImageColor    : TmcmImageColor;
    mcmImageFilter   : TmcmImageFilter;

    FAcceptDragObj   : boolean; // Used during Darg & Drop operations.

    procedure AppException(Sender: TObject; E: Exception);
    function  AppWindowHook(var Msg : TMessage) : boolean;

    procedure UpdateMenu;

    procedure ChildGettingFocus(Sender : TObject);
    procedure ChildLoosingFocus(Sender : TObject);
    procedure ChildResize(Sender : TObject);
    function  CreateImageWindow(AName        : string;
                                AWidth       : integer;
                                AHeight      : integer;
                                AImageFormat : TmcmImageFormat) : TFormChild;
    procedure OpenImageFile(Sender : TObject; Filename : string; ImageIndex : integer);

    procedure OnSetProcessTime(Sender : TObject; STime, ETime :
                               {$IFDEF DCB3}TLargeInteger{$ELSE}int64{$ENDIF});

    procedure WMStiAcquire(var Msg : TMessage); message WM_STIACQUIRE;
  public
    { Public declarations }
    FChild : TFormChild;
    procedure ImageMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure ImageRegionChanged(Sender : TObject; NewRegion : TRect);
    function  GetImageWindow : TFormChild;
    function  GetNextImageWindow : TFormChild;
    procedure FilterImage(Method       : TmcmFilter;
                          Height       : word;
                          Width        : word;
                          Bias         : integer;
                          ScaleFactor  : integer;
                          Hysteresis   : word;
                          GaussSD      : double;
                          DeltaSD      : double;
                          TraceAuto    : boolean;
                          TracePercent : integer;
                          TraceLow     : word;
                          TraceHigh    : word;
                          FormFilter   : TForm);
  end;

var FormMain : TFormMain;

implementation

{$R *.DFM}

uses {$IFNDEF GE_DXE2}
      Registry, Clipbrd, IniFiles,
     {$ELSE}
      System.Types, System.UITypes, System.Win.Registry, Vcl.Clipbrd, System.IniFiles,
     {$ENDIF}
     mcmThumbnail,
     uFormFileAssociate,
     uFormImageInfo,
     uFormPrintPreview,
     uFormThreshold,
     uFormBrowse,
     uFormHistogram,
     uFormMath,
     uFormMorph,
     uFormStretch,
     uFormCombine2RGB,
     uFormFilterBrowser,
     uFormFilterUser,
     uFormRotate,
     uFormBrightContrast,
     uFormGamma,
     uFormAffine,
     uFormPalette,
     uFormViewProfile,
     uFormNew,
     uFormPaintBox,
     uFormCanvasSize;

//------------------------------------------------------------------------------
// Time measurement
// Use
//    QueryPerformanceFrequency(PerformanceFreq);
// to obtain max timer frequency (ticks per second).
// Use
//    QueryPerformanceCounter(TimeTick);
// to obtain the current "time".
//------------------------------------------------------------------------------

procedure TFormMain.FormCreate(Sender : TObject);
var Reg : TRegIniFile;
begin
  AppPath := ExtractFilePath(Application.ExeName);

  //----------------------------------------------------------------------------
  // Used to when app is only allowed to exist as one instance.
  // In this case a second instance will signal that the first instance should
  // pop-up as the front window.
  // This will also open image files selected in "Windows Explorer".
  // See modifications made to itfd.pas, AppWindowHook and FormShow to support
  // this.
  //----------------------------------------------------------------------------
  Application.HookMainWindow(AppWindowHook);

  //----------------------------------------------------------------------------
  // Make sure that our application is registered with STI.
  mcmSTI.RegisterApplication('Imaging Toolkit for Delphi', Application.ExeName);
  // Check if we were launched by STI
  if (mcmSTI.LaunchedBySTI <> '')
  then PostMessage(Handle, WM_STIACQUIRE, 0, 0);


  //----------------------------------------------------------------------------
  //
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('\Software\Microsoft\Windows\', True)
  then begin
       try
         Application.HelpFile := Reg.ReadString('Help', 'mcmImaging.hlp', '') + '\mcmImaging.hlp';
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;

  //----------------------------------------------------------------------------
  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Left   := Reg.ReadInteger('window', 'left', 10);
         Top    := Reg.ReadInteger('window', 'top', 10);
         Width  := Reg.ReadInteger('window', 'width', 640);
         Height := Reg.ReadInteger('window', 'height', 480);

         mcmSaveDialog.InitialDir  := Reg.ReadString('', 'Path', '');
         mcmSaveDialog.FilterIndex := Reg.ReadInteger('', 'wFilter', 0);
         mcmSaveDialog.FilterIndex := -1;
         mcmOpenDialog.FilterIndex := Reg.ReadInteger('', 'rFilter', 0);
         mcmOpenDialog.InitialDir  := mcmSaveDialog.InitialDir;
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
  FScale := 1.0;

  //----------------------------------------------------------------------------
  // Specify which formats to support!
  //ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  //----------------------------------------------------------------------------
  // Register an external file format (WMF - Window Meta Format).
  ImageFileManager.RegisterFileFormat(TImageWMF, FF_MyWMF, 'wmf', resMyWMF, True, True);

  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions. 
  mcmOpenDialog.ImageFileManager := ImageFileManager;
  mcmSaveDialog.ImageFileManager := ImageFileManager;

  //----------------------------------------------------------------------------
  // TIFF - bi-level image.
  // By default SafeBiLevel is set to True to ensure that "non-compilant" TIFF
  // applications will read the images generated here.
  // Problem is that some applications doesn't read all TAG's in the TIFF file
  // and thereby mis-interpretate what's black and white in the image file.
  // The result of setting SafeBiLevel = True, is that White is Zero valued.
  {
  ImageFileManager.SafeBiLevel := False;
  }


  //----------------------------------------------------------------------------
  // JPEG - compression
  // JYCC_AUTO,   FQuality determines Y, Cb and Cr frequency.
  // JYCC_411,    Frequencies are fixed: Y = 2, Cb = 1 and Cr = 1.
  // JYCC_421     Frequencies are fixed: Y = 2,
  //              Horizontal Cb, Cr = 2
  //              Vertical Cb, Cr = 1.
  {
  ImageFileManager.YCbCrMode := JYCC_AUTO;
  }


  //----------------------------------------------------------------------------
  // Set-up the Window menu as the menu that list's our open image windows.
  Self.WindowMenu := WindowsMenu;


  //----------------------------------------------------------------------------
  // Set-up time measurement of processes.
  QueryPerformanceFrequency(FPerformanceFreq);
  QueryPerformanceCounter(FStartTime);
  QueryPerformanceCounter(FEndTime);
  StatusBar.Panels[2].Text := 'Time: -  [ms]';

  //----------------------------------------------------------------------------
  // Initialise filter menu items.
  // The FilterItemClick method below will use the TMenuItem.Tag value to
  // determine which filter to execute.
  AverageItem.Tag         := integer(FLT_AVERAGE);
  AverageHeavyItem.Tag    := integer(FLT_AVERAGEHEAVY);
  BlurItem.Tag            := integer(FLT_BLUR);
  BlurHeavyItem.Tag       := integer(FLT_BLURHEAVY);
  DegrainItem.Tag         := integer(FLT_DEGRAIN);
  GaussBlurItem.Tag       := integer(FLT_GAUSSBLUR);
  SmoothItem.Tag          := integer(FLT_SMOOTH);
  SmoothCircluarItem.Tag  := integer(FLT_SMOOTHCIRCLE);
  SmoothConeItem.Tag      := integer(FLT_SMOOTHCONE);
  SmoothPyramidalItem.Tag := integer(FLT_SMOOTHPYRAMIDAL);
  EdgeItem.Tag            := integer(FLT_EDGE);
  EdgePrewittItem.Tag     := integer(FLT_EDGEPREWITT);
  HighpassItem.Tag        := integer(FLT_HIGHPASS);
  LaplacianItem.Tag       := integer(FLT_LAPLACIAN);
  PrewittNWItem.Tag       := integer(FLT_PREWITTNS);
  PrewittSEItem.Tag       := integer(FLT_PREWITTEW);
  SharpenItem.Tag         := integer(FLT_SHARPEN);
  SharpenheavyItem.Tag    := integer(FLT_SHARPENHEAVY);
  SobelNWItem.Tag         := integer(FLT_SOBELNS);
  SobelSEItem.Tag         := integer(FLT_SOBELEW);
  EmbossItem.Tag          := integer(FLT_EMBOSS);
  MaximumItem.Tag         := integer(FLT_MAXIMUM);
  MaxMinItem.Tag          := integer(FLT_MAXMIN);
  MedianItem.Tag          := integer(FLT_MEDIAN);
  MinimumItem.Tag         := integer(FLT_MINIMUM);
  MosaicItem.Tag          := integer(FLT_MOSAIC);
  LineMaskHorzItem.Tag    := integer(FLT_LINEMASKHORZ);
  LineMaskVertItem.Tag    := integer(FLT_LINEMASKVERT);
  UnsharpMaskItem.Tag     := integer(FLT_UNSHARPMASK);


  //----------------------------------------------------------------------------
  // Child window index.
  FImageIndex := 1; // Index used when creating Image windows.


  //----------------------------------------------------------------------------
  // Create an instance of TmcmImageColor & TmcmImageFilter
  mcmImageColor := TmcmImageColor.Create(Self);
  mcmImageFilter := TmcmImageFilter.Create(Self);


  //----------------------------------------------------------------------------
  // General exception handler. AppException is called when Exception are
  // unhandled, i.e. occur outside a try - except
  Application.OnException := AppException;

  //----------------------------------------------------------------------------
  // Change default drag DragImmediate and DragThreshold. 
  {$IFNDEF DCB3}
    Mouse.DragImmediate := False;
    Mouse.DragThreshold := 3;
  {$ENDIF}

  //----------------------------------------------------------------------------
  // Load custom cursors (Ref. mcmCURSORS.RES included in itfd.dpr).
  Screen.Cursors[crPipette] := LoadCursor(HInstance, 'Pipette');
  Screen.Cursors[crPencil] := LoadCursor(HInstance, 'Pencil');
  Screen.Cursors[crRectangle] := LoadCursor(HInstance, 'Rectangle');
  Screen.Cursors[crCircle] := LoadCursor(HInstance, 'Circle');

  // Position PaintBox.
  FormPaintBox := TFormPaintBox.Create(Self);
  FormPaintBox.Width := 2 * 26 + 8;
  FormPaintBox.Top := Top + 88;
  FormPaintBox.Left := Left + Width - FormPaintBox.Width - 8;
  FormPaintBox.Show;
end; // TFormMain.FormCreate.


procedure TFormMain.FormDestroy(Sender : TObject);
var Reg : TRegIniFile;
begin
  mcmImageColor.Free;
  mcmImageFilter.Free;

  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Reg.WriteInteger('window', 'left', Left);
         Reg.WriteInteger('window', 'top', Top);
         Reg.WriteInteger('window', 'width', Width);
         Reg.WriteInteger('window', 'height', Height);
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
  FormPaintBox.Free;
end; // TFormMain.FormDestroy.


procedure TFormMain.FormActivate(Sender : TObject);
begin
  Self.WindowMenu := WindowsMenu;
  FormPaintBox.ToolChanged := ImageToolClick;
end; // TFormMain.FormActivate.


procedure TFormMain.FormShow(Sender : TObject);
begin
  // Check if we should load an image file.
  if (ParamCount > 0)
  then begin
       if FileExists(ParamStr(1))
       then OpenImageFile(Sender, ParamStr(1), -1);
  end;
end; // TFormMain.FormShow.


function TFormMain.AppWindowHook(var Msg : TMessage) : boolean;
// This function is used to intercept a message sent by the second instance
// of our application.
// The message is sent from our project source file (itfd.pas).
// We use this method to get the file name pass on to our second instance, and
// bring our application window in front.
var Reg      : TRegistry;
    FileName : string;
begin
  // A unique value for MessageID is obtain in itfd.pas by calling
  // RegisterWindowMessage.
  // MessageID is a global variable declared in this unit.
  if (integer(Msg.Msg) = MessageID)
  then begin
       // We want to restore this window if iconic
       // and put it in front.
       Result := True;
       if IsIconic(Application.Handle)
       then begin
            Self.WindowState := WSNORMAL;
            Application.Restore;
       end;
       SetForegroundWindow(Self.Handle);

       // Check if we should load an image file.
       if (Msg.lParam <> 0)
       then begin
            Reg := TRegistry.Create;
            Reg.RootKey := HKEY_CURRENT_USER;
            FileName := '';
            if Reg.OpenKey(RegKey, False)
            then begin
                 try
                   FileName := Reg.ReadString('Shell');
                   Reg.DeleteValue('Shell');
                   Reg.DeleteKey(RegKey + 'Shell');
                 except
                   on E:Exception
                   do ;
                 end;
                 Reg.CloseKey;
            end;
            Reg.Free;

            if FileExists(FileName)
            then OpenImageFile(Self, FileName, -1);
       end;
  end
  else Result := False; // Not the message we're looking fore.
end; // End TFormMain.AppWindowHook.


procedure TFormMain.AppException(Sender : TObject; E : Exception);
begin
  // Unhandled exceptions
  Application.ShowException(E);
  // It's up to you how to handle exceptions! You could terminate the application.
  // Application.Terminate;
end; // TFormMain.AppException.


procedure TFormMain.OnSetProcessTime(Sender : TObject; STime, ETime : {$IFDEF DCB3}TLargeInteger{$ELSE}int64{$ENDIF});
var Duration : double;
begin
  {$IFDEF DCB3}
    Duration := (ETime.QuadPart - STime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    Duration := (ETime - STime) / FPerformanceFreq;
  {$ENDIF}
  if (Duration < 0.001)
  then StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000000.0 * Duration, ffFixed, 15, 4) + ' us'
  else if (Duration < 1.0)
       then StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * Duration, ffFixed, 15, 4) + ' ms'
       else StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(Duration, ffFixed, 15, 4) + ' s';
end; // TFormMain.OnSetProcessTime.


procedure TFormMain.UpdateMenu;
var HasImage : boolean;
begin
  // First determin if we have an image window.
  HasImage := False;
  if Assigned(FChild)
  then HasImage := Not(FChild.ImageCtrl.Image.Empty);

  // Get windows zoom factor.
  if HasImage
  then rsZoom.Value := FChild.Scale;

  // Enable/disable menus & tools according to the active image.
  rsZoom.Enabled := HasImage;
  tbFitInWindow.Enabled := HasImage;

  EditMenu.Enabled := True;
  ImageMenu.Enabled := HasImage;
  ColorMenu.Enabled := HasImage;
  HistogramMenu.Enabled := HasImage;
  FilterMenu.Enabled := HasImage;
  MathMenu.Enabled := HasImage;
  MorphingMenu.Enabled := HasImage;
  if HasImage
  then begin
       HistogramMenu.Enabled := Not(FChild.Image.ImageFormat in [IF_BW, IF_RGB15, IF_RGB16]);
       FilterMenu.Enabled := (FChild.Image.ImageFormat in [IF_GREY8, IF_RGB24, IF_RGBA32]);
       MathMenu.Enabled := (FChild.Image.ImageFormat in [IF_GREY8, IF_RGB24, IF_RGBA32]);
       MorphingMenu.Enabled := (FChild.Image.ImageFormat in [IF_GREY8, IF_PAL8]);;
  end;
  TransformMenu.Enabled := HasImage;
end; // TFormMain.UpdateMenu.


//------------------------------------------------------------------------------
// Events fire by MDI child windows.
//------------------------------------------------------------------------------

procedure TFormMain.ChildGettingFocus(Sender : TObject);
begin
  FChild := TFormChild(Sender);
  if Assigned(FChild)
  then begin
       if Not(FChild.Image.Empty)
       then begin
            // Get Maximum Scale/Zoom factor supported by Windows & Delphi
            // edition.
            rsZoom.MaxValue := FChild.Image.MaxIntScale;
       end;
  end;
  UpdateMenu;
end; // End TFormMain.ChildGettingFocus.


procedure TFormMain.ChildLoosingFocus(Sender : TObject);
begin
  FChild := Nil;
  UpdateMenu;
end; // TFormMain.ChildLoosingFocus.


procedure TFormMain.ChildResize(Sender : TObject);
var HasImage : boolean;
begin
  HasImage := False;
  if Assigned(FChild)
  then HasImage := Not(FChild.ImageCtrl.Image.Empty);

  // Get windows zoom factor.
  if HasImage
  then rsZoom.Value := FChild.Scale;
  FScale := rsZoom.Value;
end; // TFormMain.ChildResize.


procedure TFormMain.ImageMouseMove(Sender : TObject;
                                   Shift  : TShiftState;
                                   X, Y   : integer);
var Color      : TColor;
    xs, ys     : integer;
    MouseChild : TFormChild;
begin
  if Assigned(Sender)
  then begin
       MouseChild := TFormChild(Sender);

       xs := Trunc(x / FScale);
       ys := Trunc(y / FScale);
       if (0 <= xs) and (xs < MouseChild.Image.Width) and
          (0 <= ys) and (ys < MouseChild.Image.Height)
       then begin
            Color := MouseChild.Image.Pixel[xs, ys];
            StatusBar.Panels[0].Text := '[X,Y]: ' +
                                        IntToStr(xs) + ',' + IntToStr(ys);
            if (MouseChild.Image.ImageFormat = IF_RGBA32)
            then StatusBar.Panels[1].Text := 'Color [R,G,B,A]: ' + IntToStr(GetRValue(Color)) + ',' +
                                                                   IntToStr(GetGValue(Color)) + ',' +
                                                                   IntToStr(GetBValue(Color)) + ',' +
                                                                   IntToStr(Color shr 24)
            else StatusBar.Panels[1].Text := 'Color [R,G,B]: ' + IntToStr(GetRValue(Color)) + ',' +
                                                                 IntToStr(GetGValue(Color)) + ',' +
                                                                 IntToStr(GetBValue(Color));
       end;
  end;
end; // TFormMain.ImageMouseMove.


procedure TFormMain.ImageRegionChanged(Sender : TObject; NewRegion : TRect);
begin
  // Called by the active client window, to notify that the selected region has
  // changed.
end; // TFormMain.ImageRegionChanged.


//------------------------------------------------------------------------------
// Create MDI childen.
//------------------------------------------------------------------------------

function TFormMain.CreateImageWindow(AName        : string;
                                     AWidth       : integer;
                                     AHeight      : integer;
                                     AImageFormat : TmcmImageFormat) : TFormChild;
begin
  FChild := TFormChild.Create(Self);
  FChild.LoosingFocus := ChildLoosingFocus;
  FChild.GettingFocus := ChildGettingFocus;
  FChild.OnResize     := ChildResize;
  if (AWidth > 0) and (AHeight > 0) and (AImageFormat <> IF_NONE)
  then begin
       FChild.Image.Width  := AWidth;
       FChild.Image.Height := AHeight;
       FChild.Image.ImageFormat := AImageFormat;
       FChild.Image.ImageInfo.DateTime := Now;
  end
  else begin
       FChild.Caption := AName + IntToStr(FImageIndex);
       inc(FImageIndex);
  end;
  FChild.OnXYColor := ImageMouseMove;
  FChild.OnRegionChanged := ImageRegionChanged;

  Result := FChild;
  FChild.GettingFocus(FChild);
  UpdateMenu;
end; // TFormMain.CreateImageWindow.


function TFormMain.GetImageWindow : TFormChild;
begin
  Result := Nil;
  FNextImageWin := 0;
  if (MDIChildCount > 0)
  then if (ActiveMDIChild is TFormChild)
       then Result := TFormChild(ActiveMDIChild)
end; // TFormMain.GetImageWindow.


function TFormMain.GetNextImageWindow : TFormChild;
begin
  Result := Nil;
  inc(FNextImageWin);
  if (MDIChildCount > 1)
  then begin
       while (FNextImageWin < MDIChildCount) and
             Not(MDIChildren[FNextImageWin] is TFormChild)
       do inc(FNextImageWin);
       if (MDIChildren[FNextImageWin] is TFormChild)
       then Result := TFormChild(MDIChildren[FNextImageWin])
  end;
end; // TFormMain.GetNextImageWindow.


//------------------------------------------------------------------------------
// Drag and Drop methods.
//------------------------------------------------------------------------------

procedure TFormMain.FormDragOver(    Sender, Source : TObject;
                                     X, Y           : integer;
                                     State          : TDragState;
                                 var Accept         : boolean);
var APoint     : TPoint;
    ClientRect : TRect;
    FileNames  : TStringList;
    i          : integer;
begin
  // Note: Sender is the potential drop site, Source is the object being dropped.
  // When TmcmDropTarget is the calling class, Source is TmcmDropEnum.

  Accept := False; // Initially we will not accept the object being dragged over.

  // This first section will accept images and files dragged from other
  // applications, received via the TmcmDropTarget control.
  //
  // If the object being dragged is a TmcmDropEnum class, we're receiving data
  // the Windows way, i.e. from another applications (Explorer, Word ect).
  if (Source is TmcmDropEnum)
  then begin
       if (State = dsDragEnter)
       then begin // The dragged object/cursor enters our window.
            FAcceptDragObj := False;

            // Check if we support the dragged object!
            // We'll accept bitmaps
            if (Source as TmcmDropEnum).HasClipFormat(CF_DIB) or
               (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
            then FAcceptDragObj := True;

            // and we'll accept dragged files (i.e. file names).
            if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
            then begin
                 // Check the type of file(s) begin dragged.
                 FileNames := TStringList.Create;
                 (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                 FAcceptDragObj := True;
                 if (FileNames.Count > 0)
                 then begin
                      i := 0;
                      while (i < FileNames.Count) and FAcceptDragObj
                      do begin
                         FAcceptDragObj := ImageFileManager.VerifyImageFile(FileNames.Strings[i]);
                         inc(i);
                      end;
                 end;
                 FileNames.Free;
            end;
       end;

       if FAcceptDragObj and // Did we have a valid object/file to drop ?
          (Sender = Self)    // Check that we're dropping on the main frame and
                             // not on a client window.
       then begin
            // Test if we're in the drop zone (Client area of the main form).
            APoint := ScreenToClient(Point(X, Y));
            ClientRect := GetClientRect;
            ClientRect.Top := ClientRect.Top + ToolBar.Height;
            ClientRect.Bottom := ClientRect.Bottom - StatusBar.Height;

            if ((ClientRect.Left < APoint.x) and (APoint.x < ClientRect.Right) and
                (ClientRect.Top < APoint.y) and (APoint.y < ClientRect.Bottom))
            then Accept := True;
       end;

       Exit;
  end;

  if IsDragObject(Source)
  then begin
       // If the TmcmThumbnail control is set-up to drag using the Delphi style,
       // thumbs are dragged as a file list in the TmcmDragFileList control.
       Accept := (Source is TmcmDragFileList); // We'll accept this class.
  end;

  // Add your native Delphi Drag over handling here...
  // ...

end; // TFormMain.FormDragOver.


procedure TFormMain.FormDragDrop(Sender, Source : TObject; X, Y : integer);
var Image     : TmcmImage;
    i         : integer;
    FileNames : TStringList;
begin
  // This first section will accept images and files dragged from other
  // applications, received via the TmcmDropTarget control.
  if (Sender is TFormMain)
  then begin
       // If the object being dropped is a TmcmDropEnum class, we're receiving
       // data the Windows way, fx. from another applications (Explorer, Word ect).
       if (Source is TmcmDropEnum)
       then begin
            // The object has been dropped on our applications main form.
            Application.BringToFront; // Move this application window to the top.

            if (Source as TmcmDropEnum).HasClipFormat(CF_DIB) or
               (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
            then begin
                 Image := TmcmImage.Create;
                 (Source as TmcmDropEnum).GetAsImage(Image);
                 if Not(Image.Empty)
                 then begin
                      FChild := CreateImageWindow('Drag_', 0, 0, IF_NONE);
                      FChild.Image := Image;
                      FChild.Image.ImageInfo.FileName := FChild.Caption;
                 end
                 else Image.Free;
            end
            else if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
                 then begin
                      // Create a TStringList that will receive the filenames.
                      FileNames := TStringList.Create;
                      // Get the dropped files (file names).
                      (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                      if (FileNames.Count > 0)
                      then begin
                           for i := 0 to (FileNames.Count - 1)
                           do OpenImageFile(Self, FileNames.Strings[i], -1);
                      end;
                      FileNames.Free;
                 end;
       end;
  end;

  if IsDragObject(Source)
  then begin
       // A list of files were dragged from the TmcmThumbnail control using Delphi.
       if (Source is TmcmDragFileList)
       then begin
            if ((Source as TmcmDragFileList).Filenames.Count > 0)
            then begin
                 // Open all image files provided in TmcmDragFileList.Filenames[].
                 for i := 0 to ((Source as TmcmDragFileList).Filenames.Count - 1)
                 do OpenImageFile(Self, (Source as TmcmDragFileList).Filenames.Strings[i], -1);
            end;
       end;
  end;

  // Add your native Delphi Drop handling here...
  // ...

end; // TFormMain.FormDragDrop.


//------------------------------------------------------------------------------
// Image File methods.
//------------------------------------------------------------------------------

procedure TFormMain.OpenImageFile(Sender : TObject; Filename : string; ImageIndex : integer);
var mcmImage   : TmcmImage;
    hImage     : HBitmap;
    ExtName    : string;
    IncName    : string;
    NoSubImage : boolean;
begin
  // - Filename specifies the file to open.
  // - ImageIndex specifies the page to open in a multipaged file.
  //   If ImageIndex is -1, we want to open all pages in the file.

  FSaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  NoSubImage := True;
  QueryPerformanceCounter(FStartTime);

  // Use ImageFileManager to open image file. This allows you to retreive the
  // handle that can be assign to a TBitmap.Handle.
  {
  FileFormat := FF_DETECT;
  FChild := CreateImageWindow(0, 0, IF_NONE);
  FChild.Image.Image.DibHandle := ImageFileManager.LoadImage(mcmOpenDialog.FileName, ImageFormat);
  FChild.Image.Image.XResolution := Round(ImageFileManager.ImageInfo.XResolution);
  FChild.Image.Image.YResolution := Round(ImageFileManager.ImageInfo.YResolution);
  FChild.Caption := FileName;
  }

  // Read image from disk
  mcmImage := TmcmImage.Create;
  if (ImageIndex <= 0)
  then mcmImage.FileOpen(FileName)
  else mcmImage.FilePageOpen(FileName, ImageIndex);
  if Not(mcmImage.Empty)
  then begin
       FChild := CreateImageWindow(FileName, 0, 0, IF_NONE);
       FChild.Image := mcmImage;
       FChild.Caption := FileName;
  end
  else mcmImage.Free;

  // If the file contains additional images "IsMultiImage" is True.
  if (ImageFileManager.Error = EC_OK) and ImageFileManager.IsMultiImage and (ImageIndex = -1)
  then begin
       ExtName := ExtractFileExt(FileName);
       repeat
         // Let's load all sub-images from the file.
         hImage := ImageFileManager.LoadNext(FileName, FF_DETECT, -1);
         if (hImage <> 0)
         then begin
              IncName := ChangeFileExt(FileName, '_' + IntToStr(ImageFileManager.ImageIndex + 1) +
                                                 ExtName);
              FChild := CreateImageWindow(FileName, 0, 0, IF_NONE);
              FChild.Image.DibHandle := hImage;
              FChild.Image.ImageInfo.Assign(ImageFileManager.ImageInfo);
              FChild.Image.ImageInfo.FileName := IncName;
              FChild.Caption := IncName;
         end;
       until (hImage = 0);
       NoSubImage := False; // Do not show NoSubImage error message.
  end;

  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
  Screen.Cursor := FSaveCursor;

  if ((ImageFileManager.Error <> EC_OK) and (ImageFileManager.Error <> EC_NOSUBIMAGE)) or
     ((ImageFileManager.Error = EC_NOSUBIMAGE) and NoSubImage)
  then ShowMessage('Error reading image: ' + CErrorStrings[word(ImageFileManager.Error)]);
end; // TFormMain.OpenImageFile.


//------------------------------------------------------------------------------
// Image Tools
//------------------------------------------------------------------------------


procedure TFormMain.ImageToolClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild) and (ActiveMDIChild is TFormChild)
  then begin
       case TImageTool((Sender as TToolButton).Tag) of
       IT_PROFILE : tbProfileClick(Sender);
       else FChild.ImageTool := TImageTool((Sender as TToolButton).Tag);
       end;
  end;
end; // TFormMain.ImageToolClick.


procedure TFormMain.tbProfileClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       if (ActiveMDIChild is TFormChild)
       then FChild.ImageTool := TImageTool((Sender as TToolButton).Tag);

       if Not(Assigned(FormViewProfile))
       then FormViewProfile := TFormViewProfile.Create(Self);
       FormViewProfile.OnProcessTime := OnSetProcessTime;
       FormViewProfile.Profile := FChild.mcmProfile;
       FormViewProfile.Show;
  end;
end; // TFormMain.tbProfileClick.

//------------------------------------------------------------------------------
// Zoom tools
//------------------------------------------------------------------------------

procedure TFormMain.tbFitInWindowClick(Sender : TObject);
var SaveNotify : TNotifyEvent;
begin
  if Assigned(FChild)
  then begin
       FChild.ScaleToFit := True;
       SaveNotify := rsZoom.OnChange;
       rsZoom.OnChange := Nil;
       FScale := FChild.Scale;
       rsZoom.Value := FScale;
       rsZoom.OnChange := SaveNotify;
  end;
end; // TFormMain.tbFitInWindowClick.


procedure TFormMain.rsZoomChange(Sender : TObject);
begin
  if Assigned(FChild)
  then begin
       if (rsZoom.Text <> '')
       then begin
            if (rsZoom.Value > 0.0)
            then begin
                 FScale := rsZoom.Value;
                 FChild.Scale := FScale;
                 if (FScale < 1.0)
                 then rsZoom.Increment := FScale
                 else rsZoom.Increment := 1.0;
            end
            else begin
                 FScale := FChild.Scale;
                 FScale := FScale / 2.0;
                 rsZoom.Value := FScale;
                 rsZoom.Increment := FScale;
            end;
       end;
  end;
end; // TFormMain.rsZoomChange.


//------------------------------------------------------------------------------
// File menu
//------------------------------------------------------------------------------

procedure TFormMain.FileMenuClick(Sender : TObject);
var bHasImage : boolean;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild) and (ActiveMDIChild is TFormChild)
  then bHasImage := Assigned(FChild.Image);
  SaveItem.Enabled := bHasImage;
  PrintItem.Enabled := bHasImage;
  PrintPreviewItem.Enabled := bHasImage;
end; // TFormMain.FileItemClick.


procedure TFormMain.NewItemClick(Sender : TObject);
begin
  FormNewImage := TFormNewImage.Create(Self);
  FormNewImage.FillColor := FormPaintBox.BackColor;
  if (FormNewImage.ShowModal = mrOK)
  then begin
       QueryPerformanceCounter(FStartTime);
       FChild := CreateImageWindow('New_', FormNewImage.ImageWidth,
                                           FormNewImage.ImageHeight,
                                           FormNewImage.ImageFormat);
       if (FormNewImage.ImageFormat in [IF_BW, IF_GREY4, IF_GREY8])
       then FChild.Image.CreateGreyPalette;
       FChild.Image.XResolution := Round(FormNewImage.ImageResolution);
       FChild.Image.YResolution := Round(FormNewImage.ImageResolution);
       FChild.Image.FillRGB(FormNewImage.FillColor);

       FChild.Image.ImageInfo.Artist := 'MCM DESIGN';
       FChild.Image.ImageInfo.Software := 'Imaging Toolkit for Delphi';
       FChild.Image.ImageInfo.SoftwareVersion := 1.0;
       FChild.Image.ImageInfo.DateTime := Now;
       FChild.Image.ImageInfo.Description := 'Imaging Toolkit for Delphi';
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
  FormNewImage.Free;
  UpdateMenu;
end; // TFormMain.NewItemClick.


procedure TFormMain.OpenItemClick(Sender : TObject);
var Reg : TRegIniFile;
    i   : integer;
begin
  mcmOpenDialog.FileName := '';
  if (mcmOpenDialog.Execute)
  then begin
       Update;
       FSaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;
       mcmOpenDialog.FileName := LowerCase(mcmOpenDialog.FileName);
       mcmSaveDialog.InitialDir := ExtractFileDir(mcmOpenDialog.FileName);

       // Store last directory and file extension.
       Reg := TRegIniFile.Create('');
       Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
       if Reg.OpenKey(RegKey, True)
       then begin
            try
              Reg.WriteString('', 'Path', mcmSaveDialog.InitialDir);
              Reg.WriteInteger('', 'rFilter', mcmOpenDialog.FilterIndex);
              Reg.WriteInteger('', 'rViewStyle', integer(mcmOpenDialog.ViewStyle));
            except
              on E:Exception
              do ;
            end;
            Reg.CloseKey;
       end;
       Reg.Free;
       mcmOpenDialog.InitialDir := mcmSaveDialog.InitialDir;
       Screen.Cursor := FSaveCursor;

       // If multiple files were selected - open all.
       if (mcmOpenDialog.Files.Count > 1)
       then begin
            // Mutiple files were selected - now load each one.
            for i := 0 to (mcmOpenDialog.Files.Count - 1)
            do OpenImageFile(Self, mcmOpenDialog.Files[i], -1);
       end
       else OpenImageFile(Self, mcmOpenDialog.FileName, -1); // Load the selected file.
       UpdateMenu;
  end;
end; // TFormMain.OpenItemClick.


procedure TFormMain.SaveItemClick(Sender : TObject);
var Reg : TRegIniFile;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       mcmSaveDialog.FilterIndex := -1; // Image will specify the file format.
                                        // A zero or positive value will instead
                                        // select the indexed file mask.
       mcmSaveDialog.Image       := FChild.Image;
       mcmSaveDialog.FileName    := ChangeFileExt(FChild.Caption, '');
       mcmSaveDialog.Compression := FChild.Image.Compression;
       mcmSaveDialog.Quality     := FChild.Image.Quality;
       mcmSaveDialog.Interlaced  := FChild.Image.Interlaced;

       if (mcmSaveDialog.Execute)
       then begin
            Refresh;
            mcmSaveDialog.InitialDir := ExtractFileDir(mcmSaveDialog.FileName);

            FSaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;

            // Store last directory and file extension.
            Reg := TRegIniFile.Create('');
            Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
            if Reg.OpenKey(RegKey, True)
            then begin
                 try
                   Reg.WriteString('', 'Path', mcmSaveDialog.InitialDir);
                   Reg.WriteInteger('', 'wFilter', mcmSaveDialog.FilterIndex);
                 except
                   on E:Exception
                   do ;
                 end;
                 Reg.CloseKey;
            end;
            Reg.Free;
            mcmOpenDialog.InitialDir := mcmSaveDialog.InitialDir;

            // Get selected compression settings.
            FCompress   := mcmSaveDialog.Compression;
            FQuality    := mcmSaveDialog.Quality;
            FInterlaced := mcmSaveDialog.Interlaced;

            // Store image to disk
            FChild.Image.Compression := FCompress;
            FChild.Image.Interlaced  := FInterlaced;
            FChild.Image.Quality     := FQuality;

            QueryPerformanceCounter(FStartTime);
            FChild.Image.FileSave(mcmSaveDialog.FileName);
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            Screen.Cursor := FSaveCursor;

            if (ImageFileManager.Error <> EC_OK)
            then ShowMessage('Error writing image: ' + CErrorStrings[word(ImageFileManager.Error)]);

            FChild.Caption := mcmSaveDialog.FileName;
       end;
  end;
end; // TFormMain.SaveItemClick.


procedure TFormMain.AppendItemClick(Sender : TObject);
var AppendFileManager : TmcmImageFileMgr;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       // As TIFF is the only file format that supports appending images to an
       // existing file, we'll create a TmcmImageFileMgr including just TIFF,
       // which will be used by the mcmAppendDialog (TmcmSaveDialog).
       // This way, mcmAppendDialog will only display TIFF in "Type of files".
       AppendFileManager := TmcmImageFileMgr.Create;
       AppendFileManager.UnregisterAllFileFormats;
       AppendFileManager.RegisterFileFormat(TmcmTIFFImage, FF_TIFF, 'tif,tiff,fax', resTIFF, True, True);

       mcmAppendDialog.FilterIndex := 0; // Image specifies file format.
       mcmAppendDialog.Image := FChild.Image;
       mcmAppendDialog.ImageFileManager := AppendFileManager;
       mcmAppendDialog.FileName := ChangeFileExt(mcmSaveDialog.FileName, '');
       mcmAppendDialog.DefaultExt := 'tif';

       FCompress   := FChild.Image.Compression;
       FQuality    := FChild.Image.Quality;
       FInterlaced := FChild.Image.Interlaced;
       if (mcmAppendDialog.Execute)
       then begin
            mcmAppendDialog.InitialDir := ExtractFileDir(mcmAppendDialog.FileName);
            mcmAppendDialog.FileName := ChangeFileExt(mcmAppendDialog.FileName, '.' +
                                                      mcmAppendDialog.DefaultExt);
            FCompress := mcmAppendDialog.Compression;

            FSaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;

            // Store image to multi image/page file (TIFF).
            FChild.Image.Compression := FCompress;
            FChild.Image.Interlaced  := FInterlaced;
            FChild.Image.Quality     := FQuality;

            QueryPerformanceCounter(FStartTime);
            FChild.Image.FileAppend(mcmAppendDialog.FileName);
            // or use
            //ImageFileManager.AppendImage(mcmAppendDialog.FileName, FF_DETECT, FChild.Image.DibHandle);
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            Screen.Cursor := FSaveCursor;

            if (ImageFileManager.Error <> EC_OK)
            then ShowMessage('Error writing image: ' + CErrorStrings[word(ImageFileManager.Error)]);
       end;
       AppendFileManager.Free;
  end;
end;  // TFormMain.AppendItemClick.


procedure TFormMain.BrowseItemClick(Sender : TObject);
begin
  if (FormBrowse = Nil)
  then begin
       FormBrowse := TFormBrowse.Create(Self);
       FormBrowse.OnProcessTime := OnSetProcessTime;
       FormBrowse.mcmThumbView.OnLoadImage := OpenImageFile;
  end;
  FormBrowse.Show;
end; // TFormMain.BrowseItemClick.


procedure TFormMain.WMStiAcquire(var Msg : TMessage);
var SourceName : string;
begin
  SourceName := mcmSTI.ActualDeviceName;
  mcmTWAIN.Acquire(SourceName);
end; // TFormMain.WMStiAcquire.


procedure TFormMain.AcquireItemClick(Sender : TObject);
begin
  mcmTWAIN.Acquire('');
end; // TFormMain.AcquireItemClick.


procedure TFormMain.SourceItemClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
end; // TFormMain.SourceItemClick.


{$IFNDEF DCB3}
procedure TFormMain.mcmTWAINImageReady(Sender   : TObject;
                                       pBmp     : Pointer;
                                       pBmpInfo : PBitmapInfo;
                                       hImage   : hBitmap;
                                       FilePath : String);
{$ELSE}
procedure TFormMain.mcmTWAINImageReady(Sender   : TObject;
                                       pBmp     : Pointer;
                                       pBmpInfo : PBitmapInfo;
                                       hImage   : Integer;
                                       FilePath : String);
{$ENDIF}
begin
  if (hImage <> 0) or FileExists(FilePath)
  then begin
       FChild := CreateImageWindow('TWAIN_', 0, 0, IF_NONE);
       if (hImage <> 0)
       then begin
            FChild.Image.DibHandle   := hImage;
            FChild.Image.XResolution := pBmpInfo^.bmiHeader.biXPelsPerMeter;
            FChild.Image.YResolution := pBmpInfo^.bmiHeader.biYPelsPerMeter;
       end;
       if FileExists(FilePath)
       then FChild.Image.FileOpen(FilePath);
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  UpdateMenu;
end; // TFormMain.mcmTWAINImageReady.


procedure TFormMain.PrintItemClick(Sender : TObject);
var mcmPrinter : TmcmPrinter;
    i          : integer;
begin
  if PrintDialog.Execute
  then begin
       mcmPrinter := TmcmPrinter.Create(Self);
       mcmPrinter.ImageCenter := True;
       mcmPrinter.ImageFitToPage := True;
       // Add all images(pages) to printer.
       for i := 0 to (MDIChildCount - 1)
       do begin
          if (MDIChildren[i] is TFormChild)
          then begin
               mcmPrinter.AddPage;
               FChild := TFormChild(MDIChildren[i]);
               mcmPrinter.Pages[i].Assign(FChild.Image);
          end;
       end;
       mcmPrinter.Print;
       mcmPrinter.Free;
  end;
end; // TFormMain.PrintItemClick.


procedure TFormMain.PrintPreviewItemClick(Sender : TObject);
var i, j : integer;
begin
  // Create the preview form. This form will create an instance if TmcmPrinter
  // accessed below as the mcmPrinter property on this form.
  FormPrintPreview := TFormPrintPreview.Create(Self);

  // Add all pages to printer (preview).
  j := 0;
  for i := 0 to (MDIChildCount - 1)
  do begin
     if (MDIChildren[i] is TFormChild)
     then begin
          FChild := TFormChild(MDIChildren[i]);
          FormPrintPreview.mcmPrinter.AddPage;
          FormPrintPreview.mcmPrinter.Pages[j].Assign(FChild.Image);
          inc(j);
     end;
  end;
  FormPrintPreview.mcmPrinter.PageBorderColor := RGB(0,0,128);
  FormPrintPreview.mcmPrinter.PageBorders := []; // [PB_TOP, PB_BOTTOM];
  FormPrintPreview.mcmPrinter.PageBorderWidth := 0; // 3;
  FormPrintPreview.mcmPrinter.PageBorderOffset := 0.10;

  // Show preview.
  FormPrintPreview.ShowModal;
  FormPrintPreview.Free;
end; // TFormMain.PrintPreviewItemClick.


procedure TFormMain.PrinterSetupItemClick(Sender : TObject);
begin
  if PrinterSetupDialog.Execute
  then begin
  end;
end; // TFormMain.PrintSetupItemClick.


procedure TFormMain.AssociatefileItemClick(Sender : TObject);
begin
  FormFormatAssociate := TFormFormatAssociate.Create(Self);
  FormFormatAssociate.ShowModal;
  FormFormatAssociate.Free;
end; // TFormMain.AssociatefileItemClick.


procedure TFormMain.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TFormMain.ExitItemClick.


//------------------------------------------------------------------------------
// Edit menu
//------------------------------------------------------------------------------

procedure TFormMain.EditMenuClick(Sender : TObject);
begin
  CopyItem.Enabled := (GetImageWindow <> Nil);
  if CopyItem.Enabled
  then CopySectionItem.Enabled := FChild.mcmRegion.Visible;
  DuplicateItem.Enabled := CopyItem.Enabled;
  PasteItem.Enabled := Clipboard.HasFormat(CF_DIB);
  EmptyClipboardItem.Enabled := (Clipboard.FormatCount > 0);
end; // TFormMain.EditMenuClick.


procedure TFormMain.CopyItemClick(Sender: TObject);
var AFormat : word;
    AData   : THandle;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       QueryPerformanceCounter(FStartTime);
       AFormat := CF_BITMAP; //DIB;
       FChild.Image.SaveToClipboardFormat(AFormat, AData);
       if (AData <> 0)
       then begin
            Clipboard.Open;
            Clipboard.SetAsHandle(AFormat, AData);
            Clipboard.Close;
       end;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.CopyItemClick.


procedure TFormMain.CopySectionItemClick(Sender : TObject);
var AFormat     : word;
    AData       : THandle;
    RegionImage : TmcmImage;
    Region      : TRect;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       QueryPerformanceCounter(FStartTime);
       RegionImage := Nil;
       Region := FChild.Region;
       FChild.Image.CopyRegion(RegionImage, Region);
       if Assigned(RegionImage)
       then begin
            AFormat := CF_DIB;
            RegionImage.SaveToClipboardFormat(AFormat, AData);
            if (AData <> 0)
            then begin
                 Clipboard.Open;
                 Clipboard.SetAsHandle(AFormat, AData);
                 Clipboard.Close;
            end;
            RegionImage.Free;
       end;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.CopySectionItemClick.


procedure TFormMain.PasteItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_DIB; // NOTE: CF_BITMAP is not supported yet.
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then begin
         FChild := CreateImageWindow('Paste_', 0, 0, IF_NONE);
         QueryPerformanceCounter(FStartTime);
         FChild.Image.LoadFromClipboardFormat(AFormat, AData);
         FChild.Image.ImageInfo.FileName := FChild.Caption;
         QueryPerformanceCounter(FEndTime);
         OnSetProcessTime(Self, FStartTime, FEndTime);
    end;
  finally
    Clipboard.Close;
  end;
end; // TFormMain.PasteItemClick.


procedure TFormMain.PasteSectionItemClick(Sender : TObject);
var AFormat    : word;
    AData      : THandle;
    PasteImage : TmcmImage;
    DoPaste    : boolean;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       Clipboard.Open;
       try
         AFormat :=  CF_DIB;
         // CF_BITMAP; // is not supported yet.
         AData := Clipboard.GetAsHandle(AFormat);
         if (AData <> 0)
         then begin
              DoPaste := True;
              PasteImage := TmcmImage.Create;
              PasteImage.LoadFromClipboardFormat(AFormat, AData);

              if (PasteImage.ImageFormat <> FChild.Image.ImageFormat)
              then begin
                   DoPaste := False;
                   ShowMessage('The color resolution of the image you are pasting into the selected image does not match!' + Chr($0D) +
                                'Paste to a new image and modify the color resolution, copy the modified image and then paste "As section"');
              end
              else if (PasteImage.ImageFormat in [IF_PAL4,IF_PAL8]) and
                      (FChild.Image.ImageFormat in [IF_PAL4,IF_PAL8])
                   then if (MessageDlg('You are pasting a palette indexed image into another palette indexed image.' + Chr($0D) +
                                       'If the palette from both images do not match, the result will look odd!' + Chr($0D) +
                                       'Continue?', mtWarning, [mbYes,mbNo], 0) = mrNo)
                        then DoPaste := False;
              if DoPaste
              then FChild.RegionImage := PasteImage
              else PasteImage.Free;
         end;
       finally
         Clipboard.Close;
       end;
  end;
end; // TFormMain.PasteSectionItemClick.


procedure TFormMain.DuplicateItemClick(Sender : TObject);
var SourceChild : TFormChild;
    ResultChild : TFormChild;
begin
  SourceChild := GetImageWindow;
  if Assigned(SourceChild)
  then begin
       ResultChild := CreateImageWindow('Duplicate_', 0, 0, IF_NONE);
       QueryPerformanceCounter(FStartTime);
       ResultChild.ImageCtrl.Image.Assign(SourceChild.ImageCtrl.Image);
       FChild.Image.ImageInfo.FileName := FChild.Caption;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.DuplicateItemClick.


procedure TFormMain.EmptyClipboardItemClick(Sender : TObject);
begin
  Clipboard.Clear;
end; // TFormMain.EmptyClipboardItemClick.


//------------------------------------------------------------------------------
// Image Menu
//------------------------------------------------------------------------------

procedure TFormMain.ImageMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then begin
       bHasImage := Assigned(FChild.Image);
       TransparentViewItem.Checked := FChild.Image.Transparent;
       FChild.Image.TransparentColor := RGB(255, 255, 255);
  end;

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
end; // TFormMain.ImageMenuClick


procedure TFormMain.InformationItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormImageInfo := TFormImageInfo.Create(Self);
       FormImageInfo.Image := FChild.Image;
       FormImageInfo.ShowModal;
       FormImageInfo.Free;
  end;
end; // TFormMain.InformationItemClick.


procedure TFormMain.CanvasSizeItemClick(Sender : TObject);
var FCurChild : TFormChild;
    Placement : TRect;
begin
  FCurChild := GetImageWindow;
  if Assigned(FCurChild)
  then begin
       FormCanvasSize := TFormCanvasSize.Create(Self);
       FormCanvasSize.FillColor := FormPaintBox.BackColor;
       FormCanvasSize.Image := FCurChild.Image;
       if (FormCanvasSize.ShowModal = mrOK)
       then begin
            QueryPerformanceCounter(FStartTime);
            FChild := CreateImageWindow('New_', FormCanvasSize.ImageWidth,
                                                FormCanvasSize.ImageHeight,
                                                FCurChild.Image.ImageFormat);

            if (FCurChild.Image.ImageFormat in [IF_BW, IF_GREY4, IF_PAL4, IF_GREY8, IF_PAL8])
            then FChild.Image.Palette :=  FCurChild.Image.Palette;
            FChild.Image.XResolution := FCurChild.Image.XResolution;
            FChild.Image.YResolution := FCurChild.Image.YResolution;
            FChild.Image.ImageInfo.Assign(FCurChild.Image.ImageInfo);
            FChild.Image.FillRGB(FormCanvasSize.FillColor);

            Placement := Rect(FormCanvasSize.LeftOffset,
                              FormCanvasSize.TopOffset,
                              FCurChild.Image.Width - 1 + FormCanvasSize.LeftOffset,
                              FCurChild.Image.Height - 1 + FormCanvasSize.TopOffset);
            FChild.Image.PasteRegion(FCurChild.ImageCtrl.Image, Placement);

            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);
       end;
       FormCanvasSize.Free;
       UpdateMenu;
  end;
end; // TFormMain.CanvasSizeItemClick.


procedure TFormMain.TransparentViewItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       TransparentViewItem.Checked := Not(FChild.Image.Transparent);
       FChild.Image.Transparent := TransparentViewItem.Checked;
       FChild.Repaint;
  end;
end; // TFormMain.TransparentViewItemClick.


//------------------------------------------------------------------------------
// Color menu
//------------------------------------------------------------------------------

procedure TFormMain.ColorMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
  if bHasImage
  then begin
       PaletteMenu.Enabled    := (FChild.Image.ImageFormat < IF_RGB24);
       ThresholdItem.Enabled  := (FChild.Image.ImageFormat >= IF_GREY8);
       SplitColorMenu.Enabled := (FChild.Image.ImageFormat >= IF_PAL8);
  end;
end; // TFormMain.ColorMenuClick


procedure TFormMain.UsedColorsItemClick(Sender : TObject);
var NoColors : integer;
begin
  FChild := GetImageWindow;
  QueryPerformanceCounter(FStartTime);
  NoColors := FChild.Image.UniqueColors;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
  ShowMessage('Used colors: ' + IntToStr(NoColors));
end; // TFormMain.UsedColorsItemClick


procedure TFormMain.EditPaletteItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormPalette := TFormPalette.Create(Self);
       FormPalette.Image := FChild.Image;
       FormPalette.ShowModal;
       FormPalette.Free;
  end;
end; // TFormMain.EditPaletteItemClick.


procedure TFormMain.InvertPaletteItemClick(Sender : TObject);
var i        : integer;
    pPal     : PLogPalette;
    NoColors : integer;
begin
  FChild := GetImageWindow;
  QueryPerformanceCounter(FStartTime);
  NoColors := FChild.Image.GetPaletteEntries(Nil);
  if (NoColors > 0)
  then begin
       GetMem(pPal, NoColors * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
       if Assigned(pPal)
       then begin
            FChild.Image.GetPaletteEntries(pPal);
            {$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}
            for i := 0 to (pPal^.palNumEntries - 1)
            do begin
               with pPal^.palPalEntry[i]
               do begin
                  peRed   := 255 - peRed;
                  peGreen := 255 - peGreen;
                  peBlue  := 255 - peBlue;
               end;
            end;
            {$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

            FChild.Image.SetPaletteEntries(pPal);
            FreeMem(pPal);
       end;
  end;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
end; // TFormMain.InvertPaletteItemClick.


procedure TFormMain.InvertItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  mcmImageColor.SourceImage[0] := FChild.Image;
  mcmImageColor.ResultImage  := FChild.Image;

  // Get start time
  QueryPerformanceCounter(FStartTime);

  mcmImageColor.Invert;

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild.Invalidate;
end; // TFormMain.InvertItemClick.


procedure TFormMain.ThresholdItemClick(Sender : TObject);
begin
  // Get source/MDI child window.
  FChild := GetImageWindow;

  // Create Threshold dialogue.
  FormThreshold := TFormThreshold.Create(Self);
  FormThreshold.Method := 0;
  FormThreshold.Level := 127;
  FormThreshold.Percentage := 50;

  // Set source image.
  FormThreshold.SourceImage := FChild.Image;

  // Show dialogue to user.
  if (FormThreshold.ShowModal = mrOK)
  then begin
       // Get processing time.
       FStartTime := FormThreshold.TimeStart;
       FEndTime   := FormThreshold.TimeEnd;
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Create a new MDI window
       FChild := CreateImageWindow('Threshold_', 0, 0, IF_NONE);

       // Get result image from threshold dialogue and assign to MDI window.
       FChild.Image := FormThreshold.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  FormThreshold.Free;
end; // TFormMain.ThresholdItemClick.


procedure TFormMain.BrightnessContrastItemClick(Sender : TObject);
begin
  // Get source/MDI child window.
  FChild := GetImageWindow;
  FormBrightContrast := TFormBrightContrast.Create(Self);

  // Set source image.
  FormBrightContrast.SourceImage := FChild.Image;
  FormBrightContrast.Brightness := 0;
  FormBrightContrast.Contrast := 0;

  if (FormBrightContrast.ShowModal = mrOK)
  then begin
       // Get processing time.
       FStartTime := FormBrightContrast.TimeStart;
       FEndTime   := FormBrightContrast.TimeEnd;
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Create a new MDI window
       FChild := CreateImageWindow('BrightContrast_', 0, 0, IF_NONE);

       // Get result image from threshold dialogue and assign to MDI window.
       FChild.Image := FormBrightContrast.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  FormBrightContrast.Free;
end; // TFormMain.BrightnessContrastItemClick.


procedure TFormMain.GammaCorrectionItemClick(Sender : TObject);
begin
  // Get source/MDI child window.
  FChild := GetImageWindow;

  FormGamma := TFormGamma.Create(Self);
  // Set source image.
  FormGamma.SourceImage := FChild.Image;
  FormGamma.Gamma := 1.0;

  if (FormGamma.ShowModal = mrOK)
  then begin
       // Get processing time.
       FStartTime := FormGamma.TimeStart;
       FEndTime   := FormGamma.TimeEnd;
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Create a new MDI window
       FChild := CreateImageWindow('Gamma_', 0, 0, IF_NONE);

       // Get result image from threshold dialogue and assign to MDI window.
       FChild.Image := FormGamma.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  FormGamma.Free;
end; // TFormMain.GammaCorrectionItemClick.


//------------------------------------------------------------------------------
// Convert to
//------------------------------------------------------------------------------

procedure TFormMain.GreyScaleItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetIntensity;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  if Assigned(TheImage)
  then begin
       FChild := CreateImageWindow('Grey_', 0, 0, IF_NONE);
       FChild.Image := TheImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
end; // TFormMain.GreyscaleItemClick.

//------------------------------------------------------------------------------
// Color decrease
//------------------------------------------------------------------------------

procedure TFormMain.DecColorDepthMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       case FChild.Image.ImageFormat of
       IF_BW     : begin
                     InclWindowsPalItem.Enabled := False;
                     Dec2Colors.Enabled   := False;
                     Dec16Colors.Enabled  := False;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                     Dec24MColors.Enabled := False;
                   end;
       IF_GREY4,
       IF_PAL4   : begin
                     InclWindowsPalItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := False;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                     Dec24MColors.Enabled := False;
                   end;
       IF_GREY8,
       IF_PAL8   : begin
                     InclWindowsPalItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                     Dec24MColors.Enabled := False;
                   end;
       IF_RGB15,
       IF_RGB16  : begin
                     InclWindowsPalItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := False;
                     Dec24MColors.Enabled := False;
                   end;
       IF_RGB24  : begin
                     InclWindowsPalItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := True;
                     Dec24MColors.Enabled := False;
                   end;
       IF_RGBA32 : begin
                     InclWindowsPalItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := True;
                     Dec24MColors.Enabled := True;
                   end;
       end;
  end;
end; // TFormMain.DecColorDepthMenuClick.


procedure TFormMain.InclWindowsPalItemClick(Sender : TObject);
begin
  InclWindowsPalItem.Checked := Not(InclWindowsPalItem.Checked);
end; // TFormMain.IncludeWindowsPaletteItemClick.


procedure TFormMain.Dec2ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_BW, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo2Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec2Colors1bitClick.


procedure TFormMain.Dec16ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL4, InclWindowsPalItem.Checked);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo4Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec16Colors4bitClick.


procedure TFormMain.Dec256ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL8, InclWindowsPalItem.Checked);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo8Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec256Colors8bitClick.


procedure TFormMain.Dec32KColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB15, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo15Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec32KColorsClick.


procedure TFormMain.Dec24MColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB24, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo24Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec24MColorsClick.


//------------------------------------------------------------------------------
// Color increase
//------------------------------------------------------------------------------

procedure TFormMain.IncColorDepthMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       case FChild.Image.ImageFormat of
       IF_BW     : begin
                     Inc16Colors.Enabled       := True;
                     Inc256Colors.Enabled      := True;
                     Inc32KColors.Enabled      := True;
                     Inc16MillColors.Enabled   := True;
                     Inc16MillColors32.Enabled := True;
                   end;
       IF_GREY4,
       IF_PAL4   : begin
                     Inc16Colors.Enabled       := False;
                     Inc256Colors.Enabled      := True;
                     Inc32KColors.Enabled      := True;
                     Inc16MillColors.Enabled   := True;
                     Inc16MillColors32.Enabled := True;
                   end;
       IF_GREY8,
       IF_PAL8   : begin
                     Inc16Colors.Enabled       := False;
                     Inc256Colors.Enabled      := False;
                     Inc32KColors.Enabled      := True;
                     Inc16MillColors.Enabled   := True;
                     Inc16MillColors32.Enabled := True;
                   end;
       IF_RGB15,
       IF_RGB16  : begin
                     Inc16Colors.Enabled       := False;
                     Inc256Colors.Enabled      := False;
                     Inc32KColors.Enabled      := False;
                     Inc16MillColors.Enabled   := True;
                     Inc16MillColors32.Enabled := True;
                   end;
       IF_RGB24  : begin
                     Inc16Colors.Enabled       := False;
                     Inc256Colors.Enabled      := False;
                     Inc32KColors.Enabled      := False;
                     Inc16MillColors.Enabled   := False;
                     Inc16MillColors32.Enabled := True;
                   end;
       IF_RGBA32 : begin
                     Inc16Colors.Enabled       := False;
                     Inc256Colors.Enabled      := False;
                     Inc32KColors.Enabled      := False;
                     Inc16MillColors.Enabled   := False;
                     Inc16MillColors32.Enabled := False;
                   end;
       end;
  end;
end; // TFormMain.IncColorDepthMenuClick.


procedure TFormMain.Inc16ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL4, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo4Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc16Colors4bitClick.


procedure TFormMain.Inc256ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL8, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo8Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc256Colors8bitClick.


procedure TFormMain.Inc32KColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB15, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo15Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc32KColorsClick.


procedure TFormMain.Inc16MillColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB24, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo24Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc16MillColorsClick.


procedure TFormMain.Inc16MillColors32Click(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGBA32, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo32Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc16MillColors32Click.


//------------------------------------------------------------------------------
// Color split
//------------------------------------------------------------------------------

procedure TFormMain.CIEXD65ItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCIEXD65;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('X_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CIEXD65ItemClick.


procedure TFormMain.CIEYD65ItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCIEYD65;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Y_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CIEYD65ItemClick.


procedure TFormMain.CIEZD65ItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCIEZD65;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Z_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CIEZD65ItemClick.


procedure TFormMain.CyanItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCyanChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Cyan_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CyanItemClick.


procedure TFormMain.MagentaItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetMagentaChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Magenta_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.MagentaItemClick.


procedure TFormMain.YellowItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetYellowChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Yellow_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.YellowItemClick.


procedure TFormMain.BlackItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetBlackChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Black_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.BlackItemClick.


procedure TFormMain.HueItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetHueChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Hue_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.HueItemClick.


procedure TFormMain.SaturationItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetSaturationChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Saturation_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.SaturationItemClick.


procedure TFormMain.ValueItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetValueChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Value_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.ValueItemClick.


procedure TFormMain.RGBMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       case FChild.Image.ImageFormat of
       IF_RGB24  : begin
                     AlphaItem.Enabled := False;
                   end;
       IF_RGBA32 : begin
                     AlphaItem.Enabled := True;
                   end;
       end;
  end;
end; // TFormMain.RGBMenuClick.


procedure TFormMain.RedChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetRedChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Red_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.RedChannelItemClick.


procedure TFormMain.GreenChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetGreenChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Green_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.GreenChannelItemClick.


procedure TFormMain.BlueChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetBlueChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Blue_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.BlueChannelItemClick.


procedure TFormMain.AlphaItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetAlphaChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Alpha_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.AlphaItemClick.


procedure TFormMain.YChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetLuminanceChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Y_Luminance_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.YChannelItemClick.


procedure TFormMain.CbChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCbChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Cb_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CbChannelItemClick.


procedure TFormMain.CrChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCrChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Cr_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CrChannelItemClick.


procedure TFormMain.IChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetNTSCIChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('I_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.IChannelItemClick.


procedure TFormMain.QChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetNTSCQChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Q_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.QChannelItemClick.


procedure TFormMain.UChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetPALUChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('U_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.UChannelItemClick.


procedure TFormMain.VChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetPALVChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('V_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.VChannelItemClick.


//------------------------------------------------------------------------------
// Color combine
//------------------------------------------------------------------------------

procedure TFormMain.CombineCIEItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 3;
  FormCombineImage.FormatName := 'CIE';
  FormCombineImage.ChannelNames[1] := 'X';
  FormCombineImage.ChannelNames[2] := 'Y';
  FormCombineImage.ChannelNames[3] := 'Z';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineCIED65;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('CIE-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineCIEItemClick.


procedure TFormMain.CombineCMYKItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 4;
  FormCombineImage.FormatName := 'CMYK';
  FormCombineImage.ChannelNames[1] := 'Cyan';
  FormCombineImage.ChannelNames[2] := 'Magenta';
  FormCombineImage.ChannelNames[3] := 'Yellow';
  FormCombineImage.ChannelNames[4] := 'Black';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;
       mcmImageColor.SourceImage[3] := TFormChild(FormCombineImage.SelectedImage[4]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineCMYK;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('CMYK-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineCMYKItemClick.


procedure TFormMain.CombineHSVItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 3;
  FormCombineImage.FormatName := 'HSV';
  FormCombineImage.ChannelNames[1] := 'Hue';
  FormCombineImage.ChannelNames[2] := 'Saturation';
  FormCombineImage.ChannelNames[3] := 'Value';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineHSV;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('HSV-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineHSVItemClick.


procedure TFormMain.CombineRGBItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 4;
  FormCombineImage.FormatName := 'RGB';
  FormCombineImage.ChannelNames[1] := 'Red';
  FormCombineImage.ChannelNames[2] := 'Green';
  FormCombineImage.ChannelNames[3] := 'Blue';
  FormCombineImage.ChannelNames[4] := 'Alpha';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;
       if (TFormChild(FormCombineImage.SelectedImage[4]) <> Nil)
       then mcmImageColor.SourceImage[3] := TFormChild(FormCombineImage.SelectedImage[4]).Image
       else mcmImageColor.SourceImage[3] := Nil;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineRGB;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('RGB-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineRGBItemClick.


procedure TFormMain.CombineYCbCrItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 3;
  FormCombineImage.FormatName := 'YCbCr';
  FormCombineImage.ChannelNames[1] := 'Y';
  FormCombineImage.ChannelNames[2] := 'Cb';
  FormCombineImage.ChannelNames[3] := 'Cr';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineYCbCr;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('YCbCr-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineYCbCrItemClick.


procedure TFormMain.CombineYIQCItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 3;
  FormCombineImage.FormatName := 'YIQ';
  FormCombineImage.ChannelNames[1] := 'Y';
  FormCombineImage.ChannelNames[2] := 'I';
  FormCombineImage.ChannelNames[3] := 'Q';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineYIQ;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('YIQ-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineYIQCItemClick.


procedure TFormMain.CombineYUVItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FormCombineImage := TFormCombineImage.Create(Self);
  FormCombineImage.NumChannels := 3;
  FormCombineImage.FormatName := 'YUV';
  FormCombineImage.ChannelNames[1] := 'Y';
  FormCombineImage.ChannelNames[2] := 'U';
  FormCombineImage.ChannelNames[3] := 'V';

  if (FormCombineImage.ShowModal = mrOK)
  then begin
       mcmImageColor.ResultImage := Nil;
       mcmImageColor.SourceImage[0] := TFormChild(FormCombineImage.SelectedImage[1]).Image;
       mcmImageColor.SourceImage[1] := TFormChild(FormCombineImage.SelectedImage[2]).Image;
       mcmImageColor.SourceImage[2] := TFormChild(FormCombineImage.SelectedImage[3]).Image;

       QueryPerformanceCounter(FStartTime);
       TheImage := mcmImageColor.CombineYUV;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (TheImage <> Nil)
       then begin
            FChild := CreateImageWindow('YUV-RGB_', 0, 0, IF_NONE);
            FChild.Image := TheImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
  end;
  FormCombineImage.Free;
end; // TFormMain.CombineYUVItemClick.


//------------------------------------------------------------------------------
// Bayer conversion (Grey to 24 bit RGB)
//------------------------------------------------------------------------------

procedure TFormMain.GreyToBayerItemClick(Sender : TObject);
var ResultImage  : TmcmImage;
    FBayerFilter : TmcmBayer;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ResultImage := TmcmImage.Create;
       ResultImage.Width  := FChild.Image.Width;
       ResultImage.Height := FChild.Image.Height;
       ResultImage.ImageFormat := IF_RGB24;

       mcmImageColor.SourceImage[0] := FChild.Image;
       mcmImageColor.ResultImage  := ResultImage;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       // BF_OddOdd, BF_OddEven, BF_EvenOdd, BF_EvenEven
       FBayerFilter := TmcmBayer((Sender as TMenuItem).Tag);
       mcmImageColor.BayerToRGB(FBayerFilter);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       FChild := CreateImageWindow('Bayer_', 0, 0, IF_NONE);
       FChild.Image := mcmImageColor.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
end; // TFormMain.GreyToBayerItemClick.


//------------------------------------------------------------------------------
// Histogram Menu
//------------------------------------------------------------------------------

procedure TFormMain.HistogramMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
end; // TFormMain.HistogramMenuClick.


procedure TFormMain.HistogramItemClick(Sender : TObject);
var FormHistogram : TFormHistogram;
begin
  FChild := GetImageWindow;
  FormHistogram := TFormHistogram.Create(Self);
  FormHistogram.Image := FChild.Image;
  FormHistogram.ShowModal;
  FormHistogram.Free;
end; // TFormMain.HistogramItemClick.


procedure TFormMain.EqualizeItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  mcmImageColor.SourceImage[0] := FChild.Image;
  mcmImageColor.ResultImage := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  mcmImageColor.Equalize;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
  FChild.Invalidate;
end; // TFormMain.EqualizeItemClick.


procedure TFormMain.EqualizeLumItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if (FChild.Image.ImageFormat in [IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       mcmImageColor.SourceImage[0] := FChild.Image;
       mcmImageColor.ResultImage := FChild.Image;
       QueryPerformanceCounter(FStartTime);
       mcmImageColor.EqualizeLuminance;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
       FChild.Invalidate;
  end;
end; // EqualizeLuminanceItemClick;


procedure TFormMain.StretchColorItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  mcmImageColor.SourceImage[0] := FChild.Image;
  mcmImageColor.ResultImage := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  mcmImageColor.Stretch;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild.Invalidate;
end; // TFormMain.StretchColorItemClick.


//------------------------------------------------------------------------------
// Filter menu
//------------------------------------------------------------------------------

procedure TFormMain.FilterMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
end; // TFormMain.FilterMenuClick.


procedure TFormMain.FilterImage(Method       : TmcmFilter;
                                Height       : word;
                                Width        : word;
                                Bias         : integer;
                                ScaleFactor  : integer;
                                Hysteresis   : word;
                                GaussSD      : double;
                                DeltaSD      : double;
                                TraceAuto    : boolean;
                                TracePercent : integer;
                                TraceLow     : word;
                                TraceHigh    : word;
                                FormFilter   : TForm);
var ResultImage    : TmcmImage;
    i, j, k        : integer;
    NoKernels      : integer;
begin
  Screen.Cursor := crHourGlass;
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ResultImage := TmcmImage.Create;
       ResultImage.Width  := FChild.Image.Width;
       ResultImage.Height := FChild.Image.Height;
       ResultImage.ImageFormat := FChild.Image.ImageFormat;

       mcmImageFilter.FilterHeight  := Height;
       mcmImageFilter.FilterWidth   := Width;
       mcmImageFilter.Hysteresis    := Hysteresis;
       mcmImageFilter.MaxIterations := 1;
       mcmImageFilter.GaussSD       := GaussSD;
       mcmImageFilter.TraceAuto     := TraceAuto;
       mcmImageFilter.TraceHigh     := TraceHigh;
       mcmImageFilter.TraceLow      := TraceLow;
       mcmImageFilter.TracePercent  := TracePercent;

       mcmImageFilter.SourceImage[0] := FChild.Image;
       mcmImageFilter.ResultImage  := ResultImage;

       // Special case for User defined filters.
       if Assigned(FormFilter)
       then begin
            if (Method < FLT_USERDBLRMS)
            then NoKernels := 1
            else NoKernels := 2;

            mcmImageFilter.Bias := Bias;
            mcmImageFilter.ScaleFactor := ScaleFactor;

            for k := 0 to (NoKernels - 1)
            do for i := 0 to (Width - 1)
               do for j := 0 to (Height - 1)
                  do mcmImageFilter.Kernel[k,i,j] := TFormFilterUser(FormFilter).Kernel[k,i,j];
       end;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       // Filter image.
       mcmImageFilter.Filter(Method);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ResultImage.Palette := FChild.Image.Palette;

       if (mcmImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('Filter_', 0, 0, IF_NONE);
            FChild.Image := mcmImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else ShowMessage('Error filtering image: ' + CErrorStrings[word(mcmImageFilter.Error)]);
  end;
  Screen.Cursor := crDefault;
end; // TFormMain.FilterImage.


procedure TFormMain.FilterBrowserClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormFilterBrowser := TFormFilterBrowser.Create(Self);
       FormFilterBrowser.HelpContext := 26;
       FormFilterBrowser.SourceImage := FChild.Image;
       if (FormFilterBrowser.ShowModal = mrOK)
       then begin
            OnSetProcessTime(Self, FormFilterBrowser.TimeStart, FormFilterBrowser.TimeEnd);

            FChild := CreateImageWindow('Filter_', 0, 0, IF_NONE);
            FChild.Image := FormFilterBrowser.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end;
       FormFilterBrowser.Free;
  end;
end; // TFormMain.FilterBrowserClick.


procedure TFormMain.UserDefFilterItemClick(Sender : TObject);
var FormFilterUser : TFormFilterUser;
    i, j, k        : integer;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormFilterUser := TFormFilterUser.Create(Self);

       FormFilterUser.Image := FChild.Image;
       FormFilterUser.Bias := mcmImageFilter.Bias;
       FormFilterUser.ScaleFactor := mcmImageFilter.ScaleFactor;
       FormFilterUser.FilterHeight := mcmImageFilter.FilterHeight;
       FormFilterUser.FilterWidth := mcmImageFilter.FilterWidth;
       for k := 0 to 1
       do for i := 0 to (mcmImageFilter.FilterWidth - 1)
          do for j := 0 to (mcmImageFilter.FilterHeight - 1)
             do FormFilterUser.Kernel[k,i,j] := mcmImageFilter.Kernel[k,i,j];

       if (FormFilterUser.ShowModal = mrOK)
       then begin
            with FormFilterUser
            do FilterImage(Filter, FilterHeight, FilterWidth, Bias, ScaleFactor, 0, 2.0, 0.8, True, 50, 100, 200, FormFilterUser);
       end;
       FormFilterUser.Free;
  end;
end; // TFormMain.UserDefFilterItemClick.


procedure TFormMain.FilterItemClick(Sender : TObject);
var Method : TmcmFilter;
begin
  Screen.Cursor := crHourGlass;
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       Method := TmcmFilter(TMenuItem(Sender).Tag);
       FilterImage(Method, 3, 3, 1, 1, 9, 2.0, 0.8, True, 50, 100, 200, Nil);
  end;
  Screen.Cursor := crDefault;
end; // TFormMain.FilterItemClick.


//------------------------------------------------------------------------------
// Despeckle
//------------------------------------------------------------------------------

procedure TFormMain.DespecklelightItemClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;
       ImageFilter.ResultImage := TmcmImage.Create;

       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageFilter.MaxIterations := 1;
       ImageFilter.Filter(FLT_DESPECKLE_A);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('Despeckle_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageFilter.Error)]);

       FChild.Scale := FChild.Scale;
       ImageFilter.Free;
  end;
end; // TFormMain.DespecklelightItemClick.


procedure TFormMain.DespeckleItemClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;
       ImageFilter.ResultImage := TmcmImage.Create;

       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageFilter.MaxIterations := 10;
       ImageFilter.Filter(FLT_DESPECKLE_B);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('Despeckle_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageFilter.Error)]);

       FChild.Scale := FChild.Scale;
       ImageFilter.Free;
  end;
end; // TFormMain.DespeckleItemClick.


procedure TFormMain.DespeckleAltItemClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;
       ImageFilter.ResultImage := TmcmImage.Create;

       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageFilter.MaxIterations := 1;
       ImageFilter.Filter(FLT_DESPECKLE_C);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Validate result image and insert this into a new window.
       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('DespeckleAlt_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageFilter.Error)]);
       FChild.Scale := FChild.Scale;

       ImageFilter.Free;
  end;
end; // TFormMain.DespeckleAltItemClick.


//------------------------------------------------------------------------------
// Specialised Edge filter
//------------------------------------------------------------------------------

procedure TFormMain.MarrHildrethClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  // Note: the Marr-Hildreth filter can only be used with grey scale image.
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;

       // Create result image
       ImageFilter.ResultImage := TmcmImage.Create;
       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Set-up Marr-Hildreth parameters
       ImageFilter.GaussSD := 2.0;
       ImageFilter.DeltaSD := 0.8;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       // Apply Marr-Hildreth filter
       ImageFilter.Filter(FLT_MARRHILDRETH);

       // Get end time and display processing time
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Validate result image and insert this into a new window.
       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('MarrHildreth_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else begin
            ImageFilter.ResultImage.Free;
            ShowMessage('Error filtering image: ' + CErrorStrings[word(ImageFilter.Error)]);
       end;
       FChild.Scale := FChild.Scale;

       ImageFilter.Free;
  end;
end; // TFormMain.MarrHildrethClick.


procedure TFormMain.CannyClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  // Note: the Canny filter can only be used with grey scale image.
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;

       // Create result image.
       ImageFilter.ResultImage := TmcmImage.Create;
       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Set-up Canny parameters.
       ImageFilter.TraceAuto := True;
       if ImageFilter.TraceAuto
       then ImageFilter.TracePercent := 50
       else begin
            ImageFilter.TraceHigh    := 25;
            ImageFilter.TraceLow     := 5;
       end;
       if (FChild.Image.UniqueColors > 2)
       then begin
            // Grey scale image.
            ImageFilter.GaussSD := 2.0;
       end
       else begin
            // For Grey scale images containing B/W or 2 values use
            //   GaussSD := 1.4 and TracePercent = 5;
            ImageFilter.GaussSD      := 1.4;
            ImageFilter.TracePercent := 5;
       end;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       // Apply Canny filter
       ImageFilter.Filter(FLT_CANNY);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Validate result image and insert this into a new window.
       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('Canny_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else begin
            ImageFilter.ResultImage.Free;
            ShowMessage('Error filtering image: ' + CErrorStrings[word(ImageFilter.Error)]);
       end;
       FChild.Scale := FChild.Scale;

       ImageFilter.Free;
  end;
end; // TFormMain.CannyClick.


procedure TFormMain.ShenCastanClick(Sender : TObject);
var ImageFilter : TmcmImageFilter;
begin
  // Note: the Shen-Castan filter can only be used with grey scale image.
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageFilter := TmcmImageFilter.Create(Self);
       ImageFilter.SourceImage[0] := FChild.Image;

       // Create result image.
       ImageFilter.ResultImage := TmcmImage.Create;
       ImageFilter.ResultImage.Width := ImageFilter.SourceImage[0].Width;
       ImageFilter.ResultImage.Height := ImageFilter.SourceImage[0].Height;
       ImageFilter.ResultImage.ImageFormat := IF_GREY8;
       ImageFilter.ResultImage.CreateGreyPalette;

       // Set-up Shen Castan parameters.
       ImageFilter.SmoothFactor := 0.7;
       ImageFilter.FilterSize   := 3;    
       ImageFilter.TraceAuto    := True;
       if ImageFilter.TraceAuto
       then begin
            if (FChild.Image.UniqueColors > 2)
            // Grey scale image.
            then ImageFilter.TracePercent := 50
            // For Grey scale images containing B/W or 2 values use
            //   TracePercent = 10;
            else ImageFilter.TracePercent := 10;
       end
       else begin
            ImageFilter.TraceHigh    := 25;
            ImageFilter.TraceLow     := 5;
       end;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       // Apply Shen Castan filter
       ImageFilter.Filter(FLT_SHENCASTAN);

       // Get end time and display processing time
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Validate result image and insert this into a new window.
       if (ImageFilter.Error = EC_OK)
       then begin
            FChild := CreateImageWindow('ShenCastan_', 0, 0, IF_NONE);
            FChild.Image := ImageFilter.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;
       end
       else begin
            ImageFilter.ResultImage.Free;
            ShowMessage('Error filtering image: ' + CErrorStrings[word(ImageFilter.Error)]);
       end;
       FChild.Scale := FChild.Scale;

       ImageFilter.Free;
  end;
end; // TFormMain.ShenCastanClick.


//------------------------------------------------------------------------------
// Transform menu
//------------------------------------------------------------------------------

procedure TFormMain.TransformMenuClick(Sender : TObject);
var bHasImage : boolean;
//    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  StretchItem.Enabled := bHasImage and
                        (FChild.Image.ImageFormat in [IF_BW,IF_PAL8,IF_GREY8,IF_RGB24,IF_RGBA32]);
  RotateItem.Enabled := StretchItem.Enabled;
  AffineItem.Enabled := StretchItem.Enabled;
  FlipItem.Enabled   := bHasImage;
  MirrorItem.Enabled := bHasImage and (FChild.Image.BitCount <> 4);
  DeskewItem.Enabled := StretchItem.Enabled;
  DeskewAndAutoCrop.Enabled := DeskewItem.Enabled;

end; // TFormMain.TransformMenuClick.


procedure TFormMain.FlipItemClick(Sender : TObject);
var mcmImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       mcmImageTransform := TmcmImageTransform.Create(Self);
       mcmImageTransform.SourceImage[0] := FChild.Image;
       mcmImageTransform.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       mcmImageTransform.Flip;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       mcmImageTransform.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.FlipItemClick.


procedure TFormMain.MirrorItemClick(Sender : TObject);
var mcmImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       mcmImageTransform := TmcmImageTransform.Create(Self);
       mcmImageTransform.SourceImage[0] := FChild.Image;
       mcmImageTransform.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       mcmImageTransform.Mirror;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       mcmImageTransform.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.MirrorItemClick.


procedure TFormMain.AffineItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormAffine := TFormAffine.Create(Self);
       FormAffine.Image := FChild.Image;
       FormAffine.Interpolate := ST_NEAREST; //ST_LINEAR;
       if (FormAffine.ShowModal = mrOK)
       then begin
            ImageTransform := TmcmImageTransform.Create(Self);
            ImageTransform.SourceImage[0] := FChild.Image;

            ImageTransform.Interpolate := TmcmInterpolate(FormAffine.Interpolate);
            ImageTransform.Degree := FormAffine.Degree;
            ImageTransform.xDist  := FormAffine.xDist;
            ImageTransform.yDist  := FormAffine.yDist;
            ImageTransform.xScale := FormAffine.xScale / 100.0;
            ImageTransform.yScale := FormAffine.yScale / 100.0;
            ImageTransform.xShear := FormAffine.xShear / ImageTransform.SourceImage[0].Width;
            ImageTransform.yShear := FormAffine.yShear / ImageTransform.SourceImage[0].Height;

            ImageTransform.BKColor := $FFFFFF;

            // Get start time
            QueryPerformanceCounter(FStartTime);

            ImageTransform.Affine;

            // Get end time and display processing time.
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            if (ImageTransform.Error = EC_OK)
            then begin
                 FChild := CreateImageWindow('Affine_', 0, 0, IF_NONE);
                 FChild.Image := ImageTransform.ResultImage;
                 FChild.Image.ImageInfo.FileName := FChild.Caption;
            end
            else ShowMessage('Error transforming image: ' + CErrorStrings[word(ImageTransform.Error)]);

            ImageTransform.Free;
       end;
       FormAffine.Free;
  end;
end; // TFormMain.AffineItemClick.


procedure TFormMain.StretchItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormResize := TFormResize.Create(Self);
       FormResize.Image := FChild.Image;
       FormResize.Interpolate := ST_NEAREST; //ST_LINEAR;
       if (FormResize.ShowModal = mrOK)
       then begin
            if (FormResize.PixelWidth <> FChild.Image.Width) or
               (FormResize.PixelHeight <> FChild.Image.Height)
            then begin
                 ImageTransform := TmcmImageTransform.Create(Self);
                 ImageTransform.SourceImage[0] := FChild.Image;

                 ImageTransform.ResultImage  := TmcmImage.Create;
                 ImageTransform.ResultImage.Width   := FormResize.PixelWidth;
                 ImageTransform.ResultImage.Height  := FormResize.PixelHeight;
                 ImageTransform.ResultImage.ImageFormat := FChild.Image.ImageFormat;
                 ImageTransform.ResultImage.Palette := FChild.Image.Palette;

                 // Get start time
                 QueryPerformanceCounter(FStartTime);

                 ImageTransform.Resize(FormResize.Interpolate);

                 // Get end time and display processing time.
                 QueryPerformanceCounter(FEndTime);
                 OnSetProcessTime(Self, FStartTime, FEndTime);

                 if (ImageTransform.Error = EC_OK)
                 then begin
                      FChild := CreateImageWindow('Stretch_', 0, 0, IF_NONE);
                      FChild.Image := ImageTransform.ResultImage;
                      FChild.Image.ImageInfo.FileName := FChild.Caption;
                      FChild.Image.XResolution := FormResize.Resolution;
                      FChild.Image.YResolution := FormResize.Resolution;
                 end
                 else begin
                      ImageTransform.ResultImage.Free;
                      ShowMessage('Error scaling/stretching image: ' + CErrorStrings[word(ImageTransform.Error)]);
                 end;

                 ImageTransform.Free;
            end
            else begin
                 FChild.Image.XResolution := FormResize.Resolution;
                 FChild.Image.YResolution := FormResize.Resolution;
            end;
       end;
       FormResize.Free;
  end;
end; // TFormMain.StretchItemClick.


procedure TFormMain.RotateItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
    Dir            : boolean;
    Degrees        : double;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormRotate := TFormRotate.Create(Self);
       FormRotate.Interpolate := ST_NEAREST; //ST_LINEAR;
       FormRotate.Image := FChild.Image;

       if (FormRotate.ShowModal = mrOK)
       then begin
            Dir         := FormRotate.Direction;
            Degrees     := FormRotate.Degrees;
            ImageTransform := TmcmImageTransform.Create(Self);
            ImageTransform.SourceImage[0] := FChild.Image;

            ImageTransform.ResultImage := TmcmImage.Create;
            ImageTransform.ResultImage.ImageFormat := FChild.Image.ImageFormat;
            ImageTransform.ResultImage.Palette := FChild.Image.Palette;
            ImageTransform.BKColor := $FFFFFF;
            ImageTransform.Interpolate := FormRotate.Interpolate;

            // ImageTransform automatically creates the result image.

            // Get start time
            QueryPerformanceCounter(FStartTime);

            ImageTransform.Rotate(Dir, Degrees);

            // Get end time and display processing time.
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            if (ImageTransform.Error = EC_OK)
            then begin
                 FChild := CreateImageWindow('Rotate_', 0, 0, IF_NONE);
                 FChild.Image := ImageTransform.ResultImage;
                 FChild.Image.ImageInfo.FileName := FChild.Caption;
            end
            else begin
                 ImageTransform.ResultImage.Free;
                 ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);
            end;

            ImageTransform.Free;
       end;
       FormRotate.Free;
  end;
end; // TFormMain.RotateItemClick.


procedure TFormMain.RotateFixItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageTransform := TmcmImageTransform.Create(Self);
       ImageTransform.SourceImage[0] := FChild.Image;
       ImageTransform.BKColor := $FFFFFF;
       // As we are rotating 90,180 or 270 degrees interpolation is not
       // necessary and nearest neighbour is the right choice.
       ImageTransform.Interpolate := ST_NEAREST;

       // ImageTransform automatically creates the result image.
       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageTransform.Rotate(True, TMenuItem(Sender).Tag);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (ImageTransform.Error = EC_OK)
       then FChild.Image := ImageTransform.ResultImage
       else begin
            ImageTransform.ResultImage.Free;
            ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);
       end;

       FChild.Scale := FChild.Scale;
       ImageTransform.Free;
  end;
end; // TFormMain.RotateFixItemClick.


procedure TFormMain.DeskewItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
    HoughImage     : TmcmImage;
begin
  // The Deskew method calculates the angle to rotate a skewed document ímage.
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       // Create an instance of TmcmImageTransform.
       ImageTransform := TmcmImageTransform.Create(Self);

       // Assign source image.
       ImageTransform.SourceImage[0] := FChild.Image;

       // Search for deskew angle within +14/-15 degree. Max. = +89/-90 or a range of 180 degree.
       ImageTransform.DeskewRange := 30;
       // Use 0.5 degree accuracy. Min. = 0.01 degree and Max = DeskewRange / 2.0.
       ImageTransform.DeskewStep := 0.25;
       // Max character intensity (Luminance) to 63 - expecting text to be near black.
       ImageTransform.DeskewMaxInt := 63;
       // To disable the Baird pre-process, re-instate the line below.
       // The preprocess reduces the number of pixels to Hough transform, and
       // therefore speeds up the deskew process considerably.
       //   ImageTransform.DeskewPreprocess := False;
       // To obtain/display the Hough transformed image re-instate the line below.
       //   ImageTransform.ReturnHoughImage := True;

       // Get start time
       QueryPerformanceCounter(FStartTime);
       HoughImage := ImageTransform.Deskew;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (ImageTransform.Error = EC_OK) // or (ImageTransform.Error = EC_DESKEWMAXMIN)
       then begin
            // If the angle is greater then |0.25| we'll rotate the image.
            if (abs(ImageTransform.Degree) >= 0.25)
            then begin
                 ImageTransform.BKColor := $FFFFFF;
                 if (ImageTransform.SourceImage[0].ImageFormat = IF_BW)
                 then ImageTransform.Interpolate := ST_NEAREST
                 else ImageTransform.Interpolate := ST_BILINEAR; // ST_BICUBIC;

                 // To rotate the image ImageTransform.Degree degrees use
                 ImageTransform.Affine;
                 // or
                 // ImageTransform.Rotate(False, ImageTransform.Degree);

                 // Get end time and display processing time.
                 QueryPerformanceCounter(FEndTime);
                 OnSetProcessTime(Self, FStartTime, FEndTime);

                 if (ImageTransform.Error = EC_OK)
                 then begin
                      FChild := CreateImageWindow('Deskew_', 0, 0, IF_NONE);
                      FChild.Image := ImageTransform.ResultImage;
                      FChild.Image.ImageInfo.FileName := FChild.Caption;
                 end
                 else ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);

                 // Deskew method succeeded!
                 if ImageTransform.ReturnHoughImage
                 then begin
                      // Show Hough image.
                      FChild := CreateImageWindow('Hough_', 0, 0, IF_NONE);
                      FChild.Image := HoughImage;
                      FChild.Image.ImageInfo.FileName := FChild.Caption;
                 end;
            end;

            // Display the calculated Deskew angle.
            ShowMessage('Deskew angle: '  + FloatToStrF(ImageTransform.Degree, ffFixed, 4, 2));
       end
       else ShowMessage('Error finding deskew angle: ' + CErrorStrings[word(ImageTransform.Error)]);
       ImageTransform.Free;
  end;
end; // TFormMain.DeskewItemClick.


procedure TFormMain.DeskewAndAutoCropClick(Sender : TObject);
var BkColor : TColor;
    Width   : integer;
    Height  : integer;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       Width   := FChild.ImageCtrl.Image.Width;
       Height  := FChild.ImageCtrl.Image.Height;

       BkColor := FChild.ImageCtrl.Image.Pixel[0,0];
  end;
end; // TFormMain.DeskewAndAutoCropClick.

//------------------------------------------------------------------------------
// Math menu
//------------------------------------------------------------------------------

procedure TFormMain.MathMenuClick(Sender : TObject);
var FormMath  : TFormMath;
    ImageMath : TmcmImageMath;
    i         : integer;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormMath := TFormMath.Create(Self);
       // Walk through all child windows and add images to form.
       for i := 0 to (MDIChildCount - 1)
       do begin
          if (MDIChildren[i] is TFormChild)
          then if (TFormChild(MDIChildren[i]).Image.ImageFormat in [IF_GREY8,IF_RGB24,IF_RGBA32])
               then FormMath.AddImage := TFormChild(MDIChildren[i]).Image;
       end;
       if (FormMath.ShowModal = mrOK)
       then begin
            ImageMath := TmcmImageMath.Create(Self);
            ImageMath.SourceImage[0] := FormMath.Source[0];
            ImageMath.SourceImage[1] := FormMath.Source[1];

            ImageMath.ResultImage := TmcmImage.Create;
            ImageMath.ResultImage.Width := ImageMath.SourceImage[0].Width;
            ImageMath.ResultImage.Height := ImageMath.SourceImage[0].Height;
            ImageMath.ResultImage.ImageFormat := ImageMath.SourceImage[0].ImageFormat;
            ImageMath.ResultImage.Palette := ImageMath.SourceImage[0].Palette;

            ImageMath.Scale[0] := FormMath.BlendFactor[0];
            ImageMath.Scale[1] := FormMath.BlendFactor[1];

            // Get start time
            QueryPerformanceCounter(FStartTime);

            ImageMath.Execute(FormMath.Method);

            // Get end time and display processing time.
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            FChild := CreateImageWindow('Math_', 0, 0, IF_NONE);
            FChild.Image := ImageMath.ResultImage;
            FChild.Image.ImageInfo.FileName := FChild.Caption;

            ImageMath.Free;
       end;
       FormMath.Free;
  end;
end; // TFormMain.MathMenuClick.


//------------------------------------------------------------------------------
// Morphing menu
//------------------------------------------------------------------------------

procedure TFormMain.MorphWizardItemClick(Sender : TObject);
begin
  // Get source/MDI child window.
  FChild := GetImageWindow;

  // Create Threshold dialogue.
  FormMorph := TFormMorph.Create(Self);

  // Set source image.
  FormMorph.SourceImage := FChild.Image;

  // Show dialogue to user.
  if (FormMorph.ShowModal = mrOK)
  then begin
       // Get processing time.
       FStartTime := FormMorph.TimeStart;
       FEndTime   := FormMorph.TimeEnd;
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Create a new MDI window
       FChild := CreateImageWindow('Morph_', 0, 0, IF_NONE);

       // Get result image from threshold dialogue and assign to MDI window.
       FChild.Image := FormMorph.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  FormMorph.Free;
end; // TFormMain.MorphWizardItemClick.


procedure TFormMain.CloseMorphItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.Coefficient[0] := 0;
       ImageMorph.Coefficient[1] := 0;
       ImageMorph.Close(1);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ImageMorph.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.CloseMorphItemClick.


procedure TFormMain.DilateMorphItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.Dilate(1);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ImageMorph.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.DilateMorphItemClick.


procedure TFormMain.ErodeMorphItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.Erode(1);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ImageMorph.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.ErodeMorphItemClick.


procedure TFormMain.OpenMorphItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.Coefficient[0] := 0;
       ImageMorph.Coefficient[1] := 0;
       ImageMorph.Open(1);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ImageMorph.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.OpenMorphItemClick.


procedure TFormMain.OutlineMorhpItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.Outline;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       ImageMorph.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.OutlineMorhpItemClick.


procedure TFormMain.EDMItemClick(Sender : TObject);
var ImageMorph : TmcmImageMorph;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageMorph := TmcmImageMorph.Create(Self);
       ImageMorph.SourceImage[0] := FChild.Image;
       ImageMorph.ResultImage := Nil;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageMorph.EuclideanDistanceMap;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);


       FChild := CreateImageWindow('EDM_', 0, 0, IF_NONE);
       FChild.Image := ImageMorph.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;

       ImageMorph.Free;
  end;
end; // TFormMain.EDMItemClick.


//------------------------------------------------------------------------------
// Window menu
//------------------------------------------------------------------------------

procedure TFormMain.WindowsMenuClick(Sender : TObject);
var HasChildren : boolean;
begin
  HasChildren := (MDIChildCount <> 0);
  WindowCascadeItem.Enabled := HasChildren;
  WindowTileItem.Enabled := HasChildren;
  CloseAllItem.Enabled := HasChildren;
end; // TFormMain.WindowsMenuClick.


procedure TFormMain.WindowCascadeItemClick(Sender : TObject);
begin
  Cascade;
end; // TFormMain.WindowCascadeItemClick.


procedure TFormMain.WindowTileItemClick(Sender : TObject);
begin
  TileMode := tbVertical;
  Tile;
end; // TFormMain.WindowTileItemClick.


procedure TFormMain.CloseAllItemClick(Sender : TObject);
var i : integer;
begin
  for i := (MDIChildCount - 1) downto 0
  do if (MDIChildren[i] is TFormChild)
     then MDIChildren[i].Close;
end; // TFormMain.CloseAllItemClick.

//------------------------------------------------------------------------------
// About menu
//------------------------------------------------------------------------------

procedure TFormMain.AboutItemClick(Sender : TObject);
begin
  mcmImgAboutBox := TmcmImgAboutBox.Create(Self);
  mcmImgAboutBox.ShowModal;
end; // TFormMain.AboutItemClick.


procedure TFormMain.HelpItemClick(Sender : TObject);
begin
  Application.HelpJump('Imaging_Toolkit_for_Delphi'); 
end; // TFormMain.HelpItemClick.


//------------------------------------------------------------------------------
// Toolbutton, Text
//------------------------------------------------------------------------------

procedure TFormMain.tbTextClick(Sender: TObject);
begin
  // What to do with text or other graphical shapes!?!
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FChild.ImageCtrl.Image.Canvas.Brush.Style := bsClear;
       FChild.ImageCtrl.Image.Canvas.Font.Size := 16;
       FChild.ImageCtrl.Image.Canvas.Font.Style := [fsBold];
       FChild.ImageCtrl.Image.Canvas.Font.Color := RGB(255, 255, 255);
       FChild.ImageCtrl.Image.Canvas.TextOut(10, 10, 'MCM DESIGN');
       FChild.Repaint;
  end;
end; // TFormMain.tbTextClick.


{$UNDEF DCB3}



end.
