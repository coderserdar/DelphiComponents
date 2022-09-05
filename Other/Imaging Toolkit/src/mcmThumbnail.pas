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
// $Log:  17591: mcmThumbnail.pas 
//
//    Rev 1.37    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.36    15-08-2009 13:45:14  mcm    Version: IMG 3.3
// Fixed a dead-lock.
//
//    Rev 1.35    31-10-2007 20:20:08  mcm    Version: IMG 3.2
// Added support for showing external file formats in the thumb-nail view.
//
//    Rev 1.34    20-08-2007 20:28:32  mcm
// Added support for Delphi 2007
//
//    Rev 1.33    05-11-2006 18:52:30  mcm    Version: IMG 3.1
// Added support for showing all images in a multi-paged file.
//
//    Rev 1.32    01-07-2006 11:37:54  mcm    Version: IMG 3.0
// Enabled displaying "fax" images in correct dimension by using the horizontal
// and vertical resolution to scale the vertical direction additionally.
//
//    Rev 1.31    28-05-2006 11:53:12  mcm
// Corrected mouse wheel scrolling.
//
//    Rev 1.30    18-03-2006 18:11:44  mcm    Version: IMG 2.16
// Corrected a potential problem using the Copy() method.
//
//    Rev 1.29    18-12-2005 15:42:10  mcm    Version: IMG 2.11
// Fix check on DateTime ref. TLoadImageFile.PeekMessage
//
//    Rev 1.28    22/11/2005 18:39:44  mcm    Version: IMG 2.10
// Thumbs are now always generated using half-tone disregarding the image's
// color resolution. 
//
//   Rev 1.27    27-09-2005 18:53:28  mcm    Version: IMG 2.9
// Fixed thread.resume problem when executed under Windows 98.

//
//   Rev 1.26    04/09/2005 10:22:54  mcm    Version: IMG 2.9
// Added EnableDirMonitor property, used to control if the directory monitor
// should notify about file changes in the currect directory.
// Renamed UnregisterAllFormat to UnregisterAllFormats.

//
//   Rev 1.25    23-08-2005 22:21:14  mcm    Version: IMG 2.9
// Modified TLoadImageFile to use new register file format methods, and ensure
// that only the file formats registered via the global instance
// ImageFileFormatMgr are used.

//
//   Rev 1.24    04-08-2005 18:52:24  mcm    Version: IMG 2.9
// Completed Drag & Drop supporting both the Delphi and Windows way.
// Added multi-file selection.
// Added methods to move, and copy files using the Windows Shell.
// Added OnBeforeLoadThumb event allowing programmatically to skip files and
// cancel browsing the directory.

//
//   Rev 1.23    31-07-2005 21:33:10  mcm
// More drag & drop.
// Added CopyFile and MoveFile methods, using Windows style.

//
//   Rev 1.22    30/07/2005 12:21:56  mcm
// Continued Drag & Drop implementation.

//
//   Rev 1.21    24-07-2005 18:55:08  mcm
// Initial Drag & Drop support.

//
//   Rev 1.20    23-05-2005 21:40:38  mcm    Version: IMG 2.9
// Corrected TLoadImageFile.Quit.

//
//   Rev 1.19    15-05-2005 19:45:02  mcm    Version: IMG 2.9
// Corrected error deleting to the recycle bin.
// Added Name property on TmcmThumbImage.
// Added directory monitor - reload files when changed load new file and remove
// deleted files.
// Added Thumbs property.
// Added DeleteThumb method.

//
//   Rev 1.18    14-03-2004 09:02:56  mcm    Version: IMG 2.4
// Minor correction in SetDir and Destroy, to avoid conflict with loader thread.

//
//   Rev 1.17    21-02-2004 01:10:16  mcm    Version: IMG 2.4
// Added Metadata (Exif & GPS) o hint window.

//
//   Rev 1.16    30-01-2004 20:39:22  mcm    Version: IMG 2.3
// Added three new properties on the TmcmThumbView to control the thumbnails
// color and border style. 
// The AddThumb method now enables adding in memory images to a thumb view. 

//
//   Rev 1.15    17-12-2003 14:14:52  mcm
// Corrected interpretation of color names. 

//
//   Rev 1.14    07-11-2003 18:02:50  mcm    Version: IMG 2.0
// Added SelectTextColor property and modified thumbs background color to
// reflect the background color of the TmcmThumbView.

//
//   Rev 1.13    30-09-2003 12:07:12  mcm    Version: IMG 1.6
// Fix a bug (endless loop) occuring when running on Windows 98 and compiling
// with Delphi 3, 4 & 5.

//
//   Rev 1.12    29-09-2003 18:42:40  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.11    13-08-2003 18:02:00  mcm
// Fixed variant assignment problem with FilerFrom and FilterTo.

//
//   Rev 1.10    06-07-2003 10:54:50  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.9    08-06-2003 12:32:12  mcm    Version: IMG 1.3.4
// Added run-time properties FilterFrom and FilterTo, used to filter image files
// based on Time-stamp, File name and Files size.

//
//   Rev 1.8    27-03-2003 16:17:10  mcm    Version: IMG 1.3.3
// Modified TLoadImageFile thread loop and SetDir method to avoid a memory leak
// when terminating an ongoing directory/image reading process.

//
//   Rev 1.7    10-03-2003 13:40:10  mcm    Version: IMG 1.3.2
// Added keyboard navigation using arrow, paqe, home and end keys, plus
// alphanumeric keys.
// Added mouse wheel support scrolling one line, and with Shift pressed one
// thumb.

//
//   Rev 1.6    05-02-03 16:33:46  mcm    Version: IMG 1.3
// Corrected ScrollMode to display thumbs as they should.
// Applied the ShowAtOnce facility.
// Added OnClick and OnDblClick events.

//
//   Rev 1.5    29-01-2003 15:48:08  mcm
// Added checks for valid image objects.

//
//   Rev 1.4    27-01-2003 14:05:30  mcm
// Fixed indexing error in SetSelected thumbnail.
// Modified SetDir to directory switching.
// Added UpdateThumbsInDir.

//
//   Rev 1.3    27-09-2002 13:11:04  mcm    Version: IMG 1.2
// Minor improvement on thread control.

//
//   Rev 1.2    07-08-2002 14:50:44  mcm

//
//   Rev 1.1    07-08-2002 13:54:18  mcm    Version: IMG 1.1
// Added HalfTone property to display "quality" thumbs.
// Added viewing compression method in thumbs hint.

//
//   Rev 1.0    27-05-2002 16:22:28  mcm

unit mcmThumbnail;

interface

//------------------------------------------------------------------------------
// Image index/library file
// Header:
//  ID       : string[8] = 'mcmThumbL'
//  Version  : integer, x.yz times 100, f.ex. 1.00 = 100.
//  NoThumbs : dword, number of thumbs
//  DirThumb : string
//
//  ThumbIndex : longword - Thumbnail position in 'ImgThumb.mct';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Image thumbnail file (ImgThumb.mct).
// Header:
//  ID       : string[8] = 'mcmThumbI'
//  Version  : integer, x.yz times 100, f.ex. 1.00 = 100.
//  NoThumbs : longword, number of thumbs
//------------------------------------------------------------------------------

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, Classes, Controls, Forms, Graphics,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes,
      Vcl.Graphics, Vcl.Forms, Vcl.Controls,
     {$ENDIF}
     mcmImage,
     mcmImageFile,
     mcmDragDrop,
     mcmImageTypeDef;

const
  WM_GETTHUMBS    = WM_USER + 1;

type

{$IFDEF DCB3}
  TWMMouseWheel = packed record
  Msg : Cardinal;
  Keys : SmallInt;
  WheelDelta : SmallInt;
  case integer of
  0: (XPos : Smallint;
      YPos : Smallint);
  1: (Pos : TSmallPoint;
      Result : Longint);
  end;
{$ENDIF}

  TmcmFileInfo = packed record
    DateTime : TDateTime;
    {$IFDEF DCB3}
    Size     : integer;
    {$ELSE}
    Size     : int64;
    {$ENDIF}
    Attr     : integer;
    ExcludeAttr : integer;
  end;
  PmcmFileInfo = ^TmcmFileInfo;

//------------------------------------------------------------------------------
// TmcmThumbImage
//------------------------------------------------------------------------------
  TmcmThumbImage = class(TmcmImageCtrl)
  private
    FFilename   : WideString;
    FThumbTextH : integer;
    FTextColor  : TColorRef;
    FLogFont    : TLogFont;
  protected
    procedure   WMRButtonDown(var Message : TMessage); message WM_RBUTTONDOWN;
    procedure   WMMouseWheel(var Message : TMessage); message WM_MOUSEWHEEL;
    function    GetThumbTextHeight : integer;
    procedure   Paint; override;
    procedure   SetThumbTextHeight(Value : integer);
  public
    constructor Create(AOwner: TComponent); override;
    property    TextColor  : TColorRef
      read      FTextColor
      write     FTextColor;
    property    ThumbTextHeight : integer
      read      GetThumbTextHeight
      write     SetThumbTextHeight;
    property    Name : WideString
      read      FFilename
      write     FFilename;
  published
  end;


//------------------------------------------------------------------------------
// TLoadImageFile ( Thread class)
//------------------------------------------------------------------------------

  TThreadOnImageReady = procedure(Sender : TObject; Image : TmcmImage; PageNumber : integer) of object;

  TLoadImageFile = class(TThread)
  private
    FFileMgr      : TmcmImageFileMgr;
    FReading      : boolean;
    FCancelRead   : boolean;
    FImage        : TmcmImage;
    FThumbIndex   : integer;
    FFileName     : WideString;
    FPageNumber   : integer;
    FOnImageReady : TThreadOnImageReady;
  protected
    procedure   Execute; override;
    procedure   PeekMessage;
    procedure   DoOnImageReady;
    function    GetReading : boolean;
    procedure   SetCancelRead(Value : boolean);
  public
    constructor Create(CreateSuspended : Boolean);
    destructor  Destroy; override;
    procedure   LoadFile(FileName : WideString; PageNumber : integer);
    procedure   Quit;
    property    CancelRead : boolean
      read      FCancelRead
      write     SetCancelRead;
    property    Reading : boolean
      read      GetReading;
    // Events.
    property    OnImageReady : TThreadOnImageReady
      read      FOnImageReady
      write     FOnImageReady;
  end;

  
  TOnLoadImage         = procedure(    Sender         : TObject;
                                       Filename       : string;
                                       PageNumber     : integer) of object;

//------------------------------------------------------------------------------
// TmcmDirectoryMonitor
//------------------------------------------------------------------------------

  TmcmDirectoryMonitor = class(TThread)
  private
  protected
    FPath        : String; // The path/directory that is monitored.
    FNotify      : THandle;    // Event object handle.
    FOnImageFile : TOnLoadImage;
    procedure   Execute; override;
    procedure   SetPath(Value : String);
    procedure   DoImageFile;
  public
    constructor Create(CreateSuspended : Boolean);
    destructor  Destroy; override;
    property    MonitorPath : String
      read      FPath
      write     SetPath;
    property    OnImageFile : TOnLoadImage
      read      FOnImageFile
      write     FOnImageFile;
  end;

//------------------------------------------------------------------------------
// TmcmDragFileList
//------------------------------------------------------------------------------

  TmcmDragFileList = class(TDragObject)
  private
    FFilenames : TStringList;
  protected
  public
    constructor Create;
    destructor  Destroy; override;
    property  Filenames : TStringList
      read    FFilenames;
  end;

//------------------------------------------------------------------------------
// TmcmThumbView
//------------------------------------------------------------------------------

  TOnThumbProgress     = procedure(    Sender         : TObject;
                                       Position       : word;
                                       Total          : word;
                                   var Break          : boolean) of object;
                                   
  TOnBeforeLoadThumb   = procedure(    Sender         : TObject;
                                       FileName       : string;
                                   var Skip           : boolean;              
                                   var Cancel         : boolean) of object;

  TThumbScrollMode = (SMT_HORIZ, SMT_VERT, SMT_BOTH);
  TThumbSortMethod = (TSM_None, TSM_FileName, TSM_FileExt, TSM_FileTimeStamp, TSM_FileSize);

  TmcmThumbView = class(TScrollBox)
  private
    FActive        : boolean;
    FAllowUpdate   : boolean;
    FAllowCancel   : boolean;
    FShowAllPages  : boolean;   // Display all pages in a multi pages image file.

    // Multi selection.
    FAllowMultiSel : boolean;   // If true enable multi-selection.
    FShiftStart    : integer;   // Last clicked thumb that marks the start of a
                                // Range (Shift-Key) selection.
    FShiftEnd      : integer;   //
    FSelectedThumb : TList;     // List of selected thumbs.
    FDragList      : TStringList; // List of selected thumbs used for dragging.

    FThreadLoader  : TLoadImageFile;
    FCancel        : boolean;   // Cancel reading the current directory.
    FThisLoaded    : boolean;
    FIsLoading     : boolean;
    FCount         : integer;
    FDir           : string;
    FAttr          : integer;
    FFilterFrom    : Variant;
    FFilterTo      : Variant;
    FHorizThumbs   : integer;   // Number of thumbs on a row.
    FVertThumbs    : integer;   // Number of thumbs on a column.
    FThumbList     : TList;     // The list of thumbs.

    // Thumb presentation.
    FThumbHeight   : integer;   // Thumb height.
    FThumbWidth    : integer;   // Thumb width.
    FThumbBorder   : integer;   // Thumbs border style
    FThumbStyle    : TmcmBorderStyle;
    FThumbFlat     : boolean;   // Is thumbs flat.
    FThumbTextH    : integer;   // Thumb text height.
    FThumbSpace    : integer;   // Space between thumbs.
    FThumbSpace2   : integer;
    FTotalHeight   : integer;   // Thumbs total height.
    FTotalWidth    : integer;   // Thumbs total width.
    FThumbColor    : TColor;    // Thumbs background color.
    FSelected      : integer;   // Last selected Thumb.
    FMWSelected    : integer;   // Mouse wheel selected, used to scroll up/down
                                // only.
    FSelColor      : TColorRef; // Selected thumb background color.
    FSelTextColor  : TColorRef; // Selected thumb text color.
    FFileList      : TStringList; // List of file in the current directory FDir.
    FImageToLoad   : word;      // Number of thumb images to load.
    FShowAtOnce    : boolean;
    FScrollMode    : TThumbScrollMode;
    FHalfToned     : boolean;   // Paint using halftomes. Best for BW images.

    // Drag & Drop.
    FAcceptDragObj : boolean;          // Do we accept the drag over object?
    FAllowFileMove : boolean;          // Allow files being moved when Dragging
                                       // and Dropping.
    FDragModeThumb : TDragMode;        // Thumbnails drag mode.
    FDragObject    : TmcmDragFileList; // Object dragged when using Delphi
                                       // Drag & Drop style.
    FExtDragDrop   : boolean;          // Enable external drag & drop operations
                                       // i.e. Windows Drag & Drop style.
    FSameDrive     : boolean;          // Is Source and Target driver (C:\ etc.)
                                       // the same.
    FDropSource    : TmcmDropSource;   // Source for external drag & drop.
    FDropTarget    : TmcmDropTarget;   // Target for external drag & drop.

    // Directory monitor.
    FEnableMonitor : boolean;
    FDirMonitor    : TmcmDirectoryMonitor; // Directory monitor. Updates thumbs
                                           // when new files are added or removed
                                           // from the current directory.

    // Sort
    FDescending    : boolean;
    FSortMethod    : TThumbSortMethod; // Selected sort method.

    FOnProgress    : TOnThumbProgress; // Progress event handle.
    FOnLoadImage   : TOnLoadImage;     // Event fired when an image should be
                                       // loaded by the owner.
    FOnBeforeLoadThumb : TOnBeforeLoadThumb;
  protected
    procedure   CreateWnd; override;
    procedure   CreateParams(var Params: TCreateParams); override;
    procedure   WndProc(var Message: TMessage); override;
    procedure   CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure   WMPaint(var Message : TWMPaint); message WM_PAINT;
    //procedure   WMHScroll(var Message : TMessage); message WM_HSCROLL;
    //procedure   WMVScroll(var Message : TMessage); message WM_VSCROLL;
    procedure   CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure   WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure   WMMouseWheel(var Message : TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure   CalcHorizThumbs;
    procedure   DoSelectThumb(Index : integer);
    procedure   DoDeselectThumb(Index : integer);
    procedure   DoDeselectThumbList(ExcludeIndex : integer);
    function    GetCancel : boolean;
    function    GetCount : integer;
    function    GetDir : string;
    procedure   GetFileNames;
    function    GetFilterFrom : Variant;
    function    GetFilterTo : Variant;
    function    GetScrollMode : TThumbScrollMode;
    function    GetSelected : integer;
    function    GetSelColor : TColor;
    function    GetSelTextColor : TColor;
    function    GetShowAllPages : boolean;
    function    GetShowAtOnce : boolean;
    function    GetThumb(Index : integer) : TmcmThumbImage;
    function    GetThumbBorder : integer;
    function    GetThumbBorderFlat : boolean;
    function    GetThumbBorderStyle : TmcmBorderStyle;
    function    GetThumbColor : TColor;
    procedure   GetNextThumbImage(var Message : TMessage); message WM_GETTHUMBS;
    procedure   OnExDragDrop(Sender, Source : TObject; Shift : TShiftState; X, Y : Integer);
    procedure   OnExDragOver(Sender, Source : TObject; Shift : TShiftState; X, Y : Integer; State : TDragState; var Accept : Boolean; var Copy : boolean; var Move : boolean);
    procedure   OnThumbStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure   OnThumbEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure   OnFileChange(Sender : TObject; AFileName : string; PageNumber : integer);
    procedure   OnPreStartDrag(Sender : TObject; X, Y: Integer; var Component : TPersistent);
    procedure   OnEndWinDrag(Sender : TObject; Target : TObject; X, Y: Integer);
    procedure   OnThreadImageReady(Sender : TObject; NewImage : TmcmImage;  PageNumber : integer);
    function    GetThumbHeight : integer;
    function    GetThumbSpace : integer;
    function    GetThumbWidth  : integer;
    procedure   OnImageClick(Sender : TObject);
    procedure   OnImageDblClick(Sender : TObject);
    procedure   RepositionThumbs;
    procedure   Resize; override;
    procedure   SetAllowFileMove(Value : boolean);
    procedure   SetAllThumbSizes;
    procedure   SetCancel(Value : boolean);
    procedure   SetDir(Value : string);
    procedure   SetExternalDragDrop(Value : boolean);
    procedure   SetFilterFrom(FromValue : Variant);
    procedure   SetFilterTo(ToValue : Variant);
    {$IFNDEF DCB3_4}
    procedure   SetDescending(Value : boolean);
    {$ENDIF}
    procedure   SetScrollMode(Value : TThumbScrollMode);
    procedure   SetSelected(Value : integer);
    procedure   SetSelColor(Value : TColor);
    procedure   SetSelTextColor(Value : TColor);
    procedure   SetShowAllPages(Value : boolean);
    procedure   SetShowAtOnce(Value : boolean);
    procedure   SetThumbBorder(Value : integer);
    procedure   SetThumbBorderFlat(Value : boolean);
    procedure   SetThumbBorderStyle(Value : TmcmBorderStyle);
    procedure   SetThumbColor(Value : TColor);
    procedure   SetThumbHeight(Value : integer);
    procedure   SetThumbPosition(Index : longint);
    procedure   SetThumbSizes(Index : longint);
    procedure   SetThumbSpace(Value : integer);
    procedure   SetThumbWidth(Value : integer);
    procedure   SetSortMethod(Value : TThumbSortMethod);
    function    SHELLFileCommand(Cmd : integer; const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
    function    UpdateThumb(NewImage : TmcmImage; Thumb : TmcmThumbImage; Index : integer) : integer;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    AddThumb(NewImage : TmcmImage) : integer;
    procedure   Clear;
    function    CopyFile(const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
    procedure   DeleteSelected;
    procedure   DeleteThumb(Index : integer);
    function    MoveFile(const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
    procedure   RecycleSelected(Confirm, Silent : boolean);
    procedure   RenameSelected(Filename : string);
    function    SelectedCount : integer;
    function    SelectedFileName : string;
    function    SelectedFileNames(Index : integer) : string;
    procedure   UpdateThumbsInDir;

    property    Cancel : boolean
      read      GetCancel
      write     SetCancel;
    property    FilterFrom : Variant
      read      GetFilterFrom
      write     SetFilterFrom;
    property    FilterTo : Variant
      read      GetFilterTo
      write     SetFilterTo;
    property    Selected : integer
      read      GetSelected
      write     SetSelected;
    property    Thumbs[Index : integer] : TmcmThumbImage
      read      GetThumb;
  published
    property    AllowCancel : boolean
      read      FAllowCancel
      write     FAllowCancel Default True;
    property    AllowDragFileMove : boolean
      read      FAllowFileMove
      write     SetAllowFileMove Default True;
    property    AllowMultiSelection : boolean
      read      FAllowMultiSel
      write     FAllowMultiSel default False;
    property    AllowUpdate : boolean
      read      FAllowUpdate
      write     FAllowUpdate Default True;
    property    AutoScroll Default True;
    property    Color default $00FFFFFF;
    property    Count : integer
      read      GetCount;
    property    Dir : string
      read      GetDir
      write     SetDir;
    property    DragModeThumb : TDragMode
      read      FDragModeThumb
      write     FDragModeThumb default dmManual;
    property    EnableDirMonitor : boolean
      read      FEnableMonitor
      write     FEnableMonitor default False;
    property    ExternalDragDrop : boolean
      read      FExtDragDrop
      write     SetExternalDragDrop default False;
    property    HalfToned : boolean
      read      FHalfToned
      write     FHalfToned default True;
    property    ScrollMode : TThumbScrollMode
      read      GetScrollMode
      write     SetScrollMode default SMT_VERT;
    property    SelectColor : TColor
      read      GetSelColor
      write     SetSelColor Default $0800000;
    property    SelectTextColor : TColor
      read      GetSelTextColor
      write     SetSelTextColor Default $0FFFFFF;
    property    ShowAllPages : boolean
      read      GetShowAllPages
      write     SetShowAllPages default False;
    property    ShowAtOnce : boolean
      read      GetShowAtOnce
      write     SetShowAtOnce default True;
    {$IFNDEF DCB3_4}
    property    SortDescending : boolean
      read      FDescending
      write     SetDescending default True;
    {$ENDIF}
    property    SortMethod  : TThumbSortMethod
      read      FSortMethod
      write     SetSortMethod default TSM_None;
    property    ThumbBorder : integer
      read      GetThumbBorder
      write     SetThumbBorder default 14;
    property    ThumbBorderFlat : boolean
      read      GetThumbBorderFlat
      write     SetThumbBorderFlat default False;
    property    ThumbBorderStyle : TmcmBorderStyle
      read      GetThumbBorderStyle
      write     SetThumbBorderStyle default BS_NONE;
    property    ThumbColor : TColor
      read      GetThumbColor
      write     SetThumbColor default clWhite;
    property    ThumbHeight : integer
      read      GetThumbHeight
      write     SetThumbHeight default 50;
    property    ThumbSpace : integer
      read      GetThumbSpace
      write     SetThumbSpace default 4;
    property    TabOrder;
    property    TabStop;// default True;
    property    ThumbWidth : integer
      read      GetThumbWidth
      write     SetThumbWidth  default 50;

    // Events.
    property    OnBeforeLoadThumb : TOnBeforeLoadThumb
      read      FOnBeforeLoadThumb
      write     FOnBeforeLoadThumb;
    property    OnLoadImage : TOnLoadImage
      read      FOnLoadImage
      write     FOnLoadImage;
    property    OnProgress : TOnThumbProgress
      read      FOnProgress
      write     FOnProgress;
  end;


{$IFNDEF DCB3_4}
  // Methods used for sorting thumbnails.
  function    SortByName(List : TStringList; Index1, Index2 : integer) : integer;
  function    SortByExt(List : TStringList; Index1, Index2 : integer) : integer;
  function    SortByDateTime(List : TStringList; Index1, Index2 : integer) : integer;
  function    SortByFileSize(List : TStringList; Index1, Index2 : integer) : integer;
{$ENDIF}

implementation

uses {$IFNDEF GE_DXE2}
      SysUtils, Dialogs,
      {$IFDEF DCB3_5} FileCtrl, {$ENDIF}
      {$IFNDEF DCB3_5} Variants, {$ENDIF}
      ShellAPI,
     {$ELSE}
      System.Types, System.SysUtils, Vcl.Dialogs, Vcl.FileCtrl, System.Variants, WinApi.ShellAPI, 
     {$ENDIF}
     mcmImageResStr;

const WM_THREADQUIT = WM_USER + 1;
      WM_LOADFILE   = WM_USER + 2;

var gvDescending : boolean;


constructor TLoadImageFile.Create(CreateSuspended : Boolean);
var i : integer;
begin
  FFileMgr := TmcmImageFileMgr.Create;
  FOnImageReady := Nil;
  FReading    := False;
  FCancelRead := False;
  FPageNumber  := 0;

  // Enssure that we use the file formats registered with the global instance
  // ImageFileManager.
  FFileMgr.UnregisterAllFileFormats;
  for i := 0 to (ImageFileManager.GetFormatList.Count - 1)
  do begin
     with TmcmFileFormatItem(ImageFileManager.GetFormatList.Items[i])
     do FFileMgr.RegisterFileFormat(mcmFileClass, FileClassId, Extension, Description, ReadEnabled, WriteEnabled);
  end;

  Inherited Create(CreateSuspended);
end; // TLoadImageFile.Create.


destructor TLoadImageFile.Destroy;
begin
  FFileMgr.Free;
  FFileMgr := Nil;
  Inherited Destroy;
end; // TLoadImageFile.Destroy.


procedure TLoadImageFile.LoadFile(FileName : WideString; PageNumber : integer);
var i : integer;
begin
  FFileName := FileName;
  FThumbIndex := -1;
  i := 0;
  while (PostThreadMessage(ThreadID, WM_LOADFILE, 0, PageNumber) = False) and (i < 100)
  do begin
     inc(i);
     Sleep(0);
  end;
  if Suspended
  then Resume;
  FReading := True;
end; // TLoadImageFile.LoadFile.


procedure TLoadImageFile.Quit;
var i : integer;
begin
  if Suspended
  then Resume;
  i := 0;
  while (PostThreadMessage(ThreadID, WM_QUIT, 0, 0) = False) and (i < 100)
  do begin
     inc(i);
     Sleep(0);
  end;
end; // TLoadImageFile.Quit.


procedure TLoadImageFile.DoOnImageReady;
begin
  if Assigned(FOnImageReady)
  then FOnImageReady(Self, FImage, FPageNumber);
end; // TLoadImageFile.DoOnImageReady.


function TLoadImageFile.GetReading : boolean;
begin
  Result := FReading;
end; // TLoadImageFile.GetReading.


procedure TLoadImageFile.SetCancelRead(Value : boolean);
begin
  FCancelRead := Value;
end; // TLoadImageFile.SetCancelRead.


procedure TLoadImageFile.Execute;
var Msg : TMsg;
begin
  FImage := Nil;
  while Not(Terminated)
  do begin
     try
       {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.WaitMessage;
       if {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, WM_QUIT, WM_QUIT, PM_REMOVE)
       then Terminate
       else PeekMessage;
     except
       On E:Exception
       do ;
     end;
  end;
  if Assigned(FImage)
  then FImage.Free;
  FImage := Nil;
end; // TLoadImageFile.Execute.


procedure TLoadImageFile.PeekMessage;
var Msg        : TMsg;
    Retries    : integer;
    FileHandle : integer;
    FileDate   : integer;
begin
  Retries := 0;
  while Not(Terminated) and {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
  do begin
     if (Msg.Message = WM_QUIT)
     then begin
          Terminate;
          Break;
     end;
     try
       case Msg.Message of
       WM_LOADFILE   : begin
                         // Load image from file.
                         Sleep(1);
                         if (FFileName <> '') and FileExists(FFileName)
                         then begin
                              FReading := True;
                              FImage := TmcmImage.Create;
                              try
                                try
                                  FPageNumber := Msg.lParam;
                                  if (FPageNumber = 0)
                                  then begin
                                       FImage.DibHandle := FFileMgr.LoadImage(FFileName, FF_DETECT);
                                       if (FImage.DibHandle <> 0) and FFileMgr.IsMultiImage
                                       then FPageNumber := 1
                                       else FPageNumber := 0;
                                  end
                                  else begin
                                       FImage.DibHandle := FFileMgr.LoadNext(FFileName, FF_DETECT, FPageNumber);
                                       if (FImage.DibHandle <> 0)
                                       then inc(FPageNumber)
                                       else FPageNumber := 0;
                                  end;

                                  if (FImage.DibHandle <> 0) and Not(Terminated)
                                  then begin
                                       Retries := 0;
                                       FImage.ImageInfo.Assign(FFileMgr.ImageInfo);
                                       if Not(FFileMgr.IsMultiImage)
                                       then FImage.ImageInfo.PageNumber := 0;
                                       FImage.Compression := FFileMgr.Compression;
                                       FImage.Quality  := FFileMgr.Quality;
                                       FImage.Interlaced := FFileMgr.Interlaced;

                                       FFileMgr.ImageInfo.Units := UN_METERS;
                                       FFileMgr.ImageInfo.PageNumber := FPageNumber;
                                       FImage.XResolution := Round(FFileMgr.ImageInfo.xResolution);
                                       FImage.YResolution := Round(FFileMgr.ImageInfo.yResolution);

                                       // Set files true date/time stamp
                                       FileHandle := FileOpen(FFileName, fmOpenRead or fmShareDenyNone);
                                       if (FileHandle <> -1)
                                       then begin
                                            FileDate := FileGetDate(FileHandle);
                                            FileClose(FileHandle);
                                            FImage.ImageInfo.DateTime := FileDateToDateTime(FileDate);
                                       end;
                                  end
                                  else begin
                                       if Assigned(FImage)
                                       then FImage.Free;
                                       FImage := Nil;
                                       
                                       // Special case. If a file is used by another application
                                       // we'll try to load this image 5 time at 100 ms interval.
                                       // This is of interest when a new file is added to the
                                       // current directory and we want to load this as soon as it's
                                       // available in full length.
                                       if (FFileMgr.Error = EC_OPENFILE) and (Retries < 5)
                                       then begin
                                            Sleep(100);
                                            inc(Retries);
                                            LoadFile(FFilename, FPageNumber);
                                       end
                                       else Retries := 0;
                                  end;
                                except
                                  // Don't care about exception, just catch them!
                                  FImage.Clear;
                                end;
                              finally
                                // Notify of and return loaded image to thread owner.
                                if Not(Terminated) and Not(FCancelRead)
                                then Synchronize(DoOnImageReady);
                                if Assigned(FImage)
                                then FImage.Free;
                                FImage := Nil;
                                FReading := False;
                              end;
                         end;
                       end;
       end; // case.
     except
       On E:Exception
       do ;
     end;
  end;
end; // TLoadImageFile.PeekMessage.

//------------------------------------------------------------------------------
// TmcmThumbImage
//------------------------------------------------------------------------------

constructor TmcmThumbImage.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];

  FillChar(FLogFont, SizeOf(TLogFont), 0);
  with TScrollBox(AOwner)
  do begin
     FLogFont.lfHeight         := Font.Height;
     FLogFont.lfWidth          := 0;
     FLogFont.lfEscapement     := 0;
     FLogFont.lfOrientation    := 0;
     FLogFont.lfWeight         := FW_DONTCARE;
     FLogFont.lfItalic         := 0;
     FLogFont.lfUnderline      := 0;
     FLogFont.lfStrikeOut      := 0;
     FLogFont.lfCharSet        := Font.CharSet;
     FLogFont.lfOutPrecision   := OUT_TT_PRECIS;
     FLogFont.lfClipPrecision  := CLIP_DEFAULT_PRECIS;
     FLogFont.lfQuality        := PROOF_QUALITY;
     FLogFont.lfPitchAndFamily := byte(Font.Pitch);
     StrCopy(FLogFont.lfFaceName, PChar(Font.Name));
  end;
end; // TmcmThumbImage.Create.


procedure TmcmThumbImage.WMRButtonDown(var Message : TMessage);
begin
  Inherited;
  if Not(((Message.WParam and MK_CONTROL) = MK_CONTROL) or
         ((Message.WParam and MK_SHIFT) = MK_SHIFT))
  then OnClick(self);
end; // TmcmThumbImage.WMLButtonDown.


procedure TmcmThumbImage.WMMouseWheel(var Message : TMessage);
begin
  if (Parent <> Nil)
  then Parent.WindowProc(Message);
end; // TmcmThumbImage.WMMouseWheel.


function TmcmThumbImage.GetThumbTextHeight : integer;
begin
  Result := FThumbTextH;
end; // TmcmThumbImage.GetThumbTextHeight.


procedure TmcmThumbImage.SetThumbTextHeight(Value : integer);
begin
  if (FThumbTextH <> Value)
  then FThumbTextH := Value;
end; // TmcmThumbImage.SetThumbTextHeight.


procedure TmcmThumbImage.Paint;
var x, y    : integer;
    oa      : integer;
    oc, obc : TColorRef;
    Name    : string;
    TmpStr  : string;
    NameLen : integer;
    NewFont : HFont;
    OldFont : HFont;
    ExtSize : TSize;
begin
  if Visible
  then begin
       if Assigned(FImage) and (FDC <> 0)
       then begin
            x := Round((Width - FScale * FImage.Width) / 2);
            y := Round((Height - FThumbTextH - FScale * FImage.Height) / 2);
            if (BorderStyle <> BS_NONE)
            then dec(x,2);
            FImage.SetOrigo(Point(x, y));
            Inherited Paint;

            // Write filename.
            Name := ' ' + ExtractFileName(FFileName) + ' ';

            x := Width div 2;
            y := Height - 6;
            oa := SetTextAlign(FDC, TA_CENTER or TA_BOTTOM);
            oc  := SetTextColor(FDC, FTextColor);
            obc := SetBKColor(FDC, Color); //$0FFFFFF);
            SetBKMode(FDC, OPAQUE);
            NewFont  := CreateFontIndirect(FLogFont);
            OldFont := SelectObject(FDC, NewFont);
            NameLen := Length(Name);
            GetTextExtentPoint32(FDC, PChar(Name), NameLen, ExtSize);
            if (abs(ExtSize.cx) > (Width - 8))
            then begin
                 while (abs(ExtSize.cx) > (Width - 8)) and (NameLen > 3)
                 do begin
                    dec(NameLen);
                    TmpStr := Copy(Name, 1, NameLen);
                    Name := TmpStr;
                    GetTextExtentPoint32(FDC, PChar(Name), NameLen, ExtSize);
                 end;

                 dec(NameLen, 3);
                 TmpStr := Copy(Name, 1, NameLen);
                 Name := TmpStr + '.. ';
            end;

            {$IFDEF GE_DXE2}WinApi.{$ENDIF}windows.TextOut(FDC, x, y, PChar(Name), Length(Name));
            SelectObject(FDC, OldFont);
            DeleteObject(NewFont);
            SetBKColor(FDC, obc);
            SetTextColor(FDC, oc);
            SetTextAlign(FDC, oa);
       end
       else Inherited Paint;
  end;
end; // TmcmThumbImage.Paint.


//------------------------------------------------------------------------------
// TmcmDragFileList.
//------------------------------------------------------------------------------

constructor TmcmDragFileList.Create;
begin
  Inherited Create;
  FFilenames := TStringList.Create;
end; // TmcmDragFileList.Create.


destructor TmcmDragFileList.Destroy;
begin
  FFilenames.Free;
  Inherited Destroy;
end; // TmcmDragFileList.Destroy.


//------------------------------------------------------------------------------
// TmcmThumbView
//------------------------------------------------------------------------------

constructor TmcmThumbView.Create(AOwner : TComponent);
begin
  FThisLoaded := False;
  Inherited Create(AOwner);

  ControlState := ControlState + [csFocusing];
  ControlStyle := [csSetCaption, csDoubleClicks, csOpaque, csClickEvents, csReflector, csCaptureMouse];

  // Threaded image loader.
  FShowAllPages := False;
  FThreadLoader := TLoadImageFile.Create(False);
  FThreadLoader.OnImageReady := OnThreadImageReady;
  FThreadLoader.LoadFile('', 0);
  Sleep(0);

  FAllowUpdate := True;
  FAllowCancel := True;
  Color        := RGB(255, 255, 255);
  FFileList    := TStringList.Create;
  FThumbList   := TList.Create;
  FAttr        := {$IFDEF GE_DXE2}System.{$ENDIF}SysUtils.faArchive;
  FCancel      := False;
  FIsLoading   := False;
  FSelected    := -1;
  FSelColor    := $0800000;
  FScrollMode  := SMT_VERT;
  FShowAtOnce  := True;
  FThumbColor  := clWhite;
  FThumbStyle  := BS_NONE;
  FThumbBorder := 14;
  FThumbFlat   := False;
  FThumbSpace  := 20;
  FThumbSpace2 := 10;
  FThumbHeight := 50;
  FThumbWidth  := 50;
  FThumbTextH  := Abs(Font.Height) + 7;
  FTotalHeight := FThumbHeight + FThumbBorder + FThumbTextH + FThumbSpace2;
  FTotalWidth  := FThumbWidth + FThumbBorder + FThumbSpace2;
  FSelTextColor := clWhite;
  {$IFNDEF DCB3}
  VertScrollBar.Smooth := True;
  HorzScrollBar.Smooth := True;
  {$ENDIF}
  VertScrollBar.Increment := FThumbHeight;
  HorzScrollBar.Increment := FThumbWidth;
  FHalfToned   := True;
  FThisLoaded  := True;

  FSortMethod  := TSM_None;
  VarClear(FFilterFrom);
  VarClear(FFilterTo);

  FDescending  := True;
  gvDescending := True;

  TabStop      := True;

  FEnableMonitor := False;
  FDirMonitor    := Nil;

  // Multi selection
  FShiftStart    := -1;
  FSelectedThumb := TList.Create;

  // Drag & Drop
  FAcceptDragObj := False;
  FAllowFileMove := True;

  // Delphi Drag & Drop style
  FDragModeThumb := dmManual;
  FDragObject    := Nil;

  // Windows Drag & Drop style
  FExtDragDrop := False;
  FDragList    := TStringList.Create;
  FDropTarget  := Nil;
  FDropSource  := TmcmDropSource.Create(Self);
  FDropSource.OnPreStartDrag := OnPreStartDrag;
  FDropSource.OnEndDrag := OnEndWinDrag;
  FDropSource.DragMode := dmManual;
  FDropSource.EnableMove := FAllowFileMove;
end; // TmcmThumbView.Create.


destructor TmcmThumbView.Destroy;
begin
  if Assigned(FDirMonitor)
  then begin
       FDirMonitor.FreeOnTerminate := True;
       if FDirMonitor.Suspended
       then FDirMonitor.Resume;
       FDirMonitor.Terminate;
       FDirMonitor := Nil;
  end;
  if Assigned(FDropTarget)
  then FDropTarget.Free;
  if Assigned(FDropSource)
  then FDropSource.Free;
  if Assigned(FDragList)
  then FDragList.Free;
  if Assigned(FThreadLoader)
  then begin
       FThreadLoader.CancelRead := True;
       FThreadLoader.Quit;// Terminate;
  end;
  if FThreadLoader.Suspended
  then FThreadLoader.Resume;
  FCancel := True;
  Clear;
  if Assigned(FSelectedThumb)
  then FSelectedThumb.Free;
  FFileList.Destroy;
  FThumbList.Free;
  Inherited Destroy;
end; // TmcmThumbView.Destroy.


procedure TmcmThumbView.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'mcmThumbView');
end; // TmcmThumbView.CreateParams.


procedure TmcmThumbView.CreateWnd;
var W, H : integer;
begin
  W := Width;
  H := Height;
  inherited CreateWnd;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  
  FDropTarget := TmcmDropTarget.Create(Self);
  FDropTarget.OnExDragDrop := OnExDragDrop;
  FDropTarget.OnExDragOver := OnExDragOver;
  FDropTarget.Active       := FExtDragDrop;
end; // TmcmThumbView.CreateWnd.


procedure TmcmThumbView.CMFocusChanged(var Message : TCMFocusChanged);
begin
  with Message
  do if Sender is TmcmThumbView
     then FActive := (Sender = Self)
     else FActive := False;
  inherited;
end; // TmcmThumbView.CMFocusChanged.


procedure TmcmThumbView.Clear;
var i : integer;
begin
  Visible := False;
  FSelected := -1;

  for i := 0 to (FFileList.Count - 1)
  do if Assigned(FFileList.Objects[i])
     then Dispose(PmcmFileInfo(FFileList.Objects[i]));

  FFileList.Clear;
  FSelectedThumb.Clear;
  for i := 0 to (FThumbList.Count - 1)
  do if Assigned(FThumbList.Items[i])
     then TmcmThumbImage(FThumbList.Items[i]).Free;
  FThumbList.Clear;
  Visible := True;
end; // TmcmThumbView.Clear.


procedure TmcmThumbView.WMPaint(var Message : TWMPaint);
begin
  inherited ;
  // Must call inherited,
  // otherwise TScrollBox uses 99% processing power.
  Message.Result := 0;
end; // TmcmThumbView.WMPaint.


procedure TmcmThumbView.CalcHorizThumbs;
begin
  case FScrollMode of
  SMT_HORIZ : begin
                FVertThumbs := (ClientHeight - 0) div FTotalHeight;
                if (FVertThumbs > 0)
                then FHorizThumbs := (FCount + ((FVertThumbs + 1) div 2)) div FVertThumbs
                else FHorizThumbs := FCount;
              end;
  SMT_VERT  : begin
                FVertThumbs := (ClientHeight - 0) div FTotalHeight;
                FHorizThumbs := (ClientWidth - 0) div
                                (FThumbSpace2 + FThumbWidth + FThumbBorder);
              end;
  SMT_BOTH  : begin
                FVertThumbs := (ClientHeight - 0) div FTotalHeight;
                FHorizThumbs := Trunc(Sqrt(FCount));
              end;
  end;
  if (FHorizThumbs < 1)
  then FHorizThumbs := 1;
end; // TmcmThumbView.CalcHorizThumbs.


function TmcmThumbView.GetScrollMode : TThumbScrollMode;
begin
  Result := FScrollMode;
end; // TmcmThumbView.GetScrollMode.


procedure TmcmThumbView.SetScrollMode(Value : TThumbScrollMode);
begin
  if (FScrollMode <> Value)
  then begin
       FScrollMode := Value;
       RepositionThumbs;
       if (FSelected >= 0)
       then if (FThumbList.Items[FSelected] <> Nil)
            then ScrollInView(FThumbList.Items[FSelected]);
  end;
end; // TmcmThumbView.SetScrollMode.


procedure TmcmThumbView.CNKeyDown(var Message: TWMKeyDown);
var Mask : integer;
begin
  Message.Result := 1;
  if Not(csDesigning in ComponentState)
  then begin
       if (Perform(CM_CHILDKEY, Message.CharCode, Integer(Self)) <> 0)
       then Exit;
       Mask := 0;
       case Message.CharCode of
       VK_TAB : Mask := DLGC_WANTTAB;
       VK_LEFT,
       VK_RIGHT,
       VK_UP,
       VK_DOWN : begin
                   //Mask := DLGC_WANTARROWS;
                   Message.Result := 0;
                   Exit;
                 end;
       VK_RETURN,
       VK_EXECUTE,
       VK_ESCAPE,
       VK_CANCEL : Mask := DLGC_WANTALLKEYS;
       end;
       if (Mask <> 0) and
          (Perform(CM_WANTSPECIALKEY, Message.CharCode, 0) = 0) and
          (Perform(WM_GETDLGCODE, 0, 0) and Mask = 0) and
          (GetParentForm(Self).Perform(CM_DIALOGKEY, Message.CharCode, Message.KeyData) <> 0)
       then Exit;
  end;
  Message.Result := 0;
end; // TmcmThumbView.CNKeyDown.


procedure TmcmThumbView.WndProc(var Message: TMessage);
var Form      : TCustomForm;
    Receiver  : TControl;
    ShiftDown : boolean;
    CtrlDown  : boolean;
begin
  case Message.Msg of
  WM_LBUTTONDOWN,
  WM_LBUTTONDBLCLK : if Not(csDesigning in ComponentState) and
                        Not(Focused)
                     then begin
                          {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.SetFocus(Handle);
                          if Not(Focused)
                          then Exit;
                     end
                     else begin
                          Receiver := ControlAtPos(Point(LOWORD(Message.lParam), HIWORD(Message.lParam)), True);
                          if (Receiver = Nil)
                          then begin
                               ShiftDown := (GetAsyncKeyState(VK_SHIFT) and $8000 <> 0);
                               CtrlDown  := (GetAsyncKeyState(VK_CONTROL) and $8000 <> 0);
                               if Not(ShiftDown or CtrlDown)
                               then begin
                                    // Deselect all items in list
                                    DoDeselectThumbList(-1);
                               end;
                          end;

                     end;
  WM_CHAR          : begin // Without this "Esc" and "F5" blocks other keys !?!
                           // Borland what have you done?
                       Form := GetParentForm(Self);
                       if (Form <> Nil) and Form.WantChildKey(Self, Message)
                       then Exit;
                     end;


  end;
  inherited WndProc(Message);
end; // TmcmThumbView.WndProc.


procedure TmcmThumbView.WMKeyDown(var Message : TWMKeyDown);
var NewSelected  : integer;
    UpdateScroll : boolean;
    Key          : char;
    FName        : string;
    FirstSearch  : integer;
    i, PageNumber : integer;
begin
  UpdateScroll := False;
  case Message.CharCode of
  VK_F5     : begin
                if FAllowUpdate
                then begin
                     Message.CharCode := 0;
                     UpdateThumbsInDir;
                     Message.Result := 0;
                end;
              end;
  VK_ESCAPE : begin
                if FAllowCancel
                then begin
                     Cancel := True;
                     Message.Result := 0;
                end;
              end;
  VK_SPACE  : begin
                if (FSelected > -1)
                then begin
                     i := FSelectedThumb.IndexOf(FThumbList.Items[FSelected]);
                     if (i > -1)
                     then
                     else ;
                end;
              end;
  VK_RETURN : begin
                if Assigned(FOnLoadImage) and (FSelected > -1)
                then if FAllowMultiSel
                     then begin
                          i := 0;
                          while (i < FSelectedThumb.Count)
                          do begin
                             if FShowAllPages
                             then PageNumber := TmcmThumbImage(FSelectedThumb.Items[i]).Image.ImageInfo.PageNumber
                             else PageNumber := -1;
                             FOnLoadImage(Self, TmcmThumbImage(FSelectedThumb.Items[i]).FFilename, PageNumber);
                             inc(i);
                          end;
                     end
                     else begin
                          if FShowAllPages
                          then PageNumber := TmcmThumbImage(FThumbList.Items[FSelected]).Image.ImageInfo.PageNumber
                          else PageNumber := -1;
                          FOnLoadImage(Self, TmcmThumbImage(FThumbList.Items[FSelected]).FFilename, PageNumber);
                     end;
                Message.Result := 0;
              end;
  VK_PRIOR  : begin
                NewSelected := FSelected - FVertThumbs * FHorizThumbs;
                if (NewSelected < 0)
                then NewSelected := 0;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_NEXT   : begin
                NewSelected := FSelected + FVertThumbs * FHorizThumbs;
                if (NewSelected >= FThumbList.Count)
                then NewSelected := FThumbList.Count - 1;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_END    : begin // End key
                Selected := FThumbList.Count - 1;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_HOME   : begin // Home key
                Selected := 0;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_LEFT   : begin // Left Arrow key
                NewSelected := FSelected - 1;
                if (NewSelected < 0)
                then NewSelected := 0;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_UP	    : begin
                NewSelected := FSelected - FHorizThumbs;
                if (NewSelected < 0)
                then NewSelected := 0;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_RIGHT  : begin // Right Arrow key
                NewSelected := FSelected + 1;
                if (NewSelected >= FThumbList.Count)
                then NewSelected := FThumbList.Count - 1;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  VK_DOWN   : begin
                NewSelected := FSelected + FHorizThumbs;
                if (NewSelected >= FThumbList.Count)
                then NewSelected := FThumbList.Count - 1;
                Selected := NewSelected;
                UpdateScroll := True;
                Message.Result := 0;
              end;
  else begin
       Key := Chr(Message.CharCode);
       if IsCharAlphaNumeric(Key)
       then begin
            NewSelected := Selected;
            if (NewSelected < 0)
            then NewSelected := -1;
            if (NewSelected >= FThumbList.Count - 1)
            then NewSelected := -1;

            try
              FirstSearch := 2;
              while (FirstSearch <> 0)
              do begin
                 if (FThumbList.Count > 0)
                 then begin
                      FName := ' ';
                      while (FName[1] <> Key) and
                            (NewSelected < FThumbList.Count - 1)
                      do begin
                         inc(NewSelected);
                         FName := ExtractFileName(TmcmThumbImage(FThumbList.Items[NewSelected]).FFilename);
                         FName := Uppercase(FName);
                      end;
                 end;
                 dec(FirstSearch);
                 if (FName[1] = Key)
                 then begin
                      Selected := NewSelected;
                      UpdateScroll := True;
                      FirstSearch := 0;
                 end
                 else NewSelected := -1;
              end;
            except
            end;
            Message.Result := 0;
       end
       else Inherited ; //Inherited WMKeyDown(Message);
  end;
  end;

  if UpdateScroll and (FSelected >= 0)
  then if (FThumbList.Items[FSelected] <> Nil)
       then ScrollInView(FThumbList.Items[FSelected]);
end; // TmcmThumbView.WMKeyDown.


procedure TmcmThumbView.WMMouseWheel(var Message : TWMMouseWheel);
var NewSelected  : integer;
    IncValue     : integer;
    DC           : HDC;
    IsInView     : boolean;
    APoint       : TPoint;
begin
  if ((Message.Keys and MK_SHIFT) = MK_SHIFT)
  then IncValue := 1
  else IncValue := FHorizThumbs;
  if (Message.WheelDelta > 0)
  then begin
       NewSelected := FMWSelected - IncValue;
       // Increment thumb index until we reach a non-visible thumb.
       IsInView := True;
       while IsInView
       do begin
          if (NewSelected <= 0) or (NewSelected >= FThumbList.Count)
          then Break;
          DC := GetDC(Handle);
          APoint.X := TmcmThumbImage(FThumbList.Items[NewSelected]).Left;
          APoint.Y := TmcmThumbImage(FThumbList.Items[NewSelected]).Top +
                      TmcmThumbImage(FThumbList.Items[NewSelected]).Height;
          IsInView := PtVisible(DC, APoint.X, APoint.y);
          if IsInView
          then NewSelected := NewSelected - IncValue;
          ReleaseDC(Handle, DC);
       end;

       if (NewSelected < 0)
       then NewSelected := 0;
       FMWSelected := NewSelected;
       if Not(FAllowMultiSel)
       then Selected := NewSelected;
  end
  else begin
       NewSelected := FMWSelected + IncValue;
       // Increment thumb index until we reach a non-visible thumb.
       IsInView := True;
       while IsInView
       do begin
          if (NewSelected <= 0) or (NewSelected >= FThumbList.Count)
          then Break;
          DC := GetDC(Handle);
          APoint.X := TmcmThumbImage(FThumbList.Items[NewSelected]).Left;
          APoint.Y := TmcmThumbImage(FThumbList.Items[NewSelected]).Top +
                      TmcmThumbImage(FThumbList.Items[NewSelected]).Height;
          IsInView := PtVisible(DC, APoint.X, APoint.y);
          if IsInView
          then NewSelected := NewSelected + IncValue;
          ReleaseDC(Handle, DC);
       end;

       if (NewSelected >= FThumbList.Count)
       then NewSelected := FThumbList.Count - 1;
       FMWSelected := NewSelected;
       if Not(FAllowMultiSel)
       then Selected := NewSelected;
  end;
  if (0 <= FMWSelected) and (FMWSelected < FThumbList.Count)
  then if (FThumbList.Items[FMWSelected] <> Nil)
       then ScrollInView(FThumbList.Items[FMWSelected]);
  Message.Result := 0;
end; // TmcmThumbView.WMMouseWheel.


procedure TmcmThumbView.Resize;
begin
  inherited;
  if Visible
  then RepositionThumbs;
end; // TmcmThumbView.Resize.


procedure TmcmThumbView.GetFileNames;
var SearchRec    : TSearchRec;
    SearchResult : integer;
    FileFormat   : TmcmFileFormat;
    pFileInfo    : PmcmFileInfo;
begin
  Clear;
  if (FDir <> '')
  then begin
       if (FDir[Length(FDir)] <> '\')
       then FDir := FDir + '\';
       ChDir(FDir);

       SearchResult := FindFirst(FDir + '*.*', FAttr, SearchRec);

       case VarType(FFilterFrom) of
       varInteger : begin // File size
                      while (SearchResult = 0)
                      do begin
                         if ((FFilterFrom <= SearchRec.Size) and (SearchRec.Size < FFilterTo))
                         then begin
                              FileFormat := ImageFileManager.GetFormatFromName(SearchRec.Name);
                              if (FileFormat <  FF_NONE) or (FileFormat > FF_USERDEFINED)
                              then begin
                                   new(pFileInfo);
                                   pFileInfo^.Size := SearchRec.Size;
                                   {$IFNDEF GE_DXE}
                                     pFileInfo^.DateTime := FileDateToDateTime(SearchRec.Time);
                                   {$ELSE}
                                     pFileInfo^.DateTime := SearchRec.TimeStamp;
                                   {$ENDIF}
                                   pFileInfo^.Attr :=  SearchRec.Attr;
                                   pFileInfo^.ExcludeAttr := SearchRec.ExcludeAttr;
                                   FFileList.AddObject(FDir + SearchRec.Name, pointer(pFileInfo));
                              end;
                         end;
                         SearchResult := FindNext(SearchRec);
                      end;
                    end;
       varString  : begin // file name
                      while (SearchResult = 0)
                      do begin
                         if (CompareText(FFilterFrom, SearchRec.Name) <= 0) and (CompareText(SearchRec.Name, FFilterTo) <= 0)
                         then begin
                              FileFormat := ImageFileManager.GetFormatFromName(SearchRec.Name);
                              if (FileFormat <  FF_NONE) or (FileFormat > FF_USERDEFINED)
                              then begin
                                   new(pFileInfo);
                                   pFileInfo^.Size := SearchRec.Size;
                                   {$IFNDEF GE_DXE}
                                     pFileInfo^.DateTime := FileDateToDateTime(SearchRec.Time);
                                   {$ELSE}
                                     pFileInfo^.DateTime := SearchRec.TimeStamp;
                                   {$ENDIF}
                                   pFileInfo^.Attr :=  SearchRec.Attr;
                                   pFileInfo^.ExcludeAttr := SearchRec.ExcludeAttr;
                                   FFileList.AddObject(FDir + SearchRec.Name, pointer(pFileInfo));
                              end;
                         end;
                         SearchResult := FindNext(SearchRec);
                      end;
                    end;
       varDate    : begin
                      while (SearchResult = 0)
                      do begin
                         try
                           {$IFNDEF GE_DXE}
                             if ((FFilterFrom <= FileDateToDateTime(SearchRec.Time)) and
                                 (FileDateToDateTime(SearchRec.Time) < FFilterTo))
                           {$ELSE}
                             if ((TDateTime(FFilterFrom) <= SearchRec.TimeStamp) and
                                 (SearchRec.TimeStamp < TDateTime(FFilterTo)))
                           {$ENDIF}
                             then begin
                                  FileFormat := ImageFileManager.GetFormatFromName(SearchRec.Name);
                                  if (FileFormat <  FF_NONE) or (FileFormat > FF_USERDEFINED)
                                  then begin
                                       new(pFileInfo);
                                       pFileInfo^.Size := SearchRec.Size;
                                       {$IFNDEF GE_DXE}
                                         pFileInfo^.DateTime := FileDateToDateTime(SearchRec.Time);
                                       {$ELSE}
                                         pFileInfo^.DateTime := SearchRec.TimeStamp;
                                       {$ENDIF}
                                       pFileInfo^.Attr :=  SearchRec.Attr;
                                       pFileInfo^.ExcludeAttr := SearchRec.ExcludeAttr;

                                       FFileList.AddObject(FDir + SearchRec.Name, pointer(pFileInfo));
                                  end;
                             end;
                         except // We do
                         end;
                         SearchResult := FindNext(SearchRec);
                      end;
                    end;
       else begin
            while (SearchResult = 0)
            do begin
               FileFormat := ImageFileManager.GetFormatFromName(SearchRec.Name);
               if (FileFormat <  FF_NONE) or (FileFormat > FF_USERDEFINED)
               then begin
                    new(pFileInfo);
                    pFileInfo^.Size := SearchRec.Size;
                    {$IFNDEF GE_DXE}
                      pFileInfo^.DateTime := FileDateToDateTime(SearchRec.Time);
                    {$ELSE}
                      pFileInfo^.DateTime := SearchRec.TimeStamp;
                    {$ENDIF}
                    pFileInfo^.Attr :=  SearchRec.Attr;
                    pFileInfo^.ExcludeAttr := SearchRec.ExcludeAttr;

                    FFileList.AddObject(FDir + SearchRec.Name, pointer(pFileInfo));
               end;
               SearchResult := FindNext(SearchRec);
            end;
            end;
       end;
  end;
  FCount := FFileList.Count;

  if (FCount > 0) // Sort list base on -
  then begin
       case FSortMethod of
       {$IFDEF DCB3_4}
       TSM_NONE : ;
       else FFileList.Sort;
       {$ELSE}
       TSM_FileName      : FFileList.CustomSort(SortByName);
       TSM_FileExt       : FFileList.CustomSort(SortByExt);
       TSM_FileTimeStamp : FFileList.CustomSort(SortByDateTime);
       TSM_FileSize      : FFileList.CustomSort(SortByFileSize);
       {$ENDIF}
       end;
  end;
end; // TmcmThumbView.GetFileNames.


function TmcmThumbView.UpdateThumb(NewImage : TmcmImage;
                                   Thumb    : TmcmThumbImage;
                                   Index    : integer) : integer;
var xScale      : double;
    yScale      : double;
    ColorStr    : string;
    FileSizeStr : string;
    HintStr     : string;
begin
  Result := -1;
  if Assigned(NewImage)
  then begin
       if Not(NewImage.Empty)
       then begin
            // Determine which Thumb to update.
            if Not(Assigned(Thumb))
            then begin
                 if (0 <= Index) and (Index < FThumbList.Count)
                 then begin
                      Thumb := TmcmThumbImage(FThumbList.Items[Index]);
                      Result := Index;
                 end
                 else begin
                 end;
            end
            else Result := FThumbList.Count;

            if Assigned(Thumb)
            then begin
                 Thumb.FFilename := NewImage.ImageInfo.FileName;
                 Thumb.Image.ImageFormat := IF_RGB24;
                 Thumb.Image.ScaleHVResolution := False;

                 // Create and draw thumb-nail.
                 xScale := FThumbWidth / NewImage.DispWidth;
                 yScale := FThumbHeight / NewImage.DispHeight;

                 if (xScale > yScale)
                 then xScale := yScale;
                 if (xScale > 1.0)
                 then begin
                      xScale := 1.0;
                      Thumb.Image.Height := NewImage.DispHeight;
                      Thumb.Image.Width  := NewImage.DispWidth;
                 end
                 else begin
                      Thumb.Image.Height := Trunc(NewImage.DispHeight * xScale);
                      Thumb.Image.Width  := Trunc(NewImage.DispWidth * xScale);
                 end;

                 if FHalfToned //and (NewImage.ImageFormat <> IF_GREY8)
                 then NewImage.SetStretchMode(HALFTONE);

                 Thumb.Image.Palette := NewImage.Palette;
                 Thumb.Image.ImageInfo.Assign(NewImage.ImageInfo);
                 NewImage.Draw(Thumb.Image.Canvas.Handle, xScale);

                 // Set thumb hint information.
                 case NewImage.BitCount of
                 1..14 : ColorStr := IntToStr(1 shl NewImage.BitCount);
                 15 : ColorStr := resColor32K;
                 16 : ColorStr := resColor64K;
                 24,
                 32 : ColorStr := resColor16M;
                 48 :
                 // else ColorStr := IntToStr(1 shl Thumb.Image.BitCount);
                 end;

                 if (NewImage.ImageInfo.FileSize > 1048576)
                 then FileSizeStr := FloatToStrF(NewImage.ImageInfo.FileSize / 1048576, ffFixed, 7, 3) + ' ' + resMegaBytes
                 else if (NewImage.ImageInfo.FileSize > 1024)
                      then FileSizeStr := FloatToStrF(NewImage.ImageInfo.FileSize / 1024, ffFixed, 7, 3) + ' ' + resKiloBytes
                      else FileSizeStr := IntToStr(NewImage.ImageInfo.FileSize) + ' ' + resBytes;

                 HintStr := resFileName + ': ' + ExtractFileName(Thumb.FFilename) + chr($0D) +
                            resDimension + ': ' + IntToStr(NewImage.Width) + ' x ' +
                            IntToStr(NewImage.Height) + ' x ' +
                            ColorStr + chr($0D) +
                            resFileSize + ': ' + FileSizeStr + chr($0D) +
                            resFileFormat + ': ' + ImageFileManager.FileFormatToStr(NewImage.ImageInfo.FileFormat) + chr($0D);
                 if Assigned(NewImage.ImageInfo.Exif)
                 then begin
                      HintStr := HintStr + resMetadata + ': ' + resExifShort;
                      if Assigned(NewImage.ImageInfo.GPS)
                      then HintStr := HintStr + ', ' + resGPS;
                      HintStr := HintStr + chr($0D);
                 end;
                 HintStr := HintStr +
                            resCompression + ': ' + CCompressStrings[integer(NewImage.Compression)] + chr($0D) +
                            resDateTime + ': ' + DateToStr(NewImage.ImageInfo.DateTime) +
                                          ' ' + TimeToStr(NewImage.ImageInfo.DateTime);

                 Thumb.Hint := HintStr;
                 Thumb.ShowHint := ShowHint; // Enable/disable hints.
            end;
       end;
  end;
end; // TmcmThumbView.UpdateThumb.


function TmcmThumbView.AddThumb(NewImage : TmcmImage) : integer;
var NewThumb    : TmcmThumbImage;
    bVScrollMax : boolean;
begin
  Result := -1;
  if Assigned(NewImage)
  then begin
       if Not(NewImage.Empty)
       then begin
            bVScrollMax := False;
            if (VertScrollBar.Position > 0)
            then if (VertScrollBar.Position >= VertScrollBar.Range - ClientHeight)
                 then bVScrollMax := True;

            NewThumb := TmcmThumbImage.Create(Self);

            NewThumb.Visible := False;
            if (UpdateThumb(NewImage, NewThumb, -1) >= 0)
            then begin
                 NewThumb.Top    := 0;
                 NewThumb.Left   := 0;
                 NewThumb.Height := FThumbHeight + FThumbBorder + FThumbTextH;
                 NewThumb.Width  := FThumbWidth + FThumbBorder;
                 NewThumb.ThumbTextHeight := FThumbTextH;
                 NewThumb.Center      := False;
                 NewThumb.ScaleToFit  := False;
                 NewThumb.OnClick     := OnImageClick;
                 NewThumb.OnDblClick  := OnImageDblClick;
                 NewThumb.Parent      := Self;
                 NewThumb.ParentColor := False;
                 NewThumb.Color       := ColorToRGB(FThumbColor); // If transparent (clNone) the file name isn't displayed!
                 NewThumb.TextColor   := ColorToRGB(Font.Color);
                 NewThumb.Flat        := FThumbFlat;
                 NewThumb.BorderStyle := FThumbStyle;

                 NewThumb.DragMode    := FDragModeThumb;
                 NewThumb.OnStartDrag := OnThumbStartDrag;
                 NewThumb.OnEndDrag   := OnThumbEndDrag;

                 Result := FThumbList.Add(NewThumb);
                 SetThumbPosition(FThumbList.Count - 1);

                 if FShowAtOnce
                 then begin
                      NewThumb.Visible := True;
                      NewThumb.Paint;
                 end;

                 if bVScrollMax and (VertScrollBar.Position <> VertScrollBar.Range)
                 then VertScrollBar.Position := VertScrollBar.Range;
            end
            else NewThumb.Free;
       end;
  end;
end; // TmcmThumbView.AddThumb.


procedure TmcmThumbView.OnThreadImageReady(Sender : TObject; NewImage : TmcmImage; PageNumber : integer);
begin
  if Assigned(NewImage)
  then begin
       try
         if Not(NewImage.Empty)
         then begin
              AddThumb(NewImage);
              if Assigned(FOnProgress)
              then FOnProgress(Self, FImageToLoad + 1, FCount, FCancel);
         end
         else begin
              if (PageNumber <> 0)
              then dec(FCount);
              PageNumber := 0;
         end;
       except
         On E:Exception
         do dec(FCount);
       end;
  end
  else dec(FCount);

  if (PageNumber = 0) or Not(FShowAllPages)
  then begin
       inc(FImageToLoad);
       PageNumber := 0;
  end;
  if (FCancel = False)
  then PostMessage(Handle, WM_GETTHUMBS, 0, PageNumber);
end; // TmcmThumbView.OnThreadImageReady.


procedure TmcmThumbView.GetNextThumbImage(var Message : TMessage);
var i    : integer;
    Skip : boolean;
begin
  // Request next image from TLoadImageFile thread.
  if (FCancel = False)
  then begin
       Sleep(1);
       if (FImageToLoad < FFileList.Count)
       then begin
            repeat
              Skip := False;
              if Assigned(FOnBeforeLoadThumb)
              then FOnBeforeLoadThumb(Self, FFileList.Strings[FImageToLoad], Skip, FCancel);
              if Skip
              then begin
                   dec(FCount);
                   inc(FImageToLoad);
              end;
            until Not(Skip) or FCancel or (FImageToLoad >= FFileList.Count);

            if Not(FCancel) and (FImageToLoad < FFileList.Count)
            then FThreadLoader.LoadFile(FFileList.Strings[FImageToLoad], Message.LParam);
       end;

       if (FImageToLoad >= FFileList.Count) or FCancel
       then begin
            FCount := FThumbList.Count;
            if Not(FShowAtOnce)
            then begin
                 for i := 0 to (FThumbList.Count - 1)
                 do TmcmThumbImage(FThumbList.Items[i]).Visible := True;
            end;
       end;
(*
       if (Message.LParam = 0)
       then inc(FImageToLoad);
*)
  end;
end; // TmcmThumbView.GetNextThumbImage.


procedure TmcmThumbView.SetThumbPosition(Index : longint);
var x, y   : longint;
    dx, dy : longint;
begin
  x := (Index mod FHorizThumbs);
  y := (Index div FHorizThumbs);
  dx := x * FTotalWidth - HorzScrollBar.Position + FThumbSpace2;
  dy := y * FTotalHeight - VertScrollBar.Position + FThumbSpace2;
  TmcmThumbImage(FThumbList.Items[Index]).Left := dx;
  TmcmThumbImage(FThumbList.Items[Index]).Top  := dy;
end; // TmcmThumbView.SetThumbPosition.


procedure TmcmThumbView.SetThumbSizes(Index : longint);
begin
  TmcmThumbImage(FThumbList.Items[Index]).Height := FThumbHeight + FThumbBorder + FThumbTextH;
  TmcmThumbImage(FThumbList.Items[Index]).Width  := FThumbWidth + FThumbBorder;
  TmcmThumbImage(FThumbList.Items[Index]).ThumbTextHeight := FThumbTextH;
end; // TmcmThumbView.SetThumbSizes.


procedure TmcmThumbView.SetAllThumbSizes;
var i : integer;
begin
  i := 0;
  while (i < FThumbList.Count) and (FCancel = False)
  do begin
     SetThumbSizes(i);
     inc(i);
  end;
end; // TmcmThumbView.SetAllThumbSizes.


procedure TmcmThumbView.RepositionThumbs;
var i : longint;
begin
  CalcHorizThumbs;
  for i := 0 to (FThumbList.Count - 1)
  do SetThumbPosition(i);
  InvalidateRect(Handle, Nil, True);
end; // TmcmThumbView.RepositionThumbs.


function TmcmThumbView.GetCancel : boolean;
begin
  Result := FCancel;
end; // TmcmThumbView.GetCancel.


procedure TmcmThumbView.SetCancel(Value : boolean);
begin
  FCancel := Value;
end; // TmcmThumbView.SetCancel.


function TmcmThumbView.GetCount : integer;
begin
  Result := FThumbList.Count; //FCount;
end; // TmcmThumbView.GetCount.


function TmcmThumbView.GetSelected : integer;
begin
  Result := FSelected;
end; // TmcmThumbView.GetSelected.


procedure TmcmThumbView.DoSelectThumb(Index : integer);
begin
  if (Index > -1) and (FThumbList.Count > 0)
  then begin
       TmcmThumbImage(FThumbList.Items[Index]).Color := ColorToRGB(FSelColor);
       TmcmThumbImage(FThumbList.Items[Index]).TextColor := ColorToRGB(FSelTextColor);
       TmcmThumbImage(FThumbList.Items[Index]).Paint;
  end;
end; // TmcmThumbView.DoSelectThumb.


procedure TmcmThumbView.DoDeselectThumb(Index : integer);
begin
  if (Index > -1)
  then begin
       try
         if (Index < FThumbList.Count)
         then begin
              if Assigned(FThumbList.Items[Index])
              then begin
                   TmcmThumbImage(FThumbList.Items[Index]).Color := ColorToRGB(FThumbColor);
                   TmcmThumbImage(FThumbList.Items[Index]).TextColor := ColorToRGB(Font.Color);
                   TmcmThumbImage(FThumbList.Items[Index]).Paint;
              end;
         end;
       except
       end;
  end;
end; // TmcmThumbView.DoDeselectThumb.


procedure TmcmThumbView.DoDeselectThumbList(ExcludeIndex : integer);
var i, j : integer;
begin
  for i := 0 to (FSelectedThumb.Count - 1)
  do begin
     j := FThumbList.IndexOf(FSelectedThumb.Items[i]);
     if (j <> ExcludeIndex)
     then DoDeselectThumb(j);
  end;
  FSelectedThumb.Clear;
end; // TmcmThumbView.DoDeselectThumbList.


procedure TmcmThumbView.SetSelected(Value : integer);
var ShiftDown   : boolean;
    CtrlDown    : boolean;
    RButtonDown : boolean;
    i, j, k     : integer;
begin
  if FAllowMultiSel
  then begin
       ShiftDown   := (GetAsyncKeyState(VK_SHIFT) and $8000 <> 0);
       CtrlDown    := (GetAsyncKeyState(VK_CONTROL) and $8000 <> 0);
       RButtonDown := (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0);

       // If we right-click on one of many selected items don't change selection.
       if RButtonDown
       then begin
            if (FSelectedThumb.Count > 1)
            then begin
                 j := FSelectedThumb.IndexOf(FThumbList.Items[Value]);
                 if (j > -1)
                 then Exit;
            end;
       end;

       if CtrlDown or ShiftDown
       then begin
            if ShiftDown
            then begin
                 // Deselect all items in list
                 DoDeselectThumbList(-1);

                 if (Value > FShiftStart)
                 then begin
                      k := Value;
                      j := FShiftStart;
                 end
                 else begin
                      j := Value;
                      k := FShiftStart;
                 end;

                 for i := j to k
                 do begin
                    FSelectedThumb.Add(FThumbList.Items[i]);
                    DoSelectThumb(i);
                 end;
                 FSelected := Value;
            end
            else begin
                 if CtrlDown
                 then begin
                      j := FSelectedThumb.IndexOf(FThumbList.Items[Value]);
                      if (j <> -1)
                      then begin
                           FSelected := Value;
                           DoDeselectThumb(Value);
                           FSelectedThumb.Delete(j);
                      end
                      else begin
                           FSelected := Value;
                           FSelectedThumb.Add(FThumbList.Items[FSelected]);
                           DoSelectThumb(FSelected);
                      end;
                      FShiftStart := Value;
                 end;
            end;
       end
       else begin
            // Deselect all items in list
            DoDeselectThumbList(Value);

            FSelected := Value;
            if (0 <= FSelected) and (FSelected < FThumbList.Count)
            then FSelectedThumb.Add(FThumbList.Items[FSelected]);
            DoSelectThumb(FSelected);
            FShiftStart := Value;
       end;
  end
  else begin
       FShiftStart := -1;
       DoDeselectThumb(FSelected);
       FSelected := Value;
       FShiftStart := Value;
       DoSelectThumb(FSelected);
  end;
  FMWSelected := FSelected;
  FShiftEnd := FShiftStart;
end; // TmcmThumbView.SetSelected.


function TmcmThumbView.GetSelColor : TColor;
begin
  Result := FSelColor;
end; // TmcmThumbView.GetSelColor.


procedure TmcmThumbView.SetSelColor(Value : TColor);
begin
  if (FSelColor <> TColorRef(Value))
  then begin
       FSelColor := Value;
       if (FSelected > -1)
       then SetSelected(FSelected);
  end;
end; // TmcmThumbView.SetSelColor.


function TmcmThumbView.GetSelTextColor : TColor;
begin
  Result := FSelTextColor;
end; // TmcmThumbView.GetSelTextColor.


procedure TmcmThumbView.SetSelTextColor(Value : TColor);
begin
  if (FSelColor <> TColorRef(Value))
  then begin
       FSelTextColor := Value;
       if (FSelected > -1)
       then SetSelected(FSelected);
  end;
end; // TmcmThumbView.SetSelTextColor.


function TmcmThumbView.GetDir : string;
begin
  Result := FDir;
end; // TmcmThumbView.GetDir.


procedure TmcmThumbView.SetDir(Value : string);
var SaveCursor : TCursor;
    Msg        : TMessage;
begin
  FCancel := True;
  if (FDir <> Value) //and DirectoryExists(Value)
  then begin
       SaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;
       FCancel := True;
       try
         if Assigned(FThreadLoader)
         then begin
              FThreadLoader.CancelRead := True;
              FThreadLoader.FreeOnTerminate := True;

              // Terminate thread.
              FThreadLoader.Quit;
              FThreadLoader := Nil;
              Sleep(5);
         end;

         // Threaded image loader.
         FThreadLoader := TLoadImageFile.Create(False);
         FThreadLoader.OnImageReady := OnThreadImageReady;
         FThreadLoader.LoadFile('', 0);
         Sleep(10);
         FThisLoaded := True;
         Sleep(10);
       finally
         FCancel := False;
         Screen.Cursor := SaveCursor;
       end;

       FDir := Value;
       FSelected := -1;
       FShiftStart := -1;
       FShiftEnd := -1;
       FSelectedThumb.Clear;

       if FThisLoaded
       then begin
            GetFileNames;
            FImageToLoad := 0;
            if (FFileList.Count > 0)
            then begin
                 if Visible
                 then RepositionThumbs;
                 FCancel := False;
                 FillMemory(@Msg, SizeOf(TMessage), 0);
                 GetNextThumbImage(Msg);
                 if Assigned(FDirMonitor)
                 then begin
                      FDirMonitor.OnImageFile := Nil;
                      if FDirMonitor.Suspended
                      then FDirMonitor.Resume;
                      FDirMonitor.FreeOnTerminate := True;
                      FDirMonitor.Terminate;
                      FDirMonitor := Nil;
                 end;

                 if FEnableMonitor
                 then begin
                      FDirMonitor := TmcmDirectoryMonitor.Create(False);
                      if Assigned(FDirMonitor)
                      then begin
                           FDirMonitor.MonitorPath := FDir;
                           FDirMonitor.OnImageFile := OnFileChange;
                      end;
                 end;
            end;
       end;
  end;
  FCancel := False;
end; // TmcmThumbView.SetDir.


procedure TmcmThumbView.UpdateThumbsInDir;
var SaveDir : string;
begin
  SaveDir := FDir;
  FDir := '';
  SetDir(SaveDir);
end; // TmcmThumbView.UpdateThumbsInDir.


function TmcmThumbView.GetThumbBorder : integer;
begin
  Result := FThumbBorder;
end; // TmcmThumbView.GetThumbBorder.


procedure TmcmThumbView.SetThumbBorder(Value : integer);
begin
  if (FThumbBorder <> Value) and (Value > 0)
  then begin
       FThumbBorder := Value;
       FTotalHeight := FThumbHeight + FThumbBorder + FThumbTextH + FThumbSpace2;
       FTotalWidth  := FThumbWidth + FThumbBorder + FThumbSpace2;
       SetAllThumbSizes;
       RepositionThumbs;
  end;
end; // TmcmThumbView.SetThumbBorder.


function TmcmThumbView.GetThumbBorderFlat : boolean;
begin
  Result := FThumbFlat;
end; // TmcmThumbView.GetThumbBorderFlat.


procedure TmcmThumbView.SetThumbBorderFlat(Value : boolean);
begin
  if (FThumbFlat <> Value)
  then begin
       FThumbFlat := Value;
  end;
end; // TmcmThumbView.SetThumbBorderFlat.


function TmcmThumbView.GetThumbBorderStyle : TmcmBorderStyle;
begin
  Result := FThumbStyle;
end; // TmcmThumbView.GetThumbBorderStyle.


procedure TmcmThumbView.SetThumbBorderStyle(Value : TmcmBorderStyle);
begin
  if (FThumbStyle <> Value)
  then begin
       FThumbStyle := Value;
       if (FThumbStyle <> BS_NONE)
       then if (FThumbBorder < 8)
            then FThumbBorder := 8;
  end;
end; // TmcmThumbView.SetThumbBorderStyle.


function TmcmThumbView.GetThumbColor : TColor;
begin
  Result := FThumbColor;
end; // TmcmThumbView.GetThumbColor.


procedure TmcmThumbView.SetThumbColor(Value : TColor);
begin
  if (FThumbColor <> Value)
  then begin
       FThumbColor := Value;
  end;
end; // TmcmThumbView.SetThumbColor.


function TmcmThumbView.GetThumbSpace : integer;
begin
  Result := FThumbSpace;
end; // TmcmThumbView.GetThumbSpace.


procedure TmcmThumbView.SetThumbSpace(Value : integer);
begin
  if (FThumbSpace <> Value)
  then begin
       if (Value < 10)
       then Value := 10;
       FThumbSpace := Value;
       FThumbSpace2 := FThumbSpace div 2;
       FTotalHeight := FThumbHeight + FThumbBorder + FThumbTextH + FThumbSpace2;
       FTotalWidth  := FThumbWidth + FThumbBorder + FThumbSpace2;
       SetAllThumbSizes;
       RepositionThumbs;
  end;
end; // TmcmThumbView.SetThumbSpace.


function TmcmThumbView.GetThumbHeight : integer;
begin
  Result := FThumbHeight;
end; // TmcmThumbView.GetThumbHeight.


procedure TmcmThumbView.SetThumbHeight(Value : integer);
begin
  if (FThumbHeight <> Value)
  then begin
       if (Value < 10)
       then Value := 10;
       FThumbHeight := Value;
       FTotalHeight := FThumbHeight + FThumbBorder + FThumbTextH + FThumbSpace2;
       SetAllThumbSizes;
       RepositionThumbs;
       VertScrollBar.Increment := FTotalHeight;
  end;
end; // TmcmThumbView.SetThumbHeight.


function TmcmThumbView.GetThumbWidth : integer;
begin
  Result := FThumbWidth;
end; // TmcmThumbView.GetThumbWidth.


procedure TmcmThumbView.SetThumbWidth(Value : integer);
begin
  if (FThumbWidth <> Value)
  then begin
       FThumbWidth := Value;
       FTotalWidth := FThumbWidth + FThumbBorder + FThumbSpace2;
       SetAllThumbSizes;
       RepositionThumbs;
       HorzScrollBar.Increment := FTotalWidth;
  end;
end; // TmcmThumbView.SetThumbWidth.


function TmcmThumbView.GetShowAllPages : boolean;
begin
  Result := FShowAllPages;
end; // TmcmThumbView.GetShowAllPages.


procedure TmcmThumbView.SetShowAllPages(Value : boolean);
begin
  if (FShowAllPages <> Value)
  then begin
       FShowAllPages := Value;
       if FAllowUpdate
       then UpdateThumbsInDir;
  end;
end; // TmcmThumbView.SetShowAllPages.


function TmcmThumbView.GetShowAtOnce : boolean;
begin
  Result := FShowAtOnce;
end; // TmcmThumbView.GetShowAtOnce.


procedure TmcmThumbView.SetShowAtOnce(Value : boolean);
begin
  FShowAtOnce := Value;
end; // TmcmThumbView.SetShowAtOnce.


procedure TmcmThumbView.OnImageClick(Sender : TObject);
begin
  if (Sender is TmcmThumbImage)
  then SetSelected(FThumbList.IndexOf(Sender));
  if Assigned(Self.OnClick)
  then OnClick(Self);
end; // TmcmThumbView.OnImageClick.


procedure TmcmThumbView.OnImageDblClick(Sender : TObject);
var PageNumber : integer;
begin
  if (Sender is TmcmThumbImage)
  then if Assigned(FOnLoadImage) and (FSelected > -1)
       then begin
            if FShowAllPages
            then PageNumber := TmcmThumbImage(FThumbList.Items[FSelected]).Image.ImageInfo.PageNumber
            else PageNumber := -1;
            FOnLoadImage(Sender, TmcmThumbImage(FThumbList.Items[FSelected]).FFilename, PageNumber);
       end;
  if Assigned(Self.OnDblClick)
  then OnDblClick(Self);
end; // TmcmThumbView.OnImageDblClick.


procedure TmcmThumbView.OnFileChange(Sender : TObject; AFileName : string; PageNumber : integer);
var SearchRec    : TSearchRec;
    SearchResult : integer;
    FileFormat   : TmcmFileFormat;
    i, Save      : integer;
    FileDate     : integer;
    FileHandle   : integer;
    ThumbImage   : TmcmImage;
begin
  OutputDebugString('TmcmThumbView.OnFileChange');
  // Was a file delete
  //i := FFileList.Count;
  i := FThumbList.Count;
  while (i > 0)
  do begin
     dec(i);
     // Then remove the thumb nail.
     if Not(FileExists(TmcmThumbImage(FThumbList.Items[i]).Image.ImageInfo.FileName))
     // if Not(FileExists(FFileList.Strings[i]))
     then DeleteThumb(i);
  end;

  // Check if thumb exists and needs updating.
  for i := 0 to (FThumbList.Count - 1)
  do begin
     if FileExists(TmcmThumbImage(FThumbList.Items[i]).Image.ImageInfo.FileName)
     then begin
          FileHandle := FileOpen(TmcmThumbImage(FThumbList.Items[i]).Image.ImageInfo.FileName, fmOpenRead or fmShareDenyNone);
          if (FileHandle <> -1)
          then begin
               FileDate := FileGetDate(FileHandle);
               FileClose(FileHandle);
               if (TmcmThumbImage(FThumbList.Items[i]).Image.ImageInfo.DateTime <> FileDateToDateTime(FileDate))
               then begin
                    // Re-load thumb image.
                    ThumbImage := TmcmImage.Create;
                    try
                      ThumbImage.FileOpen(TmcmThumbImage(FThumbList.Items[i]).Image.ImageInfo.FileName);
                      UpdateThumb(ThumbImage, Nil, i);
                    except
                    end;
                    ThumbImage.Free;
               end;
          end;
     end;
  end;

  // Was a file added
  if (FCancel = False)
  then begin
       Save := FFileList.Count;
       SearchResult := FindFirst(FDir + '*.*', FAttr, SearchRec);
       while (SearchResult = 0)
       do begin
          FileFormat := ImageFileManager.GetFormatFromName(SearchRec.Name);
          if (FileFormat <  FF_NONE) or (FileFormat > FF_USERDEFINED)
          then if (FFileList.IndexOf(FDir + SearchRec.Name) < 0)
               then FFileList.Add(FDir + SearchRec.Name);
          SearchResult := FindNext(SearchRec);
       end;
       if (Save < FFileList.Count)
       then begin
            FImageToLoad := Save;// - 1;
            if (FCancel = False)
            then begin
                 Sleep(100);
                 PostMessage(Handle, WM_GETTHUMBS, 0, 0);
            end;
       end;
  end;
end; // TmcmThumbView.OnFileChange.


procedure TmcmThumbView.DeleteThumb(Index : integer);
var i : integer;
begin
  if (0 <= Index) and (Index < FThumbList.Count)
  then begin
       if Assigned(FThumbList.Items[Index])
       then begin
            // Delete item from FFileList.
            i := FFileList.IndexOf(TmcmThumbImage(FThumbList.Items[Index]).FFilename);
            if (i >= 0)
            then FFileList.Delete(i);

            // Delete item from SelectedThumb.
            i := FSelectedThumb.IndexOf(FThumbList.Items[Index]);
            if (i >= 0)
            then FSelectedThumb.Delete(i);

            // Delete the Thumbnail.
            TmcmThumbImage(FThumbList.Items[Index]).Free;
            FThumbList.Delete(Index);
       end;
       if (Index = FSelected) and (FSelectedThumb.Count = 0)
       then begin
            if (FSelected > 0)
            then Selected := FSelected - 1
            else Selected := FSelected;
       end;
       FThumbList.Pack;
       RepositionThumbs;
       InvalidateRect(Handle, Nil, True);
  end;
end; // TmcmThumbView.DeleteThumb.


procedure TmcmThumbView.DeleteSelected;
var Filename : string;
    i, j, k  : integer;
begin
  if FAllowMultiSel
  then begin
       if Assigned(FDirMonitor)
       then FDirMonitor.Suspend; // Suspend to avoid conflict deleting thumbs twice.
       i := FSelectedThumb.Count - 1;
       while (i >= 0)
       do begin
          j := FThumbList.IndexOf(FSelectedThumb.Items[i]);
          Filename := TmcmThumbImage(FThumbList.Items[j]).FFilename;
          if (Filename <> '') and FileExists(Filename) and (j >= 0)
          then begin
               if DeleteFile(Filename)
               then begin
                    DeleteThumb(j);
                    if FShowAllPages
                    then begin // Multi-paged files
                         // Remove thumbs named Filename from the selected list.
                         k := FSelectedThumb.Count - 1;
                         while (k >= 0)
                         do begin
                            if (TmcmThumbImage(FSelectedThumb.Items[k]).FFilename = Filename)
                            then FSelectedThumb.Delete(k);
                            dec(k);
                         end;
                         // Find all other thumbs with the same file name and
                         // delete these.
                         k := FThumbList.Count - 1;
                         while (k >= 0)
                         do begin
                            if (TmcmThumbImage(FThumbList.Items[k]).FFilename = Filename)
                            then DeleteThumb(k);
                            dec(k);
                         end;
                    end;
               end;
          end;
          dec(i);
       end;
       if Assigned(FDirMonitor)
       then FDirMonitor.Resume;
  end
  else begin
       Filename := SelectedFilename;
       if (Filename <> '') and FileExists(Filename) and (FSelected >= 0)
       then begin
            if Assigned(FDirMonitor)
            then FDirMonitor.Suspend; // Suspend to avoid conflict deleting thumb twice.
            if DeleteFile(Filename)
            then begin
                 DeleteThumb(FSelected);
                 if FShowAllPages
                 then begin
                      // Find all other thumbs with the same file name and
                      // delete these.
                      k := FThumbList.Count - 1;
                      while (k >= 0)
                      do begin
                         if (TmcmThumbImage(FThumbList.Items[k]).FFilename = Filename)
                         then DeleteThumb(k);
                         dec(k);
                      end;
                 end;
            end;
            if Assigned(FDirMonitor)
            then FDirMonitor.Resume;
       end
       else ShowMessage(Format(resFileDoesNotExist, [Filename]));
  end;
  FSelected := -1;
  FSelectedThumb.Clear;
end; // TmcmThumbView.DeleteSelected.


{$H+}

function TmcmThumbView.SHELLFileCommand(Cmd : integer; const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
var Src, Dest : string;
    Path      : string;
    FOS       : TSHFileOpStruct;
begin
  Result := -1;
  Src  := SrcName;
  Dest := DestName;
  Path := ExtractFilePath(SrcName);
  if (Src <> '') and FileExists(Src)
  then begin
       Src  := Src + #0;
       Dest := Dest + #0;
       FillChar(FOS, SizeOf(FOS), 0);
       with FOS
       do begin
          Wnd := Handle;
          wFunc := Cmd;
          pTo   := PChar(Dest);
          pFrom := PChar(Src);
          fFlags := FOF_ALLOWUNDO;
          hNameMappings := Nil;
          if Not(Confirm)
          then fFlags := fFlags or FOF_NOCONFIRMATION;
          if Silent
          then fFlags := fFlags or FOF_SILENT;
       end;

       if SetCurrentDirectory(PChar(Path))
       then begin
            Result := SHFileOperation(FOS);
            if (Result = 0)
            then begin
                 if (FOS.fAnyOperationsAborted)
                 then Result := -1;
            end;
       end;
  end
  else ShowMessage(Format(resFileDoesNotExist, [SrcName]));
end; // TmcmThumbView.SHELLFileCommand.


function TmcmThumbView.CopyFile(const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
begin
  Result := SHELLFileCommand(FO_COPY, SrcName, DestName, Confirm, Silent);
end; // TmcmThumbView.CopyFile.


function TmcmThumbView.MoveFile(const SrcName, DestName : string; Confirm, Silent : boolean) : integer;
begin
  Result := SHELLFileCommand(FO_MOVE, SrcName, DestName, Confirm, Silent);
end; // TmcmThumbView.MoveFile.


procedure TmcmThumbView.RecycleSelected(Confirm, Silent : boolean);
var Filename : string;
    i, j, k  : integer;
begin
  if FAllowMultiSel
  then begin
       if Assigned(FDirMonitor)
       then FDirMonitor.Suspend; // Suspend to avoid conflict deleting thumb twice.
       i := FSelectedThumb.Count - 1;
       while (i >= 0)
       do begin
          j := FThumbList.IndexOf(FSelectedThumb.Items[i]);
          Filename := TmcmThumbImage(FThumbList.Items[j]).FFilename;
          if (Filename <> '') and (j >= 0)
          then begin
               if (SHELLFileCommand(FO_DELETE, Filename, '', Confirm, Silent) = 0)
               then begin
                    DeleteThumb(j);
                    if FShowAllPages
                    then begin // Multi-paged files
                         // Remove thumbs named Filename from the selected list.
                         k := FSelectedThumb.Count - 1;
                         while (k >= 0)
                         do begin
                            if (TmcmThumbImage(FSelectedThumb.Items[k]).FFilename = Filename)
                            then FSelectedThumb.Delete(k);
                            dec(k);
                         end;
                         // Find all other thumbs with the same file name and
                         // delete these.
                         k := FThumbList.Count - 1;
                         while (k >= 0)
                         do begin
                            if (TmcmThumbImage(FThumbList.Items[k]).FFilename = Filename)
                            then DeleteThumb(k);
                            dec(k);
                         end;
                    end;
               end
               else Break;
          end;
          dec(i);
          if (i > FSelectedThumb.Count - 1)
          then i := FSelectedThumb.Count - 1;
       end;
       if Assigned(FDirMonitor)
       then FDirMonitor.Resume;
  end
  else begin
       Filename := SelectedFilename;
       if (Filename <> '')
       then begin
            if Assigned(FDirMonitor)
            then FDirMonitor.Suspend; // Suspend to avoid conflict deleting thumb twice.
            if (SHELLFileCommand(FO_DELETE, Filename, '', Confirm, Silent) = 0)
            then begin
                 DeleteThumb(FSelected);
                 if FShowAllPages
                 then begin
                      // Find all other thumbs with the same file name and
                      // delete these.
                      k := FThumbList.Count - 1;
                      while (k >= 0)
                      do begin
                         if (TmcmThumbImage(FThumbList.Items[k]).FFilename = Filename)
                         then DeleteThumb(k);
                         dec(k);
                      end;
                 end;
            end;
            if Assigned(FDirMonitor)
            then FDirMonitor.Resume;
       end
       else ShowMessage(Format(resFileDoesNotExist, [Filename]));
  end;
  FSelected := -1;
  FSelectedThumb.Clear;
end; // TmcmThumbView.RecycleSelected.


procedure TmcmThumbView.RenameSelected(Filename : string);
var OldName   : string;
    ThumbRect : TRect;
begin
  if (FSelectedThumb.Count > 1)
  then Exit;

  OldName := SelectedFilename;
  if (OldName <> '') and FileExists(OldName)
  then begin
       if Not(FileExists(Filename))
       then begin
            if Assigned(FDirMonitor)
            then FDirMonitor.Suspend; // Suspend to avoid conflict deleting thumb twice.
            if RenameFile(OldName, Filename)
            then begin
                 TmcmThumbImage(FThumbList.Items[FSelected]).FFilename := Filename;
                 ThumbRect.Left   := TmcmThumbImage(FThumbList.Items[FSelected]).Left;
                 ThumbRect.Top    := TmcmThumbImage(FThumbList.Items[FSelected]).Top;
                 ThumbRect.Right  := ThumbRect.Left + TmcmThumbImage(FThumbList.Items[FSelected]).Width;
                 ThumbRect.Bottom := ThumbRect.Top + TmcmThumbImage(FThumbList.Items[FSelected]).Height;
                 InvalidateRect(Handle, @ThumbRect, False);
            end;
            if Assigned(FDirMonitor)
            then FDirMonitor.Resume;
       end
       else ShowMessage(Format(resFileAlreadyExist, [Filename]));
  end
  else ShowMessage(Format(resFileDoesNotExist, [OldName]));
end; // TmcmThumbView.RenameSelected.


function TmcmThumbView.SelectedFileName : string;
begin
  if (FSelected > -1)
  then Result := TmcmThumbImage(FThumbList.Items[FSelected]).FFilename
  else Result := '';
end; // TmcmThumbView.SelectedFileName.


function TmcmThumbView.SelectedCount : integer;
begin
  if FAllowMultiSel
  then Result := FSelectedThumb.Count
  else if (FSelected > -1)
       then Result := 1
       else Result := 0;
end; // TmcmThumbView.SelectedCount.


function TmcmThumbView.SelectedFileNames(Index : integer) : string;
begin
  if FAllowMultiSel
  then begin
       if (0 <= Index) and (Index < FSelectedThumb.Count)
       then Result := TmcmThumbImage(FSelectedThumb.Items[Index]).FFilename
       else Result := '';
  end
  else Result := SelectedFileName;
end; // TmcmThumbView.SelectedFileNames.


function TmcmThumbView.GetThumb(Index : integer) : TmcmThumbImage;
begin
  Result := Nil;
  if (0 <= Index) and (Index < FThumbList.Count)
  then Result := TmcmThumbImage(FThumbList.Items[Index]);
end; // TmcmThumbView.GetThumb.


//------------------------------------------------------------------------------
// Sort methods.

procedure TmcmThumbView.SetSortMethod(Value : TThumbSortMethod);
begin
  if (FSortMethod <> Value)
  then begin
       FSortMethod := Value;
  end;
end; // TmcmThumbView.SetSortMethod.


function TmcmThumbView.GetFilterFrom : Variant;
begin
  Result := FFilterFrom;
end; // TmcmThumbView.GetFilterFrom.


function TmcmThumbView.GetFilterTo : Variant;
begin
  Result := FFilterTo;
end; // TmcmThumbView.GetFilterTo.


procedure TmcmThumbView.SetFilterFrom(FromValue : Variant);
begin
  VarClear(FFilterFrom);
  TVarData(FFilterFrom).VType := VarType(FromValue);
  FFilterFrom := FromValue;
end; // TmcmThumbView.SetFilterFrom.


procedure TmcmThumbView.SetFilterTo(ToValue : Variant);
begin
  VarClear(FFilterTo);
  TVarData(FFilterTo).VType := VarType(ToValue);
  FFilterTo := ToValue;
end; // TmcmThumbView.SetFilterTo.


{$IFNDEF DCB3_4}
procedure TmcmThumbView.SetDescending(Value : boolean);
begin
  if (FDescending <> Value)
  then begin
       FDescending := Value;
       gvDescending := Value;
  end;
end; // TmcmThumbView.SetDescending.


function SortByName(List : TStringList; Index1, Index2 : integer) : integer;
var Name1 : string;
    Name2 : string;
begin
  // Item1 < Item2 -> Result = -1, Item1 before Items2
  // Item1 = Item2 -> Result = 0, the same
  // Item1 > Item2 -> Result = 1, Item1 after Items2
  Name1 := LowerCase(ExtractFileName(List.Strings[Index1]));
  Name2 := LowerCase(ExtractFileName(List.Strings[Index2]));
  Result := AnsiCompareStr(Name1, Name2);
  if Not(gvDescending)
  then Result := -Result;
end; // SortByName.


function SortByExt(List : TStringList; Index1, Index2 : integer) : integer;
var Name1 : string;
    Name2 : string;
    Ext1  : string;
    Ext2  : string;
begin
  // Item1 < Item2 -> Result = -1, Item1 before Items2
  // Item1 = Item2 -> Result = 0, the same
  // Item1 > Item2 -> Result = 1, Item1 after Items2
  Name1 := LowerCase(ExtractFileName(List.Strings[Index1]));
  Name2 := LowerCase(ExtractFileName(List.Strings[Index2]));
  Ext1 := ExtractFileExt(Name1);
  Ext2 := ExtractFileExt(Name2);

  Result := AnsiCompareStr(Ext1, Ext2);
  if (Result = 0)
  then Result := AnsiCompareStr(Name1, Name2);
  if Not(gvDescending)
  then Result := -Result;
end; // SortByExt.


function SortByDateTime(List : TStringList; Index1, Index2 : integer) : integer;
var FileInfo1 : PmcmFileInfo;
    FileInfo2 : PmcmFileInfo;
begin
  Result := 0;

  if (List.Objects[Index1] <> Nil)
  then FileInfo1 := PmcmFileInfo(List.Objects[Index1])
  else Exit;
  if (List.Objects[Index2] <> Nil)
  then FileInfo2 := PmcmFileInfo(List.Objects[Index2])
  else Exit;

  if (FileInfo1.DateTime > FileInfo2.DateTime)
  then Result := -1
  else if (FileInfo1.DateTime < FileInfo2.DateTime)
       then Result := 1
       else Result := 0;
  if Not(gvDescending)
  then Result := -Result;
end; // SortByDateTime.


function SortByFileSize(List : TStringList; Index1, Index2 : integer) : integer;
var FileInfo1 : PmcmFileInfo;
    FileInfo2 : PmcmFileInfo;
begin
  try
    Result := 0;
    if (List.Objects[Index1] <> Nil)
    then FileInfo1 := PmcmFileInfo(List.Objects[Index1])
    else Exit;
    if (List.Objects[Index2] <> Nil)
    then FileInfo2 := PmcmFileInfo(List.Objects[Index2])
    else Exit;

    if (FileInfo1.Size > FileInfo2.Size)
    then Result := -1
    else if (FileInfo1.Size < FileInfo2.Size)
         then Result := 1
         else Result := 0;
    if Not(gvDescending)
    then Result := -Result;
  except
    Result := 0;
  end;
end; // SortByFileSize.

{$ENDIF}  // EndIf Sort


//------------------------------------------------------------------------------
// Drag & Drop methods.

procedure TmcmThumbView.SetAllowFileMove(Value : boolean);
begin
  FAllowFileMove := Value;
  if Assigned(FDropSource)
  then FDropSource.EnableMove := FAllowFileMove;
end; // TmcmThumbView.SetAllowFileMove.


procedure TmcmThumbView.SetExternalDragDrop(Value : boolean);
begin
  FExtDragDrop := Value;
  if FExtDragDrop
  then begin
       if Assigned(FDropTarget)
       then FDropTarget.Active := True;
       FDropSource.DragMode := dmAutomatic;
  end
  else begin
       if Assigned(FDropTarget)
       then FDropTarget.Active := False;
       FDropSource.DragMode := dmManual;
  end;
end; // TmcmThumbView.SetExternalDragDrop.


procedure TmcmThumbView.OnExDragDrop(Sender, Source : TObject;
                                     Shift          : TShiftState;
                                     X, Y           : Integer);
var i         : integer;
    FileNames : TStringList;
begin
  // Event fired by TmcmDropTarget.
  if (Source is TmcmDropEnum)
  then begin
       // The object has been dropped on the TmcmThumbView.
       Application.BringToFront; // Move this application window to the top.

       if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
       then begin
            FileNames := TStringList.Create;
            (Source as TmcmDropEnum).GetAsHDrop(FileNames);
            if (FileNames.Count > 0)
            then begin
                 for i := 0 to (FileNames.Count - 1)
                 do begin
                    if FSameDrive
                    then begin
                         if (ssCtrl in Shift) or Not(FAllowFileMove)
                         then CopyFile(FileNames.Strings[i], FDir, True, True)
                         else MoveFile(FileNames.Strings[i], FDir, True, True);
                    end
                    else begin
                         if (ssShift in Shift) and FAllowFileMove
                         then MoveFile(FileNames.Strings[i], FDir, True, True)
                         else CopyFile(FileNames.Strings[i], FDir, True, True);
                    end;
                 end;
            end;
            FileNames.Free;
       end;
  end;
end; // TmcmThumbView.OnExDragDrop.


procedure TmcmThumbView.OnExDragOver(    Sender, Source : TObject;
                                         Shift          : TShiftState;
                                         X, Y           : integer;
                                         State          : TDragState;
                                     var Accept         : boolean;
                                     var Copy           : boolean;
                                     var Move           : boolean);
var i         : integer;
    FileNames : TStringList;
    IDHandle  : THandle;
    ExDir     : string;
begin
  // Event fired by TmcmDropTarget.
  Accept := False;

  // If the object being dragged is a TmcmDropEnum class, we're receiving data
  // the Windows way, i.e. from another applications (Explorer, Word ect).
  if (Source is TmcmDropEnum)
  then begin
       if (State = dsDragEnter)
       then begin
            FAcceptDragObj := False;
            FSameDrive := False;

            // Cannot drag own sub-objects to ourself.
            if (Source as TmcmDropEnum).HasClipFormat(CF_MCMSOURCEID)
            then begin
                 (Source as TmcmDropEnum).GetAsIDHandle(IDHandle);
                 if (Handle = IDHandle)
                 then Exit; // Leave drop-over and reject.
            end;

            if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
            then begin
                 FAcceptDragObj := True;
                 FileNames := TStringList.Create;
                 (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                 if (FileNames.Count > 0)
                 then begin
                      FSameDrive := LowerCase(FDir[1]) = LowerCase(FileNames.Strings[0][1]);
                      
                      // Check source directory, and reject if this equals FDir.
                      ExDir := LowerCase(ExtractFileDir(FileNames.Strings[0]));
                      if (LowerCase(FDir) = ExDir) or (LowerCase(FDir) = ExDir + '\')
                      then FAcceptDragObj := False;

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

       if FAcceptDragObj // Do we have a valid object/file to drop ?
       then begin
            Accept := True;

            Copy := False;
            Move := False;
            // Simulate the behaviour of the "Windows Explorer" when the Shift
            // or Ctrl key is pressed, and whether if the file(s) are being
            // dropped on the drive drive (C:, D: etc.) they come from or...
            if FSameDrive
            then begin
                 if (ssCtrl in Shift) or Not(FAllowFileMove)
                 then Copy := True
                 else Move := True;
            end
            else begin
                 if (ssShift in Shift) and FAllowFileMove
                 then Move := True
                 else Copy := True;
            end;
       end;
  end;
end; // TmcmThumbView.OnExDragOver.


procedure TmcmThumbView.OnPreStartDrag(Sender : TObject; X, Y : Integer; var Component : TPersistent);
var i           : integer;
    DragControl : TControl;
    DragPos     : TPoint;
begin
  // This event marks the start of a Windows style drag operation.
  Component := Nil; // Marks nothing to drag.
  FDragList.Clear;

  // Check that we (x,y) hit a thumb.
  DragPos := ScreenToClient(Point(x, y));
  DragControl := ControlAtPos(DragPos, True);
  if (DragControl = Nil) or Not(DragControl is TmcmThumbImage)
  then Exit // We didn't hit a thumb, so cancel dragging.
  else begin
       // If the thumb hit isn't selected, add this to the selectionlist.
       if (FSelectedThumb.IndexOf(DragControl) = -1)
       then begin
            i := FThumbList.IndexOf(DragControl);
            if (i <> -1)
            then SetSelected(i);
       end;
  end;

  if FAllowMultiSel
  then begin
       if (FSelectedThumb.Count > 0)
       then begin
            for i := 0 to (FSelectedThumb.Count - 1)
            do FDragList.Add(TmcmThumbImage(FSelectedThumb.Items[i]).FFilename);
       end;
  end
  else begin
       if (0 <= FSelected)
       then FDragList.Add(TmcmThumbImage(FThumbList.Items[FSelected]).FFilename);
  end;
  if (FDragList.Count > 0)
  then Component := FDragList;
end; // TmcmThumbView.OnPreStartDrag.


procedure TmcmThumbView.OnEndWinDrag(Sender : TObject; Target : TObject; X, Y: Integer);
begin
  // Clear the drag list.
  FDragList.Clear;
end; // TmcmThumbView.OnEndWinDrag.


procedure TmcmThumbView.OnThumbStartDrag(Sender : TObject; var DragObject : TDragObject);
var i           : integer;
    DragControl : TControl;
    DragPos     : TPoint;
begin
  // Used when starting a Delphi Drag & Drop operation.

  if Assigned(FDragObject)
  then FDragObject.Free;
  FDragObject := Nil;

  GetCursorPos(DragPos);
  // Check that we (x,y) hit a thumb.
  DragPos := ScreenToClient(DragPos);
  DragControl := ControlAtPos(DragPos, True);
  if (DragControl = Nil) or Not(DragControl is TmcmThumbImage)
  then Exit // We didn't hit a thumb, so cancel dragging.
  else begin
       // If the thumb hit isn't selected, add this to the selectionlist.
       if (FSelectedThumb.IndexOf(DragControl) = -1)
       then begin
            i := FThumbList.IndexOf(DragControl);
            if (i <> -1)
            then SetSelected(i);
       end;
  end;

  if (Sender is TmcmThumbImage)
  then begin
       FDragObject := TmcmDragFileList.Create;
       if FAllowMultiSel
       then begin
            if (FSelectedThumb.Count > 0)
            then begin
                 for i := 0 to (FSelectedThumb.Count - 1)
                 do FDragObject.Filenames.Add(TmcmThumbImage(FSelectedThumb.Items[i]).FFilename);
            end;
       end
       else begin
            if (0 <= FSelected)
            then FDragObject.Filenames.Add(TmcmThumbImage(FThumbList.Items[FSelected]).FFilename);
       end;
       DragObject := FDragObject;
  end;
end; // TmcmThumbView.OnThumbStartDrag.


procedure TmcmThumbView.OnThumbEndDrag(Sender, Target : TObject; X, Y : integer);
begin
  // Used when ternimating a Delphi Drag & Drop operation.
  if Assigned(FDragObject)
  then FDragObject.Free;
  FDragObject := Nil;
end; // TmcmThumbView.OnThumbEndDrag.


//------------------------------------------------------------------------------
// TmcmDirectoryMonitor
//------------------------------------------------------------------------------


constructor TmcmDirectoryMonitor.Create(CreateSuspended : Boolean);
begin
  FPath        := '';
  FNotify      := INVALID_HANDLE_VALUE;
  FOnImageFile := Nil;
  FreeOnTerminate := True;
  Inherited Create(CreateSuspended);
end; // TmcmDirectoryMonitor.Create.


destructor TmcmDirectoryMonitor.Destroy;
begin
  Inherited Destroy;
end; // TmcmDirectoryMonitor.Destroy.


procedure TmcmDirectoryMonitor.Execute;
var Stat : longword;
begin
  while Not(Terminated)
  do begin
     if (FNotify = INVALID_HANDLE_VALUE)
     then Suspend
     else begin
          Stat := WaitForSingleObject(FNotify, 2000);
          if (Stat = WAIT_OBJECT_0)
          then begin
               OutputDebugString('A file was changed in the monitored directory:');
               OutputDebugString(PChar(FPath));
//               Sleep(50);
               Synchronize(DoImageFile);
               if Not(FindNextChangeNotification(FNotify))
               then begin
                    SetPath('');
                    SetPath(FPath);
               end;
          end
          else begin
               if (Stat = WAIT_TIMEOUT)
               then
               else ;
          end;
     end;
  end;

  // Close change handle.
  if (FNotify <> INVALID_HANDLE_VALUE)
  then FindCloseChangeNotification(FNotify);
end; // TmcmDirectoryMonitor.Execute.


procedure TmcmDirectoryMonitor.SetPath(Value : String);
var NotifyFilter : integer;
    SaveNotify   : TOnLoadImage;
begin
  if (FPath <> Value)
  then begin
       FPath := Value;
       SaveNotify := FOnImageFile;
       FOnImageFile := Nil;

       if (FNotify <> INVALID_HANDLE_VALUE) // Close our old handle
       then FindCloseChangeNotification(FNotify);
       FNotify := INVALID_HANDLE_VALUE;
       if {$IFDEF GE_DXE2}System.SysUtils.{$ENDIF}DirectoryExists(Value)
       then begin
            NotifyFilter := FILE_NOTIFY_CHANGE_FILE_NAME or
                            //FILE_NOTIFY_CHANGE_DIR_NAME or
                            //FILE_NOTIFY_CHANGE_ATTRIBUTES or
                            FILE_NOTIFY_CHANGE_SIZE or
                            FILE_NOTIFY_CHANGE_LAST_WRITE;

            FNotify := FindFirstChangeNotification(PChar(FPath), False, NotifyFilter);
            // Get handle to the event object.
            if (FNotify = INVALID_HANDLE_VALUE)
            then  // raise Exception('Couldn't make handle');
            else Resume;
            FOnImageFile := SaveNotify;
       end;
  end;
end; // TmcmDirectoryMonitor.SetPath.


procedure TmcmDirectoryMonitor.DoImageFile;
var FileName : string;
begin
  if Assigned(FOnImageFile) and (FNotify <> INVALID_HANDLE_VALUE)
  then begin
       FileName := '';
       FOnImageFile(Self, FileName, -1);
  end;
end; // TmcmDirectoryMonitor.DoImageFile.


{$IFDEF DCB3} {$UNDEF DCB3} {$ENDIF}
{$IFDEF DCB3_4} {$UNDEF DCB3_4} {$ENDIF}
{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

initialization
  IsMultiThread := True;
end.
