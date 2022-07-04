(*

  TMPHexEditorEx v 02-06-2006<br>

  @author((C) markus stephany, vcl[at]mirkes[dot]de, all rights reserved.)
  @abstract(TMPHexEditorEx, an enhanced TMPHexEditor: print and preview, ole drag and drop,
            ole clipboard handling, file backups...)
  @lastmod(02-06-2006)

  credits to :<br><br>
  - John Hamm, http://users.snapjax.com/john/<br><br>

  - Christophe Le Corfec for introducing the EBCDIC format and the nice idea about
    half byte insert/delete<br><br>

  - Philippe Chessa for his suggestions about AsText, AsHex and better support for
    the french keyboard layout<br><br>

  - Daniel Jensen for octal offset display and the INS-key recognition stuff<br><br>

  - Shmuel Zeigerman for introducing more flexible offset display formats<br><br>

  - Vaf, http://carradio.al.ru for reporting missing delver.inc and suggesting OnChange<br><br>

  - Eugene Tarasov for reporting that setting the BytesPerColumn value to 4 at design
    time didn't work<br><br>

  - FuseBurner for BytesPerUnit/RulerBytesPerUnit related suggestions<br><br>

  - Motzi for SyncView/ShowPositionIfNotFocused related suggestions<br><br>

  - Martin Hsiao for bcb compatibility and reporting some bugs when moving cursor beyond eof<br><br>

  - Miyu for delphi 7 defines<br><br>

  - Nils Hoyer for bcb testing and his help on creating a BCB6 package<br><br>

  - Skamnitsly S.V for reporting a bug when doubleclicking the ruler bar<br><br>

  - Pete Fraser for reporting problems with array properties under BCB<br><br>

  - Andrew Novikov for bug reports and suggestions<br><br>

  - Al for bug reports<br><br>

  - Dieter Köhler for reporting the delphi vcl related CanFocus bug<br><br>

  - Piotr Likus for reporting a cardinal&lt;-&gt;integer related bug in the Undo method<br><br>

  - Marc Girod for bug reports<br><br>

  - Gerd Schwartz for reporting a bug with printing headers/footers that contain long texts<br><br>

  - Bogdan Ureche for reporting an integer overflow when moving the cursor over a large selection<br><br>

  <h3>history:</h3>
  <p><ul>
  <li>v 02-06-2006: february 06, 2006<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 05-23-2005: may 23, 2005<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 12-29-2004: december 29, 2004<br><br>
         - initialized Result to '' in some string functions/methods to avoid
           non empty Result vars at function startup due to compiler
           optimizations (particularly on d4), e.g. printing did not work
           correctly under d4<br>
         - updated some of the sample projects (fixed the broken bcb6 sample,
           added printing to the hex viewer and the bcb6 editor sample) <br><br></li>

  <li>v 12-28-2004: december 28, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 12-21-2004: december 21, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor))<br>
         - support for CF_HTML clipboard format<br><br></li>

  <li>v 11-12-2004: november 12, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor))<br>
         - ole drag and drop move operation is now disabled if the editor's
           ReadOnlyView property is set to True<br><br></li>

  <li>v 10-26-2004: october 26, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor))/unit (@link(mphexeditor)) only <br><br></li>

  <li>v 08-29-2004: august 29, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor))<br>
         - added pfIncludeRuler to @link(TMPHPrintFlag)<br><br></li>

  <li>v 08-14-2004: august 14, 2004<br><br>
         - changed printing (color handling, pfSelectionBold meaning)<br><br></li>

  <li>v 06-15-2004: june 15, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) and some more inherited
           published properties <br><br></li>

  <li>v 06-10-2004: june 10, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 06-07-2004: june 07, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 05-27-2004: may 27, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 05-13-2004: may 13, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 04-18-2004: april 18, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 01-08-2004: january 08, 2004<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 12-16-2003: december 16, 2003<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 12-10-2003: december 10, 2003<br><br>
         - changes in the base class (@link(TCustomMPHexEditor)) only <br><br></li>

  <li>v 09-24-2003: september 24, 2003 <br><br>
         - modified the BCB6 package <br><br></li>

  <li>v 09-09-2003: september 09, 2003<br><br>
         - changed @link(UndoBeginUpdate) and @link(UndoEndUpdate) behaviour to automatically create an undo record
           on UndoBeginUpdate and check it on UndoEndUpdate, see also @link(CreateUndoOnUndoUpdate)<br>
         - added property @link(CreateUndoOnUndoUpdate) <br>
         - added defines for delphi7, renamed delver.inc to mpdelver.inc <br>
         - @link(PasteData) method added <br><br></li>

  <li>v 07-05-2003: july 05, 2003<br><br>
         - added support for pasting clipboard data in fixed filesize mode<br>
         - added RegEdit_HexData clipboard support<br><br></li>

  <li>v 05-25-2003-b: may 25, 2003<br><br>
         - fixed a bug (moving the cursor beyond eof)<br><br></li>

  <li>v 05-25-2003: may 25, 2003<br><br>
         - no ':' is printed when offset display is not used<br>
         - added hpp generating statements for bcb compatibility<br><br></li>

  <li>v 05-20-2003: may 20, 2003<br><br>
         - added unicode support in printing<br><br></li>

  <li>v 05-17-2003: may 17, 2003<br><br>
         - moved some property related functions to protected<br>
         - corrected bottom margin handling when printing<br>
         - corrected upper/lowercase hex chars in printing<br>
         - the current unit is selected now when doubleclicking data<br>
         - added flags pfCurrentViewOnly (just print the currently
           visible data) to @link(PrintOptions).Flags<br><br></li>

  <li>v 08-18-2002: august 18, 2002<br><br>
         - first release</li>
  </ul></p>

*)

{$IFDEF BCB}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTarget)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropSource)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumFORMATETC)'}
{$ENDIF}

unit MPHexEditorEx;

{$I MPDELVER.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  MPHexEditor, ActiveX, Graphics, Printers,
  ShlObj, Menus;

type
  //@exclude
  // is data dropped or pasted
  TMPHOLEOperation = (oleDrop, oleClipboard);

  // @exclude(available clipboard / IDataObject formats)
  TClipFormats = array of TClipFormat;

  // @exclude(ole drop handler class)
  TMPHDropTarget = class;

  // @exclude(persistent print options)
  TMPHPrintOptions = class;

  (* print option flags:<br><br>
    - pfSelectionOnly: only print data currently selected<br>
    - pfSelectionBold: render the current selection using either a bold font or inverted colors (if pfSelectionOnly isn't set)<br>
    - pfMonochrome: don't use colors, print/preview black on white<br>
    - pfUseBackgroundColor: fill the margin rect with the editor's background color (if pfMonochrome isn't set)<br>
    - pfCurrentViewOnly: just print the data currently displayed<br>
    - pfIncludeRuler: draw the ruler at every page's top<br>
  *)
  TMPHPrintFlag = (pfSelectionOnly, pfSelectionBold, pfMonochrome,
    pfUseBackgroundColor, pfCurrentViewOnly, pfIncludeRuler);
  // @exclude()
  TMPHPrintFlags = set of TMPHPrintFlag;

  // @exclude(print header/footer)
  TMPHPrintHeaders = array[0..1] of string;

  (* this event is called when @link(PropertiesAsString) is read or written. TMPHexEditorEx
    has a fixed list of properties that can be read/written using PropertiesAsString.
    you can exclude some of the properties by setting IsPublic to False.
  *)

  TMPHQueryPublicPropertyEvent = procedure(Sender: TObject; const PropertyName:
    string;
    var IsPublic: boolean) of object;

  // enhanced hex editor
  TMPHexEditorEx = class(TCustomMPHexEditor)
  private
    { Private-Deklarationen }
    FCreateBackups: boolean;
    FBackupFileExt: string;
    FOleDragDrop: boolean;
    FDropTarget: TMPHDropTarget;
    FOleFormat: array[TMPHOLEOperation] of TClipFormat;
    FOleDragging, FOleStartDrag: boolean;
    FOleDragX, FOleDragY: integer;
    FOleWasTarget: boolean;
    FPrintOptions: TMPHPrintOptions;

    FPrintPages: integer;

    FPrintFont: TFont;
    FUseEditorFontForPrinting: boolean;
    FClipboardAsHexText: boolean;
    FClipData: IDataObject;
    FFlushClipboardAtShutDown: boolean;
    FSupportsOtherClipFormats: boolean;
    FOffsetPopupMenu: TPopupMenu;
    FZoomOnWheel: boolean;
    FPaintUpdateCounter: integer;
    FOnQueryPublicProperty: TMPHQueryPublicPropertyEvent;
    FHasDoubleClicked: boolean;
    FBookmarksNoChange: boolean;
    FCreateUndoOnUndoUpdate: boolean;
    FModifiedNoUndo: boolean;
    procedure SetOleDragDrop(const Value: boolean);
    function OLEHasSupportedFormat(const dataObj: IDataObject;
      const Formats: array of TClipFormat; var Format: TClipFormat): boolean;
    function GetMyOLEFormats: TClipFormats;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure SetPrintOptions(const Value: TMPHPrintOptions);

    function PrintToCanvas(ACanvas: TCanvas; const APage: integer;
      const AMargins: TRect): integer;
    function PrinterMarginRect: TRect;

    procedure SetPrintFont(const Value: TFont);
    procedure SetOffsetPopupMenu(const Value: TPopupMenu);
    function GetOffsetPopupMenu: TPopupMenu;
    function GetBookmarksAsString: string;
    procedure SetBookMarksAsString(Value: string);
  protected
    { Protected-Deklarationen }
    function CanCreateUndo(const aKind: TMPHUndoFlag; const aCount, aReplCount:
      integer): Boolean; override;
{$IFDEF DELPHI6UP}
    // @exclude()
    function GetPropertiesAsString: string; virtual;
    // @exclude()
    procedure SetPropertiesAsString(const Value: string); virtual;
    // @exclude()

    function IsPropPublic(PropName: string): boolean; virtual;

{$ENDIF}
    // @exclude(check if in offset col, if yes, popup offsetcontextmenu)
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
{$IFDEF DELPHI6UP}
    // @exclude()
    procedure DoContextPopup(MousePos: TPoint; var Handled: boolean); override;
{$ENDIF}
    // @exclude(parse control keys)
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(create backups in savefile)
    procedure PrepareOverwriteDiskFile; override;
    // @exclude(overwrite mouse handling for ole drag and drop)
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    // @exclude(overwrite mouse handling for ole drag and drop)
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    // @exclude(overwrite mouse handling for ole drag and drop)
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer);
      override;
    // @exclude(reset drop target's HWND)
    procedure CreateWnd; override;
    // @exclude(supported dnd/clipboard data available?)
    function SupportsOLEData(const dataObj: IDataObject; const grfKeyState:
      longint; const pt: TPoint; var dwEffect: longint; const Operation:
      TMPHOLEOperation): HRESULT;
    // @exclude(insert ole-dropped data)
    function InsertOLEData(const dataObj: IDataObject; const grfKeyState:
      longint; const pt: TPoint; var dwEffect: longint; const Operation:
      TMPHOLEOperation): HRESULT;
    // @exclude(modify drageffect depending on key states and data format)
    function ModifyOLEDropEffect(const grfKeyState: longint; const pt: TPoint;
      var dwEffect: longint): HRESULT;
    // @exclude(paint handler)
    procedure Paint; override;
    // @exclude(doubleclick handler for unit selection)
    procedure DblClick; override;
    // @exclude(override to avoid much updates when using setbookmarksasstring);
    procedure BookmarkChanged; override;
  public
    { Public-Deklarationen }
    // @exclude(Init)
    constructor Create(AOwner: TComponent); override;
    // @exclude(Done)
    destructor Destroy; override;
    // see inherited @inherited
    procedure WriteBuffer(const Buffer; const Index, Count: Integer); override;
    (* if set to True (default is False), an undo record is automatically created on calling
      @link(UndoBeginUpdate) and on calling @link(UndoEndUpdate) the record is deleted if the
      data has not been changed between UndoBegin- and UndoEndUpdate *)
    property CreateUndoOnUndoUpdate: boolean read FCreateUndoOnUndoUpdate write
      FCreateUndoOnUndoUpdate;
    (* each call to BeginUpdate increments an internal counter that prevents from repainting
       (see also @link(EndUpdate))
    *)
    function BeginUpdate: integer;
    (* each call to EndUpdate decrements an internal counter that prevents from repainting.
       the return value is the value of this counter. if the counter is reset to zero,
       repainting is permitted again (see also @link(BeginUpdate))
    *)
    function EndUpdate: integer;
    (* each call to UndoBeginUpdate increments an internal counter that prevents using
       undo storage and also disables undo functionality (see also @link(UndoEndUpdate))
    *)
    function UndoBeginUpdate(const StrUndoDesc: string = ''): integer;
      reintroduce;
    (* each call to UndoEndUpdate decrements an internal counter that prevents using
       undo storage and also disables undo functionality. the return value is the value
       of this counter. if the counter is reset to zero, undo creation is permitted again
       (see also @link(UndoBeginUpdate))
    *)
    function UndoEndUpdate: integer; override;
    // create an undo for a range of bytes
    procedure CreateRangeUndo(const aStart, aCount: integer; sDesc: string);
    // is pasting from clipboard possible?
    function CanPaste: boolean;
    // is copying to clipboard possible?
    function CanCopy: boolean;
    // is cutting to clipboard possible?
    function CanCut: boolean;
    // copy selection to clipboard
    function CBCopy: boolean;
    // cut selection to clipboard
    function CBCut: boolean;
    // paste clipboard's contents over current selection
    function CBPaste: boolean;
    // do we own the clipboard data?
    function OwnsClipBoard: boolean;
    // flush or empty the clipboard (if we own the IDataObject)
    procedure ReleaseClipboard(const Flush: boolean);
    // save to file (overwrite)
    procedure Save;
    // @exclude(dump undo storage)
    function DumpUndoStorage(const FileName: string): boolean;
    (* creates a TMetaFile object and renders the specified page
       on its canvas. Freeing of the TMetaFile is up to the caller!
    *)

    function PrintPreview(const Page: integer): TMetaFile;
    (* print the given page to the default printer.
      Printer.BeginDoc, Printer.NewPage and Printer.EndDoc must be issued by the caller!
    *)
    procedure Print(const Page: integer);
    // get the number of pages to print
    function PrintNumPages: integer;

    // paste data (in clipboardmanner: check current selection and so on)
    procedure PasteData(P: Pointer; const ACount: integer; const UndoDesc: string
      = '');
    // get/set bookmarks as text (for storing in registry, ini-file)
    property BookMarksAsString: string read GetBookmarksAsString write
      SetBookMarksAsString;
{$IFDEF DELPHI6UP}
    // get set properties as text (for storing in registry, ini-file);
    property PropertiesAsString: string read GetPropertiesAsString write
      SetPropertiesAsString;
{$ENDIF}
  published
    { Published-Deklarationen }
    // create a backup on save ? (see also @link(BackupExtension))
    property CreateBackup: boolean read FCreateBackups write FCreateBackups
      default True;
    // add this extension to the file if making backups, see @link(CreateBackup)
    property BackupExtension: string read FBackupFileExt write FBackupFileExt;
    (* if set To True, OLE drag and drop will used automatically when dragging starts
       or supported OLE data has been dropped on the hex editor
    *)
    property OleDragDrop: boolean read FOleDragDrop write SetOleDragDrop default
      False;
    // if set to True, CF_TEXT on the clipboard will be treated as hex formatted text
    property ClipboardAsHexText: boolean read FClipboardAsHexText write
      FClipboardAsHexText default False;
    // flush or empty clipboard at shutdown
    property FlushClipboardAtShutDown: boolean read FFlushClipboardAtShutDown
      write FFlushClipboardAtShutDown default False;
    // do we support other formats than CF_MPHEXEDITOR and CF_HDROP?
    property SupportsOtherClipFormats: boolean read FSupportsOtherClipFormats
      write FSupportsOtherClipFormats default True;
    // print/preview options, see @link(TMPHPrintOptions)
    property PrintOptions: TMPHPrintOptions read FPrintOptions write
      SetPrintOptions;
    // print using this font
    property PrintFont: TFont read FPrintFont write SetPrintFont;
    // if set to True, the editor's font will be used for printing
    property UseEditorFontForPrinting: boolean read FUseEditorFontForPrinting
      write FUseEditorFontForPrinting default True;
    (* if this property is assigned to a TPopupMenu, it will be shown on right clicking
      the offset display pane. then the normal PopupMenu will open on right
      clicking the character and hex pane.
    *)
    property OffsetPopupMenu: TPopupMenu read GetOffsetPopupMenu write
      SetOffsetPopupMenu;
    // auto-zoom on mouse wheel?
    property ZoomOnWheel: boolean read FZoomOnWheel write FZoomOnWheel default
      True;
    (* this event is called when @link(PropertiesAsString) is read or written.
       (see @link(TMPHQueryPublicPropertyEvent))
    *)
    property OnQueryPublicProperty: TMPHQueryPublicPropertyEvent read
      FOnQueryPublicProperty write FOnQueryPublicProperty;
    // @exclude(inherited)
    property Align;
    // @exclude(inherited)
    property Anchors;
    // @exclude(inherited)
    property BiDiMode;
    // @exclude(inherited)
    property BorderStyle;
    // @exclude(inherited)
    property Constraints;
    // @exclude(inherited)
    property Ctl3D;
    // @exclude(inherited)
    property DragCursor;
    // @exclude(inherited)
    property DragKind;
    // @exclude(inherited)
    property DragMode;
    // @exclude(inherited)
    property Enabled;
    // @exclude(inherited)
    property Font;
    // @exclude(inherited)
    property ImeMode;
    // @exclude(inherited)
    property ImeName;
    // @exclude(inherited)
    property OnClick;
    // @exclude(inherited)
    property OnDblClick;
    // @exclude(inherited)
    property OnDragDrop;
    // @exclude(inherited)
    property OnDragOver;
    // @exclude(inherited)
    property OnEndDock;
    // @exclude(inherited)
    property OnEndDrag;
    // @exclude(inherited)
    property OnEnter;
    // @exclude(inherited)
    property OnExit;
    // @exclude(inherited)
    property OnKeyDown;
    // @exclude(inherited)
    property OnKeyPress;
    // @exclude(inherited)
    property OnKeyUp;
    // @exclude(inherited)
    property OnMouseDown;
    // @exclude(inherited)
    property OnMouseMove;
    // @exclude(inherited)
    property OnMouseUp;
    // @exclude(inherited)
    property OnMouseWheel;
    // @exclude(inherited)
    property OnMouseWheelDown;
    // @exclude(inherited)
    property OnMouseWheelUp;
    // @exclude(inherited)
    property OnStartDock;
    // @exclude(inherited)
    property OnStartDrag;
    // @exclude(inherited)
    property ParentBiDiMode;
    // @exclude(inherited)
    property ParentCtl3D;
    // @exclude(inherited)
    property ParentFont;
    // @exclude(inherited)
    property ParentShowHint;
    // @exclude(inherited)
    property PopupMenu;
    // @exclude(inherited)
    property ScrollBars;
    // @exclude(inherited)
    property ShowHint;
    // @exclude(inherited)
    property TabOrder;
    // @exclude(inherited)
    property TabStop;
    // @exclude(inherited)
    property Visible;

    // see inherited @inherited
    property BytesPerRow;
    // see inherited @inherited
    property BytesPerColumn;
    // see inherited @inherited
    property Translation;
    // see inherited @inherited
    property OffsetFormat;
    // see inherited @inherited
    property CaretKind;
    // see inherited @inherited
    property Colors;
    // see inherited @inherited
    property FocusFrame;
    // see inherited @inherited
    property SwapNibbles;
    // see inherited @inherited
    property MaskChar;
    // see inherited @inherited
    property NoSizeChange;
    // see inherited @inherited
    property AllowInsertMode;
    // see inherited @inherited
    property DrawGridLines;
    // see inherited @inherited
    property WantTabs;
    // see inherited @inherited
    property ReadOnlyView;
    // see inherited @inherited
    property HideSelection;
    // see inherited @inherited
    property GraySelectionIfNotFocused;
    // see inherited @inherited
    property GutterWidth;
    // see inherited @inherited
    property BookmarkBitmap;

    // see inherited @inherited
    property Version;

    // see inherited @inherited
    property MaxUndo;
    // see inherited @inherited
    property InsertMode;
    // see inherited @inherited
    property HexLowerCase;
    // see inherited @inherited
    property OnProgress;
    // see inherited @inherited
    property OnInvalidKey;
    // see inherited @inherited
    property OnTopLeftChanged;
    // see inherited @inherited
    property OnChange;
    // see inherited @inherited
    property DrawGutter3D;
    // see inherited @inherited
    property ShowRuler;
    // see inherited @inherited
    property BytesPerUnit;
    // see inherited @inherited
    property RulerBytesPerUnit;
    // see inherited @inherited
    property ShowPositionIfNotFocused;
    // see inherited @inherited
    property OnSelectionChanged;
    // see inherited @inherited
    property UnicodeChars;
    // see inherited @inherited
    property UnicodeBigEndian;

    // see inherited @inherited
    property OnDrawCell;

    // see inherited @inherited
    property OnBookmarkChanged;
    // see inherited @inherited
    property OnGetOffsetText;
    // see inherited @inherited
    property BytesPerBlock;
    // see inherited @inherited
    property SeparateBlocksInCharField;
    // see inherited @inherited
    property FindProgress;
    // see inherited @inherited
    property RulerNumberBase;
  end;

  // @exclude(ole drop target class)
  TMPHDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FEditor: TMPHexEditorEx;
    FEditorHandle: THandle;
    FActive: boolean;
    procedure SetActive(const Value: boolean);
  public
    constructor Create(Editor: TMPHexEditorEx);
    procedure BeforeDestruction; override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: longint; pt:
      TPoint; var dwEffect: longint): HResult; stdcall;
    function DragOver(grfKeyState: longint; pt: TPoint; var dwEffect: longint):
      HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: longint; pt: TPoint;
      var dwEffect: longint): HResult; stdcall;
    property Active: boolean read FActive write SetActive;
  end;

  // print / preview options
  TMPHPrintOptions = class(TPersistent)
  private
    FMargins: TRect;
    FHeaders: TMPHPrintHeaders;
    FFlags: TMPHPrintFlags;
    function GetHeader(const Index: integer): string;
    function GetMargin(const Index: integer): integer;
    procedure SetHeader(const Index: integer; const Value: string);
    procedure SetMargin(const Index, Value: integer);
  public
    // @exclude(Init)
    constructor Create;
    // @exclude()
    procedure Assign(Source: TPersistent); override;
  published
    // left margin in Millimeters
    property MarginLeft: integer index 1 read GetMargin write SetMargin;
    // top margin in Millimeters
    property MarginTop: integer index 2 read GetMargin write SetMargin;
    // right margin in Millimeters
    property MarginRight: integer index 3 read GetMargin write SetMargin;
    // bottom margin in Millimeters
    property MarginBottom: integer index 4 read GetMargin write SetMargin;
    (* this line will be rendered on top of the printed page, some characters have special meanings:<br><br>
       - the string may contain three parts separated by a "|" (pipe) character (left|center|right)<br>
       - each part knows some special variables:
       <ul>
       <li><b>%f</b>: substituted with the filename part of the editor's filename</li>
       <li><b>%F</b>: substituted with the expanded name of the editor's filename</li>
       <li><b>%p</b>: substituted with the number of the current page</li>
       <li><b>%P</b>: substituted with the number of pages</li>
       <li><b>%t</b>: substituted with the current time</li>
       <li><b>%d</b>: substituted with the current date</li>
       <li><b>%&gt;</b>: substituted with the long description of the editor's current @link(Translation)</li>
       <li><b>%&lt;</b>: substituted with the short description of the editor's current @link(Translation)</li>
       </ul>
    *)
    property PageHeader: string index 0 read GetHeader write SetHeader;
    // this line will be rendered on the bottom of the printed page (see @link(PageHeader))
    property PageFooter: string index 1 read GetHeader write SetHeader;
    (* printing flags:<br><br>
      - pfSelectionOnly: only print data currently selected<br>
      - pfSelectionBold: render the current selection using either a bold font or inverted colors (if pfSelectionOnly isn't set)<br>
      - pfMonochrome: don't use colors, print/preview black on white<br>
      - pfUseBackgroundColor: fill the margin rect with the editor's background color (if pfMonochrome isn't set)<br>
      - pfCurrentViewOnly: just print the data currently displayed
    *)
    property Flags: TMPHPrintFlags read FFlags write FFlags;
  end;

  // default print margins
const
  MPH_DEF_PRINT_MARGINS: TRect = (Left: 20; Top: 15; Right: 25; Bottom: 25);

implementation

uses
  Consts, StdCtrls, ShellAPI, ComObj, TypInfo;

resourcestring

  // error messages
  ERR_NOFILE = 'No filename specified';
  ERR_INVALID_PAGE = 'Invalid page index';
  ERR_PRINTING_FAILED = 'Printing failed';
  ERR_BACKUP_DELETE = 'Cannot delete previous backup %s. (%s)';
  ERR_BACKUP_CREATE = 'Cannot create backup %s. (%s)';
  ERR_INVALID_BOOKFMT = 'Invalid bookmark format';

  // additional undo descriptions
  UNDO_PASTECB = 'Paste from clipboard';
  UNDO_CUTCB = 'Cut to clipboard';
  UNDO_DROPPED = 'Data dropped';
  UNDO_MOVED = 'Data moved';

  // select clipb/ole format dialog strings
  SELECT_FORMAT_CAPTION = 'Select data format';
  SELECT_FORMAT_ASHEX = 'Hex text';

  // when data dropped to explorer, give it this filename; first %s filename w/o ext, (second %s original file ext)
  STR_SCRAPFILE = 'Dump of %s.bin';

  // native clipboard format name
  MPTH_CF = 'TMPHexeditorEx Clipboard Format';

  // predefined clipboard format names
  STR_CF_TEXT = 'Text';
  STR_CF_BITMAP = 'Bitmap Picture';
  STR_CF_METAFILEPICT = 'Metafile Picture';
  STR_CF_SYLK = 'Microsoft Symbolic Link (SYLK) data';
  STR_CF_DIF = 'Software Arts'' Data Interchange Format';
  STR_CF_TIFF = 'Tagged Image File Format (TIFF) Picture';
  STR_CF_OEMTEXT = 'OEM Text';
  STR_CF_DIB = 'Device Independent Bitmap Picture';
  STR_CF_PALETTE = 'Color Palete';
  STR_CF_PENDATA = 'Pen Data';
  STR_CF_RIFF = 'RIFF Audio Data';
  STR_CF_WAVE = 'Wave Audio';
  STR_CF_UNICODETEXT = 'Unicode Text';
  STR_CF_ENHMETAFILE = 'Enhanced Metafile Picture';
  STR_CF_HDROP = 'File List';
  STR_CF_LOCALE = 'Text Locale';

type
  // my clipboard data struct
  PClipData = ^TClipData;
  TClipData = packed record
    Signature: DWORD;
    Version: DWORD;
    Size: integer;
    Data: array[0..0] of char;
  end;

  PRegEditHexData = ^TRegEditHexData;
  TRegEditHexData = packed record
    Size: integer;
    Data: array[0..0] of char;
  end;

const
  // signature of own format clipboard data
  CLIP_SIG = $4854504D; // MPTH;
  // version of own format clipboard data
  CLIP_VER = $00010001;

  // initial file extension of backups
  BACKUP_EXT = '.bak';

  // not so predefined common/known clipboard format names
  CFSTR_RTF = 'Rich Text Format';
  CFSTR_LOGICALPERFORMEDDROPEFFECT = 'Logical Performed DropEffect';
  CFSTR_REGEDIT_HEXDATA = 'RegEdit_HexData';
  CFSTR_HTML = 'HTML Format';

var
  // custom/ shell CF format
  CF_MPHEXEDITOR,
    CF_RTF,
    CF_FILECONTENTS,
    CF_PERFORMEDDROPEFFECT,
    CF_LOGICALPERFORMEDDROPEFFECT,
    CF_FILEDESCRIPTOR,
    CF_HTML,
    CF_REGEDIT_HEXDATA: TClipFormat;

type
  // private idataobject format enumerator
  TFormatEnum = class
  private
    FFormats: array of TFormatETC;
  public
    constructor Create(const dataObject: IDataObject);
    destructor Destroy; override;
    function HasFormat(const cfFormat: TClipFormat): boolean;
    function GetFormatETC(const cfFormat: TClipFormat): TFormatETC;
  end;

const
  // number of clip formats that we can provide
  MY_SUPPORTED_FORMATS = 4;

type
  // ole "public" format enumerator for own data
  TMPHEnumFormatETC = class(TInterfacedObject, IEnumFormatETC)
  private
    FFormats: packed array[0..MY_SUPPORTED_FORMATS - 1] of TFormatETC;
    FIndex: integer;
  public
    constructor Create;
    function Next(celt: longint; out elt; pceltFetched: PLongint): HResult;
      stdcall;
    function Skip(celt: longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  end;

  // ole drop source
  TMPHDropSource = class(TInterfacedObject, IDropSource)
  public
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: longint):
      HResult; stdcall;
    function GiveFeedback(dwEffect: longint): HResult; stdcall;
  end;

  // ole data container
  TMPHDataObject = class(TInterfacedObject, IDataObject)
  private
    FData: Pointer;
    FDataSize: integer;
    FFileName: ShortString;
    FHasDropEffect: boolean;
    FDropEffect: cardinal;
    FTextAsHex: boolean;
    FSwapNibbles: boolean;
  public
    constructor Create(Data: Pointer; DataSize: integer; ScrapFileName:
      ShortString; TextAsHex, SwapNibbles: boolean);
    constructor CreateFromStream(Stream: TStream; Position, DataSize: integer;
      ScrapFileName: ShortString; TextAsHex, SwapNibbles: boolean);
    procedure BeforeDestruction; override;
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out
      formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: longint; const advSink:
      IAdviseSink; out dwConnection: longint): HResult; stdcall;
    function DUnadvise(dwConnection: longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

  // draw hex on canvas
  TMPHCanvasPrinter = class(TObject)
  private
    FMargins: TRect;
    FHeaders,
      FPrintHeaders: TMPHPrintHeaders;
    FLinesPerPage: integer;
    FFlags: TMPHPrintFlags;
    FPages: integer;
    FEditor: TMPHexEditorEx;
    FCanvas: TCanvas;
    function GetLinesPerPage: integer;
    function BuildHeader(const S: string; const Page: integer): string;
  protected
    function DrawOrCalc(const JustCalc: boolean; const Page: integer): integer;
  public
    constructor Create(AEditor: TMPHexEditorEx; ACanvas: TCanvas; AFlags:
      TMPHPrintFlags; AMargins: TRect; AHeaders: TMPHPrintHeaders);
    procedure Draw(const Page: integer);
    property LinesPerPage: integer read GetLinesPerPage;
    property Pages: integer read FPages;
  end;

var
  // most recent selected clip format
  LAST_USED_CF: integer = -1;

  // returns the stgmedium struct for a given idataobject/format specification

function GetIDataObjectData(const dataObj: IDataObject; const Format:
  TClipFormat; out Medium: TStgMedium): HRESULT;
var
  LobjEnum: TFormatEnum;
begin
  LobjEnum := TFormatEnum.Create(dataObj);
  try
    if not LobjEnum.HasFormat(Format) then
      Result := E_FAIL
    else
      Result := dataObj.GetData(LobjEnum.GetFormatETC(Format), Medium);
  finally
    LobjEnum.Free;
  end;
end;

// cast/copy hglobal to data structure depending on the format

function GetSomeData(const PData: Pointer; const HGlobal: THandle; Format:
  TClipFormat; const DataSize: integer; const UnicodeBigEndian: Boolean):
  string;
var
  LWStrTemp: widestring;
  LRecBmpHeader: TBitmapFileheader;
  LRecPalette: TMaxLogPalette;
  LIntTemp: integer;
  LbmpTemp: TBitmap;
  LmefTemp: TMetaFile;
  LmstData: TMemoryStream;
  LIntLoop: integer;
begin
  Result := '';

  // to use case..of (cf_rtf is not a constant)
  if (Format = CF_RTF) or (Format = CF_HTML) then
    Format := CF_TEXT;

  if Format = CF_MPHEXEDITOR then
  begin
    with PClipData(PData)^ do
      if (Signature = CLIP_SIG) and (Version = CLIP_VER) then
        SetString(Result, Data, Size)
  end
  else if Format = CF_REGEDIT_HEXDATA then
  begin
    with PRegEditHexData(PData)^ do
      SetString(Result, Data, Size);
  end
  else
    case Format of
      CF_TEXT,
        CF_OEMTEXT: Result := PChar(PData);
      CF_UNICODETEXT:
        begin
          LWStrTemp := PWideChar(PData);
          if UnicodeBigEndian then
          begin
            for LIntLoop := 1 to Length(LWstrTemp) do
              SwapWideChar(LWstrTemp[LIntLoop]);
          end;
{$WARNINGS OFF}
          // don't convert, get wide data as is
          SetString(Result, PChar(LWStrTemp), Length(LWStrTemp) *
            (sizeof(widechar) div sizeof(char)));
{$WARNINGS ON}
        end;
      CF_LOCALE:
        begin
          // locale id , word pointed to by the global handle
          SetLength(Result, sizeof(word));
          Move(PWord(PData)^, Result[1], sizeof(word));
        end;
      CF_DIB:
        begin
          // stored as bitmap without header, so prefix a bmp header
          FillChar(LRecBMPHeader, sizeof(LRecBMPHeader), #0);
          LRecBMPHeader.bfType := $4D42; // BM
          SetLength(Result, sizeof(LRecBMPHeader) + DataSize);
          Move(LRecBMPHeader, Result[1], sizeof(LRecBmpHeader));
          Move(PData^, Result[1 + sizeof(LRecBMPHeader)], DataSize);
        end;
      CF_PALETTE:
        begin
          // copy palette entries
          LIntTemp := 0;
          if (GetObject(HGlobal, sizeof(LIntTemp), @LIntTemp) <> 0) and (LIntTemp
            > 0) then
          begin
            with LRecPalette do
            begin
              palVersion := $0300;
              palNumEntries := LIntTemp;
              GetPaletteEntries(HGlobal, 0, LIntTemp, palPalEntry);
            end;
            SetLength(Result, sizeof(TLogPalette) + ((LintTemp - 1) *
              sizeof(TPaletteEntry)));
            Move(LRecPalette, Result[1], Length(Result));
          end;
        end;
      CF_BITMAP:
        begin
          // data not stored in global mem, but as a bitmap handle
          LbmpTemp := TBitmap.Create;
          try
            LbmpTemp.Handle := CopyImage(HGlobal, IMAGE_BITMAP, 0, 0,
              LR_COPYRETURNORG);
            LmstData := TMemoryStream.Create;
            try
              LbmpTemp.SaveToStream(LmstData);
              SetString(Result, PChar(LmstData.Memory), LmstData.Size);
            finally
              LmstData.Free;
            end;
          finally
            LbmpTemp.Free;
          end;
        end;
      CF_METAFILEPICT:
        begin
          // global mem contains mf struct
          LIntTemp := GetMetaFileBitsEx(PMetafilePict(PData)^.hMF, 0, nil);
          if LIntTemp > 0 then
          begin
            SetLength(Result, LIntTemp);
            GetMetaFileBitsEx(PMetafilePict(PData)^.hMF, LIntTemp, @Result[1]);
          end;
        end;
      CF_ENHMETAFILE:
        begin
          // emf handle
          LmefTemp := TMetaFile.Create;
          try
            LmefTemp.Handle := CopyEnhMetafile(HGlobal, nil);
            LmstData := TMemoryStream.Create;
            try
              LmefTemp.SaveToStream(LmstData);
              SetString(Result, PChar(LmstData.Memory), LmstData.Size);
            finally
              LmstData.Free;
            end;
          finally
            LmefTemp.Free;
          end;
        end;
    else
      // format not yet known
      SetString(Result, PChar(PData), DataSize);
    end;
end;

type
  // special dialog for format selection
  TFormatSelDialog = class(TForm)
  private
    LbtnOK: TButton;
    LbtnCancel: TButton;
    LlbxFormats: TListBox;
    LcbxTextAsHex: TCheckBox;
    procedure ListDoubleClick(Sender: TObject);
    procedure ListSelect(Sender: TObject);
  end;

  // select a format out of an array of available formats

function SelectClipFormat(const Formats: array of TClipFormat; var Format:
  TClipFormat; var TextIsHexData: boolean): boolean;
var
  LfrmDialog: TFormatSelDialog;
  LIntLoop: integer;
  LWrdCurrent: TClipFormat;
  LStrFormatName: string;
  LszBuffer: array[0..511] of char;
begin
  Result := False;

  // create and show a dialog for clipboard format selection
  LfrmDialog := TFormatSelDialog.CreateNew(Application);
  with lfrmDialog do
  try
    BorderStyle := bsDialog;
    Width := Screen.Width div 4;
    Height := Screen.Height div 4;
{$IFDEF DELPHI6UP}
    Position := poOwnerFormCenter;
{$ELSE}
    Position := poScreenCenter;
{$ENDIF}
    Caption := SELECT_FORMAT_CAPTION;

    LbtnOK := TButton.Create(LfrmDialog);
    LbtnCancel := TButton.Create(LfrmDialog);
    LcbxTextAsHex := TCheckBox.Create(LfrmDialog);
    LlbxFormats := TListBox.Create(LfrmDialog);
    try
      with lbtnOK do
      begin
        Parent := LfrmDialog;
        ModalResult := mrOk;
        Caption := SOKButton;
        Default := True;
        Width := (LfrmDialog.Width div 2) - 32;
        Top := LfrmDialog.ClientHeight - Height - 8;
        Left := 16;
        Enabled := False;
      end;

      with LbtnCancel do
      begin
        Parent := LfrmDialog;
        ModalResult := mrCancel;
        Cancel := True;
        Caption := SCancelButton;
        Width := (LfrmDialog.Width div 2) - 32;
        Top := LfrmDialog.ClientHeight - Height - 8;
        Left := LfrmDialog.ClientWidth - Width - 16;
      end;

      with LcbxTextAsHex do
      begin
        Parent := LfrmDialog;
        Enabled := False;
        Caption := SELECT_FORMAT_ASHEX;
        Top := LbtnCancel.Top - Height - 8;
        Left := LbtnOK.Left;
        Width := LfrmDialog.ClientWidth - Left;
        Checked := TextIsHexData;
      end;

      with LlbxFormats do
      begin
        Parent := LfrmDialog;
        Align := alTop;
        Height := LfrmDialog.ClientHeight - 16 - LbtnCancel.Height - 8 -
          LcbxTextAsHex.Height;
        OnDblClick := ListDoubleClick;
        OnClick := ListSelect;

        for LIntLoop := Low(Formats) to High(Formats) do
        begin
          LWrdCurrent := Formats[LIntLoop];
          case LWrdCurrent of
            CF_TEXT: LStrFormatName := STR_CF_TEXT;
            CF_BITMAP: LStrFormatName := STR_CF_BITMAP;
            CF_METAFILEPICT: LStrFormatName := STR_CF_METAFILEPICT;
            CF_SYLK: LStrFormatName := STR_CF_SYLK;
            CF_DIF: LStrFormatName := STR_CF_DIF;
            CF_TIFF: LStrFormatName := STR_CF_TIFF;
            CF_OEMTEXT: LStrFormatName := STR_CF_OEMTEXT;
            CF_DIB: LStrFormatName := STR_CF_DIB;
            CF_PALETTE: LStrFormatName := STR_CF_PALETTE;
            CF_PENDATA: LStrFormatName := STR_CF_PENDATA;
            CF_RIFF: LStrFormatName := STR_CF_RIFF;
            CF_WAVE: LStrFormatName := STR_CF_WAVE;
            CF_UNICODETEXT: LStrFormatName := STR_CF_UNICODETEXT;
            CF_ENHMETAFILE: LStrFormatName := STR_CF_ENHMETAFILE;
            CF_HDROP: LStrFormatName := STR_CF_HDROP;
            CF_LOCALE: LStrFormatName := STR_CF_LOCALE;
          else
            SetString(LStrFormatName, LszBuffer,
              GetClipboardFormatName(LWrdCurrent, LszBuffer,
              sizeof(LszBuffer)));
            LStrFormatName := Trim(LStrFormatName);
          end;
          if LStrFormatName = '' then
            LStrFormatName := '(' + IntToRadix(LWrdCurrent, 10) + ')';
          Items.AddObject(LStrFormatName, Pointer(LWrdCurrent));
          LbtnOK.Enabled := True;
          ItemIndex := Items.IndexOfObject(Pointer(LAST_USED_CF));
          if ItemIndex = -1 then
            ItemIndex := 0;
        end;
      end;

      // enable hextext checkbox depending on selected format
      ListSelect(nil);

      if (ShowModal = mrOk) and (LlbxFormats.ItemIndex > -1) then
      begin
        Format := TClipFormat(LlbxFormats.Items.Objects[LlbxFormats.ItemIndex]);
        if Format in [CF_TEXT, CF_OEMTEXT] then
          TextIsHexData := LcbxTextAsHex.Checked;
        Result := True;
        LAST_USED_CF := Format;
      end;
    finally
      // not sure if they automatically get freed?
      LbtnOK.Free;
      LbtnCancel.Free;
      LcbxTextAsHex.Free;
      LlbxFormats.Free;
    end;
  finally
    Free;
  end;
end;

// query a data object's supported formats and check if we can "paste" them

function QueryOLEFormat(const SupportedFormats: array of TClipFormat; const
  dataObj: IDataObject; var Format: TClipFormat; var TextIsHexData: boolean):
  boolean;
var
  LWrdFormats: array of TClipFormat;
  LIntLoop: integer;
  LobjEnum: TFormatEnum;
begin
  Result := False;
  LWrdFormats := nil;
  LobjEnum := TFormatEnum.Create(dataObj);
  try
    // enum all available formats
    if Length(SupportedFormats) > 0 then
    begin
      for LIntLoop := Low(SupportedFormats) to High(SupportedFormats) do
        if LObjEnum.HasFormat(SupportedFormats[LIntLoop]) then
        begin
          SetLength(LWrdFormats, Succ(Length(LWrdFormats)));
          LWrdFormats[Pred(Length(LWrdFormats))] := SupportedFormats[LIntLoop];
        end;
      case Length(LWrdFormats) of
        0: Exit;
        1:
          begin
            Format := LWrdFormats[0];
            Result := True;
            Exit;
          end;
      else
        // show a dialog for data format selection
        Result := SelectClipFormat(LWrdFormats, Format, TextIsHexData);
      end;
    end;
  finally
    LObjEnum.Free;
    LWrdFormats := nil;
  end;
end;

{ TMPHexEditorEx }

// constructor

constructor TMPHexEditorEx.Create(AOwner: TComponent);
begin
  inherited;
  FModifiedNoUndo := False;
  FCreateUndoOnUndoUpdate := False;
  FBookmarksNoChange := False;
  FHasDoubleClicked := False;
  FPaintUpdateCounter := 0;
  FClipData := nil;
  FZoomOnWheel := True;
  FCreateBackups := True;
  FBackupFileExt := BACKUP_EXT;
  FOleDragDrop := False;
  FOleStartDrag := False;
  FOleDragging := False;
  FClipboardAsHexText := False;
  FFlushClipboardAtShutDown := False;
  FSupportsOtherClipFormats := True;
  FPrintOptions := TMPHPrintOptions.Create;
  FPrintFont := TFont.Create;
  FPrintFont.Assign(Font);
  FUseEditorFontForPrinting := True;
  FOffsetPopupMenu := nil;
  if not (csDesigning in ComponentState) then
    FDropTarget := TMPHDropTarget.Create(self); // not in delphi ide
end;

// destructor

destructor TMPHexEditorEx.Destroy;
begin
  // empty or flush clipboard
  ReleaseClipboard(FFlushClipboardAtShutDown);
  FPrintOptions.Free;
  FPrintFont.Free;
  if not (csDesigning in ComponentState) then
    FDropTarget.Free;
  inherited;
end;

// cb copy possible

function TMPHexEditorEx.CanCopy: boolean;
begin
  Result := (DataSize > 0) and (SelCount > 0);
end;

// cb cut possible

function TMPHexEditorEx.CanCut: boolean;
begin
  Result := CanCopy and not (ReadOnlyView or NoSizeChange);
end;

// cb paste possible

function TMPHexEditorEx.CanPaste: boolean;
var
  LifData: IDataObject;
  LIntEffect: integer;
begin
  LIntEffect := DROPEFFECT_COPY;
  Result := (not (ReadOnlyView (*or NoSizeChange*))) and
    Succeeded(OLEGetClipboard(LifData)) and (SupportsOLEData(LifData, 0,
    Point(0,
    0), LintEffect, oleClipboard) = S_OK);
  if Result and NoSizeChange then
    Result := DataSize > 0;
end;

// copy to clipboard

function TMPHexEditorEx.CBCopy: boolean;
begin
  Result := CanCopy;
  if Result then
  begin
    WaitCursor;
    try
      FClipData := TMPHDataObject.CreateFromStream(DataStorage, Min(SelStart,
        SelEnd), SelCount, ExtractFileName(FileName), FClipboardAsHexText,
        SwapNibbles);
      OleCheck(OleSetClipboard(FClipData));
    finally
      OldCursor;
    end;
  end;
end;

// cut to clipboard

function TMPHexEditorEx.CBCut: boolean;
begin
  Result := CanCut and CBCopy;
  if Result then
  begin
    WaitCursor;
    try
      DeleteSelection(UNDO_CUTCB);
    finally
      OldCursor;
    end;
  end;
end;

// paste from clipboard

function TMPHexEditorEx.CBPaste: boolean;
var
  LifData: IDataObject;
  LIntEffect: integer;
begin
  LIntEffect := DROPEFFECT_COPY;
  Result := CanPaste and Succeeded(OLEGetClipboard(LifData)) and
    Succeeded(InsertOLEData(LifData, 0, Point(0, 0), LIntEffect, oleClipboard));
end;

// create an undo for a range of bytes

procedure TMPHexEditorEx.CreateRangeUndo(const aStart, aCount: integer;
  sDesc: string);
var
  bMod: boolean;
begin
  bMod := FModified;
  try
    if aCount < 1 then
      CreateUndo(ufKindAllData, 0, 0, 0, sDesc)
    else
      CreateUndo(ufKindReplace, aStart, aCount, aCount, sDesc);
  finally
    FModified := bMod;
  end;
end;

function TMPHexEditorEx.BeginUpdate: integer;
begin
  Inc(FPaintUpdateCounter);
  Result := FPaintUpdateCounter;
end;

function TMPHexEditorEx.EndUpdate: integer;
begin
  Dec(FPaintUpdateCounter);
  if FPaintUpdateCounter < 0 then
    FPaintUpdateCounter := 0;
  if FPaintUpdateCounter = 0 then
    Invalidate;
  Result := FPaintUpdateCounter;
end;

// mouse wheel overriding for zooming (font size) if CTRL/SHIFT is pressed,
// or bytes per line changing if CTRL pressed

function TMpHexEditorEx.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint):
  boolean;
begin
  if FZoomOnWheel and (Shift = [ssCtrl]) and (BytesPerRow > 1) then
  begin
    Result := True;
    BytesPerRow := BytesPerRow - 1;
    Invalidate;
  end
  else if FZoomOnWheel and (Shift = [ssShift, ssCtrl]) and (Font.Size > 2) then
  begin
    Result := True;
    Font.Size := Font.Size - 1;
  end
  else
    Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TMpHexEditorEx.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint):
  boolean;
begin
  if FZoomOnWheel and (Shift = [ssCtrl]) and (BytesPerRow < 256) then
  begin
    Result := True;
    BytesPerRow := BytesPerRow + 1;
    Invalidate;
  end
  else if FZoomOnWheel and (Shift = [ssShift, ssCtrl]) then
  begin
    Result := True;
    Font.Size := Font.Size + 1;
  end
  else
    Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

// overwrite key handling

procedure TMPHexEditorEx.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
  case Key of
    // CTRL+A: select all
    Ord('A'): if Shift = [ssCtrl] then
      begin
        SelectAll;
      end;

    // CTRL+C: copy to clipboard
    Ord('C'): if (Shift = [ssCtrl]) and CanCopy then
      begin
        CBCopy;
      end;

    // CTRL+X: cut to clipboard
    Ord('X'): if (Shift = [ssCtrl]) and CanCut then
      begin
        CBCut;
      end;

    // CTRL+V: paste from clipboard
    Ord('V'): if (Shift = [ssCtrl]) and CanPaste then
      begin
        CBPaste;
      end;

    // CTRL+T/CTRL*SHIFT+Z: undo, redo
    Ord('Z'):
      begin
        // undo
        if (Shift = [ssCtrl]) and CanUndo then
        begin
          Undo;
        end
          // redo
        else if (Shift = [ssShift, ssCtrl]) and CanRedo then
        begin
          Redo;
        end
      end;
  end;
end;

// handle backup creation

procedure TMPHexEditorEx.PrepareOverwriteDiskFile;

var
  LStrBackup: string;
begin
  inherited;

  if (FCreateBackups and Modified) and FileExists(FileName) then
  begin
    LStrBackup := FileName + FBackupFileExt;
    if FileExists(LStrBackup) and not DeleteFile(LStrBackup) then
      raise EMPHexEditor.CreateFmt(ERR_BACKUP_DELETE,
        [LStrBackup, SysErrorMessage(GetLastError)]);

    if (not MoveFile(PChar(FileName), PChar(LStrBackup))) then
      raise EMPHexEditor.CreateFmt(ERR_BACKUP_CREATE,
        [LStrBackup, SysErrorMessage(GetLastError)]);
  end;
end;

// save to file (overwrite)

procedure TMPHexEditorEx.Save;
begin
  if not HasFile then
    raise EMPHexEditor.Create(ERR_NOFILE);
  SaveToFile(FileName);
end;

// prepare ole dragging

procedure TMPHexEditorEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if FOleDragDrop and (Button = mbLeft) and MouseOverSelection and (not
    IsSelecting) then
  begin
    FOleStartDrag := True;
    FOleDragging := False;
    FOleDragX := X;
    FOleDragY := Y;
  end
end;

// check and eventually do ole dragging

procedure TMPHexEditorEx.MouseMove(Shift: TShiftState; X, Y: integer);
var
  LHrsOperation: HRESULT;
  LIntEffect: integer;
  LobjData: TMPHDataObject;
begin
  inherited;

  if FOleDragDrop and (ssLeft in Shift) and (not FOleDragging) and FOleStartDrag
    and MouseOverSelection and (not IsSelecting) and ((Abs(X - FOleDragX) >=
    Mouse.DragThreshold) or (Abs(Y - FOleDragY) >= Mouse.DragThreshold)) then
  begin
    FOleStartDrag := False;
    FOleDragging := True;
    FoleWasTarget := False;
    // start ole dragging
    try
      LobjData := TMPHDataObject.CreateFromStream(DataStorage, Min(SelStart,
        SelEnd), SelCount, ExtractFileName(FileName), FClipboardAsHexText,
        SwapNibbles);
      if not ReadOnlyView then
        LHrsOperation := DoDragDrop(LobjData, TMPHDropSource.Create,
          DROPEFFECT_COPY or DROPEFFECT_MOVE, LIntEffect)
      else
        LHrsOperation := DoDragDrop(LobjData, TMPHDropSource.Create,
          DROPEFFECT_COPY, LIntEffect);
      // if feedback has given via idataobject.setdata
      if LObjData.FHasDropEffect then
        LIntEffect := LObjData.FDropEffect;
      // unexcpected result
      if (LHrsOperation <> DRAGDROP_S_CANCEL) and (LHrsOperation <>
        DRAGDROP_S_DROP) then
        OLECheck(LHrsOperation)
      else if (LHrsOperation = DRAGDROP_S_DROP) and (LIntEffect =
        DROPEFFECT_MOVE) then
      begin
        // dragged to an other window
        if not FOleWasTarget then
          DeleteSelection
        else
          // dragged to me, so on move, selection is already deleted, create a move undo
          CombineUndo(2, UNDO_MOVED);
      end;
    finally
      FOleDragging := False;
      FOleWasTarget := False;
      HideDragCell;
    end;
  end;
end;

// cancel dragging and flags

procedure TMPHexEditorEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
  integer);
begin
  if FHasDoubleClicked then
  begin
    MouseUpCanResetSel := False;
    FHasDoubleClicked := False;
  end;
  inherited;
  if FOleDragging then
  begin
    FOleDragging := False;
    FOleStartDrag := False;
  end;
end;

// don't allow ole dnd in ide or while loading

procedure TMPHexEditorEx.SetOleDragDrop(const Value: boolean);
begin
  if Value <> FOleDragDrop then
  begin
    FOleDragDrop := Value;
    if ComponentState * [csLoading, csDesigning] = [] then
      FDropTarget.Active := Value;
  end;
end;

// if ole dnd allowed, set new window handle in the drop target

procedure TMPHexEditorEx.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FDropTarget.Active := FOleDragDrop;
  end;
end;

// insert idataobject data

function TMPHexEditorEx.InsertOLEData(const dataObj: IDataObject; const
  grfKeyState: longint; const pt: TPoint; var dwEffect: longint; const
  Operation:
  TMPHOLEOperation): HRESULT;
var
  LRecStg: TStgMedium;
  LStrData, LStrBin: string;
  LIntData, LIntPos, LIntLoop: integer;
  LszBuf: array[0..MAX_PATH] of char;
  LfstFile: TFileStream;
  LPtrLock: Pointer;
  LIntGlobalSize: integer;
begin
  Result := E_FAIL;
  LStrData := '';
  LIntData := 0;
  WaitCursor;
  try
    // haupt-format?
    if ((FOLEFormat[Operation] = CF_MPHEXEDITOR) or (FOLEFormat[Operation] =
      CF_HDROP)) or QueryOLEFormat(GetMyOLEFormats, dataObj,
      FOLEFormat[Operation], FClipboardAsHexText) then
    begin
      // je nach format daten konvertieren
      case FOLEFormat[Operation] of
        CF_HDROP: if Succeeded(GetIDataObjectData(dataObj,
            FOLEFormat[Operation], LRecStg)) then
          try
            // link: -> put all filenames
            // copy: -> copy contents of first file
            LIntLoop := DragQueryFile(LRecStg.hGlobal, cardinal(-1), nil, 0);
            if LintLoop > 0 then
            begin
              for LIntLoop := 0 to Pred(LIntLoop) do
              begin
                DragQueryFile(LRecStg.hGlobal, LIntLoop, LszBuf,
                  sizeof(LszBuf));
                Result := S_OK;
                if dwEffect = DROPEFFECT_LINK then
                begin
                  LStrData := LStrData + StrPas(LszBuf) + #0;
                  LIntData := Length(LStrData);
                end
                else
                begin
                  Result := E_FAIL;
                  LfstFile := TFileStream.Create(LszBuf, fmOpenRead or
                    fmShareDenyNone);
                  try
                    SetLength(LStrData, LfstFile.Size);
                    LfstFile.ReadBuffer(LStrData[1], LfstFile.Size);
                    Result := S_OK;
                    LIntData := Length(LStrData);
                    Break; // just 1st file
                  finally
                    LfstFile.Free;
                  end;
                end;
              end;
            end;
          finally
            ReleaseStgMedium(LRecStg);
          end;
      else
        // format other than CF_HDROP (=files dropped), retrieve data
        if Succeeded(GetIDataObjectData(dataObj, FOLEFormat[Operation], LRecStg))
          then
        try
          if LRecStg.tymed in [TYMED_HGLOBAL, TYMED_MFPICT] then
          begin
            LPtrLock := GlobalLock(LRecStg.hGlobal);
            LIntGlobalSize := GlobalSize(LRecStg.hGlobal);
          end
          else
          begin
            LPtrLock := nil;
            LIntGlobalSize := 0;
          end;
          try
            LStrData := GetSomeData(LPtrLock, LRecStg.hGlobal,
              FOLEFormat[Operation], LIntGlobalSize, UnicodeBigEndian);
            if LStrData <> '' then
            begin
              LIntData := Length(LStrData);
              if (FOLEFormat[Operation] in [CF_TEXT, CF_OEMTEXT]) and (Operation
                = oleClipboard) and FClipBoardAsHexText then
              begin
                // convert hex text to data
                SetLength(LStrBin, Length(LStrData));
                ConvertHexToBin(@LStrData[1], @LStrBin[1], LIntData,
                  SwapNibbles, LIntData);
                LStrData := Copy(LStrBin, 1, LIntData);
              end;
              Result := S_OK;
            end;
          finally
            if Assigned(LPtrLock) then
              GlobalUnlock(LRecStg.hGlobal);
          end;
        finally
          ReleaseStgMedium(LRecStg);
        end
      end;

      CheckUnit(LIntData);

      if (LStrData <> '') and (LIntData > 0) then
      begin
        // insert the data
        case Operation of
          oleDrop:
            begin
              LIntPos := DropPosition;
              if LIntPos < 0 then
                Result := E_FAIL
              else
              begin
                if FOleDragging and (dwEffect = DROPEFFECT_MOVE) then
                begin
                  FFixedFileSizeOverride := True;
                  try
                    // delete selection if we have moved data within ourself
                    FOleWasTarget := True;
                    if LIntPos > Min(SelStart, SelEnd) then
                      Dec(LIntPos, SelCount);
                    DeleteSelection;
                    if LIntPos >= DataSize then
                      Appendbuffer(@LStrData[1], LIntData, UNDO_DROPPED)
                    else
                      InsertBuffer(@LStrData[1], LIntData, LIntPos,
                        UNDO_DROPPED);
                  finally
                    FFixedFileSizeOverride := False;
                  end;
                end
                else
                begin
                  if LIntPos >= DataSize then
                  begin
                    if not NoSizeChange then
                      Appendbuffer(@LStrData[1], LIntData, UNDO_DROPPED)
                  end
                  else
                  begin
                    if not NoSizeChange then
                    begin
                      if IsSelected(LIntPos) then
                        ReplaceSelection(@LStrData[1], LIntData, UNDO_DROPPED)
                      else
                        InsertBuffer(@LStrData[1], LIntData, LIntPos,
                          UNDO_DROPPED)
                    end
                    else
                    begin
                      if (SelCount = 0) or (not IsSelected(LIntPos)) then
                        Replace(@LStrData[1], LIntPos, LIntData, LIntData,
                          UNDO_DROPPED)
                      else
                        ReplaceSelection(@LStrData[1], LIntData, UNDO_DROPPED)
                    end;
                  end;
                end;
              end;
            end;
          oleClipboard: PasteData(PChar(LStrData), LIntData, UNDO_PASTECB);
        end;
      end
      else
        Result := E_FAIL;
    end;

  finally
    LStrData := '';
    OldCursor;
  end;
  if Result <> S_OK then
    dwEffect := DROPEFFECT_NONE;
end;

// do we support one of the provided idataobject formats?

function TMPHexEditorEx.SupportsOLEData(const dataObj: IDataObject; const
  grfKeyState: integer; const pt: TPoint; var dwEffect: integer; const
  Operation:
  TMPHOLEOperation): HRESULT;
begin
  Result := S_FALSE;
  if (not ReadOnlyView) and OLEHasSupportedFormat(dataObj, GetMyOLEFormats,
    FOLEFormat[Operation]) then
    Result := S_OK;
  if FOLEFormat[Operation] = CF_HDROP then
    if dwEffect = DROPEFFECT_MOVE then
      dwEffect := DROPEFFECT_LINK;
end;

function TMPHexEditorEx.OLEHasSupportedFormat(const dataObj: IDataObject; const
  Formats: array of TClipFormat; var Format: TClipFormat): boolean;
var
  LIntLoop: integer;
  LObjEnum: TFormatEnum;
begin
  Result := False;
  LObjEnum := TFormatEnum.Create(dataObj);
  try
    if Length(Formats) > 0 then
      for LIntLoop := Low(Formats) to High(Formats) do
        if LObjEnum.HasFormat(Formats[LIntLoop]) then
        begin
          Format := Formats[LIntLoop];
          Result := True;
          Break;
        end;
  finally
    LObjEnum.Free;
  end;
end;

// modify effect (move/copy/link) depending on key state and data format

function TMPHexEditorEx.ModifyOLEDropEffect(const grfKeyState: integer; const
  pt: TPoint; var dwEffect: integer): HRESULT;
begin
  Result := S_OK;
  if FOleDragging then
  begin
    if ReadOnlyView then
      dwEffect := DROPEFFECT_COPY
    else
    begin
      if Bool(grfKeyState and MK_CONTROL) then
        dwEffect := DROPEFFECT_COPY
      else
        dwEffect := DROPEFFECT_MOVE;
    end;
  end
  else
  begin
    if Bool(grfKeyState and MK_SHIFT) and (not ReadOnlyView) then
      dwEffect := DROPEFFECT_MOVE
    else
      dwEffect := DROPEFFECT_COPY;

    if FOLEFormat[oleDrop] = CF_HDROP then
      if dwEffect = DROPEFFECT_MOVE then
        dwEffect := DROPEFFECT_LINK;
  end;
end;

// return a clipformat array with all supported formats

function TMPHexEditorEx.GetMyOLEFormats: TClipFormats;
begin
  if FSupportsOtherClipFormats then
    SetLength(Result, 17)
  else
    SetLength(Result, 2);
  Result[0] := CF_MPHEXEDITOR;
  Result[1] := CF_HDROP;
  if FSupportsOtherClipFormats then
  begin
    Result[2] := CF_TEXT;
    Result[3] := CF_RTF;
    Result[4] := CF_UNICODETEXT;
    Result[5] := CF_BITMAP;
    Result[6] := CF_PALETTE;
    Result[7] := CF_METAFILEPICT;
    Result[8] := CF_TIFF;
    Result[9] := CF_OEMTEXT;
    Result[10] := CF_DIB;
    Result[11] := CF_RIFF;
    Result[12] := CF_WAVE;
    Result[13] := CF_ENHMETAFILE;
    Result[14] := CF_LOCALE;
    Result[15] := CF_REGEDIT_HEXDATA;
    Result[16] := CF_HTML;
  end;
end;

// reset droptarget helper interface on window destruction

procedure TMPHexEditorEx.WMDestroy(var Message: TWMDestroy);
begin
  inherited;
  if ComponentState * [csLoading, csDesigning] = [] then
    FDropTarget.Active := False;
end;

// internal

function TMPHexEditorEx.DumpUndoStorage(const FileName: string): boolean;
begin
  Result := False;
  if Assigned(UndoStorage) then
  try
    Result := True;
    UndoStorage.SaveToFile(FileName);
  except
    Result := False;
  end;
end;

// set new printing options

procedure TMPHexEditorEx.SetPrintOptions(const Value: TMPHPrintOptions);
begin
  FPrintOptions.Assign(Value);
end;

// internal: draw the specified page to a canvas using the given margins and options

function TMPHexEditorEx.PrintToCanvas(ACanvas: TCanvas; const APage: integer;
  const AMargins: TRect): integer;
var
  LObjPrinter: TMPHCanvasPrinter;
  LSetFlags: TMPHPrintFlags;
begin
  if APage < 0 then
    raise EMPHexEditor.Create(ERR_INVALID_PAGE);
  WaitCursor;
  LSetFlags := FPrintOptions.Flags;
  try
    if SelCount = 0 then
      Exclude(LSetFlags, pfSelectionOnly);
    LObjPrinter := TMPHCanvasPrinter.Create(self, ACanvas, LSetFlags, AMargins,
      FPrintOptions.FHeaders);
    try
      Result := LObjPrinter.Pages;
      if APage > Result then
        raise EMPHexEditor.Create(ERR_INVALID_PAGE);
      if APage > 0 then
        if LObjPrinter.DrawOrCalc(False, APage) < 1 then
          raise EMPHexEditor.Create(ERR_PRINTING_FAILED);
    finally
      LObjPrinter.Free;
    end;
  finally
    OldCursor;
  end;
end;

// create a metafile with the selected page as a print preview

function TMPHexEditorEx.PrintPreview(const Page: integer): TMetaFile;
var
  LcnvMeta: TMetaFileCanvas;
  LIntHeight, LIntWidth: integer;
begin
  LIntWidth := GetDeviceCaps(Printer.Handle, HORZRES);
  LIntHeight := GetDeviceCaps(Printer.Handle, VERTRES);
  Result := TMetaFile.Create;
  with Result do
  begin
    Width := LIntWidth;
    Height := LIntHeight;
    LcnvMeta := TMetaFileCanvas.Create(Result, 0);
    with LcnvMeta do
    try
      if FUseEditorFontForPrinting then
        Font.Assign(self.Font)
      else
        Font.Assign(self.FPrintFont);
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, LIntWidth, LIntHeight, nil);
      SetViewPortExtEx(Handle, LIntWidth, LIntHeight, nil);
      Font.Size := Round(Font.Size * GetDeviceCaps(Printer.Handle, LOGPIXELSY) /
        Screen.PixelsPerInch);
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, LIntWidth, LIntHeight));
      FPrintPages := PrintToCanvas(LcnvMeta, Page, PrinterMarginRect);
    finally
      Free;
    end;
  end;
end;

// print the given page

procedure TMPHexEditorEx.Print(const Page: integer);
var
  LmtfTemp: TMetaFile;
begin
  if Page < 1 then
    raise EMPHexEditor.Create(ERR_INVALID_PAGE);
  LmtfTemp := PrintPreview(Page);
  with LmtfTemp do
  try
    Printer.Canvas.StretchDraw(Rect(0, 0, Printer.PageWidth,
      Printer.PageHeight), LmtfTemp);
  finally
    Free;
  end;
end;

// calculate margins from margins in print options

function TMPHexEditorEx.PrinterMarginRect: TRect;
var
  LIntLogX, LIntLogY, LIntPhysWidth, LIntPhysHeight: integer;
begin
  Result := FPrintOptions.FMargins;
  LIntLogX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  // pixels per inch in x dir
  LIntLogY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  // pixels per inch in Y dir
  LIntPhysWidth := Printer.PageWidth;
  LIntPhysHeight := Printer.PageHeight;
  Result.Left := Round(Result.Left / 25.4 * LIntLogX);
  Result.Top := Round(Result.Top / 25.4 * LIntLogY);
  Result.Right := LIntPhysWidth - Round(Result.Right / 25.4 * LIntLogX);
  Result.Bottom := LIntPhysHeight - Round(Result.Bottom / 25.4 * LIntLogY);
end;

// calculate page count

function TMPHexEditorEx.PrintNumPages: integer;
begin
  PrintPreview(0).Free;
  Result := FPrintPages;
end;

// empty or flush ole contents in clipboard that have been stored by this instance

procedure TMPHexEditorEx.ReleaseClipboard(const Flush: boolean);
begin
  if OwnsClipboard then
  begin
    if Flush then
      OleCheck(OleFlushClipboard)
    else
      OleSetClipboard(nil);
  end;
end;

// is there data on the clipboard created by us?

function TMPHexEditorEx.OwnsClipBoard: boolean;
begin
  Result := OleIsCurrentClipBoard(FClipData) = S_OK;
end;

procedure TMPHexEditorEx.SetPrintFont(const Value: TFont);
begin
  FPrintFont.Assign(Value);
  FUseEditorFontForPrinting := False;
end;

{$IFDEF DELPHI6UP}

procedure TMPHexEditorEx.DoContextPopup(MousePos: TPoint; var Handled: boolean);
begin
  inherited;
  if (not Handled) and (Assigned(FOffsetPopupMenu)) then
  begin
    // is mouse over offset col
    with MousePos do
      if ((X > -1) and (X < (ColWidths[0] + ColWidths[1]))) or ((Y > -1) and (Y
        < (RowHeights[0] + RowHeights[1]))) then
      begin
        // in fixed range
        if FOffsetPopupMenu.AutoPopup then
        begin
          Handled := True;
          SendCancelMode(nil);
          FOffsetPopupMenu.PopupComponent := Self;
          MousePos := ClientToScreen(MousePos);
          if InvalidPoint(MousePos) then
            MousePos := ClientToScreen(Point(0, 0));
          FOffsetPopupMenu.Popup(MousePos.X, MousePos.Y);
        end;
      end;
  end
end;
{$ENDIF}

procedure TMPHexEditorEx.SetOffsetPopupMenu(const Value: TPopupMenu);
begin
  FOffsetPopupMenu := Value;
  if Assigned(Value) then
    with Value do
    begin
      ParentBiDiModeChanged(self);
      FreeNotification(self);
    end;
end;

function TMPHexEditorEx.GetOffsetPopupMenu: TPopupMenu;
begin
  Result := FOffsetPopupMenu;
end;

procedure TMPHexEditorEx.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if AComponent = FOffsetPopupMenu then
    if Operation = opRemove then
      OffsetPopupMenu := nil;
end;

function TMPHexEditorEx.CanCreateUndo(const aKind: TMPHUndoFlag;
  const aCount, aReplCount: integer): Boolean;
begin
  Result := inherited CanCreateUndo(aKind, aCount, aReplCount);
  if Result and (UndoStorage.UpdateCount > 0) then
    FModifiedNoUndo := True;
end;

function TMPHexEditorEx.GetBookmarksAsString: string;

  procedure AddBM(const bm: TMPHBookmark);
  begin
    SetLength(Result, Length(Result)+sizeof(bm));
    Move(bm, Result[Length(Result)-sizeof(bm)+1], sizeof(bm));
  end;

var
  LIntLoop: integer;
  LBMSet: boolean;

begin
  Result := '$';
  LBMSet := False;
  for LIntLoop := Low(TMPHBookMarks) to High(TMPHBookMarks) do
  begin
    AddBM(BookMark[LIntLoop]);
    if (not LBMSet) and (BookMark[LIntLoop].mPosition <> -1) then
      LBMSet := True;
  end;
  if not LBMSet then
    Result := '';
end;

procedure TMPHexEditorEx.SetBookMarksAsString(Value: string);
var
  LIntLoop, LIntCheck, LIntCheck1, LIntPos: integer;
  LBoolChars: boolean;
  LRecBook: TMPHBookMark;

  procedure NewSetBookmarks;

    procedure ExtractBM(const i: integer);
    var
      bm: TMPHBookmark;
    begin
      Move(Value[1], bm, sizeof(bm));
      Delete(Value, 1, sizeof(bm));
      BookMark[i] := bm;
    end;

  var
    LIntLoop: integer;
  begin
    Delete(Value,1,1);
    try
      for LIntLoop := Low(TMPHBookMarks) to High(TMPHBookMarks) do
        ExtractBM(LIntLoop);
    except
      raise EMPHexEditor.Create(ERR_INVALID_BOOKFMT);
    end;
  end;

begin
  BeginUpdate;
  FBookmarksNoChange := True;
  try
    // empty all bookmarks
    LRecBook.mPosition := -1;
    LRecBook.mInCharField := InCharField;
    for LIntLoop := Low(TMPHBookMarks) to High(TMPHBookMarks) do
      Bookmark[LIntLoop] := LRecBook;

    if Value <> '' then
    begin
      if Value[1] = '$' then
        NewSetBookmarks
      else
      try
        // check sum
        LIntCheck := RadixToInt(Copy(Value, 1, 8), 16);
        Delete(Value, 1, 8);

        // calc check sum
        LIntCheck1 := 0;
        for LIntLoop := 1 to Length(Value) do
          LIntCheck1 := LIntCheck1 + Ord(Value[LIntLoop]);

        if LIntCheck1 <> LIntCheck then
          raise EMPHexEditor.Create(ERR_INVALID_BOOKFMT);

        // set bookmarks
        //for LIntLoop := Low(TMPHBookMarks) to High(TMPHBookMarks) do
        while Value <> '' do
        begin
          LIntLoop := RadixToInt(Copy(Value, 1, 2), 16);
          Delete(Value, 1, 2);
          LIntPos := RadixToInt(Copy(Value, 1, 16), 16);
          Delete(Value, 1, 16);
          LBoolChars := boolean(RadixToInt(Copy(Value, 1, 2), 16));
          Delete(Value, 1, 2);
          LRecBook := Bookmark[LIntLoop];
          if (LRecBook.mPosition <> LIntPos) or (LRecBook.mInCharField <>
            LBoolChars) then
          begin
            LRecBook.mPosition := LIntPos;
            LRecBook.mInCharField := LBoolChars;
            Bookmark[LIntLoop] := LRecBook;
          end;
        end;

      except
        raise EMPHexEditor.Create(ERR_INVALID_BOOKFMT);
      end;
    end;
  finally
    EndUpdate;
    FBookmarksNoChange := False;
    BookmarkChanged;
  end;
end;

{$IFDEF DELPHI6UP}

const
  MPTH_PUBLIC_PROPS: array[0..66] of string = (
    'ShowRuler',
    'DrawGutter3D',
    'CreateBackup',
    'BackupExtension',
    'OleDragDrop',
    'ClipboardAsHexText',
    'FlushClipboardAtShutDown',
    'SupportsOtherClipFormats',
    'UseEditorFontForPrinting',
    'ZoomOnWheel',
    'BytesPerRow',
    'BytesPerColumn',
    'Translation',
    'OffsetFormat',
    'CaretKind',
    'FocusFrame',
    'SwapNibbles',
    'MaskChar',
    'NoSizeChange',
    'AllowInsertMode',
    'DrawGridLines',
    'ReadOnlyView',
    'HideSelection',
    'GraySelectionIfNotFocused',
    'GutterWidth',
    'MaxUndo',
    'InsertMode',
    'HexLowerCase',
    'Colors.Background',
    'Colors.ChangedBackground',
    'Colors.ChangedText',
    'Colors.CursorFrame',
    'Colors.NonFocusCursorFrame',
    'Colors.Offset',
    'Colors.OddColumn',
    'Colors.EvenColumn',
    'Colors.CurrentOffsetBackground',
    'Colors.OffsetBackGround',
    'Colors.CurrentOffset',
    'Colors.ActiveFieldBackground',
    'Colors.Grid',
    'PrintFont.Charset',
    'PrintFont.Color',
    'PrintFont.Name',
    'PrintFont.Size',
    'PrintFont.Style',
    'PrintOptions.MarginLeft',
    'PrintOptions.MarginTop',
    'PrintOptions.MarginRight',
    'PrintOptions.MarginBottom',
    'PrintOptions.PageHeader',
    'PrintOptions.PageFooter',
    'PrintOptions.Flags',
    'Font.Charset',
    'Font.Color',
    'Font.Name',
    'Font.Size',
    'Font.Style',
    'BytesPerUnit',
    'RulerBytesPerUnit',
    'ShowPositionIfNotFocused',
    'UnicodeChars',
    'UnicodeBigEndian',
    'BytesPerBlock',
    'SeparateBlocksInCharField',
    'FindProgress',
    'RulerNumberBase'
    );

function TMPHexEditorEx.IsPropPublic(PropName: string): boolean;
var
  LIntLoop: integer;
begin
  Result := False;
  for LIntLoop := Low(MPTH_PUBLIC_PROPS) to High(MPTH_PUBLIC_PROPS) do
    if AnsiCompareText(PropName, MPTH_PUBLIC_PROPS[LIntLoop]) = 0 then
    begin
      Result := True;
      Break;
    end;
  if Result and Assigned(FOnQueryPublicProperty) then
    FOnQueryPublicProperty(self, PropName, Result);
end;
function TMPHexEditorEx.GetPropertiesAsString: string;
var
  sl: TStrings;

  procedure Recurse(Ref: TObject; const Prefix: string);
  var
    LPtrProps: PPropList;
    LIntCount: integer;
  begin
    if Ref = nil then
      Exit;
    LIntCount := GetPropList(Ref, LPTrProps);
    if LIntCount > 0 then
    try
      for LIntCount := 0 to Pred(LIntCount) do
        with LPtrProps^[LIntCount]^ do
          if PropType^^.Kind = tkClass then
            Recurse(GetObjectProp(Ref, Name), Prefix + Name + '.')
          else if IsPropPublic(Prefix + Name) then
            sl.Add(Prefix + Name + '=' +string(GetPropValue(Ref, Name)));

    finally
      FreeMem(LPtrProps);
    end;
  end;
begin
  sl := TStringList.Create;
  try
    Recurse(self, '');
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

{+}
type
  TStringsPro = class(TStrings);
{+.}

procedure TMPHexEditorEx.SetPropertiesAsString(const Value: string);
var
  LStrData: TStrings;
  LIntLoop, LIntDot: integer;
  LStrProp, LStrVal: string;
  LObjProp: TObject;
  {+}
  function GetValueFromIndex(Index: Integer): string;
  begin
    if Index >= 0 then
      Result := Copy(TStringsPro(LStrData).Get(Index), Length(LStrData.Names[Index]) + 2, MaxInt) else
      Result := '';
  end;
  {+.}
begin
  BeginUpdate;
  try
    LStrData := TStringList.Create;
    with LStrData do
    try
      Text := Value;
      if Count > 0 then
        for LIntLoop := 0 to Pred(Count) do
        begin
          LStrProp := Names[LIntLoop];
          if IsPropPublic(LStrProp) then
          begin
            {+}
            //LStrVal := ValueFromIndex[LIntLoop];
            LStrVal := GetValueFromIndex(LIntLoop);
            {+.}
            LObjProp := self;
            repeat
              LIntDot := Pos('.', LStrProp);
              if LIntDot > 0 then
              begin
                LObjProp := GetObjectProp(LObjProp, Copy(LStrProp, 1, LIntDot -
                  1));
                System.Delete(LStrProp, 1, LIntDot);
              end;
            until LIntDot = 0;
            if Assigned(LObjProp) then
              SetPropValue(LObjProp, LStrProp, LStrVal);
          end;
        end;
    finally
      Free;
    end;
  finally
    EndUpdate;
  end;
end;

{$ENDIF}

procedure TMPHexEditorEx.Paint;
begin
  //inherited;
  if FPaintUpdateCounter < 1 then
    inherited;
end;

procedure TMPHexEditorEx.DblClick;
var
  LptMouse: TPoint;
  LIntPos: Integer;
begin
  // get the position where the mouse is
  Windows.GetCursorPos(LptMouse);
  LptMouse := ScreenToClient(LptMouse);
  with CheckMouseCoord(LptMouse.X, LptMouse.Y) do
    LIntPos := GetPosAtCursor(X, Y);
  if (LIntPos > -1) and (LIntPos < DataSize) then
  begin
    NewSelection(LIntPos, LIntPos);
    FHasDoubleClicked := True;
    MouseUpCanResetSel := False;
  end;
  inherited;
end;

procedure TMPHexEditorEx.PasteData(P: Pointer; const ACount: integer;
  const UndoDesc: string);
var
  LgrcCoords: TGridCoord;
  LIntPos: integer;
begin
  // assure that we are positionned at the beginning of a unit
  LIntPos := 0;
  if SelCount = 0 then
  begin
    LIntPos := GetPosAtCursor(Col, Row);
    if (LIntpos mod BytesPerUnit) <> 0 then
    begin
      while (LIntPos mod BytesPerUnit) <> 0 do
        Dec(LIntPos);
      LGrcCoords := GetCursorAtPos(LIntPos, InCharField);
      with LGrcCoords do
      begin
        Col := X;
        Row := Y;
      end;
    end;
  end;
  if (SelCount = 0) and NoSizeChange then
  begin
    SelStart := LIntPos;
    SelEnd := Min(DataSize - 1, LIntPos + ACount - 1);
  end;
  ReplaceSelection(P, ACount, UndoDesc);
end;

procedure TMPHexEditorEx.BookmarkChanged;
begin
  if not FBookmarksNoChange then
    inherited;
end;

function TMPHexEditorEx.UndoBeginUpdate(const StrUndoDesc: string = ''):
  integer;
begin
  if (UndoStorage.UpdateCount = 0) and (FCreateUndoOnUndoUpdate or (StrUndoDesc
    <> '')) then
  begin
    FCreateUndoOnUndoUpdate := True;
    CreateRangeUndo(0, 0, StrUndoDesc);
    FModifiedNoUndo := False;
  end;
  Result := inherited UndoBeginUpdate;
end;

function TMPHexEditorEx.UndoEndUpdate: integer;
begin
  Result := inherited UndoEndUpdate;
  if (Result = 0) and FCreateUndoOnUndoUpdate then
  begin
    if FModifiedNoUndo then
      FModifiedNoUndo := False
    else
    begin
      UndoStorage.RemoveLastUndo;
    end;
  end;
end;

procedure TMPHexEditorEx.WriteBuffer(const Buffer; const Index,
  Count: Integer);
begin
  inherited;
  FModified := True;
  if UndoStorage.UpdateCount > 0 then
    FModifiedNoUndo := True;
end;

{ TMPHDropTarget }

// constructor

constructor TMPHDropTarget.Create(Editor: TMPHexEditorEx);
begin
  inherited Create;
  FEditor := Editor;
  FEditorHandle := 0;
  FActive := False;
  _AddRef; // don't free automatically because it's an object in TMPHexEditorEx
end;

// tinterfacedobject auto-destructor hook

procedure TMPHDropTarget.BeforeDestruction;
begin
  Dec(FRefCount); // see create above
  Active := False;
  inherited;
end;

// do we support data format? if yes, set desired drop effect

function TMPHDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState:
  integer; pt: TPoint; var dwEffect: integer): HResult;
begin
  Result := FEditor.SupportsOLEData(dataObj, grfKeyState, pt, dwEffect,
    oleDrop);
  if Result = S_OK then
  begin
    Result := FEditor.ModifyOLEDropEffect(grfKeyState, pt, dwEffect);
    if Result = S_OK then
    begin
      pt := FEditor.ScreenToClient(pt);
      FEditor.ShowDragCell(pt.X, pt.Y)
    end;
  end
  else
    dwEffect := DROPEFFECT_NONE;
end;

// dragged out of window

function TMPHDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
  FEditor.HideDragCell;
end;

// dragging over window

function TMPHDropTarget.DragOver(grfKeyState: integer; pt: TPoint; var dwEffect:
  integer): HResult;
begin
  Result := FEditor.ModifyOLEDropEffect(grfKeyState, pt, dwEffect);
  if Result = S_OK then
  begin
    pt := FEditor.ScreenToClient(pt);
    FEditor.ShowDragCell(pt.X, pt.Y)
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    FEditor.HideDragCell;
  end;
end;

// dropped!

function TMPHDropTarget.Drop(const dataObj: IDataObject; grfKeyState: integer;
  pt: TPoint; var dwEffect: integer): HResult;
begin
  try
    Result := FEditor.SupportsOLEData(dataObj, grfKeyState, pt, dwEffect,
      oleDrop);
    if Result = S_OK then
    begin
      Result := FEditor.ModifyOLEDropEffect(grfKeyState, pt, dwEffect);
      if Result = S_OK then
      try
        Result := FEditor.InsertOLEData(dataObj, grfKeyState, pt, dwEffect,
          oleDrop);
      except
        Result := E_FAIL;
        ShowException(ExceptObject, ExceptAddr);
      end;
    end;
  finally
    FEditor.HideDragCell;
  end;
end;

// retrieve window handle from associated hex editor and (de)activate drop target

procedure TMPHDropTarget.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not Value then
    begin
      OleCheck(RevokeDragDrop(FEditorHandle));
      OleCheck(CoLockObjectExternal(self, False, True));
    end
    else
    begin
      FEditorHandle := FEditor.Handle;
      OleCheck(RegisterDragDrop(FEditor.Handle, self));
      OleCheck(CoLockObjectExternal(self, True, False));
    end;
  end;
end;

{ TFormatEnum }

// constructor

constructor TFormatEnum.Create(const dataObject: IDataObject);
var
  LRecFormat: TFormatETC;
  LifEnum: IEnumFormatETC;
begin
  FFormats := nil;
  if Succeeded(dataObject.EnumFormatEtc(DATADIR_GET, LifEnum)) then
  begin
    while LifEnum.Next(1, LRecFormat, nil) = S_OK do
    begin
      SetLength(FFormats, Succ(Length(FFormats)));
      FFormats[Pred(Length(FFormats))] := LRecFormat;
    end;
  end;
end;

// destructor

destructor TFormatEnum.Destroy;
begin
  FFormats := nil;
  inherited;
end;

// return the desired formatetc struct

function TFormatEnum.GetFormatETC(const cfFormat: TClipFormat): TFormatETC;
var
  LBoolOK: boolean;
  LIntLoop: integer;
begin
  LBoolOK := False;
  if Length(FFormats) > 0 then
    for LIntLoop := 0 to Pred(Length(FFormats)) do
      if FFormats[LIntLoop].cfFormat = cfFormat then
      begin
        LBoolOK := True;
        Result := FFormats[LIntLoop];
        Break;
      end;
  if not LBoolOK then
    FillChar(Result, sizeof(Result), #$FF);
end;

// is the desired format available?

function TFormatEnum.HasFormat(const cfFormat: TClipFormat): boolean;
var
  LIntLoop: integer;
begin
  Result := False;
  if Length(FFormats) > 0 then
    for LIntLoop := 0 to Pred(Length(FFormats)) do
      if FFormats[LIntLoop].cfFormat = cfFormat then
      begin
        Result := True;
        Break;
      end;
end;

{ TMPHEnumFormatETC }

// constructor

constructor TMPHEnumFormatETC.Create;
begin
  inherited Create;
  FIndex := 0;
  with FFormats[0] do
  begin
    cfFormat := CF_MPHEXEDITOR;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with FFormats[1] do
  begin
    cfFormat := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with FFormats[2] do
  begin
    cfFormat := CF_FILEDESCRIPTOR;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with FFormats[3] do
  begin
    cfFormat := CF_FILECONTENTS;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
end;

// clone myself

function TMPHEnumFormatETC.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Enum := TMPHEnumFormatETC.Create;
  Result := S_OK;
end;

// iterate over all format records

function TMPHEnumFormatETC.Next(celt: integer; out elt; pceltFetched: PLongint):
  HResult;
var
  LIntLoop: integer;
  LRecOut: packed array[0..MY_SUPPORTED_FORMATS - 1] of TFormatETC absolute elt;
begin
  LIntLoop := 0;
  while (LIntLoop < celt) and (FIndex < MY_SUPPORTED_FORMATS) do
  begin
    LRecOut[LIntLoop] := FFormats[FIndex];
    Inc(FIndex);
    Inc(LIntLoop);
  end;

  if pceltFetched <> nil then
    pceltFetched^ := LIntLoop;

  if LIntLoop = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

// reset iteration

function TMPHEnumFormatETC.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

// skip entries

function TMPHEnumFormatETC.Skip(celt: integer): HResult;
begin
  if (celt < MY_SUPPORTED_FORMATS - FIndex) then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

{ TMPHDropSource }

// use default ole dnd cursors

function TMPHDropSource.GiveFeedback(dwEffect: integer): HResult;
begin
  case dwEffect and 7 of
    DROPEFFECT_NONE,
      DROPEFFECT_COPY,
      DROPEFFECT_MOVE: Result := DRAGDROP_S_USEDEFAULTCURSORS;
  else
    Result := E_INVALIDARG;
  end;
end;

// standard behaviour

function TMPHDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState:
  integer): HResult;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON) = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

{ TMPHDataObject }

// constructor

constructor TMPHDataObject.Create(Data: Pointer; DataSize: integer;
  ScrapFileName: ShortString; TextAsHex, SwapNibbles: boolean);
begin
  inherited Create;
  FData := nil;
  FHasDropEffect := False;
  FTextAsHex := TextAsHex;
  FSwapNibbles := SwapNibbles;
  if Assigned(Data) and (DataSize > 0) then
  begin
    FDataSize := DataSize;
    FFileName := Format(STR_SCRAPFILE,
      [ChangeFileExt(ExtractFileName(ScrapFileName), ''),
      ExtractFileExt(ScrapFileName)]);
    GetMem(FData, DataSize);
    Move(Data^, FData^, FDataSize);
  end;
end;

constructor TMPHDataObject.CreateFromStream(Stream: TStream; Position, DataSize:
  integer; ScrapFileName: ShortString; TextAsHex, SwapNibbles: boolean);
begin
  inherited Create;
  FData := nil;
  FHasDropEffect := False;
  FTextAsHex := TextAsHex;
  FSwapNibbles := SwapNibbles;
  if Assigned(Stream) and (DataSize > 0) then
  begin
    FDataSize := DataSize;
    FFileName := Format(STR_SCRAPFILE,
      [ChangeFileExt(ExtractFileName(ScrapFileName), ''),
      ExtractFileExt(ScrapFileName)]);
    GetMem(FData, DataSize);
    Stream.Position := Position;
    Stream.ReadBuffer(FData^, FDataSize);
  end;
end;

// destructor hook

procedure TMPHDataObject.BeforeDestruction;
begin
  if Assigned(FData) and (FDataSize > 0) then
    FreeMem(FData);
  FData := nil;
  FDataSize := 0;
  inherited;
end;

// advise not supported

function TMPHDataObject.DAdvise(const formatetc: TFormatEtc; advf: integer; const
  advSink: IAdviseSink; out dwConnection: integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TMPHDataObject.DUnadvise(dwConnection: integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TMPHDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

// create a formetc enumerator, only for getdata

function TMPHDataObject.EnumFormatEtc(dwDirection: integer; out enumFormatEtc:
  IEnumFormatEtc): HResult;
begin
  enumFormatETC := nil;
  if dwDirection = DATADIR_GET then
  begin
    enumFormatETC := TMPHEnumFormatETC.Create;
    Result := S_OK;
  end
  else
    Result := E_NOTIMPL;
end;

// always same format

function TMPHDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc; out
  formatetcOut: TFormatEtc): HResult;
begin
  formatetcOut := formatetc;
  formatetcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

// render and return data depending on the desired format

function TMPHDataObject.GetData(const formatetcIn: TFormatEtc; out medium:
  TStgMedium): HResult;
var
  LIntDataSize: integer;
  LPtrLocal: PClipData;
  LRecSysTime: TSystemTime;
begin
  FillChar(medium, sizeof(medium), #0);
  Result := QueryGetData(formatetcIn);
  if Result = S_OK then
  begin
    if formatetcIn.cfFormat = CF_MPHEXEDITOR then
      LIntDataSize := sizeof(TClipData) - 1 + FDataSize
    else if formatetcIn.cfFormat = CF_TEXT then
    begin
      if not FTextAsHex then
        LIntDataSize := Min(FDataSize, StrLen(PChar(FData))) + 1
      else
        LIntDataSize := (FDataSize * 2) + 1;
    end
    else if formatetcIn.cfFormat = CF_FILEDESCRIPTOR then
      LIntDataSize := sizeof(TFileGroupDescriptor)
    else
      LIntDataSize := FDataSize;
    medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
      LIntDataSize);
    if medium.hGlobal = 0 then
      Result := E_OUTOFMEMORY
    else
    begin
      LPtrLocal := GlobalLock(medium.hGlobal);
      try
        try
          medium.tymed := TYMED_HGLOBAL;
          if formatetcIn.cfFormat = CF_TEXT then
          begin
            if FTextAsHex then
              ConvertBinToHex(FData, PChar(LPtrLocal), FDataSize, FSwapNibbles)
            else
              Move(FData^, LPtrLocal^, LIntDataSize - 1);
            PChar(LPtrLocal)[LIntDataSize - 1] := #0;
          end
          else if formatetcIn.cfFormat = CF_MPHEXEDITOR then
          begin
            LPtrLocal^.Signature := CLIP_SIG;
            LPtrLocal^.Version := CLIP_VER;
            LPtrLocal^.Size := FDataSize;
            Move(FData^, LPtrLocal^.Data, FDataSize);
          end
          else if formatetcIn.cfFormat = CF_FILEDESCRIPTOR then
          begin
            with PFileGroupDescriptor(LPtrLocal)^ do
            begin
              cItems := 1;
              with fgd[0] do
              begin
                dwFlags := FD_FILESIZE or FD_WRITESTIME; // or FD_PROGRESSUI;
                nFileSizeLow := FDataSize;
                nFileSizeHigh := 0;
                GetSystemTime(LRecSysTime);
                SystemTimeToFileTime(LRecSysTime, ftLastWriteTime);
                Move(FFileName[1], cFileName, Min(Length(FFileName),
                  sizeof(cFileName) - 1));
              end;
            end;
          end
          else
          begin
            Move(FData^, LPtrLocal^, LIntDataSize);
          end;
        except
          Result := E_OUTOFMEMORY;
          GlobalFree(medium.hGlobal);
          medium.hGlobal := 0;
        end;
      finally
        GlobalUnlock(medium.hGlobal);
      end;
    end;
  end;
end;

// what's this?

function TMPHDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
end;

// do we support the desired format?

function TMPHDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := DV_E_FORMATETC;
  with formatetc do
  begin
    if dwAspect <> DVASPECT_CONTENT then
      Result := DV_E_DVASPECT
    else if not Bool(tymed and TYMED_HGLOBAL)
      {// multiple tymeds may be queried (e.g. from explorer, wordpad...)}then
      Result := DV_E_TYMED
    else if (lindex <> -1) and ((cfFormat <> CF_FILECONTENTS) and (Lindex <> 0))
      then
      Result := DV_E_LINDEX
    else if (cfFormat = CF_MPHEXEDITOR) or (cfFormat = CF_TEXT) or
      (cfFormat = CF_FILEDESCRIPTOR) or (cfFormat = CF_FILECONTENTS) then
      Result := S_OK;
  end;
end;

// check for dropeffect calls (dodragdrop not always return the real effect)

function TMPHDataObject.SetData(const formatetc: TFormatEtc; var medium:
  TStgMedium; fRelease: BOOL): HResult;
var
  LPtrEffect: PDWORD;
begin
  Result := E_NOTIMPL;
  if ((formatetc.cfFormat = CF_PERFORMEDDROPEFFECT) or (formatetc.cfFormat =
    CF_LOGICALPERFORMEDDROPEFFECT)) and (medium.tymed = TYMED_HGLOBAL) then
  begin
    Result := S_OK;
    // check drop effect
    LPtrEffect := GlobalLock(medium.hGlobal);
    try
      FHasDropEffect := True;
      FDropEffect := LPtrEffect^;
    finally
      GlobalUnLock(medium.hGlobal);
    end;
  end;
  if fRelease then
    ReleaseStgMedium(medium);
end;

{ TMPHCanvasPrinter }

// init

constructor TMPHCanvasPrinter.Create(AEditor: TMPHexEditorEx; ACanvas: TCanvas;
  AFlags: TMPHPrintFlags; AMargins: TRect; AHeaders: TMPHPrintHeaders);
begin
  inherited Create;
  FMargins := AMargins;
  FCanvas := ACanvas;
  FFlags := AFlags;
  FEditor := AEditor;
  FHeaders[0] := AHeaders[0];
  FHeaders[1] := AHeaders[1];
  GetLinesPerPage;
end;

// convert %s variables

function TMPHCanvasPrinter.BuildHeader(const S: string; const Page: integer):
  string;
var
  LIntLoop: integer;
begin
  Result := '';
  LIntLoop := 1;
  while LIntLoop <= Length(S) do
  begin
    if (S[LIntLoop] = '%') and (LIntLoop < Length(S)) then
    begin
      Inc(LIntLoop);
      case S[LIntLoop] of
        'f': Result := Result + ExtractFileName(FEditor.Filename);
        'F': Result := Result + FEditor.Filename;
        'p': Result := Result + IntToRadix(Page, 10);
        'P': Result := Result + IntToRadix(FPages, 10);
        't': Result := Result + TimeToStr(now);
        'd': Result := Result + DateToStr(now);
        '>':
          begin
            if not FEditor.UnicodeChars then
              Result := Result + MPHTranslationDesc
                [FEditor.Translation]
            else
            begin
              if not FEditor.UnicodeBigEndian then
                Result := Result + MPH_UC
              else
                Result := Result + MPH_UC_BE
            end;
          end;
        '<':
          begin
            if not FEditor.UnicodeChars then
              Result := Result +
                MPHTranslationDescShort
                [FEditor.Translation]
            else
            begin
              if not FEditor.UnicodeBigEndian then
                Result := Result + MPH_UC_S
              else
                Result := Result + MPH_UC_BE_S
            end;
          end
      else
        Result := Result + '%' + S[LIntLoop];
      end;
    end
    else
      Result := Result + S[LIntLoop];
    Inc(LIntLoop);
  end;
end;

// calculate and draw page

procedure TMPHCanvasPrinter.Draw(const Page: integer);
begin
  DrawOrCalc(False, Page);
end;

type
  // text and color attributes per character
  TCellAttribute = record
    Back: TColor;
    Fore: TColor;
    Bold: boolean;
  end;

  TCellAttributes = array of TCellAttribute;

  TTextWithAttr = record
    Text: WideString;
    Attributes: TCellAttributes;
  end;

  // calculate lines per page and/or draw page

function TMPHCanvasPrinter.DrawOrCalc(const JustCalc: boolean; const Page:
  integer): integer;

  // return one line of data
  function GetOneLine(CurPosition, EndPosition: integer; const MinLen: integer):
      TTextWithAttr;

    // add spacer
    procedure AddSpacer(UseDefAttr: boolean = False);
    begin
      Result.Text := Result.Text + ' ';
      SetLength(Result.Attributes, Length(Result.Attributes) + 1);
      if UseDefAttr or (Length(Result.Attributes) = 1) then
        with Result.Attributes[Length(Result.Attributes) - 1] do
        begin
          Bold := False;
          Fore := FEditor.Font.Color;
          Back := FEditor.Colors.Background;
        end
      else
        Result.Attributes[Length(Result.Attributes) - 1] :=
          Result.Attributes[Length(Result.Attributes) - 2]
    end;

    // get hex representation of data (or empty if > datasize)
    function GetByteHex(CurPosition, EndPosition: integer): string;
    begin
      if CurPosition > EndPosition then
        Result := '  '
      else
      begin
        if FEditor.HexLowerCase then
          Result := LowerCase(IntToRadixLen(FEditor.Data[CurPosition], 16, 2))
        else
          Result := UpperCase(IntToRadixLen(FEditor.Data[CurPosition], 16, 2));
        if FEditor.SwapNibbles and (Length(Result) = 2) then
          Result := Result[2] + Result[1];
      end;
    end;
  var
    LIntLoop,
      LIntLoopAttr: integer;
    LStrPart: string;
    LWChrText: WideChar;
    lBold: boolean;
    lOdd: boolean;
    lFore,
      lBack: TColor;
  begin
    Application.ProcessMessages;
    LStrPart := FEditor.GetOffsetString(CurPosition);

    if LStrPart <> '' then
    begin
      LStrPart := StringOfChar(' ', MinLen - Length(LStrPart)) + LStrPart;
      if (not (pfUseBackgroundColor in FFlags)) or (pfMonochrome in FFlags) then
        LStrPart := LStrPart + ':';
      LStrPart := LStrPart + ' ';
    end;

    Result.Text := LStrPart;
    SetLength(Result.Attributes, Length(Result.Text));
    lBold := (FEditor.Row - FEditor.FixedRows) = (CurPosition div
      FEditor.BytesPerRow);
    for lIntLoop := 1 to Length(Result.Text) do
      with Result.Attributes[lIntLoop - 1] do
      begin
        Bold := lBold;
        if (lIntLoop = Length(Result.Text)) or (not (pfUseBackgroundColor in
          FFlags)) then
        begin
          if (lIntLoop = Length(Result.Text)) and (pfUseBackgroundColor in
            FFlags) then
            Bold := False;
          Fore := FEditor.Font.Color;
          Back := FEditor.Colors.Background;
        end
        else
        begin
          if lBold then
          begin
            if (pfUseBackgroundColor in FFlags) and not (pfMonochrome in FFlags)
              then
              Bold := False;
            Fore := FEditor.Colors.CurrentOffset;
            Back := FEditor.Colors.CurrentOffsetBackground;
          end
          else
          begin
            Fore := FEditor.Colors.Offset;
            Back := FEditor.Colors.OffsetBackground;
          end;
        end;
      end;

    LFore := FEditor.Colors.OddColumn;
    if FEditor.InCharField then
      LBack := FEditor.Colors.Background
    else
      lBack := FEditor.Colors.ActiveFieldBackground;
    lOdd := True;
    for LIntLoop := 1 to FEditor.BytesPerRow do
    begin
      LStrPart := GetByteHex(CurPosition - 1 + LIntLoop, EndPosition);
      Result.Text := Result.Text + LStrPart;
      LIntLoopAttr := Length(Result.Attributes);
      SetLength(Result.Attributes, Length(Result.Attributes) +
        Length(LStrPart));

      for LIntLoopAttr := LIntLoopAttr to Pred(Length(Result.Attributes)) do
        with Result.Attributes[LIntLoopAttr] do
        begin
          Bold := FEditor.IsSelected(CurPosition - 1 + LIntLoop);
          Fore := LFore;
          Back := LBack;
          if FEditor.ByteChanged[CurPosition - 1 + LIntLoop] then
          begin
            Fore := FEditor.Colors.ChangedText;
            Back := FEditor.Colors.ChangedBackGround;
          end;

        end;

      if LIntLoop < FEditor.BytesPerRow then
      begin

        if (FEditor.BytesPerBlock > 1) and ((LIntLoop mod FEditor.BytesPerBlock)
          = 0) then
          AddSpacer;

        if (LIntLoop mod FEditor.BytesPerColumn) = 0 then
        begin
          AddSpacer;
          lOdd := not lOdd;
          if lOdd then
          begin
            LFore := FEditor.Colors.OddColumn;
            if FEditor.InCharField then
              LBack := FEditor.Colors.Background
            else
              lBack := FEditor.Colors.ActiveFieldBackground;
          end
          else
          begin
            LFore := FEditor.Colors.EvenColumn;
            if FEditor.InCharField then
              LBack := FEditor.Colors.Background
            else
              lBack := FEditor.Colors.ActiveFieldBackground;
          end;
        end;
      end;
    end;

    AddSpacer(True);
    AddSpacer(True);

    if not FEditor.UnicodeChars then
    begin
      for LIntLoop := 1 to FEditor.BytesPerRow do
      begin
        if (CurPosition + LIntLoop - 1) > EndPosition then
          Result.Text := Result.Text + ' '
        else
          Result.Text := Result.Text +
            FEditor.TranslateToAnsiChar(FEditor.Data[CurPosition + LIntLoop -
            1]);

        SetLength(Result.Attributes, Length(Result.Attributes) + 1);

        with Result.Attributes[Pred(Length(Result.Attributes))] do
        begin
          Bold := FEditor.IsSelected(CurPosition - 1 + LIntLoop);
          if FEditor.ByteChanged[CurPosition - 1 + LIntLoop] then
          begin
            Fore := FEditor.Colors.ChangedText;
            Back := FEditor.Colors.ChangedBackGround;
          end
          else
          begin
            Fore := FEditor.Font.Color;
            if not FEditor.InCharField then
              Back := FEditor.Colors.Background
            else
              Back := FEditor.Colors.ActiveFieldBackground;
          end;

        end;

        if LIntLoop < FEditor.BytesPerRow then
        begin
          if (FEditor.BytesPerBlock > 1) and FEditor.SeparateBlocksInCharField and
            ((LIntLoop mod FEditor.BytesPerBlock) = 0) then
            AddSpacer;

          if (FEditor.UsedRulerBytesPerUnit <> 1) and
            ((LIntLoop mod FEditor.UsedRulerBytesPerUnit) = 0) then
            AddSpacer;
        end;
      end;
    end
    else
      for LIntLoop := 0 to Pred(FEditor.BytesPerRow) div 2 do
      begin
        if (CurPosition + (LIntLoop * 2)) > EndPosition then
          Result.Text := Result.Text + ' '
        else
        begin
          FEditor.ReadBuffer(LWChrText, CurPosition + (LIntLoop * 2), 2);
          if FEditor.UnicodeBigEndian then
            SwapWideChar(LWChrText);
          if (LWChrText < #256) and (Char(LWChrText) in FEditor.MaskedChars)
            then
            LWChrText := WideChar(FEditor.MaskChar);
          Result.Text := Result.Text + LWChrText;
        end;

        SetLength(Result.Attributes, Length(Result.Attributes) + 1);

        with Result.Attributes[Pred(Length(Result.Attributes))] do
        begin
          Bold := FEditor.IsSelected(CurPosition + (LIntLoop * 2));
          if FEditor.ByteChanged[CurPosition + (LIntLoop * 2)] or
            FEditor.ByteChanged[(CurPosition + (LIntLoop * 2)) + 1] then
          begin
            Fore := FEditor.Colors.ChangedText;
            Back := FEditor.Colors.ChangedBackGround;
          end
          else
          begin
            Fore := FEditor.Font.Color;
            if not FEditor.InCharField then
              Back := FEditor.Colors.Background
            else
              Back := FEditor.Colors.ActiveFieldBackground;
          end;
        end;

        if LIntLoop < FEditor.BytesPerRow then
        begin
          if (FEditor.BytesPerBlock > 1) and FEditor.SeparateBlocksInCharField
            and
            (((LIntLoop + 1) mod (FEditor.BytesPerBlock div 2)) = 0) then
            AddSpacer;

          if (FEditor.UsedRulerBytesPerUnit <> 2) and (((LIntLoop * 2) mod
            FEditor.UsedRulerBytesPerUnit) = 0) then
            AddSpacer;
        end;
      end;
  end;

  // return ruler line
  function GetRulerLine(MinLen: integer): TTextWithAttr;

    // add spacer
    procedure AddSpacer;
    begin
      Result.Text := Result.Text + ' ';
      SetLength(Result.Attributes, Length(Result.Attributes) + 1);
      with Result.Attributes[Length(Result.Attributes) - 1] do
      begin
        Bold := False;
        Fore := FEditor.Colors.Offset;
        Back := FEditor.Colors.OffsetBackground;
      end
    end;

  var
    LIntLoop: integer;
    LStrPart: string;
    lBold: boolean;
  begin
    Application.ProcessMessages;

    if MinLen > 0 then
    begin
      LStrPart := StringOfChar(' ', MinLen);
      if (not (pfUseBackgroundColor in FFlags)) or (pfMonochrome in FFlags) then
        LStrPart := LStrPart + ' ';
      LStrPart := LStrPart + ' ';
    end;

    Result.Text := LStrPart;
    SetLength(Result.Attributes, Length(Result.Text));
    for lIntLoop := 1 to Length(Result.Text) do
      with Result.Attributes[lIntLoop - 1] do
      begin
        Fore := FEditor.Colors.Offset;
        Back := FEditor.Colors.OffsetBackGround;
      end;

    for lIntLoop := 1 to Length(FEditor.FRulerString) do
    begin
      Result.Text := Result.Text + FEditor.FRulerString[lIntLoop];
      SetLength(Result.Attributes, Succ(Length(Result.Attributes)));
      lBold := (FEditor.Col - 1) = lIntLoop;
      with Result.Attributes[Pred(Length(Result.Attributes))] do
      begin
        Bold := lBold;
        if (lIntLoop = Length(Result.Text)) or (not (pfUseBackgroundColor in
          FFlags)) then
        begin
          if (lIntLoop = Length(Result.Text)) and (pfUseBackgroundColor in
            FFlags) then
            Bold := False;
          Fore := FEditor.Font.Color;
          Back := FEditor.Colors.Background;
        end
        else
        begin
          if lBold then
          begin
            if (pfUseBackgroundColor in FFlags) and not (pfMonochrome in FFlags)
              then
              Bold := False;
            Fore := FEditor.Colors.CurrentOffset;
            Back := FEditor.Colors.CurrentOffsetBackground;
          end
          else
          begin
            Fore := FEditor.Colors.Offset;
            Back := FEditor.Colors.OffsetBackground;
          end;
        end;
      end;
      if lIntLoop <> Length(FEditor.FRulerString) then
      begin
        if (FEditor.BytesPerBlock > 1) and ((LIntLoop mod (FEditor.BytesPerBlock *
          2)) = 0) then
          AddSpacer;
        if (LIntLoop mod (FEditor.BytesPerColumn * 2)) = 0 then
          AddSpacer;
      end;
    end;

    AddSpacer; AddSpacer;

    for lIntLoop := 1 to Length(FEditor.FRulerCharString) do
    begin
      Result.Text := Result.Text + FEditor.FRulerCharString[lIntLoop];
      SetLength(Result.Attributes, Succ(Length(Result.Attributes)));
      lBold := (FEditor.Col - 2 - (FEditor.BytesPerRow * 2)) = lIntLoop;
      with Result.Attributes[Pred(Length(Result.Attributes))] do
      begin
        Bold := lBold;
        if (lIntLoop = Length(Result.Text)) or (not (pfUseBackgroundColor in
          FFlags)) then
        begin
          if (lIntLoop = Length(Result.Text)) and (pfUseBackgroundColor in
            FFlags) then
            Bold := False;
          Fore := FEditor.Font.Color;
          Back := FEditor.Colors.Background;
        end
        else
        begin
          if lBold then
          begin
            if (pfUseBackgroundColor in FFlags) and not (pfMonochrome in FFlags)
              then
              Bold := False;
            Fore := FEditor.Colors.CurrentOffset;
            Back := FEditor.Colors.CurrentOffsetBackground;
          end
          else
          begin
            Fore := FEditor.Colors.Offset;
            Back := FEditor.Colors.OffsetBackground;
          end;
        end;
      end;
      if lIntLoop <> Length(FEditor.FRulerCharString) then
      begin
        if not FEditor.UnicodeChars then
        begin
          if (FEditor.BytesPerBlock > 1) and FEditor.SeparateBlocksInCharField and
            ((LIntLoop mod FEditor.BytesPerBlock) = 0) then
            AddSpacer;

          if (FEditor.UsedRulerBytesPerUnit <> 1) and
            ((LIntLoop mod FEditor.UsedRulerBytesPerUnit) = 0) then
            AddSpacer;
        end
        else
        begin
          if (FEditor.BytesPerBlock > 1) and FEditor.SeparateBlocksInCharField
            and
            ((LIntLoop mod (FEditor.BytesPerBlock div 2)) = 0) then
            AddSpacer;

          if (FEditor.UsedRulerBytesPerUnit <> 2) and ((((LIntLoop-1) * 2) mod
            FEditor.UsedRulerBytesPerUnit) = 0) then
            AddSpacer;
        end;
      end;
    end;
  end;

  // render a header to the canvas
  procedure DrawHeader(const LeftPos, Y, RightPos: integer; StrText: string);
  var
    LStrLeft, LStrCenter, LStrRight: string;
    LIntPipe, LIntOldBKMode, LIntOldAlign: integer;
    LIntRect: TRect;
  begin
    LStrLeft := '';
    LStrCenter := '';
    LStrRight := '';
    LIntPipe := Pos('|', StrText);
    if LIntPipe > 0 then
    begin
      LStrLeft := Copy(StrText, 1, LIntPipe - 1);
      Delete(StrText, 1, LIntPipe);
      LIntPipe := Pos('|', StrText);
      if LIntPipe > 0 then
      begin
        LStrCenter := Copy(StrText, 1, LIntPipe - 1);
        Delete(StrText, 1, LIntPipe);
        if StrText <> '' then
          LStrRight := StrText;
      end
      else
        LStrCenter := StrText;
    end
    else
      LStrLeft := StrText;

    LIntOldAlign := GetTextAlign(FCanvas.Handle);
    LIntOldBKMode := GetBKMode(FCanvas.Handle);
    try
      SetBKMode(FCanvas.Handle, TRANSPARENT);
      LIntRect := Rect(LeftPos, Y, RightPos, Y+FCanvas.TextHeight('Yy'));
      if LStrRight <> '' then
      begin
        SetTextAlign(FCanvas.Handle, TA_TOP or TA_RIGHT);
        ExtTextOut(FCanvas.Handle, RightPos, Y, ETO_CLIPPED,
          @LIntRect, PChar(LStrRight), Length(LStrRight), nil);
      end;
      if LStrCenter <> '' then
      begin
        SetTextAlign(FCanvas.Handle, TA_TOP or TA_CENTER);
        ExtTextOut(FCanvas.Handle, LeftPos + ((RightPos - LeftPos) div 2), Y, ETO_CLIPPED,
          @LIntRect, PChar(LStrCenter), Length(LStrCenter), nil);
      end;
      if LStrLeft <> '' then
      begin
        SetTextAlign(FCanvas.Handle, TA_TOP or TA_LEFT);
        ExtTextOut(FCanvas.Handle, LeftPos, Y, ETO_CLIPPED,
          @LIntRect, PChar(LStrLeft), Length(LStrLeft), nil);
      end;
    finally
      SetTextAlign(FCanvas.Handle, LIntOldAlign);
      SetBKMode(FCanvas.Handle, LIntOldBKMode);
    end;
  end;
var
  LfntTemp: TFont;
  LRecTextAttr: TTextWithAttr;
  LIntWidth,
    LIntHeight,
    LIntDataPos,
    LIntLeft,
    LIntY,
    LIntMaxY,
    LIntDataEnd,
    LIntDataStart: integer;
  LclrFSave: TColor;
  LclrBSave: TColor;
  LfstSave: TFontStyles;
  LIntLoop: integer;
  LIntMinWidth: integer;
  LRectOut: TRect;
begin
  FPrintHeaders[0] := BuildHeader(FHeaders[0], Page);
  FPrintHeaders[1] := BuildHeader(FHeaders[1], Page);
  Result := -1;
  if (not Assigned(FEditor)) or (FEditor.DataSize < 1) then
    Exit;

  LIntMinWidth := Length(FEditor.GetOffsetString(FEditor.DataSize));

  if (not JustCalc) and (FLinesPerPage < 1) then
    Exit;

  if (pfSelectionOnly in FFlags) and (FEditor.SelCount > 0) then
  begin
    LIntDataEnd := FEditor.SelEnd;
    LIntDataStart := FEditor.SelStart;
    if LIntDataStart > LIntDataEnd then
    begin
      LIntDataStart := FEditor.SelEnd;
      LIntDataEnd := FEditor.SelStart;
    end;
  end
  else
  begin
    if (pfCurrentViewOnly in FFlags) then
    begin
      LIntDataStart := FEditor.DisplayStart;
      LIntDataEnd := FEditor.DisplayEnd;
    end
    else
    begin
      LIntDataStart := 0;
      LIntDataEnd := Pred(FEditor.DataSize);
    end;
  end;

  if not (JustCalc) then
    LIntDataStart := LIntDataStart + ((Page - 1) * (fLinesPerPage *
      FEditor.BytesPerRow));

  if LIntDataStart > LIntDataEnd then
    Exit;

  Result := 0;
// länge einer zeile berechnen
  LRecTextAttr := GetOneLine(LIntDataStart, LIntDataEnd, LIntMinWidth);
  LfntTemp := TFont.Create;
  LfntTemp.Assign(FCanvas.Font);
  try
    if (pfMonochrome in FFlags) or (not (pfUseBackgroundColor in FFlags))
      then
      FCanvas.Brush.Color := clWhite
    else
      FCanvas.Brush.Color := FEditor.Colors.Background;
    FCanvas.Brush.Style := bsSolid;
    if (pfMonochrome in FFlags) then
      FCanvas.Font.Color := clBlack
    else
      FCanvas.Font.Color := FEditor.Font.Color;
    if not JustCalc then
      FCanvas.FillRect(FMargins);
    LIntWidth := FCanvas.TextWidth(LRecTextAttr.Text);
    while (LIntWidth > (FMargins.Right - FMargins.Left)) and
      (FCanvas.Font.Size
      > 1) do
    begin
      FCanvas.Font.Size := FCanvas.Font.Size - 1;
      LIntWidth := FCanvas.TextWidth(LRecTextAttr.Text);
    end;

    LIntHeight := FCanvas.TextHeight(LRecTextAttr.Text);

    LIntDataPos := LIntDataStart;
    LIntY := FMargins.Top;
    LIntMaxY := FMargins.Bottom;
    FPrintHeaders[0] := BuildHeader(FHeaders[0], Page);
    FPrintHeaders[1] := BuildHeader(FHeaders[1], Page);
    if FPrintHeaders[0] <> '' then
    begin
      if not JustCalc then
      begin
        DrawHeader(FMargins.Left, LIntY, FMargins.Right, FPrintHeaders[0]);
        FCanvas.MoveTo(FMargins.Left, LIntY + LIntHeight);
        FCanvas.LineTo(FMargins.Right, LIntY + LIntHeight);
      end;
      LIntY := LIntY + LIntHeight + LIntHeight;
    end;

    if FPrintHeaders[1] <> '' then
      LIntMaxY := LIntMaxY - LIntHeight;

    if (pfIncludeRuler in FFlags) and FEditor.ShowRuler then
    begin
      if not JustCalc then
      begin
        LRecTextAttr := GetRulerLine(LIntMinWidth);

        LclrFSave := FCanvas.Font.Color;
        LclrBSave := FCanvas.Brush.Color;
        LfstSave := FCanvas.Font.Style;
        LIntLeft := FMargins.Left;
        for LIntLoop := 1 to Length(LRecTextAttr.Text) do
        begin
          if not (pfMonochrome in FFlags) then
          begin
            FCanvas.Font.Color := LRecTextAttr.Attributes[LIntLoop -
              1].Fore;
            if pfUseBackgroundColor in FFlags then
              FCanvas.Brush.Color := LRecTextAttr.Attributes[LIntLoop -
                1].Back
            else
              if LRecTextAttr.Attributes[LIntLoop - 1].Fore = clWhite then
                FCanvas.Font.Color := clBlack;
          end;

          if FFlags * [pfSelectionBold, pfSelectionOnly] = [pfSelectionBold]
            then
          begin
            if FFlags * [pfMonochrome, pfUseBackgroundColor] =
              [pfUseBackGroundColor] then
            begin
              FCanvas.Font.Style := [];
              if LRecTextAttr.Attributes[LIntLoop - 1].Bold then
              begin
                FCanvas.Font.Color := ColorToRGB(FCanvas.Font.Color) xor
                  $FFFFFF;
                FCanvas.Brush.Color := ColorToRGB(FCanvas.Brush.Color) xor
                  $FFFFFF;
              end;
            end
            else
            begin
              if LRecTextAttr.Attributes[LIntLoop - 1].Bold then
                FCanvas.Font.Style := [fsBold]
              else
                FCanvas.Font.Style := [];
            end;
          end;

          LRectOut := Rect(LIntLeft, LIntY, LIntLeft +
            FCanvas.TextWidth('w'),
            LIntY + LIntHeight);
          if (not (pfUseBackgroundColor in FFlags)) or (pfMonochrome in
            FFlags) then
            LRectOut.Bottom := LIntY + (LIntHeight * 3 div 2);
          ExtTextOutW(FCanvas.Handle, LIntLeft, LIntY, ETO_CLIPPED or
            ETO_OPAQUE, @LRectOut, @LRecTextAttr.Text[LIntLoop],
            1, nil);
          if (not (pfUseBackgroundColor in FFlags)) or (pfMonochrome in
            FFlags) then
          begin
            FCanvas.MoveTo(LRectOut.Left, LIntY + LIntHeight + 1);
            FCanvas.LineTo(LRectOut.Right + 1, LIntY + LIntHeight + 1);
          end;
          LIntLeft := LRectOut.Right;
        end;
        FCanvas.Font.Color := LclrFSave;
        FCanvas.Brush.Color := LclrBSave;
        FCanvas.Font.Style := LfstSave;

        LRecTextAttr := GetOneLine(LIntDataStart, LIntDataEnd,
          LIntMinWidth);
      end;
      if (not (pfUseBackgroundColor in FFlags)) or (pfMonochrome in FFlags)
        then
        LIntY := LIntY + (LIntHeight * 3 div 2)
      else
        LIntY := LIntY + LIntHeight;
    end;

    while (LIntHeight + LIntY) <= LIntMaxY do
    begin
      if not JustCalc then
      begin
        LclrFSave := FCanvas.Font.Color;
        LclrBSave := FCanvas.Brush.Color;
        LfstSave := FCanvas.Font.Style;
        LIntLeft := FMargins.Left;
        for LIntLoop := 1 to Length(LRecTextAttr.Text) do
        begin
          if not (pfMonochrome in FFlags) then
          begin
            FCanvas.Font.Color := LRecTextAttr.Attributes[LIntLoop -
              1].Fore;
            if pfUseBackgroundColor in FFlags then
              FCanvas.Brush.Color := LRecTextAttr.Attributes[LIntLoop -
                1].Back
            else
              if LRecTextAttr.Attributes[LIntLoop - 1].Fore = clWhite then
                FCanvas.Font.Color := clBlack;
          end;

          if FFlags * [pfSelectionBold, pfSelectionOnly] = [pfSelectionBold]
            then
          begin
            if FFlags * [pfMonochrome, pfUseBackgroundColor] =
              [pfUseBackGroundColor] then
            begin
              FCanvas.Font.Style := [];
              if LRecTextAttr.Attributes[LIntLoop - 1].Bold then
              begin
                FCanvas.Font.Color := ColorToRGB(FCanvas.Font.Color) xor
                  $FFFFFF;
                FCanvas.Brush.Color := ColorToRGB(FCanvas.Brush.Color) xor
                  $FFFFFF;
              end;
            end
            else
            begin
              if LRecTextAttr.Attributes[LIntLoop - 1].Bold then
                FCanvas.Font.Style := [fsBold]
              else
                FCanvas.Font.Style := [];
            end;
          end;

          LRectOut := Rect(LIntLeft, LIntY, LIntLeft +
            FCanvas.TextWidth('w'),
            LIntY + LIntHeight);
          ExtTextOutW(FCanvas.Handle, LIntLeft, LIntY, ETO_CLIPPED or
            ETO_OPAQUE, @LRectOut, @LRecTextAttr.Text[LIntLoop],
            1, nil);
          LIntLeft := LRectOut.Right;
        end;
        FCanvas.Font.Color := LclrFSave;
        FCanvas.Brush.Color := LclrBSave;
        FCanvas.Font.Style := LfstSave;
      end;
      Inc(Result);
      LIntDataPos := LIntDataPos + FEditor.BytesPerRow;
      if LIntDataPos > LIntDataEnd then
      begin
        Break;
      end;
      if not JustCalc then
        LRecTextAttr := GetOneLine(LIntDataPos, LIntDataEnd, LIntMinWidth);
      LIntY := LIntY + LIntHeight;
    end;

    if FPrintHeaders[1] <> '' then
      if not JustCalc then
      begin
        DrawHeader(FMargins.Left, FMargins.Bottom - LIntHeight,
          FMargins.Right,
          FPrintHeaders[1]);
        FCanvas.MoveTo(FMargins.Left, FMargins.Bottom - LIntHeight);
        FCanvas.LineTo(FMargins.Right, FMargins.Bottom - LIntHeight);
      end;

  finally
    FCanvas.Font.Assign(LfntTemp);
    LfntTemp.Free;
  end;
end;

// count number of lines per page (as well as number of pages)

function TMPHCanvasPrinter.GetLinesPerPage: integer;
var
  LIntSize: integer;
  LSetTempFlags: TMPHPrintFlags;
begin
  LSetTempFlags := FFlags;
  Exclude(FFlags, pfSelectionOnly);
  try
    Result := DrawOrCalc(True, 1);
  finally
    FFlags := LSetTempFlags;
  end;
  FLinesPerPage := Result;
  if pfSelectionOnly in FFlags then
    LIntSize := Abs(FEditor.SelStart - FEditor.SelEnd)
  else if pfCurrentViewOnly in FFlags then
  begin
    LIntSize := Abs(FEditor.DisplayEnd - FEditor.DisplayStart);
  end
  else
    LIntSize := FEditor.DataSize;

  while (LIntSize mod FEditor.BytesPerRow) <> 0 do
    Inc(LIntSize);
  LIntSize := LIntSize div FEditor.BytesPerRow;
  while (LIntSize mod FLinesPerPage) <> 0 do
    Inc(LIntSize);
  FPages := LIntSize div FLinesPerPage;
end;

{ TMPHPrintOptions }

// init

constructor TMPHPrintOptions.Create;
begin
  inherited;
  FMargins := MPH_DEF_PRINT_MARGINS;
  FFlags := [pfMonochrome, pfSelectionBold];
end;

// copy props

procedure TMPHPrintOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TMPHPrintOptions then
    with TMPHPrintOptions(Source) do
    begin
      self.FMargins := FMargins;
      self.FHeaders := FHeaders;
      self.FFlags := FFlags;
    end;
end;

// header/footer

function TMPHPrintOptions.GetHeader(const Index: integer): string;
begin
  Result := FHeaders[Index];
end;

// margin (mm)

function TMPHPrintOptions.GetMargin(const Index: integer): integer;
begin
  case Index of
    1: Result := FMargins.Left;
    2: Result := FMargins.Top;
    3: Result := FMargins.Right;
  else
    Result := FMargins.Bottom;
  end;
end;

// set haeder/footer

procedure TMPHPrintOptions.SetHeader(const Index: integer; const Value:
  string);
begin
  FHeaders[Index] := Value;
end;

// set margin (mm)

procedure TMPHPrintOptions.SetMargin(const Index, Value: integer);
begin
  case Index of
    1: FMargins.Left := Value;
    2: FMargins.Top := Value;
    3: FMargins.Right := Value;
  else
    FMargins.Bottom := Value;
  end;
end;

{ TFormatSelDialog }

// ok on list doubleclick

procedure TFormatSelDialog.ListDoubleClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

// enable checkbox if cf_text or cf_oemtext

procedure TFormatSelDialog.ListSelect(Sender: TObject);
begin
  with LlbxFormats do
    LcbxTextAsHex.Enabled := (ItemIndex > -1) and
      (TClipFormat(Items.Objects[ItemIndex]) in [CF_TEXT, CF_OEMTEXT])
end;

initialization
// register clip formats
OleInitialize(nil);
CF_MPHEXEDITOR := RegisterClipboardFormat(PChar(MPTH_CF));
CF_REGEDIT_HEXDATA := RegisterClipboardFormat(CFSTR_REGEDIT_HEXDATA);
CF_RTF := RegisterClipboardFormat(CFSTR_RTF);
CF_HTML := RegisterClipboardFormat(CFSTR_HTML);
CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
CF_FILEDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
CF_PERFORMEDDROPEFFECT := RegisterClipboardFormat(CFSTR_PERFORMEDDROPEFFECT);
CF_LOGICALPERFORMEDDROPEFFECT :=
  RegisterClipboardFormat(CFSTR_LOGICALPERFORMEDDROPEFFECT);

finalization
  OleUninitialize;
end.
