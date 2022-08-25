// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : Main Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit uMain;
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, Dialogs, CommDlg, ImgList, StdActns, ActnList, Menus, ComCtrls,
  Printers, ToolWin, ExtDlgs, ActnMan, Ribbon, ActnCtrls, ActnMenus, RibbonActnMenus,
  RibbonLunaStyleActnCtrls, IEView, IEOpenSaveDlg, ImageENView, ImageEnIO, ImageEnProc,
  HYIEutils, HYIEdefs, ASGCapture;

type
  TFormMain = class ( TForm )
    ASGScreenCapture1: TASGScreenCapture;
    PageControl1: TPageControl;
    ProgressBar1: TProgressBar;
    OpenImageEnDialog1: TOpenImageEnDialog;
    SaveImageEnDialog1: TSaveImageEnDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrinterSetupDialog2: TPrinterSetupDialog;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    SelectPopupMenu: TPopupMenu;
    FlipPopupMenu1: TPopupMenu;
    RotatePopupMenu1: TPopupMenu;
    PopupMenu1: TPopupMenu;
    Rectangle1: TMenuItem;
    Circle1: TMenuItem;
    Polygon3: TMenuItem;
    MagicWand2: TMenuItem;
    Lasso2: TMenuItem;
    N13: TMenuItem;
    Move2: TMenuItem;
    Zoom2: TMenuItem;
    N14: TMenuItem;
    SelectNone2: TMenuItem;
    Vertical2: TMenuItem;
    Horzontal2: TMenuItem;
    Left3: TMenuItem;
    Right2: TMenuItem;
    Close3: TMenuItem;
    N15: TMenuItem;
    Cut1: TMenuItem;
    Copy3: TMenuItem;
    Pastetoselection3: TMenuItem;
    Undo3: TMenuItem;
    N16: TMenuItem;
    Open3: TMenuItem;
    Save3: TMenuItem;
    SaveAs3: TMenuItem;
    N17: TMenuItem;
    Properties4: TMenuItem;
    Effects2: TMenuItem;
    Rotate3: TMenuItem;
    Color1: TMenuItem;
    Brightness1: TMenuItem;
    N18: TMenuItem;
    Exit3: TMenuItem;
    StatusBar1: TStatusBar;
    Crop2: TMenuItem;
    PopupMenu2: TPopupMenu;
    Pastetoselection1: TMenuItem;
    Pasteclipboardintonewimage1: TMenuItem;
    Pasteclipboardtoactiveimage1: TMenuItem;
    OpenImageEnDialog2: TOpenImageEnDialog;
    ToolbarPopupMenu1: TPopupMenu;
    N21: TMenuItem;
    ResizeLayers1: TMenuItem;
    MoveLayers1: TMenuItem;
    ViewMainToolbar2: TMenuItem;
    ViewCaptureToolbar2: TMenuItem;
    ViewSelectToolbar2: TMenuItem;
    ViewMagnifierToolbar2: TMenuItem;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    Ribbon1: TRibbon;
    RibbonPage1: TRibbonPage;
    RibbonPage2: TRibbonPage;
    RibbonPage3: TRibbonPage;
    RibbonPage4: TRibbonPage;
    RibbonPage5: TRibbonPage;
    RibbonPage6: TRibbonPage;
    RibbonPage7: TRibbonPage;
    RibbonPage8: TRibbonPage;
    RibbonPage9: TRibbonPage;
    ActionManager1: TActionManager;
    RibbonQuickAccessToolbar1: TRibbonQuickAccessToolbar;
    RibbonGroup1: TRibbonGroup;
    RibbonGroup2: TRibbonGroup;
    RibbonGroup3: TRibbonGroup;
    RibbonGroup4: TRibbonGroup;
    RibbonGroup5: TRibbonGroup;
    RibbonGroup6: TRibbonGroup;
    RibbonGroup7: TRibbonGroup;
    RibbonGroup8: TRibbonGroup;
    ActionNew1: TAction;
    ActionOpen1: TAction;
    ActionClose1: TAction;
    ActionCloseAll1: TAction;
    RibbonGroup10: TRibbonGroup;
    ActionSave1: TAction;
    ActionSaveAs1: TAction;
    RibbonGroup11: TRibbonGroup;
    ActionPrintPreview1: TAction;
    ActionPrint1: TAction;
    ActionPrintSetup1: TAction;
    ActionCut1: TAction;
    ActionCopy1: TAction;
    ActionPaste1: TAction;
    ActionCrop1: TAction;
    ActionUndo1: TAction;
    ActionRedo1: TAction;
    RibbonGroup14: TRibbonGroup;
    ActionExit1: TAction;
    ActionCaptureDesktop1: TAction;
    ActionSpeedCapture1: TAction;
    ActionCaptureArea1: TAction;
    ActionCaptureObject1: TAction;
    ActionCaptureActive1: TAction;
    ActionCapturePolygon1: TAction;
    ActionCaptureIcon1: TAction;
    ActionCaptureSpecificSizeSelection1: TAction;
    ActionAuto1: TAction;
    ActionSelectRectangle1: TAction;
    ActionSelectEllipse1: TAction;
    ActionSelectPolygon1: TAction;
    ActionMagicWand1: TAction;
    ActionSelectLasso1: TAction;
    ActionSelectZoom1: TAction;
    Action39: TAction;
    Action41: TAction;
    RibbonGroup15: TRibbonGroup;
    ActionFit1: TAction;
    ActionFullScreen1: TAction;
    ActionGrayScale1: TAction;
    ActionBlackAndWhite1: TAction;
    Action16Color1: TAction;
    Action256Color1: TAction;
    ActionTrueColor1: TAction;
    ActionSetTransparentColor1: TAction;
    ActionSelectedAreaAlpha1: TAction;
    ActionSetSelectedColorAsTransparent1: TAction;
    ActionColorAdjust1: TAction;
    ActionRotate1: TAction;
    ActionRotateRight1: TAction;
    ActionResizeCanvas1: TAction;
    ActionResizeImage1: TAction;
    ActionEffects1: TAction;
    Action61: TAction;
    ActionOptionsMinimize1: TAction;
    ActionAutomatic1: TAction;
    ActionShowCursor1: TAction;
    ActionDelay1: TAction;
    ActionCaptureShowHint1: TAction;
    ActionAbout1: TAction;
    ImageList16H1: TImageList;
    ImageList32H1: TImageList;
    ActionPasteFromClipStretch1: TAction;
    ActionClipboardPaste1: TAction;
    ActionSelectNone1: TAction;
    ActionBrightness1: TAction;
    ActionCalcColors1: TAction;
    ActionVerticalFlip1: TAction;
    ActionHorzFlip1: TAction;
    ActionRotateLeft1: TAction;
    ActionProperties1: TAction;
    ActionViewReset1: TAction;
    ActionResetUndo1: TAction;
    ActionSelectionOptions1: TAction;
    ActionMagicWandTolerance1: TAction;
    RibbonGroup9: TRibbonGroup;
    Action32Bit1: TAction;
    ActionShowHighlightedPixel1: TAction;
    ActionDisplayGrid1: TAction;
    ActionClearRecentItems1: TAction;
    RibbonApplicationMenuBar1: TRibbonApplicationMenuBar;
    ActionScroll1: TAction;
    ActionPan1: TAction;
    TaskDialog1: TTaskDialog;
    ActionZoomtoSelection1: TAction;
    CloseAll1: TMenuItem;
    Redo1: TMenuItem;
    PasteFromClipStretch1: TMenuItem;
    ActionNavigator1: TAction;
    ActionShowInfo1: TAction;
    ActionLargeIcon1: TAction;
    ActionSmallIcon1: TAction;
    ActionHelp1: TAction;
    ImageListPageControl1: TImageList;
    procedure FormCreate ( Sender: TObject );
    procedure FormCloseQuery ( Sender: TObject; var CanClose: Boolean );
    procedure PageControl1Change ( Sender: TObject );
    procedure FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormDestroy ( Sender: TObject );
    procedure ScrollBox1MouseWheelDown ( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );
    procedure ScrollBox1MouseWheelUp ( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );
    procedure ActionList1Execute ( Action: TBasicAction; var Handled: Boolean );
    procedure ScrollBar1Change ( Sender: TObject );
    procedure SavePictureDialog1TypeChange ( Sender: TObject );
    procedure ActionOpen1Execute ( Sender: TObject );
    procedure ActionNew1Execute ( Sender: TObject );
    procedure ActionSave1Execute ( Sender: TObject );
    procedure ActionSaveAs1Execute ( Sender: TObject );
    procedure ActionClose1Execute ( Sender: TObject );
    procedure ActionPrintPreview1Execute ( Sender: TObject );
    procedure ActionPrint1Execute ( Sender: TObject );
    procedure ActionCopy1Execute ( Sender: TObject );
    procedure ActionCrop1Execute ( Sender: TObject );
    procedure ActionUndo1Execute ( Sender: TObject );
    procedure ActionRedo1Execute ( Sender: TObject );
    procedure ActionExit1Execute ( Sender: TObject );
    procedure Button1Click ( Sender: TObject );
    procedure ActionPrintSetup1Execute ( Sender: TObject );
    procedure ActionPasteFromClipStretch1Execute ( Sender: TObject );
    procedure ActionClipboardPaste1Execute ( Sender: TObject );
    procedure ActionPaste1Execute ( Sender: TObject );
    procedure ActionCut1Execute ( Sender: TObject );
    procedure ActionCaptureDesktop1Execute ( Sender: TObject );
    procedure ActionCaptureArea1Execute ( Sender: TObject );
    procedure ActionCaptureActive1Execute ( Sender: TObject );
    procedure ActionCaptureIcon1Execute ( Sender: TObject );
    procedure ActionCapturePolygon1Execute ( Sender: TObject );
    procedure ActionSpeedCapture1Execute ( Sender: TObject );
    procedure ActionCaptureSpecificSizeSelection1Execute ( Sender: TObject );
    procedure ActionAuto1Execute ( Sender: TObject );
    procedure ActionAbout1Execute ( Sender: TObject );
    procedure ActionDelay1Execute ( Sender: TObject );
    procedure ActionOptionsMinimize1Execute ( Sender: TObject );
    procedure ActionCaptureObject1Execute ( Sender: TObject );
    procedure ActionSelectPolygon1Execute ( Sender: TObject );
    procedure ActionSelectRectangle1Execute ( Sender: TObject );
    procedure ActionSelectNone1Execute ( Sender: TObject );
    procedure ActionTrueColor1Execute ( Sender: TObject );
    procedure Action256Color1Execute ( Sender: TObject );
    procedure Action16Color1Execute ( Sender: TObject );
    procedure ActionBlackAndWhite1Execute ( Sender: TObject );
    procedure ActionColorAdjust1Execute ( Sender: TObject );
    procedure ActionEffects1Execute ( Sender: TObject );
    procedure ActionBrightness1Execute ( Sender: TObject );
    procedure ActionCalcColors1Execute ( Sender: TObject );
    procedure ActionRotate1Execute ( Sender: TObject );
    procedure ActionRotateRight1Execute ( Sender: TObject );
    procedure ActionResizeImage1Execute ( Sender: TObject );
    procedure ActionResizeCanvas1Execute ( Sender: TObject );
    procedure ActionFullScreen1Execute ( Sender: TObject );
    procedure ActionVerticalFlip1Execute ( Sender: TObject );
    procedure ActionHorzFlip1Execute ( Sender: TObject );
    procedure ActionRotateLeft1Execute ( Sender: TObject );
    procedure Action45Execute ( Sender: TObject );
    procedure ActionMagicWand1Execute ( Sender: TObject );
    procedure ActionProperties1Execute ( Sender: TObject );
    procedure ActionViewReset1Execute ( Sender: TObject );
    procedure ActionResetUndo1Execute ( Sender: TObject );
    procedure ActionSelectEllipse1Execute ( Sender: TObject );
    procedure ActionSelectLasso1Execute ( Sender: TObject );
    procedure ActionSelectMove1Execute ( Sender: TObject );
    procedure ActionSelectZoom1Execute ( Sender: TObject );
    procedure ActionShowCursor1Execute ( Sender: TObject );
    procedure ActionSelectionOptions1Execute ( Sender: TObject );
    procedure ActionGrayScale1Execute ( Sender: TObject );
    procedure ActionSetTransparentColor1Execute ( Sender: TObject );
    procedure ActionSelectedAreaAlpha1Execute ( Sender: TObject );
    procedure ActionMagicWandTolerance1Execute ( Sender: TObject );
    procedure ActionSetSelectedColorAsTransparent1Execute ( Sender: TObject );
    procedure ActionCaptureShowHint1Execute ( Sender: TObject );
    procedure ActiononfigureMagnifier1Execute ( Sender: TObject );
    procedure ActionResizeLayers1Execute ( Sender: TObject );
    procedure ActionFit1Execute ( Sender: TObject );
    procedure Action32Bit1Execute ( Sender: TObject );
    procedure ActionDisplayGrid1Execute ( Sender: TObject );
    procedure ActionShowHighlightedPixel1Execute ( Sender: TObject );
    procedure ActionNumberOfImageColors1Execute ( Sender: TObject );
    procedure Ribbon1RecentItemClick ( Sender: TObject; FileName: string; index: Integer );
    procedure ActionClearRecentItems1Execute ( Sender: TObject );
    procedure Ribbon1HelpButtonClick ( Sender: TObject );
    procedure ActionAutomatic1Execute ( Sender: TObject );
    procedure ActionScroll1Execute ( Sender: TObject );
    procedure ActionPan1Execute ( Sender: TObject );
    procedure ActionZoomtoSelection1Execute ( Sender: TObject );
    procedure ActionNavigator1Execute ( Sender: TObject );
    procedure FormMainActionExit1Execute ( Sender: TObject );
    procedure FormMainActionNew1Execute ( Sender: TObject );
    procedure FormMainActionOpen1Execute ( Sender: TObject );
    procedure FormMainActionReset1Execute ( Sender: TObject );
    procedure ActionCloseAll1Execute ( Sender: TObject );
    procedure ActionShowInfo1Execute ( Sender: TObject );
    procedure ActionLargeIcon1Execute ( Sender: TObject );
    procedure ActionSmallIcon1Execute ( Sender: TObject );
    procedure ActionHelp1Execute(Sender: TObject);
  private
    { Private declarations }
    AIniFileFilename: string;
    ARecentFilesFilename: string;
    ARecentFilesList: TStringList;
    ImageENView: TImageENView;
    FDefaultExtension: string;
    AMsgLanguage: TMsgLanguage;
    ATmpFolder: string;
    procedure AddTabsheet;
    procedure TaskDialogExpanded ( Sender: TObject );
    procedure ShowHint ( Sender: TObject );
    procedure ShowIOParams ( Params: TIOParamsVals );
    // get image info from file
    procedure ShowPropertyIOParams ( Params: TIOParamsVals );
    procedure ImageEnViewDblClick ( Sender: TObject );
    procedure ImageEnViewChange ( Sender: TObject; Change: Integer );
    procedure ImageEnViewMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure ImageEnViewSelectionChange ( Sender: TObject );
    procedure ImageEnViewLayerNotify ( Sender: TObject; layer: Integer; event: TIELayerEvent );
    procedure ImageEnViewMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure ImageEnViewMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure ImageEnViewImageChange ( Sender: TObject );
    procedure ImageEnViewZoomIn ( Sender: TObject; var NewZoom: Double );
    procedure ImageEnViewZoomOut ( Sender: TObject; var NewZoom: Double );
    procedure ImageEnViewDrawLayerGrip ( Sender: TObject; ABitmap: TBitmap; layer, grip: Integer; rect: TRect );
    procedure ImageEnViewProgress ( Sender: TObject; per: Integer );
    procedure ImageEnViewFinishWork ( Sender: TObject );
    procedure UpdateToolbarControls;
    procedure UpdateUndoMenu;
    procedure UpdateStatusBar;
    procedure ClearStatusbar;
    procedure ReadIni ( IniFileFilename: string );
    procedure WriteIni ( IniFileFilename: string );
    procedure ApplicationOnIdle ( Sender: TObject; var Done: Boolean );
    procedure GetCaptureOptions;
  public
    { Public declarations }
    FBitmap: TBitmap;
    FTabSheet: TTabSheet;
    FPathFilename: string;
    FOldFilename: string;
    FDefaultFolder: string;
  end;

var
  FormMain: TFormMain;

implementation

uses
  Math, GifLZW, TIFLZW, ImageEn, INIFiles, Clipbrd, ShellApi, ShlObj, ActiveX,
  HTMLHelpViewer, uFullscrn, uScreenDelay, uConvBW, uRotate, uResize, uStatus, uPrint,
  uProperties, uSelection, uWebSite, uConfigureMagnifier, uAbout, uNavigator, uImport;
{$R *.DFM}

function IsVista: Boolean;
{ Returns true if the operating system is Windows Vista (or later) and false if not. }
var
  iPFunction: Pointer; // pointer to GetProductInfo function if exists
begin

  // Try to load GetProductInfo from Kernel32: present if Vista
  iPFunction := Windows.GetProcAddress ( Windows.GetModuleHandle ( 'kernel32.dll' ), 'GetProductInfo' );
  Result := Assigned ( iPFunction );

end;

function PixelFormatToColors ( PixelFormat: TPixelFormat ): string;
begin

  case PixelFormat of
    pf1bit:
      Result := 'RGB 1-bit';
    pf4bit:
      Result := 'RGB 4-bit';
    pf8bit:
      Result := 'RGB 8-bit';
    pf16bit:
      Result := 'RGB 16-bit';
    pf24bit:
      Result := 'RGB 24-bit';
    pf32bit:
      Result := 'RGBA 32-bit';
  else
    Result := 'Unknown';
  end;

end;

function IEBitDepthToColors ( ABitDepth: Integer ): string;
begin

  case ABitDepth of
    2:
      Result := 'RGB 1-bit';
    4:
      Result := 'RGB 4-bit';
    8:
      Result := 'RGB 8-bit';
    16:
      Result := 'RGB 16-bit';
    24:
      Result := 'RGB 24-bit';
    32:
      Result := 'RGBA 32-bit';
  else
    Result := 'Unknown';
  end;

end;

procedure FreePIDL ( PIDL: ShlObj.PItemIDList );
{ Uses to shell allocator to free the memory used by a given PIDL. }
var
  iMalloc: ActiveX.IMalloc; // shell's allocator
begin

  // Try to get shell allocator
  if Windows.Succeeded ( ShlObj.SHGetMalloc ( iMalloc ) ) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    iMalloc.Free ( PIDL );

end;

function IsSpecialFolderSupported ( CSIDL: Integer ): Boolean;
{ Returns true if the given special folder specified by a CSIDL is supported on
  the system and false if not. }
var
  iPIDL: ShlObj.PItemIDList; // PIDL of the special folder
begin

  // Try to get PIDL for folder: fails if not supported
  Result := Windows.Succeeded ( ShlObj.SHGetSpecialFolderLocation ( 0, CSIDL, iPIDL ) );
  if Result then
    // Free the PIDL using shell allocator
    FreePIDL ( iPIDL );

end;

function PIDLToFolderPath ( PIDL: ShlObj.PItemIDList ): string;
{ Returns the full path to a file system folder from a PIDL or '' if the PIDL refers to a virtual folder. }
begin

  // Set max length of return string
  SetLength ( Result, Windows.MAX_PATH );
  // Get the path
  if ShlObj.SHGetPathFromIDList ( PIDL, PChar ( Result ) ) then
    Result := PChar ( Result )
  else
    Result := '';

end;

function SpecialFolderPath ( CSIDL: Integer ): string;
{ Returns the full path to a special file system folder specified by a CSIDL constant FolderID or '' if the special folder is virtual or CSIDL is not
  supported. }
var
  iPIDL: ShlObj.PItemIDList; // PIDL of the special folder
begin

  Result := '';
  // Get PIDL for required folder
  if Windows.Succeeded ( ShlObj.SHGetSpecialFolderLocation ( 0, CSIDL, iPIDL ) ) then
  begin

    try
      // Get path from PIDL
      Result := PIDLToFolderPath ( iPIDL );
    finally
      FreePIDL ( iPIDL );
    end;

  end;

end;

function GetMyAppDataFolder: string;
{ Find AppData folder location }
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_LOCAL_APPDATA ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_LOCAL_APPDATA );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find MyAppData folder location.' );

end;

function GetMyDesktopFolder: string;
{ Find Desktop folder location }
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_DESKTOP ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_DESKTOP );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find Desktop folder location.' );

end;

function GetMyDocumentsFolder: string;
{ Find MyDocuments folder location }
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_PERSONAL ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_PERSONAL );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find MyDocuments folder location.' );

end;

function SplitStr ( const AString: string; ADelim: Char; out AString1, AString2: string ): Boolean;
{ Splits the string S at the first occurence of delimiter character Delim and
  sets AString1 to the sub-string before Delim and AString2 to substring following Delim. If
  Delim is found in string True is returned, while if Delim is not in string
  False is returned, AString1 is set to AString and AString2 is set to ''. }
var
  DelimPos: integer; { position of delimiter in source string }
begin

  // Find position of first occurence of delimter in string
  DelimPos := SysUtils.AnsiPos ( ADelim, AString );
  if DelimPos > 0 then
  begin

    // Delimiter found: do split and return True
    AString1 := Copy ( AString, 1, DelimPos - 1 );
    AString2 := Copy ( AString, DelimPos + 1, MaxInt );
    Result := True;

  end
  else
  begin

    // Delimeter not found: return false and set S1 to whole string
    AString1 := AString;
    AString2 := '';
    Result := False;

  end;

end;

function ExplodeStr ( AString: string; const ADelim: Char; const AList: Classes.TStrings;
  const AAllowEmpty: Boolean = True; const ATrim: Boolean = False ): integer;
{ Splits the string S into a list of strings, separated by Delim, and returns
  the number of strings in the list. If AllowEmpty is true then any empty
  strings are added to the list, while they are ignored if AllowEmpty is false.
  If Trim is true strings have leading and trailing spaces removed. }
var
  Item: string; // current delimted text
  Remainder: string; // remaining unconsumed part of string
  //---------------------------------------------------------------------------

  procedure AddItem;
  begin

    //Adds optionally trimmed item to list if required
    if ( ATrim ) then
      Item := SysUtils.Trim ( Item );
    if ( Item <> '' ) or AAllowEmpty then
      AList.Add ( Item );

  end;
  //---------------------------------------------------------------------------
begin

  // Clear the list
  AList.Clear;
  // Check we have some entries in the string
  if AString <> '' then
  begin

    // Repeatedly split string until we have no more entries
    while SplitStr ( AString, ADelim, Item, Remainder ) do
    begin
      AddItem;
      AString := Remainder;
    end;

    // Add any remaining item
    AddItem;

  end;

  Result := AList.Count;

end;

function CompressWhiteSpace ( const AString: string ): string;
{ Returns a copy of given string with all white space characters replaced by
  space characters and all sequences of white space replaced by a single
  space character. }
var
  Idx: integer; //loops thru all characters in string
  ResCount: integer; //counts number of characters in result string
  PRes: PChar;

  //Pointer to characters in result string
const
  // The white space characters we convert to spaces
  cWhiteSpace = [ #9, #10, #11, #12, #13, ' ' ];
begin

  // Set length of result to length of source string and set pointer to it
  SetLength ( Result, Length ( AString ) );
  PRes := PChar ( Result );
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length ( AString ) do
  begin

{$IFDEF UNICODE}
    if SysUtils.CharInSet ( AString [ Idx ], [ #9, #10, #11, #12, #13, ' ' ] ) then
{$ELSE}
    if AString [ Idx ] in cWhiteSpace then
{$ENDIF}
    begin

      // Current char is white space: replace by space char and count it
      PRes^ := ' ';
      inc ( PRes );
      inc ( ResCount );
      // Skip past any following white space
      inc ( Idx );
{$IFDEF UNICODE}
      while SysUtils.CharInSet ( AString [ Idx ], [ #9, #10, #11, #12, #13, ' ' ] ) do
{$ELSE}
      while AString [ Idx ] in cWhiteSpace do
{$ENDIF}
        inc ( Idx );

    end
    else
    begin

      // Current char is not white space: copy it literally and count it
      PRes^ := AString [ Idx ];
      inc ( PRes );
      inc ( ResCount );
      inc ( Idx );

    end;
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length ( AString ) then
    SetLength ( Result, ResCount );

end;

function GetFirstFolderFromPath ( var AString: string ): string;
// Returns the the first folder in a string
var
  Words: Classes.TStrings;
begin

  Words := TStringList.Create;
  try

    ExplodeStr ( CompressWhiteSpace ( AString ), '\', Words, False, True );
    if Words.Count > 1 then
      Result := Words [ 0 ] + '\' + Words [ 1 ]
    else
      Result := Words [ 0 ];

  finally
    Words.Free;
  end;

end;

function GetLastFolderFromPath ( var AString: string ): string;
// Returns the the last folder in a string
var
  AWordCount: integer;
  Words: Classes.TStrings;
begin

  Words := TStringList.Create;
  try

    AWordCount := ExplodeStr ( CompressWhiteSpace ( AString ), '\', Words, False, True );
    Result := Words [ AWordCount - 1 ];

  finally
    Words.Free;
  end;

end;

// Add a backslash to the end of a string

function AddBackSlash ( value: string ): string;
begin

  if ( value = '' ) then
    Result := ''
  else
  begin

    if ( value [ length ( value ) ] <> '\' ) then
      Result := value + '\'
    else
      Result := value;

  end;

end;

// Returns true if given name is a valid directory and false
// otherwise. DirName can be any file system name (with or
// without trailing path delimiter).

function IsDirectory ( const DirName: string ): Boolean;
var
  iAttr: Integer; // directory's file attributes
begin

  iAttr := SysUtils.FileGetAttr ( DirName );
  Result := ( iAttr <> -1 ) and ( iAttr and SysUtils.faDirectory = SysUtils.faDirectory );

end;

// Returns the given directory with any single trailing
// backslash removed. If  the directory does not end in a
// backslash it is returned unchanged.

function PathToDir ( const Path: string ): string;
begin

  Result := Path;
  if ( Path <> '' ) and ( Path [ length ( Path ) ] = '\' ) then
    Delete ( Result, length ( Result ), 1 );

end;

// Ensures that the given folder and its sub folders exist, and
// creates them if  they do not. Uses recursion.

procedure EnsureFolders ( Path: string );
var
  iSlashPos: Integer; // position of last backslash in path
  iSubPath: string; // immediate parent folder of given path
begin

  // Check there's a path to create
  if length ( Path ) = 0 then
    Exit;
  // Remove any trailing '\'
  Path := PathToDir ( Path );
  // Check if folder exists and quit if it does - we're done
  if IsDirectory ( Path ) then
    Exit;
  // Recursively call routine on immediate parent folder
  // remove bottomost folder from path - ie move up to parent folder
  iSubPath := Path;
  iSlashPos := length ( iSubPath );
  while ( iSlashPos > 2 ) and ( iSubPath [ iSlashPos ] <> '\' ) do
    Dec ( iSlashPos );
  Delete ( iSubPath, iSlashPos, length ( Path ) - iSlashPos + 1 );
  // do recursive call - ensures that parent folder of current path exist
  EnsureFolders ( iSubPath );
  // Create this current folder now we know parent folder exists
  SysUtils.CreateDir ( Path );

end;

// Find Desktop folder location

function GetMyDesktop: string;
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_DESKTOP ) then
  begin

    Result := iPath;
    iPath := SpecialFolderPath ( CSIDL_DESKTOP );

  end
  else
    raise Exception.Create ( 'Could not find Desktop folder location.' );

end;

function AddThousandSeparator ( const S: string; const C: Char ): string;
// Adds a specified thousand separator in the correct location in a string
var
  i: Integer; // Loop through separator position
begin

  Result := S;
  i := Length ( S ) - 2;

  while i > 1 do
  begin
    Insert ( C, Result, i );
    i := i - 3;
  end;

end;

function AddDefThousandSeparator ( const S: string ): string;
// Adds the locale default thousand separator in the correct location in a string
begin
  Result := AddThousandSeparator ( S, SysUtils.ThousandSeparator );
end;

function IntegerToString ( const i: Int64 ): string;
// Adds the locale default thousand separator in the correct location in a number and returns a string
begin
  Result := AddThousandSeparator ( IntToStr ( i ), SysUtils.ThousandSeparator );
end;

procedure TFormMain.ReadIni ( IniFileFilename: string );
var
  i: Integer;
  iIniFile: TIniFile;
begin

  iIniFile := TIniFile.Create ( IniFileFilename );
  with iIniFile do
  begin

    Left := ReadInteger ( 'Main Form', 'Left', 0 );
    Top := ReadInteger ( 'Main Form', 'Top', 0 );
    Width := ReadInteger ( 'Main Form', 'Width', 0 );
    Height := ReadInteger ( 'Main Form', 'Height', 0 );
    if FileExists ( ARecentFilesFilename ) then
    begin

      ARecentFilesList.LoadFromFile ( ARecentFilesFilename );
      for i := 0 to ARecentFilesList.Count - 1 do
        Ribbon1.AddRecentItem ( ARecentFilesList.Strings [ i ] );

    end;

    iIniFile.Free;
  end;

end;

procedure TFormMain.WriteIni ( IniFileFilename: string );
var
  i: Integer;
  iIniFile: TIniFile;
begin

  iIniFile := TIniFile.Create ( IniFileFilename );
  with iIniFile do
  begin

    WriteInteger ( 'Main Form', 'Left', Left );
    WriteInteger ( 'Main Form', 'Top', Top );
    WriteInteger ( 'Main Form', 'Width', Width );
    WriteInteger ( 'Main Form', 'Height', Height );

    for i := 0 to Ribbon1.ApplicationMenu.Menu.RecentItems.Count - 1 do
      if ( ARecentFilesList.IndexOf ( Ribbon1.ApplicationMenu.Menu.RecentItems [ i ].Caption ) < 0 ) then
        // not a duplicate
        ARecentFilesList.Add ( Ribbon1.ApplicationMenu.Menu.RecentItems [ i ].Caption );

    ARecentFilesList.SaveToFile ( ARecentFilesFilename );

  end;

  iIniFile.Free;

end;

procedure TFormMain.ApplicationOnIdle ( Sender: TObject; var Done: Boolean );
begin
  UpdateToolbarControls;
end;

procedure TFormMain.FormCreate ( Sender: TObject );
var
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  AIniFileFilename := AddBackSlash ( GetMyAppDataFolder ) + 'ASG\Apprehend\' + 'ApprehendDemo.ini';
  ARecentFilesFilename := AddBackSlash ( GetMyAppDataFolder ) + 'ASG\Apprehend\' + 'MRF.txt';
  ARecentFilesList := TStringList.Create;
  EnsureFolders ( AddBackSlash ( GetMyAppDataFolder ) + 'ASG\Apprehend\' );
  ReadIni ( AIniFileFilename );
  DefGIF_LZWDECOMPFUNC := GIFLZWDecompress;
  DefGIF_LZWCOMPFUNC := GIFLZWCompress;
  DefTIFF_LZWDECOMPFUNC := TIFFLZWDecompress;
  DefTIFF_LZWCOMPFUNC := TIFFLZWCompress;
  Caption := 'Apprehend ' + ASGScreenCapture1.Version + ' HiComponents TImageEnView Demo';
  SavePictureDialog1.Filter := SaveImageEnDialog1.BuildStrFilter;
  OpenPictureDialog1.Filter := OpenImageEnDialog1.BuildStrFilter;
  OpenImageEnDialog1.FileName := '';
  OpenImageEnDialog1.FilterIndex := 1;
  Application.OnHint := ShowHint;
  ATmpFolder := '';
  FDefaultExtension := '.bmp';
  FBitmap := TBitmap.Create;
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.CHM';
  //C:\Components\Apprehend\Help\Apprehend.chm
  Application.OnIdle := ApplicationOnIdle;

end;

procedure TFormMain.FormDestroy ( Sender: TObject );
begin

  if not IsVista then
  begin

    if Clipboard.HasFormat ( CF_PICTURE ) then
      if TaskMessageDlg ( 'The Clipboard Contains An Image', 'Remove image from Clipboard?', mtConfirmation, [ mbYes, mbNo ], 0 )
        = mrYes then
        Clipboard.Clear;

  end
  else
  begin

    if Clipboard.HasFormat ( CF_PICTURE ) then
    begin

      MessageBeep ( MB_ICONQUESTION );
      TaskDialog1.Buttons.Clear;
      TaskDialog1.Caption := 'Confirmation';
      TaskDialog1.Title := 'Clipboard Has Image';
      TaskDialog1.Text := 'The clipboard contains an image. Remove image from Clipboard?';
      TaskDialog1.ExpandedText :=
        'Select Yes to remove the image from the clipboard or select No to leave the image in the clipboard.';
      TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
      TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
      if TaskDialog1.Execute then
        if TaskDialog1.ModalResult = mrYes then
          Clipboard.Clear;

    end;

  end;

  FBitmap.Free;
  WriteIni ( AIniFileFilename );
  if Assigned ( ARecentFilesList ) then
    ARecentFilesList.Free;

end;

procedure TFormMain.AddTabsheet;
begin

  // Create a new tabsheet
  with PageControl1 do
    FTabSheet := TTabSheet.Create ( Self );
  // Set the tabsheet.pagecontrol to PageControl1
  FTabSheet.PageControl := PageControl1;
  // Set the activepage to tabsheet
  PageControl1.ActivePage := FTabSheet;
  with FTabSheet do
  begin
    // Create an image component
    ImageENView := TImageENView.Create ( Self );
    ImageENView.Parent := FTabSheet;
    ImageENView.Align := alClient;
    ImageENView.Visible := True;
    ImageENView.ShowHint := True;
    ImageENView.Bitmap.Modified := False;
    ImageENView.Scrollbars := ssBoth;
    ImageENView.MouseInteract := [ miSelect ];
    ImageENView.ZoomFilter := rfNone;
    ImageENView.SetChessboardStyle ( 6, bsSolid );
    ImageENView.SelColor1 := clBlack;
    ImageENView.SelColor2 := $00A8FFFF;
    ImageENView.SetSelectionGripStyle ( clBlack, $00A8FFFF, bsSolid, 5, True );
    ImageENView.DelayDisplaySelection := True;
    ImageENView.IO.SimplifiedParamsDialogs := False;
    ImageENView.OnMouseDown := ImageEnViewMouseDown;
    ImageENView.OnMouseMove := ImageEnViewMouseMove;
    ImageENView.OnMouseUp := ImageEnViewMouseUp;
    ImageENView.OnDblClick := ImageEnViewDblClick;
    ImageENView.OnImageChange := ImageEnViewImageChange;
    ImageENView.OnViewChange := ImageEnViewChange;
    ImageENView.OnSelectionChange := ImageEnViewSelectionChange;
    ImageENView.OnDrawLayerGrip := ImageEnViewDrawLayerGrip;
    ImageENView.OnLayerNotify := ImageEnViewLayerNotify;
    ImageENView.OnZoomIn := ImageEnViewZoomIn;
    ImageENView.OnZoomOut := ImageEnViewZoomOut;
    ImageENView.Cursor := 1784;
    ImageENView.Proc.AutoUndo := True;
    ImageENView.Scrollbars := ssBoth;
    ImageENView.ZoomFilter := rfNone;
    ImageENView.BackgroundStyle := iebsChessboard;
    ImageENView.BackGround := clWhite;
    ImageENView.Visible := True;
    ImageENView.EnableAlphaChannel := True;
    ImageENView.IO.SimplifiedParamsDialogs := False;
    ImageENView.IO.Params.BMP_HandleTransparency := True;
    ImageENView.SetChessboardStyle ( 5, bsSolid );
    ImageENView.Proc.AutoUndo := False;
    ImageENView.Proc.UndoLimit := 99;
    ImageENView.Proc.ImageResize ( 300, 300 );
    ImageENView.Proc.AttachedImageEn := ImageENView;
    ImageENView.OnProgress := ImageEnViewProgress;
    ImageENView.OnFinishWork := ImageEnViewFinishWork;
    // Mouse wheel will scroll image of 15 % of component height
    ImageENView.MouseWheelParams.Action := iemwVScroll;
    ImageENView.MouseWheelParams.Variation := iemwPercentage;
    ImageENView.MouseWheelParams.value := 15;
    // Set scrollbar params to match wheel
    ImageENView.HScrollBarParams.LineStep := 15;
    ImageENView.VScrollBarParams.LineStep := 15;
    // Set the caption of the tabsheet
    Caption := Format ( 'Image %d', [ PageControl1.ActivePage.PageIndex ] );
    if ImageENView.Proc.UndoCount > 0 then
      ImageENView.Proc.ClearUndo;
  end;

end;

procedure TFormMain.UpdateToolbarControls;
begin

  ActionClose1.Enabled := PageControl1.PageCount <> 0;
  ActionCloseAll1.Enabled := PageControl1.PageCount > 1;
  ActionSave1.Enabled := PageControl1.PageCount <> 0;
  ActionSaveAs1.Enabled := PageControl1.PageCount <> 0;
  ActionProperties1.Enabled := PageControl1.PageCount <> 0;
  ActionNavigator1.Enabled := PageControl1.PageCount <> 0;
  ActionCopy1.Enabled := PageControl1.PageCount <> 0;
  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    if ImageENView.Proc.UndoCount <> 0 then
      ActionUndo1.Enabled := True
    else
      ActionUndo1.Enabled := False;

  end
  else
  begin

    ActionUndo1.Enabled := False;
    ActionRedo1.Enabled := False;

  end;

  ActionCrop1.Enabled := PageControl1.PageCount <> 0;
  ActionCut1.Enabled := PageControl1.PageCount <> 0;
  ActionFit1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectNone1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectRectangle1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectEllipse1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectPolygon1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectPolygon1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectLasso1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectZoom1.Enabled := PageControl1.PageCount <> 0;
  ActionMagicWand1.Enabled := PageControl1.PageCount <> 0;
  ActionMagicWandTolerance1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectionOptions1.Enabled := PageControl1.PageCount <> 0;
  ActionEffects1.Enabled := PageControl1.PageCount <> 0;
  ActionRotate1.Enabled := PageControl1.PageCount <> 0;
  ActionRotateRight1.Enabled := PageControl1.PageCount <> 0;
  ActionCalcColors1.Enabled := PageControl1.PageCount <> 0;
  ActionGrayScale1.Enabled := PageControl1.PageCount <> 0;
  ActionBlackAndWhite1.Enabled := PageControl1.PageCount <> 0;
  Action16Color1.Enabled := PageControl1.PageCount <> 0;
  Action256Color1.Enabled := PageControl1.PageCount <> 0;
  ActionTrueColor1.Enabled := PageControl1.PageCount <> 0;
  Action32Bit1.Enabled := PageControl1.PageCount <> 0;
  ActionBrightness1.Enabled := PageControl1.PageCount <> 0;
  ActionResizeImage1.Enabled := PageControl1.PageCount <> 0;
  ActionResizeCanvas1.Enabled := PageControl1.PageCount <> 0;
  ActionVerticalFlip1.Enabled := PageControl1.PageCount <> 0;
  ActionProperties1.Enabled := PageControl1.PageCount <> 0;
  ActionFullScreen1.Enabled := PageControl1.PageCount <> 0;
  ActionSetTransparentColor1.Enabled := PageControl1.PageCount <> 0;
  ActionSetTransparentColor1.Enabled := PageControl1.PageCount <> 0;
  ActionSelectedAreaAlpha1.Enabled := PageControl1.PageCount <> 0;
  ActionColorAdjust1.Enabled := PageControl1.PageCount <> 0;
  ActionPrintSetup1.Enabled := PageControl1.PageCount <> 0;
  ActionPrint1.Enabled := PageControl1.PageCount <> 0;
  ActionPrintPreview1.Enabled := PageControl1.PageCount <> 0;
  ScrollBar1.Enabled := PageControl1.PageCount <> 0;
  if PageControl1.PageCount = 0 then
    ClearStatusbar;

end;

procedure TFormMain.ImageEnViewDblClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    FormFullScreen := TFormFullScreen.Create ( Self );
    try

      Screen.Cursor := crHourglass;
      try

        // Copy image to fullscreen image
        FormFullScreen.ImageEnView1.Bitmap.Assign ( ImageENView.Bitmap );
        // Show the image fullscreen
        FormFullScreen.Showmodal;

      finally
        Screen.Cursor := crDefault;
      end;

    finally
      FormFullScreen.Free;
    end;

  end;

end;

function GetcColorString ( iBitsPerPixel: Integer ): string;
begin

  case iBitsPerPixel of
    1:
      Result := '2';
    2:
      Result := '4';
    3:
      Result := '16';
    4:
      Result := '8';
    5:
      Result := '32';
    6:
      Result := '64';
    7:
      Result := '128';
    8:
      Result := '256';
    16:
      Result := '65,536';
    24:
      Result := '16 Million';
    32:
      Result := '32 Million';
  else
    Result := 'Unknown';
  end;

end;

procedure TFormMain.ShowHint ( Sender: TObject );
begin
end;

procedure TFormMain.ShowIOParams ( Params: TIOParamsVals );
var
  iss: string;
  iFileSize: Double;
  iBitsPerPixel: Integer;
begin

  // Initalize string variable and filesize
  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    iss := '';
    with Params do
    begin

      FPathFilename := PageControl1.ActivePage.Hint;
      iFileSize := IEGetFileSize ( FPathFilename );
      if iFileSize <> -1 then
        iss := Format ( 'File size:%10.0g ', [ iFileSize ] ) + ' bytes';
      StatusBar1.Panels [ 1 ].Text := iss;
      iss := 'ColorMapCount: ' + IntToStr ( ColorMapCount ) + ' ';
      StatusBar1.Panels [ 2 ].Text := iss;
      iss := 'BitsPerSample: ' + IntToStr ( BitsPerSample ) + ' ';
      StatusBar1.Panels [ 3 ].Text := iss;
      iBitsPerPixel := BitsPerSample * SamplesPerPixel;
      iss := GetcColorString ( iBitsPerPixel ) + ' colors';
      StatusBar1.Panels [ 4 ].Text := iss;
      iss := 'FileType: ' + FileTypeStr + ' ';
      StatusBar1.Panels [ 5 ].Text := iss;

    end;

  end;

end;

procedure TFormMain.FormCloseQuery ( Sender: TObject; var CanClose: Boolean );
var
  i: Integer;
  j: Integer;
  AButton: TTaskDialogBaseButtonItem;
  BButton: TTaskDialogBaseButtonItem;
  CButton: TTaskDialogBaseButtonItem;
  DButton: TTaskDialogBaseButtonItem;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    for i := 0 to PageControl1.PageCount - 1 do
    begin

      ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

      if ImageENView.Bitmap.Modified then
      begin

        if not IsVista then
        begin
          case MessageDlg ( 'The Image Has Changed Save ' + PageControl1.ActivePage.Hint + ' or Close All?', mtConfirmation, [
            mbYes, mbNo, mbAll, mbCancel ],
              0 ) of
            mrYes:
              ActionSave1Execute ( Self );
            mrNo:
              CanClose := True;
            mrAll:
              begin
                CanClose := True;
                Exit;
              end;
            mrCancel:
              Abort;
          end;

        end
        else
        begin

          TaskDialog1.Buttons.Clear;
          TaskDialog1.Title := 'The Image Has Changed';
          TaskDialog1.Caption := 'Save The Image';
          TaskDialog1.Text := 'Save ' + PageControl1.ActivePage.Hint + '?';
          TaskDialog1.CustomMainIcon.LoadFromFile ( 'Confirm.ico' );
          TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
          TaskDialog1.ExpandButtonCaption := 'Expand';
          TaskDialog1.ExpandedText := 'Select save the image to save the image to a file' + #10#13 +
            'Select exit without saving to close the tab without saving' + #10#13 + 'Select return to the demo to cancel';
          TaskDialog1.OnExpanded := TaskDialogExpanded;
          TaskDialog1.CommonButtons := [ ];
          AButton := TaskDialog1.Buttons.Add;
          AButton.Caption := 'Save the Image';
          AButton.ModalResult := 100;
          BButton := TaskDialog1.Buttons.Add;
          BButton.Caption := 'Return to the Demo';
          BButton.ModalResult := 101;
          CButton := TaskDialog1.Buttons.Add;
          CButton.Caption := 'Exit Without Saving';
          CButton.ModalResult := 102;
          CButton.default := True;

          // Add Close All if more than 1 image
          if PageControl1.PageCount > 1 then
          begin
            DButton := TaskDialog1.Buttons.Add;
            DButton.Caption := 'Close All';
            DButton.ModalResult := 103;
          end;

          if TaskDialog1.Execute then
          begin

            case TaskDialog1.ModalResult of
              100: // Save & Close
                begin

                  ActionSave1Execute ( Self );
                  CanClose := True;
                  ClearStatusbar;

                end;
              101: // Return
                CanClose := False;
              102: // Close Page
                begin

                  // Free the page
                  PageControl1.ActivePage.Free;
                  PageControl1.SelectNextPage ( False );
                  CanClose := True;
                  ClearStatusbar;

                end;
              103: // Close All Pages
                begin

                  // Free all the pages
                  for j := PageControl1.PageCount - 1 downto 0 do
                  begin

                    PageControl1.Pages [ j ].Free;
                    CanClose := True;
                    Exit;

                  end;

                end;

            end;

          end;

        end;

      end;

    end;

  end;

end;

procedure TFormMain.GetCaptureOptions;
begin

  ASGScreenCapture1.Minimize := ActionOptionsMinimize1.Checked;
  ASGScreenCapture1.Auto := ActionAutomatic1.Checked;
  ASGScreenCapture1.ShowCursor := ActionShowCursor1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ActionShowInfo1.Checked;
  ASGScreenCapture1.ShowHint := ActionCaptureShowHint1.Checked;

  FormDelay := TFormDelay.Create ( Self );

  try
    ASGScreenCapture1.Delay := StrToInt ( FormDelay.edScreenDelay.Text );
  finally
    FormDelay.Free;
  end;

end;

procedure TFormMain.ImageEnViewProgress ( Sender: TObject; per: Integer );
begin
  ProgressBar1.Position := per;
end;

procedure TFormMain.ShowPropertyIOParams ( Params: TIOParamsVals );
var
  iss: string;
  idim, ibitcount: Integer;
  iFileSize: Integer;
  iFrames: Integer;
begin

  if Assigned ( Params ) then
  begin

    with Params, FormProperties do
    begin
      txtPath.Caption := '';
      txtFilename.Caption := '';
      txtSize.Caption := '';
      txtColors.Caption := '';
      txtMem.Caption := '';
      txtFileType.Caption := '';
      txtDPI.Caption := '';
      txtDPIY.Caption := '';
      txtColorMapCount.Caption := '';
      iss := ExtractFilePath ( FPathFilename );
      txtPath.Caption := iss;
      iss := ExtractFilename ( FPathFilename );
      txtFilename.Caption := iss;
      // Width X height pixel (frames)
      iss := IntToStr ( Params.Width ) + ' x ' + IntToStr ( Params.Height ) + ' pixel';
      txtSize.Caption := iss;
      // Dpi
      iss := IntToStr ( DpiX ) + ' x ' + IntToStr ( DpiY ) + ' dpi';
      txtDPI.Caption := iss;
      // Xxx colors
      if ( SamplesPerPixel = 4 ) and ( BitsPerSample = 8 ) then
        iss := ' 16 million colors '
      else
        iss := IntToStr ( 1 shl ( SamplesPerPixel * BitsPerSample ) );
      iss := iss + ' ' + iemsg ( IEMSG_COLORS, AMsgLanguage ) + ' (';
      iss := iss + IntToStr ( SamplesPerPixel * BitsPerSample ) + ' bit)';
      txtColors.Caption := iss;
      // File size
      iFileSize := IEGetFileSize ( FPathFilename );
      if iFileSize <> -1 then
        if iFileSize < 1024 then
          iss := 'File: ' + IntToStr ( iFileSize ) + ' bytes'
        else
          iss := 'File: ' + IntToStr ( iFileSize div 1024 ) + ' Kb';
      // Memory size
      if ( SamplesPerPixel = 1 ) and ( BitsPerSample = 1 ) then
        ibitcount := 1
      else
        ibitcount := 24;
      iFrames := 1;
      idim := ( ( ( Width * ibitcount ) + 31 ) div 32 ) * 4 * Height * iFrames;
      if idim < 1024 then
        iss := 'Memory: ' + IntToStr ( idim ) + ' bytes'
      else
        iss := 'Memory: ' + IntToStr ( idim div 1024 ) + ' Kb';
      //
      txtMem.Caption := iss;
      // Compression
      iss := FileTypeStr;
      txtFileType.Caption := iss;

    end;
  end
  else
  begin

    with FormProperties do
    begin

      txtPath.Caption := '';
      txtFilename.Caption := '';
      txtSize.Caption := '';
      txtColors.Caption := '';
      txtMem.Caption := '';
      txtFileType.Caption := '';
      txtDPI.Caption := '';
      txtDPIY.Caption := '';
      txtColorMapCount.Caption := '';

    end;

  end;

end;

procedure TFormMain.PageControl1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if not ImageENView.Bitmap.Empty then
    begin

      FPathFilename := PageControl1.ActivePage.Hint;
      Ribbon1.DocumentName := FPathFilename;
      UpdateStatusBar;

    end;

  end;

end;

procedure TFormMain.Ribbon1HelpButtonClick ( Sender: TObject );
begin
  Application.HelpShowTableOfContents;
end;

procedure TFormMain.Ribbon1RecentItemClick ( Sender: TObject; FileName: string; index: Integer );
begin

  Screen.Cursor := crHourglass;
  try

    FPathFilename := FileName;
    FDefaultFolder := ExtractFileDir ( FPathFilename );
    Ribbon1.DocumentName := FPathFilename;
    SavePictureDialog1.FileName := FPathFilename;
    AddTabsheet;

    if Assigned ( PageControl1.ActivePage ) then
    begin

      ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

      ImageENView.Proc.ClearAllUndo;
      ImageENView.Proc.ClearAllRedo;
      ImageENView.Cursor := crHandPoint;
      // Set MouseInteract
      ImageENView.MouseInteract := ImageENView.MouseInteract + [ miSelect ];

      // If file exists then load it
      if FileExists ( FPathFilename ) then
      begin

        FTabSheet.Caption := ExtractFilename ( FPathFilename );
        PageControl1.ActivePage.Hint := FPathFilename;
        // Load the file
        ImageENView.IO.LoadFromFile ( FPathFilename );
        // Set progress bar position to 0 and hide it
        ProgressBar1.Position := 0;
        ImageENView.IO.ParamsFromFile ( FPathFilename );
        if ImageENView.IO.Params.FileType <> ioUnknown then
          ShowIOParams ( ImageENView.IO.Params );
        UpdateStatusBar;
        ImageENView.Visible := True;
        ProgressBar1.Position := 0;

      end;

    end;

  finally
    Screen.Cursor := crDefault;
  end;

end;
{ ================================== DrawImage ================================= }
// DrawImage strechdraws an image on a canvas
// Since strechdraw spoils the colors we use StretchDIBits
{ ============================================================================== }

procedure DrawImage ( Canvas: TCanvas; DestRect: TRect; ABitmap: TBitmap );
var
  Header, Bits: Pointer;
  HeaderSize: DWORD; // Integer;
  BitsSize: DWORD; // Longint;
begin

  GetDIBSizes ( ABitmap.Handle, HeaderSize, BitsSize );
  Header := AllocMem ( HeaderSize );
  Bits := AllocMem ( BitsSize );
  try
    GetDIB ( ABitmap.Handle, ABitmap.Palette, Header^, Bits^ );
    StretchDIBits ( Canvas.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
      0, 0, ABitmap.Width,
      ABitmap.Height, Bits, TBitmapInfo ( Header^ ), DIB_RGB_COLORS, SRCCOPY );
    { You might want to try DIB_PAL_COLORS instead, but this is well
      beyond the scope of my knowledge. }
  finally
    FreeMem ( Header, HeaderSize );
    FreeMem ( Bits, BitsSize );
  end;

end;

procedure TFormMain.ImageEnViewFinishWork ( Sender: TObject );
begin

  ProgressBar1.Position := ProgressBar1.Max;
  Sleep ( 250 );
  ProgressBar1.Position := 0;

end;

procedure TFormMain.ImageEnViewChange ( Sender: TObject; Change: Integer );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ScrollBar1.Position := Round ( ImageENView.Zoom );
    StatusBar1.Panels [ 6 ].Text := ' Zoom: ' + FloatToStr ( ImageENView.Zoom ) + '%';

  end;

end;

procedure TFormMain.FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Key = vk_ESCAPE then
    begin

      ScrollBar1.Position := 100;
      ImageENView.Zoom := 100;

    end;

  end;

end;

procedure TFormMain.FormMainActionExit1Execute ( Sender: TObject );
begin
  Close;
end;

procedure TFormMain.FormMainActionNew1Execute ( Sender: TObject );
begin

  AddTabsheet;
  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
    PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
    Ribbon1.DocumentName := FPathFilename;
    PageControl1.ActivePage.Hint := FPathFilename;
    FTabSheet.Hint := FPathFilename;
    ImageENView.Hint := FPathFilename;
    ImageENView.Proc.ClearUndo;
    ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
    ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
    ImageENView.IO.Params.FileType := ioBMP;
    ImageENView.IO.Params.Dpi := 300;
    ImageENView.IO.Params.DpiY := 300;
    UpdateStatusBar;

  end;

end;

procedure TFormMain.FormMainActionOpen1Execute ( Sender: TObject );
begin

  OpenPictureDialog1.Title := 'Open Image...';
  OpenPictureDialog1.FileName := '';
  OpenPictureDialog1.InitialDir := FDefaultFolder;
  OpenPictureDialog1.DefaultExt := FDefaultExtension;

  if OpenPictureDialog1.Execute then
  begin

    FPathFilename := OpenPictureDialog1.FileName;
    Ribbon1.AddRecentItem ( FPathFilename );
    FDefaultFolder := ExtractFileDir ( FPathFilename );
    Screen.Cursor := crHourglass;
    try
      AddTabsheet;
      if Assigned ( PageControl1.ActivePage ) then
      begin

        ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
        ImageENView.Proc.ClearAllUndo;
        ImageENView.Proc.ClearAllRedo;
        ImageENView.Cursor := crHandPoint;
        // Set MouseInteract
        ImageENView.MouseInteract := ImageENView.MouseInteract + [ miSelect ];
        // If file exists then load it
        if FileExists ( FPathFilename ) then
        begin

          FTabSheet.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          // Load the file
          ImageENView.IO.LoadFromFile ( FPathFilename );
          // Set progress bar position to 0 and hide it
          ProgressBar1.Position := 0;
          ImageENView.IO.ParamsFromFile ( FPathFilename );
          if ImageENView.IO.Params.FileType <> ioUnknown then
            ShowIOParams ( ImageENView.IO.Params );
          UpdateStatusBar;
          ImageENView.Visible := True;
          ProgressBar1.Position := 0;

        end;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.FormMainActionReset1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ScrollBar1.Position := 100;
    ImageENView.Zoom := 100;
    ActionFit1.Checked := False;

  end;

end;

procedure TFormMain.Button1Click ( Sender: TObject );
begin
  Close;
end;

procedure TFormMain.ImageEnViewMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  P1: TPoint;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // If not in zoom mode then popupmenu
    if ImageENView.MouseInteract <> [ miZoom..miScroll ] then
      if Button = mbRight then
      begin

        // Get cursor position
        GetCursorPos ( P1 );
        PopupMenu1.Popup ( P1.X, P1.Y );

      end;

    ImageENView.SetFocus;

  end;

end;

procedure TFormMain.ImageEnViewMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
var
  AX: Integer;
  AY: Integer;
  SLeft: Integer;
  STop: Integer;
  SRight: Integer;
  SBottom: Integer;
  SHeight: Integer;
  SWidth: Integer;
  P1: TPoint;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    AX := ImageENView.XScr2Bmp ( X );
    AY := ImageENView.YScr2Bmp ( Y );
    // Highlite the pixel
    P1.X := AX;
    P1.Y := AY;
    if ( ActionShowHighlightedPixel1.Checked ) and ( not ( AX >= ImageENView.Bitmap.Width ) ) and ( not ( AY >=
      ImageENView.Bitmap.Height ) ) then
      ImageENView.HighlightedPixel := P1
    else
      ImageENView.HighlightedPixel := Point ( -1, -1 );

    if ( AX >= 0 ) and ( AX <= ImageENView.IEBitmap.Width - 1 ) and ( AY >= 0 ) and ( AY <= ImageENView.IEBitmap.Height - 1 )
      then
    begin

      // Show mouse move coords
      StatusBar1.Panels [ 3 ].Text := ' Horz: ' + IntToStr ( AX + 1 ) + ' pixels ';
      StatusBar1.Panels [ 4 ].Text := ' Vert: ' + IntToStr ( AY + 1 ) + ' pixels ';

    end;

    if ( ImageENView.Selected ) and ( ImageENView.IsPointInsideSelection ( AX, AY ) ) then
    begin

      SLeft := ImageENView.SelX1;
      STop := ImageENView.SelY1;
      SRight := ImageENView.SelX2;
      SBottom := ImageENView.SelY2;
      SHeight := SBottom - STop;
      SWidth := SRight - SLeft;
      if ( SHeight > 0 ) and ( SWidth > 0 ) and ( ImageENView.IsPointInsideSelection ( AX, AY ) ) then
      begin

        // Selected area
        StatusBar1.Panels [ 3 ].Text := 'Selected: ' + IntegerToString ( SHeight ) + ' x ' + IntegerToString ( SWidth ) +
          ' pixels ';
        StatusBar1.Panels [ 4 ].Text := '';

      end;

    end;

  end;

end;

procedure TFormMain.ImageEnViewMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  //
end;

procedure TFormMain.ImageEnViewImageChange ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ScrollBar1.Position := Round ( ImageENView.Zoom );

  end;

end;

procedure TFormMain.UpdateUndoMenu;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    with ImageENView.Proc do
    begin

      // Undo menu
      ActionUndo1.Enabled := UndoCount > 0;
      // Redo menu
      ActionRedo1.Enabled := RedoCount > 0;

    end;

  end;

end;

procedure TFormMain.UpdateStatusBar;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FPathFilename := PageControl1.ActivePage.Hint;
      Ribbon1.DocumentName := FPathFilename;
      // Show image dimensions
      StatusBar1.Panels [ 0 ].Text := ExtractFilePath ( FPathFilename );
      StatusBar1.Panels [ 1 ].Text := ExtractFilename ( FPathFilename );
      StatusBar1.Panels [ 2 ].Text := ' Height: ' + IntegerToString ( ImageENView.Bitmap.Height ) + ' pixels' + '  Width: ' +
        IntegerToString (
        ImageENView.Bitmap.Width )
        + ' pixels ';
      StatusBar1.Panels [ 5 ].Text := 'Color: ' + IEBitDepthToColors ( ImageENView.IO.Params.BitsPerSample *
        ImageENView.IO.Params.SamplesPerPixel );

    end;

  end;

end;

procedure TFormMain.ClearStatusbar;
var
  i: Integer;
begin

  Ribbon1.DocumentName := '';
  for i := StatusBar1.Panels.Count - 1 downto 0 do
    StatusBar1.Panels [ i ].Text := '';

end;

procedure TFormMain.ImageEnViewSelectionChange ( Sender: TObject );
begin

  if ImageEnView.Selected then
    ImageEnView.Hint := '(' + IntegerToString ( ImageEnView.SelX2 - ImageEnView.SelX1 ) + IntegerToString ( ImageEnView.SelY2 -
      ImageEnView.SelY1 ) + ')'
  else
    ImageEnView.Hint := PageControl1.ActivePage.Hint;

end;

procedure TFormMain.ImageEnViewLayerNotify ( Sender: TObject; layer: Integer; event: TIELayerEvent );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    if ( layer = 1 ) and ( event = ielResized ) or ( event = ielResizing ) then
      FormConfigureMagnifier.UpDownMagnifierSize1.Position := ImageENView.Layers [ 1 ].Width;

  end;

end;

procedure TFormMain.SavePictureDialog1TypeChange ( Sender: TObject );
var
  FileType: string;
  FilePath: string;
  FileName: string;
  FileExt: string;
begin

  FileType := SavePictureDialog1.Filter [ SavePictureDialog1.FilterIndex ];
  FilePath := SavePictureDialog1.FileName;
  FileExt := ExtractFileExt ( FilePath );
  FileName := ExtractFilename ( FilePath ) + '.' + FileExt;

end;

procedure TFormMain.ScrollBar1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Show zoom change
    ImageENView.Zoom := ScrollBar1.Position;
    // Show hint
    ScrollBar1.Hint := 'Zoom - ' + IntToStr ( ScrollBar1.Position );
    Application.ActivateHint ( Mouse.CursorPos );

  end;

end;

procedure TFormMain.ScrollBox1MouseWheelDown ( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );
begin
  MousePos.Y := MousePos.Y + 10;
end;

procedure TFormMain.ScrollBox1MouseWheelUp ( Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean );
begin
  MousePos.Y := MousePos.Y - 10;
end;

procedure TFormMain.ActionCut1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Cut selection to clipboard
    ImageENView.Proc.SelCutToClip;
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';
  end;

end;

procedure TFormMain.ActionCopy1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ImageENView.VisibleSelection then
      // Copy selection to clipboard
      ImageENView.Proc.SelCopyToClip
    else
      ImageENView.Proc.CopyToClipboard;

  end;

end;

procedure TFormMain.ActionPan1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Select moving scroll
    ImageENView.MouseInteract := [ miMovingScroll ];

  end;

end;

procedure TFormMain.ActionPaste1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Clipboard.HasFormat ( CF_PICTURE ) then
    begin

      // Save Undo file
      ImageENView.Proc.SaveUndo;
      ImageENView.Proc.ClearAllRedo;
      // Paste from clipboard
      ImageENView.Proc.SelPasteFromClipStretch;
      if ImageENView.Proc.UndoCount > 0 then
        StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
      else
        StatusBar1.Panels [ 6 ].Text := '';
      if PageControl1.PageCount <> 0 then
        if ImageENView.Bitmap.Modified then
          StatusBar1.Panels [ 9 ].Text := 'Modified'
        else
          StatusBar1.Panels [ 9 ].Text := '';
      UpdateUndoMenu;

    end
    else
      MessageDlg ( 'There is no image in the Clipboard.', mtInformation, [ mbOK ], 0 );
  end;

end;

procedure TFormMain.ActionCrop1Execute ( Sender: TObject );
var
  ABitmap: TBitmap;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ImageENView.Selected then
    begin

      // Save undo file
      ImageENView.Proc.SaveUndo;
      ImageENView.Proc.ClearAllRedo;

      // Create a temp bitmap
      ABitmap := TBitmap.Create;
      try
        // Assign selection (crop) to Abitmap
        ImageENView.AssignSelTo ( ABitmap );
        // Copy the bitmap back to the Image component
        ImageENView.Assign ( ABitmap );
        ImageENView.Bitmap.Modified := True;
        ImageENView.Refresh;

      finally
        ABitmap.Free;
      end;

      // Update controls
      UpdateUndoMenu;
      UpdateStatusBar;
      ImageENView.DeSelect;
      ImageENView.Invalidate;

    end
    else
      MessageDlg ( 'Please select an area of the image to crop.', mtInformation, [ mbOK ], 0 );
  end;

end;

procedure TFormMain.ActionUndo1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Undo
    with ImageENView.Proc do
    begin
      SaveRedo; // saves in Redo list
      Undo;
      ClearUndo;
    end;

    UpdateUndoMenu;
    ImageENView.Refresh;

  end;

end;

procedure TFormMain.ActionRedo1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    with ImageENView.Proc do
    begin
      SaveUndo; // saves in Undo List
      Redo;
    end;

    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionExit1Execute ( Sender: TObject );
begin
  Close;
end;

procedure TFormMain.ActionNavigator1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ActionNavigator1.Checked then
    begin

      FormNavigator := TFormNavigator.Create ( Self );
      ImageENView.SetNavigator ( FormNavigator.ImageEnViewNavigator );
      FormNavigator.Show;

    end

    else if Assigned ( FormNavigator ) then
    begin

      ImageENView.SetNavigator ( nil );
      FormNavigator.Free;

    end;

  end;

end;

procedure TFormMain.ActionNew1Execute ( Sender: TObject );
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
    PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
    Ribbon1.DocumentName := FPathFilename;
    PageControl1.ActivePage.Hint := FPathFilename;
    PageControl1.ActivePage.ImageIndex := 0;
    FTabSheet.Hint := FPathFilename;
    ImageENView.Hint := FPathFilename;
    ImageENView.Proc.ClearUndo;
    ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
    ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
    ImageENView.IO.Params.FileType := ioBMP;
    ImageENView.IO.Params.Dpi := 300;
    ImageENView.IO.Params.DpiY := 300;
    UpdateStatusBar;

  end;

end;

procedure TFormMain.ActionNumberOfImageColors1Execute ( Sender: TObject );
begin
  //
end;

procedure TFormMain.ActiononfigureMagnifier1Execute ( Sender: TObject );
begin
  FormConfigureMagnifier.Show
end;

procedure TFormMain.ActionOpen1Execute ( Sender: TObject );
begin

  OpenPictureDialog1.Title := 'Open Image...';
  OpenPictureDialog1.FileName := '';
  OpenPictureDialog1.InitialDir := FDefaultFolder;
  OpenPictureDialog1.DefaultExt := FDefaultExtension;
  // build the savepicture dialog filter
  OpenPictureDialog1.Filter := '';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'All Supported Image Types' +
     '(*.tif; *.gif;*.jpg;*.pcx;*.bmp;*.ico;*.cur;*.png;*.wmf;*.emf;*.tga;*.pxm;*.jp2;*.j2k;*.wbmp;*.dcx;*.hdp;*.wpd)' +
  '*.tif;*.gif;*.jpg;*.pcx;*.bmp;*.ico;*.cur;*.png;*.wmf;*.emf;*.tga;*.pxm;*.jp2;*.j2k;*.wbmp;*.dcx;*.hdp;*.wpd';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'TIFF Bitmap (TIF)|*.tif|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'CompuServe Bitmap (GIF)|*.gif|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'JPeg Bitmap (JPG)|*.jpg|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'PaintBrush (PCX)|*.pcx|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Bitmap (BMP)|*.bmp|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Icon (ICO)|*.ico|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Cursor (CUR)|*.cur|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Portable Network Graphics (PNG)|*.png|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Metafile (WMF)|*.wmf|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Enhanced Windows Metafile (EMF)|*.emf|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Targa Bitmap (TGA)|*.tga|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Portable Pixmap, GreyMap, BitMap (PXM)|*.pxm|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Jpeg2000 (JP2)|*.jp2|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Jpeg2000 (J2K)|*.j2k|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Wireless Bitmap (WBMP)|*.wbmp|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Multipage PCX (DCX)|*.dcx|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Microsoft HD Photo (HDP)|*.hdp|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Microsoft  Media Photo (WPD)|*.wpd;*.wpd|';

  if OpenPictureDialog1.Execute then
  begin

    FPathFilename := OpenPictureDialog1.FileName;
    Ribbon1.AddRecentItem ( FPathFilename );
    Ribbon1.DocumentName := FPathFilename;
    SavePictureDialog1.FileName := FPathFilename;
    FDefaultFolder := ExtractFileDir ( FPathFilename );

    Screen.Cursor := crHourglass;
    try

      AddTabsheet;

      if Assigned ( PageControl1.ActivePage ) then
      begin

        ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

        ImageENView.Proc.ClearAllUndo;
        ImageENView.Proc.ClearAllRedo;
        ImageENView.Cursor := crHandPoint;
        // Set MouseInteract
        ImageENView.MouseInteract := ImageENView.MouseInteract + [ miSelect ];

        // If file exists then load it
        if FileExists ( FPathFilename ) then
        begin

          FTabSheet.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          // Load the file
          ImageENView.IO.LoadFromFile ( FPathFilename );
          // Set progress bar position to 0 and hide it
          ProgressBar1.Position := 0;
          ImageENView.IO.ParamsFromFile ( FPathFilename );
          if ImageENView.IO.Params.FileType <> ioUnknown then
            ShowIOParams ( ImageENView.IO.Params );
          PageControl1.ActivePage.ImageIndex := 1;
          UpdateStatusBar;
          ImageENView.Visible := True;
          ProgressBar1.Position := 0;

        end;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.ActionClose1Execute ( Sender: TObject );
var
  AResult: Integer;
  AButton: TTaskDialogBaseButtonItem;
  BButton: TTaskDialogBaseButtonItem;
  CButton: TTaskDialogBaseButtonItem;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ImageENView.Bitmap.Modified then
    begin

      if not IsVista then
      begin

        AResult := MessageDlg ( 'Save ' + PageControl1.ActivePage.Caption + '?', mtConfirmation, [ mbYes, mbNo, mbCancel ], 0 );

        case AResult of
          mrYes:
            ActionSave1Execute ( Self );
          mrNo:
            begin

              // Free the page
              PageControl1.ActivePage.Free;
              PageControl1.SelectNextPage ( False );

            end;
          mrCancel:
            Abort;
        end;

        if PageControl1.PageCount = 0 then
        begin

        ClearStatusbar;

        end;

      end
      else
      begin

        TaskDialog1.Title := 'The Image Has Changed';
        TaskDialog1.Caption := 'Save The Image';
        TaskDialog1.Text := 'Save ' + PageControl1.ActivePage.Hint + '?';
        TaskDialog1.CustomMainIcon.LoadFromFile ( 'Confirm.ico' );
        TaskDialog1.Flags := [ tfUseHiconMain ];
        TaskDialog1.ExpandButtonCaption := 'Expand';
        TaskDialog1.ExpandedText := 'Select save the image to save the image to a file' + #10#13 +
          'Select close the tabsheet to close the tabsheet' + #10#13 +
          'Select return to the demo to cancel';
        TaskDialog1.OnExpanded := TaskDialogExpanded;
        TaskDialog1.Buttons.Clear;
        AButton := TaskDialog1.Buttons.Add;
        AButton.Caption := 'Save the Image';
        AButton.ModalResult := 100;
        BButton := TaskDialog1.Buttons.Add;
        BButton.Caption := 'Return to the Demo';
        BButton.ModalResult := 101;
        CButton := TaskDialog1.Buttons.Add;
        CButton.Caption := 'Close The Tabsheet';
        CButton.ModalResult := 102;
        CButton.default := True;

        if TaskDialog1.Execute then
        begin

          case TaskDialog1.ModalResult of
            100:
              ActionSave1Execute ( Self );
            101:
              Abort;
            102:
              begin
                // Free the page
                PageControl1.ActivePage.Free;
                PageControl1.SelectNextPage ( False );
              end;
          end;

          ClearStatusbar;

        end;

      end;

    end;

  end
  else
  begin

    // Close the active page
    PageControl1.ActivePage.Free;
    PageControl1.SelectNextPage ( False );

  end;
end;

procedure TFormMain.ActionCloseAll1Execute ( Sender: TObject );
var
  i: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    MessageBeep ( MB_ICONQUESTION );
    TaskDialog1.Buttons.Clear;
    TaskDialog1.Caption := 'Confirmation';
    TaskDialog1.Title := 'Close All Images';
    TaskDialog1.Text := 'Close all images?';
    TaskDialog1.ExpandedText := 'Select Yes to close all tabs or select No to cancel.';
    TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
    TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];

    if TaskDialog1.Execute then
    begin

      if TaskDialog1.ModalResult = mrYes then
      begin

        for i := PageControl1.PageCount - 1 downto 0 do
        begin

          PageControl1.ActivePage := PageControl1.Pages [ i ];
          PageControl1.ActivePage.Free;
          PageControl1.SelectNextPage ( False );

        end;

        FPathFilename := '';

      end
      else if PageControl1.PageCount > 0 then
        for i := PageControl1.PageCount - 1 downto 0 do
        begin

          if ImageENView.Bitmap.Modified then
          begin

            MessageBeep ( MB_ICONQUESTION );
            TaskDialog1.Buttons.Clear;
            TaskDialog1.Caption := 'Confirmation';
            TaskDialog1.Title := 'Save';
            TaskDialog1.Text := 'Save ' + PageControl1.ActivePage.Caption + '?';
            TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
            TaskDialog1.CommonButtons := [ tcbYes, tcbNo, tcbCancel ];

            if TaskDialog1.Execute then

              case TaskDialog1.ModalResult of
                mrYes:
                  ActionSave1Execute ( Self );
                mrNo:
                  begin
                    PageControl1.ActivePage := PageControl1.Pages [ i ];
                    PageControl1.ActivePage.Free; // Close and Free the ActivePage
                    PageControl1.SelectNextPage ( False );
                  end;
                mrCancel:
                  Abort;
              end;

          end;

        end;

    end;

    if PageControl1.PageCount = 0 then
        ClearStatusBar;

  end;

end;

procedure TFormMain.ActionSave1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if not FileExists ( FPathFilename ) then
      FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.ActivePage.PageIndex ]
        );

    with ImageENView.IO do
    begin

      ImageENView.IO.PreviewsParams := [ ioppDefaultLockPreview ];
      ImageENView.IO.DoPreviews ( [ ppAUTO ] );

      Screen.Cursor := crHourglass;
      try
        // Save image to file
        SaveToFile ( FPathFilename );
        // Set tabsheet caption
        PageControl1.ActivePage.Caption := FPathFilename;
        UpdateStatusBar;
        ImageENView.Bitmap.Modified := False;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

    ProgressBar1.Position := 0;
  end;

end;

procedure TFormMain.ActionSaveAs1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    SavePictureDialog1.FileName := FPathFilename;
    SavePictureDialog1.DefaultExt := FDefaultExtension;
    SavePictureDialog1.InitialDir := FDefaultFolder;
    // build the savepicture dialog filter
    SavePictureDialog1.Filter := '';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'TIFF Bitmap (TIF)|*.tif|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'CompuServe Bitmap (GIF)|*.gif|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'JPeg Bitmap (JPG)|*.jpg|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'PaintBrush (PCX)|*.pcx|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Bitmap (BMP)|*.bmp|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Icon (ICO)|*.ico|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Cursor (CUR)|*.cur|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Portable Network Graphics (PNG)|*.png|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Metafile (WMF)|*.wmf|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Enhanced Windows Metafile (EMF)|*.emf|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Targa Bitmap (TGA)|*.tga|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Portable Pixmap, GreyMap, BitMap (PXM)|*.pxm|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Jpeg2000 (JP2)|*.jp2|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Jpeg2000 (J2K)|*.j2k|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'AVI Video (AVI)|*.avi|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Wireless Bitmap (WBMP)|*.wbmp|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Postscript (PS)|*.ps|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Adobe PDF (PDF)|*.pdf|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Multipage PCX (DCX)|*.dcx|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Microsoft HD Photo (HDP)|*.hdp|';
    SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Microsoft  Media Photo (WPD)|*.wpd;*.wpd|';
    // set the filter index to match the filetype
    SavePictureDialog1.FilterIndex := Ord ( ImageENView.IO.Params.FileType );

    if SavePictureDialog1.Execute then
    begin

      Screen.Cursor := crHourglass;
      try

        FPathFilename := SavePictureDialog1.FileName;
        ImageENView.IO.SaveToFile ( FPathFilename );
        UpdateStatusBar;
        Ribbon1.DocumentName := FPathFilename;
        ProgressBar1.Position := 0;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.ActionScroll1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Select scroll
    ImageENView.MouseInteract := [ miScroll ];

  end;

end;

procedure TFormMain.ActionCaptureDesktop1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );

          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 2;
          Ribbon1.DocumentName := FPathFilename;
          FTabSheet.Hint := FPathFilename;
          ImageENView.Hint := FPathFilename;
          ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
          ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
          ImageENView.IO.Params.FileType := ioBMP;
          ImageENView.IO.Params.Dpi := 300;
          ImageENView.IO.Params.DpiY := 300;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureArea1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;


  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      // Capture area of screen
      iBitmap := ASGScreenCapture1.CaptureSelection;
      try
        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 4;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCalcColors1Execute ( Sender: TObject );
var
  i: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Calc # colors and show it
    i := ImageENView.Proc.CalcImageNumColors;
    MessageDlg ( 'The active image has ' + IntToStr ( i ) + ' colors.', mtInformation, [ mbOK ], 0 );

  end;
end;

procedure TFormMain.ActionCaptureActive1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( PageControl1.ActivePage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureActiveWindow;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          Ribbon1.DocumentName := FPathFilename;
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 6;
          UpdateStatusbar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureIcon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 8;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureObject1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( PageControl1.ActivePage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureObject;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 5;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCapturePolygon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 7;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionSpeedCapture1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.SpeedCaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 3;
          Ribbon1.DocumentName := FPathFilename;
          FTabSheet.Hint := FPathFilename;
          ImageENView.Hint := FPathFilename;
          ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
          ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
          ImageENView.IO.Params.FileType := ioBMP;
          ImageENView.IO.Params.Dpi := 300;
          ImageENView.IO.Params.DpiY := 300;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureSpecificSizeSelection1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try

        if Assigned ( iBitmap ) then
          ImageENView.Assign ( iBitmap )
        else
        begin

          PageControl1.ActivePage.Free;
          exit;

        end;

      finally;
        iBitmap.Free;
      end;

      FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
      Ribbon1.DocumentName := FPathFilename;
      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FPathFilename;
      PageControl1.ActivePage.ImageIndex := 11;
      ImageENView.Hint := FPathFilename;
      ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
      ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
      ImageENView.IO.Params.FileType := ioBMP;
      ImageENView.IO.Params.Dpi := 300;
      ImageENView.IO.Params.DpiY := 300;
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.ActionAuto1Execute ( Sender: TObject );
var
  MyBitmap: TBitmap;
  AExceptionString: string;
begin

  if ActionOptionsMinimize1.Checked then
    WindowState := wsMinimized;

  ASGScreenCapture1.Auto := ActionAutomatic1.Checked;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    MyBitmap := ASGScreenCapture1.CaptureObjectByHWND_AutoScroll ( 0, 0, AExceptionString );
    MyBitmap.PixelFormat := pf24bit;
    ImageENView.Assign ( MyBitmap );
    ImageENView.Bitmap.Modified := True;
    FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
    PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
    PageControl1.ActivePage.Hint := FPathFilename;
    Ribbon1.DocumentName := FPathFilename;
    FTabSheet.Hint := FPathFilename;
    ImageENView.Hint := FPathFilename;
    ImageENView.IO.Params.Height := ImageENView.Bitmap.Height;
    ImageENView.IO.Params.Width := ImageENView.Bitmap.Width;
    ImageENView.IO.Params.FileType := ioBMP;
    ImageENView.IO.Params.Dpi := 300;
    ImageENView.IO.Params.DpiY := 300;
    UpdateStatusBar;
    MyBitmap.Free;

  end;

end;

procedure TFormMain.ActionAutomatic1Execute ( Sender: TObject );
begin
  //
end;

procedure TFormMain.ActionAbout1Execute ( Sender: TObject );
begin

  FormAbout := TFormAbout.Create ( self );
  try

    FormAbout.ShowModal;

  finally
    FormAbout.Free;
  end;

end;

procedure TFormMain.ActionDelay1Execute ( Sender: TObject );
begin

  FormDelay := TFormDelay.Create ( Self );
  try

    FormDelay.edScreenDelay.Text := IntToStr ( ASGScreenCapture1.Delay );
    if FormDelay.Showmodal = mrOK then
      ASGScreenCapture1.Delay := StrToInt ( FormDelay.edScreenDelay.Text );

  finally
    FormDelay.Free;
  end;

end;

procedure TFormMain.ActionDisplayGrid1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ImageENView.DisplayGrid := ActionDisplayGrid1.Checked;

  end;

end;

procedure TFormMain.ActionOptionsMinimize1Execute ( Sender: TObject );
begin
  ASGScreenCapture1.Minimize := ActionOptionsMinimize1.Checked;
end;

procedure TFormMain.ActionSelectNone1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Select none
    with ImageENView do
    begin

      DeSelect;
      Cursor := crHandPoint;
      // Set mouseinteract
      MouseInteract := MouseInteract + [ miScroll ];

    end;

  end;

end;

procedure TFormMain.ActionSelectPolygon1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Select polygon
    ImageENView.MouseInteract := [ miSelectPolygon ];
    ImageENView.Cursor := 1785;

  end;

end;

procedure TFormMain.ActionSelectRectangle1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Select rect
    ImageENView.MouseInteract := [ miSelect ];
    ImageENView.Cursor := 1785;

  end;

end;

procedure TFormMain.ActionTrueColor1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Set image to active page
    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Convert image
    ImageENView.Proc.ConvertTo24Bit;
    ImageENView.IO.Params.BitsPerSample := 8;
    ImageENView.IO.Params.SamplesPerPixel := 3;
    ProgressBar1.Position := 0;
    ImageENView.Refresh;
    UpdateStatusBar;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionBrightness1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Show color adjust dialog
    ImageENView.Proc.PreviewsParams := [ prppDefaultLockPreview, prppShowResetButton, prppHardReset ];
    ImageENView.Proc.DoPreviews ( ppeColorAdjust );
    // Set progress bar
    ProgressBar1.Position := 0;

  end;

end;

procedure TFormMain.ActionVerticalFlip1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // set Image to active page
    ImageENView.IO.AttachedImageEn := ImageENView;
    // Flip it
    ImageENView.Proc.Flip ( fdVertical );
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';

  end;

end;

procedure TFormMain.ActionViewReset1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ScrollBar1.Position := 100;
    ImageENView.Zoom := 100;
    ActionFit1.Checked := False;

  end;

end;

procedure TFormMain.ActionZoomtoSelection1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Zoom
    ImageENView.MouseInteract := [ miSelectZoom ];
    ImageENView.Cursor := 1779;

  end;

end;

procedure TFormMain.ActionRotateLeft1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Rotate the image, with antialiasing
    ImageENView.Proc.Rotate ( 90, True );
    ProgressBar1.Position := 0;
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.Action256Color1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Convert to 256 colors
    ImageENView.Proc.ConvertTo ( 256 );
    ImageENView.IO.Params.BitsPerSample := 4;
    ImageENView.IO.Params.SamplesPerPixel := 2;
    ProgressBar1.Position := 0;
    ImageENView.Refresh;
    UpdateStatusBar;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.Action32Bit1Execute ( Sender: TObject );
var
  RGB: TRGB;
  BX, BY: Integer;
  fExt: string;
  fTransparentColor: TColor;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    if Assigned ( ImageENView.IEBitmap ) then
    begin

      ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

      ImageENView.Proc.SaveUndo ( ieuImage );
      ImageENView.IO.Params.BitsPerSample := 8;
      ImageENView.IO.Params.SamplesPerPixel := 4;
      ImageENView.EnableAlphaChannel := True;
      ImageENView.Update;

      with ImageENView.Proc do
      begin
        BX := 0;
        BY := ImageENView.IEBitmap.Height - 1;
        RGB := ImageENView.IEBitmap.Pixels [ BX, BY ];
        SetTransparentColors ( RGB, RGB, 0 );
        fTransparentColor := TRGB2TColor ( RGB );
      end;

      fExt := LowerCase ( ExtractFileExt ( FPathFilename ) );
      if fExt = '.bmp' then
        ImageENView.Bitmap.TransparentColor := fTransparentColor;
      if fExt = '.cur' then
        ImageENView.IO.Params.CUR_Background := TColor2TRGB ( fTransparentColor );
      if fExt = '.ico' then
        ImageENView.IO.Params.ICO_Background := TColor2TRGB ( fTransparentColor );
      if ( fExt = '.gif' ) and ( ImageENView.IO.Params.GIF_FlagTranspColor ) then
        ImageENView.IO.Params.GIF_TranspColor := TColor2TRGB ( fTransparentColor );
      if fExt = '.png' then
        ImageENView.IO.Params.PNG_Background := TColor2TRGB ( fTransparentColor );
      if fExt = '.tga' then
        ImageENView.IO.Params.TGA_Background := TColor2TRGB ( fTransparentColor );

      ImageENView.Update;
      ImageENView.Bitmap.Modified := True;
      UpdateStatusBar;
      UpdateUndoMenu;

    end;

  end;

end;

procedure TFormMain.ActionResizeLayers1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Allows user to resize layers
    ImageENView.MouseInteract := [ miResizeLayers ];

  end;

end;

procedure TFormMain.ActionSelectZoom1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Zoom
    ImageENView.MouseInteract := [ miZoom, miScroll ];
    ImageENView.Cursor := 1779;

  end;

end;

procedure TFormMain.ActionSelectMove1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Allows user to move layers
    ImageENView.MouseInteract := [ miMoveLayers ];

  end;

end;

procedure TFormMain.ActionSelectLasso1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    ImageENView.MouseInteract := [ miSelectLasso ];
    ImageENView.Cursor := 1785;

  end;

end;

procedure TFormMain.ActionSelectEllipse1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );
    // Select circle
    ImageENView.MouseInteract := [ miSelectCircle ];
    ImageENView.Cursor := 1785;

  end;

end;

procedure TFormMain.ActionSelectionOptions1Execute ( Sender: TObject );
begin
  FormSelection.Showmodal;
end;

procedure TFormMain.ActionMagicWand1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Select magic wand
    ImageENView.MouseInteract := [ miSelectMagicWand ];
    ImageENView.Cursor := 1785;

  end;

end;

procedure TFormMain.ActionMagicWandTolerance1Execute ( Sender: TObject );
var
  MagicWandTolerance: Integer;
  MagicWandToleranceStr: string;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    MagicWandToleranceStr := IntToStr ( ImageENView.MagicWandTolerance );
    MagicWandToleranceStr := InputBox ( 'Selection Tolerance', 'Tolerance', MagicWandToleranceStr );
    MagicWandTolerance := StrToInt ( MagicWandToleranceStr );
    ImageENView.MagicWandTolerance := MagicWandTolerance;

  end;

end;

procedure TFormMain.Action45Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    FormProperties := TFormProperties.Create ( Self );
    try

      with FormProperties do
      begin

        ImageEnView1.Assign ( ImageENView.Bitmap );
        ShowPropertyIOParams ( ImageENView.IO.Params );
        Showmodal;

      end;

    finally
      FormProperties.Free;
    end;

  end;

end;

procedure TFormMain.ActionGrayScale1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Process image
    ImageENView.Proc.ConvertToGray;
    ImageENView.IO.Params.BitsPerSample := 8;
    ImageENView.IO.Params.SamplesPerPixel := 3;
    ProgressBar1.Position := 0;
    ImageENView.Refresh;
    UpdateStatusBar;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionFit1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ActionFit1.Checked then
      ImageENView.Fit
    else
      ImageENView.Zoom := 100;

    ScrollBar1.Position := Round ( ImageENView.Zoom );
    // Show hint
    ScrollBar1.Hint := 'Zoom - ' + IntToStr ( ScrollBar1.Position );

  end;

end;

procedure TFormMain.ActionFullScreen1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    Screen.Cursor := crHourglass;
    try

      FormFullScreen := TFormFullScreen.Create ( Self );
      try

        // Copy image to fullscreen image
        FormFullScreen.ImageEnView1.Bitmap.Assign ( ImageENView.Bitmap );
        // Show the image fullscreen
        FormFullScreen.Showmodal;

      finally
        FormFullScreen.Free;
      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.ActionHelp1Execute(Sender: TObject);
var
  iResult: boolean;
begin

  iResult := Application.HelpShowTableOfContents;
  if not iResult then
  begin

    TaskDialog1.Title := 'Error';
    TaskDialog1.Caption := 'Error ';
    TaskDialog1.Text := 'Can not find help file, ' + Application.HelpFile;
    TaskDialog1.CommonButtons := [ tcbOk ];
    TaskDialog1.Flags := [ tfAllowDialogCancellation ];
    TaskDialog1.MainIcon := tdiError;
    TaskDialog1.Execute;

  end;

end;

procedure TFormMain.ActionHorzFlip1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Set image to active page
    ImageENView.IO.AttachedImageEn := ImageENView;
    // Flip it
    ImageENView.Proc.Flip ( fdHorizontal );
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';

  end;

end;

procedure TFormMain.ActionSetTransparentColor1Execute ( Sender: TObject );
var
  RGBColor: TRGB;
  BX, BY: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    BX := 0;
    BY := ImageENView.IEBitmap.Height - 1;
    RGBColor := ImageENView.IEBitmap.Pixels [ BX, BY ];
    ImageENView.Proc.SetTransparentColors ( RGBColor, RGBColor, 0 );

  end;

  UpdateUndoMenu;

end;

procedure TFormMain.ActionSelectedAreaAlpha1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ImageENView.Selected then
      ImageENView.SetSelectedAreaAlpha ( 0 );
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionSetSelectedColorAsTransparent1Execute ( Sender: TObject );
var
  RGBColor: TRGB;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if ColorDialog1.Execute then
    begin

      // Save undo file
      ImageENView.Proc.SaveUndo;
      ImageENView.Proc.ClearAllRedo;
      RGBColor := TColor2TRGB ( ColorDialog1.Color );
      ImageENView.Proc.SetTransparentColors ( RGBColor, RGBColor, 0 );

    end;

  end;

end;

procedure TFormMain.ActionResizeCanvas1Execute ( Sender: TObject );
var
  w, h: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Create and show resize dialog
    ResizeForm := TResizeForm.Create ( Self );
    try

      // Setup resize dialog
      ResizeForm.OrgWidth := ImageENView.Bitmap.Width;
      ResizeForm.OrgHeight := ImageENView.Bitmap.Height;
      ResizeForm.Caption := 'Canvas';
      ResizeForm.Caption := '  Canvas Size';
      ResizeForm.Resize := True;
      // Copy images to resize dialog
      ResizeForm.ImageEnView1.Assign ( ImageENView.Bitmap );
      ResizeForm.ImageEnProc1.SaveUndo;
      ImageENView.Proc.ClearAllRedo;

      // Show resize dialog
      if ResizeForm.Showmodal = mrOK then
      begin

        // Setup and show status form
        FormStatus.lblStatus.Caption := 'Resizing canvas...';
        FormStatus.Show;
        FormStatus.lblStatus.Update;
        w := StrToIntDef ( ResizeForm.Edit1.Text, 0 );
        h := StrToIntDef ( ResizeForm.Edit2.Text, 0 );
        if ( w > 0 ) and ( h > 0 ) then
          ImageENView.Proc.ImageResize ( w, h );
        // Reset progress bar, menu and statusbar
        ProgressBar1.Position := 0;
        FormStatus.Hide;
        UpdateStatusBar;
        if ImageENView.Proc.UndoCount > 0 then
          StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
        else
          StatusBar1.Panels [ 6 ].Text := '';
        if PageControl1.PageCount <> 0 then
          if ImageENView.Bitmap.Modified then
            StatusBar1.Panels [ 9 ].Text := 'Modified'
          else
            StatusBar1.Panels [ 9 ].Text := '';

      end;

    finally
      ResizeForm.Free;
    end;

  end;

end;

procedure TFormMain.ActionCaptureShowHint1Execute ( Sender: TObject );
begin
  ASGScreenCapture1.ShowHint := ActionCaptureShowHint1.Checked;
end;

procedure TFormMain.ActionShowCursor1Execute ( Sender: TObject );
begin
  ASGScreenCapture1.ShowCursor := ActionShowCursor1.Checked;
end;

procedure TFormMain.ActionShowHighlightedPixel1Execute ( Sender: TObject );
begin
  //
end;

procedure TFormMain.ActionShowInfo1Execute ( Sender: TObject );
begin
  ASGScreenCapture1.ShowInfoDialog := ActionShowInfo1.Checked;
end;

procedure TFormMain.ActionSmallIcon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSmallIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 10;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionResetUndo1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    ImageENView.Proc.ClearAllUndo;

  end;

end;

procedure TFormMain.ActionResizeImage1Execute ( Sender: TObject );
var
  w, h: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Create and show resize dialog
    ResizeForm := TResizeForm.Create ( Self );
    try

      // Setup resize dialog
      ResizeForm.OrgWidth := ImageENView.Bitmap.Width;
      ResizeForm.OrgHeight := ImageENView.Bitmap.Height;
      ResizeForm.Caption := 'Resize';
      ResizeForm.Caption := '  Resize (Resample) image';
      ResizeForm.Resize := False;
      ResizeForm.Resample := True;
      // Copy images to resize dialog
      ResizeForm.ImageEnView1.Assign ( ImageENView.Bitmap );
      ResizeForm.ImageEnProc1.SaveUndo;
      ImageENView.Proc.ClearAllRedo;

      // Show resize dialog
      if ResizeForm.Showmodal = mrOK then
      begin

        // Setup and Show Status Form
        FormStatus.lblStatus.Caption := 'Resizing image...';
        FormStatus.Show;
        FormStatus.lblStatus.Update;
        w := StrToIntDef ( ResizeForm.Edit1.Text, 0 );
        h := StrToIntDef ( ResizeForm.Edit2.Text, 0 );
        if ( w > 0 ) and ( h > 0 ) then
          ImageENView.Proc.Resample ( w, h, TResampleFilter ( ResizeForm.ComboBox1.ItemIndex ) );
        // Copy resized image back to main form
        ImageENView.Bitmap.Assign ( ResizeForm.ImageEnView1.Bitmap );
        // Reset progress bar, menu and status bar
        ProgressBar1.Position := 0;
        FormStatus.Hide;

      end
      else
      begin

        // Reset progress bar, menu and status bar
        ProgressBar1.Position := 0;
        FormStatus.Hide;

      end;

      // Show image dimensions
      StatusBar1.Panels [ 2 ].Text := ' Height: ' + IntToStr ( ImageENView.Bitmap.Height ) + ' pixels' + '  Width: ' + IntToStr (
        ImageENView.Bitmap.Width )
        + ' pixels ';
      if ImageENView.Proc.UndoCount > 0 then
        StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
      else
        StatusBar1.Panels [ 6 ].Text := '';
      if PageControl1.PageCount <> 0 then
        if ImageENView.Bitmap.Modified then
          StatusBar1.Panels [ 9 ].Text := 'Modified'
        else
          StatusBar1.Panels [ 9 ].Text := '';

    finally
      ResizeForm.Free;
    end;

  end;

end;

procedure TFormMain.ActionRotate1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    FormRotate := TFormRotate.Create ( Self );
    try

      FormRotate.ImageEnView1.Assign ( ImageENView );
      // Show rotate dialog
      if FormRotate.Showmodal = mrOK then
        ImageENView.Proc.Rotate ( FormRotate.UpDown1.Position, FormRotate.Checkbox1.Checked );
      if ImageENView.Proc.UndoCount > 0 then
        StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
      else
        StatusBar1.Panels [ 6 ].Text := '';
      if PageControl1.PageCount <> 0 then
        if ImageENView.Bitmap.Modified then
          StatusBar1.Panels [ 9 ].Text := 'Modified'
        else
          StatusBar1.Panels [ 9 ].Text := '';
      ProgressBar1.Position := 0;

    finally
      FormRotate.Free;
    end;

  end;

end;

procedure TFormMain.ActionRotateRight1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Rotate the image, with antialiasing
    ImageENView.Proc.Rotate ( -90, True );
    ProgressBar1.Position := 0;
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';

    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionEffects1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    if ImageENView.Proc.UndoCount > 0 then
      StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
    else
      StatusBar1.Panels [ 6 ].Text := '';
    if PageControl1.PageCount <> 0 then
      if ImageENView.Bitmap.Modified then
        StatusBar1.Panels [ 9 ].Text := 'Modified'
      else
        StatusBar1.Panels [ 9 ].Text := '';
    // Show effects dialog
    ImageENView.Proc.PreviewsParams := [ prppDefaultLockPreview, prppShowResetButton, prppHardReset ];
    if ImageENView.Proc.DoPreviews ( ppeEffects ) = True then
      // Set progress bar
      ProgressBar1.Position := 0;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionColorAdjust1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Show color adjust dialog
    ImageENView.Proc.PreviewsParams := [ prppDefaultLockPreview, prppShowResetButton, prppHardReset ];
    ImageENView.Proc.DoPreviews ( ppeColorAdjust );
    ProgressBar1.Position := 0;

  end;

end;

procedure TFormMain.ActionBlackAndWhite1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;

    FormConvBW := TFormConvBW.Create ( Self );
    try

      // Show BW Dialog and process image
      with FormConvBW do
        if Showmodal = mrOK then
        begin

          Application.ProcessMessages;
          case RadioGroup1.ItemIndex of
            0:
              begin // Threshold
                if SpeedButton1.Down then
                  ImageENView.Proc.ConvertToBWThreshold ( -1 )
                else
                  ImageENView.Proc.ConvertToBWThreshold ( StrToIntDef ( Edit1.Text, 0 ) );
              end;
            1:
              begin // Ordered
                ImageENView.Proc.ConvertToBWOrdered;
              end;
          end;

          ImageENView.IO.Params.BitsPerSample := 1;
          ImageENView.IO.Params.SamplesPerPixel := 1;

        end;

    finally
      FormConvBW.Free;
    end;

    ProgressBar1.Position := 0;
    ImageENView.Refresh;
    UpdateStatusBar;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.Action16Color1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    // Save undo file
    ImageENView.Proc.SaveUndo;
    ImageENView.Proc.ClearAllRedo;
    // Convert to 16 colors
    ImageENView.Proc.ConvertTo ( 16 );
    ImageENView.IO.Params.BitsPerSample := 4;
    ImageENView.IO.Params.SamplesPerPixel := 1;
    ProgressBar1.Position := 0;
    ImageENView.Refresh;
    UpdateStatusBar;
    UpdateUndoMenu;

  end;

end;

procedure TFormMain.ActionPasteFromClipStretch1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Clipboard.HasFormat ( CF_PICTURE ) then
    begin

      // Save Undo file
      ImageENView.Proc.SaveUndo;
      ImageENView.Proc.ClearAllRedo;
      // Paste from clipboard
      ImageENView.Proc.SelPasteFromClipStretch;
      if ImageENView.Proc.UndoCount > 0 then
        StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
      else
        StatusBar1.Panels [ 6 ].Text := '';
      if PageControl1.PageCount <> 0 then
        if ImageENView.Bitmap.Modified then
          StatusBar1.Panels [ 9 ].Text := 'Modified'
        else
          StatusBar1.Panels [ 9 ].Text := '';
      UpdateUndoMenu;

    end

    else
      MessageDlg ( 'There is no image in the Clipboard.', mtInformation, [ mbOK ], 0 );

  end;

end;

procedure TFormMain.ActionClearRecentItems1Execute ( Sender: TObject );
begin

  MessageBeep ( MB_ICONQUESTION );
  TaskDialog1.Buttons.Clear;
  TaskDialog1.Caption := 'Confirmation';
  TaskDialog1.Title := 'Clear Recent Items';
  TaskDialog1.Text := 'Clear the recent items list?';
  TaskDialog1.ExpandedText :=
    'Select Yes to clear the filenames from the recent menu items, select No to keep the recent items intact.';
  TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
  TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
  if TaskDialog1.Execute then
    if TaskDialog1.ModalResult = mrYes then
      Ribbon1.ClearRecentItems;

end;

procedure TFormMain.ActionClipboardPaste1Execute ( Sender: TObject );
var
  Bitmap: TBitmap;
  BMH, BMW: Integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Clipboard.HasFormat ( CF_PICTURE ) then
    begin

      // Create new image
      AddTabsheet;
      if PageControl1.PageCount <> 0 then
        // Save Undo file
        ImageENView.Proc.SaveUndo;
      ImageENView.Proc.ClearAllRedo;
      // Save Undo file
      // Paste

      Bitmap := TBitmap.Create;
      { create bitmap to hold the contents on the Clipboard }
      try

        Bitmap.Assign ( Clipboard ); { get the bitmap off the Clipboard }
        BMH := Bitmap.Height;
        BMW := Bitmap.Width;
        ImageENView.Proc.ImageResize ( BMW, BMH );
        ImageENView.Proc.PasteFromClipboard;
        ImageENView.Refresh;

      finally
        Bitmap.Free;
      end;

      UpdateUndoMenu;
      ImageENView.Bitmap.Modified := True;
      if ImageENView.Proc.UndoCount > 0 then
        StatusBar1.Panels [ 6 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount )
      else
        StatusBar1.Panels [ 6 ].Text := '';
      if PageControl1.PageCount <> 0 then
        if ImageENView.Bitmap.Modified then
          StatusBar1.Panels [ 9 ].Text := 'Modified'
        else
          StatusBar1.Panels [ 9 ].Text := '';
      if not ImageENView.Bitmap.Empty then
        FPathFilename := PageControl1.ActivePage.Hint;

    end
    else
      MessageDlg ( 'There is no image in the Clipboard.', mtInformation, [ mbOK ], 0 );

  end;

end;

procedure TFormMain.ActionPrintPreview1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    ImageENView.IO.DoPrintPreviewDialog ( iedtMaxi, '' );

  end;

end;

procedure TFormMain.ActionPrint1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    Screen.Cursor := crHourglass;
    try

      with Printer do
      begin

        BeginDoc;
        // Here we draw the image on the printercanvas, scaled up by a factor 2
        DrawImage ( Canvas, rect ( 0, 0, 2 * ImageENView.Bitmap.Width, 2 * ImageENView.Bitmap.Height ), ImageENView.Bitmap );
        EndDoc;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;
end;

procedure TFormMain.ActionPrintSetup1Execute ( Sender: TObject );
begin
  PrinterSetupDialog1.Execute;
end;

procedure TFormMain.ActionProperties1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    with FormProperties do
    begin

      if FileExists ( FPathFilename ) then
      begin

        ImageEnView1.Assign ( ImageENView.Bitmap );
        ShowPropertyIOParams ( ImageENView.IO.Params );
        Showmodal;

      end
      else
        MessageDlg ( 'Image must be opened from disk to enable properties dialog.', mtInformation, [ mbOK ], 0 );

    end;
  end;

end;

procedure TFormMain.ActionLargeIcon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureLargeIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Assign ( iBitmap );
          ImageENView.Bitmap.Modified := True;
          FPathFilename := GetMyDocumentsFolder + Format ( '\Capture %d' + FDefaultExtension, [ PageControl1.PageCount ] );
          PageControl1.ActivePage.Caption := ExtractFilename ( FPathFilename );
          PageControl1.ActivePage.Hint := FPathFilename;
          PageControl1.ActivePage.ImageIndex := 9;
          Ribbon1.DocumentName := FPathFilename;
          UpdateStatusBar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionList1Execute ( Action: TBasicAction; var Handled: Boolean );
begin
  UpdateToolbarControls;
end;

procedure TFormMain.TaskDialogExpanded ( Sender: TObject );
begin

  if TaskDialog1.Expanded = True then
    TaskDialog1.ExpandButtonCaption := 'Collapse'
  else
    TaskDialog1.ExpandButtonCaption := 'Expand';

end;

procedure TFormMain.ImageEnViewZoomIn ( Sender: TObject; var NewZoom: Double );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    ImageENView.Cursor := 1779;
    FormNavigator.Button1.Hint := FloatToStr ( NewZoom ) + '%';
    FormNavigator.Button2.Hint := FloatToStr ( NewZoom ) + '%';
    FormNavigator.Caption := 'Navigator- ' + FloatToStr ( NewZoom ) + '%';
    FormNavigator.Update;
    StatusBar1.Panels [ 4 ].Text := ' Zoom: ' + FloatToStr ( NewZoom ) + '%';

  end;

end;

procedure TFormMain.ImageEnViewZoomOut ( Sender: TObject; var NewZoom: Double );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( PageControl1.ActivePage.Controls [ 0 ] );

    ImageENView.Cursor := 1778;
    FormNavigator.Button1.Hint := FloatToStr ( NewZoom ) + '%';
    FormNavigator.Button2.Hint := FloatToStr ( NewZoom ) + '%';
    FormNavigator.Caption := 'Navigator- ' + FloatToStr ( NewZoom ) + '%';
    FormNavigator.Update;
    StatusBar1.Panels [ 4 ].Text := ' Zoom: ' + FloatToStr ( NewZoom ) + '%';

  end;

end;

procedure TFormMain.ImageEnViewDrawLayerGrip ( Sender: TObject; ABitmap: TBitmap; layer, grip: Integer; rect: TRect );
begin

  with ABitmap.Canvas do
  begin

    Pen.Style := psClear;
    Pen.Mode := pmCopy;
    Pen.Color := clBlack;
    Brush.Style := bsClear;
    Brush.Color := $00A8FFFF;

  end;

end;

end.

