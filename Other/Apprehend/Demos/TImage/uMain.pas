// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 03-23-2012
// Description             : Unit1
// Compiler                : Delphi 2010
// Notes                   : All captured and opened graphics are loaded as TBitmap to preserve drawing canvas
//                         : All graphics are converted to destination format from the TImage.Picture.Bitmap for saving
// This file is copyright (c) W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit uMain;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Math, ASGCapture, ImgList, Menus, ExtDlgs, ExtCtrls,
  ToolWin, StdCtrls, Buttons, Printers, Jpeg, PNGImage, GIFImg;

type

  TFormMain = class ( TForm )
    ToolBar2: TToolBar;
    OpenItem: TToolButton;
    CloseItem: TToolButton;
    CloseAllItem: TToolButton;
    SaveItem: TToolButton;
    ToolButton3: TToolButton;
    SaveAsItem: TToolButton;
    CopyItem: TToolButton;
    PasteItem: TToolButton;
    CropItem: TToolButton;
    ToolButton7: TToolButton;
    CaptureDesktopItem: TToolButton;
    CaptureAreaItem: TToolButton;
    CaptureActiveItem: TToolButton;
    CaptureObjectItem: TToolButton;
    ExitItem: TToolButton;
    SavePictureDialog1: TSavePictureDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenItem1: TMenuItem;
    CloseItem1: TMenuItem;
    CloseAllItem1: TMenuItem;
    SaveItem1: TMenuItem;
    SaveAsItem1: TMenuItem;
    ExitItem1: TMenuItem;
    Edit1: TMenuItem;
    CopyItem1: TMenuItem;
    PasteItem1: TMenuItem;
    CropItem1: TMenuItem;
    UndoItem1: TMenuItem;
    Capture1: TMenuItem;
    DesktopItem1: TMenuItem;
    AreaItem1: TMenuItem;
    ObjectItem1: TMenuItem;
    Miminized1: TMenuItem;
    MinimizeItem1: TMenuItem;
    AutomaticItem1: TMenuItem;
    DelayItem1: TMenuItem;
    Help1: TMenuItem;
    AboutItem1: TMenuItem;
    ASGScreenCapture1: TASGScreenCapture;
    UndoItem: TToolButton;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    ProgressBar1: TProgressBar;
    CopyToSel1: TMenuItem;
    N2: TMenuItem;
    BlackandWhite1: TMenuItem;
    N16Color1: TMenuItem;
    N256Color1: TMenuItem;
    N15Bit1: TMenuItem;
    N16Bit1: TMenuItem;
    N24Bit1: TMenuItem;
    N32Bit1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    CopySelection1: TMenuItem;
    Paste1: TMenuItem;
    Crop2: TMenuItem;
    ColorDepth2: TMenuItem;
    Undo2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N6: TMenuItem;
    Device2: TMenuItem;
    N16Color2: TMenuItem;
    N256Color2: TMenuItem;
    N15BitHighColor1: TMenuItem;
    N16BitHighColor1: TMenuItem;
    N24BitColor1: TMenuItem;
    N32BitColor1: TMenuItem;
    BlackandWhite2: TMenuItem;
    N7: TMenuItem;
    Colors1: TMenuItem;
    Close1: TMenuItem;
    PrintItem: TToolButton;
    ToolButton24: TToolButton;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintItem1: TMenuItem;
    N1: TMenuItem;
    CursorItem1: TMenuItem;
    N5: TMenuItem;
    PrintSetupItem1: TMenuItem;
    CapturePolygonItem: TToolButton;
    FreeForm1: TMenuItem;
    Website1: TMenuItem;
    Image1: TImage;
    ShowHintItem1: TMenuItem;
    ToolBar4: TToolBar;
    PopupMenuMagnifier1: TPopupMenu;
    Magnifier3: TMenuItem;
    ShowBorders2: TMenuItem;
    ToolBar1: TToolBar;
    CoolBar1: TCoolBar;
    ToolBar3: TToolBar;
    CaptureWholeDesktopItem: TToolButton;
    DualMonitorDesktop1: TMenuItem;
    CaptureIconItem: TToolButton;
    CaptureIcon1: TMenuItem;
    CaptureSelectionItem: TToolButton;
    HelpContents1: TMenuItem;
    N8: TMenuItem;
    SpeedCaptureItem1: TToolButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Transparency1: TMenuItem;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    Button1: TButton;
    SpeedCapture1: TMenuItem;
    Active1: TMenuItem;
    Timer1: TTimer;
    ShowInfo1: TMenuItem;
    CaptureSelection1: TMenuItem;
    ImageList2: TImageList;
    ImageList1: TImageList;
    SaveToDesktopItem: TToolButton;
    SmallIconItem1: TToolButton;
    LargeIconItem1: TToolButton;
    TaskDialog1: TTaskDialog;
    Panel2: TPanel;
    CaptureCount1: TLabel;
    Label1: TLabel;
    Image2: TImage;
    Auto1: TCheckBox;
    Minimize1: TCheckBox;
    ShowCursor1: TCheckBox;
    ShowHint1: TCheckBox;
    ShowInfoDialog1: TCheckBox;
    CaptureDelay1: TEdit;
    UpDown1: TUpDown;
    Panel3: TPanel;
    SaveToDesktopItem1: TMenuItem;
    CaptureSmallIcon1: TMenuItem;
    CaptureLargeIcon1: TMenuItem;
    Fit1: TMenuItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Fit2: TCheckBox;
    ListView1: TListView;
    ResetCaptureCount1: TToolButton;
    NoChange1: TMenuItem;
    procedure FormCreate ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormResize ( Sender: TObject );
    procedure PageControl1Change ( Sender: TObject );
    procedure CursorItem1Click ( Sender: TObject );
    procedure OpenItemClick ( Sender: TObject );
    procedure CloseItemClick ( Sender: TObject );
    procedure CloseAllItemClick ( Sender: TObject );
    procedure SaveItemClick ( Sender: TObject );
    procedure SaveAsItemClick ( Sender: TObject );
    procedure PrintItemClick ( Sender: TObject );
    procedure CopyItemClick ( Sender: TObject );
    procedure PasteItemClick ( Sender: TObject );
    procedure CropItemClick ( Sender: TObject );
    procedure UndoItemClick ( Sender: TObject );
    procedure CaptureDesktopItemClick ( Sender: TObject );
    procedure CaptureAreaItemClick ( Sender: TObject );
    procedure CaptureActiveItemClick ( Sender: TObject );
    procedure CaptureObjectItemClick ( Sender: TObject );
    procedure ExitItemClick ( Sender: TObject );
    procedure BlackandWhite1Click ( Sender: TObject );
    procedure N16Color1Click ( Sender: TObject );
    procedure N256Color1Click ( Sender: TObject );
    procedure N15Bit1Click ( Sender: TObject );
    procedure N16Bit1Click ( Sender: TObject );
    procedure N24Bit1Click ( Sender: TObject );
    procedure N32Bit1Click ( Sender: TObject );
    procedure MinimizeItem1Click ( Sender: TObject );
    procedure AutomaticItem1Click ( Sender: TObject );
    procedure DelayItem1Click ( Sender: TObject );
    procedure AboutItem1Click ( Sender: TObject );
    procedure HelpContentsItem1Click ( Sender: TObject );
    procedure PrintSetupItem1Click ( Sender: TObject );
    procedure FreeFormCaptureClick ( Sender: TObject );
    procedure Website1Click ( Sender: TObject );
    procedure CapturePolygonItemClick ( Sender: TObject );
    procedure ShowHintItem1Click ( Sender: TObject );
    procedure CaptureWholeDesktopItemClick ( Sender: TObject );
    procedure CaptureIconItemClick ( Sender: TObject );
    procedure CaptureSelectionItemClick ( Sender: TObject );
    procedure HelpContents1Click ( Sender: TObject );
    procedure DefaultFolder1Click ( Sender: TObject );
    procedure SpeedCaptureItem1Click ( Sender: TObject );
    procedure Transparency1Click ( Sender: TObject );
    procedure ScrollBar1Change ( Sender: TObject );
    procedure Timer1Timer ( Sender: TObject );
    procedure ShowInfo1Click ( Sender: TObject );
    procedure SaveToDesktopItemClick ( Sender: TObject );
    procedure SmallIconItem1Click ( Sender: TObject );
    procedure LargeIconItem1Click ( Sender: TObject );
    procedure Auto1Click ( Sender: TObject );
    procedure Minimize1Click ( Sender: TObject );
    procedure ShowCursor1Click ( Sender: TObject );
    procedure ShowHint1Click ( Sender: TObject );
    procedure ShowInfoDialog1Click ( Sender: TObject );
    procedure Fit1Click ( Sender: TObject );
    procedure Fit2Click ( Sender: TObject );
    procedure ListView1SelectItem ( Sender: TObject; Item: TListItem; Selected: Boolean );
    procedure CopyToSel1Click ( Sender: TObject );
    procedure ResetCaptureCount1Click ( Sender: TObject );
  private
    { Private declarations }
    ATabSheet: TTabSheet;
    AScrollBox: TScrollBox;
    AImage: TImage;
    AFileName: string;
    AExtension: string;
    ATmpBmp: TBitmap;
    MarchingAntsCanvas: TControlCanvas;
    MarchingAntsPointA: TPoint;
    MarchingAntsPointB: TPoint;
    RubberbandVisible: Boolean;
    procedure AddTabSheet;
    procedure UpdateControls;
    procedure ClearStatusBar;
    procedure UpdateStatusBar;
    procedure DrawMarchingAnts;
    procedure RemoveMarchingAnts;
    procedure CopySelectionToClipboard;
    procedure ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure ImageDblClick ( Sender: TObject );
    procedure ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string );
    procedure GetCaptureProperties;
    procedure ConvertJPEGToBMP;
    procedure ConvertPNGToBMP;
    procedure ConvertGIFToBMP;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;
  MarchingAntsCounter: Byte;
  MarchingAntsCounterStart: Byte;

implementation

uses
  Clipbrd, ShellApi, ShlObj, ActiveX, StrUtils, inifiles, uFullscrn, uScreenDelay,
  uWebSite, uAbout, FileCtrl, HTMLHelpViewer;
{$R *.DFM}

const
  RubberBandColor: TColor = clNavy;
  HandleMinimumPixels: INTEGER = 8; // Minimum pixels in a dimension.

function IsVista: Boolean;
// Returns true if the operating system is Windows Vista (or later) and false if not
var
  PFunction: Pointer; // pointer to GetProductInfo function if exists
begin

  // Try to load GetProductInfo from Kernel32: present if Vista
  PFunction := Windows.GetProcAddress ( Windows.GetModuleHandle ( 'kernel32.dll' ), 'GetProductInfo' );
  Result := Assigned ( PFunction );

end;

function IsWindows7: Boolean;
// Returns true if the operating system is Windows 7 (or Windows Server 2008 R2) or later and false if not
var
  PFunction: Pointer; // points to PowerCreateRequest function if exists
begin
  // Try to load PowerCreateRequest from Kernel32:
  // present if Windows 7 or Server 2008 R2
  PFunction := Windows.GetProcAddress (
    Windows.GetModuleHandle ( 'kernel32.dll' ), 'PowerCreateRequest'
    );
  Result := Assigned ( PFunction );
end;

function PIDLToFolderPath ( const PIDL: ShlObj.PItemIDList ): string;
// Returns the full path to a file system folder from a PIDL or
// '' if the PIDL refers to a virtual folder.
begin

  // Set max length of return string
  SetLength ( Result, Windows.MAX_PATH );
  // Get the path
  if ShlObj.SHGetPathFromIDList ( PIDL, PChar ( Result ) ) then
    Result := PChar ( Result )
  else
    Result := '';

end;

procedure FreePIDL ( const PIDL: ShlObj.PItemIDList );
// FreePIDL
// Uses to shell allocator to free the memory used by a given PIDL
var
  iMalloc: ActiveX.IMalloc; // shell's allocator
begin

  // Try to get shell allocator
  if Windows.Succeeded ( ShlObj.SHGetMalloc ( iMalloc ) ) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    iMalloc.Free ( PIDL );

end;

function IsSpecialFolderSupported ( const CSIDL: integer ): boolean;
// Returns true if the given special folder specified by a
// CSIDL is supported on the system and false if not
var
  iPIDL: ShlObj.PItemIDList; // PIDL of the special folder
begin

  // Try to get PIDL for folder: fails if not supported
  Result := Windows.Succeeded ( ShlObj.SHGetSpecialFolderLocation ( 0, CSIDL, iPIDL ) );
  if Result then
    // Free the PIDL using shell allocator
    FreePIDL ( iPIDL );

end;

function SpecialFolderPath ( const CSIDL: integer ): string;
// Returns the full path to a special file system folder specified by a CSIDL constant FolderIDvar
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
      // Free the PIDL using shell allocator
      FreePIDL ( iPIDL );

    end;

  end;

end;

function DocumentsFolder: string;
// Find Documents folder location
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_PERSONAL ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_PERSONAL );
    Result := IncludeTrailingPathDelimiter ( iPath );

  end
  else
    raise Exception.Create ( 'Could not find documents folder location.' );

end;

function DesktopFolder: string;
// Find Desktop folder location
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_DESKTOPDIRECTORY ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_DESKTOPDIRECTORY );
    Result := IncludeTrailingPathDelimiter ( iPath );

  end
  else
    raise Exception.Create ( 'Could not find Desktop folder location.' );

end;

function ProfileFolder: string;
// Find Profile folder location
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_PROFILE ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_PROFILE );
    Result := IncludeTrailingPathDelimiter ( iPath );

  end
  else
    raise Exception.Create ( 'Could not find profile folder location.' );

end;

function AppDataFolder: string;
// Find AppData folder location
var
  i: Bool;
  iPath: array [ 0..MAX_PATH ] of char;
begin

  i := ShGetSpecialFolderPath ( 0, iPath, CSIDL_LOCAL_APPDATA, False );
  if not i then
    raise Exception.Create ( 'Could not find MyAppData folder location.' );
  Result := iPath;

end;

function PathToDir ( const APath: string ): string;
// Returns the given directory with any single trailing backslash removed. If  the directory does not end in a
// backslash it is returned unchanged
begin

  Result := APath;
  if ( APath <> '' ) and ( APath [ Length ( APath ) ] = '\' ) then
    Delete ( Result, Length ( Result ), 1 );

end;

function IsDirectory ( const ADirName: string ): boolean;
// Is ADirName a folder
var
  iAttr: integer; // directory's file attributes
begin

  iAttr := SysUtils.FileGetAttr ( ADirName );
  Result := ( iAttr <> -1 ) and ( iAttr and SysUtils.faDirectory = SysUtils.faDirectory );

end;

function EllipsifyText ( const AsPath: Boolean; const Text: string; const Canvas: Graphics.TCanvas; const MaxWidth: Integer ):
  string;
{ Shortens text if necessary to fit within a specified number of pixels on a
  given canvas. If AsPath is True text is shortened path-wise by inserting an
  ellipsis in the text body, otherwise it is trunctated and an ellipsis is
  appended. }
var
  TempPChar: PChar; // temp buffer to hold text
  TempRect: TRect; // temp rectangle hold max width of text
  Params: UINT; // flags passed to DraeTextEx API function
begin

  // Alocate mem for PChar
  GetMem ( TempPChar, Length ( Text ) * SizeOf ( Text ) + 1 );
  try

    // Copy Text into PChar
    TempPChar := SysUtils.StrPCopy ( TempPChar, Text );
    // Create Rectangle to Store PChar
    TempRect := Classes.Rect ( 0, 0, MaxWidth, high ( Integer ) );
    // Set Params depending whether it's a path or not
    if AsPath then
      Params := Windows.DT_PATH_ELLIPSIS
    else
      Params := Windows.DT_END_ELLIPSIS;
    // Tell it to Modify the PChar, and do not draw to the canvas
    Params := Params + Windows.DT_MODIFYSTRING + Windows.DT_CALCRECT;
    // Ellipsify the string based on availble space to draw in
    Windows.DrawTextEx ( Canvas.Handle, TempPChar, -1, TempRect, Params, nil );
    // Copy the modified PChar into the result
    Result := SysUtils.StrPas ( TempPChar );

  finally
    // Free Memory from PChar
    FreeMem ( TempPChar, Length ( Text ) * SizeOf ( Text ) + 1 );
  end;

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
  Result := AddDefThousandSeparator ( IntToStr ( i ) );
end;

function JustFilename ( const APathName: string ): string;
// Return a filename from a string
var
  i: integer;
  iString: string;

  // Turn- Reverse characters in a string ABCD -> DCBA

  function Turn ( const AString: string ): string;
  var
    i: integer;
  begin
    Result := '';
    if AString <> '' then
      for i := 1 to Length ( AString ) do
        Result := AString [ i ] + Result;
  end;

begin

  iString := Turn ( APathName );
  i := Pos ( '\', iString );
  if i = 0 then
    i := Pos ( ':', iString );
  if i = 0 then
    Result := APathName
  else
    Result := Turn ( Copy ( iString, 1, i - 1 ) );

end;

function JustName ( const APathName: string ): string;
// Return just the name from a file string
var
  iString: string;
begin

  iString := JustFilename ( APathName );
  if Pos ( '.', iString ) <> 0 then
    Result := Copy ( iString, 1, Pos ( '.', iString ) - 1 )
  else
    Result := iString;

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

function PixelFormatToString ( APixelFormat: TPixelFormat ): string;
// Returns APixelFormat as a string
begin

  case APixelFormat of
    pf1bit:
      Result := 'RGB1-bit'; // 1 bit
    pf4bit:
      Result := 'RGB4-bit'; // 4 bit
    pf8bit:
      Result := 'RGB8-bit'; // 8 bit
    pf24bit:
      Result := 'RGB24-bit'; // 24 bit
    pf32bit:
      Result := 'RGB32-bit'; // 32 bit
    pfDevice:
      Result := 'Device' // device
  else
    Result := 'Unknown Color Depth'; // unknown
  end; // case

end;

function JpegPixelFormatToString ( AJpegPixelFormat: TJpegPixelFormat ): string;
// Returns AJpegPixelFormat as a string
begin

  case AJpegPixelFormat of
    jf8bit:
      Result := 'RGB8-bit'; // 8 bit
    jf24bit:
      Result := 'RGB24-bit'; // 24 bit
  end; // case

end;

function JpegPixelFormatToPixelFormat ( AJpegPixelFormat: TJpegPixelFormat ): TPixelFormat;
// Returns AJpegPixelFormat as a string
begin

  case AJpegPixelFormat of
    jf8bit:
      Result := pf8bit; // 8 bit
    jf24bit:
      Result := pf24bit; // 24 bit
  else
    Result := pf24bit; // 24 bit
  end // case

end;

function PNGBitsPerPixel ( const AColorType, ABitDepth: Byte ): Integer;
// Return the BitDepth of PNGImage baised on AColorType and ABitDepth
begin

  case AColorType of
    COLOR_GRAYSCALEALPHA: Result := ( ABitDepth * 2 );
    COLOR_RGB: Result := ( ABitDepth * 3 );
    COLOR_RGBALPHA: Result := ( ABitDepth * 4 );
    COLOR_GRAYSCALE, COLOR_PALETTE: Result := ( ABitDepth + 7 );
  else
    Result := 0;
  end;

end;

function BitDepthToString ( ABitDepth: integer ): string;
// Return ABitDepth as a string
begin

  case ABitDepth of
    1:
      Result := 'RGB1-bit'; // 1 bit
    4:
      Result := 'RGB4-bit'; // 4 bit
    8:
      Result := 'RGB8-bit'; // 8 bit
    24:
      Result := 'RGB24-bit'; // 24 bit
    32:
      Result := 'ARGB32-bit'; // 32 bit
  else
    Result := 'Unknown Color Depth'; // unknown
  end; // case

end;

function BitDepthToPixelFormat ( ABitDepth: integer ): TPixelFormat;
// Return ABitDepth as TPixelFormat
begin

  case ABitDepth of
    1:
      Result := pf1bit; // 1 bit
    4:
      Result := pf4bit; // 4 bit
    8:
      Result := pf8bit; // 8 bit
    24:
      Result := pf24bit; // 24 bit
    32:
      Result := pf32bit; // 32 bit
  else
    Result := pfDevice; // unknown
  end; // case

end;

procedure TileDraw ( Canvas: TCanvas; const Rect: TRect; G: TGraphic );
// Usage: TileDraw(Canvas, ClientRect, Image1.Picture.Graphic);
var
  R, Rows, C, Cols: Integer;
  ClipRgn: THandle;
begin

  if not G.Empty then
  begin

    Rows := ( ( Rect.Bottom - Rect.Top ) div G.Height ) + 1;
    Cols := ( ( Rect.Right - Rect.Left ) div G.Width ) + 1;

    ClipRgn := CreateRectRgnIndirect ( Rect );
    try

      SelectClipRgn ( Canvas.Handle, ClipRgn );

    finally
      DeleteObject ( ClipRgn );
    end;

    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw ( Rect.Left + ( C - 1 ) * G.Width, Rect.Top + ( R - 1 ) * G.Height, G );
    SelectClipRgn ( Canvas.Handle, 0 );

  end;

end;

procedure RestrictCursorToDrawingArea ( const Image: TImage );
var
  iCursorClipArea: TRect;
begin

  iCursorClipArea := Bounds ( Image.ClientOrigin.X, Image.ClientOrigin.Y, Image.Width, Image.Height );
  Windows.ClipCursor ( @iCursorClipArea );

end;

procedure RemoveCursorRestrictions;
begin

  Windows.ClipCursor ( nil );

end;

procedure MarchingAnts ( X, Y: Integer; Canvas: TCanvas ); stdcall;
begin

  MarchingAntsCounter := MarchingAntsCounter shl 1; // Shift one bit left
  if MarchingAntsCounter = 0 then
    MarchingAntsCounter := 1;
  if ( MarchingAntsCounter and $E0 ) > 0 { // Any of the left 3 bits set? } then
    Canvas.Pixels [ X, Y ] := clWhite // Erase the pixel
  else
    Canvas.Pixels [ X, Y ] := clRed; // Draw the pixel

end;

function NormalizeRect ( R: TRect ): TRect;
begin

  // This routine normalizes a rectangle. It makes sure that the Left,Top
  // coords are always above and to the left of the Bottom,Right coords.

  with R do
  begin

    if Left > Right then
      if Top > Bottom then
        Result := Rect ( Right, Bottom, Left, Top )
      else
        Result := Rect ( Right, Top, Left, Bottom )
    else if Top > Bottom then
      Result := Rect ( Left, Bottom, Right, Top )
    else
      Result := Rect ( Left, Top, Right, Bottom );

  end;

end;

procedure SaveBMPAsPNG ( ABMPSrc: TBitmap; sFilename: string );
// Save bitmap as PNGImage
var
  iPNGDest: TPNGImage;
begin

  iPNGDest := TPNGImage.Create;
  try

    iPNGDest.Assign ( ABMPSrc );
    iPNGDest.SaveToFile ( sFilename );

  finally
    iPNGDest.Free;
  end;

end;

procedure SaveBMPAsJPEG ( ABMPSrc: TBitmap; sFilename: string );
// Save BMP as JPEGImage
var
  iJPEGDest: TJPEGImage;
begin

  iJPEGDest := TJPEGImage.Create;
  try

    iJPEGDest.Assign ( ABMPSrc );
    iJPEGDest.SaveToFile ( sFilename );

  finally
    iJPEGDest.Free;
  end;

end;

procedure SaveBMPAsGIF ( ABMPSrc: TBitmap; sFilename: string );
// Save BMP as GIF
var
  iGIFDest: TGIFImage;
begin

  iGIFDest := TGIFImage.Create;
  try

    iGIFDest.Assign ( ABMPSrc );
    iGIFDest.SaveToFile ( sFilename );

  finally
    iGIFDest.Free;
  end;

end;

procedure TFormMain.Fit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( AImage ) then
    begin

      if Fit2.Checked then
      begin

        AImage.Width := AScrollBox.ClientWidth;
        AImage.Height := AScrollBox.ClientHeight;

      end
      else
      begin

        AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
        AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

      end;

    end;

    Fit2.Checked := Fit1.Checked;

  end;

end;

procedure TFormMain.Fit2Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( AImage ) then
    begin

      if Fit2.Checked then
      begin

        AImage.Width := AScrollBox.ClientWidth;
        AImage.Height := AScrollBox.ClientHeight;

      end
      else
      begin

        AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
        AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

      end;

    end;

  end;

  Fit1.Checked := Fit2.Checked;

end;

procedure TFormMain.FormCreate ( Sender: TObject );
var
  iIniFile: TIniFile;
  iDataFolder: string;
  iIniFileName: string;
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;

begin

  Caption := 'Apprehend 6.0 ' + ASGScreenCapture1.Version + ' TImage Demo';
  Application.HelpFile := ExtractFileDir ( Application.ExeName ) + '\APPREHEND.HLP';
  MinimizeItem1.Checked := false;
  AutomaticItem1.Checked := false;
  CursorItem1.Checked := false;
  iDataFolder := AppDataFolder + '\ASG\Apprehend';
  iIniFileName := iDataFolder + '\Apprehend.ini';
  ForceDirectories ( iDataFolder );

  iIniFile := TIniFile.Create ( iIniFileName );
  try

    with iIniFile do
    begin

      Left := ReadInteger ( 'Main Form', 'Left', 0 );
      Top := ReadInteger ( 'Main Form', 'Top', 0 );
      Width := ReadInteger ( 'Main Form', 'Width', 700 );
      Height := ReadInteger ( 'Main Form', 'Height', 500 );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );
      MinimizeItem1.Checked := ReadBool ( 'Options', 'Minimise', true );
      AutomaticItem1.Checked := ReadBool ( 'Options', 'Automatic', true );
      CursorItem1.Checked := ReadBool ( 'Options', 'Include Cursor', false );

    end;

  finally
    iIniFile.Free;
  end;

  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;
  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  MarchingAntsCounterStart := 128;
  Timer1.Interval := 100;
  Timer1.Enabled := False;
  MarchingAntsCanvas := TControlCanvas.Create;
  RubberbandVisible := False;
  OpenPictureDialog1.Filename := '';
  SavePictureDialog1.Filename := '';
  ToolBar1.DrawingStyle := ComCtrls.dsGradient;
  ToolBar2.DrawingStyle := ComCtrls.dsGradient;
  ToolBar3.DrawingStyle := ComCtrls.dsGradient;
  ToolBar4.DrawingStyle := ComCtrls.dsGradient;
  // Setup the HelpFile
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.CHM';

  // Create a temporary bitmaps for image manipulation
  ATmpBmp := TBitmap.Create;

  UpdateControls;

end;

procedure TFormMain.FormDestroy ( Sender: TObject );
var
  iIniFile: TIniFile;
  iDataFolder: string;
  iIniFileName: string;
begin

  PageControl1.OnChange := nil;
  iDataFolder := AppDataFolder + '\ASG\Apprehend';
  iIniFileName := iDataFolder + '\Apprehend.ini';
  ForceDirectories ( iDataFolder );

  iIniFile := TIniFile.Create ( iIniFileName );
  try

    with iIniFile do
    begin

      WriteInteger ( 'Main Form', 'Left', Left );
      WriteInteger ( 'Main Form', 'Top', Top );
      WriteInteger ( 'Main Form', 'Width', Width );
      WriteInteger ( 'Main Form', 'Height', Height );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );
      WriteBool ( 'Options', 'Minimise', MinimizeItem1.Checked );
      WriteBool ( 'Options', 'Automatic', AutomaticItem1.Checked );
      WriteBool ( 'Options', 'Include Cursor', CursorItem1.Checked );

    end;

  finally
    iIniFile.Free;
  end;

  if Assigned ( ATmpBmp ) then
    ATmpBmp.Free;

end;

procedure TFormMain.FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Key = VK_F12 then
    begin

      ScrollBar1.Position := 100;
      ScrollBar1.Hint := IntToStr ( ScrollBar1.Position ) + '%';
      AImage.Width := Max ( 1, Round ( AImage.Picture.Width * ScrollBar1.Position / 100 ) );
      AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );

    end;

  end;

  if Key = VK_ESCAPE then
    Close;

end;

procedure TFormMain.FormResize ( Sender: TObject );
begin

  ScrollBar1.Width := ToolBar4.ClientWidth;
  UpdateControls;

end;

procedure TFormMain.AddTabSheet;
begin

  with PageControl1 do
  begin

    // Create a new Tabsheet
    ATabSheet := TTabSheet.Create ( PageControl1 );
    // Set the Tabsheet.PageControl to PageControl1
    ATabSheet.PageControl := PageControl1;
    // Set the activepage to tabsheet
    ActivePage := ATabSheet;

  end;
  with ATabsheet do
  begin

    ShowHint := True;
    // Create a Scrollbox component
    AScrollBox := TScrollBox.Create ( ATabSheet );
    AScrollBox.Parent := ATabSheet;
    AScrollBox.Align := alClient;
    AScrollBox.Visible := True;
    MarchingAntsCanvas.Control := TControl ( AScrollBox );
    // Create an image component
    AImage := TImage.Create ( Self );
    AImage.Parent := AScrollBox;
    AImage.Align := alNone;
    AImage.Left := 0;
    AImage.Top := 0;
    AImage.AutoSize := False;
    AImage.Center := False;
    AImage.Stretch := True;
    AImage.Visible := True;
    AImage.ShowHint := True;
    AImage.IncrementalDisplay := True;
    AImage.OnProgress := ProgressUpdate;
    AImage.OnDblClick := ImageDblClick;
    AImage.OnMouseDown := ImageMouseDown;
    AImage.OnMouseUp := ImageMouseUp;
    AImage.OnMouseMove := ImageMouseMove;
    AImage.PopupMenu := PopupMenu1;
    RubberbandVisible := False;
    UpdateControls;
    Timer1.Enabled := True;

  end;

end;

procedure TFormMain.DrawMarchingAnts;
begin

  MarchingAntsCounter := MarchingAntsCounterStart;

  // Use LineDDA to draw each of the 4 edges of the rectangle
  LineDDA ( MarchingAntsPointA.X, MarchingAntsPointA.Y, MarchingAntsPointB.X, MarchingAntsPointA.Y, @MarchingAnts, LongInt (
    MarchingAntsCanvas ) );
  LineDDA ( MarchingAntsPointB.X, MarchingAntsPointA.Y, MarchingAntsPointB.X, MarchingAntsPointB.Y, @MarchingAnts, LongInt (
    MarchingAntsCanvas ) );
  LineDDA ( MarchingAntsPointB.X, MarchingAntsPointB.Y, MarchingAntsPointA.X, MarchingAntsPointB.Y, @MarchingAnts, LongInt (
    MarchingAntsCanvas ) );
  LineDDA ( MarchingAntsPointA.X, MarchingAntsPointB.Y, MarchingAntsPointA.X, MarchingAntsPointA.Y, @MarchingAnts, LongInt (
    MarchingAntsCanvas ) );

  if MarchingAntsPointB.X > MarchingAntsPointA.X then
    RubberbandVisible := True
  else
    RubberbandVisible := False;

end;

procedure TFormMain.RemoveMarchingAnts;
var
  R: TRect;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
    R := NormalizeRect ( Rect ( MarchingAntsPointA.X, MarchingAntsPointA.Y, MarchingAntsPointB.X, MarchingAntsPointB.Y ) );
    InflateRect ( R, 1, 1 ); // Make the rectangle 1 pixel larger
    InvalidateRect ( AScrollBox.Handle, @R, TRUE ); // Mark as invalid
    InflateRect ( R, -2, -2 ); // Now shrink the rectangle 2 pixels
    ValidateRect ( AScrollBox.Handle, @R ); // Validate new rectangle
    // This leaves a 2 pixel band all the way around
    // the rectangle that will be erased and redrawn
    UpdateWindow ( AScrollBox.Handle );
    RubberbandVisible := False;

  end;

end;

procedure TFormMain.ResetCaptureCount1Click ( Sender: TObject );
begin

  ASGScreenCapture1.ResetCaptureCount;
  CaptureCount1.Caption := 'CaptureCount: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  TaskDialog1.Caption := 'Information';
  TaskDialog1.Title := 'The CaptureCount Was Reset';
  TaskDialog1.Text :=
    'The ASGScreenCapture.CaptureCount was set to 0.  The next screen capture will have a filename of Capture 1.bmp';
  TaskDialog1.CommonButtons := [ tcbOk ];
  TaskDialog1.MainIcon := tdiInformation;
  TaskDialog1.Flags := [ tfAllowDialogCancellation ];
  TaskDialog1.Execute;

end;

procedure TFormMain.ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R:
  TRect; const Msg: string );
var
  Progress: string;
begin

  if Stage = psRunning then
  begin

    Caption := Format ( '%d%%', [ PercentDone ] );
    Progress := Format ( '%d%', [ PercentDone ] );
    ProgressBar1.Position := PercentDone;
    Sleep ( 200 ); // Allow time for painting

  end
  else if Stage = psEnding then
  begin

    // Fix for incomplete painting of progressbar in vista and windows 7
    if ( IsVista ) or ( IsWindows7 ) then
    begin

      ProgressBar1.Position := ProgressBar1.Max;
      ProgressBar1.Position := ProgressBar1.Max - 1;
      ProgressBar1.Position := ProgressBar1.Max;
      Sleep ( 250 );

    end
    else
    begin

      ProgressBar1.Position := ProgressBar1.Max;
      Sleep ( 250 );

    end;

  end;

end;

procedure TFormMain.UpdateControls;
begin

  CloseItem.Enabled := PageControl1.PageCount <> 0;
  CloseAllItem.Enabled := PageControl1.PageCount > 1;
  SaveItem.Enabled := PageControl1.PageCount <> 0;
  SaveAsItem.Enabled := PageControl1.PageCount <> 0;
  SaveToDesktopItem.Enabled := PageControl1.PageCount <> 0;
  CopyItem.Enabled := PageControl1.PageCount <> 0;
  PasteItem.Enabled := Clipboard.HasFormat ( CF_PICTURE );
  CropItem.Enabled := RubberbandVisible;
  PrintItem.Enabled := PageControl1.PageCount <> 0;

  if PageControl1.PageCount <> 0 then
  begin
    if Assigned ( ATmpBmp ) then
      if not ATmpBmp.Empty then
        UndoItem.Enabled := True
      else
        UndoItem.Enabled := False;
  end
  else
    UndoItem.Enabled := False;

  ScrollBar1.Enabled := PageControl1.PageCount <> 0;

  CloseItem1.Enabled := CloseItem.Enabled;
  CloseAllItem1.Enabled := CloseAllItem.Enabled;
  SaveItem1.Enabled := SaveItem.Enabled;
  SaveAsItem1.Enabled := SaveAsItem.Enabled;
  SaveToDesktopItem1.Enabled := SaveToDesktopItem.Enabled;
  Edit1.Enabled := PageControl1.PageCount <> 0;
  CopyToSel1.Enabled := PageControl1.PageCount <> 0;
  CopyItem1.Enabled := CopyItem.Enabled;
  PasteItem1.Enabled := PasteItem.Enabled;
  CropItem1.Enabled := CropItem.Enabled;
  Colors1.Enabled := PageControl1.PageCount <> 0;
  PrintItem1.Enabled := PrintItem.Enabled;
  UndoItem1.Enabled := UndoItem.Enabled;
  ScrollBar1.Enabled := PageControl1.PageCount <> 0;
  Fit1.Enabled := PageControl1.PageCount <> 0;

end;

procedure TFormMain.ClearStatusBar;
var
  i: integer;
begin

  for i := 0 to StatusBar1.Panels.Count - 1 do
    StatusBar1.Panels [ i ].Text := '';

end;

procedure TFormMain.UpdateStatusBar;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    AFilename := PageControl1.ActivePage.Hint;
    FormMain.Caption := 'Apprehend ' + AsgCapture.ASG_COMPONENT_VERSION + ' TImage Demo- ' + AFilename;
    StatusBar1.Panels [ 0 ].Text := EllipsifyText ( True, ExtractFilePath ( AFileName ), Canvas, 225 );
    StatusBar1.Panels [ 1 ].Text := EllipsifyText ( True, ExtractFilename ( AFileName ), Canvas, 175 );
    CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

    // Show image information
    if Assigned ( AImage.Picture.Graphic ) then
      if not AImage.Picture.Graphic.Empty then
      begin

        StatusBar1.Panels [ 2 ].Text := ' Height: ' + IntegerToString ( AImage.Picture.Graphic.Height ) + ' pixels';
        StatusBar1.Panels [ 3 ].Text := 'Width: ' + IntegerToString ( AImage.Picture.Graphic.Width ) + ' pixels ';

        // All graphic formats are converted to bitmaps to provide a canvas for rubberbanding so get the pixelformat from the bitmap
        StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

        if Assigned ( AImage.Picture.Graphic ) then
        begin

          Image1.Picture.Assign ( AImage.Picture );
          Image1.Update;

        end;

      end;

  end
  else
  begin

    CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
    ClearStatusBar;

  end;

end;

procedure TFormMain.ImageDblClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    FullScreen := TFullScreen.Create ( Self );
    try

      Screen.Cursor := crHourglass;
      try

        // Copy image to fullscreen image}
        FullScreen.Image1.Picture.Bitmap.Assign ( AImage.Picture.Graphic );
        FullScreen.ScrollBox1.HorzScrollBar.Range := AImage.Picture.Graphic.Width;
        FullScreen.ScrollBox1.VertScrollBar.Range := AImage.Picture.Graphic.Height;
        // Show the image fullscreen}
        FullScreen.Showmodal;

      finally
        Screen.Cursor := crDefault;
      end;

    finally
      FullScreen.Free;
    end;

  end;

end;

procedure TFormMain.ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    X := X + ( Sender as TImage ).Left;
    Y := Y + ( Sender as TImage ).Top;
    RemoveMarchingAnts;
    MarchingAntsPointA.X := X;
    MarchingAntsPointA.Y := Y;
    MarchingAntsPointB.X := X;
    MarchingAntsPointB.Y := Y;
    // Force mouse movement to stay within TImage
    RestrictCursorToDrawingArea ( ( Sender as TImage ) );

    if MarchingAntsPointB.X > X then
      RubberbandVisible := True
    else
      RubberbandVisible := False;

    if RubberbandVisible then
      StatusBar1.Panels [ 6 ].Text := 'Selection'
    else
      StatusBar1.Panels [ 6 ].Text := '';

    UpdateControls;

  end;

end;

procedure TFormMain.ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
var
  AntsWidth: integer;
  AntsHeight: integer;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    if ssLeft in Shift then
    begin

      X := X + ( Sender as TImage ).Left;
      Y := Y + ( Sender as TImage ).top;
      RemoveMarchingAnts;
      MarchingAntsPointB.X := X;
      MarchingAntsPointB.Y := Y; // Save the new corner where the mouse is
      DrawMarchingAnts;
      AntsWidth := MarchingAntsPointB.X - MarchingAntsPointA.X;
      AntsHeight := MarchingAntsPointB.Y - MarchingAntsPointA.Y;
      StatusBar1.Panels [ 5 ].Text := '(' + IntToStr ( AntsWidth ) + ' , ' + IntToStr ( AntsHeight ) + ')';

    end
    else
    begin
      StatusBar1.Panels [ 5 ].Text := '';
    end;

    if MarchingAntsPointB.X > MarchingAntsPointA.X then
      RubberbandVisible := True
    else
      RubberbandVisible := False;

    StatusBar1.Panels [ 5 ].Text := '(' + IntToStr ( X ) + ' , ' + IntToStr ( Y ) + ')';
    if RubberbandVisible then
      StatusBar1.Panels [ 6 ].Text := 'Selection'
    else
      StatusBar1.Panels [ 6 ].Text := '';
    UpdateControls;

  end;

end;

procedure TFormMain.ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin

  if ( ABS ( MarchingAntsPointA.X - MarchingAntsPointB.X ) < HandleMinimumPixels ) and
    ( ABS ( MarchingAntsPointA.X - MarchingAntsPointB.X ) < HandleMinimumPixels ) then
  begin

    RemoveMarchingAnts;
    MarchingAntsPointB := MarchingAntsPointA;

    if MarchingAntsPointB.X > MarchingAntsPointA.X then
      RubberbandVisible := True
    else
      RubberbandVisible := False;

    if RubberbandVisible then
      StatusBar1.Panels [ 6 ].Text := 'Selection'
    else
      StatusBar1.Panels [ 6 ].Text := '';
  end;

  RemoveCursorRestrictions;

end;

procedure TFormMain.LargeIconItem1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    iBitmap := ASGScreenCapture1.CaptureLargeIcon;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 20;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.ListView1SelectItem ( Sender: TObject; Item: TListItem; Selected: Boolean );
begin

  if Selected then
  begin

    PageControl1.ActivePageIndex := Item.Index;
    UpdateStatusbar;

  end;

end;

procedure TFormMain.PageControl1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
    MarchingAntsCanvas.Control := TControl ( AScrollBox );

    ListView1.ItemIndex := PageControl1.ActivePageIndex;

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    Image2.Transparent := AImage.Picture.Bitmap.PixelFormat = pf32bit;
    Image2.Picture := AImage.Picture;
    Image2.Update;

    case AImage.Picture.Bitmap.PixelFormat of
      pf1bit: BlackandWhite1.Checked := True;
      pf4bit: N16Color1.Checked := True;
      pf8bit: N256Color1.Checked := True;
      pf15bit: N15Bit1.Checked := True;
      pf16bit: N16Bit1.Checked := True;
      pf24bit: N24Bit1.Checked := True;
      pf32bit: N32Bit1.Checked := True;
    else
      NoChange1.Checked := True;
    end;

    UpdateStatusBar;
    CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  end;

end;

procedure TFormMain.CopySelectionToClipboard;
var
  bitmap: TBitmap;
  sourcerect: TRect;
  destrect: TRect;
  StretchFactor_X: Integer;
  StretchFactor_Y: Integer;
begin

  if ( Assigned ( PageControl1.ActivePage ) ) and ( MarchingAntsPointB.X > MarchingAntsPointA.X ) and ( MarchingAntsPointB.Y >
    MarchingAntsPointA.Y ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    RemoveMarchingAnts;
    begin

      // If image present...
      if Assigned ( AImage.Picture.Bitmap ) then
      begin

        ATMPBmp.Assign ( AImage.Picture.Bitmap );

        Bitmap := TBitmap.Create;

        if AImage.Stretch then
        begin

          StretchFactor_X := Round ( AImage.Picture.Bitmap.Width / AImage.Picture.Bitmap.Width );
          StretchFactor_Y := Round ( AImage.Picture.Bitmap.Height / AImage.Picture.Bitmap.Height );
          Bitmap.Width := ( MarchingAntsPointB.X * StretchFactor_X ) - ( MarchingAntsPointA.X * StretchFactor_X );
          Bitmap.Height := ( MarchingAntsPointB.Y * StretchFactor_Y ) - ( MarchingAntsPointA.Y * StretchFactor_Y );
          SourceRect.Left := MarchingAntsPointA.X * StretchFactor_X;
          SourceRect.Top := MarchingAntsPointA.Y * StretchFactor_Y;
          SourceRect.Right := MarchingAntsPointB.X * StretchFactor_X;
          SourceRect.Bottom := MarchingAntsPointB.Y * StretchFactor_Y;
          DestRect.Left := 0;
          DestRect.Top := 0;
          DestRect.Right := ( MarchingAntsPointB.X * StretchFactor_X ) - ( MarchingAntsPointA.X * StretchFactor_X );
          DestRect.Bottom := ( MarchingAntsPointB.Y * StretchFactor_Y ) - ( MarchingAntsPointA.Y * StretchFactor_Y );

        end
        else
        begin

          Bitmap.Width := MarchingAntsPointB.X - MarchingAntsPointA.X;
          Bitmap.Height := MarchingAntsPointB.Y - MarchingAntsPointA.Y;
          SourceRect.Left := MarchingAntsPointA.X;
          SourceRect.Top := MarchingAntsPointA.Y;
          SourceRect.Right := MarchingAntsPointB.X;
          SourceRect.Bottom := MarchingAntsPointB.Y;
          DestRect.Left := 0;
          DestRect.Top := 0;
          DestRect.Right := MarchingAntsPointB.X - MarchingAntsPointA.X;
          DestRect.Bottom := MarchingAntsPointB.Y - MarchingAntsPointA.Y;

        end;

        Bitmap.Canvas.CopyRect ( DestRect, AImage.Picture.Bitmap.Canvas, SourceRect );
        // Copy bitmap to clipboard
        Clipboard.Assign ( Bitmap );
        UpdateStatusBar;
        MarchingAntsPointA.X := 0;
        MarchingAntsPointB.X := 0;
        MarchingAntsPointA.Y := 0;
        MarchingAntsPointB.Y := 0;
        Bitmap.Free;
        Invalidate;
        Refresh;

      end;

    end;

  end;

end;

procedure TFormMain.CopyToSel1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      // If Rubberband is visible then copy selection... else copy image
      if RubberbandVisible then
        // Copy Selection to clipboard
        CopySelectionToClipboard;

      UpdateControls;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.Transparency1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    AImage.Transparent := Transparency1.Checked;

  end;

end;

procedure TFormMain.CursorItem1Click ( Sender: TObject );
begin

  CursorItem1.Checked := ShowCursor1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;

end;

procedure TFormMain.OpenItemClick ( Sender: TObject );
var
  iExt: string;
  iListItem: TListItem;
begin

  //OpenPictureDialog1.Filter := GraphicFilter ( TGraphic );
  OpenPictureDialog1.Filter := '';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter +
    'All Supported Image Files (*.bmp;*.png;*.jpg*.jpeg;*.gif)|*.bmp;*.png;*.jpg;*.jpeg;*.gif|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Bitmap File (*.bmp)|*.bmp|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Png Image File (*.png)|*.png|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'JPeg Image File (*.jpg)|*.jpg|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Gif Image File (*.gif)|*.gif|';
  if OpenPictureDialog1.Execute then
  begin

    Screen.Cursor := crHourglass;
    try

      AFilename := OpenPictureDialog1.Filename;
      iExt := LowerCase ( ExtractFileExt ( OpenPictureDialog1.Filename ) );
      ProgressBar1.Visible := True;

      // Add ScrollBox and Image Controls to a new tabsheet
      AddTabSheet;
      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      // Jpeg, GIF and PNG do not have a canvas (needed for rubberbanding and cropping) so convert all images to TBitmap
      if iExt = '.jpg' then
      begin

        if Assigned ( AImage ) then
        begin

          ConvertJPEGToBMP;

        end;

      end
      else if iExt = '.png' then
      begin

        if Assigned ( AImage ) then
        begin

          ConvertPNGToBMP;

        end;

      end
      else if iExt = '.gif' then
      begin

        if Assigned ( AImage ) then
        begin

          ConvertGIFToBMP;

        end;

      end
      else
      begin

        AImage.Picture.OnProgress := ProgressUpdate;

        try
          AImage.Picture.LoadFromFile ( AFileName );
        except

          // if loading fails free the page
          PageControl1.ActivePage.Free;
          // reset the progressbar
          ProgressBar1.Position := 0;
          ProgressBar1.Visible := False;

        end;

      end;

      if AImage.Picture.Graphic.Empty then
      begin

        ShowMessage ( 'Error loading image' );
        // Close the active page
        PageControl1.ActivePage.Free;
        ProgressBar1.Visible := False;
        exit;

      end;

      PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
      PageControl1.ActivePage.Hint := AFilename;

      AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
      with AScrollBox do
      begin

        HorzScrollBar.Range := AImage.Picture.Graphic.Width;
        VertScrollBar.Range := AImage.Picture.Graphic.Height;

      end;

      // fit the image to the window?
      if Fit2.Checked then
      begin

        AImage.Width := AScrollBox.ClientWidth;
        AImage.Height := AScrollBox.ClientHeight;

      end
      else
      begin

        AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
        AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

      end;

      AImage.Refresh;

      // Add the image to the list
      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.SubItems.Add ( AFilename );
      iListItem.SubItems.Add ( IntegerToString ( AImage.Width ) + ' x ' + IntegerToString ( AImage.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Image2.Transparent := AImage.Picture.Bitmap.PixelFormat = pf32bit;
      Image2.Picture := AImage.Picture;
      Image2.Update;

      UpdateControls;
      UpdateStatusbar;

      ProgressBar1.Visible := False;
      // Send a message to prevent MarchingAnts drawing when image appears
      SendMessage ( handle, WM_LBUTTONDOWN, 0, 0 );

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.CloseItemClick ( Sender: TObject );
var
  iPageToClose: TTabSheet;
begin

  iPageToClose := PageControl1.ActivePage;
  if Assigned ( iPageToClose ) then
  begin

    ListView1.Items.Delete ( PageControl1.ActivePageIndex );

    if PageControl1.PageCount > 1 then
    begin

      if iPageToClose.PageIndex< ( PageControl1.PageCount - 1 ) then
        PageControl1.SelectNextPage ( True )
      else
        PageControl1.SelectNextPage ( False );
    end;

    iPageToClose.Free;

  end;

  UpdateControls;

  if PageControl1.PageCount = 0 then
  begin

    Image2.Picture := nil;
    Image2.Update;
    ASGScreenCapture1.ResetCaptureCount;
    UpdateStatusBar;

  end;

end;

procedure TFormMain.CloseAllItemClick ( Sender: TObject );
var
  i: Integer;
begin

  // Close all pages
  if PageControl1.PageCount > 0 then
  begin

    if TaskMessageDlg ( 'Close', 'Close all images?', mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
    begin

      for i := PageControl1.PageCount - 1 downto 0 do
        PageControl1.Pages [ i ].Free;

      ListView1.Clear;

    end;

  end;

  UpdateControls;

  if PageControl1.PageCount = 0 then
  begin

    Caption := 'Apprehend 6.0 TImage Demo';
    Image2.Picture := nil;
    Image2.Update;
    ASGScreenCapture1.ResetCaptureCount;
    UpdateStatusBar;

  end;

end;

procedure TFormMain.SaveItemClick ( Sender: TObject );
var
  iExtension: string;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Save current file
    if PageControl1.PageCount <> 0 then
    begin

      Screen.Cursor := crHourglass;
      try

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        AFileName := PageControl1.ActivePage.Hint;
        // If file exists then delete it
        if FileExists ( AFileName ) then
          // Prompt user to delete file
          if TaskMessageDlg ( 'Delete', AFileName + ' exists, Delete?', mtInformation, [ mbYes, mbNo ], 0 ) = mrYes then
            DeleteFile ( AFileName );

        iExtension := ExtractFileExt ( AFilename );

        if Length ( iExtension ) = 0 then
          iExtension := '.bmp';

        if iExtension = '.jpg' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to JPEG and save
              SaveBMPAsJPEG ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to PNG and save
              SaveBMPAsPNG ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to GIF and save
              SaveBMPAsGIF ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          AImage.Picture.SaveToFile ( SavePictureDialog1.Filename );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
          end;

        end;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.SaveToDesktopItemClick ( Sender: TObject );
var
  i: integer;
  iExtension: string;
begin

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( AImage ) then
    begin

      // Save current file

      Screen.Cursor := crHourGlass;
      try

        AFileName := PageControl1.ActivePage.Hint;
        AFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + ExtractFilename ( AFileName );
        iExtension := ExtractFileExt ( AFileName );
        i := 0;

        // if the path contains 'Capture' then check if it exists
        if StrUtils.AnsiContainsText ( AFileName, 'Capture' ) then
        begin

          // Increment the capture number if the file exists
          while FileExists ( AFileName ) do
          begin

            Inc ( i );
            AFileName := DesktopFolder + 'Capture' + IntToStr ( i ) + iExtension;

          end;

        end
        else if FileExists ( AFileName ) then
        begin

          TaskDialog1.Title := 'File Exists';
          TaskDialog1.Caption := 'File Exists';
          TaskDialog1.Text := 'The file ' + AFileName + ' exists.  Overwrite the file?';
          TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
          TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
          if TaskDialog1.Execute then
            if TaskDialog1.ModalResult = mrNo then
              exit;

        end;

        if iExtension = '.jpg' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to JPEG and save
              SaveBMPAsJPEG ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to PNG and save
              SaveBMPAsPNG ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to GIF and save
              SaveBMPAsGIF ( AImage.Picture.Bitmap, AFilename );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          AImage.Picture.SaveToFile ( AFilename );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
          end;

        end;

        PageControl1.ActivePage.Caption := ExtractFileName ( AFileName );
        PageControl1.ActivePage.Hint := AFileName;

        UpdateStatusbar;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.ScrollBar1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ScrollBar1.Hint := IntToStr ( ScrollBar1.Position ) + '% - F12 Reset';

    with AScrollBox do
    begin

      HorzScrollBar.Range := Round ( AImage.Picture.Width * ScrollBar1.Position / 100 );
      VertScrollBar.Range := Round ( AImage.Picture.Height * ScrollBar1.Position / 100 );

    end;

    AImage.Width := Max ( 1, Round ( AImage.Picture.Width * ScrollBar1.Position / 100 ) );
    AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );

    UpdateControls;

  end;

end;

procedure TFormMain.SaveAsItemClick ( Sender: TObject );
var
  iExtension: string;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      // Save current file with a new name

      SavePictureDialog1.Filename := ExtractFileName ( AFilename );
      if Length ( SavePictureDialog1.Filename ) = 0 then
        SavePictureDialog1.Filename := '*';
      SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );

      SavePictureDialog1.Filter := '';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Bitmap File (*.bmp)|*.bmp|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Png Image File (*.png)|*.png|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'JPeg Image File (*.jpg)|*.jpg|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Gif Image File (*.gif)|*.gif|';

      if SavePictureDialog1.Execute then
      begin

        AFileName := SavePictureDialog1.Filename;
        iExtension := ExtractFileExt ( AFilename );
        if Length ( iExtension ) = 0 then
          iExtension := '.bmp';

        if iExtension = '.jpg' then
        begin

          if Assigned ( AImage ) then
          begin

            // Convert TImage bitmap to JPEG and save
            SaveBMPAsJPEG ( AImage.Picture.Bitmap, AFilename );
            try

            finally
              Caption := 'Apprehend 6.0 TImage Demo' + AFilename;
              PageControl1.ActivePage.Caption := ExtractFilename ( AFileName );
              Statusbar1.Panels [ 1 ].Text := ExtractFilename ( AFilename );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to PNG and save
              SaveBMPAsPNG ( AImage.Picture.Bitmap, AFilename );
              try

              finally
                Caption := 'Apprehend 6.0 TImage Demo' + AFilename;
                PageControl1.ActivePage.Caption := ExtractFilename ( AFileName );
                Statusbar1.Panels [ 1 ].Text := ExtractFilename ( AFilename );
              end;

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( AImage ) then
          begin

            try

              // Convert TImage bitmap to GIF and save
              SaveBMPAsGIF ( AImage.Picture.Bitmap, AFilename );
              try

              finally
                Caption := 'Apprehend 6.0 TImage Demo' + AFilename;
                PageControl1.ActivePage.Caption := ExtractFilename ( AFileName );
                Statusbar1.Panels [ 1 ].Text := ExtractFilename ( AFilename );
              end;

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          try

            // Save bitmap to file
            AImage.Picture.SaveToFile ( AFilename );
            try

            finally
              Caption := 'Apprehend 6.0 TImage Demo' + AFilename;
              PageControl1.ActivePage.Caption := ExtractFilename ( AFileName );
              Statusbar1.Panels [ 1 ].Text := ExtractFilename ( AFilename );
            end;

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + AFilename, mtWarning, [ mbOK ], 0 );
          end;

        end;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.PrintItemClick ( Sender: TObject );
var
  iAspectRatio: Single;
  iOutputWidth: Single;
  iOutputHeight: Single;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    if not PrintDialog1.Execute then
      Exit;

    Screen.Cursor := crHourglass;
    try

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      Printer.BeginDoc;
      try
        iOutputWidth := AImage.Picture.Width;
        iOutputHeight := AImage.Picture.Height;
        iAspectRatio := iOutputWidth / iOutputHeight;
        if ( iOutputWidth < Printer.PageWidth ) and ( iOutputHeight < Printer.PageHeight ) then
        begin

          if iOutputWidth < iOutputHeight then
          begin

            iOutputHeight := Printer.PageHeight;
            iOutputWidth := iOutputHeight * iAspectRatio;

          end
          else
          begin

            iOutputWidth := Printer.PageWidth;
            iOutputHeight := iOutputWidth / iAspectRatio;

          end;

        end;

        if iOutputWidth > Printer.PageWidth then
        begin

          iOutputWidth := Printer.PageWidth;
          iOutputHeight := iOutputWidth / iAspectRatio;

        end;
        if iOutputHeight > Printer.PageHeight then
        begin

          iOutputHeight := Printer.PageHeight;
          iOutputWidth := iOutputHeight * iAspectRatio;

        end;

        Printer.Canvas.StretchDraw ( Rect ( 0, 0, Trunc ( iOutputWidth ), Trunc ( iOutputHeight ) ), AImage.Picture.Graphic );

      finally
        Printer.EndDoc;
      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.CopyItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      // If Rubberband is visible then copy selection... else copy image
      if RubberbandVisible then
        // Copy Selection to clipboard
        CopySelectionToClipboard
      else
        // Copy image to clipboard
        Clipboard.Assign ( AImage.Picture );

      UpdateControls;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.PasteItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  if Clipboard.HasFormat ( CF_BITMAP ) then
    // Is there a bitmap on the Clipboard?
  begin

    AddTabSheet;

    if Assigned ( PageControl1.ActivePage ) then
    begin

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      AExtension := '.bmp';
      AFilename := DocumentsFolder + Format ( '\Untitled%d' + AExtension, [ PageControl1.ActivePage.PageIndex ] );
      PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
      PageControl1.ActivePage.Hint := AFilename;

      iBitmap := TBitmap.Create;
      // Create bitmap to hold the contents on the Clipboard
      try

        iBitmap.Assign ( Clipboard ); // Get the bitmap off the Clipboard
        AImage.Picture.Bitmap.Width := iBitmap.Width;
        AImage.Picture.Bitmap.Height := iBitmap.Height;
        AImage.Picture.Bitmap.Assign ( iBitmap );
        // Copy the bitmap to the Image
        AImage.Invalidate;

        iListItem := ListView1.Items.Add;
        iListItem.Caption := IntToStr ( ListView1.Items.Count );
        iListItem.SubItems.Add ( AFilename );
        iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
          ' pixels' );
        ListView1.ItemIndex := ListView1.Items.Count - 1;

        Image2.Picture := AImage.Picture;
        Image2.Update;

        AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

        with AScrollBox do
        begin

          HorzScrollBar.Range := AImage.Picture.Graphic.Width;
          VertScrollBar.Range := AImage.Picture.Graphic.Height;

        end;

        AImage.Width := Max ( 1, Round ( AImage.Picture.Width * ScrollBar1.Position / 100 ) );
        AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );

        UpdateControls;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CropItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iSourceRect: TRect;
  iDestRect: TRect;
  iStretchFactor_X: Integer;
  iStretchFactor_Y: Integer;
begin

  if ( Assigned ( PageControl1.ActivePage ) ) and ( MarchingAntsPointB.X > MarchingAntsPointA.X ) and ( MarchingAntsPointB.Y >
    MarchingAntsPointA.Y ) then
  begin

    if ScrollBar1.Position = 100 then
    begin

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      if Assigned ( AImage ) then
      begin

        AScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
        if Assigned ( AScrollBox ) then
        begin

          if Assigned ( ATmpBmp ) then
            ATmpBmp.Free;
          ATmpBmp := TBitmap.Create;
          ATmpBmp.Assign ( AImage.Picture.Bitmap );

          // Erase the the rubberband
          RemoveMarchingAnts;
          begin

            // If image present...
            if Assigned ( AImage.Picture.Bitmap ) then
            begin

              ATMPBmp.Assign ( AImage.Picture.Bitmap );
              iBitmap := TBitmap.Create;
              if AImage.Stretch then
              begin

                iStretchFactor_X := Round ( AImage.Picture.Bitmap.Width / AImage.Picture.Bitmap.Width );
                iStretchFactor_Y := Round ( AImage.Picture.Bitmap.Height / AImage.Picture.Bitmap.Height );
                iBitmap.Width := ( MarchingAntsPointB.X * iStretchFactor_X ) - ( MarchingAntsPointA.X * iStretchFactor_X );
                iBitmap.Height := ( MarchingAntsPointB.Y * iStretchFactor_Y ) - ( MarchingAntsPointA.Y * iStretchFactor_Y );
                iSourceRect.Left := ( ( MarchingAntsPointA.X + AScrollBox.HorzScrollBar.Position ) * iStretchFactor_X );
                iSourceRect.Top := ( ( MarchingAntsPointA.Y + AScrollBox.VertScrollBar.Position ) * iStretchFactor_Y );
                iSourceRect.Right := ( MarchingAntsPointB.X * iStretchFactor_X ) + AScrollBox.HorzScrollBar.Position;
                iSourceRect.Bottom := ( MarchingAntsPointB.Y * iStretchFactor_Y ) + AScrollBox.VertScrollBar.Position;
                iDestRect.Left := 0;
                iDestRect.Top := 0;
                iDestRect.Right := ( MarchingAntsPointB.X * iStretchFactor_X ) - ( MarchingAntsPointA.X * iStretchFactor_X );
                iDestRect.Bottom := ( MarchingAntsPointB.Y * iStretchFactor_Y ) - ( MarchingAntsPointA.Y * iStretchFactor_Y );

              end
              else
              begin

                iBitmap.Width := MarchingAntsPointB.X - MarchingAntsPointA.X;
                iBitmap.Height := MarchingAntsPointB.Y - MarchingAntsPointA.Y;
                iSourceRect.Left := MarchingAntsPointA.X + AScrollBox.HorzScrollBar.Position;
                iSourceRect.Top := MarchingAntsPointA.Y + AScrollBox.VertScrollBar.Position;
                iSourceRect.Right := MarchingAntsPointB.X;
                iSourceRect.Bottom := MarchingAntsPointB.Y;
                iDestRect.Left := 0;
                iDestRect.Top := 0;
                iDestRect.Right := MarchingAntsPointB.X - MarchingAntsPointA.X;
                iDestRect.Bottom := MarchingAntsPointB.Y - MarchingAntsPointA.Y;

              end;

              SetStretchBltmode ( iBitmap.Canvas.Handle, Stretch_deletescans );
              iBitmap.Canvas.CopyRect ( iDestRect, AImage.Picture.Bitmap.Canvas, iSourceRect );
              iBitmap.Palette := AImage.Picture.Bitmap.Palette;
              AImage.Picture.Bitmap.Width := iBitmap.Width;
              AImage.Picture.Bitmap.Height := iBitmap.Height;
              AImage.Picture.Bitmap.Assign ( iBitmap );

              AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
              with AScrollBox do
              begin

                HorzScrollBar.Range := AImage.Picture.Width;
                VertScrollBar.Range := AImage.Picture.Height;

              end;

              MarchingAntsPointA.X := 0;
              MarchingAntsPointB.X := 0;
              MarchingAntsPointA.Y := 0;
              MarchingAntsPointB.Y := 0;
              iBitmap.Free;
              Invalidate;
              Refresh;
              ScrollBar1.Position := 100;
              AImage.Width := Max ( 1, Round ( AImage.Picture.Graphic.Width * ScrollBar1.Position / 100 ) );
              AImage.Height := Max ( 1, Round ( AImage.Picture.Graphic.Height * ScrollBar1.Position / 100 ) );
              UpdateControls;
              UpdateStatusBar;
              // For some reason the thumbnail is not refreshed when called in UpdateStatusBar so force a page chenge event
              PageControl1Change ( Sender );
              ListView1.Items.Item [ ListView1.ItemIndex ].SubItems [ 1 ] := IntegerToString ( AImage.Picture.Graphic.Height ) +
                ' pixels x ' +
                IntegerToString ( AImage.Picture.Graphic.Width ) + ' pixels ';
            end;

          end;

        end;

      end;

    end
    else
    begin

      TaskDialog1.Title := 'Warning';
      TaskDialog1.Caption := 'Warning ';
      TaskDialog1.Text := 'This demo only supports cropping in an unzoomed state.  Reset the zoom to 100%?';
      TaskDialog1.CommonButtons := [ tcbYes, TcbNo ];
      TaskDialog1.Flags := [ tfAllowDialogCancellation ];
      TaskDialog1.MainIcon := tdiError;
      if TaskDialog1.Execute then
        if TaskDialog1.ModalResult = mrYes then
        begin

          AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

          ScrollBar1.Position := 100;
          AImage.Width := Max ( 1, Round ( AImage.Picture.Width * ScrollBar1.Position / 100 ) );
          AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );
          Fit2.Checked := False;
          Fit1.Checked := False;

        end;

    end;

  end;

end;

procedure TFormMain.UndoItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    Screen.Cursor := crHourglass;
    try

      if Assigned ( ATmpBmp ) then
      begin

        AImage.Picture.Graphic.Assign ( ATmpBmp );
        AImage.Width := ATmpBmp.Width;
        AImage.Height := ATmpBmp.Height;
        if AImage.Picture.Bitmap.PixelFormat = pfDevice then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: Device';
        if AImage.Picture.Bitmap.PixelFormat = pf1bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: Black & White';
        if AImage.Picture.Bitmap.PixelFormat = pf4bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 16 Color';
        if AImage.Picture.Bitmap.PixelFormat = pf8bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 256 Color';
        if AImage.Picture.Bitmap.PixelFormat = pf15bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 15 Bit High Color';
        if AImage.Picture.Bitmap.PixelFormat = pf16bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 16 Bit High Color';
        if AImage.Picture.Bitmap.PixelFormat = pf24bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 24 Bit True Color';
        if AImage.Picture.Bitmap.PixelFormat = pf32bit then
          StatusBar1.Panels [ 5 ].Text := ' Color Depth: 32 Bit True Color';

        AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
        with AScrollBox do
        begin

          HorzScrollBar.Range := AImage.Picture.Graphic.Width;
          VertScrollBar.Range := AImage.Picture.Graphic.Height;

        end;

        // Clear the bitmap
        ATmpBmp.Assign ( nil );
        UpdateControls;
        UpdateStatusbar;
        // For some reason the thumbnail is not refreshed when called in UpdateStatusBar so force a page chenge event
        PageControl1Change ( Sender );

        ListView1.Items.Item [ ListView1.ItemIndex ].SubItems [ 1 ] := IntegerToString ( AImage.Picture.Graphic.Height ) +
          ' pixels x ' +
          IntegerToString ( AImage.Picture.Graphic.Width ) + ' pixels ';

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.CaptureDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    iBitmap := ASGScreenCapture1.CaptureDesktop;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 11;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );
          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;

          RubberbandVisible := False;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.CaptureAreaItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    if PageControl1.PageCount <> 0 then
    begin

      // Capture area of screen
      iBitmap := ASGScreenCapture1.CaptureSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

          if Assigned ( AImage ) then
          begin

            AImage.Picture.Assign ( iBitmap );

            AExtension := '.bmp';
            AFilename := DocumentsFolder + Format ( 'Capture%d', [ ASGScreenCapture1.CaptureCount ] ) + AExtension;
            PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
            PageControl1.ActivePage.Hint := AFilename;
            PageControl1.ActivePage.ImageIndex := 15;
            ScrollBar1.Position := 100;

            AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

            with AScrollBox do
            begin

              HorzScrollBar.Range := AImage.Picture.Graphic.Width;
              VertScrollBar.Range := AImage.Picture.Graphic.Height;
            end;

            if Fit2.Checked then
            begin

              AImage.Width := AScrollBox.ClientWidth;
              AImage.Height := AScrollBox.ClientHeight;

            end
            else
            begin

              AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
              AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

            end;

            iListItem := ListView1.Items.Add;
            iListItem.Caption := IntToStr ( ListView1.Items.Count );
            iListItem.SubItems.Add ( AFilename );
            iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
              ' pixels'
              );
            ListView1.ItemIndex := ListView1.Items.Count - 1;

            Image2.Picture := AImage.Picture;
            Image2.Update;
            UpdateControls;
            UpdateStatusBar;

          end;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureActiveItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Capture active window of screen
    iBitmap := ASGScreenCapture1.CaptureActiveWindow;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );
          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 14;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.CaptureObjectItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Capture object from screen
    iBitmap := ASGScreenCapture1.CaptureObject;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 16;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.ExitItemClick ( Sender: TObject );
begin

  Close;

end;

procedure TFormMain.BlackandWhite1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf1bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf1bit;
    Image2.Transparent := False;
    Image2.Update;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );
    BlackandWhite1.Checked := True;

  end;

end;

procedure TFormMain.N16Color1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf4bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf4bit;
    Image2.Transparent := False;
    Image2.Update;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );
    N16Color1.Checked := True;

  end;

end;

procedure TFormMain.N256Color1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf8bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf8bit;
    Image2.Transparent := False;
    Image2.Update;
    N256Color1.Checked := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

  end;

end;

procedure TFormMain.N15Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf15bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf15bit;
    Image2.Transparent := False;
    Image2.Update;
    N15Bit1.Checked := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

  end;

end;

procedure TFormMain.N16Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf16bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf16bit;
    Image2.Transparent := False;
    Image2.Update;
    N16Bit1.Checked := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

  end;

end;

procedure TFormMain.N24Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf24bit;
    AImage.Transparent := False;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf24bit;
    Image2.Transparent := False;
    Image2.Update;
    N24Bit1.Checked := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

  end;

end;

procedure TFormMain.N32Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    ATMPBmp.Assign ( AImage.Picture.Bitmap );
    AImage.Picture.Bitmap.PixelFormat := pf32bit;
    AImage.Transparent := True;

    Image2.Transparent := True;
    Image2.Picture.Bitmap.Assign ( AImage.Picture.Bitmap );
    Image2.Picture.Bitmap.PixelFormat := pf32bit;
    Image2.Update;

    N32Bit1.Checked := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( AImage.Picture.Bitmap.PixelFormat );

  end;

end;

procedure TFormMain.Minimize1Click ( Sender: TObject );
begin

  MinimizeItem1.Checked := Minimize1.Checked;
  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;

end;

procedure TFormMain.MinimizeItem1Click ( Sender: TObject );
begin

  Minimize1.Checked := MinimizeItem1.Checked;
  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;

end;

procedure TFormMain.Auto1Click ( Sender: TObject );
begin

  AutomaticItem1.Checked := Auto1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;

end;

procedure TFormMain.AutomaticItem1Click ( Sender: TObject );
begin

  Auto1.Checked := AutomaticItem1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;

end;

procedure TFormMain.DelayItem1Click ( Sender: TObject );
begin

  DelayDlg := TDelayDlg.Create ( self );
  try

    DelayDlg.edScreenDelay.Text := IntToStr ( ASGScreenCapture1.Delay );
    if DelayDlg.ShowModal = mrOk then
    begin

      ASGScreenCapture1.Delay := StrToInt ( DelayDlg.edScreenDelay.Text );
      UpDown1.Position := StrToInt ( DelayDlg.edScreenDelay.Text );

    end;

  finally
    DelayDlg.Free;
  end;

end;

procedure TFormMain.AboutItem1Click ( Sender: TObject );
begin

  frmAbout := TfrmAbout.Create ( Self );
  try

    frmAbout.ShowModal;

  finally
    frmAbout.Free;
  end;

end;

procedure TFormMain.HelpContentsItem1Click ( Sender: TObject );
begin

  Application.HelpCommand ( HELP_FINDER, 0 );

end;

procedure TFormMain.PrintSetupItem1Click ( Sender: TObject );
begin

  PrinterSetupDialog1.Execute;

end;

procedure TFormMain.FreeFormCaptureClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // Capture freeform selection or polygon
    iBitmap := ASGScreenCapture1.CapturePolygon;
    try

      AImage.Picture.Assign ( iBitmap );

    finally
      iBitmap.Free;
    end;

  end; { If }

  AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

  with AScrollBox do
  begin

    HorzScrollBar.Range := AImage.Picture.Width;
    VertScrollBar.Range := AImage.Picture.Height;

  end;

  UpdateStatusBar;
  ATabsheet.ImageIndex := 3;
  ScrollBar1.Position := 100;
  AImage.Width := Max ( 1, Round ( AImage.Picture.Width * ScrollBar1.Position / 100 ) );
  AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );

end;

procedure TFormMain.Website1Click ( Sender: TObject );
begin

  WebsiteForm.ShowModal;

end;

procedure TFormMain.CapturePolygonItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  // Create new image
  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Capture polygon of screen
    iBitmap := ASGScreenCapture1.CapturePolygon;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 17;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.GetCaptureProperties;
begin

  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfo1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;
  ASGScreenCapture1.ShowHint := ShowHint1.Checked;

end;

procedure TFormMain.ConvertJPEGToBMP;
// Convert JPEGImage to BMP
var
  iJPEGImage: TJPEGImage;
  iBitmap: TBitmap;
  iPixelFormat: TPixelFormat;
begin

  iJPEGImage := TJPEGImage.Create;
  try

    iJPEGImage.OnProgress := ProgressUpdate;

    try
      iJPEGImage.LoadFromFile ( AFileName );

      iBitmap := TBitmap.Create;
      try

        iBitmap.Width := iJPEGImage.Width;
        iBitmap.Height := iJPEGImage.Height;
        iBitmap.Canvas.Draw ( 0, 0, iJPEGImage );
        iPixelFormat := JpegPixelFormatToPixelFormat ( iJPEGImage.PixelFormat );
        iBitmap.PixelFormat := iPixelFormat;
        AImage.Picture.Bitmap.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

    except
      PageControl1.ActivePage.Free;
    end;

  finally
    iJPEGImage.Free;
  end;

end;

procedure TFormMain.ConvertPNGToBMP;
// Convert PNGImage to BMP
var
  iBitmap: TBitmap;
  iPNGImage: TPngImage;
  iPixelFormat: TPixelFormat;
  iBitsForPixel: Integer;
begin

  iPNGImage := TPNGImage.Create;
  try

    iPNGImage.OnProgress := ProgressUpdate;

    try

      iPNGImage.LoadFromFile ( AFileName );

      iBitmap := TBitmap.Create;
      try

        iBitmap.Width := iPNGImage.Width;
        iBitmap.Height := iPNGImage.Height;
        iBitmap.Canvas.Draw ( 0, 0, iPNGImage );
        iBitsForPixel := PNGBitsPerPixel ( iPNGImage.Header.ColorType, iPNGImage.Header.BitDepth );
        iPixelFormat := BitDepthToPixelFormat ( iBitsForPixel );
        iBitmap.PixelFormat := iPixelFormat;

        AImage.Picture.Bitmap.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

    except
      PageControl1.ActivePage.Free;
    end;

  finally
    iPNGImage.Free;
  end;

end;

procedure TFormMain.ConvertGIFToBMP;
// Convert GIFImage to BMP
var
  iPixelFormat: TPixelFormat;
  iBitmap: TBitmap;
  iGIFImage: TGIFImage;
begin

  iGIFImage := TGIFImage.Create;
  try

    iGIFImage.OnProgress := ProgressUpdate;

    try
      iGIFImage.LoadFromFile ( AFileName );

      iBitmap := TBitmap.Create;
      try

        iBitmap.PixelFormat := pf24bit;
        iBitmap.Width := iGIFImage.Width;
        iBitmap.Height := iGIFImage.Height;
        iBitmap.Canvas.Draw ( 0, 0, iGIFImage );
        iPixelFormat := BitDepthToPixelFormat ( iGIFImage.BitsPerPixel );
        iBitmap.PixelFormat := iPixelFormat;
        AImage.Picture.Bitmap.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

    except
      PageControl1.ActivePage.Free;
    end;

  finally
    iGIFImage.Free;
  end;

end;

// Show rect size in hint during selection captures

procedure TFormMain.ShowCursor1Click ( Sender: TObject );
begin

  CursorItem1.Checked := ShowCursor1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;

end;

procedure TFormMain.ShowHint1Click ( Sender: TObject );
begin

  ShowHintItem1.Checked := ShowHint1.Checked;
  ASGScreenCapture1.ShowHint := ShowHintItem1.Checked;

end;

procedure TFormMain.ShowHintItem1Click ( Sender: TObject );
begin

  ShowHint1.Checked := ShowHintItem1.Checked;
  ASGScreenCapture1.ShowHint := ShowHintItem1.Checked;

end;

procedure TFormMain.ShowInfo1Click ( Sender: TObject );
begin

  ShowInfoDialog1.Checked := ShowInfo1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfo1.Checked;

end;

procedure TFormMain.ShowInfoDialog1Click ( Sender: TObject );
begin

  ShowInfo1.Checked := ShowInfoDialog1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfo1.Checked;
end;

procedure TFormMain.SmallIconItem1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    iBitmap := ASGScreenCapture1.CaptureSmallIcon;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 18;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.CaptureWholeDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // dual monitor desktop capture
    iBitmap := ASGScreenCapture1.CaptureWholeDesktop;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 13;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.CaptureIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Capture 16x16 selection
    iBitmap := ASGScreenCapture1.CaptureIcon;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 19;
          ScrollBar1.Position := 100;

          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          AImage.Height := Max ( 1, Round ( AImage.Picture.Height * ScrollBar1.Position / 100 ) );

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusBar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.CaptureSelectionItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;

  if Assigned ( iBitmap ) then
  begin

    AddTabsheet;

    if Assigned ( PageControl1.ActivePage ) then
    begin

      AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
      iBitmap.PixelFormat := pf24bit;
      AImage.Picture.Assign ( iBitmap );
      AExtension := '.bmp';
      AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
      PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
      PageControl1.ActivePage.Hint := AFilename;
      PageControl1.ActivePage.ImageIndex := 21;

      AScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
      with AScrollBox do
      begin

        HorzScrollBar.range := AImage.Picture.Width;
        VertScrollBar.range := AImage.Picture.Height;

      end;

      if Fit2.Checked then
      begin

        AImage.Width := AScrollBox.ClientWidth;
        AImage.Height := AScrollBox.ClientHeight;

      end
      else
      begin

        AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
        AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

      end;

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.SubItems.Add ( AFilename );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) + ' pixels'
        );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Image2.Picture := AImage.Picture;
      Image2.Update;
      UpdateControls;
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.HelpContents1Click ( Sender: TObject );
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

procedure TFormMain.DefaultFolder1Click ( Sender: TObject );
begin

  //FileCtrl.SelectDirectory ( 'Select Default Folder', '', ADefaultDirectory, [ sdNewFolder, sdShowEdit, sdNewUI ] );

end;

procedure TFormMain.SpeedCaptureItem1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iListItem: TListItem;
begin

  GetCaptureProperties;

  AddTabSheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    iBitmap := ASGScreenCapture1.SpeedCaptureDesktop;
    try

      if Assigned ( iBitmap ) then
      begin

        AImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        if Assigned ( AImage ) then
        begin

          AImage.Picture.Assign ( iBitmap );

          AExtension := '.bmp';
          AFilename := DocumentsFolder + Format ( 'Capture%d' + AExtension, [ ASGScreenCapture1.CaptureCount ] );
          PageControl1.ActivePage.Caption := ExtractFileName ( AFilename );
          PageControl1.ActivePage.Hint := AFilename;
          PageControl1.ActivePage.ImageIndex := 12;
          ScrollBar1.Position := 100;
          AScrollBox := TScrollbox ( PageControl1.ActivePage.Controls [ 0 ] );

          with AScrollBox do
          begin

            HorzScrollBar.Range := AImage.Picture.Graphic.Width;
            VertScrollBar.Range := AImage.Picture.Graphic.Height;

          end;

          if Fit2.Checked then
          begin

            AImage.Width := AScrollBox.ClientWidth;
            AImage.Height := AScrollBox.ClientHeight;

          end
          else
          begin

            AImage.Width := Max ( 1, Round ( AImage.Picture.Width * 100 / 100 ) );
            AImage.Height := Max ( 1, Round ( AImage.Picture.Height * 100 / 100 ) );

          end;

          iListItem := ListView1.Items.Add;
          iListItem.Caption := IntToStr ( ListView1.Items.Count );
          iListItem.SubItems.Add ( AFilename );
          iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' pixels x ' + IntegerToString ( iBitmap.Height ) +
            ' pixels' );
          ListView1.ItemIndex := ListView1.Items.Count - 1;

          Image2.Picture := AImage.Picture;
          Image2.Update;
          UpdateControls;
          UpdateStatusbar;

        end;

      end;

    finally
      iBitmap.Free;
    end;

  end;

end;

procedure TFormMain.Timer1Timer ( Sender: TObject );
begin

  // Use SHR 1 for slower moving ants
  MarchingAntsCounterStart := MarchingAntsCounterStart shr 2;
  if MarchingAntsCounterStart = 0 then
    MarchingAntsCounterStart := 128;
  DrawMarchingAnts;
  UpdateControls;

end;

end.

