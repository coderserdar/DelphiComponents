// ------------------------------------------------------------------------------
// Apprehend Version  : 6.0
// Copyright © 1986-2011 : Adirondack Software & Graphics
// Last Modification  : 03-25-2012
// Compiler           : Delphi 2010
// Operating System   : Windows 7
// Description        : FrmMain Unit
// This file is copyright © W W Miller, 1986-2012.
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
  ToolWin, StdCtrls, Buttons, Printers, Jpeg, GIFImg, PNGImage,
  ActnMenus, RibbonActnMenus, ActnList, ActnMan, ActnCtrls,
  Ribbon, RibbonLunaStyleActnCtrls, RibbonActnCtrls;

type

  TFormMain = class ( TForm )SavePictureDialog1: TSavePictureDialog;
    ASGScreenCapture1: TASGScreenCapture;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    PopupMenu1: TPopupMenu;
    ColorDepth2: TMenuItem;
    N6: TMenuItem;
    Device2: TMenuItem;
    N16Color2: TMenuItem;
    N256Color2: TMenuItem;
    N15BitHighColor1: TMenuItem;
    N16BitHighColor1: TMenuItem;
    N24BitColor1: TMenuItem;
    N32BitColor1: TMenuItem;
    BlackandWhite2: TMenuItem;
    Close1: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Ribbon1: TRibbon;
    TaskDialog1: TTaskDialog;
    RibbonPage1: TRibbonPage;
    RibbonPage2: TRibbonPage;
    RibbonPage3: TRibbonPage;
    RibbonPage4: TRibbonPage;
    RibbonPage5: TRibbonPage;
    RibbonGroup1: TRibbonGroup;
    RibbonGroup2: TRibbonGroup;
    RibbonGroup3: TRibbonGroup;
    RibbonGroup4: TRibbonGroup;
    RibbonGroup5: TRibbonGroup;
    ActionManager1: TActionManager;
    ActionNew1: TAction;
    ActionOpen1: TAction;
    ActionClose1: TAction;
    ActionCloseAll1: TAction;
    ActionSave1: TAction;
    ActionSaveAs1: TAction;
    ActionExit1: TAction;
    ActionCaptureDesktop1: TAction;
    ActionSpeedCapture1: TAction;
    ActionDualMonitorDesktop1: TAction;
    ActionCaptureActive1: TAction;
    ActionCaptureArea1: TAction;
    ActionCaptureObject1: TAction;
    ActionCapturePolygon1: TAction;
    ActionCaptureIcon1: TAction;
    ActionCaptureSelection1: TAction;
    RibbonApplicationMenuBar1: TRibbonApplicationMenuBar;
    ImageList16H1: TImageList;
    ImageList32H1: TImageList;
    PercentItem: TSpeedButton;
    ActionMinimize1: TAction;
    ActionAutomatic1: TAction;
    ActionCursor1: TAction;
    ActionPrint1: TAction;
    ActionDevice1: TAction;
    BlackandWhite1: TAction;
    N16Color1: TAction;
    N256Color1: TAction;
    N15Bit1: TAction;
    N16Bit1: TAction;
    N32Bit1: TAction;
    N24Bit1: TAction;
    ActionShowHint1: TAction;
    ActionDelay1: TAction;
    ActionAbout1: TAction;
    ActionHelp1: TAction;
    ActionPrintSetup1: TAction;
    ActionBlog1: TAction;
    ActionWebsite1: TAction;
    ActionClearRecent1: TAction;
    RibbonSpinEditDelay1: TRibbonSpinEdit;
    ActionShowInfoDialog1: TAction;
    ActionCut1: TAction;
    ActionCopy1: TAction;
    ActionPaste1: TAction;
    ActionCrop1: TAction;
    ActionPasteToSelection1: TAction;
    ActionUndo1: TAction;
    RibbonQuickAccessToolbar1: TRibbonQuickAccessToolbar;
    ActionCaptureLargeIcon1: TAction;
    ActionCaptureSmallIcon1: TAction;
    Panel1: TPanel;
    ScrollBar1: TScrollBar;
    ProgressBar1: TProgressBar;
    Button1: TButton;
    ImageListSmallDisabled1: TImageList;
    ImageListLargeDisabled1: TImageList;
    ActionResetCaptureCount1: TAction;
    procedure FormCreate ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure FormCloseQuery ( Sender: TObject; var CanClose: boolean );
    procedure FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
    procedure PageControl1Change ( Sender: TObject );
    procedure PercentItemClick ( Sender: TObject );
    procedure PrintSetupItem1Click ( Sender: TObject );
    procedure DefaultFolder1Click ( Sender: TObject );
    procedure ScrollBar1Change ( Sender: TObject );
    procedure ActionCapturePolygon1Execute ( Sender: TObject );
    procedure ActionCaptureArea1Execute ( Sender: TObject );
    procedure ActionCaptureDesktop1Execute ( Sender: TObject );
    procedure ActionCaptureObject1Execute ( Sender: TObject );
    procedure ActionCaptureIcon1Execute ( Sender: TObject );
    procedure ActionCaptureSelection1Execute ( Sender: TObject );
    procedure ActionAutomatic1Execute ( Sender: TObject );
    procedure ActionShowHint1Execute ( Sender: TObject );
    procedure ActionDualMonitorDesktop1Execute ( Sender: TObject );
    procedure ActionCaptureActive1Execute ( Sender: TObject );
    procedure ActionNew1Execute ( Sender: TObject );
    procedure ActionOpen1Execute ( Sender: TObject );
    procedure ActionClose1Execute ( Sender: TObject );
    procedure ActionCloseAll1Execute ( Sender: TObject );
    procedure ActionSave1Execute ( Sender: TObject );
    procedure ActionSaveAs1Execute ( Sender: TObject );
    procedure ActionPrint1Execute ( Sender: TObject );
    procedure ActionDevice1Execute ( Sender: TObject );
    procedure BlackandWhite1Execute ( Sender: TObject );
    procedure N16Color1Execute ( Sender: TObject );
    procedure N256Color1Execute ( Sender: TObject );
    procedure N15Bit1Execute ( Sender: TObject );
    procedure N16Bit1Execute ( Sender: TObject );
    procedure N24Bit1Execute ( Sender: TObject );
    procedure N32Bit1Execute ( Sender: TObject );
    procedure ActionCursor1Execute ( Sender: TObject );
    procedure ActionDelay1Execute ( Sender: TObject );
    procedure ActionAbout1Execute ( Sender: TObject );
    procedure ActionHelp1Execute ( Sender: TObject );
    procedure ActionBlog1Execute ( Sender: TObject );
    procedure ActionWebsite1Execute ( Sender: TObject );
    procedure ActionExit1Execute ( Sender: TObject );
    procedure ActionSpeedCapture1Execute ( Sender: TObject );
    procedure Ribbon1RecentItemClick ( Sender: TObject; FileName: string; index: Integer );
    procedure ActionClearRecent1Execute ( Sender: TObject );
    procedure Ribbon1HelpButtonClick ( Sender: TObject );
    procedure SavePictureDialog1TypeChange ( Sender: TObject );
    procedure ActionShowInfoDialog1Execute ( Sender: TObject );
    procedure ActionCaptureLargeIcon1Execute ( Sender: TObject );
    procedure ActionCaptureSmallIcon1Execute ( Sender: TObject );
    procedure ActionResetCaptureCount1Execute ( Sender: TObject );
  private
    { Private declarations }
    FRecentFilesList: TStringList;
    FRecentFilesFilename: string;
    FTabSheet: TTabSheet;
    FScrollBox: TScrollBox;
    FImage: TImage;
    FFileName: string;
    FExtension: string;
    FFNE: string;
    FFilePathName: string;
    FFolder: string;
    FTmpFolder: string;
    FDefaultDirectory: string;
    FCaptureStr: string;
    FErrorSavingJPGFile: boolean;
    FModified: boolean;
    FTempBitmap: TBitmap;
    procedure GetCaptureOptions;
  public
    { Public declarations }
    procedure AddTabsheet;
    procedure UpdateControls;
    procedure ClearStatusbar;
    procedure UpdateStatusbar;
    procedure ImageDblClick ( Sender: TObject );
    procedure ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean; const r: TRect;
      const Msg: string );
  end;

var
  FormMain: TFormMain;

implementation

uses
  Clipbrd, ShellAPI, ShlObj, ActiveX, IniFiles, CommCtrl, FileCtrl, HTMLHelpViewer,
  uFullscrn, uScreenDelay, uSplash, uAbout;

const
  RubberBandColor: TColor = clNavy;
  HandleMinimumPixels: INTEGER = 8; // Minimum pixels in a dimension

{$R *.DFM}

procedure FreePIDL ( PIDL: ShlObj.PItemIDList );
var
  iMalloc: ActiveX.IMalloc; // shell's allocator
begin

  // Try to get shell allocator
  if Windows.Succeeded ( ShlObj.SHGetMalloc ( iMalloc ) ) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    iMalloc.Free ( PIDL );

end;

function IsSpecialFolderSupported ( CSIDL: Integer ): boolean;
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

function DesktopFolder: string;
// Find Desktop folder location
var
  iR: Bool;
  iPath: array [ 0..MAX_PATH ] of Char;
begin

  iR := ShlObj.ShGetSpecialFolderPath ( 0, iPath, CSIDL_DESKTOP, False );
  if not iR then
    raise Exception.Create ( 'Could not find Desktop folder location.' );

  Result := IncludeTrailingBackSlash ( iPath );

end;

function DocumentsFolder: string;
// Find MyDocuments folder location
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

function AppDataFolder: string;
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_APPDATA ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_APPDATA );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find AppData folder location.' );

end;

function LocalAppDataFolder: string;
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_LOCAL_APPDATA ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_LOCAL_APPDATA );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find LocalAppData folder location.' );

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

function PathToDir ( const Path: string ): string;
begin

  Result := Path;
  if ( Path <> '' ) and ( Path [ Length ( Path ) ] = '\' ) then
    Delete ( Result, Length ( Result ), 1 );

end;

function IsDirectory ( const DirName: string ): boolean;
var
  iAttr: Integer; // directory's file attributes
begin

  iAttr := SysUtils.FileGetAttr ( DirName );
  Result := ( iAttr <> -1 ) and ( iAttr and SysUtils.faDirectory = SysUtils.faDirectory );

end;

// Graphics
// PixelFormatToColor

function PixelFormatToColor ( APixelFormat: TPixelFormat ): string;
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
      Result := 'RGBA32-bit'; // 32 bit
    pfDevice:
      Result := 'Device' // device
  else
    Result := 'Unknown Color Depth'; // unknown
  end; // case

end;

function JustFilename ( const APathName: string ): string;
{ Return a filename from a string }
var
  iString: string;
  i: integer;
  // Turn
  // Reverse characters in a string ABCD -> DCBA

  function Turn ( const AString: string ): string;
  var
    i: integer;
  begin

    Result := '';
    if AString <> '' then
      for I := 1 to Length ( AString ) do
        Result := AString [ I ] + Result;

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

function PixelFormatToString ( APixelFormat: TPixelFormat ): string;
// Return APixelFormat as a string
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
    pfCustom:
      Result := 'Custom Pixel Format'; // custom
    pfDevice:
      Result := 'Device' // device
  else
    Result := 'Unknown Color Depth'; // unknown
  end; // case

end;

function JpegPixelFormatToString ( AJpegPixelFormat: TJpegPixelFormat ): string;
// Return AJpegPixelFormat as a string
begin

  case AJpegPixelFormat of
    jf8bit:
      Result := 'RGB8-bit'; // 8 bit
    jf24bit:
      Result := 'RGB24-bit'; // 24 bit
  end; // case

end;

function PNGBitsForPixel ( const AColorType, ABitDepth: Byte ): Integer;
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

function PNGBitDepthToString ( ABitDepth: integer ): string;
// Return ABitDepth as a string
begin

  case ABitDepth of
    1:
      Result := 'RGB1-bit'; // 1 bit
    4:
      Result := 'RGB4-bit'; // 4 bit
    8:
      Result := 'RGB8-bit'; // 8 bit
    15: Result := 'RGB15-bit'; // 15 bit
    16: Result := 'RGB16-bit'; // 16 bit
    24:
      Result := 'RGB24-bit'; // 24 bit
    32:
      Result := 'ARGB32-bit'; // 32 bit
  else
    Result := 'Unknown Color Depth'; // unknown
  end; // case

end;

function BitDepthToPixelFormat ( ABitDepth: integer ): TPixelFormat;
// Return ABitDepth as a string
begin

  case ABitDepth of
    1:
      Result := pf1bit; // 1 bit
    4:
      Result := pf4bit; // 4 bit
    6:
      Result := pfCustom; // 6 bit
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

procedure TFormMain.FormCreate ( Sender: TObject );
var
  i: Integer;
  iIniFile: TIniFile;
  iIniFileName: string;
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  Caption := 'Apprehend ' + ASGScreenCapture1.Version + ' TImage Demo';
  FModified := False;
  FTmpFolder := ExtractFileDir ( Application.ExeName );
  FDefaultDirectory := DocumentsFolder;
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.CHM';
  //C:\Components\Apprehend\Help\Apprehend.chm
  ActionMinimize1.Checked := False;
  ActionAutomatic1.Checked := False;
  ActionCursor1.Checked := False;
  ActionShowInfoDialog1.Checked := False;
  ForceDirectories ( IncludeTrailingBackslash ( AppDataFolder ) + 'Apprehend' );
  iIniFileName := IncludeTrailingBackslash ( AppDataFolder ) + 'Apprehend\ApprehendDemo.ini';
  FRecentFilesFilename := IncludeTrailingBackslash ( AppDataFolder ) + 'Apprehend\ApprehendDemoMRF.Dat';

  // read ini file
  iIniFile := TIniFile.Create ( iIniFileName );
  try
    with iIniFile do
    begin
      Left := ReadInteger ( 'Main Form', 'Left', 0 );
      Top := ReadInteger ( 'Main Form', 'Top', 0 );
      Width := ReadInteger ( 'Main Form', 'Width', 700 );
      Height := ReadInteger ( 'Main Form', 'Height', 500 );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );
      FDefaultDirectory := ReadString ( 'Options', 'Folder', FDefaultDirectory );
      ActionMinimize1.Checked := ReadBool ( 'Options', 'Minimise', True );
      ActionAutomatic1.Checked := ReadBool ( 'Options', 'Automatic', False );
      ActionCursor1.Checked := ReadBool ( 'Options', 'Include Cursor', False );
      ActionShowInfoDialog1.Checked := ReadBool ( 'Options', 'Show Info Dialog', False );
    end;

  finally
    iIniFile.Free;
  end;

  FormSplash := TFormSplash.Create ( Self );
  FormSplash.Show;

  FRecentFilesList := TStringList.Create;
  // read mrf
  if FileExists ( FRecentFilesFilename ) then
  begin

    FRecentFilesList.LoadFromFile ( FRecentFilesFilename );
    for i := 0 to FRecentFilesList.Count - 1 do
    begin

      // add filename to Ribbon Recent Items
      Ribbon1.AddRecentItem ( FRecentFilesList.Strings [ i ] );

    end;

  end;

  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  // ASGScreenCapture1.Minimize := ActionMinimize1.Checked;
  ASGScreenCapture1.Auto := ActionAutomatic1.Checked;
  ASGScreenCapture1.ShowCursor := ActionCursor1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ActionShowInfoDialog1.Checked;
  FErrorSavingJPGFile := False;
  OpenPictureDialog1.FileName := '';
  SavePictureDialog1.FileName := '';

  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.CHM';
  //C:\Components\Apprehend\Help\Apprehend.chm

  // Create a temporary bitmaps for image manipulation
  FTempBitmap := TBitmap.Create;

end;

procedure TFormMain.FormDestroy ( Sender: TObject );
var
  iIniFile: TIniFile;
  iIniFileName: string;
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

  // write ini file
  iIniFileName := IncludeTrailingBackslash ( AppDataFolder ) + 'Apprehend\ApprehendDemo.ini';

  iIniFile := TIniFile.Create ( iIniFileName );
  try
    with iIniFile do
    begin

      WriteInteger ( 'Main Form', 'Left', Left );
      WriteInteger ( 'Main Form', 'Top', Top );
      WriteInteger ( 'Main Form', 'Width', Width );
      WriteInteger ( 'Main Form', 'Height', Height );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );
      WriteString ( 'Options', 'Folder', FDefaultDirectory );
      WriteString ( 'Options', 'Folder', FDefaultDirectory );
      WriteBool ( 'Options', 'Minimise', ActionMinimize1.Checked );
      WriteBool ( 'Options', 'Automatic', ActionAutomatic1.Checked );
      WriteBool ( 'Options', 'Include Cursor', ActionCursor1.Checked );
      WriteBool ( 'Options', 'Show Info Dialog', ActionShowInfoDialog1.Checked );

    end;

  finally
    iIniFile.Free;
  end;

  FRecentFilesList.SaveToFile ( FRecentFilesFilename );
  FRecentFilesList.Free;
  if Assigned ( FTempBitmap ) then
    FTempBitmap.Free;

end;

procedure TFormMain.FormCloseQuery ( Sender: TObject; var CanClose: boolean );
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

      FImage := TImage ( PageControl1.ActivePage.Controls [ 0 ] );

      if FModified then
      begin
        TaskDialog1.Buttons.Clear;
        TaskDialog1.Title := 'The Image Has Changed';
        TaskDialog1.Caption := 'Save The Image';
        TaskDialog1.Text := 'Save ' + PageControl1.ActivePage.Hint + '?';
        TaskDialog1.CustomMainIcon.LoadFromFile ( 'Confirm.ico' );
        TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
        TaskDialog1.ExpandButtonCaption := 'Expand';
        TaskDialog1.ExpandedText := 'Select save the image to save the image to a file' + #10#13 +
          'Select close to close the tab without saving' + #10#13 +
          'Select return to the demo to cancel' + #10#13 +
          'Select close all to close all tabs without saving.';
        TaskDialog1.CommonButtons := [ ];
        AButton := TaskDialog1.Buttons.Add;
        AButton.Caption := 'Save the Image';
        AButton.ModalResult := 100;
        BButton := TaskDialog1.Buttons.Add;
        BButton.Caption := 'Return to the Demo';
        BButton.ModalResult := 101;
        CButton := TaskDialog1.Buttons.Add;
        CButton.Caption := 'Close';
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

  UpdateControls;
end;

procedure TFormMain.FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
begin

  if Key = vk_f1 then
  begin

    Application.HelpShowTableOfContents;

  end;

  if Key = vk_f12 then
  begin

    ScrollBar1.Position := 100;
    ScrollBar1.Hint := IntToStr ( ScrollBar1.Position ) + '%';
    PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
    FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
    FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

  end;

  if Key = VK_ESCAPE then
    Close;

end;

procedure TFormMain.GetCaptureOptions;
begin

  // ASGScreenCapture1.Minimize := ActionMinimize1.Checked;
  ASGScreenCapture1.Auto := ActionAutomatic1.Checked;
  ASGScreenCapture1.ShowCursor := ActionCursor1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ActionShowInfoDialog1.Checked;
  ASGScreenCapture1.Delay := RibbonSpinEditDelay1.Value;
  ASGScreenCapture1.ShowHint := ActionShowHint1.Checked;

end;

procedure TFormMain.ActionAbout1Execute ( Sender: TObject );
begin

  frmAbout := TfrmAbout.Create ( Self );
  try

    frmAbout.ShowModal;

  finally
    frmAbout.Free;
  end;

end;

procedure TFormMain.ActionAutomatic1Execute ( Sender: TObject );
begin

  ASGScreenCapture1.Auto := ActionAutomatic1.Checked;

end;

procedure TFormMain.ActionBlog1Execute ( Sender: TObject );
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://williamwmiller.wordpress.com/' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.ActionCaptureActive1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureActiveWindow;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 27;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;

          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

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

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );
          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 25;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

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

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureObject;
      try
        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 27;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

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

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );
          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 26;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

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

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 30;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureLargeIcon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureLargeIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 29;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureSelection1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 32;

          FModified := True;

          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

        end
        else
        begin

          PageControl1.ActivePage.Free;
          exit;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionCaptureSmallIcon1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureSmallIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 31;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;

          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionClearRecent1Execute ( Sender: TObject );
begin

  Ribbon1.ClearRecentItems;

end;

procedure TFormMain.ActionClose1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Close the active page
    PageControl1.ActivePage.Free;
    PageControl1.SelectNextPage ( False );
    UpdateControls;
    UpdateStatusBar;

  end;

  if PageControl1.PageCount = 0 then
    ClearStatusbar;

end;

procedure TFormMain.ActionCloseAll1Execute ( Sender: TObject );
var
  i: Integer;
begin

  // Close All pages
  if PageControl1.PageCount - 1 > 0 then
    if TaskMessageDlg ( 'Close', 'Close all images?', mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
    begin

      for i := PageControl1.PageCount - 1 downto 0 do
      begin

        PageControl1.ActivePage := PageControl1.Pages [ i ];
        PageControl1.ActivePage.Free; // Closes and Frees the ActivePage
        PageControl1.SelectNextPage ( False );

      end;

      UpdateControls;
      ClearStatusBar;

    end
    else
      for i := PageControl1.PageCount - 1 downto 0 do
      begin

        FImage := TImage ( PageControl1.Pages [ i ].Controls [ 0 ] );
        // Close the active page
        PageControl1.ActivePage.Free;
        PageControl1.SelectNextPage ( False );
        UpdateControls;
        if PageControl1.PageCount = 0 then
          ClearStatusbar;

      end;

end;

procedure TFormMain.ActionCursor1Execute ( Sender: TObject );
begin

  ASGScreenCapture1.ShowCursor := ActionCursor1.Checked;

end;

procedure TFormMain.ActionDelay1Execute ( Sender: TObject );
begin

  DelayForm := TDelayForm.Create ( Self );
  try

    DelayForm.edScreenDelay.Text := IntToStr ( ASGScreenCapture1.Delay );
    if DelayForm.ShowModal = mrOk then
      ASGScreenCapture1.Delay := StrToInt ( DelayForm.edScreenDelay.Text );

  finally
    DelayForm.Free;
  end;

end;

procedure TFormMain.ActionDevice1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pfDevice;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-Bit';
    ActionDevice1.Checked := True;

  end;

end;

procedure TFormMain.ActionDualMonitorDesktop1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    GetCaptureOptions;

    iBitmap := ASGScreenCapture1.CaptureWholeDesktop;

    if Assigned ( iBitmap ) then
    begin

      FImage.Picture.Assign ( iBitmap );
      // Give the image a default filename
      FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
      Ribbon1.DocumentName := FFilePathName;
      PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 23;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FModified := True;

      UpdateStatusbar;
      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

    end;

  end;

end;

procedure TFormMain.ActionExit1Execute ( Sender: TObject );
begin

  Close;

end;

procedure TFormMain.ActionHelp1Execute ( Sender: TObject );
begin

  Application.HelpShowTableOfContents;

end;

procedure TFormMain.ActionNew1Execute ( Sender: TObject );
begin

  AddTabsheet;
  // Set the caption of the tabsheet
  FTabSheet.Caption := Format ( 'Untitled%d', [ PageControl1.ActivePage.PageIndex ] );

end;

procedure TFormMain.ActionOpen1Execute ( Sender: TObject );
begin

  if OpenPictureDialog1.Execute then
  begin

    Screen.Cursor := crHourGlass;
    try

      // Add ScrollBox and Image Controls to a new tabsheet
      AddTabsheet;

      if Assigned ( PageControl1.ActivePage ) then
      begin

        FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
        try

          ProgressBar1.Visible := True;
          FImage.Picture.OnProgress := ProgressUpdate;
          FImage.Picture.LoadFromFile ( OpenPictureDialog1.FileName );
          Ribbon1.DocumentName := OpenPictureDialog1.FileName;
          FFilePathName := OpenPictureDialog1.FileName;

        except
          TaskMessageDlg ( 'Error', 'Error loading image', mtError, [ mbOk ], 0 );
          Ribbon1.DocumentName := '';
          FFilePathName := '';
        end;

        if FImage.Picture.Graphic.Empty then
          Exit;
        FTabSheet.Caption := ExtractFilename ( OpenPictureDialog1.FileName );

        PageControl1.ActivePage.Hint := FFilePathName;
        PageControl1.ActivePage.ImageIndex := 1;

        FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
        with FScrollBox do
        begin

          HorzScrollBar.range := FImage.Picture.Width;
          VertScrollBar.range := FImage.Picture.Height;

        end;

      end;

      FModified := False;
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      FImage.Refresh;
      UpdateStatusbar;
      Sleep ( 500 );
      FRecentFilesList.Add ( OpenPictureDialog1.FileName );
      Ribbon1.AddRecentItem ( OpenPictureDialog1.FileName );
      ProgressBar1.Position := 0;
      ProgressBar1.Visible := False;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.ActionPrint1Execute ( Sender: TObject );
var
  iAspectRatio: single;
  iOutputWidth: single;
  iOutputHeight: single;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    if not PrintDialog1.Execute then
      Exit;

    Screen.Cursor := crHourGlass;
    try

      Printer.BeginDoc;
      try

        iOutputWidth := FImage.Picture.Width;
        iOutputHeight := FImage.Picture.Height;
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

        Printer.Canvas.StretchDraw ( Rect ( 0, 0, Trunc ( iOutputWidth ), Trunc ( iOutputHeight ) ), FImage.Picture.Graphic );

      finally
        Printer.EndDoc;
      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.ActionResetCaptureCount1Execute ( Sender: TObject );
begin

  ASGScreenCapture1.ResetCaptureCount;

  TaskDialog1.Caption := 'Information';
  TaskDialog1.Title := 'The CaptureCount Was Reset';
  TaskDialog1.Text :=
    'The ASGScreenCapture.CaptureCount was set to 0.  The next screen capture will have a filename of Capture 1.bmp';
  TaskDialog1.CommonButtons := [ tcbOk ];
  TaskDialog1.MainIcon := tdiInformation;
  TaskDialog1.Flags := [ tfAllowDialogCancellation ];
  TaskDialog1.Execute;
end;

procedure TFormMain.ActionSave1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // Save current file
    if PageControl1.PageCount <> 0 then
    begin

      Screen.Cursor := crHourglass;
      try

        FFilePathName := PageControl1.ActivePage.Hint;
        FExtension := ExtractFileExt ( FFilePathName );
        if Length ( FExtension ) = 0 then
          FExtension := '.bmp';

        try
          FImage.Picture.SaveToFile ( FFilePathName );
        except
          on EInvalidGraphic do
            TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
        end;

        // Reload the file to show bitdepth
        try
          FImage.Picture.LoadFromFile ( FFilePathName );
        except
          on EInvalidGraphic do
            FImage.Picture.Graphic := nil;
        end;

        PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.ActionSaveAs1Execute ( Sender: TObject );
var
  iExtension: string;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourGlass;
    try

      FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      SavePictureDialog1.FileName := ExtractFilename ( FFilePathName );
      if Length ( SavePictureDialog1.FileName ) = 0 then
        SavePictureDialog1.FileName := '*';
      SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );
      SavePictureDialog1.Filter := '';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Windows Bitmap File (*.bmp)|*.bmp|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Png Image File (*.png)|*.png|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'JPeg Image File (*.jpg)|*.jpg|';
      SavePictureDialog1.Filter := SavePictureDialog1.Filter + 'Gif Image File (*.gif)|*.gif|';
      if SavePictureDialog1.Execute then
      begin

        FFilePathName := SavePictureDialog1.FileName;
        iExtension := ExtractFileExt ( FFilePathName );
        if Length ( iExtension ) = 0 then
          iExtension := '.bmp';

        if iExtension = '.jpg' then
        begin

          if Assigned ( FImage ) then
          begin

            try

              // Convert TImage bitmap to JPEG and save
              SaveBMPAsJPEG ( FImage.Picture.Bitmap, FFilePathName );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( FImage ) then
          begin

            try

              // Convert TImage bitmap to PNG and save
              SaveBMPAsPNG ( FImage.Picture.Bitmap, FFilePathName );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( FImage ) then
          begin

            try

              // Convert TImage bitmap to GIF and save
              SaveBMPAsGIF ( FImage.Picture.Bitmap, FFilePathName );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          FImage.Picture.SaveToFile ( SavePictureDialog1.Filename );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
          end;

        end;

        Caption := 'Apprehend 6.0 Demo- ' + FFilePathName;
        PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;
end;

procedure TFormMain.ActionShowHint1Execute ( Sender: TObject );
begin

  ASGScreenCapture1.ShowHint := ActionShowHint1.Checked;

end;

procedure TFormMain.ActionShowInfoDialog1Execute ( Sender: TObject );
begin

  ASGScreenCapture1.ShowInfoDialog := ActionShowInfoDialog1.Checked;

end;

procedure TFormMain.ActionSpeedCapture1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.SpeedCaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );
          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 24;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
          with FScrollBox do
          begin

            HorzScrollBar.range := FImage.Picture.Width;
            VertScrollBar.range := FImage.Picture.Height;

          end;

          FModified := True;
          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ActionWebsite1Execute ( Sender: TObject );
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://frontiernet.net/~w2m/index.html' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.ActionCaptureDesktop1Execute ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      GetCaptureOptions;

      iBitmap := ASGScreenCapture1.CaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          FImage.Picture.Assign ( iBitmap );

          // Give the image a default filename
          FFilePathName := DesktopFolder + 'Capture' + IntToStr ( ASGScreenCapture1.CaptureCount ) + '.bmp';
          Ribbon1.DocumentName := FFilePathName;
          PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          PageControl1.ActivePage.Hint := FFilePathName;
          PageControl1.ActivePage.ImageIndex := 22;

          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.Range := FImage.Picture.Width;
            VertScrollBar.Range := FImage.Picture.Height;

          end;

          FModified := True;

          UpdateStatusbar;
          ScrollBar1.Position := 100;
          PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
          FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
          FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.AddTabsheet;
begin

  with PageControl1 do
  begin

    // Create a new Tabsheet
    FTabSheet := TTabSheet.Create ( PageControl1 );
    // Set the Tabsheet.PageControl to PageControl1
    FTabSheet.PageControl := PageControl1;
    // Set the activepage to tabsheet
    ActivePage := FTabSheet;
  end;

  with FTabSheet do
  begin

    ShowHint := True;
    // Create a Scrollbox component
    FScrollBox := TScrollBox.Create ( FTabSheet );
    FScrollBox.Parent := FTabSheet;
    FScrollBox.Align := alClient;
    FScrollBox.Visible := True;
    // Create an image component
    FImage := TImage.Create ( Self );
    FImage.Parent := FScrollBox;
    FImage.Align := alNone;
    FImage.Left := 0;
    FImage.Top := 0;
    FImage.AutoSize := False;
    FImage.Center := False;
    FImage.Stretch := True;
    FImage.Visible := True;
    FImage.ShowHint := True;
    FImage.Picture.RegisterClipboardFormat ( cf_BitMap, TIcon );
    FImage.OnProgress := ProgressUpdate;
    FImage.OnDblClick := ImageDblClick;
    FImage.PopupMenu := PopupMenu1;
    UpdateControls;
    FImage.Update;

  end;

end;

procedure TFormMain.ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean; const r:
  TRect; const Msg: string );
var
  iProgress: string;
begin

  if Stage = psStarting then
  begin

    Caption := 'Apprehend TImage Demo';
    ProgressBar1.Position := 0;

  end
  else if Stage = psRunning then
  begin

    Caption := Format ( '%d%%', [ PercentDone ] );
    iProgress := Format ( '%d%', [ PercentDone ] );
    ProgressBar1.Position := PercentDone;

  end
  else if Stage = psEnding then
  begin

    Caption := 'Apprehend TImage Demo';
    ProgressBar1.Position := ProgressBar1.Max - 1;
    ProgressBar1.Position := ProgressBar1.Max;
    ProgressBar1.Position := ProgressBar1.Max - 1;

  end;

end;

procedure TFormMain.Ribbon1HelpButtonClick ( Sender: TObject );
begin

  Application.HelpShowTableOfContents;

end;

procedure TFormMain.Ribbon1RecentItemClick ( Sender: TObject; FileName: string; index: Integer );
begin

  if FileExists ( FileName ) then
  begin

    Screen.Cursor := crHourGlass;
    try

      FFilePathName := FileName;
      // Add ScrollBox and Image Controls to a new tabsheet
      AddTabsheet;

      if Assigned ( PageControl1.ActivePage ) then
      begin

        FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
        try

          FImage.Picture.LoadFromFile ( FileName );
          Ribbon1.DocumentName := FileName;

        except
          TaskMessageDlg ( 'Error', 'Error loading image', mtError, [ mbOk ], 0 );
          Ribbon1.DocumentName := '';
        end;

        if FImage.Picture.Graphic.Empty then
          Exit;

        FTabSheet.Caption := ExtractFilename ( FileName );
        PageControl1.ActivePage.Hint := FFilePathName;
        PageControl1.ActivePage.ImageIndex := 21;

        FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
        with FScrollBox do
        begin

          HorzScrollBar.range := FImage.Picture.Width;
          VertScrollBar.range := FImage.Picture.Height;

        end;

      end;
      FModified := False;
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      FImage.Refresh;
      UpdateStatusbar;
      Sleep ( 500 );
      ProgressBar1.Position := 0;

    finally
      Screen.Cursor := crDefault;
    end;

  end
  else
  begin

    MessageBeep ( MB_ICONQUESTION );
    TaskDialog1.Buttons.Clear;
    TaskDialog1.Caption := 'Warning';
    TaskDialog1.Title := 'File Does Not Exist';
    TaskDialog1.Text := 'The file ' + FileName + ' does not exist. Remove image from the list?';
    TaskDialog1.ExpandedText := 'Select Yes to remove the image from the list or select No to cancel.';
    TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
    TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
    if TaskDialog1.Execute then
      if TaskDialog1.ModalResult = mrYes then
        Ribbon1.RemoveRecentItem ( FileName );

  end;

end;

procedure TFormMain.UpdateControls;
begin

  ActionClose1.Enabled := PageControl1.PageCount <> 0;
  ActionCloseAll1.Enabled := PageControl1.PageCount > 1;
  ActionSave1.Enabled := PageControl1.PageCount <> 0;
  ActionSaveAs1.Enabled := PageControl1.PageCount <> 0;
  ActionPrint1.Enabled := PageControl1.PageCount <> 0;
  ScrollBar1.Enabled := PageControl1.PageCount <> 0;
  PercentItem.Enabled := PageControl1.PageCount <> 0;

end;

procedure TFormMain.ClearStatusbar;
var
  i: Integer;
begin

  for i := StatusBar1.Panels.Count - 1 downto 0 do
    StatusBar1.Panels [ i ].Text := '';

  Ribbon1.DocumentName := '';

end;

procedure TFormMain.UpdateStatusbar;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FFilePathName := PageControl1.ActivePage.Hint;
    Ribbon1.DocumentName := FFilePathName;
    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    StatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FFilePathName );
    StatusBar1.Panels [ 1 ].Text := ExtractFilename ( FFilePathName );
    if not FImage.Picture.Graphic.Empty then
    begin

      StatusBar1.Panels [ 2 ].Text := 'Height: ' + IntegerToString ( FImage.Picture.Graphic.Height ) + ' pixels';
      StatusBar1.Panels [ 3 ].Text := 'Width: ' + IntegerToString ( FImage.Picture.Graphic.Width ) + ' pixels ';

    end
    else
    begin

      StatusBar1.Panels [ 2 ].Text := '';
      StatusBar1.Panels [ 3 ].Text := '';

    end;

  end;

  // Get the pixelformat from the loaded graphic format
  if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.bmp' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( FImage.Picture.Bitmap.PixelFormat )
  else if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.png' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PNGBitDepthToString ( PNGBitsForPixel (
      TPNGImage ( FImage.Picture.Graphic ).Header.ColorType,
      TPNGImage ( FImage.Picture.Graphic ).Header.BitDepth
      ) )
  else if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.jpg' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + JpegPixelFormatToString ( TJPEGImage ( FImage.Picture.Graphic ).PixelFormat
      )
  else if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.gif' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( BitDepthToPixelFormat ( TGIFImage (
      FImage.Picture.Graphic ).BitsPerPixel ) );

end;

procedure TFormMain.ImageDblClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    FullScreenForm := TFullScreenForm.Create ( Self );
    try

      Screen.Cursor := crHourGlass;
      try

        // Copy image to fullscreen image}
        FullScreenForm.Image1.Picture.Bitmap.Assign ( FImage.Picture.Graphic );
        FullScreenForm.ScrollBox1.HorzScrollBar.range := FImage.Picture.Graphic.Width;
        FullScreenForm.ScrollBox1.VertScrollBar.range := FImage.Picture.Graphic.Height;
        // Show the image fullscreen}
        FullScreenForm.ShowModal;

      finally
        Screen.Cursor := crDefault;
      end;

    finally
      FullScreenForm.Free;
    end;

  end;

end;

procedure TFormMain.PageControl1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    UpdateStatusBar;

  end;
end;

procedure TFormMain.PercentItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    if PercentItem.Down then
    begin

      FImage.Stretch := False;
      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

    end
    else
    begin

      FImage.Stretch := True;

    end;

  end;

end;

procedure TFormMain.SavePictureDialog1TypeChange ( Sender: TObject );
var
  FilePath: string;
  FileName: string;
  FileExt: string;
begin

  FilePath := SavePictureDialog1.FileName;
  FileExt := ExtractFileExt ( FilePath );
  FileName := ExtractFileName ( FilePath );

end;

procedure TFormMain.ScrollBar1Change ( Sender: TObject );
begin

  ScrollBar1.Hint := IntToStr ( ScrollBar1.Position ) + '% - F12 Reset';
  Application.ActivateHint ( Mouse.CursorPos );
  PercentItem.Caption := '';
  PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
  PercentItem.Refresh;
  FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
  FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

end;

procedure TFormMain.BlackandWhite1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf1bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB1-Bit';
    BlackandWhite1.Checked := True;

  end;

end;

procedure TFormMain.N16Color1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf4bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB4-Bit';
    N16Color1.Checked := True;

  end;

end;

procedure TFormMain.N256Color1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf8bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB8-Bit';
    N256Color1.Checked := True;

  end;

end;

procedure TFormMain.N15Bit1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf15bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB15-bit';
    N15Bit1.Checked := True;

  end;

end;

procedure TFormMain.N16Bit1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf16bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB4-Bit';
    N16Bit1.Checked := True;

  end;

end;

procedure TFormMain.N24Bit1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf24bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-Bit';
    N24Bit1.Checked := True;

  end;

end;

procedure TFormMain.N32Bit1Execute ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf32bit;
    FImage.Transparent := True;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGBA32-Bit';
    N32Bit1.Checked := True;

  end;

end;

procedure TFormMain.PrintSetupItem1Click ( Sender: TObject );
begin

  PrinterSetupDialog1.Execute;

end;

procedure TFormMain.DefaultFolder1Click ( Sender: TObject );
var
  iFolder: string;
begin

  iFolder := FDefaultDirectory;
  if SelectDirectory ( 'Select Default Folder', 'Desktop', iFolder ) then
  begin

    FDefaultDirectory := iFolder;

  end;

end;

end.

