// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Description             : FrmMain Unit
// Compiler                : Delphi 2010
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
  Dialogs, ComCtrls, math, ASGCapture, ImgList, Menus, ExtDlgs, ExtCtrls,
  ToolWin, StdCtrls, Buttons, Printers, Jpeg, GIFImg, PNGImage;

type
  TFormMain = class ( TForm )
    ToolBar2: TToolBar;
    OpenItem: TToolButton;
    CloseItem: TToolButton;
    CloseAllItem: TToolButton;
    SaveItem: TToolButton;
    SaveAsItem: TToolButton;
    ToolButton7: TToolButton;
    CaptureDesktopItem: TToolButton;
    CaptureAreaItem: TToolButton;
    CaptureActiveItem: TToolButton;
    CaptureObjectItem: TToolButton;
    ExitItem: TToolButton;
    SavePictureDialog1: TSavePictureDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    NewItem1: TMenuItem;
    OpenItem1: TMenuItem;
    CloseItem1: TMenuItem;
    CloseAllItem1: TMenuItem;
    SaveItem1: TMenuItem;
    SaveAsItem1: TMenuItem;
    ExitItem1: TMenuItem;
    Capture1: TMenuItem;
    DesktopItem1: TMenuItem;
    AreaItem1: TMenuItem;
    ActiveWindowItem1: TMenuItem;
    ObjectItem1: TMenuItem;
    Miminized1: TMenuItem;
    MinimizeItem1: TMenuItem;
    AutomaticItem1: TMenuItem;
    DelayItem1: TMenuItem;
    Help1: TMenuItem;
    AboutItem1: TMenuItem;
    ASGScreenCapture1: TASGScreenCapture;
    StatusBar1: TStatusBar;
    BlackandWhite1: TMenuItem;
    N16Color1: TMenuItem;
    N256Color1: TMenuItem;
    N15Bit1: TMenuItem;
    N16Bit1: TMenuItem;
    N24Bit1: TMenuItem;
    N32Bit1: TMenuItem;
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
    N7: TMenuItem;
    Colors1: TMenuItem;
    Close1: TMenuItem;
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
    ShowHint1: TMenuItem;
    ToolBar4: TToolBar;
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
    DefaultFolder1: TMenuItem;
    ToolButton1: TToolButton;
    OpenPictureDialog1: TOpenPictureDialog;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Blog1: TMenuItem;
    ShowInfoItem1: TMenuItem;
    TaskDialog1: TTaskDialog;
    CaptureLargeIconItem: TToolButton;
    CaptureLargeIcon1: TMenuItem;
    CaptureSelection1: TMenuItem;
    SaveToDesktopItem: TToolButton;
    ImageList1: TImageList;
    CaptureSmallIconItem: TToolButton;
    ToolButton2: TToolButton;
    PercentItem: TToolButton;
    PageControl1: TPageControl;
    procedure FormCreate ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure FormCloseQuery ( Sender: TObject; var CanClose: boolean );
    procedure FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
    procedure FormResize ( Sender: TObject );
    procedure ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean;
      const r: TRect; const Msg: string );
    procedure PageControl1Change ( Sender: TObject );
    procedure CursorItem1Click ( Sender: TObject );
    procedure OpenItemClick ( Sender: TObject );
    procedure CloseItemClick ( Sender: TObject );
    procedure CloseAllItemClick ( Sender: TObject );
    procedure SaveItemClick ( Sender: TObject );
    procedure SaveAsItemClick ( Sender: TObject );
    procedure PrintItemClick ( Sender: TObject );
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
    procedure ShowHint1Click ( Sender: TObject );
    procedure CaptureWholeDesktopItemClick ( Sender: TObject );
    procedure CaptureIconItemClick ( Sender: TObject );
    procedure CaptureSelectionItemClick ( Sender: TObject );
    procedure HelpContents1Click ( Sender: TObject );
    procedure DefaultFolder1Click ( Sender: TObject );
    procedure ToolButton1Click ( Sender: TObject );
    procedure ScrollBar1Change ( Sender: TObject );
    procedure Blog1Click ( Sender: TObject );
    procedure SavePictureDialog1TypeChange ( Sender: TObject );
    procedure ShowInfoItem1Click ( Sender: TObject );
    procedure ShowTaskbarEntries1Click ( Sender: TObject );
    procedure CaptureLargeIcon1Click ( Sender: TObject );
    procedure SaveToDesktopItemClick ( Sender: TObject );
    procedure CaptureSmallIconItemClick ( Sender: TObject );
    procedure PercentItemClick ( Sender: TObject );
  private
    { Private declarations }
    FTabSheet: TTabSheet;
    FScrollBox: TScrollBox;
    FImage: TImage;
    FFileName: string;
    FExtension: string;
    FNE: string;
    FFilePathName: string;
    FFolder: string;
    FTmpFolder: string;
    FDefaultDirectory: string;
    FCaptureStr: string;
    FErrorSavingJPGFile: boolean;
    procedure GetCaptureOptions;
  public
    { Public declarations }
    procedure AddTabsheet;
    procedure UpdateControls;
    procedure ClearStatusbar;
    procedure UpdateStatusbar;
    procedure ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure ImageDblClick ( Sender: TObject );
    procedure ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
  end;

var
  FormMain: TFormMain;

implementation

uses
  Clipbrd, ShellAPI, ShlObj, ActiveX, IniFiles, FileCtrl, StrUtils, HTMLHelpViewer,
  uFullscrn, uScreenDelay, uAbout;
{$R *.DFM}

procedure FreePIDL ( PIDL: ShlObj.PItemIDList );
var
  Malloc: ActiveX.IMalloc; // shell's allocator
begin

  // Try to get shell allocator
  if Windows.Succeeded ( ShlObj.SHGetMalloc ( Malloc ) ) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    Malloc.Free ( PIDL );

end;

function IsSpecialFolderSupported ( CSIDL: Integer ): boolean;
var
  PIDL: ShlObj.PItemIDList; // PIDL of the special folder
begin

  // Try to get PIDL for folder: fails if not supported
  Result := Windows.Succeeded ( ShlObj.SHGetSpecialFolderLocation ( 0, CSIDL, PIDL ) );
  if Result then
    // Free the PIDL using shell allocator
    FreePIDL ( PIDL );

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
  PIDL: ShlObj.PItemIDList; // PIDL of the special folder
begin

  Result := '';
  // Get PIDL for required folder
  if Windows.Succeeded ( ShlObj.SHGetSpecialFolderLocation ( 0, CSIDL, PIDL ) ) then
  begin

    try
      // Get path from PIDL
      Result := PIDLToFolderPath ( PIDL );

    finally
      // Free the PIDL using shell allocator
      FreePIDL ( PIDL );
    end;

  end;

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

function DocumentsFolder: string;
// Find Documents folder location
var
  iPath: string;
begin

  if IsSpecialFolderSupported ( CSIDL_PERSONAL ) then
  begin

    iPath := SpecialFolderPath ( CSIDL_PERSONAL );
    Result := iPath;

  end
  else
    raise Exception.Create ( 'Could not find Documents folder location.' );

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

procedure EnsureFolders ( APath: string );
var
  iSlashPos: Integer; // position of last backslash in path
  iSubPath: string; // immediate parent folder of given path
begin

  // Check there's a path to create
  if Length ( APath ) = 0 then
    Exit;
  // Remove any trailing '\'
  APath := PathToDir ( APath );
  // Check if folder exists and quit if it does - we're done
  if IsDirectory ( APath ) then
    Exit;
  // Recursively call routine on immediate parent folder
  // remove bottomost folder from path - ie move up to parent folder
  iSubPath := APath;
  iSlashPos := Length ( iSubPath );
  while ( iSlashPos > 2 ) and ( iSubPath [ iSlashPos ] <> '\' ) do
    Dec ( iSlashPos );
  Delete ( iSubPath, iSlashPos, Length ( APath ) - iSlashPos + 1 );
  // do recursive call - ensures that parent folder of current path exist
  EnsureFolders ( iSubPath );
  // Create this current folder now we know parent folder exists
  SysUtils.CreateDir ( APath );

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

function JpegPixelFormatToColor ( AJpegPixelFormat: TJpegPixelFormat ): string;
// Convert TJpegPixelFormat to color string
begin

  case AJpegPixelFormat of
    jf8bit:
      Result := 'RGB8-bit'; // 8 bit
    jf24bit:
      Result := 'RGB24-bit'; // 24 bit
  end; // case

end;

function PNGBitsForPixel ( const AColorType, ABitDepth: Byte ): Integer;
// Return the BitDepth of PNGImage
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

function PNGBitDepthToColor ( ABitDepth: integer ): string;
// Convert BitDepth to color string
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

procedure TileDraw ( Canvas: TCanvas; const Rect: TRect; G: TGraphic );
// Usage:   TileDraw(Canvas, ClientRect, Image1.Picture.Graphic);
var
  r, Rows, C, Cols: Integer;
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

    for r := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw ( Rect.Left + ( C - 1 ) * G.Width, Rect.Top + ( r - 1 ) * G.Height, G );

    SelectClipRgn ( Canvas.Handle, 0 );

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

procedure TFormMain.FormCreate ( Sender: TObject );
var
  iIniFile: TIniFile;
  iIniFileName: string;
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  Caption := 'Apprehend ' + ASGScreenCapture1.Version + ' TImage Demo';
  FTmpFolder := ExtractFileDir ( Application.ExeName );
  FDefaultDirectory := DocumentsFolder;
  Application.HelpFile := 'c:\components\apprehend\APPREHEND.CHM'; // fix this hard code
  ForceDirectories ( AppDataFolder + '\Apprehend' );
  iIniFileName := AppDataFolder + '\Apprehend\ApprehendDemo.ini';
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
      MinimizeItem1.Checked := ReadBool ( 'Options', 'Minimise', True );
      AutomaticItem1.Checked := ReadBool ( 'Options', 'Automatic', True );
      CursorItem1.Checked := ReadBool ( 'Options', 'Include Cursor', False );
      ShowInfoItem1.Checked := ReadBool ( 'Options', 'ShowInfo', False );

    end;

  finally
    iIniFile.Free;
  end;

  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;
  FErrorSavingJPGFile := False;
  OpenPictureDialog1.FileName := '';
  SavePictureDialog1.FileName := '';
  UpdateControls;
  OpenPictureDialog1.Filter := Graphics.GraphicFilter ( TGraphic );
  SavePictureDialog1.Filter := Graphics.GraphicFilter ( TGraphic );
  ToolBar1.DrawingStyle := ComCtrls.dsGradient;
  ToolBar2.DrawingStyle := ComCtrls.dsGradient;
  ToolBar3.DrawingStyle := ComCtrls.dsGradient;
  ToolBar4.DrawingStyle := ComCtrls.dsGradient;
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.CHM';
  //C:\Components\Apprehend\Help\Apprehend.chm
end;

procedure TFormMain.FormDestroy ( Sender: TObject );
var
  iIniFile: TIniFile;
  iIniFileName: string;
begin
  iIniFileName := AppDataFolder + '\Apprehend\ApprehendDemo.ini';

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
      WriteBool ( 'Options', 'Minimise', MinimizeItem1.Checked );
      WriteBool ( 'Options', 'Automatic', AutomaticItem1.Checked );
      WriteBool ( 'Options', 'Include Cursor', CursorItem1.Checked );
      WriteBool ( 'Options', 'ShowInfo', ShowInfoItem1.Checked );

    end;

  finally
    iIniFile.Free;
  end;

end;

procedure TFormMain.FormCloseQuery ( Sender: TObject; var CanClose: boolean );
var
  i: Integer;
  iResult: boolean;
begin

  CanClose := True;
  UseLatestCommonDialogs := False;
  if Clipboard.HasFormat ( CF_PICTURE ) then
    case MessageDlg ( 'Clear Clipboard' + #10#13 + #10#13 +
      'The clipboard contains an image. Remove image from the clipboard?', mtConfirmation, [ mbYes, mbNo ], 0 ) of
      mrYes:
        Clipboard.Clear;
    end; // case
  UpdateControls;

  for i := 0 to PageControl1.PageCount - 1 do
  begin

    PageControl1.ActivePageIndex := i;
    if Assigned ( PageControl1.ActivePage ) then
    begin

      FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
      FFilePathName := PageControl1.ActivePage.Hint;
      CanClose := not FImage.Picture.Bitmap.Modified;
      if not CanClose then
      begin

        MessageBeep ( MB_ICONWARNING );
        TaskDialog1.Title := 'Warning';
        TaskDialog1.Flags := [ tfUseHiconMain ];
        TaskDialog1.CommonButtons := [ tcbYes, tcbNo, tcbClose ];
        TaskDialog1.Caption := 'Image Has Changed';
        TaskDialog1.Text := 'The image ' + FFilePathName + ' has changed. Save the image?';
        TaskDialog1.ExpandedText :=
          'Select Yes to save the image, select no to skip saving the image or select Close to close all images.';
        iResult := TaskDialog1.Execute;

        if iResult then
        begin

          if TaskDialog1.ModalResult = mrOk then
          begin

            if FileExists ( FFilePathName ) then
            begin
              FImage.
                Picture.Bitmap.Modified := False;
              FImage.Picture.SaveToFile ( FFilePathName );
              CanClose := True;

            end
            else
            begin

              Screen.Cursor := crHourglass;
              try

                FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

                if Length ( FTmpFolder ) = 0 then
                  SavePictureDialog1.InitialDir := FDefaultDirectory
                else
                  SavePictureDialog1.InitialDir := FTmpFolder;
                if Length ( FFolder ) = 0 then
                  FFolder := ExtractFilePath ( FFilePathName );
                if Length ( FFolder ) = 0 then
                  FFolder := FDefaultDirectory;
                SavePictureDialog1.FileName := '';
                SavePictureDialog1.FileName := ExtractFilename ( SavePictureDialog1.FileName );
                if Length ( SavePictureDialog1.FileName ) = 0 then
                  SavePictureDialog1.FileName := '*';
                SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );
                SavePictureDialog1.FileName := SavePictureDialog1.FileName + '.' + SavePictureDialog1.DefaultExt;
                FNE := FFileName + FExtension;
                FFilePathName := FFolder + FFileName + FExtension;

                if SavePictureDialog1.Execute then
                begin

                  FExtension := ExtractFileExt ( SavePictureDialog1.FileName );
                  FImage.Picture.SaveToFile ( SavePictureDialog1.FileName );
                  FFilePathName := SavePictureDialog1.FileName;
                  FFolder := ExtractFilePath ( SavePictureDialog1.FileName );
                  FFileName := ExtractFilename ( FFilePathName );
                  FExtension := ExtractFileExt ( SavePictureDialog1.FileName );
                  FNE := FFileName + FExtension;
                  PageControl1.ActivePage.Caption := ExtractFilename ( SavePictureDialog1.FileName );

                end;

              finally
                Screen.Cursor := crDefault;
              end;

            end;

          end
          else if TaskDialog1.ModalResult = mrNo then
            CanClose := True
          else
          begin

            CanClose := True;
            break;

          end;

        end
        else
          CanClose := True;
      end;

    end;

  end;

end;

procedure TFormMain.FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
begin

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

procedure TFormMain.FormResize ( Sender: TObject );
begin
  ScrollBar1.Width := ToolBar4.ClientWidth - PercentItem.Width - 10;
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
    FScrollBox.BorderStyle := bsNone;
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
    FImage.OnProgress := ProgressUpdate;
    FImage.OnDblClick := ImageDblClick;
    FImage.OnMouseDown := ImageMouseDown;
    FImage.OnMouseUp := ImageMouseUp;
    FImage.OnMouseMove := ImageMouseMove;
    FImage.PopupMenu := PopupMenu1;
    UpdateControls;
    FImage.Update;

  end;

end;

procedure TFormMain.ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean;
  const r: TRect; const Msg: string );
var
  iProgress: string;
begin

  if Stage = psRunning then
  begin

    Caption := Format ( '%d%%', [ PercentDone ] );
    iProgress := Format ( '%d%', [ PercentDone ] );
    ProgressBar1.Position := PercentDone;

  end
  else
  begin

    Caption := 'Apprehend TImage Demo';

  end;

end;

procedure TFormMain.UpdateControls;
begin

  CloseItem1.Enabled := PageControl1.PageCount <> 0;
  CloseAllItem1.Enabled := PageControl1.PageCount > 1;
  SaveItem1.Enabled := PageControl1.PageCount <> 0;
  SaveAsItem1.Enabled := PageControl1.PageCount <> 0;
  SaveToDesktopItem.Enabled := PageControl1.PageCount <> 0;
  Colors1.Enabled := PageControl1.PageCount <> 0;
  PrintItem1.Enabled := PageControl1.PageCount <> 0;
  ScrollBar1.Enabled := PageControl1.PageCount <> 0;
  CloseItem.Enabled := PageControl1.PageCount <> 0;
  CloseAllItem.Enabled := PageControl1.PageCount > 1;
  SaveItem.Enabled := PageControl1.PageCount <> 0;
  SaveAsItem.Enabled := PageControl1.PageCount <> 0;
  Colors1.Enabled := PageControl1.PageCount <> 0;
  PercentItem.Enabled := PageControl1.PageCount <> 0;

end;

procedure TFormMain.GetCaptureOptions;
begin

  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;
  ASGScreenCapture1.Auto := AutomaticItem1.Checked;
  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfoItem1.Checked;

  DelayForm := TDelayForm.Create ( Self );
  try
    ASGScreenCapture1.Delay := StrToInt ( DelayForm.edScreenDelay.Text );

  finally
    DelayForm.Free;
  end;

  ASGScreenCapture1.ShowHint := ShowHint1.Checked;

end;

procedure TFormMain.ClearStatusbar;
var
  i: Integer;
begin

  for i := StatusBar1.Panels.Count - 1 downto 0 do
    StatusBar1.Panels [ i ].Text := '';

end;

procedure TFormMain.UpdateStatusbar;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FFilePathName := PageControl1.ActivePage.Hint;
    Caption := 'Apprehend TImage Demo- ' + FFilePathName;
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

  // if the pixel format is not handled for jpegpixelformat then the image is blank
  if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.jpg' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + JpegPixelFormatToColor ( TJPEGImage ( FImage.Picture.Graphic ).PixelFormat
      )
  else if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.bmp' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToColor ( FImage.Picture.Bitmap.PixelFormat )
  else if Lowercase ( ExtractFileExt ( FFilePathName ) ) = '.png' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PNGBitDepthToColor ( PNGBitsForPixel (
      TPNGImage ( FImage.Picture.Graphic ).Header.ColorType,
      TPNGImage ( FImage.Picture.Graphic ).Header.BitDepth
      ) );

end;

procedure TFormMain.ImageDblClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    FullScreenForm := TFullScreenForm.Create ( Self );
    try

      Screen.Cursor := crHourglass;
      try

        // copy image to fullscreen image}
        FullScreenForm.Image1.Picture.Bitmap.Assign ( FImage.Picture.Graphic );
        FullScreenForm.ScrollBox1.HorzScrollBar.range := FImage.Picture.Graphic.Width;
        FullScreenForm.ScrollBox1.VertScrollBar.range := FImage.Picture.Graphic.Height;
        // show the image fullscreen}
        FullScreenForm.ShowModal;

      finally
        Screen.Cursor := crDefault;
      end;

    finally
      FullScreenForm.Free;
    end;

  end;

end;

procedure TFormMain.ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  //
end;

procedure TFormMain.ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: Integer );
begin
  //
end;

procedure TFormMain.ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  //
end;

procedure TFormMain.PageControl1Change ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    UpdateStatusbar;

  end;

end;

procedure TFormMain.PercentItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      if PercentItem.Down then
      begin

        ScrollBar1.Position := 100;
        PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
        FImage.Width := FScrollBox.ClientWidth;
        FImage.Height := FScrollBox.ClientHeight;

      end
      else
      begin

        FImage.Width := Max ( 1, Round ( FImage.Picture.Width * 100 / 100 ) );
        FImage.Height := Max ( 1, Round ( FImage.Picture.Height * 100 / 100 ) );

      end;

    end;

  end;

end;

procedure TFormMain.CursorItem1Click ( Sender: TObject );
begin

  ASGScreenCapture1.ShowCursor := CursorItem1.Checked;

end;

procedure TFormMain.OpenItemClick ( Sender: TObject );
begin

  if OpenPictureDialog1.Execute then
  begin

    Screen.Cursor := crHourglass;
    try

      ProgressBar1.Visible := True;
      // Add ScrollBox and Image Controls to a new tabsheet
      AddTabsheet;

      if Assigned ( PageControl1.ActivePage ) then
      begin

        FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

        FFilePathName := OpenPictureDialog1.FileName;

        try
          FImage.Picture.LoadFromFile ( FFilePathName );

        except
          ShowMessage ( 'Error loading image' );
          PageControl1.ActivePage.Free;
          ProgressBar1.Position := 0;
          ProgressBar1.Visible := False;
        end;

        if FImage.Picture.Graphic.Empty then
          Exit;

        FTabSheet.Caption := ExtractFilename ( FFilePathName );
        FTabSheet.Hint := FFilePathName;

        FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
        with FScrollBox do
        begin

          HorzScrollBar.range := FImage.Picture.Width;
          VertScrollBar.range := FImage.Picture.Height;

        end;

      end;

      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      FImage.Refresh;
      UpdateStatusbar;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

  ProgressBar1.Visible := False;

end;

procedure TFormMain.CloseItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // Close the active page
    PageControl1.ActivePage.Free;
    PageControl1.SelectNextPage ( False );
    UpdateControls;

  end;

  if PageControl1.PageCount = 0 then
  begin

    Caption := 'Apprehend TImage Demo';
    ClearStatusbar;

  end;

end;

procedure TFormMain.CloseAllItemClick ( Sender: TObject );
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
        PageControl1.ActivePage.Free; // Close and Free the ActivePage
        PageControl1.SelectNextPage ( False );

      end;

      UpdateControls;
      if PageControl1.PageCount = 0 then
      begin

        Caption := 'Apprehend TImage Demo';
        ClearStatusbar;

      end;

    end; Caption := 'Apprehend TImage Demo';

end;

procedure TFormMain.SaveItemClick ( Sender: TObject );
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

procedure TFormMain.SavePictureDialog1TypeChange ( Sender: TObject );
var
  FilePath: string;
  FileName: string;
  FileExt: string;
begin

  FilePath := SavePictureDialog1.FileName;
  FileExt := ExtractFileExt ( FilePath );
  FileName := ExtractFilename ( FilePath );

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
    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( FImage ) then
    begin

      // Save current file

      Screen.Cursor := crHourGlass;
      try

        FFilePathName := PageControl1.ActivePage.Hint;
        FFilePathName := IncludeTrailingPathDelimiter ( DesktopFolder ) + ExtractFilename ( FFilePathName );
        iExtension := ExtractFileExt ( FFilePathName );
        i := 0;

        // if the path contains 'Capture' then check if it exists
        if StrUtils.AnsiContainsText ( FFilePathName, 'Capture' ) then
        begin

          // Increment the capture number if the file exists
          while FileExists ( FFilePathName ) do
          begin

            Inc ( i );
            FFilePathName := DesktopFolder + 'Capture' + IntToStr ( i ) + iExtension;

          end;

        end
        else if FileExists ( FFilePathName ) then
        begin

          TaskDialog1.Title := 'File Exists';
          TaskDialog1.Caption := 'File Exists';
          TaskDialog1.Text := 'The file ' + FFilePathName + ' exists.  Overwrite the file?';
          TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
          TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
          if TaskDialog1.Execute then
            if TaskDialog1.ModalResult = mrNo then
              exit;

        end;

        if iExtension = '.jpg' then
        begin

          if Assigned ( FImage ) then
          begin

            try

              FImage.Picture.Graphic.SaveToFile ( FFilePathName );

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

              FImage.Picture.Graphic.SaveToFile ( FFilePathName );

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

              FImage.Picture.Graphic.SaveToFile ( FFilePathName );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          FImage.Picture.SaveToFile ( FFilePathName );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + FFilePathName, mtWarning, [ mbOK ], 0 );
          end;

        end;

        PageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
        PageControl1.ActivePage.Hint := FFilePathName;

        Caption := 'Apprehend 6.0 Demo- ' + FFilePathName;
        StatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FFilePathName );
        StatusBar1.Panels [ 1 ].Text := ExtractFilename ( FFilePathName );

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

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

procedure TFormMain.SaveAsItemClick ( Sender: TObject );
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

procedure TFormMain.PrintItemClick ( Sender: TObject );
var
  iAspectRatio: single;
  iOutputWidth, iOutputHeight: single;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if not PrintDialog1.Execute then
      Exit;
    Screen.Cursor := crHourglass;
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

          end
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

procedure TFormMain.CaptureDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := TBitmap.Create;
      try

        iBitmap := ASGScreenCapture1.CaptureDesktop;
        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin
        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;
      end;

      FFileName := DesktopFolder + 'Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 6;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureAreaItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      // Capture area of screen
      iBitmap := ASGScreenCapture1.CaptureSelection;
      try
        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 10;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureActiveItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureActiveWindow;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 9;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureObjectItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin
      iBitmap := ASGScreenCapture1.CaptureObject;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 11;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

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

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    FImage.Picture.Bitmap.PixelFormat := pf1bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB1-Bit';
    BlackandWhite1.Checked := True;

  end;

end;

procedure TFormMain.Blog1Click ( Sender: TObject );
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://williamwmiller.wordpress.com/' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.N16Color1Click ( Sender: TObject );
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

procedure TFormMain.N256Color1Click ( Sender: TObject );
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

procedure TFormMain.N15Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    FImage.Picture.Bitmap.PixelFormat := pf15bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB15-Bit';
    N15Bit1.Checked := True;

  end;

end;

procedure TFormMain.N16Bit1Click ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
    FImage.Picture.Bitmap.PixelFormat := pf16bit;
    FImage.Transparent := False;
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB16-Bit';
    N16Bit1.Checked := True;

  end;

end;

procedure TFormMain.N24Bit1Click ( Sender: TObject );
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

procedure TFormMain.N32Bit1Click ( Sender: TObject );
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

procedure TFormMain.MinimizeItem1Click ( Sender: TObject );
begin

  ASGScreenCapture1.Minimize := MinimizeItem1.Checked;

end;

procedure TFormMain.AutomaticItem1Click ( Sender: TObject );
begin

  ASGScreenCapture1.Auto := AutomaticItem1.Checked;

end;

procedure TFormMain.DelayItem1Click ( Sender: TObject );
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

  GetCaptureOptions;
  AddTabsheet;
  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    iBitmap := ASGScreenCapture1.CapturePolygon;
    try

      FImage.Picture.Assign ( iBitmap );

    finally
      iBitmap.Free;
    end;

  end;

  FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

  with FScrollBox do
  begin

    HorzScrollBar.range := FImage.Picture.Width;
    VertScrollBar.range := FImage.Picture.Height;

  end;

  PageControl1.ActivePage.Caption := 'Image ' + IntToStr ( PageControl1.PageCount );
  PageControl1.ActivePage.Hint := FFilePathName;
  UpdateStatusbar;
  FImage.Picture.Bitmap.PixelFormat := pfDevice;
  StatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-Bit';
  FTabSheet.ImageIndex := 3;
  ScrollBar1.Position := 100;
  PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
  FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
  FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );

end;

procedure TFormMain.Website1Click ( Sender: TObject );
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://frontiernet.net/~w2m/index.html' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.CapturePolygonItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 12;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

// Show rect size in hint during selection captures

procedure TFormMain.ShowHint1Click ( Sender: TObject );
begin

  ASGScreenCapture1.ShowHint := ShowHint1.Checked;

end;

procedure TFormMain.ShowInfoItem1Click ( Sender: TObject );
begin

  ASGScreenCapture1.ShowInfoDialog := ShowInfoItem1.Checked;

end;

procedure TFormMain.ShowTaskbarEntries1Click ( Sender: TObject );
begin
end;

// dual monitor desktop capture

procedure TFormMain.CaptureWholeDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureWholeDesktop;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 8;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureIcon;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 14;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureLargeIcon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureLargeIcon;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 13;

      StatusBar1.Panels [ 4 ].Text := 'Color Depth: RGB24-bit';
      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureSelectionItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try

        if Assigned ( iBitmap ) then
          FImage.Picture.Assign ( iBitmap )
        else
        begin

          PageControl1.ActivePage.Free;
          exit;

        end;

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 16;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.CaptureSmallIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSmallIcon;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 15;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.HelpContents1Click ( Sender: TObject );
begin

  Application.HelpShowTableOfContents;

end;

procedure TFormMain.DefaultFolder1Click ( Sender: TObject );
var
  iFolder: string;
begin

  iFolder := FDefaultDirectory;
  if SelectDirectory ( 'Select Default Folder', 'Desktop', iFolder ) then
  begin

    FDefaultDirectory := FFolder;

  end;

end;

procedure TFormMain.ToolButton1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    FImage := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( FImage ) then
    begin

      iBitmap := ASGScreenCapture1.SpeedCaptureDesktop;
      try

        FImage.Picture.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );
      with FScrollBox do
      begin

        HorzScrollBar.range := FImage.Picture.Width;
        VertScrollBar.range := FImage.Picture.Height;

      end;

      FFileName := FDefaultDirectory + '\Capture ' + IntToStr ( PageControl1.PageCount );
      FExtension := '.bmp';
      FFilePathName := FFileName + FExtension;

      PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( PageControl1.PageCount );
      PageControl1.ActivePage.Hint := FFilePathName;
      PageControl1.ActivePage.ImageIndex := 7;

      ScrollBar1.Position := 100;
      PercentItem.Caption := IntToStr ( ScrollBar1.Position ) + '%';
      FImage.Width := Max ( 1, Round ( FImage.Picture.Width * ScrollBar1.Position / 100 ) );
      FImage.Height := Max ( 1, Round ( FImage.Picture.Height * ScrollBar1.Position / 100 ) );
      UpdateStatusbar;

    end;

  end;

end;

end.

