// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright (c) 1986-2012 : Adirondack Software & Graphics
// Last Modification       : 04-06-2012
// Compiler                : Delphi 2010
// Description             : FrmMain Unit
// Requires                : DevelopersExpressBars, DeveloperExpressEditors and ImageEN
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
  ToolWin, StdCtrls, Buttons, Printers, cxImage, Jpeg, GIFImg, PNGImage,
  cxTrackBar, dxBar, cxBarEditItem, cxLookAndFeels, cxClasses, cxCheckBox,
  cxProgressBar, dxGDIPlusClasses, dxGDIPlusApi, cxGraphics, cxSpinEdit,
  cxColorComboBox, cxControls, cxLookAndFeelPainters, dxStatusBar,
  dxBarExtItems, cxSplitter, dximctrl, IEView, ImageENView, ImageENIO, ImageENProc,
  HYIEDefs, HYIEUtils, cxPCdxBarPopupMenu, cxPC;

type
  TFormMain = class ( TForm )
    SavePictureDialog1: TSavePictureDialog;
    ASGScreenCapture1: TASGScreenCapture;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    NewItem1: TdxBarButton;
    OpenItem1: TdxBarButton;
    CloseItem1: TdxBarButton;
    CloseAllItem1: TdxBarButton;
    SaveItem1: TdxBarButton;
    SaveAsItem1: TdxBarButton;
    PrintItem1: TdxBarButton;
    PrintSetupItem1: TdxBarButton;
    ExitItem1: TdxBarButton;
    File1: TdxBarSubItem;
    DesktopItem1: TdxBarButton;
    DualMonitorDesktop1: TdxBarButton;
    AreaItem1: TdxBarButton;
    ActiveWindowItem1: TdxBarButton;
    ObjectItem1: TdxBarButton;
    FreeForm1: TdxBarButton;
    CaptureIcon1: TdxBarButton;
    Capture1: TdxBarSubItem;
    BlackandWhite1: TdxBarButton;
    N16Color1: TdxBarButton;
    N256Color1: TdxBarButton;
    N16Bit1: TdxBarButton;
    N24Bit1: TdxBarButton;
    N32Bit1: TdxBarButton;
    Colors1: TdxBarSubItem;
    DefaultFolder1: TdxBarButton;
    Miminized1: TdxBarSubItem;
    Website1: TdxBarButton;
    Blog1: TdxBarButton;
    HelpContents1: TdxBarButton;
    AboutItem1: TdxBarButton;
    Help1: TdxBarSubItem;
    dxBarPopupMenu1: TdxBarPopupMenu;
    Device2: TdxBarButton;
    BlackandWhite2: TdxBarButton;
    N16Color2: TdxBarButton;
    N256Color2: TdxBarButton;
    N15BitHighColor1: TdxBarButton;
    N16BitHighColor1: TdxBarButton;
    N24BitColor1: TdxBarButton;
    N32BitColor1: TdxBarButton;
    ColorDepth2: TdxBarSubItem;
    Close1: TdxBarButton;
    dxBarManager1Bar2: TdxBar;
    dxBarManager1Bar3: TdxBar;
    cxLookAndFeelController1: TcxLookAndFeelController;
    Center1: TcxBarEditItem;
    Fit1: TcxBarEditItem;
    Size1: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    Cut1: TdxBarButton;
    Copy1: TdxBarButton;
    Paste1: TdxBarButton;
    PasteToSelection1: TdxBarButton;
    dxBarManager1Bar5: TdxBar;
    cxImageList1: TcxImageList;
    SaveToDesktop1: TdxBarButton;
    dxBarManager1Bar6: TdxBar;
    Crop1: TdxBarButton;
    Minimize1: TcxBarEditItem;
    Automatic1: TcxBarEditItem;
    IncludeCursor1: TcxBarEditItem;
    ShowInfo1: TcxBarEditItem;
    ShowHint1: TcxBarEditItem;
    Delay1: TcxBarEditItem;
    RubberbandColor1: TcxBarEditItem;
    RubberbandColor2: TcxBarEditItem;
    dxStatusBar1: TdxStatusBar;
    dxBarManager1Bar8: TdxBar;
    Undo1: TdxBarButton;
    Redo1: TdxBarButton;
    dxBarSubItem2: TdxBarSubItem;
    dxBarMRUListItem1: TdxBarMRUListItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarSubItem4: TdxBarSubItem;
    dxBarSubItem5: TdxBarSubItem;
    Animated1: TcxBarEditItem;
    dxBarPopupMenu2: TdxBarPopupMenu;
    ClearRecent1: TdxBarButton;
    dxImageListBox1: TdxImageListBox;
    cxSplitter1: TcxSplitter;
    cxImageList2: TcxImageList;
    PasteIntoNew1: TdxBarButton;
    dxBarSubItem6: TdxBarSubItem;
    FullScreen1: TdxBarButton;
    dxBarSubItem7: TdxBarSubItem;
    Rectangle1: TdxBarButton;
    None1: TdxBarButton;
    Zoom1: TdxBarButton;
    cxZoomTrackbar1: TcxBarEditItem;
    dxBarManager1Bar4: TdxBar;
    dxBarStatic1: TdxBarStatic;
    SmoothResize1: TdxBarButton;
    ImageEnProc1: TImageEnProc;
    Resize1: TdxBarButton;
    Resample: TdxBarButton;
    AlphaChannel1: TcxBarEditItem;
    Rotate1: TdxBarButton;
    Flip1: TdxBarButton;
    AutoEnhance1: TdxBarButton;
    AutoEnhance2: TdxBarButton;
    AutoEnhance3: TdxBarButton;
    Fit2: TdxBarButton;
    dxBarManager1Bar9: TdxBar;
    CaptureSmallIcon1: TdxBarButton;
    CaptureLargeIcon1: TdxBarButton;
    TaskDialog1: TTaskDialog;
    cxProgressBar1: TcxBarEditItem;
    cxImageListTabs1: TcxImageList;
    cxPageControl1: TcxPageControl;
    procedure FormCreate ( Sender: TObject );
    procedure FormDestroy ( Sender: TObject );
    procedure FormCloseQuery ( Sender: TObject; var CanClose: boolean );
    procedure FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
    procedure NewItemClick ( Sender: TObject );
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
    procedure BlackandWhite1Click ( Sender: TObject );
    procedure N16Color1Click ( Sender: TObject );
    procedure N256Color1Click ( Sender: TObject );
    procedure N16Bit1Click ( Sender: TObject );
    procedure N24Bit1Click ( Sender: TObject );
    procedure N32Bit1Click ( Sender: TObject );
    procedure AboutItem1Click ( Sender: TObject );
    procedure HelpContentsItem1Click ( Sender: TObject );
    procedure PrintSetupItem1Click ( Sender: TObject );
    procedure FreeFormCaptureClick ( Sender: TObject );
    procedure Website1Click ( Sender: TObject );
    procedure CapturePolygonItemClick ( Sender: TObject );
    procedure CaptureWholeDesktopItemClick ( Sender: TObject );
    procedure CaptureIconItemClick ( Sender: TObject );
    procedure CaptureSelectionItemClick ( Sender: TObject );
    procedure HelpContents1Click ( Sender: TObject );
    procedure DefaultFolder1Click ( Sender: TObject );
    procedure Blog1Click ( Sender: TObject );
    procedure SavePictureDialog1TypeChange ( Sender: TObject );
    procedure Size1Click ( Sender: TObject );
    procedure ExitItem1Click ( Sender: TObject );
    procedure IncludeCursor1Change ( Sender: TObject );
    procedure Delay1Change ( Sender: TObject );
    procedure Cut1Click ( Sender: TObject );
    procedure Copy1Click ( Sender: TObject );
    procedure Paste1Click ( Sender: TObject );
    procedure PasteToSelection1Click ( Sender: TObject );
    procedure Crop1Click ( Sender: TObject );
    procedure Undo1Click ( Sender: TObject );
    procedure SaveToDesktop1Click ( Sender: TObject );
    procedure Redo1Click ( Sender: TObject );
    procedure dxBarMRUListItem1Click ( Sender: TObject );
    procedure dxImageListBox1Click ( Sender: TObject );
    procedure PasteIntoNew1Click ( Sender: TObject );
    procedure FullScreen1Click ( Sender: TObject );
    procedure None1Click ( Sender: TObject );
    procedure Rectangle1Click ( Sender: TObject );
    procedure Zoom1Click ( Sender: TObject );
    procedure Fit1PropertiesChange ( Sender: TObject );
    procedure cxBarEditItem1PropertiesChange ( Sender: TObject );
    procedure Center1Exit ( Sender: TObject );
    procedure Center1PropertiesChange ( Sender: TObject );
    procedure cxZoomTrackbar1CurChange ( Sender: TObject );
    procedure SmoothResize1Click ( Sender: TObject );
    procedure ResampleClick ( Sender: TObject );
    procedure Resize1Click ( Sender: TObject );
    procedure AlphaChannel1PropertiesChange ( Sender: TObject );
    procedure Automatic1PropertiesChange ( Sender: TObject );
    procedure Rotate1Click ( Sender: TObject );
    procedure Flip1Click ( Sender: TObject );
    procedure AutoEnhance1Click ( Sender: TObject );
    procedure AutoEnhance2Click ( Sender: TObject );
    procedure AutoEnhance3Click ( Sender: TObject );
    procedure Fit2Click ( Sender: TObject );
    procedure CaptureSmallIcon1Click ( Sender: TObject );
    procedure CaptureLargeIcon1Click ( Sender: TObject );
    procedure cxPageControl1CanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
    procedure cxPageControl1Change(Sender: TObject);
    procedure cxPageControl1Click(Sender: TObject);
    procedure cxPageControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FIniFileName: string;
    FTabSheet: TcxTabSheet;
    ImageENView: TImageEnView;
    FFilePathName: string;
    FFolder: string;
    FFileName: string;
    FFileExtension: string;
    FFNE: string;
    FTmpFolder: string;
    FDefaultDirectory: string;
    FCaptureStr: string;
    FModified: boolean;
    FIEImageList: TIEImageList;
    procedure GetCaptureOptions;
    function CanPasteFromClipboard: boolean;
    procedure LoadMRFFromIni ( const IniFileName, Section: string );
    procedure SaveMRFToIni ( const IniFileName, Section: string );
    procedure ImageEnViewSelectionChange ( Sender: TObject );
    procedure CreateThumbnail ( ABitmap: TBitmap );
  public
    { Public declarations }
    procedure AddTabsheet;
    procedure UpdateControls;
    procedure ClearStatusbar;
    procedure UpdateStatusbar;
    procedure ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer );
    procedure ImageDblClick ( Sender: TObject );
    procedure ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: integer );
    procedure ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer );
    procedure ImageFinishWork ( Sender: TObject );
    procedure ImageProgress ( Sender: TObject; per: Integer );
  end;

var
  FormMain: TFormMain;

  // Minimum pixels in a dimension.
implementation

uses
  Clipbrd, ShellAPI, ShlObj, ActiveX, IniFiles, FileCtrl, HTMLHelpViewer,
  uSplash, uFullscrn, uNewImage, uResize, uRotate, uFlip, uAbout;
{$R *.DFM}

procedure FreePIDL ( PIDL: ShlObj.PItemIDList );
// Free PIDL
var
  iMalloc: ActiveX.IMalloc; // shell's allocator
begin

  // Try to get shell allocator
  if Windows.Succeeded ( ShlObj.SHGetMalloc ( iMalloc ) ) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    iMalloc.Free ( PIDL );

end;

function IsSpecialFolderSupported ( CSIDL: integer ): boolean;
// Is Special Folder Supported by the OS
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
// Convert a PIDL to a path string
begin

  // Set max length of return string
  SetLength ( Result, Windows.MAX_PATH );
  // Get the path
  if ShlObj.SHGetPathFromIDList ( PIDL, PChar ( Result ) ) then
    Result := PChar ( Result )
  else
    Result := '';

end;

function SpecialFolderPath ( CSIDL: integer ): string;
// Return a CSIDL as a path string
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
  ir: Bool;
  iPath: array [ 0..MAX_PATH ] of char;
begin

  ir := ShGetSpecialFolderPath ( 0, iPath, CSIDL_PERSONAL, False );
  if not ir then
    raise Exception.Create ( 'Could not find Documents folder location.' );
  Result := iPath;

end;

function AppDataFolder: string;
// Find AppDataFolder location
var
  ir: Bool;
  iPath: array [ 0..MAX_PATH ] of char;
begin

  ir := ShGetSpecialFolderPath ( 0, iPath, CSIDL_APPDATA, False );
  if not ir then
    raise Exception.Create ( 'Could not find AppData folder location.' );
  Result := iPath;

end;

function LocalAppDataFolder: string;
// Find LocalAppDataFolder location
var
  ir: Bool;
  iPath: array [ 0..MAX_PATH ] of char;
begin

  ir := ShGetSpecialFolderPath ( 0, iPath, CSIDL_LOCAL_APPDATA, False );
  if not ir then
    raise Exception.Create ( 'Could not find LocalAppData folder location.' );
  Result := iPath;

end;

function DesktopFolder: string;
// Find DesktopFolder folder location
var
  ir: Bool;
  iPath: array [ 0..MAX_PATH ] of char;
begin

  ir := ShGetSpecialFolderPath ( 0, iPath, CSIDL_DESKTOP, False );
  if not ir then
    raise Exception.Create ( 'Could not find Desktop folder location.' );
  Result := IncludeTrailingPathDelimiter( iPath );

end;

function PathToDir ( const Path: string ): string;
begin

  Result := Path;
  if ( Path <> '' ) and ( Path [ Length ( Path ) ] = '\' ) then
    Delete ( Result, Length ( Result ), 1 );

end;

function IsDirectory ( const DirName: string ): boolean;
// Return if a string is a folder
var
  iAttr: integer; // directory's file attributes
begin

  iAttr := SysUtils.FileGetAttr ( DirName );
  Result := ( iAttr <> -1 ) and ( iAttr and SysUtils.faDirectory = SysUtils.faDirectory );

end;

procedure TileDraw ( Canvas: TCanvas; const Rect: TRect; G: TGraphic );
// Usage:   TileDraw(Canvas, ClientRect, Image1.Picture.Graphic);
var
  r, Rows, C, Cols: integer;
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

function JustName ( const APathName: string ): string;
// Return just the name from a file string
var
  SS: string;

  function JustFilename ( const APathName: string ): string;
    // Return a filename from a string.
  var
    SS: string;
    i: integer;

    function Turn ( const S: string ): string;
    var
      i: integer;
    begin
      Result := '';
      if S <> '' then
        for i := 1 to Length ( S ) do
          Result := S [ i ] + Result;
    end;

  begin
    SS := Turn ( APathName );
    i := Pos ( '\', SS );
    if i = 0 then
      i := Pos ( ':', SS );
    if i = 0 then
      JustFilename := APathName
    else
      JustFilename := Turn ( Copy ( SS, 1, i - 1 ) );
  end;

begin
  SS := JustFilename ( APathName );
  if Pos ( '.', SS ) <> 0 then
    Result := Copy ( SS, 1, Pos ( '.', SS ) - 1 )
  else
    Result := SS
end;

function CreateNewFileName ( const Path, FileName: string ): string;
// Return a unique name for a new file with a number if necessary parameter
var
  i: integer;
  iJustName: string;
  iExtension: string;
begin

  Result := IncludeTrailingBackSlash ( Path ) + FileName;
  iJustName := JustName ( FileName );
  iExtension := ExtractFileExt ( FileName );
  i := 0;
  { check if file exists and if so add a number and try again }
  while FileExists ( Result ) do
  begin
    Inc ( i );
    Result := IncludeTrailingBackSlash ( Path ) + iJustName + IntToStr ( i ) + iExtension;
  end; { while.. }

end;

function SetInteger ( Value: integer ): string;
// For examples, 1233 returns 5 system indexes that would correspond to the
// phrases one thousand twohundred thirty three.
// *---------------------------------------------------------------------
const
  GR_ONES = 0;
  GR_THOUSANDS = 1;
  GR_MILLIONS = 2;
  GR_BILLIONS = 3;
  PL_ONES = 0;
  PL_TENS = 1;
  PL_HUNDREDS = 2;
resourcestring
  SPoint = 'point';
  SNegative = 'minus';
  SAnd = 'and';
  SCent = 'cent';
  SCents = 'cents';
  SCurrency = 'dollar';
  SCurrencies = 'dollars';
  SHundred = 'hundred';
  SOneHundred = 'one hundred';
  SOneThousand = 'one thousand';
const
  NumStrings: array [ 0..27 ] of string = ( 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
    'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen',
    'nineteen', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety' );
  GroupStrings: array [ 1..4 ] of string = ( 'thousand', 'million', 'billion', 'trillion' );
var
  num_digits, group, place, digit, CurPos: integer;
  silent_group: boolean;
  ValString: string;
begin

  ValString := IntToStr ( Value );
  // Get number of digits in the integer string
  num_digits := Length ( ValString );
  // Special case for zero
  if ValString = '0' then
  begin

    Result := NumStrings [ 0 ];
    Exit;

  end;
  Result := '';
  group := ( num_digits - 1 ) div 3;
  place := ( num_digits + 2 ) mod 3;
  CurPos := 1;
  while group >= GR_ONES do
  begin

    silent_group := True;
    while place >= PL_ONES do
    begin

      digit := Ord ( ValString [ CurPos ] ) - Ord ( '0' );
      Inc ( CurPos );
      if digit = 0 then
      begin

        Dec ( place );
        Continue;

      end;
      case place of
        PL_HUNDREDS:
          begin

            if digit > 1 then
              Result := Result + ' ' + NumStrings [ digit ] + ' ' + SHundred
            else
              Result := Result + ' ' + SOneHundred;
            silent_group := False;

          end;
        PL_TENS:
          begin

            if digit = 1 then
            begin

              // Special case 10 thru 19
              place := PL_ONES;
              digit := Ord ( ValString [ CurPos ] ) - Ord ( '0' );
              Inc ( CurPos );
              Result := Result + ' ' + NumStrings [ 10 + digit ];

            end
            else
              Result := Result + ' ' + NumStrings [ 20 + digit - 2 ];
            silent_group := False;
          end;
        PL_ONES:
          begin

            if ( group = GR_THOUSANDS ) and ( digit = 1 ) then
              Result := Result + ' ' + SOneThousand

            else
            begin

              Result := Result + ' ' + NumStrings [ digit ];
              silent_group := False;

            end;
          end;
      end; // end of case
      Dec ( place );
    end; // end of while place >= PL_ONES

    // handle thousands, millions and billions here
    if ( group > GR_ONES ) and not silent_group then
      Result := Result + ' ' + GroupStrings [ group ];
    place := PL_HUNDREDS;
    Dec ( group );
  end;

  if Result [ 1 ] = ' ' then
    Delete ( Result, 1, 1 );

end;

function IsWhiteSpace ( const Ch: char ): boolean;
// Checks if a character is white space.
begin
  Result := CharInSet ( Ch, [ ' ', #9, #10, #11, #12, #13 ] );
end;

function Str2ProperCase ( const S: string ): string;
// Converts a string to proper case.
var
  idx: integer; // loops through each character in string
  iWantCapital: boolean; // flag indicating whether captial letter required

  function IsWhiteSpace ( const Ch: char ): boolean;
    { Checks if a character is white space. }
  begin
    Result := CharInSet ( Ch, [ ' ', #9, #10, #11, #12, #13 ] );
  end;

begin

  Result := SysUtils.LowerCase ( S );
  iWantCapital := True;
  for idx := 1 to Length ( S ) do
  begin
    if CharInSet ( Result [ Idx ], [ 'a'..'z', 'A'..'Z' ] ) then
    begin

      if iWantCapital then
        Result [ Idx ] := UpCase ( Result [ Idx ] ); // capital letter required
      iWantCapital := False; // following chars lower case

    end
    else
      iWantCapital := IsWhiteSpace ( Result [ Idx ] ); // space: next char is capital
  end;

end;

function Double2NumericString ( ANumber: double; APrecision: integer; AProperCase: boolean ): string;
// Convert a Double to a string
resourcestring
  SPoint = 'point';
  SNegative = 'minus';
  SAnd = 'and';
  SCent = 'cent';
  SCents = 'cents';
  SCurrency = 'dollar';
  SCurrencies = 'dollars';
  SHundred = 'hundred';
  SOneHundred = 'one hundred';
  SOneThousand = 'one thousand';
const
  NumStrings: array [ 0..27 ] of string = ( 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
    'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen',
    'nineteen', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety' );
var
  ldigits: integer;
  rdigits: string;
  Ptr: integer;
begin

  ldigits := Trunc ( ANumber );
  if ANumber >= 0 then
    Result := SetInteger ( ldigits )
  else
    Result := SNegative + ' ' + SetInteger ( -ldigits );
  if APrecision > 0 then
  begin

    rdigits := Format ( '%.*f', [ APrecision, Abs ( Frac ( ANumber ) ) ] );
    Delete ( rdigits, 1, Pos ( DecimalSeparator, rdigits ) );
    Result := Result + ' ' + SPoint;
    Ptr := 1;

    while Ptr <= Length ( rdigits ) do
    begin

      if rdigits [ Ptr ] <> '0' then
        Break;
      Result := Result + ' ' + NumStrings [ Ord ( rdigits [ Ptr ] ) - Ord ( '0' ) ];
      Inc ( Ptr );

    end;

    ldigits := StrToInt ( rdigits );
    if ldigits > 0 then
      if AProperCase then
        Result := Str2ProperCase ( Result + ' ' + SetInteger ( ldigits ) )
      else
        Result := Result + ' ' + SetInteger ( ldigits );
  end;

end;

function IntToNumberText ( const Value: integer ): string;
// Takes any integer as input and returns the integer's value as English text.
const
  grOnes = 0; // group < 1,000
  grThousands = 1; // 1,000 <= group < 1,000,000
  plOnes = 0; // "ones" place within group
  plTens = 1; // "tens" place within group
  plHundreds = 2; // "hnudreds place within group
resourcestring
  // various number items
  SHundred = 'hundred';
  SOneHundred = 'one hundred';
  SOneThousand = 'one thousand';
  sMinus = 'minus';
const
  // map of number names
  cNumStrings: array [ 0..27 ] of string = ( 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
    'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen',
    'nineteen', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety' );
  // array of "group" names
  cGroupStrings: array [ 1..4 ] of string = ( 'thousand', 'million', 'billion', 'trillion' );
var
  NumDigits: integer; // number of digits in value
  group: integer; // index of group of 1000s
  place: integer; // place of current digit within group
  digit: integer; // current digit
  CurPos: integer; // position in ValString of current digit
  SilentGroup: boolean; // whether group is rendered
  ValString: string; // string representation of Value
begin

  // Special case for zero
  if Value = 0 then
  begin

    Result := cNumStrings [ 0 ];
    Exit;

  end;
  ValString := SysUtils.IntToStr ( Abs ( Value ) );
  NumDigits := Length ( ValString );
  if Value > 0 then
    Result := ''
  else
    Result := sMinus;
  group := ( NumDigits - 1 ) div 3;
  place := ( NumDigits + 2 ) mod 3;
  CurPos := 1;

  while group >= grOnes do
  begin

    SilentGroup := True;
    while place >= plOnes do
    begin

      digit := Ord ( ValString [ CurPos ] ) - Ord ( '0' );
      Inc ( CurPos );
      if digit = 0 then
      begin

        Dec ( place );
        Continue;

      end;

      case place of
        plHundreds:
          begin

            if digit > 1 then
              Result := Result + ' ' + cNumStrings [ digit ] + ' ' + SHundred
            else
              Result := Result + ' ' + SOneHundred;
            SilentGroup := False;

          end;
        plTens:
          begin

            if digit = 1 then
            begin

              // Special case 10 thru 19
              place := plOnes;
              digit := Ord ( ValString [ CurPos ] ) - Ord ( '0' );
              Inc ( CurPos );
              Result := Result + ' ' + cNumStrings [ 10 + digit ];

            end
            else
              Result := Result + ' ' + cNumStrings [ 20 + digit - 2 ];
            SilentGroup := False;

          end;
        plOnes:
          begin

            if ( group = grThousands ) and ( digit = 1 ) then
              Result := Result + ' ' + SOneThousand

            else
            begin

              Result := Result + ' ' + cNumStrings [ digit ];
              SilentGroup := False;

            end;

          end;
      end;
      Dec ( place );
    end;

    // handle thousands, millions and billions here
    if ( group > grOnes ) and not SilentGroup then
      Result := Result + ' ' + cGroupStrings [ group ];
    place := plHundreds;
    Dec ( group );

  end;

  if Result [ 1 ] = ' ' then
    Delete ( Result, 1, 1 );

end;

function FloatToFixed ( const Value: Extended; const DecimalPlaces: Byte; const SeparateThousands: boolean ): string;
// Converts a floating point value to a fixed format string. The number of
// decimal places and whether thousands should be separated can be specified.
const
  // float format specifiers
  iFmtSpec: array [ boolean ] of char = ( 'f', 'n' );
begin

  Result := SysUtils.Format ( '%.*' + iFmtSpec [ SeparateThousands ], [ DecimalPlaces, Value ] );

end;

function Int64ToFixed ( const Value: Int64; const SeparateThousands: boolean ): string;
// Converts a 64 bit integer into its string representation, with thousands
// optionally separated.
begin

  Result := FloatToFixed ( Value, 0, SeparateThousands );

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

function GetGenericFileType ( AExtension: string ): string;
// Get file type for an extension
var
  iInfo: TSHFileInfo;
begin

  SHGetFileInfo ( PChar ( AExtension ), FILE_ATTRIBUTE_NORMAL, iInfo, SizeOf ( iInfo ),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES );
  Result := iInfo.szTypeName;

end;

procedure SmoothReSize ( Src, Dest: TBitmap );
// Smooth resize a bitmap
type
  PRGB24 = ^TRGB24;

  TRGB24 = record
    B, G, r: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array [ 0..0 ] of TRGB24;
var
  X, Y, px, py: integer;
  i, x1, x2, z, z2, iz2: integer;
  w1, w2, w3, w4: integer;
  Ratio: integer;
  sDst, sDstOff: integer;
  PScanLine: array of PRGBArray;
  Src1, Src2: PRGBArray;
  C, C1, C2: TRGB24;
begin

  if ( Dest.Width < 2 ) or ( Dest.Height < 2 ) then
  begin
    Dest.Assign ( Src );
  end;

  SetLength ( PScanLine, Src.Height );
  PScanLine [ 0 ] := ( Src.Scanline [ 0 ] );
  i := integer ( Src.Scanline [ 1 ] ) - integer ( PScanLine [ 0 ] );
  for Y := 1 to Src.Height - 1 do
    PScanLine [ Y ] := PRGBArray ( integer ( PScanLine [ Y - 1 ] ) + i );
  sDst := integer ( Dest.Scanline [ 0 ] );
  sDstOff := integer ( Dest.Scanline [ 1 ] ) - sDst;
  Ratio := ( ( Src.Width - 1 ) shl 15 ) div Dest.Width;
  py := 0;

  for Y := 0 to Dest.Height - 1 do
  begin
    i := py shr 15;
    if i > Src.Height - 1 then
      i := Src.Height - 1;
    Src1 := PScanLine [ i ];
    if i < Src.Height - 1 then
      Src2 := PScanLine [ i + 1 ]
    else
      Src2 := Src1;
    z2 := py and $7FFF;
    iz2 := $8000 - z2;
    px := 0;
    for X := 0 to Dest.Width - 1 do
    begin

      x1 := px shr 15;
      x2 := x1 + 1;
      C1 := Src1 [ x1 ];
      C2 := Src2 [ x1 ];
      z := px and $7FFF;
      w2 := ( z * iz2 ) shr 15;
      w1 := iz2 - w2;
      w4 := ( z * z2 ) shr 15;
      w3 := z2 - w4;
      C.r := ( C1.r * w1 + Src1 [ x2 ].r * w2 + C2.r * w3 + Src2 [ x2 ].r * w4 ) shr 15;
      C.G := ( C1.G * w1 + Src1 [ x2 ].G * w2 + C2.G * w3 + Src2 [ x2 ].G * w4 ) shr 15;
      C.B := ( C1.B * w1 + Src2 [ x2 ].B * w2 + C2.B * w3 + Src2 [ x2 ].B * w4 ) shr 15;
      PRGBArray ( sDst ) [ X ] := C;
      Inc ( px, Ratio );

    end;

    sDst := sDst + sDstOff;
    Inc ( py, Ratio );

  end;

  SetLength ( PScanLine, 0 );

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

function IEFileTypeToExtension ( AIEFileType: TIOFileType ): string;
// Convert AIEFileType to a file extension
begin

  case AIEFileType of
    ioTIFF:
      Result := '.tif';
    ioGIF:
      Result := '.gif';
    ioJPEG:
      Result := '.jpg';
    ioPCX:
      Result := '.pcx';
    ioBMP:
      Result := '.bmp';
    ioICO:
      Result := '.ico';
    ioCUR:
      Result := '.cur';
    ioPNG:
      Result := '.png';
    ioWMF:
      Result := '.wmf';
    ioEMF:
      Result := '.emf';
    ioTGA:
      Result := '.tga';
    ioPXM:
      Result := '.ppm';
    ioJP2:
      Result := '.jp2';
    ioJ2K:
      Result := '.j2k';
    ioAVI:
      Result := '.avi';
    ioWBMP:
      Result := '.wbmp';
    ioPS:
      Result := '.ps';
    ioPDF:
      Result := '.pdf';
    ioDCX:
      Result := '.dcx';
    ioRAW:
      Result := '.raw';
    ioBMPRAW:
      Result := '.braw';
    ioWMV:
      Result := '.wmv';
    ioMPEG:
      Result := '.mpeg';
    ioPSD:
      Result := '.psd';
    ioIEV:
      Result := '.iev';
    ioHDP:
      Result := '.wdp';
    ioLYR:
      Result := '.lyr';
    ioALL:
      Result := '.all';
    ioDICOM:
      Result := '.dcm';
  end;

end;

function StripNonConforming( const AString: string; const ValidChars: TSysCharSet ): string;
{Strip nonconforming values from a string}
var
  DestI: integer;
  SourceI: integer;
begin
  SetLength( Result, Length( AString ) );
  DestI := 0;
  for SourceI := 1 to Length( AString ) do
{$IFDEF UNICODE}
    if SysUtils.CharInSet( AString[ SourceI ], ValidChars ) then
{$ELSE}
    if AString[ SourceI ] in ValidChars then
{$ENDIF}
    begin
      inc( DestI );
      Result[ DestI ] := AString[ SourceI ]
    end;
  SetLength( Result, DestI );
end;

function StripNumeric( const AString: string ): string;
{Strip numeric values from a string}
begin
  Result := StripNonConforming( AString, [ 'a'..'z', 'A'..'Z', ' ' ] );
end;

function StripNonNumeric( const AString: string ): string;
{Strip non-numeric values from a string}
begin
  Result := StripNonConforming( AString, [ '0'..'9', '.' ] );
end;

procedure TFormMain.AutoEnhance1Click ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        ImageENView.Proc.SaveUndoCaptioned ( 'Auto Enhance One ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
        Undo1.Hint := 'Auto Enhance One ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
        ImageENView.Proc.AutoImageEnhance1 ( );
        ImageENView.Update;
        UpdateControls;
        UpdateStatusbar;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );
        finally
          iBitmap.Free;
        end;

        // Delete the selected thumbnail
        cxImageList2.Delete ( dxImageListBox1.ItemIndex );
        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;

      end;

    finally
      Screen.Cursor := crDefault
    end;

  end;

end;

procedure TFormMain.AutoEnhance2Click ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        ImageENView.Proc.SaveUndoCaptioned ( 'Auto Enhance Two ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
        Undo1.Hint := 'Auto Enhance Two ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
        ImageENView.Proc.AutoImageEnhance2 ( );
        ImageENView.Update;
        UpdateControls;
        UpdateStatusbar;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );
        finally
          iBitmap.Free;
        end;

        // Delete the selected thumbnail
        cxImageList2.Delete ( dxImageListBox1.ItemIndex );
        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;

      end;

    finally
      Screen.Cursor := crDefault
    end;

  end;

end;

procedure TFormMain.AutoEnhance3Click ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        ImageENView.Proc.SaveUndoCaptioned ( 'Auto Enhance Three ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
        Undo1.Hint := 'Auto Enhance Three ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
        ImageENView.Proc.AutoImageEnhance3 ( );
        ImageENView.Update;
        UpdateControls;
        UpdateStatusbar;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );
        finally
          iBitmap.Free;
        end;

        // Delete the selected thumbnail
        cxImageList2.Delete ( dxImageListBox1.ItemIndex );
        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;

      end;

    finally
      Screen.Cursor := crDefault
    end;

  end;

end;

procedure TFormMain.Automatic1PropertiesChange ( Sender: TObject );
begin

  ASGScreenCapture1.Auto := Automatic1.EditValue;

end;

procedure TFormMain.GetCaptureOptions;
begin

  ASGScreenCapture1.Minimize := Minimize1.EditValue;
  ASGScreenCapture1.Auto := Automatic1.EditValue;
  ASGScreenCapture1.ShowCursor := IncludeCursor1.EditValue;
  ASGScreenCapture1.ShowInfoDialog := ShowInfo1.EditValue;
  ASGScreenCapture1.Delay := Delay1.EditValue;
  ASGScreenCapture1.ShowHint := ShowHint1.EditValue;

end;

procedure TFormMain.FormCreate ( Sender: TObject );
var
  iIniFile: TIniFile;
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  FormSplash := TFormSplash.Create ( Application );

  FModified := False;

  ForceDirectories ( AppDataFolder + '\Apprehend' );
  FIniFileName := AppDataFolder + '\Apprehend\ApprehendDXImageDemo.ini';

  iIniFile := TIniFile.Create ( FIniFileName );
  try

    with iIniFile do
    begin

      Left := ReadInteger ( 'Main Form', 'Left', 0 );
      Top := ReadInteger ( 'Main Form', 'Top', 0 );
      Width := ReadInteger ( 'Main Form', 'Width', 700 );
      Height := ReadInteger ( 'Main Form', 'Height', 500 );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );

      LoadMRFFromIni ( FIniFileName, 'MRF' );
      FDefaultDirectory := ReadString ( 'Options', 'Folder', FDefaultDirectory );
      Minimize1.EditValue := ReadBool ( 'Options', 'Minimise', True );
      Automatic1.EditValue := ReadBool ( 'Options', 'Automatic', False );
      IncludeCursor1.EditValue := ReadBool ( 'Options', 'Include Cursor', False );
      ShowInfo1.EditValue := ReadBool ( 'Options', 'ShowInfo', False );

    end;

  finally
    iIniFile.Free;
  end;
  FormSplash.Show;
  FormSplash.Left := Left + Width div 4;
  FormSplash.Top := Top + Height div 4;
  Caption := 'Apprehend ' + '(' + ASGScreenCapture1.Version + ')';
  FTmpFolder := ExtractFileDir ( Application.ExeName );
  FDefaultDirectory := DocumentsFolder;
  Application.HelpFile := 'Apprehend.chm';

  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  ASGScreenCapture1.Minimize := Minimize1.EditValue;
  ASGScreenCapture1.Auto := Automatic1.EditValue;
  ASGScreenCapture1.ShowCursor := IncludeCursor1.EditValue;
  OpenPictureDialog1.FileName := '';
  SavePictureDialog1.FileName := '';
  UpdateControls;
  cxLookAndFeelController1.NativeStyle := False;
  OpenPictureDialog1.Filter := Graphics.GraphicFilter ( TGraphic );
  SavePictureDialog1.Filter := Graphics.GraphicFilter ( TGraphic );

  // Setup the HelpFile
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.chm';
  FIEImageList := TIEImageList.Create;

  UpdateControls;

end;

procedure TFormMain.FormDestroy ( Sender: TObject );
var
  iIniFile: TIniFile;
begin

  iIniFile := TIniFile.Create ( FIniFileName );
  try

    with iIniFile do
    begin

      SaveMRFToIni ( FIniFileName, 'MRF' );
      WriteInteger ( 'Main Form', 'Left', Left );
      WriteInteger ( 'Main Form', 'Top', Top );
      WriteInteger ( 'Main Form', 'Width', Width );
      WriteInteger ( 'Main Form', 'Height', Height );
      WindowState := TWindowState ( ReadInteger ( 'Main Form', 'Window State', 0 ) );
      WriteString ( 'Options', 'Folder', FDefaultDirectory );
      WriteString ( 'Options', 'Folder', FDefaultDirectory );
      WriteBool ( 'Options', 'Minimise', Minimize1.EditValue );
      WriteBool ( 'Options', 'Automatic', Automatic1.EditValue );
      WriteBool ( 'Options', 'Include Cursor', IncludeCursor1.EditValue );
      WriteBool ( 'Options', 'ShowInfo', ShowInfo1.EditValue );

    end;

  finally
    iIniFile.Free;
  end;

  FIEImageList.Free;

end;

procedure TFormMain.Flip1Click ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FormFlip := TFormFlip.Create ( self );
      try

        FormFlip.ImageEnView1.Assign ( ImageENView );
        if FormFlip.ShowModal = mrOk then
        begin

          ImageENView.Proc.SaveUndo ( ieuImage );
          Undo1.Hint := 'Flip ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
          ImageENView.Proc.Flip ( FormFlip.FlipDirection );
          ImageENView.Update;

        end;

      finally
        FormFlip.Free;
      end;

    end;

  end;

end;

procedure TFormMain.FormCloseQuery ( Sender: TObject; var CanClose: boolean );
// Prompt to clear the clipboard
var
  i: integer;
begin

  CanClose := True;
  UseLatestCommonDialogs := False;
  if Clipboard.HasFormat ( CF_PICTURE ) then
  begin

    MessageBeep ( MB_ICONWARNING );
    TaskDialog1.Title := 'Warning';
    TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
    TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
    TaskDialog1.Caption := 'Clear Clipboard';
    TaskDialog1.Text := 'The clipboard contains an image. Remove the image from the clipboard?';
    TaskDialog1.FooterText := '';

    if TaskDialog1.Execute then
      if TaskDialog1.ModalResult = mrYes then
        Clipboard.Clear;

  end;

  UpdateControls;

  for i := 0 to cxPageControl1.PageCount - 1 do
  begin

    if Assigned ( cxPageControl1.ActivePage ) then
    begin

      ImageENView := TImageEnView ( cxPageControl1.Pages [ i ].Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        FFilePathName := ImageENView.IO.Params.FileName;
        if not FileExists ( FFilePathName ) then
          FModified := True
        else
          FModified := ImageENView.Proc.UndoCount <> 0;
        CanClose := not FModified;

        if FModified then
        begin

          MessageBeep ( MB_ICONWARNING );
          TaskDialog1.Title := 'Warning';
          TaskDialog1.CommonButtons := [ tcbYes, tcbNo, tcbCancel ];
          TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
          TaskDialog1.Caption := 'Image Has Changed';
          TaskDialog1.Text := 'The image ' + FFilePathName + ' has changed. Save the image?';
          TaskDialog1.FooterText :=
            'Select Yes to save.  Select No to exit without saving current image.  Select Cancel to exit without saving all images';
          if TaskDialog1.Execute then

            if TaskDialog1.ModalResult = mrYes then
            begin

              if FileExists ( FFilePathName ) then
              begin

                ImageENView.IO.SaveToFile ( FFilePathName );
                FModified := False;
                CanClose := True;

              end
              else
              begin

                ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

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
                FFNE := FFileName + FFileExtension;
                FFilePathName := FFolder + FFileName + FFileExtension;

                if SavePictureDialog1.Execute then
                begin

                  Screen.Cursor := crHourglass;
                  try
                    FFileExtension := ExtractFileExt ( SavePictureDialog1.FileName );
                    ImageENView.IO.SaveToFile ( SavePictureDialog1.FileName );
                    FFilePathName := SavePictureDialog1.FileName;
                    FFolder := ExtractFilePath ( SavePictureDialog1.FileName );
                    FFileName := ExtractFilename ( FFilePathName );
                    FFileExtension := ExtractFileExt ( SavePictureDialog1.FileName );
                    FFNE := FFileName + FFileExtension;
                    dxBarMRUListItem1.AddItem ( FFilePathName, nil );
                    cxPageControl1.ActivePage.Caption := ExtractFilename ( SavePictureDialog1.FileName );

                  finally
                    Screen.Cursor := crDefault;
                  end;

                end;

              end;

            end
            else if TaskDialog1.ModalResult = mrNo then
            begin

              CanClose := True;

            end
            else if TaskDialog1.ModalResult = mrCancel then
            begin

              CanClose := True;
              Break;

            end;

        end;

      end;

    end;

  end;

end;

procedure TFormMain.FormKeyDown ( Sender: TObject; var Key: word; Shift: TShiftState );
// Close the app on VK_ESCAPE
begin

  if Key = VK_ESCAPE then
    Close;

end;

procedure TFormMain.AddTabsheet;
// Add a tabsheet to the pagecontrol
begin

  // Create a new tabsheet
  with cxPageControl1 do
    FTabSheet := TcxTabSheet.Create ( self );

  // Set the tabsheet.pagecontrol to PageControl1
  FTabSheet.PageControl := cxPageControl1;

  // Set the activepage to tabsheet
  cxPageControl1.ActivePage := FTabSheet;

  with FTabSheet do
  begin

    ShowHint := True;
    ImageENView := TImageEnView.Create ( FTabSheet );
    ImageENView.Parent := FTabSheet;
    ImageENView.Left := 0;
    ImageENView.Top := 0;
    ImageENView.Align := alClient;
    ImageENView.Center := Center1.EditValue;
    ImageENView.Cursor := 1784;
    ImageENView.AutoStretch := False;
    ImageENView.Background := clWhite;
    ImageENView.Visible := True;
    ImageENView.ShowHint := True;
    ImageENView.BorderStyle := bsNone;
    ImageENView.IO.Params.BMP_HandleTransparency := True;
    ImageENView.MouseInteract := [ miSelect ];
    ImageENView.Proc.UndoLimit := 99;
    ImageENView.Proc.AutoUndo := False;
    ImageENView.OnDblClick := ImageDblClick;
    ImageENView.OnMouseDown := ImageMouseDown;
    ImageENView.OnMouseUp := ImageMouseUp;
    ImageENView.OnMouseMove := ImageMouseMove;
    ImageENView.OnSelectionChange := ImageEnViewSelectionChange;
    ImageENView.OnProgress := ImageProgress;
    ImageENView.OnFinishWork := ImageFinishWork;

  end;

  FModified := False;

end;

procedure TFormMain.AlphaChannel1PropertiesChange ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin
      ImageENView.EnableAlphaChannel := AlphaChannel1.EditValue;
      ImageENView.Update;
    end;

  end;

end;

function TFormMain.CanPasteFromClipboard: boolean;
// Return if CanPasteFromClipboard
begin

  Result := False;
  if Assigned ( cxPageControl1.ActivePage ) then
    Result := Clipboard.HasFormat ( CF_BITMAP );

end;

procedure TFormMain.UpdateControls;
// Update GUI
begin

  CloseItem1.Enabled := cxPageControl1.PageCount <> 0;
  CloseAllItem1.Enabled := cxPageControl1.PageCount > 1;
  SaveItem1.Enabled := cxPageControl1.PageCount <> 0;
  SaveAsItem1.Enabled := cxPageControl1.PageCount <> 0;
  SaveToDesktop1.Enabled := cxPageControl1.PageCount <> 0;

  Colors1.Enabled := cxPageControl1.PageCount <> 0;
  PrintItem1.Enabled := cxPageControl1.PageCount <> 0;
  cxZoomTrackbar1.Enabled := cxPageControl1.PageCount <> 0;
  Fit2.Enabled := cxPageControl1.PageCount <> 0;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      Cut1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Copy1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Paste1.Enabled := ImageENView.Proc.IsClipboardAvailable;
      PasteToSelection1.Enabled := ImageENView.Proc.IsClipboardAvailable;
      Crop1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Undo1.Enabled := ImageENView.Proc.UndoCount > 0;
      Redo1.Enabled := ImageENView.Proc.RedoCount > 0;

    end;

  end
  else
  begin

    Cut1.Enabled := False;
    Copy1.Enabled := False;
    Paste1.Enabled := False;
    PasteToSelection1.Enabled := False;
    Crop1.Enabled := False;
    Undo1.Enabled := False;
    Redo1.Enabled := False;

  end;

end;

procedure TFormMain.ClearStatusbar;
// Clear the statusbar
var
  i: integer;
begin

  for i := dxStatusBar1.Panels.Count - 1 downto 0 do
    dxStatusBar1.Panels [ i ].Text := '';

  Caption := 'Apprehend';

end;

procedure TFormMain.UpdateStatusbar;
// Update the statusbar
var
  iWidth: integer;
  iHeight: integer;
  iExtension: string;
  iBitDepth: integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FFilePathName := cxPageControl1.ActivePage.Hint;
      dxImageListBox1.Hint := FFilePathName;
      iExtension := ExtractFileExt ( cxPageControl1.ActivePage.Hint );
      Caption := 'Apprehend 6.0- ' + FFilePathName;
      dxStatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FFilePathName );
      dxStatusBar1.Panels [ 1 ].Text := ExtractFilename ( FFilePathName );
      iWidth := ImageENView.Bitmap.Width;
      iHeight := ImageENView.Bitmap.Height;

      if ( iWidth >= 0 ) then
        dxStatusBar1.Panels [ 2 ].Text := 'Width: ' + IntegerToString ( iWidth ) + ' pixels'
      else
        dxStatusBar1.Panels [ 2 ].Text := '';
      if ( iHeight >= 0 ) then
        dxStatusBar1.Panels [ 3 ].Text := 'Height: ' + IntegerToString ( iHeight ) + ' pixels'
      else
        dxStatusBar1.Panels [ 3 ].Text := '';

      iBitDepth := ImageENView.IO.Params.BitsPerSample * ImageENView.IO.Params.SamplesPerPixel;
      if iBitDepth = 32 then
        dxStatusBar1.Panels [ 6 ].Text := ' Color Depth: ' + 'ARGB ' + IntToStr ( iBitDepth ) + '-bit'
      else
        dxStatusBar1.Panels [ 6 ].Text := ' Color Depth: ' + 'RGB ' + IntToStr ( iBitDepth ) + '-bit';

      dxStatusBar1.Panels [ 7 ].Text := 'Type: ' + GetGenericFileType ( iExtension );
      dxStatusBar1.Panels [ 8 ].Text := 'Undo: ' + IntToStr ( ImageENView.Proc.UndoCount );
      dxBarStatic1.Caption := IntToStr ( Round ( ImageENView.Zoom ) ) + '%';
      cxZoomTrackbar1.EditValue := Round ( ImageENView.Zoom );

    end
    else
    begin

      dxBarStatic1.Caption := '100%';
      cxZoomTrackbar1.EditValue := 100;
      ClearStatusbar;

    end;

  end;

end;

procedure TFormMain.LoadMRFFromIni ( const IniFileName, Section: string );
// Load settings from ini file
var
  i: integer;
  iIni: TIniFile;
  iFileName: string;
  iPrefix: string;
begin

  dxBarMRUListItem1.Items.Clear;

  iIni := TIniFile.Create ( IniFileName );
  try

    iPrefix := 'MRF';
    for i := 0 to dxBarMRUListItem1.MaxItemCount do
    begin

      iFileName := iIni.ReadString ( Section, Format ( '%s%d', [ iPrefix, i ] ), '' );
      if iFileName = '' then
        Break
      else if dxBarMRUListItem1.Items.IndexOf ( iFileName ) < 0 then
        dxBarMRUListItem1.Items.Append ( iFileName );

    end;

  finally
    iIni.Free;
  end;

end;

procedure TFormMain.SaveMRFToIni ( const IniFileName, Section: string );
// Save settings from ini file
var
  i: integer;
  iIni: TIniFile;
  iPrefix: string;
begin

  iIni := TIniFile.Create ( IniFileName );
  try

    iIni.EraseSection ( Section );
    iPrefix := 'MRF';
    for i := 0 to dxBarMRUListItem1.Items.Count - 1 do
      iIni.WriteString ( Section, Format ( '%s%d', [ iPrefix, i ] ), dxBarMRUListItem1.Items [ i ] );

  finally
    iIni.Free;
  end;

end;

procedure TFormMain.ImageFinishWork ( Sender: TObject );
begin

  cxProgressBar1.EditValue := 0;

end;

procedure TFormMain.ImageProgress ( Sender: TObject; per: Integer );
begin

  cxProgressBar1.EditValue := per;

end;

procedure TFormMain.ImageDblClick ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );
    if Assigned ( ImageENView ) then
    begin

      FullScreenForm := TFullScreenForm.Create ( self );
      try

        Screen.Cursor := crHourglass;
        try

          // copy ImageENView to fullscreen ImageENView}
          FullScreenForm.ImageEnView1.Bitmap.Assign ( ImageENView.Bitmap );
          // show the ImageENView fullscreen}
          FullScreenForm.ShowModal;

        finally
          Screen.Cursor := crDefault;
        end;

      finally
        FullScreenForm.Free;
      end;

    end;

  end;

end;

procedure TFormMain.dxBarMRUListItem1Click ( Sender: TObject );
// Open MRU file
var
  i: integer;
  iBitmap: TBitmap;
begin

  FFilePathName := dxBarMRUListItem1.Items [ dxBarMRUListItem1.ItemIndex ];
  FFileName := ExtractFilename ( dxBarMRUListItem1.Items [ dxBarMRUListItem1.ItemIndex ] );
  FFileExtension := ExtractFileExt ( dxBarMRUListItem1.Items [ dxBarMRUListItem1.ItemIndex ] );

  if FileExists ( FFilePathName ) then
  begin

    Screen.Cursor := crHourglass;
    try

      // Add cxImage Control to a new tabsheet
      AddTabsheet;

      if Assigned ( cxPageControl1.ActivePage ) then
      begin

        ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

        if Assigned ( ImageENView ) then
        begin
          try

            ImageENView.IO.LoadFromFile ( FFilePathName );
            ImageENView.Update;
            dxBarMRUListItem1.AddItem ( FFilePathName, nil );

          except
            ShowMessage ( 'Error loading ImageENView' );
          end;

          cxPageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
          cxPageControl1.ActivePage.Hint := FFilePathName;

          // Create a thumbnail for dxImageListBox1

          iBitmap := TBitmap.Create;
          try

            iBitmap.Assign ( ImageENView.Bitmap );
            CreateThumbnail ( iBitmap );

          finally
            iBitmap.Free;
          end;

          // Add thumbnail to dxImageListBox1
          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;

          ImageENView.Update;
          UpdateControls;
          UpdateStatusbar;

        end;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.dxImageListBox1Click ( Sender: TObject );
// Sync the PageControl to the dxImageListBox1
begin

  cxPageControl1.ActivePageIndex := dxImageListBox1.ItemIndex;

end;

procedure TFormMain.Rectangle1Click ( Sender: TObject );
// Set rectangular selection
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );
    if Assigned ( ImageENView ) then
    begin
      ImageENView.MouseInteract := [ miSelect ];
    end;

  end;

end;

procedure TFormMain.Redo1Click ( Sender: TObject );
// Redo the last undo
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        with ImageENView.Proc do
        begin
          SaveUndo; // saves in Undo List
          Redo;
        end;

        UpdateControls;
        UpdateStatusbar;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );
        finally
          iBitmap.Free;
        end;

        // Delete the selected thumbnail
        cxImageList2.Delete ( dxImageListBox1.ItemIndex );
        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;
      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.ResampleClick ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FormResizeResample := TFormResizeResample.Create ( self );
      try

        with ImageENView do
        begin

          FormResizeResample.Resample := True;
          FormResizeResample.Resize := False;
          FormResizeResample.Caption := 'Resample';
          FormResizeResample.ImageEnView1.IEBitmap.Assign ( IEBitmap );
          FormResizeResample.ImageEnView1.IO.Params.Assign ( IO.Params );
          FormResizeResample.ImageEnView1.Update;
          FormResizeResample.Width1.Text := IntToStr ( IEBitmap.Width );
          FormResizeResample.Height1.Text := IntToStr ( IEBitmap.Height );

          if FormResizeResample.ShowModal = mrOk then
          begin

            Screen.Cursor := crHourglass;
            try

              ImageENView.Proc.SaveUndoCaptioned ( 'Smooth Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
              Undo1.Hint := 'Smooth Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
              Proc.Resample ( StrToInt ( FormResizeResample.Width1.Text ), StrToInt ( FormResizeResample.Height1.Text ),
                TResampleFilter ( FormResizeResample.ComboBoxResampleFilter1.ItemIndex ) );
              IO.Params.Width := StrToInt ( FormResizeResample.Width1.Text );
              IO.Params.Height := StrToInt ( FormResizeResample.Height1.Text );
              iBitmap := TBitmap.Create;
              try
                iBitmap.Assign ( ImageENView.Bitmap );
                // Create a thumbnail for dxImageListBox1
                CreateThumbnail ( iBitmap );
              finally
                iBitmap.Free;
              end;

              // Delete the selected thumbnail
              cxImageList2.Delete ( dxImageListBox1.ItemIndex );
              // Add thumbnail to dxImageListBox1
              i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
              if dxImageListBox1.CanFocus = True then
                dxImageListBox1.SetFocus;
              dxImageListBox1.Hint := FFilePathName;
              dxImageListBox1.ItemIndex := i;
              dxImageListBox1.ImageIndexes [ i ] := i;
            finally
              Screen.Cursor := crDefault;
            end;

          end;

        end;

      finally
        FormResizeResample.Free;
      end;

    end;

  end;

end;

procedure TFormMain.Resize1Click ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      with ImageENView do
      begin

        FormResizeResample := TFormResizeResample.Create ( self );
        try

          FormResizeResample.Resample := False;
          FormResizeResample.Resize := True;
          FormResizeResample.Caption := 'Resize';
          FormResizeResample.ImageEnView1.Assign ( ImageENView );
          FormResizeResample.ImageEnView1.Update;
          FormResizeResample.ImageEnView1.IO.Params.Assign ( IO.Params );
          FormResizeResample.Width1.Text := IntToStr ( IEBitmap.Width );
          FormResizeResample.Height1.Text := IntToStr ( IEBitmap.Height );
          if FormResizeResample.ShowModal = mrOk then
          begin

            Screen.Cursor := crHourglass;
            try

              ImageENView.Proc.SaveUndoCaptioned ( 'Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
              Undo1.Hint := 'Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
              Proc.ImageResize ( StrToInt ( FormResizeResample.Width1.Text ), StrToInt ( FormResizeResample.Height1.Text ) );
              IO.Params.Width := StrToInt ( FormResizeResample.Width1.Text );
              IO.Params.Height := StrToInt ( FormResizeResample.Height1.Text );

              iBitmap := TBitmap.Create;
              try
                iBitmap.Assign ( ImageENView.Bitmap );
                // Create a thumbnail for dxImageListBox1
                CreateThumbnail ( iBitmap );
              finally
                iBitmap.Free;
              end;

              // Delete the selected thumbnail
              cxImageList2.Delete ( dxImageListBox1.ItemIndex );
              // Add thumbnail to dxImageListBox1
              i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
              if dxImageListBox1.CanFocus = True then
                dxImageListBox1.SetFocus;
              dxImageListBox1.Hint := FFilePathName;
              dxImageListBox1.ItemIndex := i;
              dxImageListBox1.ImageIndexes [ i ] := i;

            finally
              Screen.Cursor := crDefault;
            end;

          end;

        finally
          FormResizeResample.Free;
        end;

      end;

    end;

  end;

end;

procedure TFormMain.Rotate1Click ( Sender: TObject );
var
  iBitDepth: integer;
  iBackgroundColor: TColor;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FormRotate := TFormRotate.Create ( self );
      try

        FormRotate.ImageEnView1.Assign ( ImageENView );
        FormRotate.ImageEnView1.Update;
        iBitDepth := ImageENView.IO.Params.BitsPerSample * ImageENView.IO.Params.SamplesPerPixel;
        if iBitDepth = 32 then
        begin

          iBackgroundColor := TRGB2TColor ( ImageENView.IO.IEBitmap.Pixels [ 0, ImageENView.IO.IEBitmap.Height - 1 ] );
          FormRotate.ImageEnView1.IO.Params.BMP_HandleTransparency := True;
          FormRotate.cxColorComboBox1.EditValue := iBackgroundColor;

        end;

        if FormRotate.ShowModal = mrOk then
        begin

          Screen.Cursor := crHourglass;
          try

            ImageENView.Proc.SaveUndo ( ieuImage );
            Undo1.Hint := 'Rotate ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
            ImageENView.Proc.Rotate ( FormRotate.Angle, FormRotate.Antialias, FormRotate.AntialiasMode,
              FormRotate.BackGroundColor );
            ImageENView.Update;
            ImageENView.Fit;
            ImageENView.Bitmap.Modified := True;
            Undo1.Enabled := ImageENView.Proc.UndoCount > 0;
            Redo1.Enabled := ImageENView.Proc.RedoCount > 0;
            UpdateStatusbar;

          finally
            Screen.Cursor := crDefault;
          end;

        end;

      finally
        FormRotate.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ImageMouseDown ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer );
begin
  //
end;

procedure TFormMain.ImageMouseMove ( Sender: TObject; Shift: TShiftState; X, Y: integer );
var
  AX: Integer;
  AY: Integer;
  SLeft: Integer;
  STop: Integer;
  SRight: Integer;
  SBottom: Integer;
  SHeight: Integer;
  SWidth: Integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageENView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    AX := ImageENView.XScr2Bmp ( X );
    AY := ImageENView.YScr2Bmp ( Y );

    if ( AX >= 0 ) and ( AX <= ImageENView.IEBitmap.Width - 1 ) and ( AY >= 0 ) and ( AY <= ImageENView.IEBitmap.Height - 1 )
      then
    begin

      // Show mouse move coords
      dxStatusBar1.Panels [ 4 ].Text := ' Horz: ' + IntToStr ( AX + 1 ) + ' pixels ';
      dxStatusBar1.Panels [ 5 ].Text := ' Vert: ' + IntToStr ( AY + 1 ) + ' pixels ';

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
        dxStatusBar1.Panels [ 4 ].Text := 'Selected: ' + IntegerToString ( SHeight ) + ' x ' + IntegerToString ( SWidth ) +
          ' pixels ';
        dxStatusBar1.Panels [ 5 ].Text := '';

      end;

    end;

    UpdateStatusbar;

  end;

end;

procedure TFormMain.ImageMouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer );
begin
  //
end;

procedure TFormMain.ImageEnViewSelectionChange ( Sender: TObject );
var
  iX: integer;
  iY: integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iX := ImageENView.ClientToScreen ( Mouse.CursorPos ).X;
      iY := ImageENView.ClientToScreen ( Mouse.CursorPos ).Y;

      if ( ImageENView.Selected ) and ( ImageENView.IsPointInsideSelection ( iX - 1, iY - 1 ) ) then
      begin

        // Selected area
        dxStatusBar1.Panels [ 3 ].Text := 'Selected: ' + IntegerToString ( ImageEnView.SelX2 - ImageEnView.SelX1 ) + ' x ' +
          IntegerToString ( ImageEnView.SelY2 - ImageEnView.SelY1 ) + ' pixels ';
        dxStatusBar1.Panels [ 4 ].Text := '';

      end
      else
        dxStatusBar1.Panels [ 3 ].Text := '';

      Cut1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Copy1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Paste1.Enabled := ImageENView.Proc.IsClipboardAvailable;
      PasteToSelection1.Enabled := ImageENView.Proc.IsClipboardAvailable;
      Crop1.Enabled := ( cxPageControl1.PageCount <> 0 ) and ImageENView.Selected;
      Undo1.Enabled := ImageENView.Proc.UndoCount > 0;
      Redo1.Enabled := ImageENView.Proc.RedoCount > 0;

    end;

  end;

end;

procedure TFormMain.IncludeCursor1Change ( Sender: TObject );
begin
  ASGScreenCapture1.ShowCursor := IncludeCursor1.EditValue;
end;

procedure TFormMain.Paste1Click ( Sender: TObject );
// Paste the clipboard contents
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if Clipboard.HasFormat ( CF_BITMAP ) then
        { is there a bitmap on the Clipboard? }
      begin
        Screen.Cursor := crHourglass;
        try
          MessageBeep ( MB_ICONWARNING );
          TaskDialog1.Title := 'Warning';
          TaskDialog1.MainIcon := tdiWarning;
          TaskDialog1.Flags := [ tfAllowDialogCancellation ];
          TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
          TaskDialog1.Caption := 'The ImageENView Will Be Replaced By The Contents Of The Clipboard';
          TaskDialog1.Text :=
            'The ImageENView will be replaced by the ImageENView in the clipboard.  Replace the ImageENView?';
          TaskDialog1.FooterText := '';

          if TaskDialog1.Execute then
            if TaskDialog1.ModalResult = mrYes then
            begin

              ImageENView.Proc.SaveUndo;
              ImageENView.Proc.ClearAllRedo;
              ImageENView.Proc.PasteFromClipboard;
              UpdateControls;
              UpdateStatusbar;
              Image1.Update;

            end;

        finally
          Screen.Cursor := crDefault;
        end;

      end;

      iBitmap := TBitmap.Create;
      try
        iBitmap.Assign ( ImageENView.Bitmap );
        // Create a thumbnail for dxImageListBox1
        CreateThumbnail ( iBitmap );
      finally
        iBitmap.Free;
      end;

      // Delete the selected thumbnail
      cxImageList2.Delete ( dxImageListBox1.ItemIndex );
      // Add thumbnail to dxImageListBox1
      i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
      if dxImageListBox1.CanFocus = True then
        dxImageListBox1.SetFocus;
      dxImageListBox1.Hint := FFilePathName;
      dxImageListBox1.ItemIndex := i;
      dxImageListBox1.ImageIndexes [ i ] := i;

    end;

  end;

end;

procedure TFormMain.PasteIntoNew1Click ( Sender: TObject );
// Paste the clipboard contents into new page
var
  i: integer;
  iBitmap: TBitmap;
begin

  Screen.Cursor := crHourglass;
  try

    AddTabsheet;

    if Assigned ( cxPageControl1.ActivePage ) then
    begin

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        if Clipboard.HasFormat ( CF_BITMAP ) then
          { is there a bitmap on the Clipboard? }
        begin

          FFileName := FDefaultDirectory + '\Untitled ' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := ExtractFileExt ( '.bmp' );
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Untitled ' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          { Get the clipboard ImageENView }
          ImageENView.Proc.PasteFromClipboard;

          iBitmap := TBitmap.Create;
          try
            iBitmap.Assign ( ImageENView.Bitmap );
            // Create a thumbnail for dxImageListBox1
            CreateThumbnail ( iBitmap );
          finally
            iBitmap.Free;
          end;

          // Delete the selected thumbnail
          cxImageList2.Delete ( dxImageListBox1.ItemIndex );
          // Add thumbnail to dxImageListBox1
          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          UpdateControls;
          UpdateStatusbar;

        end;

      end;

    end;

  finally
    Screen.Cursor := crDefault;
  end;

end;

procedure TFormMain.PasteToSelection1Click ( Sender: TObject );
// Paste the clipboard contents into selection
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        if ImageENView.Selected then
        begin

          if CanPasteFromClipboard then
          begin

            ImageENView.Proc.SaveUndo ( );
            begin

              ImageENView.Proc.SaveUndo ( );
              ImageENView.Proc.ClearAllRedo;
              ImageENView.Proc.SelPasteFromClipStretch ( );

              { Create bitmap to hold the contents on the Clipboard }

            end;

          end
          else
          begin

            MessageBeep ( MB_ICONWARNING );
            TaskDialog1.Title := 'Warning';
            TaskDialog1.MainIcon := tdiWarning;
            TaskDialog1.Flags := [ tfAllowDialogCancellation ];
            TaskDialog1.CommonButtons := [ tcbOK ];
            TaskDialog1.Caption := 'Can Not Paste';
            TaskDialog1.Text := 'Can not paste from the clipboard.';
            TaskDialog1.FooterText := '';
            TaskDialog1.Execute;

          end;

        end;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );
        finally
          iBitmap.Free;
        end;

        // Delete the selected thumbnail
        cxImageList2.Delete ( dxImageListBox1.ItemIndex );
        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

  UpdateControls;
  UpdateStatusbar;

end;

procedure TFormMain.NewItemClick ( Sender: TObject );
// create a new page
var
  iBitmap: TBitmap;
  iImageRect: TRect;
begin

  FormNewImage := TFormNewImage.Create ( self );
  try

    if FormNewImage.ShowModal = mrOk then
    begin

      AddTabsheet;

      if Assigned ( cxPageControl1.ActivePage ) then
      begin

        ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

        if Assigned ( ImageENView ) then
        begin

          FFileName := FDefaultDirectory + '\Untitled ' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := FormNewImage.fFileType;
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Untitled ' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;

          iBitmap := TBitmap.Create;
          try

            iBitmap.Width := FormNewImage.fImageWidth;
            iBitmap.Height := FormNewImage.fImageHeight;
            iBitmap.PixelFormat := FormNewImage.fPixelFormat;
            iBitmap.Canvas.Brush.Color := FormNewImage.fColor;
            iBitmap.Canvas.Brush.Style := bsSolid;
            iImageRect.Left := 0;
            iImageRect.Top := 0;
            iImageRect.Right := iBitmap.Width;
            iImageRect.Bottom := iBitmap.Height;
            iBitmap.Canvas.FillRect ( iImageRect );
            ImageENView.Bitmap.Assign ( iBitmap );
            ImageENView.Update;

          finally
            iBitmap.Free;
          end;

          UpdateControls;
          UpdateStatusbar;
          ImageENView.Update;

        end;

      end;

    end;

  finally
    FormNewImage.Free;
  end;

end;

procedure TFormMain.None1Click ( Sender: TObject );
// Select none
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.MouseInteract := [ ];
      ImageENView.DeSelect;

    end;

  end;

end;

procedure TFormMain.OpenItemClick ( Sender: TObject );
// Open an image
var
  i: integer;
  iBitmap: TBitmap;
begin

  if OpenPictureDialog1.Execute then
  begin

    if FileExists ( OpenPictureDialog1.FileName ) then
    begin

      Screen.Cursor := crHourglass;
      try

        FFilePathName := OpenPictureDialog1.FileName;
        FFileName := ExtractFilename ( OpenPictureDialog1.FileName );
        FFileExtension := ExtractFileExt ( OpenPictureDialog1.FileName );

        // Add cxImage Control to a new tabsheet
        AddTabsheet;

        if Assigned ( cxPageControl1.ActivePage ) then
        begin

          ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

          if Assigned ( ImageENView ) then
          begin

            try

              ImageENView.IO.LoadFromFile ( FFilePathName );
              ImageENView.Update;
              dxBarMRUListItem1.AddItem ( FFilePathName, nil );

            except
              ShowMessage ( 'Error loading ImageENView' );
            end;

          end;

        end;

        cxPageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );
        cxPageControl1.ActivePage.Hint := FFilePathName;

        iBitmap := TBitmap.Create;
        try

          iBitmap.Assign ( ImageENView.Bitmap );
          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

        finally
          iBitmap.Free;
        end;

        // Add thumbnail to dxImageListBox1
        i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
        if dxImageListBox1.CanFocus = True then
          dxImageListBox1.SetFocus;
        dxImageListBox1.Hint := FFilePathName;
        dxImageListBox1.ItemIndex := i;
        dxImageListBox1.ImageIndexes [ i ] := i;

        if Fit1.EditValue then
          ImageENView.Fit;
        ImageENView.Update;
        UpdateControls;
        UpdateStatusbar;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.CloseItemClick ( Sender: TObject );
// close the selected item
var
  i: integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      i := cxPageControl1.ActivePageIndex;
      cxImageList2.Delete ( i );
      dxImageListBox1.Items.Delete ( i );
      cxPageControl1.ActivePage.Free;

      if cxPageControl1.PageCount = 0 then
      begin
        Caption := 'Apprehend 6.0';
        ClearStatusbar;
      end
      else
      begin
        Caption := 'Apprehend 6.0 - ';
        UpdateStatusbar;
      end;

    end;

  end;

  UpdateControls;

end;

procedure TFormMain.Copy1Click ( Sender: TObject );
// Copy to clipboard
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.Selected then
      begin

        Screen.Cursor := crHourglass;
        try

          ImageENView.Proc.SaveUndo ( );
          begin

            // If ImageENView present...
            if Assigned ( ImageENView.Bitmap ) then
            begin

              ImageENView.Proc.SelCopyToClip ( );
              UpdateControls;
              UpdateStatusbar;

            end;

          end;

        finally
          Screen.Cursor := crDefault;
        end;

      end;
    end
    else
    begin

      MessageBeep ( MB_ICONQUESTION );
      TaskDialog1.Title := 'Warning';
      TaskDialog1.MainIcon := tdiWarning;
      TaskDialog1.Flags := [ tfAllowDialogCancellation ];
      TaskDialog1.CommonButtons := [ tcbOK ];
      TaskDialog1.Caption := 'No Selection';
      TaskDialog1.Text := 'There is no selection. Please make a selection and try again.';
      TaskDialog1.FooterText := '';
      TaskDialog1.Execute;

    end;

  end;

end;

procedure TFormMain.Crop1Click ( Sender: TObject );
// Crop to the selection
var
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.Selected then
      begin

        // If ImageENView present...
        if Assigned ( ImageENView.Bitmap ) then
        begin

          Screen.Cursor := crHourglass;
          try
            ImageENView.Proc.SaveUndo ( );
            ImageENView.Proc.ClearAllRedo;
            ImageENView.Proc.CropSel ( );
            ImageENView.DeSelect;

            iBitmap := TBitmap.Create;
            try

              iBitmap.Assign ( ImageENView.Bitmap );
              // Delete the selected thumbnail from the dxImageList
              cxImageList2.Delete ( dxImageListBox1.ItemIndex );
              // Add a thumbnail of cropped image to the dxImageList
              CreateThumbnail ( iBitmap );

            finally
              iBitmap.Free;
            end;

            dxImageListBox1.Hint := FFilePathName;

            UpdateControls;
            UpdateStatusbar;

          finally
            Screen.Cursor := crDefault;
          end;

        end;
      end
      else
      begin

        MessageBeep ( MB_ICONQUESTION );
        TaskDialog1.Title := 'Warning';
        TaskDialog1.MainIcon := tdiWarning;
        TaskDialog1.Flags := [ tfAllowDialogCancellation ];
        TaskDialog1.CommonButtons := [ tcbOK ];
        TaskDialog1.Caption := 'No Selection';
        TaskDialog1.Text := 'There is no selection. Please make a selection and try again.';
        TaskDialog1.FooterText := '';
        TaskDialog1.Execute;

      end;

    end;

  end;

end;

procedure TFormMain.Cut1Click ( Sender: TObject );
// Cut the selection
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.Selected then
      begin

        Screen.Cursor := crHourglass;
        try

          ImageENView.Proc.SaveUndo;
          ImageENView.Proc.ClearAllRedo;
          if Assigned ( ImageENView.Bitmap ) then
          begin

            ImageENView.Proc.SelCutToClip ( );
            ImageENView.Update;
            UpdateControls;
            UpdateStatusbar;

            iBitmap := TBitmap.Create;
            try
              iBitmap.Assign ( ImageENView.Bitmap );
              // Create a thumbnail for dxImageListBox1
              CreateThumbnail ( iBitmap );
            finally
              iBitmap.Free;
            end;

            // Delete the selected thumbnail
            cxImageList2.Delete ( dxImageListBox1.ItemIndex );
            // Add thumbnail to dxImageListBox1
            i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
            if dxImageListBox1.CanFocus = True then
              dxImageListBox1.SetFocus;
            dxImageListBox1.Hint := FFilePathName;
            dxImageListBox1.ItemIndex := i;
            dxImageListBox1.ImageIndexes [ i ] := i;

          end;

        finally
          Screen.Cursor := crDefault;
        end;

      end
      else
      begin

        MessageBeep ( MB_ICONQUESTION );
        TaskDialog1.Title := 'Warning';
        TaskDialog1.MainIcon := tdiWarning;
        TaskDialog1.Flags := [ tfAllowDialogCancellation ];
        TaskDialog1.CommonButtons := [ tcbOK ];
        TaskDialog1.Caption := 'No Selection';
        TaskDialog1.Text := 'There is no selection. Please make a selection and try again.';
        TaskDialog1.FooterText := '';
        TaskDialog1.Execute;

      end;

    end;

  end;

end;

procedure TFormMain.cxBarEditItem1PropertiesChange ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.Zoom := cxZoomTrackbar1.EditValue;
      ImageENView.Update;
      dxBarStatic1.Caption := IntToStr ( cxZoomTrackbar1.EditValue ) + '%';

    end;

  end;

end;

procedure TFormMain.cxPageControl1CanCloseEx(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean);
var
  i: integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      i := cxPageControl1.ActivePageIndex;
      cxImageList2.Delete ( i );
      dxImageListBox1.Items.Delete ( i );

    end;

  end;

  UpdateControls;

  ACanClose := True;

end;

procedure TFormMain.cxPageControl1Change(Sender: TObject);
begin

  if cxPageControl1.PageCount = 0 then
  begin

    Caption := 'Apprehend 6.0';
    ClearStatusbar;

  end
  else
  begin

    if FileExists ( cxPageControl1.ActivePage.Hint ) then
    begin

      FFilePathName := cxPageControl1.ActivePage.Hint;
      Caption := 'Apprehend 6.0 - ' + FFilePathName;
      dxStatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FFilePathName );
      dxStatusBar1.Panels [ 1 ].Text := ExtractFilename ( FFilePathName );
      dxImageListBox1.ItemIndex := cxPageControl1.ActivePageIndex;

    end;

  end;
end;

procedure TFormMain.cxPageControl1Click(Sender: TObject);
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FFilePathName := cxPageControl1.ActivePage.Hint;
      FFileName := ExtractFilename ( cxPageControl1.ActivePage.Hint );
      FFileExtension := ExtractFileExt ( cxPageControl1.ActivePage.Hint );
      if dxImageListBox1.Items.Count = cxPageControl1.PageCount then
        dxImageListBox1.ItemIndex := cxPageControl1.ActivePageIndex;
      UpdateStatusbar;

    end;

  end;
end;

procedure TFormMain.cxPageControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 dxImageListBox1.ItemIndex := cxPageControl1.ActivePageIndex;
end;

procedure TFormMain.cxZoomTrackbar1CurChange ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.Zoom := cxZoomTrackbar1.CurEditValue;
      ImageENView.Update;
      dxBarStatic1.Caption := IntToStr ( cxZoomTrackbar1.CurEditValue ) + '%';

    end;

  end;

end;

procedure TFormMain.CloseAllItemClick ( Sender: TObject );
// Close all items
var
  i: integer;
  iPageCount: integer;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      // Close All pages
      if cxPageControl1.PageCount - 1 > 0 then
      begin

        MessageBeep ( MB_ICONQUESTION );
        TaskDialog1.Title := 'Close All';
        TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
        TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
        TaskDialog1.Caption := 'Close All Images';
        iPageCount := cxPageControl1.PageCount;
        TaskDialog1.Text := 'Close all - ' + IntToNumberText ( iPageCount ) + ' (' + IntToStr ( iPageCount ) + ') images?';
        TaskDialog1.FooterText := '';

        if TaskDialog1.Execute then
          if TaskDialog1.ModalResult = mrYes then
          begin

            for i := cxPageControl1.PageCount - 1 downto 0 do
            begin

              dxImageListBox1.Items.Delete ( i );
              if dxImageListBox1.Items.Count > 0 then
                dxImageListBox1.ItemIndex := dxImageListBox1.Items.Count - 1;
              cxImageList2.Delete ( i );

              // Close the active page
              cxPageControl1.ActivePage := cxPageControl1.Pages [ i ];
              cxPageControl1.ActivePage.Free; // Closes and Frees the ActivePage
              cxPageControl1.SelectNextPage ( False );

            end;

            UpdateControls;

            if cxPageControl1.PageCount = 0 then
            begin
              Caption := 'Apprehend 6.0';
              ClearStatusbar;
            end;

          end;

      end;

    end;

  end;

end;

procedure TFormMain.SaveItemClick ( Sender: TObject );
// save to a file
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      // Save current file
      if cxPageControl1.PageCount <> 0 then
      begin

        Screen.Cursor := crHourglass;
        try

          if Length ( FFileExtension ) = 0 then
            FFileExtension := '.bmp';
          if Length ( FFileName ) = 0 then
            FFileName := 'Untitled' + FCaptureStr;
          if Length ( FFolder ) = 0 then
            FFolder := FDefaultDirectory + '\';
          FFNE := FFileName + FFileExtension;
          FFilePathName := FFolder + FFileName + FFileExtension;
          // If file exists then delete it
          if FileExists ( FFilePathName ) then
          begin

            // Prompt user to delete file
            MessageBeep ( MB_ICONQUESTION );
            TaskDialog1.Title := 'Delete';
            TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
            TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
            TaskDialog1.Caption := 'Close All Images';
            TaskDialog1.Text := FFilePathName + ' exists, Delete?';
            TaskDialog1.FooterText := '';

            if TaskDialog1.Execute then
              if TaskDialog1.ModalResult = mrYes then
                DeleteFile ( FFilePathName );

          end;

          try
            ImageENView.IO.SaveToFile ( FFilePathName );
          except
            on EInvalidGraphic do
            begin

              MessageBeep ( MB_ICONWARNING );
              TaskDialog1.Title := 'Error';
              TaskDialog1.MainIcon := tdiWarning;
              TaskDialog1.Flags := [ tfAllowDialogCancellation ];
              TaskDialog1.CommonButtons := [ tcbOK ];
              TaskDialog1.Caption := 'Error Saving File';
              TaskDialog1.Text := 'Error saving file,' + FFilePathName + '!';
              TaskDialog1.FooterText := '';
              TaskDialog1.Execute;

            end;

          end;
          // Reload the file to show compression
          ImageENView.IO.LoadFromFile ( FFilePathName );
          cxPageControl1.ActivePage.Caption := ExtractFilename ( FFilePathName );

        finally
          Screen.Cursor := crDefault;
        end;

      end;

    end;

  end;

end;

procedure TFormMain.SavePictureDialog1TypeChange ( Sender: TObject );
// change filename extension to match filter
var
  FilePath: string;
  FileName: string;
  FileExt: string;
begin

  FilePath := SavePictureDialog1.FileName;
  FileName := ExtractFilename ( FilePath );
  FileExt := ExtractFileExt ( FilePath );

end;

procedure TFormMain.SaveToDesktop1Click ( Sender: TObject );
// save image to the deskop
var
  i: integer;
  icounter: integer;
  iFrames: array of TObject;
  ipb: integer;
  iFilename: string;
  iExtension: string;
  iFileType: TIOFileType;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    if Assigned ( ImageENView ) then
    begin

      Screen.Cursor := crHourGlass;
      try

        ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );
        iFileType := ImageENView.IO.Params.FileType;
        iFilename :=  JustName( ImageENView.IO.Params.FileName );
        // Remove any number from end of the string
        iFilename := StripNumeric( iFilename );
        iExtension := IEFileTypeToExtension ( iFileType );
        FFilePathName := DesktopFolder + iFilename + IntToStr ( cxPageControl1.PageCount ) + iExtension;

        icounter := 0;
        // Increment the capture number if the file exists
        while FileExists ( FFilePathName ) do
        begin
          Inc( icounter );
          // Create new filename
          FFilePathName := DesktopFolder + iFilename + IntToStr ( icounter ) + iExtension;
        end;

        dxBarMRUListItem1.AddItem ( FFilePathName, nil );

        if ImageENView.IO.Params.FileType = ioICO then
        begin

          TcxProgressBarProperties ( cxProgressbar1.Properties ).Max := cxPageControl1.PageCount - 1;

          SetLength ( iFrames, cxPageControl1.PageCount );
          for i := 0 to cxPageControl1.PageCount - 1 do
          begin

            ImageENView := TImageEnView ( cxPageControl1.Pages [ i ].Controls [ 0 ] );
            iFrames [ i ] := ImageENView;
            ImageENView.IO.Params.ICO_ImageIndex := i;
            ImageENView.IO.Params.ICO_Sizes [ i ].cx := ImageENView.Bitmap.Width;
            ImageENView.IO.Params.ICO_Sizes [ i ].cY := ImageENView.Bitmap.Height;
            ImageENView.IO.Params.ICO_BitCount [ i ] := ImageENView.IO.Params.BitsPerSample *
              ImageENView.IO.Params.SamplesPerPixel;
            ImageENView.Bitmap.Modified := False;
            cxProgressbar1.EditValue := i;
            for ipb := 0 to cxProgressbar1.LinkCount - 1 do
              cxProgressbar1.Links [ ipb ].BarControl.Update;
            Sleep ( 100 );

          end;

          IEWriteICOImages ( FFilePathName, iFrames );
          ImageENView.ImageChange;
          UpdateControls;

        end
        else
        begin

          ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

          if Assigned ( ImageENView ) then
          begin

            TcxProgressBarProperties ( cxProgressbar1.Properties ).Max := 100;

            // Save to Desktop
            FFilePathName := DesktopFolder + ExtractFileName ( FFilePathName );
            ImageENView.IO.PreviewsParams := [ ioppDefaultLockPreview ];

            ImageENView.IO.SimplifiedParamsDialogs := False;
            if ImageENView.IO.DoPreviews ( [ ppAUTO ] ) then
            begin

              ImageENView.IO.SaveToFile ( FFilePathName );
              dxBarMRUListItem1.AddItem ( FFilePathName, nil );
              ImageENView.Bitmap.Modified := False;
              ImageENView.ImageChange;
              UpdateControls;

            end;

          end;

        end;

        cxProgressbar1.EditValue := 0;
        for ipb := 0 to cxProgressbar1.LinkCount - 1 do
          cxProgressbar1.Links [ ipb ].BarControl.Update;

        Caption := 'Apprehend 6.0- ' + FFilePathName;
        dxStatusBar1.Panels [ 0 ].Text := ExtractFilePath ( ImageENView.IO.Params.FileName );
        dxStatusBar1.Panels [ 1 ].Text := ExtractFileName ( ImageENView.IO.Params.FileName );

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.SaveAsItemClick ( Sender: TObject );
// Save the image with a new filename
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourglass;
    try

      ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

      if Assigned ( ImageENView ) then
      begin

        if Length ( FTmpFolder ) = 0 then
          SavePictureDialog1.InitialDir := FDefaultDirectory
        else
          SavePictureDialog1.InitialDir := FTmpFolder;
        if Length ( FFolder ) = 0 then
          FFolder := ExtractFilePath ( FFilePathName );
        if Length ( FFolder ) = 0 then
          FFolder := FDefaultDirectory;
        if SavePictureDialog1.FileName = '' then
          SavePictureDialog1.FileName := ExtractFilename ( IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Untitled' );
        if Length ( SavePictureDialog1.FileName ) = 0 then
          SavePictureDialog1.FileName := '*';
        SavePictureDialog1.DefaultExt := GraphicExtension ( TPNGImage );
        SavePictureDialog1.FileName := SavePictureDialog1.FileName + '.' + SavePictureDialog1.DefaultExt;
        FFNE := FFileName + FFileExtension;
        FFilePathName := FFolder + FFileName + FFileExtension;
        SavePictureDialog1.Filter := Graphics.GraphicFilter ( TGraphic );
        SavePictureDialog1.FilterIndex := 2;
        if SavePictureDialog1.Execute then
        begin

          FFileExtension := ExtractFileExt ( SavePictureDialog1.FileName );
          ImageENView.IO.SaveToFile ( SavePictureDialog1.FileName );
          FFilePathName := SavePictureDialog1.FileName;
          FFolder := ExtractFilePath ( SavePictureDialog1.FileName );
          FFileName := ExtractFilename ( FFilePathName );
          FFileExtension := ExtractFileExt ( SavePictureDialog1.FileName );
          FFNE := FFileName + FFileExtension;
          dxBarMRUListItem1.AddItem ( FFilePathName, nil );
          cxPageControl1.ActivePage.Caption := ExtractFilename ( SavePictureDialog1.FileName );

        end;

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TFormMain.PrintItemClick ( Sender: TObject );
// print the item
var
  iAspectRatio: single;
  iOutputWidth: single;
  iOutputHeight: single;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if not PrintDialog1.Execute then
        Exit;

      Screen.Cursor := crHourglass;
      try

        Printer.BeginDoc;
        try

          iOutputWidth := ImageENView.Bitmap.Width;
          iOutputHeight := ImageENView.Bitmap.Height;
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
          Printer.Canvas.StretchDraw ( Rect ( 0, 0, Trunc ( iOutputWidth ), Trunc ( iOutputHeight ) ), ImageENView.Bitmap );

        finally
          Printer.EndDoc;
        end;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TFormMain.CreateThumbnail ( ABitmap: TBitmap );
var
  iImageENProc: TImageENProc;
begin

  // Create thumbnail
  iImageENProc := TImageENProc.Create ( nil );
  iImageENProc.AttachedBitmap := ABitmap;
  iImageENProc.Resample ( 64, 48 );
  iImageENProc.AttachedBitmap.PixelFormat := pf24bit;
  cxImageList2.Add ( iImageENProc.AttachedBitmap, nil );
  iImageENProc.Free;

end;

procedure TFormMain.CaptureDesktopItemClick ( Sender: TObject );
// Capture the desktop
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.png';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 0;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1
          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;

        end
        else
        begin

          cxPageControl1.ActivePage.Free;

        end;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureAreaItemClick ( Sender: TObject );
// Capture selected aea
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      // Capture area of screen
      iBitmap := ASGScreenCapture1.CaptureSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 1;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;

          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureActiveItemClick ( Sender: TObject );
// Capture active window
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureActiveWindow;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 2;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureObjectItemClick ( Sender: TObject );
// Capture selected object
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureObject;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 3;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;

        end

        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.ExitItem1Click ( Sender: TObject );
// Exit the app
begin
  Close;
end;

procedure TFormMain.BlackandWhite1Click ( Sender: TObject );
// Convert to B&w
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.HasAlphaChannel = True then
        ImageENView.RemoveAlphaChannel;
      ImageENView.IO.Params.BitsPerSample := 1;
      ImageENView.IO.Params.SamplesPerPixel := 1;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB 2-bit';
      BlackandWhite1.Down := True;

    end;

  end;

end;

procedure TFormMain.Blog1Click ( Sender: TObject );
// Visit blog
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://williamwmiller.wordpress.com/' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.N16Color1Click ( Sender: TObject );
// Convert to 16 color
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.HasAlphaChannel = True then
        ImageENView.RemoveAlphaChannel;
      ImageENView.Bitmap.PixelFormat := pf4bit;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB4-bit';
      N16Color1.Down := True;

    end;

  end;

end;

procedure TFormMain.N256Color1Click ( Sender: TObject );
// Convert to 256 color
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.HasAlphaChannel = True then
        ImageENView.RemoveAlphaChannel;
      ImageENView.IO.Params.BitsPerSample := 4;
      ImageENView.IO.Params.SamplesPerPixel := 2;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB8-bit ';
      N256Color1.Down := True;

    end;

  end;

end;

procedure TFormMain.N16Bit1Click ( Sender: TObject );
// Convert to 16-bit
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.HasAlphaChannel = True then
        ImageENView.RemoveAlphaChannel;
      ImageENView.IO.Params.BitsPerSample := 8;
      ImageENView.IO.Params.SamplesPerPixel := 2;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB16-bit';
      N16Bit1.Down := True;

    end;

  end;

end;

procedure TFormMain.N24Bit1Click ( Sender: TObject );
// Convert to 24-bit
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if ImageENView.HasAlphaChannel = True then
        ImageENView.RemoveAlphaChannel;
      ImageENView.IO.Params.BitsPerSample := 8;
      ImageENView.IO.Params.SamplesPerPixel := 3;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-bit';
      N24Bit1.Down := True;

    end;

  end;

end;

procedure TFormMain.N32Bit1Click ( Sender: TObject );
// Convert to 32-bit
var
  ARGB: TRGB;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.IO.Params.BitsPerSample := 8;
      ImageENView.IO.Params.SamplesPerPixel := 4;
      with ImageENView.Proc do
      begin

        ARGB := ImageENView.IEBitmap.Pixels [ 0, ImageENView.IEBitmap.Height - 1 ];
        SetTransparentColors ( ARGB, ARGB, 0 );

      end;
      dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: ARGB32-bit';
      N32Bit1.Down := True;
    end;

  end;

end;

procedure TFormMain.Delay1Change ( Sender: TObject );
// Set the delay
begin

  ASGScreenCapture1.Delay := Delay1.EditValue;

end;

procedure TFormMain.AboutItem1Click ( Sender: TObject );
// Show about
begin

  frmAbout := TfrmAbout.Create ( self );
  try

    frmAbout.ShowModal;

  finally
    frmAbout.Free;
  end;

end;

procedure TFormMain.HelpContentsItem1Click ( Sender: TObject );
// Show help
begin

  Application.HelpCommand ( HELP_FINDER, 0 );

end;

procedure TFormMain.PrintSetupItem1Click ( Sender: TObject );
// Setup the printer
begin

  PrinterSetupDialog1.Execute;

end;

procedure TFormMain.FreeFormCaptureClick ( Sender: TObject );
// Capture FreeForm
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 4;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;
          ImageENView.Bitmap.PixelFormat := pfDevice;
          dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-bit';

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.FullScreen1Click ( Sender: TObject );
// Zoom window to full screen
{$J+} // writeable constants on
const
  Rect: TRect = ( Left: 0; Top: 0; Right: 0; Bottom: 0 );
  Ws: TWindowState = wsNormal;
{$J-} // writeable constants off
var
  r: TRect;
begin

  if BorderStyle <> bsNone then
  begin

    Ws := WindowState;
    Rect := BoundsRect;
    BorderStyle := bsNone;
    r := Screen.MonitorFromWindow ( Handle ).BoundsRect;
    SetBounds ( r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top );

  end
  else
  begin

    BorderStyle := bsSizeable;
    if Ws = wsMaximized then
      WindowState := wsMaximized
    else
      SetBounds ( Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top );

  end;

end;

procedure TFormMain.Website1Click ( Sender: TObject );
// Visit the website
begin

  ShellExecute ( Handle, 'open', PChar ( 'http://frontiernet.net/~w2m/index.html' ), nil, nil, SW_SHOWNORMAL );

end;

procedure TFormMain.Zoom1Click ( Sender: TObject );
// Set to zoom interaction
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.MouseInteract := [ miZoom ];

    end;

  end;

end;

procedure TFormMain.CapturePolygonItemClick ( Sender: TObject );
// Capture polygon
var
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        ImageENView.Bitmap.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

    end;

    FFileName := FDefaultDirectory + '\ImageENView ' + IntToStr ( cxPageControl1.PageCount );
    FFileExtension := '.bmp';
    FFilePathName := FFileName + FFileExtension;
    cxPageControl1.ActivePage.Caption := 'ImageENView ' + IntToStr ( cxPageControl1.PageCount );
    cxPageControl1.ActivePage.Hint := FFilePathName;
    UpdateStatusbar;

  end;

end;

procedure TFormMain.Size1Click ( Sender: TObject );
// Show rect size in hint during selection captures
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try

        if Assigned ( iBitmap ) then
          ImageENView.Bitmap.Assign ( iBitmap )
        else
        begin

          cxPageControl1.ActivePage.Free;
          exit;

        end;

        ImageENView.Update;

        // Create a thumbnail for dxImageListBox1
        CreateThumbnail ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
      FFileExtension := '.bmp';
      FFilePathName := FFileName + FFileExtension;
      cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
      cxPageControl1.ActivePage.Hint := FFilePathName;
      cxPageControl1.ActivePage.ImageIndex := 7;
      ImageENView.IO.Params.FileName := FFilePathName;
      ImageENView.IO.Params.FileType := ioPNG;

      // Add thumbnail to dxImageListBox1
      i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
      if dxImageListBox1.CanFocus = True then
        dxImageListBox1.SetFocus;
      dxImageListBox1.Hint := FFilePathName;
      dxImageListBox1.ItemIndex := i;
      dxImageListBox1.ImageIndexes [ i ] := i;

      if Fit1.EditValue then
        ImageENView.Fit;
      ImageENView.Update;
      UpdateStatusbar;
      UpdateControls;

    end;

  end;

end;

procedure TFormMain.SmoothResize1Click ( Sender: TObject );
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FormResizeResample := TFormResizeResample.Create ( self );
      try

        with ImageENView do
        begin

          FormResizeResample.Resample := True;
          FormResizeResample.Resize := False;
          FormResizeResample.Caption := 'Resample';
          FormResizeResample.ImageEnView1.IEBitmap.Assign ( IEBitmap );
          FormResizeResample.ImageEnView1.IO.Params.Assign ( IO.Params );
          FormResizeResample.ImageEnView1.Update;
          FormResizeResample.Width1.Text := IntToStr ( IEBitmap.Width );
          FormResizeResample.Height1.Text := IntToStr ( IEBitmap.Height );

          if FormResizeResample.ShowModal = mrOk then
          begin

            Screen.Cursor := crHourglass;
            try

              ImageENView.Proc.SaveUndoCaptioned ( 'Smooth Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 ) );
              Undo1.Hint := 'Smooth Resize ' + IntToStr ( ImageENView.Proc.UndoCount + 1 );
              Proc.Resample ( StrToInt ( FormResizeResample.Width1.Text ), StrToInt ( FormResizeResample.Height1.Text ),
                TResampleFilter ( FormResizeResample.ComboBoxResampleFilter1.ItemIndex ) );
              IO.Params.Width := StrToInt ( FormResizeResample.Width1.Text );
              IO.Params.Height := StrToInt ( FormResizeResample.Height1.Text );

              // Delete the selected thumbnail
              cxImageList2.Delete ( dxImageListBox1.ItemIndex );

              iBitmap := TBitmap.Create;
              try
                iBitmap.Assign ( ImageENView.Bitmap );
                // Create a thumbnail for dxImageListBox1
                CreateThumbnail ( iBitmap );
              finally
                iBitmap.Free;
              end;

              // Add thumbnail to dxImageListBox1
              i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
              if dxImageListBox1.CanFocus = True then
                dxImageListBox1.SetFocus;
              dxImageListBox1.Hint := FFilePathName;
              dxImageListBox1.ItemIndex := i;
              dxImageListBox1.ImageIndexes [ i ] := i;
              i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
              if dxImageListBox1.CanFocus = True then
                dxImageListBox1.SetFocus;
              dxImageListBox1.Hint := FFilePathName;
              dxImageListBox1.ItemIndex := i;
              dxImageListBox1.ImageIndexes [ i ] := i;

            finally
              Screen.Cursor := crDefault;
            end;

          end;

        end;

      finally
        FormResizeResample.Free;
      end;

    end;

  end;

end;

procedure TFormMain.Fit1PropertiesChange ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if Fit1.EditValue then
        ImageENView.Fit
      else
        ImageENView.Zoom := 100;
      ImageENView.Update;
      cxZoomTrackbar1.EditValue := Round ( ImageENView.Zoom );
      dxBarStatic1.Caption := IntToStr ( cxZoomTrackbar1.EditValue ) + '%';
      Fit2.Down := Fit1.EditValue;

    end;

  end;

end;

procedure TFormMain.Fit2Click ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      if Fit2.Down then
        ImageENView.Fit
      else
        ImageENView.Zoom := 100;
      ImageENView.Update;
      cxZoomTrackbar1.EditValue := Round ( ImageENView.Zoom );
      dxBarStatic1.Caption := IntToStr ( cxZoomTrackbar1.EditValue ) + '%';
      Fit1.EditValue := Fit2.Down;

    end;

  end;

end;

procedure TFormMain.CaptureWholeDesktopItemClick ( Sender: TObject );
// dual monitor desktop capture
var
  iBitmap: TBitmap;
begin

  GetCaptureOptions;
  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureWholeDesktop;
      try

        ImageENView.Bitmap.Assign ( iBitmap );

      finally
        iBitmap.Free;
      end;

      FFileName := FDefaultDirectory + '\ImageENView ' + IntToStr ( cxPageControl1.PageCount );
      FFileExtension := '.bmp';
      FFilePathName := FFileName + FFileExtension;
      cxPageControl1.ActivePage.Caption := 'ImageENView ' + IntToStr ( cxPageControl1.PageCount );
      cxPageControl1.ActivePage.Hint := FFilePathName;
      UpdateStatusbar;

    end;

  end;

end;

procedure TFormMain.Center1Exit ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.Center := Center1.EditValue;
      ImageENView.Update;

    end;

  end;

end;

procedure TFormMain.Center1PropertiesChange ( Sender: TObject );
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      ImageENView.Center := Center1.EditValue;
      ImageENView.Update;

    end;

  end;

end;

procedure TFormMain.CaptureIconItemClick ( Sender: TObject );
// Capture icon
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      FFileName := 'Untitled';

      iBitmap := ASGScreenCapture1.CaptureIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 6;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;
          dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-bit';

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureLargeIcon1Click ( Sender: TObject );
// Capture large icon
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureLargeIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 6;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;
          dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-bit';

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TFormMain.CaptureSelectionItemClick ( Sender: TObject );
// Capture selection
var
  i: integer;
  iBitmap: TBitmap;
begin

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try
        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          // Add thumbnail to dxImageListBox1

          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          FFileName := FDefaultDirectory + '\Capture ' + IntToStr ( cxPageControl1.PageCount );
          FFilePathName := FFileName;
          Caption := 'Apprehend- ' + FFilePathName;
          cxPageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          UpdateStatusbar;

        end;

      finally
        iBitmap.Free;
      end;

    end;

    FFileName := FDefaultDirectory + '\ImageENView ' + IntToStr ( cxPageControl1.PageCount );
    FFileExtension := '.bmp';
    FFilePathName := FFileName + FFileExtension;
    cxPageControl1.ActivePage.Caption := 'ImageENView ' + IntToStr ( cxPageControl1.PageCount );
    cxPageControl1.ActivePage.Hint := FFilePathName;
    ImageENView.IO.Params.FileName := FFilePathName;

  end;

end;

procedure TFormMain.CaptureSmallIcon1Click ( Sender: TObject );
// Capture small icon
var
  i: integer;
  iBitmap: TBitmap;
begin

  GetCaptureOptions;

  AddTabsheet;

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      iBitmap := ASGScreenCapture1.CaptureSmallIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          ImageENView.Bitmap.Assign ( iBitmap );
          ImageENView.Update;

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          FFileName := FDefaultDirectory + '\Capture' + IntToStr ( cxPageControl1.PageCount );
          FFileExtension := '.bmp';
          FFilePathName := FFileName + FFileExtension;
          cxPageControl1.ActivePage.Caption := 'Capture' + IntToStr ( cxPageControl1.PageCount );
          cxPageControl1.ActivePage.Hint := FFilePathName;
          cxPageControl1.ActivePage.ImageIndex := 5;
          ImageENView.IO.Params.FileName := FFilePathName;
          ImageENView.IO.Params.FileType := ioPNG;

          // Add thumbnail to dxImageListBox1
          i := dxImageListBox1.Items.Add ( ExtractFilename ( FFilePathName ) );
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

          if Fit1.EditValue then
            ImageENView.Fit;
          ImageENView.Update;
          UpdateStatusbar;
          UpdateControls;
          dxStatusBar1.Panels [ 4 ].Text := ' Color Depth: RGB24-bit';

        end
        else
          cxPageControl1.ActivePage.Free;

      finally
        iBitmap.Free;
      end;

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
    FDefaultDirectory := iFolder;

end;

procedure TFormMain.Undo1Click ( Sender: TObject );
// Undo the last change
var
  i: integer;
  iBitmap: TBitmap;
begin

  if Assigned ( cxPageControl1.ActivePage ) then
  begin

    ImageENView := TImageEnView ( cxPageControl1.ActivePage.Controls [ 0 ] );

    if Assigned ( ImageENView ) then
    begin

      Screen.Cursor := crHourglass;
      try

        // Undo
        with ImageENView.Proc do
        begin

          SaveRedo; // saves in Redo list
          Undo;
          ClearUndo;

        end;

        iBitmap := TBitmap.Create;
        try
          iBitmap.Assign ( ImageENView.Bitmap );

          cxImageList2.Delete ( cxPageControl1.ActivePageIndex );

          // Create a thumbnail for dxImageListBox1
          CreateThumbnail ( iBitmap );

          // Add thumbnail to dxImageListBox1
          i := dxImageListBox1.ItemIndex;
          if dxImageListBox1.CanFocus = True then
            dxImageListBox1.SetFocus;
          dxImageListBox1.Hint := FFilePathName;
          dxImageListBox1.ItemIndex := i;
          dxImageListBox1.ImageIndexes [ i ] := i;

        finally
          iBitmap.Free;
        end;

      finally
        Screen.Cursor := crDefault;
      end;

    end;

    UpdateControls;
    UpdateStatusbar;

  end;

end;

end.

