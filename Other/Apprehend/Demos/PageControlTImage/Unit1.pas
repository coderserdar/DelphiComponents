// ------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Description             : Unit1
// Compiler                : Delphi 2010
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
// ------------------------------------------------------------------------------
unit Unit1;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ASGCapture, StdCtrls, ExtCtrls, ComCtrls, ImgList, Menus, ExtDlgs,
  Buttons, ToolWin, JPeg, PngImage, GifImg;

type
  TForm1 = class ( TForm )
    Panel1: TPanel;
    ASGScreenCapture1: TASGScreenCapture;
    Auto1: TCheckBox;
    Minimize1: TCheckBox;
    ShowCursor1: TCheckBox;
    ShowHint1: TCheckBox;
    ShowInfoDialog1: TCheckBox;
    CaptureDelay1: TEdit;
    Label1: TLabel;
    UpDown1: TUpDown;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    OpenItem: TToolButton;
    CloseItem: TToolButton;
    CloseAllItem: TToolButton;
    SaveAsItem: TToolButton;
    ToolButton2: TToolButton;
    ExitItem: TToolButton;
    ToolBar3: TToolBar;
    CaptureDesktopItem: TToolButton;
    CaptureSpeedItem1: TToolButton;
    CaptureWholeDesktopItem: TToolButton;
    CaptureActiveItem: TToolButton;
    CaptureAreaItem: TToolButton;
    CaptureObjectItem: TToolButton;
    CapturePolygonItem: TToolButton;
    CaptureSmallIconItem: TToolButton;
    CaptureIconItem: TToolButton;
    CaptureLargeIconItem: TToolButton;
    CaptureSelectionItem: TToolButton;
    Panel2: TPanel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    SavePictureDialog1: TSavePictureDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenItem1: TMenuItem;
    CloseItem1: TMenuItem;
    CloseAllItem1: TMenuItem;
    SaveAsItem1: TMenuItem;
    N1: TMenuItem;
    PrintItem1: TMenuItem;
    PrintSetupItem1: TMenuItem;
    N7: TMenuItem;
    ExitItem1: TMenuItem;
    Capture1: TMenuItem;
    DesktopItem1: TMenuItem;
    DualMonitorDesktop1: TMenuItem;
    AreaItem1: TMenuItem;
    ActiveWindowItem1: TMenuItem;
    ObjectItem1: TMenuItem;
    FreeForm1: TMenuItem;
    CaptureIcon1: TMenuItem;
    Miminized1: TMenuItem;
    MinimizeItem1: TMenuItem;
    AutomaticItem1: TMenuItem;
    CursorItem1: TMenuItem;
    ShowInfoItem1: TMenuItem;
    ShowHintItem1: TMenuItem;
    N5: TMenuItem;
    DelayItem1: TMenuItem;
    Help1: TMenuItem;
    HelpContents1: TMenuItem;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    Image1: TImage;
    SpeedItem1: TMenuItem;
    CaptureSmallIconItem1: TMenuItem;
    CaptureLargeIconItem1: TMenuItem;
    CaptureSelectionItem1: TMenuItem;
    SaveToDesktopItem1: TToolButton;
    ToolButton1: TToolButton;
    ResetCaptureCountItem1: TToolButton;
    Panel3: TPanel;
    ImageList2: TImageList;
    TaskDialog1: TTaskDialog;
    procedure FormCreate ( Sender: TObject );
    procedure ASGScreenCapture1BeforeCapture ( Sender: TObject );
    procedure SaveAsItemClick ( Sender: TObject );
    procedure SavePictureDialog1TypeChange ( Sender: TObject );
    procedure Button1Click ( Sender: TObject );
    procedure PageControl1Change ( Sender: TObject );
    procedure Auto1Click ( Sender: TObject );
    procedure Minimize1Click ( Sender: TObject );
    procedure ShowCursor1Click ( Sender: TObject );
    procedure ShowInfoDialog1Click ( Sender: TObject );
    procedure ShowHint1Click ( Sender: TObject );
    procedure AutomaticItem1Click ( Sender: TObject );
    procedure MinimizeItem1Click ( Sender: TObject );
    procedure CursorItem1Click ( Sender: TObject );
    procedure ShowInfoItem1Click ( Sender: TObject );
    procedure ShowHintItem1Click ( Sender: TObject );
    procedure DelayItem1Click ( Sender: TObject );
    procedure SaveToDesktopItem1Click ( Sender: TObject );
    procedure OpenItemClick ( Sender: TObject );
    procedure CloseItemClick ( Sender: TObject );
    procedure CloseAllItemClick ( Sender: TObject );
    procedure ExitItemClick ( Sender: TObject );
    procedure CaptureDesktopItemClick ( Sender: TObject );
    procedure CaptureSpeedItem1Click ( Sender: TObject );
    procedure CaptureWholeDesktopItemClick ( Sender: TObject );
    procedure CaptureActiveItemClick ( Sender: TObject );
    procedure CaptureAreaItemClick ( Sender: TObject );
    procedure CaptureObjectItemClick ( Sender: TObject );
    procedure CapturePolygonItemClick ( Sender: TObject );
    procedure CaptureSmallIconItemClick ( Sender: TObject );
    procedure CaptureIconItemClick ( Sender: TObject );
    procedure CaptureLargeIconItemClick ( Sender: TObject );
    procedure CaptureSelectionItemClick ( Sender: TObject );
    procedure HelpContents1Click ( Sender: TObject );
    procedure ResetCaptureCountItem1Click ( Sender: TObject );
  private
    { Private declarations }
    FTabSheet: TTabSheet;
    FScrollBox: TScrollBox;
    Image: Timage;
    FPath: string;
    procedure AddTabsheet;
    procedure ClearStatusbar;
    procedure UpdateControls;
    procedure UpdateStatusbar;
    procedure ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean;
      const r: TRect; const Msg: string );
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Math, ShlObj, ShellAPI, StrUtils, HTMLHelpViewer, uScreenDelay;

function DesktopFolder: string;
// Find Desktop folder location
var
  iR: Bool;
  iPath: array [ 0..MAX_PATH ] of Char;
begin

  iR := ShlObj.ShGetSpecialFolderPath ( 0, iPath, CSIDL_DESKTOP, False );
  if not iR then
    raise Exception.Create ( 'Could not find Documents folder location.' );

  Result := IncludeTrailingBackSlash ( iPath );

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

procedure TForm1.PageControl1Change ( Sender: TObject );
begin

  FPath := PageControl1.ActivePage.Hint;

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    if Assigned ( Image ) then
    begin

      Image1.Picture := Image.Picture;
      Image1.Update;

    end;

  end;

  UpdateStatusbar;

end;

procedure TForm1.ProgressUpdate ( Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: boolean;
  const r: TRect; const Msg: string );
begin

  if Stage = psRunning then
  begin

    Caption := Format ( '%d%%', [ PercentDone ] );
    ProgressBar1.Position := PercentDone;

  end
  else
    Caption := 'Apprehend 6.0 TImage Demo';

end;

procedure TForm1.ResetCaptureCountItem1Click ( Sender: TObject );
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

procedure TForm1.AddTabsheet;
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
    FScrollBox.ParentBackground := True;

    // Create an Image component
    Image := TImage.Create ( Self );
    Image.Parent := FScrollBox;
    Image.Align := alClient;
    Image.Left := 0;
    Image.Top := 0;
    Image.AutoSize := False;
    Image.Center := True;
    Image.Visible := True;
    Image.ShowHint := True;
    Image.OnProgress := ProgressUpdate;

  end;

end;

procedure TForm1.UpdateControls;
begin

  OpenItem.Enabled := True;
  CloseItem.Enabled := PageControl1.PageCount <> 0;
  CloseAllItem.Enabled := PageControl1.PageCount > 1;
  SaveAsItem.Enabled := PageControl1.PageCount <> 0;
  SaveToDesktopItem1.Enabled := PageControl1.PageCount <> 0;
  CaptureDesktopItem.Enabled := True;
  CaptureSpeedItem1.Enabled := True;
  CaptureWholeDesktopItem.Enabled := True;
  CaptureActiveItem.Enabled := True;
  CaptureAreaItem.Enabled := True;
  CaptureObjectItem.Enabled := True;
  CapturePolygonItem.Enabled := True;
  CaptureSmallIconItem.Enabled := True;
  CaptureIconItem.Enabled := True;
  CaptureLargeIconItem.Enabled := True;
  CaptureSelectionItem.Enabled := True;
  ResetCaptureCountItem1.Enabled := True;

  CloseItem1.Enabled := PageControl1.PageCount <> 0;
  CloseAllItem1.Enabled := PageControl1.PageCount > 1;
  SaveAsItem1.Enabled := PageControl1.PageCount <> 0;
  PrintItem1.Enabled := PageControl1.PageCount <> 0;

end;

procedure TForm1.ASGScreenCapture1BeforeCapture ( Sender: TObject );
// Set the ASGScreenCapture options before image is captured
begin

  ASGScreenCapture1.Auto := Auto1.Checked;
  ASGScreenCapture1.Minimize := Minimize1.Checked;
  ASGScreenCapture1.ShowCursor := ShowCursor1.Checked;
  ASGScreenCapture1.ShowHint := ShowHint1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfoDialog1.Checked;
  ASGScreenCapture1.Delay := StrToIntDef ( CaptureDelay1.Text, 500 );

end;

procedure TForm1.Auto1Click ( Sender: TObject );
begin

  AutomaticItem1.Checked := Auto1.Checked;

end;

procedure TForm1.AutomaticItem1Click ( Sender: TObject );
begin

  Auto1.Checked := AutomaticItem1.Checked;

end;

procedure TForm1.Button1Click ( Sender: TObject );
begin

  Close;

end;

procedure TForm1.CaptureActiveItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureActiveWindow;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 13;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureAreaItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 14;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 10;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 18;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureLargeIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureLargeIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 19;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureObjectItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureObject;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 15;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CapturePolygonItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CapturePolygon;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 16;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          if Assigned ( Image.Picture.Graphic ) then
          begin
            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;
          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureSelectionItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 20;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureSmallIconItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureSmallIcon;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 17;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureSpeedItem1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.SpeedCaptureDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 11;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.CaptureWholeDesktopItemClick ( Sender: TObject );
var
  iBitmap: TBitmap;
begin

  // Add a tabsheet to the pagecontrol
  AddTabsheet;

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Capture the desktop
      iBitmap := ASGScreenCapture1.CaptureWholeDesktop;
      try

        if Assigned ( iBitmap ) then
        begin

          // Assign the image picture the bitmap
          Image.Picture.Assign ( iBitmap );
          Image.Update;

          // Set the scrollbox ranges
          FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

          with FScrollBox do
          begin

            HorzScrollBar.range := iBitmap.Width;
            VertScrollBar.range := iBitmap.Height;

          end;

          // Give the image a default filename
          FPath := DesktopFolder + 'Capture' + IntToStr ( AsgScreenCapture1.CaptureCount ) + '.bmp';
          // Set the tabsheets caption
          PageControl1.ActivePage.Caption := 'Capture ' + IntToStr ( AsgScreenCapture1.CaptureCount );
          PageControl1.ActivePage.Hint := FPath;
          PageControl1.ActivePage.ImageIndex := 12;

          if Assigned ( Image.Picture.Graphic ) then
          begin

            Image1.Picture.Assign ( Image.Picture );
            Image1.Update;

          end;

          UpdateControls;
          UpdateStatusbar;

        end;

        // Free the bitmap
      finally
        iBitmap.Free;
      end;

    end;

  end;

end;

procedure TForm1.ClearStatusbar;
var
  i: integer;
begin

  for i := StatusBar1.Panels.Count - 1 downto 0 do
    StatusBar1.Panels [ i ].Text := '';

end;

procedure TForm1.CloseAllItemClick ( Sender: TObject );
var
  i: integer;
begin

  // Close All pages
  if PageControl1.PageCount - 1 > 0 then
  begin

    TaskDialog1.Title := 'Close All';
    TaskDialog1.Caption := 'Close All ';
    TaskDialog1.Text := 'Close all images?';
    TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
    TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
    if TaskDialog1.Execute then
      if TaskDialog1.ModalResult = mrYes then

        for i := PageControl1.PageCount - 1 downto 0 do
        begin

          PageControl1.ActivePage := PageControl1.Pages [ i ];
          PageControl1.ActivePage.Free; // Closes and Frees the ActivePage
          PageControl1.SelectNextPage ( False );

        end;

    UpdateControls;

    if PageControl1.PageCount = 0 then
    begin

      Caption := 'Apprehend 6.0 TImage Demo';
      Image1.Picture := nil;
      ClearStatusbar;
      ASGScreenCapture1.ResetCaptureCount;

    end;

  end;

end;

procedure TForm1.CloseItemClick ( Sender: TObject );
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Close the active page
    PageControl1.ActivePage.Free;
    PageControl1.SelectNextPage ( False );
    UpdateControls;

  end;

  if PageControl1.PageCount = 0 then
  begin

    Caption := 'Apprehend 6.0 TImage Demo';
    ClearStatusbar;
    Image1.Picture := nil;
    ASGScreenCapture1.ResetCaptureCount;

  end;

end;

procedure TForm1.CursorItem1Click ( Sender: TObject );
begin

  ShowCursor1.Checked := CursorItem1.Checked;

end;

procedure TForm1.DelayItem1Click ( Sender: TObject );
begin

  FormDelay := TFormDelay.Create ( Self );
  try

    FormDelay.edScreenDelay.Text := IntToStr ( ASGScreenCapture1.Delay );
    if FormDelay.ShowModal = mrOk then
      ASGScreenCapture1.Delay := StrToInt ( FormDelay.edScreenDelay.Text );
    UpDown1.Position := StrToInt ( FormDelay.edScreenDelay.Text );

  finally
    FormDelay.Free;
  end;

end;

procedure TForm1.ExitItemClick ( Sender: TObject );
begin
  Close;
end;

procedure TForm1.UpdateStatusbar;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    Caption := 'Apprehend 6.0 Demo- ' + FPath;
    StatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FPath );
    StatusBar1.Panels [ 1 ].Text := ExtractFilename ( FPath );

    if Assigned ( Image.Picture.Graphic ) then
      if not Image.Picture.Graphic.Empty then
      begin

        StatusBar1.Panels [ 2 ].Text := 'Height: ' + IntegerToString ( Image.Picture.Graphic.Height ) + ' pixels';
        StatusBar1.Panels [ 3 ].Text := 'Width: ' + IntegerToString ( Image.Picture.Graphic.Width ) + ' pixels ';

      end
      else
      begin

        StatusBar1.Panels [ 2 ].Text := '';
        StatusBar1.Panels [ 3 ].Text := '';

      end;

  end;

  // Get the pixelformat from the loaded graphic format
  if Lowercase ( ExtractFileExt ( FPath ) ) = '.bmp' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( Image.Picture.Bitmap.PixelFormat )
  else if Lowercase ( ExtractFileExt ( FPath ) ) = '.png' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PNGBitDepthToString ( PNGBitsForPixel (
      TPNGImage ( Image1.Picture.Graphic ).Header.ColorType,
      TPNGImage ( Image1.Picture.Graphic ).Header.BitDepth
      ) )
  else if Lowercase ( ExtractFileExt ( FPath ) ) = '.jpg' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + JpegPixelFormatToString ( TJPEGImage ( Image.Picture.Graphic ).PixelFormat
      )
  else if Lowercase ( ExtractFileExt ( FPath ) ) = '.gif' then
    StatusBar1.Panels [ 4 ].Text := ' Color Depth: ' + PixelFormatToString ( BitDepthToPixelFormat ( TGIFImage (
      Image.Picture.Graphic ).BitsPerPixel ) );

  if Assigned ( Image.Picture.Graphic ) then
  begin

    Image1.Picture.Assign ( Image.Picture );
    Image1.Update;

  end;

  Caption := 'Apprehend 6.0 Demo - ' + FPath;

end;

procedure TForm1.FormCreate ( Sender: TObject );
var
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  ASGScreenCapture1.CaptureCount := 0;
  ToolBar1.DrawingStyle := dsGradient;
  ToolBar2.DrawingStyle := dsGradient;
  ToolBar3.DrawingStyle := dsGradient;
  UpdateControls;

  // Setup the HelpFile
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.chm';

end;

procedure TForm1.HelpContents1Click ( Sender: TObject );
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

procedure TForm1.Minimize1Click ( Sender: TObject );
begin

  MinimizeItem1.Checked := Minimize1.Checked;

end;

procedure TForm1.MinimizeItem1Click ( Sender: TObject );
begin

  Minimize1.Checked := MinimizeItem1.Checked;

end;

procedure TForm1.OpenItemClick ( Sender: TObject );
begin

  OpenPictureDialog1.Filter := '';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter +
    'All Supported Image Files (*.bmp;*.png;*.jpg*.jpeg;*.gif)|*.bmp;*.png;*.jpg;*.jpeg;*.gif|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Windows Bitmap File (*.bmp)|*.bmp|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Png Image File (*.png)|*.png|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'JPeg Image File (*.jpg)|*.jpg|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter + 'Gif Image File (*.gif)|*.gif|';

  if OpenPictureDialog1.Execute then
  begin

    Screen.Cursor := crHourGlass;
    try

      ProgressBar1.Visible := True;

      // Add ScrollBox and Image Controls to a new tabsheet
      AddTabsheet;

      if Assigned ( PageControl1.ActivePage ) then
      begin

        Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );
        try

          FPath := OpenPictureDialog1.FileName;

          Image.Picture.LoadFromFile ( FPath );
          Image.Update;

          Image1.Picture.Assign ( Image.Picture );
          Image1.Update;

        except
          ShowMessage ( 'Error loading image' );
        end;

        if Image.Picture.Graphic.Empty then
        begin

          PageControl1.ActivePage.Free;
          Exit;

        end;

        FTabSheet.Caption := ExtractFilename ( FPath );
        FTabSheet.Hint := FPath;

        FScrollBox := TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] );

        with FScrollBox do
        begin

          HorzScrollBar.range := Image.Picture.Width;
          VertScrollBar.range := Image.Picture.Height;

        end;

      end;

      UpdateControls;
      UpdateStatusbar;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

  ProgressBar1.Visible := False;
end;

procedure TForm1.SaveAsItemClick ( Sender: TObject );
var
  iExtension: string;
begin

  if Assigned ( PageControl1.ActivePage ) then
  begin

    Screen.Cursor := crHourGlass;
    try

      Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

      SavePictureDialog1.FileName := ExtractFilename ( FPath );
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

        FPath := SavePictureDialog1.FileName;
        iExtension := ExtractFileExt ( FPath );
        if Length ( iExtension ) = 0 then
          iExtension := '.bmp';

        if iExtension = '.jpg' then
        begin

          if Assigned ( Image ) then
          begin

            try

              // Convert TImage bitmap to JPEG and save
              SaveBMPAsJPEG ( Image.Picture.Bitmap, FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( Image ) then
          begin

            try

              // Convert TImage bitmap to PNG and save
              SaveBMPAsPNG ( Image.Picture.Bitmap, FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( Image ) then
          begin

            try

              // Convert TImage bitmap to GIF and save
              SaveBMPAsGIF ( Image.Picture.Bitmap, FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          Image.Picture.SaveToFile ( SavePictureDialog1.Filename );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
          end;

        end;

        Caption := 'Apprehend 6.0 Demo- ' + FPath;
        PageControl1.ActivePage.Caption := ExtractFilename ( FPath );

      end;

    finally
      Screen.Cursor := crDefault;
    end;

  end;

end;

procedure TForm1.SavePictureDialog1TypeChange ( Sender: TObject );
var
  FilePath: string;
  FileName: string;
  FileExt: string;
begin

  FilePath := SavePictureDialog1.FileName;
  FileExt := ExtractFileExt ( FilePath );
  FileName := ExtractFilename ( FilePath );

end;

procedure TForm1.SaveToDesktopItem1Click ( Sender: TObject );
var
  i: integer;
  iExtension: string;
begin

  // If page is assigned then get the image handle
  if Assigned ( PageControl1.ActivePage ) then
  begin

    // Get the image handle
    Image := TImage ( TScrollBox ( PageControl1.ActivePage.Controls [ 0 ] ).Controls [ 0 ] );

    // If Image is assigned then capture the desktop
    if Assigned ( Image ) then
    begin

      // Save current file

      Screen.Cursor := crHourGlass;
      try

        FPath := PageControl1.ActivePage.Hint;
        FPath := IncludeTrailingPathDelimiter ( DesktopFolder ) + ExtractFilename ( FPath );
        iExtension := ExtractFileExt ( FPath );
        i := 0;

        // if the path contains 'Capture' then check if it exists
        if StrUtils.AnsiContainsText ( FPath, 'Capture' ) then
        begin

          // Increment the capture number if the file exists
          while FileExists ( FPath ) do
          begin

            Inc ( i );
            FPath := DesktopFolder + 'Capture' + IntToStr ( i ) + iExtension;

          end;

        end
        else
        if FileExists ( FPath ) then
        begin

          TaskDialog1.Title := 'File Exists';
          TaskDialog1.Caption := 'File Exists';
          TaskDialog1.Text := 'The file ' + FPath + ' exists.  Overwrite the file?';
          TaskDialog1.CommonButtons := [ tcbYes, tcbNo ];
          TaskDialog1.Flags := [ tfUseHiconMain, tfAllowDialogCancellation ];
          if TaskDialog1.Execute then
          if TaskDialog1.ModalResult = mrNo then
            exit;

        end;

        if iExtension = '.jpg' then
        begin

          if Assigned ( Image ) then
          begin

            try

             Image.Picture.Graphic.SaveToFile( FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.png' then
        begin

          if Assigned ( Image ) then
          begin

            try

              Image.Picture.Graphic.SaveToFile( FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else if iExtension = '.gif' then
        begin

          if Assigned ( Image ) then
          begin

            try

              Image.Picture.Graphic.SaveToFile( FPath );

            except
              on EInvalidGraphic do
                TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
            end;

          end;

        end
        else
        begin

          // Save bitmap to file

          Image.Picture.SaveToFile ( FPath );
          try

          except
            on EInvalidGraphic do
              TaskMessageDlg ( 'Error', 'Error saving file,' + FPath, mtWarning, [ mbOK ], 0 );
          end;

        end;

        PageControl1.ActivePage.Caption := ExtractFilename ( FPath );
        PageControl1.ActivePage.Hint := FPath;

        Caption := 'Apprehend 6.0 Demo- ' + FPath;
        StatusBar1.Panels [ 0 ].Text := ExtractFileDir ( FPath );
        StatusBar1.Panels [ 1 ].Text := ExtractFilename ( FPath );

      finally
        Screen.Cursor := crDefault;
      end;

    end;

  end;

end;

procedure TForm1.ShowCursor1Click ( Sender: TObject );
begin

  CursorItem1.Checked := ShowCursor1.Checked;

end;

procedure TForm1.ShowHint1Click ( Sender: TObject );
begin

  ShowHintItem1.Checked := ShowHint1.Checked;

end;

procedure TForm1.ShowHintItem1Click ( Sender: TObject );
begin

  ShowHint1.Checked := ShowHintItem1.Checked;

end;

procedure TForm1.ShowInfoDialog1Click ( Sender: TObject );
begin

  ShowInfoItem1.Checked := ShowInfoDialog1.Checked;

end;

procedure TForm1.ShowInfoItem1Click ( Sender: TObject );
begin

  ShowInfoDialog1.Checked := ShowInfoItem1.Checked;

end;

end.

