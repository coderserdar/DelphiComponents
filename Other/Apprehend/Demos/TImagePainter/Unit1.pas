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
  Dialogs, ASGCapture, StdCtrls, ExtCtrls, ComCtrls, ImgList, ExtDlgs, ImagePainter, Buttons;

type
  TForm1 = class ( TForm )
    Panel1: TPanel;
    ASGScreenCapture1: TASGScreenCapture;
    ImageList1: TImageList;
    TaskDialog1: TTaskDialog;
    SavePictureDialog1: TSavePictureDialog;
    ImagePainter1: TImagePainter;
    Panel3: TPanel;
    Desktop1: TButton;
    Automatic1: TCheckBox;
    Active1: TButton;
    Minimize1: TCheckBox;
    Rectangle1: TButton;
    ShowCursor1: TCheckBox;
    ShowHint1: TCheckBox;
    ShowInfoDialog1: TCheckBox;
    Label1: TLabel;
    CaptureDelay1: TEdit;
    Object1: TButton;
    Polygon1: TButton;
    SmallIcon1: TButton;
    Icon1: TButton;
    LargeIcon1: TButton;
    SpecificSize1: TButton;
    UpDown1: TUpDown;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Button1: TButton;
    Save1: TButton;
    Reset1: TButton;
    StatusBar1: TStatusBar;
    Button2: TButton;
    Panel9: TPanel;
    Button5: TButton;
    Pencil1: TSpeedButton;
    Line1: TSpeedButton;
    Load1: TButton;
    ImageList2: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    Image1: TImage;
    Fit1: TCheckBox;
    CaptureCount1: TLabel;
    LineWidth1: TEdit;
    PenWidth1: TUpDown;
    Label2: TLabel;
    Label3: TLabel;
    ColorBox1: TColorBox;
    SaveAs1: TButton;
    Undo1: TButton;
    Redo1: TButton;
    procedure Desktop1Click ( Sender: TObject );
    procedure Active1Click ( Sender: TObject );
    procedure Rectangle1Click ( Sender: TObject );
    procedure Object1Click ( Sender: TObject );
    procedure Polygon1Click ( Sender: TObject );
    procedure SmallIcon1Click ( Sender: TObject );
    procedure Icon1Click ( Sender: TObject );
    procedure LargeIcon1Click ( Sender: TObject );
    procedure SpecificSize1Click ( Sender: TObject );
    procedure Reset1Click ( Sender: TObject );
    procedure FormCreate ( Sender: TObject );
    procedure ASGScreenCapture1BeforeCapture ( Sender: TObject );
    procedure Button1Click ( Sender: TObject );
    procedure Save1Click ( Sender: TObject );
    procedure Pencil1Click ( Sender: TObject );
    procedure Line1Click ( Sender: TObject );
    procedure Load1Click ( Sender: TObject );
    procedure Button2Click ( Sender: TObject );
    procedure Button5Click ( Sender: TObject );
    procedure ImagePainter1ChangeObject ( Sender: TObject; Figure: TFigure; ChangeKind: TChangeKind );
    procedure ImagePainter1MouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    procedure Fit1Click ( Sender: TObject );
    procedure ColorBox1Change ( Sender: TObject );
    procedure PenWidth1Click ( Sender: TObject; Button: TUDBtnType );
    procedure SaveAs1Click ( Sender: TObject );
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImagePainter1Cursor(Sender: TObject; Tool: string; var MyCursor: TCursor);
  private
    { Private declarations }
    FFilePath: string;
    procedure GetCaptureProperties;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  // Special Cursors
const
  crDotCross = 1;
  crDropper = 2;
  crDropper1 = 3;
  crDropper2 = 4;
  crErase = 5;
  crHand = 6;
  crSelectLasso = 7;
  crMagicWand = 8;
  crMagnifier = 9;
  crPaint = 10;
  crPen = 11;
  crPencil = 12;
  crPotOPaint = 13;
  crPotOPaint2 = 14;
  crSelectEllipse = 15;
  crSelectPolygon = 16;
  crSelectRect = 17;
  crSprayGun = 18;
  crTargetCross = 19;
  crTargetCross2 = 20;
  crZoom = 21;
  crSelectZoom = 22;
  crSmallPencil = 23;
  crDrawLine = 24;
  crClone = 25;
  crSmudge = 26;
  crSmear = 27;
  crSmooth = 28;
  crSharpen = 29;
  crLighten = 30;
  crDarken = 31;
  crWarp = 32;
  crRect = 33;
  crCircle = 34;
  crFilledRect = 35;
  crFilledCircle = 36;
  crZoomIn = 37;
  crZoomOut = 38;
implementation

{$R *.dfm}
{$R toolscursors.res}

uses ShlObj, HTMLHelpViewer;

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
  Result := AddThousandSeparator ( IntToStr ( i ), SysUtils.ThousandSeparator );
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

procedure TForm1.GetCaptureProperties;
begin

  ASGScreenCapture1.Minimize := Minimize1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfoDialog1.Checked;
  ASGScreenCapture1.Auto := Automatic1.Checked;
  ASGScreenCapture1.ShowCursor := ShowCursor1.Checked;
  ASGScreenCapture1.ShowHint := ShowHint1.Checked;

end;

procedure TForm1.SpecificSize1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
  try

    if Assigned ( iBitmap ) then
      ImagePainter1.LoadFromBitmap ( iBitmap )
    else
    begin
      exit;
    end;

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Undo1Click(Sender: TObject);
begin

  ImagePainter1.Undo;
  Undo1.Enabled := (flCanUndo in ImagePainter1.Flags);

end;

procedure TForm1.PenWidth1Click ( Sender: TObject; Button: TUDBtnType );
begin

  ImagePainter1.Pen.Width := PenWidth1.Position;

end;

procedure TForm1.ASGScreenCapture1BeforeCapture ( Sender: TObject );
// Set ASGScreenCapture properties in the BeforeCapture event
begin

  ASGScreenCapture1.Auto := Automatic1.Checked;
  ASGScreenCapture1.Minimize := Minimize1.Checked;
  ASGScreenCapture1.ShowCursor := ShowCursor1.Checked;
  ASGScreenCapture1.ShowHint := ShowHint1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfoDialog1.Checked;
  ASGScreenCapture1.Delay := StrToIntDef ( CaptureDelay1.Text, 500 );

end;

procedure TForm1.Button1Click ( Sender: TObject );
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

procedure TForm1.Button2Click ( Sender: TObject );
begin
  Close;
end;

procedure TForm1.Button5Click ( Sender: TObject );
begin
  Close;
end;

procedure TForm1.ColorBox1Change ( Sender: TObject );
begin

  ImagePainter1.Pen.Color := ColorBox1.Selected;

end;

procedure TForm1.Desktop1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureDesktop;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Fit1Click ( Sender: TObject );
begin

  if Fit1.Checked then
    ImagePainter1.FitWithGrid
  else
    ImagePainter1.ZoomFactor := 100;

end;

procedure TForm1.FormCreate ( Sender: TObject );
var
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  // Setup the screen cursors
  Screen.Cursors [ crCircle ] := LoadCursor ( HInstance, 'CR_CIRCLE' );
  Screen.Cursors [ crClone ] := LoadCursor ( HInstance, 'CR_CLONE' );
  Screen.Cursors [ crDarken ] := LoadCursor ( HInstance, 'CR_DARKEN' );
  Screen.Cursors [ crDotCross ] := LoadCursor ( HInstance, 'CR_DOTCROSS' );
  Screen.Cursors [ crDrawLine ] := LoadCursor ( HInstance, 'CR_DRAWLINE' );
  Screen.Cursors [ crDropper ] := LoadCursor ( HInstance, 'CR_DROPPER' );
  Screen.Cursors [ crDropper1 ] := LoadCursor ( HInstance, 'CR_DROPPER1' );
  Screen.Cursors [ crDropper2 ] := LoadCursor ( HInstance, 'CR_DROPPER2' );
  Screen.Cursors [ crErase ] := LoadCursor ( HInstance, 'CR_ERASE' );
  Screen.Cursors [ crFilledCircle ] := LoadCursor ( HInstance, 'CR_FILLEDCIRCLE' );
  Screen.Cursors [ crFilledRect ] := LoadCursor ( HInstance, 'CR_FILLEDRECT' );
  Screen.Cursors [ crFilledRect ] := LoadCursor ( HInstance, 'CR_FILLEDROUNDRECT' );
  Screen.Cursors [ crHand ] := LoadCursor ( HInstance, 'CR_HAND' );
  Screen.Cursors [ crSelectLasso ] := LoadCursor ( HInstance, 'CR_LASSO' );
  Screen.Cursors [ crLighten ] := LoadCursor ( HInstance, 'CR_LIGHTEN' );
  Screen.Cursors [ crMagicWand ] := LoadCursor ( HInstance, 'CR_MAGICWAND' );
  Screen.Cursors [ crZoom ] := LoadCursor ( HInstance, 'CR_MAGNIFIER' );
  Screen.Cursors [ crPaint ] := LoadCursor ( HInstance, 'CR_PAINT' );
  Screen.Cursors [ crPen ] := LoadCursor ( HInstance, 'CR_PEN' );
  Screen.Cursors [ crPencil ] := LoadCursor ( HInstance, 'CR_PENCIL' );
  Screen.Cursors [ crPotOPaint ] := LoadCursor ( HInstance, 'CR_POTOPAINT' );
  Screen.Cursors [ crPotOPaint2 ] := LoadCursor ( HInstance, 'CR_POTOPAINT2' );
  Screen.Cursors [ crSmear ] := LoadCursor ( HInstance, 'CR_RECT' );
  Screen.Cursors [ crFilledRect ] := LoadCursor ( HInstance, 'CR_ROUNDRECT' );
  Screen.Cursors [ crSelectEllipse ] := LoadCursor ( HInstance, 'CR_SELECTELLIPSE' );
  Screen.Cursors [ crSelectPolygon ] := LoadCursor ( HInstance, 'CR_SELECTPOLYGON' );
  Screen.Cursors [ crSelectRect ] := LoadCursor ( HInstance, 'CR_SELECTRECT' );
  Screen.Cursors [ crSelectZoom ] := LoadCursor ( HInstance, 'CR_SELECTZOOM' );
  Screen.Cursors [ crSharpen ] := LoadCursor ( HInstance, 'CR_SHARPEN' );
  Screen.Cursors [ crSmallPencil ] := LoadCursor ( HInstance, 'CR_SMALLPENCIL' );
  Screen.Cursors [ crSmear ] := LoadCursor ( HInstance, 'CR_SMEAR' );
  Screen.Cursors [ crSmooth ] := LoadCursor ( HInstance, 'CR_SMOOTH' );
  Screen.Cursors [ crSmudge ] := LoadCursor ( HInstance, 'CR_SMUDGE' );
  Screen.Cursors [ crSprayGun ] := LoadCursor ( HInstance, 'CR_SPRAYGUN' );
  Screen.Cursors [ crTargetCross ] := LoadCursor ( HInstance, 'CR_TARGETCROSS' );
  Screen.Cursors [ crTargetCross2 ] := LoadCursor ( HInstance, 'CR_TARGETCROSS2' );
  Screen.Cursors [ crWarp ] := LoadCursor ( HInstance, 'CR_WARP' );
  Screen.Cursors [ crZoom ] := LoadCursor ( HInstance, 'CR_ZOOM' );
  Screen.Cursors [ crZoomOut ] := LoadCursor ( HInstance, 'CR_ZOOMIN' );
  Screen.Cursors [ crZoomIn ] := LoadCursor ( HInstance, 'CR_ZOOMOUT' );

  ASGScreenCapture1.CaptureCount := 0;

  // Setup the HelpFile
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.chm';

end;

procedure TForm1.FormShow(Sender: TObject);
begin

  ImagePainter1.FitWithGrid;

  Undo1.Enabled := (flCanUndo in ImagePainter1.Flags);
  Redo1.Enabled := (flCanRedo in ImagePainter1.Flags);
end;

procedure TForm1.Active1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureActiveWindow;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';
  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Rectangle1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureSelection;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Redo1Click(Sender: TObject);
begin

  ImagePainter1.Redo;
  Redo1.Enabled := (flCanRedo in ImagePainter1.Flags);

end;

procedure TForm1.Reset1Click ( Sender: TObject );
begin

  ImagePainter1.Clear;
  ASGScreenCapture1.ResetCaptureCount;
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Object1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureObject;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Pencil1Click ( Sender: TObject );
begin

  if Pencil1.Down then
  begin

    ImagePainter1.ActiveTool := atPencil;

  end
  else
  begin

    ImagePainter1.ActiveTool := atNone;

  end;

end;

procedure TForm1.Polygon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CapturePolygon;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Save1Click ( Sender: TObject );
begin

  if ImagePainter1.ImageBitmap <> nil then
  begin

    if FileExists( FFilePath ) then
    begin

      ImagePainter1.SaveToFile ( FFilePath );

    end;

  end
  else
  begin

    TaskDialog1.Caption := 'Warning ';
    TaskDialog1.Title := 'File Does Not Exist';
    TaskDialog1.Text := 'The file' + FFilePath + ' does not exist.';
    TaskDialog1.CommonButtons := [ tcbOk ];
    TaskDialog1.Flags := [ tfAllowDialogCancellation ];
    TaskDialog1.MainIcon := tdiError;
    TaskDialog1.Execute;

  end;

end;

procedure TForm1.SaveAs1Click ( Sender: TObject );
begin

  if ImagePainter1.ImageBitmap <> nil then
  begin

    SavePictureDialog1.Filename := ExtractFileName ( FFilePath );
    SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );
    SavePictureDialog1.FilterIndex := 2; // *.bmp
    if SavePictureDialog1.Execute then
    begin

      FFilePath := SavePictureDialog1.FileName;
      ImagePainter1.SaveToFile ( FFilePath );
      Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
      Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
      Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

    end;

  end
  else
  begin

    TaskDialog1.Title := 'Warning';
    TaskDialog1.Caption := 'Warning ';
    TaskDialog1.Text := 'The image is empty.';
    TaskDialog1.CommonButtons := [ tcbOk ];
    TaskDialog1.Flags := [ tfAllowDialogCancellation ];
    TaskDialog1.MainIcon := tdiError;
    TaskDialog1.Execute;

  end;

end;

procedure TForm1.SmallIcon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureSmallIcon;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Icon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureIcon;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.ImagePainter1ChangeObject ( Sender: TObject; Figure: TFigure; ChangeKind: TChangeKind );
begin

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

  Undo1.Enabled := (flCanUndo in ImagePainter1.Flags);
  Redo1.Enabled := (flCanRedo in ImagePainter1.Flags);

end;

procedure TForm1.ImagePainter1Cursor(Sender: TObject; Tool: string; var MyCursor: TCursor);
begin

    if Tool = atPencil then
      MyCursor := crSmallPencil
    else if Tool = atLine then
      MyCursor := crDrawLine
    else if Tool = atNone then
      MyCursor := crDefault;

end;

procedure TForm1.ImagePainter1MouseUp ( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;
end;

procedure TForm1.LargeIcon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  GetCaptureProperties;

  iBitmap := ASGScreenCapture1.CaptureLargeIcon;
  try

    ImagePainter1.LoadFromBitmap ( iBitmap );

  finally
    iBitmap.Free;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
  Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
  Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

  if Fit1.Checked then
    ImagePainter1.FitWithGrid;

  Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
  Image1.Update;

end;

procedure TForm1.Line1Click ( Sender: TObject );
begin

  if Line1.Down then
  begin

    ImagePainter1.ActiveTool := atLine;
    ImagePainter1.Cursor := crDrawLine;

  end
  else
  begin

    ImagePainter1.ActiveTool := atNone;
    ImagePainter1.Cursor := crDefault;

  end;

end;

procedure TForm1.Load1Click ( Sender: TObject );
begin

  if OpenPictureDialog1.Execute ( ) then
  begin

    FFilePath := OpenPictureDialog1.FileName;
    Caption := 'Apprehend/ImagePainter Demo- ' + FFilePath;
    Statusbar1.Panels [ 0 ].Text := ExtractFilePath ( FFilePath );
    Statusbar1.Panels [ 1 ].Text := ExtractFileName ( FFilePath );

    ImagePainter1.LoadFromFile ( FFilePath );
    if ImagePainter1.IsDocument then
      ImagePainter1.ScrollTo ( ImagePainter1.ImageLeft, ImagePainter1.ImageTop );

    ImagePainter1.FitWithGrid;

    Image1.Picture.Bitmap.Assign ( ImagePainter1.ImageBitmap );
    Image1.Update;

  end;

end;

end.

