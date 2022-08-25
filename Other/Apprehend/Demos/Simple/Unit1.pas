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
  Dialogs, ASGCapture, StdCtrls, ExtCtrls, ComCtrls, ImgList, ExtDlgs;

type
  TForm1 = class ( TForm )
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Desktop1: TButton;
    Active1: TButton;
    Rectangle1: TButton;
    Object1: TButton;
    Polygon1: TButton;
    SmallIcon1: TButton;
    Icon1: TButton;
    LargeIcon1: TButton;
    SpecificSize1: TButton;
    ASGScreenCapture1: TASGScreenCapture;
    Reset1: TButton;
    CaptureCount1: TLabel;
    Auto1: TCheckBox;
    Minimize1: TCheckBox;
    ShowCursor1: TCheckBox;
    ShowHint1: TCheckBox;
    ShowInfoDialog1: TCheckBox;
    CaptureDelay1: TEdit;
    Label1: TLabel;
    UpDown1: TUpDown;
    ImageList1: TImageList;
    Button1: TButton;
    TaskDialog1: TTaskDialog;
    Save1: TButton;
    SavePictureDialog1: TSavePictureDialog;
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
  private
    { Private declarations }
    FFilePath: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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

procedure TForm1.SpecificSize1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
  try

    if Assigned ( iBitmap ) then
      Image1.Picture.Assign ( iBitmap )
    else
    begin
      exit;
    end;

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.ASGScreenCapture1BeforeCapture ( Sender: TObject );
// Set ASGScreenCapture properties in the BeforeCapture event
begin

  ASGScreenCapture1.Auto := Auto1.Checked;
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

procedure TForm1.Desktop1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureDesktop;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.FormCreate ( Sender: TObject );
var
  iAppName: string;
  iAppFolder: string;
  iRootFolder: string;
begin

  ASGScreenCapture1.CaptureCount := 0;

  // Setup the HelpFile
  iAppName := Application.ExeName;
  iAppFolder := ExtractFileDir ( iAppName );
  iRootFolder := GetFirstFolderFromPath ( iAppFolder );
  Application.HelpFile := iRootFolder + '\Apprehend\Help\Apprehend.chm';

end;

procedure TForm1.Active1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureActiveWindow;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';
  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Rectangle1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureSelection;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Reset1Click ( Sender: TObject );
begin

  Image1.Picture := nil;
  ASGScreenCapture1.ResetCaptureCount;
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Object1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureObject;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Polygon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CapturePolygon;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Save1Click ( Sender: TObject );
begin

  if Image1.Picture.Graphic <> nil then
  begin

    SavePictureDialog1.Filename := ExtractFileName ( FFilePath );
    SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );
    SavePictureDialog1.FilterIndex := 2; // *.bmp
    if SavePictureDialog1.Execute then
    begin

      FFilePath := SavePictureDialog1.FileName;
      Image1.Picture.SaveToFile ( FFilePath );
      Caption := 'Simple Apprehend Demo- ' + FFilePath;

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

  iBitmap := ASGScreenCapture1.CaptureSmallIcon;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.Icon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureIcon;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

procedure TForm1.LargeIcon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iFileName: string;
  iExtension: string;
begin

  iBitmap := ASGScreenCapture1.CaptureLargeIcon;
  try

    Image1.Picture.Assign ( iBitmap );

  finally
    iBitmap.Free;
  end;

  with ScrollBox1 do
  begin
    HorzScrollBar.range := Image1.Picture.Width;
    VertScrollBar.range := Image1.Picture.Height;
  end;

  iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  iExtension := '.bmp';

  FFilePath := iFileName + iExtension;
  Caption := 'Simple Demo - ' + FFilePath;
  // The CaptureCount is automatically inc with each sucessive capture
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );

end;

end.

