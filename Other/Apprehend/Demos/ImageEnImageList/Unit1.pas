(* ------------------------------------------------------------------------------
  Apprehend          : 6.0
  Copyright © 2012   : Copyright Adirondack Software & Graphics
  Last Modification  : 03-25-2012
  Source File        : Unit1.pas
  Compiler           : Delphi 2010
  Operating System   : Windows 7
  This file is copyright (C) W W Miller, 1986-2012.
  It may be used without restriction. This code distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
  ------------------------------------------------------------------------------ *)
unit Unit1;
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ASGCapture, IEView, ImageENView, HYIEUtils, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, ImgList, ExtDlgs;

type
  TForm1 = class ( TForm )
    Panel1: TPanel;
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
    ImageEnView1: TImageEnView;
    Index1: TLabel;
    ListView1: TListView;
    Label2: TLabel;
    UpDown1: TUpDown;
    ImageList1: TImageList;
    Help1: TButton;
    TaskDialog1: TTaskDialog;
    Save1: TButton;
    SavePictureDialog1: TSavePictureDialog;
    Crop1: TButton;
    Undo1: TButton;
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
    procedure FormClose ( Sender: TObject; var Action: TCloseAction );
    procedure ListView1SelectItem ( Sender: TObject; Item: TListItem; Selected: Boolean );
    procedure Help1Click ( Sender: TObject );
    procedure Save1Click ( Sender: TObject );
    procedure Crop1Click ( Sender: TObject );
    procedure Undo1Click(Sender: TObject);
  private
    { Private declarations }
    FFilePath: string;
    FIEImageList: TIEImageList;
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
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureSpecificSizeSelection;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 8;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Undo1Click(Sender: TObject);
var
  iListItem: TListItem;
begin

  ImageEnView1.Proc.Undo();
  ImageEnView1.Proc.ClearUndo;

  FIEImageList.Remove( FIEImageList.ImageCount - 1 );
  FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), FFilePath );

  iListItem := ListView1.Items.Item [ ListView1.ItemIndex ];
  iListItem.SubItems [ 1 ] := IntegerToString ( ImageEnView1.Bitmap.Width ) + ' x ' + IntegerToString (
    ImageEnView1.Bitmap.Height ) + ' pixels';
  Undo1.Enabled := ImageEnView1.Proc.CanUndo;

end;

procedure TForm1.ASGScreenCapture1BeforeCapture ( Sender: TObject );
begin

  ASGScreenCapture1.Auto := Auto1.Checked;
  ASGScreenCapture1.Minimize := Minimize1.Checked;
  ASGScreenCapture1.ShowCursor := ShowCursor1.Checked;
  ASGScreenCapture1.ShowHint := ShowHint1.Checked;
  ASGScreenCapture1.ShowInfoDialog := ShowInfoDialog1.Checked;
  ASGScreenCapture1.Delay := StrToIntDef ( CaptureDelay1.Text, 500 );

end;

procedure TForm1.Crop1Click ( Sender: TObject );
var
  iListItem: TListItem;
begin

  if ListView1.ItemIndex <> -1 then
  begin

    ImageEnView1.Proc.SaveUndo();
    ImageEnView1.Proc.CropSel ( );

    ListView1.Items.BeginUpdate;
    iListItem := ListView1.Items.Item [ ListView1.ItemIndex ];
    iListItem.SubItems [ 1 ] := IntegerToString ( ImageEnView1.Bitmap.Width ) + ' x ' + IntegerToString (
      ImageEnView1.Bitmap.Height ) + ' pixels';
    ListView1.Items.EndUpdate;

    ImageEnView1.DeSelect;
    Undo1.Enabled := ImageEnView1.Proc.CanUndo;

  end;

end;

procedure TForm1.Desktop1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureDesktop;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 0;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.FormClose ( Sender: TObject; var Action: TCloseAction );
begin

  // Free the multiframe imagelist
  if Assigned ( FIEImageList ) then
  begin

    FIEImageList.Clear;
    FIEImageList.Free;

  end;

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
  FIEImageList := TIEImageList.Create;
  ImageEnView1.Proc.AutoUndo := False;
  ImageEnView1.Proc.UndoLimit := 99;

end;

procedure TForm1.Help1Click ( Sender: TObject );
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

procedure TForm1.Active1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureActiveWindow;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 1;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;
      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Rectangle1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureSelection;
  try

    if Assigned ( iBitmap ) then
    begin
      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 2;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Reset1Click ( Sender: TObject );
begin

  ImageEnView1.Blank;
  FFilePath := '';
  ImageEnView1.IO.Params.FileName := '';
  ImageEnView1.Update;
  ListView1.Clear;
  FIEImageList.Clear;
  ASGScreenCapture1.ResetCaptureCount;
  CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
  Caption := 'Capture Images To A Memory List With ImageENList and Apprehend';

end;

procedure TForm1.Object1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureObject;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 3;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Polygon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CapturePolygon;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 4;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Save1Click ( Sender: TObject );
begin

  if ImageEnView1.IO.Params.FileName <> '' then
  begin

    SavePictureDialog1.Filename := ExtractFileName ( FFilePath );
    SavePictureDialog1.DefaultExt := GraphicExtension ( TBitmap );
    SavePictureDialog1.FilterIndex := 2; // *.bmp
    if SavePictureDialog1.Execute then
    begin

      FFilePath := SavePictureDialog1.FileName;
      ImageEnView1.IO.SaveToFile ( FFilePath );
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
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureSmallIcon;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 5;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.Icon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureIcon;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 6;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.LargeIcon1Click ( Sender: TObject );
var
  iBitmap: TBitmap;
  iPath: string;
  iFileName: string;
  iExtension: string;
  iListItem: TListItem;
begin

  iBitmap := ASGScreenCapture1.CaptureLargeIcon;
  try

    if Assigned ( iBitmap ) then
    begin

      iFileName := IncludeTrailingPathDelimiter ( DesktopFolder ) + 'Image ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      iExtension := '.bmp';
      iPath := iFileName + iExtension;
      FFilePath := iPath;
      ImageEnView1.IO.Params.FileName := iPath;
      ImageEnView1.IEBitmap.Assign ( iBitmap );
      ImageEnView1.IEBitmap.UpdateFromTBitmap;
      ImageEnView1.Update;
      FIEImageList.AppendImageRef ( TIEBitmap.Create ( ImageEnView1.IEBitmap ), iPath );

      iListItem := ListView1.Items.Add;
      iListItem.Caption := IntToStr ( ListView1.Items.Count );
      iListItem.ImageIndex := 7;
      iListItem.SubItems.Add ( iPath );
      iListItem.SubItems.Add ( IntegerToString ( iBitmap.Width ) + ' x ' + IntegerToString ( iBitmap.Height ) + ' pixels' );
      ListView1.ItemIndex := ListView1.Items.Count - 1;

      Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + iFileName + iExtension;
      // The CaptureCount is automatically inc with each sucessive capture
      CaptureCount1.Caption := 'Capture Count: ' + IntToStr ( ASGScreenCapture1.CaptureCount );
      Undo1.Enabled := ImageEnView1.Proc.UndoCount <> 0;

    end;

  finally
    iBitmap.Free;
  end;

end;

procedure TForm1.ListView1SelectItem ( Sender: TObject; Item: TListItem; Selected: Boolean );
begin

  if Selected then
  begin

    ImageENView1.IEBitmap.Assign ( FIEImageList.Image [ Item.Index ] );
    ImageENView1.Update;
    ImageEnView1.IEBitmap.UpdateFromTBitmap;
    Caption := 'Capture Images To A Memory List With ImageENList and Apprehend - ' + FIEImageList.Filename [ Item.Index ];
  end;

end;

end.

