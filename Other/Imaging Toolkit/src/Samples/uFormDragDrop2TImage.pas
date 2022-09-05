// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  25956: uFormDragDrop2TImage.pas 
//
//   Rev 1.0    24-07-2005 18:50:28  mcm    Version: IMG 2.9
// Drag & Drop sample project.

unit uFormDragDrop2TImage;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
     mcmImage,
     mcmImageFile,
     mcmImageTypeDef,
     mcmFileDialogues,
     mcmDragDrop;

type
  TForm1 = class(TForm)
    Image1        : TImage;
    MainMenu1     : TMainMenu;
    FileMenu      : TMenuItem;
    OpenItem      : TMenuItem;
    ExitItem      : TMenuItem;
    OpenDialog    : TOpenDialog;
    SaveAsItem    : TMenuItem;
    SaveDialog    : TSaveDialog;
    EditMenu      : TMenuItem;
    CopyItem      : TMenuItem;
    PasteItem     : TMenuItem;
    mcmDropTarget : TmcmDropTarget;
    mcmDropSource: TmcmDropSource;
    procedure OpenItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure SaveAsItemClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
    procedure mcmDropTargetDragOver(   Sender, Source : TObject;
                                       X, Y           : Integer;
                                       State          : TDragState;
                                    var Accept        : Boolean);
    procedure mcmDropTargetDragDrop(Sender, Source : TObject;
                                    X, Y           : Integer);
    procedure mcmDropSourceStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure mcmDropSourceEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure mcmDropSourcePreStartDrag(Sender: TObject; X, Y: Integer;
      var Component: TPersistent);
  private
    { Private declarations }
    FAcceptDragObj : boolean;
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

{$R *.DFM}

uses Clipbrd;

procedure TForm1.FormCreate(Sender : TObject);
var FilterStr : string;
begin
  // Specify which formats to support - if not all!
  // ImageFileManager.SetSupportedFormats([IF_TIFF, IF_BMP, IF_DIB]);
  FilterStr := ImageFileManager.GetReadFilter;
  OpenDialog.Filter := FilterStr;
  OpenDialog.FilterIndex := 15; // Max number of file formats in filter list!

  FilterStr := ImageFileManager.GetWriteFilter;
  SaveDialog.Filter := FilterStr;
  SaveDialog.FilterIndex := 9; // Let's set TIFF as default.

  FAcceptDragObj := False;
end; // TForm1.FormCreate.


procedure TForm1.OpenItemClick(Sender : TObject);
var FSaveCursor : TCursor;
begin
  if (OpenDialog.Execute)
  then begin
       FSaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;

       if (Image1.Picture.Bitmap = Nil)
       then Image1.Picture.Bitmap := TBitmap.Create;

       // Read image from disk

       // Use ImageFileManager to open file. This allows you to return the pointer to
       // a TBitmap.Handle.
       ImageFileManager.Quality := 100;
       ImageFileManager.Compression := CP_NOCOMP;
       Image1.Picture.Bitmap.Handle := ImageFileManager.LoadImage(OpenDialog.FileName, FF_DETECT);

       Screen.Cursor := FSaveCursor;

       if (ImageFileManager.Error <> EC_OK)
       then ShowMessage('Error reading image: ' + CErrorStrings[word(ImageFileManager.Error)]);
  end;
end; // TForm1.OpenItemClick.


procedure TForm1.SaveAsItemClick(Sender : TObject);
const
    ConvertFormat : array[1..14] of TmcmFileFormat =
                    (FF_CRI, FF_GIF, FF_JPEG, FF_PBM, FF_PGM, FF_PNG, FF_PPM,
                     FF_SGI, FF_TIFF, FF_TARGA, FF_BMP, FF_DIB, FF_ICO, FF_PCX);
var FSaveCursor : TCursor;
    ExtName     : string;
begin
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       if (SaveDialog.Execute)
       then begin
            SaveDialog.InitialDir := ExtractFileDir(SaveDialog.FileName);

            case ConvertFormat[SaveDialog.FilterIndex] of
            FF_CRI   : ExtName := '.cri';
            FF_GIF   : ExtName := '.gif';
            FF_JPEG  : ExtName := '.jpg';
            FF_PBM   : ExtName := '.pbm';
            FF_PGM   : ExtName := '.pgm';
            FF_PNG   : ExtName := '.png';
            FF_PPM   : ExtName := '.ppm';
            FF_SGI   : ExtName := '.rgb';
            FF_TIFF  : ExtName := '.tif';
            FF_TARGA : ExtName := '.tga';
            FF_BMP   : ExtName := '.bmp';
            FF_DIB   : ExtName := '.dib';
            FF_ICO   : ExtName := '.ico';
            FF_PCX   : ExtName := '.pcx';
            else ExtName := '.bmp';
            end;

            // Store last directory and file extension.
            SaveDialog.FileName := ChangeFileExt(SaveDialog.FileName, ExtName);

            FSaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;

            // Store image to disk
            ImageFileManager.Quality := 100;
            // If the format is TIFF, Compression could assigned CP_LZW below.
            ImageFileManager.Compression := CP_NOCOMP;
            ImageFileManager.SaveImage(SaveDialog.FileName,
                                       FF_DETECT,
                                       Image1.Picture.Bitmap.Handle);

            Screen.Cursor := FSaveCursor;

            if (ImageFileManager.Error <> EC_OK)
            then ShowMessage('Error writing image: ' + CErrorStrings[word(ImageFileManager.Error)]);
       end;

  end
  else ShowMessage('There is nothing to save !');
end;


procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TForm1.ExitItemClick.


procedure TForm1.CopyItemClick(Sender : TObject);
var AFormat  : word;
    AData    : THandle;
    APalette : HPALETTE;
begin
  if Assigned(Image1.Picture)
  then if Not(Image1.Picture.Bitmap.Empty)
       then begin
            Image1.Picture.SaveToClipboardFormat(AFormat, AData, APalette);
            if (AData <> 0)
            then begin
                 Clipboard.Open;
                 Clipboard.SetAsHandle(AFormat, AData);
                 if (APalette <> 0)
                 then Clipboard.SetAsHandle(CF_PALETTE, APalette);
                 Clipboard.Close;
            end;
       end;
end; // TForm1.CopyItemClick.


procedure TForm1.PasteItemClick(Sender : TObject);
var AFormat  : word;
    AData    : THandle;
    APalette : HPALETTE;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_PALETTE;
    APalette := Clipboard.GetAsHandle(AFormat);
    AFormat :=  CF_BITMAP;
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then begin
         Image1.Picture.LoadFromClipboardFormat(AFormat, AData, APalette);
    end;
  finally
    Clipboard.Close;
  end;
end; // TForm1.PasteItemClick.


procedure TForm1.mcmDropTargetDragOver(    Sender, Source : TObject;
                                           X, Y           : Integer;
                                           State          : TDragState;
                                       var Accept         : Boolean);
var APoint     : TPoint;
    ClientRect : TRect;
begin
  // Object is being dragged over our application form.
  Accept := False;

  // If the object being dragged is a TmcmDropEnum class, we're receiving data
  // the Windows way, i.e. from another applications (Explorer, Word ect).
  if (Source is TmcmDropEnum)
  then begin
       if (State = dsDragEnter)
       then begin
            FAcceptDragObj := False;

            // Check if we support the dragged object.
            if (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
            then FAcceptDragObj := True;

            if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
            then FAcceptDragObj := True;
            {
            then begin
                 FileNames := TStringList.Create;
                 (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                 if (FileNames.Count > 0)
                 then begin
                      i := 0;
                      while (i < FileNames.Count) and Not(FAcceptDragObj)
                      do FAcceptDragObj := ImageFileMgr.VerifyImageFile(FileNames.Strings[i]);
                 end;
                 FileNames.Free;
            end;
            }
       end;

       if FAcceptDragObj
       then begin
            // Test if we're in the drop zone (Client area of the main form).
            APoint := ScreenToClient(Point(X, Y));
            ClientRect := GetClientRect;
            // If you have placed a toolbar move the top.
            //  ClientRect.Top := ClientRect.Top + ToolBar.Height;
            // and likewise, move the bottom if a status bar is added.
            //  ClientRect.Bottom := ClientRect.Bottom - StatusBar.Height;

            if ((ClientRect.Left < APoint.x) and (APoint.x < ClientRect.Right) and
                (ClientRect.Top < APoint.y) and (APoint.y < ClientRect.Bottom))
            then Accept := True;
       end;

       Exit;
  end;

  // Add your native Delphi Drag over handling here...
  // ...

end; // TForm1.mcmDropTargetDragOver.


procedure TForm1.mcmDropTargetDragDrop(Sender, Source : TObject;
                                       X, Y           : Integer);
var i         : integer;
    FileNames : TStringList;
begin
  // Object has been dropped!
  if (Sender is TForm1)
  then begin
       if  (Source is TmcmDropEnum)
       then begin
            // The object has been dropped on our applications main form.
            Application.BringToFront; // Move this application window to the top.

            if (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
            then begin
                 (Source as TmcmDropEnum).GetAsBitmap(Image1.Picture.Bitmap);
            end
            else if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
                 then begin
                      FileNames := TStringList.Create;
                      (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                      if (FileNames.Count > 0)
                      then begin
                           for i := 0 to (FileNames.Count - 1)
                           do Image1.Picture.Bitmap.Handle := ImageFileManager.LoadImage(FileNames.Strings[i], FF_DETECT);
                      end;
                      FileNames.Free;
                 end;
       end;
  end;

  // Add your native Delphi Drop handling here...
  // ...

end; // TForm1.mcmDropTargetDragDrop.


procedure TForm1.mcmDropSourceStartDrag(Sender : TObject; var DragObject : TDragObject);
begin
 ;
end;

procedure TForm1.mcmDropSourceEndDrag(Sender, Target : TObject; X, Y : Integer);
begin
 ;
end;

procedure TForm1.mcmDropSourcePreStartDrag(Sender : TObject; X, Y : Integer; var Component : TPersistent);
begin
  // Set "Component" to the control you want to drag!
  Component := Image1;
end; // TForm1.mcmDropSourcePreStartDrag.

end.
