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
// $Log:  25958: uFormDragDrop2TmcmImage.pas 
//
//   Rev 1.1    23-08-2005 21:53:36  mcm
// Modified to use UnregisterFileFormat.

//
//   Rev 1.0    24-07-2005 18:50:28  mcm    Version: IMG 2.9
// Drag & Drop sample project.

//
//   Rev 1.0    13-02-2005 19:48:06  mcm    Version: IMG 2.8

unit uFormDragDrop2TmcmImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  mcmImage, Menus, mcmFileDialogues, mcmDragDrop;

type
  TForm1 = class(TForm)
    mcmImageCtrl  : TmcmImageCtrl;
    mcmDropTarget : TmcmDropTarget;
    mcmDropSource : TmcmDropSource;
    OpenDialog    : TmcmOpenDialog;
    SaveDialog    : TmcmSaveDialog;
    MainMenu1     : TMainMenu;
    FileMenu      : TMenuItem;
    OpenItem      : TMenuItem;
    SaveItem      : TMenuItem;
    N1            : TMenuItem;
    ExitItem      : TMenuItem;
    EditMenu      : TMenuItem;
    CopyItem      : TMenuItem;
    PasteItem     : TMenuItem;
    procedure FormCreate(Sender : TObject);
    procedure OpenItemClick(Sender : TObject);
    procedure SaveItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure EditMenuClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
    procedure mcmDropTargetDragOver(    Sender, Source : TObject;
                                        X, Y           : Integer;
                                        State          : TDragState;
                                    var Accept         : Boolean);
    procedure mcmDropTargetDragDrop(Sender, Source : TObject;
                                    X, Y           : Integer);
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

uses ClipBrd, mcmImageFile, mcmImageTypeDef;

{$R *.DFM}

procedure TForm1.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;
  OpenDialog.Filter := ImageFileManager.GetReadFilter;
  OpenDialog.FilterIndex := 0;

  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  OpenDialog.ImageFileManager := ImageFileManager;
  SaveDialog.ImageFileManager := ImageFileManager;

  FAcceptDragObj := False;
end; // TForm1.FormCreate.


procedure TForm1.OpenItemClick(Sender : TObject);
begin
  if (OpenDialog.Execute)
  then begin
       mcmImageCtrl.Image.FileOpen(OpenDialog.FileName);
  end;
end; // TForm1.OpenItemClick.


procedure TForm1.SaveItemClick(Sender : TObject);
begin
  SaveDialog.Image       := mcmImageCtrl.Image;
  SaveDialog.FileName    := mcmImageCtrl.Image.ImageInfo.FileName;
  SaveDialog.Compression := mcmImageCtrl.Image.Compression;
  SaveDialog.Quality     := mcmImageCtrl.Image.Quality;
  SaveDialog.Interlaced  := mcmImageCtrl.Image.Interlaced;
  if (SaveDialog.Execute)
  then begin
       // Set compression matching the file extension CP_xxx ref. TmcmCompress.
       mcmImageCtrl.Image.Compression := SaveDialog.Compression;
       // Set Quality, ref. TmcmImage/TmcmImageFileMgr.Quality
       mcmImageCtrl.Image.Quality     := SaveDialog.Quality;
       // Set Interlaced mode, ref. TmcmImage/TmcmImageFileMgr.Interlaced
       mcmImageCtrl.Image.Interlaced  := SaveDialog.Interlaced;
       // Save image to disk
       mcmImageCtrl.Image.FileSave(SaveDialog.FileName);
  end;
end; // TForm1.SaveItemClick.


procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TForm1.ExitItemClick.


procedure TForm1.EditMenuClick(Sender : TObject);
begin
  CopyItem.Enabled := Not(mcmImageCtrl.Image.Empty);
  PasteItem.Enabled := Clipboard.HasFormat(CF_DIB);
end; // TForm1.EditMenuClick.


procedure TForm1.CopyItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  AFormat := CF_DIB;
  mcmImageCtrl.Image.SaveToClipboardFormat(AFormat, AData);
  if (AData <> 0)
  then begin
       Clipboard.Open;
       Clipboard.SetAsHandle(AFormat, AData);
       Clipboard.Close;
  end;
end; // TForm1.CopyItemClick.


procedure TForm1.PasteItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_DIB; // NOTE: CF_BITMAP is not supported yet.
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then mcmImageCtrl.Image.LoadFromClipboardFormat(AFormat, AData);
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
  Accept := False;

  // If the object being dragged is a TmcmDropEnum class, we're receiving data
  // the Windows way, i.e. from another applications (Explorer, Word ect).
  if (Source is TmcmDropEnum) and Not(mcmDropSource.Dragging)
  then begin
       if (State = dsDragEnter)
       then begin
            FAcceptDragObj := False;

            // Check if we support the dragged object.
            if (Source as TmcmDropEnum).HasClipFormat(CF_DIB) or
               (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
            then FAcceptDragObj := True;

            if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
            then FAcceptDragObj := True;
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
var Image     : TmcmImage;
    i         : integer;
    FileNames : TStringList;
begin
  if (Source is TmcmDropEnum)
  then begin
       // The object has been dropped on our applications main form.
       Application.BringToFront; // Move this application window to the top.

       if (Source as TmcmDropEnum).HasClipFormat(CF_DIB) or
          (Source as TmcmDropEnum).HasClipFormat(CF_BITMAP)
       then begin
            Image := TmcmImage.Create;
            (Source as TmcmDropEnum).GetAsImage(Image);
            if Not(Image.Empty)
            then begin
                 mcmImageCtrl.Image := Image;
            end
            else Image.Free;
       end
       else if (Source as TmcmDropEnum).HasClipFormat(CF_HDROP)
            then begin
                 FileNames := TStringList.Create;
                 (Source as TmcmDropEnum).GetAsHDrop(FileNames);
                 if (FileNames.Count > 0)
                 then begin
                      for i := 0 to (FileNames.Count - 1)
                      do mcmImageCtrl.Image.FileOpen(FileNames.Strings[i]);
                 end;
                 FileNames.Free;
            end;
  end;

  // Add your native Delphi Drop handling here...
  // ...

end; // TForm1.mcmDropTargetDragDrop.


procedure TForm1.mcmDropSourcePreStartDrag(    Sender    : TObject;
                                               X, Y      : Integer;
                                           var Component : TPersistent);
begin
  Component := mcmImageCtrl;
end; // TForm1.mcmDropSourcePreStartDrag.

end.
