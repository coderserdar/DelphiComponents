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
// $Log:  26855: uCreateThumbnail.pas 
//
//    Rev 1.0    06-12-2005 19:38:02  mcm
unit uCreateThumbnail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs,
  StdCtrls, mcmImage, mcmImageResStr, mcmFileDialogues;

type
  TForm1 = class(TForm)
    mcmImageCtrl: TmcmImageCtrl;
    mcmImageThumb: TmcmImageCtrl;
    OpenDialog: TmcmOpenDialog;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    OpenItem: TMenuItem;
    N1: TMenuItem;
    ExitItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CreateThumbnail;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses mcmImageFile, mcmImageTypeDef;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  //ImageFileManager.RegisterFileFormat(TmcmICONImage, FF_ICO, 'ico', resWIcon, True, False);
  ImageFileManager.YCbCrMode := JYCC_AUTO;
 // OpenDialog.Filter := ImageFileManager.GetReadFilter;
  OpenDialog.FilterIndex := 13;

  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog .
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  OpenDialog.ImageFileManager := ImageFileManager;
end;

procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end;

procedure TForm1.OpenItemClick(Sender : TObject);
begin
  if (OpenDialog.Execute)
  then begin
       mcmImageCtrl.Image.FileOpen(OpenDialog.FileName);
       CreateThumbnail;
  end;
end;

procedure TForm1.CreateThumbnail;
var Scale : double;
    Ratio : double;
begin
  mcmImageThumb.Clear;
  // Set initial size to 64 x 64 pixels.
  mcmImageThumb.Image.Height := 64;
  mcmImageThumb.Image.Width := 64;
  // Retain the original image color format.
  mcmImageThumb.Image.ImageFormat := mcmImageCtrl.Image.ImageFormat;
  // alternatively, set ImageFormat to IF_RGB24.
  mcmImageThumb.Image.ImageFormat := IF_RGB24;

  // Copy the palette.
  mcmImageThumb.Image.Palette := mcmImageCtrl.Image.Palette;

  // Calculate the scale factor for Original image to thumbnail, and adjust
  // thumbnail size to maintain aspect ratio.
  if (mcmImageCtrl.Image.Width > mcmImageCtrl.Image.Height)
  then begin
       Scale := mcmImageThumb.Image.Width / mcmImageCtrl.Image.Width;
       Ratio := mcmImageCtrl.Image.Height / mcmImageCtrl.Image.Width;
       mcmImageThumb.Image.Height := Round(Ratio * mcmImageThumb.Image.Height);
  end
  else begin
       Scale := mcmImageThumb.Image.Height / mcmImageCtrl.Image.Height;
       Ratio := mcmImageCtrl.Image.Width / mcmImageCtrl.Image.Height;
       mcmImageThumb.Image.Width := Round(Ratio * mcmImageThumb.Image.Width);
  end;

  // A little trick to improve thumb quality.
  // Requires that mcmImageThumb.Image.ImageFormat := IF_RGB24;
  if (mcmImageCtrl.Image.ImageFormat <> IF_GREY8)
  then mcmImageCtrl.Image.SetStretchMode(HALFTONE)
  else mcmImageCtrl.Image.SetStretchMode(COLORONCOLOR);

  // Fill thumbs background - only interesting if aspect ration isn't maintained.
  mcmImageThumb.Image.FillRGB(RGB(255, 255, 255));
  // Now paint the original image onto the thumbnails canvas.
  mcmImageCtrl.Image.Draw(mcmImageThumb.Image.Canvas.Handle, Scale);
end; // TForm1.CreateThumbnail.

end.
