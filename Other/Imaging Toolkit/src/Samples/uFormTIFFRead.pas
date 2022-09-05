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
// $Log:  22608: uFormTIFFRead.pas 
//
//   Rev 1.1    19-07-2004 20:28:50  mcm    Version: IMG 2.5
// Changed displaying image index/numbers.

//
//   Rev 1.0    12-12-2003 19:57:26  mcm

unit uFormTIFFRead;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, mcmImage, mcmImageFile, mcmImageTypeDef;

type
  TForm1 = class(TForm)
    mcmImageCtrl1: TmcmImageCtrl;
    btnRead: TButton;
    OpenDialog1: TOpenDialog;
    ScrollBar1: TScrollBar;
    lNoImages: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
    TIFFImage : TmcmTIFFImage;
    Stream    : TFileStream;
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender : TObject);
begin
  ScrollBar1.Max := 0;
  TIFFImage := TmcmTIFFImage.Create;
  Stream    := Nil;
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  TIFFImage.Free;
  if Assigned(Stream)
  then Stream.Free;
end;

procedure TForm1.btnReadClick(Sender: TObject);
var FileName : string;
begin
  if OpenDialog1.Execute
  then begin
       FileName := OpenDialog1.FileName;
       if Assigned(Stream)
       then Stream.Free;
       Stream := Nil;
       ScrollBar1.Position := 0;
       Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
       if Assigned(Stream)
       then begin
            try
              // Load first image in the multiple paged TIFF files.
              TIFFImage.ImageIndex := 0;
              TIFFImage.LoadFromStream(Stream);
              mcmImageCtrl1.Image.DibHandle := TIFFImage.DibHandle;
              // Set number of images in the TIFF file to the scrollbar.
              ScrollBar1.Max := TIFFImage.NoImages - 1;
              lNoImages.Caption := 'Image: 1 of ' + IntToStr(TIFFImage.NoImages);
            finally
              // If the TIFF file only contains one image, then we'll close and
              // free the stream.
              if Not(TIFFImage.NoImages > 1)
              then begin
                   Stream.Free;
                   Stream := Nil;
              end;
              if (TIFFImage.Error <> EC_OK)
              then ShowMessage('Error reading file: ' + CErrorStrings[integer(TIFFImage.Error)]);
            end;
       end;
  end;
end;

procedure TForm1.ScrollBar1Change(Sender : TObject);
begin
  if Assigned(Stream)
  then begin
       // The stream position must be set to the position where the multi-TIFF
       // images starts. In this example that's at position 0.
       Stream.Position := 0;
       // Set the sub-image index that we wish to retreive.
       TIFFImage.ImageIndex := ScrollBar1.Position;
       // Load sub-image.
       TIFFImage.LoadFromStream(Stream);
       lNoImages.Caption := 'Image: ' + IntToStr(TIFFImage.ImageIndex + 1) +
                            ' of ' + IntToStr(TIFFImage.NoImages);
       mcmImageCtrl1.Image.DibHandle := TIFFImage.DibHandle;
  end;
end;

end.
