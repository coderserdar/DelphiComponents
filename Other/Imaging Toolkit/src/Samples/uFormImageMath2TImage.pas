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
// $Log:  19197: uFormImageMath2TImage.pas 
//
//   Rev 1.0    10-02-2003 18:46:52  mcm    Version: IMG 1.3

unit uFormImageMath2TImage;

//------------------------------------------------------------------------------
// This example shows how to use the TmcmImageMath component together with
// images maintained by TImage (TBitmap).
//
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1    : TImage;
    Image2    : TImage;
    Image3    : TImage;
    BlendBtn  : TButton;
    Label1    : TLabel;
    Label2    : TLabel;
    Label3    : TLabel;
    Label4    : TLabel;
    procedure BlendBtnClick(Sender : TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

{$R *.DFM}

uses mcmImage, mcmImageMath, mcmImageTypeDef;


procedure TForm1.BlendBtnClick(Sender : TObject);
var i          : integer;
    ImageMath  : TmcmImageMath;
    UpdateRect : TRect;
begin
  // Get area occupied by Image3.
  UpdateRect := Image3.BoundsRect;

  // Assign source images to the TmcmImageMath instance.
  ImageMath := TmcmImageMath.Create(Self);
  ImageMath.SourceImage[0] := TmcmImage.Create;
  ImageMath.SourceImage[0].DibHandle := Image1.Picture.Bitmap.Handle;
  ImageMath.SourceImage[1] := TmcmImage.Create;
  ImageMath.SourceImage[1].DibHandle := Image2.Picture.Bitmap.Handle;
  ImageMath.ResultImage    := TmcmImage.Create;

  i := 0;
  while (i <= 100)
  do begin
     ImageMath.Scale[0] := i;
     ImageMath.Scale[1] := 100 - i;
     if (ImageMath.Execute(IM_BLEND) <> Nil)
     then begin
          Image3.Picture.Bitmap.Handle := ImageMath.ResultImage.DibHandle;
          InvalidateRect(Handle, @UpdateRect, False);
          Update;
          Label4.Caption := 'Step: ' + IntToStr(i);
     end
     else begin
          Label4.Caption := CErrorStrings[integer(ImageMath.Error)];
          i := 100;
     end;
     inc(i);
  end;

  // Free memory.
  ImageMath.SourceImage[0].ReleaseHandle; // Ensure that bitmap handle does not get freed!
  ImageMath.SourceImage[0].Free;
  ImageMath.SourceImage[1].ReleaseHandle; // Ensure that bitmap handle does not get freed!
  ImageMath.SourceImage[1].Free;
  ImageMath.ResultImage.ReleaseHandle; // Ensure that bitmap handle does not get freed!
  ImageMath.ResultImage.Free;
end;

end.
