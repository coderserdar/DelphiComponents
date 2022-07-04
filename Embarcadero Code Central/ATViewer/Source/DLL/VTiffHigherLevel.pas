unit VTiffHigherLevel;

interface

uses
  Windows, SysUtils, Classes, Graphics, LibTiffDelphi;

{uses LibTiffDelphi, download it here:
     http://www.awaresystems.be/imaging/tiff/delphi.html }

function ReadTIFFIntoBitmap(
  S: TStream; B: TBitmap;
  Page: Integer; var PagesCount: Integer): Boolean;


implementation

procedure TIFFReadRGBAImageSwapRB(Width,Height: Cardinal; Memory: Pointer);
{$IFDEF DELPHI_5}
type
  PCardinal = ^Cardinal;
{$ENDIF}
var
  m: PCardinal;
  n: Cardinal;
  o: Cardinal;
begin
  m:=Memory;
  for n:=0 to Width*Height-1 do
  begin
    o:=m^;
    m^:= (o and $FF00FF00) or                {G and A}
        ((o and $00FF0000) shr 16) or        {B}
        ((o and $000000FF) shl 16);          {R}
    Inc(m);
  end;
end;

function ReadTIFFIntoBitmap(S: TStream; B: TBitmap; Page: Integer; var PagesCount: Integer): Boolean;
var
  OpenTiff: PTIFF;
  FirstPageWidth,FirstPageHeight: Cardinal;
begin
  Result := False;
  PagesCount := 1;

  OpenTiff := TIFFOpenStream(S,'r');
  if OpenTiff = nil then Exit; //raise Exception.Create('Cannot open TIFF stream');

  PagesCount := TIFFNumberOfDirectories(OpenTiff);
  TIFFSetDirectory(OpenTiff, Page);
  
  TIFFGetField(OpenTiff,TIFFTAG_IMAGEWIDTH,@FirstPageWidth);
  TIFFGetField(OpenTiff,TIFFTAG_IMAGELENGTH,@FirstPageHeight);
  try
    B.PixelFormat := pf32bit;
    B.Width := FirstPageWidth;
    B.Height := FirstPageHeight;
  except
    TIFFClose(OpenTiff);
    Exit; //raise Exception.Create('Cannot create bitmap buffer');
  end;
  TIFFReadRGBAImage(OpenTiff,FirstPageWidth,FirstPageHeight,
               B.Scanline[FirstPageHeight-1],0);
  TIFFClose(OpenTiff);
  TIFFReadRGBAImageSwapRB(FirstPageWidth,FirstPageHeight,
               B.Scanline[FirstPageHeight-1]);
  Result := True;
end;


end.
