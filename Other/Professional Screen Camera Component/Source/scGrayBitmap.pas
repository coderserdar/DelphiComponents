{ *********************************************************************** }
{                                                                         }
{ GrayBitmap                                                              }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (mail@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit scGrayBitmap;

interface

uses
  Windows, SysUtils, Classes, Graphics;

type
  TGrayBitmap = class(TBitmap)
  public
    constructor Create; override;
    procedure UpdatePalette; virtual;
    procedure DeletePalette; virtual;
  end;

implementation

{ TGrayBitmap }

constructor TGrayBitmap.Create;
begin
  inherited;
  UpdatePalette;
end;

procedure TGrayBitmap.DeletePalette;
begin
  Palette := 0;
end;

procedure TGrayBitmap.UpdatePalette;
var
  Pal: PLogPalette;
  I: Integer;
begin
  PixelFormat := pf8bit;
  DeletePalette;
  GetMem(Pal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * MaxByte);
  try
    Pal^.palVersion := $300;
    Pal^.palNumEntries := MaxByte + 1;
    for I := 0 to MaxByte do
      with Pal^.palPalEntry[I] do
      begin
        peRed := I;
        peGreen := I;
        peBlue := I;
      end;
    Palette := CreatePalette(Pal^);
  finally
    FreeMem(Pal);
  end;
end;

end.
