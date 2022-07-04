{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: ImageNames.pas,v 1.4 2002/05/27 13:50:04 mjs Exp $ }

unit ImageNames;

interface

uses
{$ifndef LINUX}
  Controls, Graphics, imglist,
{$else LINUX}
  QControls, QGraphics, QImgList,
{$endif LINUX}
  Classes;

procedure AddImagesToList(const ImageList : TCustomImageList; const Names : TStrings);
function AddImageToList(const ImageList : TCustomImageList; const Name : PChar) : Integer;
procedure LoadImageFromRes(const Image : TPicture; const Name : PChar);

const
  BMP_CUT = 'BMP_CUT';
  BMP_COPY = 'BMP_COPY';
  BMP_PASTE = 'BMP_PASTE';
  BMP_DELETE = 'BMP_DELETE';
  BMP_SAVE = 'BMP_SAVE';
  BMP_SAVEALL = 'BMP_SAVEALL';
  BMP_PRINT = 'BMP_PRINT';
  BMP_SORTED = 'BMP_SORTED';
  BMP_UNSORTED = 'BMP_UNSORTED';
  BMP_MARK = 'BMP_MARK';
  BMP_ACTIVEMARK = 'BMP_ACTIVEMARK';
  BMP_ARROW = 'BMP_ARROW';
  BMP_SMALLARROW = 'BMP_SMALLARROW';
  BMP_UPARROW = 'BMP_UPARROW';
  BMP_DOWNARROW = 'BMP_DOWNARROW';
  BMP_MARKROW = 'BMP_MARKROW';
  BMP_MARKCOL = 'BMP_MARKCOL';
  BMP_NEWROW = 'BMP_NEWROW';
  BMP_DELETEROW = 'BMP_DELETEROW';
  BMP_DUPLICATEROW = 'BMP_DUPLICATEROW';
  BMP_LIST = 'BMP_LIST';
  BMP_DETAIL = 'BMP_DETAIL';
  BMP_EDITDETAIL = 'BMP_EDITDETAIL';
  BMP_EXCELEDIT = 'BMP_EXCELEDIT';

implementation

uses
{$ifndef LINUX}
  Windows,
{$else LINUX}
{$endif LINUX}
  SysUtils;

procedure AddImagesToList(const ImageList : TCustomImageList; const Names : TStrings);
var
  iImg : Integer;
begin
  for iImg := 0 to Names.Count -1 do
    AddImageToList(ImageList, PChar(Names[iImg]));
end;

function AddImageToList(const ImageList : TCustomImageList; const Name : PChar) : Integer;
var
  TempImage : TPicture;
  transColor : TColor;
begin
  TempImage := TPicture.Create;
  try
    LoadImageFromRes(TempImage, Name);
{$ifndef LINUX}
    transColor := TempImage.Bitmap.Canvas.Pixels[0,0];
{$else LINUX}
    TempImage.Bitmap.TransparentMode := tmAuto;
    transColor := TempImage.Bitmap.TransparentColor;
{$endif LINUX}
     Result := ImageList.AddMasked(TempImage.Bitmap, transColor);
  finally
    TempImage.Free;
  end;
end;

procedure LoadImageFromRes(const Image : TPicture; const Name : PChar);
begin
{$ifndef LINUX}
  Image.Bitmap.Handle := LoadBitmap(HInstance, Name);
  if Image.Bitmap.Handle = 0 then
    raise Exception.Create('LoadImageFromRes: Requested image ''' + Name +
                                 ''' couldn''t be found!');
{$else LINUX}
//    raise Exception.Create('LoadImageFromRes: Feature not supported under Kylix!' );
{$endif LINUX}
end;

end.

