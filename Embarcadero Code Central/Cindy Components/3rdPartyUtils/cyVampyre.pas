{   Unit cyImageEn3

    Description:
    Unit with functions to use with Vampyre components.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyVampyre;

interface

uses System.Classes, FMX.Types, Imaging, ImagingClasses, ImagingExtras;

implementation

// *** VAMPYRE CORE *** //

// Win32 ok ...
procedure LoadImagesFromFile(aImageFile: String; var Images: TDynImageDataArray); overload;
var
  MultiPageStream: TMemoryStream;
begin
  MultiPageStream := TMemoryStream.Create;
  MultiPageStream.LoadFromFile(aImageFile);
  LoadMultiImageFromStream(MultiPageStream, Images);
  MultiPageStream.Free;
end;

// Win32 ok ...
procedure LoadImagesFromFile(aImageFile: String; var MultiImage: TMultiImage); overload;
var
  MultiPageStream: TMemoryStream;
begin
  MultiPageStream := TMemoryStream.Create;
  MultiPageStream.LoadFromFile(aImageFile);
  MultiImage.LoadMultiFromStream(MultiPageStream);
  MultiPageStream.Free;
end;

// win32 ok ...
procedure SavePageToFileTIF(aTIFFile: String; var Images: TDynImageDataArray; PageIndex: Integer; Dpis: Single); overload;
var
  SinglePageStream: TMemoryStream;
begin
  SinglePageStream := TMemoryStream.Create;
  GlobalMetadata.SetPhysicalPixelSize(ruDpi, Dpis, Dpis, True);
  SaveImageToStream('tif', SinglePageStream, Images[PageIndex]);

  SinglePageStream.SaveToFile(aTIFFile);
  SinglePageStream.Free;
end;

// win32 ok ...
procedure SaveImagesToFileTIF(aTIFFile: String; var Images: TDynImageDataArray; Dpis: Single); overload;
var
  MultiPageStream: TMemoryStream;
begin
  MultiPageStream := TMemoryStream.Create;
  GlobalMetadata.SetPhysicalPixelSize(ruDpi, Dpis, Dpis, True);
  SaveMultiImageToStream('tif', MultiPageStream, Images);

  MultiPageStream.SaveToFile(aTIFFile);
  MultiPageStream.Free;
end;

// win32 ok ...
procedure SaveImagesToFileTIF(aImageFile: String; var MultiImage: TMultiImage; Dpis: Single); overload;
var
  MultiPageStream: TMemoryStream;
  XRes, YRes: Single;
  ResUnit: TResolutionUnit;
begin
  MultiPageStream := TMemoryStream.Create;
  GlobalMetadata.SetPhysicalPixelSize(ruDpi, Dpis, Dpis, True);
  MultiImage.SaveMultiToStream('tif', MultiPageStream);

  MultiPageStream.SaveToFile(aImageFile);
  MultiPageStream.Free;
end;

// win32 ok ...
function GetPageCount(const Images: TDynImageDataArray): integer;
begin
  Result := Length(Images);
end;

// win32 ok ...
function GetPageCountFromFile(aImageFile: String): Integer;
var
  Images: TDynImageDataArray;
begin
  LoadImagesFromFile(aImageFile, Images);
  Result := GetPageCount(Images);
end;






// *** VAMPYRE UTILS *** //

// win32 ok ...
// !!! Não funciona se utilizamos GlobalMetadata.SetPhysicalPixelSize() antes de chamar esta janela
procedure BitmapLoadPageTIF(Destination: TBitmap; const Images: TDynImageDataArray; PageIndex: Integer);
var
  SinglePageStream: TStream;
begin
  SinglePageStream := TMemoryStream.Create;
  SaveImageToStream('tif', SinglePageStream, Images[PageIndex]);
  Destination.LoadFromStream(SinglePageStream);
  SinglePageStream.Free;
end;

// win32 Ok ...
procedure BitmapLoadPageFromFileTIF(Destination: TBitmap; aTIFFile: String; PageIndex: Integer);
var
  Images: TDynImageDataArray;
begin
  LoadImagesFromFile(aTIFFile, Images);
  BitmapLoadPageTIF(Destination, Images, PageIndex);
end;

end.
