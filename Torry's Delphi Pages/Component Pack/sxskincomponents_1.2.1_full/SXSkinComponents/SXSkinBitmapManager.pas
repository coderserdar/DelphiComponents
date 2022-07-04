unit SXSkinBitmapManager;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Windows, Classes, SXSkinLibrary, GR32, SysUtils, GR32_Resamplers,
     SXZipUtils;

type

 TSXSkinPreloadedBitmap=class
  public
   Filter:TSXFilterData;
   Resized:Boolean;
   StretchFilter:TSXImageStretchFilter;
   Width:Integer;
   Height:Integer;
   FilePath:String;
   Bitmap:TBitmap32;
   SkinLibrary:TSXSkinLibrary;
   constructor Create;
   destructor Destroy; override;
 end;

 TSXSkinPreloadedBitmapList=class
  protected
   FItem:TList;
   function Get(const Index:Integer):TSXSkinPreloadedBitmap;
   procedure Put(const Index:Integer;const Item:TSXSkinPreloadedBitmap);
   function GetCount:Integer;
  public
   procedure Add(Bitmap:TSXSkinPreloadedBitmap);
   function GetBitmapIndex(Bitmap:TSXSkinPreloadedBitmap):Integer;
   procedure Delete(const Index:Integer);
   procedure Clear;
   constructor Create;
   destructor Destroy; override;
   property Item[const Index:Integer]:TSXSkinPreloadedBitmap read Get write Put; default;
   property Count:Integer read GetCount;
 end;

function GetPreloadedBitmap(SkinLibrary:TSXSkinLibrary;
           const FilePath,ZipFilePath:String;Resized:Boolean;Width:Integer;
           Height:Integer;StretchFilter:TSXImageStretchFilter;
           Filter:TSXFilterData):TBitmap32;
procedure DeletePreloadedBitmaps(SkinLibrary:TSXSkinCustomLibrary);

var PreloadedBitmaps:TSXSkinPreloadedBitmapList;

implementation

uses Math, jpeg, SXPNGUtils;

function CompareBitmaps(Bitmap1,Bitmap2:TSXSkinPreloadedBitmap):Integer;
begin
 Result:=CompareValue(Integer(Bitmap1.SkinLibrary),Integer(Bitmap2.SkinLibrary));
 if Result=0 then
  Result:=CompareStr(Bitmap1.FilePath,Bitmap2.FilePath) else exit;
 if Result=0 then
  Result:=CompareValue(Ord(Bitmap1.Resized),Ord(Bitmap2.Resized)) else exit;
 if Result<>0 then exit;
 if Bitmap1.Resized then
  begin
   Result:=CompareValue(Ord(Bitmap1.Width),Ord(Bitmap2.Width));
   if Result=0 then
    Result:=CompareValue(Ord(Bitmap1.Height),Ord(Bitmap2.Height)) else exit;
   if Result=0 then
    Result:=CompareValue(Ord(Bitmap1.StretchFilter),Ord(Bitmap2.StretchFilter)) else exit;
   if Result<>0 then exit;
  end;
 if Result=0 then
  Result:=CompareValue(Ord(Bitmap1.Filter.AType),Ord(Bitmap2.Filter.AType)) else exit;
 if Result=0 then
  Result:=CompareValue(Ord(Bitmap1.Filter.Value),Ord(Bitmap2.Filter.Value)) else exit;
 if Result=0 then
  Result:=CompareValue(Ord(Bitmap1.Filter.Color),Ord(Bitmap2.Filter.Color)) else exit;
 if Result=0 then
  Result:=CompareValue(Ord(Bitmap1.Filter.Color2),Ord(Bitmap2.Filter.Color2)) else exit;
end;

{ TSXSkinPreloadedBitmap }

constructor TSXSkinPreloadedBitmap.Create;
begin
 inherited;
end;

destructor TSXSkinPreloadedBitmap.Destroy;
begin
 Bitmap.Free;
 inherited;
end;

{ TSXSkinPreloadedBitmapList }

function TSXSkinPreloadedBitmapList.Get(const Index:Integer):TSXSkinPreloadedBitmap;
begin
 Result:=TSXSkinPreloadedBitmap(FItem[Index]);
end;

procedure TSXSkinPreloadedBitmapList.Put(const Index:Integer;const Item:TSXSkinPreloadedBitmap);
begin
 FItem[Index]:=Item;
end;

function TSXSkinPreloadedBitmapList.GetCount:Integer;
begin
 Result:=FItem.Count;
end;

procedure TSXSkinPreloadedBitmapList.Add(Bitmap:TSXSkinPreloadedBitmap);
var L,H,A,C:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   C:=CompareBitmaps(Bitmap,Item[A]);
   if C<0 then L:=A+1 else
    begin
     H:=A-1;
     if C=0 then L:=A;
    end;
  end;
 FItem.Insert(L,Bitmap);
end;

function TSXSkinPreloadedBitmapList.GetBitmapIndex(Bitmap:TSXSkinPreloadedBitmap):Integer;
var  A,C:Integer;
     L,H:Integer;
begin
 L:=0;
 H:=Count-1;
 while L<=H do
  begin
   A:=(L+H) shr 1;
   C:=CompareBitmaps(Bitmap,Item[A]);
   if C<0 then L:=A+1 else
    begin
     H:=A-1;
     if C=0 then L:=A;
    end;
  end;
 if (L>=0) and (L<Count) and (CompareBitmaps(Bitmap,Item[L])=0) then
  Result:=L else Result:=-1;
end;

procedure TSXSkinPreloadedBitmapList.Delete(const Index:Integer);
begin
 Item[Index].Free;
 FItem.Delete(Index);
end;

procedure TSXSkinPreloadedBitmapList.Clear;
var A:Integer;
begin
 for A:=0 to Count-1 do
  Item[A].Free;
 FItem.Clear;
end;

constructor TSXSkinPreloadedBitmapList.Create;
begin
 inherited Create;
 FItem:=TList.Create;
end;

destructor TSXSkinPreloadedBitmapList.Destroy;
begin
 Clear;
 FItem.Free;
 inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////

function GetPreloadedBitmap(SkinLibrary:TSXSkinLibrary;
           const FilePath,ZipFilePath:String;Resized:Boolean;Width:Integer;
           Height:Integer;StretchFilter:TSXImageStretchFilter;
           Filter:TSXFilterData):TBitmap32;
var  A,W,H:Integer;
         B:TSXSkinPreloadedBitmap;
        B2:TBitmap32; 
       JPG:TJpegImage;
       PNG:TPNGObject;
        MS:TMemoryStream;
 ImageType:TSXSkinStyleImageType;
   ZipFile:TZipFile;
begin
 B:=TSXSkinPreloadedBitmap.Create;
 B.SkinLibrary:=SkinLibrary;
 B.FilePath:=FilePath;
 B.Resized:=Resized;
 B.Width:=Width;
 B.Height:=Height;
 B.StretchFilter:=StretchFilter;
 B.Filter:=Filter;
 A:=PreloadedBitmaps.GetBitmapIndex(B);
 if A>=0 then
  begin
   Result:=PreloadedBitmaps[A].Bitmap;
   B.Free;
   exit;
  end;
 if Filter.AType<>iftNone then
  begin
   B.Filter.AType:=iftNone;
   B.Filter.Value:=0;
   B.Filter.Color:=0;
   B.Filter.Color2:=0;
   A:=PreloadedBitmaps.GetBitmapIndex(B);
   if A>=0 then
    begin
     B.Filter:=Filter;
     B.Bitmap:=TBitmap32.Create;
     B.Bitmap.Assign(PreloadedBitmaps[A].Bitmap);
     ApplyFilterToBitmap(B.Bitmap,Filter);
     Result:=B.Bitmap;
     PreloadedBitmaps.Add(B);
     exit;
    end;
  end;
 if Resized then
  begin
   B.Resized:=False;
   B.Width:=0; B.Height:=0;
   A:=PreloadedBitmaps.GetBitmapIndex(B);
   if A>=0 then
    begin
     B.Filter:=Filter;
     B.Resized:=True;
     B.Width:=Width; B.Height:=Height;
     W:=Width; H:=Height;
     if W=0 then W:=PreloadedBitmaps[A].Bitmap.Width;
     if H=0 then H:=PreloadedBitmaps[A].Bitmap.Height;
     B.Bitmap:=TBitmap32.Create;
     B.Bitmap.SetSize(W,H);
     B.Bitmap.Clear(0);
     B.Bitmap.DrawMode:=dmBlend;
     B.Bitmap.CombineMode:=cmMerge;
     case StretchFilter of
      isfLinear:   TLinearResampler.Create(PreloadedBitmaps[A].Bitmap);
      else         begin
                    TKernelResampler.Create(PreloadedBitmaps[A].Bitmap);
                    case StretchFilter of
                     isfSpline:   TKernelResampler(PreloadedBitmaps[A].Bitmap.Resampler).
                                   Kernel:=TSplineKernel.Create;
                     isfLanczos:  TKernelResampler(PreloadedBitmaps[A].Bitmap.Resampler).
                                   Kernel:=TLanczosKernel.Create;
                     isfMitchell: TKernelResampler(PreloadedBitmaps[A].Bitmap.Resampler).
                                   Kernel:=TMitchellKernel.Create;
                    end;
                   end;
     end;
     PreloadedBitmaps[A].Bitmap.DrawTo(B.Bitmap,Rect(0,0,W,H));
     TNearestResampler.Create(PreloadedBitmaps[A].Bitmap);
     ApplyFilterToBitmap(B.Bitmap,Filter);
     Result:=B.Bitmap;
     PreloadedBitmaps.Add(B);
     exit;
    end;
  end;
 B.Filter:=Filter;
 B.Resized:=Resized;
 B.Width:=Width;
 B.Height:=Height;
 //
 if SameText(ExtractFileExt(FilePath),'.png') then
  ImageType:=ssitPNG else ImageType:=ssitJPEG;
 case ImageType of
  ssitJPEG: begin
             JPG:=TJpegImage.Create;
             try
              try
               if ZipFilePath<>'' then
                begin
                 MS:=TMemoryStream.Create;
                 try
                  ZipFile:=GetPreloadedZipFile(SkinLibrary,ZipFilePath);
                  if ZipFile<>nil then
                   begin
                    ZipFile.WriteFileToStream(MS,FilePath);
                    MS.Seek(0,soFromBeginning);
                    if MS.Size>0 then
                     JPG.LoadFromStream(MS);
                   end;
                 finally
                  MS.Free;
                 end;
                end else
               if FileExists(FilePath) then
                JPG.LoadFromFile(FilePath);
              except
              end;
              B.Bitmap:=TBitmap32.Create;
              B.Bitmap.Assign(JPG);
             finally
              JPG.Free;
             end;
            end;
  ssitPNG:  begin
             PNG:=TPNGObject.Create;
             try
              try
               if ZipFilePath<>'' then
                begin
                 MS:=TMemoryStream.Create;
                 try
                  ZipFile:=GetPreloadedZipFile(SkinLibrary,ZipFilePath);
                  if ZipFile<>nil then
                   begin
                    ZipFile.WriteFileToStream(MS,FilePath);
                    MS.Seek(0,soFromBeginning);
                    if MS.Size>0 then
                     PNG.LoadFromStream(MS);
                   end;
                 finally
                  MS.Free;
                 end;
                end else
               if FileExists(FilePath) then
                PNG.LoadFromFile(FilePath);
              except
              end;
              B.Bitmap:=TBitmap32.Create;
              PNGObjectToBitmap32(PNG,B.Bitmap);
              if (PNG.Width>0) and (PNG.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
               begin
                B.Bitmap.DrawMode:=dmBlend;
                B.Bitmap.CombineMode:=cmMerge;
               end;
             finally
              PNG.Free;
             end;
            end;
 end;
 //
 if Resized then
  begin
   W:=Width; H:=Height;
   if W=0 then W:=B.Bitmap.Width;
   if H=0 then H:=B.Bitmap.Height;
   B2:=TBitmap32.Create;
   B2.SetSize(W,H);
   B2.Clear(0);
   B2.DrawMode:=dmBlend;
   B2.CombineMode:=cmMerge;
   case StretchFilter of
    isfLinear:   TLinearResampler.Create(B.Bitmap);
    else         begin
                  TKernelResampler.Create(B.Bitmap);
                  case StretchFilter of
                   isfSpline:   TKernelResampler(B.Bitmap.Resampler).
                                 Kernel:=TSplineKernel.Create;
                   isfLanczos:  TKernelResampler(B.Bitmap.Resampler).
                                 Kernel:=TLanczosKernel.Create;
                   isfMitchell: TKernelResampler(B.Bitmap.Resampler).
                                 Kernel:=TMitchellKernel.Create;
                  end;
                 end;
   end;
   B.Bitmap.DrawTo(B2,Rect(0,0,W,H));
   B.Bitmap.Free;
   B.Bitmap:=B2;
  end;
 ApplyFilterToBitmap(B.Bitmap,Filter);
 Result:=B.Bitmap;
 PreloadedBitmaps.Add(B);
end;

procedure DeletePreloadedBitmaps(SkinLibrary:TSXSkinCustomLibrary);
var A:Integer;
begin
 for A:=PreloadedBitmaps.Count-1 downto 0 do
  if PreloadedBitmaps[A].SkinLibrary=SkinLibrary then
   PreloadedBitmaps.Delete(A);
end;

initialization

 PreloadedBitmaps:=TSXSkinPreloadedBitmapList.Create;

finalization

 PreloadedBitmaps.Free;

end.
