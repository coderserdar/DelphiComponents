(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMBmp2Jpg;

interface
uses Classes, NMMCustomDataProcessors, Graphics, Jpeg;

type
 TNMMBmp2Jpg = class(TNMMImageProcessor)
 protected
   FStream: TStream;
   FBitmap: TBitmap;
   FJpeg: TJPEGImage;
   FBitmapPreprocessor: TNMMCustomBitmapProcessor;
   FCompressionQuality: TJPEGQualityRange;
 public
   constructor Create;
   destructor Destroy; override;
   procedure Bmp2Jpg( ADecodedBitmap: TBitmap; AEncodedImageStream: TStream ); overload; override;
   procedure Bmp2Jpg( ADecodedImageStream: TStream;
                     AEncodedImageStream: TStream ); overload; override;
   procedure Bmp2Jpg( AStream: TStream ); overload; override;
   procedure Jpg2Bmp( AEncodedImageStream: TStream; ADecodedBitmap: TBitmap ); overload; override;
   procedure Jpg2Bmp( AEncodedImageStream: TStream;
                     ADecodedImageStream: TStream ); overload; override;
   procedure Jpg2Bmp( AStream: TStream ); overload; override;
 end;

implementation
uses SysUtils, NMMBmpColorReducer;

constructor TNMMBmp2Jpg.Create;
begin
 inherited Create;
 FCompressionQuality:= 100;
 FJpeg:= TJPEGImage.Create;
 FBitmap:= TBitmap.Create;
 FBitmapPreprocessor:= TNMMBmpColorReducer.Create(pf24bit);
 FStream:= TMemoryStream.Create;
end;

destructor TNMMBmp2Jpg.Destroy;
begin
 FreeAndNil(FJpeg);
 FreeAndNil(FBitmap);
 FreeAndNil(FStream);
 if FBitmapPreprocessor<>nil then
   FreeAndNil(FBitmapPreprocessor);
 inherited;
end;

procedure TNMMBmp2Jpg.Bmp2Jpg( ADecodedBitmap: TBitmap; AEncodedImageStream: TStream );
begin
 FJPEG.Assign(ADecodedBitmap);
 FJPEG.CompressionQuality:= FCompressionQuality;
 FJPEG.Compress;
 AEncodedImageStream.Size:= 0;
 FJPEG.SaveToStream(AEncodedImageStream);
 AEncodedImageStream.Seek( 0, soFromBeginning );
end;

procedure TNMMBmp2Jpg.Bmp2Jpg( ADecodedImageStream: TStream; AEncodedImageStream: TStream );
begin
 FBitmap.FreeImage;
 ADecodedImageStream.Seek( 0, soFromBeginning );
 FBitmap.LoadFromStream(ADecodedImageStream);
 ADecodedImageStream.Seek( 0, soFromBeginning );

 Bmp2Jpg(FBitmap,AEncodedImageStream);

 FBitmap.FreeImage;
end;

procedure TNMMBmp2Jpg.Bmp2Jpg( AStream: TStream );
begin
 Bmp2Jpg(AStream,FStream);
 FStream.Seek(0, soFromBeginning);
 AStream.Size:= 0;
 AStream.CopyFrom(FStream,FStream.Size);
 AStream.Seek(0, soFromBeginning);
end;

procedure TNMMBmp2Jpg.Jpg2Bmp( AEncodedImageStream: TStream; ADecodedBitmap: TBitmap );
begin

 AEncodedImageStream.Seek( 0, soFromBeginning );
 FJPEG.LoadFromStream(AEncodedImageStream);
 AEncodedImageStream.Seek( 0, soFromBeginning );
 ADecodedBitmap.Assign(FJPEG);
 if FBitmapPreprocessor<>nil then
   FBitmapPreprocessor.Process(ADecodedBitmap);
end;

procedure TNMMBmp2Jpg.Jpg2Bmp( AEncodedImageStream: TStream; ADecodedImageStream: TStream );
begin
 FBitmap.FreeImage;
 Jpg2Bmp(AEncodedImageStream,FBitmap);
 ADecodedImageStream.Size:= 0;
 FBitmap.SaveToStream(ADecodedImageStream);
 ADecodedImageStream.Seek( 0, soFromBeginning );
 FBitmap.FreeImage;
end;

procedure TNMMBmp2Jpg.Jpg2Bmp( AStream: TStream );
begin
 Jpg2Bmp(AStream,FStream);
 FStream.Seek(0, soFromBeginning);
 AStream.Size:= 0;
 AStream.CopyFrom(FStream,FStream.Size);
 AStream.Seek(0, soFromBeginning);
end;

end.
