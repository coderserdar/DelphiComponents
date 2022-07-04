(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMBmpColorReducer;

interface
uses Classes, NMMCustomDataProcessors, Graphics;

type
 TNMMBmpColorReducer = class(TNMMCustomBitmapProcessor)
 protected
   FPixelFormat: TPixelFormat;
 public
   constructor Create(APixelFormat: TPixelFormat);
   procedure Process( ABitmap: TBitmap ); override;
   property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;
 end;

implementation

constructor TNMMBmpColorReducer.Create(APixelFormat: TPixelFormat);
begin
 inherited Create;
 FPixelFormat:= APixelFormat;
end;

procedure TNMMBmpColorReducer.Process( ABitmap: TBitmap ); 
begin
 ABitmap.PixelFormat:= FPixelFormat;
end;


end.
