(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomImageSource;

interface

uses Windows, Graphics;

type
 TNMMCustomImageSource= class(TObject)
 protected
   FCaptureRect: TRect;
 public
   procedure GetImage(ABitmap: TBitmap); virtual; abstract;
 end;


implementation

end.

