{
  Delphi Test program for Open Odbc DbExpress Driver.
  Version 2.01, 2004-05-23

  Copyright (c) 2001-2003 Edward Benson

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.
}



The following foreign libraries are used:
========================================

// _HexEditor_
// http://www.torry.net/diffedits.htm
// [http://www.torry.net/vcl/edits/diffedits/msthexedit.zip]
{$define _HexEditor_}

// _SynEdit_
// http://synedit.sourceforge.net
{.$define _SynEdit_}

// _GraphicEx_
// Allow view many image formats.
// [http://www.delphi-gems.com/Graphics.php]
// Need change file "GraphicConfiguration.inc": need
//       enable: $define UseLZW for support GIF image format.


It is recommended to apply enclosed corrections for Delphi units:
 DBCommon.pas
 SqlExpr.pas
Or add use of module "SqlExprFix.pas".
