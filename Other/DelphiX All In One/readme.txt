(*******************************************************************************
                       EXTEND UNIT DXDRAWS FROM DELPHIX PACK

 *  Copyright (c) 2004-2017 Jaro Benes
 *  All Rights Reserved
 *  Version 1.19.x
 *  D2D Hardware module
 *  web site: www.micrel.cz/Dx
 *  e-mail: delphix_d2d@micrel.cz

 * DISCLAIMER:
   This software is provided "as is" and is without warranty of any kind.
   The author of this software does not warrant, guarantee or make any
   representations regarding the use or results of use of this software
   in terms of reliability, accuracy or fitness for purpose. You assume
   the entire risk of direct or indirect, consequential or inconsequential
   results from the correct or incorrect usage of this software even if the
   author has been informed of the possibilities of such damage. Neither
   the author nor anybody connected to this software in any way can assume
   any responsibility.


   First works started at 13.9.2004.
   All units in this pack contain last changes and was added very interesting
   units (for study or inspiration)
   Tested in Delphi 4,5,6,7 and Delphi 2005/2006/Turbo/2007/2009/2010/XE/XE2/XE3/XE4/XE5/XE6/XE7/XE8/Seattle/Berlin/Tokyo
   Note: Support is weak for package lower than Delphi 2006.

 * FEATURES:
   a) Implement Hardware acceleration for critical function like DrawAlpha {Blend},
      DrawSub and DrawAdd for both way DXIMAGELIST and DIRECTDRAWSURFACE with rotation too.
   b) Automatic adjustement for texture size different 2^n.
   c) Minimum current source code change, all accelerated code added into:
      DXDraw.BeginScene;
      //code here
      DXDraw.EndScene;
   d) DelphiX facade continues using still.

 * HOW TO USE
   a) Design code like as DelphiX and drawing routine put into
      DXDraw.BeginScene;
      //code here
      DXDraw.EndScene;
   b) setup options in code or property for turn-on acceleration like:
      DXDraw.Finalize; {done DXDraw}
      If HardwareSwitch Then
      {hardware}
      Begin
        if NOT (doDirectX7Mode in DXDraw.Options) then
          DXDraw.Options := DXDraw.Options + [doDirectX7Mode];
        if NOT (doHardware in DXDraw.Options) then
          DXDraw.Options := DXDraw.Options + [doHardware];
        if NOT (do3D in DXDraw.Options) then
          DXDraw.Options := DXDraw.Options + [do3D];
        if doSystemMemory in DXDraw.Options then
          DXDraw.Options := DXDraw.Options - [doSystemMemory];
      End
      Else
      {software}
      Begin
        if doDirectX7Mode in DXDraw.Options then
          DXDraw.Options := DXDraw.Options - [doDirectX7Mode];
        if do3D in DXDraw.Options then
          DXDraw.Options := DXDraw.Options - [do3D];
        if doHardware in DXDraw.Options then
          DXDraw.Options := DXDraw.Options - [doHardware];
        if NOT (doSystemMemory in DXDraw.Options) then
          DXDraw.Options := DXDraw.Options + [doSystemMemory];
      End;
      {to fullscreen}
      if doFullScreen in DXDraw.Options then
      begin
        RestoreWindow;
        DXDraw.Cursor := crDefault;
        BorderStyle := bsSingle;
        DXDraw.Options := DXDraw.Options - [doFullScreen];
        DXDraw.Options := DXDraw.Options + [doFlip];
      end else
      begin
        StoreWindow;
        DXDraw.Cursor := crNone;
        BorderStyle := bsNone;
        DXDraw.Options := DXDraw.Options + [doFullScreen];
        DXDraw.Options := DXDraw.Options - [doFlip];
      end;
      DXDraw1.Initialize; {up DXDraw now}

 * NOTE Main form has to declare like:
      TForm1 = class(TDXForm)

 * KNOWN BUGS OR RESTRICTION:
   1/ Cannot be use DirectDrawSurface other from DXDraw.Surface in HW mode.
   2/ New functions was newer tested for two and more DXDraws on form. Sorry.

 * HISTORY

**** please see into history.rtf document ****


 ******************************************************************************

 * PACK NOTE for (un)DelphiX pack recompilation with hardware acceleration

   For simple application start can be use the prototype application in this pack

 * PACK HISTORY________________________________________________________________
 04.12.2018 JB
 + Support Delphi 10.3 Rio added and some conditional revided.
 19.11.2017 JB
 + Support Delphi 10.2 Tokyo added.
 27.4.2016 JB
 + Support Delphi 10.1 Berlin added and some conditional revided.
 10.12.2015 JB
 + Support Delphi 10 Seattle added and some conditional revided.
 10.4.2015 JB
 + Support Delphi XE8 added and some conditional revided.
 04.9.2014 JB
 + Support Delphi XE7 added and some conditional revided.
 02.5.2014 JB
 + Support Delphi XE6 added and some conditional revided.
 20.9.2013 JB
 + Support Delphi XE5 added and some conditional revided.
 06.5.2013 JB
 + Support Delphi XE4 added and some conditional revided.
 04.9.2012 JB
 + Support Delphi XE3 added and some conditional revided.
 01.9.2011 JB
 + Support Delphi XE2 added and some conditional revided.
 02.9.2010 JB
 + Support Delphi XE added.
 17.5.2010 JB.
 - DXCommon unit remove from pack (only special purpose use in build-in DirectX.pas pack) for incompatibility with other products.
 + Added layer texture support (for future use to render image in image).
 + Clean interfaces (better support DX 7, but RM mode requires old interfaces still).
 + Better compatibility for PURE Delphi 2010 (DirectX headers are use from Delphi 2010 primary).
 + Isolate all Hori's code (code can be removed and all will be run througt DirectX).
 + Small bugs repaired.
 02.11.2009
 + Support Delphi 2010 added
 28.02.2009
 * Pack recompiled under Delphi 3,4,5,6,7,2005,2006,2007,2009 for conditionals change and Delphi 2009 bug fix. 
 07.04.2008 JB.
 + Main enhacement relelased, overwritten in the history document
 09.06.2005 JB.
 + Better compatibility with Delphi 4.
 + Some .dpk files revised, installation for all the same.
 + added the DXMisc.pas file contain some useful functions.
 + DXSprite.pas file revised for support under Delphi 3;
   Under Delphi 3 cannot be use colli3DX.pas, isn't adapt still, sorry.
 12.06.2005 JB.
 + Improvement of hardware acceleration and fix-bug.
 19.06.2005 JB.
 + Fix-bug in Draw() function.
 23.10.2005 JB.
 + DXSpriteEngine improvement
   Isn't necessary derive classes sprite, it is possible use DrawAdd, DrawAlpha and DrawSub also for BackgroundSprite.
   New proposal sprite from editor, sorting into the collection (likewise as with DXImageList or DXWaveList), generation Events.