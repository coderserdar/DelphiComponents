**********************************************************************************
* Name         : BitButton
* Component    : TDDMBitBtn
* Author       : Oliver Killguss
* Version      : 2.5.1
* Release Date : 2009-04-19
**********************************************************************************


This File contains important informations about this software. For furter information about current version
and history refer to sourcecode.


DESCRIPTION
===========
This Component is a Speedbutton like component for Delphi 1-6 which extends flat style how office 97 and
includes a automatically grayscale convert of the image. So you don't need to handle different images for
each state, there also converts glyph for enabled state to lightening image. So you can give your 16 Bit
Delphi applications a new behaviour and modern look like win 32 will do or to save development cycle time.
This Component is easy to use. Only import your preferred image and rest will be done by BitBtn.


WHAT'S NEW
==========
Anchors and Action properties added. Resource files reoganized. OnChangeCaption removed.
New improved Win-Messaging support. Some changes on handling resources. Accelerator now working.
New HTML Help added. Take a look to folder help.

Now this component inherits from TGraphicControl instead of TSpeedButton because there will be
many restrictions from origin component. To get more flexible I have to start a new component which
inherits from TGraphicControl.

Now the NumGlyph property is supported, so you can import Glyphs divided into different images in same
file. Glyphs are indicates some different states of an button e.g. disabled or selected. This Property
is similar to Delphi and so it will be able to import glyphs which shipped with Delphi into BitBtn!
BitBtn use it's own states for the imported images but to get compatible with older images and to save
time to generate new images you can use this property to get out the "normal" state which will be handled
by BitBtn to get the other states. As default color for the transparent color it will use the pixel at
position 0, height - 1 (left bottom edge) or you can select a TransparentColor by yourself.

Spacing to set space between glyph and button caption. Try out to see the effect!

Choose AutoSize to let BitBtn calculates width and height of the button. Sorry this version doesn't support
AutoSize calculation only for width or height but this is for later version planned.

The included demo application is completely revised and can handle all important properties to show the
possibility of this component.


PROPERTIES
==========
ShowCaption : Allows to switch showing caption
GrayScale   : Brightness of grayed image. Value from 0 to 255 is valid.
Style       : gisSystemDeactivated - highlighted image
              gisGray              - Grayed Image
              gisBlue              - Image has blue taint
NumGlyphs   : Equal to Delphi's SpeedButton (see what's new section!)
Spacing     : Space between Text and Glyph
AutoSize    : Calculates needed button width and height automatically to text and glyphs size.
              Remember that small image and text heights results a small button height!


EVENTS
======
+ OnChangeGlyph   : Always fired when glyph is changing
- OnChangeCaption : Occurs when caption changes **** !!!!REMOVED!!!! ****
                    in Release 2.00


INSTALL
=======
BDS 2005-2009 : Choose in file menu "File -> Open" and select file DDMBitButton.bpl. In the Project view window select 
                DDMBitButton.bpl with the right mouse button and choose "Install". The new Component will be Installed
                in the component palette under the register "DDM".

Delphi 1-7    : Choose Install components from main menu and select bitbtn.pas from your preferred directory.
                Confirm the appearing dialog's and compile it. The component will be installed on register 'DDM'
                at your component palette. Be sure that the sources are in Delphi's search path.


BUGFIX
======
- If button is disabled, text will drawn in a grayed style
- If Autosize is set to false and large text's will now drawn in a clipping area to guarantee that text
  doesn't overwrite button border.
- Some displaying errors in paint method corrected
- Error when try to install Component in Delphi 1 and resource not compatible


TODO-LIST
=========
- Disable Glyph converting is not correct
- ParentFont is not set correct
- While pressing mouse button and leaving button area causes strongly displaying effects


LICENCE CONDITIONS & DISCLAIMER
===============================
BitBtn is free!
Please have a look at License.txt shipped with this software for further information.


OTHERS
======
If you have any idea, suggestions or error reports don't hesitate to contact me at
killguss@web.de


Thanks and enjoy it !
Oliver Killguss