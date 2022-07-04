******************************************************************************
                      FREEWARE Delphi Steganography component ver. 3.0
                       Copyright (c) 2007 by Kiril Antonov
******************************************************************************

Contents:
 1. Description
 2. Installation
 3. Usage 
 4. Disclaimer of warranty

DESCRIPTTION
------------

TKASteganographyImage component provides basic Steganography features 
of embeding text and binary data in a bitmap image.
Only windows bitmaps (*.bmp) are supported
Bitmap size is not altered so empty image and image containing data have same size.
Each LSB bit in the RGB triple of the bitmap contains embedded data 
so human eye cannot find the difference between empty image and image containing data.
Please see attached demo to see how component is working.
Data is password protected.
Long passwords gives better protection of the data.

WARNING:
--------------------------
If you forget the password used to embed the data, 
data cannont be recovered in any way, 
becouse password is used as a base to encrypt the message 
XOR-ing password and the embedded data!
Version 3.0 is not compatible  with the previous versions of KASteganographyImage!
All previous versions can be found at http://www.kadao.dir.bg/


Installation for Delphi
------------------------
Open and install/compile KASI.dpk

DISCLAIMER OF WARRANTY
----------------------
This IS A FREEWARE COMPONENT!
COMPONENTS ARE SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND. THE AUTHOR
DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
THE WARRANTIES OF MERCHANTABILITY AND OF FITNESS FOR ANY PURPOSE. THE AUTHOR
ASSUMES NO LIABILITY FOR DAMAGES, DIRECT OR CONSEQUENTIAL, WHICH MAY RESULT
FROM THE USE OF COMPONENTS.
USE THIS COMPONENTS AT YOUR OWN RISK!

For contacts:
 my e-mail: kirila@abv.bg
 my site  : www.kadao.dir.bg

Best regards
   Kiril Antonov
   Sofia
   Bulgaria