Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.).
Author E-Mail: mrh.info2007@gmail.com.
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
.............................................................................................................
Contents:

  Version history
  Comments author
  Delphi installation
  Tested program
  Warranty info
  Licence info

.............................................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
.............................................................................................................
Comments author:

I developed this professional screen camera component with delphi (captures the activity on a top desktop screen and writes it into an AVI file. it supports many compressors, the program contains code derived from MRH Software Co. OPENSOURCE). 

I'm using a TThread for the recording function and a "transparent" window for a draw frame. This window is always on top of all windows and only has a frame as window region (Frame is set up by region). In the recording function I call a paint method of the draw window for every frame (PaintBorder). also i add at this unit capability record audio of input device.

If there's anybody who would like to share some time in improving something or just try it out, please send me an email!
.............................................................................................................
Delphi installation:

Notice: Before installing you sure that uninstalled the older versions of this component in delphi IDE.

The most simple way to install this component is by opening the appropriate design package in Delphi and clicking on the big "Install" button.  For instance, Delphi 7's design package is ScrCamD7_Package.dpk.
.............................................................................................................
Tested program:

I tested this program in full screen mode (Res: 1920 x 1080) at a pc with:
Delphi Ver : Delphi 2007 Code Gear
OS     : Windows Vista x64
CPU    : AMD X2 64bit DualCore 5600+
RAM   : 4 GB - DualBus 800
H.D.D  : 4 x Maxtor SATA2 32MB Cache (Raid 10)

Results at 1000 f/s (1 f/ms) by some codecs :
XviD v6.4 and DivX v1.1.3 Codecs recording avrage 50 fps.    (Fast Codec, Good Quality Image).
MSU Screen Capture Lossless v1.02 recording avrage 18 fps. (Normal Codec, Best Quality Image).
MS Video Codec 1 recording avrage 15 fps.                          (Normal Codec, Low Quality Image).
Huffyuv v2.1.1 recording avrage 17 fps.                                (Normal Codec, Best Quality Image, Very Larg File).
Logarith lossless codec recording avrage 15 fps.                    (Normal Codec, Best Quality Image).
MS MPEG-4 VKI v1.0/2.0/3.0 recording avrage 40 fps.            (Fast Codec, Low Quality Image).
ffvfw Mpeg-4 Codec Uncompressed recording avrage 15 fps.  (Low Codec, Best Quality Image, Very Larg File). 
Intel Indeo v4.5 recording avrage 20 fps.                               (Bad Codec).
Cinepak Codec by Radius recording avrage 10 fps.                 (Very Bad Codec).
Windows Media Video 9 - VCM recording avrage 30 fps.         (Normal Codec, Low Quality Image).
.............................................................................................................
Warranty info:

This code and information is provided "as is" without warranty of
any kind, either expressed or implied, including but not limited
to the implied warranties of merchantability and/or fitness for a
particular purpose.

You may use and modify this code for your personal use only.
You may also redistribute this code providing that
a) No fee is charged and 
b) This copyright notice
is retained in the source code.

Copyright (c) 2000-2008 MRH Software Co. All Right Reserved
.............................................................................................................
Licence info:

Please also notice the License agreeements from MRH Software Co. OPENSOURCE:
This product is FREEWARE and you are free to duplicate and distribute this software through the internet or any preferred media.
If you create an product that contains code derived from MRH Software Co., you are free to distribute it for any purposes, including commercial purposes. However, your product must include an acknowledgement that mention it contains code from MRH Software Co. A simple statement like "Professional Screen Camera By MRH Software Co."
in the AboutBox will do. You are not obliged to reveal the source code of your derived product but are encouraged to do so.
.............................................................................................................
Keep on hacking!

thanks:
     Kambiz R. Khojasteh  -----  http://www.delphiarea.com
     pisarev ---- http://www.myart.bz/pisarev.net
