 TEDBImage and TQREDBImage v1.6 (Enhaced TDBImage and TQRDBImage):
  by Sebastián Mayorá - Argentina - DelphiHelper@yahoo.com.ar

TQREDBimage was designed for printing image files. It has the same features that TEDBImage has.

»EDBImage works like TDBImage except :
   - It can manage .ico .bmp .wmf .emf .jpg .jpeg. Without a line of code !!!
   - Can copy to clipboard .bmp .wmf .jpg .jpeg	
   - Event OnLoadCustomImage is fired when the image type is unknown, so you can load "any" type of images (gif, tiff, png,....)
**NEW ver 1.5**:
   - property ZoomToFit: boolean  : when TRUE and the image size is smaller than component the image is zoomed to component size (keeping aspect ratio).
   - property ShrinkToFit: boolean: when TRUE and the image size is bigger than component the image is reduced to component size (keeping aspect ratio)
   - If you download (Highly recomended) GraphicEx ((c) Dipl. Ing. Mike Lischke)from www.lischke-online.de  .Tiff, .gif, .tga,
       and 25 more formats available (yes, 25 formats).
   In order to use .tiff, .gif , etc from GraphicEx ((c) Dipl. Ing. Mike Lischke) YOU MUST AGREE GraphicEx license (read License.txt of GraphicEx).

   To provide support for these formats you must change GraphicConfiguration.inc before installing TEDBImage and TQREDBImage
   
»Changing GraphicConfiguration.inc
1- Read carefully License.txt and GraphicConfiguration.inc.
2- If you do not agree, please don't change anything.
3- To enable a format (eg. Tiff) {$define TIFFGraphic}
4- To Disable a format (eg. Tiff  {.$define TIFFGraphic} or comment line  //{$define TIFFGraphic}
5- Disable as much formats as you can to save memory (exe file grow 550 Kb with all formats)
6- Save all files modified and recompile packages. (Use Build All if available)

Note: for TESTING PURPOSES ONLY, I have this configuration
{.$define SGIGraphic}             // *.bw, *.rgb, *.rgba, *.sgi images are supported
{.$define AutodeskGraphic}        // *.cel, *.pic images
{$define TIFFGraphic}            // *.tif, *.tiff images
{.$define TargaGraphic}           // *.tga, *.vst, *.icb, *.vda, *.win images
{.$define PCXGraphic}             // *.pcx, *.pcc, *.scr images
{.$define PCDGraphic}             // *.pcd images
{.$define PortableMapGraphic}     // *.ppm, *.pgm, *.pbm images
{.$define CUTGraphic}             // *.cut (+ *.pal) images
{$define GIFGraphic}             // *.gif images
{.$define RLAGraphic}             // *.rla, *.rpf images
{.$define PhotoshopGraphic}       // *.psd, *.pdd images
{.$define PaintshopProGraphic}    // *.psp images
{.$define PortableNetworkGraphic} // *.png images
{.$define EPSGraphic}             // *.eps images


If you don't want to use GraphicEx comment the line
{$DEFINE GraphicEX}
in uEDBR.pas and recompile.
   
»What you can do with EDBImage is:
  - Copy, Cut and paste from clipboard. No code needed.
  - LoadFromFile and SaveToFile (New in v1.3)
  - Load "any" type of TGraphic using OnLoadCustomImage event:
	If you need OTHER kind of graphics (such GIF, TIFF, etc)
   	then you should write something like this in OnLoadCustomImage Event:

    	procedure TForm1.EDBImage1LoadCustomImage(var B: TGraphic; Stream: TStream);
    	begin  
      	   B := TXXX.Create;		{XXX is your class of Graphic (TGifImage, TTiffImage, etc).}
           B.LoadFromStream( Stream );
    	end;  				//That is ALL.!!! - do not call B.Free.



»Please mail me for: - Bugs
                    - Suggestions
                    - say Hello.
                    - Comments
                    - etc...

»New in version. 1.6
- Packages for Delphi 7 (vclSer70.dpk, dclSer70.dpk)
- Fixed problem with DBCtrlGrid.
- Fixed problem when not using GraphicEx. (Thanks to Roberto Mamoru Matuda)
- Listen to Ctrl + Z and Esc keys.
- Thanks to Nick Spurrier, Roberto Mamoru Matuda, John Faubion, Pierre du Plessis, 
   Jaro Griscik, david david, Daniel Cañas, Rolf-Dieter Schenkel, Mauricio Vargas Echeverry,
   Thiago de Goz Ferreira, Shraga Milon, Juan Badell for their support, emails and suggestions

»New in version. 1.4
- Optimized LoadPicture;
- Some bugs fixed
- Added LoadFromFile and SavetoFile methods

»Known Issues
- Current version Tested with D7, D6. Previous version tested with D6, D5 and D4.
- OnLoadCustomImage  tested with TGIFImage (from RXLib)
- Some random bugs with .gif (using GraphicEx)
- QREDBImage: set ZoomToFit and ShrinkToFit always to TRUE to avoid some strange QR behavior.



Thanks to:
  - Thanks to Nick Spurrier, Roberto Mamoru Matuda, John Faubion, Pierre du Plessis, 
    Jaro Griscik, david david, Daniel Cañas, Rolf-Dieter Schenkel, Mauricio Vargas Echeverry,
    Thiago de Goz Ferreira, Shraga Milon, Juan Badell for their support, emails and suggestions
  - Mohsen Rahmani, Mr. Hong, René Simon, Dayne and everyone for their help.
  - Dipl. Ing. Mike Lischke for making GraphicEx, it is wonderful.
  - Thanks you for try it.

THIS IS FREEWARE - USE AT YOUR OWN RISK, ETC, ETC    


»Install
0- Before install, remove previous versions of EDBImage (and QREDBImage)
   Choose Component | Install Packages..., select EDBimage and hit Remove.
   (delete or rename: edbImage.*, qrEDBimage.* )

1-Open VCLser40.dpk (Dephi4), VCLser50.dpk (Delphi5), VCLser60.dpk (Delphi6) or VCLser70.dpk (Delphi7)
Menu Project-Options in Directory/Conditionals tab 
set OutputDirectory to C:\Windows\System (or your system directory)
Compile it. DO NOT install, it is just a runtime package.

2-Open DCLser40.dpk (Dephi4), DCLser50.dpk (Delphi5), DCLser60.dpk (Delphi6) or  DCLser70.dpk (Delphi7)
Compile it, then Install It. This is the Designtime package.

3- Enjoy!!

4- Send me an email with your opinion. Thank you!