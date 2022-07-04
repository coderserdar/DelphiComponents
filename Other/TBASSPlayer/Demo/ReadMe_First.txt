BASSPlay is a demo program for TBASSPlayer, which is a Delphi component based on BASS Audio Library.
You can get the source of TBASSPlayer at Torry's Delphi pages (http://www.torry.net/).
TBASSPlayer - the core component of BASSPlay - attempts to load following BASS Audio Library, BASS add-on and auxiliary library files at start up.
 - bass.dll, basscd.dll, bassmidi.dll, basswma.dll, Plugins\bass_aac.dll
 - MBDrawer.dll (for lyrics display)
 - VisDrawer2.dll (for Winamp-like visualization form) 
  
I only included bass.dll among the required BASS Audio Library, so you should download the library files at BASS Sound System Homepage (http://www.un4seen.com/).
note) All BASS Audio Library files should be the version of 2.4...

The BASSPlay program also attempts to load the lyrics plug-in "Gen_LeosLyrics.dll" if it is in the Plugins directory.
So, if you want to use "Gen_LeosLyrics.dll" plugin for lyrics display, put the plugin in the Plugins directory. 
note) some of auxiliary files for "Gen_LeosLyrics.dll" should be also put in the directory where BASSPlay installed.
And BASSPlay program attempts to load a language file named "lang_(AnyString).txt" for localization of menus and messages.
Edit included "_lang_eng.txt" file according to your local language and rename it "lang_(YourCountryString).txt" (ex : "lang_German.txt") if you want to apply your local language.


About Image File for Skin 
 You can change the apperence of BASSPlay by providing it with image files for skin.
 Please refer the contents of Skin\BASSPlay.res, which includes the image files for skin.
 The image files should be located in "Skin" directory with specific file name.
 The assigned image file names are as follows,
  - Gen_.bmp          : Frame image for Forms
  - MainPanel_.bmp    : Base panel image of Main Form 
  - CButtons_.bmp     : Button image of Main Form and Playlist Form
  - Shufrep_.bmp      : Button image of Main Form
  - SpectrumBack_.bmp : Spectrum panel image of Main Form 
 
For Unicode support
 You can compile demo program with old versions of Delphi compiler.
 But you should use Delphi 2009 if you want full support of Unicode, because the VCLs such as TEdit, TMemo and TListbox 
 of old Delphi (except Delphi 2009) does not support Unicode.  

Please inform me if you have any bug reports, advices or improvements on BASSPlay / TBASSPlayer.

                                    14 May 2009


 Silhwan Hyun (hyunsh@hanafos.com) 
 
