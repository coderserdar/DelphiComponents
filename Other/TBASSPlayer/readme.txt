I am not a native English speaker.
Please be patient for my clumsy expressions.

TBASSPlayer is a Delphi component, which helps you make stream players easily with the power of BASS audio library (bass.dll, basswma.dll, bassmidi.dll and basscd.dll).
Please read BASS.txt and related document if you want to know more about BASS audio library.
You can get them at http://www.un4seen.com.
TBASSPlayer can play the file formats supported by BASS audio library such as
 - stream files (WAV, MP1, MP2, MP3, Ogg Vorbis, AIFF, WMA format)
 - music files (MOD, MO3, IT, XM, S3M, MTM, MOD, UMX format)
 - MIDI files (MID, MIDI, RMI, KAR format)
 - CD Audio tracks.
 - URL stream (files from the internet and the streams from Shoutcast/Icecast)

TBASSPlayer supports "BASS add-ons" - plugin modules designed for BASS audio library which enables the playback of additional file formats.
TBASSPlayer can play other file formats, which are none-BASS native, not supported by BASS add-ons, using Winamp 2.x input plug-ins.
You can load upto 8 BASS add-ons and upto 8 Winamp input plug-ins simultaneously for various file format.

TBASSPlayer also supports followings,
 - Winamp Visualization plug-ins (supports the latest plug-in such as Milkdrop 2.0e)
 - Winamp 2.x DSP plug-ins 
 - Winamp 2.x General Purpose plug-ins (especially the plug-ins for lyrics display)
 - Tag editor for WMA, MP3, AAC and Ogg Vorbis file 
 - 10 Band Equalizer 
 - Sound effect (Flanger, Echo, Reverb) 

So, you can enjoy the power of numerous Winamp plug-ins with that of BASS audio library. 

Followings are the files used to build up TBASSPlayer.
  - BASSPlayer.pas : main unit
  - VisDrive.pas : unit to drive Winamp Visualization plug-ins.
  - GPPDrive.pas : unit to drive Winamp General Purpose plug-ins.
  - PlayListUtils.pas : unit to support managing play list.
  - WasabiAPI.pas : unit to support Wasabi services API based Winamp visualization plug-ins.
  - PluginCtrl.pas : unit to manage Winamp plug-ins and to interface
                     BASS audio library to Winamp plug-in.
  - PluginConfig.pas(dfm) : unit for easier managing of Winamp input
                     plug-ins by supplying visual dialog form.
  - MPEGInfoBox.pas(dfm), OGGInfoBox.pas(dfm), WMAInfoBox.pas(dfm), TagEdit.pas(dfm) : units
                     to enable you to view file information & to edit tag.
  - WMAReader.pas : unit for Loading & Editing WMA File Tags written by Philip Hadar.
  - wa_ipc.pas : Pascal version of wa_ipc.h file from the Winamp v5.03 API, written by Saivert.
  - ioplug.pas   : Winamp plugin header adaption for Delphi/Pascal written
                     by Mr. Christian Zbinden.
  - APETag.pas, AACfile.pas, MPEGAudio.pas, ID3v1.pas, ID3v2.pas, OggVorbis.pas, WMAFile.pas and WAVFile.pas : 
                  units written by Mr. Jurgen Faul, used to retrieve tag information on stream 
                  file ( Visit http://mac.sourceforge.net/atl/ for futher information on these files)
  - CommonATL.pas : auxiliary unit for the above units written by The MAC Team
  - Dynamic_BASS.pas : Modified version of BASS.pas for dynamic loading of bass.dll,
                   written by Mr. Ian Luck.
  - RT_BASSWMA.pas, RT_BASSMIDI.pas, RT_BASSCD.pas, RT_bassmix.pas : Modified versions of BASSWMA.pas, BASSMIDI.pas, 
    BASSCD.pas and bassmix.pas whose original versions are written by Mr. Ian Luck.for dynamic loading of them
  - BASS_AAC.pas : unit to support bass_aac.dll 
  - TntCollection.pas, UniCodeUtils.pas : units to support Unicode related functions
  - AnsiStringStream.pas : units to support Ansi String Stream. (needed only for Delphi 2009)
  - Delphi_Ver.inc : auxiliary unit for the above units. 

Followings are the files to build the demo program, BASSPlay.exe.
  - BASSPlay.dpr, BASSPlay.dof, BASSPlay.res : project files
  - BassTest.pas(dfm) : main unit
  - About.pas(dfm) : unit to show information on demo program
  - AddonConfig.pas(dfm) : unit for managing BASS add-ons.
  - InputURL.pas(dfm) : unit for URL input.  
  - DSPPluginCtrl.pas(dfm) : unit for managing Winamp DSP plug-ins.
  - VisPluginCtrl.pas(dfm) : unit for managing Winamp visualization plug-ins.
  - PlayListConfig.pas(dfm) : unit for managing play list
  - Util_LeoLyrics.pas : unit to support "Gen_LeosLyrics.dll", a kind of Winamp
                          General Purpose Plug-in for lyrics display.

Followings are the library files to support Winamp plug-ins.
 - VisDrawer.dll : Neededed to drive Winamp 5 visualization plug-ins.
                   This library provides Winamp visualization plug-ins with Winamp-like window frame.
 - MBDrawer.dll  : Neededed to drive Winamp general purpose plug-ins. 
                   This library provides Winamp general purpose plug-ins with Mini Browser or
                    Winamp-like window frame.  

And additional files for localization & License notification      
 - _lang_eng.txt : a skeleton language file for localization
 - LGPL.TXT  : Document file on License notification 

I have also included 3 units(components) which are used for the main unit of demo program.
  - TSlider(Slider.*) : written by AO ROSNO (?)
  - TKnob(Knob.*) : written by Gary Bradley Hardman
  - M7SegDisp : written by Milan R. Zavisic
  - TDnDListBox(DnDListBox.*) : written by St?hane Vidouse


Note) 
   1. You must install above TSlider, TKnob, M7SegDisp and TDnDListBox components prior to opening BASSPlay.dpr to
       compile demo program.  
      Demo program has been successfully tested on Delphi 4, Delphi 7 and Delphi 2009.
   2. Any executable program which uses TBASSPlayer requires that bass.dll (and optionally basswma.dll, bassmidi.dll, 
       basscd.dll, bassmix.dll, VisDrawer.dll, MBDrawer.dll) should be placed in the directory where the executable 
       program resides in.
   3. You should install the soundfont 'Chorium.SF2' in the '<directory demo program resides in>\Plugins' directory  
       to play MIDI files by BASS audio library (bassmidi.dll). 
       You can get it using this URL - 'www.un4seen.com/download.php?Chorium.exe'. 
       However you can use any SF2 soundfont other than 'Chorium.SF2' if you modify just one line of the source code 
       which specifies soundfont in BassTest.pas.
   4. It is also necessary that bass.dll should be installed in '<directory Delphi installed>\Bin' to get 
       correct values of published properties of TBASSPlayer at design time, prior to putting TBASSPlayer component 
       on a form.
       (not necessary if you create TBASSPlayer using "Create" method by program code as demo program does 
         - e.g. BASSPlayer1 := TBASSplayer.Create(Self);)
   5. The Winamp GPP for lyrics display, "Gen_LeosLyrics.dll" is not included in this package because it's size is 
       big.  You can download that from http://www.leoslyrics.com.
 

License)
The programs written by me are free if it is used for non-commercial purpose.
You can redistribute and modify them but author's name in program header should be keft.
The BASS audio library is free for non-commercial use. 
Please visit http://www.un4seen.com if you want to know more about the License agreement on the BASS audio library.
Please consult Nullsoft, Inc. or the copyrighters of them for the use of Winamp plug-ins.


Please inform me if you have any advice or improvement on TBASSPlayer.

                                    14 May 2009

 Silhwan Hyun, hyunsh@hanafos.com

