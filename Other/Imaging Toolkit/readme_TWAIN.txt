    TWAIN Toolkit for Delphi, version 4.1

    TWAIN application, driver & component for Delphi 7 - XE5
    -------------------------------------------------------------------------

	Imaging Toolbox
	Tamsborgvej 56
	3400 Hillerød 
	Denmark

	E-mail:		CustomerCare@imagingtoolbox.com
	Homepage:	www.imagingtoolbox.com
			


    --------------------------------------------------------------------------
                    IMPORTANT   IMPORTANT   IMPORTANT
    --------------------------------------------------------------------------
    REDISTRIBUTABLE FILES:

    TWAIN version 2.0 and later requires 2 files, the Data Source Manager "DSM", 
    one for 32 bit and one for 64 bit applications. These file are not included 
    with Windows. Therefore, You must distribute these file.
    The TWAINDSM.DLL is installed in the windows system directory (typically
    C:\Windows\System32). If installing the 32 bit file on a 64 bit Windows, 
    make sure it's installed in the WOW64 system directory (normally 
    C:\Windows\SysWow64).

    You can find the two TWAINDSM.DLL files in the "Redist" sub-folder or from
    http://sourceforge.net/projects/twain-dsm/
    --------------------------------------------------------------------------

    LICENSE INFORMATION:

    Read the online help for information about the License agreement.

    --------------------------------------------------------------------------

    HOW TO ORDER:

    To purchase a license for this product please visit:

	http://www.imagingtoolbox.com/index.php/purchase

    --------------------------------------------------------------------------

    The Toolkit is installed into this directory, i.e. the folder you installed 
    the toolkit in, (normally):

    c:\Users\Public\Documents\RAD Studio\X.0\mcmTWAIN

      or

    C:\Documents and Settings\All Users\Documents\RAD Studio\X.0\mcmTWAIN


    Sample Data Source (TWAIN Driver)
        C:\Windows\TWAIN_32\ or C:\WINNT\TWAIN_32\

    Log helper file (mcmTWAIN.INI)
        C:\Windows\ or C:\WINNT\

    --------------------------------------------------------------------------

IMPORTANT
-----------------------------------------------------------------------------------------------
This toolkit requires that Delphi 6 is patched up with Service pack 1 as a minimum. 
If this service pack is not applied, the TmcmTWAIN component will not close down!
In general it is recommended that you apply all service packs available from Borland to your 
Delphi edition.


INSTALL THE TOOLKIT:
-----------------------------------------------------------------------------------------------
After running the SETUP.EXE file, you should run Delphi. In Delphi, select the Component, 
Install Packages menu. Click the ADD button, and select the 

	MCMTWN18.BPL	for Delphi XE5
	MCMTWN17.BPL	for Delphi XE4
	MCMTWN16.BPL	for Delphi XE3
	MCMTWN15.BPL	for Delphi XE2
	MCMTWN14.BPL	for Delphi XE
	MCMTWN13.BPL	for Delphi 2010
	MCMTWN12.BPL	for Delphi 2009
	MCMTWN11.BPL	for Delphi 2007
	MCMTWN7.BPL	for Delphi 7

component file.



UN-INSTALL THE TOOLKIT:
-----------------------------------------------------------------------------------------------
In Delphi, select the Component, Install Packages menu. In the dialogue, locate and select 
the "Imaging Toolbox, TWAIN Toolkit for Delphi", and click the Remove button.

From the Control Panel, select the Add/Remove Programs, and select "Imaging Toolbox, TWAIN Toolkit for Delphi" 
and click the Add/Remove button, to un-install the toolkit.


THE TWAIN MANUAL:
-----------------------------------------------------------------------------------------------
To get the original TWAIN Manual describing the TWAIN standard contact the TWAIN Working Group at:

homepage:	http://www.twain.org

It is strongly recommend that you acquire the original TWAIN specification from the above internet 
address, so that you will have the latest revision of the manual's and the required dll 
files needed to run the example's included in this zip file. 


HELP FILES:
-----------------------------------------------------------------------------------------------
mcmTWAIN.HLP	        Windows help file
mcmTWAIN.CNT	        Windows help contents file


EXAMPLE PROJECT FOR CREATING A TWAIN APPLICATION USING THE TmcmTWAIN COMPONENT:
-----------------------------------------------------------------------------------------------
d32twain.dpr		Project 1, TWAIN application.
uTwainForm.pas		Applications Main window.
uTwainForm.dfm		-
uTwnDlgSrcInfo.pas      	Diagolue showing information about the Data Source.
uTwnDlgSrcInfo.dfm      -

d32twainLI.dpr		Project 2, TWAIN application, Large Image.
uTwainFormLI.pas		Applications Main window.
uTwainFormLI.dfm	-


EXAMPLE PROJECT FOR CREATING A DAT SOURCE:
-----------------------------------------------------------------------------------------------
srclogo.ico	        Source Icon
mcmTWAINDS.dpr	        Project file
uDSUserInterface.pas    Data Source Main window
uDSUserInterface.dfm	-
mcmTWAINDSEntry.pas     Entry point for all messages to a Data Source.
mcmTWAINDSLayer.pas     A middle layer handling the TWAIN communication, and provides the "interface" to the user interface.
uTwnDlgHandle.pas       Unit maintaining a æist of handles to all data source windows.
usabout.pas	        Source about dialogue.
usabout.dfm	        -
uTwnDlgAppInfo.pas      Application information dialogue.
uTwnDlgAppInfo.dfm      -
uTwnDlgSimEvents.pas    Simulate device events dialogue.
uTwnDlgSimEvents.dfm    -

Driver32.res            Resource file containing version information and data strings displayed in the TWAIN Select Source dialogue.


COMPILED PROGRAM FILES:
-----------------------------------------------------------------------------------------------
d32twain.exe	Delphi Example of a TWAIN application.
		
mcmTWAINDS.ds	Delphi Example of a TWAIN driver. 
		Placed at
		C:\Windows\TWAIN_32\ or C:\WINNT\TWAIN_32\


ADDITIONAL INFORMATION:
-----------------------------------------------------------------------------------------------
The source code contains a few pratical notes and hints, that are important !!!

Data Source: 
	The menu item "File Open" in the Data Source (TWAIN Driver), allows you to open bitmap and 
	JPEG images in the TWAIN Source interface. 
	These images will be transfered to the calling application upon selecting the File, Exit/
	Transfer menu item.
	If the TWAIN interface is closed in any other way, no image will be transfered back
	to the calling application.


CHANGING TWAIN VERSION INFORMATION IN THE SAMPLE DATA SOURCE:
-----------------------------------------------------------------------------------------------
You can edit the driver32.res with NOTEPAD.EXE, and convert this file useing the TRES.BAT 
file. Note: that the program "BRCC32.EXE" and pathes used in the TRES.BAT may be placed 
at another location on your computer. Therefore, verify and if required modify the path to BRCC32.EXE.


YOUR COMMENTS:
-----------------------------------------------------------------------------------------------
Send us a note if
*	you have any comments or suggestions on how to improve the TWAIN Source interface, 
	application, component.

The address is provided above, and we would realy like to hear your great ideas ! 


Imaging Toolbox
Marts 2014
