Window State Components
ReadMe
Contents
Overview
Installation
Demo Projects
Change log
License and Disclaimer
Bugs and Feature Requests
About the Author
» Contents
Overview
The Window State Components can save and restore a window's size, position and state (minimized, maximized or normal) between program executions. Three components are provided that use different means of recording the window's information. They are:

TPJWdwState
This component records window information in an ini file. The user has control over the ini file name (via the IniFileName property), the root path used when the specified file name is relative (via the IniRootPath and the name of the section of the file where window information is recorded (using the Section property). Alternatively the ini file name and section names can be configured by handling the OnGetIniData and OnGetIniDataEx events that are triggered immediately before the ini file is read or written.
TPJRegWdwState†
This component uses the registry to record window information. The registry root key and sub key where the information is stored are controlled by the RootKeyEx†† and SubKey properties, or by handling the OnGetRegDataEx††† event. This event is triggered just before the registry is accessed. Additional application defined data can be read from or written to the registry by handling the OnGettingRegData and OnPuttingRegData events that are triggered after the component reads or writes the registry.
† You are discouraged from using TPJRegWdwState for programs compiled with Delphi 5 and earlier that may be run on 64 bit Windows, because the component may not be able to access the 64 bit registry view correctly and window state may be lost.
†† As of v5.6.0 the RootKeyEx property superseded RootKey. The new property takes an enumerated value to set the required root key instead of the numerical value used for RootKey. This makes RootKeyEx much easier to use in the Delphi Object Inspector because the use selects between user friendly value names instead of having to enter an integer value. Note that the RootKey property has been retained for backward compatibility.
††† As of v5.6.0 the OnGetRegDataEx event superseded OnGetRegData. Like RootKeyEx, OnGetRegDataEx takes an enumerated root key value instead of the numerical value passed to OnGetRegData. The older event is still present, but any event handler is only called if OnGetRegDataEx does not have a handler.
TPJUserWdwState
This component relies on the user to handle saving and reading the window state data to or from persistent storage. The component gives the most flexibility of all the components at the expense of placing the storage burden on the user. The component triggers OnReadData and OnSaveData events when ready to read or save data.
All components implement the same core functionality that is controlled by some common properties and events:

The components can automatically restore and save windows when the program starts up and closes down (using the AutoSaveRestore property). If AutoSaveRestore is set to False then the Restore and Save methods must be called from the host application.
The Options property can be used to customise the way the window is restored as follows:
The components can be instructed to ignore the saved window state – the window is then displayed in the normal state.
The window's saved size can be ignored and the default size of the form used instead. This is useful for dialogue boxes and fixed size windows.
The window can be kept within the current work area of the desktop. This option works with multiple monitors. If the form containing the window state component is a MDI child form this option keeps the window within the parent form's client area, after allowing for any menu, tool-bars or status bar etc.
When the form is to be restored in a minimized state it briefly appears on screen in the normal state before being minimized. The MinimizeDelay property controls the delay between the window appearing and being minimized.
TPJWdwState and TPJRegWdwState also support the OnReadWdwState event. Handling this event enables the stored window's state, size and position values before the window is restored. This event is called after reading the data and before sizing the window. TPJUserWdwState does not expose this event because the user is in charge of reading the data and can therefore modify it in the OnReadData event.

» Contents
Compatibility
The components are compatible with the Delphi Windows 32 bit and 64 bit compiler. They have been tested using Delphi 5, Delphi 7 and Delphi 2006 through to Delphi XE4. The components may on Delphi 4, but that has not been tested.

The components are for use with the VCL only: they are not compatible with FireMonkey.

.NET and non-Windows platforms are not supported.

» Contents
Installation
The Window State Components are supplied in a zip file. Before installing you need to extract all the files from the zip file, preserving the directory structure. The following files will be extracted:

PJWdwState.pas – component source code.
PJWdwState.dcr – resource file containing component palette glyphs.
PJWdwState.hlp – component help file that integrates with the Delphi 4 to 7 OpenHelp system.
PJWdwState.als – a-link keyword file for integration with the Delphi 6 or 7 OpenHelp system.
ReadMe.htm – this file.
MPL-2.txt – the Mozilla Public License.
ChangeLog.txt – component's change log.
Documentation.URL – short-cut to the component's online documentation.
In addition to the above files you will find the the source code of four demo projects along with read-me files in the Demos directory and its 1, 2, 3 and 4 sub-directories.

You can now install the components into the Delphi IDE. If you need help doing this see here.

Note that the help file included in the download can only integrate with the IDE in Delphi 4-7. For information on how to do this, see this article. Users of other versions of Delphi can either use the help file as a stand-alone file (in which case some links won't work) or you can use the Window State Components online documentation that can be accessed via the provided Internet short-cut.

NOTE: The help file is deprecated and is no longer being updated. The last update was in the v5.4.1 release. Changes and new features added since then have not been included in the help file. For up to date documentation please refer to the online documentation.

» Contents
Demo Projects
There are four demo projects included with these components. They are:

StandardDemo.dpr
Demonstrates how to use the components in the standard way, i.e. dropped on a form from the component palette. This demo uses TPJRegWdwState.
StandAloneDemo.dpr
Demonstrates how to create and use the components dynamically using the CreateStandAlone constructor. This demo uses TPJWdwState.
UserDemo.dpr
Demonstrates how to use TPJUserWdwState and load and save data in the OnReadData and OnSaveData events.
MDIDemo.dpr
Demonstrates the use of TPJWdwState with MDI applications.
Further information about the demo programs can be found in the file Demos\DemoReadMe.htm that is included in the download.

» Contents
Change log
A complete change log is provided in a text file that is included in the download.

» Contents
License and Credits
These components are released under the terms of the Mozilla Public License v2.0.

Thanks to the following who have contributed to this project:

Stefan Winter
Enrico Bortolazzi
Bruce J Miller
Craig Symons

All relevant trademarks are acknowledged.

» Contents
Bugs and Feature Requests
Bugs can be reported or new features requested via the Issue Tracker.

If no similar report or request has been recorded already, use the New Issue link to add a new issue. Please select the most appropriate template from the Templates drop down list.

» Contents
About the Author
I'm Peter Johnson – a hobbyist programmer living in Ceredigion in West Wales, UK, writing write mainly in Delphi. My programs and code are available from: http://www.delphidabbler.com/.

I can be contacted via the website.

Please let me know if you have any comments about the component, but please use the Issue Tracker noted above to report bugs and request new features.
This document is copyright © 2005-2014, P D Johnson, www.delphidabbler.com