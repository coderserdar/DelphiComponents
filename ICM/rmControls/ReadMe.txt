
rmControls v1.90
 
Table of Contents
Components/Controls/Classes	rmLibrary - functions/procedures	Description of Components and Functions
Disclaimer	Contact Details	Library Status
History	Acknowledgments	Release Notes
 
Components/Controls/Classes
rmStandard	rmEdits	rmCombos
TrmCalendar
TrmCheckBox
TrmClock
TrmCollectionListBox
TrmGauge
TrmImageListGlyph
TrmImageListGraphic
TrmLabel
TrmListControl
TrmSpinButton
TrmTrackBar
 	TrmBtnEdit
TrmFloatSpinEdit
TrmSpinEdit
TrmTimeSpinEdit
 	TrmBtnCombo
TrmColorComboBox
TrmComboBox
TrmComboCalendar
TrmComboPathTreeView
TrmComboTreeView
TrmNewComboBox
TrmSpinCombo
 
rmEnhanced	rmNonVisual	rmAdvanced
TrmCCPageControl
TrmCCTabControl
TrmEditDrawGrid
TrmEditGrid
TrmPanel
TrmPathTreeView
TrmSpeedButton
TrmSplitter
TrmTabSet
 	TrmApplicationEvents
TrmBrowseForFolder
TrmColumns
TrmCornerGrip
TrmDGTree
TrmFileDrop
TrmKeyBindings
TrmTreeNonView
TrmMemoryDataSet
TrmTrayIcon
TrmWordTree
 	TrmBinaryDataStorage
TrmCaptionButtons
TrmDayView
TrmDiffEngine
TrmDiffMap
TrmDiffMergeViewer
TrmDiffViewer
TrmInspector
TrmMDIBackground
TrmNotebookControl
TrmOutlookActionLink
TrmOutlookButtonList
TrmOutlookControl
TrmTaskBar
TrmTextDataStorage
 
Others	rmControlsEx	 
TrmToolWinForm
 	TrmZLIBDataStorage
 	
 
 
rmLibrary - functions/procedures
Conversion	Math/Numeric	Hardware
BoolToStr
CharToStr
DateToInt
IntToDate
SizeInt
StrToBool
StrToChar
 	CompInRange
GreaterThanFloat
GreaterThanInt
IntInRange
LessThanFloat
LessThanInt
SetInRange
 	GetMediaInfo
 
Graphic	CRC	WinSock
DarkenColor
DrawGrayText
GradientFill
LightenColor
ReplaceColors
RotateImage
RotateText
 	GetFileCRC32
GetStrCRC32
GetStrmCRC32
 	LocalIP
LocalName
 
String	Shell	OS
CountSections
LeadingZero
MaskStrCmp
PadLeft
PadRight
ParseSection
rmDateTimeToStr
SoundEx
StripString
 	GetFileIcon
GetFileImageIndex
GetFileImages
GetFileType
ReadShortCutFile
WriteShortCutFile
 	FindCmdLineSwitchWithParam
GetTempDir
GetTempFile
ReadRegString
WinOSVersion
WriteRegString
 
Common ListView Sorting Procs	Rect	Forms
CaptionStringSortProc
DateSortProc
DecodeSortData
EncodeSortData
FloatSortProc
IntegerSortProc
StringSortProc
 	ClientRectToScreenRect
RectDiameter
RectHeight
RectWidth
ScreenRectToClientRect
 	LoadDialogPosition
LoadFormState
SaveDialogPosition
SaveFormState
 

 
Description of Components and Functions
Name	Description
TrmApplicationEvents	This component allows for designtime assigning of the standard application events.

This was originally created to correct a VCL bug in the original release version of Delphi5.
TrmBaseEdit	As apart of the general tidying up I did I created a base edit control that most of the edit based components now descend from.
TrmBinaryDataStorage	This component allows you to load and store binary data as a part of the forms resource.
You can't load data at runtime but you can save the data to disk or stream it out from the component.
TrmBrowseForFolder	This is actually a couple of controls in one.
It allows you to display the standard windows dialog for: Browse for folder, Browse for Computers, Browse for Printers and Browser for Files or Folders
TrmBtnCombo	This component descends from the TrmBtnEdit.
It implements an edit control that has an ellipsis button and has a dropdown combo list built in.
TrmBtnEdit	This was another of my early components.
As such it's probably one of the most worked on components in the library.
It is used as a base control for a number of other rmControls.
It is an edit that has upto two ellipsis buttons.
TrmCalendar	This is a simple replacement for the M$ common control calendar.
TrmCaptionButtons	This allows you to place buttons on the caption bar of the form that the component is owned by.
TrmCCPageControl	This and the TrmCCTabControl were originally apart of the ComCtrls95 component set.

They have enhanced drawing functionality along with some of the original enhanced features of the original component set.
The TrmCCPageControl has the ability to have it's pages ripped off to form floating forms that can then be reattached to the original control when closed.
TrmCCTabControl	This and the TrmCCPageControl were originally apart of the ComCtrls95 component set.

They have enhanced drawing functionality along with some of the original enhanced features of the original component set.
TrmCheckBox	This is a non-windows implementation of a Checkbox.
TrmClock	This is a simple clock face that will show the appropriate hand position for the specified time.
TrmCollectionListBox	This is control similar to a listbox but instead of a string list it uses a collection.
Each item can be multi-line and also can have an image associated with it.
TrmColorComboBox	This is a standard combo box that has been modified to display colors and their associated names.
TrmColumns	This component is tied to the details view of a TListView control.
It can store and retrieve the columns, their sizes and which on had sorting.
Useful for storing and retrieving stuff to registry for multiple views.
TrmCornerGrip	This component is just placed on a form.
As long as there is nothing aligned so as to cover the bottom-right corner of the form then you will see the standard SizeGrip rect on the form at runtime.
TrmComboBox	This combo box descends from the original Delphi TComboBox, but has extra features like look ahead completion and MRU history.
TrmComboCalendar	This is a combobox that drops down a version of the TrmCalendar.
TrmComboPathTreeView	This is a combobox that drops down a version of the TrmPathTreeView.
TrmComboTreeView	This is a combobox that drops down a standard TTreeView.
TrmDayView	This is a "PIM" day viewer component. Supports multiple time divisions (5min, 10, 15, 20, 30, 1hr).
TrmDGTree	This is a very specialized type of non-visual tree.
I primarily use it in a spell-checker component/application that I've written.
TrmDiffEngine	This is the non-visual difference engine that I developed from some C source I found on the net.
It can be hooked up to multiple rmDiffViewers/rmDiffMergeViewers at once.
TrmDiffMap	This is a simple control that can be placed along side either an rmDiffViewer or rmDiffMergeViewer and can be linked to show where differences are located with in the two sources.
TrmDiffMergeViewer	This control allows the end user to create a new, merged, version of two diff'ed files.
TrmDiffViewer	This control shows the differences found in two diff'ed files.
TrmEditDrawGrid	This was originally a standard DrawGrid but a friend needed the ability to have an inplace editor in a cell and so I provided this and the TrmEditGrid as possible solutions.
TrmEditGrid	This was originally a standard StringGrid but a friend needed the ability to have an inplace editor in a cell and so I provided this and the TrmEditDrawGrid as possible solutions.
TrmFileDrop	Drop this component on a form and set a couple of properties and you can turn any TWinControl descendant in to a drop zone for files from the shell.
TrmFloatSpinEdit	This is a spinedit for editing real numbers.
TrmGauge	This is a simple component that can be used to as a measuring gauge.
It has multiple drawing styles and can support gradient color filling.
TrmImageListGlyph	This is a simple control that attaches to an imagelist and can change the displayed image via an imageindex property.

The difference between this component and the TrmImageListGraphic is that this one also allows you to have a caption attached to the image.
TrmImageListGraphic	This is a simple control that attaches to an imagelist and can change the displayed image via an imageindex property.

The difference between this component and the TrmImageListGlyph is that this one has NO caption property.
TrmInspector	This is a control similar to that found in Delphi's properties inspector.
TrmKeyBindings	Personally I don't know why Borland didn't come up with something like this.
This control allows the end user to customize the short-cut keys assigned in actionlists.
The changes can then be saved/loaded at any time.
The dialog that the end user sees is very similar to the one found in M$-Word.
TrmLabel	This was one of the first components I ever wrote.
It is basically an exercise in eye candy.

It provides the ability for borders, font textures and gradient shading.
TrmListControl	This control was originally created for the rmDiff viewer controls.
It is essentially a listbox control but one that has a couple of extra properties and features.
TrmMDIBackground	This component allows you to place an image in the MDI Client area of a form or to change it's color.

The image can be centered, tiled or stretched.
TrmMemoryDataSet	This is a Dataset descendant that can be used just like a regular dataset with on major exception.
It doesn't hook up to a Database, a datasource or anything else.
You can set up the columns at design time or runtime and the it operates almost exactly like a standard dataset but with out the overhead of having a DB lying around.
TrmNewComboBox	This was my first attempt at doing a non M$ Win32 based component.

It is similar in behavior and operation to the TrmComboBox but it is NOT a subclassed M$ Comboboxl.
TrmNotebookControl	The TrmNotebookControl is a TNotebook replacement. It is similar to the TrmOutlook control in how you use it but it will probably never be "deprecated" from the control set.

And it doesn't suffer from the extra border problem that you get if using the TPageControl.
TrmOutlookActionLink	This component allows you to automatically build rmOutlookButtonLists and add them to an rmOutlookControl from an ActionList.
Each category in the ActionList becomes a page in the rmOutlookControl.

This component was written by Piotr Jurago of Poland.
He sent it back to me and I've included it here.
Thanks Piotr!
TrmOutlookButtonList	This is similar to the M$ list control that you see in their outlook controls.

It supports large (32x32), small(16x16) and Custom image sizes.
This has been rewritten to reduce memory usage and to speed it up.
This can now handle thousands of items and is very efficient in how it stores and displays them.
TrmOutlookControl	This is actually very similar to a PageControl is how you use it, but it looks and feels like the control found in M$ Outlook.
Hence the name.
TrmPanel	This panel has a builtin splitter/sizer grip built in as well as a sizing button.
Either one can be used separately or in conjunction with each other.

Both of these features only work when the panel has been aligned to an edge.
TrmPathTreeView	This is treeview was originally based on the Delphi TTreeview.
The addition of the pathing code allows nodes to be added via a path and indexed via paths very quickly.

On average testing over 10000 nodes were added to the rmPathTreeView in less than 2000ms and all nodes were indexed/accessed via the pathing routines in less than 100ms. 
TrmSpeedButton	This enhanced speedbutton is used through out the rmControls library.

It has a couple of different drawing styles to allow for dropdown menus and combo boxes.
TrmSpinButton	This is an enhanced version of the Delphi Spin Button.
It provides some greater flexibility when used for automatic repeats.
TrmSpinCombo	This is a spinedit that has a dropdown list combo built in.
TrmSpinEdit	This is the base spinedit for most of the spinedit rmControls.
TrmSplitter	This was originally created to over come a bug in an early version of the Delphi 4 VCL.
That bug has sinced been fixed as of Delphi 5 and this is probably a redundant component.
I always ment to do more with it but I think I superceded it with the TrmPanel functionality.
TrmTabSet	This is an updated version of the old Win 3.1 Tabset component.
It is more resource friendly and provides the ability for further drawing styles. (ah-la - Windows 2000 style tabset)
TrmTaskBar	This is similar to the M$ Windows Taskbar.
It is to be used on an applications mainform and it shows all windows attached to the application.
It is especially useful in MDI applications and has many useful functions like hiding the minimized MDI Child windows.
TrmTextDataStorage	This is similar to the TrmBinaryDataStorage, but it is different in that it uses a stringlist.
This can be edited at designtime just like any other component that has a Stringlist in it.

You can't load data at runtime but you can save the data to disk or stream it out from the component.
TrmTimeSpinEdit	This is a spinedit for editing time values (24 and 12 hr time formats)
TrmToolWinForm	There is a very annoying bug in the M$ implementation the ToolWin windows style. (ie. bsSizeToolWin and bsToolWindow)

It causes what's know as the ALT-Tab bug where any form with a toolwin style that has focus will not allow the application to change focus to another application via the ALT-Tab key sequence.
(Check it out with the Delphi property inspector!!)

This form allows you to use a form that looks EXACTLY like a sizeable tool window but doesn't suffer from the ALT-Tab problem.

Look under the 'File|New...' menu item and select the rmToolWinForm to get it.
TrmTrackBar	This is a replacement for the M$ common control trackbar component.
It offers the ability to change the thumb and other customizations.
TrmTrayIcon	This component allows you to place an icon in the windows tray and provide functionality to handle menus and the such.
TrmTreeNonView	This has to be the most useful component in the entire rmControl library.
This is a non-visual treeview with the same pathing functionality as the TrmPathTreeView.
The speed increases on this component are almost to amazing to believe, your just going to have to believe me.
TrmWordTree	This non-visual component attempts to provide a simple dictionary type word lookup interface.
Given a list of words it will veryify words found in it's dictionary or provide a list of similar words found in its dictionary verified by a soundex algorithm.
TrmZLIBDataStorage	This component allows you to load and store ZLIB compressed data as a part of the forms resource.
You can't load data at runtime but you can save the uncompressed data to disk or stream it out from the component.

To use this component you must first install the rmZLIB package and then the rmCtrlsEx package.
BoolToStr	Returns the strings 'True' or 'False' for the boolean value given.
CaptionStringSortProc	A function that does a compare of two TListItems caption property and returns the result.

Uses the CompareText function.
CharToStr	Takes a character and converts it to is string equivalent.  (ie. 'A' to '#65')
ClientRectToScreenRect	Operates similar to ClientToScreen except that it works on a TRect instead of a TPoint.
CompInRange	Determines whether a Comp value is within a given range.
CountSections	Counts the number of sections in a string separated by a specified character.
DarkenColor	Returns the given color value darkend by x percent.
DateSortProc	Takes two TListItems and from a given subitem index sorts the items as TDateTime values.
DateToInt	Takes a date and returns it as an integer in the format of "yyyymmdd".
DecodeSortData	Decodes the ListViews column id and sort direction from a single encoded value generated from the EncodeSortData function.

Handy for a single ListView that needs to emulate multiple LiveViews.
DrawGrayText	Similar to the DrawText command but draws the text disabled.
EncodeSortData	Encodes a ListViews column id and sort direction into a single encoded value that can be decoded by the DecodeSortData function.

Handy for a single ListView that needs to emulate multiple LiveViews.
FindCmdLineSwitchWithParam	A function that allows for the processing of commandline options that need to contain parameters.

A command line option like "/load:filename.txt" can return the "filename.txt" value from the option "/Load:".
FloatSortProc	Takes two TListItems and from a given subitem index sorts the items as Float values.
GetFileCRC32	Calculates a CRC32 value on a given filename.
GetFileIcon	Returns the icon for a given file extension.
GetFileImageIndex	Returns the shell icon image index for a given file extension.
GetFileImages	Returns the shells imagelist handle for displaying shell icons.

(Warning: Use this function with caution.  Read up on using shared imagelists before using.)
GetFileType	Returns the Shell file type string for a given file extension.
GetMediaInfo	Returns information about a drive based on the flags passed into the call.
GetStrCRC32	Calculates a CRC32 value on a given string.
GetStrmCRC32	Calculates a CRC32 value on a given stream.
GetTempDir	Handles the tedious work of the standard windows GetTempPath routine.
GetTempFile	Handles the tedious work of the standard windows GetTempFilename routine.
GradientFill	Draws a gradient fill on a canvas based upon a start and end color.
GreaterThanFloat	Returns the larger of the two real number values.
GreaterThanInt	Returns the larger of the two integer values.
IntInRange	Determines whether an Integer value is within a given range.
IntegerSortProc	Takes two TListItems and from a given subitem index sorts the items as Integer values.
IntToDate	Takes an integer, value formated as "yyyymmdd", and returns it as a TDate.
LeadingZero	Pads an integer value with leading zeros to a specified string length.
LessThanFloat	Returns the smaller of the two real number values.
LessThanInt	Returns the smaller of the two integer values.
LightenColor	Returns the given color value lightend by x percent.
LoadDialogPosition	Loads a dialog forms last position and size from the registry.

The main difference between this function and the LoadFormState function is that this doesn't restore the WindowState variable.
LoadFormState	Loads a forms last position, size and optionally its last WindowState from the registry.

The main difference between this function and the LoadDialogPos function is that this can restore, if desired, the WindowState variable.
LocalIP	Returns the Local machines IP address if available.
LocalName	Returns the Local machines Name if available.
MaskStrCmp	Does a string comparison with the * and ? wildcard characters
PadLeft	Pads a string with leading spaces to a specified string length.
PadRight	Pads a string with trailing spaces to a specified string length.
ParseSection	Parses a returns data from a string separated by a specified character.
(ie 'One|Two|Three|' the index 2 would return 'Two')
ReadRegString	Basic wrapper function for the tediousness of having to read basic string information from the registry.
ReadShortCutFile	Reads a .LNK or .PIF file and returns the details of it in an TShortCutDetails record.
RectDiameter	Takes a TRect and returns what the circular diameter of the Rect is.
RectHeight	Returns the height of the Rect
RectWidth	Returns the width of the Rect
ReplaceColors	Takes a simple black and white bitmap and does a two color replacement.
rmDateTimeToStr	Returns the date time as a string in the format of  'yyyymmddhhnnsszzz'
RotateImage	Rotates an image by a specified rotational angle.
RotateText	Draws text on an canvas specified by a rotational angle.
SaveDialogPosition	Saves a dialog forms last position and size to the registry.

The main difference between this function and the SaveFormState function is that this doesn't save the WindowState variable.
SaveFormState	Saves a forms last position, size and WindowState to the registry.

The main difference between this function and the SaveDialogPos function is that this saves the WindowState variable.
ScreenRectToClientRect	Operates similar to ScreenToClient except that it works on a TRect instead of a TPoint.
SetInRange	Determines whether a value is with in a given range and sets it to one of the bounds if it isn't.
SizeInt	Takes an integer value and returns the shorted string version.  (ie 1024 to '1kb')
SoundEx	Does a simple SoundEx calculation on a given string.
StringSortProc	Takes two TListItems and from a given subitem index sorts the items as string values.
StripString	Strips a string of all characters from a specified character set.
StrToBool	Attempts to return the Boolean value of the given string.
StrToChar	Takes a string and attempts to convert it to is character equivalent.  (ie. '#65' to  'A')
WinOSVersion	This returns an enumerated type specifying the current operating system.
WriteRegString	Basic wrapper function for the tediousness of having to write basic string information to the registry.
WriteShortCutFile	Writes a .LNK file from the details given in a TShortCutDetails record.
Other Files, Units and Unmentionables
TrmHint	A rewrite of the standard Borland Hint window with some extra properties.
CompilerDefines.INC
(File)	This file provides every rmControl unit with a standard set of compiler defines and options.

Do not modify this file, doing so will most likely cause your library to stop compiling.
sergey_orlik_def.inc
(File)	Since I use parts of Sergeys form expert code to install the TrmToolWinForm I still have to include a copy of his include file with his compiler defines in them.
rmMSGList
(Unit)	A unit that converts standard windows messages into strings for logging or debugging purposes.

It fairly complete list also including VCL messages and some common ComCtrl messages when able.

Function GetMessageName(Msg:integer):string;
rmGlobalComponentHook
(Unit)	Provides a standard stack type interface to the rmControls that descend from TComponent and that hook their owners WndProc.
This enables the component to correctly backout of the hook with out hanging the form or application.

***** This is not to be used lightly!! *****
PLEASE look at all the units that use the functions located in this unit and how they use them.
** You have been warned **
rmInspectorItems
(Unit)	This unit contains most of the usable rmInspectorItems.

This are most of the items I've ever had need for.
rmScrnCtrls
(Unit)	This unit shows how I've implemented most of the rm'ified Combo controls from standard controls such as the TrmTreeViewCombo.

If you want to make your own "combo" controls looking through this file is probably a very good place to start.
rmZLIB
(Package)	Starting with the v1.90 release I'm shipping an rm'ified version of the ZLIB routines.
I'm doing this to help support the rmZLIBDataStorage Component and to avoid possible name resolution conflicts.
The version of ZLIB being shipped is 1.1.4.
rmCtrlsEX
(Package)	Starting with the v1.90 release I'm shipping an extra unit that has outside dependancies.

This package requires that the rmZLIB package be installed before installing this one.
This package contains the TrmZLIBDataStorage component.
There are other bits and pieces of this component set that I could tell you about but I'm not.
If you are really interested and have and somewhat advanced knowledge of both Windows and Delphi programming then feel to browse the code.

 
Disclaimer
THIS SOFTWARE AND THE ACCOMPANYING FILES ARE DISTRIBUTED "AS IS" AND WITHOUT WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.

Because of the various software environments into which this code may be used, NO WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE IS OFFERED.

Good data processing procedure dictates that any program based on this code be thoroughly tested with non-critical data before relying on it.

THE USER MUST ASSUME THE ENTIRE RISK OF USING ANY OF THE ACCOMPANYING CODE.

 
Library Status
This code is being released as freeware.
I only ask that if you modify the code that you email a copy of the new source back to me.
If you do use and enjoy the library, drop me a line and let me know.

 
Contact Details
My name is Ryan Mills and the rmControl library homepage is http://www.mills-enterprise.ca/delphicomp.html
If you have questions or source code updates you can email them to this address rmcontrols@mills-enterprise.ca

 
History
I originally started this library in 1997 as the meControl library.
It wasn't long after this that I found the Delphi Prefix Registry and discovered that the "me" prefix was already spoken for.
Since then I have registered the "rm" prefix renamed all of the original components and produced a few new ones.

 
Acknowledgments
I wrote alot of the code found in this library and the code that I didn't write I either have been asked to take over and maintain or have found on the net and included in this library.

If you look in the header comments of the units you will find the relevant comments giving credit to the original author.

 
Release Notes
Date Released	Version	Comments
11-24-2002	1.90	EEK! Over a year since I last did an update. I'm so sorry!

Anyways here is the latest and greatest. (All disclaimers in effect! <g>) I've been busy starting my own company and getting married. It takes a bit out of a guy.
I've added a few new components, restructured a number of others and generally been a bad boy.
There should be no big breaks of functionality from the last version but testing is necessary.

**** This version now fully supports D7, D6 and D5, I think. ****

I no longer have D5 installed and haven't tested any of the new stuff.
If you have any problems please let me know.
New components in this release: TrmClock TrmDayView, TrmNoteBook, TrmZLIBDataStorage

New functions/procedures in this release: DateToInt, IntToDate, DarkenColor, LightenColor, FindCmdLineSwitchWithParam, GetTempDir, GetTempFile, ReadRegString, WriteRegString, CaptionStringSortProc, DateSortProc, DecodeSortData, EncodeSortData, FloatSortProc, IntegerSortProc, StringSortProc, ClientRectToScreenRect, ScreenRectToClientRect, LoadDialogPosition, LoadFormState, SaveDialogPosition, SaveFormState

I have located a bug in original release of D6 that affects the ActionManager.
**This bug affects the TrmKeyBindings component specifically.
If you change the shortcut of a TClientActionItem at runtime (for sure) it will change the shortcut but it wont change the caption on things like the menu items.  BAD!!

I finally have an email address that is a little more permanent: rmcontrols@mills-enterprise.ca

- Fixed a bug in TrmInspector that was causing a memory leak.  Thanks to Patrick O'Keeffe for finding it.
- Fixed a bug in TrmPathTreeView, thanks to the guys at the Marathon project for that one.
- Generally fixed a number of bugs most if not all the units. (Read: I'm sure I touched every unit!)
06-22-2001	1.80	I finally got my copy of D6!  Thus the big jump in the version numbers.  This release now supports both D5 and D6.  The most changes had to occur in the design-time code.  There were a couple of function deprication issues that I had to IFDEF out but for the most part this version of the code base is same.  You shouldn't notice any difference. 
*****Please Note*****
Just because I've gotten the entire component set compiled in D6 doesn't mean that at this time I have run a full regression test on all the code.  Please notify me immediatly of any problems encountered while useing these components with Delphi 6.
*****Please Note*****

I have once again had an email address change: ryan.mills@quest.com (no longer in use)
I am working on getting a more stable and permanent address but I haven't yet as I am moving in the next week or so.  Please be patient with me.  I do monitor the borland news groups if you can't seem to get ahold of me any other way.

- Updated TrmTreeNonView and TrmPathTreeView with some speed enhancements
   and a bug fix that caused the indexing to fail on the Items.assign function. 
- Updated the ancestry of TrmImageListGlyph 
- Updated with bug fixes TrmCalendarCombo, TrmDiffViewer, TrmNewComboBox
- Fixed a bug in TrmListControl that was causing a memory leak.  Thanks to 
   Ingo Eckel for finding it.
- Rewrote the clientarea code for the TrmPanel to properly handle multi-embeded
   rmPanels
- Modified the rmConditionalDefines.INC file to include what I consider necessary 
   compiler defines.
- Rewrote the drawing code for the TrmTab3x component to allow for alTop 
   aligned tabs.  All other alignments still work like the original alBottom alignment did.

Added SoundEx, RectHeight, RectWidth, rmCheckBox, rmCheckboxInspectorItem
  rmOutlookActionLink (written by Piotr Jurago of Poland)

05-03-2001	1.70	- Updated the TrmMemoryDataSet.  Mea Culpa, I included an 'in progress' copy of 
   the control instead of the working copy.  Thanks to Graeme D. Keast for finding 
   that one. 
- Updated the rmNewComboBox.  I was again in the progress of changing over the 
   base component of the copy I sent out. Thanks to Peter Giroux for that one. 
Added rmToolWinForm

04-19-2001	1.65	Updated a number of components implementing bug fixes and enhancements.

Added rmDGTree, rmMemoryDataSet and rmInspector.
03-19-2001	1.60	This is the original public release version.