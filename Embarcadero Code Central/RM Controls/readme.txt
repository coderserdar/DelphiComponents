Table of Contents:
      Disclaimer
      Library Status
      History
      Acknowledgments
      Release Notes
 
 
rmControls
v1.80
Enhanced Standard Components
Compound Components
Advanced Components
TrmApplicationEvents
TrmCCTabControl
TrmCCPageControl
TrmEditDrawGrid
TrmEditGrid
TrmLabel
TrmPanel
TrmPathTreeView
TrmSpeedButton
TrmSplitter
TrmSpinButton
TrmTabSet	TrmBtnCombo
TrmBtnEdit
TrmCalendar
TrmComboCalendar
TrmComboPathTreeView
TrmComboTreeView
TrmFloatSpinEdit
TrmSpinCombo
TrmSpinEdit
TrmTimeSpinEdit	TrmCollectionListBox
TrmDGTree
TrmDiffEngine
TrmDiffMap
TrmDiffMergeViewer
TrmDiffViewer
TrmInspector
TrmListControl
TrmKeyBindings
TrmMemoryDataSet
TrmOutlookActionLink
TrmOutlookButtonList
TrmOutlookControl
Shell Related Components
Miscellanous Components
TrmBrowseForFolder
TrmFileDrop
TrmTrayIcon	
TrmBinaryDataStorage
TrmCaptionButtons
TrmCheckBox
TrmColorComboBox
TrmColumns
TrmComboBox
TrmCornerGrip
TrmGauge
TrmImageListGlyph	TrmImageListGraphic
TrmMDIBackground
TrmNewComboBox
TrmTaskBar
TrmTextDataStorage
TrmTrackBar
TrmTreeNonView
TrmToolWinForm
TrmWordTree
rmLibrary Routines
Conversion
Math/Numeric
Hardware
StrToChar
CharToStr
BoolToStr
StrToBool
SizeInt	IntInRange
CompInRange
SetInRange
GreaterThanInt
LessThanInt
GreaterThanFloat
LessThanFloat
RectHeight
RectWidth
RectDiameter	GetMediaInfo
Graphic
CRC32
WinSock
GradientFill
RotateImage
RotateText
ReplaceColors
DrawGrayText	GetFileCRC32
GetStrCRC32
GetStrmCRC32	LocalIP
LocalName
String
Shell
OS
ParseSection
CountSections
LeadingZero
PadLeft
PadRight
StripString
rmDateTimeToStr
MaskStrCmp
SoundEx	ReadShortCutFile
WriteShortCutFile
GetFileType
GetFileIcon
GetFileImageIndex
GetFileImages	WinOSVersion

 
 
 
Name
Description
TrmApplicationEvents	This component allows for designtime assigning of the standard application events. 
Note:  It was originally created to correct a VCL bug in the original release version of Delphi5.

TrmCCTabControl	This and the TrmCCPageControl were originally apart of the ComCtrls95 component set. 
They have enhanced drawing functionality along with some of the original enhanced features of the original component set. 

TrmCCPageControl	This and the TrmCCTabControl were originally apart of the ComCtrls95 component set. 
They have enhanced drawing functionality along with some of the original enhanced features of the original component set. 

The TrmCCPageControl has the ability to have it's pages ripped off to form floating forms that can then be reattached to the original control when closed. 

TrmEditDrawGrid	This was originally a standard DrawGrid but a friend needed the ability to have an inplace editor in a cell and so I provided this and the TrmEditGrid as possible solutions.
TrmEditGrid	This was originally a standard StringGrid but a friend needed the ability to have an inplace editor in a cell and so I provided this and the TrmEditDrawGrid as possible solutions.
TrmLabel	This was one of the first components I ever wrote.  It is basically an exercise in eye candy. 
It provides the ability for borders, font textures and gradient shading. 

TrmPanel	This panel has a builtin splitter/sizer grip built in as well as a sizing button.  Either one can be used separately or in conjunction with each other. 
Note: Both of these features only work when the panel has been aligned to an edge. 

TrmPathTreeView	This is treeview was originally based on the Delphi TTreeview.  The addition of the pathing code allows nodes to be added via a path and indexed via paths very quickly. 
Note: On average testing over 10000 nodes were added to the rmPathTreeView in less than 2000ms and all nodes were indexed/accessed via the pathing routines in less than 100ms. 

TrmSpeedButton	This enhanced speedbutton is used through out the rmControls library. 
It has a couple of different drawing styles to allow for dropdown menus and combo boxes. 

TrmSplitter	This was original created to over come a bug in an early version of the Delphi 4 VCL.  That bug has sinced been fixed as of Delphi 5 and this is probably a redundant component.  I always ment to do more with it but I think I superceded it with the TrmPanel functionality.
TrmSpinButton	This is an enhanced version of the Delphi Spin Button.  It provides some greater flexibility when used for automatic repeats.
TrmTabSet	This is an updated version of the old Win 3.1 Tabset component to be more resource friendly and to provide the ability for further drawing styles.  (ah-la - Windows 2000 style tabset)
TrmBtnCombo	This component descends from the TrmBtnEdit.  It implements an edit control that has an ellipsis button and has a dropdown combo list built in.
TrmBtnEdit	This was another of my early components.  As such it's probably one of the most worked on components in the library.  It is used as a base control for a number of other rmControls. 
It is an edit that has upto two ellipsis buttons. 

TrmCalendar	This is a simple replacement for the M$ common control calendar.
TrmComboCalendar	This is a combobox that drops down a version of the TrmCalendar.
TrmComboPathTreeView	This is a combobox that drops down a version of the TrmPathTreeView.
TrmComboTreeView	This is a combobox that drops down a standard TTreeView.
TrmFloatSpinEdit	This is a spinedit for editing real numbers.
TrmSpinCombo	This is a spinedit that has a dropdown list combo built in.
TrmSpinEdit	This is the base spinedit for most of the spinedit rmControls.
TrmTimeSpinEdit	This is a spinedit for editing time values (24 and 12 hr time formats)
TrmCollectionListBox	This is control similar to a listbox but instead of a string list it uses a collection.  Each item can be multi-line and also can have an 
image associated with it.
TrmDGTree	This is a very specialized type of non-visual tree.  I primarily use it in a spell-checker component/application that I've written. 
Note: I haven't included the spell checker as a demo application, but it will be coming. 

TrmDiffEngine	This is the non-visual difference engine that I developed from some C source I found on the net.  It can be hooked up to multiple rmDiffViewers/rmDiffMergeViewers at once.
TrmDiffMap	This is a simple control that can be placed along side either an rmDiffViewer or rmDiffMergeViewer and can be linked  to show where differences are located with in the two sources.
TrmDiffMergeViewer	This control allows the end user to create a new version of two diff'ed files.
TrmDiffViewer	This control shows the differences found in two diff'ed files.
TrmInspector	This is a control similar to that found in Delphi's properties inspector.
TrmListControl	This control was originally created for the rmDiff viewer controls.  It is essentially a listbox control but one that has a couple of extra properties and features.
TrmKeyBindings	Personally I don't know why Borland didn't come up with something like this.  This control allows the end user to customize the short-cut keys assigned in actionlists.  The changes can then be saved/loaded at any time.  The dialog that the end user sees is very similar to the one found in M$-Word.
TrmMemoryDataSet	This is a Dataset descendant that can be used just like a regular dataset with on major exception.  It doesn't hook up to a Database, a datasource or anything else.  You can set up the columns at design time or runtime and the it operates almost exactly like a standard dataset but with out the overhead of having a DB lying around.
TrmOutlookButtonList	This is similar to the M$ list control that you see in their outlook controls.  It supports both the large and small Icon sizes.
TrmOutlookControl	This is actually very similar to a PageControl is how you use it, but it looks and feels like the control found in M$ Outlook.  Hence the name.
TrmOutlookActionLink	This component allows you to automatically build rmOutlookButtonLists and adds them to an rmOutlookControl from an ActionList.  Each category in the ActionList becomes a page in the rmOutlookControl.
This component was written by Piotr Jurago of Poland.  He sent it back to me and I've included it here.  Thanks Piotr!

TrmBrowseForFolder	This is actually a couple of controls in one.  It allows you to display the standard windows dialog for: Browse for folder, Browse for Computers, Browse for Printers and Browser for Files or Folders
TrmFileDrop	Drop this component on a form and set a couple of properties and you can turn any TWinControl descendant in to a drop zone for files from the shell.
TrmTrayIcon	This component allows you to place an icon in the windows tray and provide functionality to handle menus and the such.
TrmBinaryDataStorage	This component allows you to store binary data as a part of the forms resource.  It can also load data directly from a files. 
You can't load data at runtime but you can save the data to disk or stream it out from the component.
TrmCaptionButtons	This allows you to place buttons on the caption of the form that the component is owned by.
TrmCheckBox	This is a non-windows implementation of a Checkbox.
TrmColorComboBox	This is a standard combo box that has been modified to display colors and their associated names.
TrmColumns	This component is tied to a TListView control and in the details view remembers the columns, their sizes and which on had sorting.  It can save the data out and load it back in.  Useful for storing and retrieving stuff to registry for multiple views.
TrmComboBox	This combo box descends from the original Delphi TComboBox, but has extra features like look ahead completion and MRU history.
TrmCornerGrip	This component is just placed on a form.  If there is nothing aligned so as to cover the bottom-right corner of the form then you will see the standard SizeGrip rect on the form at runtime.
TrmGauge	This is a simple component that can be used to as a measuring gauge.  It has multiple drawing styles and can support gradient color filling.
TrmImageListGlyph	This is a simple control that allows the user to attach an imagelist and change the displayed image via an imageindex property. 
The difference between this component and the TrmImageListGraphic is that this one also allows you to have a caption attached to the image.

TrmImageListGraphic	This is a simple control that allows the user to attach an imagelist and change the displayed image via an imageindex property. 
The difference between this component and the TrmImageListGlyph is that this one has NO caption property.

TrmMDIBackground	This component allows you to place an image in the MDI Client area of a form or to change it's color.  The image can be centered, tiled or stretched.
TrmNewComboBox	This is my first attempt at doing a non M$ Win32 based component. 
It is similar in behavior and operation to the TrmComboBox but it is NOT a subclassed M$ Comboboxl.

TrmTaskBar	This is similar to the M$ Windows Taskbar.  It is to be used on an applications mainform and it shows all windows attached to the application.  It is especially useful in MDI applications and has many useful functions like hiding the minimized MDI Child windows.
TrmTextDataStorage	This is similar to the TrmBinaryDataStorage, but it is different in that it uses a stringlist.  This can be edited at designtime just like any other component that has a Stringlist in it.  It can also load data directly from a files.  You can't load data at runtime but you can save the data to disk or stream it out from the component.
TrmTrackBar	This is a replacement for the M$ common control trackbar component.  It offers the ability to change the thumb and other customizations.
TrmTreeNonView	This has to be the most useful component in the entire rmControl library.  This is a non-visual treeview with the same pathing functionality as the TrmPathTreeView.  The speed increases on this component are almost to amazing to believe, your just going to have to believe me.
TrmToolWinForm	There is a very annoying bug in the M$ implementation the ToolWin windows style. (ie. bsSizeToolWin and bsToolWindow) 
It causes what's know as the ALT-Tab bug where any form with a toolwin style that has focus will not allow the application to change focus to another application via the ALT-Tab key sequence.  (Check it out with the Delphi property inspector!!) 
This form allows you to use a form that looks EXACTLY like a sizeable tool window but doesn't suffer from the ALT-Tab problem.  Look under the 'File|New...' menu item and select the rmToolWinForm to get it. 

TrmWordTree	This non-visual component attempts to provide a simple dictionary type word lookup interface.  Given a list of words it will veryify words found in it's dictionary or provide a list of similar words found in its dictionary verified by a soundex algorithm.
StrToChar	Takes a string and attempts to convert it to is character equivalent.  (ie. '#65' to  'A')
CharToStr	Takes a character and converts it to is string equivalent.  (ie. 'A' to '#65')
BoolToStr	Returns the strings 'True' or 'False' for the boolean value given.
StrToBool	Attempts to return the Boolean value of the given string.
SizeInt	Takes an integer value and returns the shorted string version.  (ie 1024 to '1kb')
IntInRange	Determines whether an Integer value is within a given range.
CompInRange	Determines whether a Comp value is within a given range. 
SetInRange	Determines whether a value is with in a given range and sets it to one of the bounds if it isn't.
GreaterThanInt	Returns the larger of the two integer values.
LessThanInt	Returns the smaller of the two integer values.
GreaterThanFloat	Returns the larger of the two real number values.
LessThanFloat	Returns the smaller of the two real number values.
RectHeight	Returns the height of the Rect
RectWidth	Returns the width of the Rect
RectDiameter	Takes a TRect and returns what the circular diameter of the Rect is.
GetMediaInfo	Returns information about a drive based on the flags passed into the call.
GradientFill	Draws a gradient fill on a canvas based upon a start and end color.
RotateImage	Rotates an image by a specified rotational angle.
RotateText	Draws text on an canvas specified by a rotational angle.
ReplaceColors	Takes a simple black and white bitmap and does a two color replacement.
DrawGrayText	Similar to the DrawText command but draws the text disabled.
GetFileCRC32	Calculates a CRC32 value on a given filename.
GetStrCRC32	Calculates a CRC32 value on a given string.
GetStrmCRC32	Calculates a CRC32 value on a given stream.
LocalIP	Returns the Local machines IP address if available.
LocalName	Returns the Local machines Name if available.
ParseSection	Parses a returns data from a string separated by a specified character.  (ie 'One|Two|Three|' the index 2 would return 'Two')
CountSections	Counts the number of sections in a string separated by a specified character.
LeadingZero	Pads an integer value with leading zeros to a specified string length.
PadLeft	Pads a string with leading spaces to a specified string length.
PadRight	Pads a string with trailing spaces to a specified string length.
StripString	Strips a string of all characters from a specified character set.
rmDateTimeToStr	Returns the date time as a string in the format of  'yyyymmddhhnnsszzz'
MaskStrCmp	Does a string comparison with the * and ? wildcard characters
SoundEx	Does a simple SoundEx calculation on a given string.
ReadShortCutFile	Reads a .LNK or .PIF file and returns the details of it in an TShortCutDetails record.
WriteShortCutFile	Writes a .LNK file from the details given in a TShortCutDetails record.
GetFileType	Returns the Shell file type string for a given file extension.
GetFileIcon	Returns the icon for a given file extension.
GetFileImageIndex	Returns the shell icon image index for a given file extension.
GetFileImages	Returns the shells imagelist handle for displaying shell icons. 
(Warning: Use this function with caution.  Read up on using shared imagelists before using.)

WinOSVersion	This returns an enumerated type specifying the current operating system.
Note:  There are other bits and pieces of this component set that I could tell you about but I'm not for the sake of yours and my sanity’s.  If you are really interested and have and somewhat advanced knowledge of both Windows and Delphi programming then feel to browse the code.


 

Disclaimer
THIS SOFTWARE AND THE ACCOMPANYING FILES ARE DISTRIBUTED "AS IS" 
AND WITHOUT WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY 
OR ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED. 

Because of the various software environments into which this code may be used, 
NO WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE IS OFFERED. 

Good data processing procedure dictates that any program based on this 
code be thoroughly tested with non-critical data before relying on it. 

THE USER MUST ASSUME THE ENTIRE RISK OF 
USING ANY OF THE ACCOMPANYING CODE.

Library Status: This code is being released as freeware.  I only ask that if you modify the code that you email a copy of the new source back to me.

Contact Details: My name is Ryan Mills, unfortunately I'm currently between home pages.  Once I get a stable home page and email address I'll update the contact details here.

If you have source code updates you can email them to my work address, ryan.mills@quest.com.

I can't answer any question regarding the library at that email address, I will only respond if you have sent me source code updates.

History:  I originally started this library in 1997 as the meControl library.  It wasn't long after this that I found the Delphi Prefix Registry and discovered that the "me" prefix was already spoken for.  Since then I have registered the "rm" prefix renamed all of the original components and produced a few new ones.

Acknowledgments: I did not write all of the code found in this library and the code that I didn't write I have been asked to take over and maintain.  If you look in the header comments of the units you will find the relevant comments giving credit to the original author.

Release Notes:
 
Date Released
Version
Comments
 06-22-2001	
1.80
I finally got my copy of D6!  Thus the bug jump in the version numbers.  This release now supports both D5 and D6.  The most changes had to occur in the design-time code.  There were a couple of function deprication issues that I had to IFDEF out but for the most part this version of the code base is same.  You shouldn't notice any difference. 
*****Please Note*****
Just because I've gotten the entire component set compiled in D6 doesn't mean that at this time I have run a full regression test on all the code.  Please notify me immediatly of any problems encountered while useing these components with Delphi 6.
*****Please Note*****

I have once again had an email address change: ryan.mills@quest.com
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

05-03-2001
1.70
- Updated the TrmMemoryDataSet.  Mea Culpa, I included an 'in progress' copy of 
   the control instead of the working copy.  Thanks to Graeme D. Keast for finding 
   that one. 
- Updated the rmNewComboBox.  I was again in the progress of changing over the 
   base component of the copy I sent out. Thanks to Peter Giroux for that one. 
Added rmToolWinForm

04-19-2001
1.65
Updated a number of components implementing bug fixes and enhancements. 
Added rmDGTree, rmMemoryDataSet and rmInspector.

03-19-2001
1.60
This is the original public release version.