TagCloud for VCL 2.1
====================

This package brings you the visual components, that are based on the idea
of a well known "tag cloud" navigation element.

Namely, you can use TTagCloud and TTagIndex components coupled with
a predefined styling components, that allows you to manage the look
and feel of your tag clouds and indexes. Components are designed for
all Embarcadero Delphi versions.

TTagCloud component has all the common features for easy presentation
of the list of data to the user, that you can expect from this kind
of visual representation.

TTagIndex allows you to present an index of keywords to the user in a
simple but effective form. It can be used also as a special menu bar
thanks to the possibility of customizing your index labels.

The visual components are descendants of TGraphicControl and have no
other libraries or third-party component requirements. They work very fast
and without the flickering in a double-buffered mode, as well as in the
direct canvas painting mode.

So, if you want to give your users the same opportunities to navigation,
that are familiar from the Web environment, also into your desktop
applications, or if you want to easily implement an index of keywords
feature into your applications, try these components.

"TagCloud for VCL" is distributed as a Freeware product.
Read License.txt for more infomations.


Requirements:
--------------------
Embarcadero Delphi, no other third-party components and libraries are required.


Installation:
-------------
TagCloud for VCL is distributed as a standard zip file with the design-time
packages, source files and demos.

1. Create a new folder in any location and unzip the archive content to it.
2. Add the Source subdirectory of a newly created folder to your Library path.
3. Open, compile and install an appropriate package (TagCloud_XYZ.dpk)
   that is intended for your Delphi version

If you do not want to install the components manually, then you can download
an automatic installer of 'TagCloud for VCL' and let the Setup Wizard
do all the work for you.

1. Download and run the installer.
2. Follow the steps of Setup Wizard.

Then you can open, compile and execute the demo projects, that can be found
in the Demos folder. Examples for an older Delphi versions are
available in the Demos\D5 and Demos\D7 folders.


Change log:
-----------
* Version 2.1 (2014-09-29)
  - added: Delphi XE6/XE7 support
  - fixed: A bug in TCustomTagCloud.Rebuild method (line 4082), that caused some memory leaks
    
* Version 2.0 (2013-11-16)
  - added: Delphi XE4/XE5 support
  - added: OnTagDblClick event
  - added: HoverColSpace - a new property of TCustomTagCloud, that allows you to handle the space between items
           as a part of the hovered item, so you can draw additional icon or anything else inside custom drawing
           event handlers and react on clicks when the mouse cursor is inside this area.
  - added: New sample project, named DeleteTagsGUI, that demonstrates the HoverColSpace usage

* Version 1.9.5 (2013-01-01)
  - added: Delphi XE3 support
  - and other minor improvements

* Version 1.9 (2012-07-13)
  - added: VerticalAlignment property
  - added: AutoScaleFont property, that scales the items (their display font size) to fit as much available space as possible
  - improved: Missing default values have been assigned to properties

* Version 1.8 (2012-03-15)
  - added: TCustomTagCloud.RowCount and TCustomTagCloud.VisibleRowCount properties
  - added: TCustomTagCloud.GetItemRowIndex function
  - added: TNavPathLabel - a new simple path info component

* Version 1.7 (2012-01-18)
  - added: Selected property to TTagCloudItem - by this way you can handle multi-select behavior of the tag cloud (see also StylingDemo)
  - changed: Visible property of TTagCloudItem is now declared as published
  - fixed: Bug in component installer (Setup Wizard) - Delphi 5 IDE was not recognized on developer's computer

* Version 1.6 (2011-09-26)
  - added: Delphi XE2 compatibility
  - added: Automatic installer (Setup Wizard)
  - added: GetItemPageIndex function
  - fixed: Corrected conditional defines for using GetTextExtentPoint32 function

* Version 1.5 (2011-03-16)
  - added: TTagIndex component
  - added: Custom scale definition (you can setup your own value ranges along with their font sizes and colors)
  - added: Frames and backgrounds support for tag items, including colors, styles, margins, etc.
  - added: TTagCloudPrmStyler as a new component for managing the styles of your tag clouds (see also TCustomTagCloudStyler)
  - added: OnBeforePaint and OnAfterPaint events, to support custom painting onto the whole TagCloud component surface
  - added: OnTagPositioning event, that allows you to customize the position of tag items (ie. insert breaks, spaces, etc.)
  - added: FixedColCount property, that forces an automatic calculation of FixedColWidth, so the tag cloud items are arranged into the specified number of columns
  - added: Direction property, that allows you to compose the tags horizontally (by default) or vertically (as an Index, rather than Cloud). It is applicable only for fixed-width tag clouds.
  - added: SelectedItem property, that identifies one of the items as selected (see also SelectedColor and SelectedFrame)
  - added: Colors.AddColor method, to allow more comfortable adding of color levels at run-time
  - added: ShrinkDiacritic property, that can produce even more shrunken tag clouds, when you are not using captions with diacritical characters
  - added: GetItemMetrics method, that can be used to retrieve the calculated rectangles, text height, and other fields of an item used for drawing
  - added: New demonstration projects, that presents the newly implemented components and properties in action.
  - improved: Aligning the tags on the baseline
  - improved: Redesign of TTagCloud component declaration, to support more comfortable inheritance (TTagCloud is inherited from TCustomTagCloud now)
  - improved: MinValue and MaxValue properties are available as public now
  - improved: Better centering the hovered tag items inside their drawing rectangles
  - improved: The speed of rebuild process for a large amount of items
  - improved: More precise calculation of auto-shrinked rows
  - improved: Now all temporary fields of TTagCloudItem are always calculated (even if OnCustomDrawItem is not assigned), so also the GetItemAt method
              and detection of hovered item are now more precise
  - improved: DisplayName for design-time editing of color level items
  - improved: Autosizing of the component (when large items together with small items are located on the last row)
  - improved: Displaying a maximum count of available tags even if the component has the smallest possible size
  - improved: Suppressed calling of Rebuild methods during the load process of the component
  - fixed: A validation flag for items is now handled correctly, so the previously invisible items are rebuilt after switching the relevant
           properties (filter, logscale, etc.) on and off.
  - fixed: Sorting with diacritics
  - and other minor improvements and fixes

* Version 1.1 (2010-11-03)
  - added: Compatibility with Delphi XE
  - added: Multi-page and AutoSize support (see the PageCount, PageIndex, DisplayCount, FirstDisplayIndex, LastDisplayIndex and AutoSize properties)
  - added: Automatic sorting (see the Sorted property)
  - added: Custom sorting (see the OnCompareItems event)
  - added: Fast binary searching for items (see the Find(Caption) method)
  - added: Searching the items by Data pointer (see the Find(Data) method)
  - added: Quick access methods, such as IncreaseValue(Caption, Increment, ...), AddItem(Caption, Value, ...)
  - added: Automatic and/or user defined filtering (see the Filter and VisibleCount properties and an OnFilter event handler)
  - added: A variable row heights (see the AutoShrinkRows property)
  - added: Fixed column width support (see the FixedColWidth property)
  - added: OnCustomDrawItem event handler, that supports both standard and user defined drawing of items
  - improved: Cache mechanism (tag cloud rebuilding) has been significantly improved, so the component can be used for working with large amounts of data
  - improved: Sorting speed for large number of items (it is implemented as a non-standard workaround, so if you want to use, you have to enable it
              by setting the UseSpeedSort public property to True at run-time)
  - fixed: Truncated item labels, that were located near by the edges of the component (when HoverEnlarge was True and ColSpacing was too small)
  - added: An advanced demonstration project, that shows the newly implemented properties and methods in action
           (also includes an example of "Creating the tag cloud items from text", "Custom drawing example" and "Opacity simulation by color levels definition")

* Version 1.0 (2010-08-20)
  - The first release of TTagCloud component


Used third-party components and libraries:
------------------------------------------

- Embarcadero Delphi for Win32 Professional, http://www.embarcadero.com/
- PasDoc, a documentation tool for ObjectPascal source code, http://pasdoc.sourceforge.net
- IcoFX, http://icofx.ro/


Contact:
--------
Precision software & consulting
e-mail:  info@be-precision.com
www:     http://www.be-precision.com
support: www.be-precision.com/support
         support@be-precision.com
forum:   www.be-precision.com/forum
rss:     www.be-precision.com/rss_en.xml


========================================================
Copyright (c) 2008-2014  Precision software & consulting
All rights reserved.
