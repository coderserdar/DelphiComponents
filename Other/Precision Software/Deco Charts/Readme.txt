DecoCharts for VCL 1.3
======================

"DecoCharts for VCL" is a set of interactive charting components for Embarcadero
Delphi, that can extended your applications with decomposition bar charts, budget
status indicators, KPI status tables, comparison tables and similar presentations.

Components are designed as a lightweight alternative to universal grids and
charting components. Every component from the DecoCharts package can be used
separately for its concrete purposes, without some resource-heavy universal
component class in the background.


DecoCharts for VCL package includes the following components:

TDecoBar:
  An interactive decomposition chart, that allows you to present a different
  types of financial, statistical or general numeric data in a clear form of
  expandable bars. Sections in each bar can be expanded and collapsed using
  the mouse or keyboard, so the end-users can easily navigate through the
  hierarchical structure of data defined in the chart. Of course, TDecoBar can
  be used to display a simple (one-level) stacked bar chart.

TDecoProgressGrid:
  A specialized grid of progress bars and their additional (supporting) data.
  Such a grid is a perfect for presenting a various kinds of budgets, KPI values,
  and other data that need to be displayed in a percentage form. Of course, it can
  also be used as a simple progress bar, if needed.

TDecoCompareGrid:
  Implements an easy to use comparison table that is based on the list of
  criterias, the list of alternatives and criteria values for each alternative.
  TDecoCompareGrid supports numeric, rating, progress bar, logical and text based
  criteria values, as well as custom drawing of comparison grid cells.

TDecoRatingStars:
  It is a simple interactive rating component. You can use it to display, or to
  get the user rating feedback. It supports a few internal styles and works also
  with linked image list (that contains the rating stars pictures)

TDecoProgressBar:
  Standalone progress bar component, that mimics the visual style of progress bars
  used in other complex DecoCharts controls.

TDecoBarPlus, TDecoProgressGridPlus and other components with "Plus" suffix:
  Descendants of the standard DecoCharts components (mentioned above), that use
  the GDI+ library for antialiased and composited drawing. This enhances the look
  and the feel of resulting charts/grids. DecoChartsPlus components supports also
  the Aero-Glass effect, so they can be used in conjunction with GlassFrame
  and ParentBackground options of your forms.


Requirements:
-------------
"DecoCharts for VCL" is designed for all Delphi versions. No other
third-party components and libraries are required for using the DecoCharts
standard components.

For using the DecoChartsPlus components in Delphi XE and older, a dynamic loading
implementation of GDI+ API library is required. This library is
included in the "DecoCharts for VCL" package, and it is distributed under the
terms of Mozilla Public License MPL v1.1, http://www.mozilla.org/MPL/MPL-1.1.html.
More info can be found at these web sites:
http://www.progdigy.com,
http://themiron.mirandaim.ru.


Installation (from ZIP archive):
--------------------------------
"DecoCharts for VCL" is distributed as a standard ZIP file with the design-time
packages, source files, demos and the GDI+ API library (see the Requirements above).

1. Create a new folder in any location and unzip the archive content to it.
2. Add the Source subdirectory of a newly created folder to your Library path.
3. Open, compile and install an appropriate package of DecoCharts standard components,
   that is intended for your Delphi version. (ie. DecoCharts_D7.dpk,
   DecoCharts_D2009.dpk, DecoCharts_DXE.dpk, etc.).
4. If you want to use a DecoChartsPlus components in Delphi XE and older,
   add the Source\GDIP subdirectory of a newly created folder to your
   Library path (you can skip this step, if you are using another variant of this
   public GDI+ library).
5. Then open, compile and install an appropriate package of DecoChartsPlus
   components, that is intended for your Delphi version. (ie. DecoChartsPlus_D7.dpk,
   DecoChartsPlus_D2009.dpk, DecoChartsPlus_DXE.dpk, etc.).

Then you can open, compile and execute the demo projects, that can be found
in the Demos folder. Examples for an older Delphi versions are available
in the Demos\D7-D2007 folder.

Installation (using the Setup Wizard):
--------------------------------------
You can also download an automatic installer of 'DecoCharts for VCL' and let
the Setup Wizard do all the work for you.

1. Download and run the installer.
2. Follow the steps of Setup Wizard.

If you encounter any problem during the installation, please feel free to contact us.


Change log:
-----------
* Version 1.3 (2013-11-18)
  - added: Support for Delphi XE3/XE4/XE5
  - added: AntiAliasing property has been added to all "Plus" variants of DecoCharts components. It allows you to turn off the usage of GDI+ drawing
  - added: Method PrintTo has been added to DecoProgressBar and DecoRatingStars components
  - added: Properties for setting the rendering mode (RenderCompositingMode, RenderSmoothingMode, RenderTextMode) have been added to TDecoCompareGridPlus, TDecoProgressBarPlus and TDecoRatingStarsPlus components
  - fixed: Flashing of the mouse cursor (between hovered and the normal state) on Windows Vista and later when moving the mouse over TDecoCompareGrid cells

* Version 1.2.2 (2012-05-14)
  - fixed: Embarcadero Delphi XE2 compatibility issues

* Version 1.2 (2012-03-11)
  - added: TDecoCompareGrid and TDecoCompareGridPlus components
  - added: TDecoRatingStars and TDecoRatingStarsPlus components
  - added: TDecoProgressBar and TDecoProgressBarPlus components
  - fixed: a few minor fixes and improvements

* Version 1.1 (2011-09-26)
  - added: Compatibility with Delphi XE2
  - added: Automatic installer (Setup Wizard)

* Version 1.0 (2011-07-17)
  - The first release of DecoCharts components


Used third-party components and libraries:
------------------------------------------

- Embarcadero Delphi for Win32 Professional, http://www.embarcadero.com/
- GDI+ API Library (a dynamic loading implementation), http://www.progdigy.com, http://themiron.mirandaim.ru
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
Copyright (c) 2008-2013  Precision software & consulting
All rights reserved.
