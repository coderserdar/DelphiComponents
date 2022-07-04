===============================
   Data Master 2003 VCL  
===============================


Table of contents
===============================

  1. Introduction
  2. Installation
  3. Terms of use


  Introduction
  ============

Data Master 2003 VCL component library is a part 
of Data Master 2003 project. Primary purpose of 
this library is to provide reusable, high 
performance 2D graphic engine specially optimized 
for scientific and technical applications.

In addition to the graphics-related components,
this library includes property editors and other
VCL components used by DM2003:

AxisDlg.pas		axis properties form
DMComboBox.pas		combo for property editors
DMContainer.pas		data container component
DMFloatEdit.pas		float number spineditor
DMHTMLText.pas		HTML-compatible rich text
DMMemo.pas		safely dockable memo
DMPlot.pas		graphic engine core
DMPlotEdit.pas		property editors
DMSpeedButton.pas	MDI system buttons
DMUserAction.pas	action providers support
DMUserActionEditor.pas	property editors
DMWebBrowser.pas	specialized TWebBrowser
DMWebIniFile.pas	can download INI files
DMWorksheet.pas		worksheet viewer
HTextDlg.pas		rich text editor dialog
SerieDlg.pas		series properties form
TextDlg.pas		used internally by DMComboBox

See online documentation (a file named library.hlp) 
and demonstration projects for more details. 


  Installation
  ============

This software was tested for compatibility with 
Delphi 7 and Delphi 2006 (Win32). However, I think 
it may be compiled with minor changes by other 32 
bit versions of Delphi. The package included in the 
archive has Delphi 7 format. You may create another 
package with different set of components (keep in 
mind that some units depend on others).

Data Master 2003 VCL are partially compatible with 
their previous version - DM2000 VCL components. 
However, not all features are supported: the major 
difference between DM2000 and DM2003 versions is 
design-time expressions evaluation. For more 
flexibility, in the new version axes and series 
expressions may be evaluated only at run-time by 
the external expression parser (see PlotDemo sample). 

Notice that as of version 11.8.0.342, TPlotLabel 
object was almost completely rewritten, so you should
properly test all your dependent code for possible 
compatibility issues.

You don't need to install the package in order to
compile demonstration projects. Copy the whole 
directory structure into some temporary folder
and use Delphi command line compiler:
dcc32 WksDemo.dpr
dcc32 PlotDemo.dpr
dcc32 UpdateDemo.dpr


  Terms of use
  ============

1. Copyright

Data Master 2003 VCL created by Alex Pronin aka RRR.
(c) 1993-2009 RRR. All rights reserved.

2. License Agreement

This software may be legally used under the 
terms and conditions of the Mozilla Public License 
(MPL). You may obtain a copy of this license at 
http://www.mozilla.org/MPL/MPL-1.1.html.

3. Disclaimer of Warranty

THIS SOFTWARE DISTRIBUTED "AS IS" WITHOUT 
WARRANTIES AS TO PERFORMANCE OR FITNESS 
FOR ANY PARTICULAR PURPOSE OR ANY OTHER 
WARRANTIES WHETHER EXPRESSED OR IMPLIED. 
YOU USE THIS SOFTWARE AT YOUR OWN RISK. 
NOBODY WILL BE LIABLE FOR DATA LOSS, 
DAMAGES, LOSS OF PROFITS OR ANY OTHER 
KIND OF LOSS WHILE USING OR MISUSING 
THIS SOFTWARE.

4. Distribution

Data Master 2003 VCL is free software. 

You may install and use unlimited number of 
copies of the software. You may distribute 
this software in its unmodified form as long 
as each copy that you make and distribute 
contains this license agreement and the same 
copyright and other proprietary notices 
pertaining to this software that appear in the 
software. There is no charge for any of the above.


===============================
Thank you for using Data Master 2003 VCL. 

Feel free to contact me by e-mail: 
rrr4@hotmail.com?subject=DM2000
  or
support@datamaster2003.com?subject=DM2003

Visit DM2003 home page for more information:
http://www.datamaster2003.com
