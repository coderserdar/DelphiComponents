Trend Component - Version 2.03

Added Properties:

About			version and author info
BackGroundColor		color of the background
GridStyle		style of the grid:
				gsNone - no grid
				gsXGrid - grid for X values
				gsYGrid - grid for Y values
				gsFull - grid for X and Y values
GridColor		color of the grid lines
GridXstep		distance of X grid lines (must be greater than 0)
GridYstep		distance of Y grid lines (must be greater than 0)
AutoScale		scale Y axis automatically (not valid for demo mode)

Added options:

toTransparent		Values draw transparent 
toRotated		Values vertical (only TrueType or Adobe PS, not for .fon Fonts)
toNoZeros		do not display values = 0 when toValues in Options


Additional:

Some more error checking and additional fixing, mostly marked by // jbk 


Installation:

Install	JBKTrendD5.dpk		Delphi 5

	JBKTrendD6.dpk		Delphi 6  Runtime Library
	DclJBKTrendD6.dpk	Delphi 6  Designtime Library

	JBKTrendD7.dpk		Delphi 7  Runtime Library
	DclJBKTrendD7.dpk	Delphi 7  Designtime Library

	JBKTrendD9.dpk		Delphi 2005  Runtime Library
	DclJBKTrendD9.dpk	Delphi 2005  Designtime Library


Version:

2.01	split packages for Delphi 6 into runtime and designtime
	should be compatible with Delphi 7 too.

2.02	made compatible with Delphi 7
	use jedi.inc for compiler directives
	features unchanged

2.03	made compatible with Delphi 2005
	Some changes by Bart Michel in Paint procedure.
	Font.Name, Font.Size, Font.Style and Font.Color now used.
	Added options: toTransparent, toRotated, toNoZeros

Trend Component - additional information. 
-----------------------------------------
The Trend component is a graphical component designed to show 
an animated plot of parameter values. As values are added old values 
scroll off the graph to the left. The trend can be viewed in a number 
of ways 
 
      tsBar       - Each value is represented as a rectangle 
      tsLine      - A line is drawn connecting each value 
      tsScatter   - A small rectangle is drawn at each value 
      ts3D        - Each value is represented by a 3D rectangle 
      tsFilled    - As tsLine with the area below the line filled 
 
A few options can be applied 

      toValues    - On tsBar and ts3D text values are added to each column. 
      toTwin      - Two values are displayed for each slot 
 
One area where the toTwin option is useful is when monitoring systems 
with demand and feedback values. Here it is important to know that a 
given command is tracked accuratley. Differences between command and 
feedback are especially well highlighted by using ts3D and toTwin as 
shown in the TrendTwinDemo program. 

Before you can compile the demo programs you need to install the 
component trend.pas in the usual way.

To see what the component does without installing it just run the 
.exe demo files included in the demo archive.

Two example programs are included with source.

How to use it.
--------------
Once installed the component appears on a new tab called 'Mark'. It
is based on a standard TPanel component so inherits all of TPanel's properties.
The new things it creates are as follows :-

Methods.
--------
Add( Integer )     - Adds the specified value to the graph on the right,
                     shifting the previous values to the left.
AddBase( Integer ) - Adds the specified value to the Base values on the right,
                     shifting the previous values to the left. Base values are
                     only used when the option toTwin is true.
Clear              - Set all values to zero.
ClearBase          - Set all Base values to zero.
Get( Integer )     - Return the value at the specified position.
GetBase( Integer ) - Return the specified base value at the specified position.
Demo               - Each time you invoke this a new random point is added to 
                     the graph.

Properties.
-----------
Divisions          - The number of points in the graph. (2 to 100)
ColorBase          - The colour to use for Base values in toTwin mode.
MinVal             - The minimum value to display.
MaxVal             - The maximum value to display.
Options		   - toValues  to display text values of tsBar and ts3D
                     toTwin    to display twin data sets
Style              - tsBar     to display as 2d barchart
                     tsLine    to display as lines
                     ts3D      to display as 3d barchart
                     tsFilled  to display as a line filled underneath
                     tsScatter to display as points

Archive file details.
---------------------
trend.pas                           - Component source file
trend.dcr                           - Component toolbar image
trend.txt                           - This file

A demo of the toTwin option
TrendTwinDemo\TrendTwinDemoProj.exe - Ready built demo
TrendTwinDemo\TrendTwinDemoProj.dpr - Project file for building demo
TrendTwinDemo\TrendTwinDemo.pas     - Source
TrendTwinDemo\TrendTwinDemo.dfm     - Form info

A general demo of trend types
TrendDemo\TrendDemoProj.exe         - Ready built demo
TrendDemo\TrendDemoProj.dpr         - Project file for building demo
TrendDemo\TrendDemo.pas             - Source
TrendDemo\TrendDemo.dfm             - Form info

---------------------------------------------------------------------------------
Author  - Mark Dodson
Date	- 4th July 1997
email	- MarkDodson@aol.com 
---------------------------------------------------------------------------------