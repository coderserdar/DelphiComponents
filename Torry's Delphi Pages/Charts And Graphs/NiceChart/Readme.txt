NiceChart v1.00
© 2007, Priyatna
Bandung - Indonesia

http://www.priyatna.org/
mailto:me@priyatna.org

Introduction
License
Installation
Methods
Properties
Contacts

Version: 1.00
Release Date: May 26th, 2007

 

Introduction

Once I need a chart component that can shows interpolated lines for one of my project. I don't need a sophisticated full-featured chart, I just need a simple one with basic functionalities. Don't find a way to do it with standard Delphi TChart component, I decided to write my own.

Currently it has three graph styles: lines, smooth lines with B-Splines, and bars. It has multilined title, legend, automatic y-axis, and supports multiple series. It also can produce a monochrome version of the graph using monochromatic brushes. Those features are sufficient for my programming needs.

I learned a lot from creating this component. I learned the algorithm to pick axis value range that are aligned to zero from an arbitrary set of values. I also learned some linear interpolation algoritms, although currently only B-splines that is accomodated in this component. And finally I understand why there's only a handful chart component out there.

 


License

This library is released under Mozilla Public License. You can use it in your freeware, shareware or commercial softwares. You can send your modification to me, and if I decide to include it in the main distribution, I will add your name as a contributor. You can read full licensing information here.

 

Installation

There is no special process to install it on Delphi. Just open NiceChartD7.dpk on Delphi IDE and press Install button. I use Delphi 7. If you use another Delphi version, you may have to make some minor changes.

The component will appear in priyatna.org tab.

 


Methods

There are some main methods of the component:

procedure BeginUpdate;
Call this method to prevent control updating its content. This is useful when you want to add bulky data to the chart. Don't forget to call EndUpdate when you are done supplying data.

procedure EndUpdate;
Call this method to reenable update. You must call BeginUpdate prior to call this method.

function AddSeries(AKind: TSeriesKind): TNiceSeries;
Add a new series to the chart. A series can be a bar, line or smooth line type.

function ClientToChart(const X, Y: Integer; var AX, AY: Double): Boolean;
Call this function to get X-axis and Y-axis value from mouse position. The X and Y parameters are the mouse position, and the corresponding X-axis and Y-axis values are stored in AX and AY variables. This function returns True if the supplied mouse position is within the X-axis and Y-axis range. Otherwise, it returns False.

procedure RemoveSeries(ASeries: TNiceSeries);
Removes a series from the chart.

procedure Clear;
Removes all series from the chart.

function CreateMetafile: TMetafile;
Creates metafile (WMF) from the chart. This function is useful when you want to save the content of the chart to a file.

procedure CopyToClipboard;
Copies the content of the chart to the clipboard. The content can be pasted in any application as a graphic image.
 


Properties

There are some main properties of the component:

property Series[Index: Integer]: TNiceSeries;
A collection of series that is contained in the chart.

property SeriesCount: Integer;
Specifies how many series are in the chart.

property ShowLegend: Boolean;
Shows or hides legend. Set to True to show the legend, or set to False to hide it.

property ShowTitle: Boolean;
Shows or hides title. Set to True to show the title, or set to False to hide it.

property ShowXGrid: Boolean;
Shows or hides horizontal grid. Set to True to show the gris, or set to False to hide it.

property ShowYGrid: Boolean;
Shows or hides vertical grid. Set to True to show the gris, or set to False to hide it.

property Title: string;
Specifies title of the chart. You can use a multilined title separated by #13 (carriage return).

property TitleFont: TFont;
Specifies title font name, size, color and styles (bold, italic, underline or strike-out).

property AxisXTitle: string;
Specifies horizontal axis title.

property AxisYTitle: string;
Specifies vertical axis title.

property AxisXOnePerValue: Boolean;
Forces the chart to treat each x-axis value as a different value, so all point or bar can be viewed even though they have same x value.

property AxisXScale: Single;
You can scale the horizontal axis so it shows different values than the original data. This is useful, for example, if you want to supply the data in real values, but want to show the axis in thousands. In this case, simply set this property to 1000.

property AxisYScale: Single;
Same as the above, but this is for vertical axis.

property Monochrome: Boolean;
If set to True, the chart will be drawn in monochrome, using different brush pattern for bars. Otherwise, it will be drawn in color. Colors and brush patterns for each series are automatically choosen by the component.
 


Contacts

Don't hesitate to report any bug or whish to me:
Priyatna
Bandung - Indonesia

http://www.priyatna.org/
mailto:me@priyatna.org