Holy (Holey) Component Batman!
The THole component

by matt tagliaferi
matt.tagliaferri@pcohio.com
Compuserve 102443,1240

This is my first crack at component writing.  It draws a 3D-looking
hole on a form.  There are many properties for controlling the look
of the hole.  In addition, you can use the hole to clip any
other drawing operations on the form.  Other than that, you can
do very little else with it.

Enclosed:

HOLE.DCU  component
HOLE.PAS  source

HOLETEST.PAS  test form
HTEST.DRP     project using HOLETEST above


{-------------------------------------------}
Properties of note:

BackColor: the color behind the hole. (usually set to color of form)

EdgeColor: the color of the 'bevel'

EdgeDirection:  Determines the direction the bevel edge protrudes.

Edgewidth: The number of pixels (in the x and y direction) that each
   bevel point is offset from the original point.  For example, if
   EdgeDirection is hdDownLeft, each bevel pixel is <-Edgewidth,
   -EdgeWidth> away its corresponding hole point.  If the direction
   is hdUp, each bevel point is <0, EdgeWidth> away

HoleColor: the color inside the hole.

HoleShape: Either hsPerfect, which will create each point around
   an ellipse that encloses the size of the component, or hsRandom,
   which will offset each point slightly by shortening it a small
   distance.  This gives the hole a 'jagged' effect, which is better
   for showing off the clipping.

Outlinecolor: Color used to draw the outline around each bevel edge,
   as well as the hole itself.

Points:  The number of points used to make up the hole.  Values range
   from 3 - 100.

{-------------------------------------------}

Methods of Note:

ClippingOn:  Allows the hole to be used as a Clipping region for the
  hole's parent.  Note that only 1 hole can be used as a Clipping
  region at any one time.  (It would be a killer exercise to allow
  multiple holes act as a clipping region together).

ClippingOff:  Turns off the clipping for the hole.

{-------------------------------------------}

Programming notes: interesting things I discovered.


THole makes extensive use of the Windows API call SelectClipRgn,
which clips drawing of a DC to a windows region.  There are
three regions created by the hole: one defined by the outer points,
one defined by the inner (bevel) points, and a third defined by
AND-ing those two regions together.  This third region is the one
turned on/off by the methods ClippingOn/Off.  I found SelectClipRgn
interesting because unlike many other API calls, SelectClipRgn
makes a COPY of the region in question before selecting it into
the device context.  This means that you don't have to save the old
Clipping region and select it back in, like you do with Pens and
Brushes.  When you're done using it, you just say SelectClipRgn(DC, 0);

I had some trouble figuring out what Windows messages were called 
when sizing a TGraphicContol in the design environment.  WM_SIZE
is NOT sent, I'm assuming because TGraphicContols are not Windowed
controls.  After a session with WinSight, I discovered I could trap
WM_WINDOWPOSCHANGED and this would do most of what I wanted it to.
The only drawback is that my hole's points get regenerated when you
merely MOVE the component, which isn't necessary: I would rather only
regenerate points when the component gets reSIZED.

{-------------------------------------------}

If you find any use for this component, you're a better man than I!
:) Seriously, if you find something cool to use it on (screen savers
come to mind), feel free to use it.  

Feel free to write me with any questions or comments.

-tag  

-> <-
_____
  U
