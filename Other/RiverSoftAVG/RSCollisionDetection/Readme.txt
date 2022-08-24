TRSCollisionMap Readme

In this readme, we discuss performing 2D collision detection using the TRSCollisionMap class, a free class with source code (available at www.RiverSoftAVG.com/Files/RSCollisionDetection.zip ).  For a certain class of problems, this class provides easy and fast collision detection for 2D games.

We certainly don't claim this class is perfect, or that there are not better methods.  But for a certain class of problems, this class has the advantage of being simple to use and accurate.  Plus, it's free :-)

What kind of games is TRSCollisionMap good for?

The TRSCollisionMap is really optimized for 2D Level, or platformer-type, games with not very many moving objects.  The old overhead type games, like tank battle with buildings and a couple moving tanks (and bullets), are perfect for it.  Some other examples would be BreakOut, Jumpman, Space Taxi, etc.  The levels should not be too large either as this can consume a lot of memory.  The perfect type of games would have a setup period where you could initialize the collision map and not too many moving objects.

How does it work?

The TRSCollisionMap object works like an image map for objects.  You basically build a map by drawing your objects “colored,” not by their images, but by their indices (or pointers) that reference them.  For example, suppose your world was a 10x10 map.  You have two game objects.  You would build your map where every pixel is 0 (no object), 1 (object 1) or 2 (object 2).  So, assuming object 1 has dimensions of (left = 2, top =2, width =3, and height = 3) and object 2 has dimensions of (left = 6, top = 4, width = 2, and height = 4), your collision map would look like this:

0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0
0	0	1	1	1	0	0	0	0	0
0	0	1	1	1	0	0	0	0	0
0	0	1	1	1	0	2	2	0	0
0	0	0	0	0	0	2	2	0	0
0	0	0	0	0	0	2	2	0	0
0	0	0	0	0	0	2	2	0	0
0	0	0	0	0	0	0	0	0	0
0	0	0	0	0	0	0	0	0	0

Now, a third moving object can easily detect collisions by checking its locations on the map.  The collision map provides point, bounding rectangle, and bounding ellipse checks.  If the third object is a 2x2 object at (8, 4), you would call the IsCollision method to determine if there was a collision.  In this case, there would be no collision.  If the third object moved left one (to 7, 4), calling IsCollision would return true and the point of collision.

How do you use it?

Please see the CollisionMapExampleMain.pas


This software is freeware.  You are authorized to duplicate and modify this software.  No guarantees or warranties.

Copyright © 2004, Thomas G. Grubb.