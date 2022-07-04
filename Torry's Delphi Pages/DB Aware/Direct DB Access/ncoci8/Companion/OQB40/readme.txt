Integration of NCOCI8 server with Sergey Orlik Open QBuilder.
            last modified 22-may-2000
---------------------------------------------------

This directory contains TOQBEngineNCOCI8 component.
It is database engine for Sergey Orlik Open QBuilder.
Also, there is file QBuilder.pas. It is adapted to
Oracle OQB4. So, after download of original distributive,
replace QBuilder.pas with supplied.

You can find Open QBuilder at: 
  http://www.geocities.com/SiliconValley/Way/9006/index.html
  
=====================================================================
                Installation
=====================================================================
To use OQB & NCOCI8 you should do following steps:
1) Download OQB from Sergey Orlik site (http://www.geocities.com/SiliconValley/Way/9006/index.html)
2) Extract files from OQB archive into some directory. Let say c:\OQB
3) Copy files from ncoci8_companion archive into some directory. Let say c:\ncoci8\companion.
4) Copy file c:\ncoci8\companion\oqb40\QBuilder.pas into c:\OQB\Source (overwrite existing one !).
5) Install OQB package (c:\OQB\Source) into Delphi
6) Install doqbNCOCI850.dpk (c:\ncoci8\companion\oqb40\source) package into Delphi
7) Add to library path:
c:\OQB\Source
c:\ncoci8\companion\oqb40\source

Now you can run Demo ! (c:\ncoci8\Companion\OQB40\Demo)


=====================================================================
                The Open QBuilder for Delphi and C++Builder
=====================================================================

The Open QBuilder (OQBuilder) is a simple visual query builder component set
for Delphi and C++Builder developers. 
Open QBuilder is the next generation of well-known QBuilder.

---------------------------------------------------
(c), 1999-2001, Dmitry Arefiev
http://www.da-soft.com