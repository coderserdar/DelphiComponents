{**********************************************************}
{                                                          }
{  TinyDB Database Engine                                  }
{  Version 2.94 32/64.bit                                  }
{                                                          }
{  Author: DayDream Software                               }
{  Email: haoxg@21cn.com                                   }
{  URL: http://www.tinydb.com                              }
{  Last Modified Date: 2005-10-31 original source          }
{                                                          }
{  Refreshed and modification for unicode by Jaro Benes    }
{   26.III.2019 corrected 64-bit version                   }
{   19.XI.2009 + simple translation to simple English      }
{   27.I.2012 actualized XE2-specific by R.Cervinka        }
{   28.XII.2012 actualized XE3-by JB.                      }
{   6.V.2013 actualized XE4-by JB.                         }
{   20.IX. actualized XE5-by JB.                           }
{   10.IV. actualized up to XE8-by JB.                     }
{                                                          }
{  Last Modified Date: 2019-3-28 by JB.                    }
{**********************************************************}

Compatible for Delphi 2005/2006/2007/2009/2010/XE/XE2..XE8/Seattle/Berlin/Tokyo/Rio

Older version can be supported see into "packages" directory.

-JB
Compilable in Delphi 7 too (pack contain two version of file UTF-8 and ASCII for advanced users).

=============================
 TinyDB Engine Release Notes
=============================

CONTENTS
========

1. Overview
2. Contact Information

1. Overview
-----------

TinyDB is a small, fast and very reliable database engine for 
developers in Delphi and C++Builder applications. It provides
access to a flat file in its own format without BDE, and
doesn't need any installation and configuration. With TinyDB 
engine, all data can be stored in only one database file. 
TinyDB engine is an ideal BDE replacement for small database 
applications in Delphi and C++Builder, and soon also for Kylix.

TinyDB engine consists of TinyTable, TinyDatabase, TinySession
and TinyDB Desktop utility. TTinyTable is inherited from 
TTinyDataSet which is a direct descendant from standard TDataSet. 
It provides all the TDataSet's functions such as Filtering, 
Searching, Sorting, Blob fields and it has some advanced features 
such as database Encryption, blob fields Compression, CRC32 
protection, In-memory database and Multiple indexes.

Key features:  
 a. Inherited from TDataSet, supports all visual DB-aware components;  
 b. Without BDE, and no DLL, no OCX; 
 c. All data in ONE database file; 
 d. Supports BLOB data Compression;  
 e. Supports database Encryption, and CRC32 protection; 
 f. Supports In-memory database; 
 g. Supports Multi-indexes, Master-Detail relationship;
 h. Maximum records quantity over 100,000; 
 i. TinyDB Desktop tool, detailed manual, and abundant samples; 
 
To know more information see "Help\TinyDB.chm" file.


2. Installation notes
---------------------

Package files are placed in \Sources directory and sub-directory named
like pckg_Dxxxx where xxxx is installed version od Delphi (directly
supported Delphi 2005, 2006, 2007, 2009, 2010, XE, XE2..XE8). Others
versions of Delphi are placed in \Source\packages directory.

Installation contain two module runtime and designtime and for one
pass process is prepared the group file named TinyDB_Dxxxx (where xxxx
is installed version of Delphi, directly supported Delphi 2005, 2006,
2007, 2009, 2010, XE, XE2..XE8) as groupproj or bdsgroup type.

Installation:

 a) Remove older version from IDE Delphi and clean directory of source

 b) Create directory under name like "TinyDB" and de-archive all source into
 
 c) Start your preferred version of Delphi and open group in \Source\pckg_Dxxxx
 
 d) Build all modules and install into IDE module dclTinyDBxxxx.dpk