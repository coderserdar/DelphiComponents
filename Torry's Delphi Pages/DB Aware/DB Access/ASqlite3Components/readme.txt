Author:       Albert Drent
Description:  SQLite DataSet class (encapsulates the Delphi DataSet Class)
Creation:     November 2003
Version:      See source for details.
EMail:        a.drent@aducom.com (www.aducom.com/SQLite)
Support:      support@aducom.com (www.aducom.com/SQLite)
              Please register to the forum on www.aducom.com/sqlite. Here you're questions will be 
              answered. Do not send mail unnoticed to me or support as our filters will intercept 
              and delete. If needed, we will ask you to send us sources / samples etc. to our support 
              center. 

Release 2006.03.C

Legal issues: See licence.txt

Installation

D4/D5 users:
Within the source of ASGSQLite3.pas there are some switches needed to get the source going for D4 and/of D5. 

// Disable this for ignoring IProvider interface (for D4)
{$DEFINE IPROVIDER}

// Enable this for compilation under D4/D5 
{.$DEFINE D45}


For other users the package should install just fine. This is a VCL package, it does NOT compile with .Net environments. 
             
New installation:

For Delphi 4 and 5 the suffix D4 and D5 are used. Unpack the downloaded zipfile in a subdirectory of your choice. Within Delphi open the required ASQLite package. Choose 'install' to install the components. Don't forget to add the binary folder to the environment search path.

There are two packages, (the second one will by automatically included, so do not install this one!). ASQLite (ASQLiteD4, ASQLiteD5) is the package containing the design-time components. the ASQLitePkg (D4/D5) package contains the runtime package. This is the one to distribute if you are not compiling your projects including the packages. For Borland C++ there are BPK files too. Installation in the similar way.

Upgrade:

Please recompile both packages.

Static linking.

In ASGSqlite set the correct declaration:
// enable this if you want to link the SQLite library statically. (No need for dll)
{.$DEFINE SQLite_Static}

The most actual obj files needed for linking can be downloaded from the download section of www.aducom.com
Change the first obj reference in the source to the correct obj, currently {$L 'OBJ\sqlite3_3_4.obj'}

The website of Aducom SOftware is bilangual. The sqlite components and documentation will be found on the 
English section of the website (select 'english' in the language pull-down).

 
			