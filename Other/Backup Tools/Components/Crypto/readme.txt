******************************************************
CRYPTOCard Corporation
http://www.crpytocard.com
Carleton Place, ON CANADA

Crypto Library Contact: Greg Carter, 
gregc@cryptocard.com
******************************************************

Just what are these pascal routines anyways?

Industrial Strength Encryption and Hashing routines
for BP7 and Delphi compilers.  If you need to
protect data in your applications, then this
is a good place to start.  These are NOT the simple
XOR routines found in most programming books.  These
are implementations of the best cryptographic algorithms
available today.  


Why did CRYPTOCard place the code on the net?

With these routines, a Delphi developer has the 
building blocks necessary to produce applications
which integrate high levels of data protection and
verification.  This is particularly important to 
developers who are targeting the Internet, where
privacy and data integrity are becoming very
desirable features.

Until the release of this library, Delphi Developers
did not have the luxury that C developers had, when
it came to encryption code(there is a wealth of C 
implementations available for cryptographic code on
the net), and would be forced to use C DLLs.  These
had the disadvantage of the having to pass Keys
between EXE and DLL in the clear.  These routines
eliminate this, and encapsulate the functions in an
easy to use Delphi Component.  Just drag and Drop.


******************INSTALLATION*********************

There are four directories in the zip file:
demo16 - Contains test project for Delphi1.0
demo32 - Contains test project for Delphi2.0
rangen - A Random Number generator, which can be used
         to generate Keys.  It uses MD5.  The project
         compiles to a DLL
units  - the source code to all the routines.
       - desunit2.pas
       - md5unit.pas
       - rc4unit.pas
       - rc5unit.pas
       - ideaunit.pas
       - cryptcon.pas - base class
       - crypto.hlp - Help file for library
       - crypto.kwf - KeyWord file for component Help
       - cryptdef.inc - include file
As Well as the *.dcr(16bit version only)files for displaying on the Delphi
toolbar after installation. 

To install:
Help Install:
You may wish to move the crypto.hlp file to your Delphi\bin directory.
Then place the crypto.kwf into the Delphi\help directory.  Then with
Delphi NOT running, start the program HelpInst, Open the file 
'Delphi.hdx'(it is in your Delphi\bin dir).  Then choose 'Add' file,
add the crypto.kwf file (should be in your Delphi\help dir).  Choose
save.  Exit the HelpInst program.

Unzip the contents of the UNITS directory to the directory
where you place all your other 3rd party components (ie 
c:\delphi\compnt\lib) or if using Delphi 2.0 unzip to their
own directory(ie c:\program files\Borland\Delphi 2.0\Components\Crypto)

After you have unzipped the files, start Delphi choose
Options|Install Components, add the files
       - desunit2.pas
       - md5unit.pas
       - rc4unit.pas
       - rc5unit.pas
       - ideaunit.pas
to the component list.  Select ok, after the Delphi component library  
rebuilds you should have a new page 'Crypto' with the components on
it.

unzip the demo directory to Delphi\Demos\Crypt\ (or similar)
Do NOT put both (16&32) demo projects in the same directory.


NOTE: For Delphi 2.0 users, David R Michael <DavidRM@AOL.com> has done
a nice job of converting the 16bit dcr files to 32bit dcr files, he
has also update the bitmap files.  They look much for professional then
my attempts.

Look at the help file and source for examples!
Have Fun, Encrypt, and be Happy!

Restrictions
You may use these routines in any of your own applications.  No fee
is required for this code, although some of the algorithms are 
COPYRIGHT and PATENT protected! and MAY require you to pay the 
patent holds license fees for using them.  Contact information
is provided in the help file.  You may NOT redistribute these in
any developer component(Delphi Component, VBX, C DLL) and CHARGE
for the component.  Any derived components must also be placed
into the public domain.

Questions, Comments, Suggestions:
gregc@crpytocard.com
******************************************************************
PS These routines are meant only as a starting block, much more
work needs to be done to make a truely complete encryption library
for Delphi.  These components were put into the public domain, with
hopes that others may contribute to creating the ultimate 
cryptographic library for Delphi.  
******************************************************************