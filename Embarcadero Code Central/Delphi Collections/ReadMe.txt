Readme
Compiling
The build scripts use the Delphi command line compiler and the Microsoft Help Workshop, which are included with Delphi.

First check the following line in buildD5.bat or buildD7.bat as appropriate.

SET DELPHIBASE=C:\PROGRAM FILES\BORLAND\DELPHI5

This is needed to locate the Delphi compiler and the help compiler.

Run buildD5.bat or buildD7.bat as appropriate.

Installing
Installing Delphi Collections into a project only requires adding the package name to the run-time package list.


If Build with runtime packages must be unchecked, then the Library path of the Environment Options menu must contain the library source code directory.

Testing
A test harness is included and is compiled with the package and help files.  In the Test subdirectory, run Test_D5.exe or Test_D7.exe

 as appropriate.  Select a class name, select the type of test and press the Run functional test, Run performance test or Run memory leak test button as appropriate.


Note that if the test harnesses are run within a debugger (such as the Delphi IDE), exceptions will be detected.  These are the result of functional tests, checking for exceptions thrown for inappropriate use of the collection, and is correct behaviour.
Documentation
The class diagram file, Design.vsd

, is a Visio 2002 document but it is not necessary to view it.  The class relationships are simple and obvious from the help file but a Visio viewer is available from http://download.microsoft.com/download/VisioStandard2002/vviewer/2002/W98NT42KMeXP/EN-US/vviewer.exe .
Changes
v1.0.4


Memory leak in ContainsKey function of TAbstractStringMap and TAbstractIntegerMap fixed.
v1.0.3


Memory leak test added to test harness.


Memory leak when creating maps, integer maps and string maps fixed.
New Capacity

 property defined in ICollection and implemented in all collections.
v1.0.2


Expanded introduction and quick start sections in help file.
v1.0.1


Packages and test harness no longer list unused packages.