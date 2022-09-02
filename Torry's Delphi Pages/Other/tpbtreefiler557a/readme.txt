TurboPower B-Tree Filer


Table of contents

1.    Introduction
2.    Package names
3.    Installation
4.    Version history
4.1   Release 5.55
4.2   Release 5.56
4.3   Release 5.57
4.4   Release 5.57a

==============================================


1. Introduction


B-Tree Filer is a fast library of file-based database routines for
Borland Turbo Pascal & Delphi. B-Tree Filer supports stand-alone
programs or those running on Microsoft-compatible networks including
Novell Netware.

This is a source-only release of TurboPower B-Tree Filer. It includes
designtime and runtime packages for Delphi 3 through 5.

==============================================

2. Package names


TurboPower B-Tree Filer package names have the following form:

  TNNN_KVV.*
   |   ||
   |   |+------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 50=Delphi 5)
   |   +------- K   Kind of package (R=runtime, D=designtime)
   |
   +----------- NNN Product version number (e.g., 555=version 5.55)

For example, the B-Tree Filer runtime package files for Delphi 5 have
the filename T555_R50.*.

The runtime package contains the core functionality of the product and
is not installed into the IDE. The designtime package references the
runtime package, registers the components, and contains any property
editors used in the IDE.

==============================================

3. Installation


To install TurboPower B-Tree Filer into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\btreefiler).

  2. Start Delphi.

  3. Add the source subdirectory (e.g., d:\btreefiler\source) to the
     IDE's library path.

  4. Open & compile the runtime package specific to the IDE being
     used.

  5. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

==============================================

4. Version history


4.1 Release 5.55

New Features and Changes
----------------------------------------------------------------------
This version of B-Tree Filer has been released for two main reasons:

   1. Delphi 5.0 compatibility (core units only)
   2. Minor bug fixes and other sundry items.

1. Delphi 5.0 compatibility
---------------------------

The core B-Tree Filer units will now compile with the Delphi 5
compiler, in the same manner as the support for Delphi 2, Delphi 3,
and Delphi 4.


2. Minor bug fixes and other sundry items
-----------------------------------------

Bugs fixed
----------
- The FILER unit would cause a crash on program termination with
  Delphi 1 and BP7.
- B-Tree Filer would not compile is ASCIIZeroKeys was set.
- The UnpackXXX routines in the NUMKEYS and NUMKEY32 units would
  sometimes give an invalid result.
- The Delphi 4 and Delphi 5 warnings about promoting integer types
  in the NUMKEY32 unit have been solved.

Sundry
------
- Code changes for Delphi 5 support


4.2 Release 5.56

New Features and Changes
----------------------------------------------------------------------
This version of B-Tree Filer has been released for one main reason:

   1. Delphi 6.0 and 7.0 compatibility (core units only)


1. Delphi 6.0 and 7.0 compatibility
---------------------------

The core B-Tree Filer units will now compile with the Delphi 6 and 7
compiler, in the same manner as the support for Delphi 2, Delphi 3,
Delphi 4, and Delphi 5.


4.3 Release 5.57

New Features and Changes
----------------------------------------------------------------------
This version of B-Tree Filer has been released for two main reasons:

   1. Delphi 2005 for Win32 compatibility (core units only)
   2. Minor bug fixes and feature enhancements


1. Delphi 2005 for Win32 compatibility
--------------------------------------

The core B-Tree Filer units will now compile with the Delphi 2005
compiler (Win32 only), in the same manner as the support for Delphi
versions 2 through 7.

2. Minor bug fixes and feature enhancements
-------------------------------------------

Bugs fixed
----------
- When compiled in 16-bit mode (Borland Pascal or Delphi 1), the first
  call to IsamDelay would delay for 80 ms regardless of the requested
  delay time.

Feature enhancements
--------------------
- Mapped DOS error 53 ("The network path was not found") to IsamError
  9900 ("Invalid path name"), which is a better description of the
  error than the misleading IsamError 10355 ("A lock prevents the
  operation") that was previously returned.
- Added mouse wheel scrolling to the TFvcBrowser (Delphi 4 and above
  only).

4.4 Release 5.57a

New Features and Changes
----------------------------------------------------------------------
This version of B-Tree Filer has been released for one main reason:

   1. Delphi 2006 for Win32 compatibility (core units only)


1. Delphi 2006 for Win32 compatibility
--------------------------------------

The core B-Tree Filer units will now compile with the Delphi 2006
compiler (Win32 only), in the same manner as the support for Delphi
versions 2 through 7 and 2005.

