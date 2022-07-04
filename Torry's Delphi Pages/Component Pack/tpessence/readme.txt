TurboPower Essentials


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 1.11

==============================================


1. Introduction


Essentials contains 13 native VCL controls for Borland Delphi &
C++Builder. The controls include drop-down calendars & calculators,
roll-up dialogs, 3-D labels, tiled backgrounds, scrolling messages,
menu buttons, and more.

This is a source-only release of TurboPower Essentials. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6.

==============================================

2. Package names


TurboPower Essentials package names have the following form:

  ENNN_KVV.*
   |   ||
   |   |+------ VV  VCL version (30=Delphi 3, 35=C++Builder 3, 40=Delphi 4)
   |   +------- K   Kind of package (R=runtime, D=designtime)
   |
   +----------- NNN Product version number (e.g., 111=version 1.11)

For example, the Essentials runtime package files for Delphi 7 have
the filename E111_D70.*.

The runtime package contains the core functionality of the product and
is not installed into the IDE. The designtime package references the
runtime package, registers the components, and contains property
editors used in the IDE.

==============================================

3. Installation


To install TurboPower Essentials into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\essence).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\essence\source) to the
     IDE's library path.

  4. Open & compile the runtime package specific to the IDE being
     used.

  5. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

==============================================

4. Version history


4.1 Release 1.11

    Enhancements
    -------------------------------------------------------------
    ALL       - Added Delphi 7 compiler support
