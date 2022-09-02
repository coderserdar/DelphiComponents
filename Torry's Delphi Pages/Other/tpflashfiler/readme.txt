TurboPower FlashFiler 2


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  FlashFiler Explorer functionality
5.  String resources
6.  Version history
6.1   Release 2.13

==============================================


1. Introduction

<TODO>

This is a source-only release of TurboPower FlashFiler 2. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6.

For help files and a PDF manual, please see the tpflashfiler_docs
package on SourceForge (http://sourceforge.net/projects/tpflashfiler).

For precompiled binaries of FlashFiler Server and the other FlashFiler
utilities, please see the tpflashfiler_bin package on SourceForge (
http://sourceforge.net/projects/tpflashfiler).

==============================================

2. Package names


TurboPower FlashFiler 2 package names have the following form:

  FF2MKVV.*
     |||
     ||+------ VV  VCL version (30=Delphi 3, 35=C++Builder 3, 70=Delphi 7)
     |+------- K   Kind of package (R=runtime, D=designtime)
     +-------- M   Product-specific modifier, typically an underscore
     
For example, the FlashFiler 2 runtime package files for Delphi 7 have
the filename FF2_R70.*.

The runtime package contains the core functionality of the product and
is not installed into the IDE. The designtime package references the
runtime package, registers the components, and contains property
editors used in the IDE.

==============================================

3. Installation


To install TurboPower FlashFiler 2 into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\ff2).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\ff2\source) to the IDE's
     library path.

  4. Open & compile the runtime package specific to the IDE being
     used.

  5. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

==============================================

4.  FlashFiler Explorer functionality

The CSV Import functionality was copyrighted by another party and
permission was given to TurboPower to use the functionality only in
the commercial version of FlashFiler 2.. That functionality has been
removed from the open source distribution of FlashFiler Explorer.

==============================================

5. String resources

Most of FlashFiler's error messages are stored in string resource
files having the extension STR. If you change these files, you must
recompile them using the TurboPower String Resource Manager located at
http://sourceforge.net/projects/tpsrmgr

==============================================

6. Version history


6.1 Release 2.13

  Please note that the following issue #s are from Bugzilla. These
  bugs were not exported to SourceForge.


 Enhancements
 ------------
 
 4043 - Allow FFCheckValToString to return ref number for BLOB fields 
 4112 - Restructure: Support conversion of strings to integers 
 
 Bugs fixed
 ----------
 
 3403 - Unable to change string field to blob memo field in restructure
 3870 - BDE2FF, invisible target database
 3915 - INSERT: Value of string field truncated to 255 chars
 4003 - Params do not support BLOB field type
 4025 - AutoInc field in SQL result set should be returned as type fftAutoInc
 4028 - Incorrect count() value with LEFT OUTER JOIN
 4029 - Field alias not resolved in WHERE clause
 4030 - When not waiting for a reply, Legacy transport no longer raises lost
        connection event in 2.12
 4031 - Result set does not contain values for subselect
 4032 - If TffDatabase closed before child TffQuery then AV may occur in
        SQL engine
 4037 - Index count not updated if use Table.AddIndex
 4038 - '<>' operator does not take NULL values into consideration
 4039 - Query returns empty result set
 4042 - Server should not prompt for password during auto-start-up
 4046 - Table modified by INSERT/UPDATE/DELETE may be closed prior to
        transaction commit
 4048 - StartTransactionWith error handling may return incorrect result
 4061 - TffClient does not use registry server name for explicit transport
        components
 4070 - "select field, count(*)" returns zero for count
 4077 - Service should use recovery engine that does not require user interface
 4084 - Restored connection re-opens query before preparing the query
 4095 - INSERT should validate column types & number of source columns
 4107 - TffServerCommandHandler.nmDatabaseAddAlias has incorrect format string
 4126 - Cursors pending close may not be freed at end of transaction
 4140 - Pack incorrectly increments NextFlushPoint
 4143 - SQL engine raises 'Not found' error on nested join
 4144 - Server UI Avs if reset counters or right click on transport when server
        is down
 4160 - Calling BLOBWrite followed by BLOBTruncate can eventually lead to
        corupted BLOB
 4167 - Initial sorting for DISTINCT should be case-sensitive
 4168 - SQL: SUM(x)/(2) does not give same result as SUM(x)/2
