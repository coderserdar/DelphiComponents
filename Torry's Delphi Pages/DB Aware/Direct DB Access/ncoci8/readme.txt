+-----------------------------------------------+
|ALL OFFERS, REMARKS, REPORTS, ETC WELCOME !    |
+-----------------------------------------------+

NCOCI8 Library, V 1.0.6
================================

The Set of Native Delphi Components for Borland Delphi.
100% Source Code. Last information you can find at
http://www.da-soft.com

Last revision date Jan, 4, 2007.

PLEASE FOLLOW THE INSTRUCTIONS PROVIDED IN THE INSTALLATION SECTION!

TABLE OF CONTENTS
-----------------
Overview
History
Known problems
License Agreement
Compatibility
Installation
Demonstration Programs
Source Files
Copyright Notes
Contacts

Overview
--------

This is a component library for direct access to
Oracle8 server. Now set includes 7 core components:
  TOCIDatabase            - analog of TDatabase
  TOCIQuery               - analog of TQuery
  TOCIStoredProc          - analog of TStoredProc
  TNCSQLMonitorClient     - client of Borland's SQLMonitor
  TOCIUpdateSQL           - analog of TUpdateSQL
  TOCISequence            - interface to Oracle sequences
  TOCINestedDataSet       - analog of TNestedDataSet

2 experts:
  PL/SQL Wrapper Objects code generator - it allows to
    automatically create Delphi class for PL/SQL package.
  Component Namer - it allows automatically rename components,
    dropped onto the form, basign on user-defined rules. After
    installation it will appear in Delphi Edit menu and as
    property editor of property 'Name'.

and 3 advanced components:
  TOCITransactionManager  - transaction management
  TOCIImpHndlDatabase     - connection sharing with other TOCIDatabase (DLL)
  TOCIBDEConnection       - connection sharing with BDE

and 5 companion components (Companion\NC):
  TNCTimer                - multithreaded timer
  TNCDBTreeView           - NCOCI8 based SQL drived db tree view
  TNCCompNamer            - rule based components namer
  TNCDblListBox           - doubled list box
  TNCMemoDialog           - dialog for memo editing

Also companion includes:
  (Companion\ASTA) ASTA NCOCI8 Server - do you know ASTA ?
    not ?! www.astatech.com
  (Companion\OQB40) NCOCI8 driver for Orlic Query Builder -
    NCOCI8 driver for end-user query builder
  (Companion\KSMQ) NCOCI87 driver for Korzh Simple Query - 
    NCOCI8 driver for yet another end-user query builder

Current advantages:
- Oracle8i support;
- High performance and multithreaded support;
- Automatic minimization of open cursors count;
- Full fetch tuning (optimization for memory or speed);
- Full login control (as SYSDBA/SYSOPER, change password);
- Full BLOB's interface (transparent locators usage);
- Full OCI Array operations support (fetch/bind);
- Full PL/SQL support (Boolean/Table/Record/Cursor parameter types);
- PL/SQL Wrapper Objects code generator;
- Full editing, locking and cached updates support;
- Enahanced cached updates support (track of changes in
  multiple datasets, undo to any point in time);
- Full client constraints, defaults, sequences, calc fields with expressions support;
- Support of Oracle8 NESTED TABLE;
- Multiple Oracle home's support;
- RETURNING clause support;
- Powerfull filters with Oracle8 language;
- Full transaction control (parallel, nested, global, discrette, etc);
- Debuging capabilities - SQLMonitor support;
- High compatibility with native Delphi TQuery, TStoredProc;
- Support of good known third party component librarys;
- 3-tier support: MIDAS 3, ASTA;
- automatic naming of droped to form components;
- Oracle8 IDE - demo project NC O8 Console;
- Help;
- Web site with forums, support info, mailing list.

Near future (limitations):
- Filters (on server);
- Indexes (on client / on server);
- TOCITable;
- Object extensions;
- Full Oracle8i scalability support:
    - Migratable sessions;
    - Multiple sessions per server process;
- Oracle8 IDE - debugging/reverse engineering/...;
- ... and let me know what you whant :))

History
-------

You can find full log of NC OCI8 Library changes in
file HISTORY.TXT.

NC OCI8 1.0.6 (Jan, 4, 2006)
    D5 incompatibility fixes
NC OCI8 1.0.5 (Sept, 19, 2006)
    Delphi 2006 support
NC OCI8 1.0.4 (Feb, 8, 2006)
    Collection of last years fixes.
NC OCI8 1.0.3 (Oct, 25, 2004)
    Collection of last years fixes.
NC OCI8 1.0 (Jan, 30, 2002)
    Release, Bugs, BDE compatibility comps,
    PL/SQL Wrapper Objects Gen
NC OCI8 0.9 betta (Aug, 24, 2000)
    Bugs, Enhanced cached updates support
NC OCI8 0.8 betta (Jun, 19, 2000)
    Bugs, new comp: TOCINestedDataSet, PL/SQL cursor
    parameter support, NESTED TABLE support,
    MIDAS 3 support
NC OCI8 0.7 betta (Apr, 04, 2000)
    Bugs, Client constraints, defaults support,
    new props, new comp: TOCISequence
NC OCI8 0.6 betta (Mar, 05, 2000)
    Bugs, Editing capabilities, Cached Updates
    support, Web site
NC OCI8 0.5 betta (Feb, 09, 2000)
    Bugs, new props, new comp's: SQLMon, TM,
    new 4 companion comps, Semi-natural filter
    language, new demo
NC OCI8 0.4 betta (Nov, 06, 1999)
    Ready for multithreaded environment.
    Compatibility with D4, D5, BCB4.
NC OCI8 0.3 betta (Oct, 31, 1999)
    Bugs, New events, New prop's, Mailing list,
    full set of Oracle SQL functions in filters
NC OCI8 0.2 betta (Oct, 24, 1999)
    Client filters, Find*, Locate, Lookup,
    Macros, Bussy cursor, IDE
NC OCI8 0.1 betta (Oct, 17, 1999)
    Initial betta release available on Internet.
NC OCI8 first line of code (Aug, 15, 1999)

License Agreement
-----------------

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appears in all copies and
that both the above copyright notice and this permission notice appear
in supporting documentation, and that the name of NC OCI8 author
not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission. This
software is made available "as is", and NC OCI8 LIBRARY AUTHOR DISCLAIM
ALL WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO THIS SOFTWARE,
INCLUDING WITHOUT LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND IN NO EVENT SHALL AUTHORS BE
LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, TORT (INCLUDING NEGLIGENCE) OR
STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

You can find full text of NC OCI8 Library Software License agreement
in the file LICENSE.TXT.

Compatibility
-------------

Library was initally developed for Delphi 4, Oracle 8.0.4.
Library is compatible with Inprise tools:
- Delphi 3;
- Delphi 4;
- Delphi 5;
- Delphi 6;
- C++ Builder 3;
- C++ Builder 4;
- C++ Builder 5;
- C++ Builder 6.
Library is tested with Oracle clients:
- Oracle8 8.0.4.3.7;
- Oracle8 8.0.5.2.0;
- Oracle8i 8.1.5.0.0.
- Oracle8i 8.1.6.0.0.
Library is tested with Oracle servers:
- Oracle8 8.0.4.3.7.
- Oracle8i 8.1.6.0.0.
- Oracle9i 9.0.1.1.0.

Installation
------------

*****************************************************
IMPORTANT !
Because, i does not have enough time to prepare DFM
files for D3, D4, BCB3, BCB4, they are 100% compatible
with D5, D6 only. So, you should adapt DFM files for
other tools manually.
*****************************************************

Unzip ncoci8.zip. If you want to use non english resources, then
enter into appropriate catalog and copy files from it into SOURCE.
There are catalogs:
DK  Danish
EN  English
HR  Croatian
RU  Russian
UA  Ukrainian
FR  French
Also, repeat this for COMPANION\NC.

Delphi 3:

Use "File\Open..." menu item of Delphi IDE to open consistently
NC OCI8 package SOURCE\NCOCI83.DPK. In "Package..." window click
"Install" button to register NC OCI8 Library components on the
"NCOCI8 DB" page.
Repeat this operation for COMPANION\NC\NCComp4.dpk package. It
contains NCOCI8 companion components.

Delphi 4:

Use "File\Open..." menu item of Delphi IDE to open consistently
NC OCI8 package SOURCE\NCOCI84.DPK. In "Package..." window click
"Install" button to register NC OCI8 Library components on the
"NCOCI8 DB" page.
Repeat this operation for COMPANION\NC\NCComp4.dpk package. It
contains NCOCI8 companion components.

Delphi 5:

Use "File\Open..." menu item of Delphi IDE to open consistently
NC OCI8 package SOURCE\NCOCI85.DPK. In "Package..." window click
"Install" button to register NC OCI8 Library components on the
"NCOCI8 DB" page.
Repeat this operation for COMPANION\NC\NCComp5.dpk package. It
contains NCOCI8 companion components.

C++ Builder 4:

Use "File\Open..." menu item of C++ Builder IDE to open consistently
NC OCI8 package SOURCE\NCO845.BPK. In "Package..." window click 
"Install" button to register NC OCI8 Library components on the 
"NC OCI8" page. Make sure the NC0845.BPL file is placed on the 
directory listed in the PATH environment variable.


Demonstration Programs
----------------------

Demos can be found in NCOCI8\DEMOS folder:
    BDE              - Usage of BDE compatibility component TOCIBDEConnection
    Blobs\1          - How to setup components for BLOB editing.
    Blobs\2          - How to manually edit BLOBs.
    Editing          - Usage of TOCIUpdateSQL, CachedUpdates.
    Filters          - Filters in TOCIDataSet (see comment in main form).
    Macros           - Usage of TOCIMacros.
    MIDAS            - How to setup components for MIDAS server.
    NCO8Console      - IDE for Oracle8 server (Only D4, D5). 
    NestedTable      - Usage of TOCINestedDataSet with Oracle8 nested tables.
    Numbers          - Translation of varios NUMBER types into
                       Delphi data types.
    PLSQLGen         - Usage of PL/SQL Code Gen (DBMS_SQL)
    PLSQLTable       - PL/SQL tables, work with package DBMS_UTILITY.
    ProcUpd          - Shows how to setup components for editing table,
                       using stored procedures.
    RefCursor        - Usage of TOCINestedDataSet with REF CURSOR parameters.
    Services         - Very simle NT service (see comment in unit).
                       (Only D4, D5).
    Threads          - Multy threading with NCOCI8 (see comment in unit).
    TransMan         - Usage of multiple transaction managers and global
                       TX (see comment in unit).

Also, in folder "From Delphi5"\*.* is located ported
"NCOCI8\Demos\From Delphi5" to NCOCI8.

Source Files
------------

All sources (100%) of NCOCI8 Library are available in NCOCI8\SOURCE
directorie. And all sources of NCOCI8 Companion Library are available
in NCOCI8\COMPANION\NC directorie. Also there are subdirectories:
RU, EN, UA, DK, HR, FR each of that contains localized files. All
DFM files are text and created using D5. It is up to you to convert
them to D3, D4, if you will need that.

THANKS
---------------

Borland <http://www.inprise.com>                    Delphi
Oracle <http://technet.oracle.com>                  Oracle8i
Primoz Gabrijelcic <http://www.eccentrica.org/gabr> GpProfile profiler        
Vjacheslav Dutka <VSDutka@rolf.ru>                  D5 testing, Bugs report
Dmitry Blinov                                       Suggestions, BCB4 testing, bugs report
Sergey Pimenov <spimenov@sta.gov.ua>                Resources translation to RU & UA, bugs report
Josef Zatko <jahoda@mbcom.cz>                       Suggestions, bug report
Kurt Bilde <kub@sam.sdu.dk>                         Resources translation to DK
Goran Kliska <gorankli@usa.net>                     Master-detail implementation, resources transaltion
                                                    to Croatian, suggestions, bug report
Philippe Bonnefoy Cudraz <pbonnefoy@caramail.com>   French translation
Tom Sun <ytsun@utstar.hz.zj.cn>                     D3 port
Yaroslav Tatarenko <yarik@meta.sebastopol.ua>       Suggestions
Paul Batylin <polbat@mail.ru>                       BCB real support
Vladimir Muravliov <mur@service77.ru>               Bugs report, suggestions
Kelly Shipp <kshipp@acxiom.com>                     Bugs report (he was first :))
Mario Andreato <mario.andreato@oraplus.com>         Bugs report
Michael Michalak <mmichala@anza.com>                Bugs report
Guy Grigg <guy@morrisint.com.au>                    Bugs report
Joerg Schiffmann <Joerg.Schiffmann@telekom.de>      Bugs report
PAVEL ODSTRCIL <pavel.o.odstrcil@usa.net>           Bugs report
Paris <kolobos@otenet.gr>                           Bugs report
Mihail Fedorovsky <itl395@online.kharkov.ua>        Bugs report
Uwe Schuster <jedivcs@bitcommander.de>              Oracle 10g support

And **a lot** of other people, who was helping me to fight with a bugs,
problems and to make NCOCI8 stable and usable. There must be very
long list ! Thanks to ALL !

Copyright Notes
---------------

Most of the modules in our library are written by me. I have to make
a note of units based on sources of other authors.

Unit NCOciErrorDlg is based on original Borland Delphi VCL unit Dbexcept.
Unit NCOciFilter is based on original Borland Delphi VCL unit DBCommon.
Unit NCOciUpdateSQL is based on original Borland Delphi VCL code of TUpdateSQL.

Demo project "NC O8 Console" is based on Alexey Kochetov's SQLTools.
Demo project from "NCOCI8\Demos\From Delphi5" is ports of standard Delphi5
demo projects from catalog <Delphi5>\Demos\DB.

"Readme.txt" and "License.txt" files are based on RX Library's files.

CONTACTS
--------

  Author:
    Dmitry Arefiev, Nizhny Novgorod, Russia
      EMail: darefiev@gs-soft.ru
      ICQ: 50741007

  NC OCI8 Library Home Page:
    http://www.da-soft.com

  NC OCI8 & other discussions:
    http://www.da-soft.com/cgi-bin/Ultimate.cgi?action=intro

  NC OCI8 Library Mailing List:
    for subcribe: send empty mail to ncoci8_subscribe@da-soft.com
                  with subject "SUBSCRIBE NCOCI8"
    for unsubscribe: send empty mail to ncoci8_unsubscribe@da-soft.com
                  with subject "UNSUBSCRIBE NCOCI8"
