                   B-Tree Filer BONUS Files
                   ------------------------

The bonus files on the install disks are non-commercial units and
utilities developed for B-Tree Filer.

If you have a unit or utility for B-Tree Filer that you think would be an
appropriate addition to the BONUS disk, please notify one of the project
admins.

Each unit/program has been compressed into an LZH file with its
supporting files in order to save disk space. The install program will
extract each LZH file from the diskette and place it in the
FILER\BONUS directory. To extract the individual files from each LZH
archive, use the supplied LHA.EXE program. This is supplied in a self
-extracting archive called LHA213.EXE; run this first to extract the
LHA.EXE program and documentation.

To extract files from LZH archives enter the following at a DOS
prompt:

  LHA x ArchiveName

For example, LHA x OOFILER.LZH would extract all of the files from the
OOFILER archive.

The following descriptions indicate whether the associated files are
compatible with (a) Turbo Pascal 6.0, 7.0 and real mode Borland Pascal
7.0 - 'realmode', (b) DOS protected mode Borland Pascal 7.0 - 'pmode',
and/or (c) Turbo Pascal for Windows 1.5 or Borland Pascal with a
Windows target - 'Windows'.

Here's a brief description of the BONUS files:

CCSKEYS                                     (realmode, pmode, Windows)
  Unit for translating string data in your records from one code page
  to another. Supports code pages 437, 850 and Windows 1250.

CHKFB                                                (realmode, pmode)
  Program that verifies internal B-Tree Filer data in fileblocks.
  Checks the system record (record 0) in both the data and index
  files. Verifies the deleted record chain, variable length record
  links, index pages.

EXTEND                                      (realmode, pmode, Windows)
  Units that allow programs under DOS 2.0 or later to open up to 255
  files.

FBROWSE                                              (realmode, pmode)
  A unit that implements a fileblock browser for use with Object
  Professional (version 1.03 or later). Archive includes demo program,
  documentation, and help file.

JULSTULS                                    (realmode, pmode, Windows)
  Various routines for B-Tree Filer and for NetWare.

LHA213.EXE
  Self-extracting archive. Contains program to decompress LZH files.

NUMKEYSZ                                    (realmode, pmode, Windows)
  Unit that provides the same functionality as NUMKEYS but for ASCIIZ
  strings. Uses the same algorithms as those in B-Tree Filer for C.

OOFILER                                     (realmode, pmode, Windows)
  Objects for managing fixed length and variable length record
  fileblocks. Archive includes two units, demo program, and
  documentation file. Also includes a program that demonstrates
  relational file management using the object-oriented units.

TVBRWDLG                                             (realmode, pmode)
  An example program that shows how to put a Turbo Vision TBrowserView
  (from the OBROWSE file described above) into a TDialog box.

TWOBRDOS                                             (realmode, pmode)
  An example program that shows how to use two linked browsers within
  the same program. Written using the Object Professional browser
  unit, FBROWSE.

TWOBRWIN                                                     (Windows)
  An example program that shows how to use two linked OWL browsers
  within the same Windows program. Written using the ObjectWindows
  Library browser unit, WBROWSER.
