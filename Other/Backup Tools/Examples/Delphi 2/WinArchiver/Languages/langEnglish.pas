unit langEnglish;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Choose "New" to create or "Open" to open an archive' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Cancel' );
  AddStr(   4, '&Help' );
  // unit fmAboutBox
  AddStr( 500, 'About' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, 'These Components are Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib library:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'BlowFish Implementation provided by Greg Carter, CRYPTOCard' );
  AddStr( 507, 'SFX code by Oliver Buschjost' );
  AddStr( 508, 'Web site:' );
  // unit fmTiming
  AddStr( 600, 'Elapsed time :' );
  AddStr( 601, 'Remaining time :' );
  // unit fmMain
  AddStr( 700, 'New...' );
  AddStr( 701, 'Open...' );
  AddStr( 702, '&Add...' );
  AddStr( 703, '&Extract...' );
  AddStr( 704, '&Delete...' );
  AddStr( 705, '&Abort' );
  AddStr( 706, 'New Archive' );
  AddStr( 707, 'Open Archive' );
  AddStr( 708, 'Add files...' );
  AddStr( 709, 'Extract files...' );
  AddStr( 710, 'Delete files...' );
  AddStr( 711, 'Archive files (*.mmm)|*.mmm|SFX Archives (*.exe)|*.exe|All files (*.*)|*.*' );
  AddStr( 712, 'Archive files (*.mmm)|*.mmm|All files (*.*)|*.*' );
  AddStr( 713, 'Open an existing Archive' );
  AddStr( 714, 'Create a new Archive' );
  AddStr( 715, 'Open one segment of an Archive' );
  AddStr( 718, '%d file(s), %s' );
  AddStr( 720, 'The file "%s", already exists' );
  AddStr( 721, 'Do you want to reset this archive ?' );
  AddStr( 722, 'Do you want to delete this archive ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, 'selected %d file(s), %s' );
  AddStr( 729, 'Not available' );
  AddStr( 730, 'Rename the current archive to:' );
  AddStr( 731, 'Could not rename archive to "%s" !' );
  AddStr( 732, 'SFX Configuration' );
  AddStr( 733, 'Make a Self-Extracting archive' );
  AddStr( 734, 'Make' );
  AddStr( 735, 'Could not make a Self-Extracting archive !' );
  AddStr( 736, 'Set Arc&hive comment' );
  AddStr( 737, 'Archive comment' );
  AddStr( 738, 'An operation is in progress. Wait until the end or click on Abort.' );
  AddStr( 739, 'You''ve executed a file. Terminate it before and try again.' );
  AddStr( 740, 'You must first open or create an archive.' );
  AddStr( 741, 'Could not find associated program for %s' );
  AddStr( 742, 'Name' );
  AddStr( 743, 'Date' );
  AddStr( 744, 'Time' );
  AddStr( 745, 'Size' );
  AddStr( 746, 'Ratio' );
  AddStr( 747, 'Packed' );
  AddStr( 748, 'Seg#' );
  AddStr( 749, 'Path' );
  AddStr( 750, '&File' );
  AddStr( 751, '&Actions' );
  AddStr( 752, '&Options' );
  AddStr( 753, '&Help' );
  AddStr( 754, '&New archive...' );
  AddStr( 755, '&Open archive...' );
  AddStr( 756, 'Open &segment...' );
  AddStr( 757, '&Close archive' );
  AddStr( 758, '&Information...' );
  AddStr( 759, 'Re&name Archive' );
  AddStr( 760, '&Reset archive' );
  AddStr( 761, '&Delete archive' );
  AddStr( 762, '&Quit' );
  AddStr( 763, '&View...' );
  AddStr( 764, '&Select all' );
  AddStr( 765, '&Make .EXE file' );
  AddStr( 766, 'Set Archive Comment...' );
  AddStr( 767, '&SFX Configuration...' );
  AddStr( 769, '&About...' );
  AddStr( 770, 'Create a new archive' );
  AddStr( 771, 'Open an existing archive' );
  AddStr( 772, 'Open one segment of an Archive' );
  AddStr( 773, 'Closes this archive' );
  AddStr( 774, 'Displays informations on this archive' );
  AddStr( 775, 'Rename the current archive...' );
  AddStr( 776, 'Reset the content of the archive' );
  AddStr( 777, 'Delete archive' );
  AddStr( 778, 'Quit the application' );
  AddStr( 781, 'Add files to the archive' );
  AddStr( 782, 'Extract files from the archive' );
  AddStr( 783, 'Delete files from the archive' );
  AddStr( 784, 'View files' );
  AddStr( 785, 'Select all files of the archive' );
  AddStr( 786, 'Make a Self extracting archive' );
  AddStr( 787, 'Define a comment for the current archive' );
  AddStr( 788, 'Change the configuration' );
  AddStr( 789, 'Change the configuration of the SFX creation' );
  AddStr( 790, 'About this application' );
  AddStr( 798, '&Configuration...' );
  AddStr( 799, '%s File' );
  AddStr( 800, 'Closing archive...' );
  AddStr( 801, 'Select &none' );
  AddStr( 802, '&Invert selection' );
  AddStr( 803, 'Root' );
  AddStr( 804, 'Tree view' );
  AddStr( 805, 'Large Icons' );
  AddStr( 806, 'Small Icons' );
  AddStr( 807, 'List' );
  AddStr( 808, 'Detail' );
  AddStr( 809, 'Full expand' );
  AddStr( 810, 'Full collapse' );
  AddStr( 811, 'Clearing Files List' );
  AddStr( 812, 'Building Files List' );
  AddStr( 813, 'Sorting Files List' );
  AddStr( 814, 'The archive %s does not exist !' );
  AddStr( 815, 'Chec&k integrity' );
  AddStr( 816, 'Check integrity of the current archive' );
  AddStr( 817, '&View last Output...' );
  AddStr( 818, 'View the output of the last operation' );
  AddStr( 819, 'Install' );
  AddStr( 820, 'Extract content and execute install program' );
  AddStr( 821, '&Font...' );
  AddStr( 822, '&Sort' );
  AddStr( 823, '&Original Order' );
  AddStr( 824, 'Change the current font' );
  AddStr( 825, 'Select a sort order' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Add from' );
  AddStr( 901, 'Name :' );
  AddStr( 902, 'Folders' );
  AddStr( 903, 'Recurse folders' );
  AddStr( 904, 'Include current path' );
  AddStr( 905, 'Store empty folders' );
  AddStr( 906, 'Path storage :' );
  AddStr( 907, 'Encrypt files ?' );
  AddStr( 908, 'Compression level:' );
  AddStr( 909, 'Add' );
  AddStr( 910, 'None'+#13+
               'Whole'+#13+
               'Relative' );
  AddStr( 911, 'Maximum (slowest)'+#13+
               'Normal'+#13+
               'Fast'+#13+
               'Super Fast'+#13+
               'None' );
  AddStr( 912, 'Add Droped Files' );
  AddStr( 913, 'Add items' );
  AddStr( 914, 'Filter :' );
  AddStr( 915, 'Add to current folder ?' );
  // unit fmConfiguration
  AddStr( 1000, 'Configuration' );
  AddStr( 1001, 'Disk spanning' );
  AddStr( 1002, 'Archive creation' );
  AddStr( 1003, 'Options' );
  AddStr( 1004, 'Split archive' );
  AddStr( 1005, 'Maximum size of an archive segment:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Other (Kb):' );
  AddStr( 1007, 'Compress archive' );
  AddStr( 1008, 'Crypt archive' );
  AddStr( 1009, 'Solid archive' );
  AddStr( 1010, 'ReadOnly' );
  AddStr( 1011, 'Create SFX archive' );
  AddStr( 1014, 'Block size' );
  AddStr( 1015, 'Reserve space' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Language:' );
  AddStr( 1018, 'Automatic'+#13+
                'English'+#13+
                'French'+#13+
                'Chinese'+#13+
                'Portuguese'+#13+
                'German'+#13+
                'Italian'+#13+
                'Russian'+#13+
                'Spanish' );
  AddStr( 1019, 'Show empty folders' );
  AddStr( 1020, 'Show Tree View' );
  // unit fmCreateFolder
  AddStr( 1100, 'Current folder:' );
  AddStr( 1101, 'Name:' );
  // unit fmDelete
  AddStr( 1200, 'Delete' );
  AddStr( 1201, 'Files' );
  AddStr( 1202, '&Entire Archive'+#13+
                '&Selected Files'+#13+
                '&Files:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'System message' );
  AddStr( 1301, 'Hide password ?' );
  // unit fmExtract
  AddStr( 1400, 'Extract' );
  AddStr( 1401, 'Extract to:' );
  AddStr( 1402, 'Files' );
  AddStr( 1403, '&Selected Files'+#13+
                '&All files'+#13+
                'F&iles:' );
  AddStr( 1404, '&Use folder names' );
  AddStr( 1405, 'Overwrite existing files'+#13+
                'Skip existing files'+#13+
                'Update new files'+#13+
                'Ask confirmation'+#13+
                'Restore existing files only'+#13+
                'Update existing files' );
  AddStr( 1406, 'Folders / Drives' );
  AddStr( 1407, 'New Folder...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'The following keywords may be used in the "Command Line" and'+#13+
                '"Default Extract Path" fields :' );
  AddStr( 1501, 'will be replaced with the Temp Directory'+#13+
                '(often ''c:\windows\temp'' or ''c:\win95\temp'' or ''c:\temp'')' );
  AddStr( 1502, 'will be replaced with the Windows Directory'+#13+
                '(often ''c:\windows'' or ''c:\win95'')' );
  AddStr( 1503, 'will be replaced with the System Directory'+#13+
                '(often ''c:\windows\system'' or ''c:\win95\system'')' );
  AddStr( 1504, 'will be replaced with the ProgramFiles Directory'+#13+
                '(often ''c:\Program Files'' [depending on the language'+#13+
                'of the installed Windows])' );
  AddStr( 1505, 'will be replaced with the directory where the files'+#13+
                'were extracted to (only for the "Command Line" or'+#13+
                '"Arguments" fields)' );
  AddStr( 1506, 'Example:' );
  AddStr( 1507, '<PF>MyCompany\MyStuff' );
  // unit fmInformation
  AddStr( 1600, 'Path:' );
  AddStr( 1601, 'Name:' );
  AddStr( 1602, 'File Size:' );
  AddStr( 1603, 'Files:' );
  AddStr( 1604, 'Compression:' );
  AddStr( 1605, 'Date/Time:' );
  AddStr( 1606, 'Segment:' );
  AddStr( 1607, 'Attributes' );
  AddStr( 1608, 'Encrypted' );
  AddStr( 1609, 'Compressed' );
  AddStr( 1610, 'Solid' );
  AddStr( 1611, 'ReadOnly' );
  AddStr( 1612, 'Final Segment' );
  AddStr( 1613, 'Information' );
  // unit fmSFXComments
  AddStr( 1700, 'Comments' );
  AddStr( 1701, 'Comment shown when opening the SFX Archive' );
  AddStr( 1702, 'Comment shown after the extraction of the files stored in the SFX Archive' );
  AddStr( 1703, 'Clear comments' );
  // unit fmSFXConfig
  AddStr( 1800, 'SFX Configuration' );
  AddStr( 1801, 'Execute file after extract ?' );
  AddStr( 1802, 'User chooses files to extract ?' );
  AddStr( 1803, 'User chooses overwrite mode ?');
  AddStr( 1804, 'Caption:' );
  AddStr( 1805, 'Command line:' );
  AddStr( 1806, 'Arguments:' );
  AddStr( 1807, 'Default extract path:' );
  AddStr( 1808, 'Overwrite mode:' );
  AddStr( 1809, 'Comments...' );
  AddStr( 1810, 'Ask confirmation'+#13+
                'Overwrite existing files'+#13+
                'Skip existing files'+#13+
                'Overwrite only if newer'+#13+
                'Restore existing files only'+#13+
                'Extract the file only if it already exists and is newer' );
  AddStr( 1811, 'User allowed not to run the file ?' );
  // unit fmTextViewer
  AddStr( 1900, 'View: %s' );
  AddStr( 1901, '&Clipboard copy' );
  AddStr( 1902, '&Font' );
  // unit fmView
  AddStr( 2000, 'View : %s' );
  AddStr( 2001, 'Using' );
  AddStr( 2002, 'View' );
  AddStr( 2003, '&Associated program (%s)'+#13+
                '&Internal ASCII Text Viewer' );
  // unit fmLastOutput
  AddStr( 2100, 'View Last Output' );
end;

end.
