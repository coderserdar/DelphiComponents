unit ATxMsg;

interface

var
  MsgViewerErrCannotFindText: string = 'Search string not found: "%s"';
  MsgViewerErrInvalidHex: string = 'Hex string is invalid: "%s"';
  MsgViewerErrInvalidFilelist: string = 'Cannot handle files list: "%s"';
  MsgViewerSearchProgress: string = 'Searching for "%s":';
  MsgViewerLangMissed: string = 'Language file "%s" not found.'#13'Try to reinstall application or delete the Viewer.ini file.';

  MsgViewerJumpToFirst: string = 'This is the last file in directory. Jump to the first file?';
  MsgViewerJumpToLast: string = 'This is the first file in directory. Jump to the last file?';
  MsgViewerJumpNotFound: string = 'Current filename "%s" is not found in directory. Jump to the first file?';
  MsgViewerJumpDirEmpty: string = 'Current directory is empty. Cannot jump to another file.';
  MsgViewerJumpSingleFile: string = 'This is the single file in directory. Cannot jump to another file.';
  MsgViewerDirEmpty: string = 'Folder is empty: "%s"';

  MsgViewerPluginsNameNotFound: string = '(not found)';
  MsgViewerPluginsLabelTCIni: string = 'Total Commander configuration &file:';
  MsgViewerPluginsLabelFile: string = '&File:';
  MsgViewerPluginsLabelFolder: string = '&Folder:';
  MsgViewerPluginsFilterExe: string = 'Applications (*.exe)|*.EXE';
  MsgViewerPluginsFilterIni: string = 'Ini files (*.ini)|*.INI';
  MsgViewerPluginsFilterWLX: string = 'Plugins (*.wlx)|*.WLX';
  MsgViewerPluginsFilterZip: string = 'Archives (*.zip;*.rar)|*.ZIP;*.RAR';

  MsgViewerPluginsNone: string = '(none)'#13;
  MsgViewerPluginsInstalled: string =
    'The following plugins were added:'#13#13'%s'#13+
    'The following plugins are already installed and were not added:'#13#13'%s';
  MsgViewerPluginsInstalledZip: string =
    'The following plugins were added:'#13#13'%s'#13+
    'The following plugins were updated:'#13#13'%s';

  MsgViewerPluginUnsup: string = 'This archive contains plugin of unsupported type (%s)';
  MsgViewerPluginSup: string = 'This archive contains plugin:'#13'"%s".'#13#13'Do you want to install it in Universal Viewer?';

  MsgViewerPluginsInvalidFile: string = 'Source file/folder name is not valid';
  MsgViewerPluginsInvalidTCExe: string = 'Selected Total Commander executable name is not valid';

  MsgViewerDeleteCaption: string = 'Delete file';
  MsgViewerDeleteWarningRecycle: string = 'Do you want to move "%s" to Recycle Bin?';
  MsgViewerDeleteWarningPermanent: string = 'Do you want to delete "%s"?';
  MsgViewerCopyMoveError: string = 'Cannot copy/move "%s" to "%s"';
  MsgViewerDeleteError: string = 'Cannot delete "%s"';

  MsgViewerIconDef: string = '(Default)';
  MsgViewerIconSave: string = '(Save template...)';
  MsgViewerExifMissed: string = 'No EXIF information in this file';
  MsgViewerNavMissed: string = 'Cannot find Navigation Panel add-on (Nav.exe)';

implementation

end.
