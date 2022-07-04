{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDEResStrs;

interface

resourcestring
  SSCDE_NotValidColumnIndex = '%d is not a valid column index.';
  SSCDE_NotValidItemIndex = '%d is not a valid item index.';
  SSCDE_MaskErr = 'Invalid input value. ''%s''';
  SSCDE_MaskEditErr = 'Invalid input value. Use escape key to abandon changes';
  SSCDE_DuplicateString = 'String list does not allow duplicates';
  SSCDE_SortedListError = 'Operation not allowed on sorted string list';
  SSCDE_ListIndexError = 'List index out of bounds (%d)';
  SSCDE_Error = 'Error';
  SSCDE_CalcError = 'Calculation error';
  SSCDE_MenuUndo = 'Undo';
  SSCDE_MenuRedo = 'Redo';
  SSCDE_MenuCopy = 'Copy';
  SSCDE_MenuCut = 'Cut';
  SSCDE_MenuPaste = 'Paste';
  SSCDE_MenuDelete = 'Delete';
  SSCDE_MenuSelectAll = 'Select All';
  SSCDE_CalClear = 'Clear';
  SSCDE_CalToday = 'Today';
  SSCDE_Warning = 'Warning';
  SSCDE_SchemeChangeWarning = 'Changing sheme will clear your current custom cell settings.' +
    #13 + 'Do you want to apply new sheme?';
  SSCDE_CircularDataLink = 'Circular datalinks are not allowed';
  SSCDE_FieldNotFound = 'Field ''%s'' not found';
  SSCDE_DataSourceFixed = 'Operation not allowed in a DBCtrlGrid';
  SSCDE_PropDefByLookup = 'Property already defined by lookup field';
  SSCDE_OSError = 'System Error.  Code: %d.' + #13#10 + '%s';
  SSCDE_UnkOSError = 'A call to an OS function failed';

implementation

end.