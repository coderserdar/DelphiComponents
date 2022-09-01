unit GSCQBConst;

interface

uses Windows, Graphics;

const
  CColumn    = 1;
  CAlias     = 2;
  CTable     = 3;
  CShow      = 4;
  CSortType  = 5;
  CSortOrder = 6;
  CGroupBy   = 7;
  CCriteria  = 8;

  Hand  = 15;
  Hand2 = 12;

  SAllColumns = '*';
  SShow       = 'Show';

  SUnsorted   = 'Unsorted';
  SAsc        = 'Ascending';
  SDesc       = 'Descending';
  SSortMenu   = '"' + SAsc + '","' + SDesc + '","' + SUnsorted + '"';

  SGroupBy    = 'Group By';
  SCount      = 'Count';
  SGroupMenu  = '"' + SGroupBy + '","' + SCount + '","Sum","Avg","Min","Max"';

  MIN_FIELDLIST_WIDTH = 110;

resourcestring
  SNotValidParent      = 'Parent must be TGSCQBWorkArea!';
  SNotValidOwner       = 'Owner must be TGSCQBWorkArea!';
  STablesLinked        = 'These tables are already linked!';
  SNoFieldsInTable     = 'Table "%s" doesn''t have any fields!';
  SAliasChangeCaption  = 'Change table alias';
  SAliasChangePrompt   = 'Please enter the new alias for the table:';
  SAliasExists         = 'The alias you entered already exists!';
  SCantFindColumn      = 'Can''t find row for column "%s" in table "%s"';
  SFieldNotInTable     = 'The field "%s" doesn''t exists in table "%s"!';
  SInvalidFieldIndex   = 'Invalid field index (%d) for table "%s"';
  STableListPropNil    = 'Property TableList not assigned!';
  STableAliasExists    = 'Alias "%s" already used by an other table!';

  SCaptionColumn       = 'Column';
  SCaptionAlias        = 'Alias';
  SCaptionTable        = 'Table';
  SCaptionOutput       = 'Output';
  SCaptionSortType     = 'Sort Type';
  SCaptionSortOrder    = 'Sort Order';
  SCaptionGroupBy      = 'Group By';
  SCaptionCriterias    = 'Criteria';

  SClose               = 'Close';
  SRemoveLink          = 'Remove';
  SRemoveLinks         = 'Remove links';
  SSelectAll           = 'Select All';
  SSelectNone          = 'Select None';
  SAllRowsForm         = 'All rows from %s';
  SProperties          = 'Properties';
  SSetTableAlias       = 'Change table alias';

implementation

end.
