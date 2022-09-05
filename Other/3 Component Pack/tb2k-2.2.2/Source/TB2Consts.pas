unit TB2Consts;
{ $jrsoftware: tb2k/Source/TB2Consts.pas,v 1.8 2006/03/12 23:11:58 jr Exp $ }

interface

{$I TB2Ver.inc}

resourcestring
  { Exceptions }
  STBToolbarIndexOutOfBounds = 'Toolbar item index out of range';
  STBToolbarItemReinserted = 'Toolbar item already inserted';
  STBToolbarItemParentInvalid = 'Toolbar item cannot be inserted into container of type %s'; 
  STBViewerNotFound = 'An item viewer associated the specified item could not be found';

  { TTBChevronItem }
  STBChevronItemMoreButtonsHint = 'More Buttons|';

  { TTBMRUListItem }
  STBMRUListItemDefCaption = '(MRU List)';

  { TTBMDIWindowItem }
  STBMDIWindowItemDefCaption = '(Window List)';

  { TTBDock exception messages }
  STBDockParentNotAllowed = 'A TTBDock control cannot be placed inside a tool window or another TTBDock';
  STBDockCannotChangePosition = 'Cannot change Position of a TTBDock if it already contains controls';

  { TTBCustomDockableWindow exception messages }
  STBToolwinNameNotSet = 'Cannot save dockable window''s position because Name property is not set';
  STBToolwinDockedToNameNotSet = 'Cannot save dockable window''s position because DockedTo''s Name property not set';

implementation

end.
