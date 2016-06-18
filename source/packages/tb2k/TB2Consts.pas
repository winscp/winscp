unit TB2Consts;
{ $jrsoftware: tb2k/Source/TB2Consts.pas,v 1.7 2002/11/14 18:07:19 jr Exp $ }

interface

{$I TB2Ver.inc}

resourcestring
  { Exceptions }
  STBToolbarIndexOutOfBounds = 'Toolbar item index out of range';
  STBToolbarItemReinserted = 'Toolbar item already inserted';
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
