unit TB97Cnst_FR;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants for Dutch language by Stephane Vandenbussche
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
    {'A TDock97 control cannot be placed inside a tool window or another TDock97'}
  STB97DockParentNotAllowed ='Een TDock97 component kan niet in een ToolWindow97 of in een andere TDock97 component geplaatst worden.';

  {'Cannot change Position of a TDock97 if it already contains controls'}
  STB97DockCannotChangePosition = 'Onmogelijk de plaats van een TDock97 te veranderen als het al componenten bevat.';

  { TCustomToolWindow97 exception messages }
    {'Cannot save tool window''s position because Name property is not set'}
  STB97ToolwinNameNotSet = 'Onmogelijk de plaats van het ToolWindow97 te saven want de component naam ontbreekt.';
    {'Cannot save tool window''s position because DockedTo''s Name property not set'}
  STB97ToolwinDockedToNameNotSet = 'Onmogelijk de plaats van het ToolWindow97 te saven want de DockedTo eigenshap ontbreekt.';
    {'A tool window can only be placed on a TDock97 or directly on the form'}
  STB97ToolwinParentNotAllowed = 'Een ToolWindow97 kan alleen op een TDock97 of direct op een TForm geplaatst worden.';

  { TCustomToolbar97 exception messages }
    {'Control ''%s'' is not a child of the toolbar'}
  STB97ToolbarControlNotChildOfToolbar = 'De component ''%s'' is niet een kind van de toolbar.';

  { TToolbarSep97 exception messages }
    {'TToolbarSep97 can only be placed on a TToolbar97'}
  STB97SepParentNotAllowed = 'TToolbarSep97 kan alleen op een TToolbar97 geplaatst worden.';

implementation

end.

