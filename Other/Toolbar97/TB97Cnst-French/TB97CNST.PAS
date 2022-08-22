unit TB97Cnst_FR;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants for french language by Stephane Vandenbussche
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
    {'A TDock97 control cannot be placed inside a tool window or another TDock97'}
  STB97DockParentNotAllowed ='Un composant TDock97 ne peut pas être placé dans un ToolWindow97 ou un autre composant TDock97.';

  {'Cannot change Position of a TDock97 if it already contains controls'}
  STB97DockCannotChangePosition = 'Impossible de changer la position d''un TDock97 s''il contient déjà des composants.';

  { TCustomToolWindow97 exception messages }
    {'Cannot save tool window''s position because Name property is not set'}
  STB97ToolwinNameNotSet = 'Impossible de sauvegarder la position du ToolWindow97 car sa propriété Name n''est pas remplie.';
    {'Cannot save tool window''s position because DockedTo''s Name property not set'}
  STB97ToolwinDockedToNameNotSet = 'Impossible de sauvegarder la position du ToolWindow97 car sa propriété DockedTo n''est pas remplie.';
    {'A tool window can only be placed on a TDock97 or directly on the form'}
  STB97ToolwinParentNotAllowed = 'Un ToolWindow97 ne peut être placé que sur un TDock97 ou directement sur un TForm.';

  { TCustomToolbar97 exception messages }
    {'Control ''%s'' is not a child of the toolbar'}
  STB97ToolbarControlNotChildOfToolbar = 'Le composant ''%s'' n''est pas un enfant du Toolbar.';

  { TToolbarSep97 exception messages }
    {'TToolbarSep97 can only be placed on a TToolbar97'}
  STB97SepParentNotAllowed = 'TToolbarSep97 ne peut être placé que sur un TToolbar97.';

implementation

end.

