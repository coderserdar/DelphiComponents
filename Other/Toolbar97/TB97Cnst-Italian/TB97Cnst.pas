unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = 'Un controllo TDock97 non può essere posto all''interno di una tool window o di un''altra TDock97';
  STB97DockCannotChangePosition = 'Non è possibile cambiare la posizione di un TDock97 se al suo interno sono presenti altri componenti';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Non è possibile salvare la posizione della tool window perchè la proprietà Name non è impostata';
  STB97ToolwinDockedToNameNotSet = 'Non è possibile salvare la posizione della tool window perchè la proprietà Name di DockedTo non è impostata';
  STB97ToolwinParentNotAllowed = 'Una tool window può essere posizionata solamente su di un componente TDock97 oppure direttamente sul form';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = 'Il controllo ''%s'' non è figlio di una toolbar';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'Il componente TToolbarSep97 può essere inserito solo all''interno di una TToolbar97';

implementation

end.
 
