unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants for russian language by Grischenko Alexander.
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = ' омпонент TDock97 не может быть помещен внутри Tool window или другого TDock97';
  STB97DockCannotChangePosition = 'Ќевозможно изменить свойство Position компонента TDock97, если он уже содержит другие компоненты';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Ќевозможно сохранить позицию Tool window, т.к. не определено свойство Name';
  STB97ToolwinDockedToNameNotSet = 'Ќевозможно сохранить позицию Tool window, т.к. не определено свойство Name компонента DockedTo';
  STB97ToolwinParentNotAllowed = 'Tool window может быть помещено только на TDock97 либо непосредственно на форму';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = ' омпонент ''%s'' не принадлежит Toolbar''у';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'TToolbarSep97 может быть помещен только на TToolbar97';

  STB97Language = 'Russian';

implementation

end.
 
