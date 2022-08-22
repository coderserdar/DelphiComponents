unit Tb97cnst;
{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants for german language by Christof Schelian
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
    {'A TDock97 control cannot be placed inside a tool window or another TDock97'}
  STB97DockParentNotAllowed ='Eine TDock97 kann nicht in einem anderen ToolWindow97 oder in einem anderen TDock97 plaziert werden.';

  {'Cannot change Position of a TDock97 if it already contains controls'}
  STB97DockCannotChangePosition = 'Es ist nicht möglich eine Position von einem TDock97 zu verändern, da untergeordete Komponenten eines Steuerelements bereits enthalten sind.';

  { TCustomToolWindow97 exception messages }
    {'Cannot save tool window''s position because Name property is not set'}
  STB97ToolwinNameNotSet = 'Die Position des ToolWindow97 kann nicht gespeichert werden, da die Eigenschaft "Name" nicht gesetzt ist.';
    {'Cannot save tool window''s position because DockedTo''s Name property not set'}
  STB97ToolwinDockedToNameNotSet = 'Die Position des ToolWindow97 kann nicht gespeichert werden, da die Eigenschaft "DockedTo" nicht gesetzt ist.';
    {'A tool window can only be placed on a TDock97 or directly on the form'}
  STB97ToolwinParentNotAllowed = 'Ein ToolWindow97 kann nicht nur auf einem TDock97 oder direkt auf einem TForm plaziert werden.';

  { TCustomToolbar97 exception messages }
    {'Control ''%s'' is not a child of the toolbar'}
  STB97ToolbarControlNotChildOfToolbar = 'Die untergeordete Komponenten eines Steuerelements ''%s'' sind kein Child von Toolbar.';

  { TToolbarSep97 exception messages }
    {'TToolbarSep97 can only be placed on a TToolbar97'}
  STB97SepParentNotAllowed = ' Ein TToolbarSep97 kann nicht nur auf ein TToolbar97 plaziert werden.';

implementation

end.

