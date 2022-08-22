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
  STB97DockParentNotAllowed = 'Un control TDock97 no puede estar localizado en el interior de una ventana de herramientas o de otro TDock97';
  STB97DockCannotChangePosition = 'No puede cambiar la posicion de un TDock97 si contiene algun control';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'No puede guardar la posicion de una ventana de herramientas por que no tiene asignado ningun valor la propiedad Name';
  STB97ToolwinDockedToNameNotSet = 'No puede guardar la posicion de una ventana de herramientas por que que no tiene asignado ningun valor a la propiedad Name del Docked';
  STB97ToolwinParentNotAllowed = 'Una ventana de herramientas solo puede estar localizada sobre un TDock97 o directamente sobre un formulario';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = 'El Control ''%s'' no es descendiente de la toolbar';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'TToolbarSep97 solo puede estar localizada sobre un TToolbar97';

implementation

end.
 
