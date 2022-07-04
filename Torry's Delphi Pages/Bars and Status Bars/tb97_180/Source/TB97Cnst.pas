unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-2004 by Jordan Russell
  http://www.jrsoftware.org/

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  
  String constants

  $jrsoftware: tb97/Source/TB97Cnst.pas,v 1.3 2004/02/23 22:53:00 jr Exp $
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = 'A TDock97 control cannot be placed inside a tool window or another TDock97';
  STB97DockCannotChangePosition = 'Cannot change Position of a TDock97 if it already contains controls';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Cannot save tool window''s position because Name property is not set';
  STB97ToolwinDockedToNameNotSet = 'Cannot save tool window''s position because DockedTo''s Name property not set';
  STB97ToolwinParentNotAllowed = 'A tool window can only be placed on a TDock97 or directly on the form';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = 'Control ''%s'' is not a child of the toolbar';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'TToolbarSep97 can only be placed on a TToolbar97';

implementation

end.
 
