unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants for Korean language by Baek UnYoung, 2000.03. (TB97 v1.75e base)
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = 'TDock97 컨트롤은 TToolWindow97 또는 다른 TDock97 에 놓일 수 없습니다.';
      {'A TDock97 control cannot be placed inside a tool window or another TDock97'}
  STB97DockCannotChangePosition = '이미 컨트롤을 포함하고 있어, TDock97 의 위치를 변경할 수 없습니다.';
      {'Cannot change Position of a TDock97 if it already contains controls'}

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Name 프로퍼티가 설정되어 있지 않기 때문에, TToolWindow97 의 위치를 저장할 수 없습니다.';
      {'Cannot save tool window''s position because Name property is not set'}
  STB97ToolwinDockedToNameNotSet = 'DackedTo 의 Name 프로퍼티가 설정되어 있지 않기 때문에, TToolWindow97 의 위치를 저장할 수 없습니다.';
      {'Cannot save tool window''s position because DockedTo''s Name property not set'}
  STB97ToolwinParentNotAllowed = 'TToolWindow97 는 TDock97 또는 폼 바로 위에만 놓일 수 있습니다.';
      {'A tool window can only be placed on a TDock97 or directly on the form'}

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = '컨트롤 ''%s'' 는 툴바의 자식이 아닙니다.';
      {'Control ''%s'' is not a child of the toolbar'}

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'TToolbarSep97 는 TToolbar97 위에만 놓일 수 있습니다.';
      {'TToolbarSep97 can only be placed on a TToolbar97'}

implementation

end.
 
