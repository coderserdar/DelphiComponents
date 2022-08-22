unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-99 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants
  
  Translated to Brazilian Portuguese by Marcus Vinicius de A. Baeta Neves.
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = 'Um controle TDock97 não pode ser inserido dentro de uma janela "ToolWindow" ou de um outro controle TDock97';
  STB97DockCannotChangePosition = 'A posição de um controle TDock97 não pode mudar se ele já contem controles';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Não posso salvar a posição da janelas "ToolWindow" porque a propriedade "Name" está em branco';
  STB97ToolwinDockedToNameNotSet = 'Não posso salvar a posição da janelas "ToolWindow" porque a propriedade "Name" do controle "DockedTo" está em branco';
  STB97ToolwinParentNotAllowed = 'Uma janela "ToolWindow" somente pode ser colocada em um controle TDock97 ou diretamente sobre um form';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = 'O controle ''%s'' não é filho da toolbar';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'O controle "TToolbarSep97" somente pode ser insrido em um controle "TToolbar97"';

implementation

end.
 
