{ Copyright (C) 1998-2006, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization : Portuguese(Standard)
  freeware SMComponent library
}  
unit SMCnst;

interface
{Portuguese strings - Translated by Fernando Dias (fernandodias@easygate.com.pt)
 Last update: 03-May-2006}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Pretende mesmo guardar as alterações na base de dados ?';
  strErrSaveChanges = 'Não foi possível guardar os dados! Verifique a ligação ao servidor e a validade dos dados.';
  strDeleteWarning = 'Pretende mesmo eliminar a tabela %s?';
  strEmptyWarning = 'Pretende mesmo limpar o conteudo da tabela %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Acrescentar registo',
    'Inserir registo',
    'Editar registo',
    'Apagar registo',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtrar ...',
    'Procurar ...',
    '-',
    'Guardar alterações',
    'Cancelar alterações',
    'Refrescar',
    '-',
    'Marcar/Desmarcar registos',
       'Marcar registo',
       'Marcar todos',
       '-',
       'Desmarcar registo',
       'Desmarcar todos',
    '-',
    'Guardar aspecto da coluna',
    'Recuperar aspecto da coluna',
    '-',
    'Configuração...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulo ';
  SgbData = ' Dados ';
  STitleCaption = 'Rótulo:';
  STitleAlignment = 'Alinhamento:';
  STitleColor = 'Fundo:'; 
  STitleFont = 'Letra:';
  SWidth = 'Largura:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'esquerda';
  SAlignRight = 'direita';
  SAlignCenter = 'centro';
  
const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'não igual';
  strNonMore = 'não maior';
  strNonLess = 'não menor';
  strLessThan = 'menor que';
  strLargeThan = 'maior que';
  strExist = 'vazio';
  strNonExist = 'não vazio';
  strIn = 'na lista';
  strBetween = 'entre';
  strLike = 'como';

  strOR = 'OU';
  strAND = 'E';

  strField = 'Campo';
  strCondition = 'Condição';
  strValue = 'Valor';

  strAddCondition = ' Defina a condição adicional:';
  strSelection = ' Seleccionar registos pela seguinte condição:';

  strAddToList = 'Adicionar à lista';
  strEditInList = 'Editar a lista';
  strDeleteFromList = 'Apagar da lista';

  strTemplate = 'Modelo de filtro';
  strFLoadFrom = 'Lêr de...';
  strFSaveAs = 'Guardar como...';
  strFDescription = 'Descrição';
  strFFileName = 'Ficheiro';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Proteger para escrita';
  strFProtectErr = 'Ficheiro está protegido!';

const //for SMDBNavigator
  SFirstRecord = 'Primeiro registo';
  SPriorRecord = 'Registo anterior';
  SNextRecord = 'Próximo registo';
  SLastRecord = 'Último registo';
  SInsertRecord = 'Inserir registo';
  SCopyRecord = 'Copiar registo';
  SDeleteRecord = 'Apagar registo';
  SEditRecord = 'Editar registo';
  SFilterRecord = 'Filtrar';
  SFindRecord = 'Procurar';
  SPrintRecord = 'Imprimir';
  SExportRecord = 'Exportar';
  SImportRecord = 'Importar';
  SPostEdit = 'Guardar alterações';
  SCancelEdit = 'Cancelar alterações';
  SRefreshRecord = 'Refrescar dados';
  SChoice = 'Escolher registo';
  SClear = 'Anular escolha de registo';
  SDeleteRecordQuestion = 'Apagar registo?';
  SDeleteMultipleRecordsQuestion = 'Pretende mesmo apagar os registos seleccionados?';
  SRecordNotFound = 'Registo não encontrado';

  SFirstName = 'Primeiro';
  SPriorName = 'Anterior';
  SNextName = 'Próximo';
  SLastName = 'Último';
  SInsertName = 'Inserir';
  SCopyName = 'Copiar';
  SDeleteName = 'Apagar';
  SEditName = 'Editar';
  SFilterName = 'Filtrar';
  SFindName = 'Procurar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SImportName = 'Importar';  
  SPostName = 'Guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Refrescar';
  SChoiceName = 'Escolher';
  SClearName = 'Limpar';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Abrir';
  SBtnSave = 'Guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Colar';
  SBtnClear = 'Limpar';

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'numero válido';
  etValidInteger = 'numero inteiro válido';
  etValidDateTime = 'data/hora válida';
  etValidDate = 'data válida';
  etValidTime = 'hora válida';
  etValid = 'válido';
  etIsNot = 'não é um(a)';
  etOutOfRange = 'Valor %s fora do intervalo %s..%s';

  SApplyAll = 'Aplicar a todos';
  
  SNoDataToDisplay = '<Sem dados para apresentar>';
  
implementation

end.
