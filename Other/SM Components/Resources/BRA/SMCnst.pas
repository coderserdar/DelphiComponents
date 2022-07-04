unit SMCnst;

interface


{Brazilian Portuguese strings}
{translated by Rodrigo Hjort, rodrigo_hjort@excite.com}
{Atualizado por Alexandre Sant'Anna em 06/08/2007, alex.santanna@globo.com} 
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Deseja realmente salvar alterações no Servidor de Banco de Dados?';
  strErrSaveChanges = 'Não foi possível salvar um dado! Verifique a conexão com o Servidor ou validação de dados.';
  strDeleteWarning = 'Deseja realmente excluir a tabela %s?';
  strEmptyWarning = 'Deseja realmente esvaziar a tabela %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Incluir registro',
    'Insert registro',
    'Alterar registro',
    'Excluir registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtra ...',
    'Pesquisa ...',
    '-',
    'Salvar alterações',
    'Cancelar alterações',
    'Atualizar',
    '-',
    'Selecionar/Desselecionar registros',
       'Selecionar registro',
       'Selecionar todos registros',
       '-',
       'Desselecionar registro',
       'Desselecionar todos registros',
    '-',
    'Salvar layout da coluna',
    'Abrir layout da coluna',
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Titulo ';
  SgbData = ' Data ';
  STitleCaption = 'Legenda:';
  STitleAlignment = 'Alinhamento:';
  STitleColor = 'Cor:';
  STitleFont = 'Fonte:';
  SWidth = 'Largura:';
  SWidthFix = 'Fixos';
  SAlignLeft = 'esquerda';
  SAlignRight = 'direita';
  SAlignCenter = 'centro';

const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'diferente';
  strNonMore = 'não maior';
  strNonLess = 'não menor';
  strLessThan = 'menor que';
  strLargeThan = 'maior que';
  strExist = 'vazio';
  strNonExist = 'preenchido';
  strIn = 'na lista';
  strBetween = 'entre';
  strLike = 'parecido';

  strOR = 'OU';
  strAND = 'E';

  strField = 'Campo';
  strCondition = 'Condição';
  strValue = 'Valor';

  strAddCondition = ' defina a condição adicional:';
  strSelection = ' escolha os regritro pelo próxima condição:';

  strAddToList = 'incluir na lista';
  strEditInList = 'Editar a lista';
  strDeleteFromList = 'Excluir da lista';

  strTemplate = 'Dialogo de filtro padrão';
  strFLoadFrom = 'Carregar de...';
  strFSaveAs = 'Salvar como...';
  strFDescription = 'descrição';
  strFFileName = 'Arquivo';
  strFCreate = 'Criado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Somente leitura';
  strFProtectErr = 'Arquivo esta protegido!';

const //for SMDBNavigator
  SFirstRecord = 'Primeiro registro';
  SPriorRecord = 'Registro anterior';
  SNextRecord = 'Próximo registro';
  SLastRecord = 'Último registro';
  SInsertRecord = 'Inserir registro';
  SCopyRecord = 'Copiar registro';
  SDeleteRecord = 'Excluir registro';
  SEditRecord = 'Alterar registro';
  SFilterRecord = 'Condições de filtragem';
  SFindRecord = 'Localizar registro';
  SPrintRecord = 'Imprimir registros';
  SExportRecord = 'Exportar registros';
  SImportRecord = 'Importar os registros';
  SPostEdit = 'Salvar alterações';
  SCancelEdit = 'Cancelar alterações';
  SRefreshRecord = 'Atualizar dados';
  SChoice = 'Escolher registro';
  SClear = 'Limpar escolha de registro';
  SDeleteRecordQuestion = 'Excluir registro?';
  SDeleteMultipleRecordsQuestion = 'Deseja realmente excluir registros selecionados?';
  SRecordNotFound = 'Registro não encontrado';

  SFirstName = 'Primeiro';
  SPriorName = 'Anterior';
  SNextName = 'Próximo';
  SLastName = 'Último';
  SInsertName = 'Inserir';
  SCopyName = 'Copiar';
  SDeleteName = 'Excluir';
  SEditName = 'Alterar';
  SFilterName = 'Filtrar';
  SFindName = 'Localizar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SImportName = 'Importar';
  SPostName = 'Salvar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Atualizar';
  SChoiceName = 'Escolher';
  SClearName = 'Limpar';

  SBtnOk = '&OK';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Carregar';
  SBtnSave = 'Salvar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Colar';
  SBtnClear = 'Limpar';

  SRecNo = '#';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'número válido';
  etValidInteger = 'número inteiro válido';
  etValidDateTime = 'data/hora válida';
  etValidDate = 'data válida';
  etValidTime = 'hora válida';
  etValid = 'válido';
  etIsNot = 'não é um';
  etOutOfRange = 'Valor %s está fora dos limites %s..%s';
  SApplyAll = 'Aplicar em todos';

  SNoDataToDisplay = '<Sem registros>';

implementation

end.
