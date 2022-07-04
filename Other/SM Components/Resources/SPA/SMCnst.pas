{ Copyright (C) 1998-2007, written by Mike Shkolnik, Scalabium Software
  E-Mail: mshkolnik@scalabium
  WEB: http://www.scalabium.com

  Const strings for localization
  freeware SMComponent library
}
unit SMCnst;

interface

{Spanish strings}
const
  strMessage = 'Imprimir...';
  strSaveChanges = 'Confirma la grabación de datos en el servidor';
  strErrSaveChanges = '¡No se puede grabar! Comprueba la conexión con el servidor o los datos.';
  strDeleteWarning = '¿Seguro que quiere eliminar la tabla %s?';
  strEmptyWarning = '¿Seguro que quiere vaciar la tabla %s?';     // Here you can use "limpiar" instead of "vaciar" too

const
  PopUpCaption: array [0..24] of string[33] =
   ('Agregar registro',
    'Insertar registro',
    'Editar registro',
    'Eliminar registro',
    '-',
    'Imprimir ...',
    'Exportar ...',
    'Filtrar...',
    'Buscar...',
    '-',
    'Guardar cambios',
    'Cancelar cambios',
    'Actualizar',
    '-',
    'Seleccionar/Deseleccionar registros',
       'Seleccionar registro',
       'Seleccionar todos los registros',
       '-',
       'Deseleccionar registro',
       'Deseleccionar todos los registros',
    '-',
    'Guardar formato de columna',
    'Recuperar formato de columna',    // Here you can use "recuperar" or "abrir"
    '-',
    'Configurar...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Título ';
  SgbData = ' Datos ';
  STitleCaption = 'Título:';
  STitleAlignment = 'Alineación:';
  STitleColor = 'Fondo:';
  STitleFont = 'Fuente:';
  SWidth = 'Ancho:';
  SWidthFix = 'caracteres';
  SAlignLeft = 'izquierda';
  SAlignRight = 'derecha';
  SAlignCenter = 'centro';

const //for TSMDBFilterDialog
  strEqual = 'igual';
  strNonEqual = 'distinto';
  strNonMore = 'menor o igual';
  strNonLess = 'mayor o igual';
  strLessThan = 'menor';
  strLargeThan = 'mayor';
  strExist = 'vacío';                // Empty??????
  strNonExist = 'lleno';             // Fill???????
  strIn = 'en la lista';
  strBetween = 'entre';
  strLike = 'como';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Campo';
  strCondition = 'Condición';
  strValue = 'Valor';

  strAddCondition = ' Definir condición adicional:';
  strSelection = ' Seleccionar registros con las siguientes condiciones:';

  strAddToList = 'Agregar a la lista';
  strEditInList = 'Editar de la lista';
  strDeleteFromList = 'Borrar de la lista';

  strTemplate = 'Dialogo plantilla de filtro';      // I've to check this. When does it appear?
  strFLoadFrom = 'Cargar desde...';
  strFSaveAs = 'Guardar como..';
  strFDescription = 'Descripción';
  strFFileName = 'Archivo';
  strFCreate = 'Creado: %s';
  strFModify = 'Modificado: %s';
  strFProtect = 'Proteger para escritura';           // Not very common... I'm looking for a better expression
  strFProtectErr = '¡Archivo protegido!';            // Not very common... I'm looking for a better expression

const //for SMDBNavigator
  SFirstRecord = 'Primero';
  SPriorRecord = 'Anterior';
  SNextRecord = 'Siguiente';
  SLastRecord = 'Último';
  SInsertRecord = 'Nuevo';
  SCopyRecord = 'Copiar';
  SDeleteRecord = 'Eliminar';
  SEditRecord = 'Modificar';
  SFilterRecord = 'Filtrar';
  SFindRecord = 'Buscar';
  SPrintRecord = 'Imprimir';
  SExportRecord = 'Exportar registros';
  SImportRecord = 'Importar registros';
  SPostEdit = 'Guardar';
  SCancelEdit = 'Deshacer cambios';
  SRefreshRecord = 'Actualizar datos';
  SChoice = 'Elegir registro';
  SClear = 'Vaciar registro';
  SDeleteRecordQuestion = '¿Seguro que quiere eliminar el registro?';
  SDeleteMultipleRecordsQuestion = '¿Seguro que quiere eliminar los registros seleccionados?';
  SRecordNotFound = 'Registro no encontrado';

  SFirstName = 'Primero';
  SPriorName = 'Anterior';
  SNextName = 'Siguiente';
  SLastName = 'Ultimo';
  SInsertName = 'Nuevo';
  SCopyName = 'Copiar';
  SDeleteName = 'Eliminar';
  SEditName = 'Modificar';
  SFilterName = 'Filtrar';
  SFindName = 'Buscar';
  SPrintName = 'Imprimir';
  SExportName = 'Exportar';
  SImportName = 'Importar';
  SPostName = 'Guardar';
  SCancelName = 'Cancelar';
  SRefreshName = 'Actualizar';
  SChoiceName = 'Elegir';
  SClearName = 'Vaciar';               // Here you can use too "Limpiar"

  SBtnOk = '&Aceptar';
  SBtnCancel = '&Cancelar';
  SBtnLoad = 'Cargar';
  SBtnSave = 'Guardar';
  SBtnCopy = 'Copiar';
  SBtnPaste = 'Pegar';
  SBtnClear = 'Vaciar';                // "Limpiar" is valid too

  SRecNo = 'reg.';
  SRecOf = ' de ';

const //for EditTyped
  etValidNumber = 'Número correcto';
  etValidInteger = 'Número entero correcto';
  etValidDateTime = 'Fecha/Hora correcto ';
  etValidDate = 'Fecha correcta';
  etValidTime = 'Hora correcta';
  etValid = 'Correcto';
  etIsNot = 'no es un/a';
  etOutOfRange = 'Valor %s fuera del rango %s..%s';

  SApplyAll = 'Aplicar a todo';

  SNoDataToDisplay = '<No hay datos para mostrar>';

implementation

end.
