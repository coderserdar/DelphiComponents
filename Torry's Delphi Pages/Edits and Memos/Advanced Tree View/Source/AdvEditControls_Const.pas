unit AdvEditControls_Const;

interface

const
// English
  SWrongFieldType = 'Wrong field data type';
  SInvalidMemoSize = 'Text exceeds memo capacity';

  SInsert = 'Insert';
  SModify = 'Modify';
  SDelete = 'Delete';
  SCancel = 'Cancel';

  SCustomColor = 'Custom color ...';
  SSelect = 'Select';
  SColor_list = 'Color list';

// TAdvDBTreeView
  SNode = ' node';
  SAddNodeOnThisLavel = SInsert + ' sibling' + SNode; // SInsert + SNode + ' on this level';
  SAddChildNode =  SInsert + ' child' + SNode;
  SDeleteNode = SDelete + SNode;
  SConfirmNodeDelete = SDeleteNode + ' ?';
  SAttachNode = 'Attach' + SNode;
  SCopyNode = 'Copy' + SNode;
  SMoveNode = 'Move' + SNode;
  SPasteNode = 'Paste' + SNode;
  SForeignNode = 'Foreign' + SNode;
  STheNode_AlreadyOwns_ThisNode = 'The Node %s already owns %s';
{
// German
  SWrongFieldType = 'Falscher Datentyp vom Feld';
  SInvalidMemoSize = 'Text übersteigt Memokapazität';
  SInsert = 'Fügen ein';
  SModify = 'Ändere';
  SDelete = 'Lösche';
  SCancel = 'Breche ab';

// Polish
  SWrongFieldType = 'B³êdny typ pola';
  SInvalidMemoSize = 'Text przekracza pjemnoœæ memo';
  SInsert = 'Dodaj';
  SModify = 'Popraw';
  SDelete = 'Usuñ';
  SCancel = 'Anuluj';
}

implementation

end.
