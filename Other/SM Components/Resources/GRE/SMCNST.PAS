unit SMCnst;

interface

{Greek strings}
const
  strMessage = 'Εκτύπωση...';
  strSaveChanges = 'Να καταχωρηθούν οι τροποποιήσεις στά αρχεία;';
  strErrSaveChanges = 'Αδυναμία αποθήκευσης δεδομένων.Ελέγξτε τις συνδέσεις του δίκτύου σας.';
  strDeleteWarning = 'Θέλετε να διαγράψετε το αρχείο %s;';
  strEmptyWarning = 'θέλετε να αδειάσετε το αρχείο %s;';

const
  PopUpCaption: array [0..22] of string[33] =
   ('Πρόσθεση εγγραφής',
    'Εισαγωγή εγγραφής',
    'Τροποποίηση εγγραφής',
    'Διαγραφή εγγραφής',
    '-',
    'Εκτύπωση.',
    'Εξαγωγή...',
    '-',
    'Αποθήκευση αλλαγών',
    'Απόρριψη αλλαγών',
    'Ανανέωση',
    '-',
    'Επιλογή/Αποεπιλογή εγγραφών',
       'Επιλογή εγγραφής',
       'Επιλογή όλων ',
       '-',
       'Αποεπιλογή εγγραφής',
       'Αποεπιλογή όλων',
    '-',
    'Αποθήκευση μορφής στήλης',
    'Επαναφορά μορφής στήλης',
    '-',
    'Ορισμός...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Τίτλος ';
  SgbData = ' Δεδομένα ';
  STitleCaption = 'Εττικέτα:';
  STitleAlignment = 'Ευθυγράμμιση:';
  STitleColor = 'Φόντο:';
  STitleFont = 'Γραμματοσειρά:';
  SWidth = 'Πλάτος:';
  SWidthFix = 'χαρακτήρες';
  SAlignLeft = 'Αριστερά';
  SAlignRight = 'Δεξιά';
  SAlignCenter = 'Κέντρο';

const //for TSMDBFilterDialog
  strEqual = 'ίσο';
  strNonEqual = 'διάφορο';
  strNonMore = 'όχι μεγαλύτερο';
  strNonLess = 'όχι μικρότερο';
  strLessThan = 'μικρότερο';
  strLargeThan = 'μεγαλύτερο';
  strExist = 'κενό';
  strNonExist = 'όχι κενό';
  strIn = 'στην λίστα';
  strBetween = 'μεταξύ';

  strOR = 'OR';
  strAND = 'AND';

  strField = 'Πεδίο';
  strCondition = 'Συνθήκη';
  strValue = 'Τιμή';

  strAddCondition = ' Προσδιορισμός της επιπλέον συνθήκης:';
  strSelection = ' Επιλογή εγγραφών σύμφωνα με τις παρακάτω συνθήκες:';

  strAddToList = 'Προσθήκη στη λίστα';
  strDeleteFromList = 'Διαγραφή από τη λίστα';

  strTemplate = 'Διάλογος περιεχομένων φίλτρου';
  strFLoadFrom = 'Αναγνωση από...';
  strFSaveAs = 'Αποθήκευση ώς..';
  strFDescription = 'Περιγραφή';
  strFFileName = 'Ονομα αρχείου';
  strFCreate = 'Δημιουργήθηκε: %s';
  strFModify = 'Τροποιποιήθηκε: %s';
  strFProtect = 'Προστασία διαγραφής';
  strFProtectErr = 'Το αρχείο είναι προστατευμένο !';

const //for SMDBNavigator
  SFirstRecord = 'Πρώτη εγγραφή';
  SPriorRecord = 'Προηγούμενη εγγράφή';
  SNextRecord = 'Επόμενη εγγραφή';
  SLastRecord = 'Τελευταία εγγραφή';
  SInsertRecord = 'Εισαγωγή εγγραφής';
  SCopyRecord = 'Αντιγραφή εγγραφής';
  SDeleteRecord = 'Διαγραφή εγγραφής';
  SEditRecord = 'Διόρθωση εγγραφής';
  SFilterRecord = 'Συνθήκες Φίλτρου';
  SFindRecord = 'Αναζήτηση στις εγγραφές';
  SPrintRecord = 'Εκτύπωση των εγγραφών';
  SExportRecord = 'Εξαγωγή των εγγραφών';
  SPostEdit = 'Αποθήκευση αλλαγών';
  SCancelEdit = 'Ακύρωση αλλαγών';
  SRefreshRecord = 'Ανανέωση δεδομένων';
  SChoice = 'Επιλογή εγγραφής';
  SClear = 'Ακύρωση επιλογής εγγραφής';
  SDeleteRecordQuestion = 'Να διαγραφή η εγγραφή;';
  SDeleteMultipleRecordsQuestion = 'Θέλετε σίγουρα να διαγράψετε τις επιλεγμένες εγγραφές;';
  SRecordNotFound = 'Η εγγραφή δενμ βρέθηκε';

  SFirstName = 'Πρώτη';
  SPriorName = 'Προηγούμενη';
  SNextName = 'Επόμενη';
  SLastName = 'Τελευταία';
  SInsertName = 'Εισαγωγή';
  SCopyName = 'Αντιγραφή';
  SDeleteName = 'Διαγραφή';
  SEditName = 'Διόρθωση';
  SFilterName = 'Φίλτρο';
  SFindName = 'Αναζήτηση';
  SPrintName = 'Εκτύπωση';
  SExportName = 'Εξαγωγή';
  SPostName = 'Αποθήκευση';
  SCancelName = 'Ακύρωση';
  SRefreshName = 'Ανανέωση';
  SChoiceName = 'Επιλογή';
  SClearName = 'Καθαρισμός';

  SBtnOk = '&Αποδοχή';
  SBtnCancel = 'Α&κύρωση';
  SBtnLoad = 'Φόρτωμα';
  SBtnSave = 'Αποθήκευση';
  SBtnCopy = 'Αντιγραφή';
  SBtnPaste = 'Επικόληση';
  SBtnClear = 'Καθαρισμός';

  SRecNo = 'Εγγρ.';
  SRecOf = ' από ';

const //for EditTyped
  etValidNumber = 'αποδεκτός αριθμός';
  etValidInteger = 'αποδεκτή ακέραια τιμή';
  etValidDateTime = 'αποδεκτή ημερομηνία/ώρα';
  etValidDate = 'αποδεκτή ημερομηνία';
  etValidTime = 'αποδεκτή ώρα';
  etValid = 'αποδεκτός';
  etIsNot = 'δεν είναι ';
  etOutOfRange = 'Η Τιμή %s είναι εκτός των ορίων %s..%s';

implementation

end.
