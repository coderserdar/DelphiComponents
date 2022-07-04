unit SMCnst;

interface

{Indonesian strings}
const
  strMessage = 'Mencetak...';
  strSaveChanges = 'Rekam perubahan-perubahan pada Database Server?';
  strErrSaveChanges = 'Gagal merekam data! Cek Server connection atau data validation.';
  strDeleteWarning = 'Hapus table %s?';
  strEmptyWarning = 'Hapus semua record dari table %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('Tambah record',
    'Sisipkan record',
    'Ubah record',
    'Hapus record',
    '-',
    'Cetak ...',
    'Export ...',
    'Filter ...',
    'Cari ...',
    '-',
    'Save perubahan-perubahan',
    'Batalkan perubahan-perubahan',
    'Refresh',
    '-',
    'Pilih/jangan pilih record-2',
       'Pilih record',
       'Pilih semua record',
       '-',
       'Jangan pilih record',
       'Jangan pilih semua record',
    '-',
    'Save layout kolom',
    'Restore layout kolom',
    '-',
    'Setup...');

const //for TSMSetDBGridDialog
  SgbTitle = ' Title ';
  SgbData = ' Data ';
  STitleCaption = 'Caption:';
  STitleAlignment = 'Alignment:';
  STitleColor = 'Background:';
  STitleFont = 'Font:';
  SWidth = 'Lebar:';
  SWidthFix = 'huruf';
  SAlignLeft = 'kiri';
  SAlignRight = 'kanan';
  SAlignCenter = 'tengah';

const //for TSMDBFilterDialog
  strEqual = 'sama dengan';
  strNonEqual = 'tidak sama dengan';
  strNonMore = 'tidak lebih dari';
  strNonLess = 'tidak kurang dari';
  strLessThan = 'kurang dari';
  strLargeThan = 'lebih dari';
  strExist = 'kosong';
  strNonExist = 'tidak kosong';
  strIn = 'pada list';
  strBetween = 'diantara';
  strLike = 'seperti';

  strOR = 'ATAU';
  strAND = 'DAN';

  strField = 'Field';
  strCondition = 'Kondisi';
  strValue = 'Nilai';

  strAddCondition = ' Tambahkan kondisi-kondisi:';
  strSelection = ' Pilih record-record sesuai kondisi-kondisi berikut:';

  strAddToList = 'Tambahkan ke list';
  strEditInList = 'Ubah pada list';
  strDeleteFromList = 'Hapus dari list';

  strTemplate = 'Filter template dialog';
  strFLoadFrom = 'Load dari...';
  strFSaveAs = 'Save sebagai..';
  strFDescription = 'Deskripsi';
  strFFileName = 'Nama file';
  strFCreate = 'Dibuat: %s';
  strFModify = 'Diubah: %s';
  strFProtect = 'Di Protek untuk tidak dapat diubah';
  strFProtectErr = 'File diprotek!';

const //for SMDBNavigator
  SFirstRecord = 'Record pertama';
  SPriorRecord = 'Record sebelumnya';
  SNextRecord = 'Record berikutnya';
  SLastRecord = 'Record terakhir';
  SInsertRecord = 'Tambahkan record';
  SCopyRecord = 'Copy record';
  SDeleteRecord = 'Hapus record';
  SEditRecord = 'Ubah record';
  SFilterRecord = 'Kondisi-2 Filter';
  SFindRecord = 'Cari record';
  SPrintRecord = 'Cetak record-2';
  SExportRecord = 'Ekspor record-2';
  SPostEdit = 'Save perubahan-perubahan';
  SCancelEdit = 'Batalkan perubahan-perubahan';
  SRefreshRecord = 'Refresh data';
  SChoice = 'Pilih sebuah record';
  SClear = 'Batalkan pilihan pada sebuah record';
  SDeleteRecordQuestion = 'Hapus record ini?';
  SDeleteMultipleRecordsQuestion = 'Hapus record-2 yang dipilih?';
  SRecordNotFound = 'Record tidak diketemukan';

  SFirstName = 'Pertama';
  SPriorName = 'Sebelumnya';
  SNextName = 'Berikutnya';
  SLastName = 'Terakhir';
  SInsertName = 'Tambah';
  SCopyName = 'Copy';
  SDeleteName = 'Hapus';
  SEditName = 'Ubah';
  SFilterName = 'Filter';
  SFindName = 'Cari';
  SPrintName = 'Cetak';
  SExportName = 'Ekspor';
  SPostName = 'Save';
  SCancelName = 'Batal';
  SRefreshName = 'Refresh';
  SChoiceName = 'Pilih';
  SClearName = 'Reset';

  SBtnOk = '&OK';
  SBtnCancel = '&Batal';
  SBtnLoad = 'Load';
  SBtnSave = 'Save';
  SBtnCopy = 'Copy';
  SBtnPaste = 'Paste';
  SBtnClear = 'Reset';

  SRecNo = 'rec.';
  SRecOf = ' dari ';

const //for EditTyped
  etValidNumber = 'angka valid';
  etValidInteger = 'angka integer valid';
  etValidDateTime = 'tanggal/jam valid';
  etValidDate = 'tanggal valid';
  etValidTime = 'jam valid';
  etValid = 'valid';
  etIsNot = 'bukan sebuah';
  etOutOfRange = 'Nilai %s diluar rentang %s..%s';

  SApplyAll = 'Terapkan ke semua';

implementation

end.
