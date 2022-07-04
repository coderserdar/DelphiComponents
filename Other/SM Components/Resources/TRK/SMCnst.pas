unit SMCnst;

interface

{English strings}
const
 strMessage = 'Yazdýr...';
 strSaveChanges = 'Database Server''a bilgileri yazmak istediðinize emin misiniz?';
 strErrSaveChanges = 'Veriler kaydedilemedi. Veri tabaný baðlantýsýný ve/veya datalarý kontrol ediniz.';
 strDeleteWarning = '%s tablosunu gerçekten silmek istiyor musunuz?';
 strEmptyWarning = '%s tablosunu gerçekten boþaltmak istiyor musunuz?';

const
 PopUpCaption: array [0..24] of string[33] = 
 ('Kayýt ekle', 
 'Araya ekle', 
 'Kaydý düzelt', 
 'Kaydý sil', 
 '-', 
 'Yazdýr ...', 
 'Ýhraç ...', 
 'Filtreleme', 
 'Ara-Bul...', 
 '-', 
 'Deðiþiklikleri kaydet', 
 'Deðiþiklikleri iptal et', 
 'Görüntüyü Yenile', 
 '-', 
 'Kaydý seç/seçimi kaldýr', 
 'Kaydý seç', 
 'Tüm kayýtlarý seç', 
 '-', 
 'Seçimden çýkar', 
 'Tüm Kayýt seçimini kaldýr', 
 '-', 
 'Sütun yapýsýný kaydet', 
 'Sütun yapýsýný yükle', 
 '-', 
 'Ayarlar...'); 
 
const //for TSMSetDBGridDialog
 SgbTitle = ' Baþlýk ';
 SgbData = ' Veri ';
 STitleCaption ='Manþet';
 STitleAlignment = 'Yön:';
 STitleColor = 'Arkaplan';
 STitleFont = 	'Yazý karakteri';
 SWidth = 	'Geniþlik';
 SWidthFix = 	'karakter';
 SAlignLeft = 	'sol';
 SAlignRight = 'sað';
 SAlignCenter = 'merkez';
 
const //for TSMDBFilterDialog
 strEqual =	'eþit';
 strNonEqual = 	'eþit deðil';
 strNonMore = 	'daha fazla deðil';
 strNonLess = 'daha az deðil';
 strLessThan = 'daha küçük';
 strLargeThan = 	'daha büyük';
 strExist =	'boþ';
 strNonExist = 	'boþ deðil';
 strIn = 'liste içinde';
 strBetween = 'arasýnda';
 strLike = 'benzer';

 strOR = 	'VEYA';
 strAND = 		'VE';

 strField = 	'Alan';
 strCondition = 	'Kriter';
 strValue =	'Deðer';

 strAddCondition = ' Ek kriteri tanýmlayýn:';
 strSelection = ' Diðer kritere dayanarak kayýtlarý seçin: ';
 strAddToList = 'Listeye ekle';
 strEditInList = 'Liste içinde düzenle';
 strDeleteFromList = 'Listeden sil';

 strTemplate = 'Þablon diyaloðu süz';
 strFLoadFrom = 'Dosyadan Yükle';
 strFSaveAs = 'Farklý kaydet...';
 strFDescription = 'Taným';
 strFFileName = 'Dosya adý';
 strFCreate = '%s Yaratýldý';
 strFModify = '%s Deðiþtirildi';
 strFProtect = 'Yazmaya karþý korumalý';
 strFProtectErr = 'Dosya korunuyor!';

const //for SMDBNavigator
 SFirstRecord = 	'Ýlk kayýt';
 SPriorRecord =	'Önceki kayýt';
 SNextRecord =	'Sonraki kayýt';
 SLastRecord =	'Son kayýt';
 SInsertRecord = 	'Kayýt ekle';
 SCopyRecord =		'Kaydý kopyala';
 SDeleteRecord =	'Kaydý sil';
 SEditRecord =	'Kaydý düzenle';
 SFilterRecord = 	'Kriterlere göre süz';
 SFindRecord = 	'Kaydý ara';
 SPrintRecord = 'Kaydý bas';
 SExportRecord = 'Kayýtlarýn ihracý';
 SPostEdit = 'Deðiþiklikleri kaydet';
 SCancelEdit = 'Deðiþiklikleri sil';
 SRefreshRecord = 'Veri güncelle';
 SChoice ='Bir kayýt seç';
 SClear = 'Seçili kaydý temizle';
 SDeleteRecordQuestion = 'Kayýt silinsin mi?';
 SDeleteMultipleRecordsQuestion ='Seçili kaydý silmek istediðinizden emin misiniz?';
 SRecordNotFound = 'Kayýt bulunamadý';

 SFirstName = 'Ýlk';
 SPriorName = 'Önceki';	
 SNextName = 'Sonraki';
 SLastName = 'Son';
 SInsertName = 'Ekle';
 SCopyName = 'Kopyala';
 SDeleteName = 'Sil';
 SEditName = 'Düzenle';
 SFilterName = 'Süz';
 SFindName = 'Bul';
 SPrintName = 'Yazdýr';
 SExportName = 'Taþý';
 SPostName = 'Kaydet';
 SCancelName = 'Ýptal';
 SRefreshName = 'Yenile';
 SChoiceName ='Seç';
 SClearName = 'Temizle';

 SBtnOk = '&TAMAM';
 SBtnCancel = '&Ýptal';
 SBtnLoad = 'Yükle';
 SBtnSave = 'Kaydet';
 SBtnCopy = 'Kopyala';
 SBtnPaste = 'Yapýþtýr';
 SBtnClear = 'Temizle';

 SRecNo = 'Kayýt No.';
 SRecOf = ' / ';
 SApplyAll = 'Tümüne Uygula';

const //for EditTyped
 etValidNumber = 'gecerli sayý';
 etValidInteger = 'gecerli tamsayý';
 etValidDateTime = 'geçerli tarih/saat';
 etValidDate = 'geçerli tarih';
 etValidTime = 'geçerli saat';
 etValid = 'geçerli';
 etIsNot = 'deðildir';
 etOutOfRange = '%s deðeri (%s..%s arasýndan)';
implementation

end.
