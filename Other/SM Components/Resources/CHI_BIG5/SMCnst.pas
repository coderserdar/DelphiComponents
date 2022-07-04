unit SMCnst;

interface

{Chinese Traditional strings}
const
  strMessage = '列印...';
  strSaveChanges = '確定儲存變更到資料庫?';
  strErrSaveChanges = '無法儲存資料! 請檢查伺服器連線或資料正確性.';
  strDeleteWarning = '確定要刪除資料表 %s?';
  strEmptyWarning = '確定要清空資料表 %s?';

const
  PopUpCaption: array [0..24] of string[33] =
   ('新增紀錄',
    '插入紀錄',
    '編輯紀錄',
    '刪除紀錄',
    '-',
    '列印 ...',
    '輸出 ...',
    '過濾 ...',
    '搜尋 ...',
    '-',
    '儲存變更',
    '取消變更',
    '更新',
    '-',
    '選取/取消選取 紀錄',
       '選取紀錄',
       '選取全部紀錄',
       '-',
       '取消選取紀錄',
       '取消選取全部紀錄',
    '-',
    '儲存欄位 layout',
    '還原欄位 layout',
    '-',
    '設定...');

const //for TSMSetDBGridDialog
  SgbTitle = ' 標題 ';
  SgbData = ' 資料 ';
  STitleCaption = '標題:';
  STitleAlignment = '對齊:';
  STitleColor = '背景:'; 
  STitleFont = '字型:';
  SWidth = '寬度:';
  SWidthFix = 'characters';
  SAlignLeft = '向左對齊';
  SAlignRight = '向右對齊';
  SAlignCenter = '置中對齊';
  
const //for TSMDBFilterDialog
  strEqual = '等於';
  strNonEqual = '不等於';
  strNonMore = '不大於';
  strNonLess = '不小於';
  strLessThan = '小於';
  strLargeThan = '大於';
  strExist = '空值';
  strNonExist = '非空值';
  strIn = '在清單中';
  strBetween = '介於';
  strLike = 'like';

  strOR = '或';
  strAND = '且';

  strField = '欄位';
  strCondition = '條件';
  strValue = '值';

  strAddCondition = ' 定義更多條件:';
  strSelection = ' 依照下列條件選擇紀錄:';

  strAddToList = '新增清單';
  strEditInList = '編輯清單';
  strDeleteFromList = '刪除清單';

  strTemplate = '過濾樣本視窗';
  strFLoadFrom = '載入...';
  strFSaveAs = '儲存為..';
  strFDescription = '描述';
  strFFileName = '檔案名稱';
  strFCreate = '新建: %s';
  strFModify = '修改: %s';
  strFProtect = '防止覆寫';
  strFProtectErr = '檔案不可覆寫!';

const //for SMDBNavigator
  SFirstRecord = '第一筆紀錄';
  SPriorRecord = '上一筆記錄';
  SNextRecord = '下一筆記錄';
  SLastRecord = '最後一筆記錄';
  SInsertRecord = '新增紀錄';
  SCopyRecord = '複製紀錄';
  SDeleteRecord = '刪除紀錄';
  SEditRecord = '編輯紀錄';
  SFilterRecord = '過濾條件';
  SFindRecord = '搜尋紀錄';
  SPrintRecord = '列印紀錄';
  SExportRecord = '匯出紀錄';
  SPostEdit = '儲存變更';
  SCancelEdit = '取消變更';
  SRefreshRecord = '更新資料';
  SChoice = '選擇一筆記錄';
  SClear = '清除選取的紀錄';
  SDeleteRecordQuestion = '刪除紀錄?';
  SDeleteMultipleRecordsQuestion = '確定要刪除選取的紀錄?';
  SRecordNotFound = '找不到紀錄';

  SFirstName = '第一筆';
  SPriorName = '上一筆';
  SNextName = '下一筆';
  SLastName = '最後一筆';
  SInsertName = '新增';
  SCopyName = '複製';
  SDeleteName = '刪除';
  SEditName = '編輯';
  SFilterName = '過濾';
  SFindName = '搜尋';
  SPrintName = '列印';
  SExportName = '匯出';
  SPostName = '儲存';
  SCancelName = '取消';
  SRefreshName = '更新';
  SChoiceName = '選擇';
  SClearName = '清除';

  SBtnOk = '確定 (&O)';
  SBtnCancel = '取消 (&C)';
  SBtnLoad = '載入';
  SBtnSave = '儲存';
  SBtnCopy = '複製';
  SBtnPaste = '貼上';
  SBtnClear = '清除';

  SRecNo = 'rec.';
  SRecOf = ' of ';

const //for EditTyped
  etValidNumber = '合法數字';
  etValidInteger = '合法整數';
  etValidDateTime = '合法日期/時間';
  etValidDate = '合法日期';
  etValidTime = '合法時間';
  etValid = '合法';
  etIsNot = '不是';
  etOutOfRange = '數值 %s 超出範圍 %s..%s';

  SApplyAll = '套用到全部';
  
implementation

end.
