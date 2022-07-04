unit SMCnst;

interface

{Chinese GB strings}

const
  strMessage = '打印...';
  strSaveChanges = '是否确认保存变更到数据库中?';
  strErrSaveChanges = '保存失败, 请检查数据库连接.';
  strDeleteWarning = '是否确认删除表 %s?';
  strEmptyWarning = '是否确认清空表 %s?';

const
  PopUpCaption: array [0..24] of string[33] =  // I think "string" is better
   ('增加记录',
    '插入记录',
    '编辑记录',
    '删除记录',
    '-',
    '打印...',
    '导出...',
    '过滤...',
    '查找...',
    '-',
    '保存变更',
    '放弃保存变更',
    '刷新',
    '-',
    '记录选取',
       '选取当前记录',
       '全选',
       '-',
       '不选取当前记录',
       '全不选',
    '-',
    '保存列布局',
    '恢复列布局',
    '-',
    '设置...');

const //for TSMSetDBGridDialog
  SgbTitle = ' 标题 ';
  SgbData = ' 数据 ';
  STitleCaption = '标题:';
  STitleAlignment = '对齐:';
  STitleColor = '背景:';
  STitleFont = '字体:';
  SWidth = '宽度:';
  SWidthFix = '字符';
  SAlignLeft = '左';
  SAlignRight = '右';
  SAlignCenter = '居中';

const //for TSMDBFilterDialog
  strEqual = '等于';
  strNonEqual = '不等于';
  strNonMore = '不大于';
  strNonLess = '不小于';
  strLessThan = '小于';
  strLargeThan = '大于';
  strExist = '为空';
  strNonExist = '不为空';
  strIn = '在列表中';
  strBetween = '在范围内';
  strLike = '包含'; // 模糊匹配

  strOR = '或者';
  strAND = '并且';

  strField = '字段';
  strCondition = '条件';
  strValue = '值';

  strAddCondition = ' 定义额外条件:';
  strSelection = ' 选择下一条件的记录:';

  strAddToList = '添加到列表';
  strEditInList = '编辑列表项';
  strDeleteFromList = '从列表中删除';

  strTemplate = '过滤模板对话框';
  strFLoadFrom = '装载...';
  strFSaveAs = '另存为..';
  strFDescription = '描述';
  strFFileName = '文件名';
  strFCreate = '创建: %s';
  strFModify = '修改: %s';
  strFProtect = '重写保护';
  strFProtectErr = '文件被保护!';

const //for SMDBNavigator
  SFirstRecord = '第一条记录';
  SPriorRecord = '上一条记录';
  SNextRecord = '下一条记录';
  SLastRecord = '最后一条记录';
  SInsertRecord = '插入记录';
  SCopyRecord = '复制记录';
  SDeleteRecord = '删除记录';
  SEditRecord = '编辑记录';
  SFilterRecord = '过滤条件';
  SFindRecord = '查找记录';
  SPrintRecord = '打印记录';
  SExportRecord = '导出记录';
  SPostEdit = '保存变更';
  SCancelEdit = '取消变更';
  SRefreshRecord = '刷新数据';
  SChoice = '选择一条记录';
  SClear = '清除选择记录';
  SDeleteRecordQuestion = '删除记录?';
  SDeleteMultipleRecordsQuestion = '是否确认删除选取的记录?';
  SRecordNotFound = '记录不存在';

  SFirstName = '第一';
  SPriorName = '上一';
  SNextName = '下一';
  SLastName = '最后';
  SInsertName = '插入';
  SCopyName = '复制';
  SDeleteName = '删除';
  SEditName = '编辑';
  SFilterName = '过滤';
  SFindName = '查找';
  SPrintName = '打印';
  SExportName = '导出';
  SPostName = '保存';
  SCancelName = '取消';
  SRefreshName = '刷新';
  SChoiceName = '选择';
  SClearName = '清除'; //???

  SBtnOk = '确定(&O)';
  SBtnCancel = '取消(&C)';
  SBtnLoad = '装载';
  SBtnSave = '保存';
  SBtnCopy = '复制';
  SBtnPaste = '粘贴';
  SBtnClear = '清除';

  SRecNo = '记录 ';
  SRecOf = ' 共 ';

const //for EditTyped
  etValidNumber = '合法的数字';
  etValidInteger = '合法的整数';
  etValidDateTime = '合法的日期/时间';
  etValidDate = '合法的日期';
  etValidTime = '合法的时间';
  etValid = '合法的';
  etIsNot = '不是一个';
  etOutOfRange = '值 %s 超出范围 %s..%s';

  SApplyAll = '应用到所有';

implementation

end.