unit langChineseGB;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, '选择 "开新档案" 或 "开启旧档" 来建立及开启现有的压缩档' );
  AddStr(   2, '确定' );
  AddStr(   3, '取消' );
  AddStr(   4, '说明(&H)' );
  // unit fmAboutBox
  AddStr( 500, '关於' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, '这些元件都是免费的.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib 函式库:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'BlowFish 演算法的实作由 Greg Carter, CRYPTOCard 提供' );
  AddStr( 507, '自我解压缩程式码由 Oliver Buschjost 提供' );
  AddStr( 508, '网站:' );
  // unit fmTiming
  AddStr( 600, '已花时间 :' );
  AddStr( 601, '剩馀时间 :' );
  // unit fmMain
  AddStr( 700, '开新档案...' );
  AddStr( 701, '开启旧档...' );
  AddStr( 702, '加入(&A)...' );
  AddStr( 703, '解压缩(&E)...' );
  AddStr( 704, '删除(&D)...' );
  AddStr( 705, '放弃(&A)' );
  AddStr( 706, '开新档案' );
  AddStr( 707, '开启旧档' );
  AddStr( 708, '加入档案...' );
  AddStr( 709, '解开档案...' );
  AddStr( 710, '删除档案...' );
  AddStr( 711, '压缩档 (*.mmm)|*.mmm|SFX Archives (*.exe)|*.exe|所有的档案 (*.*)|*.*' );
  AddStr( 712, '压缩档 (*.mmm)|*.mmm|所有的档案 (*.*)|*.*' );
  AddStr( 713, '开启现有的压缩档' );
  AddStr( 714, '建立新的压缩档' );
  AddStr( 715, '开启压缩档的一个分割区段' );
  AddStr( 718, '%d 个档案, %s' );
  AddStr( 720, '档案 "%s" 已经存在' );
  AddStr( 721, '您要重设这个压缩档吗 ?' );
  AddStr( 722, '您要删除这个压缩档吗 ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, '选择了 %d 个档案, %s' );
  AddStr( 729, '无效' );
  AddStr( 730, '将目前的压缩档更名为:' );
  AddStr( 731, '无法将压缩档更名为 "%s" !' );
  AddStr( 732, '自解档(SFX) 组态设定' );
  AddStr( 733, '建立一个自我解压缩档案' );
  AddStr( 734, '建立' );
  AddStr( 735, '无法建立自解档 !' );
  AddStr( 736, '设定压缩档注解' );
  AddStr( 737, '压缩档注解' );
  AddStr( 738, '程序进行中, 请等待直到工作完成, 或者按[放弃]来中断。' );
  AddStr( 739, '您已经执行了某个档案, 请将其结束然後在试一次。' );
  AddStr( 740, '你必须先开启一个压缩档。' );
  AddStr( 741, '找不到与档案类型 %s 关联的程式' );
  AddStr( 742, '档名' );
  AddStr( 743, '日期' );
  AddStr( 744, '时间' );
  AddStr( 745, '长度' );
  AddStr( 746, '比例' );
  AddStr( 747, '压缩後的长度' );
  AddStr( 748, '分割#' );
  AddStr( 749, '路径' );
  AddStr( 750, '档案(&F)' );
  AddStr( 751, '动作(&A)' );
  AddStr( 752, '选项(&O)' );
  AddStr( 753, '说明(&H)' );
  AddStr( 754, '开新档案(&N)...' );
  AddStr( 755, '开启旧档(&O)...' );
  AddStr( 756, '开启一个分割档(&S)...' );
  AddStr( 757, '关闭压缩档(&C)' );
  AddStr( 758, '&资讯...' );
  AddStr( 759, '更改压缩档名称(&N)' );
  AddStr( 760, '重设压缩档(&R)' );
  AddStr( 761, '删除压缩档(&D)' );
  AddStr( 762, '结束(&Q)' );
  AddStr( 763, '检视(&V)...' );
  AddStr( 764, '全选(&S)' );
  AddStr( 765, '建立可执行档(&M)' );
  AddStr( 766, '设定压缩档注解...' );
  AddStr( 767, '自解档(&SFX) 组态设定...' );
  AddStr( 769, '关於(&A)...' );
  AddStr( 770, '建立新的压缩档' );
  AddStr( 771, '开启现存的压缩档' );
  AddStr( 772, '开启压缩档的一个分割区段' );
  AddStr( 773, '关闭压缩档' );
  AddStr( 774, '显示压缩档相关资讯' );
  AddStr( 775, '更改目前压缩档的名称...' );
  AddStr( 776, '重设压缩档内容' );
  AddStr( 777, '删除压缩档' );
  AddStr( 778, '结束程式' );
  AddStr( 781, '加入档案' );
  AddStr( 782, '解压缩档案' );
  AddStr( 783, '删除档案' );
  AddStr( 784, '检视档案' );
  AddStr( 785, '选择所有档案' );
  AddStr( 786, '建立自解档' );
  AddStr( 787, '为目前的压缩档定义注解' );
  AddStr( 788, '变更设定' );
  AddStr( 789, '变更自解档设定' );
  AddStr( 790, '关於本程式' );
  AddStr( 798, '设定(&C)...' );
  AddStr( 799, '%s 档案' );
  AddStr( 800, '关闭档案...' );
  AddStr( 801, '取消选择(&N)' );
  AddStr( 802, '反向选择(&I)' );
  AddStr( 803, '根' );
  AddStr( 804, '树状检视' );
  AddStr( 805, '大图示' );
  AddStr( 806, '小图示' );
  AddStr( 807, '清单' );
  AddStr( 808, '详细资料' );
  AddStr( 809, '全部展开' );
  AddStr( 810, '全部闭合' );
  AddStr( 809, '完全展开' );
  AddStr( 810, '完全闭合' );
  AddStr( 811, '清除档案清单' );
  AddStr( 812, '建立档案清单' );
  AddStr( 813, '排序档案清单' );
  AddStr( 814, '%s'#10#13'压缩档不存在!' );
  AddStr( 815, '检查完整性' );
  AddStr( 816, '检查压缩档的完整性' );
  AddStr( 817, '检视上一次的输出(&V)...' );
  AddStr( 818, '检视上一次操作的输出结果' );
  AddStr( 819, '安装' );
  AddStr( 820, '解压缩并执行安装程式' );
  AddStr( 821, '字型(&F)...' );
  AddStr( 822, '排序(&S)' );
  AddStr( 823, '原始顺序(&O)' );
  AddStr( 824, '变更目前的字型' );
  AddStr( 825, '选择排序方式' );
{*}  AddStr( 826, 'F&ilters...' );
{*}  AddStr( 827, 'Lets you define filters to select files to be added' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, '加入哪些档案' );
  AddStr( 901, '名称 :' );
  AddStr( 902, '资料夹' );
  AddStr( 903, '搜寻子资料夹' );
  AddStr( 904, '包含目前路径' );
  AddStr( 905, '储存空资料夹' );
  AddStr( 906, '路径储存方式 :' );
  AddStr( 907, '是否将档案加密 ?' );
  AddStr( 908, '压缩等级:' );
  AddStr( 909, '加入' );
  AddStr( 910, '无'+#13+
               '完整'+#13+
               '相对' );
  AddStr( 911, '最大 (最慢)'+#13+
               '一般'+#13+
               '快速'+#13+
               '超快速'+#13+
               '无' );
  AddStr( 912, '加入拖曳的档案' );
  AddStr( 913, '加入项目' );
  AddStr( 914, '筛选 :' );
  AddStr( 915, '加到目前的资料夹 ?' );
{*}AddStr( 916, 'Filter files ?' );
  // unit fmConfiguration
  AddStr( 1000, '设定' );
  AddStr( 1001, '档案分割' );
  AddStr( 1002, '压缩档的建立' );
  AddStr( 1003, '选项' );
  AddStr( 1004, '切割压缩档' );
  AddStr( 1005, '每个分割档案最大容量:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                '其他 (Kb):' );
  AddStr( 1007, '使用压缩' );
  AddStr( 1008, '使用加密' );
  AddStr( 1009, '强固压缩(Solid)' );
  AddStr( 1010, '唯读' );
  AddStr( 1011, '建立自解档' );
  AddStr( 1014, '区块大小' );
  AddStr( 1015, '保留空间' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, '语系:' );
  AddStr( 1018, '自动'+#13+
                '英文'+#13+
                '法文'+#13+
                'BIG5中文'+#13+
                'GB中文'+#13+
                '葡萄牙文'+#13+
                '德文'+#13+
                '义大利文'+#13+
                '苏联文'+#13+
                '西班牙文'+#13+
{*}             'Danish'+#13+
                'Dutch'+#13+
                'Czech'
                );
  AddStr( 1019, '显示空的资料夹' );
  AddStr( 1020, '显示树状结构' );
  // unit fmCreateFolder
  AddStr( 1100, '目前资料夹:' );
  AddStr( 1101, '名称:' );
  // unit fmDelete
  AddStr( 1200, '删除' );
  AddStr( 1201, '档案' );
  AddStr( 1202, '整个压缩档(&E)'+#13+
                '选择的档案(&S)'+#13+
                '档案(&F):' );
  // unit fmEnterCryptKey
  AddStr( 1300, '系统讯息e' );
  AddStr( 1301, '隐藏密码 ?' );
  // unit fmExtract
  AddStr( 1400, '解压缩' );
  AddStr( 1401, '加压缩至:' );
  AddStr( 1402, '档案' );
  AddStr( 1403, '选择的档案(&S)'+#13+
                '所有档案(&A)'+#13+
                '档案(&I):' );
  AddStr( 1404, '使用资料夹名称(&U)' );
  AddStr( 1405, '覆盖已存在的档案'+#13+
                '略过已存在的档案'+#13+
                '更新较新的档案'+#13+
                '要求确认'+#13+
                '只回存已存在的档案'+#13+
                '更新已存在的档案' );
  AddStr( 1406, '资料夹 / 磁碟机' );
  AddStr( 1407, '新资料夹...' );
  // unit fmHelpOnSFX
  AddStr( 1500, '以下关键字可能会使用於"命令列"及'+#13+
                '"内定解压缩路径" 栏位 :' );
  AddStr( 1501, '将会被暂存目录所取代'+#13+
                '(通常是 ''c:\windows\temp'' 或 ''c:\win95\temp'' 或 ''c:\temp'')' );
  AddStr( 1502, '将会被 Windows 目录所取代'+#13+
                '(通常是 ''c:\windows'' 或 ''c:\win95'')' );
  AddStr( 1503, '将会被 System 目录所取代'+#13+
                '(通常是 ''c:\windows\system'' 或 ''c:\win95\system'')' );
  AddStr( 1504, '将会被 Program Files 目录所取代'+#13+
                '(通常是 ''c:\Program Files'' [视 Windows 安装的语言而定])' );
  AddStr( 1505, '将会被档案欲解压至的目录所取代'+#13+
                '(只适用於 "命令列" 或 "引数" 栏位)' );
  AddStr( 1506, '  例:' );
  AddStr( 1507, '<PF>MyCompany\MyStuff' );
  // unit fmInformation
  AddStr( 1600, '路径:' );
  AddStr( 1601, '名称:' );
  AddStr( 1602, '档案大小:' );
  AddStr( 1603, '档案:' );
  AddStr( 1604, '压缩:' );
  AddStr( 1605, '日期/时间:' );
  AddStr( 1606, '分割区段:' );
  AddStr( 1607, '属性' );
  AddStr( 1608, '已加密' );
  AddStr( 1609, '已压缩' );
  AddStr( 1610, '强固压缩(Solid)' );
  AddStr( 1611, '唯读' );
  AddStr( 1612, '最後的分割区段' );
  AddStr( 1613, '资讯' );
  // unit fmSFXcomments
  AddStr( 1700, '注解' );
  AddStr( 1701, '开启自解档时显示注解' );
  AddStr( 1702, '解开自解档之後显示注解' );
  AddStr( 1703, '清除注解' );
  // unit fmSFXConfig
  AddStr( 1800, '自解档设定' );
  AddStr( 1801, '解压缩後执行档案 ?' );
  AddStr( 1802, '使用者选择要解压缩的档案 ?' );
  AddStr( 1803, '使用者选择覆盖模式 ?');
  AddStr( 1804, '标题:' );
  AddStr( 1805, '命令列:' );
  AddStr( 1806, '引数:' );
  AddStr( 1807, '内定解压缩路径:' );
  AddStr( 1808, '档案覆盖模式:' );
  AddStr( 1809, '注解...' );
  AddStr( 1810, '要求确认'+#13+
                '覆盖已存在的档案'+#13+
                '略过已存在的档案'+#13+
                '只覆盖较新的档案'+#13+
                '只还原已存在的档案'+#13+
                '只解压缩已存在而且比较新的档案' );
  AddStr( 1811, '允许使用者不执行这个档案 ?' );
  // unit fmTextViewer
  AddStr( 1900, '检视: %s' );
  AddStr( 1901, '复制剪贴簿(&C)' );
  AddStr( 1902, '字型(&F)' );
  // unit fmView
  AddStr( 2000, '检视 : %s' );
  AddStr( 2001, '使用' );
  AddStr( 2002, '检视' );
  AddStr( 2003, '登记关联的程式(&A) (%s)'+#13+
                '内建的 ASCII 文字浏览器' );
  // unit fmLastOutput
  AddStr( 2100, '检视上次输出' );
  // unit fmFilters
{*}  AddStr( 2200, 'Filters' );
{*}  AddStr( 2202, '新增' );
{*}  AddStr( 2203, '编辑' );
{*}  AddStr( 2204, '删除' );
{*}  AddStr( 2205, '全部清除' );
{*}  AddStr( 2206, 'filter类型' );
{*}  AddStr( 2207, '包含'+#13+
{*}                '排除' );
{*}  AddStr( 2208, '修改filter:' );
{*}  AddStr( 2209, '新增filter:' );
end;

end.
