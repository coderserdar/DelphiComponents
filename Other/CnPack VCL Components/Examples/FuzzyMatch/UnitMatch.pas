unit UnitMatch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnDebug;

type
  TFormFuzzy = class(TForm)
    chkCase: TCheckBox;
    lblSearch: TLabel;
    edtSearch: TEdit;
    mmoResult: TMemo;
    chkScore: TCheckBox;
    pbString: TPaintBox;
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbStringPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPaintStr: string;
    FPaintText: string;
    FMatchedIndexes: TList;
    procedure CopyList(Src, Dest: TList);
    function ListToString(List: TList): string;
  public
    { Public declarations }
  end;

var
  FormFuzzy: TFormFuzzy;

implementation

uses
  CnStrings;

{$R *.DFM}

const
  SAR_STRS: array[1..1390] of string =
    (
      'CleanInplace.bat',
      'Doc',
      'Examples',
      'License.chs.txt',
      'License.cht.txt',
      'License.enu.txt',
      'Packages',
      'Readme.chs.txt',
      'Readme.cht.txt',
      'Readme.enu.txt',
      'Source',
      './Doc:',
      'Design',
      'Develop',
      'Project',
      'Report',
      'Templates',
      './Doc/Design:',
      'Common',
      'Debug',
      'MultiLang',
      'NonVisual',
      'Skin',
      './Doc/Design/Common:',
      'CnClasses开发包基础类.txt',
      './Doc/Design/Debug:',
      'CnDebugger需求与设计说明书.doc',
      './Doc/Design/MultiLang:',
      'CnPack多语组件包概要设计说明书.doc',
      '多语包修改后的设计方案.txt',
      '多语包在专家包中的使用说明.txt',
      '多语包需求说明.txt',
      './Doc/Design/NonVisual:',
      'CnDock系列组件设计说明书.doc',
      'CnTimer组件设计说明书.doc',
      './Doc/Design/Skin:',
      'CnSkin系列组件设计说明书.doc',
      './Doc/Develop:',
      'CnCalendar历法说明.txt',
      'CnDebugger接口帮助文档.doc',
      'CnPackDHibernate帮助文档.doc',
      'CnPack不可视组件帮助文档.doc',
      'CnPack停靠组件帮助文档.doc',
      'CnPack多语组件帮助文档.doc',
      'CnPack平滑字体组件帮助文档.doc',
      'CnPack网络组件帮助文档.doc',
      'CnTimer详解.txt',
      '如何使用编译开关建立可移植的代码.txt',
      './Doc/Project:',
      'CVS使用说明.txt',
      'CnPack公益基金帐目.txt',
      'CnPack公益基金章程.doc',
      'CnPack协同开发预案.htm',
      'CnPack开发方案.doc',
      'CnPack开发组成员手册.doc',
      'CnPack开发组报名申请函.txt',
      'Delphi开发能力自我评测.doc',
      'GIT使用说明.txt',
      'SVN使用说明.txt',
      '如何为CnPack组件包捐献及移植代码.txt',
      './Doc/Report:',
      '2002年10月份工作总结.txt',
      '2002年11月份工作总结.txt',
      '2002年2月份工作总结.txt',
      '2002年3月份工作总结.txt',
      '2002年4月份工作总结.txt',
      '2002年5月份工作总结.txt',
      '2002年6月份工作总结.txt',
      '2002年7月份工作总结.txt',
      '2002年8月份工作总结.txt',
      '2002年9月15日北京聚会会议记要.doc',
      '2002年9月份工作总结.txt',
      '2003年10月11日管理员会议纪要.doc',
      '2003年10月份工作总结.txt',
      '2003年11月份工作总结.txt',
      '2003年12月份与2004年1月份工作总结.txt',
      '2003年4月份工作总结.txt',
      '2003年5月份工作总结.txt',
      '2003年6月份工作总结.txt',
      '2003年7月份工作总结.txt',
      '2003年8月份工作总结.txt',
      '2003年9月份工作总结.txt',
      '2003年第一季度工作总结.txt',
      '2004年2月份与2004年3月份工作总结.txt',
      '2004年4月份与2004年5月份工作总结.txt',
      '2004年6月份与2004年7月份工作总结.txt',
      '2004年8月份9月份10月份工作总结.txt',
      '2004年年度工作总结.txt',
      '2005年第一季度工作总结.txt',
      '2005年第三季度工作总结.txt',
      '2005年第二季度工作总结.txt',
      '2006年工作总结.txt',
      '2007年上半年工作总结.txt',
      '2007年下半年工作总结.txt',
      '2008年上半年工作总结.txt',
      '2008年下半年工作总结.txt',
      '2009年上半年工作总结.txt',
      '2009年下半年工作总结.txt',
      '2010年上半年工作总结.txt',
      '2010年下半年工作总结.txt',
      '2011年上半年工作总结.txt',
      '2011年下半年工作总结.txt',
      '2012年工作总结.txt',
      '2013年工作总结.txt',
      '2014年工作总结.txt',
      '2015年工作总结.txt',
      '2016年工作总结.txt',
      'InfoQ采访稿.txt',
      './Doc/Templates:',
      'CnPack.dot',
      'CnPack_CVSTrac任务单模板.txt',
      'CnPack发布文档模板.dot',
      'CnPack演示文稿模板.pot',
      'CnPack组件帮助文档模板.dot',
      'Delphi单元规范格式.pas',
      'Delphi编码规范.htm',
      '开发组会议记录模版.dot',
      '文本文档标准格式.txt',
      '组件包框架详细设计模板.dot',
      '组件包概要设计模板.dot',
      '组件设计说明书模板.dot',
      './Examples:',
      'AAFont',
      'ADOUpdateSQL',
      'ActiveScript',
      'AntiCheater',
      'AutoOption',
      'BigNumber',
      'BinDiffPatch',
      'BloomFilter',
      'Button',
      'Calendar',
      'CameraEye',
      'CheckTreeView',
      'ColorGrid',
      'Console',
      'Crypt',
      'DH_PODO',
      'DHibernate',
      'DancingLinks',
      'DebugSender',
      'DialUp',
      'Dock',
      'DragResizer',
      'Edit',
      'ErrorProvider',
      'EventBus',
      'EventHook',
      'FilePacker',
      'FileSystemWatcher',
      'FitCurve',
      'FloatConvert_D7',
      'Graphics',
      'HardwareBreakpoint',
      'HardwareInfo',
      'HashTest_D7',
      'HexEditor',
      'Hint',
      'IISCtrl',
      'InProcessAPIHook',
      'Inet',
      'IniCfg',
      'IocpMemPool',
      'IocpSocketAdapter_D7',
      'KeyBlocker',
      'LEDText',
      'LinkedList',
      'MDIBackground',
      'Memo',
      'MemorySearch',
      'Modem',
      'MultiLang',
      'MultiLangMerger',
      'Multicast_D7',
      'NetDecl',
      'ObjAuto',
      'ObjectPool',
      'OpenGLPaintBox',
      'OuterControls',
      'PingIP',
      'PrimeNumber',
      'ProgressForm',
      'RawInput',
      'RedisClient_D7',
      'Ropes',
      'SkinMagic',
      'SkipList',
      'SpinEdit',
      'Stdcallback',
      'SunRiseSet',
      'SysDebugControl',
      'TaskBar',
      'ThreadPool_D7',
      'TrayIcon',
      'Tree',
      'Twain',
      'UDP',
      'ValidateImage',
      'VarList',
      'VolumeCtrl',
      'WaterImage',
      'WinampCtrl',
      'XMLPersistent',
      'XlsWriter',
      'XorIniFile',
      './Examples/AAFont:',
      'AAFont.dof',
      'AAFont.dpr',
      'AAFont.res',
      'Mid.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/ADOUpdateSQL:',
      'CnADOUpdateSQL.mdb',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/ActiveScript:',
      'CnASDemo.cfg',
      'CnASDemo.dof',
      'CnASDemo.dpr',
      'CnASDemo.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/AntiCheater:',
      'AntiCheater.cfg',
      'AntiCheater.dof',
      'AntiCheater.dpr',
      'AntiCheater.res',
      'AntiCheaterTest.dfm',
      'AntiCheaterTest.pas',
      './Examples/AutoOption:',
      'AutoOption.cfg',
      'AutoOption.dof',
      'AutoOption.dpr',
      'AutoOption.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/BigNumber:',
      'BigNumTest.cfg',
      'BigNumTest.dof',
      'BigNumTest.dpr',
      'BigNumTest.res',
      'UnitMain.dfm',
      'UnitMain.pas',
      './Examples/BinDiffPatch:',
      'BinDiffPatch.cfg',
      'BinDiffPatch.dof',
      'BinDiffPatch.dpr',
      'BinDiffPatch.res',
      'BinDiffPatchUnit.dfm',
      'BinDiffPatchUnit.pas',
      './Examples/BloomFilter:',
      'TestBloomFilter.cfg',
      'TestBloomFilter.dof',
      'TestBloomFilter.dpr',
      'TestBloomFilterUnit.dfm',
      'TestBloomFilterUnit.pas',
      './Examples/Button:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      'Unit2.dfm',
      'Unit2.pas',
      './Examples/Calendar:',
      'TestCal.cfg',
      'TestCal.dof',
      'TestCal.dpr',
      'TestCal.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/CameraEye:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/CheckTreeView:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/ColorGrid:',
      'ColorGridDemo.cfg',
      'ColorGridDemo.dof',
      'ColorGridDemo.dpr',
      'ColorGridDemo.res',
      'UfrmMain.dfm',
      'UfrmMain.pas',
      './Examples/Console:',
      'CnConsoleTest.cfg',
      'CnConsoleTest.dpr',
      'CnConsoleTest.res',
      'Mainfrm.dfm',
      'Mainfrm.pas',
      './Examples/Crypt:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/DH_PODO:',
      'CnPODOConsts.pas',
      'CnPODOFormMain.dfm',
      'CnPODOFormMain.pas',
      'CnPODOUtils.pas',
      'PODO.cfg',
      'PODO.dof',
      'PODO.dpr',
      'PODO.res',
      './Examples/DHibernate:',
      'Access',
      'Delphi_6_or_Later.txt',
      'Export',
      'GroupMgr',
      'Import',
      'MasterDetail',
      './Examples/DHibernate/Access:',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      'orz.mdb',
      './Examples/DHibernate/Export:',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/DHibernate/GroupMgr:',
      'Conn.udl',
      'Database',
      'PODO',
      'QQGroupMgr.dpr',
      'frmAddOrEdit.dfm',
      'frmAddOrEdit.pas',
      'frmAddResearch.dfm',
      'frmAddResearch.pas',
      'frmAddWarning.dfm',
      'frmAddWarning.pas',
      'frmMain.dfm',
      'frmMain.pas',
      'frmViewResearch.dfm',
      'frmViewResearch.pas',
      'frmViewWarning.dfm',
      'frmViewWarning.pas',
      './Examples/DHibernate/GroupMgr/Database:',
      'CreateDB.sql',
      './Examples/DHibernate/GroupMgr/PODO:',
      'PODO_CONSTANTS.pas',
      'PODO_MEMBERS.pas',
      'PODO_RESEARCHS.pas',
      'PODO_WARNINGS.pas',
      './Examples/DHibernate/Import:',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      'country.xls',
      './Examples/DHibernate/MasterDetail:',
      'Database',
      'MainDetailDemo.dpr',
      'MainDetailDemo.res',
      'PODO',
      'frmMain.dfm',
      'frmMain.pas',
      './Examples/DHibernate/MasterDetail/Database:',
      'CreateDB.sql',
      './Examples/DHibernate/MasterDetail/PODO:',
      'PODO_DETAILTABLE.pas',
      'PODO_IDG.pas',
      'PODO_MAINTABLE.pas',
      './Examples/DancingLinks:',
      'TestDancingLinks.cfg',
      'TestDancingLinks.dof',
      'TestDancingLinks.dpr',
      'TestDancingLinksUnit.dfm',
      'TestDancingLinksUnit.pas',
      './Examples/DebugSender:',
      'OutPut.cfg',
      'OutPut.dof',
      'OutPut.dpr',
      'OutPut.res',
      'UnitOutput.dfm',
      'UnitOutput.pas',
      'UnitThread.pas',
      './Examples/DialUp:',
      'CnDialUpDemo.dfm',
      'CnDialUpDemo.pas',
      'Demo.cfg',
      'Demo.dof',
      'Demo.dpr',
      'Demo.res',
      './Examples/Dock:',
      'Advanced',
      'DockOption',
      'HowtoUse',
      './Examples/Dock/Advanced:',
      'AdvancePro.cfg',
      'AdvancePro.dof',
      'AdvancePro.dpr',
      'AdvancePro.res',
      'Main.dfm',
      'Main.pas',
      'Unit1.dfm',
      'Unit1.pas',
      'Unit2.dfm',
      'Unit2.pas',
      'Unit3.dfm',
      'Unit3.pas',
      'Unit4.dfm',
      'Unit4.pas',
      './Examples/Dock/DockOption:',
      'DockInfo.ini',
      'DockOptionDemo.dpr',
      'DockOptionDemo.res',
      'DockWindow.dfm',
      'DockWindow.pas',
      'MainForm.dfm',
      'MainForm.pas',
      './Examples/Dock/HowtoUse:',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      'Unit2.dfm',
      'Unit2.pas',
      './Examples/DragResizer:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/Edit:',
      'CnEditDemo.cfg',
      'CnEditDemo.dof',
      'CnEditDemo.dpr',
      'CnEditDemo.res',
      'uMain.dfm',
      'uMain.pas',
      './Examples/ErrorProvider:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/EventBus:',
      'EventBus.cfg',
      'EventBus.dof',
      'EventBus.dpr',
      'EventBus.res',
      'UnitEventBus.dfm',
      'UnitEventBus.pas',
      './Examples/EventHook:',
      'EventHookTest.cfg',
      'EventHookTest.dof',
      'EventHookTest.dpr',
      'EventHookUnit.dfm',
      'EventHookUnit.pas',
      './Examples/FilePacker:',
      'PackerDemo.cfg',
      'PackerDemo.dof',
      'PackerDemo.dpr',
      'PackerDemo.res',
      'UMain.dfm',
      'UMain.pas',
      'Uzip.pas',
      './Examples/FileSystemWatcher:',
      'FileSystemWatcherTest.cfg',
      'FileSystemWatcherTest.dof',
      'FileSystemWatcherTest.dpr',
      'FileSystemWatcherTest.res',
      'Test.dfm',
      'Test.pas',
      './Examples/FitCurve:',
      'CnFitCurveDemo.cfg',
      'CnFitCurveDemo.dof',
      'CnFitCurveDemo.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/FloatConvert_D7:',
      'Project1.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/Graphics:',
      'Source',
      'Test.bmp',
      'Test.jpg',
      './Examples/Graphics/Source:',
      'Demo.cfg',
      'Demo.dof',
      'Demo.dpr',
      'Demo.res',
      'MainFrm.dfm',
      'MainFrm.pas',
      'MainFrm_1033.dfm',
      './Examples/HardwareBreakpoint:',
      'HardwareBreakDemo.cfg',
      'HardwareBreakDemo.dof',
      'HardwareBreakDemo.dpr',
      'uHardwareBreakDemo.dfm',
      'uHardwareBreakDemo.pas',
      './Examples/HardwareInfo:',
      'CpuID.cfg',
      'CpuID.dof',
      'CpuID.dpr',
      'CpuID.res',
      'uCpuIDMain.dfm',
      'uCpuIDMain.pas',
      './Examples/HashTest_D7:',
      'HashTest.dpr',
      'HashTest.res',
      'TxtGen.dpr',
      'TxtGen.res',
      'fMainUnit.dfm',
      'fMainUnit.pas',
      'fTxtGenUnit.dfm',
      'fTxtGenUnit.pas',
      'test.out.txt',
      'test.txt',
      './Examples/HexEditor:',
      'HexEditorApp.cfg',
      'HexEditorApp.dof',
      'HexEditorApp.dpr',
      'HexEditorApp.res',
      'HexEditorUnit.dfm',
      'HexEditorUnit.pas',
      './Examples/Hint:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/IISCtrl:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/InProcessAPIHook:',
      'CnHookDemo.cfg',
      'CnHookDemo.dof',
      'CnHookDemo.dpr',
      'uCnHookDemo.dfm',
      'uCnHookDemo.pas',
      './Examples/Inet:',
      'CnInetDemo.cfg',
      'CnInetDemo.dof',
      'CnInetDemo.dpr',
      'CnInetDemo.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/IniCfg:',
      'CnIniCfgDemo.cfg',
      'CnIniCfgDemo.dof',
      'CnIniCfgDemo.dpr',
      'CnIniCfgDemo.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/IocpMemPool:',
      'MainFrm.dfm',
      'MainFrm.pas',
      'MemPoolTest.cfg',
      'MemPoolTest.dof',
      'MemPoolTest.dpr',
      'MemPoolTest.res',
      './Examples/IocpSocketAdapter_D7:',
      'FrmMain.dfm',
      'FrmMain.pas',
      'IOCPtest.cfg',
      'IOCPtest.dof',
      'IOCPtest.dpr',
      'IOCPtest.res',
      './Examples/KeyBlocker:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/LEDText:',
      'LEDSample.cfg',
      'LEDSample.dof',
      'LEDSample.dpr',
      'LEDSample.res',
      'UFrmMain.dfm',
      'UFrmMain.pas',
      './Examples/LinkedList:',
      'Demo.cfg',
      'Demo.dof',
      'Demo.dpr',
      'Demo.res',
      'Main.dfm',
      'Main.pas',
      './Examples/MDIBackground:',
      'ChildWin.dfm',
      'ChildWin.pas',
      'Main.dfm',
      'Main.pas',
      'MdiApp.cfg',
      'MdiApp.dof',
      'MdiApp.dpr',
      'MdiApp.res',
      './Examples/Memo:',
      'ProjectMemo.cfg',
      'ProjectMemo.dof',
      'ProjectMemo.dpr',
      'ProjectMemo.res',
      'UnitMemo.dfm',
      'UnitMemo.pas',
      './Examples/MemorySearch:',
      'CnSearchDemo.cfg',
      'CnSearchDemo.dof',
      'CnSearchDemo.dpr',
      'SearchMainFrm.dfm',
      'SearchMainFrm.pas',
      'Target.cfg',
      'Target.dof',
      'Target.dpr',
      'TargetMainFrm.dfm',
      'TargetMainFrm.pas',
      './Examples/Modem:',
      'ProjectModem.cfg',
      'ProjectModem.dof',
      'ProjectModem.dpr',
      'ProjectModem.res',
      'uFrmModem.dfm',
      'uFrmModem.pas',
      './Examples/MultiLang:',
      '1033',
      '2052',
      'CHS.txt',
      'ENU.txt',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      'Unit2.dfm',
      'Unit2.pas',
      'Unit3.dfm',
      'Unit3.pas',
      './Examples/MultiLang/1033:',
      'Project1.txt',
      './Examples/MultiLang/2052:',
      'Project1.txt',
      './Examples/MultiLangMerger:',
      'CnMultiLangMerger.dpr',
      'CnMultiLangMerger.dproj',
      'CnMultiLangMerger.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/Multicast_D7:',
      'FMainUnit.dfm',
      'FMainUnit.pas',
      'MCTest.cfg',
      'MCTest.dof',
      'MCTest.dpr',
      'MCTest.res',
      './Examples/NetDecl:',
      'NetDecl.cfg',
      'NetDecl.dof',
      'NetDecl.dpr',
      'NetDecl.res',
      'UnitNetDecl.dfm',
      'UnitNetDecl.pas',
      './Examples/ObjAuto:',
      'Project1.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/ObjectPool:',
      'ObjPool.cfg',
      'ObjPool.dof',
      'ObjPool.dpr',
      'ObjPool.res',
      'Unit1.dfm',
      'Unit1.pas',
      'Unit2.pas',
      './Examples/OpenGLPaintBox:',
      'CnGLPaintBox.dpr',
      'CnGLPaintBoxDemo.dfm',
      'CnGLPaintBoxDemo.pas',
      'test.bmp',
      './Examples/OuterControls:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/PingIP:',
      'CnPingDemo.cfg',
      'CnPingDemo.dof',
      'CnPingDemo.dpr',
      'CnPingDemo.res',
      'uCnPingDemo.dfm',
      'uCnPingDemo.pas',
      './Examples/PrimeNumber:',
      'Prime.cfg',
      'Prime.dof',
      'Prime.dpr',
      'UnitPrime.dfm',
      'UnitPrime.pas',
      './Examples/ProgressForm:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/RawInput:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/RedisClient_D7:',
      'RedisClient.cfg',
      'RedisClient.dof',
      'RedisClient.dpr',
      'RedisClient.res',
      'uMainFrm.dfm',
      'uMainFrm.pas',
      'uRedisTestMainFrm.dfm',
      'uRedisTestMainFrm.pas',
      './Examples/Ropes:',
      'TestRopeUnit.dfm',
      'TestRopeUnit.pas',
      'TestRopes.cfg',
      'TestRopes.dof',
      'TestRopes.dpr',
      './Examples/SkinMagic:',
      'CnSkinMagic_Sample.pas',
      'MainFrm.dfm',
      'MainFrm.pas',
      'SkinMagicDemo.cfg',
      'SkinMagicDemo.dof',
      'SkinMagicDemo.dpr',
      'SkinMagicDemo.res',
      './Examples/SkipList:',
      'TestSkipList.cfg',
      'TestSkipList.dof',
      'TestSkipList.dpr',
      'TestSkipListUnit.dfm',
      'TestSkipListUnit.pas',
      './Examples/SpinEdit:',
      'SpinEdit.cfg',
      'SpinEdit.dof',
      'SpinEdit.dpr',
      'SpinEdit.res',
      'SpinEditUnit.dfm',
      'SpinEditUnit.pas',
      './Examples/Stdcallback:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/SunRiseSet:',
      'SunRiseSet.cfg',
      'SunRiseSet.dof',
      'SunRiseSet.dpr',
      'SunRiseSet.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/SysDebugControl:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/TaskBar:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/ThreadPool_D7:',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/TrayIcon:',
      'CnTrayIconDemo.cfg',
      'CnTrayIconDemo.dof',
      'CnTrayIconDemo.dpr',
      'CnTrayIconDemo.res',
      'CnTrayIconMainUnit.dfm',
      'CnTrayIconMainUnit.pas',
      './Examples/Tree:',
      'CnTreeTest.cfg',
      'CnTreeTest.dof',
      'CnTreeTest.dpr',
      'CnTreeTestUnit.dfm',
      'CnTreeTestUnit.pas',
      './Examples/Twain:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/UDP:',
      'UDPDemo.cfg',
      'UDPDemo.dof',
      'UDPDemo.dpr',
      'UDPDemo.res',
      'UDPDemoFrm.dfm',
      'UDPDemoFrm.pas',
      './Examples/ValidateImage:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/VarList:',
      'Demo.cfg',
      'Demo.dof',
      'Demo.dpr',
      'Demo.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/VolumeCtrl:',
      'Unit1.dfm',
      'Unit1.pas',
      'VolumeCtrl.cfg',
      'VolumeCtrl.dof',
      'VolumeCtrl.dpr',
      'VolumeCtrl.res',
      './Examples/WaterImage:',
      'Unit1.dfm',
      'Unit1.pas',
      'WaterImage.cfg',
      'WaterImage.dof',
      'WaterImage.dpr',
      'WaterImage.res',
      './Examples/WinampCtrl:',
      'Unit1.dfm',
      'Unit1.pas',
      'WinampCtrl.cfg',
      'WinampCtrl.dof',
      'WinampCtrl.dpr',
      'WinampCtrl.res',
      './Examples/XMLPersistent:',
      'Test.bmp',
      'Test.xml',
      'TestCase.cfg',
      'TestCase.dof',
      'TestCase.dpr',
      'TestCase.res',
      'w_PersistentClassSample.pas',
      'w_PivotUnit.pas',
      'w_StreamUnitTest.pas',
      'w_XMLPersistentTestCase.pas',
      'w_frmTestcase.dfm',
      'w_frmTestcase.pas',
      './Examples/XlsWriter:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Project1.res',
      'Unit1.dfm',
      'Unit1.pas',
      './Examples/XorIniFile:',
      'Project1.cfg',
      'Project1.dof',
      'Project1.dpr',
      'Unit1.dfm',
      'Unit1.pas',
      './Packages:',
      'BCB2007',
      'BCB5',
      'BCB6',
      'Delphi101B',
      'Delphi10S',
      'Delphi2005',
      'Delphi2006',
      'Delphi2007',
      'Delphi2009',
      'Delphi2010',
      'Delphi5',
      'Delphi6',
      'Delphi7',
      'DelphiXE',
      'DelphiXE2',
      'DelphiXE3',
      'DelphiXE4',
      'DelphiXE5',
      'DelphiXE6',
      'DelphiXE7',
      'DelphiXE8',
      './Packages/BCB2007:',
      'CnPack_CB2007.cbproj',
      'CnPack_CB2007.cpp',
      'CnPack_CB2007.res',
      'dclCnPack_CB2007.cbproj',
      'dclCnPack_CB2007.cpp',
      'dclCnPack_CB2007.res',
      './Packages/BCB5:',
      'CnPack_CB5.bpk',
      'CnPack_CB5.cpp',
      'CnPack_CB5.res',
      'dclCnPack_CB5.bpk',
      'dclCnPack_CB5.cpp',
      'dclCnPack_CB5.res',
      './Packages/BCB6:',
      'CnPack_CB6.bpk',
      'CnPack_CB6.cpp',
      'CnPack_CB6.res',
      'dclCnPack_CB6.bpk',
      'dclCnPack_CB6.cpp',
      'dclCnPack_CB6.res',
      './Packages/Delphi101B:',
      'CnPack_D101B.dpk',
      'CnPack_D101B.dproj',
      'CnPack_D101B.res',
      'dclCnPack_D101B.dpk',
      'dclCnPack_D101B.dproj',
      'dclCnPack_D101B.res',
      './Packages/Delphi10S:',
      'CnPack_D10S.dpk',
      'CnPack_D10S.dproj',
      'CnPack_D10S.res',
      'dclCnPack_D10S.dpk',
      'dclCnPack_D10S.dproj',
      'dclCnPack_D10S.res',
      './Packages/Delphi2005:',
      'CnPack_D2005.bdsproj',
      'CnPack_D2005.cfg',
      'CnPack_D2005.dpk',
      'CnPack_D2005.res',
      'dclCnPack_D2005.bdsproj',
      'dclCnPack_D2005.cfg',
      'dclCnPack_D2005.dpk',
      'dclCnPack_D2005.res',
      './Packages/Delphi2006:',
      'CnPack_D2006.bdsproj',
      'CnPack_D2006.cfg',
      'CnPack_D2006.dpk',
      'CnPack_D2006.res',
      'dclCnPack_D2006.bdsproj',
      'dclCnPack_D2006.cfg',
      'dclCnPack_D2006.dpk',
      'dclCnPack_D2006.res',
      './Packages/Delphi2007:',
      'CnPack_D2007.bdsproj',
      'CnPack_D2007.cfg',
      'CnPack_D2007.dpk',
      'CnPack_D2007.res',
      'dclCnPack_D2007.bdsproj',
      'dclCnPack_D2007.cfg',
      'dclCnPack_D2007.dpk',
      'dclCnPack_D2007.res',
      './Packages/Delphi2009:',
      'CnPack_D2009.dpk',
      'CnPack_D2009.dproj',
      'CnPack_D2009.res',
      'dclCnPack_D2009.dpk',
      'dclCnPack_D2009.dproj',
      'dclCnPack_D2009.res',
      './Packages/Delphi2010:',
      'CnPack_D2010.dpk',
      'CnPack_D2010.dproj',
      'CnPack_D2010.res',
      'dclCnPack_D2010.dpk',
      'dclCnPack_D2010.dproj',
      'dclCnPack_D2010.res',
      './Packages/Delphi5:',
      'CnPack_D5.cfg',
      'CnPack_D5.dof',
      'CnPack_D5.dpk',
      'CnPack_D5.res',
      'dclCnPack_D5.cfg',
      'dclCnPack_D5.dof',
      'dclCnPack_D5.dpk',
      'dclCnPack_D5.res',
      './Packages/Delphi6:',
      'CnPack_D6.cfg',
      'CnPack_D6.dof',
      'CnPack_D6.dpk',
      'CnPack_D6.res',
      'dclCnPack_D6.cfg',
      'dclCnPack_D6.dof',
      'dclCnPack_D6.dpk',
      'dclCnPack_D6.res',
      './Packages/Delphi7:',
      'CnPack_D7.cfg',
      'CnPack_D7.dof',
      'CnPack_D7.dpk',
      'CnPack_D7.res',
      'dclCnPack_D7.cfg',
      'dclCnPack_D7.dof',
      'dclCnPack_D7.dpk',
      'dclCnPack_D7.res',
      './Packages/DelphiXE:',
      'CnPack_DXE.dpk',
      'CnPack_DXE.dproj',
      'CnPack_DXE.res',
      'dclCnPack_DXE.dpk',
      'dclCnPack_DXE.dproj',
      'dclCnPack_DXE.res',
      './Packages/DelphiXE2:',
      'CnPack_DXE2.dpk',
      'CnPack_DXE2.dproj',
      'CnPack_DXE2.res',
      'dclCnPack_DXE2.dpk',
      'dclCnPack_DXE2.dproj',
      'dclCnPack_DXE2.res',
      './Packages/DelphiXE3:',
      'CnPack_DXE3.dpk',
      'CnPack_DXE3.dproj',
      'CnPack_DXE3.res',
      'dclCnPack_DXE3.dpk',
      'dclCnPack_DXE3.dproj',
      'dclCnPack_DXE3.res',
      './Packages/DelphiXE4:',
      'CnPack_DXE4.dpk',
      'CnPack_DXE4.dproj',
      'CnPack_DXE4.res',
      'dclCnPack_DXE4.dpk',
      'dclCnPack_DXE4.dproj',
      'dclCnPack_DXE4.res',
      './Packages/DelphiXE5:',
      'CnPack_DXE5.dpk',
      'CnPack_DXE5.dproj',
      'CnPack_DXE5.res',
      'dclCnPack_DXE5.dpk',
      'dclCnPack_DXE5.dproj',
      'dclCnPack_DXE5.res',
      './Packages/DelphiXE6:',
      'CnPack_DXE6.dpk',
      'CnPack_DXE6.dproj',
      'CnPack_DXE6.res',
      'dclCnPack_DXE6.dpk',
      'dclCnPack_DXE6.dproj',
      'dclCnPack_DXE6.res',
      './Packages/DelphiXE7:',
      'CnPack_DXE7.dpk',
      'CnPack_DXE7.dproj',
      'CnPack_DXE7.res',
      'dclCnPack_DXE7.dpk',
      'dclCnPack_DXE7.dproj',
      'dclCnPack_DXE7.res',
      './Packages/DelphiXE8:',
      'CnPack_DXE8.dpk',
      'CnPack_DXE8.dproj',
      'CnPack_DXE8.res',
      'dclCnPack_DXE8.dpk',
      'dclCnPack_DXE8.dproj',
      'dclCnPack_DXE8.res',
      './Source:',
      'Common',
      'DbReport',
      'Graphics',
      'Lang',
      'MultiLang',
      'NetComm',
      'NonVisual',
      'ObjRep',
      'Readme.txt',
      'Skin',
      'ToCHS.bat',
      'ToCHT.bat',
      'ToENU.bat',
      './Source/Common:',
      'CnAES.pas',
      'CnAntiCheater.pas',
      'CnBase64.pas',
      'CnBigNumber.pas',
      'CnBinaryDiffPatch.pas',
      'CnBloomFilter.pas',
      'CnCRC32.pas',
      'CnCalClass.pas',
      'CnCalendar.pas',
      'CnCallBack.pas',
      'CnClasses.pas',
      'CnCommon.pas',
      'CnCompAboutFrm.dfm',
      'CnCompAboutFrm.pas',
      'CnCompUtils.pas',
      'CnConsts.pas',
      'CnDES.pas',
      'CnDancingLinks.pas',
      'CnDebug.pas',
      'CnDynObjBuilder.pas',
      'CnEventBus.pas',
      'CnEventHook.pas',
      'CnFitCurve.pas',
      'CnFloatConvert.pas',
      'CnFmxUtils.pas',
      'CnGraphUtils.pas',
      'CnHardWareInfo.pas',
      'CnHashMap.pas',
      'CnHashTable.pas',
      'CnIni.pas',
      'CnIniCfg.pas',
      'CnIniStrUtils.pas',
      'CnLinkedList.pas',
      'CnMD5.pas',
      'CnMemProf.pas',
      'CnMethodHook.pas',
      'CnMulticastEvent.pas',
      'CnNativeDecl.pas',
      'CnOTAUtils.pas',
      'CnObjAuto.pas',
      'CnPack.dcr',
      'CnPack.inc',
      'CnPack.pas',
      'CnPackRegister.pas',
      'CnPrimeNumber.pas',
      'CnPropEditors.pas',
      'CnPropSheetFrm.dfm',
      'CnPropSheetFrm.pas',
      'CnQueue.pas',
      'CnRopes.pas',
      'CnSHA1.pas',
      'CnSHA2.pas',
      'CnSM3.pas',
      'CnSM4.pas',
      'CnSQLite.pas',
      'CnShellUtils.pas',
      'CnSingleton.pas',
      'CnSingletonComp.pas',
      'CnSkipList.pas',
      'CnStrDiff.pas',
      'CnStream.pas',
      'CnStrings.pas',
      'CnThreadTaskMgr.pas',
      'CnTree.pas',
      'CnVCLBase.pas',
      'CnVarList.pas',
      'CnWideStrings.pas',
      'CnWinSvc.pas',
      'CnXMLPersistent.pas',
      './Source/DbReport:',
      'CnADOBinding.pas',
      'CnADOUpdateSQL.pas',
      'CnADOUpdateSQLEditor.pas',
      'CnADOUpdateSQLFrm.dfm',
      'CnADOUpdateSQLFrm.pas',
      'CnDBConsts.pas',
      'CnDBRegister.pas',
      'CnDHibernateAbout.dfm',
      'CnDHibernateAbout.pas',
      'CnDHibernateAppUtils.pas',
      'CnDHibernateArrayList.pas',
      'CnDHibernateBackupRestore.pas',
      'CnDHibernateBase.pas',
      'CnDHibernateBatchSQL.pas',
      'CnDHibernateCalc.pas',
      'CnDHibernateClasses.pas',
      'CnDHibernateConsts.pas',
      'CnDHibernateDateUtils.pas',
      'CnDHibernateExport.pas',
      'CnDHibernateImport.pas',
      'CnDHibernateMemData.pas',
      'CnDHibernateNav.res',
      'CnDHibernateNavigator.pas',
      'CnDHibernatePodoList.pas',
      'CnDHibernateQueryAdv.pas',
      'CnDHibernateSQLThread.pas',
      'CnDHibernateSet.pas',
      'CnDHibernateStringUtils.pas',
      'CnDHibernateSubQuery.pas',
      'CnDHibernateSubQueryAdv.pas',
      'CnDHibernateThread.pas',
      'CnDHibernateUtils.pas',
      'CnDataGrid.pas',
      'CnExcelUnit.pas',
      'CnPagedGrid.pas',
      'CnRunSqlFrame.dfm',
      'CnRunSqlFrame.pas',
      'CnRunSqlUnit.pas',
      'CnSQLAnalyzer.pas',
      'CnXlsWriter.pas',
      './Source/Graphics:',
      'CnAACtrls.pas',
      'CnAAFont.pas',
      'CnAAFontDialog.dfm',
      'CnAAFontDialog.pas',
      'CnAAFontEditor.pas',
      'CnAOTreeView.pas',
      'CnAOTreeView.res',
      'CnAppStoreBox.res',
      'CnAutoOption.pas',
      'CnButtonEdit.pas',
      'CnButtonEdit.res',
      'CnButtons.pas',
      'CnButtons.res',
      'CnCheckTreeView.pas',
      'CnCheckTreeView.res',
      'CnColorGrid.pas',
      'CnEdit.pas',
      'CnErrorProvider.pas',
      'CnErrorProvider.res',
      'CnGauge.pas',
      'CnGraphConsts.pas',
      'CnGraphPropEditors.pas',
      'CnGraphRegister.pas',
      'CnGraphics.pas',
      'CnHexEditor.pas',
      'CnHint.pas',
      'CnIconUtils.pas',
      'CnImage.pas',
      'CnLED.pas',
      'CnListBox.pas',
      'CnMemo.pas',
      'CnMonthCalendar.pas',
      'CnOpenGLPaintBox.pas',
      'CnPanel.pas',
      'CnQQPanel.res',
      'CnShellCtrls.pas',
      'CnSkinMagic.pas',
      'CnSpin.pas',
      'CnSpin.res',
      'CnSplitter.pas',
      'CnTabSet.pas',
      'CnValidateImage.pas',
      'CnWaterEffect.pas',
      'CnWaterImage.pas',
      'CnWizardImage.pas',
      './Source/Lang:',
      '1028',
      '1033',
      '2052',
      './Source/Lang/1028:',
      'CnAAFontDialog.dfm',
      'CnCompAboutFrm.dfm',
      'CnCompConsts.pas',
      'CnConsts.pas',
      'CnDBConsts.pas',
      'CnDockGlobal.pas',
      'CnFoxmailMsgFrm.dfm',
      'CnGraphConsts.pas',
      'CnLangConsts.pas',
      'CnNetConsts.pas',
      'CnProgressFrm.dfm',
      'CnRS232Dialog.dfm',
      './Source/Lang/1033:',
      'CnAAFontDialog.dfm',
      'CnCompAboutFrm.dfm',
      'CnCompConsts.pas',
      'CnConsts.pas',
      'CnDBConsts.pas',
      'CnDockGlobal.pas',
      'CnFoxmailMsgFrm.dfm',
      'CnGraphConsts.pas',
      'CnLangConsts.pas',
      'CnNetConsts.pas',
      'CnProgressFrm.dfm',
      'CnRS232Dialog.dfm',
      'Gen.bat',
      './Source/Lang/2052:',
      'CnAAFontDialog.dfm',
      'CnCompAboutFrm.dfm',
      'CnCompConsts.pas',
      'CnConsts.pas',
      'CnDBConsts.pas',
      'CnDockGlobal.pas',
      'CnFoxmailMsgFrm.dfm',
      'CnGraphConsts.pas',
      'CnLangConsts.pas',
      'CnNetConsts.pas',
      'CnProgressFrm.dfm',
      'CnRS232Dialog.dfm',
      './Source/MultiLang:',
      'CnHashIniFile.pas',
      'CnHashLangStorage.pas',
      'CnIniLangFileStorage.pas',
      'CnLangCollection.pas',
      'CnLangConsts.pas',
      'CnLangEditors.pas',
      'CnLangMgr.pas',
      'CnLangReg.pas',
      'CnLangStorage.pas',
      'CnLangTranslator.pas',
      'CnLangUtils.pas',
      'CnTransEditor.dfm',
      'CnTransEditor.pas',
      'CnTransFilter.dfm',
      'CnTransFilter.pas',
      'QLangIDs.inc',
      './Source/NetComm:',
      'CnCameraEye.pas',
      'CnDialUp.pas',
      'CnIISCtrl.pas',
      'CnIP.pas',
      'CnInetUtils.pas',
      'CnIocpSimpleMemPool.pas',
      'CnIocpSocketAdapter.pas',
      'CnModem.pas',
      'CnNetConsts.pas',
      'CnNetDecls.pas',
      'CnNetPropEditor.pas',
      'CnNetRegister.pas',
      'CnPing.pas',
      'CnRS232.pas',
      'CnRS232Dialog.dfm',
      'CnRS232Dialog.pas',
      'CnRedisClient.pas',
      'CnTwain.pas',
      'CnUDP.pas',
      './Source/NonVisual:',
      'CnADOConPool.pas',
      'CnASCommon.pas',
      'CnASHostServices.pas',
      'CnASIDispatchProxy.pas',
      'CnASInvoker.pas',
      'CnASPropEditors.pas',
      'CnActionListHook.pas',
      'CnActiveScript.pas',
      'CnCompConsts.pas',
      'CnCompRegister.pas',
      'CnConjoinDockHost.dfm',
      'CnConsole.pas',
      'CnControlHook.pas',
      'CnDelphiDockStyle.pas',
      'CnDockFormControl.pas',
      'CnDockGlobal.pas',
      'CnDockHashTable.pas',
      'CnDockInfo.pas',
      'CnDockPropertyReg.pas',
      'CnDockSupportClass.pas',
      'CnDockSupportControl.pas',
      'CnDockSupportProc.pas',
      'CnDockTree.pas',
      'CnDockableForm.dfm',
      'CnDragResizer.pas',
      'CnFilePacker.pas',
      'CnFileSystemWatcher.pas',
      'CnFormScaler.pas',
      'CnGlobalKeyHook.pas',
      'CnHardwareBreakpoint.pas',
      'CnInProcessAPIHook.pas',
      'CnKeyBlocker.pas',
      'CnMDIBackGround.pas',
      'CnMemorySearch.pas',
      'CnMenuHook.pas',
      'CnObjectPool.pas',
      'CnOuterControls.pas',
      'CnRawInput.pas',
      'CnRestoreSystemMenu.pas',
      'CnSystemDebugControl.pas',
      'CnTabDockHost.dfm',
      'CnTaskBar.pas',
      'CnThreadPool.pas',
      'CnTimer.pas',
      'CnTrayIcon.pas',
      'CnVCDockStyle.pas',
      'CnVIDDockStyle.pas',
      'CnVSNETDockStyle.pas',
      'CnVolumeCtrl.pas',
      'CnWinampCtrl.pas',
      './Source/ObjRep:',
      'CnFoxmailMsgFrm.dfm',
      'CnFoxmailMsgFrm.pas',
      'CnProgressFrm.dfm',
      'CnProgressFrm.pas',
      './Source/Skin:',
      'CnSkinForm.pas',
      'CnSkinMenu.pas',
      'CnSkinStdCtrls.pas',
      'CnSkinStyle.pas',
      'CnSkinStyleXP.rc',
      'CnSkinTheme.pas',
      'CnSkinXPBlueStyle.pas',
      'CnSkinXPBlueStyle.res',
      'CnSkinXPGreenStyle.pas',
      'CnSkinXPGreenStyle.res',
      'CnSkinXPSilverStyle.pas',
      'CnSkinXPSilverStyle.res',
      'Readme.txt'
    );

procedure TFormFuzzy.CopyList(Src, Dest: TList);
var
  I: Integer;
begin
  if (Src = nil) or (Dest = nil) then
    Exit;

  Dest.Clear;
  for I := 0 to Src.Count - 1 do
    Dest.Add(Src[I]);
end;

procedure TFormFuzzy.edtSearchChange(Sender: TObject);
var
  Pattern: string;
  MatchedIndexes: TList;
  I, Score: Integer;
  FirstMatch: Boolean;
begin
  mmoResult.Clear;
  Pattern := Trim(edtSearch.Text);
  if Pattern <> '' then
  begin
    FirstMatch := True;
    MatchedIndexes := TList.Create;
    if chkScore.Checked then
    begin
      for I := Low(SAR_STRS) to High(SAR_STRS) do
      begin
        Score := 0;
        if FuzzyMatchStrWithScore(Pattern, SAR_STRS[I], Score, MatchedIndexes, chkCase.Checked) then
        begin
          mmoResult.Lines.Add(Format('%d - %s - %s', [Score, SAR_STRS[I], ListToString(MatchedIndexes)]));

          if FirstMatch then
          begin
            FPaintStr := SAR_STRS[I];
            CopyList(MatchedIndexes, FMatchedIndexes);
            FirstMatch := False;
          end;
        end;
      end;
    end
    else
    begin
      for I := Low(SAR_STRS) to High(SAR_STRS) do
      begin
        if FuzzyMatchStr(Pattern, SAR_STRS[I], MatchedIndexes, chkCase.Checked) then
        begin
          mmoResult.Lines.Add(Format('%s -%s', [SAR_STRS[I], ListToString(MatchedIndexes)]));

          if FirstMatch then
          begin
            FPaintStr := SAR_STRS[I];
            CopyList(MatchedIndexes, FMatchedIndexes);
            FirstMatch := False;
          end;
        end;
      end;
    end;
    MatchedIndexes.Free;
  end;
  pbString.Invalidate;
end;

procedure TFormFuzzy.FormCreate(Sender: TObject);
var
  Score: Integer;
  List: TList;
begin
  FMatchedIndexes := TList.Create;
  FuzzyMatchStr('HM', 'CnHint.pas');
  List := TList.Create;
  FuzzyMatchStrWithScore('Hit', 'Hiatet', Score, List);
  mmoResult.Lines.Add(ListToString(List));
  List.Free;
end;

function TFormFuzzy.ListToString(List: TList): string;
var
  I: Integer;
begin
  Result := '';
  if (List <> nil) and (List.Count > 0) then
    for I := 0 to List.Count - 1 do
      Result := Result + ' ' + IntToStr(Integer(List[I]));
end;

procedure TFormFuzzy.pbStringPaint(Sender: TObject);
var
  R: TRect;
  I, L, W: Integer;
  C: Char;
  Size: TSize;
begin
  // Draw match strings
  pbString.Canvas.Brush.Style := bsSolid;
  pbString.Canvas.Brush.Color := clWhite;
  R := Rect(0, 0, pbString.Width, pbString.Height);
  pbString.Canvas.FillRect(R);

  if FPaintStr <> '' then
  begin
    pbString.Canvas.TextOut(2, 2, FPaintStr);
    SetLength(FPaintText, Length(FPaintStr));
    StrCopy(PChar(FPaintText), PChar(FPaintStr));
    // CnDebugger.LogRawString(FPaintText);
    pbString.Canvas.Font.Color := clRed;
    // CnDebugger.LogMsg('Draw Chars Count: ' +  IntToStr(FMatchedIndexes.Count));
    for I := FMatchedIndexes.Count - 1 downto 0 do
    begin
      L := Integer(FMatchedIndexes[I]);
      if (L <= 0) or (L > Length(FPaintText)) then
        Continue;
      // CnDebugger.LogFmt('Draw #%d Index %d.', [I, L]);

      if L < Length(FPaintText) then
        FPaintText[L + 1] := #0;
      C := FPaintText[L];
      FPaintText[L] := #0;

      Size.cx := 0;
      Size.cy := 0;
      if L = 1 then
        W := 0
      else
      begin
        Windows.GetTextExtentPoint32(pbString.Canvas.Handle, PChar(@(FPaintText[1])), L - 1, Size);
        W := Size.cx; // 计算需绘制字符前的宽度
      end;
      FPaintText[L] := C;
      Windows.TextOut(pbString.Canvas.Handle, 2 + W, 2, PChar(@(FPaintText[L])), 1);
    end;
  end;
end;

procedure TFormFuzzy.FormDestroy(Sender: TObject);
begin
  FMatchedIndexes.Free;
end;

end.
