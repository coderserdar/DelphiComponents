unit GTComponentPack_Reg;

interface
uses
   Classes
  ,o_BenchMark
  ,o_ClassFinder
  ,o_FileInfo
  ,o_FormEvents
  ,o_GTExplorerSysObjects
  ,o_GTExtendedShellCtrls
  ,o_GTListBox
  ,o_GTMessageBox
  ,o_GTTimer
  ,o_GTWindowsCPL
  ,o_LinkLabel
  ,o_ProcessListView
  ,o_ProcessManager
  ,o_RegionalSettings
  ,o_StringStore
  ,o_FileInfoListView
  ,o_PackageManager
  ,o_GTStatusBar
  ,o_GTDateControls
  ,o_GTIPEdit
  ,o_GTGroupBox
  ,o_GTButtons
  ,o_GTCombos
  ,o_GTFileDownload
  ,o_GTControlArays
  ,o_GTInfoTrayDialog
  ,o_GTAutoAppStarter
  ,o_GTSplashScreen
  ,o_GTYouTubeVideoPlayer
  ,o_GTFormFader
  ;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('GTComponent MegaPack',
                                          [TgtBenchMark
                                          ,TgtRegisteredClasses
                                          ,TgtFileInfo
                                          ,TgtFormEvents
                                          ,TgtExplorerSysObjs
                                          ,TgtShellTreeView
                                          ,TgtShellComboBox
                                          ,TgtShellListView
                                          ,TgtListBox
                                          ,TgtMessageBox
                                          ,TgtTimer
                                          ,TgtWindowsCPL
                                          ,TgtLinkLabel
                                          ,TgtProcessListView
                                          ,TgtProcessManager
                                          ,TgtRegionalSettings
                                          ,TgtStringStore
                                          ,TgtFileInfoListView
                                          ,TgtPackageManager
                                          ,TgtDateCtrlManager
                                          ,TgtDayCombo
                                          ,TgtMonthCombo
                                          ,TgtYearCombo
                                          ,TgtGroupBox
                                          ,TgtIPEdit
                                          ,TgtStatusBar
                                          ,TgtDialogButton
                                          ,TgtControlPanelDialog
                                          ,TgtPrinterCombo
                                          ,TgtFontCombo
                                          ,TgtSQLServerCombo
                                          ,TgtNetResourceCombo
                                          ,TgtFileDownload
                                          ,TgtInfoTrayDialog
                                          ,TgtAutoAppStarter
                                          ,TgtSplashScreen
                                          ,TgtYouTubeVideoPlayer
                                          ,TgtFormFader
                                          ]);
end;

end.
