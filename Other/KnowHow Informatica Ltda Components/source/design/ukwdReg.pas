{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukwdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	Classes, Forms, DsgnIntf, ExptIntf, uksyUtils, uksydClasses, ukwUtils,
	ukwClasses, ukwCtrls, ukwdConsts, ukwdMComments, ukwdClasses;

procedure Register;
begin

{ Classes }
{$IFNDEF EXCLUDED_CLASSES}
	RegisterClasses( [TKTabSheet] );
{$ENDIF}

{ Components }
	RegisterComponents( sRegWinAPI, [TKMonitor, TKTrayIcon, {$IFNDEF EXCLUDED_CLASSES}
		TKPageControl, {$ENDIF} // not yet completed
		TKTreeView, TKFileMap, TKMailSlot, TKAppControl, TKPageSetUpDialog,
		TKNetworkDialog, TKBrowseFolderDialog, TKWorkerThreadComp, TKThreadedTimer,
		TKProducerConsumer, TKWorkerThreadPool, TKThreadedTimerPool, TKShellComboBox,
		TKShellListBox] );

{ Windows 95 Only }
	{ if ( not CheckWinNT ) then - function only when in design time }
	begin
		RegisterComponents( sRegWinAPI, [TKPerformanceObjects, TKFileManager,
			TKFileInfo] );
		RegisterPropertyEditor( TypeInfo( string ), TKW95PerformanceItem, 'ObjectName',
			TKObjectNameProperty );
		RegisterPropertyEditor( TypeInfo( string ), TKW95PerformanceItem, 'CounterName',
			TKCounterNameProperty );
		RegisterPropertyEditor( TypeInfo( string ), TKW95PerformanceItem, 'CounterDescription',
			TKW95PerformanceStrReadOnly );
		RegisterPropertyEditor( TypeInfo( string ), TKW95PerformanceItem, 'CounterInfo',
			TKW95PerformanceStrReadOnly );
		RegisterPropertyEditor( TypeInfo( Boolean ), TKW95PerformanceItem, 'CounterDifferentiate',
			TKW95PerformanceBoolReadOnly );

{----------------------- TKW95PerformanceItem Comments -------------------------}

		RegisterDefaultMethod( TypeInfo( TKStatEvent ), TKW95PerformanceItem, 'OnStatEvent',
			sComW95PIStat );

	end;
	{ else }
	begin
		RegisterComponents( sRegWinAPI, [TKWin32ServiceManager{*}] );
		RegisterPropertyEditor( TypeInfo( string ), TKCustomWin32ServiceManager, 'MachineName',
			TKMachineNameProperty );
	end;

{ Component Editors }

{$IFNDEF EXCLUDED_CLASSES}
{$IFDEF DELPHI3}
	RegisterComponentEditor( TKTabSheet, TKPageControlEditor );
	RegisterComponentEditor( TKPageControl, TKPageControlEditor );
{$ENDIF}
{$ENDIF}

	RegisterComponentEditor( TKPageSetUpDialog, TKPageSetUpDialogEditor );
	RegisterComponentEditor( TKBrowseFolderDialog, TKBrowseFolderDialogEditor );

	RegisterComponentEditor( TKShellComboBox, TKShellComboBoxEditor );
	RegisterComponentEditor( TKShellListBox, TKShellListBoxEditor );

{ Property Editors }

{$IFNDEF EXCLUDED_CLASSES}
{$IFDEF DELPHI3}
	RegisterPropertyEditor( TypeInfo( TKTabSheet ), TKPageControl, 'ActivePage',
		TKActivePageProperty );
{$ENDIF}
{$ENDIF}

	RegisterPropertyEditor( TypeInfo( string ), TKFileInfo, 'FileName', TKFileNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKMonitor, 'Directory', TKDirectoryProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKMailSlot, 'Domain', TKDomainProperty );

{ Method Comments }

{-------------------------- TKThreadedTimer Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKThreadedTimer, 'OnTimer',
		sComThTimer );

{------------------------ TKThreadedTimerItem Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKThreadedTimerItem, 'OnTimer',
		sComThTimer );

{------------------------- TKWorkerThreadComp Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKGetThreadEvent ), TKWorkerThreadComp, 'OnGetThread',
		sComWorkThrGetThrd );
	RegisterDefaultMethod( TypeInfo( TKWorkerThreadExecuteEvent ), TKWorkerThreadComp, 
		'OnThreadExecute', sComWorkThrThrdEx );
	RegisterDefaultMethod( TypeInfo( TKWorkerThreadTerminateEvent ), TKWorkerThreadComp, 
		'OnThreadTerminate', sComWorkThrThrdTerm );

{----------------------- TKWorkerThreadedItem Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKGetThreadEvent ), TKWorkerThreadItem, 'OnGetThread',
		sComWorkThrGetThrd );
	RegisterDefaultMethod( TypeInfo( TKWorkerThreadExecuteEvent ), TKWorkerThreadItem, 
  		'OnThreadExecute', sComWorkThrThrdEx );
	RegisterDefaultMethod( TypeInfo( TKWorkerThreadTerminateEvent ), TKWorkerThreadItem, 
 		'OnThreadTerminate', sComWorkThrThrdTerm );

{------------------------ TKProducerConsumer Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TKProduceConsumeEvent ), TKProducerConsumer,
		'OnProduce', sComProdConsProd );
	RegisterDefaultMethod( TypeInfo( TKProduceConsumeEvent ), TKProducerConsumer,
		'OnConsume', sComProdConsCons );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKProducerConsumer,
		'OnStart', sComProdConsStart );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKProducerConsumer,
		'OnStop', sComProdConsStop );

{-------------------------- TKCustomMailSlot Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMailSlot, 'OnActivate',
		sComCMSActive );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMailSlot, 'OnDeactivate',
		sComCMSDeactive );

{----------------------------- TKMailSlot Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKDataArriveEvent ), TKMailSlot, 'OnDataArrive',
		sComMSDataArrive );
	RegisterDefaultMethod( TypeInfo( TKFileArriveEvent ), TKMailSlot, 'OnFileArrive',
		sComMSFileArrive );
	RegisterDefaultMethod( TypeInfo( TKMailACKEvent ), TKMailSlot, 'OnReceiveACK',
		sComMSRcvACK );
	RegisterDefaultMethod( TypeInfo( TKMailNDatagramEvent ), TKMailSlot, 'OnReceiveControl',
		sComMSRcvCtrl );
	RegisterDefaultMethod( TypeInfo( TKMailNDatagramEvent ), TKMailSlot, 'OnSynchronize',
		sComMSSync );
	RegisterDefaultMethod( TypeInfo( TKTextArriveEvent ), TKMailSlot, 'OnTextArrive',
		sComMSTextArrive );
	RegisterDefaultMethod( TypeInfo( TKMailTimeOutEvent ), TKMailSlot, 'OnTimeOut',
		sComMSTimeOut );
	RegisterDefaultMethod( TypeInfo( TKTransmissionEndEvent ), TKMailSlot, 'OnTransmissionEnd',
		sComMSTxEnd );

{---------------------------- TKAppControl Comments ----------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnActivate',
		sComAppCtrlActivate );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnDeactivate',
		sComAppCtrlDeactivate );
	RegisterDefaultMethod( TypeInfo( TKInitEnvEvent ), TKAppControl, 'OnInitEnvironment',
		sComAppCtrlInitEnv );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnReadRegistry',
		sComAppCtrlReadReg );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnWriteRegistry',
		sComAppCtrlWriteReg );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnHint',
		sComAppCtrlAppEvents );
	RegisterDefaultMethod( TypeInfo( TIdleEvent ), TKAppControl, 'OnIdle',
		sComAppCtrlAppEvents );
	RegisterDefaultMethod( TypeInfo( TShowHintEvent ), TKAppControl, 'OnShowHint',
		sComAppCtrlAppEvents );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnMaximize',
		sComAppCtrlAppEvents );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKAppControl, 'OnMinimize',
		sComAppCtrlAppEvents );

{-------------------------- TKCustomFileMap Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomFileMap, 'AfterClose',
		sComCFMAfterClose );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomFileMap, 'AfterOpen',
		sComCFMAfterOpen );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomFileMap, 'BeforeClose',
		sComCFMBeforeClose );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomFileMap, 'BeforeOpen',
		sComCFMBeforeOpen );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomFileMap, 'OnChange',
		sComCFMChange );

{--------------------------- TKCustomMonitor Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMonitor, 'OnDirChanged',
		sComCMonDirChanged );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMonitor, 'OnStart',
		sComCMonStart );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomMonitor, 'OnStop',
		sComCMonStop );

{-------------------------- TKCustomTrayIcon Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomTrayIcon, 'OnClick',
		sComTrayClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomTrayIcon, 'OnDblClick',
		sComTrayDblClick );
	RegisterDefaultMethod( TypeInfo( TKTrayAnimateEvent ), TKCustomTrayIcon, 'OnAnimate',
		sComTrayAnimate );

{------------------------- TKCustomDragDrop Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKDragDropGetHandle ), TKCustomDragDrop, 'OnGetHandle',
		sComCDragDropGetHandle );
	RegisterDefaultMethod( TypeInfo( TKDragDropGetWndProc ), TKCustomDragDrop, 'OnGetWndProc',
		sComCDragDropGetWndProc );
	RegisterDefaultMethod( TypeInfo( TKDragDropEvent ), TKCustomDragDrop, 'OnDrop',
		sComCDragDropDrop );
	RegisterDefaultMethod( TypeInfo( TKDragDropPointEvent ), TKCustomDragDrop, 'OnDropPoint',
		sComCDragDropDropPoint );

{----------------------- TKBrowseFolderDialog Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKSHBrowseFolderEvent ), TKBrowseFolderDialog,
		'OnInitialize', sComBFDlgInit );
	RegisterDefaultMethod( TypeInfo( TKSHBrowseFolderEvent ), TKBrowseFolderDialog,
		'OnSelChanged', sComBFDlgSelChange );

{------------------------- TKPageSetupDialog Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKPageSetupDialog, 'OnPageSetup',
		sComPSDlgPgSetUp );
	RegisterDefaultMethod( TypeInfo( TKInitPaintPageEvent ), TKPageSetupDialog,
		'OnInitialize', sComPSDlgInitPaint );
	RegisterDefaultMethod( TypeInfo( TKPaintPageEvent ), TKPageSetupDialog, 'OnPaintPage',
		sComPSDlgPaintPg );

{------------------------- TKNetworkDialog Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKNetworkDialog, 'OnExecute',
	  sComNetDlgExec );

{----------------------------- TKDirTree Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKAddFileEvent ), TKDirTree, 'OnAddFile',
	  sComDirTreeAddFile );

{----------------------------- TKTreeView Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKPaintItemEvent ), TKTreeView, 'OnPaintItem',
		sComTreeViewPaintItem );

{------------------- TKShellComboBox/TKShellListBox Comments -------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKShellComboBox, 'OnAddPath',
		sComShellBoxAddPath );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKShellListBox, 'OnAddPath',
		sComShellBoxAddPath );

{-------------------- TKCustomWin32ServiceManager Comments ---------------------}

	RegisterDefaultMethod( TypeInfo( TKRemoteWin32Error ), TKCustomWin32ServiceManager,
		'OnWin32Error', sComCWSMRemoteWin32Error );

end;

end.
