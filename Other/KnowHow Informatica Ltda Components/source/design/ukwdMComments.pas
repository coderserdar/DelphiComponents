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

unit ukwdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{------------------------ TKW95PerformanceItem Comments ------------------------}

{
	TKW95PerformanceItem.OnStatEvent: TKStatEvent = procedure ( Sender: TObject;
		Item: TKW95PerformanceItem; StatOption: TKStatOption; Differentiate: Boolean ) of object;
}
	sComW95PIStat =
	'» Parameters: Sender = TKPerformanceObjects; Item = actual performance item;'#13#10 +
	'    StatOption = Determines if the data value is for start, stop or get more'#13#10 +
	'    information; Differentiate = determines if the data value is raw or if it'#13#10 +
	'    is differentiated over time (use the formula Data2-Data1/Time2-Time1).'#13#10 +
	'» Invoked at regular time intervals while monitoring is taking place. Also'#13#10 +
	'    invoked at the beginning and end of the monitoring.'#13#10 +
	'» Use this event to retrieve performance information about the selected object.';

{-------------------------- TKThreadedTimer Comments ---------------------------}
{------------------------ TKThreadedTimerItem Comments -------------------------}

{
	TKThreadedTimer.OnTimer: TNotifyEvent
}
	sComThTimer = '';

{-------------------------- TKWorkerThread Comments ---------------------------}
{------------------------ TKWorkerThreadItem Comments -------------------------}

{
	TKWorkerThread.OnGetThread: TKGetThreadEvent = procedure( Sender: TKWorkerThread;
		var Thread: TKThread ) of object;
}
	sComWorkThrGetThrd = '';

{
	TKWorkerThread.OnThreadExecute: TKThreadEvent = procedure( Sender: TKWorkerThread;
		Thread: TKThread ) of object;
}
	sComWorkThrThrdEx = '';

{
	TKWorkerThread.OnThreadTerminate: TKThreadEvent = procedure( Sender: TKWorkerThread;
		Thread: TKThread ) of object;
}
	sComWorkThrThrdTerm = '';

{
	TKWorkerThreadItemExecuteMsgEvent = procedure( Source: TKWorkerThreadComp;
    Sender: TKWorkerThreadItem; Thread: TKWorkerThread; Param: Pointer; Msg: TMsg;
    var Processed, Continue: Boolean ) of object;
	TKWorkerThreadExecuteMsgEvent = procedure( Sender: TKWorkerThreadComp;
    Thread: TKWorkerThread; Param: Pointer; Msg: TMsg; var Processed,
    Continue: Boolean ) of object;
}  
{------------------------- TKProducerConsumer Comments -------------------------}

{
	TKProducerConsumer.OnProduce: TKProduceConsumeEvent = procedure( Sender: TKProducerConsumer;
		Thread: TKProducerConsumerThread; const Count, MaxCount: Cardinal;
		var DownCount: Cardinal ) of object;
}
	sComProdConsProd = '';

{
	TKProducerConsumer.OnConsume: TKProduceConsumeEvent = procedure( Sender: TKProducerConsumer;
		Thread: TKProducerConsumerThread; const Count, MaxCount: Cardinal;
		var DownCount: Cardinal ) of object;
}
	sComProdConsCons = '';

{
	TKProducerConsumer.OnStart: TNotifyEvent
}
	sComProdConsStart = '';

{
	TKProducerConsumer.OnStop: TNotifyEvent
}
	sComProdConsStop = '';

{-------------------------- TKCustomMailSlot Comments --------------------------}

{
	TKCustomMailSlot.OnActivate: TNotifyEvent
}
	sComCMSActive = '';

{
	TKCustomMailSlot.OnDeactivate: TNotifyEvent
}
	sComCMSDeactive = '';

{----------------------------- TKMailSlot Comments -----------------------------}

{
	TKMailSlot.OnDataArrive: TKDataArriveEvent = procedure( Sender: TKMailSlot;
		Success: Boolean; TxID: LongInt; Data: Pointer; Size: LongInt ) of object;
}
	sComMSDataArrive = '';

{
	TKMailSlot.OnFileArrive: TKFileArriveEvent = procedure( Sender: TKMailSlot;
		Success: Boolean; TxID: LongInt; var FileName: TFileName; Size: LongInt ) of object;
}
	sComMSFileArrive = '';

{
	TKMailSlot.OnReceiveACK: TKMailACKEvent = procedure( Sender: TKMailSlot;
		Signature: TDateTime; UserInfo: TKMSUserInfo; AType: TKMailType ) of object;

}
	sComMSRcvACK = '';

{
	TKMailSlot.OnReceiveControl: TKMailNDatagramEvent = procedure( Sender: TKMailSlot;
		Signature: TDateTime; UserInfo: TKMSUserInfo; Control_01, Control_02: LongInt )
		of object;
}
	sComMSRcvCtrl = '';

{
	TKMailSlot.OnSynchronize: TKMailNDatagramEvent = procedure( Sender: TKMailSlot;
		Signature: TDateTime; UserInfo: TKMSUserInfo; Control_01, Control_02: LongInt )
		of object;
}
	sComMSSync = '';

{
	TKMailSlot.OnTextArrive: TKTextArriveEvent = procedure( Sender: TKMailSlot;
		Success: Boolean; TxID: LongInt; const Text: string ) of object;
}
	sComMSTextArrive = '';

{
	TKMailSlot.OnTimeOut: TKMailTimeOutEvent = procedure( Sender: TKMailSlot;
		Signature: TDateTime; UserInfo: TKMSUserInfo; MailType: TKMailType ) of object;

}
	sComMSTimeOut = '';

{
	TKMailSlot.OnTransmissionEnd: TKTransmissionEndEvent = procedure( Sender: TKMailSlot;
		TxID: LongInt; UserInfo: TKMSUserInfo; Success: Boolean ) of object;
}
	sComMSTxEnd = '';

{---------------------------- TKAppControl Comments ----------------------------}

{
	TKAppControl.OnActivate: TNotifyEvent
}
	sComAppCtrlActivate = '';

{
	TKAppControl.OnDeactivate: TNotifyEvent
}
	sComAppCtrlDeactivate = '';

{
	TKAppControl.OnReadRegistry: TNotifyEvent
}
	sComAppCtrlReadReg = '';

{
	TKAppControl.OnWriteRegistry: TNotifyEvent
}
	sComAppCtrlWriteReg = '';

{
	TKAppControl.OnInitEnvironment: TKInitEnvEvent = procedure( Sender: TKAppControl;
		var CancelWait: Boolean ) of object;
}
	sComAppCtrlInitEnv = '';

{
	TKAppControl.OnHint: TNotifyEvent
	TKAppControl.OnIdle: TIdleEvent
	TKAppControl.OnShowHint: TShowHintEvent
	TKAppControl.OnMaximize: TNotifyEvent
	TKAppControl.OnMinimize: TNotifyEvent
}
	sComAppCtrlAppEvents = '';

{-------------------------- TKCustomFileMap Comments ---------------------------}

{
	TKCustomFileMap.AfterClose : TNotifyEvent
}
	sComCFMAfterClose = '';

{
	TKCustomFileMap.AfterOpen : TNotifyEvent
}
	sComCFMAfterOpen = '';

{
	TKCustomFileMap.BeforeClose : TNotifyEvent
}
	sComCFMBeforeClose = '';

{
	TKCustomFileMap.BeforeOpen : TNotifyEvent
}
	sComCFMBeforeOpen = '';

{
	TKCustomFileMap.OnChange: TNotifyEvent
}
	sComCFMChange = '';

{--------------------------- TKCustomMonitor Comments --------------------------}

{
	TKCustomMonitor.OnDirChanged: TNotifyEvent
}
	sComCMonDirChanged = '';

{
	TKCustomMonitor.OnStart: TNotifyEvent
}
	sComCMonStart = '';

{
	TKCustomMonitor.OnStop: TNotifyEvent
}
	sComCMonStop = '';

{-------------------------- TKCustomTrayIcon Comments --------------------------}

{
	TKCustomTrayIcon.OnClick: TNotifyEvent
}
	sComTrayClick = '';

{
	TKCustomTrayIcon.OnDblClick: TNotifyEvent
}
	sComTrayDblClick = '';

{
	TKCustomTrayIcon.OnAnimate: TKTrayAnimateEvent = procedure ( Sender: TKCustomTrayIcon;
		Icon: TIcon; Index, Count: Integer ) of object;
}
	sComTrayAnimate = '';

{------------------------- TKCustomDragDrop Comments ---------------------------}

{
	TKCustomDragDrop.OnGetHandle: TKDragDropGetHandle = procedure( Sender: TKCustomDragDrop;
		var Source: TObject; var Handle: Hwnd ) of object;
}
	sComCDragDropGetHandle = '';

{
	TKCustomDragDrop.OnGetWndProc: TKDragDropGetWndProc = procedure( Sender: TKCustomDragDrop;
		var Source: TObject; var Method: TWndMethod ) of object;
}
	sComCDragDropGetWndProc = '';

{
	TKCustomDragDrop.OnDrop: TKDragDropEvent = procedure( Sender: TKCustomDragDrop;
		Source: TObject; DropList: TStrings ) of object;
}
	sComCDragDropDrop = '';

{
	TKCustomDragDrop.OnDropPoint: TKDragDropPointEvent = procedure( Sender: TKCustomDragDrop;
		Source: TObject; DroppedIntoClientArea: Boolean; X, Y: Integer ) of object;
}
	sComCDragDropDropPoint = '';

{----------------------- TKBrowseFolderDialog Comments -------------------------}

{
	TKBrowseFolderDialog.OnInitialize: TKSHBrowseFolderEvent = procedure( Sender:
		TKBrowseFolderDialog; var SetFolder, SetStatus: string; var EnableOK: Boolean )
		of object;
}
	sComBFDlgInit = '';

{
	TKBrowseFolderDialog.OnSelChanged: TKSHBrowseFolderEvent = procedure( Sender:
		TKBrowseFolderDialog; var SetFolder, SetStatus: string; var EnableOK: Boolean )
		of object;
}
	sComBFDlgSelChange = '';

{------------------------- TKPageSetupDialog Comments --------------------------}

{
	TKPageSetupDialog.OnPageSetUp: TNotifyEvent
}
	sComPSDlgPgSetUp = '';

{
	TKPageSetupDialog.OnInitPaintPage: TKInitPaintPageEvent = procedure( Sender: TKPageSetupDialog;
		PPageSetUpInfo: PKPageSetUpDialogInfo; var Handled: Boolean ) of object;
}
	sComPSDlgInitPaint = '';

{
	TKPageSetupDialog.OnPaintPage: TKPaintPageEvent = procedure ( Sender: TKPageSetupDialog;
		PaintOption: TKPaintOption; Canvas: TCanvas; Rect: TRect; var Handled: Boolean ) of object;
}
	sComPSDlgPaintPg = '';

{------------------------- TKNetworkDialog Comments --------------------------}

{
	TKNetworkDialog.OnExecute: TNotifyEvent
}
	sComNetDlgExec = '';

{----------------------------- TKDirTree Comments -----------------------------}

{
	TKDirTree.OnAddFile: TKAddFileEvent = procedure( const FileName: string;
		var AddFile: Boolean ) of object;
}
	sComDirTreeAddFile = '';

{----------------------------- TKTreeView Comments -----------------------------}

{
	TKTreeView.OnPaintItem: TKPaintItemEvent = procedure( Sender: TObject;
		Node: TTreeNode; DC: HDC ) of object;
}
	sComTreeViewPaintItem = '';

{------------------- TKShellComboBox/TKShellListBox Comments -------------------}

{
	TKShellComboBox/TKShellListBox.OnAddPath: TNotifyEvent;
}
	sComShellBoxAddPath = '';

{-------------------- TKCustomWin32ServiceManager Comments ---------------------}

{
	TKCustomWin32ServiceManager.OnWin32Error: TKRemoteWin32Error =
		procedure ( SvcManager: TKCustomWin32ServiceManager; ErrorCode: Cardinal;
		var Handled: Boolean ) of object;
}
	sComCWSMRemoteWin32Error = '';

implementation

end.
