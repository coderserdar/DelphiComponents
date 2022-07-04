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

unit ukwClasses;

{$I s:\v100\include\iKLIB100.inc}
{$BOOLEVAL OFF}

interface

uses
	Windows, SysUtils, WinSVC, ShellAPI, ShlObj, CommDlg, Messages, Classes, Dialogs,
	Controls, Forms, Menus, Graphics, ExtCtrls, Registry, uksyConsts, uksyTypes,
	uksyUtils, uksyClasses, ukwConsts, ukwUtils;
  
type       

	EKWClasses = class( EKWinAPI );

{
--------------------------------------------------------------------------------
--------------- Inter Process Comunications Basic API Mapping ------------------
--------------------------------------------------------------------------------
}

{ TKThreadedTimer }

  EKThreadedTimer = class( EKWClasses );

	TKThreadedTimer = class( TComponent )
	private
		FEnabled: Boolean;
		FTimer: TKThreadedTimerObject;
    
		function GetInterval: Cardinal;
		function GetTerminated: Boolean;
		function GetLockable: Boolean;
		function GetOnTimer: TNotifyEvent;
		function GetPriority: TKThreadPriority;
		procedure SetLockable( Value: Boolean );
		procedure SetEnabled( Value: Boolean );
		procedure SetInterval( Value: Cardinal );
		procedure SetOnTimer( Value: TNotifyEvent );
		procedure SetPriority( Value: TKThreadPriority );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Terminated: Boolean
						 read GetTerminated;
		property Timer: TKThreadedTimerObject
						 read FTimer;

	published
		property Enabled: Boolean
						 read FEnabled write SetEnabled default False;
		property Lockable: Boolean
						 read GetLockable write SetLockable default False;
		property Interval: Cardinal
						 read GetInterval write SetInterval default SECOND_TO_MSECOND;
		property Priority: TKThreadPriority
						 read GetPriority write SetPriority default ktpNormal;
		property OnTimer: TNotifyEvent
						 read GetOnTimer write SetOnTimer;

	end;

	EKThreadedTimerPool = class( EKWClasses );

	TKThreadedTimerItem = class;
	TKThreadedTimers = class;
	TKThreadedTimerPool = class;

{ TKThreadedTimerItem }

	TKThreadedTimerItemTimerEvent = procedure( Sender: TKThreadedTimerItem ) of object;

	TKThreadedTimerItem = class( TKCustomCollectionItem )
	private
		FData: Pointer;
		FTimer: TKThreadedTimer;
		FOnTimer: TKThreadedTimerItemTimerEvent;

		function GetOwnerCollection: TKThreadedTimers;

		function GetEnabled: Boolean;
		function GetInterval: Cardinal;
		function GetTerminated: Boolean;
		function GetLockable: Boolean;
		function GetPriority: TKThreadPriority;
		procedure SetLockable( Value: Boolean );
		procedure SetEnabled( Value: Boolean );
		procedure SetInterval( Value: Cardinal );
		procedure SetOnTimer( Value: TKThreadedTimerItemTimerEvent );
		procedure SetPriority( Value: TKThreadPriority );
		procedure TimerEvent( Sender: TObject );

	protected
		procedure DoTimer; dynamic;

		property Timer: TKThreadedTimer
						 read FTimer;

	public
		destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property Owner: TKThreadedTimers
						 read GetOwnerCollection;
		property Data: Pointer
						 read FData write FData;
		property Terminated: Boolean
						 read GetTerminated;

	published
		property Enabled: Boolean
						 read GetEnabled write SetEnabled default False;
		property Lockable: Boolean
						 read GetLockable write SetLockable default False;
		property Interval: Cardinal
						 read GetInterval write SetInterval default SECOND_TO_MSECOND;
		property Priority: TKThreadPriority
						 read GetPriority write SetPriority default ktpNormal;
		property OnTimer: TKThreadedTimerItemTimerEvent
						 read FOnTimer write SetOnTimer; 

		property Name;

	end;

{ TKThreadedTimers }

	TKThreadedTimers = class( TKCustomCollection )
	private
		function GetOwnerComp: TKThreadedTimerPool;

		procedure SetItem( Index: Integer; AItem: TKThreadedTimerItem );
		function GetItem( Index: Integer ): TKThreadedTimerItem;

		function GetItemByName( const AName: string ): TKThreadedTimerItem;

	protected
		procedure Update( Item: TCollectionItem ); override;

	public
		constructor Create( AComp: TKThreadedTimerPool ); virtual;

		function Add: TKThreadedTimerItem; virtual;

		property Component: TKThreadedTimerPool
						 read GetOwnerComp;
		property Items[Index: Integer]: TKThreadedTimerItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKThreadedTimerItem
						 read GetItemByName;
		property Names;

	end;

{ TKThreadedTimerPool }

	TKEnumTimerFunc = function( Sender: TKThreadedTimerPool; Timer: TKThreadedTimerItem;
	  Index: Integer; Data: Pointer ): Boolean of object;

	TKThreadedTimerPool = class( TComponent )
	private
		FTimers: TKThreadedTimers;
		procedure SetTimers( Value: TKThreadedTimers );

	protected
		procedure UpdateItem( Item: TKThreadedTimerItem ); virtual;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

		function ForEachTimerDo( EnumTimerFunc: TKEnumTimerFunc; Data: Pointer ): Boolean; dynamic;

	published
		property Timers: TKThreadedTimers
						 read FTimers write SetTimers;

	end;

{ TKWorkerThread }

	EKWorkerThread = class( EKWClasses );

	TKWorkerThreadComp = class;

	TKWorkerThread = class( TKCustomWorkerThread )
	protected
		function GetOwner: TKWorkerThreadComp; virtual;

    function CheckExecuteCondition: Boolean; override;
		procedure SynchronizeOwner; override;

	public
		constructor CreateWorkerThread( AOwner: TKWorkerThreadComp ); virtual;

    property CrtSec;
		property Handle;
		property LastError;
		property LastErrorMessage;

		property OnFail;
		property OnTimeOut;

		property ThreadTimes;
		property Terminated;
		property Priority;
		property Suspended;
		property ThreadID;

		property Owner: TKWorkerThreadComp
						 read GetOwner;

	end;

	TKWorkerThreadClass = class of TKWorkerThread;

{ TKWorkerThreadComp }

	TKGetThreadEvent = procedure( Sender: TKWorkerThreadComp; var Thread: TKWorkerThread ) of object;
	TKWorkerThreadExecuteEvent = procedure( Sender: TKWorkerThreadComp; Thread: TKWorkerThread;
		Param: Pointer; var ReturnCode: Cardinal; var Continue: Boolean ) of object;
	TKWorkerThreadTerminateEvent = procedure( Sender: TKWorkerThreadComp;
		Thread: TKWorkerThread ) of object;
	TKWorkerThreadExecuteMsgEvent = procedure( Sender: TKWorkerThreadComp;
    Thread: TKWorkerThread; Param: Pointer; Msg: TMsg; var Processed,
    Continue: Boolean ) of object;

	TKWorkerThreadComp = class( TComponent )
	private
		FThread: TKWorkerThread;
    FStoppingAll: Boolean;
		FWaitForThread: Boolean;
		FWaitForInterval: Cardinal;
		FPriority: TKThreadPriority;
    FCreateSuspended: Boolean;
    FCreateMessageQueue: Boolean;
    FFreeThreadOnTerminate: Boolean;
		FOnGetThread: TKGetThreadEvent;
    FMsgFilterMin: Integer;
    FMsgFilterMax: Integer;
		FOnThreadExecute: TKWorkerThreadExecuteEvent;
    FOnThreadExecuteMsg: TKWorkerThreadExecuteMsgEvent;
		FOnThreadTerminate: TKWorkerThreadTerminateEvent;
    FOnThreadDestroy: TKWorkerThreadTerminateEvent;

		procedure ThreadExecuteEvent( Sender: TKCustomWorkerThread; Param: Pointer;
			var ReturnCode: Cardinal; var Continue: Boolean );
		procedure ThreadExecuteMsgEvent( Sender: TKCustomWorkerThread; Param: Pointer;
  	  Msg: TMsg; var Processed, Continue: Boolean );
		procedure ThreadTerminateEvent( Sender: TObject );
		procedure SetPriority( Value: TKThreadPriority );

	protected
		function GetThreadClass: TKWorkerThreadClass; virtual;
		function GetThread: TKWorkerThread; { descendent can override this method returning
																		appropriate TThread descendent }
		procedure DoGetThread; dynamic;
		function DoThreadExecute( Param: Pointer; var ReturnCode: Cardinal ): Boolean; dynamic;
    function DoThreadExecuteMsg( Param: Pointer; msg: TMsg;
      var Processed: Boolean ): Boolean; dynamic;
		procedure DoThreadTerminate; dynamic;
    procedure DoThreadDestroy; dynamic;

    procedure SetStoppingAll( Value: Boolean );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function Stop: Boolean; virtual;
		function Start: Boolean; virtual;
		function Running: Boolean; virtual;
    procedure PostQuitMessage; virtual;

		property Thread: TKWorkerThread
						 read GetThread;
    property StoppingAll: Boolean
             read FStoppingAll; { Hard Couple! Used by ThreadPool for synch... :( }

	published
    property MsgFilterMin: Integer
             read FMsgFilterMin write FMsgFilterMin default 0;
    property MsgFilterMax: Integer
             read FMsgFilterMax write FMsgFilterMax default 0;
    property CreateSuspended: Boolean
             read FCreateSuspended write FCreateSuspended default False;
    property CreateMessageQueue: Boolean
             read FCreateMessageQueue write FCreateMessageQueue default False;
		property WaitForThread: Boolean
						 read FWaitForThread write FWaitForThread default true;
		property WaitForInterval: Cardinal
						 read FWaitForInterval write FWaitForInterval default Cardinal( INFINITE );
		property Priority: TKThreadPriority
						 read FPriority write SetPriority default ktpNormal;
    property FreeThreadOnTerminate: Boolean
             read FFreeThreadOnTerminate write FFreeThreadOnTerminate default False;
		property OnGetThread: TKGetThreadEvent
						 read FOnGetThread write FOnGetThread;
		property OnThreadExecute: TKWorkerThreadExecuteEvent
						 read FOnThreadExecute write FOnThreadExecute;
		property OnThreadExecuteMsg: TKWorkerThreadExecuteMsgEvent
						 read FOnThreadExecuteMsg write FOnThreadExecuteMsg;
		property OnThreadTerminate: TKWorkerThreadTerminateEvent
						 read FOnThreadTerminate write FOnThreadTerminate;
    property OnThreadDestroy: TKWorkerThreadTerminateEvent
             read FOnThreadDestroy write FOnThreadDestroy;

	end;

	TKWorkerThreads = class;
	TKWorkerThreadItem = class;
	TKWorkerThreadPool = class;

{ TKWorkerThreadItem }

	TKGetThreadItemEvent = procedure( Source: TKWorkerThreadComp; Sender: TKWorkerThreadItem;
    var Thread: TKWorkerThread ) of object;
	TKWorkerThreadItemExecuteEvent = procedure( Source: TKWorkerThreadComp;
    Sender: TKWorkerThreadItem; Thread: TKWorkerThread; Param: Pointer;
    var ReturnCode: Cardinal; var Continue: Boolean ) of object;
	TKWorkerThreadItemExecuteMsgEvent = procedure( Source: TKWorkerThreadComp;
    Sender: TKWorkerThreadItem; Thread: TKWorkerThread; Param: Pointer; Msg: TMsg;
    var Processed, Continue: Boolean ) of object;
	TKWorkerThreadItemTerminateEvent = procedure( Source: TKWorkerThreadComp;
    Sender: TKWorkerThreadItem; Thread: TKWorkerThread ) of object;

	TKWorkerThreadItem = class( TKCustomCollectionItem )
	private
		FData: Integer;
		FWorkerThreadComp: TKWorkerThreadComp;
		FOnGetThread: TKGetThreadItemEvent;
		FOnThreadExecute: TKWorkerThreadItemExecuteEvent;
    FOnThreadExecuteMsg: TKWorkerThreadItemExecuteMsgEvent;
		FOnThreadTerminate: TKWorkerThreadItemTerminateEvent;
    FOnThreadDestroy: TKWorkerThreadItemTerminateEvent;

		function GetThread: TKWorkerThread;
		function GetOwnerCollection: TKWorkerThreads;
    function GetFreeThreadOnTerminate: Boolean;
		function GetPriority: TKThreadPriority;
		function GetWaitForInterval: Cardinal;
		function GetWaitForThread: Boolean;
		function GetCreateSuspended: Boolean;
		function GetCreateMessageQueue: Boolean;
		function GetMsgFilterMin: Integer;
    function GetMsgFilterMax: Integer;
		procedure SetPriority( Value: TKThreadPriority );
		procedure SetWaitForInterval( Value: Cardinal );
		procedure SetWaitForThread( Value: Boolean );
    procedure SetFreeThreadOnTerminate( Value: Boolean );
    procedure SetCreateSuspended( Value: Boolean );
    procedure SetCreateMessageQueue( Value: Boolean );
    procedure SetMsgFilterMin( Value: Integer );
    procedure SetMsgFilterMax( Value: Integer );

	protected
		procedure GetThreadEvent( Sender: TKWorkerThreadComp; var Thread: TKWorkerThread ); dynamic;
		procedure ThreadExecuteEvent( Sender: TKWorkerThreadComp; Thread: TKWorkerThread;
			Param: Pointer; var ReturnCode: Cardinal; var Continue: Boolean ); dynamic;
		procedure ThreadExecuteMsgEvent( Sender: TKWorkerThreadComp; Thread: TKWorkerThread;
			Param: Pointer; Msg: TMsg; var Processed, Continue: Boolean ); dynamic;
		procedure ThreadTerminateEvent( Sender: TKWorkerThreadComp; Thread: TKWorkerThread ); dynamic;
    procedure ThreadDestroyEvent( Sender: TKWorkerThreadComp; Thread: TKWorkerThread ); dynamic;

		property WorkerThreadComp: TKWorkerThreadComp
						 read FWorkerThreadComp;

	public

		destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property Owner: TKWorkerThreads
						 read GetOwnerCollection;
		property Thread: TKWorkerThread
						 read GetThread;

		function Stop: Boolean; virtual;
		function Start: Boolean; virtual;
		function Running: Boolean; virtual;
    procedure PostQuitMessage; virtual;

	published
		property Name;
    property MsgFilterMin: Integer
             read GetMsgFilterMin write SetMsgFilterMin default 0;
    property MsgFilterMax: Integer
             read GetMsgFilterMax write SetMsgFilterMax default 0;
    property CreateSuspended: Boolean
             read GetCreateSuspended write SetCreateSuspended default False;
    property CreateMessageQueue: Boolean
             read GetCreateMessageQueue write SetCreateMessageQueue default False;
    property WaitForThread: Boolean
						 read GetWaitForThread write SetWaitForThread default True;
		property WaitForInterval: Cardinal
						 read GetWaitForInterval write SetWaitForInterval default Cardinal( INFINITE );
		property Priority: TKThreadPriority
						 read GetPriority write SetPriority default ktpNormal;
    property FreeThreadOnTerminate: Boolean
             read GetFreeThreadOnTerminate write SetFreeThreadOnTerminate default False;
		property Data: Integer
						 read FData write FData;
		property OnGetThread: TKGetThreadItemEvent
						 read FOnGetThread write FOnGetThread;
		property OnThreadExecute: TKWorkerThreadItemExecuteEvent
						 read FOnThreadExecute write FOnThreadExecute;
		property OnThreadExecuteMsg: TKWorkerThreadItemExecuteMsgEvent
						 read FOnThreadExecuteMsg write FOnThreadExecuteMsg;
		property OnThreadTerminate: TKWorkerThreadItemTerminateEvent
						 read FOnThreadTerminate write FOnThreadTerminate;
    property OnThreadDestroy: TKWorkerThreadItemTerminateEvent
             read FOnThreadDestroy write FOnThreadDestroy;
	end;

{ TKWorkerThreads }

	TKEnumWorkersFunc = function( Sender: TKWorkerThreadPool; Worker: TKWorkerThreadItem;
		Index: Integer; Data: Pointer ): Boolean of object;

	TKWorkerThreads = class( TKCustomCollection )
	private
		function GetOwnerComp: TKWorkerThreadPool;

		function GetItem( Index: Integer ): TKWorkerThreadItem;
		function GetItemByName( const AName: string ): TKWorkerThreadItem;
		procedure SetItem( Index: Integer; AItem: TKWorkerThreadItem );
    procedure SetStoppingAll( Value: Boolean );

	protected
		procedure Update( Item: TCollectionItem ); override;

		property Component: TKWorkerThreadPool
						 read GetOwnerComp;

	public
		constructor Create( AComp: TKWorkerThreadPool ); virtual;

		function Add: TKWorkerThreadItem; virtual;
    function RunningCount: Integer;

		procedure StartAll; virtual;
		procedure StopAll; virtual;
		procedure Start( const Index: array of Integer ); virtual;
		procedure Stop( const Index: array of Integer ); virtual;

		function ForEachWorkerDo( EnumWorkerFunc: TKEnumWorkersFunc; Data: Pointer ): Boolean; dynamic;

		property Items[Index: Integer]: TKWorkerThreadItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKWorkerThreadItem
						 read GetItemByName;
		property Names;

	end;

{ TKWorkerThreadPool }

	TKWorkerThreadPool = class( TComponent )
	private
		FThreads: TKWorkerThreads;
		procedure SetThreads( Value: TKWorkerThreads );

	protected
		procedure UpdateItem( Item: TKWorkerThreadItem ); virtual;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

	published
		property Threads: TKWorkerThreads
						 read FThreads write SetThreads;

	end;

{ TKProducerConsumerThread }

	TKProducerConsumer = class;

	TKProducerConsumerThread = class( TKCustomWorkerThread )
	protected
		function GetOwner: TKProducerConsumer; virtual;

	public
		constructor CreateProducerConsumer( AOwner: TKProducerConsumer; CreateSuspended: Boolean;
			ThreadPriority: TKThreadPriority; ExecuteEvent, TerminateEvent: TNotifyEvent ); virtual;

		property Owner: TKProducerConsumer
						 read GetOwner;

	end;

{ TKProducerConsumer }
																
	EKProducerConsumer = class( EKWClasses );

	TKProduceConsumeEvent = procedure( Sender: TKProducerConsumer;
		Thread: TKProducerConsumerThread; const Count, MaxCount: Cardinal;
		var DownCount: Cardinal ) of object;

	TKProducerConsumer = class( TComponent )
	private
		FProducer: TKProducerConsumerThread;
		FConsumer: TKProducerConsumerThread;
		FProducerQueue: TKSemaphore;
		FConsumerQueue: TKSemaphore;
		FOnProduce: TKProduceConsumeEvent;
		FOnConsume: TKProduceConsumeEvent;
		FMutex: TKMutex;
		FCounterMutex: TKMutex;
		FMaxProducerCount: Cardinal;
		FMaxConsumerCount: Cardinal;
		FProducerCount: Cardinal;
		FConsumerCount: Cardinal;
		FRunning: Boolean;
		FSuspended: Boolean;
		FOnStart: TNotifyEvent;
		FOnStop: TNotifyEvent;
		FConsumerPriority: TThreadPriority;
		FProducerPriority: TThreadPriority;

		function GetMaxProducerCount: Cardinal;
		function GetMaxConsumerCount: Cardinal;
		function GetConsumerPriority: TThreadPriority;
		function GetProducerPriority: TThreadPriority;
		function GetOnProduce: TKProduceConsumeEvent;
		function GetOnConsume: TKProduceConsumeEvent;
		function GetOnStart: TNotifyEvent;
		function GetOnStop: TNotifyEvent;
		procedure SetMaxProducerCount( Value: Cardinal );
		procedure SetMaxConsumerCount( Value: Cardinal );
		procedure SetProducerPriority( Value: TThreadPriority );
		procedure SetConsumerPriority( Value: TThreadPriority );
		procedure SetOnProduce( Value: TKProduceConsumeEvent );
		procedure SetOnConsume( Value: TKProduceConsumeEvent );
		procedure SetOnStart( Value: TNotifyEvent );
		procedure SetOnStop( Value: TNotifyEvent );

		function GetProducerCount: Cardinal;
		function GetConsumerCount: Cardinal;
		function GetRunning: Boolean;
		function GetSuspended: Boolean;

{$HINTS OFF}
		procedure ProduceExecute( Sender: TObject );
		procedure ProduceTerminate( Sender: TObject );
		procedure ConsumeExecute( Sender: TObject );
		procedure ConsumeTerminate( Sender: TObject );
{$HINTS ON}

	protected
		procedure ForceActive; dynamic;
		procedure ForceInActive; dynamic;

		procedure DoStart; dynamic;
		procedure DoStop; dynamic;
		function DoProduce( const Count: Cardinal ): Cardinal; dynamic;
		function DoConsume( const Count: Cardinal ): Cardinal; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function Start( CreateSuspended: Boolean ): Boolean; virtual;
		function Stop: Boolean; virtual;
		function Suspend: Boolean; virtual;
		function Resume: Boolean; virtual;

		property Running: Boolean
						 read GetRunning;
		property Suspended: Boolean
						 read GetSuspended;				 
		property ProducerCount: Cardinal
						 read GetProducerCount;
		property ConsumerCount: Cardinal
						 read GetConsumerCount;

	published
		property MaxProducerCount: Cardinal
						 read GetMaxProducerCount write SetMaxProducerCount default High( Cardinal );
		property MaxConsumerCount: Cardinal
						 read GetMaxConsumerCount write SetMaxConsumerCount default High( Cardinal );
		property ProducerPriority: TThreadPriority
						 read GetProducerPriority write SetProducerPriority default tpNormal;
		property ConsumerPriority: TThreadPriority
						 read GetConsumerPriority write SetConsumerPriority default tpNormal;
		property OnProduce: TKProduceConsumeEvent
						 read GetOnProduce write SetOnProduce;
		property OnConsume: TKProduceConsumeEvent
						 read GetOnConsume write SetOnConsume;
		property OnStart: TNotifyEvent
						 read GetOnStart write SetOnStart;
		property OnStop: TNotifyEvent
						 read GetOnStop write SetOnStop;

	end;


{
--------------------------------------------------------------------------------
--------------------------------- TKMailSlot -----------------------------------
--------------------------------------------------------------------------------
}

{
	Notes on MailSlot architecture:

		« Mailslots are supposed to work on local networks on which
			the OSI network and transport layers may be trusted. »

		Some technical issues covered by KMailSlot include:

			.Since duplicate datagrams may arrive due to the existence
			 of multiple protocols bound to the same NIC, the component
			 identifies and eliminates duplicate datagrams as they arrive;

			.Messages that require ACKnowledgement timeout after a fixed
			 amount of time (user-defined);

			.Two special datagram types allow transmission of large blocks
			 of data; this transmission is carried out by breaking the data
			 into smaller blocks; the receiver waits for each smaller block,
			 reorders them, builds the original data, and signals the user a
			 message has arrived;

			.Automatic confirmation of receipt: ACK datagrams are sent
			 automatically when acknowledgement is requested;


	KMailsots work on two modes: the normal mode (N-MODE), on which the
	main datagram types are exchanged among communicating parts, and, the
	segmentation mode (S-MODE), on which data is broken down into smaller
	parts prior to transmission.


		N-MODE
		------
			On normal mode, mailslots operate with one kind of datagram
			for all datagram types. This datagram is the N-Datagram.

				N-Datagram (25-121 bytes):

					FIELD   TYPE           SIZE (bytes)   NAME
					-----   -----------    ------------   ---------------------------
						0      LONGINT        04             Application Handle
						1      TIMESTAMP      08             Signature
						2      BYTE           01             Datagram Type*
						3      LONG           04             Control Options 01 (CO_01)
						4      LONG           04             Control Options 02 (CO_02)
						5      PCHAR          01-65          Sender Slot Name
						6      PCHAR          01-17          Sender Server Name
						7      PCHAR          01-17          Sender User Name
						8      BYTE           01             Reserved ($00)

				*Bit 7 of Datagram Type indicates whether the message needs ACK.

			N-Datagram types are normally used for control, sinchronization,
			and acknowledgement purposes. Also, to initialize and terminate
			large binary data transmissions (S-MODE). Broadcasts and confirmation
			requests are not allowed in all circumstances; the following table
			indicates the services allowed for each N-MODE datagram.

					TYPE         CO_01    CO_02    CONFIRMATION    BROADCAST
					---------    ------   ------   -------------   -------------
					 ACK          DATE#   TIME#     NOT ALLOWED     NOT ALLOWED
					 SYNC         0       0         ALLOWED         ALLOWED
					 CONTROL      ANY     ANY       ALLOWED**       ALLOWED
					 SOB          SIZE*   TX_ID     ALLOWED**       ALLOWED
					 EOB          STAT    TX_ID     ALLOWED**       ALLOWED
					 ERN          STAT    TX_ID     ALLOWED**       ALLOWED

				#Date and time of the original message to which this one is
				 ACK;

				*Bits 30 and 31 determinte the data transmission type:

						TYPE    BIT 30   BIT 31
						------  ------   ------
						 Data    OFF      OFF
						 Text    OFF      ON
						 File    ON       OFF

			 **Only if the datagram is not a broadcast datagram. A special case
				 of confirmed broadcast is allowed: SYNC. This eases the inspection
				 of active mailslot users on the network.


		S-MODE
		------
			Segmented mode is initiated by a SOB (Start of Binary) datagram. Data
			transmission itself is held with a different datagram, the S-Datagram.

				S-Datagram (265 bytes):

					FIELD   TYPE           SIZE (bytes)   NAME
					-----   -----------    ------------   ---------------------------
						0      LONG           04             Transmission ID (TX_ID)*
						1      LONG           04             Sequence Number (SEQNO)**
						2      SIZE           01             Data Size
						3      BYTES          256            Data

				*The transmission ID is the same identifier as indicated by SOT and
				 SOB datagrams, on the CO_02 field.
			 **Sequence numbers indicate the order in which the arriving datagrams
				 should be considered.


			S-MODE transmissions must follow the steps:

				SENDER
				------

				#1 - A SOB N-Datagram is sent to the destination slot
						 indicating the size of the data to be transmitted (CO_01),
						 and the transmission ID (CO_02).

				#2 - Data is broken down in blocks no larger than 256 bytes and
						 sent to the destination slot using S-datagrams. Each of these
						 datagrams will indicate the transmission ID in use, the
						 sequence number of the data block (1..n), the data size of
						 the current block (1..256), and the data itself.

				#3 - After transmitting all data blocks, an EOB N-Datagram is
						 sent to the destination slot indicating the end of transmission.
						 The CO_01 field will indicate the Status of the transmission,
						 which may be OK or ABORTED. The CO_02 field will indicate the
						 transmission ID that is being finished.


				RECEIVER
				--------

				#1 - After receiving the N-Datagram indicating the start of a
						 data transmission, the receiver will perform the following
						 tasks: calculate the number of data blocks to be transmitted
						 using the data size*; allocate memory for each data block;
						 initialize a bitmap indicating all arrived data blocks; save
						 the transmission ID for this session.

				*SEQNO ranges from 1 to ( [DATA_SIZE div 256] + [DATA_SIZE mod 256] )

				#2 - For each S-Datagram arriving on this transmission ID, the
						 sequence number of the data block (SEQNO field) is used to
						 gather data correctly. Sequence numbers are also used to update
						 the arrived blocks bitmap.

				#3 - After a N-Datagram arrives indicating the end of transmission,
						 the receiver will check the arrived blocks bitmap and verifiy
						 if all expected blocks in fact arrived. This information will
						 be provided in an End of Receipt Notification ERN N-Datagram.
						 The CO_01 field will indicate the final status of the message:
						 OK or FAIL; the CO_02 field will indicate the transmission ID
						 of the message.

				#4 - Alternatively, the receiver may send an ERN N-Datagram after
						 a long wait (timeout) or after all datagrams arrive. The CO_01
						 and CO_02 fields will be the same as in #3.

}

	TKMailType = ( mtACK, mtSYNC, mtControl, mtSOB, mtEOB, mtERN );

	TKMSUserInfo = record
		SlotName: string;
		ServerName: string;
		UserName: string;
	end;

	PKNDatagramHeader = ^TKNDatagramHeader;
	TKNDatagramHeader = packed record
		AppHwnd: Cardinal;
		Signature: TTimeStamp;
		DatagramType: Byte;
		Control_01: LongInt;
		Control_02: LongInt;
{
		SlotName: PChar;
		ServerName: PChar;
		UserName: PChar;
		Reserved: Byte;
}
	end;

	PKSDatagram = ^TKSDatagram;
	TKSDatagram = packed record
		TransmissionID: LongInt;
		SequenceNo: LongInt;
		DataSize: Byte;
		Data: array[1..256] of Byte;
	end;

	PKMailSlotRawData = ^TKMailSlotRawData;
	TKMailSlotRawData = record
		dwSize: LongInt;
		lpData: Pointer;
	end;

	TKBroadcastMode = ( bmDefault, bmDomain, bmAllDomains );

	TKMailSlot = class;
	TKCustomMailSlot = class;

{	TKReaderThread }

	TKReaderThread = class( TThread )
	private
		FMsgsWaiting: DWORD;
		FNextMsgSize: DWORD;
		FMailSlot: TKCustomMailSlot;

	protected
		function MailSlotInfo: Boolean;

	public
		constructor Create( Slot: TKCustomMailSlot );

		procedure Execute; override;
		procedure MessageReady;

	end;

{	TKReceiverSession }

	TKMSDataType = ( mdtData, mdtText, mdtFile ); 

	TKReceiverSession = class( TObject )
	private
		FSize: LongInt;
		FData: Pointer;
		FReceived: TBits;
		FSlotName: string;
		FServerName: string;
		FIdleSince: LongInt;
		FDataBlocks: LongInt;
		FTransmissionID: LongInt;
		FDataType: TKMSDataType;

		FOwner: TKMailSlot;

		function GetDataComplete: Boolean;
		function GetReceived( Index: Integer ): Boolean;
		procedure SetReceived( Index: Integer; Value: Boolean );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TKMailSlot; const ASlot, AServer: string; ASize,
			ATxID: LongInt; ADataType: TKMSDataType );

		procedure ReceiveDatagram( ADatagram: PKSDatagram );

		property Data: Pointer
						 read FData write FData;
		property DataBlocks: LongInt
						 read FDataBlocks;
		property DataComplete: Boolean
						 read GetDataComplete;
		property DataType: TKMSDataType
						 read FDataType;
		property IdleSince: LongInt
						 read FIdleSince;
		property Owner: TKMailSlot
						 read FOwner;
		property Received[Index: Integer]: Boolean
						 read GetReceived write SetReceived;
		property ServerName: string
						 read FServerName;
		property Size: LongInt
						 read FSize;
		property SlotName: string
						 read FSlotName;
		property TransmissionID: LongInt
						 read FTransmissionID;

	end;

{ TKReceiverList }

	TKReceiverList = class( TObject )
	private
		FList: TList;

		function GetCount: Integer;
		function GetItems( Index: Integer ): TKReceiverSession;
		function GetItemsFromID( TxID: LongInt ): TKReceiverSession;

	public
		destructor Destroy; override;
		constructor Create;

		procedure Pack;
		procedure Clear;
		procedure Delete( Index: Integer );
		procedure Remove( Value: TKReceiverSession );

		function Add( Value: TKReceiverSession ): Integer;
		function IndexOf( Value: TKReceiverSession ): Integer;


		property Count: Integer
						 read GetCount;
		property SessionsFromID[TxID: LongInt]: TKReceiverSession
						 read GetItemsFromID;
		property Sessions[Index: Integer]: TKReceiverSession
						 read GetItems; default;

	end;

{ TKCustomMailSlot }

	EKCustomMailSlot = class( EKWClasses );

	TKMReadMail = record
		Msg: LongInt;
		Index: LongInt;
		LParam: LongInt;
		Result: LongInt;
	end;

	TKCustomMailSlot = class( TComponent )
	private
		FSlot: string;
		FMaxSize: DWORD;
		FHandle: THandle;
		FActive: Boolean;
		FMailList: TList;
		FReaderThread: TKReaderThread;

		FOnActivate: TNotifyEvent;
		FOnDeactivate: TNotifyEvent;

		function  GetMsgsWaiting: DWORD;
		function  GetNextMsgSize: DWORD;
		function GetMail( Index: Integer ): PKMailSlotRawData;
		procedure SetSlot( const Value: string );

		procedure ReadMessage;
		procedure PackMailList;
		procedure ClearMailList;
		procedure DeleteRawData( Index: Integer );
		procedure DoReceiveMail( pms: PKMailSlotRawData );
		function AppWndProc( var Message: TMessage ): Boolean;

		procedure KMReadMail( var Message: TKMReadMail );
							message KM_READMAIL;

	protected
		procedure DoActivate; virtual;
		procedure DoDeactivate; virtual;

		procedure CheckInactive;

		property Active: Boolean
						 read FActive;
		property MailList[Index: Integer]: PKMailSlotRawData
						 read GetMail;
		property MaxSize: DWORD
						 read FMaxSize write FMaxSize default 0;
		property MsgsWaiting: DWORD
						 read GetMsgsWaiting;
		property NextMsgSize: DWORD
						 read GetNextMsgSize;
		property Slot: string
						 read FSlot write SetSlot;

		property OnActivate: TNotifyEvent
						 read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent
						 read FOnDeactivate write FOnDeactivate;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Open;
		procedure Close;
		function HasMail: Boolean;

	end;

{ TKMailSlot }

	EKMailSlot = class( EKCustomMailSlot );

	TKMSDataSize = 1..$3FFFFFFF;

	TKMMSTimeOut = record 
		Msg: LongInt;
		Index: LongInt;
		Unused: LongInt;
		Result: LongInt;
	end;	

	TKMMSReceiveACK = record
		Msg: LongInt;
		Index: LongInt;
		Unused1: Word;
		Unused2: Byte;
		DatagramType: TKMailType;
		Result: LongInt;
	end;

	TKMailTimeOutEvent = procedure( Sender: TKMailSlot; Signature: TDateTime;
		UserInfo: TKMSUserInfo; MailType: TKMailType ) of object;

	TKMailACKEvent = procedure( Sender: TKMailSlot; Signature: TDateTime;
		UserInfo: TKMSUserInfo; AType: TKMailType ) of object;

	TKMailNDatagramEvent = procedure( Sender: TKMailSlot; Signature: TDateTime;
		UserInfo: TKMSUserInfo; Control_01, Control_02: LongInt ) of object;

	TKTextArriveEvent = procedure( Sender: TKMailSlot; Success: Boolean; TxID: LongInt;
		const Text: string ) of object;

	TKFileArriveEvent = procedure( Sender: TKMailSlot; Success: Boolean; TxID: LongInt;
		var FileName: TFileName; Size: LongInt ) of object;

	TKDataArriveEvent = procedure( Sender: TKMailSlot; Success: Boolean; TxID: LongInt;
		Data: Pointer; Size: LongInt ) of object;

	TKTransmissionEndEvent = procedure( Sender: TKMailSlot; TxID: LongInt;
		UserInfo: TKMSUserInfo; Success: Boolean ) of object;

	TKMailSlot = class( TKCustomMailSlot )
	private
		FEcho: Boolean;
		FTimer: TTimer;
		FDomain: string;
		FNeedACK: TList;
		FReceived: TList;
		FTimeOut: Cardinal;
		FDomains: TStrings;
		FBurstCount: Cardinal;
		FBurstDelay: Cardinal;
		FBufferSize: Cardinal;
		FReceivers: TKReceiverList;
		FBroadcastMode: TKBroadcastMode;

		FOnReceiveACK: TKMailACKEvent;
		FOnTimeOut: TKMailTimeOutEvent;
		FOnDataArrive: TKDataArriveEvent;
		FOnTextArrive: TKTextArriveEvent;
		FOnFileArrive: TKFileArriveEvent;
		FOnSynchronize: TKMailNDatagramEvent;
		FOnReceiveControl: TKMailNDatagramEvent;
		FOnTransmissionEnd: TKTransmissionEndEvent;

		procedure MailCheck( Sender: TObject );
		function GetDomain: string;
		function GetInterval: Cardinal;
		procedure SetInterval( Value: Cardinal );
		procedure SetTimeOut( Value: Cardinal );
		procedure SetBufferSize( Value: Cardinal );
		procedure SetBurstCount( Value: Cardinal );
		procedure SetBurstDelay( Value: Cardinal );
		procedure SetDomain( const Value: string );

		function GetCurrentDomain: string;

		procedure	ClearNeedACKList;
		procedure ClearReceivedList;

		function GetMailType( Value: Byte ): TKMailType;
		procedure UpdateACKMail( pdh: PKNDatagramHeader; const ToSlot, ToServer: string );
		procedure SendSDatagram( psd: PKSDatagram; const ToSlot, ToServer: string );
		procedure SendNDatagram( NeedACK: Boolean; AMailType: TKMailType;
			AControl_01, AControl_02: LongInt; const ToSlot, ToServer: string );
		function NeedACK( pdh: PKNDatagramHeader ): Boolean;
		function Equals( pdh1, pdh2: PKNDatagramHeader ): Boolean;
		function UpdateReceivedMail( var pdh: PKNDatagramHeader ): Boolean;
		function GetUserInfo( pdh: PKNDatagramHeader ): TKMSUserInfo;

		procedure DelayTimeOut( Index: Integer );
		procedure DelayReceiveACK( Index: Integer; ADatagramType: Byte );

		procedure KMReadMail( var Message: TKMReadMail );
							message KM_READMAIL;
		procedure KMMSTimeOut( var Message: TKMMSTimeOut );
							message KM_MSTIMEOUT;
		procedure KMMSReceiveACK( var Message: TKMMSReceiveACK );
							message KM_MSRECEIVEACK;

		function InternalSendData( const ToSlot, ToServer: string; Confirmed: Boolean;
			Data: Pointer; Size: LongInt ): LongInt;
		procedure InternalBroadcast( const ToSlot: string; AType: TKMailType;
			Confirmed: Boolean; Control_01, Control_02: LongInt; Data: Pointer;
			DataSize: LongInt );

	protected
		procedure DoActivate; override;
		procedure DoDeactivate; override;
		procedure DoDataArrive( ASession: TKReceiverSession ); dynamic;
		procedure DoReceiveEOB( UserInfo: TKMSUserInfo; Status, TxID: LongInt ); dynamic;
		procedure DoReceiveERN( UserInfo: TKMSUserInfo; Status, TxID: LongInt ); dynamic;
		procedure DoTimeOut( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; AType: TKMailType ); dynamic;
		procedure DoReceiveACK( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; AType: TKMailType ); dynamic;
		procedure DoReceiveSOB( UserInfo: TKMSUserInfo; DataSize, TxID: LongInt; DataType: TKMSDataType ); dynamic;
		procedure DoSynchronize( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; Control_01, Control_02: LongInt ); dynamic;
		procedure DoReceiveControl( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; Control_01, Control_02: LongInt ); dynamic;

		procedure Process_SDatagram( Data: PKSDatagram );
		procedure Process_NDatagram( Data: Pointer; Size: Integer );

		procedure DoDispatchMail( pdh: PKNDatagramHeader );
		procedure EndOfSegmentation( ASession: TKReceiverSession );

		property Domains: TStrings
						 read FDomains;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Sync( const ToSlot, ToServer: string; Confirmed: Boolean;
			Control_01, Control_02: LongInt );
		procedure SendControl( const ToSlot, ToServer: string; Confirmed: Boolean;
			Control_01, Control_02: LongInt );
		function SendData( const ToSlot, ToServer: string; Confirmed: Boolean;
			Data: Pointer; Size: TKMSDataSize ): LongInt;
		function SendText( const ToSlot, ToServer: string; Confirmed: Boolean;
			const Data: string ): LongInt;
		function SendFile( const ToSlot, ToServer: string; Confirmed: Boolean;
			const FileName: TFileName ): LongInt;

		procedure BroadcastSync( const ToSlot: string; Confirmed: Boolean;
			Control_01, Control_02: LongInt );
		procedure BroadcastControl( const ToSlot: string; Confirmed: Boolean;
			Control_01, Control_02: LongInt );
		procedure BroadcastData( const ToSlot: string; Confirmed: Boolean;
			Data: Pointer; Size: TKMSDataSize );
		procedure BroadcastText( const ToSlot: string; Confirmed: Boolean;
			const Data: string );
		procedure BroadcastFile( const ToSlot: string; Confirmed: Boolean;
			const FileName: TFileName );

		property Active;

		property CurrentDomain: string
						 read GetCurrentDomain;

	published
		property Slot;
		property OnActivate;
		property OnDeactivate;

		property BroadcastMode: TKBroadcastMode
						 read FBroadcastMode write FBroadcastMode default bmDefault;
		property BufferSize: Cardinal
						 read FBufferSize write SetBufferSize default DEF_MS_BUFFSIZE;
		property BurstCount: Cardinal
						 read FBurstCount write SetBurstCount default DEF_MS_BURSTCOUNT;
		property BurstDelay: Cardinal
						 read FBurstDelay write SetBurstDelay default DEF_MS_BURSTDELAY;
		property Domain: string
						 read GetDomain write SetDomain;
		property Echo: Boolean
						 read FEcho write FEcho;
		property Interval: Cardinal
						 read GetInterval write SetInterval default DEF_MS_INTERVAL;
		property TimeOut: Cardinal
						 read FTimeOut write SetTimeOut default DEF_MS_TIMEOUT;

		property OnDataArrive: TKDataArriveEvent
						 read FOnDataArrive write FOnDataArrive;
		property OnFileArrive: TKFileArriveEvent
						 read FOnFileArrive write FOnFileArrive;
		property OnReceiveACK: TKMailACKEvent
						 read FOnReceiveACK write FOnReceiveACK;
		property OnReceiveControl: TKMailNDatagramEvent
						 read FOnReceiveControl write FOnReceiveControl;
		property OnSynchronize: TKMailNDatagramEvent
						 read FOnSynchronize write FOnSynchronize;
		property OnTextArrive: TKTextArriveEvent
						 read FOnTextArrive write FOnTextArrive;
		property OnTimeOut: TKMailTimeOutEvent
						 read FOnTimeOut write FOnTimeOut;
		property OnTransmissionEnd: TKTransmissionEndEvent
						 read FOnTransmissionEnd write FOnTransmissionEnd;

	end;

{ TKAppControl }

  TKAppControl = class;

	EKEAppControl = class( EKWClasses );

	TKInitEnvEvent = procedure( Sender: TKAppControl; var CancelWait: Boolean ) of object;
  
	TKAppControl = class( TComponent )
	private
		FTitle: string;
		FInternalID: Cardinal;
		FHintColor: TColor;
		FHintPause: Integer;
		FSplashWait: Integer;
		FRootDir: string;
		FBuiltInName: string;
		FTerminated: Boolean;
		FShowMainForm: Boolean;
		FShowOnTaskbar: Boolean;
		FNetworkSearch: Boolean;
		FHintHidePause: Integer;
		FHintShortPause: Integer;
		FInstancesAllowed: Integer;
		FRegistryItems: TStrings;
		FOldInstance: Integer;
		FNetInstances: Integer;
		FLocalInstances: Integer;

		FOnIdle: TIdleEvent;
		FOnHint: TNotifyEvent;
		FOnHelp: THelpEvent;
		FOnRestore: TNotifyEvent;
		FOnMinimize: TNotifyEvent;
		FOnMaximize: TNotifyEvent;
		FOnActivate: TNotifyEvent;
		FOnShowHint: TShowHintEvent;
		FOnDeactivate: TNotifyEvent;
		FInitEnvironment: TKInitEnvEvent;

		FRegistryRoot: string;
		FAutoRegistry: Boolean;
		FRootKey: TKRegistryKey;
		FOnReadRegistry: TNotifyEvent;
		FOnWriteRegistry: TNotifyEvent;

		FWaitTimer: TTimer;
		msMailSlot: TKMailSlot;

		procedure GotoPreviousInstance;
		function GetNetInstances: Integer;
		function GetNetTimeOut: Cardinal;
		function GetLocalInstances: Integer;
		function InstanceCheck: Boolean;
		procedure SetNetTimeOut( Value: Cardinal );
		procedure EndSplashWait( Sender: TObject );

		procedure SetTitle( const Value: string );
		procedure SetHintColor( Value: TColor );
		procedure SetHintPause( Value: Integer );
		procedure SetBuiltInName( const Value: string );
		procedure SetRootDir( const Value: string );
		procedure SetShowMainForm( Value: Boolean );
		procedure SetHintHidePause( Value: Integer );
		procedure SetShowOnTaskbar( Value: Boolean );
		procedure SetHintShortPause( Value: Integer );
		procedure SetRegistryItems( Value: TStrings );
		procedure SetRegistryRoot( const Value: string );

		procedure ReadRegistry;
		procedure WriteRegistry;
		
	protected
		procedure Loaded; override;
		function AppMessages( var Message: TMessage ): Boolean; dynamic;
		procedure MailReceiveControl( Sender: TKMailSlot; Signature: TDateTime;
			UserInfo: TKMSUserInfo; Control_01, Control_02: LongInt ); dynamic;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure DoHint( Sender: TObject ); dynamic;
		procedure DoMaximize( Sender: TObject ); dynamic;
		procedure DoRestore( Sender: TObject ); dynamic;
		procedure DoMinimize( Sender: TObject ); dynamic;
		procedure DoActivate( Sender: TObject ); dynamic;
		procedure DoDeactivate( Sender: TObject ); dynamic;
		procedure DoIdle( Sender: TObject; var Done: Boolean ); dynamic;
		function DoHelp( Command: Word; Data: Longint; var CallHelp: Boolean ): Boolean; dynamic;
		procedure DoShowHint( var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo ); dynamic;

		procedure DoReadRegistry; dynamic;
		procedure DoWriteRegistry; dynamic;
		procedure AddDefaultRegistryItems; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure CheckInstancesAllowed;
		procedure ShowSplashScreen( SplashClass: TFormClass );
		property NetInstances: Integer
						 read GetNetInstances;
		property LocalInstances: Integer
						 read GetLocalInstances;

	published
		property AutoRegistry: Boolean
						 read FAutoRegistry write FAutoRegistry default True;
		property BuiltInName: string
						 read FBuiltInName write SetBuiltInName;
		property HintColor: TColor
						 read FHintColor write SetHintColor stored true;
		property HintHidePause: Integer
						 read FHintHidePause write SetHintHidePause stored true;
		property HintPause: Integer
						 read FHintPause write SetHintPause stored true;
		property HintShortPause: Integer
						 read FHintShortPause write SetHintShortPause stored true;
		property InstancesAllowed: Integer
						 read FInstancesAllowed write FInstancesAllowed;
		property InternalID: Cardinal
						 read FInternalID write FInternalID;
		property NetworkSearch: Boolean
						 read FNetworkSearch write FNetworkSearch default true;
		property NetTimeout: Cardinal
						 read GetNetTimeOut write SetNetTimeOut default 500;
		property RootKey: TKRegistryKey
						 read FRootKey write FRootKey default rkCurrentUser;
		property RegistryRoot: string
						 read FRegistryRoot write SetRegistryRoot;
		property RegistryItems: TStrings
						 read FRegistryItems write SetRegistryItems;
		property RootDir: string
						 read FRootDir write SetRootDir;
		property ShowOnTaskbar: Boolean
						 read FShowOnTaskbar write SetShowOnTaskbar default true;
		property ShowMainForm: Boolean
						 read FShowMainForm write SetShowMainForm;
		property SplashWait: Integer
						 read FSplashWait write FSplashWait;
		property Terminated: Boolean
						 read FTerminated;
		property Title: string
						 read FTitle write SetTitle stored true;

		property OnActivate: TNotifyEvent
						 read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent
						 read FOnDeactivate write FOnDeactivate;
		property OnHint: TNotifyEvent
						 read FOnHint write FOnHint;
		property OnIdle: TIdleEvent
						 read FOnIdle write FOnIdle;
		property OnShowHint: TShowHintEvent
						 read FOnShowHint write FOnShowHint;
		property OnHelp: THelpEvent
						 read FOnHelp	write FOnHelp;
		property OnMaximize: TNotifyEvent
						 read FOnMaximize	write FOnMaximize;
		property OnMinimize: TNotifyEvent
						 read FOnMinimize	write FOnMinimize;
		property OnRestore: TNotifyEvent
						 read FOnRestore write FOnRestore;
		property OnReadRegistry: TNotifyEvent
						 read FOnReadRegistry write FOnReadRegistry;
		property OnWriteRegistry: TNotifyEvent
						 read FOnWriteRegistry write FOnWriteRegistry;
{
		property OnInitEnvironment: TKInitEnvEvent
						 read FInitEnvironment write FInitEnvironment;
}
	end;

{
--------------------------------------------------------------------------------
----------------------------- TKPerformanceObjects -----------------------------
--------------------------------------------------------------------------------
}

	EKW95Performance = class( EKWClasses );
	
	TKW95PerformanceItem = class;
	TKW95Performance = class;
	TKPerformanceObjects = class;

{ TKW95PerformanceItem }

	TKStatOption = ( soStopInfo, soStartInfo, soGetInfo );

	TKStatEvent = procedure ( Sender: TObject; Item: TKW95PerformanceItem;
		StatOption: TKStatOption; Differentiate: Boolean ) of object;

	TKW95PerformanceItem = class( TKCustomCollectionItem )
	private
		FEnabled: Boolean;
		FData: Integer;
		FStatOption: TKStatOption;
		FObjectName: string;
		FCounterName: string;
		FCounterInfos: TStrings;
		FStatEvent: TKStatEvent;

		procedure SetEnabled( Value: Boolean );
    procedure SetObjectName( const Value: string );
    procedure SetCounterName( const Value: string );

		function GetCounterInf: string;
		function GetInternalCounterName: string;
		function GetCounterDescription: string;
		function GetCounterDifferentiate: Boolean;
    procedure SetCounterInf( const Value: string );
    procedure SetCounterDescription( const Value: string );
    procedure SetCounterDifferentiate( Value: Boolean );

		function GetOwnerCollection: TKW95Performance;

		procedure ClearCounter;
    procedure SetCounterInfo( const Value: string );
    procedure SetObjectInfo( const Value: string );

		procedure SetData( Value: Integer; StatOption: TKStatOption );

	public
		constructor Create( ACollection: TCollection ); override;
    destructor Destroy; override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

    property AddInfo: TStrings
             read FCounterInfos;
    property Data: Integer
             read FData;
		property InternalCounterName: string
             read GetInternalCounterName;
		property Owner: TKW95Performance
						 read GetOwnerCollection;
    property StatOption: TKStatOption
             read FStatOption;
             
	published
		property Name;

		property Enabled: Boolean
						 read FEnabled write SetEnabled default true;

    property ObjectName: string
             read FObjectName write SetObjectName;
    property CounterName: string
						 read FCounterName write SetCounterName;
		property CounterInfo: string
						 read GetCounterInf write SetCounterInf stored False;
    property CounterDescription: string
						 read GetCounterDescription write SetCounterDescription stored False;
    property CounterDifferentiate: Boolean
             read GetCounterDifferentiate write SetCounterDifferentiate stored False;

		property OnStatEvent: TKStatEvent
             read FStatEvent write FStatEvent;

	end;

{ TKW95Performance }

	TKW95Performance = class( TKCustomCollection )
	private
		{ User defined private fields/methods }

		{ Default overrideen protected methods as private }
		function GetOwnerComp: TKPerformanceObjects;

		procedure SetItem( Index: Integer; AItem: TKW95PerformanceItem );
		function GetItem( Index: Integer ): TKW95PerformanceItem;

		function GetItemByName( const AName: string ): TKW95PerformanceItem;

	protected
		procedure Update( Item: TCollectionItem ); override;

	public
		constructor Create( AComp: TKPerformanceObjects ); virtual;

		function Add: TKW95PerformanceItem; virtual;
		property Items[Index: Integer]: TKW95PerformanceItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKW95PerformanceItem
						 read GetItemByName;
		property Names;
    
    property Component: TKPerformanceObjects
						 read GetOwnerComp;

	end;

{ TKPerformanceObjects }

	TKPerformanceObjects = class( TComponent )
	private
		FTimer: TTimer;
		FRegistry: TRegistry;
		FCollection: TKW95Performance;

		procedure UpdateItems;
		procedure FinishStat;
		procedure SetCollection( Value: TKW95Performance );

		procedure SetInterval( Value: Cardinal );
		function GetInterval: Cardinal;
		procedure SetEnabled( Value: Boolean );
		function GetEnabled: Boolean;

	protected
		procedure UpdateItem( Item: TKW95PerformanceItem ); virtual;

    procedure TimerEvent( Sender: TObject ); dynamic;

		property Timer: TTimer
             read FTimer;
    property Registry: TRegistry
						 read FRegistry;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

	published
		property Collection: TKW95Performance
						 read FCollection write SetCollection;
		property Enabled: Boolean
						 read GetEnabled write SetEnabled default false;
		property Interval: Cardinal
						 read GetInterval write SetInterval default SECOND_TO_MSECOND;

	end;

{
--------------------------------------------------------------------------------
---------------------- Memory Mapped Files Implementation ----------------------
--------------------------------------------------------------------------------
}

	EKFileMap = class( EKIPC );

{ TKCustomFileMap }

	TKCustomFileMap = class( TComponent )
	private
		FData: PChar;
		FEvent: THandle;
		FMutex: THandle;
		FHandle: THandle;
		FThread: THandle;
		FSize: LongInt;
		FFileMapName: string;

		FOnChange: TNotifyEvent;
		FAfterOpen: TNotifyEvent;
		FAfterClose: TNotifyEvent;
		FBeforeOpen: TNotifyEvent;
		FBeforeClose: TNotifyEvent;

		function  GetData: PChar;
		function  HasHandle: Boolean;
		procedure SetSize( Size: LongInt );
		procedure SetFileMapName( const MapName: string );
		function InternalThreadCallback: Integer;
		procedure ClearHandles;

	protected
		procedure SetValue( Value: PChar; CountBytes: Integer );
		procedure DoClose; virtual;

		property Data: PChar
						 read GetData;
		property Size: LongInt
						 read FSize write SetSize default 128;
		property FileMapName: string
						 read FFileMapName write SetFileMapName;

		property AfterClose: TNotifyEvent
						 read FAfterClose write FAfterClose;
		property AfterOpen: TNotifyEvent
						 read FAfterOpen write FAfterOpen;
		property BeforeClose: TNotifyEvent
						 read FBeforeClose write FBeforeClose;
		property BeforeOpen: TNotifyEvent
						 read FBeforeOpen write FBeforeOpen;
		property OnChange: TNotifyEvent
						 read FOnChange write FOnChange;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Lock;
		procedure Close;
		procedure UnLock;
		function IsOpen: Boolean;
		function Open: Boolean; virtual;

	end;

{ TKFileMap }

	TKFileMap = class( TKCustomFileMap )
	public
		property Data;

	published
		property Size;
		property FileMapName;

		property AfterClose;
		property AfterOpen;
		property BeforeClose;
		property BeforeOpen;

	end;
	
{
--------------------------------------------------------------------------------
--------------------- Directory Monitoring Implementation ----------------------
--------------------------------------------------------------------------------
}

{ TKCustomMonitor }

	EKMonitor = class( EKWinAPI );

	TKMonitorValue = ( mvFileName, mvDirName, mvAttributes, mvSize, mvLastWrite,
		mvSecurity );

	TKMonitorType = set of TKMonitorValue;

	PKMonitorParams = ^TKMonitorParams;
	TKMonitorParams = record
		hMutex: THandle;
		WindowHandle: HWND;
		NotifyFilter: DWORD;
		Directory: array [0..255] of Char;
	end;

{ TKCustomMonitor }

	TKCustomMonitor = class( TComponent )
	private
		FThreadID: DWORD;
		FDirectory: string;
		FParamPtr: Pointer;
		FWindowHandle: HWND;
		FNotifyFilter: DWORD;
		FMutexHandle: THandle;
		FThreadHandle: THandle;

		FOnStop: TNotifyEvent;
		FOnStart: TNotifyEvent;
		FDirChanged: TNotifyEvent;

		procedure WndProc( var Message: TMessage );

	protected
		procedure DoStopEvent; virtual;
		procedure DoStartEvent; virtual;
		procedure DoDirChangedEvent; virtual;
		function  GetNotifyFilter : TKMonitorType;
		procedure SetDirectory( const NewValue : string );
		procedure SetNotifyFilter( NewValue : TKMonitorType );

		property Directory: string
						 read FDirectory write SetDirectory;
		property Handle: HWND
						 read FWindowHandle;
		property NotifyFilter: TKMonitorType
						 read GetNotifyFilter write SetNotifyFilter;

		property OnDirChanged: TNotifyEvent
						 read FDirChanged write FDirChanged;
		property OnStart: TNotifyEvent
						 read FOnStart write FOnStart;
		property OnStop: TNotifyEvent
						 read FOnStop write FOnStop;

	public
		destructor  Destroy; override;
		constructor Create( AOwner : TComponent ); override;

		procedure Stop;
		procedure Start;

	end;

{ TKMonitor }

	TKMonitor = class( TKCustomMonitor )
	published

		property Directory;
		property NotifyFilter;

		property OnDirChanged;
		property OnStart;
		property OnStop;

	end;

{
--------------------------------------------------------------------------------
---------------------- Shell API Utilities Implementation ----------------------
--------------------------------------------------------------------------------
}

const

	DEF_FO_FLAGS = [fofAllowUndo, fofNoConfirmation, fofNoConfirmMkDir,
		fofNoErrorUI, fofRenameOnCollision];
	DEF_FA_FLAGS = [sfaReadOnly, sfaHidden, sfaSystem, sfaDirectory,
		sfaArchive, sfaNormal, sfaTemporary, sfaCompressed];
	DEF_FI_FLAGS = [fifAttributes, fifDisplayName, fifSmallIcon,
		fifSysIconIndex, fifTypeName, fifUseFileAttributes];

type

	EKShellAPI = class( EKWinAPI );
	EKFileOperation = class( EKShellAPI );
	EKFileInformation = class( EKShellAPI );

{ TKCustomFileManager }

	TKCustomFileManager = class( TComponent )
	private
		FHandle: HWND;
		FSource: TStrings;
		FProgressTitle: string;
		FDestination: TStrings;
		FNameMappings: TStrings;
		FNameMappingsPtr: Pointer;
		FOperation: TShellFileOperation;
		FAborted: Boolean;
		FFileOperationFlags: TShellFileOperationFlags;

		function GetNameMappings: TStrings;
		procedure SetSource( Value: TStrings );
		procedure SetDestination( Value: TStrings );

	protected
		procedure FreeNameMappings;

		property AnyOperationsAborted: Boolean
						 read FAborted;
		property NameMappings: TStrings
						 read GetNameMappings;

		property Destination: TStrings
						 read FDestination write SetDestination;
		property FileOpFlags: TShellFileOperationFlags
						 read FFileOperationFlags write FFileOperationFlags default DEF_FO_FLAGS;
		property Operation: TShellFileOperation
						 read FOperation write FOperation default sfoCopy;
		property ProgressTitle: string
						 read FProgressTitle write FProgressTitle;
		property Source: TStrings
						 read FSource write SetSource;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		constructor CreateFromHandle( AOwner: TComponent; AHandle: HWND ); virtual;

		function Execute: Boolean; virtual;

	end;

{ TKFileManager }

	TKFileManager = class( TKCustomFileManager )
	public
		property AnyOperationsAborted;
		property NameMappings;

	published
		property Destination;
		property FileOpFlags;
		property Operation;
		property ProgressTitle;
		property Source;

	end;

{ TKCustomFileInfo }

	TKCustomFileInfo = class( TComponent )
	private
		FIcon: TIcon;
		FExecuted: Boolean;
		FFileName: string;
		FLastExecutedFileName: string;
		FIconIndex: Integer;
		FDisplayName: string;
		FIconLocation: string;
		FSysImageList: TImageList;
		FFileInfoFlags: TShellFileInfoFlags;
		FFileAttributes: TShellFileAttributes;

		function GetExecuted: Boolean;
		procedure ClearInfo;
		procedure MakeIcon( hi: HICON );
		procedure MakeImageList( dwImageList: DWORD );
		procedure MakeIconFromImageList( Index: Integer );

  protected
		property Executed: Boolean
						 read GetExecuted;
		property Icon: TIcon
						 read FIcon;
		property IconIndex: Integer
						 read FIconIndex;
		property DisplayName: string
						 read FDisplayName;
		property IconLocation: string
						 read FIconLocation;
		property SysImageList: TImageList
						 read FSysImageList;
		property LastExecutedFileName: string
						 read FLastExecutedFileName;
		property FileAttributes: TShellFileAttributes
						 read FFileAttributes write FFileAttributes default DEF_FA_FLAGS;
		property FileName: string
						 read FFileName write FFileName;
		property FileInfoFlags: TShellFileInfoFlags
						 read FFileInfoFlags write FFileInfoFlags default DEF_FI_FLAGS;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function Execute: Boolean; virtual;

	end;

{ TKFileInfo }

	TKFileInfo = class( TKCustomFileInfo )
	public
		property Icon;
		property IconIndex;
		property DisplayName;
		property IconLocation;
		property SysImageList;
		property Executed;
		property LastExecutedFileName;
		
	published
		property FileAttributes;
		property FileName;
		property FileInfoFlags;

	end;

{
--------------------------------------------------------------------------------
---------------------------- Windows NT Services API ---------------------------
--------------------------------------------------------------------------------
}

const
	maAllAccess = [maConnect..maStandardRightsReq];

	stServiceAll = [stKernelDriver..stInteractiveProcess];
	stServiceWin32 = [stWin32OwnProcess, stWin32ShareProcess];
	stServiceDriver = [stKernelDriver, stFileSystemDriver, stRecognizerDriver];

type

	EKServiceManager = class( EKWClasses );

	{ TKServiceDependency }

	TKServiceDependency = class( TCollectionItem )
	private
		FName: string;
		FIsGroup: Boolean;

	protected
		function GetDisplayName: string; override;

	published
		property Name: string
						 read FName write FName;
		property IsGroup: Boolean
						 read FIsGroup write FIsGroup default False;

	end;

	{ TKServiceDependencies }

	TKServiceDependencies = class( TCollection )
	private
		FOwner: TPersistent;
		function GetItem( Index: Integer ): TKServiceDependency;
		procedure SetItem( Index: Integer; Value: TKServiceDependency );

	protected
		function GetOwner: TPersistent; override;

		property Owner: TPersistent
						 read FOwner;
						 
	public
		constructor Create( AOwner: TPersistent );

		property Items[Index: Integer]: TKServiceDependency
						 read GetItem write SetItem; default;

	end;

{ TKEnumSvcStrings }

	EKEnumSvcStrngs = class( EKServiceManager );

	PEnumServiceStatusArray = ^TEnumServiceStatusArray;
	TEnumServiceStatusArray = array[0..( MaxListSize div SizeOf( TEnumServiceStatus ) )] of TEnumServiceStatus;

	TKEnumSvcStrings = class( TStringList )
	private
		FCanAccessObject: Boolean;
		
		function GetServiceStatus( Index: Integer ): TServiceStatus;
		function GetServiceTypes( Index: Integer ): TKServiceTypes;
		function GetServiceState( Index: Integer ): TKServiceState;
		function GetServiceControlsAccepted( Index: Integer ): TKControlsAccepted;
		function GetStatusWaitHint( Index: Integer ): DWORD;
		function GetStatusCheckPoint( Index: Integer ): DWORD;
		function GetStatusWin32ExitCode( Index: Integer ): DWORD;
		function GetStatusSpecificExitCode( Index: Integer ): DWORD;

	protected
		function GetObject( Index: Integer ): TObject; override;
		procedure PutObject( Index: Integer; AObject: TObject ); override;

		procedure SvcStatusDelete( Index: Integer );
		procedure FillStrings( pessa: PEnumServiceStatusArray; ACount: Cardinal ); virtual;

		property CanAccessObject: Boolean
						 read FCanAccessObject;

	public
		destructor Destroy; override;

		procedure Clear; override;
		procedure Delete( Index: Integer ); override;
		function AddService( EnumSvcStatus: TEnumServiceStatus ): Integer; virtual;
		function AddServiceEx( const SvcName, SvcDispName: string;
		  SvcStat: TServiceStatus ): Integer; virtual;

		procedure Exchange( Index1, Index2: Integer ); override;
		procedure AddStrings( Strings: TStrings ); override;

		property ServiceStatus[Index: Integer]: TServiceStatus
						 read GetServiceStatus;
		property ServiceTypes[Index: Integer]: TKServiceTypes
						 read GetServiceTypes;
		property ServiceState[Index: Integer]: TKServiceState
						 read GetServiceState;
		property ServiceControlsAccepted[Index: Integer]: TKControlsAccepted
						 read GetServiceControlsAccepted;
		property StatusWaitHint[Index: Integer]: DWORD
						 read GetStatusWaitHint;
		property StatusCheckPoint[Index: Integer]: DWORD
						 read GetStatusCheckPoint;
		property StatusWin32ExitCode[Index: Integer]: DWORD
						 read GetStatusWin32ExitCode;
		property StatusSpecificExitCode[Index: Integer]: DWORD
						 read GetStatusSpecificExitCode;

	end;

{ TKCustomWin32ServiceManager }

	TKEnumServiceStatus = ( esActive, esInactive, esAny );

	TKSvcUpdateInfo = packed record
		SvcTypes: DWORD;
		SvcCtrls: DWORD;
		SvcTagID: DWORD;
		SvcStart: TKServiceStart;
		SvcError: TKServiceError;
		SvcState: TKServiceState;
	end;

	TKCustomWin32ServiceManager = class;

	TKRemoteWin32Error = procedure ( SvcManager: TKCustomWin32ServiceManager;
		ErrorCode: Cardinal; var Handled: Boolean ) of object;

	TKCustomWin32ServiceManager = class( TComponent )
	private
		FTagID: DWORD;
		FServiceName: string;
		FLoadGroup: string;
		FPassword: string;
		FMachineName: string;
		FDatabaseName: string;
		FServiceBinPath: string;
		FServiceStartName: string;
		FServiceDisplayName: string;
		FAutoRefresh: Boolean;
		FDependenciesStop: Boolean;
		FDependencies: TKServiceDependencies;

		FServiceStatus: TServiceStatus;
		FStatusWaitHint: DWORD;
		FStatusCheckPoint: DWORD;
		FStatusWin32ExitCode: DWORD;
		FStatusSpecificExitCode: DWORD;

		FServiceTypes: TKServiceTypes;
		FServiceError: TKServiceError;
		FServiceStart: TKServiceStart;
		FServiceState: TKServiceState;
		FServiceAccess: TKServiceAccessMask;
		FServiceControlsAccepted: TKControlsAccepted;

		FOnWin32Error: TKRemoteWin32Error;

		function GetServiceDependencies( const Delimiter: Char ): string;
		procedure SetServiceDependencies( const Delimiter: Char; const Value: string );
		procedure SetDependencies( Value: TKServiceDependencies );

	protected
		procedure RefreshServiceStatus; virtual;
		procedure ClearServiceStatus; virtual;
		procedure SetServiceName( const Value: string ); virtual;

		function pcServiceName: PChar; dynamic;
		function pcMachineName: PChar; dynamic;
		function pcDatabaseName: PChar; dynamic;
		function pcServiceBinPath: PChar; dynamic;
		function pcServiceDisplayName: PChar; dynamic;
		function GetServiceAccessFromCtrl( fdwControl: DWORD ): DWORD; dynamic;
		function OpenService( dwAccess: DWORD ): Boolean; virtual;
		function GetEnumSvcStrings: TKEnumSvcStrings; virtual;
		procedure RaiseLastError; virtual;
		function DoWin32Error( ErrorCode: Cardinal ): Boolean; dynamic;


		function GetServiceHandle( dwAccess: DWORD ): Boolean; virtual; abstract;
		function InstallService: Boolean; virtual; abstract;
		function DeleteService: Boolean; virtual; abstract;
		function QueryServiceStatus: Boolean; virtual; abstract;
		function UpdateServiceConfig: Boolean; virtual; abstract;
		function ControlService( fdwControl: DWORD ): Boolean; virtual; abstract;
		function CloseService: Boolean; virtual; abstract;
		function StartService( Params: TStrings ): Boolean; virtual; abstract;
		function QueryServiceConfig: Boolean; virtual; abstract;
		function EnumServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ):
			Boolean; virtual; abstract;
		function EnumDependentServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ):
			Boolean; virtual; abstract;

		property ServiceStatusInfo: TServiceStatus
						 read FServiceStatus;
		property ServiceState: TKServiceState
						 read FServiceState write FServiceState;
		property StatusCheckPoint: DWORD
						 read FStatusCheckPoint;
		property StatusWaitHint: DWORD
						 read FStatusWaitHint;
		property StatusWin32ExitCode: DWORD
						 read FStatusWin32ExitCode;
		property StatusSpecificExitCode: DWORD
						 read FStatusSpecificExitCode;
		property DependenciesAsString[const Delimiter: Char]: string
						 read GetServiceDependencies write SetServiceDependencies;
		property Dependencies: TKServiceDependencies
						 read FDependencies write SetDependencies;
		property DatabaseName: string
						 read FDatabaseName write FDatabaseName;
		property LoadGroup: string
						 read FLoadGroup write FLoadGroup;
		property MachineName: string
						 read FMachineName write FMachineName;
		property ServiceBinPath: string
						 read FServiceBinPath write FServiceBinPath;
		property ServiceDisplayName: string
						 read FServiceDisplayName write FServiceDisplayName;
		property ServiceName: string
						 read FServiceName write SetServiceName;
		property ServiceStartName: string
						 read FServiceStartName write FServiceStartName;
		property PassWord: string  
						 read FPassWord write FPassWord;
		property ServiceAccess: TKServiceAccessMask
						 read FServiceAccess write FServiceAccess default [];
		property ServiceControlsAccepted: TKControlsAccepted
						 read FServiceControlsAccepted write FServiceControlsAccepted default [];
		property ServiceError: TKServiceError
						 read FServiceError write FServiceError default seNormal;
		property ServiceStart: TKServiceStart
						 read FServiceStart write FServiceStart default ssAuto;
		property ServiceTypes: TKServiceTypes
						 read FServiceTypes write FServiceTypes default stServiceWin32;
		property TagID: DWORD
						 read FTagID write FTagID default 0;
		property AutoRefresh: Boolean
						 read FAutoRefresh write FAutoRefresh default True;
		property DependenciesStop: Boolean
						 read FDependenciesStop write FDependenciesStop default True;
		property OnWin32Error: TKRemoteWin32Error
    				 read FOnWin32Error write FOnWin32Error;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function GetServiceInfoSize: Integer; virtual;
		procedure QueryToServiceInfo( p: Pointer ); virtual;
		function UpdateFromServiceInfo( p: Pointer; szData: Integer ): Boolean; virtual;

		function StopService: Boolean; virtual;
		function PauseService: Boolean; virtual;
		function ContinueService: Boolean; virtual;

		function AllServices( Strings: TStrings ): Boolean; virtual;
		function ActiveServices( Strings: TStrings ): Boolean; virtual;
		function InactiveServices( Strings: TStrings ): Boolean; virtual;

		function AllDependentServices( Strings: TStrings ): Boolean; virtual;
		function ActiveDependentServices( Strings: TStrings ): Boolean; virtual;
		function InactiveDependentServices( Strings: TStrings ): Boolean; virtual;

		function AllServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;
		function ActiveServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;
		function InactiveServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;

		function AllDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;
		function ActiveDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;
		function InactiveDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean; virtual;

	end;

{ TKWin32ServiceManager }

  PKRSMWin32ErrorInfo = ^TKRSMWin32ErrorInfo;
	TKRSMWin32ErrorInfo = record
		ErrorMessage: string;
		case Raised: Boolean of
			False: ( ErrorCode: Cardinal );
			True : ( ErrorClass: PChar );
	end;

	TKRSMWin32Error = record
		Msg: Cardinal;		
		ServiceName: string;
		ErrorInfo: PKRSMWin32ErrorInfo;
		Result: Cardinal;
	end;

	TKRSMQueryDpdStr = record
		Msg: Cardinal;
		ServiceName: string;
		Unused: Cardinal;
		Result: TKEnumSvcStrings;
	end;

	TKWin32ServiceManager = class( TKCustomWin32ServiceManager )
	private
		FManagerLock: SC_LOCK;
		FManagerHandle: SC_HANDLE;
		FServiceHandle: SC_HANDLE;

		FManagerAccess: TKManagerAccessMask;
		FLockOwner: string;
		FLockDuration: Integer;
		FManagerLocked: Boolean;

		FRemoteServerSvcManager: THandle; { for linking otimization }

	protected
		function GetServiceHandle( dwAccess: DWORD ): Boolean; override;
		function CloseService: Boolean; override;
		procedure RaiseLastError; override;
		function GetEnumSvcStrings: TKEnumSvcStrings; override;
		function GetManagerHandle: Boolean; virtual;
		function OpenManager: Boolean; virtual;
		function LockDatabase: Boolean; virtual;
		function UnlockDatabase: Boolean; virtual;

		procedure SetRemoteServerSvcManager( Value: THandle );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function InstallService: Boolean; override;
		function DeleteService: Boolean; override;
		function QueryServiceStatus: Boolean; override;
		function UpdateServiceConfig: Boolean; override;
		function ControlService( fdwControl: DWORD ): Boolean; override;
		function StartService( Params: TStrings ): Boolean; override;
		function QueryServiceConfig: Boolean; override;
		function EnumServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ):
			Boolean; override;
		function EnumDependentServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ):
			Boolean; override;

		function QueryServiceLockStatus: Boolean; virtual;	

		property ServiceStatusInfo;
		property ServiceState;
		property StatusCheckPoint;
		property StatusWaitHint;
		property StatusWin32ExitCode;
		property StatusSpecificExitCode;
		property DependenciesAsString;

		property ManagerHandle: SC_HANDLE
						 read FManagerHandle;
		property ServiceHandle: SC_HANDLE
						 read FServiceHandle;
		property ManagerLocked: Boolean
						 read FManagerLocked;
		property LockOwner: string
						 read FLockOwner;
		property LockDuration: Integer
						 read FLockDuration;
		property RemoteServerSvcManager: THandle
						 read FRemoteServerSvcManager;

	published
		property AutoRefresh;
		property DependenciesStop;
		property Dependencies;
		property DatabaseName;
		property LoadGroup;
		property MachineName;
		property ServiceBinPath;
		property ServiceDisplayName;
		property ServiceStartName;
		property ServiceName;
		property ServiceAccess;
		property ServiceControlsAccepted;
		property ServiceError;
		property ServiceStart;
		property ServiceTypes;
		property PassWord;
		property TagID;
		property OnWin32Error;

		property ManagerAccess: TKManagerAccessMask
						 read FManagerAccess write FManagerAccess default maAllAccess;

	end;

const
	ENUMSERVICE_STATE: array[TKEnumServiceStatus] of DWORD =
	(
		SERVICE_ACTIVE,
		SERVICE_INACTIVE,
		SERVICE_ACTIVE or SERVICE_INACTIVE
	);

{
--------------------------------------------------------------------------------
------------------- Generic WinAPI Utilities Implementation --------------------
--------------------------------------------------------------------------------
}

type

	EKTrayIcon = class( EKWinAPI );

{ TKCustomTrayIcon }

	TKCustomTrayIcon = class;

	TKTrayAnimateEvent = procedure ( Sender: TKCustomTrayIcon; Icon: TIcon;
		Index, Count: Integer ) of object;

	TKCustomTrayIcon = class( TComponent )
	private
		FActive: Boolean;
		FAlwaysAnimate: Boolean;
		FShowAppTaskbar: Boolean;
		FAutoRestore: Boolean;
		FInterval: Cardinal;
		FImgCount : Integer;
		FImgIndex : Integer;
		FIcon: TIcon;
		FResIcon: TIcon;
		FHint: string;
		FDocked: Boolean;
		FLBtnDown: Boolean;
		FData: TNotifyIconData;
		FImages: TImageList;
		FPopupMenu: TPopupMenu;
		FOnAnimate: TKTrayAnimateEvent;

		FOnClick: TNotifyEvent;
		FOnDblClick: TNotifyEvent;

		procedure SetPopupMenu( Value: TPopUpMenu );
		procedure SetImageList( Value: TImageList );
		procedure SetInterval( Value: Cardinal );
		procedure SetOnAnimate( Value: TKTrayAnimateEvent );
		procedure SetActive( Value: Boolean );
		procedure SetShowAppTaskBar( Value: Boolean );

		procedure ResyncAppTaskBar;
		function AppWndProc( var Message: TMessage ): Boolean;

	protected
		procedure Click; dynamic;
		procedure DoPopup; dynamic;
		procedure Changed; dynamic;
		procedure DblClick; dynamic;
		procedure Animate; dynamic;
		procedure SetIcon( AIcon: TIcon ); virtual;
		procedure SetHint( const Value: string ); virtual;
		procedure WndProc( var Message: TMessage ); virtual;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
		procedure DoAnimate( Icon: TIcon ); dynamic;

		procedure RefreshTimer; dynamic;

		property Data: TNotifyIconData
						 read FData;

		property Docked: Boolean
						 read FDocked;

		property Hint: string
						 read FHint write SetHint;
		property Icon: TIcon
						 read FIcon write SetIcon;
		property PopupMenu: TPopupMenu
						 read FPopupMenu write SetPopupMenu;
    property Active: Boolean
             read FActive write SetActive default False;
		property AlwaysAnimate: Boolean
						 read FAlwaysAnimate write FAlwaysAnimate default False;
		property ShowAppTaskbar: Boolean
						 read FShowAppTaskbar write SetShowAppTaskbar default False;
		property AutoRestore: Boolean
					   read FAutoRestore write FAutoRestore default True;
		property AniImages: TImageList
						 read FImages write SetImageList;
		property Interval: Cardinal
						 read FInterval write SetInterval default INTERVAL_ANITRAY_DISABLED;

		property OnClick: TNotifyEvent
						 read FOnClick write FOnClick;
		property OnDblClick: TNotifyEvent
						 read FOnDblClick write FOnDblClick;
		property OnAnimate: TKTrayAnimateEvent
						 read FOnAnimate write SetOnAnimate;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Dock; virtual;
		procedure Remove; virtual;

	end;

{ TKTrayIcon }

	TKTrayIcon = class( TKCustomTrayIcon )
	public
		property Docked;

	published
		property Hint;
		property Icon;
		property PopupMenu;
		property Active;
		property AlwaysAnimate;
		property AutoRestore;
		property ShowAppTaskBar;
		property AniImages;
		property Interval;

		property OnClick;
		property OnDblClick;
		property OnAnimate;

	end;

{ TKCustomDragDrop }

	EKDragDrop = class( EKWinAPI );

	TKCustomDragDrop = class;

	TKDragDropGetHandle = procedure( Sender: TKCustomDragDrop;
		var Source: TObject; var Handle: Hwnd ) of object;
	TKDragDropGetWndProc = procedure( Sender: TKCustomDragDrop;
		var Source: TObject; var Method: TWndMethod ) of object;
	TKDragDropEvent = procedure( Sender: TKCustomDragDrop;
		Source: TObject; DropList: TStrings ) of object;
	TKDragDropPointEvent = procedure( Sender: TKCustomDragDrop;
		Source: TObject; DroppedIntoClientArea: Boolean; X, Y: Integer ) of object;

	TKCustomDragDrop = class( TComponent )
	private
		FHwnd: HWnd;
		FSource: TObject;
		FWndProc: TWndMethod;
		FLastCount: Integer;
		FDropList: TStrings;
		FOnGetHandle: TKDragDropGetHandle;
		FOnGetWndProc: TKDragDropGetWndProc;
		FOnDrop: TKDragDropEvent;
		FOnDropPoint: TKDragDropPointEvent;

		function GetHandle: Hwnd;
		procedure SetHandle( Value: Hwnd );
		procedure ClearDropList;

		procedure WMDropFiles( var Message: TWMDropFiles );
							message WM_DROPFILES;

	protected
		procedure CheckHandle; dynamic;
		procedure CheckWndProc; dynamic;

		procedure WndProc( var Message: TMessage ); dynamic;
		procedure HandleNeeded; dynamic;
		procedure DoGetHandle( var AHandle: HWnd ); dynamic;
		procedure DoGetWndProc; dynamic;
		procedure DoDrop; dynamic;
		procedure DoDropPoint( const Dropped: Boolean;
			DropPoint: TPoint ); dynamic;
		procedure SetWndProc; virtual;
		procedure RestoreWndProc; virtual;
		procedure ClearReferences;

		procedure Loaded; override;

		property Source: TObject
						 read FSource;
		property SavedWndProc: TWndMethod
						 read FWndProc;
		property LastDragCount: Integer
						 read FLastCount;
		property Handle: Hwnd
						 read GetHandle write SetHandle;

		property OnGetHandle: TKDragDropGetHandle
						 read FOnGetHandle write FOnGetHandle;
		property OnGetWndProc: TKDragDropGetWndProc
						 read FOnGetWndProc write FOnGetWndProc;
		property OnDrop: TKDragDropEvent
						 read FOnDrop write FOnDrop;
		property OnDropPoint: TKDragDropPointEvent
						 read FOnDropPoint write FOnDropPoint;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

	end;

{ TKDragDrop }

	TKDragDrop = class( TKCustomDragDrop )
	public
		property LastDragCount;
		property Handle;

	published
		property OnGetHandle;
		property OnGetWndProc;
		property OnDrop;
		property OnDropPoint;

	end;

{ TKBrowseFolderDialog }

  EKBrowseFolder = class( EKWClasses );

	TKBrowseFolderFlags = ( bffAll, bffComputers, bffPrinters,
		bffAncestors, bffFileSystem, bffCustom );

  TKBrowseFolderDialog = class;

	TKSHBrowseFolderEvent = procedure( Sender: TKBrowseFolderDialog; var SetFolder,
		SetStatus: string; var EnableOK: Boolean ) of object;

	TKBrowseFolderDialog = class( TComponent )
	private
		FTop: Integer;
		FLeft: Integer;
		FRoot: WideString;
		FTitle: PChar;
		FHandle: HWND;
		FCentered: Boolean;
		FFolderName: PChar;
		FGoBelowDomain: Boolean;
		FShowStatusText: Boolean;
		FStyle: TKBrowseFolderFlags;

		pbi: PBrowseInfo;
		FRootItemIDList: PItemIDList;

		FPosChanged: Boolean;

		FCallback: TFNBFFCallBack;

		FOnInitialize: TKSHBrowseFolderEvent;
		FOnSelChanged: TKSHBrowseFolderEvent;

		procedure HandleNeeded;

		function GetRoot: WideString;
		function GetTitle: string;
		function GetFolderName: string;
		function StyleToFlags( Value: TKBrowseFolderFlags ): UINT;

		procedure SetStyle( Value: TKBrowseFolderFlags );
		procedure SetGoBelowDomain( Value: Boolean );
		procedure SetShowStatusText( Value: Boolean );
		procedure SetRoot( const Value: WideString );
		procedure SetTitle( const Value: string );
		procedure SetFolderName( const Value: string );
		procedure UpdateBrowseFolderDialog( AHandle: HWND; pil: PItemIDList;
			const sFolder, sStatus: string; bEnableOK: Boolean );

	protected
		procedure WndProc( var Message: TMessage ); virtual;
		procedure DoSelChanged( AHandle: HWND; pil: PItemIDList ); virtual;
		procedure DoInitialized( AHandle: HWND; pil: PItemIDList ); virtual;

		property CallBack: TFNBFFCallBack
						 read FCallback;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function Execute: Boolean; virtual;

	published
		property Centered: Boolean
						 read FCentered write FCentered default true;
		property FolderName: string
						 read GetFolderName write SetFolderName;
		property GoBelowDomain: Boolean
						 read FGoBelowDomain write SetGoBelowDomain default true;
		property Left: Integer
						 read FLeft write FLeft default 60;
		property Root: WideString
						 read GetRoot write SetRoot;
		property ShowStatusText: Boolean
						 read FShowStatusText write SetShowStatusText default false;
		property Style: TKBrowseFolderFlags
						 read FStyle write SetStyle default bffAll;
		property Title: string
						 read GetTitle write SetTitle;
		property Top: Integer
						 read FTop write FTop default 40;

		property OnInitialize: TKSHBrowseFolderEvent
						 read FOnInitialize write FOnInitialize;
		property OnSelChanged: TKSHBrowseFolderEvent
						 read FOnSelChanged write FOnSelChanged;

	end;

{ TKPageSetUpDialog }

	EKPageSetUpDialog = class( EKWClasses );

	PKPageSetUpDialogInfo = ^TKPageSetUpDialogInfo;
	TKPageSetUpDialogInfo = record
		sPaperSize: Short;
		ptPaperType: TKPaperType;
		poPaperOrientation: TKPaperOrientation;
		prtPrinterType: TKPrinterType;
		lpPageSetUpDlg: PPageSetupDlg;
	end;

	TKPageSetupDialog = class;

	TKInitPaintPageEvent = procedure( Sender: TKPageSetupDialog; PPageSetUpInfo:
		PKPageSetUpDialogInfo; var Handled: Boolean ) of object;

	TKPaintPageEvent = procedure ( Sender: TKPageSetupDialog; PaintOption: TKPaintOption;
		Canvas: TCanvas; Rect: TRect; var Handled: Boolean ) of object;

	TKPageSetupDialog = class( TCommonDialog )
	private
		FTop: Integer;
		FLeft: Integer;
		FCentered: Boolean;
		FPosChanged: Boolean;
		FHandle: Hwnd;
		FPageSetupHook: Pointer;
		FDefWndProc: Pointer;
		FPrinterInfo: TKPrinterCanvasInfo;
		FOptions: TKPageSetupOptions;
		FPaperSize: TKPoint;
		FMinimumMargins: TKRect;
    FMargins: TKRect;
		FMeasurements: TKMeasurements;
    FMarginsMeasures: TKRectMeasures;
		FMinimumMarginsMeasures: TKRectMeasures;
		FPaperSizeMeasures: TKPointMeasures;
		FOnPageSetUp: TNotifyEvent;
		FOnInitPaintPage: TKInitPaintPageEvent;
		FOnPaintPage: TKPaintPageEvent;
		procedure InternalWndProc( var Message: TMessage );
		procedure SetMeasures( Sender: TObject; Index: TKScalingFlag; Value: Single );
		procedure GetMeasures( Sender: TObject; Index: TKScalingFlag; var Value: Single );

	protected
		function DoExecute( Func: Pointer ): Boolean; dynamic;
		function DoPrinter: Boolean; dynamic;
		function DoPaintPage( PaintOption: TKPaintOption; PaintCanvas: TCanvas;
		  PaintRect: TRect ): Boolean; dynamic;
		function DoInitPaint( PSInfo: PKPageSetUpDialogInfo ): Boolean; dynamic;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

		procedure DefaultHandler( var Message ); override;

		function Execute: boolean; override;

		property Handle: Hwnd
						 read FHandle;

	published
		property Centered: Boolean
						 read FCentered write FCentered default true;
    property Options: TKPageSetupOptions
						 read FOptions write FOptions default [poDefaultMinMargins, poShowHelp];
		property Measurements: TKMeasurements
						 read FMeasurements write FMeasurements default pmMillimeters;
    property PaperSize: TKPoint
						 read FPaperSize write FPaperSize;
		property MinimumMargins: TKRect
						 read FMinimumMargins write FMinimumMargins;
		property Margins: TKRect
						 read FMargins write FMargins;
    property MarginsMeasures: TKRectMeasures
             read FMarginsMeasures write FMarginsMeasures stored False;
		property MinimumMarginsMeasures: TKRectMeasures
             read FMinimumMarginsMeasures write FMinimumMarginsMeasures stored False;
    property PaperSizeMeasures : TKPointMeasures
             read FPaperSizeMeasures write FPaperSizeMeasures stored False;

    property DlgLeft: Integer
						 read FLeft write FLeft default 60;
		property DlgTop: Integer
						 read FTop write FTop default 40;

		property OnPageSetUp: TNotifyEvent
						 read FOnPageSetUp write FOnPageSetUp;
		property OnInitPaintPage: TKInitPaintPageEvent
						 read FOnInitPaintPage write FOnInitPaintPage;
		property OnPaintPage: TKPaintPageEvent
						 read FOnPaintPage write FOnPaintPage;

	end;

{ TKNetworkDialog }

	TKNetworkDialogType = ( ndtConnect, ndtDisconnect );
	TKNetworkResourceType = ( nrtDisk, nrtPrinter );

	TKNetworkDialog = class( TComponent )
	private
		FNetDialogType: TKNetworkDialogType;
		FNetResourceType: TKNetworkResourceType;
		FLastError: Integer;
		FOnExecute: TNotifyEvent;

		function GetErrorMessage: string;

	public
		function Execute: Boolean; dynamic;

		property ErrorMessage: string
						 read GetErrorMessage;

	published
		property NetDialogType: TKNetworkDialogType
						 read FNetDialogType write FNetDialogType default ndtConnect;
		property NetResourceType: TKNetworkResourceType
						 read FNetResourceType write FNetResourceType default nrtDisk;
		property OnExecute: TNotifyEvent
						 read FOnExecute write FOnExecute;

	end;

function SendMail( const Server, Slot: string; Buff: Pointer; Size: Integer ): Boolean;

implementation

uses
	Consts, Dlgs, ComObj, Printers, ExptIntf, uksyResStr, ukwResStr;

{
--------------------------------------------------------------------------------
--------------- Inter Process Comunications Basic API Mapping ------------------
--------------------------------------------------------------------------------
}

type
	TKThreadedTimerObjectHack = class( TKThreadedTimerObject );

{------------------------------- TKThreadedTimer -------------------------------}

constructor TKThreadedTimer.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FEnabled := False;
	FTimer := TKThreadedTimerObject.Create( True );
	FTimer.Interval := SECOND_TO_MSECOND;
	{ Just to set the new values }
	TKThreadedTimerObjectHack( FTimer ).UpdateTimer;
end;

destructor TKThreadedTimer.Destroy;
begin
	FTimer.Free;
	inherited Destroy;
end;

function TKThreadedTimer.GetTerminated: Boolean;
begin
	Result := FTimer.Thread.Terminated;
end;

function TKThreadedTimer.GetInterval: Cardinal;
begin
	Result := FTimer.Interval;
end;

function TKThreadedTimer.GetOnTimer: TNotifyEvent;
begin
	Result := FTimer.OnTimer;
end;

function TKThreadedTimer.GetPriority: TKThreadPriority;
begin
	Result := FTimer.Thread.Priority;
end;

function TKThreadedTimer.GetLockable: Boolean;
begin
	Result := FTimer.Lockable;
end;

procedure TKThreadedTimer.SetLockable( Value: Boolean );
begin
	FTimer.Lockable := Value;
end;

procedure TKThreadedTimer.SetEnabled( Value: Boolean );
begin
	if ( Value <> FEnabled ) then
	begin
		FEnabled := Value;
		if ( not Designing( Self ) ) then
			FTimer.Enabled := Value;
	end;
end;

procedure TKThreadedTimer.SetInterval( Value: Cardinal );
begin
	FTimer.Interval := Value;
end;

procedure TKThreadedTimer.SetOnTimer( Value: TNotifyEvent );
begin
	FTimer.OnTimer := Value;
end;

procedure TKThreadedTimer.SetPriority( Value: TKThreadPriority );
begin
	FTimer.Thread.Priority := Value;
end;

{ TKThreadedTimerItem }

constructor TKThreadedTimerItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKThreadedTimers );
	inherited Create( ACollection );
	ForceObject( Owner.Component );
	FTimer := TKThreadedTimer.Create( nil );
end;

destructor TKThreadedTimerItem.Destroy;
begin
	FTimer.Free;
	inherited Destroy;
end;

procedure TKThreadedTimerItem.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKThreadedTimerItem ) then
	begin
		Enabled  := ( Source as TKThreadedTimerItem ).Enabled;
		Interval := ( Source as TKThreadedTimerItem ).Interval;
		Priority := ( Source as TKThreadedTimerItem ).Priority;
		Lockable := ( Source as TKThreadedTimerItem ).Lockable;
		OnTimer  := ( Source as TKThreadedTimerItem ).OnTimer;
	end;
end;

function TKThreadedTimerItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( inherited Equals( Item ) and
		( Enabled  = ( Item as TKThreadedTimerItem ).Enabled ) and
		( Interval = ( Item as TKThreadedTimerItem ).Interval ) and
		( Priority = ( Item as TKThreadedTimerItem ).Priority ) and
		( Lockable = ( Item as TKThreadedTimerItem ).Lockable ) );
end;

procedure TKThreadedTimerItem.TimerEvent( Sender: TObject );
begin
	DoTimer;
end;

procedure TKThreadedTimerItem.DoTimer;
begin
	if Assigned( FOnTimer ) then
		FOnTimer( Self );
end;

function TKThreadedTimerItem.GetOwnerCollection: TKThreadedTimers;
begin
	Result := TKThreadedTimers( inherited GetOwnerCollection );
end;

function TKThreadedTimerItem.GetEnabled: Boolean;
begin
	Result := FTimer.Enabled;
end;

function TKThreadedTimerItem.GetInterval: Cardinal;
begin
	Result := FTimer.Interval;
end;

function TKThreadedTimerItem.GetLockable: Boolean;
begin
	Result := FTimer.Lockable;
end;

function TKThreadedTimerItem.GetTerminated: Boolean;
begin
	Result := FTimer.Terminated;
end;

function TKThreadedTimerItem.GetPriority: TKThreadPriority;
begin
	Result := FTimer.Priority;
end;

procedure TKThreadedTimerItem.SetEnabled( Value: Boolean );
begin
	FTimer.Enabled := Value;
end;

procedure TKThreadedTimerItem.SetInterval( Value: Cardinal );
begin
	FTimer.Interval := Value;
end;

procedure TKThreadedTimerItem.SetLockable( Value: Boolean );
begin
	FTimer.Lockable := Value;
end;

procedure TKThreadedTimerItem.SetOnTimer( Value: TKThreadedTimerItemTimerEvent );
begin
	FOnTimer := Value;
	if ( not Designing( Owner.Component ) ) then
		if Assigned( Value ) then
			FTimer.OnTimer := TimerEvent
		else
			FTimer.OnTimer := nil;
end;

procedure TKThreadedTimerItem.SetPriority( Value: TKThreadPriority );
begin
	FTimer.Priority := Value;
end;

{ TKThreadedTimers }

constructor TKThreadedTimers.Create( AComp: TKThreadedTimerPool );
begin
	ForceObject( AComp );
	inherited Create( AComp, TKThreadedTimerItem, False );
end;

function TKThreadedTimers.Add: TKThreadedTimerItem;
begin
	Result := TKThreadedTimerItem( inherited Add );
end;

function TKThreadedTimers.GetItem( Index: Integer ): TKThreadedTimerItem;
begin
	Result := TKThreadedTimerItem( inherited GetItem( Index ) );
end;

function TKThreadedTimers.GetItemByName( const AName: string ): TKThreadedTimerItem;
begin
	 Result := TKThreadedTimerItem( inherited GetItemByName( AName ) );
end;

function TKThreadedTimers.GetOwnerComp: TKThreadedTimerPool;
begin
	Result := TKThreadedTimerPool( inherited GetOwnerComp );
end;

procedure TKThreadedTimers.SetItem( Index: Integer; AItem: TKThreadedTimerItem );
begin
	inherited SetItem( Index, AItem );
end;

procedure TKThreadedTimers.Update( Item: TCollectionItem );
var
	i: Integer;
begin
	if CheckObject( Item ) then
		Component.UpdateItem( TKThreadedTimerItem( Item ) )
	else
		for i := 0 to Count - 1 do
			Component.UpdateItem( Items[i] );
end;

{ TKThreadedTimerPool }

constructor TKThreadedTimerPool.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTimers := TKThreadedTimers.Create( Self );
end;

destructor TKThreadedTimerPool.Destroy;
begin
	FreeClean( FTimers );
	inherited Destroy;
end;

procedure TKThreadedTimerPool.SetTimers( Value: TKThreadedTimers );
begin
	FTimers.Assign( Value );
end;

procedure TKThreadedTimerPool.UpdateItem( Item: TKThreadedTimerItem );
begin
	{ Default update item code }
end;

function TKThreadedTimerPool.ForEachTimerDo( EnumTimerFunc: TKEnumTimerFunc; Data: Pointer ): Boolean;
var
	i: Integer;
begin
	Result := ( CheckReference( EnumTimerFunc ) and CheckCollection( Timers ) );
	if Result then
		for i := 0 to Timers.Count - 1 do
{$BOOLEVAL ON}
			Result := Result and EnumTimerFunc( Self, Timers[i], i, Data );
{$BOOLEVAL OFF}
end;

{ TKWorkerThread }

constructor TKWorkerThread.CreateWorkerThread( AOwner: TKWorkerThreadComp );
begin
	ForceObject( AOwner );
	{
	  MUST BE TRUE! To do not generate an access violation in SynchronizeOwner (called
		before the parent inherited CreateThread constructor (that will also create the
		CrtSec). Either if we use a local variable in SynchronizeOwner this is necessary
		since we alter some flags that trust in this CrtSec object (eg. FreeOnTerminate)
	}
  with AOwner do                    
  	inherited CreateWorker( AOwner, ( ( not Designing( AOwner ) ) and CreateSuspended ),
      CreateMessageQueue );
end;

function TKWorkerThread.GetOwner: TKWorkerThreadComp;
begin
	Result := TKWorkerThreadComp( inherited GetOwner );
end;

function TKWorkerThread.CheckExecuteCondition: Boolean;
begin
  { USe the field to avoid field Get... }
  Result := ( ( inherited CheckExecuteCondition ) and ( not Owner.FStoppingAll ) ); 
end;

procedure TKWorkerThread.SynchronizeOwner;
begin
  inherited SynchronizeOwner; { MUST CALL FOR MESSAGE QUEUE SYNCH }
	{
    CrtSec.Lock; // There is no need for critical sections here! the thread wasn't yet created
	try
  }
		Priority := Owner.Priority;
    MsgFilterMin := Owner.MsgFilterMin;
    MsgFilterMax := Owner.MsgFilterMax;
    FreeOnTerminate := Owner.FreeThreadOnTerminate;
    if ( not Assigned( OnExecute ) ) then
  		OnExecute := Owner.ThreadExecuteEvent;          { Hard Couple! }
    if ( not Assigned( OnExecuteMsg ) ) then
      OnExecuteMsg := Owner.ThreadExecuteMsgEvent;    { Hard Couple! }
    if ( not Assigned( OnTerminate ) ) then
  		OnTerminate := Owner.ThreadTerminateEvent;      { Hard Couple! }
  {
	finally
		CrtSec.UnLock;
	end;
  }
end;

{ TKWorkerThreadComp }

constructor TKWorkerThreadComp.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FPriority := ktpNormal;
	FWaitForThread := True;
  FMsgFilterMin := 0;
  FMsgFilterMax := 0;
  FStoppingAll := False;
  FCreateSuspended := False;
  FCreateMessageQueue := False;
  FFreeThreadOnTerminate := False;
	FWaitForInterval := Cardinal( INFINITE );
end;

destructor TKWorkerThreadComp.Destroy;
begin
	Stop;
	inherited Destroy;
end;

procedure TKWorkerThreadComp.SetStoppingAll( Value: Boolean );
begin
  FStoppingAll := True;
end;

function TKWorkerThreadComp.GetThreadClass: TKWorkerThreadClass;
begin
	Result := TKWorkerThread;
end;

function TKWorkerThreadComp.GetThread: TKWorkerThread;
begin
	Result := FThread;
end;

procedure TKWorkerThreadComp.DoGetThread;
begin
	if Assigned( FOnGetThread ) then
		FOnGetThread( Self, FThread );
	if ( not CheckObject( FThread ) ) then
		FThread := GetThreadClass.CreateWorkerThread( Self );
end;

function TKWorkerThreadComp.DoThreadExecute( Param: Pointer; var ReturnCode: Cardinal ): Boolean;
begin
	try
    Result := Assigned( FOnThreadExecute );
		ReturnCode := THREAD_EXIT_CODE_NO_ERROR;
		if Result then
			FOnThreadExecute( Self, Thread, Param, ReturnCode, Result );
	except
		Application.HandleException( Self );
	end;
end;

function TKWorkerThreadComp.DoThreadExecuteMsg( Param: Pointer; msg: TMsg; 
  var Processed: Boolean ): Boolean;
begin
  try
    Result := True;
    Processed := False;
    if Assigned( FOnThreadExecuteMsg ) then
      FOnThreadExecuteMsg( Self, Thread, Param, msg, Processed, Result );
  except
    Application.HandleException( Self );
  end;
end;

procedure TKWorkerThreadComp.DoThreadDestroy;
begin
  if Assigned( FOnThreadDestroy ) then
    FOnThreadDestroy( Self, Thread );
  FreeClean( FThread );
end;

procedure TKWorkerThreadComp.DoThreadTerminate;
begin
	try
		try
			if Assigned( FOnThreadTerminate ) then
				FOnThreadTerminate( Self, Thread );
      if ( not FreeThreadOnTerminate ) then
        DoThreadDestroy;
		except
			Application.HandleException( Self );
		end;
	finally
		FThread := nil; { allways FreeOnTerminate }
	end;
end;

procedure TKWorkerThreadComp.SetPriority( Value: TKThreadPriority );
begin
	if ( Value <> FPriority ) then
	begin
		FPriority := Value;
		if CheckObject( Thread ) then
			Thread.Priority := Priority;
	end;
end;

function TKWorkerThreadComp.Running: Boolean;
begin
	Result := CheckObject( FThread );
end;

function TKWorkerThreadComp.Start: Boolean;
begin
	Result := ( not Running );
	if Result then
		DoGetThread;
end;

function TKWorkerThreadComp.Stop: Boolean;
begin
	Result := Running;
	if Result then
	begin
    Thread.SynchEvent.ResetEvent; 
    Thread.Terminate;
    if ( WaitForThread ) then //and ( not Destroying( Self ) ) ) then
      Thread.Lock( WaitForInterval ); { Wait for thread completion }
	end;
end;

procedure TKWorkerThreadComp.PostQuitMessage;
begin
  if Running then
    Thread.PostQuitMessage;
end;

procedure TKWorkerThreadComp.ThreadExecuteEvent( Sender: TKCustomWorkerThread;
	Param: Pointer; var ReturnCode: Cardinal; var Continue: Boolean );
begin
	Continue := DoThreadExecute( Param, ReturnCode );
end;

procedure TKWorkerThreadComp.ThreadTerminateEvent( Sender: TObject );
begin
	DoThreadTerminate;
end;

procedure TKWorkerThreadComp.ThreadExecuteMsgEvent( Sender: TKCustomWorkerThread;
  Param: Pointer; Msg: TMsg; var Processed, Continue: Boolean );
begin
  Continue := DoThreadExecuteMsg( Param, msg, Processed );
end;

{ TKWorkerThreadItem }

constructor TKWorkerThreadItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKWorkerThreads );
	inherited Create( ACollection );
	ForceObject( Owner.Component );
	FWorkerThreadComp := TKWorkerThreadComp.Create( nil );
	FWorkerThreadComp.OnGetThread := GetThreadEvent;
	FWorkerThreadComp.OnThreadExecute := ThreadExecuteEvent;
	FWorkerThreadComp.OnThreadTerminate := ThreadTerminateEvent;
  FWorkerThreadComp.OnThreadDestroy := ThreadDestroyEvent; 
end;

destructor TKWorkerThreadItem.Destroy;
begin
	FWorkerThreadComp.Free;
	inherited Destroy;
end;

procedure TKWorkerThreadItem.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKWorkerThreadItem ) then
	begin
		Priority := ( Source as TKWorkerThreadItem ).Priority;
    MsgFilterMin := ( Source as TKWorkerThreadItem ).MsgFilterMin;
    MsgFilterMax := ( Source as TKWorkerThreadItem ).MsgFilterMax;
    FreeThreadOnTerminate := ( Source as TKWorkerThreadItem ).FreeThreadOnTerminate;
    CreateSuspended := ( Source as TKWorkerThreadItem ).CreateSuspended;
    CreateMessageQueue := ( Source as TKWorkerThreadItem ).CreateMessageQueue;
		OnGetThread := ( Source as TKWorkerThreadItem ).OnGetThread;
		OnThreadExecute := ( Source as TKWorkerThreadItem ).OnThreadExecute;
    OnThreadExecuteMsg := ( Source as TKWorkerThreadItem ).OnThreadExecuteMsg;
		OnThreadTerminate := ( Source as TKWorkerThreadItem ).OnThreadTerminate;
    OnThreadDestroy := ( Source as TKWorkerThreadItem ).OnThreadDestroy;
	end;
end;

function TKWorkerThreadItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( inherited Equals( Item ) and ( ( Item as TKWorkerThreadItem ).Priority =
    Priority ) and ( ( Item as TKWorkerThreadItem ).FreeThreadOnTerminate =
    FreeThreadOnTerminate ) and ( ( Item as TKWorkerThreadItem ).CreateMessageQueue =
    CreateMessageQueue ) and ( ( Item as TKWorkerThreadItem ).MsgFilterMin =
    MsgFilterMin ) and ( ( Item as TKWorkerThreadItem ).MsgFilterMax = MsgFilterMax ) );
end;

procedure TKWorkerThreadItem.GetThreadEvent( Sender: TKWorkerThreadComp; var Thread: TKWorkerThread );
begin
	if Assigned( FOnGetThread ) then
		FOnGetThread( Sender, Self, Thread );
end;

procedure TKWorkerThreadItem.ThreadExecuteEvent( Sender: TKWorkerThreadComp;
  Thread: TKWorkerThread; Param: Pointer; var ReturnCode: Cardinal;
  var Continue: Boolean );
begin
  Continue := Assigned( FOnThreadExecute );
	if Continue then
		FOnThreadExecute( Sender, Self, Thread, Param, ReturnCode, Continue );
end;

procedure TKWorkerThreadItem.ThreadExecuteMsgEvent( Sender: TKWorkerThreadComp;
  Thread: TKWorkerThread; Param: Pointer; Msg: TMsg; var Processed,
  Continue: Boolean );
begin
  Continue := Assigned( FOnThreadExecuteMsg );
  if Continue then
    FOnThreadExecuteMsg( Sender, Self, Thread, Param, Msg, Processed, Continue );
end;

procedure TKWorkerThreadItem.ThreadTerminateEvent( Sender: TKWorkerThreadComp;
  Thread: TKWorkerThread );
begin
	if Assigned( FOnThreadTerminate ) then
		FOnThreadTerminate( Sender, Self, Thread );
end;

procedure TKWorkerThreadItem.ThreadDestroyEvent( Sender: TKWorkerThreadComp;
  Thread: TKWorkerThread );
begin
	if Assigned( FOnThreadDestroy ) then
		FOnThreadDestroy( Sender, Self, Thread );
end;

function TKWorkerThreadItem.GetOwnerCollection: TKWorkerThreads;
begin
	Result := TKWorkerThreads( inherited GetOwnerCollection );
end;

function TKWorkerThreadItem.GetThread: TKWorkerThread;
begin
	Result := FWorkerThreadComp.Thread;
end;

function TKWorkerThreadItem.GetPriority: TKThreadPriority;
begin
	Result := FWorkerThreadComp.Priority;
end;

function TKWorkerThreadItem.GetWaitForInterval: Cardinal;
begin
	Result := FWorkerThreadComp.WaitForInterval;
end;

function TKWorkerThreadItem.GetWaitForThread: Boolean;
begin
	Result := FWorkerThreadComp.WaitForThread;
end;

function TKWorkerThreadItem.GetFreeThreadOnTerminate: Boolean;
begin
  Result := FWorkerThreadComp.FreeThreadOnTerminate;
end;

function TKWorkerThreadItem.GetCreateSuspended: Boolean;
begin
  Result := FWorkerThreadComp.CreateSuspended;
end;

function TKWorkerThreadItem.GetCreateMessageQueue: Boolean;
begin
  Result := FWorkerThreadComp.CreateMessageQueue;
end;

function TKWorkerThreadItem.GetMsgFilterMin: Integer;
begin
  Result := FWorkerThreadComp.MsgFilterMin;
end;

function TKWorkerThreadItem.GetMsgFilterMax: Integer;
begin
  Result := FWorkerThreadComp.MsgFilterMax;
end;

procedure TKWorkerThreadItem.SetWaitForInterval( Value: Cardinal );
begin
	FWorkerThreadComp.WaitForInterval := Value;
end;

procedure TKWorkerThreadItem.SetWaitForThread( Value: Boolean );
begin
	FWorkerThreadComp.WaitForThread := Value;
end;

procedure TKWorkerThreadItem.SetPriority( Value: TKThreadPriority );
begin
	FWorkerThreadComp.Priority := Value;
end;

procedure TKWorkerThreadItem.SetFreeThreadOnTerminate( Value: Boolean );
begin
  FWorkerThreadComp.FreeThreadOnTerminate := Value;
end;

procedure TKWorkerThreadItem.SetCreateSuspended( Value: Boolean );
begin
  FWorkerThreadComp.CreateSuspended := Value;
end;

procedure TKWorkerThreadItem.SetCreateMessageQueue( Value: Boolean );
begin
  FWorkerThreadComp.CreateMessageQueue := Value;
end;

procedure TKWorkerThreadItem.SetMsgFilterMin( Value: Integer );
begin
	FWorkerThreadComp.MsgFilterMin := Value;
end;

procedure TKWorkerThreadItem.SetMsgFilterMax( Value: Integer );
begin
	FWorkerThreadComp.MsgFilterMax := Value;
end;

function TKWorkerThreadItem.Running: Boolean;
begin
	Result := FWorkerThreadComp.Running;
end;

function TKWorkerThreadItem.Start: Boolean;
begin
	Result := FWorkerThreadComp.Start;
end;

function TKWorkerThreadItem.Stop: Boolean;
begin
	Result := FWorkerThreadComp.Stop;
end;

procedure TKWorkerThreadItem.PostQuitMessage;
begin
  FWorkerThreadComp.PostQuitMessage;
end;

{ TKWorkerThreads }

constructor TKWorkerThreads.Create( AComp: TKWorkerThreadPool );
begin
	ForceObject( AComp );
	inherited Create( AComp, TKWorkerThreadItem, False );
end;

function TKWorkerThreads.Add: TKWorkerThreadItem;
begin
	Result := TKWorkerThreadItem( inherited Add );
end;

function TKWorkerThreads.GetItem( Index: Integer ): TKWorkerThreadItem;
begin
	Result := TKWorkerThreadItem( inherited GetItem( Index ) );
end;

function TKWorkerThreads.GetItemByName( const AName: string ): TKWorkerThreadItem;
begin
	Result := TKWorkerThreadItem( inherited GetItemByName( AName ) );
end;

function TKWorkerThreads.GetOwnerComp: TKWorkerThreadPool;
begin
	Result := TKWorkerThreadPool( inherited GetOwnerComp );
end;

procedure TKWorkerThreads.SetItem( Index: Integer; AItem: TKWorkerThreadItem );
begin
	inherited SetItem( Index, AItem );
end;

procedure TKWorkerThreads.Update( Item: TCollectionItem );
var
	i: Integer;
begin
	if CheckObject( Item ) then
		Component.UpdateItem( TKWorkerThreadItem( Item ) )
	else
		for i := 0 to Count - 1 do
			Component.UpdateItem( Items[i] );
end;

function TKWorkerThreads.RunningCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Inc( Result, Ord( Items[i].Running ) );
end;

procedure TKWorkerThreads.StartAll;
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		Items[i].Start;
end;

procedure TKWorkerThreads.SetStoppingAll( Value: Boolean );
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].WorkerThreadComp.SetStoppingAll( Value );
end;

procedure TKWorkerThreads.StopAll;
var
	i: Integer;
  wt: TKWorkerThread;
  wti: TKWorkerThreadItem;
begin
  if ( RunningCount <= 0 ) then
    Exit;
  SetStoppingAll( True );
  try
    { First signal all threads to start terminating... (leave it's execution procedure) }
    for i := 0 to Count - 1 do
    begin
      wti := Items[i];
      if wti.Running then
      begin
        wt := Items[i].Thread;
        wt.SynchEvent.ResetEvent;
        wt.Terminate;
      end;
    end;
    { Finally, accordinalgy to the user guess wait or go away with each thread }
    for i := 0 to Count - 1 do
    begin
      wti := Items[i];
      if ( wti.Running and wti.WaitForThread ) then
        wti.Thread.WaitFor( wti.WaitForInterval );
    end;
  finally
    SetStoppingAll( False );
  end;
{
  Could not be this simple manner to avoid the posiblity of a deadlock

	for i := 0 to Threads.Count - 1 do
		Threads[i].Stop;
}
end;

procedure TKWorkerThreads.Start( const Index: array of Integer );
var
	i: Integer;
begin
	for i := Low( Index ) to High( Index ) do
		if ( Index[i] <= Count - 1 ) then
			Items[Index[i]].Start;
end;

procedure TKWorkerThreads.Stop( const Index: array of Integer );
var
	i: Integer;
begin
  { Be carefully with this method... !!! }
	for i := Low( Index ) to High( Index ) do
		if ( Index[i] <= Count - 1 ) then
			Items[Index[i]].Stop;
end;

function TKWorkerThreads.ForEachWorkerDo( EnumWorkerFunc: TKEnumWorkersFunc; Data: Pointer ): Boolean;
var
	i: Integer;
begin
	Result := ( CheckReference( EnumWorkerFunc ) and CheckCollection( Self ) );
	if Result then
		for i := 0 to Count - 1 do
{$BOOLEVAL ON}
			Result := Result and EnumWorkerFunc( Component, Items[i], i, Data );
{$BOOLEVAL OFF}
end;

{ TKWorkerThreadPool }

constructor TKWorkerThreadPool.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FThreads := TKWorkerThreads.Create( Self );
end;

destructor TKWorkerThreadPool.Destroy;
begin
  Threads.StopAll;
	FreeClean( FThreads );
	inherited Destroy;
end;

procedure TKWorkerThreadPool.SetThreads( Value: TKWorkerThreads );
begin
	FThreads.Assign( Value );
end;

procedure TKWorkerThreadPool.UpdateItem( Item: TKWorkerThreadItem );
begin
	{ do nothing }
end;

{ TKProducerConsumerThread }

constructor TKProducerConsumerThread.CreateProducerConsumer( AOwner: TKProducerConsumer;
	CreateSuspended: Boolean; ThreadPriority: TKThreadPriority;
	ExecuteEvent, TerminateEvent: TNotifyEvent );
begin
	ForceObject( AOwner );
	ForceReference( ExecuteEvent );
	FreeOnTerminate := True;
	{
	Priority := ThreadPriority;
	OnExecute := ExecuteEvent;
	OnTerminate := TerminateEvent;
	inherited Create( AOwner, CreateSuspended );
}
end;

function TKProducerConsumerThread.GetOwner: TKProducerConsumer;
begin
	Result := TKProducerConsumer( inherited GetOwner );
end;

{ TKProducerConsumer }

constructor TKProducerConsumer.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FRunning := False;
	FSuspended := False;
	FMaxProducerCount := High( Cardinal );
	FMaxConsumerCount := High( Cardinal );
	FConsumerCount := 0;
	FProducerCount := FMaxProducerCount;
	FProducerPriority := tpNormal;
	FConsumerPriority := tpNormal;
	FMutex := TKMutex.Create( False, nil, False, '' );
end;

destructor TKProducerConsumer.Destroy;
begin
	Stop;
	FMutex.Free;
	inherited Destroy;
end;

procedure TKProducerConsumer.ForceActive;
begin
	if ( ( not FRunning ) or FSuspended ) then
		RaiseException( EKProducerConsumer, sErrProdConsStopped );
end;

procedure TKProducerConsumer.ForceInActive;
begin
	if ( FRunning and ( not FSuspended ) ) then
		RaiseException( EKProducerConsumer, sErrProdConsRunning );
end;

function TKProducerConsumer.GetMaxProducerCount: Cardinal;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FMaxProducerCount;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetMaxConsumerCount: Cardinal;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FMaxConsumerCount;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetConsumerPriority: TThreadPriority;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FProducerPriority;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetProducerPriority: TThreadPriority;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FConsumerPriority;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetOnProduce: TKProduceConsumeEvent;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FOnProduce;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetOnConsume: TKProduceConsumeEvent;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FOnConsume;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetOnStart: TNotifyEvent;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FOnStart;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetOnStop: TNotifyEvent;
begin
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FOnStop;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetMaxProducerCount( Value: Cardinal );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FMaxProducerCount := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetMaxConsumerCount( Value: Cardinal );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FMaxConsumerCount := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetProducerPriority( Value: TThreadPriority );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FProducerPriority := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetConsumerPriority( Value: TThreadPriority );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FConsumerPriority := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetOnProduce( Value: TKProduceConsumeEvent );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FOnProduce := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetOnConsume( Value: TKProduceConsumeEvent );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FOnConsume := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetOnStart( Value: TNotifyEvent );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FOnStart := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.SetOnStop( Value: TNotifyEvent );
begin
	ForceInactive;
	if ( not Designing( Self ) ) then
		FMutex.Lock( Cardinal( INFINITE ) );
	try
		FOnStop := Value;
	finally
		if ( not Designing( Self ) ) then
			FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetRunning: Boolean;
begin
	FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FRunning;
	finally
		FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetSuspended: Boolean;
begin
	FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FSuspended;
	finally
		FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetProducerCount: Cardinal;
begin
	FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FProducerCount;
	finally
		FMutex.UnLock;
	end;
end;

function TKProducerConsumer.GetConsumerCount: Cardinal;
begin
	FMutex.Lock( Cardinal( INFINITE ) );
	try
		Result := FConsumerCount;
	finally
		FMutex.UnLock;
	end;
end;

procedure TKProducerConsumer.ProduceExecute( Sender: TObject );
var
	dc: Cardinal;
begin
	//ProduceItem
	while Running do
	begin
		FProducerQueue.Lock( Cardinal( INFINITE ) );
		FCounterMutex.Lock( Cardinal( INFINITE ) );
		try
			dc := Max( 1, DoProduce( FProducerCount ) );
		finally
			FCounterMutex.Unlock;
		end;
		FConsumerQueue.UnLockEx( dc );
		FConsumerCount := FConsumerQueue.PreviousCount;
	end;
end;

procedure TKProducerConsumer.ProduceTerminate( Sender: TObject );
begin
	FProducer := nil;
end;

procedure TKProducerConsumer.ConsumeExecute( Sender: TObject );
var
	dc: Cardinal;
begin
	while Running do
	begin
		FConsumerQueue.Lock( Cardinal( INFINITE ) );
		FCounterMutex.Lock( Cardinal( INFINITE ) );
		try
			dc := Max( 1, DoConsume( FConsumerCount ) );
		finally
			FCounterMutex.Unlock;
		end;
		FProducerQueue.UnlockEx( dc );
		FProducerCount := FProducerQueue.PreviousCount;
		//ConsumeItem
	end;
end;

procedure TKProducerConsumer.ConsumeTerminate( Sender: TObject );
begin
	FConsumer := nil;
end;

function TKProducerConsumer.DoProduce( const Count: Cardinal ): Cardinal;
begin
	Result := Cardinal( Assigned( FOnProduce ) );
	if Assigned( FOnProduce ) then
		FOnProduce( Self, FProducer, Count, FMaxProducerCount, Result );
end;

function TKProducerConsumer.DoConsume( const Count: Cardinal ): Cardinal;
begin
	Result := Cardinal( Assigned( FOnConsume ) );
	if Assigned( FOnConsume ) then
		FOnConsume( Self, FConsumer, Count, FMaxConsumerCount, Result );
end;

procedure TKProducerConsumer.DoStart;
begin
	if Assigned( FOnStart ) then
		FOnStart( Self );
end;

procedure TKProducerConsumer.DoStop;
begin
	if Assigned( FOnStop ) then
		FOnStop( Self );
end;

function TKProducerConsumer.Start( CreateSuspended: Boolean ): Boolean;
begin
	Result := ( ( not FRunning ) and FMutex.Lock( Cardinal( INFINITE ) ) );
	try
		if ( not Result ) then
			Exit;
		try
			FSuspended := CreateSuspended;
			FProducerQueue := TKSemaphore.Create( False, nil, FProducerCount,
				FMaxProducerCount, '' );
			FConsumerQueue := TKSemaphore.Create( False, nil, FConsumerCount,
				FMaxConsumerCount, '' );
			FCounterMutex := TKMutex.Create( False, nil, False, '' );
{
			FProducer := TKProducerConsumerThread.Create( Self, CreateSuspended, FProducerPriority,
				ProduceExecute, ProduceTerminate );
			FConsumer := TKProducerConsumerThread.Create( Self, CreateSuspended, FConsumerPriority,
				ConsumeExecute, ConsumeTerminate );
}
			DoStart;
		except
			Stop;
			FRunning := False;
			raise;
		end;
		FRunning := True;
	finally
		//if FMutex.Locked then
			Result := FMutex.Unlock;
		Result := ( Result and FRunning );
	end;
end;

function TKProducerConsumer.Stop: Boolean;
begin
	Result := ( FRunning and FMutex.Lock( Cardinal( INFINITE ) ) );
	try
		if ( not ( Result and Suspend ) ) then
			Exit;
		DoStop;
		FConsumer.Terminate;
		FProducer.Terminate;
		FreeClean( FCounterMutex );
		FreeClean( FConsumerQueue );
		FreeClean( FProducerQueue );
	finally
		//if FMutex.Locked then
			Result := FMutex.UnLock;
		Result := ( Result and ( not FRunning ) );
	end;
end;

function TKProducerConsumer.Suspend: Boolean;
begin
	Result := ( FRunning and ( not FSuspended ) );
	if ( not Result ) then
		Exit;
	FProducer.Suspend;
	FConsumer.Suspend;
	Result := ( FProducer.Suspended and FConsumer.Suspended );
	FSuspended := Result;
end;

function TKProducerConsumer.Resume: Boolean;
begin
	Result := ( FRunning and FSuspended );
	if ( not Result ) then
		Exit;
	FConsumer.Resume;
	FProducer.Resume;
	Result := ( not ( FProducer.Suspended or FConsumer.Suspended ) );
	FSuspended := ( not Result );
end;

{ TKCustomMailSlot }

{--------------------------- Internal Implementation ---------------------------}

function GetMailSlot( const Server, Slot: string ): THandle;
{ retrieves a mailslot handle given the server and slot names }
var
	sSlot: string;
begin
	sSlot := '\\' + Server + '\mailslot\' + Slot;
	Result := CreateFile( PChar( sSlot ), GENERIC_WRITE, FILE_SHARE_READ,	nil,
		OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, Cardinal( INVALID_HANDLE_VALUE ) );
	ForceHandle( Result );
end;

function SendMail( const Server, Slot: string; Buff: Pointer; Size: Integer ): Boolean;
{ sends Msg to the slot and server passed }
var
	hSlot: THandle;
	iWritten: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	bClosed, bWritten: Boolean;
begin
	bClosed := false;
	bWritten := false;
	hSlot := GetMailSlot( Server, Slot );
	try
		try
			bWritten := WriteFile( hSlot, Pointer( Buff )^, Size, iWritten, nil );
		finally
			bClosed := CloseHandle( hSlot );
		end;
	except
		if ( hSlot = INVALID_HANDLE_VALUE ) then
			RaiseException( EKMailSlot, sErrMSUnableRetrieve )
		else if ( not bWritten ) then
			RaiseExceptionFmt( EKMailSlot, sErrMSUnableWrite, [SysErrorMessage( GetLastError )] )
		else if ( not bClosed ) then
			RaiseException( EKMailSlot, sErrMSUnableClose )
		else
			raise;
	end;
	Result := bWritten;//true;
end;

{---------------------------- Public Implementation ----------------------------}

{--------------------------------- TKMailSlot ----------------------------------}

{	TKReaderThread }

constructor TKReaderThread.Create( Slot: TKCustomMailSlot );
begin
	ForceObject( Slot );
{ Start thread in suspended state }
	FreeOnTerminate := true;
{ point to the slot responsible for the thread }
	FMailSlot := Slot;
{ execute when Windows is IDLE }
	FMsgsWaiting := 0;
	FNextMsgSize := 0;
	inherited Create( true );
	Priority := tpLowest;
end;

function TKReaderThread.MailSlotInfo: Boolean;
begin
	Result := GetMailSlotInfo( FMailSlot.FHandle, nil, FNextMsgSize, @FMsgsWaiting, nil );
end;

procedure TKReaderThread.Execute;
begin
	while ( not Terminated ) do
		if ( MailSlotInfo ) and ( FMsgsWaiting > 0 ) and
			 ( FNextMsgSize <> MAILSLOT_NO_MESSAGE ) and ( FNextMsgSize > 0 ) then
			MessageReady;
end;

procedure TKReaderThread.MessageReady;
begin
	FMailSlot.ReadMessage;
end;

{	TKReceiverSession }

constructor TKReceiverSession.Create( AOwner: TKMailSlot; const ASlot, AServer: string;
	ASize, ATxID: LongInt; ADataType: TKMSDataType );
begin
	ForceObject( AOwner );
	ForceTrimStrs( [ASlot, AServer] );
	inherited Create;
	FOwner := AOwner;
	FSize := ASize;
	FSlotName := ASlot;
	FServerName := AServer;
	FDataType := ADataType;
	FTransmissionID := ATxID;
	FData := AllocMem( Size );
	FDataBlocks := ( Size div MAX_SDATA_SIZE ) + Ord( ( Size mod MAX_SDATA_SIZE ) > 0 );
	FReceived := TBits.Create;
	FReceived.Size := DataBlocks;
	FIdleSince := GetTickCount;
end;

destructor TKReceiverSession.Destroy;
begin
	FreeMem( FData, Size );
	FReceived.Free;
	inherited Destroy;
end;

function TKReceiverSession.GetDataComplete: Boolean;
begin
	Result := ( FReceived.OpenBit = FReceived.Size );
end;

function TKReceiverSession.GetReceived( Index: Integer ): Boolean;
begin
	Result := FReceived[Index - 1];
end;

procedure TKReceiverSession.SetReceived( Index: Integer; Value: Boolean );
begin
	FReceived[Index - 1] := Value;
end;

procedure TKReceiverSession.ReceiveDatagram( ADatagram: PKSDatagram );
var
	pt: Pointer;
	iSeq,
	szData: LongInt;
begin
	iSeq := ADatagram^.SequenceNo;
	if ( not Received[iSeq] ) then
	begin
		pt := IncPtr( FData, ( iSeq - 1 ) * MAX_SDATA_SIZE );
		szData := ADatagram^.DataSize;
		if ( szData = 0 ) then
			szData := MAX_SDATA_SIZE;
		Move( ADatagram^.Data, pt^, szData );
		Received[iSeq] := true;
		FIdleSince := GetTickCount;
{   if DataComplete then
			Owner.EndOfSegmentation( Self ); }
	end;
end;

{ TKReceiverList }

constructor TKReceiverList.Create;
begin
	inherited Create;
	FList := TList.Create;
end;

destructor TKReceiverList.Destroy;
begin
	while CheckList( FList ) do
	begin
		TKReceiverSession( FList[0] ).Free;
    FList.Delete( 0 );
	end;
	FList.Free;
	inherited Destroy;
end;

function TKReceiverList.GetCount: Integer;
begin
	Result := FList.Count;
end;

function TKReceiverList.GetItems( Index: Integer ): TKReceiverSession;
begin
	Result := TKReceiverSession( FList[Index] );
end;

function TKReceiverList.GetItemsFromID( TxID: LongInt ): TKReceiverSession;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if ( Sessions[i].TransmissionID = TxID ) then
		begin
			Result := Sessions[i];
			Exit;
		end;
end;

procedure TKReceiverList.Pack;
begin
	FList.Pack; 
end;

procedure TKReceiverList.Clear;
begin
	FList.Clear;
end;

procedure TKReceiverList.Delete( Index: Integer );
begin
	FList.Delete( Index );
end;

procedure TKReceiverList.Remove( Value: TKReceiverSession );
begin
	FList.Remove( Pointer( Value ) );
end;

function TKReceiverList.Add( Value: TKReceiverSession ): Integer;
begin
	Result := FList.Add( Pointer( Value ) );
end;

function TKReceiverList.IndexOf( Value: TKReceiverSession ): Integer;
begin
	Result := FList.IndexOf( Pointer( Value ) );
end;

{ TKCustomMailSlot }

constructor TKCustomMailSlot.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FMaxSize := 0;
	FActive := false;
	FSlot := MAILSLOT_KNOWHOW_SLOT;
	FHandle := INVALID_HANDLE_VALUE;
	FMailList := TList.Create;
	FReaderThread := TKReaderThread.Create( Self );
	if ( not Designing( Self ) ) then
		Application.HookMainWindow( AppWndProc );
end;

destructor TKCustomMailSlot.Destroy;
begin
{ close the mailslot: ie, stop communications }
	Close;
	if ( CheckObject( FReaderThread ) and
		 ( not FReaderThread.Terminated ) ) then
	begin
{ ask thread to finish up }
		FReaderThread.Terminate;
{ make sure it will do its house keeping }
		FReaderThread.Resume;
		FReaderThread.WaitFor;
	end;
	if ( not Designing( Self ) ) then
		Application.UnHookMainWindow( AppWndProc );
{ Give a chance to any posted mail to be processed }
	Application.ProcessMessages;
	ClearMailList;
	FMailList.Free;
	inherited Destroy;
end;

procedure TKCustomMailSlot.ClearMailList;
var
	i: Integer;
begin
	PackMailList;
	for i := 0 to FMailList.Count - 1 do
		DeleteRawData( i );
	FMailList.Clear;
end;

procedure TKCustomMailSlot.DeleteRawData( Index: Integer );
begin
	with PKMailSlotRawData( FMailList[Index] )^ do
		FreeMem( lpData, dwSize );
	Dispose( FMailList[Index] );
	FMailList.Items[Index] := nil;
end;

procedure TKCustomMailSlot.CheckInactive;
begin
  if Active then
		RaiseException( EKCustomMailSlot, sErrCMSInvActiveOp );
end;

procedure TKCustomMailSlot.SetSlot( const Value: string );
begin
	if ( not CheckStrEqual( Value, FSlot ) ) then
	begin
		CheckInactive;
		FSlot := Value;
	end;	
end;

procedure TKCustomMailSlot.Open;

	procedure OpenFailed;
	begin
		FActive := false;
		FHandle := INVALID_HANDLE_VALUE;
		if ( not FReaderThread.Suspended ) then
			FReaderThread.Suspend;
	end;

var
	SlotName: string;
begin
{ If slot is openned or it is design-time, do not open }
	if ( Designing( Self ) or Active ) then
		Exit;
{ suppose open is successful }
	FActive := true;
	try
		PackMailList;
{ construct actual slot name }
		SlotName := MAILSLOT_ROOT + FSlot;
{ create the slot }
		FHandle := CreateMailSlot( PChar( SlotName ), MaxSize, MAILSLOT_WAIT_FOREVER, nil );
{ check the handle for a valid mailslot }
		if ( not CheckHandle( FHandle ) ) then
{ if an invalid handle is returned, fail this operation }
			OpenFailed
		else
{ if a valid handle is returned restart the reader thread }
			FReaderThread.Resume;
{ if everything is OK up to this point, the slot has been successfully openned }
		if FActive then
			DoActivate;
	except
{ for any unexpected exception, make sure the slot is left closed, the handle
	is reset to an invalid value, and the thread is suspended. }
		if FActive then
			OpenFailed;
		raise;
	end;
end;

procedure TKCustomMailSlot.Close;
begin
{ If slot is closed or it is design-time, do not close }
	if ( Designing( Self ) or ( not Active ) ) then
		Exit;
{ suppose close is successful }
	FActive := false;
	try
{ Stop thread from reading the mailslot }
		FReaderThread.Suspend;
		if ( FHandle <> INVALID_HANDLE_VALUE ) then
		begin
{ close the mailslot }
			CloseHandle( FHandle );
{ reset the handle to an invalid value }
			FHandle := INVALID_HANDLE_VALUE;
		end;
		ClearMailList;
{ if everything is OK up to this point, the slot has been successfully closed }
		DoDeactivate;
	except
		RaiseException( EKCustomMailSlot, sErrCMSClosingError );
	end;
end;

function TKCustomMailSlot.GetMsgsWaiting: DWORD;
begin
	Result := 0;
	if ( Active and ( not FReaderThread.Suspended ) ) then
		Result := FReaderThread.FMsgsWaiting;
end;

function TKCustomMailSlot.GetNextMsgSize: DWORD;
begin
	Result := 0;
	if ( Active and ( not FReaderThread.Suspended ) ) then
		Result := FReaderThread.FNextMsgSize;
end;

function TKCustomMailSlot.GetMail( Index: Integer ): PKMailSlotRawData;
begin
	Result := PKMailSlotRawData( FMailList[Index] );
end;

function TKCustomMailSlot.HasMail: Boolean;
begin
	Result := ( MsgsWaiting > 0 );
end;

procedure TKCustomMailSlot.PackMailList;
begin
	FMailList.Pack;
end;

procedure TKCustomMailSlot.ReadMessage;
var
	pms: PKMailSlotRawData;
	dwReadSize: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
begin
	pms := New( PKMailSlotRawData );
	try
		pms^.dwSize := NextMsgSize;
		GetMem( pms^.lpData, pms^.dwSize );
		try
			if ( not ReadFile( FHandle, pms^.lpData^, pms^.dwSize, dwReadSize, nil ) ) then
				RaiseExceptionFmt( EKCustomMailSlot, sErrCMSReadingErrorEx, [GetLastError, SysErrorMessage( GetLastError )] );
			if ( pms^.dwSize <> Integer( dwReadSize ) ) then
				RaiseException( EKCustomMailSlot, sErrCMSReadingError );
{ descendant classes should know how to manage this data }
			DoReceiveMail( pms );
		except
			FreeMem( pms^.lpData, pms^.dwSize );
			raise;
		end;
	except
		Dispose( pms );
		raise;
	end;
end;

procedure TKCustomMailSlot.DoActivate;
begin
	if Assigned( FOnActivate ) then
		FOnActivate( Self );
end;

procedure TKCustomMailSlot.DoDeactivate;
begin
	if Assigned( FOnDeactivate ) then
		FOnDeactivate( Self );
end;

function TKCustomMailSlot.AppWndProc( var Message: TMessage ): Boolean;
begin
	Result := ValueBetween( Message.Msg, KM_MSFIRST, KM_MSLAST, True );
	if Result then
	begin
		Dispatch( Message );
		Result := Boolean( Message.Result );
	end;
end;

procedure TKCustomMailSlot.DoReceiveMail( pms: PKMailSlotRawData );
var
	i: Integer;
begin
{ Collect Mail }
	i := FMailList.Add( pms );
	PostMessage( Application.Handle, KM_READMAIL, i, 0 );
end;

procedure TKCustomMailSlot.KMReadMail( var Message: TKMReadMail );
begin
	DeleteRawData( Message.Index );
	Message.Result := 1;
end;

{ TKMailSlot }

constructor TKMailSlot.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FEcho := false;
{ list of messages that need acknowledgement of receipt }
	FNeedACK := TList.Create;
{ list of recently received messages }
	FReceived := TList.Create;
{ default property values }
  FBroadcastMode := bmDefault;
	FBufferSize := DEF_MS_BUFFSIZE;
	FTimeOut := DEF_MS_TIMEOUT;
	FBurstCount := DEF_MS_BURSTCOUNT;
	FBurstDelay := DEF_MS_BURSTDELAY;
{ timer properties }
	FTimer := TTimer.Create( nil );
	FTimer.Enabled := false;
	FTimer.Interval := DEF_MS_INTERVAL;
	FTimer.OnTimer := MailCheck;
	FReceivers := TKReceiverList.Create;
	FDomains := TStringList.Create;
end;

destructor TKMailSlot.Destroy;
begin
	ClearNeedACKList;
	ClearReceivedList;
	FDomains.Free;
	inherited Destroy;
	FTimer.Free;
	FNeedACK.Free;
	FReceived.Free;
	FReceivers.Free;
end;

procedure TKMailSlot.KMReadMail( var Message: TKMReadMail );
var
	pms: PKMailSlotRawData;
begin
	pms := PKMailSlotRawData( MailList[Message.Index] );
	try
		if ( pms^.dwSize = SizeOf( TKSDatagram ) ) then
			Process_SDatagram( pms^.lpData )
		else if ValueBetween( pms^.dwSize, SizeOf( TKNDatagramHeader ) + MAILSLOT_NDATA_COMPLETION,
			MAILSLOT_USERNAME_SIZE + MAILSLOT_COMPUTERNAME_SIZE + SLOTNAME_SIZE +
			SizeOf( TKNDatagramHeader ) + MAILSLOT_NDATA_COMPLETION, true ) then
			Process_NDatagram( pms^.lpData, pms^.dwSize )
		else
			RaiseExceptionFmt( EKMailSlot, sErrMSInvDatagramSize, [pms^.dwSize] );
	finally
	{ The inherited method will delete the item from list }
		inherited;
	end;
end;

procedure TKMailSlot.KMMSTimeOut( var Message: TKMMSTimeOut );
var
	mt: TKMailType;
	ts: TTimeStamp;
	ui: TKMSUserInfo;
	pdh: PKNDatagramHeader;
begin
	pdh := PKNDatagramHeader( FNeedACK[Message.Index] );
	ts := pdh^.Signature;
	ui := GetUserInfo( pdh );
	mt := GetMailType( pdh^.DatagramType );
	FreeMem( pdh );
	FNeedACK[Message.Index] := nil;
	DoTimeOut( ui, ts, mt );
	Message.Result := 1;
end;

procedure TKMailSlot.KMMSReceiveACK( var Message: TKMMSReceiveACK );
var
	ts: TTimeStamp;
	ui: TKMSUserInfo;
	pdh: PKNDatagramHeader;
begin
	pdh := PKNDatagramHeader( FReceived[Message.Index] );
	ui := GetUserInfo( pdh );
	ts.Date := pdh^.Control_01;
	ts.Time := pdh^.Control_02;
	FreeMem( pdh );
	FReceived[Message.Index] := nil;
	DoReceiveACK( ui, ts, Message.DataGramType );
	Message.Result := 1;
end;

function TKMailSlot.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TKMailSlot.SetInterval( Value: Cardinal );
begin
	if ( FTimer.Interval <> Value ) then
	begin
		CheckInactive;
		FTimer.Interval := Value;
	end;
end;

procedure TKMailSlot.SetTimeOut( Value: Cardinal );
begin
	if ( FTimeOut <> Value ) then
	begin
		CheckInactive;
		FTimeOut := Value;
	end;	
end;

procedure TKMailSlot.SetBufferSize( Value: Cardinal );
begin
	if ( Value <> FBufferSize ) then
	begin
		CheckInactive;
		FBufferSize := Value;
		FNeedACK.Capacity := FBufferSize;
		FReceived.Capacity := ( 2 * FBufferSize ); { Double space for the ACKs }
	end;	
end;

procedure TKMailSlot.SetBurstCount( Value: Cardinal );
begin
	if ( Value <> FBurstCount ) then
	begin
		CheckInactive;
		FBurstCount := Value;
	end;
end;

function TKMailSlot.GetCurrentDomain: string;
begin
	Result := DomainName;
end;

function TKMailSlot.GetDomain: string;
begin
	Result := GetFirstString( [FDomain, MAILSLOT_DEFAULT_DOMAIN_BROADCAST] );
end;

procedure TKMailSlot.SetDomain( const Value: string );
begin
	if ( not CheckStrEqual( Value, FDomain ) ) then
	begin
		CheckInactive;
		FDomain := Value;
	end;
end;

procedure TKMailSlot.SetBurstDelay( Value: Cardinal );
begin
	if ( Value <> FBurstDelay ) then
	begin
		CheckInactive;
		FBurstDelay := Value;
	end;
end;

function TKMailSlot.GetMailType( Value: Byte ): TKMailType;
begin
	Result := TKMailType( Value and $7F );
end;

procedure TKMailSlot.UpdateACKMail( pdh: PKNDatagramHeader; const ToSlot, ToServer: string );
var
	sz: LongInt;
	p,
	pp: Pointer;
begin
	with FNeedACK do
	begin
		Pack;
	{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
		violates the Integer Range and generates a warning here. Even, Count
  	also returns Integer ranged values... }
		if ( Count = Integer( BufferSize ) ) then
		begin
			FreeMem( Items[0] );
			Delete( 0 );
		end;
{ User name will not be included }
		sz := SizeOf( TKNDatagramHeader ) + Length( ToSlot ) + Length( ToServer ) +
		  ( MAILSLOT_NDATA_COMPLETION - 1 );
		p := AllocMem( sz );
		try
			System.Move( pdh^, p^, SizeOf( TKNDatagramHeader ) );
			pp := IncPtr( p, SizeOf( TKNDatagramHeader ) );
			System.Move( Pointer( ToSlot )^, pp^, Length( ToSlot ) );
			pp := IncPtr( pp, Length( ToSlot ) + 1 );
			System.Move( Pointer( ToServer )^, pp^, Length( ToServer ) );
			Add( p );
		except
			FreeMem( p, sz );
			raise;
		end;
	end;
end;

procedure TKMailSlot.SendSDatagram( psd: PKSDatagram; const ToSlot, ToServer: string );
begin
	SendMail( ToServer, ToSlot, psd, SizeOf( TKSDatagram ) );
end;

procedure TKMailSlot.SendNDatagram( NeedACK: Boolean; AMailType: TKMailType;
	AControl_01, AControl_02: LongInt; const ToSlot, ToServer: string );
var
	p,
	pp: Pointer;
	sUser: string;
	sz,
	szSlot,
	szUser,
	szComputer: LongInt;
	Confirmed,
	IsBroadcast: Boolean;
begin
	GetDomainNames( FDomains );
	FDomains.Add( MAILSLOT_DEFAULT_DOMAIN_BROADCAST );
	IsBroadcast := ( FDomains.IndexOf( ToServer ) <> -1 );
	if ( IsBroadcast and ( AMailType = mtACK ) ) then
		RaiseException( EKMailSlot, sErrMSInvBroadcastAck );
	sUser := UserName;
	szSlot := Length( Slot );
	szComputer := Length( ComputerName );
	szUser := Min( Length( sUser ), MAILSLOT_USERNAME_SIZE );
	sz := ( SizeOf( TKNDatagramHeader ) + szSlot + szComputer + szUser +
		MAILSLOT_NDATA_COMPLETION );
	p := AllocMem( sz );
	try
		with PKNDatagramHeader( p )^ do
		begin
			AppHwnd := Application.Handle;
			Signature := DateTimeToTimeStamp( Now );
			DatagramType := Byte( AMailType );
			Confirmed := ( NeedACK and ( AMailType <> mtACK ) and
				( ( not IsBroadcast ) or ( AMailType = mtSYNC ) ) );
			if Confirmed then
			begin
				DatagramType := TurnBitOn( DatagramType, 7 );
				UpdateACKMail( p, ToSlot, ToServer );
			end;
			Control_01 := AControl_01;
			Control_02 := AControl_02;
		end;
		pp := IncPtr( p, SizeOf( TKNDatagramHeader ) );
		Move( Pointer( Slot )^, pp^, szSlot );
		pp := IncPtr( pp, szSlot + 1 );
		Move( Pointer( ComputerName )^, pp^, szComputer );
		pp := IncPtr( pp, szComputer + 1 );
		Move( Pointer( sUser )^, pp^, szUser );
		SendMail( ToServer, ToSlot, p, sz );
	finally
		FreeMem( p, sz );
	end;
end;

function TKMailSlot.NeedACK( pdh: PKNDatagramHeader ): Boolean;
begin
	Result := IsBitOn( pdh^.DatagramType, 7 );
end;

function TKMailSlot.Equals( pdh1, pdh2: PKNDatagramHeader ): Boolean;
begin
	Result := CompareMem( pdh1, pdh2, SizeOf( TKNDatagramHeader ) );
end;

function TKMailSlot.UpdateReceivedMail( var pdh: PKNDatagramHeader ): Boolean;
var
	i,
	sz: Integer;
	p,
	pp: Pointer;
	ui: TKMSUserInfo;
begin
	if ( not CheckPointer( pdh ) ) then
		RaiseException( EKMailSlot, sErrMSInvHeaderData );
{ pdh is the header of a message newly received by the reader thread; for now,
	suppose this IS a duplicate message }
	Result := false;
{ pack the list of received mail, so no nil pointers get processed }
	FReceived.Pack;
	with FReceived do
	begin
{ process through the list of received mail and check for a copy of the
	current message- if such a copy is found, exit: duplicate message }
		for i := 0 to Count - 1 do
			if Equals( pdh, PKNDatagramHeader( Items[i] ) ) then
				Exit;
{ this IS NOT a duplicate message }

{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Count
	also returns Integer ranged values... }
		if ( Count = Integer( BufferSize ) ) then

{ if the queue of received mail if full, remove the first message ( should be
	the oldest one ), and make room for the new message }
		begin
			FreeMem( Items[0] );
			Delete( 0 );
		end;
{ now, we're sure there's room for the new message in the queue; allocate
	some memory for this message header }
		ui := GetUserInfo( pdh );
		sz := SizeOf( TKNDatagramHeader ) + Length( ui.SlotName ) +
			Length( ui.ServerName ) + Length( ui.UserName ) + 4;
		p := AllocMem( sz );
		try
			System.Move( pdh^, p^, SizeOf( TKNDatagramHeader ) );
			pp := IncPtr( p, SizeOf( TKNDatagramHeader ) );
			System.Move( Pointer( ui.SlotName )^, pp^, Length( ui.SlotName ) );
			pp := IncPtr( pp, Length( ui.SlotName ) + 1 );
			System.Move( Pointer( ui.ServerName )^, pp^, Length( ui.ServerName ) );
			pp := IncPtr( pp, Length( ui.ServerName ) + 1 );
			System.Move( Pointer( ui.UserName )^, pp^, Length( ui.UserName ) );
			Add( p );
			pdh := p;
			Result := true;
		except
			FreeMem( p, sz );
			raise;
		end;
	end;
end;

function TKMailSlot.GetUserInfo( pdh: PKNDatagramHeader ): TKMSUserInfo;
var
	ppc: PChar;
	sl: TStringList;
begin
	ZeroMemory( @Result, SizeOf( TKMSUserInfo ) );
	sl := TStringList.Create;
	try
		ppc := IncPtr( pdh, SizeOf( TKNDatagramHeader ) );
		try
			PCharListToStrings( ppc, sl );
			if ( sl.Count = 2 ) then
			begin
				Result.SlotName := sl[0];
				Result.ServerName := sl[1];
				Result.UserName := '';
			end
			else if ( sl.Count = 3 ) then
			begin
				Result.SlotName := sl[0];
				Result.ServerName := sl[1];
				Result.UserName := sl[2];
			end;
		except
{ ignore... header has no additional info about user, server, or slot }
		end;
	finally
		sl.Free;
	end;
end;

procedure TKMailSlot.DoDataArrive( ASession: TKReceiverSession );
var
	sz: LongInt;
	sData,
	sName: TFileName;
	fs: TFileStream;
begin
	case ASession.DataType of
		mdtData:
			if Assigned( FOnDataArrive ) then
				FOnDataArrive( Self, ASession.DataComplete, ASession.TransmissionID, ASession.Data, ASession.Size );
		mdtText:
			if Assigned( FOnTextArrive ) then
			begin
				SetString( sData, PChar( ASession.Data ), ASession.Size );
				FOnTextArrive( Self, ASession.DataComplete, ASession.TransmissionID, sData );
			end;
		mdtFile:
			if Assigned( FOnFileArrive ) then
			begin
				sName := PChar( ASession.Data );
				sz := StrLen( PChar( ASession.Data ) ) + 1;
				FOnFileArrive( Self, ASession.DataComplete, ASession.TransmissionID, sName, ASession.Size - sz );
				if ASession.DataComplete then
				begin
					fs := TFileStream.Create( sName, fmCreate or fmShareExclusive );
					try
						fs.Size := 0;
						fs.WriteBuffer( IncPtr( ASession.Data, sz )^, ASession.Size - sz );
					finally
						fs.Free;
					end;
				end;
			end;
	end;
end;

procedure	TKMailSlot.ClearNeedACKList;
var
	i: Integer;
begin
	FNeedACK.Pack;
	for i := FNeedACK.Count - 1 downto 0 do
		FreeMem( FNeedACK.Items[i] );
	FNeedACK.Clear;
end;

procedure TKMailSlot.ClearReceivedList;
var
	i: Integer;
begin
	FReceived.Pack;
	for i := FReceived.Count - 1 downto 0 do
		FreeMem( FReceived.Items[i] );
	FReceived.Clear;
end;

procedure TKMailSlot.DoActivate;
begin
	FTimer.Enabled := true;
	inherited DoActivate;
end;

procedure TKMailSlot.DoDeactivate;
begin
	FTimer.Enabled := false;
	ClearNeedACKList;
	ClearReceivedList;
	inherited DoDeactivate;
end;

procedure TKMailSlot.DelayTimeOut( Index: Integer );
begin
	PostMessage( Application.Handle, KM_MSTIMEOUT, Index, 0 );
end;

procedure TKMailSlot.DoTimeOut( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; AType: TKMailType );
begin
	if Assigned( FOnTimeOut ) then
		FOnTimeOut( Self, TimeStampToDateTime( ASignature ), UserInfo, AType );
end;

procedure TKMailSlot.DelayReceiveACK( Index: Integer; ADatagramType: Byte );
begin
	PostMessage( Application.Handle, KM_MSRECEIVEACK, Index, LongInt(
	  GetMailType( ADatagramType ) ) );
end;

procedure TKMailSlot.DoReceiveACK( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; AType: TKMailType );
begin
	if Assigned( FOnReceiveACK ) then
		FOnReceiveACK( Self, TimeStampToDateTime( ASignature ), UserInfo, AType );
end;

procedure TKMailSlot.DoSynchronize( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; Control_01, Control_02: LongInt );
begin
	if Assigned( FOnSynchronize ) then
		FOnSynchronize( Self, TimeStampToDateTime( ASignature ), UserInfo, Control_01, Control_02 );
end;

procedure TKMailSlot.DoReceiveControl( UserInfo: TKMSUserInfo; ASignature: TTimeStamp; Control_01, Control_02: LongInt );
begin
	if Assigned( FOnReceiveControl ) then
		FOnReceiveControl( Self, TimeStampToDateTime( ASignature ), UserInfo, Control_01, Control_02 );
end;

procedure TKMailSlot.DoReceiveSOB( UserInfo: TKMSUserInfo; DataSize, TxID: LongInt; DataType: TKMSDataType );
begin
	if CheckObject( FReceivers.SessionsFromID[TxID] ) then
		Exit; { MUST BE A RAISE }
	FReceivers.Add( TKReceiverSession.Create( Self, UserInfo.SlotName,
		UserInfo.ServerName, DataSize, TxID, DataType ) );
end;

procedure TKMailSlot.DoReceiveEOB( UserInfo: TKMSUserInfo; Status, TxID: LongInt );
var
	rcv: TKReceiverSession;
begin
	if ( not CheckObject( FReceivers.SessionsFromID[TxID] ) ) then
		Exit;
	rcv := FReceivers.SessionsFromID[TxID];
	EndOfSegmentation( rcv );
end;

procedure TKMailSlot.DoReceiveERN( UserInfo: TKMSUserInfo; Status, TxID: LongInt );
begin
	if Assigned( FOnTransmissionEnd ) then
		FOnTransmissionEnd( Self, TxID, UserInfo, Boolean( Status ) );
end;

procedure TKMailSlot.DoDispatchMail( pdh: PKNDatagramHeader );
const
	MS_DATATYPE: array[Boolean, Boolean] of Byte =
	(
		( 0, 1 ),
		( 2, 3 )
	);
var
	i,
	j: Integer;
	ts: TTimeStamp;
	ui: TKMSUserInfo;
	dt: TKMSDataType;
begin
	ui := GetUserInfo( pdh );
	case GetMailType( pdh^.DatagramType ) of
		mtACK:
		begin
			i := FReceived.IndexOf( pdh );
			if ( i <> -1 ) then
			begin
				ts.Date := pdh^.Control_01;
				ts.Time := pdh^.Control_02;
				for j := 0 to FNeedACK.Count - 1 do
					if ( PKNDatagramHeader( FNeedACK[j] )^.Signature.Date = ts.Date ) and
						 ( PKNDatagramHeader( FNeedACK[j] )^.Signature.Time = ts.Time ) then
					begin
						DelayReceiveACK( i, PKNDatagramHeader( FNeedACK[j] )^.DatagramType );
						FreeMem( FNeedACK[j] );
						FNeedACK[j] := nil;
						Break;
					end;
			end;
		end;
		mtSYNC:
			DoSynchronize( ui, pdh^.Signature, pdh^.Control_01, pdh^.Control_02 );
		mtControl:
			DoReceiveControl( ui, pdh^.Signature, pdh^.Control_01, pdh^.Control_02 );
		mtSOB:
		begin
			dt := mdtData;
			case MS_DATATYPE[IsBitOn( pdh^.Control_01, 30 ), IsBitOn( pdh^.Control_01, 31 )] of
				0: dt := mdtData;
				1: dt := mdtText;
				2: dt := mdtFile;
			else
				RaiseException( EKMailSlot, sErrMSInvDataType );
			end;
			DoReceiveSOB( ui, pdh^.Control_01 and $3FFFFFFF, pdh^.Control_02, dt );
		end;
		mtEOB:
			DoReceiveEOB( ui, pdh^.Control_01, pdh^.Control_02 );
		mtERN:
			DoReceiveERN( ui, pdh^.Control_01, pdh^.Control_02 );
	end;
end;

procedure TKMailSlot.Process_NDatagram( Data: Pointer; Size: Integer );
var
	ui: TKMSUserInfo;
	pdh: PKNDatagramHeader;
begin
	pdh := PKNDatagramHeader( Data );
	if ( ( pdh^.AppHwnd = Application.Handle ) and ( not FEcho ) ) then
		Exit;
{ If FReceived is not updated, the message is a duplicate. But, if this is
	not a duplicate message, a copy of its header has been added to the list of
	newly received messages. }
	ui := GetUserInfo( pdh );
	if UpdateReceivedMail( pdh ) then
	begin
{	If this message needs confirmation, send an ACK immediately }
		if ( ( pdh^.AppHwnd = Application.Handle ) and NeedACK( pdh ) ) then
			SendNDatagram( false, mtACK, pdh^.Signature.Date, pdh^.Signature.Time,
				ui.SlotName, ui.ServerName );
		DoDispatchMail( pdh );
	end;
end;

procedure TKMailSlot.Process_SDatagram( Data: PKSDatagram );
var
	rcv: TKReceiverSession;
begin
{ if Echo is OFF, the transmissionID will not be found }
	rcv := FReceivers.SessionsFromID[Data^.TransmissionID];
	if CheckObject( rcv ) then
		rcv.ReceiveDatagram( Data );
end;

procedure TKMailSlot.EndOfSegmentation( ASession: TKReceiverSession );
begin
	SendNDatagram( false, mtERN, LongInt( ASession.DataComplete ),
		ASession.TransmissionID, ASession.SlotName, ASession.ServerName );
	FReceivers.Remove( ASession );
	DoDataArrive( ASession );
	ASession.Free;
end;

procedure TKMailSlot.Sync( const ToSlot, ToServer: string; Confirmed: Boolean;
	Control_01, Control_02: LongInt );
begin
	ForceTrimStrs( [ToSlot, ToServer] );
	SendNDatagram( Confirmed, mtSYNC, Control_01, Control_02, ToSlot, ToServer );
end;

procedure TKMailSlot.SendControl( const ToSlot, ToServer: string; Confirmed: Boolean;
	Control_01, Control_02: LongInt );
begin
	ForceTrimStrs( [ToSlot, ToServer] );
	SendNDatagram( Confirmed, mtControl, Control_01, Control_02, ToSlot, ToServer );
end;

function TKMailSlot.InternalSendData( const ToSlot, ToServer: string; Confirmed: Boolean;
	Data: Pointer; Size: LongInt ): LongInt;
var
	pp: Pointer;
	i,
	nBlocks: Cardinal;
	psd: PKSDatagram;
begin
	ForceTrimStrs( [ToSlot, ToServer] );
	Result := GetTickCount;
	SendNDatagram( Confirmed, mtSOB, Size, Result, ToSlot, ToServer );
	Size := TurnBitOff( Size, 31 );
	Size := TurnBitOff( Size, 30 );
	nBlocks := ( Size div MAX_SDATA_SIZE ) + Ord( ( Size mod MAX_SDATA_SIZE ) > 0 );
	pp := Data;
	GetMem( psd, SizeOf( TKSDatagram ) );
	try
		for i := 1 to nBlocks do
		begin
			ZeroMemory( psd, SizeOf( TKSDatagram ) );
			psd^.TransmissionID := Result;
			psd^.SequenceNo := i;
{ if DataSize = 0, it will be interpreted as 256 in the reader routine }
			if ( i = nBlocks ) then
			begin
				psd^.DataSize := ( Size mod MAX_SDATA_SIZE );
				Move( pp^, psd^.Data, psd^.DataSize );
			end
			else
			begin
				Move( pp^, psd^.Data, MAX_SDATA_SIZE );
				pp := IncPtr( pp, MAX_SDATA_SIZE );
			end;
			SendSDatagram( psd, ToSlot, ToServer );
			if ( ( i mod FBurstCount ) = 0 ) then
				Sleep( FBurstDelay );
		end;
	finally
		FreeMem( psd, SizeOf( TKSDatagram ) );
	end;
	SendNDatagram( Confirmed, mtEOB, LongInt( true ), Result, ToSlot, ToServer );
end;

function TKMailSlot.SendData( const ToSlot, ToServer: string; Confirmed: Boolean;
	Data: Pointer; Size: TKMSDataSize ): LongInt;
begin
	Result := -1;
	if CheckPointer( Data ) then
		Result := InternalSendData( ToSlot, ToServer, Confirmed, Data, Size );
end;

function TKMailSlot.SendText( const ToSlot, ToServer: string; Confirmed: Boolean;
	const Data: string ): LongInt;
var
	sz: LongInt;
begin
	Result := -1;
	if CheckStr( Data ) then
	begin
		sz := Length( Data );
		if ( sz <= $3FFFFFFF ) then
		begin
			sz := TurnBitOn( sz, 31 );
			Result := InternalSendData( ToSlot, ToServer, Confirmed, Pointer( Data ), sz );
		end;
	end;
end;

function TKMailSlot.SendFile( const ToSlot, ToServer: string; Confirmed: Boolean;
	const FileName: TFileName ): LongInt;
var
	sz: LongInt;
	sName: string;
	fs: TFileStream;
	ms: TMemoryStream;
begin
	Result := -1;
	if CheckFile( FileName ) then
	begin
		ForceTrimStrs( [ToSlot, ToServer] );
		fs := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
		try
			sName := ExtractFileName( FileName ) + CH_NULL;
			sz := fs.Size + Length( sName );
			if ( sz <= $3FFFFFFF ) then
			begin
				sz := TurnBitOn( sz, 30 );
				ms := TMemoryStream.Create;
				try
					ms.WriteBuffer( Pointer( sName )^, Length( sName ) );
					ForceStreamCopy( fs, ms );
					Result := InternalSendData( ToSlot, ToServer, Confirmed, ms.Memory, sz );
				finally
					ms.Free;
				end;
			end;
		finally
			fs.Free;
		end;
	end;
end;

procedure TKMailSlot.InternalBroadcast( const ToSlot: string; AType: TKMailType;
	Confirmed: Boolean; Control_01, Control_02: LongInt; Data: Pointer;
	DataSize: LongInt );
var
	i: Integer;
begin
	FDomains.Clear;
	case FBroadcastMode of
		bmDefault:
			FDomains.Add( MAILSLOT_DEFAULT_DOMAIN_BROADCAST );
		bmDomain:
			FDomains.Add( FDomain );
		bmAllDomains:
			uksyUtils.GetDomainNames( FDomains );
	end;
	for i := 0 to FDomains.Count - 1 do
		case AType of
			mtSYNC:
				Sync( ToSlot, FDomains[i], Confirmed, Control_01, Control_02 );
			mtControl:
				SendControl( ToSlot, FDomains[i], Confirmed, Control_01, Control_02 );
			mtSOB:
        InternalSendData( ToSlot, FDomains[i], Confirmed, Data, DataSize );
		else
			RaiseException( EKMailSlot, sErrMSBroadCastError );
		end;
end;

procedure TKMailSlot.BroadcastSync( const ToSlot: string; Confirmed: Boolean;
	Control_01, Control_02: LongInt );
begin
	InternalBroadcast( ToSlot, mtSYNC, Confirmed, Control_01, Control_02, nil, 0 );
end;

procedure TKMailSlot.BroadcastControl( const ToSlot: string; Confirmed: Boolean;
	Control_01, Control_02: LongInt );
begin
	InternalBroadcast( ToSlot, mtControl, Confirmed, Control_01, Control_02, nil, 0 );
end;

procedure TKMailSlot.BroadcastData( const ToSlot: string; Confirmed: Boolean;
	Data: Pointer; Size: TKMSDataSize );
begin
	if CheckPointer( Data ) then
		InternalBroadcast( ToSlot, mtSOB, Confirmed, 0, 0, Data, Size );
end;

procedure TKMailSlot.BroadcastText( const ToSlot: string; Confirmed: Boolean;
	const Data: string );
var
	sz: LongInt;
begin
	if CheckStr( Data ) then
	begin
		sz := Length( Data );
		if ( sz <= $3FFFFFFF ) then
		begin
			sz := TurnBitOn( sz, 31 );
			InternalBroadcast( ToSlot, mtSOB, Confirmed, 0, 0, Pointer( Data ), sz );
		end;
	end;
end;

procedure TKMailSlot.BroadcastFile( const ToSlot: string; Confirmed: Boolean;
	const FileName: TFileName );
var
	sz: LongInt;
	sName: string;
	fs: TFileStream;
	ms: TMemoryStream;
begin
	if CheckFile( FileName ) then
	begin
		ForceTrimStr( ToSlot );
		fs := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
		try
			sName := ExtractFileName( FileName ) + CH_NULL;
			sz := fs.Size + Length( sName );
			if ( sz <= $3FFFFFFF ) then
			begin
				sz := TurnBitOn( sz, 30 );
				ms := TMemoryStream.Create;
				try
					ms.WriteBuffer( Pointer( sName )^, Length( sName ) );
					ForceStreamCopy( fs, ms );
					InternalBroadcast( ToSlot, mtSOB, Confirmed, 0, 0, ms.Memory, sz );
				finally
					ms.Free;
				end;
			end;
		finally
			fs.Free;
		end;
	end;
end;

procedure TKMailSlot.MailCheck( Sender: TObject );
var
	i,
	j: Integer;
	dt: TDateTime;
	pdhRCV,
	pdhACK: PKNDatagramHeader;
begin
{ eliminate nil pointers }
	FNeedACK.Pack;
	FReceived.Pack;
	FReceivers.Pack;

{ if there are no messages in the internal queues, Exit }
	if ( FNeedACK.Count = 0 ) and
		 ( FReceived.Count = 0 ) and
		 ( FReceivers.Count = 0 ) then
		Exit;

{
	Currently, ACKs are queued in the FReceived list together with
	other kinds of messages; to verify if any message has already been
	ACKed, it is necessary to run a search loop in the FReceived list
	and look for the corresponding ACK;
}
	for i := 0 to FNeedACK.Count - 1 do
	begin
		pdhACK := PKNDatagramHeader( FNeedACK.Items[i] );
		if CheckPointer( pdhACK ) then
			for j := 0 to FReceived.Count - 1 do
			begin
				pdhRCV := PKNDatagramHeader( FReceived.Items[j] );
				if ( not CheckPointer( pdhRCV ) ) then
					Break;
				if ( GetMailType( pdhRCV^.DatagramType ) = mtACK ) then
{	compare its signature with the original message's signature for a match;
	in the ACK N-Datagram, the original signature timestamp can be found in
	Control_01 and Control_02 fields. }
					if ( pdhACK^.Signature.Date = pdhRCV^.Control_01 ) and
						 ( pdhACK^.Signature.Time = pdhRCV^.Control_02 ) then
{	if this is a match, found a matching ACK for a message in the FNeedACK list;
	it is time to remove this message from the FNeedACK list }
					begin
{ dispatch an event to inform of the just received ACK }
						DelayReceiveACK( j, pdhACK^.DatagramType );
						FreeMem( pdhACK );
						FNeedACK.Items[i] := nil;
						Break;
					end;
			end;
	end;
	FNeedACK.Pack;

{ check for TIMED OUT messages }
	dt := Now;
	with FNeedACK do
		for i := 0 to Count - 1 do
			if CheckPointer( Items[i] ) then
				with PKNDatagramHeader( Items[i] )^ do
					if ( MSecondsBetween( TimeStampToDateTime( Signature ), dt ) >= FTimeOut ) then
						DelayTimeOut( i );
	FNeedACK.Pack;

{ check for TIMED OUT transmissions }
	with FReceivers do
		for i := Count - 1 downto 0 do
{ Here we must TypeCast FTimeOut to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, the expression
	also returns Integer ranged values... }
			if ( ( GetTickCount - Cardinal( Sessions[i].IdleSince ) ) >= FTimeOut ) then
				EndOfSegmentation( Sessions[i] );
end;

{--------------------------------- TKAppControl --------------------------------}

const

	SHOW_APP_TASKBAR: array[Boolean] of Integer = ( SWP_HIDEWINDOW, SWP_SHOWWINDOW );

	AS_NULL = $00;          { AppSpy_Null }
	AS_ADDINSTANCE = $01; 	{ AppSpy_AddInstance }
	AS_QUERYINSTANCE = $02; { AppSpy_QueryInstance }

var
	KM_INSTANCEFOUND,
	KM_FINDINSTANCES,
	KM_NETINSTANCES,
	KM_OPENMAILSLOT,
	KM_COUNTNETINSTANCES: Cardinal;

constructor TKAppControl.Create( AOwner: TComponent );
begin
	ForceSingleton( AOwner, TKAppControl );
	inherited Create( AOwner );
	FTerminated := False;
	FAutoRegistry := True;
	FRootKey := rkCurrentUser;
	FRegistryRoot := APPCTRL_BASEROOT;
	FWaitTimer := nil;
	msMailSlot := nil;
	FOldInstance := 0;
	FSplashWait := 3000;
	if ( not Designing( Self ) ) then
		with Application do
		begin
			OnActivate := DoActivate;
			OnDeactivate := DoDeactivate;
			OnMaximize := DoMaximize;
			OnMinimize := DoMinimize;
			OnRestore := DoRestore;
			OnHelp := DoHelp;
			OnHint := DoHint;
			OnIdle := DoIdle;
			OnShowHint := DoShowHint;
			msMailSlot := TKMailSlot.Create( nil );
			msMailSlot.BroadcastMode := bmAllDomains;
			msMailSlot.OnReceiveControl := MailReceiveControl;
			KM_NETINSTANCES := RegisterWindowMessage( 'KM_NETINSTANCES' );
			KM_OPENMAILSLOT := RegisterWindowMessage( 'KM_OPENMAILSLOT' );
			KM_INSTANCEFOUND := RegisterWindowMessage( 'KM_INSTANCEFOUND' );
			KM_FINDINSTANCES := RegisterWindowMessage( 'KM_FINDINSTANCES' );
			KM_COUNTNETINSTANCES := RegisterWindowMessage( 'KM_COUNTNETINSTANCES' );
			Application.HookMainWindow( AppMessages );
		end;
	FWaitTimer := TTimer.Create( nil );
	with FWaitTimer do
	begin
		Enabled := False;
		Interval := 500;
		OnTimer := EndSplashWait;
	end;
	FInternalID := GetTickCount;
	if Designing( Self ) then
		FBuiltInName := ChangeFileExt( ExtractFileName( ToolServices.GetProjectName ), '.exe' )
	else
	begin
		FTitle := Application.Title;
		FRootDir := ApplicationPath;
	end;
	FShowMainForm := true;
	FShowOnTaskbar := true;
	FNetworkSearch := true;
	FInstancesAllowed := 0;
	FHintColor := Application.HintColor;
	FHintPause := Application.HintPause;
	FHintHidePause := Application.HintHidePause;
	FHintShortPause := Application.HintShortPause;
	FRegistryItems := TStringList.Create;
end;

destructor TKAppControl.Destroy;
var
	bMailActive: Boolean;
begin
	if ( not Designing( Self ) ) then
		Application.UnhookMainWindow( AppMessages );
	bMailActive := False;
	FWaitTimer.Free;
	if CheckObject( msMailSlot ) then
	begin
		bMailActive := msMailSlot.Active;
		FreeClean( msMailSlot );
	end;
	if ( bMailActive ) then
		SendMessage( HWND_BROADCAST, KM_OPENMAILSLOT, 0, Application.Handle );
{ Check FRegistryItems for a singleton error (the object will not be created) }
	if CheckObject( FRegistryItems ) and AutoRegistry and ( not Designing( Self ) ) then
	begin
		AddDefaultRegistryItems;
		WriteRegistry;
	end;
	FRegistryItems.Free;
	inherited Destroy;
end;

function TKAppControl.AppMessages( var Message: TMessage ): Boolean;
begin
	Result := false;
	if ( Message.Msg = KM_OPENMAILSLOT ) then
	begin
		Result := true;
		if ( Application.Handle <> Cardinal( Message.LParam ) ) and CheckObject( msMailSlot ) then
			msMailSlot.Open;
	end
	else if ( Message.Msg = KM_INSTANCEFOUND ) then
	begin
		Result := true;
		if ( Cardinal( Message.LParam ) <> Application.Handle ) then // Added to test. /;
			FOldInstance := Message.lParam;
		Inc( FLocalInstances );
	end
	else if ( Message.Msg =	KM_FINDINSTANCES ) then
	begin
		Result := true;
		with Application do
			SendMessage( Message.lParam, KM_INSTANCEFOUND, 0, Handle );
	end
	else if ( Message.Msg =	KM_NETINSTANCES ) then
	begin
		Result := true;
		FNetInstances := Message.lParam;
	end
	else if ( Message.Msg =	KM_COUNTNETINSTANCES ) then
	begin
		Result := true;
		if msMailSlot.Active then
			SendMessage( Message.lParam, KM_NETINSTANCES, 0, NetInstances );
	end
	else  if ( Message.Msg = WM_WINDOWPOSCHANGING ) and ( not ShowOnTaskbar ) then
		with PWindowPos( Message.lParam )^ do
			Flags := SWP_HIDEWINDOW;
end;

procedure TKAppControl.ShowSplashScreen( SplashClass: TFormClass );
var
	frmSplash: TForm;
	bCancelWait: Boolean;

	procedure SplashWait;
	begin
		with FWaitTimer do
		begin
			Enabled := True;
			while Enabled do
				Application.ProcessMessages;
		end;
	end;

begin
	if FTerminated then
		Exit;
	SetCapture( Application.Handle );
	try
		Screen.Cursor := crHourglass;
		try
		frmSplash := SplashClass.Create( nil );
			try
				frmSplash.FormStyle := fsStayOnTop;
				frmSplash.Show;
				frmSplash.Update;
				if ( FShowMainForm and ( Application.ShowMainForm ) and CheckObject( Application.MainForm ) ) then
					with Application.MainForm do
					begin
						Enabled := False;
						Show;
						Update;
					end;
				Application.ProcessMessages;
				bCancelWait := False;
				if Assigned( FInitEnvironment ) then
					FInitEnvironment( Self, bCancelWait );
				if ( FInstancesAllowed > 0 ) and ( not InstanceCheck ) then
				begin
					frmSplash.Hide;
					ShowDialogFmt( Title, sErrAPPInvSimultInstances, nil, dsOk, boExclamation01,
						[InstancesAllowed, FNetInstances, FLocalInstances - 1] );
					GotoPreviousInstance;
					Application.Terminate;
					FTerminated := True;
				end;
				if ( FInstancesAllowed = 0 ) and ( not bCancelWait ) and ( not FTerminated ) then
					SplashWait;
				if ( FShowMainForm and ( Application.ShowMainForm ) and CheckObject( Application.MainForm ) ) then
					Application.MainForm.Enabled := True;
			finally
				frmSplash.Free;
			end;
		finally
			Screen.Cursor := crDefault;
		end;
	finally
		ReleaseCapture;
	end;
end;

procedure TKAppControl.GotoPreviousInstance;
begin
	if ( FOldInstance <> 0 ) then
	begin
		if IsIconic( FOldInstance ) then
			SendMessage( FOldInstance, WM_SYSCOMMAND, SC_RESTORE, 0 )
		else
			SetForeGroundWindow( FOldInstance );
	end;
end;

procedure TKAppControl.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( AComponent <> Self ) and
		 ( Operation = opInsert ) and
		 CheckObjectClass( AComponent, TKAppControl ) then
		RaiseException( EKEAppControl, sErrAPPOnlyOne );
end;

procedure TKAppControl.Loaded;
begin
	inherited Loaded;
	if ( not Designing( Self ) ) then
	begin
		if CheckStr( BuiltInName ) and ( not CheckStrEqual( ExtractFileName( Application.EXEName ), BuiltInName ) ) then
		begin
			Application.ShowMainForm := false;
			FTerminated := True;
			FatalError( sErrAPPIllegalRename );
		end;
		with msMailSlot do
		begin
			Slot := APP_CTRL_MS_PREFIX + Self.Name;
			Open;
		end;
		CheckInstancesAllowed;
		if AutoRegistry then
			ReadRegistry;
	end;
end;

procedure TKAppControl.AddDefaultRegistryItems;
begin
	RegistryItems.Insert( 0, APPCTRL_BUILTNAME + CH_EQUAL_TOKEN + BuiltInName );
	RegistryItems.Insert( 1, APPCTRL_ROOTDIR + CH_EQUAL_TOKEN + RootDir );
	RegistryItems.Insert( 2, APPCTRL_INTERNALID + CH_EQUAL_TOKEN + IntToHex( InternalID, 8 ) );
	RegistryItems.Insert( 3, APPCTRL_TITLE + CH_EQUAL_TOKEN + Title );
	RegistryItems.Insert( 4, APPCTRL_LASTUSER + CH_EQUAL_TOKEN + UserName );
	RegistryItems.Insert( 5, APPCTRL_LASTUSED + CH_EQUAL_TOKEN +
		FormatDateTime( ShortDateFormat + CH_SPACE + ShortTimeFormat, Now ) );
end;

procedure TKAppControl.DoReadRegistry;
begin
	if Assigned( FOnReadRegistry ) then
		FOnReadRegistry( Self );
end;

procedure TKAppControl.DoWriteRegistry;
begin
	if Assigned( FOnWriteRegistry ) then
		FOnWriteRegistry( Self );
end;

procedure TKAppControl.ReadRegistry;
begin
	if ( not ReadRegistryStrings( RootKey, False, RegistryRoot, RegistryItems ) ) then
	begin
		AddDefaultRegistryItems;
		WriteRegistryStrings( RootKey, True, RegistryRoot, RegistryItems );
	end;	
	DoReadRegistry;
end;

procedure TKAppControl.WriteRegistry;
begin
	WriteRegistryStrings( RootKey, True, RegistryRoot, RegistryItems );
	DoWriteRegistry;
end;

procedure TKAppControl.CheckInstancesAllowed;
begin
	if ( FInstancesAllowed > 0 ) and ( not InstanceCheck ) then
	begin
		ShowDialogFmt( Title, sErrAPPInvSimultInstances, nil, dsOk, boExclamation01,
			[InstancesAllowed, FNetInstances, FLocalInstances - 1] );
		GotoPreviousInstance;
		Application.Terminate;
		FTerminated := True;
	end;
end;

function TKAppControl.InstanceCheck: Boolean;
var
	iTotalInstances: Integer;
begin
{ Local instance count }
	iTotalInstances := LocalInstances;
{ Network instance count }
	if NetworkSearch then
		Inc( iTotalInstances, NetInstances );
{ Instances allowed }
	Result := ( iTotalInstances <= InstancesAllowed );
end;

function TKAppControl.DoHelp( Command: Word; Data: Longint; var CallHelp: Boolean ): Boolean;
begin
	Result := Assigned( FOnHelp );
	if Result then
		Result := FOnHelp( Command, Data, CallHelp );
end;

procedure TKAppControl.DoMaximize( Sender: TObject );
begin
	if Assigned( FOnMaximize ) then
		FOnMaximize( Sender );
end;

procedure TKAppControl.DoMinimize( Sender: TObject );
begin
	if Assigned( FOnMinimize ) then
		FOnMinimize( Sender );
end;

procedure TKAppControl.DoRestore( Sender: TObject );
begin
	if Assigned( FOnRestore ) then
		FOnRestore( Sender );
end;

procedure TKAppControl.DoActivate( Sender: TObject );
begin
	if Assigned( FOnActivate ) then
		FOnActivate( Sender );
end;

procedure TKAppControl.DoDeactivate( Sender: TObject );
begin
	if Assigned( FOnDeactivate ) then
		FOnDeactivate( Sender );
end;

procedure TKAppControl.DoHint( Sender: TObject );
begin
	if Assigned( FOnHint ) then
		FOnHint( Sender );
end;

procedure TKAppControl.DoIdle( Sender: TObject; var Done: Boolean );
begin
	if Assigned( FOnIdle ) then
		FOnIdle( Sender, Done );
end;

procedure TKAppControl.DoShowHint( var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo );
begin
	if Assigned( FOnShowHint ) then
		FOnShowHint( HintStr, CanShow, HintInfo );
end;

procedure TKAppControl.SetRegistryRoot( const Value: string );
begin
	if ( not CheckStrEqual( Value, FRegistryRoot ) ) then
		FRegistryRoot := Value;
end;

procedure TKAppControl.SetRegistryItems( Value: TStrings );
begin
	FRegistryItems.Assign( Value );
end;

procedure TKAppControl.SetHintColor( Value: TColor );
begin
	if not ( csDesigning in ComponentState ) then
		Application.HintColor := Value;
	FHintColor := Value;
end;

procedure TKAppControl.SetTitle( const Value: string );
begin
	if ( not Designing( Self ) ) then
		Application.Title := Value;
	FTitle := Value;
end;

procedure TKAppControl.SetRootDir( const Value: string );
begin
	if ( not ( Designing( Self ) or Loading( Self ) ) ) then
		Exit;
	if ( not CheckStrEqual( Value, FBuiltInName ) ) then
		FRootDir := Value;
end;

procedure TKAppControl.SetBuiltInName( const Value: string );
begin
	if ( not ( Designing( Self ) or Loading( Self ) ) ) then
		Exit;
	if ( not CheckStrEqual( Value, FBuiltInName ) ) then
		FBuiltInName := Value;
end;

procedure TKAppControl.SetHintPause( Value: Integer );
begin
	if ( not Designing( Self ) ) then
		Application.HintPause := Value;
	FHintPause := Value;
end;

procedure TKAppControl.SetShowMainForm( Value: Boolean );
begin
	if ( not Designing( Self ) ) then
		Application.ShowMainForm := Value;
	FShowMainForm := Value;
end;

procedure TKAppControl.SetHintHidePause( Value: Integer );
begin
	if ( not Designing( Self ) ) then
		Application.HintHidePause := Value;
	FHintHidePause := Value;
end;

procedure TKAppControl.SetShowOnTaskbar( Value: Boolean );
begin
	if ( FShowOnTaskbar <> Value ) then
	begin
		FShowOnTaskbar := Value;
		if ( not Designing( Self ) ) then
			SetWindowPos( Application.Handle, 0, GetSystemMetrics( SM_CXSCREEN ) div 2,
				GetSystemMetrics( SM_CYSCREEN ) div 2, 0, 0, SHOW_APP_TASKBAR[FShowOnTaskbar] );
	end;
end;

procedure TKAppControl.SetHintShortPause( Value: Integer );
begin
	if ( not Designing( Self ) ) then
		Application.HintShortPause := Value;
	FHintShortPause := Value;
end;

function TKAppControl.GetNetTimeOut: Cardinal;
begin
	Result := FWaitTimer.Interval;
end;

procedure TKAppControl.SetNetTimeOut( Value: Cardinal );
begin
	if ( Value > 50 ) then
		FWaitTimer.Interval := Value
	else
		FWaitTimer.Interval := 500;
end;

function TKAppControl.GetLocalInstances: Integer;
begin
	FOldInstance := 0;
	FLocalInstances := 0;
	SendMessage( HWND_BROADCAST, KM_FINDINSTANCES, 0, Application.Handle );
	Result := FLocalInstances;
end;

function TKAppControl.GetNetInstances: Integer;
begin
	FNetInstances := 0;
	if ( msMailSlot.Active ) then
	begin
		FWaitTimer.Enabled := true;
		msMailSlot.BroadcastControl( msMailSlot.Slot, false, AS_QUERYINSTANCE, AS_NULL );
		while FWaitTimer.Enabled do
			Application.ProcessMessages;
	end
	else
		SendMessage( HWND_BROADCAST, KM_COUNTNETINSTANCES, 0, Application.Handle );
	Result := FNetInstances;
end;

procedure TKAppControl.EndSplashWait( Sender: TObject );
begin
	FWaitTimer.Enabled := False;
end;

procedure TKAppControl.MailReceiveControl( Sender: TKMailSlot; Signature: TDateTime;
	UserInfo: TKMSUserInfo; Control_01, Control_02: LongInt );
begin
	case Control_01 of
		AS_ADDINSTANCE:
			Inc( FNetInstances, Control_02 );
		AS_QUERYINSTANCE:
			if ( UserInfo.ServerName <> ComputerName ) then
				msMailSlot.SendControl( UserInfo.SlotName, UserInfo.ServerName,
					false, AS_ADDINSTANCE, LocalInstances );
	end;
end;

{
--------------------------------------------------------------------------------
----------------------------- TKPerformanceObjects -----------------------------
--------------------------------------------------------------------------------
}

{ TKW95PerformanceItem }

const
  COUNTER_INFO_COUNT = 3;
  COUNTER_DESCRIPTION = 'Description';
  COUNTER_DIFFERENTIATE = 'Differentiate';
  COUNTER_NAME = 'Name';

  STAT_START = 'StartStat';
  STAT_STOP  = 'StopStat';
  STAT_DATA  = 'StatData';

constructor TKW95PerformanceItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKW95Performance );
	FEnabled := true;
  FCounterInfos := TStringList.Create;
	inherited Create( ACollection );
	ForceObject( Owner.Component );
end;

destructor TKW95PerformanceItem.Destroy;
begin
  FCounterInfos.Free;
  inherited Destroy;
end;

procedure TKW95PerformanceItem.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKW95PerformanceItem ) then
    with ( Source as TKW95PerformanceItem ) do
    begin
      ObjectName  := Self.FObjectName;
      CounterName := Self.FCounterName;
      AddInfo.Assign( Self.AddInfo );
      Enabled     := Self.FEnabled;
		end
	else
		inherited Assign( Source );
end;

function TKW95PerformanceItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
  with ( Item as TKW95PerformanceItem ) do
    Result := inherited Equals( Item ) and
			CheckStrEqual( Self.FObjectName, ObjectName ) and
			CheckStrEqual( Self.FCounterName, CounterName ) and
      Self.FCounterInfos.Equals( AddInfo );
end;

function TKW95PerformanceItem.GetOwnerCollection: TKW95Performance;
begin
 	Result := TKW95Performance( inherited GetOwnerCollection );
end;

procedure TKW95PerformanceItem.ClearCounter;
begin
{ First at all let the last event occours... }
  Enabled := False;
	FCounterName := '';
  FCounterInfos.Clear;
  FData := 0;
  FStatOption := soStopInfo;
end;

procedure TKW95PerformanceItem.SetData( Value: Integer; StatOption: TKStatOption );
begin
  FData := Value;
  FStatOption := StatOption;
  if Assigned( FStatEvent ) and ( not Destroying( Owner.Component ) ) then
    FStatEvent( Owner.Component, Self, StatOption, CounterDifferentiate );
end;

procedure TKW95PerformanceItem.SetCounterInfo( const Value: string );
var
  i: Integer;
begin
	ForceTrimStr( FObjectName );
	if ( not CheckTrimStr( Value ) ) then
    Exit;
	Owner.Component.Registry.RootKey := HKEY_LOCAL_MACHINE;
  if ( not Owner.Component.Registry.OpenKey( sW95PerfObjKey + '\' + FObjectName + '\' + Value, False ) ) then
    uksyUtils.RaiseExceptionFmt( EKW95Performance, sErrW95InvCounterName, [FObjectName + '\' + Value] );
  try
    ClearCounter;
    FCounterName := Value;
    Owner.Component.Registry.GetValueNames( FCounterInfos );
    ForceStrings( FCounterInfos );
    if ( FCounterInfos.Count < COUNTER_INFO_COUNT ) then
      uksyUtils.RaiseExceptionFmt( EKW95Performance, sErrW95InvCounterInfos, [
        InternalCounterName] );
    for i := 0 to FCounterInfos.Count - 1 do
			FCounterInfos.Strings[i] := FCounterInfos.Strings[i] + CH_EQUAL_TOKEN +
        Owner.Component.Registry.ReadString( FCounterInfos.Strings[i] );
    Enabled := true;
  finally
    Owner.Component.Registry.CloseKey;
	end;
end;

procedure TKW95PerformanceItem.SetObjectInfo( const Value: string );
var
	sl: TStrings;
begin
  if ( not CheckTrimStr( Value ) ) then
    Exit;
  Owner.Component.Registry.RootKey := HKEY_LOCAL_MACHINE;
  sl := TStringList.Create;
  try
    if ( not Owner.Component.Registry.OpenKey( sW95PerfObjKey + '\' + Value, False ) ) then
      uksyUtils.RaiseExceptionFmt( EKW95Performance, sErrW95InvObjectName, [Value] );
    try
      ClearCounter;
      FObjectName := Value;
{ Set the first counter as default ... }
      Owner.Component.Registry.GetKeyNames( sl );
    finally
			Owner.Component.Registry.CloseKey;
    end;
    if CheckStrings( sl ) then
			SetCounterInfo( sl.Strings[0] );
  finally
		sl.Free;
	end;
end;

procedure TKW95PerformanceItem.SetEnabled( Value: Boolean );
begin
  if ( FEnabled <> Value ) then
  begin
    if ( Value and ( not CheckTrimStrs( [FObjectName, FCounterName] ) ) ) then
      uksyUtils.RaiseExceptionFmt( EKW95Performance, sErrW95PerfInvEnable, [Name] );
    FEnabled := Value;
    Changed( False );
  end;
end;

procedure TKW95PerformanceItem.SetObjectName( const Value: string );
begin
  if ( Designing( Owner.Component ) or Loading( Owner.Component ) ) then
	begin
		if ( not CheckStrEqual( Value, FObjectName ) ) then
		begin
			SetObjectInfo( Value );
      Changed( False );
    end;
  end
  else
    uksyUtils.RaiseException( EKW95Performance, sErrW95PerfInvRunTimeProp );
end;

function TKW95PerformanceItem.GetInternalCounterName: string;
begin
  ForceTrimStrs( [FObjectName, FCounterName] );
  Result := FObjectName + '\' + FCounterName;
end;

procedure TKW95PerformanceItem.SetCounterName( const Value: string );
begin
  if ( Designing( Owner.Component ) or Loading( Owner.Component ) ) then
  begin
		if ( not CheckStrEqual( Value, FCounterName ) ) then
    begin
			SetCounterInfo( Value );
			Changed( False );
    end;
	end
  else
    uksyUtils.RaiseException( EKW95Performance, sErrW95PerfInvRunTimeProp );
end;

function TKW95PerformanceItem.GetCounterInf: string;
begin
  if CheckStrings( FCounterInfos ) then
    Result := FCounterInfos.Values[COUNTER_NAME]
  else
		Result := '';
end;

procedure TKW95PerformanceItem.SetCounterInf( const Value: string );
begin
  if ( not Designing( Owner.Component ) ) then
		uksyUtils.RaiseException( EKW95Performance, sErrW95PerfInvRunTimeProp );
end;

function TKW95PerformanceItem.GetCounterDescription: string;
begin
	if CheckStrings( FCounterInfos ) then
		Result := FCounterInfos.Values[COUNTER_DESCRIPTION]
  else
    Result := '';
end;

procedure TKW95PerformanceItem.SetCounterDescription( const Value: string );
begin
  if ( not Designing( Owner.Component ) ) then
		uksyUtils.RaiseException( EKW95Performance, sErrW95PerfInvRunTimeProp );
end;

function TKW95PerformanceItem.GetCounterDifferentiate: Boolean;
begin
  Result := False;
  if CheckStrings( FCounterInfos ) then
		Result := CheckStrEqual( FCounterInfos.Values[COUNTER_DIFFERENTIATE], BOOL_NAME[true] );
end;

procedure TKW95PerformanceItem.SetCounterDifferentiate( Value: Boolean );
begin
  if ( not Designing( Owner.Component ) ) then
    uksyUtils.RaiseException( EKW95Performance, sErrW95PerfInvRunTimeProp );
end;

{ TKW95Performance }

constructor TKW95Performance.Create( AComp: TKPerformanceObjects );
begin
	ForceObject( AComp );
	inherited Create( AComp, TKW95PerformanceItem, False );
end;

function TKW95Performance.Add: TKW95PerformanceItem;
begin
  Result := TKW95PerformanceItem( inherited Add );
end;

function TKW95Performance.GetItem( Index: Integer ): TKW95PerformanceItem;
begin
	Result := TKW95PerformanceItem( inherited GetItem( Index ) );
end;

function TKW95Performance.GetItemByName( const AName: string ): TKW95PerformanceItem;
begin
	Result := TKW95PerformanceItem( inherited GetItemByName( AName ) );
end;

function TKW95Performance.GetOwnerComp: TKPerformanceObjects;
begin
  Result := TKPerformanceObjects( inherited GetOwnerComp );
end;

procedure TKW95Performance.SetItem( Index: Integer; AItem: TKW95PerformanceItem );
begin
  inherited SetItem( Index, AItem );
end;

procedure TKW95Performance.Update( Item: TCollectionItem );
begin
	if CheckObject( Item ) then
		Component.UpdateItem( TKW95PerformanceItem( Item ) )
	else
		Component.UpdateItems; { Hard couple! }
end;

{ TKPerformanceObjects }

constructor TKPerformanceObjects.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	if ( not Designing( Self ) ) then
		ForceWinNT( false );
	FCollection := TKW95Performance.Create( Self );
	FRegistry := TRegistry.Create;
	FTimer := TTimer.Create( nil );
	FTimer.Interval := SECOND_TO_MSECOND;
	FTimer.OnTimer := TimerEvent;
	FTimer.Enabled := false;
end;

destructor TKPerformanceObjects.Destroy;
begin
	Enabled := false;
	FTimer.Free;
	FCollection.Free;
	FRegistry.Free;
	inherited Destroy;
end;

procedure TKPerformanceObjects.FinishStat;
var
	i: Integer;
begin
	for i := 0 to FCollection.Count - 1 do
		if FCollection.Items[i].Enabled then
		begin
			FCollection.Items[i].Enabled := False;
			FCollection.Items[i].FEnabled := True; { Hard Couple! }
		end;
end;

function TKPerformanceObjects.GetEnabled: Boolean;
begin
	Result := FTimer.Enabled;
end;

procedure TKPerformanceObjects.SetEnabled( Value: Boolean );
begin
	if ( FTimer.Enabled <> Value ) then
	begin
		FTimer.Enabled := Value;
		if ( not Value ) then
			FinishStat
		else
			UpdateItems;
	end;
end;

function TKPerformanceObjects.GetInterval: Cardinal;
begin
	Result := FTimer.Interval;
end;

procedure TKPerformanceObjects.SetInterval( Value: Cardinal );
begin
	if ( FTimer.Interval <> Value ) then
	begin
		Enabled := ( Value <> 0 );
		FTimer.Interval := Value;
	end;
end;

procedure TKPerformanceObjects.SetCollection( Value: TKW95Performance );
begin
	FCollection.Assign( Value );
end;

procedure TKPerformanceObjects.UpdateItems;
var
	i: Integer;
begin
	for i := 0 to FCollection.Count - 1 do
		UpdateItem( FCollection.Items[i] );
end;

procedure TKPerformanceObjects.UpdateItem( Item: TKW95PerformanceItem );
const
	DYN_DATA_STAT: array[Boolean] of string[9] = ( STAT_STOP, STAT_START );
	DYN_DATA_STAT_OPT: array[Boolean] of TKStatOption = ( soStopInfo, soStartInfo );
var
	iData: Integer;
begin
	if Designing( Self ) then
		Exit;
	Registry.RootKey := HKEY_DYN_DATA;
	Registry.OpenKey( sW95PerfDynKey + '\' + DYN_DATA_STAT[Item.Enabled], False );
	try
		iData := 0;
		Registry.ReadBinaryData( Item.InternalCounterName, iData, SizeOf( Integer ) );
		Item.SetData( iData, DYN_DATA_STAT_OPT[Item.Enabled] ); { Hard Couple! }
	finally
		Registry.CloseKey;
	end;
end;

procedure TKPerformanceObjects.TimerEvent( Sender: TObject );
var
	i,
	iData: Integer;
begin
	if Designing( Self ) then
		Exit;
	Registry.RootKey := HKEY_DYN_DATA;
	for i := 0 to Collection.Count - 1 do
		if Collection.Items[i].Enabled then
		begin
			Registry.OpenKey( sW95PerfDynKey + '\' + STAT_DATA, False );
			try
				iData := 0;
				Registry.ReadBinaryData( Collection.Items[i].InternalCounterName, iData,
					SizeOf( Integer ) );
				Collection.Items[i].SetData( iData, soGetInfo ); { Hard Couple! }
			finally
				Registry.CloseKey;
			end;
		end;
end;

{
--------------------------------------------------------------------------------
---------------------- Memory Mapped Files Implementation ----------------------
--------------------------------------------------------------------------------
}

const
  DEFAULT_MAPFILE_NAME = 'Default_Server';

function LocalThreadFunc( Parameter: Pointer ): Integer;
begin
	Result := TKCustomFileMap( Parameter ).InternalThreadCallback;
end;

{ TKCustomFileMap }

destructor TKCustomFileMap.Destroy;
begin
	DoClose;
	inherited Destroy;
end;

constructor TKCustomFileMap.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ClearHandles;
	FSize := 128;
	FFileMapName := DEFAULT_MAPFILE_NAME;
end;

function TKCustomFileMap.GetData: PChar;
begin
	if ( not HasHandle ) then
		RaiseException( EKFileMap, sErrFMInvAvail );
	Lock;
	Result := FData;
end;

procedure TKCustomFileMap.DoClose;
begin
	if Assigned( FBeforeClose ) then
		FBeforeClose( Self );
	if HasHandle then
	begin
		CloseHandle( FThread );   // Kill the thread that responds to the write-event.
		CloseHandle( FMutex );    // Close the mutex.
		CloseHandle( FEvent );    // Close the event.
		UnmapViewOfFile( FData ); // unmap the shared buffer
		CloseHandle( FHandle );   // close the MMF
		ClearHandles;
	end;
	if Assigned( FAfterClose ) then
		FAfterClose( Self );
end;

function TKCustomFileMap.HasHandle: Boolean;
begin
	Result := ( FHandle <> NULL_HANDLE_VALUE );
end;

procedure TKCustomFileMap.Close;
begin
	DoClose;
end;

function TKCustomFileMap.IsOpen: Boolean;
begin
	Result := HasHandle;
end;

procedure TKCustomFileMap.SetSize( Size: LongInt );
begin
	if HasHandle then
		RaiseException( EKFileMap, sErrFMInvOperation );
	FSize := Size;
end;

procedure TKCustomFileMap.SetFileMapName( const MapName: string );
begin
	if HasHandle then
		RaiseException( EKFileMap, sErrFMInvOperation );
	FFileMapName := MapName;
end;

procedure TKCustomFileMap.Lock;
begin
	WaitForSingleObject( FMutex, INFINITE );
end;

procedure TKCustomFileMap.SetValue( Value: PChar; CountBytes: Integer );
var
	i: Integer;
begin
	Lock;                             // Get mutex access to MMF
	if ( CountBytes > FSize ) then    // Guarantee that the write
		CountBytes := FSize;            // process doesn't go over its boundaries.
	for i := 0 to CountBytes - 1 do   // Now copy the data in Value to FData
		FData[i] := Value[i];
	PulseEvent( FEvent );             // Tell everyone that the MMF has changed.
	Unlock;                           // Release the mutex.
end;

procedure TKCustomFileMap.Unlock;
begin
	ReleaseMutex( FMutex );
end;

procedure TKCustomFileMap.ClearHandles;
begin
  FEvent := NULL_HANDLE_VALUE;
	FMutex := NULL_HANDLE_VALUE;
	FThread := NULL_HANDLE_VALUE;
	FHandle := NULL_HANDLE_VALUE;
	FData := nil;
end;

function TKCustomFileMap.Open: Boolean;
var
	pcMap: PChar;
begin
	Result := HasHandle;
	if Result then
		RaiseException( EKFileMap, sErrFMInvOperation );
	ClearHandles;
	try
		FEvent := CreateEvent( nil, true, false, PChar( FileMapName + '_Event' ) );
		if ( FEvent = NULL_HANDLE_VALUE ) then
			RaiseExceptionFmt( EKFileMap, sErrInvEventCreate, [SysErrorMessage( GetLastError )] );
		FMutex := CreateMutex( nil, false, PChar( FileMapName + '_Mutex' ) );
		if ( FMutex = NULL_HANDLE_VALUE ) then
			RaiseExceptionFmt( EKFileMap, sErrInvMutexCreate, [SysErrorMessage( GetLastError )] );
		WaitForSingleObject( FMutex, INFINITE );
		try
			BeginThread( nil, 0, LocalThreadFunc, Self, 0, FThread );
			pcMap := StrAlloc( Length( FileMapName ) + 1 );
			try
				StrPCopy( pcMap, FileMapName );
				if Assigned( FBeforeOpen ) then
					FBeforeOpen( Self );
				FHandle := OpenFileMapping( FILE_MAP_READ + FILE_MAP_WRITE, false, pcMap );
				if ( not HasHandle ) then
					FHandle := CreateFileMapping( $FFFFFFFF, nil, FILE_MAP_READ + FILE_MAP_WRITE,
						0, FSize, pcMap );
			finally
				StrDispose( pcMap );
			end;
			Result := HasHandle;
			if ( not Result ) then
				RaiseException( EKFileMap, sErrFMInvCreate );
			FData := MapViewOfFile( FHandle, FILE_MAP_READ + FILE_MAP_WRITE, 0, 0, 0);
		finally
			ReleaseMutex( FMutex );
		end;
		if Assigned( FAfterOpen ) then
			FAfterOpen( Self );
	except
		on E: Exception do
		begin
			if CheckPointer( FData ) then
			begin
				UnmapViewOfFile( FData );
				FData := nil;
			end;
			if ( FHandle <> NULL_HANDLE_VALUE ) then
			begin
				CloseHandle( FHandle );
				FHandle := NULL_HANDLE_VALUE;
			end;
			if ( FThread <> NULL_HANDLE_VALUE ) then
			begin
				CloseHandle( FThread );
				FThread := NULL_HANDLE_VALUE;
			end;
			if ( FMutex <> NULL_HANDLE_VALUE ) then
			begin
				CloseHandle( FMutex );
				FMutex := NULL_HANDLE_VALUE;
			end;
			if ( FEvent <> NULL_HANDLE_VALUE ) then
			begin
				CloseHandle( FEvent );
				FEvent := NULL_HANDLE_VALUE;
			end;
		end;
	end;
	Result := HasHandle;
end;

function TKCustomFileMap.InternalThreadCallback: Integer;
begin
	while true do
	begin
		WaitForSingleObject( FEvent, INFINITE );
		if Assigned( FOnChange ) then
		begin
			Lock;
			try
				FOnChange( Self );
			finally
				Unlock;
			end;
		end;
	end;
	Result := 0;
end;

{
--------------------------------------------------------------------------------
--------------------- Directory Monitoring Implementation ----------------------
--------------------------------------------------------------------------------
}

{
	Baseado em código desenvolvido por François PIETTE;
	François PIETTE pode ser encontrado em um dos endereços:
		 francois.piette@ping.be
		 francois.piette@f2202.n293.z2.fidonet.org
		 http://www.rtfm.be/fpiette
}

{
	FILE_NOTIFY_CHANGE_FILE_NAME:
		Any filename change in the watched directory or subtree
		satisfies a change notification wait.	Changes include renames,
		creations, and deletions.

	FILE_NOTIFY_CHANGE_DIR_NAME:
		Any directory-name change in the watched directory or subtree
		satisfies a change notification wait. Changes include directory
		creations and deletions.

	FILE_NOTIFY_CHANGE_ATTRIBUTES:
		Any attribute change in the watched directory or subtree satisfies
		a change notification wait.

	FILE_NOTIFY_CHANGE_SIZE:
		Any file-size change in the watched directory or subtree satisfies
		a change notification wait. The operating system detects a change
		in file size only when the file is written to the disk. For operating
		systems that use extensive caching, detection occurs only when the
		cache is sufficiently flushed.

	FILE_NOTIFY_CHANGE_LAST_WRITE:
		Any change to the last write-time of files in the watched directory
		or subtree satisfies a change notification wait. The operating system
		detects a change to the last write-time only when the file is written
		to the disk. For operating systems that use extensive caching,
		detection occurs only when the cache is sufficiently flushed.

	FILE_NOTIFY_CHANGE_SECURITY:
		Any security-descriptor change in the watched directory or subtree
		satisfies a change notification wait.

}

const
	FILE_NOTIFY_FLAGS: Array[TKMonitorValue] of DWORD =
		( FILE_NOTIFY_CHANGE_FILE_NAME,  FILE_NOTIFY_CHANGE_DIR_NAME,
			FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
			FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY );

procedure Error( const Value: string );
begin
	RaiseExceptionFmt( EKMonitor, sErrMInvRunningChange, [Value] );
end;

{ Thread function used to monitor the directory }
function MonitorThread( Prm: Pointer ): Integer; { DWORD;
	stdcall; -> See CreateThread above to know why this calling convention was omitted.
              Seet also BeginThread en TThreadFunc!}
var
	err: DWORD;
	Hdl: THandle;
	Status: DWORD;
	pParams: PKMonitorParams;
	hObjects: array [0..1] of THandle;
begin
	if ( not CheckPointer( Prm ) ) then
		ExitThread( 1 );
	pParams := PKMonitorParams( Prm );
	with pParams^ do
		Hdl := FindFirstChangeNotification( Directory, false, NotifyFilter );
	if ( Hdl = INVALID_HANDLE_VALUE ) then
	begin
		err := GetLastError;
		EndThread( err ); { Just for VCL RTL Complient... }
		RaiseExceptionFmt( EKMonitor, sErrMFailFFCN, [SysErrorMessage( err )] );
	end;
	while true do
	begin
		hObjects[0] := Hdl;
		hObjects[1] := pParams.hMutex;
		Status := WaitForMultipleObjects( 2, @hObjects, false, INFINITE );
		if ( Status = WAIT_OBJECT_0 ) then
		begin
			PostMessage( pParams^.WindowHandle, KM_DIRCHANGED, 0, 0 );
			if ( not FindNextChangeNotification( Hdl ) ) then
			begin
				FindCloseChangeNotification( Hdl );
				EndThread( GetLastError ); { Just for VCL RTL Complient... }
			end;
		end;
		if ( Status = ( WAIT_OBJECT_0 + 1 ) ) then
		begin
			FindCloseChangeNotification( Hdl );
			ReleaseMutex( pParams^.hMutex );
			EndThread( 0 ); { Just for VCL RTL Complient... Use EndThread instead of ExitThread }
		end;
	end;
end;

{ TKCustomMonitor }

constructor TKCustomMonitor.Create( AOwner : TComponent );
begin
	inherited Create( AOwner );
	FWindowHandle := AllocateHWnd( WndProc );
	FNotifyFilter := FILE_NOTIFY_CHANGE_FILE_NAME;
end;

destructor TKCustomMonitor.Destroy;
begin
{ if there is a handle wait as long as it takes to release its ownership }
	if ( FThreadHandle <> NULL_HANDLE_VALUE ) then
	begin
		ReleaseMutex( FMutexHandle );
		WaitForSingleObject( FThreadHandle, INFINITE );
		FThreadHandle := NULL_HANDLE_VALUE;
		FThreadID := 0;
	end;
	if CheckPointer( FParamPtr ) then
	begin
		FreeMem( FParamPtr, SizeOf( TKMonitorParams ) );
		FParamPtr := nil;
	end;
{ Now it is time to close the mutex }
	if ( FMutexHandle <> NULL_HANDLE_VALUE ) then
	begin
		CloseHandle( FMutexHandle );
		FMutexHandle := NULL_HANDLE_VALUE;
	end;
	DeallocateHWnd( FWindowHandle );
	inherited Destroy;
end;

procedure TKCustomMonitor.WndProc( var Message: TMessage );
begin
	with Message do
	begin
		if ( Msg = KM_DIRCHANGED ) then
			DoDirChangedEvent
		else
			Result := DefWindowProc( Handle, Msg, wParam, lParam );
	end;
end;

procedure TKCustomMonitor.SetDirectory( const NewValue : string );
begin
	if ( NewValue = FDirectory ) then
	  Exit;
	if ( FThreadHandle <> NULL_HANDLE_VALUE ) then
		Error( sMDir );
	if CheckPath( NewValue ) then
		FDirectory := NewValue
	else
		RaiseExceptionFmt( EKMonitor, sErrMDirNotExists, [NewValue] );
end;

procedure TKCustomMonitor.SetNotifyFilter( NewValue : TKMonitorType );
var
	i: TKMonitorValue;
begin
	if ( FThreadHandle <> NULL_HANDLE_VALUE ) then
		Error( sErrMNotifyFilter );
	FNotifyFilter := 0;
	for i := Low( TKMonitorValue ) to High( TKMonitorValue ) do
		if ( i in NewValue ) then
			FNotifyFilter := ( FNotifyFilter or FILE_NOTIFY_FLAGS[i] );
end;

function TKCustomMonitor.GetNotifyFilter: TKMonitorType;
var
	i: TKMonitorValue;
begin
	Result := [];
	for i := Low( TKMonitorValue ) to High( TKMonitorValue ) do
		if ( ( FNotifyFilter and FILE_NOTIFY_FLAGS[i] ) = FILE_NOTIFY_FLAGS[i] ) then
			Result := Result + [i];
end;

procedure TKCustomMonitor.DoDirChangedEvent;
begin
	if Assigned( FDirChanged ) then
		FDirChanged( Self );
end;

procedure TKCustomMonitor.DoStartEvent;
begin
	if Assigned( FOnStart ) then
		FOnStart( Self );
end;

procedure TKCustomMonitor.DoStopEvent;
begin
	if Assigned( FOnStop ) then
		FOnStop( Self );
end;

procedure TKCustomMonitor.Start;
var
	p : PKMonitorParams;
begin
{ if Monitor is already running then do nothing }
	if ( FThreadHandle <> NULL_HANDLE_VALUE ) then
		Exit;
{ Create mutex for stopping thread when needed }
	FMutexHandle := CreateMutex( nil, true, nil );
	if ( FMutexHandle = NULL_HANDLE_VALUE ) then
		RaiseExceptionFmt( EKMonitor, sErrInvMutexCreate, [SysErrorMessage( GetLastError )] );
{ Allocate some memory for thread parameters }
	GetMem( p, SizeOf( TKMonitorParams ) );
	try
		ZeroMemory( p, SizeOf( TKMonitorParams ) );
		with p^ do
		begin
			hMutex := FMutexHandle;
			WindowHandle := FWindowHandle;
			NotifyFilter := FNotifyFilter;
			StrCopy( Directory, PChar( FDirectory ) );
		end;
		FParamPtr := p;

	{ Start the working thread }
	{ Do not use create Thread because the IsMultiThread variable will not be setted.}
		FThreadHandle := BeginThread( nil, 0, MonitorThread, FParamPtr, 0, FThreadID );

	(* 	FThreadHandle := CreateThread(
								 nil,	           { pointer to thread security attributes }
								 0,	             { initial thread stack size, in bytes }
								 @MonitorThread, { pointer to thread function }
								 FParamPtr,      { argument for new thread }
								 0,          	   { creation flags }
								 FThreadId ); 	 { pointer to returned thread identifier } *)

		if ( FThreadHandle = NULL_HANDLE_VALUE ) then
			RaiseExceptionFmt( EKMonitor, sErrInvThreadCreate, [SysErrorMessage( GetLastError )] );
		DoStartEvent;
	except
		FreeMem( p, SizeOf( TKMonitorParams ) );
		FParamPtr := nil;
		raise;
	end;
end;

procedure TKCustomMonitor.Stop;
begin
{ if Monitor is not running then do nothing }
	if ( FThreadHandle = NULL_HANDLE_VALUE ) then
	  Exit;
	if ( FMutexHandle <> NULL_HANDLE_VALUE ) then
		ReleaseMutex( FMutexHandle );
{ if there is a handle wait as long as it takes to release its ownership }
	if ( FThreadHandle <> NULL_HANDLE_VALUE ) then
	begin
		WaitForSingleObject( FThreadHandle, INFINITE );
		FThreadHandle := NULL_HANDLE_VALUE;
		FThreadID := 0;
	end;
	if CheckPointer( FParamPtr ) then
	begin
		FreeMem( FParamPtr, SizeOf( TKMonitorParams ) );
		FParamPtr := nil;
	end;
	if ( FMutexHandle <> NULL_HANDLE_VALUE ) then
	begin
		CloseHandle( FMutexHandle );
		FMutexHandle := NULL_HANDLE_VALUE;
	end;
	DoStopEvent;
end;

{
--------------------------------------------------------------------------------
---------------------- Shell API Utilities Implementation ----------------------
--------------------------------------------------------------------------------
}

{ TKCustomFileManager }

constructor TKCustomFileManager.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	if CheckObjectClass( AOwner, TWinControl ) then
		FHandle := TWinControl( AOwner ).Handle
	else
		FHandle := Application.Handle;
	FOperation := sfoCopy;
	FProgressTitle := '';
	FSource := TStringList.Create;
	FAborted := false;
	FNameMappings := TStringList.Create;
	FDestination := TStringList.Create;
	FFileOperationFlags := DEF_FO_FLAGS;
end;

constructor TKCustomFileManager.CreateFromHandle( AOwner: TComponent; AHandle: HWND );
begin
	inherited Create( AOwner );
	FHandle := AHandle;
end;

destructor TKCustomFileManager.Destroy;
begin
	FreeNameMappings;
	FSource.Free;
	FDestination.Free;
	FNameMappings.Free;
	inherited Destroy;
end;

procedure TKCustomFileManager.FreeNameMappings;
begin
	if CheckPointer( FNameMappingsPtr ) then
	begin
		SHFreeNameMappings( GlobalHandle( FNameMappingsPtr ) );
		FNameMappingsPtr := nil;
	end;
end;

function TKCustomFileManager.GetNameMappings: TStrings;
var
	iSize: Integer;
begin
	Result := FNameMappings;
	Result.BeginUpdate;
	try
		Result.Clear;
{ REVISAR....... }
		iSize := GlobalSize( GlobalHandle( FNameMappings ) ); // or MappingsPtr?
		RaiseException( EKFileOperation, '' );
		while ( iSize > 0 ) do
		begin
			Dec( iSize );
		end;
	finally
		Result.EndUpdate;
	end;
end;

procedure TKCustomFileManager.SetSource( Value: TStrings );
begin
	FSource.Assign( Value );
end;

procedure TKCustomFileManager.SetDestination( Value: TStrings );
begin
	FDestination.Assign( Value );
end;

function TKCustomFileManager.Execute: Boolean;
var
	p: Pointer;
begin
	FreeNameMappings;
	Result := ShellFileOperation( FHandle, FProgressTitle, Operation, FSource,
		FDestination, FFileOperationFlags, p, FAborted );
	if ( not FAborted ) and ( fofWantMappingHandle in FFileOperationFlags ) then
		FNameMappingsPtr := p;
end;

{ TKCustomFileInfo }

constructor TKCustomFileInfo.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ClearInfo;
	FFileName := '';
	FSysImageList := nil;
	FIcon := TIcon.Create;
	FFileInfoFlags := DEF_FI_FLAGS;
	FFileAttributes := DEF_FA_FLAGS;
	FSysImageList := TImageList.Create( nil );
	FSysImageList.ShareImages := true;
end;

destructor TKCustomFileInfo.Destroy;
begin
	FreeClean( FSysImageList );
	FreeClean( FIcon );
	inherited Destroy;
end;

procedure TKCustomFileInfo.ClearInfo;
begin
	FIconIndex := -1;
	FExecuted := False;
	FDisplayName := '';
	FIconLocation := '';
	FLastExecutedFileName := '';
end;

function TKCustomFileInfo.GetExecuted: Boolean;
begin
  Result := ( FExecuted and CheckStrEqual( FFileName, FLastExecutedFileName ) );
end;

procedure TKCustomFileInfo.MakeIcon( hi: HICON );
var
	lhi: HICON;
	tmpIco: TIcon;
begin
	tmpIco := TIcon.Create;
	try
		lhi := tmpIco.Handle;
		tmpIco.Handle := hi;
		FIcon.Assign( tmpIco );
		tmpIco.Handle := lhi;
	finally
		tmpIco.Free;
	end;
end;

procedure TKCustomFileInfo.MakeIconFromImageList( Index: Integer );
begin
	FSysImageList.GetIcon( Index, FIcon );
end;

procedure TKCustomFileInfo.MakeImageList( dwImageList: DWORD );
begin
	if ( dwImageList <> FSysImageList.Handle ) then
		FSysImageList.Handle := dwImageList;
end;

function TKCustomFileInfo.Execute: Boolean;
var
	sfi: TSHFileInfo;
	iRslt: Cardinal;
begin
	iRslt := ShellGetFileInfo( FFileName, FFileAttributes, FFileInfoFlags, sfi );
	Result := ( iRslt <> NULL_HANDLE_VALUE );
{
  There is negative handle values..... not just for Delphi4 complaint... 

	if ( ( [fifIcon, fifSysIconIndex, fifSmallIcon] * FFileInfoFlags ) = [] ) then
		Result := ( iRslt > INVALID_HANDLE_VALUE )
	else
		Result := ( iRslt <> NULL_HANDLE_VALUE );
}

	FExecuted := Result;
	if ( not Result ) then
		Exit;
{ the function completed successfully }
	ClearInfo;
	FLastExecutedFileName := FFileName;
	if ( fifAttributes in FFileInfoFlags ) then
		FFileAttributes := RetrieveFileAttributes( sfi.dwAttributes );
	if ( fifDisplayName in FFileInfoFlags ) then
	begin
		FDisplayName := sfi.szDisplayName;
		FIconLocation := '';
	end
	else if ( fifIconLocation in FFileInfoFlags ) then
	begin
		FDisplayName := '';
		FIconLocation := sfi.szDisplayName;
	end
	else
	begin
		FDisplayName := '';
		FIconLocation := '';
	end;
	if ( fifSysIconIndex in FFileInfoFlags ) then
	begin
		MakeImageList( iRslt );
		FIconIndex := sfi.iIcon;
		MakeIconFromImageList( FIconIndex );
	end;
	if ( fifIcon in FFileInfoFlags ) then
	begin
		MakeIcon( sfi.hIcon );
		FIconIndex := sfi.iIcon;
		MakeImageList( iRslt );
	end;
end;

{
--------------------------------------------------------------------------------
---------------------------- Windows NT Services API ---------------------------
--------------------------------------------------------------------------------
}

{ TKServiceDependency }

function TKServiceDependency.GetDisplayName: string;
begin
	if CheckStr( Name ) then
		Result := Name
	else
    Result := inherited GetDisplayName;
end;

{ TKServiceDependencies }

constructor TKServiceDependencies.Create( AOwner: TPersistent );
begin
  FOwner := AOwner;
  inherited Create( TKServiceDependency );
end;

function TKServiceDependencies.GetItem( Index: Integer ): TKServiceDependency;
begin
	Result := TKServiceDependency( inherited GetItem( Index ) );
end;

procedure TKServiceDependencies.SetItem( Index: Integer; Value: TKServiceDependency );
begin
	inherited SetItem( Index, TCollectionItem( Value ) );
end;

function TKServiceDependencies.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

{ TKEnumSvcStrings }

destructor TKEnumSvcStrings.Destroy;
begin
	Clear;
	inherited Destroy;
end;

function TKEnumSvcStrings.GetServiceStatus( Index: Integer ): TServiceStatus;
begin
  FCanAccessObject := True;
	try
		if ( not CheckObject( Objects[Index] ) ) then
			RaiseExceptionFmt( EKEnumSvcStrngs, sErrInvEnumSvcStatus, [Names[Index]] );
		Result := PServiceStatus( Objects[Index] )^;
	finally
		FCanAccessObject := False;
	end;
end;

function TKEnumSvcStrings.GetServiceTypes( Index: Integer ): TKServiceTypes;
begin
	Result := ServiceTypeSet( ServiceStatus[Index].dwServiceType );
end;

function TKEnumSvcStrings.GetServiceState( Index: Integer ): TKServiceState;
begin
	Result := ServiceStateEnum( ServiceStatus[Index].dwCurrentState );
end;

function TKEnumSvcStrings.GetServiceControlsAccepted( Index: Integer ): TKControlsAccepted;
begin
	Result := ControlAcceptSet( ServiceStatus[Index].dwControlsAccepted );
end;

function TKEnumSvcStrings.GetStatusWaitHint( Index: Integer ): DWORD;
begin
	Result := ServiceStatus[Index].dwWaitHint;
end;

function TKEnumSvcStrings.GetStatusCheckPoint( Index: Integer ): DWORD;
begin
	Result := ServiceStatus[Index].dwCheckPoint;
end;

function TKEnumSvcStrings.GetStatusWin32ExitCode( Index: Integer ): DWORD;
begin
	Result := ServiceStatus[Index].dwWin32ExitCode;
end;

function TKEnumSvcStrings.GetStatusSpecificExitCode( Index: Integer ): DWORD;
begin
	Result := ServiceStatus[Index].dwServiceSpecificExitCode;
end;

procedure TKEnumSvcStrings.SvcStatusDelete( Index: Integer );
begin
	FCanAccessObject := True;
	try
		if CheckObject( Objects[Index] ) then
			Dispose( PServiceStatus( Objects[Index] ) );
	finally
		FCanAccessObject := False;
	end;
end;

function TKEnumSvcStrings.GetObject( Index: Integer ): TObject;
begin
	Result := nil;
	if FCanAccessObject then
		Result := inherited GetObject( Index )
	else
		RaiseException( EKEnumSvcStrngs, sErrEnumSvcObj );
end;

procedure TKEnumSvcStrings.PutObject( Index: Integer; AObject: TObject );
begin
	if FCanAccessObject then
		inherited PutObject( Index, AObject )
	else
		RaiseException( EKEnumSvcStrngs, sErrEnumSvcObj );
end;

procedure TKEnumSvcStrings.Exchange( Index1, Index2: Integer );
begin
	FCanAccessObject := True;
	try
		inherited Exchange( Index1, Index2 );
	finally
		FCanAccessObject := False;
	end;
end;

procedure TKEnumSvcStrings.AddStrings( Strings: TStrings );
var
	i: Integer;
begin
	if CheckObjectClass( Strings, TKEnumSvcStrings ) then
	begin
		FCanAccessObject := True;
		try
			( Strings as TKEnumSvcStrings ).FCanAccessObject := True;
			try
				BeginUpdate;
				try
					for i := 0 to Strings.Count - 1 do
						AddServiceEx( Strings.Names[i], Strings.Values[Strings.Names[i]],
							( Strings as TKEnumSvcStrings ).ServiceStatus[i] );
				finally
					EndUpdate;
				end;
				{ inherited AddStrings( Strings ); do not call the inherited method }
			finally
				( Strings as TKEnumSvcStrings ).FCanAccessObject := False;
			end;
		finally
			FCanAccessObject := False;
		end;
	end
	else
		RaiseException( EKEnumSvcStrngs, sErrEnumSvcObj );
end;

procedure TKEnumSvcStrings.Clear;
begin
	while CheckStrings( Self ) do
		Delete( Count - 1 );
	{ inherited Clear; do not call the inherited method }
end;

procedure TKEnumSvcStrings.Delete( Index: Integer );
begin
	SvcStatusDelete( Index );
	inherited Delete( Index );
end;

function TKEnumSvcStrings.AddServiceEx( const SvcName, SvcDispName: string;
  SvcStat: TServiceStatus ): Integer;
var
	pss: PServiceStatus;
begin
	pss := New( PServiceStatus );
	ZeroMemory( pss, SizeOf( TServiceStatus ) );
	System.Move( SvcStat, pss^, SizeOf( TServiceStatus ) );
	FCanAccessObject := True;
	try
		Result := AddObject( SvcName + CH_EQUAL_TOKEN + SvcDispName, TObject( pss ) );
	finally
		FCanAccessObject := False;
	end;
end;

function TKEnumSvcStrings.AddService( EnumSvcStatus: TEnumServiceStatus ): Integer;
begin
	Result := AddServiceEx( EnumSvcStatus.lpServiceName, EnumSvcStatus.lpDisplayName,
		EnumSvcStatus.ServiceStatus );
end;

procedure TKEnumSvcStrings.FillStrings( pessa: PEnumServiceStatusArray; ACount: Cardinal );
var
	i: Cardinal;
begin
	ForcePointer( pessa );
	if ( ACount = 0 ) then
		Exit;
	BeginUpdate;
	try
		Clear;
		for i := 0 to ACount - 1 do
			AddService( pessa^[Integer( i )] );
	finally
		EndUpdate;
	end;
end;

{ TKCustomWin32ServiceManager }

constructor TKCustomWin32ServiceManager.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FServiceName := '';
	FMachineName := '';
	FDatabaseName := '';
	FServiceBinPath := '';
	FServiceStartName := '';
	FServiceDisplayName := '';
	FAutoRefresh := True;
	FDependenciesStop := True;
	FDependencies := TKServiceDependencies.Create( Self );

	FStatusCheckPoint := 0;
	FStatusWaitHint := 0;
	FStatusWin32ExitCode := 0;
	FStatusSpecificExitCode := 0;

	FServiceError := seNormal;
	FServiceStart := ssAuto;
	FServiceState := ssStopped;
	FServiceAccess := [];
	FServiceControlsAccepted := [];
	FServiceTypes := stServiceWin32;
end;

destructor TKCustomWin32ServiceManager.Destroy;
begin
	FDependencies.Free;
	inherited Destroy;
	CloseService;
end;

function TKCustomWin32ServiceManager.pcServiceName: PChar;
begin
	Result := PChar( FServiceName );
end;

function TKCustomWin32ServiceManager.pcMachineName: PChar;
begin
	if ( not CheckTrimStr( FMachineName ) ) then
		Result := nil
	else
		Result := PChar( FMachineName );
end;

function TKCustomWin32ServiceManager.pcDatabaseName: PChar;
begin
	if ( not CheckTrimStr( FDatabaseName ) ) then
		Result := nil
	else
		Result := PChar( FDatabaseName );
end;

function TKCustomWin32ServiceManager.pcServiceBinPath: PChar;
begin
	Result := PChar( FServiceBinPath );
end;

function TKCustomWin32ServiceManager.pcServiceDisplayName: PChar;
begin
	Result := PChar( FServiceDisplayName );
end;

function TKCustomWin32ServiceManager.GetServiceDependencies( const Delimiter: Char ): string;
var
	i,
	iLen: Integer;
	pc: PChar;
begin
	Result := '';
  { get as a pchar list }
	if ( Delimiter <> CH_NULL ) and CheckCollection( Dependencies ) then
	begin
		for i := 0 to Dependencies.Count - 1 do
		begin
			if Dependencies[i].IsGroup then
				Result := Result + SC_GROUP_IDENTIFIER;
			Result := Result + Dependencies[i].Name + Delimiter;
		end;
		if CheckStr( Result ) then
		  Delete( Result, Length( Result ), 1 );
	end
	else
	begin
		iLen := 0;
		for i := 0 to Dependencies.Count - 1 do
		begin
			Inc( iLen, Length( Dependencies[i].Name ) );
			if Dependencies[i].IsGroup then
				Inc( iLen );
		end;
		Inc( iLen, Dependencies.Count * 2 );
		if ( iLen <> 0 ) then
		begin
			SetLength( Result, iLen );
			pc := @Result[1];
			for i := 0 to Dependencies.Count - 1 do
			begin
				if Dependencies[i].IsGroup then
				begin
					pc^ := SC_GROUP_IDENTIFIER;
					Inc( pc );
				end;
				pc := StrECopy( pc, PChar( Dependencies[i].Name ) );
				Inc( pc );
				pc^ := Delimiter;
				Inc( pc );
			end;
		end;
	end;
end;

procedure TKCustomWin32ServiceManager.SetServiceName( const Value: string );
begin
	if ( not CheckStrEqual( Value, FServiceName ) ) then
	begin
		FServiceName := Value;
		if ( CheckTrimStr( FServiceName ) and AutoRefresh and
		   ( not ( Designing( Self ) or Loading( Self ) ) ) ) then
			if ( not QueryServiceStatus ) then
			  FServiceName := '';
	end;
end;

procedure TKCustomWin32ServiceManager.SetServiceDependencies( const Delimiter: Char;
  const Value: string );
var
	i: Integer;
	sl: TStrings;
begin
	if ( not CheckStrEqual( Value, GetServiceDependencies( Delimiter ) ) ) then
	begin
		sl := TStringList.Create;
		try
			Dependencies.BeginUpdate;
			try
				Dependencies.Clear;
				if CheckStr( Value ) then
				begin
					PCharListToStrings( PChar( Value ), sl );
					TrimStrings( sl );
					for i := 0 to sl.Count - 1 do
						with ( Dependencies.Add as TKServiceDependency ) do
						begin
							IsGroup := ( sl[i][1] = SC_GROUP_IDENTIFIER );
							Name := sl[i];
							if IsGroup then
								Delete( FName, 1, 1 );
						end;
				end;		
			finally
				Dependencies.EndUpdate;
			end;
		finally
			sl.Free;
		end;
	end;
end;

procedure TKCustomWin32ServiceManager.SetDependencies( Value: TKServiceDependencies );
begin
	FDependencies.Assign( Value );
end;

function TKCustomWin32ServiceManager.GetServiceInfoSize: Integer;
{
		DWORD ServiceTypes						DWORD ServiceControlsAccepted	DWORD TagID
		TKServiceStart StartType			TKServiceError ServiceError 	TKServiceState ServiceState
		PChar	ServiceDisplayName			PChar ServiceBinPath					PChar DependenciesAsString[CH_LIST_TOKEN]
		PChar LoadGroup								PChar ServiceStartName				#0 (as a pchar list)
}
begin
	ForceTrimStr( ServiceName );
	Result := ( ( 3 * SizeOf( DWORD ) ) + ( 6 * SizeOf( CH_NULL ) ) +
		Length( ServiceDisplayName ) + Length( ServiceBinPath ) + Length(
		DependenciesAsString[CH_LIST_TOKEN] ) + Length( LoadGroup ) +
		Length( ServiceStartName ) + SizeOf( TKServiceStart ) + SizeOf( TKServiceError ) +
		SizeOf( TKServiceState ) );
end;

procedure TKCustomWin32ServiceManager.QueryToServiceInfo( p: Pointer );
var
	s: string;
	sui: TKSvcUpdateInfo;
begin
	ForcePointer( p );
	with sui do
	begin         
		SvcTypes := ServiceTypeDWORDFromSet( ServiceTypes );
		SvcCtrls := ControlAcceptDWORDFromSet( ServiceControlsAccepted );
		SvcTagID := TagID;
		SvcStart := ServiceStart;
		SvcError := ServiceError;
		SvcState := ServiceState;
	end;
	Move( sui, p^, SizeOf( TKSvcUpdateInfo ) );
	p := IncPtr( p, SizeOf( TKSvcUpdateInfo ) );
	s := ServiceDisplayName;
	Move( Pointer( s )^, p^, Length( s ) );
	p := IncPtr( p, Length( s ) + 1 );
	s := ServiceBinPath;
	Move( Pointer( s )^, p^, Length( s ) );
	p := IncPtr( p, Length( s ) + 1 );
	s := DependenciesAsString[CH_LIST_TOKEN];
	Move( Pointer( s )^, p^, Length( s ) );
	p := IncPtr( p, Length( s ) + 1 );
	s := LoadGroup;
	Move( Pointer( s )^, p^, Length( s ) );
	p := IncPtr( p, Length( s ) + 1 );
	s := ServiceStartName;
	Move( Pointer( s )^, p^, Length( s ) );
	{ p := IncPtr( p, Length( s ) + 2 ); }
end;

function TKCustomWin32ServiceManager.UpdateFromServiceInfo( p: Pointer;
	szData: Integer ): Boolean;
var
	pc: PChar;
	sl: TStrings;
	sui: TKSvcUpdateInfo;
begin
	Result := (	CheckPointer( p ) and ( szData > SizeOf( TKSvcUpdateInfo ) ) );
	if ( not Result ) then
		Exit;
	ZeroMemory( @sui, SizeOf( TKSvcUpdateInfo ) );
	Move( p^, sui, SizeOf( TKSvcUpdateInfo ) );
	with sui do
	begin
		ServiceTypes := ServiceTypeSet( SvcTypes );
		ServiceControlsAccepted := ControlAcceptSet( SvcCtrls );
		TagID := SvcTagID;
		ServiceStart := SvcStart;
		ServiceError := SvcError;
		ServiceState := SvcState;
	end;
	Dec( szData, SizeOf( TKSvcUpdateInfo ) );
	if ( szData > 0 ) then
	begin
		p := IncPtr( p, SizeOf( TKSvcUpdateInfo ) );
		sl := TStringList.Create;
		try
			PCharListToStringsEx( p, szData, sl );
			Result := ( sl.Count = 5 );
			if Result then
			begin
				ServiceDisplayName := sl[0];
				ServiceBinPath := sl[1];
				LoadGroup := sl[3];
				ServiceStartName := sl[4];
				Dependencies.Clear;
				if CheckTrimStr( sl[2] ) then
				begin
					ExtractStrings( sl[2], CH_LIST_TOKEN, sl );
					pc := StrAllocMem( StringsToPCharList( sl, nil ) );
					try
						Result := ( StringsToPCharList( sl, pc ) = sl.Count );
						if Result then
							DependenciesAsString[CH_NULL] := pc;
					finally
						StrDispose( pc );
					end;
				end;	
			end;
		finally
			sl.Free;
		end;
	end;
end;

function TKCustomWin32ServiceManager.OpenService( dwAccess: DWORD ): Boolean;
begin
	Result := GetServiceHandle( dwAccess );
	if ( not Result ) then
	  RaiseLastError;
end;

procedure TKCustomWin32ServiceManager.RaiseLastError;
begin
  RaiseLastWin32Error;
end;

function TKCustomWin32ServiceManager.DoWin32Error( ErrorCode: Cardinal ): Boolean;
begin
	Result := Assigned( FOnWin32Error );
	if Result then
		FOnWin32Error( Self, ErrorCode, Result );
end;

procedure TKCustomWin32ServiceManager.ClearServiceStatus;
begin
	ZeroMemory( @FServiceStatus, SizeOf( TServiceStatus ) );
end;

procedure TKCustomWin32ServiceManager.RefreshServiceStatus;
begin
	with FServiceStatus do
	begin
		FServiceTypes := ServiceTypeSet( dwServiceType );
		FServiceState := ServiceStateEnum( dwCurrentState );
		FServiceControlsAccepted := ControlAcceptSet( dwControlsAccepted );
		FStatusWaitHint := dwWaitHint;
		FStatusCheckPoint := dwCheckPoint;
		FStatusWin32ExitCode := dwWin32ExitCode;
		FStatusSpecificExitCode := dwServiceSpecificExitCode;
	end;
end;

function TKCustomWin32ServiceManager.GetServiceAccessFromCtrl( fdwControl: DWORD ): DWORD;
begin
	case fdwControl of
		SERVICE_CONTROL_STOP:
			Result := SERVICE_STOP or SERVICE_QUERY_STATUS;
		SERVICE_CONTROL_PAUSE,
		SERVICE_CONTROL_CONTINUE:
			Result := SERVICE_PAUSE_CONTINUE;
		SERVICE_CONTROL_INTERROGATE:
			Result := SERVICE_INTERROGATE or SERVICE_QUERY_STATUS;
	else
		Result := SERVICE_CONTROL_INTERROGATE;
	end;
end;

function TKCustomWin32ServiceManager.PauseService: Boolean;
begin
	Result := ControlService( SERVICE_CONTROL_PAUSE );
end;

function TKCustomWin32ServiceManager.GetEnumSvcStrings: TKEnumSvcStrings;
begin
  Result := nil; { default option is to leave stop create the strings }
end;

function TKCustomWin32ServiceManager.StopService: Boolean;
var
	i: Integer;
  bFree,
	bOldRefresh: Boolean;
	sOldSvcName: string;
	EnumStr: TKEnumSvcStrings;
begin
	Result := DependenciesStop;
	if ( not Result ) then
		Result := ControlService( SERVICE_CONTROL_STOP )
	else
	begin
		sOldSvcName := ServiceName;
		try
			bOldRefresh := AutoRefresh;
			try
				AutoRefresh := False;
				{
				"Dependency on a service means that this service can only run if the service it
				depends on is running. Dependency on a group means that this service can run if
				at least one member of the group is running after an attempt to start all members
				of the group."
				We must be sure that there is any other service that depend with the service
				been stopped. To do so, we need to collect the list o all services (with their
				states), and check if it has the current service as a dependent (not grouped).
				}
				EnumStr := GetEnumSvcStrings; { to eliminate remote packets overhead... }
				bFree := ( not CheckObject( EnumStr ) );
				if bFree then
					EnumStr := TKEnumSvcStrings.Create;
				try
					Result := ActiveDependentServicesEx( EnumStr );
					if ( Result and CheckStrings( EnumStr ) ) then
						for i := 0 to EnumStr.Count - 1 do
						begin
							ServiceName := EnumStr.Names[i];
							Result := StopService;
							if ( not Result ) then
								Exit;
						end;
				finally
					if bFree then
  					EnumStr.Free;
				end;
			finally
				AutoRefresh := bOldRefresh;
			end;
		finally
			ServiceName := sOldSvcName;
		end;
		if Result then
  		Result := ControlService( SERVICE_CONTROL_STOP );
	end;	
end;

function TKCustomWin32ServiceManager.ContinueService: Boolean;
begin
	Result := ControlService( SERVICE_CONTROL_CONTINUE );
end;

function TKCustomWin32ServiceManager.AllServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumServices( Strings, esAny );
end;

function TKCustomWin32ServiceManager.ActiveServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumServices( Strings, esActive );
end;

function TKCustomWin32ServiceManager.InactiveServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumServices( Strings, esInactive );
end;

function TKCustomWin32ServiceManager.AllDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumDependentServices( Strings, esAny );
end;

function TKCustomWin32ServiceManager.ActiveDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumDependentServices( Strings, esActive );
end;

function TKCustomWin32ServiceManager.InactiveDependentServicesEx( Strings: TKEnumSvcStrings ): Boolean;
begin
	Result := EnumDependentServices( Strings, esInactive );
end;

function TKCustomWin32ServiceManager.AllServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
	ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumServices( Str, esAny );
		Strings.Assign( Str );
	finally
	  Str.Free;
	end;
end;

function TKCustomWin32ServiceManager.ActiveServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
  ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumServices( Str, esActive );
		Strings.Assign( Str );
	finally
		Str.Free;
	end;
end;

function TKCustomWin32ServiceManager.InactiveServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
  ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumServices( Str, esInactive );
		Strings.Assign( Str );
	finally
	  Str.Free;
	end;
end;

function TKCustomWin32ServiceManager.AllDependentServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
  ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumDependentServices( Str, esAny );
		Strings.Assign( Str );
	finally
		Str.Free;
	end;
end;

function TKCustomWin32ServiceManager.ActiveDependentServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
	ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumDependentServices( Str, esActive );
		Strings.Assign( Str );
	finally
		Str.Free;
	end;
end;

function TKCustomWin32ServiceManager.InactiveDependentServices( Strings: TStrings ): Boolean;
var
	Str: TKEnumSvcStrings;
begin
	ForceStrings( Strings );
	Str := TKEnumSvcStrings.Create;
	try
		Result := EnumDependentServices( Str, esInactive );
		Strings.Assign( Str );
	finally
		Str.Free;
	end;
end;

{ TKWin32ServiceManager }

constructor TKWin32ServiceManager.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	if ( not Designing( Self ) ) then
		ForceWinNT( True );
	FManagerLock := nil;
	FManagerHandle := NULL_HANDLE_VALUE;
	FServiceHandle := NULL_HANDLE_VALUE;
	FRemoteServerSvcManager := NULL_HANDLE_VALUE;
	FManagerAccess := maAllAccess;
end;

destructor TKWin32ServiceManager.Destroy;
begin
	if CheckPointer( FManagerLock ) then
	begin
		WinSVC.UnLockServiceDatabase( FManagerLock );
		FManagerLock := nil;
	end;
	if ( FManagerHandle <> NULL_HANDLE_VALUE ) then
	begin
		WinSVC.CloseServiceHandle( FManagerHandle );
		FManagerHandle := NULL_HANDLE_VALUE;
	end;
	inherited Destroy;
end;

function TKWin32ServiceManager.OpenManager: Boolean;
begin
	Result := GetManagerHandle;
	if ( not Result ) then
		RaiseLastError;
end;

procedure TKWin32ServiceManager.RaiseLastError;
var
	bOk: Boolean;
	rsmwei: TKRSMWin32ErrorInfo;
begin
	rsmwei.Raised := False;
	rsmwei.ErrorCode := GetLastError;
	rsmwei.ErrorMessage := SysErrorMessage( GetLastError );
	if ( ( FRemoteServerSvcManager <> NULL_HANDLE_VALUE ) and CheckStr( ServiceName ) ) then
		bOk := Boolean( SendMessage( FRemoteServerSvcManager, KM_RSM_WIN32ERROR,
			Integer( PChar( ServiceName ) ), Integer( @rsmwei ) ) )
	else
	  bOk := DoWin32Error( rsmwei.ErrorCode );
	if ( not bOk ) then
		inherited RaiseLastError;
end;

function TKWin32ServiceManager.GetEnumSvcStrings: TKEnumSvcStrings;
begin
	if ( FRemoteServerSvcManager = NULL_HANDLE_VALUE ) then
		Result := inherited GetEnumSvcStrings
	else
		Result := TKEnumSvcStrings( SendMessage( FRemoteServerSvcManager, KM_RSM_QUERYDPDSTR,
		  Integer( PChar( ServiceName ) ), 0 ) );
end;

function TKWin32ServiceManager.GetManagerHandle: Boolean;
begin
	if ( FManagerHandle <= NULL_HANDLE_VALUE ) then
		FManagerHandle := WinSVC.OpenSCManager( pcMachineName,
			pcDatabaseName, ManagerAccessDWORDFromSet( FManagerAccess ) );
	Result := ( FManagerHandle <> NULL_HANDLE_VALUE );
end;

function TKWin32ServiceManager.GetServiceHandle( dwAccess: DWORD ): Boolean;
begin
	if ( ( FServiceHandle <= NULL_HANDLE_VALUE ) and GetManagerHandle ) then
		FServiceHandle := WinSVC.OpenService( FManagerHandle, pcServiceName, dwAccess );
	Result := ( FServiceHandle <> NULL_HANDLE_VALUE );
end;

function TKWin32ServiceManager.LockDatabase: Boolean;
begin
	Result := OpenManager;
	if Result then
	begin
		FManagerLock := LockServiceDatabase( FManagerHandle );
		Result := CheckPointer( FManagerLock );
	end;
end;

function TKWin32ServiceManager.UnlockDatabase: Boolean;
begin
	Result := UnLockServiceDatabase( FManagerLock );
	if Result then
		FManagerLock := nil
	else
		RaiseLastError;
end;

procedure TKWin32ServiceManager.SetRemoteServerSvcManager( Value: THandle );
begin
  FRemoteServerSvcManager := Value;
end;

function TKWin32ServiceManager.CloseService: Boolean;
begin
	Result := True;
	if ( FServiceHandle <> NULL_HANDLE_VALUE ) then
	begin
		Result := WinSVC.CloseServiceHandle( FServiceHandle );
		if Result then
			FServiceHandle := NULL_HANDLE_VALUE;
	end;
end;

function TKWin32ServiceManager.InstallService: Boolean;
var
	tmpTagID: DWORD;
	PTag: Pointer;
begin
	Result := OpenManager;
	try
		PTag := nil;
		tmpTagID := FTagID;
		if ( tmpTagID > 0 ) then
			PTag := @tmpTagID;
		FServiceHandle := CreateService( FManagerHandle, pcServiceName,
			pcServiceDisplayName, ServiceAccessDWORDFromSet( FServiceAccess ),
			ServiceTypeDWORDFromSet( FServiceTypes ), ServiceStartDWORD( FServiceStart ),
			ServiceErrorDWORD( FServiceError ), pcServiceBinPath, PChar( FLoadGroup ), PTag,
			PChar( GetServiceDependencies( CH_NULL ) ), PChar( FServiceStartName ),
			PChar( FPassWord ) );
		FTagID := tmpTagID;
		Result := ( FServiceHandle <> NULL_HANDLE_VALUE );
	finally
		CloseService;
		if ( not Result ) then
			RaiseLastError;
	end;
end;

function TKWin32ServiceManager.DeleteService: Boolean;
{
	in WinNT.h: #define DELETE  (0x00010000L)
}
const
	SC_DELETE = $00010000;
begin
	Result := OpenService( LongInt( SC_DELETE ) );
	try
		if ( Result and ( not WinSVC.DeleteService( FServiceHandle ) ) ) then
			RaiseLastError;
	finally
		CloseService;
	end;
end;

function TKWin32ServiceManager.QueryServiceStatus: Boolean;
begin
	if ( FServiceHandle <> NULL_HANDLE_VALUE ) then
		Result := WinSVC.QueryServiceStatus( FServiceHandle, FServiceStatus )
	else
		Result := ControlService( SERVICE_CONTROL_INTERROGATE );
{ For the case of a ControlService call for a stopped service }
	if ( ( FServiceHandle = NULL_HANDLE_VALUE ) and
		( GetLastError = ERROR_SERVICE_NOT_ACTIVE ) and ( not Result ) ) then
		Result := True;
	if ( not Result ) then
		RaiseLastError;
	RefreshServiceStatus;
end;

function TKWin32ServiceManager.UpdateServiceConfig: Boolean;
var
	PTag: Pointer;
	tmpTagID: DWORD;
begin
	Result := LockDatabase;
	if ( not Result ) then
	begin
		if ( GetLastError <> ERROR_SERVICE_DATABASE_LOCKED ) then
			RaiseLastError;
		Result := QueryServiceLockStatus;
		if Result and ( not ManagerLocked ) then
		begin
			Result := LockDatabase;
			if ( not Result ) then
				RaiseLastError;
		end
		else
			RaiseLastError;
	end;
{ we do have a lock on the database now, for sure! }
	try
		if OpenService( SERVICE_CHANGE_CONFIG ) then
			try
				PTag := nil;
				tmpTagID := FTagID;
				if ( tmpTagID > 0 ) then
					PTag := @tmpTagID;
				Result := ChangeServiceConfig( FServiceHandle,
					ServiceTypeDWORDFromSet( FServiceTypes ), ServiceStartDWORD( FServiceStart ),
					ServiceErrorDWORD( FServiceError ), pcServiceBinPath, PChar( FLoadGroup ), PTag,
					PChar( GetServiceDependencies( CH_NULL ) ), PChar( FServiceStartName ),
					PChar( FPassWord ), pcServiceDisplayName );
				FTagID := tmpTagID;
				if ( not Result ) then
					RaiseLastError;
			finally
				CloseService;
			end;
	finally
		UnlockDatabase;
	end;
end;

function TKWin32ServiceManager.StartService( Params: TStrings ): Boolean;
var
	pc: PChar;
	iCount: Integer;
	iOldCheckPoint: DWORD;
begin
	pc := nil;
	Result := OpenService( SERVICE_START or SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS );
	if ( not Result ) then
	  Exit;
{ There is a valid service handle now. }
	try
		if ( not CheckStrings( Params ) ) then
			Result := WinSVC.StartService( FServiceHandle, 0, pc )
		else
		begin
			if ( not CheckStrEqual( Params[0], ServiceBinPath ) ) then
				Params.Insert( 0, ServiceBinPath );
			pc := StrAlloc( StringsToPCharList( Params, nil ) );
			try
				iCount := StringsToPCharList( Params, pc );
				Result := WinSVC.StartService( FServiceHandle, iCount, pc );
			finally
				StrDispose( pc );
			end;
		end;
		if Result then
		begin
			QueryServiceStatus;
			with FServiceStatus do
				while Result and ( dwCurrentState <> SERVICE_RUNNING ) do
				begin
					iOldCheckPoint := dwCheckPoint;
					Sleep( dwWaitHint );
					QueryServiceStatus;
					if ( iOldCheckPoint >= dwCheckPoint ) then
						Break;
				end;
			Result := FServiceStatus.dwCurrentState = SERVICE_RUNNING;
		end;
	finally
		CloseService;
	end;
end;

function TKWin32ServiceManager.ControlService( fdwControl: DWORD ): Boolean;
begin
	Result := OpenService( GetServiceAccessFromCtrl( fdwControl ) );
	if Result then
		try
			ClearServiceStatus;
			Result := WinSVC.ControlService( FServiceHandle, fdwControl, FServiceStatus );
			RefreshServiceStatus;
		finally
			CloseService;
		end;
end;

function TKWin32ServiceManager.QueryServiceConfig: Boolean;
var
	iMoreBytes,
	iBytesNeeded: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	lpqsc: PQueryServiceConfig;
begin
	Result := OpenService( SERVICE_QUERY_CONFIG );
	if Result then
		try
			ukwUtils.QueryServiceConfig( FServiceHandle, nil, 0, iBytesNeeded );
			lpqsc := AllocMem( iBytesNeeded );
			try
				Result := ukwUtils.QueryServiceConfig( FServiceHandle, lpqsc, iBytesNeeded,
					iMoreBytes );
				with lpqsc^ do
				begin
					ServiceTypes := ServiceTypeSet( dwServiceType );
					ServiceStart := ServiceStartEnum( dwStartType );
					ServiceError := ServiceErrorEnum( dwErrorControl );
					TagID := dwTagId;
					LoadGroup := lpLoadOrderGroup;
					ServiceBinPath := lpBinaryPathName;
					ServiceDisplayName := lpDisplayName;
					ServiceStartName := lpServiceStartName;
					SetServiceDependencies( CH_NULL, lpDependencies );
				end;
			finally
				FreeMem( lpqsc, iBytesNeeded );
			end;
		finally
			CloseService;
		end;
end;

function TKWin32ServiceManager.QueryServiceLockStatus: Boolean;
var
	dwBytesNeeded: DWORD;
	lpqsls: PQueryServiceLockStatus;
begin
	Result := OpenManager;
	if Result then
	begin
		lpqsls := AllocMem( SizeOf( TQueryServiceLockStatus ) + MAX_PATH ); { MAX_PATH for LockOwner }
		try
			if ( not WinSvc.QueryServiceLockStatus( FManagerHandle, lpqsls^, SizeOf(
				TQueryServiceLockStatus ) + MAX_PATH, dwBytesNeeded ) ) then
				RaiseLastError;
			with lpqsls^ do
			begin
				FManagerLocked := ( fIsLocked <> 0 );
				FLockOwner := lpLockOwner;
				FLockDuration := dwLockDuration;
			end;
		finally
			FreeMem( lpqsls, SizeOf( TQueryServiceLockStatus ) + MAX_PATH );
		end;
	end;
end;

function TKWin32ServiceManager.EnumServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ): Boolean;
var
	iMoreBytes,
	iBytesNeeded,
	iServicesReturned,
	dwResumeHandle: DWORD;
	pData: Pointer;
begin
	ForceObject( Strings );
	dwResumeHandle := 0;
	Result := OpenManager;
	if ( not Result ) then
		Exit;
	ukwUtils.EnumServicesStatus( FManagerHandle, SERVICE_WIN32, ENUMSERVICE_STATE[Status],
		nil, 0, iBytesNeeded, iServicesReturned, dwResumeHandle );
	if ( iBytesNeeded > 0 ) then
	begin
		pData := AllocMem( iBytesNeeded );
		try
			Result := ukwUtils.EnumServicesStatus( FManagerHandle, SERVICE_WIN32, ENUMSERVICE_STATE[Status],
				pData, iBytesNeeded, iMoreBytes, iServicesReturned, dwResumeHandle );
			Strings.FillStrings( pData, Cardinal( iServicesReturned ) ); 
		finally
			FreeMem( pData, iBytesNeeded );
		end;
	end;
end;

function TKWin32ServiceManager.EnumDependentServices( Strings: TKEnumSvcStrings; Status: TKEnumServiceStatus ): Boolean;
var
	iMoreBytes,
	iBytesNeeded,
	iServicesReturned: DWORD;
	pData: Pointer;
begin
	ForceObject( Strings );
	Result := OpenService( SERVICE_ENUMERATE_DEPENDENTS );
	if Result then
		try
			ukwUtils.EnumDependentServices( ServiceHandle, ENUMSERVICE_STATE[Status], nil, 0,
				iBytesNeeded, iServicesReturned );
			if ( iBytesNeeded > 0 ) then
			begin
				pData := AllocMem( iBytesNeeded );
				try
					Result := ukwUtils.EnumDependentServices( ServiceHandle, ENUMSERVICE_STATE[Status],
						pData, iBytesNeeded, iMoreBytes, iServicesReturned );
					Strings.FillStrings( pData, Cardinal( iServicesReturned ) );
				finally
					FreeMem( pData, iBytesNeeded );
				end;
			end;
		finally
			CloseService;
		end;
end;

{
--------------------------------------------------------------------------------
------------------- Generic WinAPI Utilities Implementation --------------------
--------------------------------------------------------------------------------
}

{ TKCustomTrayIcon }

const
	TRAY_TIMER_EVENT_ID = 1;

constructor TKCustomTrayIcon.Create( AOwner: TComponent );
begin
  ForceSingleton( AOwner, TComponentClass( ClassType ) );
	inherited Create( AOwner );
	FHint := ClassName;
	FDocked := false;
	FActive := False;
	FPopupMenu := nil;
	FImages := nil;
	FLBtnDown := false;
	FInterval :=  INTERVAL_ANITRAY_DISABLED;
{ FAnimated := False; }
	FImgCount := 0;
	FImgIndex := 0;
	FAlwaysAnimate := False;
	FAutoRestore := True;
	FShowAppTaskbar := True;
	SetShowAppTaskbar( False );
	FResIcon := nil;
	FIcon := TIcon.Create;
	if ( not Designing( Self ) ) then
	begin
		FHint := Application.Title;
		Icon := Application.Icon; { Call SetIcon }
		ZeroMemory( @FData, SizeOf( TNotifyIconData ) );
		with FData do
		begin
			cbSize := SizeOf( TNotifyIconData );
			Wnd := AllocateHwnd( WndProc );
			uCallbackMessage := KM_TRAYICON;
			StrPLCopy( szTip, FHint, Length( FHint ) );
		end;
		Application.HookMainWindow( AppWndProc );		
	end;
end;

destructor TKCustomTrayIcon.Destroy;
begin
{ Check FIcon for a singleton error (the object will not be created) }
	if CheckObject( FIcon ) then
		Remove;
	FIcon.Free;
	FResIcon.Free;
	if ( not Designing( Self ) ) then
		Application.UnHookMainWindow( AppWndProc );
	inherited Destroy;
end;

procedure TKCustomTrayIcon.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) then
	begin
		if CheckObjectClass( AComponent, TPopupMenu ) and ( TPopupMenu( AComponent ) = FPopupMenu ) then
			FPopupMenu := nil;
		if CheckObjectClass( AComponent, TImageList ) and ( TImageList( AComponent ) = FImages ) then
			FImages := nil;
	end;
end;

procedure TKCustomTrayIcon.ResyncAppTaskBar;
begin
	if ( ( not Designing( Self ) ) and FActive ) then
		SetWindowPos( Application.Handle, 0, GetSystemMetrics( SM_CXSCREEN ) div 2,
			GetSystemMetrics( SM_CYSCREEN ) div 2, 0, 0, SHOW_APP_TASKBAR[FShowAppTaskBar] );
end;

procedure TKCustomTrayIcon.Changed;
begin
	if ( not Designing( Self ) ) then
	begin
		Shell_NotifyIcon( NIM_MODIFY, @FData );
		Application.ProcessMessages;
	end;	
{
	if Active and ( ( not Assigned( FOnAnimate ) ) or ( FInterval = 0 ) or
		( not CheckObject( FImages ) ) ) then
		Application.ProcessMessages;
}		
end;

procedure TKCustomTrayIcon.SetShowAppTaskBar( Value: Boolean );
begin
	if ( FShowAppTaskBar <> Value ) then
	begin
		FShowAppTaskBar := Value;
		ResyncAppTaskBar;
	end;
end;

procedure TKCustomTrayIcon.SetInterval( Value: Cardinal );
begin
	if ( Value <> FInterval ) then
	begin
		FInterval := Value;
		RefreshTimer;
	end;
end;

procedure TKCustomTrayIcon.SetActive( Value: Boolean );
begin
	if ( Value <> FActive ) then
	begin
		FActive := Value;
		if FActive then
			Dock
		else
			Remove;
	end;
end;

procedure TKCustomTrayIcon.SetPopupMenu( Value: TPopUpMenu );
begin
	if ( Value <> FPopUpMenu ) then
	begin
		if CheckObject( Value ) then
			Value.FreeNotification( Self );
		FPopUpMenu := Value;
	end;
end;

procedure TKCustomTrayIcon.SetImageList( Value: TImageList );
var
	iC: TIcon;
begin
	if ( Value <> FImages ) then
	begin
		FImages  := Value;
		FImgCount := 0;
		FImgIndex := 0;
		if CheckObject( Value ) then
		begin
			Value.FreeNotification( Self );
			if ( not Designing( Self ) ) then
			begin
				FImgCount := Value.Count;
				if ( FImgCount <> 0 ) then
				begin
					iC := TIcon.Create;
					try
						with Value do
							GetIcon( FImgIndex, iC );
						Inc( FImgIndex );
						SetIcon( iC );
					finally
						iC.Free;
					end;
				end;
				RefreshTimer;
			end;
		end;
	end;
end;

procedure TKCustomTrayIcon.SetOnAnimate( Value: TKTrayAnimateEvent );
begin
	FOnAnimate := Value;
	RefreshTimer;
end;

procedure TKCustomTrayIcon.RefreshTimer;
begin
	if ( not Designing( Self ) ) and ( FData.Wnd <> NULL_HANDLE_VALUE ) then
	begin
		KillTimer( FData.Wnd, TRAY_TIMER_EVENT_ID );
		if ( FInterval <> 0 ) and ( FImgCount <> 0 ) and
			( SetTimer( FData.Wnd, TRAY_TIMER_EVENT_ID , FInterval, nil ) = 0 ) then
			RaiseException( EKTrayIcon, SNoTimers );
	end;
end;

procedure TKCustomTrayIcon.SetHint( const Value: string );
begin
	if ( not CheckStrEqual( FHint, Value ) ) then
	begin
		FHint := Value;
		StrPLCopy( FData.szTip, Value, Length( Value ) );
		if CheckStr( Value ) then
			FData.uFlags := FData.uFlags or NIF_TIP
		else
			FData.uFlags := FData.uFlags and ( not NIF_TIP );
		Changed;
	end;
end;

procedure TKCustomTrayIcon.SetIcon( AIcon: TIcon );
begin
	if ( FIcon <> AIcon ) then
	begin
		FIcon.Assign( AIcon );
		if ( not Designing( Self ) ) then
		begin
			FData.hIcon := NULL_HANDLE_VALUE;
			if CheckObject( AIcon ) then
				FData.hIcon := AIcon.Handle;
		end;		
		Changed;
	end;
end;

procedure TKCustomTrayIcon.Remove;
begin
	if ( ( not FDocked ) or Designing( Self ) ) then
		Exit;
	FDocked := ( not Shell_NotifyIcon( NIM_DELETE, @FData ) );
	Application.ProcessMessages;
	if FDocked then
		RaiseException( EKTrayIcon, sErrTIInvRemove );
	RefreshTimer;
	ResyncAppTaskBar;
end;

procedure TKCustomTrayIcon.Dock;
begin
	if ( FDocked or Designing( Self ) ) then
		Exit;
	with FData do
	begin
		hIcon := Icon.Handle;
		StrPLCopy( szTip, FHint, Length( FHint ) );
		uFlags := Nif_Icon or Nif_Message;
		if CheckStr( FHint ) then
			uFlags := uFlags or NIF_TIP;
		FDocked := Shell_NotifyIcon( NIM_ADD, @FData );
		Application.ProcessMessages;
		if ( not FDocked ) then
			RaiseException( EKTrayIcon, sErrTIInvCreate );
	end;
	RefreshTimer;
	ResyncAppTaskBar;
end;

function TKCustomTrayIcon.AppWndProc( var Message: TMessage ): Boolean;
begin
	Result := False;
	if ( Message.Msg = WM_WINDOWPOSCHANGING ) and ( not ShowAppTaskbar ) then
		with PWindowPos( Message.lParam )^ do
			Flags := SWP_HIDEWINDOW;
end;

procedure TKCustomTrayIcon.WndProc( var Message: TMessage );
begin
	try
		case Message.Msg of
			WM_QUERYENDSESSION:
				Message.Result := 1;
			WM_ENDSESSION:
				if TWMEndSession( Message ).EndSession then
					Remove;
			KM_TRAYICON:
				case Message.lParam of
					WM_LBUTTONDOWN:
						FLBtnDown := true;
					WM_LBUTTONUP:
						if FLBtnDown then
						begin
							FLBtnDown := false;
							Click;
						end;
					WM_LBUTTONDBLCLK:
						DblClick;
					WM_RBUTTONDOWN:
						DoPopup;
				end;
			WM_TIMER:
				if ( FAlwaysAnimate or IsIconic( Application.Handle ) ) then
					Animate;
			else if ( FData.Wnd <> NULL_HANDLE_VALUE ) then
				with Message do
					Result := DefWindowProc( FData.Wnd, Msg, WParam, LParam );
		end;
	except
		Application.HandleException( Self );
	end;
end;

procedure TKCustomTrayIcon.DoAnimate( Icon: TIcon );
begin
	if ( ( not ( Designing( Self ) or uksyUtils.Destroying( Self ) ) ) and
		 Assigned( FOnAnimate ) ) then
		FOnAnimate( Self, Icon, FImgIndex, FImgCount );
end;

procedure TKCustomTrayIcon.Animate;
begin
  if ( not CheckObject( FResIcon ) ) then
    FResIcon := TIcon.Create;
  with AniImages do
    GetIcon( FImgIndex, FResIcon );
	Icon := FResIcon;
	DoAnimate( FResIcon );
  Inc( FImgIndex );
  if ( FImgIndex > FImgCount ) then
    FImgIndex := 0;
end;

procedure TKCustomTrayIcon.DoPopup;
var
	Pt: TPoint;
begin
	if CheckObject( FPopupMenu ) and ( ShowAppTaskBar or
	   ( not IsWindowVisible( Application.Handle ) ) ) then
	begin
		GetCursorPos( Pt );
		SetForeGroundWindow( Application.Handle );
		Application.ProcessMessages;
		FPopupMenu.Popup( Pt.X, Pt.Y );
	end;
end;

procedure TKCustomTrayIcon.Click;
begin
	if Assigned( FOnClick ) then
		FOnClick( Self );
end;

procedure TKCustomTrayIcon.DblClick;
begin
	if FAutoRestore then
	  Application.Restore;
	if Assigned( FOnDblClick ) then
		FOnDblClick( Self );
end;

{ TKCustomDragDrop }

constructor TKCustomDragDrop.Create(AOwner: TComponent);
begin
	inherited Create( AOwner );
	FLastCount := -1;
{ GetHandle; - If this object is instantiated at run-time, the user must clearly
							 try to access the handle to force the process to start. }
end;

destructor TKCustomDragDrop.Destroy;
begin
	ClearDropList;
	FreeClean( FDropList );
  RestoreWndProc;
  ClearReferences;
	inherited Destroy;
end;

procedure TKCustomDragDrop.RestoreWndProc;
begin
	if CheckObject( FSource ) and CheckReference( @FWndProc ) then
    if CheckObjectClass( FSource, TControl ) or ( FSource = Owner ) then
		begin
      ( FSource as TControl ).WindowProc := FWndProc;
      ClearReferences;
		end
    else
			RaiseExceptionFmt( EKDragDrop, sErrDDInvSource, [ClassName] );
end;

procedure TKCustomDragDrop.ClearReferences;
begin
	DragAcceptFiles( FHwnd, False );
  FHwnd := NULL_HANDLE_VALUE;
  FSource := nil;
  FWndProc := nil;
end;

procedure TKCustomDragDrop.DoGetHandle( var AHandle: HWnd );
begin
	RestoreWndProc;
  if Assigned( FOnGetHandle ) then
    FOnGetHandle( Self, FSource, AHandle );
	if ( not CheckObject( FSource ) ) and CheckObject( Owner ) and
		 CheckObjectClass( Owner, TWinControl ) then
		FSource := Owner;
	if ( AHandle = NULL_HANDLE_VALUE ) and CheckObject( FSource ) and
		CheckObjectClass( FSource, TWinControl ) then
		AHandle := ( FSource as TWinControl ).Handle;
end;

procedure TKCustomDragDrop.SetWndProc;
begin
	if ( CheckObject( FSource ) and CheckObjectClass( FSource, TControl ) and
			 CheckReference( @FWndProc ) and ( @FWndProc = @( FSource as TControl ).WindowProc ) ) then
    ( FSource as TControl ).WindowProc := WndProc;
end;

procedure TKCustomDragDrop.DoGetWndProc;
begin
  RestoreWndProc;
  if Assigned( FOnGetWndProc ) then
    FOnGetWndProc( Self, FSource, FWndProc );
  if ( not CheckObject( FSource ) ) and CheckObject( Owner ) and
		 CheckObjectClass( Owner, TControl ) then
	if ( ( not CheckReference( @FWndProc ) ) and CheckObject( FSource ) and
		CheckObjectClass( FSource, TControl ) ) then
    FWndProc := ( FSource as TControl ).WindowProc;
  SetWndProc;  
end;

procedure TKCustomDragDrop.DoDrop;
begin
  if Assigned( FOnDrop ) then
    FOnDrop( Self, Source, FDropList );
end;

procedure TKCustomDragDrop.DoDropPoint( const Dropped: Boolean; DropPoint: TPoint );
begin
  if Assigned( FOnDropPoint ) then
    FOnDropPoint( Self, FSource, Dropped, DropPoint.X, DropPoint.Y );
end;

procedure TKCustomDragDrop.Loaded;
begin
	inherited Loaded;
  GetHandle;
end;

procedure TKCustomDragDrop.SetHandle( Value: Hwnd );
begin
  if ( FHWnd <> Value ) then
  begin
    if ( FHWnd <> NULL_HANDLE_VALUE ) then
      RestoreWndProc;
    FSource := FindControl( Value );
    if ( not CheckObject( FSource ) ) then
			RaiseExceptionFmt( EKDragDrop, sErrDDInvSourceForHandle, [Value] );
    FHWnd := Value;
    HandleNeeded;
	end;
end;

function TKCustomDragDrop.GetHandle: Hwnd;
begin
	if ( FHwnd = NULL_HANDLE_VALUE ) and ( not Loading( Self ) ) then
    HandleNeeded;
  Result := FHwnd;
end;

procedure TKCustomDragDrop.CheckHandle;
begin
  if ( FHwnd = NULL_HANDLE_VALUE ) then
		RaiseException( EKDragDrop, sErrDDInvHandle );
end;

procedure TKCustomDragDrop.CheckWndProc;
begin
  if ( not CheckReference( @FWndProc ) ) then
		RaiseException( EKDragDrop, sErrDDInvWndProc );
end;

procedure TKCustomDragDrop.HandleNeeded;
begin
  if Designing( Self ) then
		Exit;
	DoGetHandle( FHwnd );
  CheckHandle;
  DoGetWndProc;
  CheckWndProc;
  if ( not CheckObject( FDropList ) ) then
    FDropList := TStringList.Create;
  DragAcceptFiles( FHwnd, True );
end;

procedure TKCustomDragDrop.ClearDropList;
begin
  if ( not CheckObject( FDropList ) ) then
    Exit;
  FDropList.Clear;
end;

procedure TKCustomDragDrop.WMDropFiles(var Message: TWMDropFiles);
var
	i: Cardinal;
	FSize: Integer;
	FBool: Boolean;
  p: TPoint;
  s: string;
begin
  ClearDropList;
	FLastCount := DragQueryFile( Message.Drop, DRAG_QUERY_COUNT, nil, 0 );
	if ( FLastCount > 0 ) and Assigned( FOnDrop ) then
  begin
		for i := 0 to FLastCount - 1 do
			try
        FSize := DragQueryFile( Message.Drop, i, nil, 0 );
{
	using StrAlloc with a pchar we raise a memory leak because ClearDropList
	(StrDispose) raises a invalid pointer operation and we cannot free the
	allocated PChar.
}
				SetLength( s, FSize );
        DragQueryFile( Message.Drop, i, PChar( s ), FSize + 1 );
        FDropList.Add( s );
			except
        ClearDropList;
				raise;
			end;
    DoDrop;
  end;
  if Assigned( FOnDropPoint ) then
  begin
		ZeroMemory( @p, SizeOf( TPoint ) );
    FBool := DragQueryPoint( Message.Drop, p );
    DoDropPoint( FBool, p );
  end;
  DragFinish( Message.Drop );
end;

procedure TKCustomDragDrop.WndProc(var Message: TMessage);
begin
  CheckWndProc;
  FWndProc( Message );
  if ( Message.Msg = WM_DROPFILES ) then
		Dispatch( Message );
end;

{ TKBrowseFolderDialog }

const
	KM_BROWSEFOLDERCALLBACK = WM_USER + 1;

type

	PSHBrowseFolderCallbackData = ^TSHBrowseFolderCallbackData;
	TSHBrowseFolderCallbackData = record
		Msg: UINT;
		Handle: HWND;
	end;

function SHBrowseFolderInternalCallback( AHandle: HWND; uMsg: UINT; lParam,
	lpData: LPARAM ): Integer; stdcall;
var
	psbfcd: PSHBrowseFolderCallbackData;
begin
{ Always return zero (see API Reference for ShellBrowseForFolder->BROWSEINFO structure }
	Result := 0;
	psbfcd := New( PSHBrowseFolderCallbackData );
	try
		ZeroMemory( psbfcd, SizeOf( TSHBrowseFolderCallbackData ) );
		with psbfcd^ do
		begin
			Msg := uMsg;
			Handle := AHandle;
		end;
		SendMessage( lpData, KM_BROWSEFOLDERCALLBACK, Integer( psbfcd ), lParam );
	finally
		Dispose( psbfcd );
	end;
end;

constructor TKBrowseFolderDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTop := 40;
	FLeft := 60;
	FHandle := 0;
	HandleNeeded;
	FCentered := true;
	FRootItemIDList := nil;
	FGoBelowDomain := true;
	FStyle := bffFileSystem;
	FShowStatusText := false;
	pbi := New( PBrowseInfo );
	ZeroMemory( pbi, SizeOf( TBrowseInfo ) );
	FPosChanged := false;
	FCallback := SHBrowseFolderInternalCallback;
	FTitle := StrAlloc( MAX_PATH + 1 );
	ZeroMemory( FTitle, ( MAX_PATH + 1 ) );
	FFolderName := StrAlloc( MAX_PATH + 1 );
	ZeroMemory( FFolderName, ( MAX_PATH + 1 ) );
	with pbi^ do
	begin
		hwndOwner := Application.Handle;
		pidlRoot := FRootItemIDList;
		pszDisplayName := FFolderName;
		lpszTitle := FTitle;
		ulFlags := 0;
		lpfn := FCallback;
		lParam := FHandle;
	end;
end;

destructor TKBrowseFolderDialog.Destroy;
begin
	StrDispose( FTitle );
	StrDispose( FFolderName );
	Dispose( pbi );
	if CheckHandle( FHandle ) then
		DeallocateHWnd( FHandle );
	inherited Destroy;
end;

procedure TKBrowseFolderDialog.HandleNeeded;
begin
	if ( not CheckHandle( FHandle ) ) then
  	FHandle := AllocateHWnd( WndProc );
end;

function TKBrowseFolderDialog.StyleToFlags( Value: TKBrowseFolderFlags ): UINT;
begin
	case Value of
		bffAll,
		bffCustom: Result := 0;
		bffComputers: Result := BIF_BROWSEFORCOMPUTER;
		bffPrinters: Result := BIF_BROWSEFORPRINTER;
		bffAncestors: Result := BIF_RETURNFSANCESTORS;
	else
		Result := BIF_RETURNONLYFSDIRS;
	end;
end;

procedure TKBrowseFolderDialog.UpdateBrowseFolderDialog( AHandle: HWND; pil: PItemIDList;
	const sFolder, sStatus: string; bEnableOK: Boolean );
var
	pcPathName: PChar;
begin
	pcPathName := StrAlloc( MAX_PATH + 1 );
	try
		if CheckTrimStr( sFolder ) then
		begin
			SHGetPathFromIDList( pil, pcPathName );
			SendMessage( AHandle, BFFM_SETSELECTION, Integer( true ), LongInt( PChar( pcPathName ) ) );
		end;
		if ( CheckTrimStr( sStatus ) and FShowStatusText ) then
			SendMessage( AHandle, BFFM_SETSTATUSTEXT, 0, LongInt( PChar( sStatus ) ) );
	finally
		StrDispose( pcPathName );
	end;
	if ( FStyle = bffCustom ) then
		SendMessage( AHandle, BFFM_ENABLEOK, 0, Integer( bEnableOK ) );
end;

procedure TKBrowseFolderDialog.DoInitialized( AHandle: HWND; pil: PItemIDList );
var
	sFolder,
	sStatus: string;
	bEnableOK: Boolean;
begin
	if Assigned( FOnInitialize ) then
	begin
		sFolder := '';
		sStatus := '';
		bEnableOK := true;
		FOnInitialize( Self, sFolder, sStatus, bEnableOK );
		UpdateBrowseFolderDialog( AHandle, pil, sFolder, sStatus, bEnableOK );
	end;
end;

procedure TKBrowseFolderDialog.DoSelChanged( AHandle: HWND; pil: PItemIDList );
var
	sFolder,
	sStatus: string;
	bEnableOK: Boolean;
begin
	if Assigned( FOnSelChanged ) then
	begin
		sFolder := '';
		sStatus := '';
		bEnableOK := true;
		FOnSelChanged( Self, sFolder, sStatus, bEnableOK );
		UpdateBrowseFolderDialog( AHandle, pil, sFolder, sStatus, bEnableOK );
	end;
end;

procedure TKBrowseFolderDialog.WndProc( var Message: TMessage );
var
	psbfcd: PSHBrowseFolderCallbackData;
begin
	Message.Result := 0;
	if ( Message.Msg <> KM_BROWSEFOLDERCALLBACK ) then
		Exit;
	Message.Result := 1;
	psbfcd := PSHBrowseFolderCallbackData( Message.WParam );
	if ( not FPosChanged ) then
	begin
		FPosChanged := true;
		if FCentered then
			CenterWindow( psbfcd^.Handle )
{
			GetWindowRect( psbfcd^.Handle, Rect );
			SetWindowPos( psbfcd^.Handle, 0,
				( GetSystemMetrics( SM_CXSCREEN ) - Rect.Right + Rect.Left ) div 2,
				( GetSystemMetrics( SM_CYSCREEN ) - Rect.Bottom + Rect.Top ) div 3,
				0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER )
}
		else
			SetWindowPos( psbfcd^.Handle, 0, FLeft, FTop, 0, 0,
				SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER );
	end;
	if ( psbfcd^.Msg = BFFM_INITIALIZED ) then
		DoInitialized( psbfcd^.Handle, PItemIDList( Message.LParam ) )
	else if ( psbfcd^.Msg = BFFM_SELCHANGED ) then
		DoSelChanged( psbfcd^.Handle, PItemIDList( Message.LParam ) );
end;

function TKBrowseFolderDialog.GetRoot: WideString;
begin
	Result := FRoot;
end;

function TKBrowseFolderDialog.GetTitle: String;
begin
	Result := FTitle;
end;

function TKBrowseFolderDialog.GetFolderName: String;
begin
	Result := FFolderName;
end;

procedure TKBrowseFolderDialog.SetStyle( Value: TKBrowseFolderFlags );
begin
	FStyle := Value;
	pbi^.ulFlags := StyleToFlags( FStyle );
	if ( not FGoBelowDomain ) then
		pbi^.ulFlags := ( pbi^.ulFlags or BIF_DONTGOBELOWDOMAIN );
	if FShowStatusText then
		pbi^.ulFlags := ( pbi^.ulFlags or BIF_STATUSTEXT );
end;

procedure TKBrowseFolderDialog.SetGoBelowDomain( Value: Boolean );
begin
	FGoBelowDomain := Value;
	pbi^.ulFlags := ( pbi^.ulFlags and ( not BIF_DONTGOBELOWDOMAIN ) );
	if ( not FGoBelowDomain ) then
		pbi^.ulFlags := ( pbi^.ulFlags or BIF_DONTGOBELOWDOMAIN );
end;

procedure TKBrowseFolderDialog.SetShowStatusText( Value: Boolean );
begin
	FShowStatusText := Value;
	pbi^.ulFlags := ( pbi^.ulFlags and ( not BIF_STATUSTEXT ) );
	if FShowStatusText then
		pbi^.ulFlags := ( pbi^.ulFlags or BIF_STATUSTEXT );
end;

procedure TKBrowseFolderDialog.SetRoot( const Value: WideString );
var
	isf: IShellFolder;
	Eaten, Flags: DWORD;
begin
	OleCheck( SHGetDesktopFolder( isf ) );
	try
		FRoot := Value;
		isf.ParseDisplayName( Application.Handle, nil, PWideChar( FRoot ), Eaten,
			FRootItemIDList, Flags );
	finally
		isf._Release;
	end;
	pbi^.pidlRoot := FRootItemIDList;
end;

procedure TKBrowseFolderDialog.SetTitle( const Value: String );
begin
	StrPCopy( FTitle, Value );
end;

procedure TKBrowseFolderDialog.SetFolderName( const Value: String );
begin
end;

function TKBrowseFolderDialog.Execute: Boolean;
var
	pil: PItemIDList;
begin
	pil := SHBrowseForFolder( pbi^ );
	Result := CheckPointer( pil );
	if Result then
	begin
		pbi^.pidlRoot := pil;
		SHGetPathFromIDList( pil, FFolderName );
	end;
end;

{ TKPageSetupDialog }

var
	HookCtl3D: Boolean = False;
	PageSetUpInstance: TKPageSetupDialog = nil;
	WM_PSD_INTERNAL_HELP: Cardinal = 0;

function PageSetUpDialogInternalHook( Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
	case Msg of
    WM_INITDIALOG:
    begin
			ForceObject( PageSetUpInstance );
      PageSetUpInstance.FHandle := Wnd;
			PageSetUpInstance.FDefWndProc := Pointer( SetWindowLong( Wnd, GWL_WNDPROC,
				Longint( PageSetUpInstance.FPageSetupHook ) ) );
      CallWindowProc( PageSetUpInstance.FPageSetupHook, Wnd, Msg, WParam, LParam);
      PageSetUpInstance := nil;
		end;
  end;
end;

constructor TKPageSetupDialog.Create(AOwner: TComponent);
var
  r: TRect;
  p: TPoint;
begin
  inherited Create(AOwner);
  FOptions := [poDefaultMinMargins, poShowHelp];
  FTop := 40;
	FLeft := 60;
	FCentered := true;
	FPosChanged := false;
  FHandle := 0;
  FDefWndProc := nil;
	FPrinterInfo := TKPrinterCanvasInfo.CreateFromPrinter( Printer );
  FPageSetupHook := MakeObjectInstance( InternalWndProc );
  with FPrinterInfo do
	begin
    p := Point( DeviceWidth, DeviceHeight );
    r := Gutter.Rect;
  end;
  FPaperSize := TKPoint.Create( @p );
	FMinimumMargins := TKRect.Create( @r );
  FMargins := TKRect.Create( @r );
  FMarginsMeasures := TKRectMeasures.CreateLinked( Self, GetMeasures, SetMeasures );
  FMinimumMarginsMeasures := TKRectMeasures.CreateLinked( Self, GetMeasures, SetMeasures );
  FPaperSizeMeasures := TKPointMeasures.CreateLinked( Self, GetMeasures, SetMeasures );
	FMeasurements := pmMillimeters;
end;

destructor TKPageSetupDialog.Destroy;
begin
  FMarginsMeasures.Free;
  FMinimumMarginsMeasures.Free;
  FPaperSizeMeasures.Free;
	FPaperSize.Free;
  FMinimumMargins.Free;
  FMargins.Free;
  FPrinterInfo.Free;
	if CheckPointer( FPageSetUpHook ) then
    FreeObjectInstance( FPageSetUpHook );
  inherited Destroy;
end;

procedure TKPageSetupDialog.GetMeasures( Sender: TObject; Index: TKScalingFlag; var Value: Single );
begin
  with FPrinterInfo do
    case FMeasurements of
			pmMillimeters :
        if ( Sender = FMinimumMarginsMeasures ) then
          case Index of
            ksfLeft  : Value := PixelsToMM_X( FMinimumMargins.Left );
            ksfTop   : Value := PixelsToMM_Y( FMinimumMargins.Top );
						ksfRight : Value := PixelsToMM_X( FMinimumMargins.Right );
            ksfBottom: Value := PixelsToMM_Y( FMinimumMargins.Bottom );
          end
				else if ( Sender = FMarginsMeasures ) then
          case Index of
            ksfLeft  : Value := PixelsToMM_X( FMargins.Left );
            ksfTop   : Value := PixelsToMM_Y( FMargins.Top );
            ksfRight : Value := PixelsToMM_X( FMargins.Right );
						ksfBottom: Value := PixelsToMM_Y( FMargins.Bottom );
          end
        else if ( Sender = FPaperSizeMeasures ) then
          case Index of
						ksfLeft  : Value := PixelsToMM_X( FPaperSize.cX );
            ksfTop   : Value := PixelsToMM_Y( FPaperSize.cY );
          end;
      pmInches      :
        if ( Sender = FMinimumMarginsMeasures ) then
          case Index of
						ksfLeft  : Value := PixelsToInch_X( FMinimumMargins.Left );
            ksfTop   : Value := PixelsToInch_Y( FMinimumMargins.Top );
						ksfRight : Value := PixelsToInch_X( FMinimumMargins.Right );
            ksfBottom: Value := PixelsToInch_Y( FMinimumMargins.Bottom );
          end
        else if ( Sender = FMarginsMeasures ) then
          case Index of
						ksfLeft  : Value := PixelsToInch_X( FMargins.Left );
            ksfTop   : Value := PixelsToInch_Y( FMargins.Top );
            ksfRight : Value := PixelsToInch_X( FMargins.Right );
            ksfBottom: Value := PixelsToInch_Y( FMargins.Bottom );
					end
        else if ( Sender = FPaperSizeMeasures ) then
          case Index of
            ksfLeft : Value := PixelsToInch_X( FPaperSize.cX );
            ksfTop  : Value := PixelsToInch_Y( FPaperSize.cY );
          end;
    end;
end;

procedure TKPageSetupDialog.SetMeasures( Sender: TObject; Index: TKScalingFlag; Value: Single );
begin
  with FPrinterInfo do
    case FMeasurements of
			pmMillimeters :
        if ( Sender = FMinimumMarginsMeasures ) then
          case Index of
            ksfLeft  : FMinimumMargins.Left := MMToPixels_X( Value );
						ksfTop   : FMinimumMargins.Top := MMToPixels_Y( Value );
            ksfRight : FMinimumMargins.Right := MMToPixels_X( Value );
            ksfBottom: FMinimumMargins.Bottom := MMToPixels_Y( Value );
          end
        else if ( Sender = FMarginsMeasures ) then
          case Index of
            ksfLeft  : FMargins.Left := MMToPixels_X( Value );
            ksfTop   : FMargins.Top := MMToPixels_Y( Value );
						ksfRight : FMargins.Right := MMToPixels_X( Value );
            ksfBottom: FMargins.Bottom := MMToPixels_Y( Value );
          end
        else if ( Sender = FPaperSizeMeasures ) then
          case Index of
						ksfLeft  : FPaperSize.cX := MMToPixels_X( Value );
            ksfTop   : FPaperSize.cY := MMToPixels_Y( Value );
          end;
      pmInches      :
				if ( Sender = FMinimumMarginsMeasures ) then
          case Index of
            ksfLeft  : FMinimumMargins.Left := InchToPixels_X( Value );
            ksfTop   : FMinimumMargins.Top := InchToPixels_Y( Value );
            ksfRight : FMinimumMargins.Right := InchToPixels_X( Value );
            ksfBottom: FMinimumMargins.Bottom := InchToPixels_Y( Value );
          end
        else if ( Sender = FMarginsMeasures ) then
					case Index of
						ksfLeft  : FMargins.Left := InchToPixels_X( Value );
            ksfTop   : FMargins.Top := InchToPixels_Y( Value );
            ksfRight : FMargins.Right := InchToPixels_X( Value );
            ksfBottom: FMargins.Bottom := InchToPixels_Y( Value );
					end
        else if ( Sender = FPaperSizeMeasures ) then
					case Index of
						ksfLeft  : FPaperSize.cX := InchToPixels_X( Value );
						ksfTop   : FPaperSize.cY := InchToPixels_Y( Value );
					end;
		end;
end;

procedure TKPageSetupDialog.InternalWndProc( var Message: TMessage );
const
	PRINTER_MASK = $00000002;
	ORIENT_MASK  = $00000004;
	PAPER_MASK   = $00000008;

	PAINT_OPTION: array[WM_PSD_FULLPAGERECT..WM_PSD_YAFULLPAGERECT] of TKPaintOption =
	 ( poFullPage, poMinimumMargins, poMargins, poGreekText, poEnvStamp, poYAFullPage );
 
	PAPER_DATA_MASK: array[Boolean] of TKPaperType = ( ptPaper, ptEnvelope );
	ORIENT_DATA_MASK: array[Boolean] of TKPaperOrientation = ( uksyClasses.poLandscape,
		uksyClasses.poPortrait );
	PRINTER_DATA_MASK: array[Boolean] of TKPrinterType = ( ptHPPCL, ptDotMatrix );

	ID_PRINTER_BTN = Dlgs.psh3;


var
	PaperData: Word;
	psInfo: PKPageSetUpDialogInfo;
	PaintRect: TRect;
	PaintCanvas: TCanvas;

begin
	with Message do
		case Msg of
			WM_INITDIALOG:
				begin
          if HookCtl3D then
					begin
						Subclass3DDlg( FHandle, CTL3D_ALL );
						SetAutoSubClass( True );
					end;
					if FCentered then
						CenterWindow( FHandle )
					else
						SetWindowPos( FHandle, 0, FLeft, FTop, 0, 0,
							SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER );
					Result := 1;
				end;
      WM_COMMAND:
				if ( LongRec( WParam ).Lo = ID_PRINTER_BTN ) and
					 ( LongRec( WParam ).Hi = BN_CLICKED ) then
					Result := Ord( DoPrinter );
			WM_DESTROY:
        if HookCtl3D then
					SetAutoSubClass( False );
      WM_NCDESTROY:
        begin
          FHandle := NULL_HANDLE_VALUE;
					FDefWndProc := nil;
				end;
			WM_PSD_PAGESETUPDLG:
				if Assigned( FOnInitPaintPage ) and Assigned( FOnPaintPage ) then
				begin
					PSInfo := New( PKPageSetUpDialogInfo );
					try
						ZeroMemory( psInfo, SizeOf( TKPageSetUpDialogInfo ) );
						PaperData := HiWord( WParam );
						with PSInfo^ do
						begin
							sPaperSize := LoWord( WParam );
							ptPaperType := PAPER_DATA_MASK[( PaperData and PAPER_MASK > 0 )];
							poPaperOrientation := ORIENT_DATA_MASK[( PaperData and ORIENT_MASK > 0 )];
							prtPrinterType := PRINTER_DATA_MASK[( PaperData and PAPER_MASK > 0 )];
							lpPageSetUpDlg := PPageSetupDlg( LParam );
						end;
						Result := Ord( DoInitPaint( PSInfo ) );
					finally
						Dispose( PSInfo );
					end;
				end;
			WM_PSD_FULLPAGERECT,
      WM_PSD_MINMARGINRECT,
      WM_PSD_MARGINRECT,
      WM_PSD_GREEKTEXTRECT,
			WM_PSD_ENVSTAMPRECT,
			WM_PSD_YAFULLPAGERECT:
        if Assigned( FOnInitPaintPage ) and Assigned( FOnPaintPage ) then
        begin
					if ( LParam <> 0 )then
            PaintRect := PRect(LParam)^
          else
						PaintRect := Rect( 0, 0, 0, 0 );
					PaintCanvas := TCanvas.Create;
					try
						PaintCanvas.Handle := HDC(WParam);
						Result := Ord( DoPaintPage( PAINT_OPTION[Msg], PaintCanvas, PaintRect ) );
					finally
						PaintCanvas.Free;
					end;
				end;
				else if ( Msg = WM_PSD_INTERNAL_HELP ) then
					Result := Ord( Self.MessageHook( Message ) );
		end;
	Dispatch( Message );
end;

procedure TKPageSetupDialog.DefaultHandler( var Message );
begin
	if CheckHandle( FHandle ) and CheckPointer( FDefWndProc ) then
		with TMessage( Message ) do
			Result := CallWindowProc( FDefWndProc, FHandle, Msg, WParam, LParam )
	else
    inherited DefaultHandler( Message );
end;

function TKPageSetupDialog.DoInitPaint( PSInfo: PKPageSetUpDialogInfo ): Boolean;
begin
  try
		Result := Assigned( FOnInitPaintPage );
		if Result then
      FOnInitPaintPage( Self, PSInfo, Result );
	except
		Result := False;
    Application.HandleException( Self );
  end;
end;

function TKPageSetupDialog.DoPaintPage( PaintOption: TKPaintOption;
	PaintCanvas: TCanvas; PaintRect: TRect ): Boolean;
begin
	try
		Result := Assigned( FOnPaintPage );
		if Result then
			FOnPaintPage( Self, PaintOption, PaintCanvas, PaintRect, Result );
	except
		Result := False;
		Application.HandleException( Self );
	end;
end;

function TKPageSetupDialog.DoPrinter: boolean;
begin
	try
		Result := Assigned( FOnPageSetUp );
		if Result then
			FOnPageSetUp( Self );
	except
		Result := false;
		Application.HandleException(Self);
	end;
end;

function TKPageSetupDialog.DoExecute( Func: Pointer ): Boolean;
const
	PAGESETUP_OPTIONS: array [TKPageSetupOption] of DWORD = (
			PSD_DEFAULTMINMARGINS, PSD_DISABLEMARGINS, PSD_DISABLEORIENTATION,
			PSD_DISABLEPAGEPAINTING, PSD_DISABLEPAPER, PSD_DISABLEPRINTER,
			PSD_NOWARNING, PSD_SHOWHELP
		);
	PAGESETUP_MEASUREMENTS: array [TKMeasurements] of DWORD = (
			PSD_INHUNDREDTHSOFMILLIMETERS, PSD_INTHOUSANDTHSOFINCHES
		);
		
var
	Option: TKPageSetupOption;
	pgSetup: TPageSetupDlg;
	DevHandle: THandle;
begin
	Result := False;
	ZeroMemory( @pgSetup, SizeOf( TPageSetupDlg ) );
  with pgSetup do
    try
      lStructSize := SizeOf( TPageSetupDlg );
			lCustData := LPARAM( nil );
			hInstance := System.MainInstance;
			Flags := PSD_MARGINS;
      if Assigned( FOnPageSetUp ) or Assigned( FOnInitPaintPage ) or
         Assigned( FOnPaintPage ) then
      begin
        Flags := ( Flags or PSD_ENABLEPAGESETUPHOOK );
        lpfnPageSetupHook := PageSetUpDialogInternalHook;
			end;
      for Option := Low( TKPageSetupOption ) to High( TKPageSetupOption ) do
				if ( Option in FOptions ) then
          Flags := ( Flags or PAGESETUP_OPTIONS[Option] );
			Flags := ( Flags or PAGESETUP_MEASUREMENTS[FMeasurements] );
  {   if not Assigned( FOnPageSetUp ) then
        Flags := ( Flags or PSD_DISABLEPRINTER ); }
			if ( Assigned( FOnInitPaintPage ) and Assigned( FOnPaintPage ) ) then
			begin
				Flags := ( Flags or PSD_ENABLEPAGEPAINTHOOK );
				lpfnPagePaintHook := PageSetUpDialogInternalHook;
      end;
			hWndOwner := Application.Handle;
			GetPrinter( DevHandle, hDevNames );
			hDevMode := CopyMemGlobalData( DevHandle );
			ptPaperSize := PointMulBy( FPaperSizeMeasures.Point, ADJUST_RECT_SCALE[Self.Measurements] );
			rtMinMargin := RectMulBy( FMinimumMarginsMeasures.Rect, ADJUST_RECT_SCALE[Self.Measurements] );
			rtMargin := RectMulBy( FMarginsMeasures.Rect, ADJUST_RECT_SCALE[Self.Measurements] );
      HookCtl3D := Ctl3D;
			PageSetUpInstance := Self;
      Result := TaskModalDialog( Func, pgSetup );
		finally
      if Result then
      begin
				FPaperSizeMeasures.Point := PointDivBy( ptPaperSize, ADJUST_RECT_SCALE[Self.Measurements] );
			{ FMinimumMarginsMeasures.Rect := RectDivBy( rtMinMargin, ADJUST_RECT_SCALE[Self.Measurements] ); }
				FMarginsMeasures.Rect := RectDivBy( rtMargin, ADJUST_RECT_SCALE[Self.Measurements] );
				uksyUtils.SetPrinter( hDevMode, hDevNames );
			end
			else
			begin
				if ( hDevMode <> NULL_HANDLE_VALUE ) then
					GlobalFree( hDevMode );
				if ( hDevNames <> NULL_HANDLE_VALUE ) then
					GlobalFree( hDevNames );
			end;
		end;
end;

function TKPageSetupDialog.Execute: boolean;
begin
	Result := DoExecute( @PageSetupDlg );
end;

{ TKNetworkDialog }

function TKNetworkDialog.GetErrorMessage: string;
var
	iError: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	sProvider: string;
begin
	if ( FLastError = ERROR_SUCCESS ) then
		Result := ''
	else if ( FLastError = ERROR_EXTENDED_ERROR ) then
	begin
		SetString( Result, nil, SizeOf( ShortString ) );
		SetString( Result, nil, SizeOf( ShortString ) );
		iError := FLastError;
		FLastError := WNetGetLastError( iError, PChar( Result ), SizeOf( ShortString ),
			PChar( sProvider ), SizeOf( ShortString ) );
		SetLength( Result, StrLen( PChar( Result ) ) );
		SetLength( sProvider, StrLen( PChar( sProvider ) ) );
		if ( FLastError <> ERROR_SUCCESS ) and ( FLastError <> ERROR_EXTENDED_ERROR ) then
			Result := SysErrorMessage( FLastError )
		else
			Result := Format( sNetDlgExtErrMsg, [sProvider, Result] );
	end
	else
		Result := SysErrorMessage( FLastError );
end;

function TKNetworkDialog.Execute: Boolean;
const
	RESOURCE_TYPE: array[TKNetworkResourceType] of DWORD =
		( RESOURCETYPE_DISK, RESOURCETYPE_PRINT );
begin
	FLastError := ERROR_SUCCESS;
	if ( FNetDialogType = ndtConnect ) then
		FLastError := WNetConnectionDialog( 0, RESOURCETYPE_DISK )
	else if ( FNetDialogType = ndtDisconnect ) then
		FLastError := WNetDisconnectDialog( 0, RESOURCE_TYPE[FNetResourceType] );
	Result := ValueBetween( FLastError, -1, ERROR_SUCCESS, True );
	if ( not Result ) then
		FLastError := GetLastError
	else if Assigned( FOnExecute ) then
		FOnExecute( Self );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	WM_PSD_INTERNAL_HELP := RegisterWindowMessage( HelpMsgString );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
