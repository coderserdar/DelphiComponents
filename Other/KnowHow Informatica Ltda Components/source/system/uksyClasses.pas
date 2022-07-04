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

unit uksyClasses;

{$I s:\v100\include\iKLIB100.inc}
{$BOOLEVAL OFF}

interface

uses
	Windows, WinSock, SysUtils, Messages, Classes, Graphics, StdCtrls, Controls,
	Forms, Printers, uksyConsts, uksyTypes, uksyUtils;

type

	EKSYClasses = class( EKSystem );

{
--------------------------------------------------------------------------------
---------------------------- Generic IPC Objects -------------------------------
--------------------------------------------------------------------------------
}

	EKWinAPI = class( EKSYClasses );
	EKIPC = class( EKWinAPI );
	EKCriticalSection = class( EKIPC );
	EKKernelObject = class( EKIPC );
	EKKernelObjPool = class( EKIPC );
	EKThreadLocalStorage = class( EKIPC );
	EKSyncObject = class( EKKernelObject );
	EKEvent = class( EKSyncObject );
	EKMutex = class( EKSyncObject );
	EKSemaphore = class( EKSyncObject );
	EKThread = class( EKKernelObject );
	EKProcess = class( EKKernelObject );
                            
{ TKCriticalSection }

	TKCriticalSection = class( TObject )
	private
		FLockCount: Integer;
		FCriticalSection: TRTLCriticalSection;

	protected
		property RTLCritialSection: TRTLCriticalSection
						 read FCriticalSection;

	public
		destructor Destroy; override;
		constructor Create( StartLocked: Boolean ); virtual;

		procedure Lock; dynamic;
		procedure UnLock; dynamic;

		property LockCount: LongInt
						 read FLockCount;

	end;

{ TKKernelObject }

	TKWaitResult = ( kwrUnknown, kwrSignaled, kwrTimeout, kwrAbandoned, kwrFail );

	TKKernelObject = class( TObject )
	private
		FHandle: THandle;
		FLastError: Cardinal;
		FCrtSec: TKCriticalSection;
		FLastWaitResult: TKWaitResult;
		FSecurityAttributes: PSecurityAttributes;

		FOnFail: TNotifyEvent;
		FOnTimeOut: TNotifyEvent;

		function GetLastError: Cardinal;
		function GetLastWaitResult: TKWaitResult;
		function GetLastErrorMessage: string;
		function GetInheritHandles: Boolean;
		function GetEvents( Index: Integer ): TNotifyEvent;
		procedure SetEvents( Index: Integer; Value: TNotifyEvent );

	protected
		function GetHandle: THandle; virtual;
		procedure SetLastError( Value: Cardinal ); virtual;
		procedure CloseHandle; virtual;

		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer ); 

		procedure DoFail; dynamic;
		procedure DoTimeOut; dynamic;

		property CrtSec: TKCriticalSection
						 read FCrtSec;
		property Handle: THandle
						 read GetHandle;
		property InheritHandles: Boolean
						 read GetInheritHandles;
		property SecurityAttributes: PSecurityAttributes
						 read FSecurityAttributes;
		property LastWaitResult: TKWaitResult
						 read GetLastWaitResult;
		property LastError: Cardinal
						 read GetLastError;
		property LastErrorMessage: string
						 read GetLastErrorMessage;

		property OnFail: TNotifyEvent
						 index 0 read GetEvents write SetEvents;
		property OnTimeOut: TNotifyEvent
						 index 1 read GetEvents write SetEvents;

	public
		destructor Destroy; override;

		function UnLock: Boolean; virtual; abstract;
		function Lock( AWaitTime: Cardinal ): Boolean; virtual; 

		function WaitFor( AWaitTime: Cardinal ): TKWaitResult; virtual;
		function WaitForEx( AWaitTime: Cardinal; AlertAble: Boolean ): TKWaitResult; virtual;
		procedure Sleep( ASleepTime: Cardinal ); virtual;
		procedure SleepEx( ASleepTime: Cardinal; AlertAble: Boolean ); virtual;

		function ChangeInheritance( IsInheritable: Boolean ): Boolean;
		
	end;

	TKKernelObjectrClass = class of TKKernelObject;

{ TKSyncObject }

	TKSyncObject = class( TKKernelObject )
	private
		FName: string;
		FOpenInheritable: Boolean;

	protected
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			const AName: string );

		procedure HandleNeeded( IsOpen: Boolean ); virtual; abstract;

		property Name: string
						 read FName;
		property OpenInheritable: Boolean
						 read FOpenInheritable;

	public
	  destructor Destroy; override;
		constructor CreateOpen( CanInherit: Boolean; const AName: string ); virtual;

	end;

  TKSyncObjectClass = class of TKSyncObject;

{ TKCustomEvent }

	TKCustomEvent = class( TKSyncObject )
	private
		FManualReset: Boolean;
		FInitialState: Boolean;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure HandleNeeded( IsOpen: Boolean ); override;

	protected
		property ManualReset: Boolean
						 read FManualReset;
		property InitialState: Boolean
						 read FInitialState;
						 
	public
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			AManualReset, AInitialState: Boolean; const AName: string ); virtual;

		function SetEvent: Boolean; dynamic;
		function ResetEvent: Boolean; dynamic;
		function PulseEvent: Boolean; dynamic;

		function UnLock: Boolean; override;
		function Lock( AWaitTime: Cardinal ): Boolean; override;

	end;

{ TKEvent }

	TKEvent = class( TKCustomEvent )
	public
		property Handle;
		property Name;
		property InheritHandles;
		property OpenInheritable;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property OnFail;
		property OnTimeOut;

		property ManualReset;
		property InitialState;

	end;

{ TKSimpleEvent }

	TKSimpleEvent = class( TKEvent )
	public
		constructor CreateSimple; virtual;

	end;

{ TKCustomSemaphore }

	TKCustomSemaphore = class( TKSyncObject )
	private
		FMaxCount: Cardinal;
		FInitialCount: Cardinal;
		FPreviousCount: Cardinal;

		function GetPreviousCount: Cardinal;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure HandleNeeded( IsOpen: Boolean ); override;
		procedure CloseHandle; override;

	protected
		property InitialCount: Cardinal
						 read FInitialCount;
		property MaxCount: Cardinal
						 read FMaxCount;
		property PreviousCount: Cardinal
						 read GetPreviousCount;

	public
		destructor Destroy; override;
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			AInitialCount, AMaxCount: Cardinal; const AName: string ); virtual;

		function UnLock: Boolean; override;
		function UnlockEx( Count: Cardinal ): Boolean; dynamic;

	end;

{ TKSemaphore }

	TKSemaphore = class( TKCustomSemaphore )
	public
		property Handle;
		property Name;
		property InheritHandles;
		property OpenInheritable;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property OnFail;
		property OnTimeOut;

		property InitialCount;
		property MaxCount;
		property PreviousCount;

	end;

{ TKSimpleSemaphore }

	TKSimpleSemaphore = class( TKSemaphore )
	public
		constructor CreateSimple( AInitialCount, AMaxCount: Cardinal ); virtual;

	end;

{ TKCustomMutex }

	TKCustomMutex = class( TKSyncObject )
	private
		FInitialOwned: Boolean;
		FOnAbandoned: TNotifyEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}

		procedure HandleNeeded( IsOpen: Boolean ); override;

	protected
		procedure DoAbandoned; dynamic;

		property InitialOwned: Boolean
						 read FInitialOwned;
		property OnAbandoned: TNotifyEvent
						 read FOnAbandoned write FOnAbandoned;

	public
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			AnInitialOwned: Boolean; const AName: string ); virtual;

		function Unlock: Boolean; override;

		function WaitFor( AWaitTime: Cardinal ): TKWaitResult; override;
		function WaitForEx( AWaitTime: Cardinal; AlertAble: Boolean ): TKWaitResult; override;

	end;

{ TKMutex }

	TKMutex = class( TKCustomMutex )
	public
		property Handle;
		property Name;
		property InheritHandles;
		property OpenInheritable;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property InitialOwned;
		property OnFail;
		property OnTimeOut;
		property OnAbandoned;

	end;

{ TKSimpleMutex }

	TKSimpleMutex = class( TKMutex )
	public
		constructor CreateSimple; virtual;

	end;

{ TKSingletonMutex }

	TKSingletonMutex = class( TKMutex )
	private
		FObjectClass: TClass;

	public
		constructor CreateSingleton( AClassType: TClass ); virtual;

		property ObjectClass: TClass
						 read FObjectClass;

	end;

{ TKCustomThread }

	PKKernelObjTimes = ^TKKernelObjTimes;
	TKKernelObjTimes = record
		CreationTime: TFileTime;
		ExitTime: TFileTime;
		KernelTime: TFileTime;
		UserTime: TFileTime;
	end;

	TKCustomThread = class;

	TKThreadPriority = ( ktpIdle, ktpLowest, ktpLower, ktpNormal, ktpHigher, ktpHighest, ktpTimeCritical, ktpError );

	TKThreadFunc = function( Sender: TKCustomThread; Param: Pointer ): Cardinal of object;

	TKCustomThread = class( TKKernelObject )
	private
		FThreadID: Cardinal;
		FSuspended: Boolean;
		FFinished: Boolean;
		FReturnValue: Integer;
		FTerminated: Boolean;
		FStackSize: Cardinal;
		FCreationFlags: Cardinal;
		FFreeOnTerminate: Boolean;
    FThreadUnLockTerminate: Boolean;
		FLastSuspendCount: Cardinal;
    FSynchEvent: TKSimpleEvent;
		FExecuteFunc: TKThreadFunc;
		FTerminateProc: TNotifyEvent;
		FThreadWrapperPtr: Pointer;

		function GetThreadTimes: TKKernelObjTimes;
		function GetTerminated: Boolean;
		function GetPriority: TKThreadPriority;
		function GetSuspended: Boolean;
		function GetFreeOnTerminate: Boolean;
		function GetLastSuspendCount: Cardinal;
		procedure SetPriority( Value: TKThreadPriority );
		procedure SetSuspended( Value: Boolean );
		procedure SetFreeOnTerminate( Value: Boolean );

	protected
    procedure CloseHandle; override;

		property ThreadTimes: TKKernelObjTimes
						 read GetThreadTimes;
		property Terminated: Boolean
						 read GetTerminated;
		property LastSuspendCount: Cardinal
						 read GetLastSuspendCount;
		property FreeOnTerminate: Boolean
						 read GetFreeOnTerminate write SetFreeOnTerminate default True;
		property Priority: TKThreadPriority
						 read GetPriority write SetPriority default ktpNormal;
		property Suspended: Boolean
						 read GetSuspended write SetSuspended;
		property ThreadID: Cardinal
						 read FThreadID;
		property CreationFlags: Cardinal
						 read FCreationFlags;
		property StackSize: Cardinal
						 read FStackSize;
		property ReturnValue: Integer
						 read FReturnValue;
		property Finished: Boolean
						 read FFinished;
    property SynchEvent: TKSimpleEvent
             read FSynchEvent;

	public
		destructor Destroy; override;
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			AStackSize: Cardinal; AFreeOnTerminate: Boolean; AExecuteFunc: TKThreadFunc;
			Parameter: Pointer; ATerminateProc: TNotifyEvent; ACreationFlags: Cardinal ); virtual;

		procedure Resume; virtual;
		procedure Suspend; virtual;
		procedure Terminate; virtual;
		procedure TerminateEx( AWaitTime: Cardinal ); virtual;
		function UnLock: Boolean; override;

		procedure ForceTerminate; virtual;

	end;

	TKCustomThreadClass = class of TKCustomThread;  

{ TKThread }

	TKThread = class( TKCustomThread )
	public
		property Handle;
    property SynchEvent;
		property InheritHandles;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property OnFail;
		property OnTimeOut;

		property ThreadTimes;
		property Terminated;
		property LastSuspendCount;
		property FreeOnTerminate;
		property Priority;
		property Suspended;
		property ThreadID;
		property CreationFlags;
		property StackSize;
		property ReturnValue;
		property Finished;

	end;

{ TKSimpleThread }

	TKSimpleThread = class( TKThread )
	public
		constructor CreateSimple( AExecuteFunc: TKThreadFunc; Parameter: Pointer;
			ATerminateProc: TNotifyEvent ); virtual;

	end;

{ TKCustomWorkerThread }

  EKWorkerThread = class( EKThread );

	TKCustomWorkerThread = class;

	TKWorkerThreadExecuteEvent = procedure( Sender: TKCustomWorkerThread; Param: Pointer;
	  var ReturnCode: Cardinal; var Continue: Boolean ) of object;
	TKWorkerThreadExecuteMsgEvent = procedure( Sender: TKCustomWorkerThread; Param: Pointer;
	  Msg: TMsg; var Processed, Continue: Boolean ) of object;

	TKCustomWorkerThread = class( TKCustomThread )
	private
		FOwner: TObject;
    FMsgFilterMin: Integer;
    FMsgFilterMax: Integer;
    FMsgEvent: TKSimpleEvent;
		FOnTerminate: TNotifyEvent;
    FCreateMessageQueue: Boolean;
		FOnExecute: TKWorkerThreadExecuteEvent;
    FOnExecuteMsg: TKWorkerThreadExecuteMsgEvent;

		procedure ThreadTerminate( Sender: TObject );
		function ThreadExecute( Sender: TKCustomThread; Param: Pointer ): Cardinal;

	protected
		function GetOwner: TObject;

    function CheckExecuteCondition: Boolean; virtual;
		function DoExecute( Param: Pointer ): Cardinal; dynamic;
    function DoExecuteMsg( Param: Pointer; msg: TMsg; var Processed: Boolean ): Boolean; dynamic;

		procedure DoTerminate; dynamic;
		procedure SynchronizeOwner; virtual;

		property Owner: TObject
						 read GetOwner;
    property CreateMessageQueue: Boolean
             read FCreateMessageQueue;
		property OnExecute: TKWorkerThreadExecuteEvent
						 read FOnExecute write FOnExecute;
    property OnExecuteMsg: TKWorkerThreadExecuteMsgEvent
             read FOnExecuteMsg write FOnExecuteMsg;
		property OnTerminate: TNotifyEvent
						 read FOnTerminate write FOnTerminate;
    property MsgFilterMin: Integer
             read FMsgFilterMin write FMsgFilterMin;
    property MsgFilterMax: Integer
             read FMsgFilterMax write FMsgFilterMax;

	public
    destructor Destroy; override;
		constructor CreateWorker( AOwner: TObject; CreateSuspended,
      CreateMessageQueue: Boolean ); virtual;

    procedure Terminate; override;
    
		procedure Execute; virtual;
    procedure PostQuitMessage; virtual;

    property SynchEvent;

	end;

	TKCustomWorkerThreadClass = class of TKCustomWorkerThread;

{ TKSimpleWorkerThread }

	TKSimpleWorkerThread = class( TKCustomWorkerThread )
	public
		constructor CreateSimple( AOwner: TObject; ExecuteEvent: TKWorkerThreadExecuteEvent;
		  TerminateEvent: TNotifyEvent ); virtual;

		property Owner;

	end;

{ TKCustomThreadedTimer }

	TKCustomThreadedTimer = class( TObject )
	private
		FMutex: TKMutex;
		FEvent: TKEvent;
		FThread: TKThread;
		FEnabled: Boolean;
		FLockable: Boolean;
		FInterval: Cardinal;
		FOnTimer: TNotifyEvent;

		function GetPriority: TKThreadPriority;
		procedure SetEnabled( Value: Boolean );
		procedure SetInterval( Value: Cardinal );
		procedure SetOnTimer( Value: TNotifyEvent );
		procedure SetPriority( Value: TKThreadPriority );

	protected
		procedure UpdateTimer;
		function DoTimer: Cardinal; dynamic;
		procedure ThreadTerminate( Sender: TObject ); virtual;
		function ThreadExecute( Sender: TKCustomThread; Param: Pointer ): Cardinal; virtual;

		property Thread: TKThread
						 read FThread;
		property Enabled: Boolean
						 read FEnabled write SetEnabled default True;
		property Interval: Cardinal
						 read FInterval write SetInterval default SECOND_TO_MSECOND;
		property Lockable: Boolean
						 read FLockable write FLockable default False;
		property OnTimer: TNotifyEvent
						 read FOnTimer write SetOnTimer;
		property ThreadMutex: TKMutex
						 read FMutex;
		property Priority: TKThreadPriority
						 read GetPriority write SetPriority;

	public
		destructor Destroy; override;
		constructor Create( CreateSuspended: Boolean ); virtual;

	end;

{ TKThreadedTimerObject }

	TKThreadedTimerObject = class( TKCustomThreadedTimer )
	public
		property Thread;
		property Enabled;
		property Lockable;
		property Interval;
		property OnTimer;

	end;

{ TKCustomProcess }

	TKProcessPriority = ( ppNormal, ppIdle, ppHigh, ppRealTime );

	TKCustomProcess = class( TKKernelObject )
	private
		FOpened: Boolean;
		FOpenAccess: Cardinal;
		FCommandLine: string;
		FApplicationName: string;
		FCurrentDirectory: string;
		FThreadSecurity: PSecurityAttributes;
		FOpenInheritable: Boolean;
		FInheritParentHandles: Boolean;
		FCreationFlags: Cardinal;
		FEnvironment: TStrings;
		FStartUpInfo: TStartupInfo;
		FProcessInfo: TProcessInformation;

		function GetExitCode: Cardinal;
		function GetProcessTimes: TKKernelObjTimes;
		function GetPriority: TKProcessPriority;
		function GetAfinityMask( Index: Integer ): {$IFDEF DELPHI4}Cardinal{$ELSE}DWORD{$ENDIF};
		function GetThreadInheritHandles: Boolean;
		procedure SetPriority( Value: TKProcessPriority );

	protected
		function GetHandle: THandle; override;

		property Opened: Boolean
						 read FOpened;
		property OpenAccess: Cardinal
						 read FOpenAccess;
		property CommandLine: string
						 read FCommandLine;
		property ApplicationName: string
						 read FApplicationName;
		property CurrentDirectory: string
						 read FCurrentDirectory;
		property ThreadInheritHandles: Boolean
						 read GetThreadInheritHandles;
		property ThreadSecurity: PSecurityAttributes
						 read FThreadSecurity;	 
		property OpenInheritable: Boolean
						 read FOpenInheritable;
		property InheritParentHandles: Boolean
						 read FInheritParentHandles;
		property CreationFlags: Cardinal
						 read FCreationFlags;
		property Environment: TStrings
						 read FEnvironment;
		property ProcessInfo: TProcessInformation
						 read FProcessInfo;
		property StartUpInfo: TStartUpInfo
						 read FStartUpInfo;

		property ExitCode: Cardinal
						 read GetExitCode;
		property ProcessTimes: TKKernelObjTimes
						 read GetProcessTimes;
		property ProcessAfinityMask: {$IFDEF DELPHI4}Cardinal{$ELSE}DWORD{$ENDIF}
						 index 0 read GetAfinityMask;
		property SystemAfinityMask: {$IFDEF DELPHI4}Cardinal{$ELSE}DWORD{$ENDIF}
						 index 1 read GetAfinityMask;
		property PriorityClass: TKProcessPriority
						 read GetPriority write SetPriority default ppNormal;

	public
		destructor Destroy; override;

		constructor Create( const AnApplicationName, ACommandLine: string;
			bProcessInheritHandles: Boolean; lpProcessSecurityDescriptor: Pointer;
			bThreadInheritHandles: Boolean; lpThreadSecurityDescriptor: Pointer;
			bInheritParentHandles: Boolean; ACreationFlags: Cardinal; AnEnvironment: TStrings;
			const ACurrentDirectory: string; AStartUpInfo: PStartUpInfo ); virtual;
		constructor CreateOpen( AOpenAccess, AProcessID: Cardinal; CanInherit: Boolean ); virtual;

		function UnLock: Boolean; override;
		procedure ExitProcess( AnExitCode: Cardinal ); virtual;
		procedure ForceTerminateProcess( AnExitCode: Cardinal ); virtual;
		function WaitForInputIdle( AWaitTime: Cardinal ): TKWaitResult; virtual;
		function WaitForDebugEvent( var DebugEvent: TDebugEvent; AWaitTime: Cardinal ): Boolean; virtual;
		function ShutDownParams( Level, Flags: Cardinal ): Boolean; virtual;

	end;

{ TKProcess }

	TKProcess = class( TKCustomProcess )
	public
		property Handle;
    property InheritHandles;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property OnFail;
		property OnTimeOut;

		property Opened;
		property OpenAccess;
		property CommandLine;
		property ApplicationName;
		property CurrentDirectory;
		property ThreadSecurity;
		property InheritParentHandles;
		property CreationFlags;
		property Environment;
		property ProcessInfo;
		property StartUpInfo;

		property ExitCode;
		property ProcessTimes;
		property ProcessAfinityMask;
		property SystemAfinityMask;
		property PriorityClass;

	end;

{ TKSimpleProcess }

	TKSimpleProcess = class( TKProcess )
	public
		constructor CreateSimple( const ACommandLine: string ); virtual;

	end;

{ TKCustomPipe }

	TKCustomPipe = class;

{
	Buffer should be written to the destination. Buffer size is the actual memory
	available, and BufferWrittenSize is the total bytes written!
	Eg: Result := WriteFile( THandle( Data ), Buffer^, BufferSize, BufferWrittenSize, nil );
}
	TKPipeReaderProc = function( Pipe: TKCustomPipe; Data: Pointer; const Buffer: Pointer;
		BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean of object;
		
{
	Buffer should be filled by BufferSize. And return in BytesRead the total number
	of copied bytes.
	Eg: Result := ReadFile( THandle( Data ), Buffer^, BufferSize, BytesRead, nil );
}
	TKPipeWriterProc = function( Pipe: TKCustomPipe; Data: Pointer;
		var Buffer: Pointer; const BufferSize: Integer; var BytesRead: Integer ): Boolean of object;

	TKCustomPipe = class( TKKernelObject )
	private
		FRead: THandle;
		FWrite: THandle;
		FBuffer: Pointer;
		FBufferSize: Integer;
		FPipeBufferSize: DWORD;

		function ReaderFileProc( Pipe: TKCustomPipe; Data: Pointer; const Buffer: Pointer;
			BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;
		function ReaderBufferProc( Pipe: TKCustomPipe; Data: Pointer; const Buffer: Pointer;
			BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;
		function ReaderStreamProc( Pipe: TKCustomPipe; Data: Pointer; const Buffer: Pointer;
			BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;

		function WriterFileProc( Pipe: TKCustomPipe; Data: Pointer;
			var Buffer: Pointer; const BufferSize: Integer; var BytesRead: Integer ): Boolean;

	protected
		procedure CloseHandle; override;

		{ property Handle is invalid! }
		function GetHandle: THandle; override;

		property hRead: THandle
						 read FRead;
		property hWrite: THandle
						 read FWrite;
		property PipeBufferSize: DWORD
						 read FPipeBufferSize;

	public
		constructor Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
			APipeBufferSize: DWORD ); virtual;

		function UnLock: Boolean; override;
		procedure ChangeInheritance( IsRead, IsInheritable: Boolean );
		function ReadTo( ReaderProc: TKPipeReaderProc; Data: Pointer; const ReadSize: Cardinal ): Integer; virtual;
		function WriteFrom( WriterProc: TKPipeWriterProc; Data: Pointer; const WriteSize: Cardinal ): Integer; virtual;

{
	Read from pipe and write to file or buffer
	Write to pipe and read from file or buffer

	For the buffer version is the caller responsibility to free the resources allocated
	(the buffer size is equal to Result!).
}
		function ReadToFile( hFile: THandle; const ReadSize: Cardinal ): Integer; virtual;
		function ReadToBuffer( var Buffer: Pointer; const ReadSize: Cardinal ): Integer; virtual;
		function ReadToStream( ms: TMemoryStream; const ReadSize: Cardinal ): Integer; virtual;
		function WriteFromFile( hFile: THandle; const WriteSize: Cardinal ): Integer; virtual;

	end;

{ TKPipe }

	TKPipe = class( TKCustomPipe )
	public
		property InheritHandles;
		property SecurityAttributes;
		property LastError;
		property LastErrorMessage;

		property hRead;
		property hWrite;
		property PipeBufferSize;
		
		property OnFail;
		property OnTimeOut;

	end;

{ TKSimplePipe }

	TKSimplePipe = class( TKPipe )
	public
		constructor CreateSimple; virtual;
		
	end;

{ TKKernelObjPool }

	TKKernelObjPool = class;

	TKKernelObjEnumFunc = function( Sender: TKKernelObjPool; KernelObj: TKKernelObject;
		Index: Integer; Data: Pointer ): Boolean of object;

	TKKernelObjPool = class( TObject )
	private
		FObjList: TThreadList;
		FHandles: TWOHandleArray;
		FOwnerShip: TKObjectOwnerShip;

		function GetCount: Integer;
		function GetHandles( const ObjsIdx: array of Integer ): TWOHandleArray;
		function GetKernelObject( Index: Integer ): TKKernelObject;
		function GetAsEvent( Index: Integer ): TKEvent;
		function GetAsMutex( Index: Integer ): TKMutex;
		function GetAsSemaphore( Index: Integer ): TKSemaphore;
		function GetAsThread( Index: Integer ): TKThread;
		function GetAsWorker( Index: Integer ): TKCustomWorkerThread;
		function GetAsProcess( Index: Integer ): TKProcess;

	protected
		function CheckObjects: Boolean; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwnerShip: TKObjectOwnerShip ); virtual;

		procedure Clear; virtual;
		procedure Delete( ObjIdx: Integer ); virtual;
		procedure Remove( Obj: TKKernelObject ); virtual;
		function Add( Obj: TKKernelObject ): Integer; virtual;
		function IndexOf( Obj: TKKernelObject ): Integer; virtual;

		function WaitFor( const ObjsIdx: array of Integer; AWaitTime, AWakeMask: Cardinal; WaitAll: Boolean ): DWORD; virtual;
		function WaitForEx( const ObjsIdx: array of Integer; AWaitTime: Cardinal; WaitAll, AlertAble: Boolean ): DWORD; virtual;

		function ForEachObjDo( EnumFunc: TKKernelObjEnumFunc; Data: Pointer ): Boolean; dynamic;

		property OwnerShip: TKObjectOwnerShip
						 read FOwnerShip;
		property Count: Integer
						 read GetCount;

		property KernelObject[Index: Integer]: TKKernelObject
						 read GetKernelObject; default;
		property AsEvent[Index: Integer]: TKEvent
						 read GetAsEvent;
		property AsMutex[Index: Integer]: TKMutex
						 read GetAsMutex;
		property AsSemaphore[Index: Integer]: TKSemaphore
						 read GetAsSemaphore;
		property AsThread[Index: Integer]: TKThread
						 read GetAsThread;
		property AsWorker[Index: Integer]: TKCustomWorkerThread
						 read GetAsWorker;
		property AsProcess[Index: Integer]: TKProcess
						 read GetAsProcess;

	end;

{ TKThreadLocalStorage }

	TKThreadLocalStorage = class( TObject )
	private
		FTlsIndex: Cardinal;
		FLastError: Cardinal;

		function GetLastErrorMessage: string;

	public
		destructor Destroy; override;
		constructor Create; virtual;

		function SetValue( pData: Pointer ): Boolean; virtual;
		function GetValue( var pData: Pointer ): Boolean; virtual;

		property TlsIndex: Cardinal
						 read FTlsIndex;
		property LastError: Cardinal
						 read FLastError;
		property LastErrorMessage: string
						 read GetLastErrorMessage;

	end;

{ TKSharedObjects }

	TKSharedObjects = class( TObject )
	private
		FList: TObject;
		FListClass: TClass;
		FLock: TKCriticalSection;

	protected
		function GetList: TObject;
		function GetListClass: TClass;

		procedure DestroyList; virtual;

		property List: TObject
						 read GetList;
		property ListClass: TClass
						 read GetListClass;

	public
		destructor Destroy; override;
		constructor Create( AListClass: TClass );

		procedure Clear; virtual;
		procedure Unlock; virtual;
		procedure ReCreateListAs( AListClass: TClass ); virtual;
		function  Lock: TObject;

	end;

{ TKThreadStrings }

	TStringsClass = class of TStrings;
  TStringListClass = class of TStringList;

	TKThreadStrings = class( TKSharedObjects )
	protected
		function GetList: TStrings; virtual;
		function GetListClass: TStringsClass; virtual;

		property List: TStrings
						 read GetList;
		property ListClass: TStringsClass
						 read GetListClass;

	public
		constructor Create( StringsClass: TStringsClass ); virtual;

		function Add( const s: string ): Integer; virtual;
		function AddObject( const s: string; AObject: TObject ): Integer; virtual;
		function IndexOf( const s: string ): Integer; virtual;
		procedure Delete( Index: Integer ); virtual;

		procedure Clear; override;
		function  Lock: TStrings; virtual;

	end;

{ TKThreadMsgDispatcher - This object became a nosense object, but leave it there for now }

	TKThreadMsgDispatcher = class;

	TKThreadMsgDispacherEvent = procedure( Sender: TKThreadMsgDispatcher;
		DestThreadID: Cardinal; SenderThread: TKCustomThread; ThrHandle, WParam,
		LParam: LongInt; var Handled: Boolean ) of object;

	TKThreadMsgDispatcher = class( TObject )
	private
		FEvent: TKEvent;
		FLock: TKCriticalSection;
		FOldOnMessage: TMessageEvent;
		FPostThreadMsgHandle: LongInt;
		FWaitToProcessMessages: Boolean;
		FOnThreadMessage: TKThreadMsgDispacherEvent;

		procedure SetWaitToProcessMessages( Value: Boolean );
		procedure AppOnMessageEvent( var Msg: TMsg; var Handled: Boolean );

	protected
		function DoThreadMessage( DestThreadID: Cardinal; SenderThread: TKCustomThread;
		  ThrHandle, WParam, LParam: LongInt ): Boolean; dynamic;

	public
		destructor Destroy; override;
		constructor Create; virtual;

		function PostThreadMessage( DestThreadID: Cardinal; SenderThread: TKCustomThread;
		  WParam, LParam: LongInt ): LongInt; virtual;

		property WaitToProcessMessages: Boolean
						 read FWaitToProcessMessages write SetWaitToProcessMessages default True;
		property OnThreadMessage: TKThreadMsgDispacherEvent
						 read FOnThreadMessage write FOnThreadMessage;

	end;

const

	PROCESS_PRIORITY: array[TKProcessPriority] of DWORD =
	(
		NORMAL_PRIORITY_CLASS,
		IDLE_PRIORITY_CLASS,
		HIGH_PRIORITY_CLASS,
		REALTIME_PRIORITY_CLASS
	 );

	THREAD_PRIORITY: array [TKThreadPriority] of Integer =
	(
		THREAD_PRIORITY_IDLE,
		THREAD_PRIORITY_LOWEST,
		THREAD_PRIORITY_BELOW_NORMAL,
		THREAD_PRIORITY_NORMAL,
		THREAD_PRIORITY_ABOVE_NORMAL,
		THREAD_PRIORITY_HIGHEST,
		THREAD_PRIORITY_TIME_CRITICAL,
		THREAD_PRIORITY_ERROR_RETURN
	);

function ForceSingletonClass( AClass: TClass ): TKSingletonMutex;

{
--------------------------------------------------------------------------------
-------------------------- Generic Library Objects -----------------------------
--------------------------------------------------------------------------------
}

type

	EKCustomLibraryMapping = class( EKSYClasses );

	TKCustomLibraryMapping = class( TObject )
	private
		FFree: Boolean;
		FhLibrary: HModule;
		FLibraryName: string;

		function GethLibrary: HModule;

	protected

		procedure FreeHandle; virtual;
		procedure HandleNeeded; virtual;
		function CheckLibrary: Boolean; virtual;
		function CheckProc( const Proc ): Boolean; virtual;
		procedure SetLibraryName( const LibraryName: string ); dynamic;
		function InternalLoadProc( const ProcName: Shortstring; var Proc ): Pointer; virtual;

	public
		destructor Destroy; override;
		constructor Create;

		property hLibrary: HModule
						 read GethLibrary;
		property LibraryName: string
						 read FLibraryName;

	end;

{
--------------------------------------------------------------------------------
---------------------------- Generic GUI Objects -------------------------------
--------------------------------------------------------------------------------
}

const
	NULL_POINT: TPoint = ( x: 0; y: 0 );
	INVALID_POINT: TPoint = ( x: INVALID_POSITION; y: INVALID_POSITION );

	NULL_RECT: TRect = ( Left: 0; Top: 0; Right: 0; Bottom: 0 );
	INVALID_RECT: TRect = ( Left: INVALID_POSITION; Top: INVALID_POSITION;
		Right: INVALID_POSITION; Bottom: INVALID_POSITION );

type

{ TKPoint }

	TKPoint = class( TPersistent )
	private
		FX: Integer;
		FY: Integer;
		function GetPointEx: TPoint;
		procedure SetPointEx( const Value: TPoint );

	protected
		function GetInvalid: Boolean; virtual;

	public
		constructor Create( pp: PPoint ); virtual;

		procedure SetInvalid; virtual;
		
    procedure GetPoint( pp: PPoint );
    procedure SetPoint( pp: PPoint );
		procedure Assign( Source: TPersistent ); override;

		property Invalid: Boolean
						 read GetInvalid;

		property Point: TPoint
						 read GetPointEx write SetPointEx;

  published
		property cX: Integer
						 read FX write FX default INVALID_POSITION;
		property cY: Integer
						 read FY write FY default INVALID_POSITION;

	end;

{ TKRect }

	TKRect = class( TPersistent )
	private
		FLeft: Integer;
		FTop: Integer;
		FRight: Integer;
		FBottom: Integer;
		function GetRectEx: TRect;
		procedure SetRectEx( const Value: TRect );
		function GetHeightWidth( Index: Integer ): Integer;
    procedure SetHeightWidth( Index, Value: Integer );

	protected
		function GetInvalid: Boolean; virtual;

    property Height: Integer
             index 0 read GetHeightWidth write SetHeightWidth
						 stored False default INVALID_POSITION;
    property Width: Integer
             index 1 read GetHeightWidth write SetHeightWidth
						 stored False default INVALID_POSITION;

	public
		constructor Create( pr: PRect ); virtual;

		procedure SetInvalid; virtual;
		
		procedure GetRect( pr: PRect );
		procedure SetRect( pr: PRect );
		procedure GetTopLeft( pp: PPoint );
		procedure SetTopLeft( pp: PPoint );
		procedure GetBottomRight( pp: PPoint );
		procedure SetBottomRight( pp: PPoint );
		procedure Assign( Source: TPersistent ); override;

    property Rect: TRect
             read GetRectEx write SetRectEx;
    property Invalid: Boolean
						 read GetInvalid;

	published
		property Left: Integer
						 read FLeft write FLeft default INVALID_POSITION;
    property Top: Integer
						 read FTop write FTop default INVALID_POSITION;
		property Right: Integer
						 read FRight write FRight default INVALID_POSITION;
		property Bottom: Integer
						 read FBottom write FBottom default INVALID_POSITION;

	end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Window Support ----------------------------
--------------------------------------------------------------------------------
}

{ TKWindowInfo }

	TKWindowInfo = class( TPersistent )
	private
		FHandle: HWnd;
		FFont: TFont;

		function GetActive: Boolean;
		function GetEnabled: Boolean;
		function GetAveCharSize: TPoint;
		function GetBorderStyle: TFormBorderStyle;
		function GetButtonWidth: Integer;
		function GetCaptionHeight: Integer;
		function GetCaptionRect: TRect;
		function GetWindowClassName: String;
		function GetClientHandle: HWnd;
		function GetClientHeight: Integer;
		function GetClientRect: TRect;
		function GetClientWidth: Integer;
		function GetFormStyle: TFormStyle;
		function GetFrameHeight: Integer;
		function GetFrameWidth: Integer;
		function GetLeft: Integer;
		function GetHeight: Integer;
		function GetSystemCaptionFont: TFont;
		function GetTop: Integer;
		function GetVisible: Boolean;
		function GetWidth: Integer;
		function GetWindowRect: TRect;
		function GetWindowState: TWindowState;
		function GetWndProc: Pointer;

	protected
		function HasMDIClientHandle: Boolean; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AHandle: HWnd ); virtual;

		property Active: Boolean
						 read GetActive;
		property AveCharSize: TPoint
						 read GetAveCharSize;
		property BorderStyle: TFormBorderStyle
						 read GetBorderStyle;
		property ButtonWidth: Integer
						 read GetButtonWidth;
		property CaptionHeight: Integer
						 read GetCaptionHeight;
		property CaptionRect: TRect
						 read GetCaptionRect;
		property ClientHandle: HWnd
						 read GetClientHandle;
		property ClientHeight: Integer
						 read GetClientHeight;
		property ClientRect: TRect
						 read GetClientRect;
		property ClientWidth: Integer
						 read GetClientWidth;
		property Enabled: Boolean
						 read GetEnabled;
		property FrameHeight: Integer
						 read GetFrameHeight;
		property FrameWidth: Integer
						 read GetFrameWidth;
		property FormStyle: TFormStyle
						 read GetFormStyle;
		property Handle: HWnd
						 read FHandle;
		property Height: Integer
						 read GetHeight;
		property Left: Integer
						 read GetLeft;
		property SystemCaptionFont: TFont
						 read GetSystemCaptionFont;
		property Top: Integer
						 read GetTop;
		property Visible: Boolean
						 read GetVisible;
		property Width: Integer
						 read GetWidth;
		property WindowClassName: String
						 read GetWindowClassName;
		property WindowRect: TRect
						 read GetWindowRect;
		property WindowState: TWindowState
						 read GetWindowState;
		property WndProc: Pointer
						 read GetWndProc;

	end;

{ TKWindowTrack }

	TKWindowManager = class;
	TTrackStyle = ( tsMinTrack, tsMaxTrack, tsNone );

	TKWindowTrack = class( TPersistent )
	private
		FWidth: Integer;
		FHeight: Integer;
		FIgnore: Boolean;

		FOwner: TKWindowManager;
		FTrackStyle: TTrackStyle;

		function GetDeskTopWorkArea: TRect;

		procedure SetHeight( Value: Integer );
		procedure SetWidth( Value: Integer );

	protected
		property Owner: TKWindowManager
						 read FOwner;

	public
		constructor Create( AOwner: TKWindowManager; AStyle: TTrackStyle );

		property DesktopWorkArea: TRect
						 read GetDeskTopWorkArea;

	published
		property Ignore: Boolean
						 read FIgnore write FIgnore;
		property Height: Integer
						 read FHeight write SetHeight;
		property Width: Integer
						 read FWidth write SetWidth;

	end;

{ TKWindowManager }

	TKWindowManager = class( TKWindowInfo )
	private
		FSizeable: Boolean;
		FMoveable: Boolean;
		FMaxTrack: TKWindowTrack;
		FMinTrack: TKWindowTrack;

		FOldProc: Pointer;
		FNewProc: Pointer;

		function GetHeight: Integer;
		procedure SetHeight( Value: Integer );

	public
		destructor Destroy; override;
		constructor Create( AHandle: HWnd ); override;

		procedure CenterWindow; dynamic;
		procedure NewWndProc( var Message: TMessage ); virtual;

	published
		property Sizeable: Boolean
						 read FSizeable write FSizeable default true;
		property MaxTrack: TKWindowTrack
						 read FMaxTrack write FMaxTrack;
		property MinTrack: TKWindowTrack
						 read FMinTrack write FMinTrack;
		property Moveable: Boolean
						 read FMoveable write FMoveable default true;
		property Height: Integer
						 read GetHeight write SetHeight;

	end;

function GetSystemCaptionFont( AHandle: HWnd; AFont: TFont ): Boolean;

{
--------------------------------------------------------------------------------
-------------------------- Generic Gradient Support ----------------------------
--------------------------------------------------------------------------------
}

type

{ TKEffect }

	EKGradient = class( EKSYClasses );

	TKDepth = 0..10;
	TKSteps = 16..255;
	TKDirXY = ( drX, drY );

	TKGradientStyle = ( gsNone, gsVertical, gsHorizontal, gsDoubleVertical,
											gsDoubleHorizontal, gsRectangular, gsCircular,
											gsFirstBisector, gsSecondBisector, gsBitmapTiled,
											gsBitmapStretched );

	TKEffectDirection = ( edNone, edUp, edUpRight, edRight, edDownRight,
												edDown, edDownLeft, edLeft, edUpLeft );

	TKEffect = class( TPersistent )
	private
		FDepth: TKDepth;
		FColor: TColor;
		FDirection: TKEffectDirection;

		FOwner: TControl;

		procedure SetDepth( Value: TKDepth );
		procedure SetColor( Value: TColor );
		procedure SetDirection( Value: TKEffectDirection );

	public
		constructor Create( AControl: TControl );

		property Owner: TControl
						 read FOwner;

	published
		property Color: TColor
						 read FColor write SetColor default clSilver;
		property Depth: TKDepth
						 read FDepth write SetDepth default 1;
		property Direction: TKEffectDirection
						 read FDirection write SetDirection default edUpLeft;

	end;

const
	TEXT_ALIGNMENT: array[TAlignment] of Integer = ( DT_LEFT, DT_RIGHT, DT_CENTER );

	EFFECT_OFFSETS: array[TKEffectDirection, TKDirXY] of -1..1 =
		( (  0,  0 ), (  0, -1 ), ( +1, -1 ), ( +1,  0 ), ( +1, +1 ),
			(  0, +1 ), ( -1, +1 ), ( -1,  0 ), ( -1, -1 ) );

type

	TKGradientData = record
		BeginColor: TColor;
		EndColor: TColor;
		GradientStyle: TKGradientStyle;
		Steps: TKSteps;
		BitmapObj: TBitmap;
		BitmapFile: PChar;
	end;

{ TKGradient }

	TKGradient = class( TPersistent )
	private
		FSteps: TKSteps;
		FBitmap: TBitmap;
		FEndColor: TColor;
		FBeginColor: TColor;
		FFirstPaint: Boolean;
		FGradientBmp: TBitmap;
		FGradientChanged: Boolean;
		FExternalGradient: Boolean;
		FGradientStyle: TKGradientStyle;
		FOnGradientChange: TNotifyEvent;

		FOwner: TControl;

		procedure BitmapChange( Sender: TObject );
		
		function  GetBitmap: TBitmap;
		procedure SetSteps( Value: TKSteps );
		procedure SetBitmap( Value: TBitmap );
		procedure SetEndColor( Value: TColor );
		procedure SetBeginColor( Value: TColor );
		procedure SetGradientStyle( Value: TKGradientStyle );

	protected
		procedure Paint; virtual;

	public
		destructor Destroy; override;
		constructor Create( AControl: TControl; ABitmap: TBitmap );

		procedure Invalidate; virtual;
		procedure Assign( Source: TPersistent ); override;
		procedure GradientChange( Sender: TObject ); dynamic;
		procedure Changed( ForceRepaintOwner: Boolean ); dynamic;

		function BeginR: TColor;
		function BeginG: TColor;
		function BeginB: TColor;
		function DeltaR: TColor;
		function DeltaG: TColor;
		function DeltaB: TColor;
		procedure SetOwner( AControl: TControl );

		procedure AssignFromGradData( GradientData: TKGradientData );
		procedure AssignToGradData( var GradientData: TKGradientData );

		property Owner: TControl
						 read FOwner write SetOwner;

		property GradientBmp: TBitmap
						 read FGradientBmp;
		property GradientChanged: Boolean
						 read FGradientChanged;
		property FirstPaint: Boolean
						 read FFirstPaint;
		property OnGradientChange: TNotifyEvent
						 read FOnGradientChange write FOnGradientChange;

	published
		property BeginColor: TColor
						 read FBeginColor write SetBeginColor default clAqua;
		property Bitmap: TBitmap
						 read GetBitmap write SetBitmap;
		property EndColor: TColor
						 read FEndColor write SetEndColor default clNavy;
		property GradientStyle: TKGradientStyle
						 read FGradientStyle write SetGradientStyle default gsVertical;
		property Steps: TKSteps
						 read FSteps write SetSteps default High( TKSteps );

	end;

	TKEditGradientFunc = function( Gradient: TKGradient ): Boolean;

var
	EditGradientFunc: TKEditGradientFunc = nil;

{ Generic Gradient Routines }

procedure PaintGradient( grad: TKGradient );
procedure PaintGradientDC( DC: HDC; ARect: PRect; grad: TKGradient );
procedure ControlPaintGradient( AControl: TControl; bmp: TBitmap; grad: TKGradient );

function EditGradient( Gradient: TKGradient ): Boolean;

{
--------------------------------------------------------------------------------
---------------------------- Generic Canvas Support ----------------------------
--------------------------------------------------------------------------------
}

type

	EKCanvas = class( EKSYClasses );

	TKDeviceType = ( dtNone, dtPlotter, dtDisplay, dtPrinter, dtCamera,
		dtCharStream, dtMetafile, dtDispFile );
	TKDeviceTypes = set of TKDeviceType;
	TKCanvasZoom = MIN_CANVAS_ZOOM..MAX_CANVAS_ZOOM;

{ TKCustomCanvasInfo }

	TKCustomCanvasInfo = class( TObject )
	private
		FZoom: TKCanvasZoom;
		FWidth: LongInt;
		FGutter: TKRect;
		FCanvas: TCanvas;
		FMargins: TKRect;
		FHeight: LongInt;
		FPageWidth: LongInt;
		FPageHeight: LongInt;
		FDeviceType: TKDeviceType;
		FPixelsPerInchX: LongInt;
		FPixelsPerInchY: LongInt;

		function GetMargins: TKRect;
		function GetDeviceGutter: TKRect;
		function GetDeviceWidth: LongInt;
		function GetDeviceHeight: LongInt;
		function GetDeviceType: TKDeviceType;
		function GetDevicePageWidth: LongInt;
		function GetDevicePageHeight: LongInt;
		function GetDeviceLogPixelsX: LongInt;
		function GetDeviceLogPixelsY: LongInt;

		procedure SetMargins( Value: TKRect );

	protected
		function DeviceCaps( Index: Integer ): Integer; virtual;
		function AdjustMargins( Value: TKRect ): TKRect; virtual;

		property DeviceType: TKDeviceType
						 read GetDeviceType;
		property DeviceHeight: LongInt
						 read GetDeviceHeight;
		property DeviceWidth: LongInt
						 read GetDeviceWidth;
		property Gutter: TKRect
						 read GetDeviceGutter;
		property Margins: TKRect
						 read GetMargins write SetMargins;
		property PageWidth: LongInt
						 read GetDevicePageWidth;
		property PageHeight: LongInt
						 read GetDevicePageHeight;
		property PixelsPerInchX: LongInt
						 read GetDeviceLogPixelsX;
		property PixelsPerInchY: LongInt
						 read GetDeviceLogPixelsY;
		property Zoom: TKCanvasZoom
						 read FZoom write FZoom;

	public
		destructor Destroy; override;
		constructor Create( ACanvas: TCanvas ); virtual;

    function MMToPixels_X( MM: Single ): LongInt;
		function MMToPixels_Y( MM: Single ): LongInt;
		function PixelsToMM_X( Pixels: LongInt ): Single;
		function PixelsToMM_Y( Pixels: LongInt ): Single;
		function InchToPixels_X( Inches: Single ): LongInt;
		function InchToPixels_Y( Inches: Single ): LongInt;
		function PixelsToInch_X( Pixels: LongInt ): Single;
		function PixelsToInch_Y( Pixels: LongInt ): Single;

	end;

{ TKCanvasInfo }

	TKCanvasInfo = class( TKCustomCanvasInfo )
	public
		property DeviceType;
		property DeviceHeight;
		property DeviceWidth;
		property Gutter;
		property Margins;
		property PageWidth;
		property PageHeight;
		property PixelsPerInchX;
		property PixelsPerInchY;
		property Zoom;

	end;

{ TKCustomPrinterCanvasInfo }

	TKCustomPrinterCanvasInfo = class( TKCustomCanvasInfo )
	private
		FPrinter: TPrinter;

	protected
		function DeviceCaps( Index: Integer ): Integer; override;

	public
		destructor Destroy; override;
		constructor Create( ACanvas: TCanvas ); override;
		constructor CreateFromPrinter( APrinter: TPrinter ); virtual;

	end;

{ TKPrinterCanvasInfo }

	TKPrinterCanvasInfo = class( TKCustomPrinterCanvasInfo )
	public
		property DeviceType;
		property DeviceHeight;
		property DeviceWidth;
		property Gutter;
		property Margins;
		property PageWidth;
		property PageHeight;
		property PixelsPerInchX;
		property PixelsPerInchY;
		property Zoom;

	end;

const
	DISPLAY_DEVICES: TKDeviceTypes = [dtDisplay];
	PRINT_DEVICES: TKDeviceTypes = [dtPlotter, dtPrinter];

{
--------------------------------------------------------------------------------
---------------------------- Generic Stream Support ----------------------------
--------------------------------------------------------------------------------
}

type

	{ TKCustomStringStream }

	EKCustomStringStream = class( EKSYClasses );

	TKCustomStringStream = class( TStringStream )
	public
    constructor Create( const AString: string ); virtual;
    constructor CreateFromFile( const FileName: TFileName ); virtual;
    
		{ Basic output routines. }
		function PutChar( Ch: Char ): TKCustomStringStream;
		function PutInteger( Int: LongInt ): TKCustomStringStream;
		function PutFloat( Flt: Extended ): TKCustomStringStream;
		function PutString( const Str: string ): TKCustomStringStream;
		function PutLine( const Str: string ): TKCustomStringStream;
		function PutPChar( const Str: PChar ): TKCustomStringStream;

		{ Special output characters. }
		function PutSpace: TKCustomStringStream;
		function PutTab: TKCustomStringStream;
		function PutEndOfLine: TKCustomStringStream;

		{ Formatted output routines. }
		procedure Format( const Fmt: string; Args: array of const );
		procedure FormatLn( const Fmt: string; Args: array of const );

{ Normal output routines }
		procedure WriteLn( const Str: string );
		procedure NewLine;

	end;

	TKCustomStringStreamClass = class of TKCustomStringStream;

{ TKCustomMemoryStream }

	EKCustomMemoryStream = class( EKSYClasses );

	TKCustomMemoryStream = class( TMemoryStream )
	public
		constructor CreateFromFile( const FileName: TFileName ); virtual;
		constructor CreateFromString( const AString: string ); virtual;
		constructor CreateFromStringStream( ss: TStringStream ); virtual;
		constructor CreateFromMemory( AMemory: Pointer; ADataSize: Integer ); virtual;

	end;

	TKCustomMemoryStreamClass = class of TKCustomMemoryStream;

	{ TKMemoryStream }

	TKMemoryStream = class( TKCustomMemoryStream );
  
  { TKCustomFileStream }

	EKCustomFileStream = class( EKSYClasses );

	TKCustomFileStream = class( TFileStream )
	private
		FFileName: string;
		FMode: Word;

	protected
		function GetHandle: THandle; virtual;
		procedure SetHandle( Value: THandle ); virtual;

	public
		destructor Destroy; override;
		constructor Create( const AFileName: string; AMode: Word ); virtual;

		procedure CloseFile; dynamic;
		procedure OpenFile( const AFileName: string; AMode: Word ); dynamic;

		property Handle: THandle
						 read GetHandle write SetHandle;
		property Mode: Word
             read FMode;
    property FileName: string
             read FFileName;
                      
	end;

	TKCustomFileStreamClass = class of TKCustomFileStream;

{ TKSocketStream }

  EKSocketStream = class( EKSYClasses );

	TKSocketStream = class( TStream )
	private
		FSocket: TSocket;
		FBlocked: Boolean;
		FTimeOut: Cardinal;
		FEvent: TKSimpleEvent;
		FSocketLock: TKCriticalSection;

		function GetTimeOut: Cardinal;
		procedure SetTimeOut( Value: Cardinal );

	protected
		procedure Disconnect; dynamic;

		property Event: TKSimpleEvent
						 read FEvent;

	public
		destructor Destroy; override;
		constructor Create( ASocket: TSocket; ABlocked: Boolean; ATimeOut: Cardinal );

		function ReadLength: LongInt; dynamic;
		function WaitForData( ATimeout: Cardinal ): Boolean; dynamic;

		function Read( var Buffer; Count: Longint ): Longint; override;
		function Write( const Buffer; Count: Longint ): Longint; override;
		function Seek( Offset: Longint; Origin: Word ): Longint; override;

		property Socket: TSocket
						 read FSocket write FSocket;
		property Blocked: Boolean
						 read FBlocked write FBlocked;
		property EventTimeOut: Cardinal
						 read GetTimeout write SetTimeout;

	end;

	TKSocketStreamClass = class of TKSocketStream;

{ TKWSAAsyncGetXbyY }

  EKWSAAsyncGetXbyY = class( EKSYClasses );

  TKWSAAsyncGetXbyYMsg = record
		Msg: Cardinal;
		Handle: THandle;
		BufLen: Word;
		Error: Word;
		Result: Integer;
	end;

  TKWSAAsyncGetXbyYKind = ( wagkHostByName, wagkHostByAddr, wagkProtoByName,
    wagkProtoByNumber, wagkServByName, wagkServByPort );

  PKWSAAsyncStruct = ^TKWSAAsyncStruct;
  TKWSAAsyncStruct = record
    FHandle: THandle;
    FBuffer: Pointer;
    FUserData: Pointer;
    FKind: TKWSAAsyncGetXbyYKind;
  end;

  TKWSAAsyncGetXbyY = class;

  TKWSAAsyncGetXbyYEvent = procedure( Sender: TKWSAAsyncGetXbyY;
    Kind: TKWSAAsyncGetXbyYKind; Index: Integer ) of object;
  TKWSAAsyncGetXbyYErrorEvent = procedure( Sender: TKWSAAsyncGetXbyY;
    Kind: TKWSAAsyncGetXbyYKind; Index, ErrorCode: Integer ) of object;

  TKWSAAsyncGetXbyY = class( TObject )
  private
    FHandle: HWnd;
    FWSAList: TList;
    FOnGetHostByY: TKWSAAsyncGetXbyYEvent;
    FOnGetServByY: TKWSAAsyncGetXbyYEvent;
    FOnGetProtoByY: TKWSAAsyncGetXbyYEvent;
    FOnAsyncSockError: TKWSAAsyncGetXbyYErrorEvent;

    function GetCount: Integer;
    function GetHandle: Hwnd;
    function GetBuffers( Index: Integer ): Pointer;
    function GetUserData( Index: Integer ): Pointer;
    function GetAsHostEnt( Index: Integer ): PHostEnt;
    function GetAsServEnt( Index: Integer ): PServEnt;
    function GetAsProtoEnt( Index: Integer ): PProtoEnt;
    function GetAsyncHandles( Index: Integer ): THandle;
    function GetAsyncKind( Index: THandle ): TKWSAAsyncGetXbyYKind;
    procedure SetUserData( Index: Integer; Value: Pointer );
    function CheckKind( Kind: TKWSAAsyncGetXbyYKind ): Boolean;

  protected
    procedure WndProc( var Message: TMessage ); dynamic;

    procedure DoGetHostByY( AHandle: Integer ); dynamic;
    procedure DoGetProtoByY( AHandle: Integer ); dynamic;
    procedure DoGetServByY( AHandle: Integer ); dynamic;
    procedure DoAsyncSockError( AHandle, Error: Integer ); dynamic;

  public
    destructor Destroy; override;
    constructor Create; virtual;

{
    * Depending on the kind the Data will be interpreted properly, the return value
      is the index of this entry if success or KWSA_ASYNCGETXBYY_PARAM_ERROR or
      KWSA_ASYNCGETXBYY_WINSOCK_ERROR for failure.

      KWSA_ASYNCGETXBYY_PARAM_ERROR
        If Data or Events are not valid parameters. The Data MUST NOT BE nil.
        The user data is free for the calling user. Depending on the Kind, one
        of the three suplied events MUST BE filled.

      KWSA_ASYNCGETXBYY_WINSOCK_ERROR
        A Winsock error code for failure. Call WSAGetLastError for detailed information

    * In the case of failure all data structure that should be allocated will be freed.
    * For ServByY the proto field will be nil (default).
    * There is no need to worry about the size of the Buffer it will be MAXGETHOSTSTRUC
      as defined by Rfc833 and Winsock 1.1 spec.

    * The force version will automatically generate accurrate exceptions determinated
      by the two conditions defined above.
}
    function Add( Data, UserData: Pointer; Kind: TKWSAAsyncGetXbyYKind ): Integer; virtual;
    procedure Force( Data, UserData: Pointer; Kind: TKWSAAsyncGetXbyYKind ); virtual;

    function IndexOf( Handle: THandle ): Integer; virtual;
    procedure Delete( Index: Integer ); virtual;
    function Remove( Handle: THandle ): Integer; virtual;
    procedure Clear; virtual;

    function IndexOfIP( IP: Integer ): Integer; virtual;
    function IndexOfHost( const Host: string ): Integer; virtual;

    property Handle: Hwnd
             read GetHandle;
    property AsyncHandles[Index: Integer]: THandle
             read GetAsyncHandles;
    property UserData[Index: Integer]: Pointer
             read GetUserData write SetUserData;
    property AsyncKind[Handle: THandle]: TKWSAAsyncGetXbyYKind
             read GetAsyncKind;
    property AsHostEnt[Index: Integer]: PHostEnt
             read GetAsHostEnt;
    property AsProtoEnt[Index: Integer]: PProtoEnt
             read GetAsProtoEnt;
    property AsServEnt[Index: Integer]: PServEnt
             read GetAsServEnt;
    property Buffers[Index: Integer]: Pointer
             read GetBuffers;
    property Count: Integer
             read GetCount;
    property OnGetHostByY: TKWSAAsyncGetXbyYEvent
             read FOnGetHostByY write FOnGetHostByY;
    property OnGetServByY: TKWSAAsyncGetXbyYEvent
             read FOnGetServByY write FOnGetServByY;
    property OnGetProtoByY: TKWSAAsyncGetXbyYEvent
             read FOnGetProtoByY write FOnGetProtoByY;
    property OnAsyncSockError: TKWSAAsyncGetXbyYErrorEvent
             read FOnAsyncSockError write FOnAsyncSockError;
             
  end;
  
{
--------------------------------------------------------------------------------
------------------------ Generic Collection Objects ----------------------------
--------------------------------------------------------------------------------
}

type

	EKCollection = class( EKSYClasses );
	EKCollectionItem = class( EKCollection );

	TKCustomCollection = class;

	TKCustomCollectionItem = class( TCollectionItem )
	private
		FName: string;
	  FData: Pointer;
		FEnabled: Boolean;
		FGroupIndex: Cardinal;
		FGroupAborted: Boolean;
		FOwner: TKCustomCollection;

		procedure SetEnabled( Value: Boolean );
		procedure SetGroupIndex( Value: Cardinal );

	protected
		function GetOwnerCollection: TKCustomCollection;

		function GetDisplayName: string; override;
		procedure SetDisplayName( const Value: string ); override;

		procedure MarkGroupAborted;
		procedure ClearGroupAborted;
 		procedure LoadFromStream( Stream: TStream ); dynamic;
		procedure SaveToStream( Stream: TStream ); dynamic;

		property Name: string
						 read GetDisplayName write SetDisplayName;
		property Owner: TKCustomCollection
						 read GetOwnerCollection;
		property GroupAborted: Boolean
						 read FGroupAborted;
		property Data: Pointer
						 read FData write FData;
		property Enabled: Boolean
						 read FEnabled write SetEnabled default True;
		property GroupIndex: Cardinal
						 read FGroupIndex write SetGroupIndex default COLLECTION_ITEM_GROUP_NULL;

	public
    destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; virtual;

		procedure LoadFromFile( const FileName: string );
		procedure SaveToFile( const FileName: string );

	end;

	TKCustomCollectionItemClass = class of TKCustomCollectionItem;

{ TKCustomCollection }

  TKCollectionItemFunc = function( Item: TKCustomCollectionItem; Data: Pointer ): Boolean of object;

	TKCustomCollection = class( TCollection )
	private
		FOwner: TPersistent;
		FAllowDuplicateNames: Boolean;

	protected
		procedure SetItem( Index: Integer; AItem: TKCustomCollectionItem );
		function GetItem( Index: Integer ): TKCustomCollectionItem;

    procedure ClearGroupAborted;

		procedure SetItemName( Item: TCollectionItem ); override;

		function GetOwner: TPersistent; override;
		function GetOwnerComp: TPersistent;

		class function GetDefaultItemName( Item: TKCustomCollectionItem ): string; virtual;
		function GetNames( Index: Integer ): string; virtual;

		function Add: TKCustomCollectionItem;
		function GetItemByName( const AName: string ): TKCustomCollectionItem;

  	property AllowDuplicateNames: Boolean
						 read FAllowDuplicateNames;
		property ItemByName[const AName: string]: TKCustomCollectionItem
						 read GetItemByName;
		property Items[Index: Integer]: TKCustomCollectionItem
						 read GetItem write SetItem;
		property Names[Index: Integer]: string
						 read GetNames;
		property Owner: TPersistent
						 read GetOwnerComp;

	public
		constructor Create( AOwner: TPersistent; ItemClass: TKCustomCollectionItemClass;
			AllowDupNames: Boolean );

		function Equals( Collection: TKCustomCollection ): Boolean; virtual;
		procedure AddItems( Source: TKCustomCollection ); dynamic;
    procedure ClearItems; dynamic;

		procedure GetGroupSelection( Group: Cardinal; sl: TStrings );
		function ForEachItemDo( Group: Cardinal; ItemGroupFunc: TKCollectionItemFunc;
		  Data: Pointer ): Integer;

		function FindItem( const AName: string ): TKCustomCollectionItem;

	end;

	TKCustomCollectionClass = class of TKCustomCollection;

	TKCustomNamedCollectionItem = class( TKCustomCollectionItem )
	published
		property Name;

	end;

	TKCustomNamedCollection = class( TKCustomCollection )
	public
		property AllowDuplicateNames;
		property ItemByName;
		property Items;
		property Names;
		property Owner;

	end;

{
--------------------------------------------------------------------------------
--------------- Generic Memory Garbage Collectior Architecture -----------------
--------------------------------------------------------------------------------
}

	EKGarbageCollector = class( EKSYClasses );
	TKGarbageCollectorList = class;

{ TKGarbageCollector }

	TKGarbageCollector = class( TObject )
	private
 		FID: TDateTime; 
		FPChars: TList;
		FMemory: TList;
		FOwner: TKGarbageCollectorList;

		function GetMemory: TList;
		function GetPChars: TList;
		procedure FreeMemory;
		procedure FreePChars;

		property Memory: TList
						 read GetMemory;
		property PChars: TList
						 read GetPChars;

		constructor Create( AOwner: TKGarbageCollectorList ); virtual;
		
	protected
		procedure FreeLists; dynamic;

	public
		destructor Destroy; override;
		
		property ID: TDateTime
						 read FID;

{ Memory support }
		procedure GetMem( var P: Pointer; Size: Cardinal );
		function AllocMem( Size: Cardinal ): Pointer;
		procedure FreeMem( var P: Pointer; Size: Cardinal );

{ PChar support }
		function StrNew( Str: PChar ): PChar;
		function StrAlloc( Size: Cardinal ): PChar;
		procedure StrDispose( Str: PChar );

	end;

	TKGarbageCollectorClass = class of TKGarbageCollector;

{ TKGarbageCollectorList }

	TKGarbageCollectorList = class( TObject )
	private
		FList: TList;

		function GetList: TList;
		function GetCollectors( Index: TDateTime ): TKGarbageCollector;

		property List: TList
						 read GetList;

	protected
		procedure FreeCollectors; dynamic;
		function IndexOf( AID: TDateTime ): Integer;

	public
		destructor Destroy; override;
		constructor Create; virtual;

		procedure DisposeCollectorByID( AID: TDateTime );
		procedure DisposeCollector( var ACollector: TKGarbageCollector );
		function NewCollector( CollectorClass: TKGarbageCollectorClass;
			var ID: TDateTime ): TKGarbageCollector;

		property Collectors[Index: TDateTime]: TKGarbageCollector
						 read GetCollectors;
						 
	end;

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

	TKCustomInternalDialogForm = class( TCustomForm );

{ TKCustomInternalForm }

	TKCustomInternalForm = class( TKCustomInternalDialogForm )
	private
		FOldAlign: Boolean;
		FLabel: TLabel;
		FText: string;
		FButtons: TList;
		FIsShowModal: Boolean;
		FDialogStyle: TKDialogStyle;
		FWindowManager: TKWindowManager;
		FExecutorProc: TKDialogExecuteProc;

	protected
		function ScaleX( Value: Integer ): Integer; dynamic;
		function ScaleY( Value: Integer ): Integer; dynamic;

		procedure ButtonClick( Sender: TObject ); dynamic;
		procedure FormClose( Sender: TObject; var Action: TCloseAction ); dynamic;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function CopyToClipBoard: Boolean; virtual;
		function CutToClipBoard: Boolean; virtual;
		function PasteFromClipBoard: Boolean; virtual;

		procedure FixButtons; virtual;
		procedure PrepareForm; virtual;
		procedure UnprepareForm; virtual;
		procedure DestroyCtrls; virtual;
		procedure DoPrepareShow; dynamic;
		procedure DoUnprepareShow; dynamic;

		procedure AddButton( const bnCaption: string;	bnResult: TModalResult;
			IsDefault: Boolean ); dynamic;

		property Buttons: TList
						 read FButtons;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function ShowModal: Integer; {$IFDEF DELPHI4}override;{$ELSE}dynamic;{$ENDIF}
    procedure Show; dynamic;

		property IsShowModal: Boolean
						 read FIsShowModal write FIsShowModal;
		property DialogStyle: TKDialogStyle
						 read FDialogStyle write FDialogStyle;
		property Text: string
						 read FText write FText;
		property lbMessage: TLabel
						 read FLabel;
		property ExecutorProc: TKDialogExecuteProc
						 read FExecutorProc write FExecutorProc;
		property ModalResult;

	end;

const
	DEFAULT_INTERNAL_FORM_SCALE: TPoint = ( X: 800; Y: 600 );

{
--------------------------------------------------------------------------------
--------------------------------- TKMeasures -----------------------------------
--------------------------------------------------------------------------------
}

type

	TKPageSetupOption = ( poDefaultMinMargins, poDisableMargins,
		poDisableOrientation, poDisablePagePainting, poDisablePaper,
		poDisablePrinter, poNoWarning, poShowHelp );

	TKPageSetupOptions = set of TKPageSetupOption;

	TKPaperType = ( ptPaper, ptEnvelope );

	TKPaperOrientation = ( poPortrait, poLandscape );

	TKPrinterType = ( ptDotMatrix, ptHPPCL );
	TKPaintOption = ( poFullPage, poMinimumMargins, poMargins,
		poGreekText, poEnvStamp, poYAFullPage );

	EKMeasures = class( EKCanvas );

	TKScalingFlag = ( ksfLeft, ksfTop, ksfRight, ksfBottom );

	TKSetMeasuresEvent = procedure( Sender: TObject; Index: TKScalingFlag;
		Value: Single ) of object;
	TKGetMeasuresEvent = procedure( Sender: TObject; Index: TKScalingFlag;
		var Value: Single ) of object;

	TKMeasurements = ( pmMillimeters, pmInches );

{ TKCustomMeasures }

	TKCustomMeasures = class( TPersistent )
	private
		FChanging: Boolean;
		FOnSetMeasures: TKSetMeasuresEvent;
		FOnGetMeasures: TKGetMeasuresEvent;

		FOwner: TComponent;

	protected
		function GetValue( Index: Integer ): Single;
		procedure SetValue( Index: Integer; Value: Single );

		function GetOwnerComp: TComponent;

		procedure DoSetMeasures( Index: TKScalingFlag; Value: Single ); dynamic;
		procedure DoGetMeasures( Index: TKScalingFlag; var Value: Single ); dynamic;

		property Owner: TComponent
						 read GetOwnerComp;

		property OnGetMeasures: TKGetMeasuresEvent
						 read FOnGetMeasures write FOnGetMeasures;
		property OnSetMeasures: TKSetMeasuresEvent
						 read FOnSetMeasures write FOnSetMeasures;

	public
		constructor Create( AOwner: TComponent ); virtual;
		constructor CreateLinked( AOwner: TComponent; AGet: TKGetMeasuresEvent;
			ASet: TKSetMeasuresEvent ); virtual;

	end;

{ TKPointMeasures }

	TKPointMeasures = class( TKCustomMeasures )
	private
		function GetPoint: TPoint;
		procedure SetPoint( Value: TPoint );

	public
		property Point: TPoint
						 read GetPoint write SetPoint;

	published
		property cX: Single
						 index Integer( ksfLeft ) read GetValue write SetValue;
		property cY: Single
						 index Integer( ksfTop ) read GetValue write SetValue;

	end;

{ TKRectMeasures }

	TKRectMeasures = class( TKCustomMeasures )
	private
		function GetRect: TRect;
		procedure SetRect( Value: TRect );

	public
		property Rect: TRect
						 read GetRect write SetRect;

	published
		property Left: Single
						 index Integer( ksfLeft ) read GetValue write SetValue;
		property Top: Single
						 index Integer( ksfTop ) read GetValue write SetValue;
		property Right: Single
						 index Integer( ksfRight ) read GetValue write SetValue;
		property Bottom: Single
						 index Integer( ksfBottom ) read GetValue write SetValue;

	end;

const
	ADJUST_RECT_SCALE: array[TKMeasurements] of Integer = ( 100, 1000 );

{
--------------------------------------------------------------------------------
-------------------------------- TKTickCount -----------------------------------
--------------------------------------------------------------------------------
}

type

{ TKTickCount }

	TKTickCount = class( TObject )
	private
		FTime1: TLargeInteger;
		FTime2: TLargeInteger;
		FFrequency: TLargeInteger;
		FElapsedTime: {$IFDEF DELPHI4}LARGE_INTEGER{$ELSE}TLargeInteger{$ENDIF};

	protected
		property Frequency: TLargeInteger
						 read FFrequency;	 

	public
		procedure Start; virtual;
		procedure Stop; virtual;

		property ElapsedTime: {$IFDEF DELPHI4}LARGE_INTEGER{$ELSE}TLargeInteger{$ENDIF}
						 read FElapsedTime;

	end;
        
implementation

uses
	Consts, ExtCtrls, Buttons, TypInfo, uksyResStr;

{
--------------------------------------------------------------------------------
---------------------------- Generic IPC Objects -------------------------------
--------------------------------------------------------------------------------
}

const
	W95_CREATE = 0;
	W95_OPEN   = 1;
	WNT_CREATE = 2;
	WNT_OPEN   = 3;

	CREATION_FILTER: array[Boolean, Boolean] of Byte =
	(
		( W95_CREATE, W95_OPEN ),
		( WNT_CREATE, WNT_OPEN )
	);

	SEMAPHORE_MODIFY_STATE = $0002;
	SEMAPHORE_ALL_ACCESS = ( STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $03 );

	SINGLETON_MUTEXT_NAME_PATTERN = 'SINGLETON_MUTEX_%s_%.8x';
																	 
{------------------------------ TKCriticalSection ------------------------------}

constructor TKCriticalSection.Create( StartLocked: Boolean );
begin
	inherited Create;
	FLockCount := 0;
	ZeroMemory( @FCriticalSection, SizeOf( TRTLCriticalSection ) );
	InitializeCriticalSection( FCriticalSection );
	if StartLocked then
		Lock;
end;

destructor TKCriticalSection.Destroy;
begin
	while ( FLockCount > 0 ) do
		UnLock;
	DeleteCriticalSection( FCriticalSection );
	inherited Destroy;
end;

procedure TKCriticalSection.Lock;
begin
	InterLockedIncrement( FLockCount );
	EnterCriticalSection( FCriticalSection );
end;

procedure TKCriticalSection.Unlock;
begin
	LeaveCriticalSection( FCriticalSection );
	InterLockedDecrement( FLockCount );
end;

{ TKKernelObject }

constructor TKKernelObject.Create( bInheritHandles: Boolean;
	lpSecurityDescriptor: Pointer );
begin
	inherited Create;
	FHandle := INVALID_HANDLE_VALUE;
	FLastWaitResult := kwrUnknown;
	FLastError := NO_ERROR;
	FCrtSec := TKCriticalSection.Create( False );
	FSecurityAttributes := nil;
	if ( bInheritHandles or CheckPointer( FSecurityAttributes ) ) then
	begin
		FSecurityAttributes := New( PSecurityAttributes );
		ZeroMemory( FSecurityAttributes, SizeOf( TSecurityAttributes ) );
		FSecurityAttributes^.nLength := SizeOf( TSecurityAttributes );
		FSecurityAttributes^.bInheritHandle := bInheritHandles;
		FSecurityAttributes^.lpSecurityDescriptor := lpSecurityDescriptor;
	end;
end;

destructor TKKernelObject.Destroy;
begin
	if CheckPointer( FSecurityAttributes ) then
		Dispose( FSecurityAttributes );
	CloseHandle;	
	FCrtSec.Free;
	inherited Destroy;
end;

procedure TKKernelObject.CloseHandle;
begin
	if CheckHandle( Handle ) then
		if ( not Windows.CloseHandle( FHandle ) ) then
			Self.SetLastError( Windows.GetLastError );
	FHandle := INVALID_HANDLE_VALUE;
end;

function TKKernelObject.GetLastWaitResult: TKWaitResult;
begin
	{
	FCrtSec.Lock;
	try
	}
		Result := FLastWaitResult;
	{
	finally
		FCrtSec.UnLock;
	end;
	}
end;

function TKKernelObject.GetHandle: THandle;
begin
	{
	FCrtSec.Lock; // ? For this value there is no need for critical section lock...
	try
	}
		Result := FHandle;
	{
	finally
		FCrtSec.UnLock;
	end;
	}
end;

function TKKernelObject.GetLastError: Cardinal;
begin
	{
	FCrtSec.Lock;
	try
	}
		Result := FLastError;
	{
	finally
		FCrtSec.UnLock;
	end;
	}
end;

function TKKernelObject.GetLastErrorMessage: string;
begin
	Result := SysErrorMessage( Self.GetLastError );
end;

function TKKernelObject.GetInheritHandles: Boolean;
begin
  Result := ( CheckPointer( FSecurityAttributes ) and FSecurityAttributes^.bInheritHandle );
end;

function TKKernelObject.GetEvents( Index: Integer ): TNotifyEvent;
begin
	{
	FCrtSec.Lock;
	try
	}
		case Index of
			0 : Result := FOnFail;
			1 : Result := FOnTimeOut;
		else
			Result := nil;
		end;
	{
	finally
		FCrtSec.UnLock;
	end;
	}
end;

procedure TKKernelObject.SetLastError( Value: Cardinal );
begin
 	FCrtSec.Lock;
	try
		FLastError := Value;
	finally
 		FCrtSec.UnLock;
	end;
end;

procedure TKKernelObject.SetEvents( Index: Integer; Value: TNotifyEvent );
begin
 	FCrtSec.Lock;
	try
		case Index of
			0 : FOnFail := Value;
			1 : FOnTimeOut := Value;
		end;
	finally
    if CheckObject( FCrtSec ) then
  		FCrtSec.UnLock;
	end;
end;

procedure TKKernelObject.DoFail;
begin
 	FCrtSec.Lock;
	try
		if Assigned( FOnFail ) then
		  FOnFail( Self );
	finally
 		FCrtSec.UnLock;
	end;
end;

procedure TKKernelObject.DoTimeOut;
begin
  FCrtSec.Lock;
  try
		if Assigned( FOnTimeOut ) then
			FOnTimeOut( Self );
	finally
 		FCrtSec.UnLock;
	end;
end;

function TKKernelObject.Lock( AWaitTime: Cardinal ): Boolean;
begin
	{
		Semaphore:

		With this Lock Method policy, the return value is True unless an error or a
		TimeOut occurred. If the WaitTime is Infinite, just the kwrSignaled or
		kwrFailed can be returned

		Mutex:

		With this Lock Method policy, the return value is True unless an error,
		a TimeOut, or a WAIT_ABANDONED occurred. If the WaitTime is Infinite,
		the kwrSignaled, kwrAbandoned or kwrFailed can be returned.

		Event:

		With this Lock Method policy, the return value is True unless an error or a
		TimeOut occurred. If the WaitTime is Infinite, the kwrSignaled, kwrTimeOut,
		kwrFailed can be returned.

		Thread:

		The thread is signaled when it's threadfunc finish. Othrewise, it's nonsignaled.

		Process:

		The process is signaled when it's finish it's execution. Otherwise, it's nonsignaled.

	}
	Result := ( WaitFor( AWaitTime ) = kwrSignaled );
end;

function TKKernelObject.WaitFor( AWaitTime: Cardinal ): TKWaitResult;
var
	h: THandle;
begin
	h := Handle;
	case WaitForSingleObject( h, AWaitTime ) of
		WAIT_ABANDONED: Result := kwrAbandoned;
		WAIT_OBJECT_0 : Result := kwrSignaled;
		WAIT_TIMEOUT  :
		begin
			Result := kwrTimeout;
			DoTimeOut;
		end;
		{ WAIT_FAILED   : }
	else
		begin
			Result := kwrFail;
			Self.SetLastError( Windows.GetLastError );
			DoFail;
		end;
	end;
	FLastWaitResult := Result;
end;

function TKKernelObject.WaitForEx( AWaitTime: Cardinal; Alertable: Boolean ): TKWaitResult;
var
	h: THandle;
begin
	h := Handle;
	case WaitForSingleObjectEx( h, AWaitTime, Alertable ) of
		WAIT_ABANDONED: Result := kwrAbandoned;
		WAIT_OBJECT_0 : Result := kwrSignaled;
		WAIT_TIMEOUT  :
		begin
			Result := kwrTimeout;
			DoTimeOut;
		end;
		{ WAIT_FAILED   : }
	else
		begin
			Result := kwrFail;
			Self.SetLastError( Windows.GetLastError );
			DoFail;
		end;
	end;
	FLastWaitResult := Result;
end;

procedure TKKernelObject.Sleep( ASleepTime: Cardinal );
begin
	Windows.Sleep( ASleepTime );
end;

procedure TKKernelObject.SleepEx( ASleepTime: Cardinal; AlertAble: Boolean );
begin
	Windows.SleepEx( ASleepTime, AlertAble );
end;

function TKKernelObject.ChangeInheritance( IsInheritable: Boolean ): Boolean;

	function DupHandles( Inherit: Boolean ): Boolean;
	var
		hAux: THandle;
	begin
		Result := DuplicateHandle( GetCurrentProcess, FHandle, GetCurrentProcess, @hAux,
			0, Inherit, DUPLICATE_SAME_ACCESS );
		if Result then	
		begin
			if ( not Windows.CloseHandle( FHandle ) ) then
				Self.SetLastError( Windows.GetLastError )
			else
 				FHandle := hAux;
		end
		else
		  Self.SetLastError( Windows.GetLastError );
	end;

begin
	Result := ( ( IsInheritable and ( not InheritHandles ) and DupHandles( True ) ) or
		( ( ( not IsInheritable ) and InheritHandles ) and DupHandles( False ) ) );
end;

{ TKSyncObject }

constructor TKSyncObject.Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
	const AName: string );
begin
	inherited Create( bInheritHandles, lpSecurityDescriptor );
	FName := AName;
	HandleNeeded( false );
end;

constructor TKSyncObject.CreateOpen( CanInherit: Boolean; const AName: string );
begin
	ForceTrimStr( AName );
	inherited Create( False, nil ); { For opening operation we will not use these values }
	FName := AName;
	FOpenInheritable := CanInherit;
	HandleNeeded( true );
end;

destructor TKSyncObject.Destroy;
begin
	UnLock; { leave any waiting threads to be signaled... }
	inherited Destroy;
end;

(*
procedure TKSyncObject.FreeSecurityDescriptor;
begin
	if ( FDescSizeNeeded > 0 ) then
	begin
		FreeMem( FSecurityAttributes.lpSecurityDescriptor, FDescSizeNeeded );
		FDescSizeNeeded := 0;
	end;
end;

procedure TKSyncObject.GetSecurityDescriptor;
var
	i: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
begin
	ForceWinNT( True );
	FreeSecurityDescriptor;
	ZeroMemory( FSecurityAttributes, SizeOf( TSecurityAttributes ) );
	FSecurityAttributes.nLength := SizeOf( TSecurityAttributes );
	GetKernelObjectSecurity( Handle, OWNER_SECURITY_INFORMATION, nil, 0, FDescSizeNeeded );
	GetMem( FSecurityAttributes.lpSecurityDescriptor, FDescSizeNeeded );
	try
		GetKernelObjectSecurity( Handle, OWNER_SECURITY_INFORMATION,
			PSecurityDescriptor( FSecurityAttributes.lpSecurityDescriptor ), FDescSizeNeeded, i );
	except
		FreeMem( FSecurityAttributes.lpSecurityDescriptor, FDescSizeNeeded );
		raise;
	end;
end;

procedure TKSyncObject.SetSecurityDescriptor( ASecurity: PSecurityAttributes );
begin
	ForceWinNT( True );
	FreeSecurityDescriptor;
	ZeroMemory( FSecurityAttributes, SizeOf( TSecurityAttributes ) );
	if CheckPointer( ASecurity ) then
		Move( ASecurity^, FSecurityAttributes, SizeOf( TSecurityAttributes ) );
{
	else
	begin
		FSecurityAttributes.bInheritHandle := FOpenInheritable;

	Access violation because lpSecurityDescriptor was not initialized, but it is a untyped pointer
	so, with how much memory do I have to initialize it ?

		InitializeSecurityDescriptor( PSecurityDescriptor( FSecurityAttributes.lpSecurityDescriptor ),
			SECURITY_DESCRIPTOR_REVISION );
	end;
}
	FSecurityAttributes.nLength := SizeOf( TSecurityAttributes );
end;
*)

{ TKCustomEvent }

constructor TKCustomEvent.Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
	AManualReset, AInitialState: Boolean; const AName: string );
begin
	FManualReset := AManualReset;
	FInitialState := AInitialState;
	inherited Create( bInheritHandles, lpSecurityDescriptor, AName );
end;

procedure TKCustomEvent.HandleNeeded( IsOpen: Boolean );
var
	i: Integer;
begin
	if ( not CheckHandle( FHandle ) ) then
	begin
		if IsOpen then
			ForceTrimStr( FName );
		{ Hard Couple! }
		case CREATION_FILTER[CheckWinNT, IsOpen] of
			W95_CREATE: FHandle := Windows.CreateEvent( nil, FManualReset, FInitialState, PChar( FName ) );
			W95_OPEN  : FHandle := OpenEvent( EVENT_ALL_ACCESS or	EVENT_MODIFY_STATE, FOpenInheritable, PChar( FName ) );
			WNT_CREATE: FHandle := Windows.CreateEvent( FSecurityAttributes, FManualReset, FInitialState, PChar( FName ) );
			WNT_OPEN  : FHandle := OpenEvent( EVENT_ALL_ACCESS or EVENT_MODIFY_STATE or SYNCHRONIZE, FOpenInheritable, PChar( FName ) );
		end;
		i := Windows.GetLastError;
		Self.SetLastError( i );
		if ( ( i <> NO_ERROR ) or ( not CheckHandle( FHandle ) ) ) then
			RaiseExceptionFmt( EKEvent, sErrEventCreate, [SysErrorMessage( i )] );
	end;
end;

function TKCustomEvent.UnLock: Boolean;
begin
{
	The SetEvent API will put the event object in a signaled state, so, it will
	became unlocked.
}
	Result := SetEvent;
end;

function TKCustomEvent.Lock( AWaitTime: Cardinal ): Boolean;
begin
{
	The ResetEvent API will put the event object in a nonsignaled state, so, it
	will became locked. If the event is manual reset, the ResetEvent API will be called
	directly, otherwise, the WaitForSingleObject will be called instead.
	Here the Reset/Set logic was inverted... the reset put to nonsignaled!!!
}
	if FManualReset then
		Result := ResetEvent    // And if i want to put the Event object to wait ?
	else
  	Result := inherited Lock( AWaitTime );
end;

function TKCustomEvent.SetEvent: Boolean;
begin
	Result := Windows.SetEvent( Handle );
	if ( not Result ) then
		Self.SetLastError( Windows.GetLastError );
end;

function TKCustomEvent.ResetEvent: Boolean;
begin
	Result := Windows.ResetEvent( Handle );
	if ( not Result ) then
		Self.SetLastError( Windows.GetLastError );
end;

function TKCustomEvent.PulseEvent: Boolean;
begin
	Result := Windows.PulseEvent( Handle );
	if ( not Result ) then
		Self.SetLastError( Windows.GetLastError );
end;

{ TKSimpleEvent }

constructor TKSimpleEvent.CreateSimple;
begin
	{ Creates a non inheriable unamed manual reset and nonsignaled event without security. }
	inherited Create( False, nil, True, False, '' );
end;

{ TKCustomSemaphore }

constructor TKCustomSemaphore.Create( bInheritHandles: Boolean;
	lpSecurityDescriptor: Pointer; AInitialCount, AMaxCount: Cardinal; const AName: string );
begin
	FMaxCount := AMaxCount;
	FPreviousCount := 0;
	FInitialCount := AInitialCount;
	if ( FMaxCount = 0 ) then
		Inc( FMaxCount );
	if ( FMaxCount < FInitialCount ) then
		FInitialCount := ( FMaxCount - 1 );
	inherited Create( bInheritHandles, lpSecurityDescriptor, AName );
end;

destructor TKCustomSemaphore.Destroy;
begin
	inherited Destroy;
	UnLockEx( FMaxCount - FPreviousCount - 1 ); { leave any waiting threads to be signaled... }
	if CheckHandle( Handle ) then
		Windows.CloseHandle( FHandle ); { Hard Couple! }
end;

procedure TKCustomSemaphore.CloseHandle;
begin
	{ Do nothing! }
end;

function TKCustomSemaphore.GetPreviousCount: Cardinal;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FPreviousCount;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

procedure TKCustomSemaphore.HandleNeeded( IsOpen: Boolean );
var
	i: Integer;
begin
	if ( not CheckHandle( FHandle ) ) then
	begin
		if IsOpen then
			ForceTrimStr( FName );
		{ Hard Couple! }
		case CREATION_FILTER[CheckWinNT, IsOpen] of
			W95_CREATE: FHandle := Windows.CreateSemaphore( nil, FInitialCount, FMaxCount, PChar( FName ) );
			W95_OPEN  : FHandle := OpenSemaphore( SEMAPHORE_ALL_ACCESS or SEMAPHORE_MODIFY_STATE, FOpenInheritable, PChar( FName ) );
			WNT_CREATE: FHandle := Windows.CreateSemaphore( FSecurityAttributes, FInitialCount, FMaxCount, PChar( FName ) );
			WNT_OPEN  : FHandle := OpenSemaphore( SEMAPHORE_ALL_ACCESS or SEMAPHORE_MODIFY_STATE or SYNCHRONIZE, FOpenInheritable, PChar( FName ) );
		end;
		i := Windows.GetLastError;
		Self.SetLastError( i );
		if ( ( i <> NO_ERROR ) or ( not CheckHandle( FHandle ) ) ) then
			RaiseExceptionFmt( EKSemaphore, sErrSemaphoreCreate, [SysErrorMessage( i )] );
	end;
end;

function TKCustomSemaphore.Unlock: Boolean;
begin
	Result := UnlockEx( 1 );
end;

function TKCustomSemaphore.UnlockEx( Count: Cardinal ): Boolean;
begin
{
	This function can be called in a initialized nonsignaled semaphore (InitialCount = 0).
	Then, when all initialization has been done, Unlock to leave other threads access the
	resources protected by this semaphore. This is the Same policy as an initial Owned mm
	mutex! If count caused the release action to exeed the MaxCount or Less then or equal
	to 0, the return value is False and no changes will be done.
}
	Result := ( Count > 0 );
	if Result then
	begin
		Result := ReleaseSemaphore( Handle, Count, @FPreviousCount );
		if ( not Result ) then
			Self.SetLastError( Windows.GetLastError );
	end;
end;

{ TKSimpleSemaphore }

constructor TKSimpleSemaphore.CreateSimple( AInitialCount, AMaxCount: Cardinal );
begin
	{
		Creates a non inheriable unamed semaphore without security with AInitialCount and AMaxCount.
	}
	inherited Create( False, nil, AInitialCount, AMaxCount, '' );
end;

{ TKCustomMutex }

constructor TKCustomMutex.Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
	AnInitialOwned: Boolean; const AName: string );
begin
	FInitialOwned := AnInitialOwned;
	inherited Create( bInheritHandles, lpSecurityDescriptor, AName );
end;

procedure TKCustomMutex.HandleNeeded( IsOpen: Boolean );
var
	i: Integer;
begin
	if ( not CheckHandle( FHandle ) ) then
	begin
		if IsOpen then
			ForceTrimStr( FName );
		{ Hard Couple! }
		case CREATION_FILTER[CheckWinNT, IsOpen] of
			W95_CREATE: FHandle := Windows.CreateMutex( nil, FInitialOwned, PChar( FName ) );
			W95_OPEN  : FHandle := OpenMutex( MUTEX_ALL_ACCESS, FOpenInheritable, PChar( FName ) );
			WNT_CREATE: FHandle := Windows.CreateMutex( FSecurityAttributes, FInitialOwned, PChar( FName ) );
			WNT_OPEN  : FHandle := OpenMutex( MUTEX_ALL_ACCESS or SYNCHRONIZE, FOpenInheritable, PChar( FName ) );
		end;
		i := Windows.GetLastError;
		Self.SetLastError( i );
		if ( ( i <> NO_ERROR ) or ( not CheckHandle( FHandle ) ) ) then
			RaiseExceptionFmt( EKMutex, sErrMutexCreate, [SysErrorMessage( i )] );
	end;
end;

procedure TKCustomMutex.DoAbandoned;
begin
 	CrtSec.Lock;
	try
		if Assigned( FOnAbandoned ) then
			FOnAbandoned( Self );
	finally
 		CrtSec.UnLock;
	end;
end;

function TKCustomMutex.WaitFor( AWaitTime: Cardinal ): TKWaitResult;
begin
	Result := inherited WaitFor( AWaitTime );
	if ( Result = kwrAbandoned ) then
		DoAbandoned;
end;

function TKCustomMutex.WaitForEx( AWaitTime: Cardinal; AlertAble: Boolean ): TKWaitResult;
begin
	Result := inherited WaitForEx( AWaitTime, AlertAble );
	if ( Result = kwrAbandoned ) then
		DoAbandoned;
end;

function TKCustomMutex.Unlock: Boolean;
begin
	{
		The unlock will fail if the calling thread does not own the Mutex Object
		either by a WaitFor/Lock or Initial owned mutex object.
	}
	Result := ReleaseMutex( Handle );
	if ( not Result ) then
		Self.SetLastError( Windows.GetLastError );
end;

{ TKSimpleMutex }

constructor TKSimpleMutex.CreateSimple;
begin
	{ Creates a non inheriable unamed initial owned (nonsignaled) mutex without security }
	Create( False, nil, True, '' );
end;

{ TKSingletonMutex }

constructor TKSingletonMutex.CreateSingleton( AClassType: TClass );
begin
	{
		We use this object to garantee that only one ClassType can be created in a
		per-thread based policy (see the Mutex Name construction below). Other
		policies can be applied like only one per process. But we use this inside
		our DLL's (so, GetCurrentProcessID could conflict with multiple DLL loading).
		We assume that is dificult to have this kind of object inside a worker thread.
		(eg. TKTrayIcon, TKExcept, TKAppControl, etc... ).
	}
	ForceClass( AClassType );
	FObjectClass := AClassType;
	inherited Create( True, nil, True, Format( SINGLETON_MUTEXT_NAME_PATTERN,
		[FObjectClass.ClassName, GetCurrentThreadID] ) );
end;

function ForceSingletonClass( AClass: TClass ): TKSingletonMutex;
begin
	Result := nil;
	try
		Result := TKSingletonMutex.CreateSingleton( AClass );
	except
		on E: Exception do
		begin
			Result.Free;
			if CheckObjectClass( E, EKMutex ) then
				RaiseExceptionFmt( EKMutex, sErrSingletonClass, [AClass.ClassName] )
			else
				raise;
		end;
	end;
end;

{ TKCustomThread }

type

	PKThreadRec = ^TKThreadrec;
	TKThreadRec = record
		lpParam: Pointer;
		cThread: TKCustomThread;
	end;

function ThreadWrapperFunc( Parameter: Pointer ): Integer;
var
	bFree: Boolean;
begin
	Result := THREAD_EXIT_CODE_NO_ERROR;
	try
		with PKThreadRec( Parameter )^ do
		begin
			if Assigned( cThread.FExecuteFunc ) then
				Result := cThread.FExecuteFunc( cThread, lpParam );
			cThread.CrtSec.Lock; { Hard Couple!!! }
			try
				bFree := cThread.FFreeOnTerminate;
				cThread.FReturnValue := Result;
				cThread.FFinished := True;
			finally
				cThread.CrtSec.UnLock;
			end;
			try
        { does not need critical section because FTerminateProc could not change }
				if Assigned( cThread.FTerminateProc ) then
					cThread.FTerminateProc( cThread );
			finally
				if bFree then
					cThread.Free;
			end;
  	end;
	finally
    EndThread( Result ); 
	end;
end;
  
constructor TKCustomThread.Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
	AStackSize: Cardinal; AFreeOnTerminate: Boolean; AExecuteFunc: TKThreadFunc;
	Parameter: Pointer; ATerminateProc: TNotifyEvent; ACreationFlags: Cardinal );
begin
	inherited Create( bInheritHandles, lpSecurityDescriptor );
	FFinished := False;
	FTerminated := False;
	FStackSize := AStackSize;
	FExecuteFunc := AExecuteFunc;
	FTerminateProc := ATerminateProc;
	FCreationFlags := ACreationFlags;
	FFreeOnTerminate := AFreeOnTerminate;
  FSynchEvent := TKSimpleEvent.CreateSimple;
	FReturnValue := THREAD_EXIT_CODE_NO_ERROR;
	FLastSuspendCount := 0;
	FSuspended := ( ACreationFlags = CREATE_SUSPENDED );
	PKThreadRec( FThreadWrapperPtr ) := New( PKThreadRec );
	PKThreadRec( FThreadWrapperPtr )^.cThread := Self;
	PKThreadRec( FThreadWrapperPtr )^.lpParam := Parameter;
	FHandle := BeginThread( SecurityAttributes, AStackSize, ThreadWrapperFunc,
		FThreadWrapperPtr, ACreationFlags, {$IFNDEF DELPHI4}Integer( FThreadID )
		{$ELSE}FThreadID{$ENDIF}); { Hard Couple!!! }
end;

destructor TKCustomThread.Destroy;
begin
  FSynchEvent.Free;
	if ( not ( FFinished or FSuspended ) ) then
	begin
		Terminate;
		Lock( Cardinal( INFINITE ) ); { wait thread to terminate }
	end;
	Dispose( PKThreadRec( FThreadWrapperPtr ) );
	inherited Destroy;
end;

function TKCustomThread.GetThreadTimes: TKKernelObjTimes;
begin
	{
		The FILETIME structure is a 64-bit value representing the number of
		100-nanosecond intervals since January 1, 1601.
		1 nanosecond = 10e-9 seconds 1 milisecond = 10e-3 seconds
		So, if X = 100-nsec; there are 1e7 X / sec; there are 3.6e10 X / hour;
		there are 8.64e11 X / day; there are 3.1536e14 X / year; 1999-1601 (years)=398
		there are 1.2551328e17 X from 01/01/1601-01/01/1999 (398); /.... ??? ..../
	}
 	CrtSec.Lock;
	try
		with Result do
			if ( not Windows.GetThreadTimes( Handle, CreationTime, ExitTime, KernelTime, UserTime ) ) then
			begin
				ZeroMemory( @Result, SizeOf( TKKernelObjTimes ) );
				Self.SetLastError( Windows.GetLastError );
			end;
	finally
 		CrtSec.UnLock;
	end;
end;

function TKCustomThread.GetTerminated: Boolean;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FTerminated;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomThread.GetLastSuspendCount: Cardinal;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FLastSuspendCount;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomThread.GetPriority: TKThreadPriority;
var
	p: Integer;
	i: TKThreadPriority;
begin
	{
	CrtSec.Lock;
	try
	}
		p := GetThreadPriority( FHandle );
		Result := ktpNormal;
		for i := Low( TKThreadPriority ) to High( TKThreadPriority ) do
			if ( THREAD_PRIORITY[i] = p ) then
				Result := i;
		if ( Result = ktpError ) then
			Self.SetLastError( Windows.GetLastError );
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomThread.GetSuspended: Boolean;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FSuspended;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomThread.GetFreeOnTerminate: Boolean;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FFreeOnTerminate;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

procedure TKCustomThread.SetPriority( Value: TKThreadPriority );
begin
 	CrtSec.Lock;
	try
		if ( Value <> ktpError ) then
			if ( not SetThreadPriority( FHandle, THREAD_PRIORITY[Value] ) ) then
				Self.SetLastError( Windows.GetLastError );
	finally
 		CrtSec.UnLock;
	end;
end;

procedure TKCustomThread.SetSuspended( Value: Boolean );
begin
 	CrtSec.Lock;
	try
		if ( Value <> FSuspended ) then
			if Value then
				Suspend
			else
				Resume;
	finally
 		CrtSec.UnLock;
	end;
end;

procedure TKCustomThread.SetFreeOnTerminate( Value: Boolean );
begin
 	CrtSec.Lock;
	try
		FFreeOnTerminate := Value;
	finally
 		CrtSec.UnLock;
	end;
end;

procedure TKCustomThread.Resume;
begin
	FLastSuspendCount := ResumeThread( Handle );
	if ( FLastSuspendCount = 1 ) then
		FSuspended := False
	else if ( FLastSuspendCount = THREAD_SUSPEND_ERROR ) then
		Self.SetLastError( Windows.GetLastError );
end;

procedure TKCustomThread.Suspend;
begin
	FLastSuspendCount := SuspendThread( Handle );
	if ( FLastSuspendCount <> THREAD_SUSPEND_ERROR ) then
		FSuspended := True
	else
		Self.SetLastError( Windows.GetLastError );
end;

procedure TKCustomThread.Terminate;
begin
 	CrtSec.Lock;
	try
		FTerminated := True;
	finally
 		CrtSec.UnLock;
	end;
end;

procedure TKCustomThread.TerminateEx( AWaitTime: Cardinal );
begin
	if ( not Lock( AWaitTime ) ) then
	  ForceTerminate;	  
end;

procedure TKCustomThread.CloseHandle;
begin
  if ( not FThreadUnLockTerminate ) then
  begin
  	if CheckHandle( Handle ) then
	  	if ( not Windows.CloseHandle( FHandle ) ) then
		  	Self.SetLastError( Windows.GetLastError );
  	FHandle := INVALID_HANDLE_VALUE;
  end
  else
    { do nothing };  
end;

function TKCustomThread.UnLock: Boolean;
begin
  Result := True;
  FThreadUnLockTerminate := True;
  Free; { this could cause troubles ? }
  EndThread( THREAD_EXIT_CODE_NO_ERROR );
	{
		Unlock for a thread will terminate it's execution! Do not call TerminateThread
  	Result := Windows.TerminateThread( Handle, THREAD_EXIT_CODE_NO_ERROR );

	  if ( not Result ) then
	    Self.SetLastError( Windows.GetLastError ); // this line will never be executed 
  }
end;

procedure TKCustomThread.ForceTerminate;
begin
	if ( not TerminateThread( Handle, FReturnValue ) ) then
		Self.SetLastError( Windows.GetLastError );
end;

{ TKSimpleThread }

constructor TKSimpleThread.CreateSimple( AExecuteFunc: TKThreadFunc; Parameter: Pointer;
	ATerminateProc: TNotifyEvent );
begin
	{
		Creates a free on terminate non suspended thread without security, with default
		stack size. There is no check for the Parameter and ATerminateProc.
	}
	inherited Create( False, nil, 0, True, AExecuteFunc, Parameter, ATerminateProc, 0 );
end;

{ TKCustomWorkerThread }

const
	THREAD_SUSPENDED: array[Boolean] of Cardinal = ( 0, CREATE_SUSPENDED );

constructor TKCustomWorkerThread.CreateWorker( AOwner: TObject; CreateSuspended,
  CreateMessageQueue: Boolean );
begin
	FOwner := AOwner;
  FCreateMessageQueue := CreateMessageQueue;
  FMsgFilterMin := 0;
  FMsgFilterMax := 0;
  if FCreateMessageQueue then
    FMsgEvent := TKSimpleEvent.CreateSimple;
	inherited Create( False, nil, 0, True, ThreadExecute, FOwner, ThreadTerminate,
		THREAD_SUSPENDED[CreateSuspended] );
	SynchronizeOwner;
  if ( CreateMessageQueue and ( not ( FMsgEvent.WaitFor( Cardinal( INFINITE ) ) = kwrSignaled ) ) ) then
    RaiseException( EKWorkerThread, sErrWorkerMsgQueue );
end;

destructor TKCustomWorkerThread.Destroy;
begin
  FreeClean( FMsgEvent );
  inherited Destroy;
end;

function TKCustomWorkerThread.GetOwner: TObject;
begin
	Result := FOwner;
end;

function TKCustomWorkerThread.CheckExecuteCondition: Boolean;
begin
  Result := True;
end;

function TKCustomWorkerThread.DoExecute( Param: Pointer ): Cardinal;
var
  b: Boolean;
begin
  Result := THREAD_EXIT_CODE_NO_ERROR;
  if Assigned( FOnExecute ) then
  begin
    b := True;
    SynchEvent.SetEvent;
    while ( b and ( Result = THREAD_EXIT_CODE_NO_ERROR ) and ( not Terminated ) and
      CheckExecuteCondition ) do
    begin
      if ( SynchEvent.WaitFor( 100 ) <> kwrSignaled ) then
        Exit;
      FOnExecute( Self, Param, Result, b );
    end;
  end;    
end;

function TKCustomWorkerThread.DoExecuteMsg( Param: Pointer; msg: TMsg; 
  var Processed: Boolean ): Boolean;
begin
  Result := True;
  Processed := False;               
  if Assigned( FOnExecuteMsg ) then
    FOnExecuteMsg( Self, Param, msg, Processed, Result );
end;

procedure TKCustomWorkerThread.DoTerminate;
begin
	if Assigned( FOnTerminate ) then
		FOnTerminate( Self );
end;

procedure TKCustomWorkerThread.SynchronizeOwner;
begin
	{ Do nothing }
end;

procedure TKCustomWorkerThread.Execute;
begin
	if Suspended then
		Resume;
end;

procedure TKCustomWorkerThread.ThreadTerminate( Sender: TObject );
begin
	DoTerminate;
end;

procedure TKCustomWorkerThread.Terminate;
begin
  if FCreateMessageQueue then
    PostQuitMessage;
  inherited Terminate;
end;

procedure TKCustomWorkerThread.PostQuitMessage;
begin
  if FCreateMessageQueue then
    PostThreadMessage( ThreadID, WM_QUIT, 0, 0 )
  else
    RaiseExceptionFmt( EKWorkerThread, sErrInvPostQuitMessage, [ThreadID] );
end;

function TKCustomWorkerThread.ThreadExecute( Sender: TKCustomThread; Param: Pointer ): Cardinal;
var
  msg: TMsg;
  bProcessed: Boolean;
begin
  if FCreateMessageQueue then
  begin
    PeekMessage( msg, 0, 0, 0, PM_NOREMOVE); { Create message queue }
    FMsgEvent.SetEvent;
    Result := THREAD_EXIT_CODE_NO_ERROR; 
    { When terminate was called the PostQuitMessage will be issued } 
    while GetMessage( msg, 0, FMsgFilterMin, FMsgFilterMax ) do
      if ( not DoExecuteMsg( Param, msg, bProcessed ) ) then
        Exit
      else if ( not bProcessed ) then
      begin
        TranslateMessage( msg );
        DispatchMessage( msg );
      end;  
  end
  else
  	Result := DoExecute( Param );
end;

{ TKSimpleWorkerThread }

constructor TKSimpleWorkerThread.CreateSimple( AOwner: TObject; ExecuteEvent:
	TKWorkerThreadExecuteEvent; TerminateEvent: TNotifyEvent );
begin
	ForceReference( ExecuteEvent );
	OnExecute := ExecuteEvent;
	OnTerminate := TerminateEvent;
	inherited CreateWorker( AOwner, False, False );
	FreeOnTerminate := True;
end;

{ TKCustomTimerThread }

constructor TKCustomThreadedTimer.Create( CreateSuspended: Boolean );
begin
	inherited Create;
	FLockable := False;
	FEnabled := ( not CreateSuspended );
	FInterval := SECOND_TO_MSECOND;
	FEvent := TKSimpleEvent.CreateSimple;
	FMutex := TKMutex.Create( False, nil, False, '' );
	FThread := TKThread.Create( False, nil, 0, False, ThreadExecute, nil, ThreadTerminate,
		THREAD_SUSPENDED[CreateSuspended] );
end;

destructor TKCustomThreadedTimer.Destroy;
begin
	FThread.Terminate;
	FMutex.UnLock;
	if FEvent.SetEvent then      { signal thread to terminate }
		FThread.Lock( Interval );
	if ( not FThread.Lock( 0 ) ) then
		FThread.Suspend; { to avoid thread deadlock (WaitFor( Infinite ))}
	FThread.Free;
	FEvent.Free;
	FreeClean( FMutex );
	inherited Destroy;
end;

procedure TKCustomThreadedTimer.UpdateTimer;
begin
	if ( not FThread.Suspended ) then
		FThread.Suspend;
	if ( ( FInterval <> 0 ) and FEnabled and Assigned( FOnTimer ) ) then
		if FThread.Suspended then
			FThread.Resume;
end;

function TKCustomThreadedTimer.GetPriority: TKThreadPriority;
begin
	Result := FThread.Priority;
end;

procedure TKCustomThreadedTimer.SetEnabled( Value: Boolean );
begin
	if ( Value <> FEnabled ) then
	begin
		FEnabled := Value;
		UpdateTimer;
	end;
end;

procedure TKCustomThreadedTimer.SetInterval( Value: Cardinal );
begin
	if ( Value <> FInterval ) then
	begin
		FInterval := Value;
		UpdateTimer;
	end;
end;

procedure TKCustomThreadedTimer.SetOnTimer( Value: TNotifyEvent );
begin
	FOnTimer := Value;
	UpdateTimer;
end;

procedure TKCustomThreadedTimer.SetPriority( Value: TKThreadPriority );
begin
	if ( Value <> FThread.Priority ) then
	begin
		FThread.Priority := Value;
		UpdateTimer;
	end;
end;

function TKCustomThreadedTimer.DoTimer: Cardinal;
begin
	Result := THREAD_EXIT_CODE_NO_ERROR;
	if ( Assigned( FOnTimer ) and ( not FThread.Terminated ) ) then
		FOnTimer( Self );
end;

function TKCustomThreadedTimer.ThreadExecute( Sender: TKCustomThread; Param: Pointer ): Cardinal;
begin
	repeat
		if ( FLockable and CheckObject( FMutex ) ) then
			FMutex.Lock( Cardinal( INFINITE ) );
		try
			Windows.Sleep( FInterval );
			Result := DoTimer;
			if ( FEvent.Lock( 0 ) ) then
				Exit;
		finally
			if ( FLockable and CheckObject( FMutex ) ) then
				FMutex.UnLock;
		end;
	until ( ( Result <> THREAD_EXIT_CODE_NO_ERROR ) or Sender.Terminated );
end;

procedure TKCustomThreadedTimer.ThreadTerminate( Sender: TObject );
begin
  { do nothing }  
end;

{ TKCustomProcess }

constructor TKCustomProcess.Create( const AnApplicationName, ACommandLine: string;
	bProcessInheritHandles: Boolean; lpProcessSecurityDescriptor: Pointer;
	bThreadInheritHandles: Boolean; lpThreadSecurityDescriptor: Pointer;
	bInheritParentHandles: Boolean; ACreationFlags: Cardinal; AnEnvironment: TStrings;
	const ACurrentDirectory: string; AStartUpInfo: PStartUpInfo );
var
	pc: PChar;
begin
	inherited Create( bProcessInheritHandles, lpProcessSecurityDescriptor );
	FOpened := False;
	FOpenAccess := 0;
	FCommandLine := ACommandLine;
	FApplicationName := AnApplicationName;
	FCurrentDirectory := ACurrentDirectory;
	FOpenInheritable := False;
	FInheritParentHandles := bInheritParentHandles;
	FCreationFlags := ACreationFlags;
	FEnvironment := TStringList.Create;
	FEnvironment.Assign( AnEnvironment );
	FThreadSecurity := nil;
	if ( bThreadInheritHandles or CheckPointer( FThreadSecurity ) ) then
	begin
		FThreadSecurity := New( PSecurityAttributes );
		ZeroMemory( FThreadSecurity, SizeOf( TSecurityAttributes ) );
		FThreadSecurity^.nLength := SizeOf( TSecurityAttributes );
		FThreadSecurity^.bInheritHandle := bThreadInheritHandles;
		FThreadSecurity^.lpSecurityDescriptor := lpThreadSecurityDescriptor;
	end;
	ZeroMemory( @FStartUpInfo, SizeOf( TStartUpInfo ) );
	ZeroMemory( @FProcessInfo, SizeOf( TProcessInformation ) );
	if CheckPointer( AStartUpInfo ) then
		Move( AStartUpInfo^, FStartUpInfo, SizeOf( TStartUpInfo ) )
	else
		with FStartUpInfo do
		begin
			cb := SizeOf( TStartupInfo );
			dwFlags := STARTF_USESHOWWINDOW;
			wShowWindow := SW_SHOWNORMAL;
			hStdInput := INVALID_HANDLE_VALUE;
			hStdOutput := INVALID_HANDLE_VALUE;
			hStdError := INVALID_HANDLE_VALUE;
		end;
	pc := nil;
	if CheckStrings( FEnvironment ) then
		pc := StrAllocMem( StringsToPCharList( FEnvironment, nil ) );
	try
		if CheckPointer( pc ) then
			StringsToPCharList( FEnvironment, pc );
		if ( not Windows.CreateProcess( PChar( FApplicationName ), PChar( FCommandLine ),
			SecurityAttributes, ThreadSecurity, FInheritParentHandles, FCreationFlags,
			pc, PChar( FCurrentDirectory ), FStartUpInfo, FProcessInfo ) ) then
				Self.SetLastError( Windows.GetLastError );
	finally
		if CheckPointer( pc ) then
			StrDispose( pc );
	end;
end;

constructor TKCustomProcess.CreateOpen( AOpenAccess, AProcessID: Cardinal; CanInherit: Boolean );
var
	pc: PChar;
begin
	inherited Create( False, nil ); { For opening operation we will not use these values }
	FOpened := True;
	FOpenAccess := AOpenAccess;
	FCommandLine := GetCommandLine;
	FCreationFlags := 0;
	FThreadSecurity := nil;
	FInheritParentHandles := False;
	FOpenInheritable := CanInherit;
	FEnvironment := TStringList.Create;
	pc := GetEnvironmentStrings;
	try
		PCharListToStrings( pc, FEnvironment );
	finally
		FreeEnvironmentStrings( pc );
	end;
	GetStartUpInfo( FStartUpInfo );
	ZeroMemory( @FProcessInfo, SizeOf( TProcessInformation ) );
	FProcessInfo.dwProcessId := AProcessID;
	FProcessInfo.hProcess := OpenProcess( FOpenAccess, FOpenInheritable, AProcessID );
	if ( FProcessInfo.hProcess = NULL_HANDLE_VALUE ) then
		Self.SetLastError( Windows.GetLastError )
	else
	begin
		FProcessInfo.hThread := GetCurrentThread;
		FProcessInfo.dwThreadId := GetCurrentThreadID;
	end;
end;

destructor TKCustomProcess.Destroy;
begin
	FEnvironment.Free;
	if CheckPointer( FThreadSecurity ) then
		Dispose( FThreadSecurity );
	inherited Destroy;
end;

function TKCustomProcess.GetHandle: THandle;
begin
	{
	CrtSec.Lock;
	try
	}
		Result := FProcessInfo.hProcess;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomProcess.GetExitCode: Cardinal;
begin
	{
	CrtSec.Lock;
	try
	}
		if ( not GetExitCodeProcess( Handle, {$IFNDEF DELPHI4}Integer( Result )
			{$ELSE}Result{$ENDIF} ) ) then
		begin
			Result := 0;
			Self.SetLastError( Windows.GetLastError );
		end;
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomProcess.GetProcessTimes: TKKernelObjTimes;
begin
 	CrtSec.Lock;
	try
		with Result do
			if ( not Windows.GetProcessTimes( Handle, CreationTime, ExitTime, KernelTime, UserTime ) ) then
			begin
				ZeroMemory( @Result, SizeOf( TKKernelObjTimes ) );
				Self.SetLastError( Windows.GetLastError );
			end;
	finally
 		CrtSec.UnLock;
	end;
end;

function TKCustomProcess.GetPriority: TKProcessPriority;
var
	p: DWORD;
	i: TKProcessPriority;
begin
	{
	CrtSec.Lock;
	try
	}
		p := GetPriorityClass( Handle );
		Result := ppNormal;
		if ( p <> 0 ) then
			for i := Low( TKProcessPriority ) to High( TKProcessPriority ) do
				if ( PROCESS_PRIORITY[i] = p ) then
					Result := i
		else
			Self.SetLastError( Windows.GetLastError );
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomProcess.GetAfinityMask( Index: Integer ):
  {$IFDEF DELPHI4}Cardinal{$ELSE}DWORD{$ENDIF};
var
	b: Boolean;
begin
	{
	CrtSec.Lock;
	try
	}
		b := False;
		case Index of
			0: b := GetProcessAffinityMask( Handle, Result,
				{$IFDEF DELPHI4}Cardinal( Index ){$ELSE}Index{$ENDIF} );
			1: b := GetProcessAffinityMask( Handle,
			  {$IFDEF DELPHI4}Cardinal( Index ){$ELSE}Index{$ENDIF}, Result );
		else
		  Result := 0;	
		end;
		if ( not b ) then
  		Self.SetLastError( Windows.GetLastError );
	{
	finally
		CrtSec.UnLock;
	end;
	}
end;

function TKCustomProcess.GetThreadInheritHandles: Boolean;
begin
  Result := ( CheckPointer( FThreadSecurity ) and FThreadSecurity^.bInheritHandle );
end;

procedure TKCustomProcess.SetPriority( Value: TKProcessPriority );
begin
 	CrtSec.Lock;
	try
		if ( not SetPriorityClass( Handle, PROCESS_PRIORITY[Value] ) ) then
			Self.SetLastError( Windows.GetLastError );
	finally
    CrtSec.UnLock;
	end;
end;

function TKCustomProcess.UnLock: Boolean;
begin
	{
		Unlock for a process will terminate it's execution!
	}
	Result := TerminateProcess( Handle, PROCESS_EXIT_CODE_NO_ERROR );
end;

procedure TKCustomProcess.ExitProcess( AnExitCode: Cardinal );
begin
	Windows.ExitProcess( AnExitCode );
end;

procedure TKCustomProcess.ForceTerminateProcess( AnExitCode: Cardinal );
begin
	if ( not Windows.TerminateProcess( Handle, AnExitCode ) ) then
		Self.SetLastError( Windows.GetLastError );
end;

function TKCustomProcess.WaitForInputIdle( AWaitTime: Cardinal ): TKWaitResult;
begin
	Result := kwrUnknown;
	case Windows.WaitForInputIdle( Handle, AWaitTime ) of
		0           : Result := kwrSignaled;
		WAIT_TIMEOUT:
		begin
			Result := kwrTimeOut;
			DoTimeOut;
		end;
	{ $FFFFFFFF   : }
		WAIT_FAILED :
		begin
			Result := kwrFail;
			Self.SetLastError( Windows.GetLastError );
			DoFail;
		end;
	end;
end;

function TKCustomProcess.WaitForDebugEvent( var DebugEvent: TDebugEvent;
	AWaitTime: Cardinal ): Boolean;
begin
	Result := Windows.WaitForDebugEvent( DebugEvent, AWaitTime );
	if ( not Result ) then
		Self.SetLastError( Windows.GetLastError );
end;

function TKCustomProcess.ShutDownParams( Level, Flags: Cardinal ): Boolean;
begin
	Result := SetProcessShutdownParameters( Level, Flags );
end;

{ TKSimpleProcess }

constructor TKSimpleProcess.CreateSimple( const ACommandLine: string );
begin
	Create( '', ACommandLine, False, nil, False, nil, False, NORMAL_PRIORITY_CLASS, nil,
		GetCurrentDir, nil );
end;

{ TKCustomPipe }

constructor TKCustomPipe.Create( bInheritHandles: Boolean; lpSecurityDescriptor: Pointer;
	APipeBufferSize: DWORD );
begin
	inherited Create( bInheritHandles, lpSecurityDescriptor );
	FHandle := INVALID_HANDLE_VALUE; { Hard Couple! }
	FPipeBufferSize := APipeBufferSize;
	if ( not CreatePipe( FRead, FWrite, SecurityAttributes, FBufferSize ) ) then
	begin
		Self.SetLastError( Windows.GetLastError );
		FRead := INVALID_HANDLE_VALUE;
		FWrite := INVALID_HANDLE_VALUE;
	end;
end;

function TKCustomPipe.GetHandle: THandle;
begin
	Result := INVALID_HANDLE_VALUE; { use hRead instead }
end;

procedure TKCustomPipe.CloseHandle;
begin
	inherited CloseHandle; { there is no need, but, just for oop complience }
	if CheckHandle( FRead ) then
		if ( not Windows.CloseHandle( FRead ) ) then
			Self.SetLastError( Windows.GetLastError );
	if CheckHandle( FWrite ) then
		if ( not Windows.CloseHandle( FWrite ) ) then
			Self.SetLastError( Windows.GetLastError );
	FRead := INVALID_HANDLE_VALUE;
	FWrite := INVALID_HANDLE_VALUE;
end;

function TKCustomPipe.UnLock: Boolean;
begin
	Result := False; { there is no meaning for a anonymous pipe }
end;

procedure TKCustomPipe.ChangeInheritance( IsRead, IsInheritable: Boolean );

	procedure DupHandles( Inherit: Boolean );
	var
		hAux: THandle;
	begin
		if IsRead then
		begin
			if DuplicateHandle( GetCurrentProcess, hRead, GetCurrentProcess, @hAux,
				0, Inherit, DUPLICATE_SAME_ACCESS ) then
			begin
				if ( not Windows.CloseHandle( FRead ) ) then
					Self.SetLastError( Windows.GetLastError )
				else
					FRead := hAux;
			end;
		end
		else if DuplicateHandle( GetCurrentProcess, hWrite, GetCurrentProcess, @hAux,
			0, Inherit, DUPLICATE_SAME_ACCESS ) then
		begin
			if ( not Windows.CloseHandle( FWrite ) ) then
				Self.SetLastError( Windows.GetLastError )
			else
  			FWrite := hAux;
		end;
	end;

begin
	if ( IsInheritable and ( not InheritHandles ) ) then
		DupHandles( True )
	else if ( ( not IsInheritable ) and InheritHandles ) then
		DupHandles( False );
end;

function TKCustomPipe.ReadTo( ReaderProc: TKPipeReaderProc; Data: Pointer;
	const ReadSize: Cardinal ): Integer;
var
	dwRead:  {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	dwWritten: Integer;
	bs: Integer;
	pb: PByte;
begin
	Result := -1;
	if ( CheckReference( ReaderProc ) and CheckHandles( [FRead, FWrite] ) ) then
		if ( not Windows.CloseHandle( FWrite ) ) then
			Self.SetLastError( Windows.GetLastError )
		else
		begin
			Result := 0;
			FWrite := INVALID_HANDLE_VALUE;
			bs := Max( PIPE_MINREAD_BUFFER_SIZE, ReadSize );
			pb := AllocMem( bs * SizeOf( Byte ) );
			try
				while True do
				begin
					dwRead := 0;
					dwWritten := 0;
					if ( ( not ReadFile( FRead, pb^, bs, dwRead, nil ) ) or
						( dwRead = 0 ) ) then
						Break;                                             
					if ( not ReaderProc( Self, Data, pb, dwRead, dwWritten ) ) then
						Break;
					Inc( Result, dwWritten );
				end;
			finally
				FreeMem( pb, Max( PIPE_MINREAD_BUFFER_SIZE, ReadSize ) * SizeOf( Byte ) );
			end;
		end;
end;

function TKCustomPipe.WriteFrom( WriterProc: TKPipeWriterProc; Data: Pointer;
	const WriteSize: Cardinal ): Integer;
var
	dwRead: Integer;
	dwWritten: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	bs: Integer;
	pb: PByte;
begin
	Result := -1;
	if ( CheckReference( WriterProc ) and CheckHandles( [FRead, FWrite] ) ) then//and ChangeInheritance( False, False ) ) then
	begin
		Result := 0;
		bs := Max( PIPE_MINWRITE_BUFFER_SIZE, WriteSize );
		pb := AllocMem( bs * SizeOf( Byte ) );
		try
			while True do
			begin
				dwRead := 0;
				dwWritten := 0;
				if ( not WriterProc( Self, Data, Pointer( pb ), bs, dwRead ) ) then
					Break;
				if ( not WriteFile( FWrite, pb^, {$IFDEF DELPHI4}Cardinal( dwRead ){$ELSE}
          dwRead{$ENDIF}, dwWritten, nil ) ) then
					Break;
				Inc( Result, dwWritten );
			end;
		finally
			FreeMem( pb, Max( PIPE_MINWRITE_BUFFER_SIZE, WriteSize ) * SizeOf( Byte ) );
		end;
	end;
end;

function TKCustomPipe.ReaderFileProc( Pipe: TKCustomPipe; Data: Pointer;
	const Buffer: Pointer; BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;
begin
	Result := WriteFile( THandle( Data ), Buffer^, BufferSize, {$IFDEF DELPHI4}Cardinal(
    BufferWrittenSize ){$ELSE} BufferWrittenSize{$ENDIF}, nil );
end;

function TKCustomPipe.ReadToFile( hFile: THandle; const ReadSize: Cardinal ): Integer;
begin
	if CheckHandle( hFile ) then
		Result := ReadTo( ReaderFileProc, Pointer( hFile ), ReadSize )
	else
		Result := -1;
end;

function TKCustomPipe.ReaderBufferProc( Pipe: TKCustomPipe; Data: Pointer;
	const Buffer: Pointer; BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;
var
	p: Pointer;
begin
	if ( not CheckPointer( FBuffer ) ) then
	begin
		FBuffer := AllocMem( BufferSize );
		p := FBuffer;
	end
	else
	begin
		p := IncPtr( FBuffer, FBufferSize );
		Inc( FBufferSize, BufferSize );
		ReAllocMem( FBuffer, FBufferSize );
	end;
	Move( Buffer^, p^, BufferSize );
	BufferWrittenSize := BufferSize;
	Result := True;
end;

function TKCustomPipe.ReadToBuffer( var Buffer: Pointer; const ReadSize: Cardinal ): Integer;
begin
	FBuffer := nil;
	FBufferSize := Max( PIPE_MINREAD_BUFFER_SIZE, ReadSize );
	Result := ReadTo( ReaderBufferProc, FBuffer, ReadSize );
	if ( Result = FBufferSize ) then
		Buffer := FBuffer
	else
	begin
		Result := -1;
		FreeMem( FBuffer, FBufferSize );
	end;
end;

function TKCustomPipe.ReaderStreamProc( Pipe: TKCustomPipe; Data: Pointer;
	const Buffer: Pointer; BufferSize: Integer; var BufferWrittenSize: Integer ): Boolean;
begin
	BufferWrittenSize := TMemoryStream( Data ).Write( Buffer^, BufferSize );
	Result := ( BufferWrittenSize > 0 );	
end;

function TKCustomPipe.ReadToStream( ms: TMemoryStream; const ReadSize: Cardinal ): Integer;
begin
	if ( not CheckObject( ms ) ) then
		Result := -1
	else
		Result := ReadTo( ReaderStreamProc, ms, ReadSize );
end;

function TKCustomPipe.WriterFileProc( Pipe: TKCustomPipe; Data: Pointer;
	var Buffer: Pointer; const BufferSize: Integer; var BytesRead: Integer): Boolean;
begin
	Result := ReadFile( THandle( Data ), Buffer^, BufferSize, {$IFDEF DELPHI4}Cardinal(
    BytesRead ){$ELSE} BytesRead{$ENDIF}, nil );
end;

function TKCustomPipe.WriteFromFile( hFile: THandle; const WriteSize: Cardinal ): Integer;
begin
	if CheckHandle( hFile ) then
		Result := WriteFrom( WriterFileProc, Pointer( hFile ), WriteSize )
	else
		Result := -1;
end;

{ TKSimplePipe }

constructor TKSimplePipe.CreateSimple;
begin
	{ Create a inheriable default buffer sized anonymous pipe }
	inherited Create( True, nil, 0 );
end;

{ TKKernelObjPool }

constructor TKKernelObjPool.Create( AOwnerShip: TKObjectOwnerShip );
begin
	inherited Create;
	FOwnerShip := AOwnerShip;
	FObjList := TThreadList.Create;
	ZeroMemory( @FHandles, SizeOf( TWOHandleArray ) );
end;

destructor TKKernelObjPool.Destroy;
begin
	Clear;
	FObjList.Free;
	inherited Destroy;
end;

function TKKernelObjPool.GetCount: Integer;
begin
	Result := FObjList.LockList.Count;
	try
	finally
		FObjList.UnlockList;
	end;
end;

function TKKernelObjPool.GetKernelObject( Index: Integer ): TKKernelObject;
begin
	Result := TKKernelObject( FObjList.LockList.Items[Index] );
	try
	finally
		FObjList.UnlockList;
	end;
end;

function TKKernelObjPool.GetAsEvent( Index: Integer ): TKEvent;
begin
	Result := ( GetKernelObject( Index ) as TKEvent );
end;

function TKKernelObjPool.GetAsMutex( Index: Integer ): TKMutex;
begin
	Result := ( GetKernelObject( Index ) as TKMutex );
end;

function TKKernelObjPool.GetAsSemaphore( Index: Integer ): TKSemaphore;
begin
	Result := ( GetKernelObject( Index ) as TKSemaphore );
end;

function TKKernelObjPool.GetAsThread( Index: Integer ): TKThread;
begin
	Result := ( GetKernelObject( Index ) as TKThread );
end;

function TKKernelObjPool.GetAsWorker( Index: Integer ): TKCustomWorkerThread;
begin
	Result := ( GetKernelObject( Index ) as TKCustomWorkerThread );
end;

function TKKernelObjPool.GetAsProcess( Index: Integer ): TKProcess;
begin
	Result := ( GetKernelObject( Index ) as TKProcess );
end;

function TKKernelObjPool.GetHandles( const ObjsIdx: array of Integer ): TWOHandleArray;
var
	i: Integer;
begin
	ZeroMemory( @Result, SizeOf( TWOHandleArray ) );
	if ( ObjsIdx[Low( ObjsIdx )] = WAIT_FOR_ALL_OBJECTS ) then
		for i := 0 to Count - 1 do
			Result[i] := KernelObject[i].Handle
	else
		for i := Low( ObjsIdx ) to High( ObjsIdx ) do
			Result[i] := KernelObject[i].Handle;
end;

procedure TKKernelObjPool.Clear;
var
	lst: TList;
begin
	if ( FOwnerShip = oosReference ) then
		FObjList.Clear
	else
	begin
		lst := FObjList.LockList;
		try
			while CheckList( lst ) do
			begin
				TKKernelObject( lst.First ).Free;
				lst.Delete( 0 );
			end;
		finally
			FObjList.UnlockList;
		end;
	end;
end;

function TKKernelObjPool.Add( Obj: TKKernelObject ): Integer;
begin
	Result := Count;
	FObjList.Add( Obj );
end;

procedure TKKernelObjPool.Delete( ObjIdx: Integer );
begin
	FObjList.LockList.Delete( ObjIdx );
	try
	finally
		FObjList.UnLockList;
	end;
end;

procedure TKKernelObjPool.Remove( Obj: TKKernelObject );
begin
	FObjList.Remove( Obj );
end;

function TKKernelObjPool.IndexOf( Obj: TKKernelObject ): Integer;
begin
	Result := FObjList.LockList.IndexOf( Obj );
	try
	finally
		FObjList.UnlockList;
	end;
end;

function TKKernelObjPool.CheckObjects: Boolean;
begin
	Result := ( Count <> 0 );
	if ( not Result ) then
	  SetLastError( ERROR_INVALID_HANDLE );
end;

function TKKernelObjPool.WaitFor( const ObjsIdx: array of Integer;
	AWaitTime, AWakeMask: Cardinal; WaitAll: Boolean ): DWORD;
var
	i: Cardinal;
begin
	if ( not CheckObjects ) then
		Result := WAIT_FAILED
	else
	begin
		FHandles := GetHandles( ObjsIdx );
		if ( AWakeMask = 0 ) then
			Result := WaitForMultipleObjects( Count, @FHandles, WaitAll, AWaitTime )
		else
			Result := MsgWaitForMultipleObjects( Count, PWOHandleArray( @FHandles[0] )^,
				WaitAll, AWaitTime, AWakeMask );
		if ( not CheckObjects ) then
			Result := INVALID_HANDLE_VALUE
		else
			case Result of
				WAIT_TIMEOUT  :
					for i := 0 to Count - 1 do
						KernelObject[i].DoTimeOut;
				WAIT_FAILED   :
					for i := 0 to Count - 1 do
						KernelObject[i].DoFail;
			else
				if ValueBetween( Result, WAIT_ABANDONED_0, ( WAIT_ABANDONED_0 + Count - 1 ), True ) then
					for i := 0 to Count - 1 do
						if CheckObjectClass( KernelObject[i], TKCustomMutex ) then
							TKCustomMutex( KernelObject[i] ).DoAbandoned;
			end;
	end;
end;

function TKKernelObjPool.WaitForEx( const ObjsIdx: array of Integer;
	AWaitTime: Cardinal; WaitAll, AlertAble: Boolean ): DWORD;
var
	i: Cardinal;
begin
	if ( not CheckObjects ) then
		Result := WAIT_FAILED
	else
	begin
		FHandles := GetHandles( ObjsIdx );
		Result := WaitForMultipleObjectsEx( Count, @FHandles, WaitAll, AWaitTime, AlertAble );
		if ( not CheckObjects ) then
			Result := INVALID_HANDLE_VALUE
		else
			case Result of
				WAIT_TIMEOUT  :
					for i := 0 to Count - 1 do
						KernelObject[i].DoTimeOut;
				WAIT_FAILED   :
					for i := 0 to Count - 1 do
						KernelObject[i].DoFail;
			else
				if ValueBetween( Result, WAIT_ABANDONED_0, ( WAIT_ABANDONED_0 + Count - 1 ), True ) then
					for i := 0 to Count - 1 do
						if CheckObjectClass( KernelObject[i], TKCustomMutex ) then
							TKCustomMutex( KernelObject[i] ).DoAbandoned;
			end;
	end;
end;

function TKKernelObjPool.ForEachObjDo( EnumFunc: TKKernelObjEnumFunc; Data: Pointer ): Boolean;
var
	i: Integer;
begin
	Result := ( CheckReference( EnumFunc ) and Self.CheckObjects );
	if Result then
		for i := 0 to Count - 1 do
{$BOOLEVAL ON}
			Result := ( Result and EnumFunc( Self, KernelObject[i], i, Data ) );
{$BOOLEVAL OFF}
end;

{ TKThreadLocalStorage }

constructor TKThreadLocalStorage.Create;
begin
	inherited Create;
	FLastError := NO_ERROR;
	FTlsIndex := TlsAlloc;
	if ( FTlsIndex = INVALID_TLS_INDEX ) then
		RaiseLastWin32Error;
end;

destructor TKThreadLocalStorage.Destroy;
begin
	if ( ( FTlsIndex <> INVALID_TLS_INDEX ) and ( not TlsFree( FTlsIndex ) ) ) then
		RaiseLastWin32Error;
	inherited Destroy;
end;

function TKThreadLocalStorage.GetLastErrorMessage: string;
begin
	Result := SysErrorMessage( FLastError );
end;

function TKThreadLocalStorage.SetValue( pData: Pointer ): Boolean;
begin
	Result := TlsSetValue( FTlsIndex, pData );
	if ( not Result ) then
		FLastError := GetLastError;
end;

function TKThreadLocalStorage.GetValue( var pData : Pointer ): Boolean;
begin
	pData := TlsGetValue( FTlsIndex );
	Result := CheckPointer( pData );
	if ( not Result ) then
		FLastError := GetLastError;
end;

{ TKSharedObjects }

constructor TKSharedObjects.Create( AListClass: TClass );
begin
	ForceClass( AListClass );
	inherited Create;
	FListClass := AListClass;
	FLock := TKCriticalSection.Create( False );
	ReCreateListAs( nil );
end;

destructor TKSharedObjects.Destroy;
begin
	DestroyList;
	FLock.Free;
	inherited Destroy;
end;

procedure TKSharedObjects.DestroyList;
begin
	Lock;
	try
		FreeClean( FList );
	finally
		UnLock;
	end;
end;

function TKSharedObjects.GetList: TObject;
begin
	Result := FList;
end;

function TKSharedObjects.GetListClass: TClass;
begin
	Result := FListClass;
end;

procedure TKSharedObjects.Clear;
begin
  { do nothing }
end;

procedure TKSharedObjects.Unlock;
begin
	FLock.UnLock;
end;

function TKSharedObjects.Lock: TObject;
begin
	FLock.Lock;
	Result := FList;
end;

procedure TKSharedObjects.ReCreateListAs( AListClass: TClass );
begin
	DestroyList;
	Lock;
	try
		if CheckClass( AListClass ) then
			FListClass := AListClass;
		FList := FListClass.Create;		
	finally
	  UnLock;
	end;
end;

{ TKThreadStrings }

constructor TKThreadStrings.Create( StringsClass: TStringsClass );
begin
	if ( not CheckClass( StringsClass ) ) then
		StringsClass := TStringList;
	inherited Create( StringsClass );
end;

function TKThreadStrings.GetList: TStrings;
begin
	Result := TStrings( inherited GetList );
end;

function TKThreadStrings.GetListClass: TStringsClass;
begin
	Result := TStringsClass( inherited GetListClass );
end;

function TKThreadStrings.Add( const s: string ): Integer;
begin
	Lock;
	try
		Result := List.Add( s );
	finally
		UnLock;
	end;
end;

function TKThreadStrings.AddObject( const S: string; AObject: TObject ): Integer;
begin
	Lock;
	try
		Result := List.AddObject( s, AObject );
	finally
		UnLock;
	end;
end;

function TKThreadStrings.IndexOf( const s: string ): Integer;
begin
	Lock;
	try
		Result := List.IndexOf( s );
	finally
		UnLock;
	end;
end;

procedure TKThreadStrings.Delete( Index: Integer );
begin
	Lock;
	try
		List.Delete( Index );
	finally
		UnLock;
	end;
end;

procedure TKThreadStrings.Clear;
begin
	Lock;
	try
		List.Clear;
	finally
		UnLock;
	end;
end;

function TKThreadStrings.Lock: TStrings;
begin
	Result := TStrings( inherited Lock );
end;

{ TKThreadMsgDispatcher }

type

	PKThreadMsgRec = ^TKThreadMsgRec;
	TKThreadMsgRec = record
		DestThreadID: Cardinal;
		SenderThread: TKCustomThread;
		LParam: LongInt;
		WParam: LongInt;
	end;

constructor TKThreadMsgDispatcher.Create;
begin
	inherited Create;
	FWaitToProcessMessages := True;
	FEvent := TKSimpleEvent.CreateSimple;
	FLock := TKCriticalSection.Create( False );
	FPostThreadMsgHandle := 0;
	FOldOnMessage := Application.OnMessage;
	Application.OnMessage := AppOnMessageEvent;
end;

destructor TKThreadMsgDispatcher.Destroy;
begin
	if WaitToProcessMessages then
		FEvent.Lock( Cardinal( INFINITE ) );
	FEvent.Free;
	FLock.Free;
	Application.OnMessage := FOldOnMessage; 
	inherited Destroy;
end;

procedure TKThreadMsgDispatcher.AppOnMessageEvent( var Msg: TMsg; var Handled: Boolean );
var
	ptmr: PKThreadMsgRec;
begin
	Handled := ( Msg.Message = KM_THREAD_MESSAGE_DISPATCHER );
	if Handled then
	begin
	  ptmr := PKThreadMsgRec( Msg.wParam );
		try
			Handled := DoThreadMessage( ptmr^.DestThreadID, ptmr^.SenderThread, Msg.lParam,
				ptmr^.WParam, ptmr^.LParam );
		finally
			Dispose( ptmr );
			if ( InterlockedDecrement( FPostThreadMsgHandle ) = 0 ) then
				FEvent.SetEvent;
			Msg.wParam := Integer( nil );
		end;
	end
	else if Assigned( FOldOnMessage ) then
		FOldOnMessage( Msg, Handled );
end;

function TKThreadMsgDispatcher.DoThreadMessage( DestThreadID: Cardinal;
	SenderThread: TKCustomThread; ThrHandle, WParam, LParam: LongInt ): Boolean;
begin
	Result := Assigned( FOnThreadMessage );
	if Result then
		FOnThreadMessage( Self, DestThreadID, SenderThread, ThrHandle, LParam, WParam, Result );
end;

procedure TKThreadMsgDispatcher.SetWaitToProcessMessages( Value: Boolean );
begin
	FLock.Lock;
	try
		FWaitToProcessMessages := Value;
	finally
		FLock.UnLock;
	end;
end;

function TKThreadMsgDispatcher.PostThreadMessage( DestThreadID: Cardinal;
	SenderThread: TKCustomThread; WParam, LParam: LongInt ): LongInt;
var
  ptmr: PKThreadMsgRec;
begin
	Result := -1;
	if CheckHandle( DestThreadID ) then
	begin
		ptmr := New( PKThreadMsgRec );
		try
			ptmr^.DestThreadID := DestThreadID;
			ptmr^.SenderThread := SenderThread;
			ptmr^.WParam := WParam;
			ptmr^.LParam := LParam;
			if Windows.PostThreadMessage( DestThreadID, KM_THREAD_MESSAGE_DISPATCHER, LongInt( ptmr ), FPostThreadMsgHandle ) then
				Result := InterlockedIncrement( FPostThreadMsgHandle )
			else
				Result := -1;
		finally
			if ( Result = -1 ) then
				Dispose( ptmr );
		end;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------- Generic Library Objects -----------------------------
--------------------------------------------------------------------------------
}

constructor TKCustomLibraryMapping.Create;
begin
	inherited Create;
	FLibraryName := '';
	FhLibrary := NULL_HANDLE_VALUE;
end;

destructor TKCustomLibraryMapping.Destroy;
begin
	FreeHandle;
	inherited Destroy;
end;

function TKCustomLibraryMapping.CheckLibrary: Boolean;
begin
	Result := CheckWin32Library( FhLibrary );
end;

procedure TKCustomLibraryMapping.SetLibraryName( const LibraryName: string );
begin
	if CheckLibrary then
		RaiseExceptionFmt( EKCustomLibraryMapping, sErrLibraryLoaded, [FLibraryName] );
	if CheckTrimStr( LibraryName ) then
		FLibraryName := LibraryName
	else
		RaiseException( EKCustomLibraryMapping, sErrLibraryNoName );
end;

function TKCustomLibraryMapping.GethLibrary: HModule;
begin
{ Before emitting a HandleNeeded, check for a module with the given name
	currently mappend on the process' memory space }
	if ( not FFree ) then
	begin
		FFree := false;
		FhLibrary := GetModuleHandle( PChar( FLibraryName ) );
{ The module is not mapped yet, therefore, it is time to load it }
		if ( not CheckLibrary ) then
			HandleNeeded;
	end;
	Result := FhLibrary;
end;

procedure TKCustomLibraryMapping.FreeHandle;
begin
	if ( FFree and CheckLibrary ) then
	begin
		FreeLibrary( FhLibrary );
    FhLibrary := HINSTANCE_ERROR;
	end;
end;

procedure TKCustomLibraryMapping.HandleNeeded;
begin
	if ( not CheckLibrary ) then
	begin
		FFree := true;
		FhLibrary := LoadLibrary( PChar( FLibraryName ) );
		if ( not CheckLibrary ) then
		begin
			FFree := false;
			RaiseExceptionFmt( EKCustomLibraryMapping, sErrLibraryLoad, [FLibraryName] );
		end;
	end;
end;

function TKCustomLibraryMapping.CheckProc( const Proc ): Boolean;
begin
	Result := CheckReference( Proc );
end;

function TKCustomLibraryMapping.InternalLoadProc( const ProcName: Shortstring; var Proc ): Pointer;
begin
	if ( not CheckProc( Proc ) ) then
	begin
		Pointer( Proc ) := GetProcAddress( hLibrary, PChar( string( ProcName ) ) );
		if ( not CheckProc( Proc ) ) then
			RaiseExceptionFmt( EKCustomLibraryMapping, sErrLibraryLoadProc, [FLibraryName,
			  ProcName] );
	end;
	Result := Pointer( Proc );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic GUI Objects -------------------------------
--------------------------------------------------------------------------------
}

{----------------------------------- TKPoint ----------------------------------}

constructor TKPoint.Create( pp: PPoint );
begin
	inherited Create;
  if ( not CheckReference( pp ) ) then
    SetInvalid
  else
  begin
    FX := pp^.x;
    FY := pp^.y;
  end;
end;

function TKPoint.GetInvalid: Boolean;
begin
	Result := ( FY <= INVALID_POSITION ) and ( FX <= INVALID_POSITION );
end;

function TKPoint.GetPointEx: TPoint;
begin
  GetPoint( @Result );
end;

procedure TKPoint.SetPointEx( const Value: TPoint );
begin
  SetPoint( @Value );
end;

procedure TKPoint.SetInvalid;
begin
	FY := INVALID_POSITION;
	FX := INVALID_POSITION;
end;

procedure TKPoint.Assign( Source: TPersistent );
begin
	if ( Source is TKPoint ) then
	begin
		FY := TKPoint( Source ).cY;
		FX := TKPoint( Source ).cX;
	end
	else
		inherited Assign( Source );
end;

procedure TKPoint.GetPoint( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		pp^.Y := FY;
		pp^.X := FX;
	end;
end;

procedure TKPoint.SetPoint( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		FY := pp^.Y;
		FX := pp^.X;
	end;
end;

{----------------------------------- TKRect -----------------------------------}

constructor TKRect.Create( pr: PRect );
begin
	inherited Create;
	if ( not CheckReference( pr ) ) then
		SetInvalid
  else
  begin
    FTop := pr^.Top;
    FLeft := pr^.Left;
		FRight := pr^.Right;
		FBottom := pr^.Bottom;
  end;
end;

function TKRect.GetInvalid: Boolean;
begin
	Result := ( FTop <= INVALID_POSITION ) and ( FLeft <= INVALID_POSITION ) and
						( FRight <= INVALID_POSITION ) and ( FBottom <= INVALID_POSITION );
end;

function TKRect.GetHeightWidth( Index: Integer ): Integer;
begin
  case Index of
    0: Result := FBottom - FTop;
    1: Result := FRight - FLeft;
    else
			Result := INVALID_POSITION;
  end;
end;

procedure TKRect.SetHeightWidth( Index, Value: Integer );
begin
  case Index of
    0: FBottom := FTop + Value;
		1: FRight := FLeft + Value;
	end;
end;

procedure TKRect.SetInvalid;
begin
	FTop := INVALID_POSITION;
	FLeft := INVALID_POSITION;
	FRight := INVALID_POSITION;
	FBottom := INVALID_POSITION;
end;

procedure TKRect.Assign( Source: TPersistent );
begin
	if ( Source is TKRect ) then
	begin
		FTop := TKRect( Source ).Top;
		FLeft := TKRect( Source ).Left;
		FRight := TKRect( Source ).Right;
		FBottom := TKRect( Source ).Bottom;
	end
	else
		inherited Assign( Source );
end;

procedure TKRect.GetRect( pr: PRect );
begin
	if CheckReference( pr ) then
	begin
		GetTopLeft( @pr^.TopLeft );
		GetBottomRight( @pr^.BottomRight );
	end;
end;

procedure TKRect.SetRect( pr: PRect );
begin
	if CheckReference( pr ) then
	begin
		SetTopLeft( @pr^.TopLeft );
		SetBottomRight( @pr^.BottomRight );
	end;
end;

procedure TKRect.GetTopLeft( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		pp^.Y := FTop;
		pp^.X := FLeft;
	end;
end;

procedure TKRect.SetTopLeft( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		FTop := pp^.Y;
		FLeft := pp^.X;
	end;
end;

procedure TKRect.GetBottomRight( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		pp^.Y := FBottom;
		pp^.X := FRight;
	end;
end;

procedure TKRect.SetBottomRight( pp: PPoint );
begin
	if CheckReference( pp ) then
	begin
		FBottom := pp^.Y;
		FRight := pp^.X;
	end;
end;

function TKRect.GetRectEx: TRect;
begin
  GetRect( @Result );
end;

procedure TKRect.SetRectEx( const Value: TRect );
begin
	SetRect( @Value );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Window Support ----------------------------
--------------------------------------------------------------------------------
}

function GetSystemCaptionFont( AHandle: HWnd; AFont: TFont ): Boolean;
var
	lf: TLogFont;
	fs: TFontStyles;
	wi: TKWindowInfo;
	ncm: TNonClientMetrics;
begin
	if ( not CheckObject( AFont ) or ( not CheckWindow( AHandle ) ) ) then 
	begin
		Result := false;
		Exit;
	end;
	wi := TKWindowInfo.Create( AHandle );
	try
		ZeroMemory( @lf, SizeOf( TLogFont ) );
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
		begin
			if ( wi.BorderStyle in [bsToolWindow, bsSizeToolWin] ) then
				lf := ncm.lfSmCaptionFont
			else
				lf := ncm.lfCaptionFont;
			with AFont, lf do
			begin
				Name := lfFaceName;
				Height := lfHeight;
				case ( lfPitchAndFamily and $F ) of
					FIXED_PITCH: Pitch := fpFixed;
					VARIABLE_PITCH: Pitch := fpVariable;
				else
					Pitch := fpDefault;
				end;
				fs := [];
				if ( lfWeight >= FW_BOLD ) then
					Include( fs, fsBold );
				if ( lfItalic >= 1 ) then
					Include( fs, fsItalic );
				if ( lfUnderline = 1 ) then
					Include( fs, fsUnderline );
				if ( lfStrikeOut = 1 ) then
					Include( fs, fsStrikeOut );
				Style := fs;
				CharSet := TFontCharSet( lfCharSet );
			end;
		end;
	finally
		wi.Free;
	end;
	Result := true;
end;

type
	PKClientInfo = ^TKClientInfo;
	TKClientInfo = record
		Handle: HWnd;
		ClassName: array[0..255] of char;
	end;

const
  MDI_CLIENT_WND_CLASSNAME = 'MDICLIENT';	

function EnumChildrenCallback( Handle: HWnd; LParam: LongInt ): Boolean; stdcall;
var
	ci: TKClientInfo;
begin
{ Since callback does not honor the proper behaviour of quitting
	when false is returned, we'll short circuit the callback }
	ci := PKClientInfo( LParam )^;
	if CheckStrEqual( ci.ClassName, MDI_CLIENT_WND_CLASSNAME ) then
		Result := false
	else
	begin
		ZeroMemory( PChar( LParam ), SizeOf( ShortString ) );
		Result := ( ( GetClassName( Handle, PChar( LParam ), SizeOf( ShortString ) - 1 ) > 0 ) and
   							( not CheckStrEqual( PChar( LParam ), MDI_CLIENT_WND_CLASSNAME ) ) );
		if ( not Result ) then
			ci.Handle := Handle;
	end;
end;

{-------------------------------- TKWindowInfo ---------------------------------}

constructor TKWindowInfo.Create( AHandle: HWnd );
begin
	ForceWindow( AHandle );
	inherited Create;
	FHandle := AHandle;
	FFont := nil;
end;

destructor TKWindowInfo.Destroy;
begin
  FreeClean( FFont );
	inherited Destroy;
end;

function TKWindowInfo.HasMDIClientHandle: Boolean;
begin
	Result := CheckHandle( ClientHandle );
end;

function TKWindowInfo.GetClientHandle: HWnd;
var
	ci: TKClientInfo;
begin
	Result := 0;
	ZeroMemory( @ci, SizeOf( TKClientInfo ) );
	EnumChildWindows( Handle, @EnumChildrenCallback, Integer( @ci ) );
	if CheckStrEqual( ci.ClassName, MDI_CLIENT_WND_CLASSNAME ) then
		Result := ci.Handle;
end;

function TKWindowInfo.GetActive: Boolean;
begin
	Result := ( GetActiveWindow = Handle );
end;

function TKWindowInfo.GetEnabled: Boolean;
begin
	Result := IsWindowEnabled( Handle );
end;

function TKWindowInfo.GetAveCharSize: TPoint;
var
	DC: HDC;
begin
	DC := GetWindowDC( Handle );
	try
		Result := uksyUtils.GetAveCharSizeDC( DC );
	finally
		ReleaseDC( Handle, DC );
	end;
end;

function TKWindowInfo.GetBorderStyle: TFormBorderStyle;
{ Returns the border style (as defined in forms) of the current window }
var
	ws,
	ews: LongInt;
	bDialog,
	bCaption,
	bSizeable,
	bToolWindow: Boolean;
begin
	ws := GetWindowLong( Handle, GWL_STYLE );
	ews := GetWindowLong( Handle, GWL_EXSTYLE );

	bCaption := ( ( ws and WS_CAPTION ) <> 0 );
	bSizeable := ( ( ws and WS_THICKFRAME ) <> 0 );
	bToolWindow := ( ( ews and WS_EX_TOOLWINDOW ) <> 0 );
	bDialog := ( ( ( ws and WS_DLGFRAME ) <> 0 ) and
							 ( ( ws and DS_MODALFRAME ) <> 0 ) ) or
						 ( ( ( ews and WS_EX_DLGMODALFRAME ) <> 0 ) and
							 ( ( ews and WS_EX_WINDOWEDGE ) <> 0 ) );

	if bToolWindow then
	begin
		if bSizeable then
			Result := bsSizeToolWin
		else
			Result := bsToolWindow;
	end
	else if bCaption then
	begin
		if bDialog then
			Result := bsDialog
		else if bSizeable then
			Result := bsSizeable
		else
			Result := bsSingle;
	end
	else
		Result := bsNone;
end;

function TKWindowInfo.GetButtonWidth: Integer;
{ Returns the button width of the current window }
var
	bs: TFormBorderStyle;
begin
	bs := BorderStyle;
	if ( bs in [bsToolWindow, bsSizeToolWin] ) then
		Result := GetSystemMetrics( SM_CXSMSIZE )
	else
		Result := GetSystemMetrics( SM_CXSMICON );
end;

function TKWindowInfo.GetCaptionHeight: Integer;
{ Returns the caption height of the current window }
var
	bs: TFormBorderStyle;
begin
	bs := BorderStyle;
	if ( bs = bsNone ) then
		Result := 0
	else if ( bs in [bsToolWindow, bsSizeToolWin] ) then
		Result := GetSystemMetrics( SM_CYSMCAPTION )
	else
		Result := GetSystemMetrics( SM_CYCAPTION );
end;

function TKWindowInfo.GetCaptionRect: TRect;
{ Returns the screen-base rectangle containing the window caption }
var
	bs: TFormBorderStyle;
begin
	bs := BorderStyle;
	if ( bs = bsNone ) then
		SetRectEmpty( Result )
	else
	begin
		Result := WindowRect;
		OffsetRect( Result, -Result.Left, -Result.Top );
		InflateRect( Result, -FrameWidth, -FrameHeight );
		Result.Bottom := Result.Top + CaptionHeight - 1;
	end;
end;

function TKWindowInfo.GetClientHeight: Integer;
var
	rt: TRect;
begin
	rt := WindowRect;
	if ( WindowState = wsMinimized ) then
		Result := 0
	else
	begin
		Result := rt.Bottom - rt.Top - CaptionHeight - ( 2 * FrameHeight );
		if ( Result < 0 ) then
			Result := 0;
	end;
end;

function TKWindowInfo.GetClientRect: TRect;
var
	rt: TRect;
begin
	rt := WindowRect;
	if ( WindowState = wsMinimized ) then
		SetRectEmpty( Result )
	else
		with Result do
		begin
			Left := rt.Left + FrameWidth;
			Top := rt.Top + CaptionHeight + FrameHeight;
			Right := Left + ClientWidth;
			Bottom := Top + ClientHeight;
		end;
end;

function TKWindowInfo.GetClientWidth: Integer;
var
	rt: TRect;
begin
	rt := WindowRect;
	Result := rt.Right - rt.Left - ( 2 * FrameWidth );
end;

function TKWindowInfo.GetFormStyle: TFormStyle;
var
	ews: LongInt;
begin
	ews := GetWindowLong( Handle, GWL_EXSTYLE );
	if HasMDIClientHandle then
		Result := fsMDIForm
	else if ( ( ews and WS_EX_MDICHILD ) <> 0 ) then
		Result := fsMDIChild
	else if ( ( ews and WS_EX_TOPMOST ) <> 0 ) then
		Result := fsStayOnTop
	else
		Result := fsNormal;
end;

function TKWindowInfo.GetFrameHeight: Integer;
{ Returns the Height of the current window frame }
var
	bs: TFormBorderStyle;
begin
	bs := BorderStyle;
	if ( bs in [bsSingle, bsDialog, bsToolWindow] ) then
		Result := GetSystemMetrics( SM_CYFIXEDFRAME )
	else if ( bs in [bsSizeable, bsSizeToolWin] ) then
		Result := GetSystemMetrics( SM_CYSIZEFRAME )
	else
		Result := 0;
end;

function TKWindowInfo.GetFrameWidth: Integer;
{ Returns the width of the current window frame }
var
	bs: TFormBorderStyle;
begin
	bs := BorderStyle;
	if ( bs in [bsSingle, bsDialog, bsToolWindow] ) then
		Result := GetSystemMetrics( SM_CXFIXEDFRAME )
	else if ( bs in [bsSizeable, bsSizeToolWin] ) then
		Result := GetSystemMetrics( SM_CXSIZEFRAME )
	else
		Result := 0;
end;

function TKWindowInfo.GetLeft: Integer;
begin
	Result := WindowRect.Left;
end;

function TKWindowInfo.GetHeight: Integer;
var
	rt: TRect;
begin
	rt := WindowRect;
	Result := rt.Bottom - rt.Top;
end;

function TKWindowInfo.GetSystemCaptionFont: TFont;
begin
	if ( not CheckObject( FFont ) ) then
		FFont := TFont.Create;
	Result := FFont;
	uksyClasses.GetSystemCaptionFont( Handle, FFont );
end;

function TKWindowInfo.GetTop: Integer;
begin
	Result := WindowRect.Top;
end;

function TKWindowInfo.GetVisible: Boolean;
begin
	Result := IsWindowVisible( Handle );
end;

function TKWindowInfo.GetWidth: Integer;
var
	rt: TRect;
begin
	rt := WindowRect;
	Result := rt.Right - rt.Left;
end;

function TKWindowInfo.GetWindowClassName: String;
var
	cClass: array[0..255] of char;
begin
	ZeroMemory( @cClass[0], SizeOf( cClass ) );
	SetString( Result, cClass, GetClassName( Handle, cClass, 255 ) );
end;

function TKWindowInfo.GetWindowRect: TRect;
{ Returns the screen-base rectangle containing the entire window }
begin
	Windows.GetWindowRect( Handle, Result );
end;

function TKWindowInfo.GetWindowState: TWindowState;
begin
	if IsZoomed( Handle ) then
		Result := wsMaximized                 
	else if IsIconic( Handle ) then
		Result := wsMinimized
	else
		Result := wsNormal;
end;

function TKWindowInfo.GetWndProc: Pointer;
begin
	Result := Pointer( GetWindowLong( Handle, GWL_WNDPROC ) );
end;

{------------------------------- TKWindowTrack ---------------------------------}

constructor TKWindowTrack.Create( AOwner: TKWindowManager; AStyle: TTrackStyle );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
	FTrackStyle := AStyle;
	FWidth := Owner.Width;
	FHeight := Owner.Height;
	FIgnore := true;
end;

function TKWindowTrack.GetDeskTopWorkArea: TRect;
begin
	Result := uksyUtils.GetDesktopWorkArea;
end;

procedure TKWindowTrack.SetHeight( Value: Integer );
var
  rt: TRect;
begin
	if ( FIgnore ) or ( Value = FHeight ) then
		Exit;
	if ( FTrackStyle = tsMinTrack ) then
		FHeight := Min( Value, FOwner.Height )
	else if ( FTrackStyle = tsMaxTrack ) then
	begin
		FHeight := Max( Value, FOwner.Height );
		rt := DeskTopWorkArea;
		if ( FHeight > rt.Bottom - rt.Top ) then
			FHeight := rt.Bottom - rt.Top;
	end
	else
		FHeight := Value;
end;

procedure TKWindowTrack.SetWidth( Value: Integer );
var
  rt: TRect;
begin
	if ( FIgnore ) or ( Value = FWidth ) then
		Exit;
	if ( FTrackStyle = tsMinTrack ) then
		FWidth := Min( Value, FOwner.Width )
	else if ( FTrackStyle = tsMaxTrack ) then
	begin
		FWidth := Max( Value, FOwner.Width );
		rt := DeskTopWorkArea;
		if ( FWidth > rt.Right - rt.Left ) then
			FWidth := rt.Right - rt.Left;
	end
	else
		FWidth := Value;
end;

{------------------------------- TKWindowManager -------------------------------}

constructor TKWindowManager.Create( AHandle: HWnd );
begin
	inherited Create( AHandle );
	FSizeable := true;
	FMoveable := true;
	FMaxTrack := TKWindowTrack.Create( Self, tsMaxTrack );
	FMinTrack := TKWindowTrack.Create( Self, tsMinTrack );
	FNewProc := MakeObjectInstance( NewWndProc );
	FOldProc := Pointer( SetWindowLong( Handle, GWL_WNDPROC, LongInt( FNewProc ) ) );
end;

destructor TKWindowManager.Destroy;
begin
	FMaxTrack.Free;
	FMinTrack.Free;
	SetWindowLong( Handle, GWL_WNDPROC, Integer( FOldProc ) );
	FreeObjectInstance( FNewProc );
	inherited Destroy;
end;

function TKWindowManager.GetHeight: Integer;
begin
	Result := inherited Height;
end;

procedure TKWindowManager.CenterWindow;
begin
	uksyUtils.CenterWindow( Handle );
end;

procedure TKWindowManager.SetHeight( Value: Integer );
begin
	SetWindowPos( Handle, 0, 0, 0, Width, Value, SWP_NOMOVE or SWP_NOACTIVATE or
	  SWP_NOZORDER );
end;

procedure TKWindowManager.NewWndProc( var Message: TMessage );

	procedure OldWndProc;
	begin
		with Message do
			Result := CallWindowProc( FOldProc, Handle, Msg, WParam, LParam );
	end;

begin
	case Message.Msg of

		WM_GETMINMAXINFO:
		begin
			with TWMGetMinMaxInfo( Message ), MinMaxInfo^ do
{ Lock min and max sizes to current size }
				if ( not FSizeable ) then
				begin
					with ptMaxTrackSize do
					begin
						X := Width;
						Y := Height;
					end;
					with ptMinTrackSize do
					begin
						X := Width;
						Y := Height;
					end;
				end
				else
				begin
{ sets min window }
					if ( not FMinTrack.Ignore ) then
						with ptMinTrackSize do
						begin
							X := FMinTrack.Width;
							Y := FMinTrack.Height;
						end;
{ sets max window }
					if ( not FMaxTrack.Ignore ) then
						with ptMaxTrackSize do
						begin
							X := FMaxTrack.Width;
							Y := FMaxTrack.Height;
						end;
				end;
      Message.Result := 0;
		end; { WM_GETMINMAXINFO }

	else
		OldWndProc;
		
	end; { case }

end;

{
--------------------------------------------------------------------------------
-------------------------- Generic Gradient Support ----------------------------
--------------------------------------------------------------------------------
}

{ TKEffect }


constructor TKEffect.Create( AControl: TControl );
begin
  ForceObject( AControl ); 
	inherited Create;
	FOwner := AControl;
	FDepth := 1;
	FColor := clSilver;
	FDirection := edUpLeft;
end;

procedure TKEffect.SetDepth( Value: TKDepth );
begin
	if ( FDepth <> Value ) then
	begin
		FDepth := Value;
		Owner.Invalidate;
	end;
end;

procedure TKEffect.SetColor( Value: TColor );
begin
	if ( FColor <> Value ) then
	begin
		FColor := Value;
		Owner.Invalidate;
	end;
end;

procedure TKEffect.SetDirection( Value: TKEffectDirection );
begin
	if ( FDirection <> Value ) then
	begin
		FDirection := Value;
		Owner.Invalidate;
	end;
end;

{ TKGradient }

constructor TKGradient.Create( AControl: TControl; ABitmap: TBitmap );
begin
	inherited Create;
	FSteps := High( TKSteps );
	FBitmap := nil;
	FOwner := AControl;
	FEndColor := clNavy;
	FBeginColor := clAqua;
	FGradientChanged := true;
	FFirstPaint := true;
	FExternalGradient := false;
	if ( not CheckObject( ABitmap ) ) then
		FGradientBmp := TBitmap.Create
	else
	begin
		FGradientBmp := ABitmap;
		FExternalGradient := true;
	end;
	FGradientStyle := gsVertical;
end;

destructor TKGradient.Destroy;
begin
	if ( not FExternalGradient ) then
		FGradientBmp.Free;
	FGradientBmp := nil;
	FreeClean( FBitMap );
	inherited Destroy;
end;

procedure TKGradient.Paint;
begin
	PaintGradient( Self );
end;

procedure TKGradient.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKGradient ) then
	begin
		Steps := TKGradient( Source ).Steps;
		Bitmap := TKGradient( Source ).Bitmap;
		EndColor := TKGradient( Source ).EndColor;
		BeginColor := TKGradient( Source ).BeginColor;
		GradientStyle := TKGradient( Source ).GradientStyle;
	end
	else if CheckObjectClass( Source, TGraphic ) then
		Bitmap.Assign( Source ) { will create the bitmap if necessary }
	else
		inherited Assign( Source );
end;

procedure TKGradient.AssignFromGradData( GradientData: TKGradientData );
begin
	BeginColor := GradientData.BeginColor;
	EndColor := GradientData.EndColor;
	GradientStyle := GradientData.GradientStyle;
	Steps := GradientData.Steps;
	if CheckObject( GradientData.BitmapObj ) then
		Bitmap := GradientData.BitmapObj
	else if CheckFile( GradientData.BitmapFile ) then
		Bitmap.LoadFromFile( GradientData.BitmapFile ); { see get/set }
end;

procedure TKGradient.AssignToGradData( var GradientData: TKGradientData );
begin
	ZeroMemory( @GradientData, SizeOf( TKGradientData ) );
	GradientData.BeginColor := BeginColor;
	GradientData.EndColor := EndColor;
	GradientData.GradientStyle := GradientStyle;
	GradientData.Steps := Steps;
	if CheckObject( FBitMap ) then { do not create if it does not exists }
  	GradientData.BitmapObj := Bitmap;
end;

procedure TKGradient.GradientChange( Sender: TObject );
begin
{ do whatever you'ld like in descendant classes }
end;

procedure TKGradient.Invalidate;
begin
	Paint;
end;

procedure TKGradient.Changed( ForceRepaintOwner: Boolean );
begin
	FGradientChanged := true;
	Invalidate;
	if Assigned( FOnGradientChange ) then
	begin
		FOnGradientChange( Self );
		if ForceRepaintOwner then
			Owner.Invalidate;
	end
	else if CheckObject( Owner ) then
		if ( ( ForceRepaintOwner ) or
				 ( not FExternalGradient ) and
				 ( ( not CheckObjectClass( Owner, TCustomForm ) ) or
					 ( csDesigning in Owner.ComponentState ) ) ) then
			Owner.Invalidate;
end;

procedure TKGradient.SetOwner( AControl: TControl );
begin
	FOwner := AControl;
end;

procedure TKGradient.BitmapChange( Sender: TObject );
begin
	if ( FGradientStyle = gsBitmapTiled ) or ( FGradientStyle = gsBitmapStretched ) then
	begin
		if TBitmap( Sender ).Empty then
			FGradientStyle := gsNone;
		Changed( true );
	end;
end;

function TKGradient.GetBitmap: TBitmap;
begin
	if ( not CheckObject( FBitmap ) )
		 {( Owner <> nil ) and( csLoading in Owner.ComponentState )} then
	begin
		FBitmap := TBitmap.Create;
		FBitMap.OnChange := BitmapChange;
	end;
	Result := FBitmap;
end;

procedure TKGradient.SetSteps( Value: TKSteps );
begin
	if ( FSteps <> Value ) then
	begin
		FSteps := Value;
		Changed( true );
	end;
end;

procedure TKGradient.SetBitmap( Value: TBitmap );
begin
	if ( not CheckObject( Value ) ) then
	begin
		FreeClean( FBitMap );
		if ( FGradientStyle = gsBitmapTiled ) or
			 ( FGradientStyle = gsBitmapStretched ) then
		begin
			FGradientStyle := gsNone;
			Changed( true );
		end
	end
	else if ( FBitmap <> Value ) then
		Bitmap.Assign( Value ); { will call get to create if necessary }
end;

procedure TKGradient.SetEndColor( Value: TColor );
begin
	if ( FEndColor <> Value ) then
	begin
		FEndColor := Value;
		if ( FGradientStyle <> gsNone ) and
			 ( FGradientStyle <> gsBitmapTiled ) and
			 ( FGradientStyle <> gsBitmapStretched ) then
			Changed( true );
	end;
end;

procedure TKGradient.SetBeginColor( Value: TColor );
begin
	if ( FBeginColor <> Value ) then
	begin
		FBeginColor := Value;
		if ( FGradientStyle <> gsNone ) and
			 ( FGradientStyle <> gsBitmapTiled ) and
			 ( FGradientStyle <> gsBitmapStretched ) then
			Changed( true );
	end;
end;

procedure TKGradient.SetGradientStyle( Value: TKGradientStyle );
begin
	if ( FGradientStyle <> Value ) then
	begin
		if ( ( Value in [gsBitmapTiled, gsBitmapStretched] ) and
			   ( ( not CheckObject( FBitmap ) ) or FBitmap.Empty ) ) then
			RaiseException( EKGradient, sErrInvGradieBmp );
		FGradientStyle := Value;
		Changed( true );
	end;
end;

function TKGradient.BeginR: TColor;
begin
	Result := GetRValue( ColorToRGB( FBeginColor ) );
end;

function TKGradient.BeginG: TColor;
begin
	Result := GetGValue( ColorToRGB( FBeginColor ) );
end;

function TKGradient.BeginB: TColor;
begin
	Result := GetBValue( ColorToRGB( FBeginColor ) );
end;

function TKGradient.DeltaR: TColor;
begin
	Result := GetRValue( FEndColor ) - BeginR;
end;

function TKGradient.DeltaG: TColor;
begin
	Result := GetGValue( FEndColor ) - BeginG;
end;

function TKGradient.DeltaB: TColor;
begin
	Result := GetBValue( FEndColor ) - BeginB;
end;

{-------------------------- Generic Gradient Routines --------------------------}

{--------------------------- Internal Implementation ---------------------------}

procedure DoVertical( bmp: TBitmap; grad: TKGradient );
var
	rt: TRect;
	i: Integer;
	R, G, B: Byte;
begin
	rt.Left := 0;
	rt.Right := bmp.Width;
	with grad, bmp do
		for i := 0 to Steps do
		begin
			rt.Top := MulDiv( i, bmp.Height, Steps + 1 );
			rt.Bottom := MulDiv( i + 1, bmp.Height, Steps + 1 );
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
		end;
end;

procedure DoHorizontal( bmp: TBitmap; grad: TKGradient );
var
	rt: TRect;
	i: Integer;
	R, G, B: Byte;
begin
	rt.Top := 0;
	rt.Bottom := bmp.Height;
	with grad, bmp do
		for i := 0 to Steps do
		begin
			rt.Left := MulDiv( i, bmp.Width, Steps + 1 );
			rt.Right := MulDiv( i + 1, bmp.Width, Steps + 1 );
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
		end;
end;

function Walk( ID: Integer; bmp: TBitmap; var PT_O, PT_D: TPoint ): Boolean;
begin
	Result := false;
{ first bisector }
	if ( ID = 1 ) then
	begin
{ lower left turn? }
		if ( PT_O.X = -1 ) and ( PT_O.Y = bmp.Height ) then
		begin
			PT_O.X := 0;
			Result := true;
		end
		else if ( PT_O.X = -1 ) then
			PT_O.Y := PT_O.Y + 1
		else if ( PT_O.X < bmp.Width ) then
			PT_O.X := PT_O.X + 1;
{ top right turn? }
		if ( PT_D.X = bmp.Width ) and ( PT_D.Y = -1 ) then
		begin
			PT_D.Y := 0;
			Result := true;
		end
		else if ( PT_D.Y = -1 ) then
			PT_D.X := PT_D.X + 1
		else if ( PT_D.Y < bmp.Height ) then
			PT_D.Y := PT_D.Y + 1;
	end
{ second bisector }
	else if ( ID = -1 ) then
	begin
{ bottom right turn? }
		if ( PT_O.X = bmp.Width ) and ( PT_O.Y = bmp.Height ) then
		begin
			PT_O.X := PT_O.X - 1;
			Result := true;
		end
		else if ( PT_O.X = bmp.Width ) then
			PT_O.Y := PT_O.Y + 1
		else if ( PT_O.X > -1 ) then
			PT_O.X := PT_O.X - 1;
{ top left turn? }
		if ( PT_D.X = -1 ) and ( PT_D.Y = -1 ) then
		begin
			PT_D.Y := 0;
			Result := true;
		end
		else if ( PT_D.Y = -1 ) then
			PT_D.X := PT_D.X - 1
		else if ( PT_D.Y < bmp.Height ) then
			PT_D.Y := PT_D.Y + 1;
	end;
end;

procedure DoBisector( IsFirst: Boolean; bmp: TBitmap; grad: TKGradient );
var
	i,
	j,
	iOrphanLines,
	iLinesPerStep: Integer;
	R, G, B: Byte;
	pt_oi,
	pt_of,
	pt_di,
	pt_df: TPoint;
	bTurned: Boolean;
	pts: array[1..4] of TPoint;
	pts_corner: array[1..5] of TPoint;
begin
	if ( bmp.Width = 0 ) or ( bmp.Height = 0 ) then
		Exit;
	if IsFirst then
	begin
		pt_oi.X := -1;
		pt_oi.Y := -1;
		pt_of.X := -1;
		pt_of.Y := -1;
		pt_di.X := -1;
		pt_di.Y := -1;
		pt_df.X := -1;
		pt_df.Y := -1;
	end
	else
	begin
		pt_oi.X := bmp.Width;
		pt_oi.Y := -1;
		pt_of.X := bmp.Width;
		pt_of.Y := -1;
		pt_di.X := bmp.Width;
		pt_di.Y := -1;
		pt_df.X := bmp.Width;
		pt_df.Y := -1;
	end;
	with grad, bmp do
	begin
		i := 0;
		pts[1] := pt_oi;
		pts[2] := pt_di;
		pts[3] := pt_df;
		pts[4] := pt_of;
		Canvas.Pen.Style := psClear;
		iOrphanLines := ( Width + Height + 4 ) mod Steps;
		iLinesPerStep := ( Width + Height + 4 ) div Steps;
		while ( i <= Steps ) do
		begin
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			bTurned := false;
			for j := 0 to iLinesPerStep - 1 + Ord( iOrphanLines > 0 ) do
				bTurned := Walk( -1 + ( 2 * Ord( IsFirst ) ), bmp, pt_of, pt_df ) or bTurned;
			pts[3] := pt_df;
			pts[4] := pt_of;
			if bTurned then
			begin
				pts_corner[1] := pts[1];
				pts_corner[2] := pts[2];
				if IsFirst then
				begin
{ lower left turn }
					if ( pts[1].X = -1 ) and ( pts[4].X > -1 ) then
					begin
						pts_corner[3] := pts[3];
						pts_corner[4] := pts[4];
						pts_corner[5] := Point( -1, bmp.Height );
					end
					else if ( pts[2].Y = -1 ) and ( pts[3].Y > -1 ) then
{ top right turn }
					begin
						pts_corner[3] := Point( bmp.Width, -1 );
						pts_corner[4] := pts[3];
						pts_corner[5] := pts[4];
					end;
				end
				else
				begin
{ lower right turn }
					if ( pts[1].X = bmp.Width ) and ( pts[4].X < bmp.Width ) then
					begin
						pts_corner[3] := pts[3];
						pts_corner[4] := pts[4];
						pts_corner[5] := Point( bmp.Width, bmp.Height );
					end
{ top left turn }
					else if ( pts[2].Y = -1 ) and ( pts[3].Y > -1 ) then
					begin
						pts_corner[3] := Point( -1, -1 );
						pts_corner[4] := pts[3];
						pts_corner[5] := pts[4];
					end;
				end;
				Canvas.Polygon( pts_corner );
			end
			else
				Canvas.Polygon( pts );
			Inc( i );
			Dec( iOrphanLines );
{ o último polígono pode estar com as coordenadas *&^%$#@! }
			pts[1] := pts[4];
			pts[2] := pts[3];
		end;
	end;
end;

procedure DoSecondBisector( bmp: TBitmap; grad: TKGradient );
begin
	DoBisector( false, bmp, grad );
end;

procedure DoFirstBisector( bmp: TBitmap; grad: TKGradient );
begin
	DoBisector( true, bmp, grad );
end;

procedure DoCircular( bmp: TBitmap; grad: TKGradient );
var
	i: Integer;
	R, G, B: Byte;
	iR, iX, iY, Radius: Integer;
begin
	bmp.Canvas.Pen.Style := psClear;
	iX := ( bmp.Width div 2 ) + ( bmp.Width mod 2 );
	iY := ( bmp.Height div 2 ) + ( bmp.Height mod 2 );
	Radius := Trunc( Sqrt( iX * iX + iY * iY ) );
	with grad do
		for i := 0 to Steps do
		begin
			iR := MulDiv( Radius, Steps - i, Steps );
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			bmp.Canvas.Brush.Color := RGB( R, G, B );
			bmp.Canvas.Ellipse( iX - iR, iY - iR, iX + iR, iY + iR );
		end;
end;

procedure DoBitmapTiled( bmp: TBitmap; grad: TKGradient );
var
	wRow, wCol: Word;
begin
	with grad.Bitmap do
		for wRow := 0 to ( bmp.Height div Height ) do
			for wCol := 0 to ( bmp.Width div Width ) do
				BitBlt( bmp.Canvas.Handle, wCol * Width, wRow * Height,
								Width, Height, Canvas.Handle, 0, 0, SRCCOPY );
end;

procedure DoBitmapStretched( bmp: TBitmap; grad: TKGradient );
begin
	with grad.Bitmap do
	begin
{ necessary to make the bitmap to paint properly and not leave unpainted raster
  lines ( image trash ) when resizing }
		SetStretchBltMode( bmp.Canvas.Handle, COLORONCOLOR );
		StretchBlt( bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height,
						Canvas.Handle, 0, 0, Width, Height, SRCCOPY );
	end;
end;

procedure DoRectangular( bmp: TBitmap; grad: TKGradient );
var
	R, G, B: Byte;
	i,
	iWidth,
	iOrphanLines,
	x1, y1, x2, y2: Integer;
begin
	with bmp, grad do
	begin
		i := 0;
		x1 := 0;
		x2 := Width;
		y1 := 0;
		y2 := Height;
		Canvas.Pen.Style := psClear;
		iWidth := Max( Height, Width ) div ( 2 * Steps );
		iOrphanLines := Max( Height, Width ) mod ( 2 * Steps );
		while ( i <= Steps ) and ( x1 <= x2 ) and ( y1 <= y2 ) do
		begin
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( Rect( x1, y1, x2, y2 ) );
			Inc( i );
			Dec( iOrphanLines );
			x1 := x1 + iWidth + Ord( iOrphanLines > 0 );
			y1 := y1 + iWidth + Ord( iOrphanLines > 0 );
			x2 := X2 - iWidth - Ord( iOrphanLines > 0 );
			y2 := y2 - iWidth - Ord( iOrphanLines > 0 );
		end;
	end;
end;

procedure DoDoubleVertical( bmp: TBitmap; grad: TKGradient );
var
	rt: TRect;
	R, G, B: Byte;
	i, Ref: Integer;
begin
	rt.Left := 0;
	rt.Right := bmp.Width;
	Ref := ( bmp.Height div 2 ) + ( bmp.Height mod 2 );
	with grad, bmp do
		for i := 0 to Steps do
		begin
			rt.Top := MulDiv( i, Ref, Steps + 1 );
			rt.Bottom := MulDiv( i + 1, Ref, Steps + 1 );
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
			rt.Top := Height - MulDiv( i, Ref, Steps + 1 );
			rt.Bottom := Height - MulDiv( i + 1, Ref, Steps + 1 );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
		end;
end;

procedure DoDoubleHorizontal( bmp: TBitmap; grad: TKGradient );
var
	rt: TRect;
	R, G, B: Byte;
	i, Ref: Integer;
begin
	rt.Top := 0;
	rt.Bottom := bmp.Height;
	Ref := ( bmp.Width div 2 ) + ( bmp.Width mod 2 );
	with grad, bmp do
		for i := 0 to Steps do
		begin
			rt.Left := MulDiv( i, Ref, Steps + 1 );
			rt.Right := MulDiv( i + 1, Ref, Steps + 1 );
			R := BeginR + MulDiv( i, DeltaR, Steps );
			G := BeginG + MulDiv( i, DeltaG, Steps );
			B := BeginB + MulDiv( i, DeltaB, Steps );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
			rt.Left := Width - MulDiv( i, Ref, Steps + 1 );
			rt.Right := Width - MulDiv( i + 1, Ref, Steps + 1 );
			Canvas.Brush.Color := RGB( R, G, B );
			Canvas.FillRect( rt );
		end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure PaintGradient( grad: TKGradient );
{ short form to call ControlPaintGradient }
begin
	ControlPaintGradient( grad.Owner, grad.GradientBmp, grad );
end;

procedure PaintGradientDC( DC: HDC; ARect: PRect; grad: TKGradient );
{
	This procedure has two flavors: with or without a defined
	rectangle. If ARect is not nil, it is used as the clipping region,
	and as a means to define the size of the bitmap to be painted to
	the device context; otherwise, the current clipping region of the
	DC is used for this purpose.
}
var
	rt: TRect;
	bmp: TBitmap;
begin
	if ( not ( CheckPointer( ARect ) and CheckHandle( DC ) ) ) then
		Exit;
	rt := ARect^;
	bmp := TBitmap.Create;
	try
		with rt do
		begin
			bmp.Width := Right - Left;
			bmp.Height := Bottom - Top;
			ControlPaintGradient( nil, bmp, grad );
			BitBlt( DC, Left, Top, Right - Left, Bottom - Top,
				bmp.Canvas.Handle, 0, 0, SRCCOPY );
		end;
	finally
		bmp.Free;
	end;
end;

procedure ControlPaintGradient( AControl: TControl; bmp: TBitmap; grad: TKGradient );
{
	AControl is the object that needs to be painted: thus, it is used
	to define the dimensions of the work bitmap; if AControl is nil,
	it is assumed that bmp already has the correct dimensions; bmp is
	the work bitmap( it will be gradient-painted); grad is the object
	with the information on how to paint the gradient.
}

	function FormSizeChanged: Boolean;
	begin
		Result := CheckObjectClass( AControl, TCustomForm ) and
							( ( bmp.Width <> TCustomForm( AControl ).ClientWidth ) or
								( bmp.Height <> TCustomForm( AControl ).ClientHeight ) );
	end;

	function PanelSizeChanged: Boolean;
	begin
		Result := CheckObjectClass( AControl, TPanel ) ;
		if Result then
			with TPanel( AControl ) do
				Result := Result and
								 ( ( bmp.Width <> ( TPanel( AControl ).Width - 2 *
										 ( BevelWidth + BorderWidth ) ) ) or
									 ( bmp.Height <> ( TPanel( AControl ).Height - 2 *
										 ( BevelWidth + BorderWidth ) ) ) );
	end;

	function ControlSizeChanged: Boolean;
	begin
		Result := ( bmp.Width <> AControl.Width ) or
							( bmp.Height <> AControl.Height );
	end;

var
	bChanged: Boolean;
begin
{ if you can't paint, you can't paint; if AControl is nil, assume that
	bmp is correctly dimensioned }
	if not CheckObjects( [bmp, grad] ) then
		Exit;
{ check if it is necessary to rebuild the bitmap }
	bChanged := grad.FirstPaint or
							grad.GradientChanged or
							( not CheckObjects( [AControl, bmp] ) ) or
							( FormSizeChanged or
								PanelSizeChanged or
								ControlSizeChanged );
	if ( not bChanged ) then
		Exit;
	if CheckObject( AControl ) then
    if CheckObjectClass( AControl, TCustomForm ) then
      with TCustomForm( AControl ) do
      begin
        bmp.Width := ClientWidth;
        bmp.Height := ClientHeight;
      end
		else if CheckObjectClass( AControl, TPanel ) then
      with TPanel( AControl ) do
      begin
        bmp.Width := Width - 2 * ( BevelWidth + BorderWidth );
				bmp.Height := Height - 2 * ( BevelWidth + BorderWidth );
      end
		else
    begin
      bmp.Width := AControl.Width;
      bmp.Height := AControl.Height;
    end;
	case grad.GradientStyle of
		gsVertical: DoVertical( bmp, grad );
		gsCircular: DoCircular( bmp, grad );
		gsHorizontal: DoHorizontal( bmp, grad );
		gsBitmapTiled: DoBitmapTiled( bmp, grad );
		gsBitmapStretched: DoBitmapStretched( bmp, grad );
		gsRectangular: DoRectangular( bmp, grad );
		gsFirstBisector: DoFirstBisector( bmp, grad );
		gsSecondBisector: DoSecondBisector( bmp, grad );
		gsDoubleVertical: DoDoubleVertical( bmp, grad );
		gsDoubleHorizontal: DoDoubleHorizontal( bmp, grad );
	end;
  grad.FFirstPaint := false; { uh! hard couple! }
	grad.FGradientChanged := false; { uh! hard couple! }
end;

function EditGradient( Gradient: TKGradient ): Boolean;
begin
	if CheckPointer( @EditGradientFunc ) then
		Result := EditGradientFunc( Gradient )
	else
	  Result := False;	
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Canvas Support ----------------------------
--------------------------------------------------------------------------------
}

{ TKCustomCanvasInfo }

constructor TKCustomCanvasInfo.Create( ACanvas: TCanvas );
begin
	ForceObject( ACanvas );
	inherited Create;
{ default property values }
	FZoom := 100;
	FWidth := INVALID_POSITION;
	FHeight := INVALID_POSITION;
	FPageWidth := INVALID_POSITION;
	FPageHeight := INVALID_POSITION;
	FDeviceType := dtNone;
	FPixelsPerInchX := INVALID_POSITION;
	FPixelsPerInchY := INVALID_POSITION;
{ object association and creation }
	FCanvas := ACanvas;
	FGutter := TKRect.Create( @INVALID_RECT );
	FMargins := TKRect.Create( @INVALID_RECT );
{ Initialize the more important properties }
	GetDeviceType;
	GetDeviceLogPixelsX;
	GetDeviceLogPixelsY;
	GetDevicePageWidth;
	GetDevicePageHeight;
	GetDeviceWidth;
	GetDeviceHeight;
	GetDeviceGutter;
	GetMargins;
end;

destructor TKCustomCanvasInfo.Destroy;
begin
	FGutter.Free;
	FMargins.Free;
	inherited Destroy;
end;

function TKCustomCanvasInfo.DeviceCaps( Index: Integer ): Integer;
begin
	Result := GetDeviceCaps( FCanvas.Handle, Index );
end;

function TKCustomCanvasInfo.AdjustMargins( Value: TKRect ): TKRect;
begin
	if ( Value.Top < FGutter.Top ) then
		Value.Top := FGutter.Top;
	if ( Value.Left < FGutter.Left ) then
		Value.Left := FGutter.Left;
	if ( Value.Right < FGutter.Right ) then
		Value.Right := FGutter.Right;
	if ( Value.Bottom < FGutter.Bottom ) then
		Value.Bottom := FGutter.Bottom;
	Result := Value;
end;

function TKCustomCanvasInfo.GetDeviceGutter: TKRect;
begin
	if ( DeviceType in PRINT_DEVICES ) then
	begin
		if FGutter.Invalid then
		begin
			FGutter.Top := DeviceCaps( PHYSICALOFFSETY );
			FGutter.Left := DeviceCaps( PHYSICALOFFSETX );
			FGutter.Right := DeviceWidth - FGutter.Left - DeviceCaps( HORZRES );
			FGutter.Bottom := DeviceHeight - FGutter.Top - DeviceCaps( VERTRES );
		end;
	end
	else
		FGutter.SetInvalid;
	Result := FGutter;
end;

function TKCustomCanvasInfo.GetDeviceLogPixelsX: LongInt;
begin
	if ( FPixelsPerInchX = INVALID_POSITION ) then
		FPixelsPerInchX := DeviceCaps( LOGPIXELSX );
	Result := FPixelsPerInchX;
end;

function TKCustomCanvasInfo.GetDeviceWidth: LongInt;
begin
	if ( FWidth = INVALID_POSITION ) then
		if ( DeviceType in DISPLAY_DEVICES ) then
			FWidth := DeviceCaps( HORZRES )
		else if ( DeviceType in PRINT_DEVICES ) then
			FWidth := DeviceCaps( PHYSICALWIDTH );
	Result := FWidth;
end;

function TKCustomCanvasInfo.GetDevicePageWidth: LongInt;
begin
	if ( FPageWidth = INVALID_POSITION ) then
		FPageWidth := DeviceCaps( HORZRES );
	Result := FPageWidth;
end;

function TKCustomCanvasInfo.GetDeviceLogPixelsY: LongInt;
begin
	if ( FPixelsPerInchY = INVALID_POSITION ) then
		FPixelsPerInchY := DeviceCaps( LOGPIXELSY );
	Result := FPixelsPerInchY;
end;

function TKCustomCanvasInfo.GetDeviceHeight: LongInt;
begin
	if ( FHeight = INVALID_POSITION ) then
		if ( DeviceType in DISPLAY_DEVICES ) then
			FHeight := DeviceCaps( VERTRES )
		else if ( DeviceType in PRINT_DEVICES ) then
			FHeight := DeviceCaps( PHYSICALHEIGHT );
	Result := FHeight;
end;

function TKCustomCanvasInfo.GetDevicePageHeight: LongInt;
begin
	if ( FPageHeight = INVALID_POSITION ) then
		FPageHeight := DeviceCaps( VERTRES );
	Result := FPageHeight;
end;

function TKCustomCanvasInfo.GetDeviceType: TKDeviceType;
begin
	if ( FDeviceType = dtNone ) then
		case DeviceCaps( TECHNOLOGY ) of
				 DT_PLOTTER: FDeviceType := dtPlotter;
			DT_RASDISPLAY: FDeviceType := dtDisplay;
			DT_RASPRINTER: FDeviceType := dtPrinter;
			 DT_RASCAMERA: FDeviceType := dtCamera;
			DT_CHARSTREAM: FDeviceType := dtCharStream;
				DT_METAFILE: FDeviceType := dtMetaFile;
				DT_DISPFILE: FDeviceType := dtDispFile;
		else
			FDeviceType := dtNone;
		end;
	Result := FDeviceType;
end;

function TKCustomCanvasInfo.MMToPixels_X( MM: Single ): LongInt;
begin
	Result := Round( MM * PixelsPerInchX / INCH_TO_MM );
end;

function TKCustomCanvasInfo.MMToPixels_Y( MM: Single ): LongInt;
begin
	Result := Round( MM * PixelsPerInchY / INCH_TO_MM );
end;

function TKCustomCanvasInfo.PixelsToMM_X( Pixels: LongInt ): Single;
begin
	Result := Pixels * INCH_TO_MM / PixelsPerInchX;
end;

function TKCustomCanvasInfo.PixelsToMM_Y( Pixels: LongInt ): Single;
begin
	Result := Pixels * INCH_TO_MM / PixelsPerInchY;
end;

function TKCustomCanvasInfo.InchToPixels_X( Inches: Single ): LongInt;
begin
	Result := Round( Inches * PixelsPerInchX );
end;

function TKCustomCanvasInfo.InchToPixels_Y( Inches: Single ): LongInt;
begin
	Result := Round( Inches * PixelsPerInchY );
end;

function TKCustomCanvasInfo.PixelsToInch_X( Pixels: LongInt ): Single;
begin
	Result := Pixels / PixelsPerInchX;
end;

function TKCustomCanvasInfo.PixelsToInch_Y( Pixels: LongInt ): Single;
begin
	Result := Pixels / PixelsPerInchY;
end;

function TKCustomCanvasInfo.GetMargins: TKRect;
begin
	if ( FMargins.Invalid ) then
		FMargins.Assign( FGutter );
	Result := AdjustMargins( FMargins );
end;

procedure TKCustomCanvasInfo.SetMargins( Value: TKRect );
begin
	if ( FMargins.Invalid ) then
		AdjustMargins( FMargins );
	if CheckObject( Value ) or ( Value.Invalid ) then
	  Exit;
	FMargins.Assign( AdjustMargins( Value ) );
end;

{ TKCustomPrinterCanvasInfo }

constructor TKCustomPrinterCanvasInfo.Create( ACanvas: TCanvas );
begin
	RaiseExceptionFmt( EKCanvas, sErrPrnDCInfoInvConstructor, [ClassName] );
end;

constructor TKCustomPrinterCanvasInfo.CreateFromPrinter( APrinter: TPrinter );
begin
	ForceObject( APrinter );
	FPrinter := APrinter;
	inherited Create( FPrinter.Canvas );
end;

destructor TKCustomPrinterCanvasInfo.Destroy;
begin
	FPrinter := nil;
	inherited Destroy;
end;

function TKCustomPrinterCanvasInfo.DeviceCaps( Index: Integer ): Integer;
begin
	Result := GetDeviceCaps( FPrinter.Handle, Index );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Stream Support ----------------------------
--------------------------------------------------------------------------------
}

{ TKCustomStringStream }

constructor TKCustomStringStream.Create( const AString: string );
begin
{ just for polimorfism creation... }
	inherited Create( AString );
end;

constructor TKCustomStringStream.CreateFromFile( const Filename: TFileName );
var
	fs: TFileStream;
begin
	Create( '' );
	fs := TFileStream.Create( FileName, fmOpenRead or fmShareDenyNone );
	try
		ForceStreamCopy( fs, Self );
		Position := 0;
	finally
		fs.Free;
	end;
end;

function TKCustomStringStream.PutChar( Ch: Char ): TKCustomStringStream;
begin
	WriteBuffer( Ch, 1 );
	Result := Self;
end;

function TKCustomStringStream.PutInteger( Int: LongInt ): TKCustomStringStream;
begin
	PutString( IntToStr( Int ) );
	Result := Self;
end;

function TKCustomStringStream.PutFloat( Flt: Extended ): TKCustomStringStream;
begin
	PutString( FloatToStr( Flt ) );
	Result := Self;
end;

function TKCustomStringStream.PutString( const Str: string ): TKCustomStringStream;
begin
	if CheckTrimStr( Str ) then
		WriteBuffer( Pointer( Str )^, Length( Str ) );
	Result := Self;
end;

function TKCustomStringStream.PutLine( const Str: string ): TKCustomStringStream;
begin
	if CheckTrimStr( Str ) then
		WriteBuffer( Pointer( Str )^, Length( Str ) );
	PutEndOfLine;
	Result := Self;
end;

function TKCustomStringStream.PutPChar( const Str: PChar ): TKCustomStringStream;
begin
	if CheckPChar( Str ) then
		WriteBuffer( Str[0], StrLen( Str ) );
	Result := Self;
end;

function TKCustomStringStream.PutSpace: TKCustomStringStream;
begin
	PutChar( CH_SPACE );
	Result := Self;
end;

function TKCustomStringStream.PutTab: TKCustomStringStream;
begin
	PutChar( CH_TAB );
	Result := Self;
end;

function TKCustomStringStream.PutEndOfLine: TKCustomStringStream;
begin
	PutChar( CH_CR );
	PutChar( CH_LF );
	Result := Self;
end;

procedure TKCustomStringStream.WriteLn( const Str: string );
begin
	PutString( Str );
	PutEndOfLine;
end;

procedure TKCustomStringStream.NewLine;
begin
	PutEndOfLine;
end;

procedure TKCustomStringStream.Format( const Fmt: string; Args: array of const );
begin
	PutString( SysUtils.Format( Fmt, Args ) );
end;

procedure TKCustomStringStream.FormatLn( const Fmt: string; Args: array of const );
begin
	PutString( SysUtils.Format( Fmt, Args ) );
	PutEndOfLine;
end;

{ TKCustomMemoryStream }

constructor TKCustomMemoryStream.CreateFromMemory( AMemory: Pointer; ADataSize: LongInt );
begin
	inherited Create;
	if ( ADataSize > 0 ) then
	begin
		ForcePointer( AMemory );
		SetPointer( AMemory, ADataSize );
	end;
	Position := 0;
end;

constructor TKCustomMemoryStream.CreateFromFile( const FileName: TFileName );
begin
	inherited Create;
	LoadFromFile( FileName );
	Position := 0;
end;

constructor TKCustomMemoryStream.CreateFromString( const AString: string );
begin
	CreateFromMemory( Pointer( AString ), Length( AString ) );
end;

constructor TKCustomMemoryStream.CreateFromStringStream( ss: TStringStream );
begin
	if CheckStream( ss ) then
		CreateFromString( ss.DataString )
	else
	  inherited Create;	
end;

{ TKCustomFileStream }

type

	THandleStreamSHack = class( TStream )
	private
		FHandle: Integer;
		
	end;
	
constructor TKCustomFileStream.Create( const AFileName: string; AMode: Word );
begin
  { do not call inherited Create; }
	OpenFile( AFileName, AMode );
	Position := 0;
end;

destructor TKCustomFileStream.Destroy;
begin
	CloseFile; { do not call inherited Destroy; }
end;

function TKCustomFileStream.GetHandle: THandle;
begin
	Result := inherited Handle;
end;

procedure TKCustomFileStream.SetHandle( Value: THandle );
begin
  CloseFile;
	THandleStreamSHack( Self ).FHandle := Value;
end;

procedure TKCustomFileStream.CloseFile;
begin
	if ( Handle <> 0 ) then
	begin
		FileClose( Handle );
		Handle := INVALID_HANDLE_VALUE;
		FFileName := '';
		FMode := fmOpenRead;
	end;
end;

procedure TKCustomFileStream.OpenFile( const AFileName: string; AMode: Word );
begin
	if ( AMode = fmCreate ) then
	begin
		Handle := FileCreate( AFileName );
		// if ( Handle < 0 ) then! Generates a hint in delphi 4 because
		// THandle are declared as LongWord which is allways positive, so
		// put the equal! That is a illegal case in D3 too...
		if ( Handle <= 0 ) then
			RaiseExceptionFmt( EKCustomFileStream, SFCreateError, [AFileName] );
	end
	else
	begin
		Handle := FileOpen( AFileName, AMode );
		if ( Handle <= 0 ) then
			RaiseExceptionFmt( EKCustomFileStream, SFOpenError, [AFileName] );
	end;
	FFileName := AFileName;
	FMode := AMode;
end;

{ TKSocketStream }

constructor TKSocketStream.Create( ASocket: TSocket; ABlocked: Boolean; ATimeOut: Cardinal );
begin
	FSocket := ASocket;
	FBlocked := ABlocked;
	FTimeOut := Cardinal( INFINITE );
	FSocketLock := TKCriticalSection.Create( False );
	if FBlocked then
		FEvent := TKSimpleEvent.CreateSimple;
	inherited Create;
end;

destructor TKSocketStream.Destroy;
begin
	FEvent.Free;
	FSocketLock.Free;
	inherited Destroy;
end;

function TKSocketStream.GetTimeOut: Cardinal;
begin
	Result := FTimeOut;
end;

procedure TKSocketStream.SetTimeOut( Value: Cardinal );
begin
  FTimeOut := Value;
end;

function TKSocketStream.ReadLength: LongInt;
begin
	if CheckSocket( FSocket ) then
	begin
		FSocketLock.Lock;
		try
			IOCtlSocket( FSocket, FIONREAD, Longint( Result ) );
		finally
			FSocketLock.UnLock;
		end;
	end
	else
		Result := 0;
end;

function TKSocketStream.WaitForData( ATimeout: Cardinal ): Boolean;
var
	FDSet: TFDSet;
	TimeVal: TTimeVal;
begin
	Result := CheckSocket( FSocket );
	if Result then
	begin
		TimeVal.tv_sec := ( ATimeout div SECOND_TO_MSECOND );
		TimeVal.tv_usec := ( ( EventTimeout mod SECOND_TO_MSECOND ) * SECOND_TO_MSECOND );
		FD_ZERO( FDSet );
		FD_SET( FSocket, FDSet );
		Result := ( Select( 0, @FDSet, nil, nil, @TimeVal ) > 0 );
	end;
end;

procedure TKSocketStream.Disconnect;
begin
	FSocketLock.Lock;
	try
		CloseSocket( FSocket );
		FSocket := INVALID_SOCKET;
	finally
		FSocketLock.UnLock;
	end;
end;

function TKSocketStream.Read( var Buffer; Count: Longint ): Longint;
var
	ErrorCode: Integer;
	Overlapped: TOverlapped;
begin
	if CheckSocket( FSocket ) then
	begin
		if ( Count = -1 ) then
			Count := ReadLength;
		FSocketLock.Lock;
		try
			if Blocked then
			begin
				ZeroMemory( @Overlapped, SizeOf( TOverlapped ) );
				Overlapped.hEvent := FEvent.Handle;
				if ( not ReadFile( FSocket, Buffer, Count, {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF}( Result ),
					@Overlapped ) and ( GetLastError <> ERROR_IO_PENDING ) ) then
				begin
					ErrorCode := GetLastError;
					RaiseExceptionFmt( EKSocketStream, sErrSocketIOError, [sErrSocketRead, ErrorCode,
						SysErrorMessage( ErrorCode )] );
				end;
				if ( FEvent.WaitFor( EventTimeOut ) <> kwrSignaled ) then
					Result := 0
				else
				begin
					if ( not ( GetOverlappedResult( FSocket, Overlapped, {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF}( Result ),
						False ) and FEvent.ResetEvent ) ) then
						Result := 0;
				end;
			end
			else
			begin
				Result := recv( FSocket, Buffer, Count, 0 );
				if ( Result = SOCKET_ERROR ) then
				begin
					ErrorCode := WSAGetLastError;
					if ( ErrorCode <> WSAEWOULDBLOCK ) then
					begin
						Disconnect;
						if ( ErrorCode <> NO_ERROR ) then
							RaiseExceptionFmt( EKSocketStream, sErrWinSocketError, [SysErrorMessage(
								ErrorCode ), ErrorCode, 'recv'] ); { do not resource }
					end;
				end;
			end;
		finally
			FSocketLock.UnLock;
		end;
	end
	else
		Result := 0;
end;

function TKSocketStream.Write( const Buffer; Count: Longint ): Longint;
var
	p: Pointer;
	ErrorCode: Integer;
	Overlapped: TOverlapped;
begin
	if CheckSocket( FSocket ) then
	begin
		FSocketLock.Lock;
		try
			if Blocked then
			begin
				ZeroMemory( @Overlapped, SizeOf( TOverlapped ) );
				Overlapped.hEvent := FEvent.Handle;
				if ( not WriteFile( FSocket, Buffer, Count, {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF}( Result ),
					@Overlapped ) and ( GetLastError <> ERROR_IO_PENDING ) ) then
				begin
					ErrorCode := GetLastError;
					RaiseExceptionFmt( EKSocketStream, sErrSocketIOError, [sErrSocketWrite, ErrorCode,
						SysErrorMessage( ErrorCode )] );
				end;
				if ( FEvent.WaitFor( EventTimeOut ) <> kwrSignaled ) then
					Result := 0
				else if ( not GetOverlappedResult( FSocket, Overlapped, {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF}( Result ), False ) ) then
					Result := 0;
			end
			else
			begin
				p := @Buffer;
				Result := Send( FSocket, p^, Count, 0 );
				if ( Result = SOCKET_ERROR ) then
				begin
					ErrorCode := WSAGetLastError;
					if ( ErrorCode <> WSAEWOULDBLOCK ) then
					begin
						Disconnect;
						if ( ErrorCode <> NO_ERROR ) then
							RaiseExceptionFmt( EKSocketStream, sErrWinSocketError, [SysErrorMessage(
								ErrorCode ), ErrorCode, 'send'] ); { do not resource }
					end;
				end;
			end;
		finally
			FSocketLock.UnLock;
		end;
	end
	else
		Result := 0;
end;

function TKSocketStream.Seek( Offset: Longint; Origin: Word ): Longint;
begin
	Result := 0;
end;

{ TKWSAAsyncGetXbyY }

constructor TKWSAAsyncGetXbyY.Create;
begin
  inherited Create;
  FHandle := NULL_HANDLE_VALUE;
  FWSAList := TList.Create;
  StartUpWinSock;
end;

destructor TKWSAAsyncGetXbyY.Destroy;
begin
  Clear;
  FWSAList.Free;
  if CheckHandle( FHandle ) then
    DeAllocateHwnd( FHandle );
  CleanUpWinSock;
  inherited Destroy;
end;

function TKWSAAsyncGetXbyY.GetCount: Integer;
begin
  Result := FWSAList.Count;
end;

function TKWSAAsyncGetXbyY.GetAsyncKind( Index: THandle ): TKWSAAsyncGetXbyYKind;
begin
  Result := PKWSAAsyncStruct( FWSAList[IndexOf( Index )] )^.FKind;
end;

function TKWSAAsyncGetXbyY.GetAsyncHandles( Index: Integer ): THandle;
begin
  Result := PKWSAAsyncStruct( FWSAList[Index] )^.FHandle;
end;

function TKWSAAsyncGetXbyY.GetBuffers( Index: Integer ): Pointer;
begin
  Result := PKWSAAsyncStruct( FWSAList[Index] )^.FBuffer;
end;

function TKWSAAsyncGetXbyY.GetUserData( Index: Integer ): Pointer;
begin
  Result := PKWSAAsyncStruct( FWSAList[Index] )^.FUserData;
end;

procedure TKWSAAsyncGetXbyY.SetUserData( Index: Integer; Value: Pointer );
begin
  PKWSAAsyncStruct( FWSAList[Index] )^.FUserData := Value;
end;

function TKWSAAsyncGetXbyY.GetAsHostEnt( Index: Integer ): PHostEnt;
begin
  Result := PHostEnt( PKWSAAsyncStruct( FWSAList[Index] )^.FBuffer );
end;

function TKWSAAsyncGetXbyY.GetAsProtoEnt( Index: Integer ): PProtoEnt;
begin
  Result := PProtoEnt( PKWSAAsyncStruct( FWSAList[Index] )^.FBuffer );
end;

function TKWSAAsyncGetXbyY.GetAsServEnt( Index: Integer ): PServEnt;
begin
  Result := PServEnt( PKWSAAsyncStruct( FWSAList[Index] )^.FBuffer );
end;

function TKWSAAsyncGetXbyY.GetHandle: Hwnd;
begin
  if ( FHandle = NULL_HANDLE_VALUE ) then
    FHandle := AllocateHwnd( WndProc );
  Result := FHandle;  
end;

procedure TKWSAAsyncGetXbyY.WndProc( var Message: TMessage );
begin
  with TKWSAAsyncGetXbyYMsg( Message ) do
    if ValueBetween( Msg, WM_WSAASYNGETHOSTBYY, WM_WSAASYNGETSERVBYY, True ) then
      if ( Error = 0 ) then
        case Msg of
          WM_WSAASYNGETHOSTBYY : DoGetHostByY( Handle );
          WM_WSAASYNGETPROTOBYY: DoGetProtoByY( Handle );
          WM_WSAASYNGETSERVBYY : DoGetServByY( Handle );
        else
          DoAsyncSockError( Handle, -1 );
        end
      else
        DoAsyncSockError( Handle, Error );
end;

procedure TKWSAAsyncGetXbyY.DoGetHostByY( AHandle: Integer );
begin
  if Assigned( FOnGetHostByY ) then
    FOnGetHostByY( Self, AsyncKind[AHandle], IndexOf( AHandle ) );
end;

procedure TKWSAAsyncGetXbyY.DoGetProtoByY( AHandle: Integer );
begin
  if Assigned( FOnGetProtoByY ) then
    FOnGetProtoByY( Self, AsyncKind[AHandle], IndexOf( AHandle ) );
end;

procedure TKWSAAsyncGetXbyY.DoGetServByY( AHandle: Integer );
begin
  if Assigned( FOnGetServByY ) then
    FOnGetServByY( Self, AsyncKind[AHandle], IndexOf( AHandle ) );
end;

procedure TKWSAAsyncGetXbyY.DoAsyncSockError( AHandle, Error: Integer );
begin
  if Assigned( FOnAsyncSockError ) then
    FOnAsyncSockError( Self, AsyncKind[AHandle], IndexOf( AHandle ), Error )
  else
    RaiseExceptionFmt( EKWSAAsyncGetXbyY, sErrInvAsyncReqWS, [AHandle, LoadStr( Error )] );
end;

procedure TKWSAAsyncGetXbyY.Force( Data, UserData: Pointer; Kind: TKWSAAsyncGetXbyYKind );
begin
  case Add( Data, UserData, Kind ) of
    KWSA_ASYNCGETXBYY_WINSOCK_ERROR:
      RaiseExceptionFmt( EKWSAAsyncGetXbyY, sErrInvAsyncReqWSE, [LoadStr( WSAGetLastError )] );
    KWSA_ASYNCGETXBYY_PARAM_ERROR  :
      RaiseException( EKWSAAsyncGetXbyY, sErrInvAsyncReqPE );
  end;
end;

function TKWSAAsyncGetXbyY.CheckKind( Kind: TKWSAAsyncGetXbyYKind ): Boolean;
begin
  case Kind of
    wagkHostByName, wagkHostByAddr    : Result := Assigned( FOnGetHostByY );
    wagkProtoByName, wagkProtoByNumber: Result := Assigned( FOnGetProtoByY );
    wagkServByName, wagkServByPort    : Result := Assigned( FOnGetServByY );
  else
    Result := False;  
  end;
end;

function TKWSAAsyncGetXbyY.Add( Data, UserData: Pointer; Kind: TKWSAAsyncGetXbyYKind ): Integer;
var
  pwas: PKWSAAsyncStruct;
begin
  Result := KWSA_ASYNCGETXBYY_PARAM_ERROR;
  if ( CheckPointer( Data ) and CheckKind( Kind ) ) then
  begin
    pwas := New( PKWSAAsyncStruct );
    try
      ZeroMemory( pwas, SizeOf( TKWSAAsyncStruct ) );
      pwas^.FBuffer := AllocMem( MAXGETHOSTSTRUCT );
      try
        pwas^.FKind := Kind;
        pwas^.FUserData := UserData;
        Result := FWSAList.Add( pwas );
      except
        FreeMem( pwas^.FBuffer, MAXGETHOSTSTRUCT );
        raise;
      end;
    except
      Dispose( pwas );
      raise;
    end;
    case Kind of
      wagkHostByName:
        pwas^.FHandle := WSAAsyncGetHostByName( Handle, WM_WSAASYNGETHOSTBYY,
          PChar( Data ), pwas^.FBuffer, MAXGETHOSTSTRUCT );
      wagkHostByAddr:
        pwas^.FHandle := WSAAsyncGetHostByAddr( Handle, WM_WSAASYNGETHOSTBYY,
          PChar( Data ), 4, PF_INET, pwas^.FBuffer, MAXGETHOSTSTRUCT );
      wagkProtoByName:
        pwas^.FHandle := WSAAsyncGetProtoByName( Handle, WM_WSAASYNGETPROTOBYY,
          PChar( Data ), pwas^.FBuffer, MAXGETHOSTSTRUCT );
      wagkProtoByNumber:
        pwas^.FHandle := WSAAsyncGetProtoByNumber( Handle, WM_WSAASYNGETPROTOBYY,
          PInteger( Data )^, pwas^.FBuffer, MAXGETHOSTSTRUCT );
      wagkServByName:
        pwas^.FHandle := WSAAsyncGetServByName( Handle, WM_WSAASYNGETSERVBYY,
          PChar( Data ), nil, pwas^.FBuffer, MAXGETHOSTSTRUCT );
      wagkServByPort:
        pwas^.FHandle := WSAAsyncGetServByPort( Handle, WM_WSAASYNGETSERVBYY,
          PInteger( Data )^, nil, pwas^.FBuffer, MAXGETHOSTSTRUCT );
    end;
    { An Winsock error has occurred, free everything and return it! }
    if ( pwas^.FHandle = 0 ) then
    begin
      Delete( Result );
      Result := KWSA_ASYNCGETXBYY_WINSOCK_ERROR;
    end;
  end;
end;

function TKWSAAsyncGetXbyY.IndexOfIP( IP: Integer ): Integer;
begin
  Result := 0;
  while ( ( Result < FWSAList.Count ) and ( ( not ( PKWSAAsyncStruct(
    FWSAList[Result] )^.FKind in [wagkHostByName, wagkHostByAddr] ) ) or (
    HostEntToIP( AsHostEnt[Result] ) <> IP ) ) ) do
    Inc( Result );
  if ( Result = FWSAList.Count ) then
    Result := -1;
end;

function TKWSAAsyncGetXbyY.IndexOfHost( const Host: string ): Integer;
begin
  Result := 0;
  while ( ( Result < FWSAList.Count ) and ( ( not ( ( PKWSAAsyncStruct(
    FWSAList[Result] )^.FKind in [wagkHostByName, wagkHostByAddr] ) and
    CheckStrEqual( AsHostEnt[Result]^.h_name, Host ) ) ) ) ) do
    Inc( Result );
  if ( Result = FWSAList.Count ) then
    Result := -1;
end;

function TKWSAAsyncGetXbyY.IndexOf( Handle: THandle ): Integer;
begin
  Result := 0;
  while ( Result < FWSAList.Count ) and ( AsyncHandles[Result] <> Handle ) do
    Inc( Result );
  if ( Result = FWSAList.Count ) then
    Result := -1;
end;

procedure TKWSAAsyncGetXbyY.Delete( Index: Integer );
begin
  with PKWSAAsyncStruct( FWSAList[Index] )^ do
  begin
    FreeMem( FBuffer, MAXGETHOSTSTRUCT );
    WSACancelAsyncRequest( FHandle );
    FUserData := nil;
  end;
  Dispose( PKWSAAsyncStruct( FWSAList[Index] ) );
  FWSAList[Index] := nil;
  FWSAList.Delete( Index );
end;

function TKWSAAsyncGetXbyY.Remove( Handle: THandle ): Integer;
begin
  Result := IndexOf( Handle );
  if ( Result <> -1 ) then
    Delete( Result );
end;

procedure TKWSAAsyncGetXbyY.Clear;
begin
  while CheckList( FWSAList ) do
    Delete( FWSAList.Count - 1 );
end;

{
--------------------------------------------------------------------------------
------------------------ Generic Collection Objects ----------------------------
--------------------------------------------------------------------------------
}

{
	TKCustomCollection
	------------------

	PROTECTED METHODS
	--------- -------

	» These methods define the polimorphism/inheritance access through the FOwner
		field	by the correct class reference. Derived classes can subscribe
		GetOwnerComp and declare a proper return class reference (just call the
		inherited method and typecast). The GetOwner is the base funcionality of
		the TPersistent class and calls GetOwnerComp!

	 	function GetOwner: TPersistent; override;
		function GetOwnerComp: TPersistent;

	» Derived classes should determine the default itemname for SetItemName. The
		returned value MUST BE in the following format: Name%d. The default
		implementation is just ClassName%d.

		class function GetDefaultItemName( Item: TKCustomCollectionItem ): string; virtual;

	» Method suggested to be defined as virtual in derived classes with proper
		return class types

		function FindItem( const AName: string ): TKCustomCollectionItem;

	» This method calls FindItem and raises an exception if the return value is nil.
		It is the read method of the ItemByName property.

		function GetItemByName( const AName: string ): TKCustomCollectionItem;

}

{--------------------------- TKCustomCollectionItem ----------------------------}

constructor TKCustomCollectionItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKCustomCollection );
 	FEnabled := True;
	FGroupIndex := COLLECTION_ITEM_GROUP_NULL;
  FGroupAborted := False;
	inherited Create( ACollection );
	FOwner := ( ACollection as TKCustomCollection );
	FData := nil;
end;

destructor TKCustomCollectionItem.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;

procedure TKCustomCollectionItem.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomCollectionItem ) then
	begin
		if Owner.AllowDuplicateNames or
			 ( not CheckObject( Owner.FindItem( ( Source as TKCustomCollectionItem ).Name ) ) ) then
			Name := ( Source as TKCustomCollectionItem ).Name;
		Enabled := TKCustomCollectionItem( Source ).Enabled;
    GroupIndex := TKCustomCollectionItem( Source ).GroupIndex;
	end
	else
		inherited Assign( Source );
end;

function TKCustomCollectionItem.GetOwnerCollection: TKCustomCollection;
begin
	Result := FOwner;
end;

function TKCustomCollectionItem.GetDisplayName: string;
begin
	Result := FName; { ignore inherited that is ClassName }
end;

procedure TKCustomCollectionItem.SetDisplayName( const Value: string );
var
	i : Integer;
begin
	if ( not CheckStrEqual( Value, Name ) ) then
	begin
		if ( CheckObject( Owner ) and ( not Owner.AllowDuplicateNames ) ) then
			with Owner do
				for i := 0 to Count - 1 do
					if ( Items[I] <> Self ) and CheckObjectClass( Items[i], TKCustomCollectionItem ) and
						   CheckStrEqual( Value, Items[i].Name ) then
						RaiseExceptionFmt( EKCollectionItem, sErrDuplicateColItemName, [Value] );
		FName := Value;
		inherited SetDisplayName( Value );
	end;
end;

function TKCustomCollectionItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( CheckObject( Item ) and ( ID = Item.ID ) and ( Enabled = Item.Enabled ) and
    ( GroupIndex = Item.GroupIndex ) and ( Collection = Item.Collection ) and
    CheckStrEqual( Name, Item.Name ) );
end;

procedure TKCustomCollectionItem.SetEnabled( Value: Boolean );
begin
	if ( Value <> FEnabled ) then
	begin
		FEnabled := Value;
		Changed( False );
	end;
end;

procedure TKCustomCollectionItem.SetGroupIndex( Value: Cardinal );
begin
	if ( Value <> FGroupIndex ) then
	begin
		FGroupIndex := Value;
		ClearGroupAborted;
{ notify everyone for group rearrangement... }
		Changed( True );
	end;
end;

procedure TKCustomCollectionItem.MarkGroupAborted;
var
  i: Integer;
begin
	FGroupAborted := True;
	for i := 0 to Owner.Count - 1 do
		if ( Owner.Items[i].GroupIndex <> COLLECTION_ITEM_GROUP_NULL ) and
			 ( Owner.Items[i].GroupIndex = GroupIndex ) then
			Owner.Items[i].FGroupAborted := True; { Hard Couple! }
end;

procedure TKCustomCollectionItem.ClearGroupAborted;
var
  i: Integer;
begin
	FGroupAborted := False;
	for i := 0 to Owner.Count - 1 do
		if ( Owner.Items[i].GroupIndex <> COLLECTION_ITEM_GROUP_NULL ) and
			 ( Owner.Items[i].GroupIndex = GroupIndex ) then
			Owner.Items[i].FGroupAborted := False; { Hard Couple! }
end;

procedure TKCustomCollectionItem.LoadFromStream( Stream: TStream );
begin
end;

procedure TKCustomCollectionItem.LoadFromFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead );
	try
		LoadFromStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKCustomCollectionItem.SaveToStream( Stream: TStream );
begin
end;

procedure TKCustomCollectionItem.SaveToFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	Stream := TFileStream.Create( FileName, fmCreate );
	try
		SaveToStream( Stream );
	finally
		Stream.Free;
	end;
end;

{------------------------------ TKCustomCollection -----------------------------}

constructor TKCustomCollection.Create( AOwner: TPersistent;
	ItemClass: TKCustomCollectionItemClass; AllowDupNames: Boolean );
begin
	if ( not CheckClass( ItemClass ) ) then
		ItemClass := TKCustomCollectionItem;
	FAllowDuplicateNames := AllowDupNames;
	inherited Create( ItemClass );
	FOwner := AOwner; { nil Owners are allowed! }
end;

procedure TKCustomCollection.SetItem( Index: Integer; AItem: TKCustomCollectionItem );
begin
	inherited SetItem( Index, AItem );
end;

function TKCustomCollection.GetItem( Index: Integer ): TKCustomCollectionItem;
begin
	Result := TKCustomCollectionItem( inherited GetItem( Index ) );
end;

function TKCustomCollection.GetOwner: TPersistent;
begin
	Result := GetOwnerComp;
end;

function TKCustomCollection.GetOwnerComp: TPersistent;
begin
	Result := FOwner;
end;

procedure TKCustomCollection.SetItemName( Item: TCollectionItem );
var
	i,
	j,
	k: Integer;
	ItemName: string;
	CurItem: TKCustomCollectionItem;
begin
	inherited SetItemName( Item );
	j := 1;
	k := Count;
	while True do
	begin
		ItemName := GetDefaultItemName( TKCustomCollectionItem( Item ) );
		ForceTrimStr( ItemName );
		ItemName := Format( ItemName + '%d' , [j] );
		i := 0;
		while ( i < k ) do
		begin
			CurItem := Items[i];
			if ( ( CurItem <> Item ) and
				   CheckStrEqual( CurItem.Name, ItemName ) ) then
			begin
				Inc( j );
				Break;
			end;
			Inc( i );
		end;
		if ( i >= k ) then
		begin
			( Item as TKCustomCollectionItem ).Name := ItemName;
			Exit;
		end;
	end;
end;

class function TKCustomCollection.GetDefaultItemName( Item: TKCustomCollectionItem ): string;
begin
	Result := Item.ClassName;
end;

function TKCustomCollection.GetNames( Index: Integer ): string;
begin
	Result := Items[Index].Name;
end;

function TKCustomCollection.FindItem( const AName: string ): TKCustomCollectionItem;
var
	i: Integer;
begin
  ForceTrimStr( AName );
	for i:= 0 to Count - 1 do
	begin
		Result := Items[i];
		if CheckStrEqual( Result.Name, AName ) then
		  Exit;
	end;
	Result := nil;
end;

function TKCustomCollection.GetItemByName( const AName: string ): TKCustomCollectionItem;
begin
	Result := FindItem( AName );
	if ( Result = nil ) then
		RaiseExceptionFmt( EKCollection, sErrColItemNotFound, [AName] );
end;

function TKCustomCollection.Equals( Collection: TKCustomCollection ): Boolean;
var
	i: Integer;
begin
	Result := CheckObject( Collection ) and ( Count = Collection.Count ) and
		( Collection.GetOwner = GetOwner );
	if ( Result ) then
		for i:= 0 to Count - 1 do
			if ( not Items[i].Equals( Collection.Items[i] ) ) then
			begin
				Result := False;
				Exit;
			end;
end;

function TKCustomCollection.Add: TKCustomCollectionItem;
begin
	Result := TKCustomCollectionItem( inherited Add );
end;

procedure TKCustomCollection.ClearGroupAborted;
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
    Items[i].ClearGroupAborted; { Hard Couple! }
end;

procedure TKCustomCollection.ClearItems;
begin
	while CheckCollection( Self ) do
	  Items[Count - 1].Free;
end;

procedure TKCustomCollection.AddItems( Source: TKCustomCollection );
var
	i: Integer;
begin
	if CheckCollection( Source ) then
	begin
		BeginUpdate;
		try
			for i := 0 to Source.Count - 1 do
				Add.Assign( Source.Items[i] );
		finally
			EndUpdate;
		end;
	end;
end;

procedure TKCustomCollection.GetGroupSelection( Group: Cardinal; sl: TStrings );
var
	i: Integer;
begin
	ForceObject( sl );
	sl.BeginUpdate;
	try
		sl.Clear;
		for i := 0 to Count - 1 do
			if ( ( Group = COLLECTION_ITEM_GROUP_NULL ) or ( Group = Items[i].GroupIndex ) ) then
				sl.Add( Items[i].Name );
	finally
	  sl.EndUpdate;
	end;
end;

function TKCustomCollection.ForEachItemDo( Group: Cardinal;
	ItemGroupFunc: TKCollectionItemFunc; Data: Pointer ): Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Count - 1 do
		if ( ( Group = COLLECTION_ITEM_GROUP_NULL ) or ( Group = Items[i].GroupIndex ) ) then
			if ItemGroupFunc( Items[i], Data ) then
				Result := i
			else
				Exit;
end;

{
--------------------------------------------------------------------------------
--------------- Generic Memory Garbage Collectior Architecture -----------------
--------------------------------------------------------------------------------
}

{------------------------------ TKGarbageCollector -----------------------------}

constructor TKGarbageCollector.Create( AOwner: TKGarbageCollectorList );
begin
  ForceObject( AOwner );
	inherited Create;
	FID := Now;
	FPChars := nil;
	FMemory := nil;
	FOwner := AOwner;
end;

destructor TKGarbageCollector.Destroy;
begin
	FreeLists;
	inherited Destroy;
end;

procedure TKGarbageCollector.FreeLists;
begin
	FreePChars;
	FreeMemory;
end;

procedure TKGarbageCollector.FreeMemory;
var
	i: Integer;
begin
{ Memory list has not been instantiated }
	if ( not CheckObject( FMemory ) ) then
		Exit;
{ Dispose each memory block in the list }
	for i := FMemory.Count - 1 downto 0 do
	begin
		if CheckPointer( FMemory[i] ) then
			System.FreeMem( FMemory[i] );
		FMemory.Delete( i );
	end;
{ Dispose the Memory list }
	FreeClean( FMemory );
end;

procedure TKGarbageCollector.FreePChars;
var
	i: Integer;
begin
{ PChar list has not been instantiated }
	if ( not CheckObject( FPChars ) ) then
		Exit;
{ Dispose each PChar in the list }
	for i := FPChars.Count - 1 downto 0 do
	begin
		if CheckPointer( FPChars[i] ) then
			SysUtils.StrDispose( FPChars[i] );
		FPChars.Delete( i );
	end;
{ Dispose the PChar list }
	FreeClean( FPChars );
end;

function TKGarbageCollector.GetMemory: TList;
begin
	if ( not CheckObject( FMemory ) ) then
		FMemory := TList.Create;
	Result := FMemory;
end;

function TKGarbageCollector.GetPChars: TList;
begin
	if ( not CheckObject( FPChars ) ) then
		FPChars := TList.Create;
	Result := FPChars;
end;

{ Memory support }

procedure TKGarbageCollector.GetMem( var P: Pointer; Size: Cardinal );
begin
	System.GetMem( P, Size );
	Memory.Add( P );
end;

function TKGarbageCollector.AllocMem( Size: Cardinal ): Pointer;
begin
	Result := SysUtils.AllocMem( Size );
	Memory.Add( Result );
end;

procedure TKGarbageCollector.FreeMem( var P: Pointer; Size: Cardinal );
var
	pt: Pointer;
begin
	pt := P;
	System.FreeMem( P, Size );
	Memory.Remove( pt );
	P := nil;
end;

{ PChar support }

function TKGarbageCollector.StrNew( Str: PChar ): PChar;
begin
	Result := SysUtils.StrNew( Str );
	PChars.Add( Result );
end;

function TKGarbageCollector.StrAlloc( Size: Cardinal ): PChar;
begin
	Result := SysUtils.StrAlloc( Size );
	PChars.Add( Result );
end;

procedure TKGarbageCollector.StrDispose( Str: PChar );
var
	pc: PChar;
begin
	pc := Str;
	SysUtils.StrDispose( Str );
	PChars.Remove( pc );
end;

{---------------------------- TKGarbageCollectorList ---------------------------}

constructor TKGarbageCollectorList.Create;
begin
	inherited Create;
	FList := nil;
end;

destructor TKGarbageCollectorList.Destroy;
begin
	FreeCollectors;
	inherited Destroy;
end;

function TKGarbageCollectorList.GetCollectors( Index: TDateTime ): TKGarbageCollector;
var
	i: Integer;
begin
	Result := nil;
	i := IndexOf( Index );
	if ( i <> -1 ) then
		Result := FList[i];
end;

function TKGarbageCollectorList.GetList: TList;
begin
	if ( not CheckObject( FList ) ) then
		FList := TList.Create;
	Result := FList;
end;

procedure TKGarbageCollectorList.FreeCollectors;
var
	i: Integer;
begin
{ List has not been instantiated }
	if ( not CheckObject( FList ) ) then
		Exit;
{ Free each collector in the list }
	for i := FList.Count - 1 downto 0 do
	begin
		if CheckObject( FList[i] ) then
			TKGarbageCollector( FList[i] ).Free;
		FList.Delete( i );
	end;
{ Dispose the collector list }
	FreeClean( FList );
end;

function TKGarbageCollectorList.IndexOf( AID: TDateTime ): Integer;
var
	i: Integer;
begin
	Result := -1;
	if ( not CheckList( FList ) ) then
		Exit;
	for i := FList.Count - 1 downto 0 do
		if ( TKGarbageCollector( FList[i] ).ID = AID ) then
		begin
			Result := i;
			Exit;
		end;
end;

procedure TKGarbageCollectorList.DisposeCollectorByID( AID: TDateTime );
var
	i: Integer;
	gc: TKGarbageCollector;
begin
	i := IndexOf( AID );
	if ( i <> -1 ) then
	begin
		gc := TKGarbageCollector( List.Items[i] );
		DisposeCollector( gc );
	end;
end;

procedure TKGarbageCollectorList.DisposeCollector( var ACollector: TKGarbageCollector );
begin
	if ( not CheckObject( ACollector ) ) then
		Exit;
	List.Remove( ACollector );
	FreeClean( ACollector );
end;

function TKGarbageCollectorList.NewCollector( CollectorClass: TKGarbageCollectorClass;
	var ID: TDateTime ): TKGarbageCollector;
begin
	Result := CollectorClass.Create( Self );
	List.Add( Result );
	ID := Result.ID;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

const

	IM_LEFT = 12;
	BN_WIDTH = 82;
	BN_HEIGHT = 28;
	BN_GUTTER = 10;
	BN_DISTANCE = 14;

	FM_NAME = 'fmKowHowDlg';
	FM_WIDTH = 480;
	FM_HEIGHT = 240;
	FM_GUTTER = 86;

	LB_TOP = 15;
	LB_LEFT = 68;
	LB_HEIGHT = 16;
	LB_WIDTH = FM_WIDTH - 106;

	FM_LEFTMARGIN = IM_LEFT;
	FM_RIGHTMARGIN = FM_WIDTH - LB_LEFT - LB_WIDTH;

{ TKCustomInternalForm }

constructor TKCustomInternalForm.Create( AOwner: TComponent );
const
	DLG_STYLE: array[Boolean] of TFormBorderStyle = ( bsDialog, bsToolWindow );
begin
	CreateNew( AOwner );

	KeyPreview := True;
	FIsShowModal := True;
	
	Name := FM_NAME;

	Scaled := false;
	AutoScroll := false;
	BorderIcons := [biSystemMenu];
	BorderStyle := DLG_STYLE[DialogData.ToolWindow];

	with DialogData do
		if Centered or ( Top < 0 ) or ( Left < 0 ) or ( Top > Screen.Height ) or
			( Left > Screen.Width ) then
			Position := poScreenCenter
		else
		begin
			Position := poDesigned;
			Self.Top := DialogData.Top;
			Self.Left := DialogData.Left;
		end;

	Font.Size := DialogData.Font.Size;
	Font.Style := DialogData.Font.Style;
	Font.Name := DialogData.Font.Name;
	Font.Color := DialogData.Font.Color;

	Width := ScaleX( FM_WIDTH );
	Height := ScaleY( FM_HEIGHT );

	FText := '';
	FDialogStyle := dsOK;
	FButtons := TList.Create;

	FWindowManager := TKWindowManager.Create( Handle );
	FWindowManager.MinTrack.Ignore := false;

	FOldAlign := False;
	FExecutorProc := nil;
end;

destructor TKCustomInternalForm.Destroy;
begin
	if ( not IsShowModal ) then
		DestroyCtrls;
	FButtons.Free;
	FWindowManager.Free;
	inherited Destroy;
end;

procedure TKCustomInternalForm.FormClose( Sender: TObject; var Action: TCloseAction );
begin
	if ( not IsShowModal ) then
		Action := caFree;
end;

procedure TKCustomInternalForm.KeyDown( var Key: Word; Shift: TShiftState );
begin
	inherited KeyDown( Key, Shift );
	if ( Key = VK_ESCAPE ) then
	begin
		if IsShowModal then
			ModalResult := mrCancel
		else
		  Close;	
	end
	else if ( ssCtrl in Shift ) then
		case Key of
			Ord( 'c' ), Ord( 'C' ): if CopyToClipBoard then Key := 0;
			Ord( 'x' ), Ord( 'X' ): if CutToClipBoard then Key := 0;
			Ord( 'v' ), Ord( 'V' ): if PasteFromClipBoard then Key := 0;
		end;
end;

function TKCustomInternalForm.ScaleX( Value: Integer ): Integer;
begin
	Result := MulDiv( Value, Screen.Width, DEFAULT_INTERNAL_FORM_SCALE.X );
end;

function TKCustomInternalForm.ScaleY( Value: Integer ): Integer;
begin
	Result := MulDiv( Value, Screen.Height, DEFAULT_INTERNAL_FORM_SCALE.Y );
end;

function TKCustomInternalForm.CopyToClipBoard: Boolean;
begin
	Result := False;
	{ Derived classes can use this method to copy anything to clipboard.... }
end;

function TKCustomInternalForm.CutToClipBoard: Boolean;
begin
	Result := False;
	{ Derived classes can use this method to cut anything to clipboard.... }
end;

function TKCustomInternalForm.PasteFromClipBoard: Boolean;
begin
  Result := False;
	{ Derived classes can use this method to paste anything from clipboard.... }
end;

procedure TKCustomInternalForm.AddButton( const bnCaption: string;	bnResult: TModalResult;
	IsDefault: Boolean );
const
	MODAL_RESULTS: array[mrNone..mrYesToAll] of string[10] = ( 'bnNone', 'bnOk',
		'bnCancel', 'bnAbort', 'bnRetry', 'bnIgnore', 'bnYes', 'bnNo', 'bnAll',
		'bnNoToAll', 'bnYesToAll' );
var
	bn: TButton;
begin
	bn := TBitBtn.Create( nil );
	try
		with bn do
		begin
			Parent := Self;
			Caption := bnCaption;
			Default := IsDefault;
			ModalResult := bnResult;
			Width := ScaleX( BN_WIDTH );
			Height := ScaleY( BN_HEIGHT );
			Name := MODAL_RESULTS[ModalResult];
			if ( not IsShowModal ) then
  			OnClick := ButtonClick;
		end;
	except
		bn.Free;
		raise;
	end;
	FButtons.Add( bn );
end;

procedure TKCustomInternalForm.ButtonClick( Sender: TObject );
begin
	if ( not IsShowModal ) then
	  Close;
end;

procedure TKCustomInternalForm.PrepareForm;
begin
	case DialogStyle of
		dsOK:
			AddButton( sCapOK, mrOK, true );
		dsCancel:
			AddButton( sCapCancel, mrCancel, true );
		dsOKCancel:
		begin
			AddButton( sCapOK, mrOK, true );
			AddButton( sCapCancel, mrCancel, false );
		end;
		dsYesNo:
		begin
			AddButton( sCapYes, mrYes, true );
			AddButton( sCapNo, mrNo, false );
		end;
		dsYesNoCancel:
		begin
			AddButton( sCapYes, mrYes, true );
			AddButton( sCapNo, mrNo, false );
			AddButton( sCapCancel, mrCancel, false );
		end;
		dsYesAllNoCancel:
		begin
			AddButton( sCapYes, mrYes, true );
			AddButton( sCapYesAll, mrAll, false );
			AddButton( sCapNo, mrNo, false );
			AddButton( sCapCancel, mrCancel, false );
		end;
		dsYesNoAllCancel:
		begin
			AddButton( sCapYes, mrYes, true );
			AddButton( sCapNo, mrNo, false );
			AddButton( sCapNoAll, mrAll, false );
			AddButton( sCapCancel, mrCancel, false );
		end;
		dsAbortRetryIgnore:
		begin
			AddButton( sCapAbort, mrAbort, true );
			AddButton( sCapRetry, mrRetry, false );
			AddButton( sCapIgnore, mrIgnore, false );
		end;
	end;
	FLabel := TLabel.Create( nil );
	FLabel.Name := 'lbMessage';
	if ( not IsShowModal ) then
		OnClose := FormClose;
end;

procedure TKCustomInternalForm.FixButtons;
var
	i: Integer;
begin
	if CheckObject( FButtons ) then
		for i := 0 to FButtons.Count - 1 do
			with FButtons, TButton( FButtons[i] ) do
			begin
				Top := Self.ClientHeight - Height - ScaleY( BN_GUTTER );
				if ( i = 0 ) then
					if DialogData.RightAlign then
						Left :=
							( Self.ClientWidth ) - ( Count * Width ) -
							( ScaleX( FM_RIGHTMARGIN ) ) -
							( ( Count - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
					else
						Left :=
							( Self.Width div 2 ) -
							( Count * Width div 2 ) -
							( ( Count - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
				else
					Left := TButton( FButtons[i-1] ).Left + Width +
									ScaleX( BN_DISTANCE );
			end;
end;

procedure TKCustomInternalForm.DestroyCtrls;
var
	i: Integer;
begin
	if CheckObject( FButtons ) then
		for i := FButtons.Count - 1 downto 0 do
			if CheckObject( FButtons[i] ) then
				TButton( FButtons[i] ).Free;
	FLabel.Free;
end;

procedure TKCustomInternalForm.UnprepareForm;
begin
	if IsShowModal then
	  DestroyCtrls;
end;

{ This two method *MUST* allways be called within a try..finally block }

procedure TKCustomInternalForm.DoPrepareShow;
begin
	FOldAlign := DialogData.RightAlign;
	PrepareForm;
	FixButtons;
	FWindowManager.MinTrack.Width := Width;
	FWindowManager.MinTrack.Height := Height;
	with DialogData do
		if CheckReference( @swCallBack ) then
			swCallBack( Self, UserData );
	Width := FWindowManager.MinTrack.Width;;
	Height := FWindowManager.MinTrack.Height;
end;

procedure TKCustomInternalForm.DoUnprepareShow;
begin
	UnprepareForm;
	DialogData.RightAlign := FOldAlign;
end;

function TKCustomInternalForm.ShowModal: Integer;
begin
	DoPrepareShow;
	try
		Result := inherited ShowModal;
	finally
		DoUnprepareShow;
	end;
end;

procedure TKCustomInternalForm.Show;
begin
	DoPrepareShow;
	try
	  inherited Show;
	finally
		DoUnprepareShow;
	end;
end;

{
---------------------------------------------------------------------------------
---------------------------------- TKMeasures -----------------------------------
---------------------------------------------------------------------------------
}

{------------------------------ TKCustomMeasures -------------------------------}

constructor TKCustomMeasures.Create( AOwner: TComponent );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
	FChanging := false;
end;

constructor TKCustomMeasures.CreateLinked( AOwner: TComponent; AGet: TKGetMeasuresEvent;
	ASet: TKSetMeasuresEvent );
begin
	Create( AOwner );
	FOnSetMeasures := ASet;
	FOnGetMeasures := AGet;
end;

procedure TKCustomMeasures.DoSetMeasures( Index: TKScalingFlag; Value: Single );
begin
	if Assigned( FOnSetMeasures ) then
	begin
		FChanging := true;
		try
			FOnSetMeasures( Self, Index, Value );
		finally
			FChanging := false;
		end;
	end;
end;

procedure TKCustomMeasures.DoGetMeasures( Index: TKScalingFlag; var Value: Single );
begin
	if Assigned( FOnGetMeasures ) then
	begin
		FChanging := true;
		try
			FOnGetMeasures( Self, Index, Value );
		finally
			FChanging := false;
		end;
	end;
end;

function TKCustomMeasures.GetValue( Index: Integer ): Single;
begin
	if FChanging then
	  Exit;
	DoGetMeasures( TKScalingFlag( Index ), Result );
end;

procedure TKCustomMeasures.SetValue( Index: Integer; Value: Single );
begin
	if FChanging then
	  Exit;
	DoSetMeasures( TKScalingFlag( Index ), Value );
end;

function TKCustomMeasures.GetOwnerComp: TComponent;
begin
	Result := FOwner;
end;

{------------------------------ TKPointMeasures --------------------------------}

procedure TKPointMeasures.SetPoint( Value: TPoint );
begin
  cX := Value.x;
  cY := Value.y;
end;

function TKPointMeasures.GetPoint: TPoint;
begin
  Result := Classes.Point( Round( cX ), Round( cY ) );
end;

{------------------------------ TKRectMeasures ---------------------------------}

procedure TKRectMeasures.SetRect( Value: TRect );
begin
  Left := Value.Left;
	Top := Value.Top;
  Right := Value.Right;
  Bottom := Value.Bottom;
end;

function TKRectMeasures.GetRect: TRect;
begin
  Result := Classes.Rect( Round( Left ), Round( Top ), Round( Right ), Round( Bottom ) );
end;

{ TKTickCount }

procedure TKTickCount.Start;
begin
	QueryPerformanceFrequency( FFrequency );
	QueryPerformanceCounter( FTime1 );
	FElapsedTime.QuadPart := 0;
end;

procedure TKTickCount.Stop;
begin
	QueryPerformanceCounter( FTime2 );
	FElapsedTime.QuadPart :=
	{$IFDEF DELPHI4}
		( ( FTime2 - FTime1 ) div FFrequency ); { sorry... }
	{$ELSE}
	  ( ( FTime2.QuadPart - FTime1.QuadPart ) / FFrequency.QuadPart );
	{$ENDIF}	
end;

end.
