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

unit ukwConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, uksyConsts, ukwResStr;

const

{--------------------------------- TKMailSlot ----------------------------------}

	DEF_MS_BUFFSIZE   = 64;
	DEF_MS_BURSTCOUNT = 12;
	DEF_MS_BURSTDELAY = 75;
	DEF_MS_INTERVAL   = 200;
	DEF_MS_TIMEOUT    = 4000;
	
	KM_READMAIL     = KM_USER + $01;
	KM_MSTIMEOUT    = KM_USER + $02;
	KM_MSRECEIVEACK = KM_USER + $03;
	KM_MSFIRST			= KM_READMAIL;
	KM_MSLAST				= KM_MSRECEIVEACK;

	SLOTNAME_SIZE   = 64;
	MAX_SDATA_SIZE  = 256;
	MAILSLOT_NDATA_COMPLETION  = 4;
	MAILSLOT_USERNAME_SIZE     = 16;
	MAILSLOT_COMPUTERNAME_SIZE = 16;
	MAILSLOT_KNOWHOW_SLOT = 'KnowHow';
	MAILSLOT_DEFAULT_DOMAIN_BROADCAST = '*';
	MAILSLOT_ROOT = '\\.\mailslot\';
	MAILSLOT_PRIMARYBROADCAST = '\\*\mailslot\';

	APP_CTRL_MS_PREFIX = '\spy_';
	APPCTRL_BASEROOT = '\Software\';

	sW95PerfObjKey = 'System\CurrentControlSet\control\PerfStats\Enum';
	sW95PerfDynKey = 'PerfStats';

{ -------------------- Remote Service Manager Components ---------------------- }

  KM_RSM_BASE				 = KM_USER + $03;

	KM_RSM_WIN32ERROR  = KM_RSM_BASE + $01; { Remote exception management                    }

	KM_RSM_QUERYDPDSTR = KM_RSM_BASE + $02; { Called when StopService with DependenciesStop  }
																					{ is true 																			 }

{-------------------------------- ukwCtrls.pas ---------------------------------}

	KM_DIRCHANGED   = KM_USER + $04;
	KM_TRAYICON     = KM_USER + $05;

	DRAG_QUERY_COUNT = $FFFFFFFF;

	INTERVAL_ANITRAY_DISABLED = 0;

{##NI##}

{$IFDEF KLIB100}
	WINAPI_VER = '1.00';
	WINAPI_VER_INT = 100;
	WINAPI_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	WINAPI_VER = '?.??';
	WINAPI_VER_INT = 0;
	WINAPI_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetWinAPIRegistryInfo: LongInt;

{##NI##}

implementation

uses
	SysUtils, uksyTypes;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}

function GetWinAPIRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.
