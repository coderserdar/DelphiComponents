/****************************************************************************

Copyright (c) 2003  Microsoft Corporation

Module Name:

	 ewfapi.h

Abstract:

	  Defines the EWF APIs

Environment:

	 User mode

Revision History:


****************************************************************************/
#ifndef __EWFAPI_H__
#define __EWFAPI_H__

#ifdef __cplusplus
extern "C" {
#endif

#ifndef  EWFIMP
#define EWFIMP __declspec(dllimport)
#endif


typedef struct _EWF_VOLUME_NAME_ENTRY 
{
	struct _EWF_VOLUME_NAME_ENTRY * Next;
	WCHAR   Name[1];
} EWF_VOLUME_NAME_ENTRY, *PEWF_VOLUME_NAME_ENTRY;


typedef enum
{
	EWF_NO_CMD = 0,		// no pending command
	EWF_ENABLE,			// overlay will be enabled
	EWF_DISABLE,		// overlay will be disabled
	EWF_SET_LEVEL,		// overlay level will be set
	EWF_COMMIT			// current level will be committed to the protected volume
} EWF_CMD;

typedef enum
{
	EWF_ENABLED,		// The overlay is enabled on the volume
	EWF_DISABLED		// The overlay is disabled on the volume
} EWF_STATE;

typedef enum
{
	EWF_DISK,			// DISK overlay
	EWF_RAM,			// RAM overlay, with an associated overlay volume store
	EWF_RAM_REG,		// RAM overlay, without an associated overlay volume store
} EWF_TYPE, *PEWF_TYPE;


#define EWF_MAX_DEVICE_NAME_LENGTH (256)
#define EWF_VOLUME_ID_SIZE (16)

typedef struct _EWF_VOLUME_DESC
{
	WCHAR DeviceName[EWF_MAX_DEVICE_NAME_LENGTH];	// Device name of the volume
	UCHAR VolumeID[EWF_VOLUME_ID_SIZE];				// 16 byte volume identifier
} EWF_VOLUME_DESC, * PEWF_VOLUME_DESC;


//
// This is a variable size structure depending on how many protected overlay
// volumes there are.
//
typedef struct _EWF_OVERLAY_STORE_CONFIG
{
	ULONG FormatVersion;	// Version of Overlay-Store format
	LONGLONG VolumeSize;	// Size of the overlay volume in bytes
	
	ULONG NumSegments;		// Number of segments that the volume is divided into
	ULONG FreeSegments;		// Number of segments that are free
	ULONG SegmentSize;		// Size of each segment in bytes
	
	ULONG MaxVolumes;		// Maximum number of protected volumes
	ULONG NumVolumes;		// Number of currently protected volumes
	USHORT MaxLevels;		// Maximum number of overlay levels
	EWF_VOLUME_DESC VolumeDescArray[1]; 
							 // The array holds NumVolume count volume descriptions
} EWF_OVERLAY_STORE_CONFIG, * PEWF_OVERLAY_STORE_CONFIG;


typedef struct _EWF_COMMAND
{
	EWF_CMD Command;    // ENABLE, DISABLE, etc..
	ULONG Param1;       // command first parameter.
	ULONG Param2;	    // command second parameter.
	
} EWF_COMMAND, *PEWF_COMMAND;


#define EWF_MAX_LEVEL_NAME_LENGTH (64)

typedef struct _EWF_LEVEL_DESC
{
	WCHAR LevelName[EWF_MAX_LEVEL_NAME_LENGTH];	
										// friendly name of the level
										// If the length is equal to EWF_MAX_LEVEL_NAME_LENGTH
										// then no null terminator is stored.
	FILETIME LevelEndTime;				// time at which the level was ended
	LONGLONG LevelDataSize;				// Size of the data in the level in bytes 
} EWF_LEVEL_DESC, * PEWF_LEVEL_DESC;


#define EWF_MAX_PERSISTENT_DATA (32)	// maximum number of bytes that can be persisted


typedef struct _EWF_VOLUME_CONFIG
{
	EWF_TYPE Type;					// Type of overlay for this volume
	EWF_STATE State;				// state of the overlay for this volume, ENABLED or DISABLED
	EWF_COMMAND BootCommand;		// Command to execute on next restart

	UCHAR PersistentData[EWF_MAX_PERSISTENT_DATA];	
									// Small amount of persistent data that survives a restore
	
	USHORT MaxLevels;				// Maximum number of checkpoint levels for this volume
	ULONG ClumpSize;				// 512 bytes
	USHORT CurrentLevel;			// Current checkpoint level

	union
	{
		struct
		{
			LONGLONG DiskMapSize;	// Size of the mapping data on disk
			LONGLONG DiskDataSize;	// Size of the data stored on disk for this protected volume  
		} DiskOverlay;

		struct
		{
			LONGLONG RamDataSize;	// Size of the data stored in RAM for this protected volume
		} RamOverlay;
	};
	ULONG MemMapSize;				// Size of the mapping data in memory

	EWF_VOLUME_DESC VolumeDesc;		// volume device name, and volume ID

	EWF_LEVEL_DESC LevelDescArray[1];	// Level descripton and end time, and level data size
	
} EWF_VOLUME_CONFIG, * PEWF_VOLUME_CONFIG;


EWFIMP
WCHAR
WINAPI
EwfMgrGetDriveLetterFromVolumeName(
	IN LPCWSTR lpVolumeName
	);


EWFIMP
BOOL
WINAPI
EwfMgrVolumeNameListIsEmpty(
	IN PEWF_VOLUME_NAME_ENTRY pVolumeNameList
	);


EWFIMP
VOID
WINAPI
EwfMgrVolumeNameEntryPop(
	IN PEWF_VOLUME_NAME_ENTRY * ppVolumeNameList
	);
	

EWFIMP
VOID
WINAPI
EwfMgrVolumeNameListDelete(
	IN PEWF_VOLUME_NAME_ENTRY pVolumeNameList
	);


EWFIMP
HANDLE
WINAPI
EwfMgrOpenProtected(
	IN LPCWSTR lpVolume
	);

EWFIMP
BOOL
WINAPI
EwfMgrClose(
	IN HANDLE hDevice
	);

EWFIMP
BOOL
WINAPI
EwfMgrClearCommand(
	IN HANDLE hDevice
	);


EWFIMP
BOOL 
WINAPI
EwfMgrSetPersistentData(
	IN HANDLE hDevice,
	IN LPBYTE lpPersistentData,
	IN DWORD  cbPersistentData
  );


EWFIMP
BOOL 
WINAPI
EwfMgrGetPersistentData(
	IN HANDLE hDevice,
	OUT LPBYTE lpPersistentData,
	IN DWORD  cbPersistentData
  );



EWFIMP	
BOOL 
WINAPI
EwfMgrCheckpoint(
	IN HANDLE hDevice,
	IN OPTIONAL LPCWSTR lpDescription
  );


EWFIMP
BOOL 
WINAPI
EwfMgrRestore(
	IN HANDLE hDevice
	);

EWFIMP
BOOL 
WINAPI
EwfMgrDisable(
	IN HANDLE hDevice,
	IN BOOL fCommit
	);


EWFIMP
BOOL 
WINAPI
EwfMgrEnable(
	IN HANDLE hDevice
	);

EWFIMP	
BOOL 
WINAPI
EwfMgrCommit(
	IN HANDLE hDevice
	);


EWFIMP
BOOL 
WINAPI
EwfMgrSetLevel(
	IN HANDLE hDevice,
	IN OPTIONAL LPCWSTR lpDescription, 
	IN int Level,
	IN BOOL fDeleteLevel
	);

EWFIMP  
PEWF_VOLUME_CONFIG
WINAPI
EwfMgrGetProtectedVolumeConfig(
	IN HANDLE hDevice
	);

EWFIMP
PEWF_VOLUME_NAME_ENTRY
WINAPI
EwfMgrGetProtectedVolumeList(
	VOID
	);

EWFIMP
HANDLE
WINAPI
EwfMgrOpenOverlayStore(
	IN BOOL fOpenForAsyncIO
	);

EWFIMP
PEWF_OVERLAY_STORE_CONFIG
WINAPI
EwfMgrGetOverlayStoreConfig(
	IN HANDLE hDevice
	);

EWFIMP
BOOL
WINAPI
EwfMgrRegisterLowSpaceNotification(
	IN HANDLE hDevice,
	IN LONGLONG FreeBytesRemaining,
	IN LPOVERLAPPED lpOverlapped
	);
	

#ifdef __cplusplus
}
#endif

#endif // __EWFAPI_H__

