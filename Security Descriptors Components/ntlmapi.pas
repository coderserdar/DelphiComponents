{------------------------------------------------------------------------------}
{ ntlmapi                                                                      }
{------------------------------------------------------------------------------}
{ Microsoft LAN Manager                                                        }
{ Copyright(c) Microsoft Corp., 1987-1997                                      }
{                                                                              }
{ Translated from Microsoft's lm*.h header files                               }
{ by Damien.Thouvenin@wanadoo.fr                                               }
{------------------------------------------------------------------------------}
 Unit ntlmapi;


{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Clauses Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Windows;

 Procedure InitNTLanManAPI;
 Procedure FinalizeNTLanManAPI;

 Type
    TNTStatus = Longint;

{------------------------------------------------------------------------------}
{  BUILD Version: 0003                                                         }
{  Copyright 1990 - 1998 Microsoft Corporation                                 }
{      LMCONS.H (was NETCONS.H in LM 2.x)                                      }
{  Abstract:                                                                   }
{      This file contains constants used throughout the LAN Manager            }
{      API header files.  It should be included in any source file             }
{      that is going to include other LAN Manager API header files or          }
{      call a LAN Manager API.                                                 }
{      NOTE:  Lengths of strings are given as the maximum lengths of the       }
{      string in characters (not bytes).  This does not include space for the  }
{      terminating 0-characters.  When allocating space for such an item,      }
{      use the form:                                                           }
{          TCHAR username[UNLEN+1];                                            }
{      Definitions of the form LN20_* define those values in effect for        }
{      LanMan 2.0.                                                             }
{------------------------------------------------------------------------------}

 Const
    // String Lengths for various LanMan names
    CNLEN             = 15;                          // Computer name length
    LM20_CNLEN        = 15;                          // LM 2.0 Computer name length
    DNLEN             = CNLEN;                       // Maximum domain name length
    LM20_DNLEN        = LM20_CNLEN;                  // LM 2.0 Maximum domain name length


    UNCLEN            = CNLEN+2;                     // UNC computer name length
    LM20_UNCLEN       = LM20_CNLEN+2;                // LM 2.0 UNC computer name length

    NNLEN             = 80;                          // Net name length (share name)
    LM20_NNLEN        = 12;                          // LM 2.0 Net name length

    RMLEN             = UNCLEN+1+NNLEN;              // Max remote name length
    LM20_RMLEN        = LM20_UNCLEN+1+LM20_NNLEN;    // LM 2.0 Max remote name length

    SNLEN             = 80;                          // Service name length
    LM20_SNLEN        = 15;                          // LM 2.0 Service name length
    STXTLEN           = 256;                         // Service text length
    LM20_STXTLEN      = 63;                          // LM 2.0 Service text length

    PATHLEN           = 256;                         // Max. path (not including drive name)
    LM20_PATHLEN      = 256;                         // LM 2.0 Max. path

    DEVLEN            = 80;                          // Device name length
    LM20_DEVLEN       = 8;                           // LM 2.0 Device name length

    EVLEN             = 16;                          // Event name length

    // User, Group and Password lengths
    UNLEN             = 256;                         // Maximum user name length
    LM20_UNLEN        = 20;                          // LM 2.0 Maximum user name length

    GNLEN             = UNLEN;                       // Group name
    LM20_GNLEN        = LM20_UNLEN;                  // LM 2.0 Group name

    PWLEN             = 256;                         // Maximum password length
    LM20_PWLEN        = 14;                          // LM 2.0 Maximum password length

    SHPWLEN           = 8;                           // Share password length (bytes)


    CLTYPE_LEN        = 12;                          // Length of client type string


    MAXCOMMENTSZ      = 256;                         // Multipurpose comment length
    LM20_MAXCOMMENTSZ = 48;                          // LM 2.0 Multipurpose comment length

    QNLEN             = NNLEN;                       // Queue name maximum length
    LM20_QNLEN        = LM20_NNLEN;                  // LM 2.0 Queue name maximum length

    // The ALERTSZ and MAXDEVENTRIES defines have not yet been NT'ized.
    // Whoever ports these components should change these values appropriately.
    ALERTSZ           = 128;                        // size of alert string in server
    MAXDEVENTRIES     = SizeOf(integer)*8;          // Max number of device entries

    NETBIOS_NAME_LEN  = 16;                         // NetBIOS net name (bytes)

    // Value to be used with APIs which have a "preferred maximum length"
    // parameter.  This value indicates that the API should just allocate
    // "as much as it takes."
    MAX_PREFERRED_LENGTH    = $FFFFFFFF;

    //        Constants used with encryption
    CRYPT_KEY_LEN           =  7;
    CRYPT_TXT_LEN           =  8;
    ENCRYPTED_PWLEN         = 16;
    SESSION_PWLEN           = 24;
    SESSION_CRYPT_KLEN      = 21;

    //  Value to be used with SetInfo calls to allow setting of all
    //  settable parameters (parmnum zero option)
    PARMNUM_ALL             = 0;

    PARM_ERROR_UNKNOWN      = $FFFFFFFF;
    PARM_ERROR_NONE         = 0;
    PARMNUM_BASE_INFOLEVEL  = 1000;

    // Message File Names
    MESSAGE_FILENAME        = 'NETMSG';
    OS2MSG_FILENAME         = 'BASE';
    HELP_MSG_FILENAME       = 'NETH';

    // The backup message file named here is a duplicate of net.msg. It
    // is not shipped with the product, but is used at buildtime to
    // msgbind certain messages to netapi.dll and some of the services.
    // This allows for OEMs to modify the message text in net.msg and
    // have those changes show up.        Only in case there is an error in
    // retrieving the messages from net.msg do we then get the bound
    // messages out of bak.msg (really out of the message segment).

    BACKUP_MSG_FILENAME     = 'BAK.MSG';

 Type
    // Keywords used in Function Prototypes
    NET_API_STATUS          = DWORD;
    API_RET_TYPE            = NET_API_STATUS;      // Old value: do not use

 Const
    // The platform ID indicates the levels to use for platform-specific
    // information.
    PLATFORM_ID_DOS = 300;
    PLATFORM_ID_OS2 = 400;
    PLATFORM_ID_NT  = 500;
    PLATFORM_ID_OSF = 600;
    PLATFORM_ID_VMS = 700;


{------------------------------------------------------------------------------}
{ NERR_BASE is the base of error codes from network utilities,                 }
{ chosen to avoid conflict with system and redirector error codes.             }
{ 2100 is a value that has been assigned to us by system.                      }
{------------------------------------------------------------------------------}
 Const
    NERR_Success           =  0;       // Success
    NERR_BASE              = 2100;

{------------------------------------------------------------------------------}
{ There message numbers assigned to different LANMAN components                }
{ are as defined below.                                                        }
{                                                                              }
{ lmerr.h:        2100 - 2999     NERR_BASE                                    }
{ alertmsg.h:     3000 - 3049     ALERT_BASE                                   }
{ lmsvc.h:        3050 - 3099     SERVICE_BASE                                 }
{ lmerrlog.h:     3100 - 3299     ERRLOG_BASE                                  }
{ msgtext.h:      3300 - 3499     MTXT_BASE                                    }
{ apperr.h:       3500 - 3999     APPERR_BASE                                  }
{ apperrfs.h:     4000 - 4299     APPERRFS_BASE                                }
{ apperr2.h:      4300 - 5299     APPERR2_BASE                                 }
{ ncberr.h:       5300 - 5499     NRCERR_BASE                                  }
{ alertmsg.h:     5500 - 5599     ALERT2_BASE                                  }
{ lmsvc.h:        5600 - 5699     SERVICE2_BASE                                }
{ lmerrlog.h      5700 - 5799     ERRLOG2_BASE                                 }
{------------------------------------------------------------------------------}
 Const
    MIN_LANMAN_MESSAGE_ID  = NERR_BASE;
    MAX_LANMAN_MESSAGE_ID  = 5799;

{------------------------------------------------------------------------------}
{ BUILD Version: 0001    // Increment this if a change has global effect       }
{ lmerr.h - network error definitions                                          }
{------------------------------------------------------------------------------}
{ WARNING                                                                      }
{ See the comment in lmcons.h for                                              }
{ info on the allocation of errors                                             }
{------------------------------------------------------------------------------}
{ WARNING                                                                      }
{ The range 2750-2799 has been                                                 }
{ allocated to the IBM LAN Server                                              }
{------------------------------------------------------------------------------}
{ WARNING                                                                      }
{ The range 2900-2999 has been                                                 }
{ reserved for Microsoft OEMs                                                  }
{------------------------------------------------------------------------------}
 Const
    // UNUSED BASE+0
    // UNUSED BASE+1
    NERR_NetNotStarted      = NERR_BASE+2;   // The workstation driver is not installed.
    NERR_UnknownServer      = NERR_BASE+3;   // The server could not be located.
    NERR_ShareMem           = NERR_BASE+4;   // An internal error occurred.  The network cannot access a shared memory segment.

    NERR_NoNetworkResource  = NERR_BASE+5;   // A network resource shortage occurred .
    NERR_RemoteOnly         = NERR_BASE+6;   // This operation is not supported on workstations.
    NERR_DevNotRedirected   = NERR_BASE+7;   // The device is not connected.
    // NERR_BASE+8 is used for ERROR_CONNECTED_OTHER_PASSWORD
    // UNUSED BASE+9
    // UNUSED BASE+10
    // UNUSED BASE+11
    // UNUSED BASE+12
    // UNUSED BASE+13
    NERR_ServerNotStarted   = NERR_BASE+14;  // The Server service is not started.
    NERR_ItemNotFound       = NERR_BASE+15;  // The queue is empty.
    NERR_UnknownDevDir      = NERR_BASE+16;  // The device or directory does not exist.
    NERR_RedirectedPath     = NERR_BASE+17;  // The operation is invalid on a redirected resource.
    NERR_DuplicateShare     = NERR_BASE+18;  // The name has already been shared.
    NERR_NoRoom             = NERR_BASE+19;  // The server is currently out of the requested resource.
    // UNUSED BASE+20
    NERR_TooManyItems       = NERR_BASE+21;  // Requested addition of items exceeds the maximum allowed.
    NERR_InvalidMaxUsers    = NERR_BASE+22;  // The Peer service supports only two simultaneous users.
    NERR_BufTooSmall        = NERR_BASE+23;  // The API return buffer is too small.
    // UNUSED BASE+24
    // UNUSED BASE+25
    // UNUSED BASE+26
    NERR_RemoteErr          = NERR_BASE+27;  // A remote API error occurred.
    // UNUSED BASE+28
    // UNUSED BASE+29
    // UNUSED BASE+30
    NERR_LanmanIniError     = NERR_BASE+31;  // An error occurred when opening or reading the configuration file.
    // UNUSED BASE+32
    // UNUSED BASE+33
    // UNUSED BASE+34
    // UNUSED BASE+35
    NERR_NetworkError       = NERR_BASE+36;  // A general network error occurred.
    NERR_WkstaInconsistentState = NERR_BASE+37; // The Workstation service is in an inconsistent state. Restart the computer before restarting the Workstation service.
    NERR_WkstaNotStarted    = NERR_BASE+38;  // The Workstation service has not been started.
    NERR_BrowserNotStarted  = NERR_BASE+39;  // The requested information is not available.
    NERR_InternalError      = NERR_BASE+40;  // An internal Windows NT error occurred.
    NERR_BadTransactConfig  = NERR_BASE+41;  // The server is not configured for transactions.
    NERR_InvalidAPI         = NERR_BASE+42;  // The requested API is not supported on the remote server.
    NERR_BadEventName       = NERR_BASE+43;  // The event name is invalid.
    NERR_DupNameReboot      = NERR_BASE+44;  // The computer name already exists on the network. Change it and restart the computer.

{------------------------------------------------------------------------------}
{ Config API related                                                           }
{ Error codes from BASE+45 to BASE+49                                          }
{------------------------------------------------------------------------------}
 Const
    // UNUSED BASE+45
    NERR_CfgCompNotFound    = NERR_BASE+46;  // The specified component could not be found in the configuration information.
    NERR_CfgParamNotFound   = NERR_BASE+47;  // The specified parameter could not be found in the configuration information.
    NERR_LineTooLong        = NERR_BASE+49;  // A line in the configuration file is too long.

{------------------------------------------------------------------------------}
{ Spooler API related                                                          }
{ Error codes from BASE+50 to BASE+79                                          }
{------------------------------------------------------------------------------}
 Const
    NERR_QNotFound          = NERR_BASE+50;  // The printer does not exist.
    NERR_JobNotFound        = NERR_BASE+51;  // The print job does not exist.
    NERR_DestNotFound       = NERR_BASE+52;  // The printer destination cannot be found.
    NERR_DestExists         = NERR_BASE+53;  // The printer destination already exists.
    NERR_QExists            = NERR_BASE+54;  // The printer queue already exists.
    NERR_QNoRoom            = NERR_BASE+55;  // No more printers can be added.
    NERR_JobNoRoom          = NERR_BASE+56;  // No more print jobs can be added.
    NERR_DestNoRoom         = NERR_BASE+57;  // No more printer destinations can be added.
    NERR_DestIdle           = NERR_BASE+58;  // This printer destination is idle and cannot accept control operations.
    NERR_DestInvalidOp      = NERR_BASE+59;  // This printer destination request contains an invalid control function.
    NERR_ProcNoRespond      = NERR_BASE+60;  // The print processor is not responding.
    NERR_SpoolerNotLoaded   = NERR_BASE+61;  // The spooler is not running.
    NERR_DestInvalidState   = NERR_BASE+62;  // This operation cannot be performed on the print destination in its current state.
    NERR_QInvalidState      = NERR_BASE+63;  // This operation cannot be performed on the printer queue in its current state.
    NERR_JobInvalidState    = NERR_BASE+64;  // This operation cannot be performed on the print job in its current state.
    NERR_SpoolNoMemory      = NERR_BASE+65;  // A spooler memory allocation failure occurred.
    NERR_DriverNotFound     = NERR_BASE+66;  // The device driver does not exist.
    NERR_DataTypeInvalid    = NERR_BASE+67;  // The data type is not supported by the print processor.
    NERR_ProcNotFound       = NERR_BASE+68;  // The print processor is not installed.

{------------------------------------------------------------------------------}
{ Service API related                                                          }
{ Error codes from BASE+80 to BASE+99                                          }
{------------------------------------------------------------------------------}
 Const
    NERR_ServiceTableLocked = NERR_BASE+80; // The service database is locked.
    NERR_ServiceTableFull   = NERR_BASE+81; // The service table is full.
    NERR_ServiceInstalled   = NERR_BASE+82; // The requested service has already been started.
    NERR_ServiceEntryLocked = NERR_BASE+83; // The service does not respond to control actions.
    NERR_ServiceNotInstalled = NERR_BASE+84; // The service has not been started.
    NERR_BadServiceName     = NERR_BASE+85; // The service name is invalid.
    NERR_ServiceCtlTimeout  = NERR_BASE+86; // The service is not responding to the control function.
    NERR_ServiceCtlBusy     = NERR_BASE+87; // The service control is busy.
    NERR_BadServiceProgName = NERR_BASE+88; // The configuration file contains an invalid service program name.
    NERR_ServiceNotCtrl     = NERR_BASE+89; // The service could not be controlled in its present state.
    NERR_ServiceKillProc    = NERR_BASE+90; // The service ended abnormally.
    NERR_ServiceCtlNotValid = NERR_BASE+91; // The requested pause or stop is not valid for this service.
    NERR_NotInDispatchTbl   = NERR_BASE+92; // The service control dispatcher could not find the service name in the dispatch table.
    NERR_BadControlRecv     = NERR_BASE+93; // The service control dispatcher pipe read failed.
    NERR_ServiceNotStarting = NERR_BASE+94; // A thread for the new service could not be created.

{------------------------------------------------------------------------------}
{ Wksta and Logon API related                                                  }
{ Error codes from BASE+100 to BASE+118                                        }
{------------------------------------------------------------------------------}
 Const
    NERR_AlreadyLoggedOn    = NERR_BASE+100; // This workstation is already logged on to the local-area network.
    NERR_NotLoggedOn        = NERR_BASE+101; // The workstation is not logged on to the local-area network.
    NERR_BadUsername        = NERR_BASE+102; // The user name or group name parameter is invalid.
    NERR_BadPassword        = NERR_BASE+103; // The password parameter is invalid.
    NERR_UnableToAddName_W  = NERR_BASE+104; // @W The logon processor did not add the message alias.
    NERR_UnableToAddName_F  = NERR_BASE+105; // The logon processor did not add the message alias.
    NERR_UnableToDelName_W  = NERR_BASE+106; // @W The logoff processor did not delete the message alias.
    NERR_UnableToDelName_F  = NERR_BASE+107; // The logoff processor did not delete the message alias.
    // UNUSED BASE+108
    NERR_LogonsPaused       = NERR_BASE+109; // Network logons are paused.
    NERR_LogonServerConflict = NERR_BASE+110;// A centralized logon-server conflict occurred.
    NERR_LogonNoUserPath    = NERR_BASE+111; // The server is configured without a valid user path.
    NERR_LogonScriptError   = NERR_BASE+112; // An error occurred while loading or running the logon script.
    // UNUSED BASE+113
    NERR_StandaloneLogon    = NERR_BASE+114; // The logon server was not specified.  Your computer will be logged on as STANDALONE.
    NERR_LogonServerNotFound = NERR_BASE+115; // The logon server could not be found.
    NERR_LogonDomainExists  = NERR_BASE+116; // There is already a logon domain for this computer.
    NERR_NonValidatedLogon  = NERR_BASE+117; // The logon server could not validate the logon.

{------------------------------------------------------------------------------}
{ ACF API related (access, user, group)                                        }
{ Error codes from BASE+119 to BASE+149                                        }
{------------------------------------------------------------------------------}
 Const
    NERR_ACFNotFound        = NERR_BASE+119; // The security database could not be found.
    NERR_GroupNotFound      = NERR_BASE+120; // The group name could not be found.
    NERR_UserNotFound       = NERR_BASE+121; // The user name could not be found.
    NERR_ResourceNotFound   = NERR_BASE+122; // The resource name could not be found.
    NERR_GroupExists        = NERR_BASE+123; // The group already exists.
    NERR_UserExists         = NERR_BASE+124; // The user account already exists.
    NERR_ResourceExists     = NERR_BASE+125; // The resource permission list already exists.
    NERR_NotPrimary         = NERR_BASE+126; // This operation is only allowed on the primary domain controller of the domain.
    NERR_ACFNotLoaded       = NERR_BASE+127; // The security database has not been started.
    NERR_ACFNoRoom          = NERR_BASE+128; // There are too many names in the user accounts database.
    NERR_ACFFileIOFail      = NERR_BASE+129; // A disk I/O failure occurred.
    NERR_ACFTooManyLists    = NERR_BASE+130; // The limit of 64 entries per resource was exceeded.
    NERR_UserLogon          = NERR_BASE+131; // Deleting a user with a session is not allowed.
    NERR_ACFNoParent        = NERR_BASE+132; // The parent directory could not be located.
    NERR_CanNotGrowSegment  = NERR_BASE+133; // Unable to add to the security database session cache segment.
    NERR_SpeGroupOp         = NERR_BASE+134; // This operation is not allowed on this special group.
    NERR_NotInCache         = NERR_BASE+135; // This user is not cached in user accounts database session cache.
    NERR_UserInGroup        = NERR_BASE+136; // The user already belongs to this group.
    NERR_UserNotInGroup     = NERR_BASE+137; // The user does not belong to this group.
    NERR_AccountUndefined   = NERR_BASE+138; // This user account is undefined.
    NERR_AccountExpired     = NERR_BASE+139; // This user account has expired.
    NERR_InvalidWorkstation = NERR_BASE+140; // The user is not allowed to log on from this workstation.
    NERR_InvalidLogonHours  = NERR_BASE+141; // The user is not allowed to log on at this time.
    NERR_PasswordExpired    = NERR_BASE+142; // The password of this user has expired.
    NERR_PasswordCantChange = NERR_BASE+143; // The password of this user cannot change.
    NERR_PasswordHistConflict = NERR_BASE+144; // This password cannot be used now.
    NERR_PasswordTooShort   = NERR_BASE+145; // The password is shorter than required.
    NERR_PasswordTooRecent  = NERR_BASE+146; // The password of this user is too recent to change.
    NERR_InvalidDatabase    = NERR_BASE+147; // The security database is corrupted.
    NERR_DatabaseUpToDate   = NERR_BASE+148; // No updates are necessary to this replicant network/local security database.
    NERR_SyncRequired       = NERR_BASE+149; // This replicant database is outdated; synchronization is required.

{------------------------------------------------------------------------------}
{ Use API related                                                              }
{ Error codes from BASE+150 to BASE+169                                        }
{------------------------------------------------------------------------------}
 Const
    NERR_UseNotFound        = NERR_BASE+150; // The network connection could not be found.
    NERR_BadAsgType         = NERR_BASE+151; // This asg_type is invalid.
    NERR_DeviceIsShared     = NERR_BASE+152; // This device is currently being shared.

{------------------------------------------------------------------------------}
{ Message Server related                                                       }
{ Error codes BASE+170 to BASE+209                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_NoComputerName     = NERR_BASE+170; // The computer name could not be added as a message alias.  The name may already exist on the network.
    NERR_MsgAlreadyStarted  = NERR_BASE+171; // The Messenger service is already started.
    NERR_MsgInitFailed      = NERR_BASE+172; // The Messenger service failed to start.
    NERR_NameNotFound       = NERR_BASE+173; // The message alias could not be found on the network.
    NERR_AlreadyForwarded   = NERR_BASE+174; // This message alias has already been forwarded.
    NERR_AddForwarded       = NERR_BASE+175; // This message alias has been added but is still forwarded.
    NERR_AlreadyExists      = NERR_BASE+176; // This message alias already exists locally.
    NERR_TooManyNames       = NERR_BASE+177; // The maximum number of added message aliases has been exceeded.
    NERR_DelComputerName    = NERR_BASE+178; // The computer name could not be deleted.
    NERR_LocalForward       = NERR_BASE+179; // Messages cannot be forwarded back to the same workstation.
    NERR_GrpMsgProcessor    = NERR_BASE+180; // An error occurred in the domain message processor.
    NERR_PausedRemote       = NERR_BASE+181; // The message was sent, but the recipient has paused the Messenger service.
    NERR_BadReceive         = NERR_BASE+182; // The message was sent but not received.
    NERR_NameInUse          = NERR_BASE+183; // The message alias is currently in use. Try again later.
    NERR_MsgNotStarted      = NERR_BASE+184; // The Messenger service has not been started.
    NERR_NotLocalName       = NERR_BASE+185; // The name is not on the local computer.
    NERR_NoForwardName      = NERR_BASE+186; // The forwarded message alias could not be found on the network.
    NERR_RemoteFull         = NERR_BASE+187; // The message alias table on the remote station is full.
    NERR_NameNotForwarded   = NERR_BASE+188; // Messages for this alias are not currently being forwarded.
    NERR_TruncatedBroadcast = NERR_BASE+189; // The broadcast message was truncated.
    NERR_InvalidDevice      = NERR_BASE+194; // This is an invalid device name.
    NERR_WriteFault         = NERR_BASE+195; // A write fault occurred.
    // UNUSED BASE+196
    NERR_DuplicateName      = NERR_BASE+197; // A duplicate message alias exists on the network.
    NERR_DeleteLater        = NERR_BASE+198; // @W This message alias will be deleted later.
    NERR_IncompleteDel      = NERR_BASE+199; // The message alias was not successfully deleted from all networks.
    NERR_MultipleNets       = NERR_BASE+200; // This operation is not supported on computers with multiple networks.

{------------------------------------------------------------------------------}
{ Server API related                                                           }
{ Error codes BASE+210 to BASE+229                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_NetNameNotFound    = NERR_BASE+210; // This shared resource does not exist.
    NERR_DeviceNotShared    = NERR_BASE+211; // This device is not shared.
    NERR_ClientNameNotFound = NERR_BASE+212; // A session does not exist with that computer name.
    NERR_FileIdNotFound     = NERR_BASE+214; // There is not an open file with that identification number.
    NERR_ExecFailure        = NERR_BASE+215; // A failure occurred when executing a remote administration command.
    NERR_TmpFile            = NERR_BASE+216; // A failure occurred when opening a remote temporary file.
    NERR_TooMuchData        = NERR_BASE+217; // The data returned from a remote administration command has been truncated to 64K.
    NERR_DeviceShareConflict = NERR_BASE+218; // This device cannot be shared as both a spooled and a non-spooled resource.
    NERR_BrowserTableIncomplete = NERR_BASE+219; // The information in the list of servers may be incorrect.
    NERR_NotLocalDomain     = NERR_BASE+220; // The computer is not active in this domain.
    NERR_IsDfsShare         = NERR_BASE+221; // The share must be removed from the Distributed File System before it can be deleted.

{------------------------------------------------------------------------------}
{ CharDev API related                                                          }
{ Error codes BASE+230 to BASE+249                                             }
{------------------------------------------------------------------------------}
 Const
    // UNUSED BASE+230
    NERR_DevInvalidOpCode   = NERR_BASE+231; // The operation is invalid for this device.
    NERR_DevNotFound        = NERR_BASE+232; // This device cannot be shared.
    NERR_DevNotOpen         = NERR_BASE+233; // This device was not open.
    NERR_BadQueueDevString  = NERR_BASE+234; // This device name list is invalid.
    NERR_BadQueuePriority   = NERR_BASE+235; // The queue priority is invalid.
    NERR_NoCommDevs         = NERR_BASE+237; // There are no shared communication devices.
    NERR_QueueNotFound      = NERR_BASE+238; // The queue you specified does not exist.
    NERR_BadDevString       = NERR_BASE+240; // This list of devices is invalid.
    NERR_BadDev             = NERR_BASE+241; // The requested device is invalid.
    NERR_InUseBySpooler     = NERR_BASE+242; // This device is already in use by the spooler.
    NERR_CommDevInUse       = NERR_BASE+243; // This device is already in use as a communication device.

{------------------------------------------------------------------------------}
{ NetICanonicalize and NetIType and NetIMakeLMFileName                         }
{ NetIListCanon and NetINameCheck                                              }
{ Error codes BASE+250 to BASE+269                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_InvalidComputer   = NERR_BASE+251; // This computer name is invalid.
    // UNUSED BASE+252
    // UNUSED BASE+253
    NERR_MaxLenExceeded    = NERR_BASE+254; // The string and prefix specified are too long.
    // UNUSED BASE+255
    NERR_BadComponent      = NERR_BASE+256; // This path component is invalid.
    NERR_CantType          = NERR_BASE+257; // Could not determine the type of input.
    // UNUSED BASE+258
    // UNUSED BASE+259
    NERR_TooManyEntries    = NERR_BASE+262; // The buffer for types is not big enough.

{------------------------------------------------------------------------------}
{ NetProfile                                                                   }
{ Error codes BASE+270 to BASE+276                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_ProfileFileTooBig  = NERR_BASE+270; // Profile files cannot exceed 64K.
    NERR_ProfileOffset      = NERR_BASE+271; // The start offset is out of range.
    NERR_ProfileCleanup     = NERR_BASE+272; // The system cannot delete current connections to network resources.
    NERR_ProfileUnknownCmd  = NERR_BASE+273; // The system was unable to parse the command line in this file.
    NERR_ProfileLoadErr     = NERR_BASE+274; // An error occurred while loading the profile file.
    NERR_ProfileSaveErr     = NERR_BASE+275; // @W Errors occurred while saving the profile file.  The profile was partially saved.

{------------------------------------------------------------------------------}
{ NetAudit and NetErrorLog                                                     }
{ Error codes BASE+277 to BASE+279                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_LogOverflow           = NERR_BASE+277; // Log file %1 is full.
    NERR_LogFileChanged        = NERR_BASE+278; // This log file has changed between reads.
    NERR_LogFileCorrupt        = NERR_BASE+279; // Log file %1 is corrupt.

{------------------------------------------------------------------------------}
{ NetRemote related                                                            }
{ Error codes BASE+280 to BASE+299                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_SourceIsDir        = NERR_BASE+280; // The source path cannot be a directory.
    NERR_BadSource          = NERR_BASE+281; // The source path is illegal.
    NERR_BadDest            = NERR_BASE+282; // The destination path is illegal.
    NERR_DifferentServers   = NERR_BASE+283; // The source and destination paths are on different servers.
    // UNUSED BASE+284
    NERR_RunSrvPaused       = NERR_BASE+285; // The Run server you requested is paused.
    // UNUSED BASE+286
    // UNUSED BASE+287
    // UNUSED BASE+288
    NERR_ErrCommRunSrv      = NERR_BASE+289; // An error occurred when communicating with a Run server.
    // UNUSED BASE+290
    NERR_ErrorExecingGhost  = NERR_BASE+291; // An error occurred when starting a background process.
    NERR_ShareNotFound      = NERR_BASE+292; // The shared resource you are connected to could not be found.
    // UNUSED BASE+293
    // UNUSED BASE+294

{------------------------------------------------------------------------------}
{ NetWksta.sys (redir; //returned error codes.                                 }
{ Error codes BASE+300 to BASE+329                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_InvalidLana        = NERR_BASE+300; // The LAN adapter number is invalid.
    NERR_OpenFiles          = NERR_BASE+301; // There are open files on the connection.
    NERR_ActiveConns        = NERR_BASE+302; // Active connections still exist.
    NERR_BadPasswordCore    = NERR_BASE+303; // This share name or password is invalid.
    NERR_DevInUse           = NERR_BASE+304; // The device is being accessed by an active process.
    NERR_LocalDrive         = NERR_BASE+305; // The drive letter is in use locally.

{------------------------------------------------------------------------------}
{ Alert error codes.                                                           }
{ Error codes BASE+330 to BASE+339                                             }
{------------------------------------------------------------------------------}
 Const
  NERR_AlertExists        = NERR_BASE+330; // The specified client is already registered for the specified event.
  NERR_TooManyAlerts      = NERR_BASE+331; // The alert table is full.
  NERR_NoSuchAlert        = NERR_BASE+332; // An invalid or nonexistent alert name was raised.
  NERR_BadRecipient       = NERR_BASE+333; // The alert recipient is invalid.
  NERR_AcctLimitExceeded  = NERR_BASE+334; // A user's session with this server has been deleted
                                           // because the user's logon hours are no longer valid.

{------------------------------------------------------------------------------}
{ Additional Error and Audit log codes.                                        }
{ Error codes BASE+340 to BASE+343                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_InvalidLogSeek     = NERR_BASE+340; // The log file does not contain the requested Record number.
    // UNUSED BASE+341
    // UNUSED BASE+342
    // UNUSED BASE+343

{------------------------------------------------------------------------------}
{ Additional UAS and NETLOGON codes                                            }
{ Error codes BASE+350 to BASE+359                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_BadUasConfig       = NERR_BASE+350; // The user accounts database is not configured correctly.
    NERR_InvalidUASOp       = NERR_BASE+351; // This operation is not permitted when the Netlogon service is running.
    NERR_LastAdmin          = NERR_BASE+352; // This operation is not allowed on the last administrative account.
    NERR_DCNotFound         = NERR_BASE+353; // Could not find domain controller for this domain.
    NERR_LogonTrackingError = NERR_BASE+354; // Could not set logon information for this user.
    NERR_NetlogonNotStarted = NERR_BASE+355; // The Netlogon service has not been started.
    NERR_CanNotGrowUASFile  = NERR_BASE+356; // Unable to add to the user accounts database.
    NERR_TimeDiffAtDC       = NERR_BASE+357; // This server's clock is not synchronized with the primary domain controller's clock.
    NERR_PasswordMismatch   = NERR_BASE+358; // A password mismatch has been detected.

{------------------------------------------------------------------------------}
{ Server Integration error codes.                                              }
{ Error codes BASE+360 to BASE+369                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_NoSuchServer       = NERR_BASE+360; // The server identification does not specify a valid server.
    NERR_NoSuchSession      = NERR_BASE+361; // The session identification does not specify a valid session.
    NERR_NoSuchConnection   = NERR_BASE+362; // The connection identification does not specify a valid connection.
    NERR_TooManyServers     = NERR_BASE+363; // There is no space for another entry in the table of available servers.
    NERR_TooManySessions    = NERR_BASE+364; // The server has reached the maximum number of sessions it supports.
    NERR_TooManyConnections = NERR_BASE+365; // The server has reached the maximum number of connections it supports.
    NERR_TooManyFiles       = NERR_BASE+366; // The server cannot open more files because it has reached its maximum number.
    NERR_NoAlternateServers = NERR_BASE+367; // There are no alternate servers registered on this server.
    // UNUSED BASE+368
    // UNUSED BASE+369
    NERR_TryDownLevel       = NERR_BASE+370; // Try down-level (remote admin protocol; //version of API instead.

{------------------------------------------------------------------------------}
{ UPS error codes.                                                             }
{ Error codes BASE+380 to BASE+384                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_UPSDriverNotStarted    = NERR_BASE+380; // The UPS driver could not be accessed by the UPS service.
    NERR_UPSInvalidConfig       = NERR_BASE+381; // The UPS service is not configured correctly.
    NERR_UPSInvalidCommPort     = NERR_BASE+382; // The UPS service could not access the specified Comm Port.
    NERR_UPSSignalAsserted      = NERR_BASE+383; // The UPS indicated a line fail or low battery situation. Service not started.
    NERR_UPSShutdownFailed      = NERR_BASE+384; // The UPS service failed to perform a system shut down.

{------------------------------------------------------------------------------}
{ Remoteboot error codes.                                                      }
{                                                                              }
{ Error codes BASE+400 To BASE+419                                             }
{                                                                              }
{ Error codes 400 - 405 are used by RPLBOOT.SYS.                               }
{ Error codes 403, 407 - 416 are used by RPLLOADR.COM,                         }
{ Error code 417 is the alerter message of REMOTEBOOT (RPLSERVR.EXE).          }
{ Error code 418 is for when REMOTEBOOT can't start                            }
{ Error code 419 is for a disallowed 2nd rpl connection                        }
{------------------------------------------------------------------------------}
 Const
    NERR_BadDosRetCode      = NERR_BASE+400; // The program below returned an MS-DOS error code:
    NERR_ProgNeedsExtraMem  = NERR_BASE+401; // The program below needs more memory:
    NERR_BadDosFunction     = NERR_BASE+402; // The program below called an unsupported MS-DOS function:
    NERR_RemoteBootFailed   = NERR_BASE+403; // The workstation failed to boot.
    NERR_BadFileCheckSum    = NERR_BASE+404; // The file below is corrupt.
    NERR_NoRplBootSystem    = NERR_BASE+405; // No loader is specified in the boot-block definition file.
    NERR_RplLoadrNetBiosErr = NERR_BASE+406; // NetBIOS returned an error: The NCB and SMB are dumped above.
    NERR_RplLoadrDiskErr    = NERR_BASE+407; // A disk I/O error occurred.
    NERR_ImageParamErr      = NERR_BASE+408; // Image parameter substitution failed.
    NERR_TooManyImageParams = NERR_BASE+409; // Too many image parameters cross disk sector boundaries.
    NERR_NonDosFloppyUsed   = NERR_BASE+410; // The image was not generated from an MS-DOS diskette formatted with /S.
    NERR_RplBootRestart     = NERR_BASE+411; // Remote boot will be restarted later.
    NERR_RplSrvrCallFailed  = NERR_BASE+412; // The call to the Remoteboot server failed.
    NERR_CantConnectRplSrvr = NERR_BASE+413; // Cannot connect to the Remoteboot server.
    NERR_CantOpenImageFile  = NERR_BASE+414; // Cannot open image file on the Remoteboot server.
    NERR_CallingRplSrvr     = NERR_BASE+415; // Connecting to the Remoteboot server...
    NERR_StartingRplBoot    = NERR_BASE+416; // Connecting to the Remoteboot server...
    NERR_RplBootServiceTerm = NERR_BASE+417; // Remote boot service was stopped; check the error log for the cause of the problem.
    NERR_RplBootStartFailed = NERR_BASE+418; // Remote boot startup failed; check the error log for the cause of the problem.
    NERR_RPL_CONNECTED      = NERR_BASE+419; // A second connection to a Remoteboot resource is not allowed.

{------------------------------------------------------------------------------}
{ FTADMIN API error codes                                                      }
{ Error codes BASE+425 to BASE+434                                             }
{ Currently not used in NT                                                     }
{------------------------------------------------------------------------------}
{ Browser service API error codes                                              }
{ Error codes BASE+450 to BASE+475                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_BrowserConfiguredToNotRun     = NERR_BASE+450; // The browser service was configured with MaintainServerList=No.

{------------------------------------------------------------------------------}
{ Additional Remoteboot error codes.                                           }
{ Error codes BASE+510 To BASE+550                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_RplNoAdaptersStarted          = NERR_BASE+510; //Service failed to start since none of the network adapters started with this service.
    NERR_RplBadRegistry                = NERR_BASE+511; //Service failed to start due to bad startup information in the registry.
    NERR_RplBadDatabase                = NERR_BASE+512; //Service failed to start because its database is absent or corrupt.
    NERR_RplRplfilesShare              = NERR_BASE+513; //Service failed to start because RPLFILES share is absent.
    NERR_RplNotRplServer               = NERR_BASE+514; //Service failed to start because RPLUSER group is absent.
    NERR_RplCannotEnum                 = NERR_BASE+515; //Cannot enumerate service records.
    NERR_RplWkstaInfoCorrupted         = NERR_BASE+516; //Workstation Record information has been corrupted.
    NERR_RplWkstaNotFound              = NERR_BASE+517; //Workstation Record was not found.
    NERR_RplWkstaNameUnavailable       = NERR_BASE+518; //Workstation name is in use by some other workstation.
    NERR_RplProfileInfoCorrupted       = NERR_BASE+519; //Profile Record information has been corrupted.
    NERR_RplProfileNotFound            = NERR_BASE+520; //Profile Record was not found.
    NERR_RplProfileNameUnavailable     = NERR_BASE+521; //Profile name is in use by some other profile.
    NERR_RplProfileNotEmpty            = NERR_BASE+522; //There are workstations using this profile.
    NERR_RplConfigInfoCorrupted        = NERR_BASE+523; //Configuration Record information has been corrupted.
    NERR_RplConfigNotFound             = NERR_BASE+524; //Configuration Record was not found.
    NERR_RplAdapterInfoCorrupted       = NERR_BASE+525; //Adapter id Record information has been corrupted.
    NERR_RplInternal                   = NERR_BASE+526; //An internal service error has occurred.
    NERR_RplVendorInfoCorrupted        = NERR_BASE+527; //Vendor id Record information has been corrupted.
    NERR_RplBootInfoCorrupted          = NERR_BASE+528; //Boot block Record information has been corrupted.
    NERR_RplWkstaNeedsUserAcct         = NERR_BASE+529; //The user account for this workstation Record is missing.
    NERR_RplNeedsRPLUSERAcct           = NERR_BASE+530; //The RPLUSER local group could not be found.
    NERR_RplBootNotFound               = NERR_BASE+531; //Boot block Record was not found.
    NERR_RplIncompatibleProfile        = NERR_BASE+532; //Chosen profile is incompatible with this workstation.
    NERR_RplAdapterNameUnavailable     = NERR_BASE+533; //Chosen network adapter id is in use by some other workstation.
    NERR_RplConfigNotEmpty             = NERR_BASE+534; //There are profiles using this configuration.
    NERR_RplBootInUse                  = NERR_BASE+535; //There are workstations, profiles or configurations using this boot block.
    NERR_RplBackupDatabase             = NERR_BASE+536; //Service failed to backup Remoteboot database.
    NERR_RplAdapterNotFound            = NERR_BASE+537; //Adapter Record was not found.
    NERR_RplVendorNotFound             = NERR_BASE+538; //Vendor Record was not found.
    NERR_RplVendorNameUnavailable      = NERR_BASE+539; //Vendor name is in use by some other vendor Record.
    NERR_RplBootNameUnavailable        = NERR_BASE+540; //(boot name, vendor id; //is in use by some other boot block Record.
    NERR_RplConfigNameUnavailable      = NERR_BASE+541; //Configuration name is in use by some other configuration.

{------------------------------------------------------------------------------}
{ Dfs API error codes.                                                         }
{ Error codes BASE+560 To BASE+590                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_DfsInternalCorruption         = NERR_BASE+560; //The internal database maintained by the Dfs service is corrupt
    NERR_DfsVolumeDataCorrupt          = NERR_BASE+561; //One of the records in the internal Dfs database is corrupt
    NERR_DfsNoSuchVolume               = NERR_BASE+562; //There is no volume whose entry path matches the input Entry Path
    NERR_DfsVolumeAlreadyExists        = NERR_BASE+563; //A volume with the given name already exists
    NERR_DfsAlreadyShared              = NERR_BASE+564; //The server share specified is already shared in the Dfs
    NERR_DfsNoSuchShare                = NERR_BASE+565; //The indicated server share does not support the indicated Dfs volume
    NERR_DfsNotALeafVolume             = NERR_BASE+566; //The operation is not valid on a non-leaf volume
    NERR_DfsLeafVolume                 = NERR_BASE+567; //The operation is not valid on a leaf volume
    NERR_DfsVolumeHasMultipleServers   = NERR_BASE+568; //The operation is ambiguous because the volume has multiple servers
    NERR_DfsCantCreateJunctionPoint    = NERR_BASE+569; //Unable to create a junction point
    NERR_DfsServerNotDfsAware          = NERR_BASE+570; //The server is not Dfs Aware
    NERR_DfsBadRenamePath              = NERR_BASE+571; //The specified rename target path is invalid
    NERR_DfsVolumeIsOffline            = NERR_BASE+572; //The specified Dfs volume is offline
    NERR_DfsNoSuchServer               = NERR_BASE+573; //The specified server is not a server for this volume
    NERR_DfsCyclicalName               = NERR_BASE+574; //A cycle in the Dfs name was detected
    NERR_DfsNotSupportedInServerDfs    = NERR_BASE+575; //The operation is not supported on a server-based Dfs
    NERR_DfsInternalError              = NERR_BASE+590; //Dfs internal error

{------------------------------------------------------------------------------}
{ Net setup error codes.                                                       }
{ Error codes BASE+591 to BASE+595                                             }
{------------------------------------------------------------------------------}
 Const
    NERR_SetupAlreadyJoined            = NERR_BASE+591; //This machine is already joined to a domain.
    NERR_SetupNotJoined                = NERR_BASE+592; //This machine is not currently joined to a domain.
    NERR_SetupDomainController         = NERR_BASE+593; //This machine is a domain controller and cannot be unjoined from a domain.

{------------------------------------------------------------------------------}
{ WARNING                                                                      }
{ The range 2750-2799 has been                                                 }
{ allocated to the IBM LAN Server                                              }
{------------------------------------------------------------------------------}
{ WARNING                                                                      }
{ The range 2900-2999 has been                                                 }
{ reserved for Microsoft OEMs                                                  }
{------------------------------------------------------------------------------}
 Const
    MAX_NERR                = NERR_BASE+899; // This is the last error in NERR range.

{------------------------------------------------------------------------------}
{ WARNING:  Do not exceed MAX_NERR; values above this are used by              }
{           other error code ranges (errlog.h, service.h, apperr.h).           }
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{ BUILD Version: 0001    // Increment this if a change has global effects      }
{ Copyright 1991-1998 Microsoft Corporation                                    }
{ Module Name: lmapibuf.h                                                      }
{ Abstract: This file contains information about NetApiBuffer APIs.            }
{ Environment: User Mode - Win32                                               }
{ Notes: You must include LMCONS.H before this file, since this file depends   }
{     on values defined in LMCONS.H.                                           }
{------------------------------------------------------------------------------}
 Var
   // Function Prototypes
   NetApiBufferAllocate: Function(ByteCount: DWORD; Var Buffer: Pointer): NET_API_STATUS; StdCall;
   NetApiBufferFree:  Function(Buffer: Pointer): NET_API_STATUS; StdCall;
   NetApiBufferReallocate: Function(OldBuffer: Pointer; NewByteCount: DWORD; Var NewBuffer: Pointer): NET_API_STATUS; StdCall;
   NetApiBufferSize: Function(Buffer: Pointer; Var ByteCount: DWORD): NET_API_STATUS; StdCall;

   // The following private function will go away eventually.
   // Call NetApiBufferAllocate instead.
   Function NetapipBufferAllocate(ByteCount: DWORD; Var Buffer: Pointer): NET_API_STATUS; StdCall;

{------------------------------------------------------------------------------}
{ BUILD Version: 0002                                                          }
{ Copyright 1991-1998 Microsoft Corporation                                    }
{ Module Name: lmaccess.h                                                      }
{ Abstract: This file contains structures, function prototypes, and definitions}
{       for the NetUser, NetUserModals, NetGroup, NetAccess, and NetLogon API. }
{ Environment: User Mode - Win32                                               }
{ Notes: You must include NETCONS.H before this file, since this file depends  }
{        on values defined in NETCONS.H.                                       }
{------------------------------------------------------------------------------}
 Var
   // Function Prototypes - User
   NetUserAdd: Function(ServerName: PWideChar; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
   NetUserEnum: Function(ServerName: PWideChar; level, filter: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
   NetUserGetInfo: Function(ServerName: PWideChar; Const UserName: WideString; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
   NetUserSetInfo: Function(ServerName: PWideChar; Const UserName: WideString; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
   NetUserDel: Function(ServerName: PWideChar; Const UserName: WideString): NET_API_STATUS; StdCall;
   NetUserGetGroups: Function(ServerName: PWideChar; Const UserName: WideString; level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD): NET_API_STATUS; StdCall;
   NetUserSetGroups: Function(ServerName: PWideChar; Const UserName: WideString; level: DWORD; buf: PByte; num_entries: DWORD): NET_API_STATUS; StdCall;
   NetUserGetLocalGroups: Function(ServerName: PWideChar; Const UserName: WideString; level, flags: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD): NET_API_STATUS; StdCall;
   NetUserModalsGet: Function(ServerName: PWideChar; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
   NetUserModalsSet: Function(ServerName: PWideChar; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
   NetUserChangePassword: Function(ServerName, UserName: PWideChar; Const oldpassword, newpassword: WideString): NET_API_STATUS; StdCall;

{------------------------------------------------------------------------------}
{ Data Structures - User                                                       }
{------------------------------------------------------------------------------}
 Type
    PUserInfo0 = ^TUserInfo0;
    TUserInfo0 = Record
      usri0_name: WideString;
    End;

    PUserInfo1 = ^TUserInfo1;
    TUserInfo1 = Record
      usri1_name: WideString;
      usri1_password: WideString;
      usri1_password_age: DWORD;
      usri1_priv: DWORD;
      usri1_home_dir: WideString;
      usri1_comment: WideString;
      usri1_flags: DWORD;
      usri1_script_path: WideString;
    End;

    PUserInfo2 = ^TUserInfo2;
    TUserInfo2 = Record
      usri2_name: WideString;
      usri2_password: WideString;
      usri2_password_age: DWORD;
      usri2_priv: DWORD;
      usri2_home_dir: WideString;
      usri2_comment: WideString;
      usri2_flags: DWORD;
      usri2_script_path: WideString;
      usri2_auth_flags: DWORD;
      usri2_full_name: WideString;
      usri2_usr_comment: WideString;
      usri2_parms: WideString;
      usri2_workstations: WideString;
      usri2_last_logon: DWORD;
      usri2_last_logoff: DWORD;
      usri2_acct_expires: DWORD;
      usri2_max_storage: DWORD;
      usri2_units_per_week: DWORD;
      usri2_logon_hours: PByte;
      usri2_bad_pw_count: DWORD;
      usri2_num_logons: DWORD;
      usri2_logon_server: WideString;
      usri2_country_code: DWORD;
      usri2_code_page: DWORD;
    End;

    PUserInfo3 = ^TUserInfo3;
    TUserInfo3 = Record
      usri3_name: WideString;
      usri3_password: WideString;
      usri3_password_age: DWORD;
      usri3_priv: DWORD;
      usri3_home_dir: WideString;
      usri3_comment: WideString;
      usri3_flags: DWORD;
      usri3_script_path: WideString;
      usri3_auth_flags: DWORD;
      usri3_full_name: WideString;
      usri3_usr_comment: WideString;
      usri3_parms: WideString;
      usri3_workstations: WideString;
      usri3_last_logon: DWORD;
      usri3_last_logoff: DWORD;
      usri3_acct_expires: DWORD;
      usri3_max_storage: DWORD;
      usri3_units_per_week: DWORD;
      usri3_logon_hours: PByte;
      usri3_bad_pw_count: DWORD;
      usri3_num_logons: DWORD;
      usri3_logon_server: WideString;
      usri3_country_code: DWORD;
      usri3_code_page: DWORD;
      usri3_user_id: DWORD;
      usri3_primary_group_id: DWORD;
      usri3_profile: WideString;
      usri3_home_dir_drive: WideString;
      usri3_password_expired: DWORD;
    End;

    PUserInfo10 = ^TUserInfo10;
    TUserInfo10 = Record
      usri10_name: WideString;
      usri10_comment: WideString;
      usri10_usr_comment: WideString;
      usri10_full_name: WideString;
    End;

    PUserInfo11 = ^TUserInfo11;
    TUserInfo11 = Record
      usri11_name: WideString;
      usri11_comment: WideString;
      usri11_usr_comment: WideString;
      usri11_full_name: WideString;
      usri11_priv: DWORD;
      usri11_auth_flags: DWORD;
      usri11_password_age: DWORD;
      usri11_home_dir: WideString;
      usri11_parms: WideString;
      usri11_last_logon: DWORD;
      usri11_last_logoff: DWORD;
      usri11_bad_pw_count: DWORD;
      usri11_num_logons: DWORD;
      usri11_logon_server: WideString;
      usri11_country_code: DWORD;
      usri11_workstations: WideString;
      usri11_max_storage: DWORD;
      usri11_units_per_week: DWORD;
      usri11_logon_hours: PByte;
      usri11_code_page: DWORD;
    End;

    PUserInfo20 = ^TUserInfo20;
    TUserInfo20 = Record
      usri20_name: WideString;
      usri20_full_name: WideString;
      usri20_comment: WideString;
      usri20_flags: DWORD;
      usri20_user_id: DWORD;
    End;

    PUserInfo21 = ^TUserInfo21;
    TUserInfo21 = Record
      usri21_password: Array[0..ENCRYPTED_PWLEN-1] Of Byte;
    End;

    PUserInfo22 = ^TUserInfo22;
    TUserInfo22 = Record
      usri22_name: WideString;
      usri22_password: Array[0..ENCRYPTED_PWLEN-1] Of Byte;
      usri22_password_age: DWORD;
      usri22_priv: DWORD;
      usri22_home_dir: WideString;
      usri22_comment: WideString;
      usri22_flags: DWORD;
      usri22_script_path: WideString;
      usri22_auth_flags: DWORD;
      usri22_full_name: WideString;
      usri22_usr_comment: WideString;
      usri22_parms: WideString;
      usri22_workstations: WideString;
      usri22_last_logon: DWORD;
      usri22_last_logoff: DWORD;
      usri22_acct_expires: DWORD;
      usri22_max_storage: DWORD;
      usri22_units_per_week: DWORD;
      usri22_logon_hours: PByte;
      usri22_bad_pw_count: DWORD;
      usri22_num_logons: DWORD;
      usri22_logon_server: WideString;
      usri22_country_code: DWORD;
      usri22_code_page: DWORD;
    End;

    PUserInfo1003 = ^TUserInfo1003;
    TUserInfo1003 = Record
      usri1003_password: WideString;
    End;

    PUserInfo1005 = ^TUserInfo1005;
    TUserInfo1005 = Record
      usri1005_priv: DWORD;
    End;

    PUserInfo1006 = ^TUserInfo1006;
    TUserInfo1006 = Record
      usri1006_home_dir: WideString;
    End;

    PUserInfo1007 = ^TUserInfo1007;
    TUserInfo1007 = Record
      usri1007_comment: WideString;
    End;

    PUserInfo1008 = ^TUserInfo1008;
    TUserInfo1008 = Record
      usri1008_flags: DWORD;
    End;

    PUserInfo1009 = ^TUserInfo1009;
    TUserInfo1009 = Record
      usri1009_script_path: WideString;
    End;

    PUserInfo1010 = ^TUserInfo1010;
    TUserInfo1010 = Record
      usri1010_auth_flags: DWORD;
    End;

    PUserInfo1011 = ^TUserInfo1011;
    TUserInfo1011 = Record
      usri1011_full_name: WideString;
    End;

    PUserInfo1012 = ^TUserInfo1012;
    TUserInfo1012 = Record
      usri1012_usr_comment: WideString;
    End;

    PUserInfo1013 = ^TUserInfo1013;
    TUserInfo1013 = Record
      usri1013_parms: WideString;
    End;

    PUserInfo1014 = ^TUserInfo1014;
    TUserInfo1014 = Record
      usri1014_workstations: WideString;
    End;

    PUserInfo1017 = ^TUserInfo1017;
    TUserInfo1017 = Record
      usri1017_acct_expires: DWORD;
    End;

    PUserInfo1018 = ^TUserInfo1018;
    TUserInfo1018 = Record
      usri1018_max_storage: DWORD;
    End;

    PUserInfo1020 = ^TUserInfo1020;
    TUserInfo1020 = Record
      usri1020_units_per_week: DWORD;
      usri1020_logon_hours: PBYTE;
    End;

    PUserInfo1023 = ^TUserInfo1023;
    TUserInfo1023 = Record
      usri1023_logon_server: WideString;
    End;

    PUserInfo1024 = ^TUserInfo1024;
    TUserInfo1024 = Record
      usri1024_country_code: DWORD;
    End;

    PUserInfo1025 = ^TUserInfo1025;
    TUserInfo1025 = Record
      usri1025_code_page: DWORD;
    End;

    PUserInfo1051 = ^TUserInfo1051;
    TUserInfo1051 = Record
      usri1051_primary_group_id: DWORD;
    End;

    PUserInfo1052 = ^TUserInfo1052;
    TUserInfo1052 = Record
      usri1052_profile: WideString;
    End;

    PUserInfo1053 = ^TUserInfo1053;
    TUserInfo1053 = Record
      usri1053_home_dir_drive: WideString;
    End;

{------------------------------------------------------------------------------}
{ Data Structures - User Modals                                                }
{------------------------------------------------------------------------------}
 Type
    PUserModalsInfo0 = ^TUserModalsInfo0;
    TUserModalsInfo0 = Record
      usrmod0_min_passwd_len: DWORD;
      usrmod0_max_passwd_age: DWORD;
      usrmod0_min_passwd_age: DWORD;
      usrmod0_force_logoff: DWORD;
      usrmod0_password_hist_len: DWORD;
    End;

    PUserModalsInfo1 = ^TUserModalsInfo1;
    TUserModalsInfo1 = Record
      usrmod1_role: DWORD;
      usrmod1_primary: WideString;
    End;

    PUserModalsInfo2 = ^TUserModalsInfo2;
    TUserModalsInfo2 = Record
      usrmod2_domain_name: WideString;
      usrmod2_domain_id: Pointer;
    End;

    PUserModalsInfo3 = ^TUserModalsInfo3;
    TUserModalsInfo3 = Record
      usrmod3_lockout_duration: DWORD;
      usrmod3_lockout_observation_window: DWORD;
      usrmod3_lockout_threshold: DWORD;
    End;

    PUserModalsInfo1001 = ^TUserModalsInfo1001;
    TUserModalsInfo1001 = Record
      usrmod1001_min_passwd_len: DWORD;
    End;

    PUserModalsInfo1002 = ^TUserModalsInfo1002;
    TUserModalsInfo1002 = Record
      usrmod1002_max_passwd_age: DWORD;
    End;

    PUserModalsInfo1003 = ^TUserModalsInfo1003;
    TUserModalsInfo1003 = Record
      usrmod1003_min_passwd_age: DWORD;
    End;

    PUserModalsInfo1004 = ^TUserModalsInfo1004;
    TUserModalsInfo1004 = Record
      usrmod1004_force_logoff: DWORD;
    End;

    PUserModalsInfo1005 = ^TUserModalsInfo1005;
    TUserModalsInfo1005 = Record
      usrmod1005_password_hist_len: DWORD;
    End;

    PUserModalsInfo1006 = ^TUserModalsInfo1006;
    TUserModalsInfo1006 = Record
      usrmod1006_role: DWORD;
    End;

    PUserModalsInfo1007 = ^TUserModalsInfo1007;
    TUserModalsInfo1007 = Record
      usrmod1007_primary: WideString;
    End;

{------------------------------------------------------------------------------}
{ Special Values and Constants - User                                          }
{------------------------------------------------------------------------------}
 Const
    //  Bit masks for field usriX_flags Of USER_INFO_X (X = 0/1).
    UF_SCRIPT               = $0001;
    UF_ACCOUNTDISABLE       = $0002;
    UF_HOMEDIR_REQUIRED     = $0008;
    UF_LOCKOUT              = $0010;
    UF_PASSWD_NOTREQD       = $0020;
    UF_PASSWD_CANT_CHANGE   = $0040;

    // Account type bits as part Of usri_flags.
    UF_TEMP_DUPLICATE_ACCOUNT       = $0100;
    UF_NORMAL_ACCOUNT               = $0200;
    UF_INTERDOMAIN_TRUST_ACCOUNT    = $0800;
    UF_WORKSTATION_TRUST_ACCOUNT    = $1000;
    UF_SERVER_TRUST_ACCOUNT         = $2000;

    UF_MACHINE_ACCOUNT_MASK = UF_INTERDOMAIN_TRUST_ACCOUNT Or
                              UF_WORKSTATION_TRUST_ACCOUNT Or
                              UF_SERVER_TRUST_ACCOUNT;

    UF_ACCOUNT_TYPE_MASK = UF_TEMP_DUPLICATE_ACCOUNT Or
                           UF_NORMAL_ACCOUNT Or
                           UF_INTERDOMAIN_TRUST_ACCOUNT Or
                           UF_WORKSTATION_TRUST_ACCOUNT Or
                           UF_SERVER_TRUST_ACCOUNT;

    UF_DONT_EXPIRE_PASSWD  = $10000;
    UF_MNS_LOGON_ACCOUNT   = $20000;


    UF_SETTABLE_BITS  =  UF_SCRIPT Or
                         UF_ACCOUNTDISABLE Or
                         UF_LOCKOUT Or
                         UF_HOMEDIR_REQUIRED  Or
                         UF_PASSWD_NOTREQD Or
                         UF_PASSWD_CANT_CHANGE Or
                         UF_ACCOUNT_TYPE_MASK Or
                         UF_DONT_EXPIRE_PASSWD Or
                         UF_MNS_LOGON_ACCOUNT;

    // bit masks for the NetUserEnum filter parameter.
    FILTER_TEMP_DUPLICATE_ACCOUNT      = $0001;
    FILTER_NORMAL_ACCOUNT              = $0002;
    FILTER_PROXY_ACCOUNT               = $0004;
    FILTER_INTERDOMAIN_TRUST_ACCOUNT   = $0008;
    FILTER_WORKSTATION_TRUST_ACCOUNT   = $0010;
    FILTER_SERVER_TRUST_ACCOUNT        = $0020;

    // bit masks for the NetUserGetLocalGroups flags
    LG_INCLUDE_INDIRECT         = 0001;

    //  Bit masks for field usri2_auth_flags Of USER_INFO_2.
    AF_OP_PRINT              = $1;
    AF_OP_COMM               = $2;
    AF_OP_SERVER             = $4;
    AF_OP_ACCOUNTS           = $8;
    AF_SETTABLE_BITS         = AF_OP_PRINT Or
                               AF_OP_COMM Or
                               AF_OP_SERVER Or
                               AF_OP_ACCOUNTS;

    //  UAS role manifests under NETLOGON
    UAS_ROLE_STANDALONE    = 0;
    UAS_ROLE_MEMBER        = 1;
    UAS_ROLE_BACKUP        = 2;
    UAS_ROLE_PRIMARY       = 3;

    //  Values for ParmError for NetUserSetInfo.
    USER_NAME_PARMNUM              = 1;
    USER_PASSWORD_PARMNUM          = 3;
    USER_PASSWORD_AGE_PARMNUM      = 4;
    USER_PRIV_PARMNUM              = 5;
    USER_HOME_DIR_PARMNUM          = 6;
    USER_COMMENT_PARMNUM           = 7;
    USER_FLAGS_PARMNUM             = 8;
    USER_SCRIPT_PATH_PARMNUM       = 9;
    USER_AUTH_FLAGS_PARMNUM        = 10;
    USER_FULL_NAME_PARMNUM         = 11;
    USER_USR_COMMENT_PARMNUM       = 12;
    USER_PARMS_PARMNUM             = 13;
    USER_WORKSTATIONS_PARMNUM      = 14;
    USER_LAST_LOGON_PARMNUM        = 15;
    USER_LAST_LOGOFF_PARMNUM       = 16;
    USER_ACCT_EXPIRES_PARMNUM      = 17;
    USER_MAX_STORAGE_PARMNUM       = 18;
    USER_UNITS_PER_WEEK_PARMNUM    = 19;
    USER_LOGON_HOURS_PARMNUM       = 20;
    USER_PAD_PW_COUNT_PARMNUM      = 21;
    USER_NUM_LOGONS_PARMNUM        = 22;
    USER_LOGON_SERVER_PARMNUM      = 23;
    USER_COUNTRY_CODE_PARMNUM      = 24;
    USER_CODE_PAGE_PARMNUM         = 25;
    USER_PRIMARY_GROUP_PARMNUM     = 51;
    USER_PROFILE                   = 52; // ?? Delete when convenient
    USER_PROFILE_PARMNUM           = 52;
    USER_HOME_DIR_DRIVE_PARMNUM    = 53;

    // the new infolevel counterparts Of the old info level + parmnum
    USER_NAME_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL Or USER_NAME_PARMNUM;
    USER_PASSWORD_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL Or USER_PASSWORD_PARMNUM;
    USER_PASSWORD_AGE_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_PASSWORD_AGE_PARMNUM;
    USER_PRIV_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL Or USER_PRIV_PARMNUM;
    USER_HOME_DIR_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL Or USER_HOME_DIR_PARMNUM;
    USER_COMMENT_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL Or USER_COMMENT_PARMNUM;
    USER_FLAGS_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL Or USER_FLAGS_PARMNUM;
    USER_SCRIPT_PATH_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or USER_SCRIPT_PATH_PARMNUM;
    USER_AUTH_FLAGS_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL Or USER_AUTH_FLAGS_PARMNUM;
    USER_FULL_NAME_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL Or USER_FULL_NAME_PARMNUM;
    USER_USR_COMMENT_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or USER_USR_COMMENT_PARMNUM;
    USER_PARMS_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL Or USER_PARMS_PARMNUM;
    USER_WORKSTATIONS_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_WORKSTATIONS_PARMNUM;
    USER_LAST_LOGON_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL Or USER_LAST_LOGON_PARMNUM;
    USER_LAST_LOGOFF_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or USER_LAST_LOGOFF_PARMNUM;
    USER_ACCT_EXPIRES_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_ACCT_EXPIRES_PARMNUM;
    USER_MAX_STORAGE_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or USER_MAX_STORAGE_PARMNUM;
    USER_UNITS_PER_WEEK_INFOLEVEL = PARMNUM_BASE_INFOLEVEL Or USER_UNITS_PER_WEEK_PARMNUM;
    USER_LOGON_HOURS_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or USER_LOGON_HOURS_PARMNUM;
    USER_PAD_PW_COUNT_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_PAD_PW_COUNT_PARMNUM;
    USER_NUM_LOGONS_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL Or USER_NUM_LOGONS_PARMNUM;
    USER_LOGON_SERVER_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_LOGON_SERVER_PARMNUM;
    USER_COUNTRY_CODE_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL Or USER_COUNTRY_CODE_PARMNUM;
    USER_CODE_PAGE_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL Or USER_CODE_PAGE_PARMNUM;
    USER_PRIMARY_GROUP_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or USER_PRIMARY_GROUP_PARMNUM;
    //USER_POSIX_ID_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL Or USER_POSIX_ID_PARMNUM;
    USER_HOME_DIR_DRIVE_INFOLEVEL = PARMNUM_BASE_INFOLEVEL Or USER_HOME_DIR_DRIVE_PARMNUM;

    //  For SetInfo call (parmnum 0) when password change not required
    NULL_USERSETINFO_PASSWD     = '              ';

    TIMEQ_FOREVER               = $FFFFFFFF;
    USER_MAXSTORAGE_UNLIMITED   = $FFFFFFFF;
    USER_NO_LOGOFF              = $FFFFFFFF;
    UNITS_PER_DAY               = 24;
    UNITS_PER_WEEK              = UNITS_PER_DAY * 7;

    // Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)).
    USER_PRIV_MASK     = $3;
    USER_PRIV_GUEST    = $0;
    USER_PRIV_USER     = $1;
    USER_PRIV_ADMIN    = $2;

    // user modals related defaults
    MAX_PASSWD_LEN      = PWLEN;
    DEF_MIN_PWLEN       = 6;
    DEF_PWUNIQUENESS    = 5;
    DEF_MAX_PWHIST      = 8;

    DEF_MAX_PWAGE       = TIMEQ_FOREVER;      // forever
    DEF_MIN_PWAGE       = $00000000;          // 0 days
    DEF_FORCE_LOGOFF    = $FFFFFFFF;          // never
    DEF_MAX_BADPW       = $00000000;          // no limit
    ONE_DAY             = 01*24*3600;         // 01 day

    // User Logon Validation (codes returned)
    VALIDATED_LOGON       =  0;
    PASSWORD_EXPIRED      =  2;
    NON_VALIDATED_LOGON   =  3;

    VALID_LOGOFF          =  1;

    // parmnum manifests for user modals
    MODALS_MIN_PASSWD_LEN_PARMNUM               = 1;
    MODALS_MAX_PASSWD_AGE_PARMNUM               = 2;
    MODALS_MIN_PASSWD_AGE_PARMNUM               = 3;
    MODALS_FORCE_LOGOFF_PARMNUM                 = 4;
    MODALS_PASSWD_HIST_LEN_PARMNUM              = 5;
    MODALS_ROLE_PARMNUM                         = 6;
    MODALS_PRIMARY_PARMNUM                      = 7;
    MODALS_DOMAIN_NAME_PARMNUM                  = 8;
    MODALS_DOMAIN_ID_PARMNUM                    = 9;
    MODALS_LOCKOUT_DURATION_PARMNUM             = 10;
    MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM   = 11;
    MODALS_LOCKOUT_THRESHOLD_PARMNUM            = 12;

    // the new infolevel counterparts Of the old info level + parmnum
    MODALS_MIN_PASSWD_LEN_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or MODALS_MIN_PASSWD_LEN_PARMNUM;
    MODALS_MAX_PASSWD_AGE_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or MODALS_MAX_PASSWD_AGE_PARMNUM;
    MODALS_MIN_PASSWD_AGE_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or MODALS_MIN_PASSWD_AGE_PARMNUM;
    MODALS_FORCE_LOGOFF_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or MODALS_FORCE_LOGOFF_PARMNUM;
    MODALS_PASSWD_HIST_LEN_INFOLEVEL = PARMNUM_BASE_INFOLEVEL Or MODALS_PASSWD_HIST_LEN_PARMNUM;
    MODALS_ROLE_INFOLEVEL            = PARMNUM_BASE_INFOLEVEL Or MODALS_ROLE_PARMNUM;
    MODALS_PRIMARY_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL Or MODALS_PRIMARY_PARMNUM;
    MODALS_DOMAIN_NAME_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL Or MODALS_DOMAIN_NAME_PARMNUM;
    MODALS_DOMAIN_ID_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL Or MODALS_DOMAIN_ID_PARMNUM;

{------------------------------------------------------------------------------}
{ Function Prototypes - Group                                                  }
{------------------------------------------------------------------------------}
 Var
    NetGroupAdd: Function(ServerName: PWideChar; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetGroupAddUser: Function(ServerName: PWideChar; Const GroupName, UserName: WideString): NET_API_STATUS; StdCall;
    NetGroupEnum: Function(ServerName: PWideChar; level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    NetGroupGetInfo: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
    NetGroupSetInfo: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetGroupDel: Function(ServerName: PWideChar; Const GroupName: WideString): NET_API_STATUS; StdCall;
    NetGroupDelUser: Function(ServerName: PWideChar; Const GroupName, UserName: WideString): NET_API_STATUS; StdCall;
    NetGroupGetUsers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    NetGroupSetUsers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; StdCall;

{------------------------------------------------------------------------------}
{ Data Structures - Group                                                      }
{------------------------------------------------------------------------------}
 Type
    PGroupInfo0 = ^TGroupInfo0;
    TGroupInfo0 = Record
      grpi0_name: WideString;
    End;

    PGroupInfo1 = ^TGroupInfo1;
    TGroupInfo1 = Record
      grpi1_name: WideString;
      grpi1_comment: WideString;
    End;

    PGroupInfo2 = ^TGroupInfo2;
    TGroupInfo2 = Record
      grpi2_name: WideString;
      grpi2_comment: WideString;
      grpi2_group_id: DWORD;
      grpi2_attributes: DWORD;
    End;

    PGroupInfo1002 = ^TGroupInfo1002;
    TGroupInfo1002 = Record
      grpi1002_comment: WideString;
    End;

    PGroupInfo1005 = ^TGroupInfo1005;
    TGroupInfo1005 = Record
      grpi1005_attributes: DWORD;
    End;

    PGroupUsersInfo0 = ^TGroupUsersInfo0;
    TGroupUsersInfo0 = Record
      grui0_name: WideString;
    End;

    PGroupUsersInfo1 = ^TGroupUsersInfo1;
    TGroupUsersInfo1 = Record
      grui1_name: WideString;
      grui1_attributes: DWORD;
    End;

{------------------------------------------------------------------------------}
{ Special Values and Constants - Group                                         }
{------------------------------------------------------------------------------}
 Const
    GROUPIDMASK               =  $8000;      // MSB set if uid refers to a group

    // Predefined group for all normal users, administrators and guests
    // LOCAL is a special group for pinball local security.
    GROUP_SPECIALGRP_USERS    =  'USERS';
    GROUP_SPECIALGRP_ADMINS   =  'ADMINS';
    GROUP_SPECIALGRP_GUESTS   =  'GUESTS';
    GROUP_SPECIALGRP_LOCAL    =  'LOCAL';

    // parmnum manifests for SetInfo calls (only comment is settable)
    GROUP_ALL_PARMNUM         = 0;
    GROUP_NAME_PARMNUM        = 1;
    GROUP_COMMENT_PARMNUM     = 2;
    GROUP_ATTRIBUTES_PARMNUM  = 3;

    // the new infolevel counterparts Of the old info level + parmnum
    GROUP_ALL_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL Or GROUP_ALL_PARMNUM;
    GROUP_NAME_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL Or GROUP_NAME_PARMNUM;
    GROUP_COMMENT_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL Or GROUP_COMMENT_PARMNUM;
    GROUP_ATTRIBUTES_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or GROUP_ATTRIBUTES_PARMNUM;
    //  GROUP_POSIX_ID_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or GROUP_POSIX_ID_PARMNUM;

{------------------------------------------------------------------------------}
{ Function Prototypes - Local Group                                            }
{------------------------------------------------------------------------------}
 Var
    NetLocalGroupAdd: Function(ServerName: PWideChar; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetLocalGroupAddMember: Function(ServerName: PWideChar; Const GroupName: WideString; memberSID: Pointer): NET_API_STATUS; StdCall;
    NetLocalGroupEnum: Function(ServerName: PWideChar; level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    NetLocalGroupGetInfo: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
    NetLocalGroupSetInfo: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetLocalGroupDel: Function(ServerName: PWideChar; Const GroupName: WideString): NET_API_STATUS; StdCall;
    NetLocalGroupDelMember: Function(ServerName: PWideChar; Const GroupName: WideString; memberSID: Pointer): NET_API_STATUS; StdCall;
    NetLocalGroupGetMembers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    NetLocalGroupSetMembers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; StdCall;
    NetLocalGroupAddMembers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; StdCall;
    NetLocalGroupDelMembers: Function(ServerName: PWideChar; Const GroupName: WideString; level: DWORD; buf: PByte; totalentries: DWORD): NET_API_STATUS; StdCall;

{------------------------------------------------------------------------------}
{ Data Structures - Local Group                                                }
{------------------------------------------------------------------------------}
 Type
    PLocalGroupInfo0 = ^TLocalGroupInfo0;
    TLocalGroupInfo0 = Record
      lgrpi0_name: WideString;
    End;

    PLocalGroupInfo1 = ^TLocalGroupInfo1;
    TLocalGroupInfo1 = Record
      lgrpi1_name: WideString;
      lgrpi1_comment: WideString;
    End;

    PLocalGroupInfo1002 = ^TLocalGroupInfo1002;
    TLocalGroupInfo1002 = Record
      lgrpi1002_comment: WideString;
    End;

    PLocalGroupMembersInfo0 = ^TLocalGroupMembersInfo0;
    TLocalGroupMembersInfo0 = Record
      lgrmi0_sid: Pointer;
    End;

    TSidNameUse = DWORD;

    PLocalGroupMembersInfo1 = ^TLocalGroupMembersInfo1;
    TLocalGroupMembersInfo1 = Record
      lgrmi1_sid: Pointer;
      lgrmi1_sidusage: TSidNameUse;
      lgrmi1_name: WideString;
    End;

    PLocalGroupMembersInfo2 = ^TLocalGroupMembersInfo2;
    TLocalGroupMembersInfo2 = Record
      lgrmi2_sid: Pointer;
      lgrmi2_sidusage: TSidNameUse;
      lgrmi2_domainandname: WideString;
    End;

    PLocalGroupMembersInfo3 = ^TLocalGroupMembersInfo3;
    TLocalGroupMembersInfo3 = Record
      lgrmi3_domainandname: WideString;
    End;

    PLocalGroupUsersInfo0 = ^TLocalGroupUsersInfo0;
    TLocalGroupUsersInfo0 = Record
      lgrui0_name: WideString;
    End;

 Const
    LOCALGROUP_NAME_PARMNUM         = 1;
    LOCALGROUP_COMMENT_PARMNUM      = 2;


{------------------------------------------------------------------------------}
{ Display Information APIs                                                     }
{------------------------------------------------------------------------------}
 Var
    NetQueryDisplayInformation: Procedure(ServerName: PWideChar; level, Index, EntriesRequested, PreferredMaximumLength: DWORD; Var ReturnedEntryCount: DWORD; Var SortedBuffer: Pointer); StdCall;
    NetGetDisplayInformationIndex: Procedure(ServerName: PWideChar; level: DWORD; Const Prefix: WideString; Var Index: DWORD); StdCall;

 Type
    // QueryDisplayInformation levels
    PNetDisplayUser = ^TNetDisplayUser;
    TNetDisplayUser = Record
      usri1_name: WideString;
      usri1_comment: WideString;
      usri1_flags: DWORD;
      usri1_full_name: WideString;
      usri1_user_id: DWORD;
      usri1_next_index: DWORD;
    End;

    PNetDisplayMachine = ^TNetDisplayMachine;
    TNetDisplayMachine = Record
      usri2_name: WideString;
      usri2_comment: WideString;
      usri2_flags: DWORD;
      usri2_user_id: DWORD;
      usri2_next_index: DWORD;
    End;

    PNetDisplayGroup = ^TNetDisplayGroup;
    TNetDisplayGroup = Record
      grpi3_name: WideString;
      grpi3_comment: WideString;
      grpi3_group_id: DWORD;
      grpi3_attributes: DWORD;
      grpi3_next_index: DWORD;
    End;

{------------------------------------------------------------------------------}
{ Function Prototypes - Access                                                 }
{ The NetAccess APIs are only available to downlevel                           }
{------------------------------------------------------------------------------}
 Var
    NetAccessAdd: Function(ServerName: PWideChar; Level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    RxNetAccessAdd: Function(ServerName: PWideChar; Level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetAccessEnum: Function(ServerName: PWideChar; Const BasePath: WideString; Recursive, level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    RxNetAccessEnum: Function(ServerName: PWideChar; Const BasePath: WideString; Recursive, level: DWORD; Var bufptr: PByte; prefmaxlen: DWORD; Var entriesread, totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; StdCall;
    NetAccessGetInfo: Function(ServerName: PWideChar; Const resource: WideString; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
    RxNetAccessGetInfo: Function(ServerName: PWideChar; Const resource: WideString; level: DWORD; Var bufptr: PByte): NET_API_STATUS; StdCall;
    NetAccessSetInfo: Function(ServerName: PWideChar; Const resource: WideString; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    RxNetAccessSetInfo: Function(ServerName: PWideChar; Const resource: WideString; level: DWORD; buf: PByte; parm_err: PDWORD): NET_API_STATUS; StdCall;
    NetAccessDel: Function(ServerName: PWideChar; Const resource: WideString): NET_API_STATUS; StdCall;
    RxNetAccessDel: Function(ServerName: PWideChar; Const resource: WideString): NET_API_STATUS; StdCall;
    NetAccessGetUserPerms: Function(ServerName: PWideChar; Const UGname, resource: WideString; Var Perms: DWORD): NET_API_STATUS; StdCall;
    RxNetAccessGetUserPerms: Function(ServerName: PWideChar; Const UGname, resource: WideString; Var Perms: DWORD): NET_API_STATUS; StdCall;

{------------------------------------------------------------------------------}
{ Data Structures - Access                                                     }
{------------------------------------------------------------------------------}
 Type
    PAccessInfo0 = ^TAccessInfo0;
    TAccessInfo0 = Record
      acc0_resource_name: WideString;
    End;

    PAccessInfo1 = ^TAccessInfo1;
    TAccessInfo1 = Record
      acc1_resource_name: WideString;
      acc1_attr: DWORD;
      acc1_count: DWORD;
    End;

    PAccessInfo1002 = ^TAccessInfo1002;
    TAccessInfo1002 = Record
      acc1002_attr: DWORD;
    End;

    PAccessList = ^TAccessList;
    TAccessList = Record
      acl_ugname: WideString;
      acl_access: DWORD;
    End;

{------------------------------------------------------------------------------}
{ Special Values and Constants - Access                                        }
{------------------------------------------------------------------------------}
 Const
    // Maximum number Of permission entries for each resource.
    MAXPERMENTRIES     = 64;

    //  Bit values for the access permissions.  ACCESS_ALL is a handy
    //  way to specify maximum permissions.  These are used in
    //  acl_access field Of access_list structures.
    ACCESS_NONE        = 0;

    ACCESS_READ        = $01;
    ACCESS_WRITE       = $02;
    ACCESS_CREATE      = $04;
    ACCESS_EXEC        = $08;
    ACCESS_DELETE      = $10;
    ACCESS_ATRIB       = $20;
    ACCESS_PERM        = $40;

    ACCESS_GROUP       = $8000;

    ACCESS_ALL         = ACCESS_READ Or
                         ACCESS_WRITE Or
                         ACCESS_CREATE Or
                         ACCESS_EXEC Or
                         ACCESS_DELETE Or
                         ACCESS_ATRIB Or
                         ACCESS_PERM;

    // Bit values for the acc1_attr field Of the ACCESS_INFO_1 structure.
    ACCESS_AUDIT              = $1;

    ACCESS_SUCCESS_OPEN        = $10;
    ACCESS_SUCCESS_WRITE       = $20;
    ACCESS_SUCCESS_DELETE      = $40;
    ACCESS_SUCCESS_ACL         = $80;
    ACCESS_SUCCESS_MASK        = $F0;

    ACCESS_FAIL_OPEN           = $100;
    ACCESS_FAIL_WRITE          = $200;
    ACCESS_FAIL_DELETE         = $400;
    ACCESS_FAIL_ACL            = $800;
    ACCESS_FAIL_MASK           = $F00;

    ACCESS_FAIL_SHIFT          = $4;

    // Parmnum value for NetAccessSetInfo.
    ACCESS_RESOURCE_NAME_PARMNUM  =  1;
    ACCESS_ATTR_PARMNUM           =  2;
    ACCESS_COUNT_PARMNUM          =  3;
    ACCESS_ACCESS_LIST_PARMNUM    =  4;

    // the new infolevel counterparts Of the old info level + parmnum
    ACCESS_RESOURCE_NAME_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL Or ACCESS_RESOURCE_NAME_PARMNUM;
    ACCESS_ATTR_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL Or ACCESS_ATTR_PARMNUM;
    ACCESS_COUNT_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL Or ACCESS_COUNT_PARMNUM;
    ACCESS_ACCESS_LIST_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL Or ACCESS_ACCESS_LIST_PARMNUM;

    // ACCESS_LETTERS defines a letter for each bit position in
    // the acl_access field Of struct access_list.  Note that some
    // bits have a corresponding letter Of ' ' (space).
    ACCESS_LETTERS      = 'RWCXDAP         ';


{------------------------------------------------------------------------------}
{ Function Prototypes - Domain                                                 }
{------------------------------------------------------------------------------}
 Var
    NetGetDCName: Function(ServerName, DomainName: PWideChar; Var bufptr: PByte): NET_API_STATUS; StdCall;
    NetGetAnyDCName: Function(ServerName, DomainName: PWideChar; Var bufptr: PByte): NET_API_STATUS; StdCall;
    I_NetLogonControl: Function(ServerName: PWideChar; FunctionCode, QueryLevel: DWORD; Var Buffer: PByte): NET_API_STATUS; StdCall;
    I_NetLogonControl2: Function(ServerName: PWideChar; FunctionCode, QueryLevel: DWORD; Data: PByte; Var Buffer: PByte): NET_API_STATUS; StdCall;
    NetEnumerateTrustedDomains: Function(ServerName: PWideChar; Var DomainNames: PWideChar): TNTStatus;

{------------------------------------------------------------------------------}
{ Special Values and Constants - Domain                                        }
{------------------------------------------------------------------------------}
{ FunctionCode values for I_NetLogonControl.                                   }
{ NOTE : if you change the following NETLOGON_CONTROL_* values,                }
{ change them in net\svcdlls\logonsrv\logon.idl file also.                     }
{------------------------------------------------------------------------------}
Const
   NETLOGON_CONTROL_QUERY            = 1;    // No-op: just query
   NETLOGON_CONTROL_REPLICATE        = 2;    // Force replicate on BDC
   NETLOGON_CONTROL_SYNCHRONIZE      = 3;    // Force synchronize on BDC
   NETLOGON_CONTROL_PDC_REPLICATE    = 4;    // Force PDC to broadcast change
   NETLOGON_CONTROL_REDISCOVER       = 5;    // Force to re-discover trusted domain DCs
   NETLOGON_CONTROL_TC_QUERY         = 6;    // Query status Of specified trusted channel status
   NETLOGON_CONTROL_TRANSPORT_NOTIFY = 7;    // Notify netlogon that a new transport has come online
   NETLOGON_CONTROL_FIND_USER        = 8;    // Find named user in a trusted domain

   // Debug Function codes
   NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL = $FFFB;
   NETLOGON_CONTROL_BACKUP_CHANGE_LOG   = $FFFC;
   NETLOGON_CONTROL_TRUNCATE_LOG        = $FFFD;
   NETLOGON_CONTROL_SET_DBFLAG          = $FFFE;
   NETLOGON_CONTROL_BREAKPOINT          = $FFFF;

   // Query level 1 for I_NetLogonControl
 Type
    PNetLogonInfo1 = ^TNetLogonInfo1;
    TNetLogonInfo1 = Record
      netlog1_flags: DWORD;
      netlog1_pdc_connection_status: NET_API_STATUS;
    End;

    PNetLogonInfo2 = ^TNetLogonInfo2;
    TNetLogonInfo2 = Record
      netlog2_flags: DWORD;
      netlog2_pdc_connection_status: NET_API_STATUS;
      netlog2_trusted_dc_name: WideString;
      netlog2_tc_connection_status: NET_API_STATUS;
    End;

    PNetLogonInfo3 = ^TNetLogonInfo3;
    TNetLogonInfo3 = Record
      netlog3_flags: DWORD;
      netlog3_logon_attempts: DWORD;
      netlog3_reserved1: DWORD;
      netlog3_reserved2: DWORD;
      netlog3_reserved3: DWORD;
      netlog3_reserved4: DWORD;
      netlog3_reserved5: DWORD;
    End;

    PNetLogonInfo4 = ^TNetLogonInfo4;
    TNetLogonInfo4 = Record
      netlog4_trusted_dc_name: WideString;
      netlog4_trusted_domain_name: WideString;
    End;

 Const
    // Values Of netlog1_flags
    NETLOGON_REPLICATION_NEEDED       = $01; // Database is out Of date
    NETLOGON_REPLICATION_IN_PROGRESS  = $02; // Replication is happening now
    NETLOGON_FULL_SYNC_REPLICATION    = $04; // full sync replication required/progress
    NETLOGON_REDO_NEEDED              = $08; // Redo Of previous replication needed

 Const
    NetApi32 = 'NetApi32.dll';
    NetApi = 'NetApi.dll';

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ Clauses Uses                                                                 }
{------------------------------------------------------------------------------}
 Uses
  SysUtils;

{------------------------------------------------------------------------------}
{ Private Vars                                                                 }
{------------------------------------------------------------------------------}
 Var
    hNetAPI32: THandle;
    hNetAPI: THandle;
    bIsWinNT: Boolean = True;

{------------------------------------------------------------------------------}
{ NetapipBufferAllocate(...): NET_API_STATUS;                                  }
{ This private Function will go away eventually.                               }
{ Call NetApiBufferAllocate instead.                                           }
{------------------------------------------------------------------------------}
 Function NetapipBufferAllocate(ByteCount: DWORD; Var Buffer: Pointer): NET_API_STATUS; StdCall;
 Begin
   Result := NetApiBufferAllocate(ByteCount, Buffer);
 End;

{------------------------------------------------------------------------------}
{ InitNTLanManAPI                                                              }
{------------------------------------------------------------------------------}
 Procedure InitNTLanManAPI;
 Var
   Osvi: TOSVersionInfo;

  {----------------------------------------------------------------------------}
   Procedure Bind(Var ProcAddr: Pointer; Lib: THandle; Const ProcName: String);
   Begin
      ProcAddr := GetProcAddress(Lib, PChar(ProcName));
      If (ProcAddr=Nil) Then
         Raise Exception.Create('Echec à l''initialisation: '+ProcName+' non trouvée');
   End;
  {----------------------------------------------------------------------------}

 Begin
    ZeroMemory(@Osvi, SizeOf(Osvi));
    Osvi.dwOSVersionInfoSize := SizeOf(Osvi);
    GetVersionEx(Osvi);
    bIsWinNT := Osvi.dwPlatformId = VER_PLATFORM_WIN32_NT;
    If (hNetAPI32<=0) Then Begin
       hNetAPI32 := LoadLibrary(NetApi32);
       If (hNetAPI32<=0) Then
          Raise Exception.Create('Ne peut charger la DLL d''API Lan Manager (NetAPI32.dll)'#13#10+SysErrorMessage(GetLastError));
       If bIsWinNT Then
          hNetAPi := hNetApi32
       Else
          hNetAPI := GetModuleHandle(NetApi);
       If (hNetAPI<=0) Then
          Raise Exception.Create('Ne peut charger la DLL d''API Lan Manager (NetAPI.dll)'#13#10+SysErrorMessage(GetLastError));
       Bind(@NetUserAdd, hNetAPI, 'NetUserAdd');
       Bind(@NetUserEnum, hNetAPI, 'NetUserEnum');
       Bind(@NetUserGetInfo, hNetAPI, 'NetUserGetInfo');
       Bind(@NetUserSetInfo, hNetAPI, 'NetUserSetInfo');
       Bind(@NetUserDel, hNetAPI, 'NetUserDel');
       Bind(@NetUserGetGroups, hNetAPI, 'NetUserGetGroups');
       Bind(@NetUserSetGroups, hNetAPI, 'NetUserSetGroups');
       Bind(@NetUserGetLocalGroups, hNetAPI, 'NetUserGetLocalGroups');
       Bind(@NetUserModalsGet, hNetAPI, 'NetUserModalsGet');
       Bind(@NetUserModalsSet, hNetAPI, 'NetUserModalsSet');
       Bind(@NetUserChangePassword, hNetAPI, 'NetUserChangePassword');

       Bind(@NetApiBufferAllocate, hNetAPI, 'NetApiBufferAllocate');
       Bind(@NetApiBufferFree, hNetAPI, 'NetApiBufferFree');
       Bind(@NetApiBufferReallocate, hNetAPI, 'NetApiBufferReallocate');
       Bind(@NetApiBufferSize, hNetAPI, 'NetApiBufferSize');

       Bind(@NetGroupAdd, hNetAPI, 'NetGroupAdd');
       Bind(@NetGroupAddUser, hNetAPI, 'NetGroupAddUser');
       Bind(@NetGroupEnum, hNetAPI, 'NetGroupEnum');
       Bind(@NetGroupGetInfo, hNetAPI, 'NetGroupGetInfo');
       Bind(@NetGroupSetInfo, hNetAPI, 'NetGroupSetInfo');
       Bind(@NetGroupDel, hNetAPI, 'NetGroupDel');
       Bind(@NetGroupDelUser, hNetAPI, 'NetGroupDelUser');
       Bind(@NetGroupGetUsers, hNetAPI, 'NetGroupGetUsers');
       Bind(@NetGroupSetUsers, hNetAPI, 'NetGroupSetUsers');

       Bind(@NetLocalGroupAdd, hNetAPI, 'NetLocalGroupAdd');
       Bind(@NetLocalGroupAddMember, hNetAPI, 'NetLocalGroupAddMember');
       Bind(@NetLocalGroupEnum, hNetAPI, 'NetLocalGroupEnum');
       Bind(@NetLocalGroupGetInfo, hNetAPI, 'NetLocalGroupGetInfo');
       Bind(@NetLocalGroupSetInfo, hNetAPI, 'NetLocalGroupSetInfo');
       Bind(@NetLocalGroupDel, hNetAPI, 'NetLocalGroupDel');
       Bind(@NetLocalGroupDelMember, hNetAPI, 'NetLocalGroupDelMember');
       Bind(@NetLocalGroupGetMembers, hNetAPI, 'NetLocalGroupGetMembers');
       Bind(@NetLocalGroupSetMembers, hNetAPI, 'NetLocalGroupSetMembers');
       Bind(@NetLocalGroupAddMembers, hNetAPI, 'NetLocalGroupAddMembers');
       Bind(@NetLocalGroupDelMembers, hNetAPI, 'NetLocalGroupDelMembers');

       Bind(@NetQueryDisplayInformation, hNetAPI, 'NetQueryDisplayInformation');
       Bind(@NetGetDisplayInformationIndex, hNetAPI, 'NetGetDisplayInformationIndex');

       Bind(@RxNetAccessAdd, hNetAPI, 'RxNetAccessAdd');
       Bind(@RxNetAccessEnum, hNetAPI, 'RxNetAccessEnum');
       Bind(@RxNetAccessGetInfo, hNetAPI, 'RxNetAccessGetInfo');
       Bind(@RxNetAccessSetInfo, hNetAPI, 'RxNetAccessSetInfo');
       Bind(@RxNetAccessDel, hNetAPI, 'RxNetAccessDel');
       Bind(@RxNetAccessGetUserPerms, hNetAPI, 'RxNetAccessGetUserPerms');
       @NetAccessAdd := @RxNetAccessAdd;
       @NetAccessEnum := @RxNetAccessEnum;
       @NetAccessGetInfo := @RxNetAccessGetInfo;
       @NetAccessSetInfo := @RxNetAccessSetInfo;
       @NetAccessDel := @RxNetAccessDel;
       @NetAccessGetUserPerms := @RxNetAccessGetUserPerms;

       Bind(@NetGetDCName, hNetAPI, 'NetGetDCName');
       Bind(@NetGetAnyDCName, hNetAPI, 'NetGetAnyDCName');
       Bind(@I_NetLogonControl, hNetAPI, 'I_NetLogonControl');
       Bind(@I_NetLogonControl2, hNetAPI, 'I_NetLogonControl2');
       Bind(@NetEnumerateTrustedDomains, hNetAPI, 'NetEnumerateTrustedDomains');
    End;
 End;

{------------------------------------------------------------------------------}
{ FinalizeNTLanManAPI;                                                         }
{------------------------------------------------------------------------------}
 Procedure FinalizeNTLanManAPI;
 Begin
    If Not bIsWinNT Then
       If (hNetAPI>0) Then FreeLibrary(hNetAPI);
    hNetAPI := 0;
    If (hNetAPI32>0) Then
       FreeLibrary(hNetAPI32);
    hNetAPI32 := 0;
 End;

{------------------------------------------------------------------------------}
{ Initialization part                                                          }
{------------------------------------------------------------------------------}
 Initialization
   hNetAPI32 := 0;
   hNetAPI := 0;

{------------------------------------------------------------------------------}
{ Finalization part                                                            }
{------------------------------------------------------------------------------}
 Finalization
   FinalizeNTLanManAPI;

{------------------------------------------------------------------------------}
{ End Of ntlmapi.pas                                                           }
{------------------------------------------------------------------------------}
 End.
