{------------------------------------------------------------------------------}
{ aclconst                                                                     }
{------------------------------------------------------------------------------}
{ Author(s) : Franck Musson (franck.musson@wanadoo.fr)                         }
{ Copyright : Franck Musson 1998-2001                                          }
{ Created   : 01/10/2001                                                       }
{ Version   : 1.000 (Delphi 5)                                                 }
{------------------------------------------------------------------------------}
{ Copyright (c) 1987-2002 Franck Musson                                        }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.                        }
{ IN NO EVENT SHALL FRANCK MUSSON BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER    }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS       }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{ Except as contained in this notice, the name of Franck Musson shall not be   }
{ used in advertising or otherwise to promote the sale, use or other dealings  }
{ in this Software without prior written authorization from Franck Musson.     }
{------------------------------------------------------------------------------}
 Unit aclconst;

{$A+}

{==============================================================================}
{                                                                              }
{                                                                              }
{ I N T E R F A C E                                                            }
{                                                                              }
{                                                                              }
{==============================================================================}
 Interface

{------------------------------------------------------------------------------}
{ Uses clauses                                                                 }
{------------------------------------------------------------------------------}
 Uses
    Windows;

{------------------------------------------------------------------------------}
{ Public Constants                                                             }
{------------------------------------------------------------------------------}
 Const
    HEAP_NO_SERIALIZE               = $00000001;
    HEAP_GROWABLE                   = $00000002;
    HEAP_GENERATE_EXCEPTIONS        = $00000004;
    HEAP_ZERO_MEMORY                = $00000008;
    HEAP_REALLOC_IN_PLACE_ONLY      = $00000010;
    HEAP_TAIL_CHECKING_ENABLED      = $00000020;
    HEAP_FREE_CHECKING_ENABLED      = $00000040;
    HEAP_DISABLE_COALESCE_ON_FREE   = $00000080;
    HEAP_CREATE_ALIGN_16            = $00010000;
    HEAP_CREATE_ENABLE_TRACING      = $00020000;
    HEAP_MAXIMUM_TAG                = $0FFF;
    HEAP_PSEUDO_TAG_FLAG            = $8000;
    HEAP_TAG_SHIFT                  = 18;

    ACL_REVISION: Cardinal            = 2;

    // ACLS
    OWNER_SECURITY_INFORMATION        = $00000001;
    GROUP_SECURITY_INFORMATION        = $00000002;
    DACL_SECURITY_INFORMATION         = $00000004;
    SACL_SECURITY_INFORMATION         = $00000008;

    // ACES
    ACCESS_ALLOWED_ACE_TYPE           = $0;
    ACCESS_DENIED_ACE_TYPE            = $1;
    SYSTEM_AUDIT_ACE_TYPE             = $2;
    SYSTEM_ALARM_ACE_TYPE             = $3;
    ACCESS_ALLOWED_COMPOUND_ACE_TYPE  = $4;
    ACCESS_ALLOWED_OBJECT_ACE_TYPE    = $5;
    ACCESS_DENIED_OBJECT_ACE_TYPE     = $6;
    SYSTEM_AUDIT_OBJECT_ACE_TYPE      = $7;
    SYSTEM_ALARM_OBJECT_ACE_TYPE      = $8;

    ACE_OBJECT_TYPE_PRESENT           = $1;
    ACE_INHERITED_OBJECT_TYPE_PRESENT = $2;

    // ACES Flags
    OBJECT_INHERIT_ACE                = $1;
    CONTAINER_INHERIT_ACE             = $2;
    NO_PROPAGATE_INHERIT_ACE          = $4;
    INHERIT_ONLY_ACE                  = $8;
    INHERITED_ACE                     = $10;
    VALID_INHERIT_FLAGS               = $1F;

    SUCCESSFUL_ACCESS_ACE_FLAG        = $40;
    FAILED_ACCESS_ACE_FLAG            = $80;


    // GENERIC
    ACCESS_NONE                       = 0;
    ACCESS_READ                       = $01;
    ACCESS_WRITE                      = $02;
    ACCESS_CREATE                     = $04;
    ACCESS_EXEC                       = $08;
    ACCESS_DELETE                     = $10;
    ACCESS_ATRIB                      = $20;
    ACCESS_PERM                       = $40;
    ACCESS_GROUP                      = $8000;
    ACCESS_ALL                        = ACCESS_READ Or ACCESS_WRITE Or ACCESS_CREATE Or
                                        ACCESS_EXEC Or ACCESS_DELETE Or ACCESS_ATRIB Or
                                        ACCESS_PERM;

    GENERIC_ALL                       = $10000000;
    GENERIC_EXECUTE                   = $20000000;
    GENERIC_WRITE                     = $40000000;
    GENERIC_READ                      = $80000000;

    // STANDARD
    _DELETE                           = $00010000;
    READ_CONTROL                      = $00020000;
    SYNCHRONIZE                       = $00100000;
    WRITE_DAC                         = $00040000;
    WRITE_OWNER                       = $00080000;
    STANDARD_RIGHTS_REQUIRED          = _DELETE Or READ_CONTROL Or WRITE_DAC Or WRITE_OWNER;
    STANDARD_RIGHTS_READ              = READ_CONTROL;
    STANDARD_RIGHTS_WRITE             = READ_CONTROL;
    STANDARD_RIGHTS_EXECUTE           = READ_CONTROL;
    STANDARD_RIGHTS_ALL               = _DELETE Or READ_CONTROL Or SYNCHRONIZE Or WRITE_DAC Or WRITE_OWNER;

    // SPECIFICS
    SPECIFIC_RIGHTS_ALL               = $0000FFFF;
    ACCESS_SYSTEM_SECURITY            = $01000000;
    MAXIMUM_ALLOWED                   = $02000000;

    // FILES SPECIFICS
    FILE_READ_DATA                    = $0001;    // file & pipe
    FILE_LIST_DIRECTORY               = $0001;    // directory
    FILE_WRITE_DATA                   = $0002;    // file & pipe
    FILE_ADD_FILE                     = $0002;    // directory
    FILE_APPEND_DATA                  = $0004;    // file
    FILE_ADD_SUBDIRECTORY             = $0004;    // directory
    FILE_CREATE_PIPE_INSTANCE         = $0004;    // named pipe
    FILE_READ_EA                      = $0008;    // file & directory
    FILE_WRITE_EA                     = $0010;    // file & directory
    FILE_EXECUTE                      = $0020;    // file
    FILE_TRAVERSE                     = $0020;    // directory
    FILE_DELETE_CHILD                 = $0040;    // directory
    FILE_READ_ATTRIBUTES              = $0080;    // all
    FILE_WRITE_ATTRIBUTES             = $0100;    // all
    FILE_ALL_ACCESS                   = STANDARD_RIGHTS_REQUIRED Or SYNCHRONIZE Or $1FF;
    FILE_GENERIC_READ                 = STANDARD_RIGHTS_READ Or FILE_READ_DATA Or
                                          FILE_READ_ATTRIBUTES Or FILE_READ_EA Or SYNCHRONIZE;
    FILE_GENERIC_WRITE                = STANDARD_RIGHTS_WRITE Or FILE_WRITE_DATA Or
                                          FILE_WRITE_ATTRIBUTES Or FILE_WRITE_EA Or
                                          FILE_APPEND_DATA Or SYNCHRONIZE;
    FILE_GENERIC_EXECUTE              = STANDARD_RIGHTS_EXECUTE Or FILE_READ_ATTRIBUTES Or
                                          FILE_EXECUTE Or SYNCHRONIZE;

    // REGISTRY SPECIFICS
    KEY_QUERY_VALUE                   = $0001;
    KEY_SET_VALUE                     = $0002;
    KEY_CREATE_SUB_KEY                = $0004;
    KEY_ENUMERATE_SUB_KEYS            = $0008;
    KEY_NOTIFY                        = $0010;
    KEY_CREATE_LINK                   = $0020;
    KEY_READ                          = (STANDARD_RIGHTS_READ Or KEY_QUERY_VALUE Or
                                           KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY) And Not SYNCHRONIZE;
    KEY_WRITE                         = (STANDARD_RIGHTS_WRITE Or KEY_SET_VALUE Or
                                           KEY_CREATE_SUB_KEY) And Not SYNCHRONIZE;
    KEY_EXECUTE                       = KEY_READ And Not SYNCHRONIZE;
    KEY_ALL_ACCESS                    = (STANDARD_RIGHTS_ALL Or KEY_QUERY_VALUE Or
                                           KEY_SET_VALUE Or KEY_CREATE_SUB_KEY Or
                                           KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY Or
                                           KEY_CREATE_LINK) And Not SYNCHRONIZE;


{------------------------------------------------------------------------------}
{ Security Authorities                                                         }
{------------------------------------------------------------------------------}
 Const
    SECURITY_NULL_SID_AUTHORITY    = 0;
    SECURITY_WORLD_SID_AUTHORITY   = 1;
    SECURITY_LOCAL_SID_AUTHORITY   = 2;
    SECURITY_CREATOR_SID_AUTHORITY = 3;
    SECURITY_NON_UNIQUE_AUTHORITY  = 4;
    SECURITY_NT_AUTHORITY          = 5;

{------------------------------------------------------------------------------}
{ Universal well-known SIDs                                                    }
{------------------------------------------------------------------------------}
{ Null SID                     S-1-0-0                                         }
{ World                        S-1-1-0                                         }
{ Local                        S-1-2-0                                         }
{ Creator Owner ID             S-1-3-0                                         }
{ Creator Group ID             S-1-3-1                                         }
{ Creator Owner Server ID      S-1-3-2                                         }
{ Creator Group Server ID      S-1-3-3                                         }
{ (Non-unique IDs)             S-1-4                                           }
{------------------------------------------------------------------------------}
 Const
    SECURITY_NULL_RID                 = $00000000;
    SECURITY_WORLD_RID                = $00000000;
    SECURITY_LOCAL_RID                = $00000000;

    SECURITY_CREATOR_OWNER_RID        = $00000000;
    SECURITY_CREATOR_GROUP_RID        = $00000001;

    SECURITY_CREATOR_OWNER_SERVER_RID = $00000002;
    SECURITY_CREATOR_GROUP_SERVER_RID = $00000003;

{------------------------------------------------------------------------------}
{ NT well-known SIDs                                                           }
{------------------------------------------------------------------------------}
{ NT Authority          S-1-5                                                  }
{ Dialup                S-1-5-1                                                }
{ Network               S-1-5-2                                                }
{ Batch                 S-1-5-3                                                }
{ Interactive           S-1-5-4                                                }
{ Service               S-1-5-6                                                }
{ AnonymousLogon        S-1-5-7       (aka null logon session)                 }
{ Proxy                 S-1-5-8                                                }
{ ServerLogon           S-1-5-9       (aka domain controller account)          }
{ Self                  S-1-5-10      (self RID)                               }
{ Authenticated User    S-1-5-11      (Authenticated user somewhere)           }
{ Restricted Code       S-1-5-12      (Running restricted code)                }
{ Local System          S-1-5-18                                               }
{ (Logon IDs)           S-1-5-5-X-Y                                            }
{ (NT non-unique IDs)   S-1-5-21-...                                           }
{ (Built-in domain)     S-1-5-32-...                                           }
{------------------------------------------------------------------------------}
 Const
    SECURITY_LOGON_IDS_RID_COUNT        = 3;
    SECURITY_DIALUP_RID                 = $00000001;  // 1
    SECURITY_NETWORK_RID                = $00000002;  // 2
    SECURITY_BATCH_RID                  = $00000003;  // 3
    SECURITY_INTERACTIVE_RID            = $00000004;  // 4
    SECURITY_LOGON_IDS_RID              = $00000005;  // 5
    SECURITY_SERVICE_RID                = $00000006;  // 6
    SECURITY_ANONYMOUS_LOGON_RID        = $00000007;  // 7
    SECURITY_PROXY_RID                  = $00000008;  // 8
    SECURITY_ENTERPRISE_CONTROLLERS_RID = $00000009;  // 9
    SECURITY_SERVER_LOGON_RID           = $0000000A;  // 10
    SECURITY_PRINCIPAL_SELF_RID         = $0000000A;  // 10
    SECURITY_AUTHENTICATED_USER_RID     = $0000000B;  // 11
    SECURITY_RESTRICTED_CODE_RID        = $0000000C;  // 12
    SECURITY_TERMINAL_SERVER_RID        = $0000000D;  // 13
    SECURITY_LOCAL_SYSTEM_RID           = $00000012;  // 18
    SECURITY_NT_NON_UNIQUE              = $00000015;  // 21
    SECURITY_BUILTIN_DOMAIN_RID         = $00000020;  // 32

{------------------------------------------------------------------------------}
{ well-known domain relative sub-authority values (RIDs)...                    }
{------------------------------------------------------------------------------}
 Const
    // Well-known users ...
    DOMAIN_USER_RID_ADMIN          = $000001F4;  // 500
    DOMAIN_USER_RID_GUEST          = $000001F5;  // 501
    DOMAIN_USER_RID_KRBTGT         = $000001F6;  // 502

    // well-known groups ...
    DOMAIN_GROUP_RID_ADMINS        = $00000200;  // 512
    DOMAIN_GROUP_RID_USERS         = $00000201;  // 513
    DOMAIN_GROUP_RID_GUESTS        = $00000202;  // 514
    DOMAIN_GROUP_RID_COMPUTERS     = $00000203;  // 515
    DOMAIN_GROUP_RID_CONTROLLERS   = $00000204;  // 516
    DOMAIN_GROUP_RID_CERT_ADMINS   = $00000205;  // 517
    DOMAIN_GROUP_RID_SCHEMA_ADMINS = $00000206;  // 518

    // well-known aliases ...
    DOMAIN_ALIAS_RID_ADMINS        = $00000220;  // 544
    DOMAIN_ALIAS_RID_USERS         = $00000221;  // 545
    DOMAIN_ALIAS_RID_GUESTS        = $00000222;  // 546
    DOMAIN_ALIAS_RID_POWER_USERS   = $00000223;  // 547

    DOMAIN_ALIAS_RID_ACCOUNT_OPS   = $00000224;  // 548
    DOMAIN_ALIAS_RID_SYSTEM_OPS    = $00000225;  // 549
    DOMAIN_ALIAS_RID_PRINT_OPS     = $00000226;  // 550
    DOMAIN_ALIAS_RID_BACKUP_OPS    = $00000227;  // 551

    DOMAIN_ALIAS_RID_REPLICATOR    = $00000228;  // 552

{------------------------------------------------------------------------------}
{ Privileges                                                                   }
{------------------------------------------------------------------------------}
 Const
    SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
    SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
    SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
    SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
    SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
    SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
    SE_TCB_NAME                 = 'SeTcbPrivilege';
    SE_SECURITY_NAME            = 'SeSecurityPrivilege';
    SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
    SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
    SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
    SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
    SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
    SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
    SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
    SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
    SE_BACKUP_NAME              = 'SeBackupPrivilege';
    SE_RESTORE_NAME             = 'SeRestorePrivilege';
    SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
    SE_DEBUG_NAME               = 'SeDebugPrivilege';
    SE_AUDIT_NAME               = 'SeAuditPrivilege';
    SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
    SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
    SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';

{------------------------------------------------------------------------------}
{ Public Types                                                                 }
{------------------------------------------------------------------------------}
 Type
    TAccessInformation       = (aiOwner, aiGroup, aiSacl, aiDacl);
    TSDInformation           = (sdWantOwner, sdWantGroup, sdWantACL, sdWantSystemACL);
    TSDInformations          = Set Of TSDInformation;

    TRootKey                 = (CLASSES_ROOT, CURRENT_USER, LOCAL_MACHINE,
                                USERS, PERFORMANCE_DATA, CURRENT_CONFIG,
                                DYN_DATA);

    TACLKind                 = (akSystem, akDiscretionnary, akCreatorOwner, akCreatorGroup);

    TSidType                 = (siNone, sidUser, sidGroup, sidDomain, sidAlias, sidWellKnownGroup,
                                sidDeletedAccount, sidInvalid, sidUnknown, sidComputer);

    TAceEntryType            = (aeNone, aeAllowed, aeDenied, aeAudit, aeAlarm,
                                aeCompound,
                                aeAllowedObject, aeDeniedObject, aeAuditObject, aeAlarmObject);

    TAceFlagType             = (afObjectInherit, afContainerInherit,
                                afNoPropagate, afInheritOnly, afInherited, afValidInheritFlags,
                                afSuccessfullAuditFlag, afFailedAuditFlag);
    TAceFlagTypes            = Set Of TAceFlagType;

    TInheritMode             = (imDefault, imCopy, imDelete);
    TFlagKind                = (fkYes, fkNo);

    TNTRegFlagsMode          = (rfmCustom, rfmRead, rfmAll);
    TNTFileFlagsMode         = (ffmCustom, ffmAll, ffmModify, ffmReadExecute, ffmRead, ffmWrite);
    TNTDirFlagsMode          = (dfmCustom, dfmAll, dfmModify, dfmReadExecute, dfmRead, dfmWrite);

    TFileInheritFlag         = (fiCurrentdir, fiSubdirs, fiFiles, fiPropagate);
    TFileInheritFlags        = Set Of TFileInheritFlag;

    TAccessFlagValue         = (fvNone, fvAllowed, fvDenied);
    TAuditFlagValue          = (faNone, faSucceeded, faFailed, faSucceededFailed);
    TCheckFlagValue          = (fcAllowed, fcDenied);

    TWellKnownSid            = (wnsNull,
                                wnsWorld,
                                wnsLocal,
                                wnsCreatorOwner,
                                wnsCreatorGroup,
                                wnsCreatorOwnerServer,
                                wnsCreatorGroupServer,
                                wnsDialup,
                                wnsNetwork,
                                wnsBatch,
                                wnsInteractive,
                                wnsService,
                                wnsAnonymous,
                                wnsProxy,
                                wnsEnterpriseControllers,
                                wnsSelf,
                                wnsAuthenticatedUser,
                                wnsRestrictedCode,
                                wnsTerminalServer,
                                wnsWinLocalSystem,
                                wnsBuiltInAdministrators,
                                wnsBuiltInUsers,
                                wnsBuiltInGuests,
                                wnsBuiltInPowerUsers,
                                wnsBuiltInAccountOperators,
                                wnsBuiltInSystemOperators,
                                wnsBuiltInPrintOperators,
                                wnsBuiltInBackupOperators,
                                wnsBuiltInReplicator,
                                wnsDomainAdministrator,
                                wnsDomainGuest,
                                wnsDomainKrbtgt,
                                wnsDomainAdministrators,
                                wnsDomainUsers,
                                wnsDomainGuests,
                                wnsDomainComputers,
                                wnsDomainControllers,
                                wnsDomainCertAdmins,
                                wnsDomainSchemaAdmins,
                                wnsDomainEnterpriseAdmins,
                                wnsDomainPolicyAdmins,
                                wnsDomainRasAndIasServers,
                                wnsCustom,
                                wnsError);


    TAceHeader = Packed Record
      AceFlags: Byte;
      AceSize : Word;
    End;

    TAccessAllowedAce = Packed Record
      Header  : TAceHeader;
      Mask    : ACCESS_MASK;
      SidStart: DWORD;
    End;

    TAccessDeniedAce = Packed Record
      Header  : TAceHeader;
      Mask    : ACCESS_MASK;
      SidStart: DWORD;
    End;

    TSystemAuditAce = Packed Record
      Header  : TAceHeader;
      Mask    : ACCESS_MASK;
      SidStart: DWORD;
    End;

    TSystemAlarmAce = Packed Record
      Header  : TAceHeader;
      Mask    : ACCESS_MASK;
      SidStart: DWORD;
    End;

    TAccessAllowedCompoundAce = Packed Record
      Header             : TAceHeader;
      Mask               : ACCESS_MASK;
      Flags              : DWORD;
      ObjectType         : TGUID;
      InheritedObjectType: TGUID;
      SidStart           : DWORD;
    End;

    TAccessAllowedObjectAce = Packed Record
      Header             : TAceHeader;
      Mask               : ACCESS_MASK;
      Flags              : DWORD;
      ObjectType         : TGUID;
      InheritedObjectType: TGUID;
      SidStart           : DWORD;
    End;

    TAccessDeniedObjectAce = Packed Record
      Header             : TAceHeader;
      Mask               : ACCESS_MASK;
      Flags              : DWORD;
      ObjectType         : TGUID;
      InheritedObjectType: TGUID;
      SidStart           : DWORD;
    End;

    TSystemAuditObjectAce = Packed Record
      Header             : TAceHeader;
      Mask               : ACCESS_MASK;
      Flags              : DWORD;
      ObjectType         : TGUID;
      InheritedObjectType: TGUID;
      SidStart           : DWORD;
    End;

    TSystemAlarmObjectAce = Packed Record
      Header             : TAceHeader;
      Mask               : ACCESS_MASK;
      Flags              : DWORD;
      ObjectType         : TGUID;
      InheritedObjectType: TGUID;
      SidStart           : DWORD;
    End;

    TAce = Packed Record
      Case AceType: Byte Of
        ACCESS_ALLOWED_ACE_TYPE          : (AccessAllowedAce        : TAccessAllowedAce);
        ACCESS_DENIED_ACE_TYPE           : (AccessDeniedAce         : TAccessDeniedAce);
        SYSTEM_AUDIT_ACE_TYPE            : (SystemAuditAce          : TSystemAuditAce);
        SYSTEM_ALARM_ACE_TYPE            : (SystemAlarmAce          : TSystemAlarmAce);
        ACCESS_ALLOWED_COMPOUND_ACE_TYPE : (AccessAllowedCompoundAce: TAccessAllowedCompoundAce);
        ACCESS_ALLOWED_OBJECT_ACE_TYPE   : (AccessAllowedObjectAce  : TAccessAllowedObjectAce);
        ACCESS_DENIED_OBJECT_ACE_TYPE    : (AccessDeniedObjectAce   : TAccessDeniedObjectAce);
        SYSTEM_AUDIT_OBJECT_ACE_TYPE     : (SystemAuditObjectAce    : TSystemAuditObjectAce);
        SYSTEM_ALARM_OBJECT_ACE_TYPE     : (SystemAlarmObjectAce    : TSystemAlarmObjectAce);
    End;
    PAce = ^TAce;

    PACLSizeInformation = ^TACLSizeInformation;
    TACLSizeInformation = Packed Record
       AceCount: DWORD;
       AclBytesInUse: DWORD;
       AclBytesFree: DWORD;
    End;

{------------------------------------------------------------------------------}
{ ResourceStrings                                                              }
{------------------------------------------------------------------------------}
 ResourceString
   SUnknownACEType   = 'ACE Type Inconnu !';
   SSidInvalid       = 'SID Invalid !';
   SAlwaysActive     = 'Ne peut éffectuer cette opération sur un objet actif !';
   SAlwaysInActive   = 'Ne peut éffectuer cette opération sur un objet inactif !';
   SCannotBeAssigned = 'Ne peut éffectuer cette opération, l''assignation est proscrite !';
   SReadOnly         = 'Ne peut éffectuer cette opération sur un objet en lecture seule !';
   SCannotModifyInherited = 'Ne peut éffectuer cette opération sur un objet hérité !';

{==============================================================================}
{                                                                              }
{                                                                              }
{ I M P L E M E N T A T I O N                                                  }
{                                                                              }
{                                                                              }
{==============================================================================}
 Implementation

{------------------------------------------------------------------------------}
{ End of aclconst.pas                                                          }
{------------------------------------------------------------------------------}
 End.

