unit LMAccess;

interface

uses
  Windows, LMCons;

function NetUserEnum(ServerName: PWideChar; Level, Filter: Longint; var Buffer: PByte; PrefMaxLen: DWord;
                     var EntriesRead, TotalEntries: DWord; ResumeHandle: PDWord): NET_API_STATUS cdecl stdcall;
function NetUserGetInfo(ServerName: PWideChar; UserName: PWideChar; Level: DWord; Buffer: PByte): NET_API_STATUS cdecl stdcall;

{
NET_API_STATUS NET_API_FUNCTION
NetUserGetInfo (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     username,
    IN  DWORD      level,
    OUT LPBYTE     *bufptr
    );

NET_API_STATUS NET_API_FUNCTION
NetUserSetInfo (
    IN  LPCWSTR    servername OPTIONAL,
    IN  LPCWSTR    username,
    IN  DWORD     level,
    IN  LPBYTE    buf,
    OUT LPDWORD   parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetUserDel (
    IN  LPCWSTR    servername OPTIONAL,
    IN  LPCWSTR    username
    );

NET_API_STATUS NET_API_FUNCTION
NetUserGetGroups (
    IN  LPCWSTR    servername OPTIONAL,
    IN  LPCWSTR    username,
    IN  DWORD     level,
    OUT LPBYTE    *bufptr,
    IN  DWORD     prefmaxlen,
    OUT LPDWORD   entriesread,
    OUT LPDWORD   totalentries
    );

NET_API_STATUS NET_API_FUNCTION
NetUserSetGroups (
    IN  LPCWSTR    servername OPTIONAL,
    IN  LPCWSTR    username,
    IN  DWORD     level,
    IN  LPBYTE    buf,
    IN  DWORD     num_entries
    );

NET_API_STATUS NET_API_FUNCTION
NetUserGetLocalGroups (
    IN  LPCWSTR    servername OPTIONAL,
    IN  LPCWSTR    username,
    IN  DWORD     level,
    IN  DWORD     flags,
    OUT LPBYTE    *bufptr,
    IN  DWORD     prefmaxlen,
    OUT LPDWORD   entriesread,
    OUT LPDWORD   totalentries
    );

NET_API_STATUS NET_API_FUNCTION
NetUserModalsGet (
    IN  LPCWSTR    servername OPTIONAL,
    IN  DWORD     level,
    OUT LPBYTE    *bufptr
    );

NET_API_STATUS NET_API_FUNCTION
NetUserModalsSet (
    IN  LPCWSTR    servername OPTIONAL,
    IN  DWORD     level,
    IN  LPBYTE    buf,
    OUT LPDWORD   parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetUserChangePassword (
    IN  LPCWSTR   domainname OPTIONAL,
    IN  LPCWSTR   username OPTIONAL,
    IN  LPCWSTR   oldpassword,
    IN  LPCWSTR   newpassword
    );


//
//  Data Structures - User
//
}
type
  PUSER_INFO_0 = ^TUSER_INFO_0;
  TUSER_INFO_0 = record
    usri0_name: PWideChar;
  end;

  PUSER_INFO_1 = ^TUSER_INFO_1;
  TUSER_INFO_1 = packed record
    usri1_name: LPWSTR;
    usri1_password: LPWSTR;
    usri1_password_age: DWORD;
    usri1_priv: DWORD;
    usri1_home_dir: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_script_path: LPWSTR;
  end;


  PUSER_INFO_2 = ^TUSER_INFO_2;
  TUSER_INFO_2 = record
    usri2_name: LPWSTR   ;
    usri2_password: LPWSTR   ;
    usri2_password_age: DWORD    ;
    usri2_priv: DWORD    ;
    usri2_home_dir: LPWSTR   ;
    usri2_comment: LPWSTR   ;
    usri2_flags: DWORD    ;
    usri2_script_path: LPWSTR   ;
    usri2_auth_flags: DWORD    ;
    usri2_full_name: LPWSTR   ;
    usri2_usr_comment: LPWSTR   ;
    usri2_parms: LPWSTR   ;
    usri2_workstations: LPWSTR   ;
    usri2_last_logon: DWORD    ;
    usri2_last_logoff: DWORD    ;
    usri2_acct_expires: DWORD    ;
    usri2_max_storage: DWORD    ;
    usri2_units_per_week: DWORD    ;
    usri2_logon_hours: PBYTE    ;
    usri2_bad_pw_count: DWORD    ;
    usri2_num_logons: DWORD    ;
    usri2_logon_server: LPWSTR   ;
    usri2_country_code: DWORD    ;
    usri2_code_page: DWORD    ;
  end;

  PUSER_INFO_3 = ^TUSER_INFO_3;
  TUSER_INFO_3 = record
    usri3_name: LPWSTR   ;
    usri3_password: LPWSTR   ;
    usri3_password_age: DWORD    ;
    usri3_priv: DWORD    ;
    usri3_home_dir: LPWSTR   ;
    usri3_comment: LPWSTR   ;
    usri3_flags: DWORD    ;
    usri3_script_path: LPWSTR   ;
    usri3_auth_flags: DWORD    ;
    usri3_full_name: LPWSTR   ;
    usri3_usr_comment: LPWSTR   ;
    usri3_parms: LPWSTR   ;
    usri3_workstations: LPWSTR   ;
    usri3_last_logon: DWORD    ;
    usri3_last_logoff: DWORD    ;
    usri3_acct_expires: DWORD    ;
    usri3_max_storage: DWORD    ;
    usri3_units_per_week: DWORD    ;
    usri3_logon_hours: PBYTE    ;
    usri3_bad_pw_count: DWORD    ;
    usri3_num_logons: DWORD    ;
    usri3_logon_server: LPWSTR   ;
    usri3_country_code: DWORD    ;
    usri3_code_page: DWORD    ;
    usri3_user_id: DWORD    ;
    usri3_primary_group_id: DWORD    ;
    usri3_profile: LPWSTR   ;
    usri3_home_dir_drive: LPWSTR   ;
    usri3_password_expired: DWORD    ;
  end;

  PUSER_INFO_10 = ^TUSER_INFO_10;
  TUSER_INFO_10 = record
    usri10_name: LPWSTR   ;
    usri10_comment: LPWSTR   ;
    usri10_usr_comment: LPWSTR   ;
    usri10_full_name: LPWSTR   ;
  end;

  PUSER_INFO_11 = ^TUSER_INFO_11;
  TUSER_INFO_11 = record
    usri11_name: LPWSTR   ;
    usri11_comment: LPWSTR   ;
    usri11_usr_comment: LPWSTR   ;
    usri11_full_name: LPWSTR   ;
    usri11_priv: DWORD    ;
    usri11_auth_flags: DWORD    ;
    usri11_password_age: DWORD    ;
    usri11_home_dir: LPWSTR   ;
    usri11_parms: LPWSTR   ;
    usri11_last_logon: DWORD    ;
    usri11_last_logoff: DWORD    ;
    usri11_bad_pw_count: DWORD    ;
    usri11_num_logons: DWORD    ;
    usri11_logon_server: LPWSTR   ;
    usri11_country_code: DWORD    ;
    usri11_workstations: LPWSTR   ;
    usri11_max_storage: DWORD    ;
    usri11_units_per_week: DWORD    ;
    usri11_logon_hours: PBYTE    ;
    usri11_code_page: DWORD    ;
  end;

  PUSER_INFO_20 = ^TUSER_INFO_20;
  TUSER_INFO_20 = record
    usri20_name: LPWSTR   ;
    usri20_full_name: LPWSTR   ;
    usri20_comment: LPWSTR   ;
    usri20_flags: DWORD    ;
    usri20_user_id: DWORD    ;
  end;

  PUSER_INFO_21 = ^TUSER_INFO_21;
  TUSER_INFO_21 = record
    usri21_password: array[0..ENCRYPTED_PWLEN-1] of Byte;
  end;

  PUSER_INFO_22 = ^TUSER_INFO_22;
  TUSER_INFO_22 = record
    usri22_name: LPWSTR   ;
    usri22_password: array [0..ENCRYPTED_PWLEN] of Byte;
    usri22_password_age: DWORD    ;
    usri22_priv: DWORD    ;
    usri22_home_dir: LPWSTR   ;
    usri22_comment: LPWSTR   ;
    usri22_flags: DWORD    ;
    usri22_script_path: LPWSTR   ;
    usri22_auth_flags: DWORD    ;
    usri22_full_name: LPWSTR   ;
    usri22_usr_comment: LPWSTR   ;
    usri22_parms: LPWSTR   ;
    usri22_workstations: LPWSTR   ;
    usri22_last_logon: DWORD    ;
    usri22_last_logoff: DWORD    ;
    usri22_acct_expires: DWORD    ;
    usri22_max_storage: DWORD    ;
    usri22_units_per_week: DWORD    ;
    usri22_logon_hours: PBYTE    ;
    usri22_bad_pw_count: DWORD    ;
    usri22_num_logons: DWORD    ;
    usri22_logon_server: LPWSTR   ;
    usri22_country_code: DWORD    ;
    usri22_code_page: DWORD    ;
  end;

  PUSER_INFO_1003 = ^TUSER_INFO_1003;
  TUSER_INFO_1003 = record
     usri1003_password: LPWSTR  ;
  end;

  PUSER_INFO_1005 = ^TUSER_INFO_1005;
  TUSER_INFO_1005 = record
     usri1005_priv: DWORD   ;
  end;

  PUSER_INFO_1006 = ^TUSER_INFO_1006;
  TUSER_INFO_1006 = record
     usri1006_home_dir: LPWSTR  ;
  end;

  PUSER_INFO_1007 = ^TUSER_INFO_1007;
  TUSER_INFO_1007 = record
     usri1007_comment: LPWSTR  ;
  end;

  PUSER_INFO_1008 = ^TUSER_INFO_1008;
  TUSER_INFO_1008 = record
     usri1008_flags: DWORD   ;
  end;

  PUSER_INFO_1009 = ^TUSER_INFO_1009;
  TUSER_INFO_1009 = record
     usri1009_script_path: LPWSTR  ;
  end;

  PUSER_INFO_1010 = ^TUSER_INFO_1010;
  TUSER_INFO_1010 = record
     usri1010_auth_flags: DWORD   ;
  end;

  PUSER_INFO_1011 = ^TUSER_INFO_1011;
  TUSER_INFO_1011 = record
     usri1011_full_name: LPWSTR  ;
  end;

  PUSER_INFO_1012 = ^TUSER_INFO_1012;
  TUSER_INFO_1012 = record
     usri1012_usr_comment: LPWSTR  ;
  end;

  PUSER_INFO_1013 = ^TUSER_INFO_1013;
  TUSER_INFO_1013 = record
     usri1013_parms: LPWSTR  ;
  end;

  PUSER_INFO_1014 = ^TUSER_INFO_1014;
  TUSER_INFO_1014 = record
     usri1014_workstations: LPWSTR  ;
  end;

  PUSER_INFO_1017 = ^TUSER_INFO_1017;
  TUSER_INFO_1017 = record
     usri1017_acct_expires: DWORD   ;
  end;

  PUSER_INFO_1018 = ^TUSER_INFO_1018;
  TUSER_INFO_1018 = record
     usri1018_max_storage: DWORD   ;
  end;

  PUSER_INFO_1020 = ^TUSER_INFO_1020;
  TUSER_INFO_1020 = record
    usri1020_units_per_week: DWORD   ;
    usri1020_logon_hours: PByte;
  end;

  PUSER_INFO_1023 = ^TUSER_INFO_1023;
  TUSER_INFO_1023 = record
     usri1023_logon_server: LPWSTR  ;
  end;

  PUSER_INFO_1024 = ^TUSER_INFO_1024;
  TUSER_INFO_1024 = record
     usri1024_country_code: DWORD   ;
  end;

  PUSER_INFO_1025 = ^TUSER_INFO_1025;
  TUSER_INFO_1025 = record
     usri1025_code_page: DWORD   ;
  end;

  PUSER_INFO_1051 = ^TUSER_INFO_1051;
  TUSER_INFO_1051 = record
     usri1051_primary_group_id: DWORD   ;
  end;

  PUSER_INFO_1052 = ^TUSER_INFO_1052;
  TUSER_INFO_1052 = record
     usri1052_profile: LPWSTR  ;
  end;

  PUSER_INFO_1053 = ^TUSER_INFO_1053;
  TUSER_INFO_1053 = record
     usri1053_home_dir_drive: LPWSTR  ;
  end;


//
//  Data Structures - User Modals
//

  PUSER_MODALS_INFO_0 = ^TUSER_MODALS_INFO_0;
  TUSER_MODALS_INFO_0 = record
    usrmod0_min_passwd_len: DWORD    ;
    usrmod0_max_passwd_age: DWORD    ;
    usrmod0_min_passwd_age: DWORD    ;
    usrmod0_force_logoff: DWORD    ;
    usrmod0_password_hist_len: DWORD    ;
  end;

  PUSER_MODALS_INFO_1 = ^TUSER_MODALS_INFO_1;
  TUSER_MODALS_INFO_1 = record
    usrmod1_role: DWORD    ;
    usrmod1_primary: LPWSTR   ;
  end;

  PUSER_MODALS_INFO_2 = ^TUSER_MODALS_INFO_2;
  TUSER_MODALS_INFO_2 = record
    usrmod2_domain_name: LPWSTR  ;
    usrmod2_domain_id: PSID    ;
  end;

  PUSER_MODALS_INFO_3 = ^TUSER_MODALS_INFO_3;
  TUSER_MODALS_INFO_3 = record
    usrmod3_lockout_duration: DWORD   ;
    usrmod3_lockout_observation_window: DWORD   ;
    usrmod3_lockout_threshold: DWORD   ;
  end;

  PUSER_MODALS_INFO_1001 = ^TUSER_MODALS_INFO_1001;
  TUSER_MODALS_INFO_1001 = record
     usrmod1001_min_passwd_len: DWORD   ;
  end;

  PUSER_MODALS_INFO_1002 = ^TUSER_MODALS_INFO_1002;
  TUSER_MODALS_INFO_1002 = record
     usrmod1002_max_passwd_age: DWORD   ;
  end;

  PUSER_MODALS_INFO_1003 = ^TUSER_MODALS_INFO_1003;
  TUSER_MODALS_INFO_1003 = record
     usrmod1003_min_passwd_age: DWORD   ;
  end;

  PUSER_MODALS_INFO_1004 = ^TUSER_MODALS_INFO_1004;
  TUSER_MODALS_INFO_1004 = record
     usrmod1004_force_logoff: DWORD   ;
  end;

  PUSER_MODALS_INFO_1005 = ^TUSER_MODALS_INFO_1005;
  TUSER_MODALS_INFO_1005 = record
     usrmod1005_password_hist_len: DWORD   ;
  end;

  PUSER_MODALS_INFO_1006 = ^TUSER_MODALS_INFO_1006;
  TUSER_MODALS_INFO_1006 = record
     usrmod1006_role: DWORD   ;
  end;

  PUSER_MODALS_INFO_1007 = ^TUSER_MODALS_INFO_1007;
  TUSER_MODALS_INFO_1007 = record
     usrmod1007_primary: LPWSTR  ;
  end;


//
// Special Values and Constants - User
//

//
//  Bit masks for field usriX_flags of USER_INFO_X (X = 0/1).
//

const
  UF_SCRIPT                          = $0001;
  UF_ACCOUNTDISABLE                  = $0002;
  UF_HOMEDIR_REQUIRED                = $0008;
  UF_LOCKOUT                         = $0010;
  UF_PASSWD_NOTREQD                  = $0020;
  UF_PASSWD_CANT_CHANGE              = $0040;
  UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $0080;

//
// Account type bits as part of usri_flags.
//

  UF_TEMP_DUPLICATE_ACCOUNT       = $0100;
  UF_NORMAL_ACCOUNT               = $0200;
  UF_INTERDOMAIN_TRUST_ACCOUNT    = $0800;
  UF_WORKSTATION_TRUST_ACCOUNT    = $1000;
  UF_SERVER_TRUST_ACCOUNT         = $2000;
{
#define UF_MACHINE_ACCOUNT_MASK ( UF_INTERDOMAIN_TRUST_ACCOUNT | \
                                  UF_WORKSTATION_TRUST_ACCOUNT | \
                                  UF_SERVER_TRUST_ACCOUNT )

#define UF_ACCOUNT_TYPE_MASK         ( \
                    UF_TEMP_DUPLICATE_ACCOUNT | \
                    UF_NORMAL_ACCOUNT | \
                    UF_INTERDOMAIN_TRUST_ACCOUNT | \
                    UF_WORKSTATION_TRUST_ACCOUNT | \
                    UF_SERVER_TRUST_ACCOUNT \
                )
}
  UF_DONT_EXPIRE_PASSWD           = $10000;
  UF_MNS_LOGON_ACCOUNT            = $20000;
  UF_SMARTCARD_REQUIRED           = $40000;
  UF_TRUSTED_FOR_DELEGATION       = $80000;
  UF_NOT_DELEGATED               = $100000;
{


#define UF_SETTABLE_BITS        ( \
                    UF_SCRIPT | \
                    UF_ACCOUNTDISABLE | \
                    UF_LOCKOUT | \
                    UF_HOMEDIR_REQUIRED  | \
                    UF_PASSWD_NOTREQD | \
                    UF_PASSWD_CANT_CHANGE | \
                    UF_ACCOUNT_TYPE_MASK | \
                    UF_DONT_EXPIRE_PASSWD | \
                    UF_MNS_LOGON_ACCOUNT |\
                    UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED |\
                    UF_SMARTCARD_REQUIRED | \
                    UF_TRUSTED_FOR_DELEGATION | \
                    UF_NOT_DELEGATED \
                )
}

//
// bit masks for the NetUserEnum filter parameter.
//
const
  FILTER_TEMP_DUPLICATE_ACCOUNT       = $0001;
  FILTER_NORMAL_ACCOUNT               = $0002;
// #define FILTER_PROXY_ACCOUNT                (0x0004)
  FILTER_INTERDOMAIN_TRUST_ACCOUNT    = $0008;
  FILTER_WORKSTATION_TRUST_ACCOUNT    = $0010;
  FILTER_SERVER_TRUST_ACCOUNT         = $0020;

//
// bit masks for the NetUserGetLocalGroups flags
//
  LG_INCLUDE_INDIRECT         = $0001;

//
//  Bit masks for field usri2_auth_flags of USER_INFO_2.
//

  AF_OP_PRINT             = $1;
  AF_OP_COMM              = $2;
  AF_OP_SERVER            = $4;
  AF_OP_ACCOUNTS          = $8;
  AF_SETTABLE_BITS        = AF_OP_PRINT or AF_OP_COMM or AF_OP_SERVER
                            or AF_OP_ACCOUNTS;

//
//  UAS role manifests under NETLOGON
//

  UAS_ROLE_STANDALONE     = 0;
  UAS_ROLE_MEMBER         = 1;
  UAS_ROLE_BACKUP         = 2;
  UAS_ROLE_PRIMARY        = 3;

//
//  Values for ParmError for NetUserSetInfo.
//

  USER_NAME_PARMNUM               = 1;
  USER_PASSWORD_PARMNUM           = 3;
  USER_PASSWORD_AGE_PARMNUM       = 4;
  USER_PRIV_PARMNUM               = 5;
  USER_HOME_DIR_PARMNUM           = 6;
  USER_COMMENT_PARMNUM            = 7;
  USER_FLAGS_PARMNUM              = 8;
  USER_SCRIPT_PATH_PARMNUM        = 9;
  USER_AUTH_FLAGS_PARMNUM         = 10;
  USER_FULL_NAME_PARMNUM          = 11;
  USER_USR_COMMENT_PARMNUM        = 12;
  USER_PARMS_PARMNUM              = 13;
  USER_WORKSTATIONS_PARMNUM       = 14;
  USER_LAST_LOGON_PARMNUM         = 15;
  USER_LAST_LOGOFF_PARMNUM        = 16;
  USER_ACCT_EXPIRES_PARMNUM       = 17;
  USER_MAX_STORAGE_PARMNUM        = 18;
  USER_UNITS_PER_WEEK_PARMNUM     = 19;
  USER_LOGON_HOURS_PARMNUM        = 20;
  USER_PAD_PW_COUNT_PARMNUM       = 21;
  USER_NUM_LOGONS_PARMNUM         = 22;
  USER_LOGON_SERVER_PARMNUM       = 23;
  USER_COUNTRY_CODE_PARMNUM       = 24;
  USER_CODE_PAGE_PARMNUM          = 25;
  USER_PRIMARY_GROUP_PARMNUM      = 51;
  USER_PROFILE                    = 52; // ?? Delete when convenient
  USER_PROFILE_PARMNUM            = 52;
  USER_HOME_DIR_DRIVE_PARMNUM     = 53;

//
// the new infolevel counterparts of the old info level + parmnum
//

  USER_NAME_INFOLEVEL             = PARMNUM_BASE_INFOLEVEL + USER_NAME_PARMNUM;
  USER_PASSWORD_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_PARMNUM;
  USER_PASSWORD_AGE_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_AGE_PARMNUM;
  USER_PRIV_INFOLEVEL             = PARMNUM_BASE_INFOLEVEL + USER_PRIV_PARMNUM;
  USER_HOME_DIR_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_PARMNUM;
  USER_COMMENT_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL + USER_COMMENT_PARMNUM;
  USER_FLAGS_INFOLEVEL            = PARMNUM_BASE_INFOLEVEL + USER_FLAGS_PARMNUM;
  USER_SCRIPT_PATH_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_SCRIPT_PATH_PARMNUM;
  USER_AUTH_FLAGS_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_AUTH_FLAGS_PARMNUM;
  USER_FULL_NAME_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + USER_FULL_NAME_PARMNUM;
  USER_USR_COMMENT_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_USR_COMMENT_PARMNUM;
  USER_PARMS_INFOLEVEL            = PARMNUM_BASE_INFOLEVEL + USER_PARMS_PARMNUM;
  USER_WORKSTATIONS_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_WORKSTATIONS_PARMNUM;
  USER_LAST_LOGON_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGON_PARMNUM;
  USER_LAST_LOGOFF_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGOFF_PARMNUM;
  USER_ACCT_EXPIRES_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_ACCT_EXPIRES_PARMNUM;
  USER_MAX_STORAGE_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_MAX_STORAGE_PARMNUM;
  USER_UNITS_PER_WEEK_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_UNITS_PER_WEEK_PARMNUM;
  USER_LOGON_HOURS_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_LOGON_HOURS_PARMNUM;
  USER_PAD_PW_COUNT_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_PAD_PW_COUNT_PARMNUM;
  USER_NUM_LOGONS_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_NUM_LOGONS_PARMNUM;
  USER_LOGON_SERVER_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_LOGON_SERVER_PARMNUM;
  USER_COUNTRY_CODE_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_COUNTRY_CODE_PARMNUM;
  USER_CODE_PAGE_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + USER_CODE_PAGE_PARMNUM;
  USER_PRIMARY_GROUP_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_PRIMARY_GROUP_PARMNUM;
//  USER_POSIX_ID_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL + USER_POSIX_ID_PARMNUM;
  USER_HOME_DIR_DRIVE_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_DRIVE_PARMNUM;

//
//  For SetInfo call (parmnum 0) when password change not required
//

  NULL_USERSETINFO_PASSWD     = '              ';

  TIMEQ_FOREVER               = $FFFFFFFF;
  USER_MAXSTORAGE_UNLIMITED   = $FFFFFFFF;
  USER_NO_LOGOFF              = $FFFFFFFF;
  UNITS_PER_DAY               = 24;
  UNITS_PER_WEEK              = UNITS_PER_DAY * 7;

//
// Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)).
//

  USER_PRIV_MASK      = $3;
  USER_PRIV_GUEST   =   0;
  USER_PRIV_USER    =   1;
  USER_PRIV_ADMIN   =   2;

//
// user modals related defaults
//

  MAX_PASSWD_LEN      = PWLEN;
  DEF_MIN_PWLEN       = 6;
  DEF_PWUNIQUENESS    = 5;
  DEF_MAX_PWHIST      = 8;

  DEF_MAX_PWAGE       = TIMEQ_FOREVER;               // forever
  DEF_MIN_PWAGE       = 0;          // 0 days
  DEF_FORCE_LOGOFF    = $FFFFFFFF;  // never
  DEF_MAX_BADPW       = 0;                           // no limit
  ONE_DAY             = 01*24*3600;  // 01 day
{
//
// User Logon Validation (codes returned)
//

#define VALIDATED_LOGON         0
#define PASSWORD_EXPIRED        2
#define NON_VALIDATED_LOGON     3

#define VALID_LOGOFF            1

//
// parmnum manifests for user modals
//

#define MODALS_MIN_PASSWD_LEN_PARMNUM       1
#define MODALS_MAX_PASSWD_AGE_PARMNUM       2
#define MODALS_MIN_PASSWD_AGE_PARMNUM       3
#define MODALS_FORCE_LOGOFF_PARMNUM         4
#define MODALS_PASSWD_HIST_LEN_PARMNUM      5
#define MODALS_ROLE_PARMNUM                 6
#define MODALS_PRIMARY_PARMNUM              7
#define MODALS_DOMAIN_NAME_PARMNUM          8
#define MODALS_DOMAIN_ID_PARMNUM            9
#define MODALS_LOCKOUT_DURATION_PARMNUM     10
#define MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM 11
#define MODALS_LOCKOUT_THRESHOLD_PARMNUM    12

//
// the new infolevel counterparts of the old info level + parmnum
//

#define MODALS_MIN_PASSWD_LEN_INFOLEVEL     \
            (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_LEN_PARMNUM)
#define MODALS_MAX_PASSWD_AGE_INFOLEVEL     \
            (PARMNUM_BASE_INFOLEVEL + MODALS_MAX_PASSWD_AGE_PARMNUM)
#define MODALS_MIN_PASSWD_AGE_INFOLEVEL     \
            (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_AGE_PARMNUM)
#define MODALS_FORCE_LOGOFF_INFOLEVEL       \
            (PARMNUM_BASE_INFOLEVEL + MODALS_FORCE_LOGOFF_PARMNUM)
#define MODALS_PASSWD_HIST_LEN_INFOLEVEL    \
            (PARMNUM_BASE_INFOLEVEL + MODALS_PASSWD_HIST_LEN_PARMNUM)
#define MODALS_ROLE_INFOLEVEL               \
            (PARMNUM_BASE_INFOLEVEL + MODALS_ROLE_PARMNUM)
#define MODALS_PRIMARY_INFOLEVEL            \
            (PARMNUM_BASE_INFOLEVEL + MODALS_PRIMARY_PARMNUM)
#define MODALS_DOMAIN_NAME_INFOLEVEL        \
            (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_NAME_PARMNUM)
#define MODALS_DOMAIN_ID_INFOLEVEL          \
            (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_ID_PARMNUM)

#endif // _LMUSER_

//
// Group Class
//

#ifndef _LMGROUP_
#define _LMGROUP_

//
// Function Prototypes
//

NET_API_STATUS NET_API_FUNCTION
NetGroupAdd (
    IN  LPCWSTR   servername OPTIONAL,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupAddUser (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   GroupName,
    IN  LPCWSTR   username
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupEnum (
    IN  LPCWSTR      servername OPTIONAL,
    IN  DWORD       level,
    OUT LPBYTE      *bufptr,
    IN  DWORD       prefmaxlen,
    OUT LPDWORD     entriesread,
    OUT LPDWORD     totalentries,
    IN OUT PDWORD_PTR resume_handle OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupGetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  DWORD    level,
    OUT LPBYTE   *bufptr
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupSetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupDel (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupDelUser (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   GroupName,
    IN  LPCWSTR   Username
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupGetUsers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     groupname,
    IN  DWORD      level,
    OUT LPBYTE     *bufptr,
    IN  DWORD      prefmaxlen,
    OUT LPDWORD    entriesread,
    OUT LPDWORD    totalentries,
    IN OUT PDWORD_PTR ResumeHandle
    );

NET_API_STATUS NET_API_FUNCTION
NetGroupSetUsers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     groupname,
    IN  DWORD      level,
    IN  LPBYTE     buf,
    IN  DWORD      totalentries
    );

//
//  Data Structures - Group
//

  PGROUP_INFO_0 = ^TGROUP_INFO_0;
  TGROUP_INFO_0 = record
    LPWSTR   grpi0_name;
  end;

  PGROUP_INFO_1 = ^TGROUP_INFO_1;
  TGROUP_INFO_1 = record
    LPWSTR   grpi1_name;
    LPWSTR   grpi1_comment;
  end;

  PGROUP_INFO_2 = ^TGROUP_INFO_2;
  TGROUP_INFO_2 = record
    LPWSTR   grpi2_name;
    LPWSTR   grpi2_comment;
    DWORD    grpi2_group_id;
    DWORD    grpi2_attributes;
  end;

  PGROUP_INFO_1002 = ^TGROUP_INFO_1002;
  TGROUP_INFO_1002 = record
     LPWSTR  grpi1002_comment;
  end;

  PGROUP_INFO_1005 = ^TGROUP_INFO_1005;
  TGROUP_INFO_1005 = record
     DWORD  grpi1005_attributes;
  end;


  PGROUP_USERS_INFO_0 = ^TGROUP_USERS_INFO_0;
  TGROUP_USERS_INFO_0 = record
     LPWSTR  grui0_name;
  end;

  PGROUP_USERS_INFO_1 = ^TGROUP_USERS_INFO_1;
  TGROUP_USERS_INFO_1 = record
     LPWSTR  grui1_name;
     DWORD   grui1_attributes;
  end;

//
// Special Values and Constants - Group
//

#define GROUPIDMASK                 0x8000      // MSB set if uid refers
                                                // to a group

//
// Predefined group for all normal users, administrators and guests
// LOCAL is a special group for pinball local security.
//

#define GROUP_SPECIALGRP_USERS      L"USERS"
#define GROUP_SPECIALGRP_ADMINS     L"ADMINS"
#define GROUP_SPECIALGRP_GUESTS     L"GUESTS"
#define GROUP_SPECIALGRP_LOCAL      L"LOCAL"

//
// parmnum manifests for SetInfo calls (only comment is settable)
//

#define GROUP_ALL_PARMNUM           0
#define GROUP_NAME_PARMNUM          1
#define GROUP_COMMENT_PARMNUM       2
#define GROUP_ATTRIBUTES_PARMNUM    3

//
// the new infolevel counterparts of the old info level + parmnum
//

#define GROUP_ALL_INFOLEVEL             \
            (PARMNUM_BASE_INFOLEVEL + GROUP_ALL_PARMNUM)
#define GROUP_NAME_INFOLEVEL            \
            (PARMNUM_BASE_INFOLEVEL + GROUP_NAME_PARMNUM)
#define GROUP_COMMENT_INFOLEVEL         \
            (PARMNUM_BASE_INFOLEVEL + GROUP_COMMENT_PARMNUM)
#define GROUP_ATTRIBUTES_INFOLEVEL      \
            (PARMNUM_BASE_INFOLEVEL + GROUP_ATTRIBUTES_PARMNUM)
#define GROUP_POSIX_ID_INFOLEVEL        \
            (PARMNUM_BASE_INFOLEVEL + GROUP_POSIX_ID_PARMNUM)

#endif  // _LMGROUP_

//
// LocalGroup Class
//

#ifndef _LMLOCALGROUP_
#define _LMLOCALGROUP_

//
// Function Prototypes
//

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupAdd (
    IN  LPCWSTR   servername OPTIONAL,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupAddMember (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  PSID     membersid
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupEnum (
    IN  LPCWSTR      servername OPTIONAL,
    IN  DWORD       level,
    OUT LPBYTE      *bufptr,
    IN  DWORD       prefmaxlen,
    OUT LPDWORD     entriesread,
    OUT LPDWORD     totalentries,
    IN OUT PDWORD_PTR resumehandle OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupGetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  DWORD    level,
    OUT LPBYTE   *bufptr
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupSetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupDel (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupDelMember (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   groupname,
    IN  PSID     membersid
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupGetMembers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     localgroupname,
    IN  DWORD      level,
    OUT LPBYTE     *bufptr,
    IN  DWORD      prefmaxlen,
    OUT LPDWORD    entriesread,
    OUT LPDWORD    totalentries,
    IN OUT PDWORD_PTR resumehandle
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupSetMembers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     groupname,
    IN  DWORD      level,
    IN  LPBYTE     buf,
    IN  DWORD      totalentries
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupAddMembers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     groupname,
    IN  DWORD      level,
    IN  LPBYTE     buf,
    IN  DWORD      totalentries
    );

NET_API_STATUS NET_API_FUNCTION
NetLocalGroupDelMembers (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     groupname,
    IN  DWORD      level,
    IN  LPBYTE     buf,
    IN  DWORD      totalentries
    );

//
//  Data Structures - LocalGroup
//

  PLOCALGROUP_INFO_0 = ^TLOCALGROUP_INFO_0;
  TLOCALGROUP_INFO_0 = record
    LPWSTR   lgrpi0_name;
  end;

  PLOCALGROUP_INFO_1 = ^TLOCALGROUP_INFO_1;
  TLOCALGROUP_INFO_1 = record
    LPWSTR   lgrpi1_name;
    LPWSTR   lgrpi1_comment;
  end;

  PLOCALGROUP_INFO_1002 = ^TLOCALGROUP_INFO_1002;
  TLOCALGROUP_INFO_1002 = record
     LPWSTR  lgrpi1002_comment;
  end;

  PLOCALGROUP_MEMBERS_INFO_0 = ^TLOCALGROUP_MEMBERS_INFO_0;
  TLOCALGROUP_MEMBERS_INFO_0 = record
     PSID    lgrmi0_sid;
  end;

  PLOCALGROUP_MEMBERS_INFO_1 = ^TLOCALGROUP_MEMBERS_INFO_1;
  TLOCALGROUP_MEMBERS_INFO_1 = record
     PSID         lgrmi1_sid;
     SID_NAME_USE lgrmi1_sidusage;
     LPWSTR       lgrmi1_name;
  end;

  PLOCALGROUP_MEMBERS_INFO_2 = ^TLOCALGROUP_MEMBERS_INFO_2;
  TLOCALGROUP_MEMBERS_INFO_2 = record
     PSID         lgrmi2_sid;
     SID_NAME_USE lgrmi2_sidusage;
     LPWSTR       lgrmi2_domainandname;
  end;

  PLOCALGROUP_MEMBERS_INFO_3 = ^TLOCALGROUP_MEMBERS_INFO_3;
  TLOCALGROUP_MEMBERS_INFO_3 = record
     LPWSTR       lgrmi3_domainandname;
  end;

  PLOCALGROUP_USERS_INFO_0 = ^TLOCALGROUP_USERS_INFO_0;
  TLOCALGROUP_USERS_INFO_0 = record
     LPWSTR  lgrui0_name;
  end;


#define LOCALGROUP_NAME_PARMNUM          1
#define LOCALGROUP_COMMENT_PARMNUM       2

//
// Display Information APIs
//
}
function NetQueryDisplayInformation(ServerName: PWideChar; Level, Index,
  EntriesRequested, PreferredMaximumLength: DWord; var ReturnedEntryCount: DWord;
  var SortedBuffer: PByte): NET_API_STATUS cdecl stdcall;
function NetGetDisplayInformationIndex(ServerName: PWideChar; Level: DWord;
  Prefix: PWideChar; Index: PDWord): NET_API_STATUS cdecl stdcall;

//
// QueryDisplayInformation levels
type
  PNET_DISPLAY_USER = ^TNET_DISPLAY_USER;
  TNET_DISPLAY_USER = record
    usri1_name: LPWSTR;       { Pointer to a Unicode string that specifies the name of the user account. }
    usri1_comment: LPWSTR;    { Pointer to a Unicode string that contains a comment associated with the user. This string can be a null string, or it can have any number of characters before the terminating null character (MAXCOMMENTSZ). }
    usri1_flags: DWORD;
    usri1_full_name: LPWSTR;  { Pointer to a Unicode string that contains the full name of the user. This string can be a null string, or it can have any number of characters before the terminating null character. }
    usri1_user_id: DWORD;     { Specifies the relative identifier of the user. The relative identifier is determined by the accounts database when the user is created. It uniquely defines this user account to the account manager within the domain.  }
    usri1_next_index: DWORD;  { Specifies the index of the next entry to return from the NetQueryDisplayInformation function. Pass this value as the Index parameter of NetQueryDisplayInformation to return the next logical entry. }
  end;

  PNET_DISPLAY_MACHINE = ^TNET_DISPLAY_MACHINE;
  TNET_DISPLAY_MACHINE = record
    usri2_name: LPWSTR   ;
    usri2_comment: LPWSTR   ;
    usri2_flags: DWORD    ;
    usri2_user_id: DWORD    ;
    usri2_next_index: DWORD    ;
  end;

  PNET_DISPLAY_GROUP = ^TNET_DISPLAY_GROUP;
  TNET_DISPLAY_GROUP = record
    grpi3_name: LPWSTR   ;
    grpi3_comment: LPWSTR   ;
    grpi3_group_id: DWORD    ;
    grpi3_attributes: DWORD    ;
    grpi3_next_index: DWORD    ;
  end;

{
//
// Access Class
//

#ifndef _LMACCESS_
#define _LMACCESS_



//
// Function Prototypes - Access
//
//
// The NetAccess APIs are only available to downlevel
//

#define NetAccessAdd RxNetAccessAdd

NET_API_STATUS NET_API_FUNCTION
NetAccessAdd (
    IN  LPCWSTR   servername OPTIONAL,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

#define NetAccessEnum RxNetAccessEnum

NET_API_STATUS NET_API_FUNCTION
NetAccessEnum (
    IN  LPCWSTR     servername OPTIONAL,
    IN  LPCWSTR     BasePath,
    IN  DWORD      Recursive,
    IN  DWORD      level,
    OUT LPBYTE     *bufptr,
    IN  DWORD      prefmaxlen,
    OUT LPDWORD    entriesread,
    OUT LPDWORD    totalentries,
    IN OUT LPDWORD resume_handle OPTIONAL
    );

#define NetAccessGetInfo RxNetAccessGetInfo

NET_API_STATUS NET_API_FUNCTION
NetAccessGetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   resource,
    IN  DWORD    level,
    OUT LPBYTE   *bufptr
    );

#define NetAccessSetInfo RxNetAccessSetInfo

NET_API_STATUS NET_API_FUNCTION
NetAccessSetInfo (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   resource,
    IN  DWORD    level,
    IN  LPBYTE   buf,
    OUT LPDWORD  parm_err OPTIONAL
    );

#define NetAccessDel RxNetAccessDel

NET_API_STATUS NET_API_FUNCTION
NetAccessDel (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   resource
    );

#define NetAccessGetUserPerms RxNetAccessGetUserPerms

NET_API_STATUS NET_API_FUNCTION
NetAccessGetUserPerms (
    IN  LPCWSTR   servername OPTIONAL,
    IN  LPCWSTR   UGname,
    IN  LPCWSTR   resource,
    OUT LPDWORD  Perms
    );

//
// Data Structures - Access
//

  PACCESS_INFO_0 = ^TACCESS_INFO_0;
  TACCESS_INFO_0 = record
    LPWSTR   acc0_resource_name;
  end;

  PACCESS_INFO_1 = ^TACCESS_INFO_1;
  TACCESS_INFO_1 = record
    LPWSTR   acc1_resource_name;
    DWORD    acc1_attr;
    DWORD    acc1_count;
  end;

  PACCESS_INFO_1002 = ^TACCESS_INFO_1002;
  TACCESS_INFO_1002 = record
     DWORD   acc1002_attr;
  end;


  PACCESS_LIST = ^TACCESS_LIST;
  TACCESS_LIST = record
    LPWSTR   acl_ugname;
    DWORD    acl_access;
  end;

//
// Special Values and Constants - Access
//

//
// Maximum number of permission entries for each resource.
//

#define MAXPERMENTRIES      64

//
//  Bit values for the access permissions.  ACCESS_ALL is a handy
//  way to specify maximum permissions.  These are used in
//  acl_access field of access_list structures.
//

#define ACCESS_NONE         0
#define ACCESS_ALL          ( ACCESS_READ | \
                                ACCESS_WRITE | \
                                ACCESS_CREATE | \
                                ACCESS_EXEC | \
                                ACCESS_DELETE | \
                                ACCESS_ATRIB | \
                                ACCESS_PERM \
                            )

#define ACCESS_READ         0x01
#define ACCESS_WRITE        0x02
#define ACCESS_CREATE       0x04
#define ACCESS_EXEC         0x08
#define ACCESS_DELETE       0x10
#define ACCESS_ATRIB        0x20
#define ACCESS_PERM         0x40

#define ACCESS_GROUP        0x8000

//
// Bit values for the acc1_attr field of the ACCESS_INFO_1 structure.
//

#define ACCESS_AUDIT        0x1

#define ACCESS_SUCCESS_OPEN         0x10
#define ACCESS_SUCCESS_WRITE        0x20
#define ACCESS_SUCCESS_DELETE       0x40
#define ACCESS_SUCCESS_ACL          0x80
#define ACCESS_SUCCESS_MASK         0xF0

#define ACCESS_FAIL_OPEN            0x100
#define ACCESS_FAIL_WRITE           0x200
#define ACCESS_FAIL_DELETE          0x400
#define ACCESS_FAIL_ACL             0x800
#define ACCESS_FAIL_MASK            0xF00

#define ACCESS_FAIL_SHIFT           4

//
// Parmnum value for NetAccessSetInfo.
//

#define ACCESS_RESOURCE_NAME_PARMNUM    1
#define ACCESS_ATTR_PARMNUM             2
#define ACCESS_COUNT_PARMNUM            3
#define ACCESS_ACCESS_LIST_PARMNUM      4

//
// the new infolevel counterparts of the old info level + parmnum
//

#define ACCESS_RESOURCE_NAME_INFOLEVEL  \
            (PARMNUM_BASE_INFOLEVEL + ACCESS_RESOURCE_NAME_PARMNUM)
#define ACCESS_ATTR_INFOLEVEL           \
            (PARMNUM_BASE_INFOLEVEL + ACCESS_ATTR_PARMNUM)
#define ACCESS_COUNT_INFOLEVEL          \
            (PARMNUM_BASE_INFOLEVEL + ACCESS_COUNT_PARMNUM)
#define ACCESS_ACCESS_LIST_INFOLEVEL    \
            (PARMNUM_BASE_INFOLEVEL + ACCESS_ACCESS_LIST_PARMNUM)

//
// ACCESS_LETTERS defines a letter for each bit position in
// the acl_access field of struct access_list.  Note that some
// bits have a corresponding letter of ' ' (space).
//

#define ACCESS_LETTERS      "RWCXDAP         "

#endif // _LMACCESS_
}
//
// Domain Class
//

//
// Function Prototypes - Domain
//

function NetUserAdd(ServerName: PWideChar; Level: Longint; Buffer: PByte; Error: PDWord): NET_API_STATUS cdecl stdcall;
function NetGetDCName(ServerName, DomainName: PWideChar; var Buffer: PByte): NET_API_STATUS cdecl stdcall;
function NetGetAnyDCName(ServerName, DomainName: PWideChar; var Buffer: PByte): NET_API_STATUS cdecl stdcall;
function I_NetLogonControl(ServerName: PWideChar; FunctionCode, QueryLevel: DWord; var Buffer: PByte): NET_API_STATUS cdecl stdcall;
function I_NetLogonControl2(ServerName: PWideChar; FunctionCode, QueryLevel: DWord; Data: PByte; var Buffer: PByte): NET_API_STATUS cdecl stdcall;

{
#ifndef _NTDEF_
typedef LONG NTSTATUS, *PNTSTATUS;
#endif

NTSTATUS
NetEnumerateTrustedDomains (
    IN LPWSTR ServerName OPTIONAL,
    OUT LPWSTR *DomainNames
    );



//
// Special Values and Constants - Domain
//

//
// FunctionCode values for I_NetLogonControl.
//
// NOTE : if you change the following NETLOGON_CONTROL_* values,
// change them in net\svcdlls\logonsrv\logon.idl file also.
//

#define NETLOGON_CONTROL_QUERY         1    // No-op: just query
#define NETLOGON_CONTROL_REPLICATE     2    // Force replicate on BDC
#define NETLOGON_CONTROL_SYNCHRONIZE   3    // Force synchronize on BDC
#define NETLOGON_CONTROL_PDC_REPLICATE 4    // Force PDC to broadcast change
#define NETLOGON_CONTROL_REDISCOVER    5    // Force to re-discover trusted domain DCs
#define NETLOGON_CONTROL_TC_QUERY      6    // Query status of specified trusted channel status
#define NETLOGON_CONTROL_TRANSPORT_NOTIFY 7 // Notify netlogon that a new transport has come online
#define NETLOGON_CONTROL_FIND_USER     8    // Find named user in a trusted domain

// Debug function codes

#define NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL 0xFFFB
#define NETLOGON_CONTROL_BACKUP_CHANGE_LOG  0xFFFC
#define NETLOGON_CONTROL_TRUNCATE_LOG       0xFFFD
#define NETLOGON_CONTROL_SET_DBFLAG         0xFFFE
#define NETLOGON_CONTROL_BREAKPOINT         0xFFFF

//
// Query level 1 for I_NetLogonControl
//

  PNETLOGON_INFO_1 = ^TNETLOGON_INFO_1;
  TNETLOGON_INFO_1 = record
        DWORD netlog1_flags;
        NET_API_STATUS netlog1_pdc_connection_status;
  end;

  PNETLOGON_INFO_2 = ^TNETLOGON_INFO_2;
  TNETLOGON_INFO_2 = record
        DWORD netlog2_flags;
        NET_API_STATUS netlog2_pdc_connection_status;
#ifdef MIDL_PASS
        [string] wchar_t * netlog2_trusted_dc_name;
#else
        LPWSTR netlog2_trusted_dc_name;
#endif // MIDL_PASS
        NET_API_STATUS netlog2_tc_connection_status;
  end;

  PNETLOGON_INFO_3 = ^TNETLOGON_INFO_3;
  TNETLOGON_INFO_3 = record
        DWORD netlog3_flags;
        DWORD netlog3_logon_attempts;
        DWORD netlog3_reserved1;
        DWORD netlog3_reserved2;
        DWORD netlog3_reserved3;
        DWORD netlog3_reserved4;
        DWORD netlog3_reserved5;
  end;

  PNETLOGON_INFO_4 = ^TNETLOGON_INFO_4;
  TNETLOGON_INFO_4 = record
#ifdef MIDL_PASS
        [string] wchar_t * netlog4_trusted_dc_name;
        [string] wchar_t * netlog4_trusted_domain_name;
#else
        LPWSTR netlog4_trusted_dc_name;
        LPWSTR netlog4_trusted_domain_name;
#endif // MIDL_PASS
  end;

//
// Values of netlog1_flags
//

#define NETLOGON_REPLICATION_NEEDED       0x01  // Database is out of date
#define NETLOGON_REPLICATION_IN_PROGRESS  0x02  // Replication is happening now
#define NETLOGON_FULL_SYNC_REPLICATION    0x04  // full sync replication required/progress
#define NETLOGON_REDO_NEEDED              0x08  // Redo of previous replication needed

#ifdef __cplusplus
}

implementation

const
  NetApi32 = 'netapi32.dll';

function NetUserEnum; external NetApi32 name 'NetUserEnum';
function NetUserGetInfo; external NetApi32 name 'NetUserGetInfo';

function NetUserAdd; external NetApi32 name 'NetUserAdd';
function NetGetDCName; external NetApi32 name 'NetGetDCName';
function NetGetAnyDCName; external NetApi32 name 'NetGetAnyDCName';
function I_NetLogonControl; external NetApi32 name 'I_NetLogonControl';
function I_NetLogonControl2; external NetApi32 name 'I_NetLogonControl2';

function NetQueryDisplayInformation; external NetApi32 name 'NetQueryDisplayInformation';
function NetGetDisplayInformationIndex; external NetApi32 name 'NetGetDisplayInformationIndex';

end.
