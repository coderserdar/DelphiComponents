unit LMCons;

interface

uses
  Windows;

const
  CNLEN       = 15;                  // Computer name length
  LM20_CNLEN  = 15;                  // LM 2.0 Computer name length
  DNLEN       = CNLEN;               // Maximum domain name length
  LM20_DNLEN  = LM20_CNLEN;          // LM 2.0 Maximum domain name length

  UNCLEN      = (CNLEN+2);           // UNC computer name length
  LM20_UNCLEN = (LM20_CNLEN+2);      // LM 2.0 UNC computer name length

  NNLEN       = 80;                  // Net name length (share name)
  LM20_NNLEN  = 12;                  // LM 2.0 Net name length

  RMLEN       = (UNCLEN+1+NNLEN);    // Max remote name length
  LM20_RMLEN  = (LM20_UNCLEN+1+LM20_NNLEN); // LM 2.0 Max remote name length

  SNLEN       = 80;                  // Service name length
  LM20_SNLEN  = 15;                  // LM 2.0 Service name length
  STXTLEN     = 256;                 // Service text length
  LM20_STXTLEN = 63;                 // LM 2.0 Service text length

  PATHLEN     = 256;                 // Max. path (not including drive name)
  LM20_PATHLEN = 256;                // LM 2.0 Max. path

  DEVLEN      = 80;                  // Device name length
  LM20_DEVLEN = 8;                   // LM 2.0 Device name length

  EVLEN       = 16;                  // Event name length

//
// User, Group and Password lengths
//

  UNLEN       = 256;                 // Maximum user name length
  LM20_UNLEN  = 20;                  // LM 2.0 Maximum user name length

  GNLEN       = UNLEN;               // Group name
  LM20_GNLEN  = LM20_UNLEN;          // LM 2.0 Group name

  PWLEN       = 256;                 // Maximum password length
  LM20_PWLEN  = 14;                  // LM 2.0 Maximum password length

  SHPWLEN     = 8;                   // Share password length (bytes)


  CLTYPE_LEN  = 12;                  // Length of client type string


  MAXCOMMENTSZ = 256;                // Multipurpose comment length
  LM20_MAXCOMMENTSZ = 48;            // LM 2.0 Multipurpose comment length

  QNLEN       = NNLEN;               // Queue name maximum length
  LM20_QNLEN  = LM20_NNLEN;          // LM 2.0 Queue name maximum length

//
// The ALERTSZ and MAXDEVENTRIES defines have not yet been NT'ized.
// Whoever ports these components should change these values appropriately.
//

  ALERTSZ     = 128;                 // size of alert string in server
  MAXDEVENTRIES = (sizeof(Integer)*8);  // Max number of device entries

                                        //
                                        // We use int bitmap to represent
                                        //

  NETBIOS_NAME_LEN  = 16;            // NetBIOS net name (bytes)

//
// Value to be used with APIs which have a "preferred maximum length"
// parameter.  This value indicates that the API should just allocate
// "as much as it takes."
//

  MAX_PREFERRED_LENGTH = $FFFFFFFF;

//
//        Constants used with encryption
//

  CRYPT_KEY_LEN           = 7;
  CRYPT_TXT_LEN           = 8;
  ENCRYPTED_PWLEN         = 16;
  SESSION_PWLEN           = 24;
  SESSION_CRYPT_KLEN      = 21;

//
//  Value to be used with SetInfo calls to allow setting of all
//  settable parameters (parmnum zero option)
//
  PARMNUM_ALL             = 0;

  PARM_ERROR_UNKNOWN      = -1;
  PARM_ERROR_NONE         = 0;
  PARMNUM_BASE_INFOLEVEL  = 1000;

//
//        Message File Names
//

  MESSAGE_FILENAME        = 'NETMSG';
  OS2MSG_FILENAME         = 'BASE';
  HELP_MSG_FILENAME       = 'NETH';

{ INTERNAL_ONLY }

// The backup message file named here is a duplicate of net.msg. It
// is not shipped with the product, but is used at buildtime to
// msgbind certain messages to netapi.dll and some of the services.
// This allows for OEMs to modify the message text in net.msg and
// have those changes show up.        Only in case there is an error in
// retrieving the messages from net.msg do we then get the bound
// messages out of bak.msg (really out of the message segment).

  BACKUP_MSG_FILENAME     = 'BAK.MSG';

{ END_INTERNAL }

//
// Keywords used in Function Prototypes
//

type
  NET_API_STATUS          = DWord;
  API_RET_TYPE            = NET_API_STATUS;      // Old value: do not use

//
// The platform ID indicates the levels to use for platform-specific
// information.

//
const
  PLATFORM_ID_DOS = 300;
  PLATFORM_ID_OS2 = 400;
  PLATFORM_ID_NT  = 500;
  PLATFORM_ID_OSF = 600;
  PLATFORM_ID_VMS = 700;

//
//      There message numbers assigned to different LANMAN components
//      are as defined below.
//
//      lmerr.h:        2100 - 2999     NERR_BASE
//      alertmsg.h:     3000 - 3049     ALERT_BASE
//      lmsvc.h:        3050 - 3099     SERVICE_BASE
//      lmerrlog.h:     3100 - 3299     ERRLOG_BASE
//      msgtext.h:      3300 - 3499     MTXT_BASE
//      apperr.h:       3500 - 3999     APPERR_BASE
//      apperrfs.h:     4000 - 4299     APPERRFS_BASE
//      apperr2.h:      4300 - 5299     APPERR2_BASE
//      ncberr.h:       5300 - 5499     NRCERR_BASE
//      alertmsg.h:     5500 - 5599     ALERT2_BASE
//      lmsvc.h:        5600 - 5699     SERVICE2_BASE
//      lmerrlog.h      5700 - 5799     ERRLOG2_BASE
//

  {MIN_LANMAN_MESSAGE_ID  = NERR_BASE;}
  MAX_LANMAN_MESSAGE_ID  = 5799;

implementation

end.
