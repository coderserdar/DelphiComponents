{*******************************************************}
{File:      NCOci.PAS                                   }
{Revision:  0.04.01 / 13.04.2005                        }
{Comment:   NC OCI8 VCL: OCI8 interface unit            }
{Copyright: (c) 1999-2005, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}
{CE_Desc_Include(HelpText\NCOci.txt)}

unit NCOci;

interface

Uses Classes, DB;

type
    // Generic Oracle Types
    sword   = Integer;
    eword   = Integer;
    uword   = {$IFDEF OCI_D4} LongWord {$ELSE} LongInt {$ENDIF};
    sb4     = Integer;
    ub4     = {$IFDEF OCI_D4} LongWord {$ELSE} LongInt {$ENDIF};
    sb2     = SmallInt;
    ub2     = Word;
    sb1     = ShortInt;
    ub1     = Byte;
    dvoid   = Pointer;
    text    = PChar;
    size_T  = Integer;

    pUb1 = ^ub1;
    pSb1 = ^sb1;
    pUb2 = ^ub2;
    pSb2 = ^sb2;
    pUb4 = ^ub4;
    pSb4 = ^sb4;

    ppUb1 = ^pUb1;
    ppUb4 = ^pUb4;

    TSB2Array = array[0..$FFFF] of sb2;
    PSB2Array = ^TSB2Array;
    TUB2Array = array[0..$FFFF] of ub2;
    PUB2Array = ^TUB2Array;
    TBoolArray = array[0..$FFFF] of Boolean;
    PBoolArray = ^TBoolArray;

    // Handle Types
    pOCIHandle = pointer;
    ppOCIHandle = ^pointer;
    pOCIEnv = pOCIHandle;
    pOCIServer = pOCIHandle;
    pOCIError = pOCIHandle;
    pOCISvcCtx = pOCIHandle;
    pOCIStmt = pOCIHandle;
    pOCIDefine = pOCIHandle;
    pOCISession = pOCIHandle;
    pOCIBind = pOCIHandle;
    pOCIDescribe = pOCIHandle;
    pOCIDirPathCtx = pOCIHandle;
    pOCIDirPathColArray = pOCIHandle;
    pOCIDirPathStream = pOCIHandle;

    // Descriptor Types
    pOCIDescriptor = pointer;
    pOCISnapshot = pOCIDescriptor;
    pOCILobLocator = pOCIDescriptor;
    pOCIParam = pOCIDescriptor;
    pOCIRowid = pOCIDescriptor;
    pOCIComplexObjectComp = pOCIDescriptor;
    pOCIAQEnqOptions = pOCIDescriptor;
    pOCIAQDeqOptions = pOCIDescriptor;
    pOCIAQMsgProperties = pOCIDescriptor;
    pOCIAQAgent = pOCIDescriptor;
    pOCIDirPathDesc = pOCIDescriptor;

    // OBJECT Duration
    OCIDuration = ub2;

    EOCIErrorBase = class(EDatabaseError);      // base error class
        EOCIDBError = class(EOCIErrorBase);     // NCOCI8 errors
     // EOCINativeError = class(EOCIErrorBase); // ORACLE errors
     // ... placed in NCOciWrapper

const
    MAXTXNAMELEN    = 64;
    XIDDATASIZE     = 128; // size in bytes
    MAXGTRIDSIZE    = 64;  // maximum size in bytes of gtrid
    MAXBQUALSIZE    = 64;  // maximum size in bytes of bqual
    NULLXID_ID      = -1;
    // Transaction branch identification: XID and NULLXID:
type
    PXID = ^TXID;
    TXID = record
        formatID: sb4;     // format identifier
        gtrid_length: sb4; // value from 1 through 64
        bqual_length: sb4; // value from 1 through 64
        data: array [0 .. XIDDATASIZE - 1] of ub1;
    end;

const
    MAXUB4  = High(ub4);
    MAXSB4  = High(sb4);

    // OCI Handle Types
    OCI_HTYPE_FIRST     = 1;
    OCI_HTYPE_ENV       = 1;
    OCI_HTYPE_ERROR     = 2;
    OCI_HTYPE_SVCCTX    = 3;
    OCI_HTYPE_STMT      = 4;
    OCI_HTYPE_BIND      = 5;
    OCI_HTYPE_DEFINE    = 6;
    OCI_HTYPE_DESCRIBE  = 7;
    OCI_HTYPE_SERVER    = 8;
    OCI_HTYPE_SESSION   = 9;
    OCI_HTYPE_TRANS     = 10;
    OCI_HTYPE_COMPLEXOBJECT = 11;
    OCI_HTYPE_SECURITY  = 12;
    OCI_HTYPE_SUBSCRIPTION      = 13;           // subscription handle
    OCI_HTYPE_DIRPATH_CTX       = 14;           // direct path context
    OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;        // direct path column array
    OCI_HTYPE_DIRPATH_STREAM       = 16;        // direct path stream
    OCI_HTYPE_PROC              = 17;           // process handle
    OCI_HTYPE_LAST              = 17;

    // OCI Descriptor Types
    OCI_DTYPE_FIRST     = 50;
    OCI_DTYPE_LOB       = 50;
    OCI_DTYPE_SNAP      = 51;
    OCI_DTYPE_RSET      = 52;
    OCI_DTYPE_PARAM     = 53;
    OCI_DTYPE_ROWID     = 54;
    OCI_DTYPE_COMPLEXOBJECTCOMP = 55;
    OCI_DTYPE_FILE      = 56;
    OCI_DTYPE_AQENQ_OPTIONS     = 57;
    OCI_DTYPE_AQDEQ_OPTIONS     = 58;
    OCI_DTYPE_AQMSG_PROPERTIES  = 59;
    OCI_DTYPE_AQAGENT           = 60;
    OCI_DTYPE_LOCATOR   = 61;                   // LOB locator
    OCI_DTYPE_DATETIME  = 62;                   // datetime structure
    OCI_DTYPE_INTERVAL  = 63;                   // interval structure
    OCI_DTYPE_AQNFY_DESCRIPTOR  = 64;           // AQ notify descriptor
    OCI_DTYPE_LAST      = 64;

    // OCI Attributes Types
    OCI_ATTR_FNCODE =   1;                              // the OCI function code
    OCI_ATTR_OBJECT =    2;                             // is the environment initialized in object mode
    OCI_ATTR_NONBLOCKING_MODE =   3;                    // non blocking mode
    OCI_ATTR_SQLCODE =   4;                             // the SQL verb
    OCI_ATTR_ENV =   5;                                 // the environment handle
    OCI_ATTR_SERVER =  6;                               // the server handle
    OCI_ATTR_SESSION =  7;                              // the user session handle
    OCI_ATTR_TRANS =    8;                              // the transaction handle
    OCI_ATTR_ROW_COUNT =    9;                          // the rows processed so far
    OCI_ATTR_SQLFNCODE =  10;                           // the SQL verb of the statement
    OCI_ATTR_PREFETCH_ROWS =   11;                      // sets the number of rows to prefetch
    OCI_ATTR_NESTED_PREFETCH_ROWS =  12;                // the prefetch rows of nested table
    OCI_ATTR_PREFETCH_MEMORY =  13;                     // memory limit for rows fetched
    OCI_ATTR_NESTED_PREFETCH_MEMORY =  14;              // memory limit for nested rows
    OCI_ATTR_CHAR_COUNT =   15;                         // this specifies the bind and define size in characters
    OCI_ATTR_PDSCL =    16;                             // packed decimal scale
    OCI_ATTR_FSPRECISION = OCI_ATTR_PDSCL;              // fs prec for datetime data types
    OCI_ATTR_PDPRC =    17;                             // packed decimal format
    OCI_ATTR_LFPRECISION = OCI_ATTR_PDPRC;              // fs prec for datetime data types
    OCI_ATTR_PARAM_COUNT =  18;                         // number of column in the select list
    OCI_ATTR_ROWID =    19;                             // the rowid
    OCI_ATTR_CHARSET =   20;                            // the character set value
    OCI_ATTR_NCHAR =    21;                             // NCHAR type
    OCI_ATTR_USERNAME =  22;                            // username attribute
    OCI_ATTR_PASSWORD =  23;                            // password attribute
    OCI_ATTR_STMT_TYPE =    24;                         // statement type
    OCI_ATTR_INTERNAL_NAME =    25;                     // user friendly global name
    OCI_ATTR_EXTERNAL_NAME =    26;                     // the internal name for global txn
    OCI_ATTR_XID =      27;                             // XOPEN defined global transaction id
    OCI_ATTR_TRANS_LOCK =  28;                          //
    OCI_ATTR_TRANS_NAME =  29;                          // string to identify a global transaction
    OCI_ATTR_HEAPALLOC =  30;                           // memory allocated on the heap
    OCI_ATTR_CHARSET_ID =  31;                          // Character Set ID
    OCI_ATTR_CHARSET_FORM =  32;                        // Character Set Form
    OCI_ATTR_MAXDATA_SIZE =  33;                        // Maximumsize of data on the server
    OCI_ATTR_CACHE_OPT_SIZE =  34;                      // object cache optimal size
    OCI_ATTR_CACHE_MAX_SIZE =  35;                      // object cache maximum size percentage
    OCI_ATTR_PINOPTION =  36;                           // object cache default pin option
    OCI_ATTR_ALLOC_DURATION =  37;                      // object cache default allocation duration
    OCI_ATTR_PIN_DURATION =  38;                        // object cache default pin duration
    OCI_ATTR_FDO =           39;                        // Format Descriptor object attribute
    OCI_ATTR_POSTPROCESSING_CALLBACK =  40;             // Callback to process outbind data
    OCI_ATTR_POSTPROCESSING_CONTEXT =  41;              // Callback context to process outbind data
    OCI_ATTR_ROWS_RETURNED =  42;                       // Number of rows returned in current iter - for Bind handles
    OCI_ATTR_FOCBK =         43;                        // Failover Callback attribute
    OCI_ATTR_IN_V8_MODE  = 44;                          // is the server/service context in V8 mode
    OCI_ATTR_LOBEMPTY =      45;                        // empty lob ?
    OCI_ATTR_SESSLANG =      46;                        // session language handle

    OCI_ATTR_VISIBILITY =              47;              // visibility
    OCI_ATTR_RELATIVE_MSGID =          48;              // relative message id
    OCI_ATTR_SEQUENCE_DEVIATION =      49;              // sequence deviation

    OCI_ATTR_CONSUMER_NAME =           50;              // consumer name
    OCI_ATTR_DEQ_MODE =                51;              // dequeue mode
    OCI_ATTR_NAVIGATION =              52;              // navigation
    OCI_ATTR_WAIT =                    53;              // wait
    OCI_ATTR_DEQ_MSGID =               54;              // dequeue message id

    OCI_ATTR_PRIORITY =                55;              // priority
    OCI_ATTR_DELAY =                   56;              // delay
    OCI_ATTR_EXPIRATION =              57;              // expiration
    OCI_ATTR_CORRELATION =             58;              // correlation id
    OCI_ATTR_ATTEMPTS =                59;              // # of attempts
    OCI_ATTR_RECIPIENT_LIST =          60;              // recipient list
    OCI_ATTR_EXCEPTION_QUEUE =         61;              // exception queue name
    OCI_ATTR_ENQ_TIME =                62;              // enqueue time (only OCIAttrGet)
    OCI_ATTR_MSG_STATE =               63;              // message state (only OCIAttrGet)
                                                        // NOTE: 64-66 used below
    OCI_ATTR_AGENT_NAME =              64;              // agent name
    OCI_ATTR_AGENT_ADDRESS =           65;              // agent address
    OCI_ATTR_AGENT_PROTOCOL =          66;              // agent protocol

    OCI_ATTR_SENDER_ID =               68;              // sender id
    OCI_ATTR_ORIGINAL_MSGID =          69;              // original message id

    OCI_ATTR_QUEUE_NAME =              70;              // queue name
    OCI_ATTR_NFY_MSGID =               71;              // message id
    OCI_ATTR_MSG_PROP =                72;              // message properties

    OCI_ATTR_NUM_DML_ERRORS =          73;              // num of errs in array DML
    OCI_ATTR_DML_ROW_OFFSET =          74;              // row offset in the array

    OCI_ATTR_DATEFORMAT =              75;              // default date format string
    OCI_ATTR_BUF_ADDR =                76;              // buffer address
    OCI_ATTR_BUF_SIZE =                77;              // buffer size
    OCI_ATTR_DIRPATH_MODE =            78;              // mode of direct path operation
    OCI_ATTR_DIRPATH_NOLOG =           79;              // nologging option
    OCI_ATTR_DIRPATH_PARALLEL =        80;              // parallel (temp seg) option
    OCI_ATTR_NUM_ROWS =                81;              // number of rows in column array
                                                        // NOTE that OCI_ATTR_NUM_COLS is a column
                                                        // array attribute too.

    OCI_ATTR_COL_COUNT =               82;              // columns of column array processed so far.
    OCI_ATTR_STREAM_OFFSET =           83;              // str off of last row processed
    OCI_ATTR_SHARED_HEAPALLOC =        84;              // Shared Heap Allocation Size

    OCI_ATTR_SERVER_GROUP =            85;              // server group name

    OCI_ATTR_MIGSESSION =              86;              // migratable session attribute

    OCI_ATTR_NOCACHE =                 87;              // Temporary LOBs

    OCI_ATTR_MEMPOOL_SIZE =            88;              // Pool Size
    OCI_ATTR_MEMPOOL_INSTNAME =        89;              // Instance name
    OCI_ATTR_MEMPOOL_APPNAME =         90;              // Application name
    OCI_ATTR_MEMPOOL_HOMENAME =        91;              // Home Directory name
    OCI_ATTR_MEMPOOL_MODEL =           92;              // Pool Model (proc,thrd,both)
    OCI_ATTR_MODES =                   93;              // Modes

    OCI_ATTR_SUBSCR_NAME =             94;              // name of subscription
    OCI_ATTR_SUBSCR_CALLBACK =         95;              // associated callback
    OCI_ATTR_SUBSCR_CTX =              96;              // associated callback context
    OCI_ATTR_SUBSCR_PAYLOAD =          97;              // associated payload
    OCI_ATTR_SUBSCR_NAMESPACE =        98;              // associated namespace

    OCI_ATTR_PROXY_CREDENTIALS =       99;              // Proxy user credentials
    OCI_ATTR_INITIAL_CLIENT_ROLES =   100;              // Initial client role list

    OCI_ATTR_UNK =               101;                   // unknown attribute
    OCI_ATTR_NUM_COLS =          102;                   // number of columns
    OCI_ATTR_LIST_COLUMNS =      103;                   // parameter of the column list
    OCI_ATTR_RDBA =              104;                   // DBA of the segment header
    OCI_ATTR_CLUSTERED =         105;                   // whether the table is clustered
    OCI_ATTR_PARTITIONED =       106;                   // whether the table is partitioned
    OCI_ATTR_INDEX_ONLY =        107;                   // whether the table is index only
    OCI_ATTR_LIST_ARGUMENTS =    108;                   // parameter of the argument list
    OCI_ATTR_LIST_SUBPROGRAMS =  109;                   // parameter of the subprogram list
    OCI_ATTR_REF_TDO =           110;                   // REF to the type descriptor
    OCI_ATTR_LINK =              111;                   // the database link name
    OCI_ATTR_MIN =               112;                   // minimum value
    OCI_ATTR_MAX =               113;                   // maximum value
    OCI_ATTR_INCR =              114;                   // increment value
    OCI_ATTR_CACHE =             115;                   // number of sequence numbers cached
    OCI_ATTR_ORDER =             116;                   // whether the sequence is ordered
    OCI_ATTR_HW_MARK =           117;                   // high-water mark
    OCI_ATTR_TYPE_SCHEMA =       118;                   // type's schema name
    OCI_ATTR_TIMESTAMP =         119;                   // timestamp of the object
    OCI_ATTR_NUM_ATTRS =         120;                   // number of sttributes
    OCI_ATTR_NUM_PARAMS =        121;                   // number of parameters
    OCI_ATTR_OBJID =             122;                   // object id for a table or view
    OCI_ATTR_PTYPE =             123;                   // type of info described by
    OCI_ATTR_PARAM =             124;                   // parameter descriptor
    OCI_ATTR_OVERLOAD_ID =       125;                   // overload ID for funcs and procs
    OCI_ATTR_TABLESPACE =        126;                   // table name space
    OCI_ATTR_TDO =               127;                   // TDO of a type
    OCI_ATTR_LTYPE =             128;                   // list type
    OCI_ATTR_PARSE_ERROR_OFFSET =129;                   // Parse Error offset
    OCI_ATTR_IS_TEMPORARY =      130;                   // whether table is temporary
    OCI_ATTR_IS_TYPED =          131;                   // whether table is typed
    OCI_ATTR_DURATION =          132;                   // duration of temporary table
    OCI_ATTR_IS_INVOKER_RIGHTS = 133;                   // is invoker rights
    OCI_ATTR_OBJ_NAME =          134;                   // top level schema obj name
    OCI_ATTR_OBJ_SCHEMA =        135;                   // schema name
    OCI_ATTR_OBJ_ID =            136;                   // top level schema object id

    // OCI Error Return Values
    OCI_SUCCESS             = 0;
    OCI_SUCCESS_WITH_INFO   = 1;
    OCI_NO_DATA             = 100;
    OCI_ERROR               = -1;
    OCI_INVALID_HANDLE      = -2;
    OCI_NEED_DATA           = 99;
    OCI_STILL_EXECUTING     = -3123;
    OCI_CONTINUE            = -24200;

    // Generic Default Value for Modes, ....
    OCI_DEFAULT     = $0;

    // OCI Init Mode
    OCI_THREADED    = $1;
    OCI_OBJECT      = $2;
    OCI_EVENTS      = $4;
    OCI_SHARED      = $10;
    OCI_NO_UCB      = $40;
    OCI_NO_MUTEX    = $80;

    // OCI Credentials
    OCI_CRED_RDBMS  = 1;
    OCI_CRED_EXT    = 2;
    OCI_CRED_PROXY  = 3;

    // OCI Authentication Mode
    OCI_MIGRATE     = $0001;                        // migratable auth context
    OCI_SYSDBA      = $0002;                        // for SYSDBA authorization
    OCI_SYSOPER     = $0004;                        // for SYSOPER authorization
    OCI_PRELIM_AUTH = $0008;                        // for preliminary authorization

    // OCIPasswordChange
    OCI_AUTH        = $08;                          // Change the password but do not login

    // OCI Data Types
    SQLT_CHR = 1  ; 
    SQLT_NUM = 2  ; 
    SQLT_INT = 3  ; 
    SQLT_FLT = 4  ; 
    SQLT_STR = 5  ; 
    SQLT_VNU = 6  ; 
    SQLT_PDN = 7  ; 
    SQLT_LNG = 8  ; 
    SQLT_VCS = 9  ; 
    SQLT_NON = 10 ; 
    SQLT_RID = 11 ; 
    SQLT_DAT = 12 ; 
    SQLT_VBI = 15 ; 
    SQLT_BIN = 23 ; 
    SQLT_LBI = 24 ; 
    _SQLT_PLI = 29;
    SQLT_UIN = 68 ;
    SQLT_SLS = 91 ;
    SQLT_LVC = 94 ;
    SQLT_LVB = 95 ;
    SQLT_AFC = 96 ;
    SQLT_AVC = 97 ;
    SQLT_CUR = 102;
    SQLT_RDD = 104;
    SQLT_LAB = 105;
    SQLT_OSL = 106;
    SQLT_NTY = 108;
    SQLT_REF = 110;
    SQLT_CLOB = 112;
    SQLT_BLOB = 113;
    SQLT_BFILEE = 114;
    SQLT_CFILEE = 115;
    SQLT_RSET = 116;
    SQLT_NCO = 122;
    SQLT_VST = 155;
    SQLT_ODT = 156;

    _SQLT_REC = 250;
    _SQLT_TAB = 251;
    _SQLT_BOL = 252;

    // OCI Statement Types
    OCI_STMT_SELECT  = 1;   // select statement
    OCI_STMT_UPDATE  = 2;   // update statement
    OCI_STMT_DELETE  = 3;   // delete statement
    OCI_STMT_INSERT  = 4;   // Insert Statement
    OCI_STMT_CREATE  = 5;   // create statement
    OCI_STMT_DROP    = 6;   // drop statement
    OCI_STMT_ALTER   = 7;   // alter statement
    OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
    OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

    // OCI Statement language
    OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
    OCI_V7_SYNTAX   = 2;    // V7 language
    OCI_V8_SYNTAX   = 3;    // V8 language

    // OCI Statement Execute mode
    OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
    OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
    OCI_SCROLLABLE_CURSOR = $08;    // cursor scrollable
    OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
    OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
    OCI_NON_BLOCKING      = $40;    // non-blocking
    OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
    OCI_PARSE_ONLY        = $100;   // only parse the statement

    OCI_DATA_AT_EXEC    = $02;      // data at execute time
    OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
    OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

    // OCI Transaction modes
    OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
    OCI_TRANS_JOIN         = $00000002; // join an existing transaction
    OCI_TRANS_RESUME       = $00000004; // resume this transaction
    OCI_TRANS_STARTMASK    = $000000ff;

    OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
    OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
    OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
    OCI_TRANS_ISOLMASK     = $0000ff00;

    OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
    OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
    OCI_TRANS_TYPEMASK     = $000f0000;

    OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
    OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

    // OCI pece wise fetch
    OCI_ONE_PIECE       = 0; // one piece
    OCI_FIRST_PIECE     = 1; // the first piece
    OCI_NEXT_PIECE      = 2; // the next of many pieces
    OCI_LAST_PIECE      = 3; // the last piece

    // OCI fetch modes
    OCI_FETCH_NEXT      = $02;  // next row
    OCI_FETCH_FIRST     = $04;  // first row of the result set
    OCI_FETCH_LAST      = $08;  // the last row of the result set
    OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
    OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
    OCI_FETCH_RELATIVE  = $40;  // offset relative to current

    //=======================Describe Handle Parameter Attributes ===============
    //===========================================================================

    // Attributes common to Columns and Stored Procs
    OCI_ATTR_DATA_SIZE      = 1;    // maximum size of the data 
    OCI_ATTR_DATA_TYPE      = 2;    // the SQL type of the column/argument 
    OCI_ATTR_DISP_SIZE      = 3;    // the display size 
    OCI_ATTR_NAME           = 4;    // the name of the column/argument 
    OCI_ATTR_PRECISION      = 5;    // precision if number type 
    OCI_ATTR_SCALE          = 6;    // scale if number type 
    OCI_ATTR_IS_NULL        = 7;    // is it null ? 
    OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types 
    OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name 
    OCI_ATTR_SUB_NAME       = 10;   // type name if package private type 
    OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

    // complex object retrieval parameter attributes 
    OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
    OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
    OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
    OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

    // Only Columns 
    OCI_ATTR_DISP_NAME      = 100;  // the display name 

    // Only Stored Procs
    OCI_ATTR_OVERLOAD       = 210;  // is this position overloaded 
    OCI_ATTR_LEVEL          = 211;  // level for structured types 
    OCI_ATTR_HAS_DEFAULT    = 212;  // has a default value 
    OCI_ATTR_IOMODE         = 213;  // in, out inout 
    OCI_ATTR_RADIX          = 214;  // returns a radix 
    OCI_ATTR_NUM_ARGS       = 215;  // total number of arguments 

    // only named type attributes
    OCI_ATTR_TYPECODE                  = 216;   // object or collection 
    OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table 
    OCI_ATTR_VERSION                   = 218;   // user assigned version 
    OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type 
    OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type 
    OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type 
    OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type 
    OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type 
    OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr 
    OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute 
    OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute 
    OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute 
    OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types 
    OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes 
    OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods 
    OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods 
    OCI_ATTR_MAP_METHOD                = 232;   // map method of type 
    OCI_ATTR_ORDER_METHOD              = 233;   // order method of type 

    // only collection element 
    OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements 

    // only type methods 
    OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level 
    OCI_ATTR_IS_SELFISH                = 236;   // method selfish 
    OCI_ATTR_IS_VIRTUAL                = 237;   // virtual 
    OCI_ATTR_IS_INLINE                 = 238;   // inline 
    OCI_ATTR_IS_CONSTANT               = 239;   // constant 
    OCI_ATTR_HAS_RESULT                = 240;   // has result 
    OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
    OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor 
    OCI_ATTR_IS_OPERATOR               = 243;   // operator 
    OCI_ATTR_IS_MAP                    = 244;   // a map method 
    OCI_ATTR_IS_ORDER                  = 245;   // order method
    OCI_ATTR_IS_RNDS                   = 246;   // read no data state method 
    OCI_ATTR_IS_RNPS                   = 247;   // read no process state
    OCI_ATTR_IS_WNDS                   = 248;   // write no data state method 
    OCI_ATTR_IS_WNPS                   = 249;   // write no process state 

    OCI_ATTR_DESC_PUBLIC               = 250;   // public object

    // Object Cache Enhancements : attributes for User Constructed Instances     
    OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
    OCI_ATTR_UCI_CONSTRUCT             = 252;
    OCI_ATTR_UCI_DESTRUCT              = 253;
    OCI_ATTR_UCI_COPY                  = 254;
    OCI_ATTR_UCI_PICKLE                = 255;
    OCI_ATTR_UCI_UNPICKLE              = 256;
    OCI_ATTR_UCI_REFRESH               = 257;

    // for type inheritance 
    OCI_ATTR_IS_SUBTYPE                = 258;
    OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
    OCI_ATTR_SUPERTYPE_NAME            = 260;

    // for schemas 
    OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

    // for database 
    OCI_ATTR_NCHARSET_ID               = 262;   // char set id 
    OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas 
    OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length 
    OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length 
    OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior 
    OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength 
    OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location 
    OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support 
    OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
    OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
    OCI_ATTR_LOCKING_MODE              = 272;   // locking mode 

    OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
    OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
    OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

    // Piece Information
    OCI_PARAM_IN    = $01;  // in parameter
    OCI_PARAM_OUT   = $02;  // out parameter

    // LOB Buffering Flush Flags
    OCI_LOB_BUFFER_FREE     = 1;
    OCI_LOB_BUFFER_NOFREE   = 2;

    // FILE open modes
    OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
    // LOB open modes
    OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
    OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs

    // CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information
    SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
    SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
    SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
    SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
    SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

    // ==================================================================
    // OCIDesribeAny

    // Describe mode
    OCI_OTYPE_NAME = 1;
    OCI_OTYPE_REF = 2;
    OCI_OTYPE_PTR = 3;

    // Object type
    OCI_PTYPE_UNK           = 0;    // unknown
    OCI_PTYPE_TABLE         = 1;    // table
    OCI_PTYPE_VIEW          = 2;    // view
    OCI_PTYPE_PROC          = 3;    // procedure
    OCI_PTYPE_FUNC          = 4;    // function
    OCI_PTYPE_PKG           = 5;    // package
    OCI_PTYPE_TYPE          = 6;    // user-defined type
    OCI_PTYPE_SYN           = 7;    // synonym
    OCI_PTYPE_SEQ           = 8;    // sequence
    OCI_PTYPE_COL           = 9;    // column
    OCI_PTYPE_ARG           = 10;   // argument
    OCI_PTYPE_LIST          = 11;   // list
    OCI_PTYPE_TYPE_ATTR     = 12;   // user-defined type's attribute
    OCI_PTYPE_TYPE_COLL     = 13;   // collection type's element
    OCI_PTYPE_TYPE_METHOD   = 14;   // user-defined type's method
    OCI_PTYPE_TYPE_ARG      = 15;   // user-defined type method's argument
    OCI_PTYPE_TYPE_RESULT   = 16;   // user-defined type method's result

    // Proc/Func param type
    OCI_TYPEPARAM_IN    = 0;
    OCI_TYPEPARAM_OUT   = 1;
    OCI_TYPEPARAM_INOUT = 2;

    // Number formats
    OCI_NUMBER_UNSIGNED = 0;
    OCI_NUMBER_SIGNED   = 2;

    //======================= Direct Path Loading ===============================
    //===========================================================================
    // values for OCI_ATTR_DIRPATH_MODE attribute
    OCI_DIRPATH_LOAD       = 1;    // direct path load operation
    OCI_DIRPATH_UNLOAD     = 2;    // direct path unload operation
    OCI_DIRPATH_CONVERT    = 3;    // direct path convert only operation

    // values for OCI_ATTR_DIRPATH_INDEX_MAINT_METHOD attribute
    OCI_DIRPATH_INDEX_MAINT_SINGLE_ROW    = 1;
    OCI_DIRPATH_INDEX_MAINT_SKIP_UNUSABLE = 2;
    OCI_DIRPATH_INDEX_MAINT_SKIP_ALL      = 3;

    // values for OCI_ATTR_STATE attribute of OCIDirPathCtx
    OCI_DIRPATH_NORMAL       = 1;    // can accept rows, last row complete
    OCI_DIRPATH_PARTIAL      = 2;    // last row was partial
    OCI_DIRPATH_NOT_PREPARED = 3;    // direct path context is not prepared

    // values for cflg argument to OCIDirpathColArrayEntrySet
    OCI_DIRPATH_COL_COMPLETE = 0;    // column data is complete
    OCI_DIRPATH_COL_NULL     = 1;    // column is null
    OCI_DIRPATH_COL_PARTIAL  = 2;    // column data is partial

    // values for action parameter to OCIDirPathDataSave
    OCI_DIRPATH_DATASAVE_SAVEONLY = 0;  // data save point only
    OCI_DIRPATH_DATASAVE_FINISH   = 1;  // execute finishing logic

    // OCIDuration values
    OCI_DURATION_INVALID    = -1;                      // Invalid duration
    OCI_DURATION_BEGIN      = 10;                      // beginning sequence of duration
    OCI_DURATION_NULL       = OCI_DURATION_BEGIN - 1;  // null duration
    OCI_DURATION_DEFAULT    = OCI_DURATION_BEGIN - 2;  // default
    OCI_DURATION_USER_CALLBACK = OCI_DURATION_BEGIN - 3; 
    OCI_DURATION_NEXT       = OCI_DURATION_BEGIN - 4;  // next special duration
    OCI_DURATION_SESSION    = OCI_DURATION_BEGIN;      // the end of user session
    OCI_DURATION_TRANS      = OCI_DURATION_BEGIN + 1;  // the end of user transaction

    // values for lobtype parameter to OCILOBCreateTemporary  
    OCI_TEMP_BLOB = 1;  // LOB type - BLOB
    OCI_TEMP_CLOB = 2;  // LOB type - CLOB 

    // Some Oracle client versions
    cvOracle80000 = 800000000;
    cvOracle80400 = 800040000;
    cvOracle80500 = 800050000;
    cvOracle80501 = 800050001;
    cvOracle81000 = 801000000;
    cvOracle81500 = 801050000;
    cvOracle81600 = 801060000;
    cvOracle90000 = 900010101;

type
    PPointer = ^Pointer;
    TOCIInitialize = function(mode: ub4; ctxp: pointer; malocfp: pointer;
                              ralocfp: pointer; mfreefp: pointer): sword; cdecl;
    TOCIEnvInit = function(var envhpp: pOCIEnv; mode: ub4; xtramemsz: size_T;
                           usrmempp: PPointer): sword; cdecl;
    TOCIEnvCreate = function(var envhpp: pOCIEnv; mode: ub4; ctxp: pointer; malocfp: pointer;
                             ralocfp: pointer; mfreefp: pointer; xtramemsz: size_T;
                             usrmempp: PPointer): sword; cdecl;
    TOCIHandleAlloc = function(parenth: pOCIHandle; var hndlpp: pOCIHandle; atype: ub4;
                               xtramem_sz: size_T; usrmempp: PPointer): sword; cdecl;
    TOCIServerAttach = function(srvhp: pOCIServer; errhp: pOCIError; dblink: text;
                                dblink_len: sb4; mode: ub4): sword; cdecl;
    TOCIAttrSet = function(trgthndlp: pOCIHandle; trghndltyp: ub4; attributep: pointer;
                           size: ub4; attrtype: ub4; errhp: pOCIError):sword; cdecl;
    TOCISessionBegin = function(svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
                                credt: ub4; mode: ub4):sword; cdecl;
    TOCISessionEnd = function(svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
                              mode: ub4): sword; cdecl;
    TOCIServerDetach = function(srvhp: pOCIServer; errhp: pOCIError;
                                mode: ub4): sword; cdecl;
    TOCIHandleFree = function(hndlp: pointer; atype: ub4): sword; cdecl;
    TOCIErrorGet = function(hndlp: pointer; recordno: ub4; sqlstate: text;
                            var errcodep: sb4; bufp: text; bufsiz: ub4;
                            atype: ub4): sword; cdecl;
    TOCIStmtPrepare = function(stmtp: pOCIStmt; errhp: pOCIError; stmt: text;
                               stmt_len: ub4; language:ub4; mode: ub4):sword; cdecl;
    TOCIStmtExecute = function(svchp: pOCISvcCtx; stmtp: pOCIStmt; errhp: pOCIError;
                               iters: ub4; rowoff: ub4; snap_in: pOCISnapshot;
                               snap_out: pOCISnapshot; mode: ub4): sword; cdecl;
    TOCIParamGet = function(hndlp: pointer; htype: ub4; errhp: pOCIError;
                            var parmdpp: pointer; pos: ub4): sword; cdecl;
    TOCIAttrGet = function(trgthndlp: pOCIHandle; trghndltyp: ub4; attributep: pointer;
                           sizep: pointer; attrtype: ub4; errhp: pOCIError):sword; cdecl;
    TOCIStmtFetch = function(stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4;
                             orientation: ub2; mode: ub4): sword; cdecl;
    TOCIDefineByPos = function(stmtp: pOCIStmt; var defnpp: pOCIDefine;
                               errhp: pOCIError; position: ub4; valuep: pointer;
                               value_sz: sb4; dty: ub2; indp: pointer; rlenp: pointer;
                               rcodep: pointer; mode: ub4): sword; cdecl;
    TOCIDefineArrayOfStruct = function(defnpp: pOCIDefine; errhp: pOCIError;
                                       pvskip: ub4; indskip: ub4; rlskip: ub4;
                                       rcskip: ub4): sword; cdecl;
    TOCIBindByPos = function(stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
                             position: ub4; valuep: pointer; value_sz: sb4; dty: ub2;
                             indp: pointer; alenp: pointer; rcodep: pointer;
                             maxarr_len: ub4; curelep: pointer; mode: ub4): sword; cdecl;
    TOCIBindByName = function(stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
                              placeholder: text; placeh_len: sb4; valuep: pointer;
                              value_sz: sb4; dty: ub2; indp: pointer; alenp: pointer;
                              rcodep: pointer; maxarr_len: ub4; curelep: pointer;
                              mode: ub4): sword; cdecl;
    TOCIBindDynamic = function(bindp: pOCIBind; errhp: pOCIError; ictxp: Pointer;
                               icbfp: Pointer; octxp: Pointer; ocbfp: Pointer): sword; cdecl;
    TOCITransStart = function(svchp: pOCISvcCtx; errhp: pOCIError; timeout: word;
                              flags: ub4): sword; cdecl;
    TOCITransRollback = function(svchp:pOCISvcCtx; errhp:pOCIError; flags: ub4): sword; cdecl;
    TOCITransCommit = function(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; cdecl;
    TOCITransDetach = function(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; cdecl;
    TOCITransPrepare = function(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; cdecl;
    TOCITransForget = function(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; cdecl;
    TOCIDescribeAny = function(svchp: pOCISvcCtx; errhp: pOCIError; objptr: pointer;
                               objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
                               objtyp: ub1; dschp: pOCIDescribe): sword; cdecl;
    TOCIBreak = function(svchp: pOCISvcCtx; errhp:pOCIError): sword; cdecl;
    TOCIReset = function(svchp: pOCISvcCtx; errhp:pOCIError): sword; cdecl;
    TOCIDescriptorAlloc = function(parenth: pOCIEnv; var descpp: pOCIDescriptor; htype: ub4;
                                   xtramem_sz: integer; usrmempp: pointer): sword; cdecl;
    TOCIDescriptorFree = function(descp: pointer; htype: ub4): sword; cdecl;
    TOCILobAppend = function(svchp: pOCISvcCtx; errhp: pOCIError; dst_locp,
                             src_locp: pOCILobLocator): sword; cdecl;
    TOCILobAssign = function(svchp: pOCISvcCtx; errhp: pOCIError;
                             src_locp: pOCILobLocator; var dst_locpp: pOCILobLocator): sword; cdecl;
    TOCILobClose = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword; cdecl;
    TOCILobCopy = function(svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
                           src_locp: pOCILobLocator; amount: ub4; dst_offset: ub4;
                           src_offset: ub4): sword; cdecl;
    TOCILobEnableBuffering = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword; cdecl;
    TOCILobDisableBuffering = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword; cdecl;
    TOCILobErase = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                            var amount: ub4; offset: ub4): sword; cdecl;
    TOCILobFileExists = function(svchp: pOCISvcCtx; errhp: pOCIError;
                                 filep: pOCILobLocator; var flag: Boolean): sword; cdecl;
    TOCILobFileGetName = function(envhp: pOCIEnv; errhp: pOCIError; filep: pOCILobLocator;
                                  dir_alias: text; var d_length: ub2; filename: text;
                                  var f_length: ub2): sword; cdecl;
    TOCILobFileSetName = function(envhp: pOCIEnv; errhp: pOCIError; var filep: pOCILobLocator;
                                  dir_alias: text; d_length: ub2; filename: text;
                                  f_length: ub2): sword; cdecl;
    TOCILobFlushBuffer = function(svchp: pOCISvcCtx; errhp: pOCIError;
                                  locp: pOCILobLocator; flag: ub4): sword; cdecl;
    TOCILobGetLength = function (svchp: pOCISvcCtx; errhp: pOCIError;
                                 locp: pOCILobLocator; var lenp: ub4): sword; cdecl;
    TOCILobIsOpen = function(svchp: pOCISvcCtx; errhp: pOCIError;
                             locp: pOCILobLocator; var flag: LongBool): sword; cdecl;
    TOCILobLoadFromFile = function (svchp: pOCISvcCtx; errhp: pOCIError;
                                    dst_locp: pOCILobLocator; src_locp: pOCILobLocator;
                                    amount: ub4; dst_offset: ub4; src_offset: ub4): sword; cdecl;
    TOCILobLocatorIsInit = function (envhp: pOCIEnv; errhp: pOCIError;
                                     locp: pOCILobLocator; var is_initialized: LongBool): sword; cdecl;
    TOCILobOpen = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                            mode: ub1): sword; cdecl;
    TOCILobRead = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                            var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
                            ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    TOCILobTrim = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                            newlen: ub4): sword; cdecl;
    TOCILobWrite = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                            var amtp: ub4; offset: ub4; bufp: pointer; bufl: ub4;
                            piece: ub1; ctxp: pointer; cbfp: pointer;
                            csid: ub2; csfrm: ub1): sword; cdecl;
    TOCIStmtGetPieceInfo = function(stmtp: pOCIStmt; errhp: pOCIError; var hndlpp: pointer;
                                    var typep: ub4; var in_outp: ub1; var iterp: ub4;
                                    var idxp: ub4; var piecep: ub1): sword; cdecl;
    TOCIStmtSetPieceInfo = function(handle: pointer; typep: ub4; errhp: pOCIError;
                                    buf: pointer; var alenp: ub4; piece: ub1;
                                    indp: pointer; var rcodep: ub2): sword; cdecl;
    TOCIPasswordChange = function(svchp: pOCISvcCtx; errhp: pOCIError; user_name: text;
                                  usernm_len: ub4; opasswd: text; opasswd_len: ub4;
                                  npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;
    TOCIServerVersion = function(hndlp: pOCIHandle; errhp: pOCIError; bufp: text;
                                 bufsz: ub4; hndltype: ub1): sword; cdecl;
    TOCIResultSetToStmt = function(rsetdp: pOCIHandle; errhp: pOCIError): sword; cdecl;
    TOCIDirPathAbort = function(dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; cdecl;
    TOCIDirPathDataSave = function(dpctx: pOCIDirPathCtx; errhp: pOCIError; action: ub4): sword; cdecl;
    TOCIDirPathFinish = function(dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; cdecl;
    TOCIDirPathFlushRow = function(dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; cdecl;
    TOCIDirPathPrepare = function(dpctx: pOCIDirPathCtx; svchp: pOCISvcCtx; errhp: pOCIError): sword; cdecl;
    TOCIDirPathLoadStream = function(dpctx: pOCIDirPathCtx; dpstr: pOCIDirPathStream;
                         errhp: pOCIError): sword; cdecl;
    TOCIDirPathColArrayEntryGet = function(dpca: pOCIDirPathColArray; errhp: pOCIError;
                               rownum: ub4; colIdx: ub2; var cvalpp: pUb1; var clenp: ub4;
                               var cflgp: ub1): sword; cdecl;
    TOCIDirPathColArrayEntrySet = function(dpca: pOCIDirPathColArray; errhp: pOCIError;
                               rownum: ub4; colIdx: ub2; cvalp: pUb1; clen: ub4;
                               cflg: ub1): sword; cdecl;
    TOCIDirPathColArrayRowGet = function(dpca: pOCIDirPathColArray; errhp: pOCIError;
                             rownum: ub4; var cvalppp: ppUb1; var clenpp: pUb4;
                             var cflgpp: pUb1): sword; cdecl;
    TOCIDirPathColArrayReset = function(dpca: pOCIDirPathColArray; errhp: pOCIError): sword; cdecl;
    TOCIDirPathColArrayToStream = function(dpca: pOCIDirPathColArray; dpctx: pOCIDirPathCtx;
                               dpstr: pOCIDirPathStream; errhp: pOCIError;
                               rowcnt: ub4; rowoff: ub4): sword; cdecl;
    TOCIDirPathStreamReset = function(dpstr: pOCIDirPathStream; errhp: pOCIError): sword; cdecl;
    TOCIDirPathStreamToStream = function(istr: pOCIDirPathStream; ostr: pOCIDirPathStream;
                             dpctx: pOCIDirPathCtx; errhp: pOCIError;
                             isoff: ub4; osoff: ub4): sword; cdecl;

    // Temporary Lob works (8.1 and above)
    TOCILobCreateTemporary = function(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                          csid: ub2; csfrm: ub1; lobtype: ub1; cache: LongBool;
                          duration: OCIDuration): sword; cdecl;
    TOCILobFreeTemporary = function(svchp: pOCISvcCtx; errhp: pOCIError;
                        locp: pOCILobLocator): sword; cdecl;
    TOCILobIsTemporary = function(envhp: pOCIEnv; errhp: pOCIError; locp: pOCILobLocator;
                      var is_temporary: LongBool): sword; cdecl;

    TOCICustomComponent = class(TComponent);
    TOCICustomDataSet = class(TDataSet);

var
    FOCIKey: String;
    FOCIOracleHome: String;
    FOCIDllName: String;
    FOCIPO8: Boolean;
    FOCITnsNames: String;
    FOCIhDll: Integer;
    FOCIVersion: Integer;
    FOCIInitialized: Boolean;
    FOCIBDECompatibility: Boolean;

    OCIPasswordChange: TOCIPasswordChange;
    OCIInitialize: TOCIInitialize;
    OCIEnvInit: TOCIEnvInit;
    OCIEnvCreate: TOCIEnvCreate;
    OCIHandleAlloc: TOCIHandleAlloc;
    OCIServerAttach: TOCIServerAttach;
    OCIAttrSet: TOCIAttrSet;
    OCISessionBegin: TOCISessionBegin;
    OCISessionEnd: TOCISessionEnd;
    OCIServerDetach: TOCIServerDetach;
    OCIHandleFree: TOCIHandleFree;
    OCIErrorGet: TOCIErrorGet;
    OCIStmtPrepare: TOCIStmtPrepare;
    OCIStmtExecute: TOCIStmtExecute;
    OCIParamGet: TOCIParamGet;
    OCIAttrGet: TOCIAttrGet;
    OCIStmtFetch: TOCIStmtFetch;
    OCIDefineByPos: TOCIDefineByPos;
    OCIDefineArrayOfStruct: TOCIDefineArrayOfStruct;
    OCIBindByPos: TOCIBindByPos;
    OCIBindByName: TOCIBindByName;
    OCITransStart: TOCITransStart;
    OCITransCommit: TOCITransCommit;
    OCITransRollback: TOCITransRollback;
    OCITransDetach: TOCITransDetach;
    OCITransPrepare: TOCITransPrepare;
    OCITransForget: TOCITransForget;
    OCIDescribeAny: TOCIDescribeAny;
    OCIBreak: TOCIBreak;
    OCIReset: TOCIReset;
    OCIDescriptorAlloc: TOCIDescriptorAlloc;
    OCIDescriptorFree: TOCIDescriptorFree;
    OCIStmtGetPieceInfo: TOCIStmtGetPieceInfo;
    OCIStmtSetPieceInfo: TOCIStmtSetPieceInfo;
    OCIServerVersion: TOCIServerVersion;
    OCIBindDynamic: TOCIBindDynamic;
    OCILobAppend: TOCILobAppend;
    OCILobAssign: TOCILobAssign;
    OCILobClose: TOCILobClose;
    OCILobCopy: TOCILobCopy;
    OCILobEnableBuffering: TOCILobEnableBuffering;
    OCILobDisableBuffering: TOCILobDisableBuffering;
    OCILobErase: TOCILobErase;
    OCILobFileExists: TOCILobFileExists;
    OCILobFileGetName: TOCILobFileGetName;
    OCILobFileSetName: TOCILobFileSetName;
    OCILobFlushBuffer: TOCILobFlushBuffer;
    OCILobGetLength: TOCILobGetLength;
    OCILobIsOpen: TOCILobIsOpen;
    OCILobLoadFromFile: TOCILobLoadFromFile;
    OCILobLocatorIsInit: TOCILobLocatorIsInit;
    OCILobOpen: TOCILobOpen;
    OCILobRead: TOCILobRead;
    OCILobTrim: TOCILobTrim;
    OCILobWrite: TOCILobWrite;
    OCIResultSetToStmt: TOCIResultSetToStmt;
    OCIDirPathAbort: TOCIDirPathAbort;
    OCIDirPathDataSave: TOCIDirPathDataSave;
    OCIDirPathFinish: TOCIDirPathFinish;
    OCIDirPathFlushRow: TOCIDirPathFlushRow;
    OCIDirPathPrepare: TOCIDirPathPrepare;
    OCIDirPathLoadStream: TOCIDirPathLoadStream;
    OCIDirPathColArrayEntryGet: TOCIDirPathColArrayEntryGet;
    OCIDirPathColArrayEntrySet: TOCIDirPathColArrayEntrySet;
    OCIDirPathColArrayRowGet: TOCIDirPathColArrayRowGet;
    OCIDirPathColArrayReset: TOCIDirPathColArrayReset;
    OCIDirPathColArrayToStream: TOCIDirPathColArrayToStream;
    OCIDirPathStreamReset: TOCIDirPathStreamReset;
    OCIDirPathStreamToStream: TOCIDirPathStreamToStream;
    OCILobCreateTemporary: TOCILobCreateTemporary;
    OCILobFreeTemporary: TOCILobFreeTemporary;
    OCILobIsTemporary: TOCILobIsTemporary;

    procedure InitOCI;
    procedure TerminateOCI;
    procedure OCIDBError(const AMessage: String;
        AComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF});
    procedure OCIDBErrorFmt(const AMessage: String; AArgs: array of const;
        AComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF});
    procedure OCIDBHandleException(Sender: TObject);

// this are fixed values, you should not change it !
const
    IMaxNumPrecision = 38;
    IRowIdLen = 18;
    IMaxDlp2StrSize = 255;
    IMaxDlp4StrSize = 8192;

// this consts you can change freely, they are only
// default values for some properties or default
// values for some internals
const
    // key there NCOCI8 put required registry values
    SNCVCLRoot: String = '\Software\Nica-Com\NCVCL\';
    // key there NCOCI8 put login profiles
    SOCIProfiles: String = 'OCIProfiles';
    // default DatabaseName
    SDefaultDBName: String = 'Default';
    // default prefix, that NCOCI8 uses for forming names of SAVEPOINT, etc
    SDefPrefix: String = 'NCSYS_';
    // default name for Result parameter of stored funcs
    SDefParRESULT: String = 'RESULT';
    // Must be in sync with SDefParRESULT
    SDefParRESULTLen: Integer = 6;
    // delimiter of name parts in parameters, that form PL/SQL record
    SDefParMembDelim: String = '$';
    // default filter name
    SDefaultFltName: String = 'NCSysDefault';
    // Lookup filter name
    SLookupFltName: String = 'NCSysLookup';
    // Symbol, that is used as start of macro
    CMacroChar = '&';
    // Delay, before break form
    WDelayBeforeSQLCrs: ub4 = 200;
    // 100 - IDSPctUsable = percents of queries, that are closed
    // after MaxCursors reaches
    IDSPctUsable = 80;
    // default XID format
    IXIDFormatID = 123456;
    // default number of login retries
    IDefLoginRetries = 3;
    // default length of Long parameters
    IDefLongSize = 0;
    // default length of String parameters
    IDefStrSize = 255;
    IDefLStrSize = 4000;
    // default values for properties of TOCIFetchParams
    IDefInrecDataSize = 4000;
    IDefPieceBuffLen = 4000;
    IDefRowSetSize = 10;

implementation

Uses Windows, Registry, SysUtils, NCOciMsg, NCOciUtil
{$IFNDEF OCI_D6}
     , Forms, FileCtrl
{$ENDIF}
     ;

procedure OCIDBError(const AMessage: String;
    AComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF});
begin
    if Assigned(AComponent) and (AComponent.Name <> '') then
        raise EOCIDBError.Create(Format('%s: %s', [AComponent.Name, AMessage]))
    else
        raise EOCIDBError.Create(AMessage);
end;

procedure OCIDBErrorFmt(const AMessage: String; AArgs: array of const;
    AComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF});
begin
    OCIDBError(Format(AMessage, AArgs), AComponent);
end;

function NormalizePath(const APath: String): String;
begin
    if (APath <> '') and (APath[Length(APath)] = '\') then
        Result := Copy(APath, 1, Length(APath) - 1)
    else
        Result := APath;
end;

function GetRegPathVar(AReg: TRegistry; const AName: String): String;
begin
    Result := NormalizePath(AReg.ReadString(AName));
end;

procedure GetOCIPaths;
var
    s: String;
    bestInd, i, n: Integer;
    path: String;
    reg: TRegistry;

    procedure InitBest;
    var
        sz: DWORD;
    begin
        bestInd := MAXINT;
        FOCIOracleHome := '';
        sz := GetEnvironmentVariable(PChar('PATH'), nil, 0);
        SetString(path, nil, sz);
        GetEnvironmentVariable(PChar('PATH'), PChar(path), sz);
        path := AnsiUpperCase(Copy(path, 1, Length(path) - 1));
    end;

    function TestBestHome(const AKey: String): Boolean;
    var
        i: Integer;
        bin, home: String;
    begin
{$IFDEF OCI_D4}
        Result := reg.OpenKeyReadOnly(AKey);
{$ELSE}
        Result := reg.OpenKey(AKey, False);
{$ENDIF}        
        if Result then begin
            home := GetRegPathVar(reg, 'ORACLE_HOME');
            bin := home + '\bin';
            i := 1;
            while i <= Length(path) do begin
                if AnsiCompareText(bin, NormalizePath(ExtractFieldName(path, i))) = 0 then begin
                    if DirectoryExists(bin) then
                        if bestInd > i then begin
                            bestInd := i;
                            FOCIKey := AKey;
                            FOCIOracleHome := home;
                            FOCIDllName := reg.ReadString('ORAOCI');
                            FOCIPO8 := (CompareText(reg.ReadString('PO8'), 'YES') = 0);
                        end;
                    Break;
                end;
            end;
        end;
    end;

var
  xSubKeys: TStrings;

begin
    reg := TRegistry.Create;
    with reg do
    try
        RootKey := HKEY_LOCAL_MACHINE;
        InitBest;
        if TestBestHome('\Software\Oracle') then
          begin
{$IFDEF OCI_D4}
            if OpenKeyReadOnly('\Software\Oracle\All_Homes') then
{$ELSE}
            if OpenKey('\Software\Oracle\All_Homes', False) then
{$ENDIF}
              begin
                s := ReadString('HOME_COUNTER');
                if s = '' then
                    n := 1
                else
                    n := StrToInt(s);
                for i := 0 to n - 1 do
                    TestBestHome('\Software\Oracle\Home' + IntToStr(i));
              end;

            // Locate Oracle 10g Homes
{$IFDEF OCI_D4}
            if OpenKeyReadOnly('\Software\Oracle') then
{$ELSE}
            if OpenKey('\Software\Oracle', False) then 
{$ENDIF}
              begin
                xSubKeys := TStringList.Create;
                try
                  GetKeyNames(xSubKeys);
                  for i := 0 to xSubKeys.Count - 1 do
                    if AnsiStrLIComp(PChar(xSubKeys[i]), 'KEY_', 4) = 0 then
                      TestBestHome(xSubKeys[i]);
                finally
                  xSubKeys.Free;
                end;
              end;
          end;

        if FOCIOracleHome = '' then
            OCIDBError(msgOCINotInstalled, nil);
        if FOCIDllName = '' then begin
            FOCIDllName := FOCIOracleHome + '\Bin\OCI.DLL';
            // single case, than things differs - 8.0.3
            if not FileExists(FOCIDllName) then
                FOCIDllName := FOCIOracleHome + '\Bin\ORA803.DLL';
        end;
    finally
        Free;
    end;
end;

procedure GetTNSPaths;
var
    reg: TRegistry;
begin
    reg := TRegistry.Create;
    with reg do
    try
        RootKey := HKEY_LOCAL_MACHINE;
{$IFDEF OCI_D4}
        OpenKeyReadOnly(FOCIKey);
{$ELSE}        
        OpenKey(FOCIKey, False);
{$ENDIF}
        FOCITnsNames := ReadString('TNS_NAMES');
        if FOCITnsNames = '' then begin
            FOCITnsNames := GetRegPathVar(reg, 'TNS_ADMIN');
            if FOCITnsNames = '' then begin
                if FOCIVersion >= cvOracle81000 then
                    FOCITnsNames := GetRegPathVar(reg, 'NETWORK')
                else
                    FOCITnsNames := GetRegPathVar(reg, 'NET80');
                if FOCITnsNames = '' then begin
                    if FOCIVersion >= cvOracle81000 then
                        FOCITnsNames := FOCIOracleHome + '\Network'
                    else
                        FOCITnsNames := FOCIOracleHome + '\Net80';
                end;
                FOCITnsNames := FOCITnsNames + '\Admin';
            end;
            FOCITnsNames := FOCITnsNames + '\TNSNames.ora';
        end;
    finally
        Free;
    end;
end;

procedure RaiseOCINotLoaded;
var
    s: String;
    buf: PChar;
begin
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
        nil, GetLastError(), (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL,
        PChar(@buf), 0, nil);
    s := buf;
    LocalFree(HLOCAL(buf));
    OCIDBErrorFmt(msgOCINotLoaded, [s], nil);
end;

procedure GetOCIVersion;
const
    s: String = '\StringFileInfo\040904B0\FileVersion';
    undef: String = '<UNDEFINED>';
var
    hndl, sz: DWORD;
    buf: Pointer;
    len: UINT;
    pStrVer: PChar;
begin
    FOCIVersion := 0;
    pStrVer := nil;
    sz := GetFileVersionInfoSize(PChar(FOCIDllName), hndl);
    if sz > 0 then begin
        GetMem(buf, sz);
        try
            if GetFileVersionInfo(PChar(FOCIDllName), hndl, sz, buf) then
                if VerQueryValue(buf, PChar(s), Pointer(pStrVer), Len) then
                    FOCIVersion := VerStr2Int(pStrVer);
        finally
            FreeMem(buf, sz);
        end;
    end
    else
        RaiseOCINotLoaded;
    if FOCIVersion < cvOracle80000 then begin
        if pStrVer = nil then
            pStrVer := PChar(undef);
        OCIDBErrorFmt(msgOCIBadVersion, [StrPas(pStrVer)], nil);
    end;
end;

procedure LoadOCILibrary;
begin
    FOCIhDll := LoadLibrary(PChar(FOCIDllName));
    if FOCIhDll = 0 then
        RaiseOCINotLoaded;
end;

procedure OCIDBHandleException(Sender: TObject);
begin
{$IFDEF OCI_D6}
    if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
    else
        ShowException(ExceptObject, ExceptAddr)
{$ELSE}
    Application.HandleException(Sender);
{$ENDIF}
end;

procedure OCIDBProcessMessages;
begin
{$IFDEF OCI_D6}
    DBApplication.ProcessMessages;
{$ELSE}
    Application.ProcessMessages;
{$ENDIF}
end;

procedure LoadOCIEntrys;

  function GetOCIProcAddress(const AProcName: String): Pointer;
  begin
      Result := nil;
      try
          Result := GetProcAddress(FOCIhDll, PChar(AProcName));
          if Result = nil then
              OCIDBErrorFmt(msgOCIFuncNotFound, [AProcName], nil);
      except
          OCIDBHandleException(nil);
      end;
  end;

begin
    if (FOCIVersion >= cvOracle81000) and not FOCIBDECompatibility then
        @OCIEnvCreate := GetOCIProcAddress('OCIEnvCreate')
    else begin
        @OCIInitialize := GetOCIProcAddress('OCIInitialize');
        @OCIEnvInit := GetOCIProcAddress('OCIEnvInit');
    end;

    @OCIHandleAlloc := GetOCIProcAddress('OCIHandleAlloc');
    @OCIHandleFree := GetOCIProcAddress('OCIHandleFree');
    @OCIAttrSet := GetOCIProcAddress('OCIAttrSet');
    @OCIAttrGet := GetOCIProcAddress('OCIAttrGet');
    @OCIDescriptorAlloc := GetOCIProcAddress('OCIDescriptorAlloc');
    @OCIDescriptorFree := GetOCIProcAddress('OCIDescriptorFree');
    @OCIErrorGet := GetOCIProcAddress('OCIErrorGet');

    @OCIServerAttach := GetOCIProcAddress('OCIServerAttach');
    @OCIServerDetach := GetOCIProcAddress('OCIServerDetach');
    @OCIServerVersion := GetOCIProcAddress('OCIServerVersion');
    @OCIBreak := GetOCIProcAddress('OCIBreak');
    if FOCIVersion >= cvOracle81000 then
        @OCIReset := GetOCIProcAddress('OCIReset');

    @OCISessionBegin := GetOCIProcAddress('OCISessionBegin');
    @OCISessionEnd := GetOCIProcAddress('OCISessionEnd');
    @OCIPasswordChange := GetOCIProcAddress('OCIPasswordChange');

    @OCITransStart := GetOCIProcAddress('OCITransStart');
    @OCITransCommit := GetOCIProcAddress('OCITransCommit');
    @OCITransRollback := GetOCIProcAddress('OCITransRollback');
    @OCITransDetach := GetOCIProcAddress('OCITransDetach');
    @OCITransPrepare := GetOCIProcAddress('OCITransPrepare');
    @OCITransForget := GetOCIProcAddress('OCITransForget');

    @OCIStmtPrepare := GetOCIProcAddress('OCIStmtPrepare');
    @OCIStmtExecute := GetOCIProcAddress('OCIStmtExecute');
    @OCIStmtFetch := GetOCIProcAddress('OCIStmtFetch');
    @OCIStmtGetPieceInfo := GetOCIProcAddress('OCIStmtGetPieceInfo');
    @OCIStmtSetPieceInfo := GetOCIProcAddress('OCIStmtSetPieceInfo');
    @OCIParamGet := GetOCIProcAddress('OCIParamGet');
    @OCIResultSetToStmt := GetOCIProcAddress('OCIResultSetToStmt');

    @OCIDefineByPos := GetOCIProcAddress('OCIDefineByPos');
    @OCIDefineArrayOfStruct := GetOCIProcAddress('OCIDefineArrayOfStruct');

    @OCIBindByPos := GetOCIProcAddress('OCIBindByPos');
    @OCIBindByName := GetOCIProcAddress('OCIBindByName');
    @OCIBindDynamic := GetOCIProcAddress('OCIBindDynamic');

    @OCILobAppend := GetOCIProcAddress('OCILobAppend');
    @OCILobAssign := GetOCIProcAddress('OCILobAssign');
    @OCILobCopy := GetOCIProcAddress('OCILobCopy');
    @OCILobEnableBuffering := GetOCIProcAddress('OCILobEnableBuffering');
    @OCILobDisableBuffering := GetOCIProcAddress('OCILobDisableBuffering');
    @OCILobErase := GetOCIProcAddress('OCILobErase');
    @OCILobFileExists := GetOCIProcAddress('OCILobFileExists');
    @OCILobFileGetName := GetOCIProcAddress('OCILobFileGetName');
    @OCILobFileSetName := GetOCIProcAddress('OCILobFileSetName');
    @OCILobFlushBuffer := GetOCIProcAddress('OCILobFlushBuffer');
    @OCILobGetLength := GetOCIProcAddress('OCILobGetLength');
    @OCILobLoadFromFile := GetOCIProcAddress('OCILobLoadFromFile');
    @OCILobLocatorIsInit := GetOCIProcAddress('OCILobLocatorIsInit');
    @OCILobRead := GetOCIProcAddress('OCILobRead');
    @OCILobTrim := GetOCIProcAddress('OCILobTrim');
    @OCILobWrite := GetOCIProcAddress('OCILobWrite');
    if FOCIVersion >= cvOracle81000 then begin
        @OCILobClose := GetOCIProcAddress('OCILobClose');
        @OCILobIsOpen := GetOCIProcAddress('OCILobIsOpen');
        @OCILobOpen := GetOCIProcAddress('OCILobOpen');
    end      
    else begin
        @OCILobClose := GetOCIProcAddress('OCILobFileClose');
        @OCILobIsOpen := GetOCIProcAddress('OCILobFileIsOpen');
        @OCILobOpen := GetOCIProcAddress('OCILobFileOpen');
    end;

    @OCIDescribeAny := GetOCIProcAddress('OCIDescribeAny');

    if FOCIVersion >= cvOracle81000 then begin
        @OCIDirPathAbort := GetOCIProcAddress('OCIDirPathAbort');
        @OCIDirPathDataSave := GetOCIProcAddress('OCIDirPathDataSave');
        @OCIDirPathFinish := GetOCIProcAddress('OCIDirPathFinish');
        @OCIDirPathFlushRow := GetOCIProcAddress('OCIDirPathFlushRow');
        @OCIDirPathPrepare := GetOCIProcAddress('OCIDirPathPrepare');
        @OCIDirPathLoadStream := GetOCIProcAddress('OCIDirPathLoadStream');
        @OCIDirPathColArrayEntryGet := GetOCIProcAddress('OCIDirPathColArrayEntryGet');
        @OCIDirPathColArrayEntrySet := GetOCIProcAddress('OCIDirPathColArrayEntrySet');
        @OCIDirPathColArrayRowGet := GetOCIProcAddress('OCIDirPathColArrayRowGet');
        @OCIDirPathColArrayReset := GetOCIProcAddress('OCIDirPathColArrayReset');
        @OCIDirPathColArrayToStream := GetOCIProcAddress('OCIDirPathColArrayToStream');
        @OCIDirPathStreamReset := GetOCIProcAddress('OCIDirPathStreamReset');
        if FOCIVersion < cvOracle90000 then
          @OCIDirPathStreamToStream := GetOCIProcAddress('OCIDirPathStreamToStream');
    end;

    if FOCIVersion >= cvOracle81000 then begin
        @OCILobCreateTemporary := GetOCIProcAddress('OCILobCreateTemporary');
        @OCILobFreeTemporary := GetOCIProcAddress('OCILobFreeTemporary');
        @OCILobIsTemporary := GetOCIProcAddress('OCILobIsTemporary');
    end;
end;

procedure InitOCI;
begin
    if not FOCIInitialized then begin
        GetOCIPaths;
        GetOCIVersion;
        GetTNSPaths;
        LoadOCILibrary;
        LoadOCIEntrys;
        FOCIInitialized := True;
    end;
end;

procedure TerminateOCI;
begin
    if FOCIInitialized then begin
        FreeLibrary(FOCIhDll);
        FOCIInitialized := False;
    end;
end;

initialization

    FOCIInitialized := False;
    FOCIBDECompatibility := False;

finalization

    TerminateOCI;

end.
