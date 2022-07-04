unit OraDefines;

interface

const
 OCI_DEFAULT=0;
 OCI_THREADED=1;
 OCI_OBJECT=2;

 OCI_DYNAMIC_FETCH=2;

 OCI_ONE_PIECE=0;
 OCI_FIRST_PIECE=1;
 OCI_NEXT_PIECE=2;
 OCI_LAST_PIECE=3;

 OCI_V7_SYNTAX=2;
 OCI_V8_SYNTAX=3;
 OCI_NTV_SYNTAX=1;

//------------------------Authentication Modes-------------------------------//
 OCI_MIGRATE     = $0001;                         // migratable auth context //
 OCI_SYSDBA      = $0002;                        // for SYSDBA authorization //
 OCI_SYSOPER     = $0004;                       // for SYSOPER authorization //
 OCI_PRELIM_AUTH = $0008;                   // for preliminary authorization //
 OCIP_ICACHE     = $0010;       // Private OCI cache mode to notify cache db //
//---------------------------------------------------------------------------//

 OCI_BATCH_MODE        = $01;
 OCI_EXACT_FETCH       = $02;
 OCI_KEEP_FETCH_STATE  = $04;
 OCI_SCROLLABLE_CURSOR = $08;
 OCI_DESCRIBE_ONLY     = $10;
 OCI_COMMIT_ON_SUCCESS = $20;

 OCI_OTYPE_NAME = 1;
 OCI_OTYPE_REF  = 2;
 OCI_OTYPE_PTR  = 3;

 OCI_PTYPE_TYPE = 6;
 OCI_PTYPE_LIST = 11;

 OCI_HTYPE_FIRST=1;
 OCI_HTYPE_ENV=1;
 OCI_HTYPE_ERROR=2;
 OCI_HTYPE_SVCCTX=3;
 OCI_HTYPE_STMT=4;
 OCI_HTYPE_BIND=5;
 OCI_HTYPE_DEFINE=6;
 OCI_HTYPE_DESCRIBE=7;
 OCI_HTYPE_SERVER=8;
 OCI_HTYPE_SESSION=9;
 OCI_HTYPE_TRANS=10;
 OCI_HTYPE_COMPLEXOBJECT=11;
 OCI_HTYPE_SECURITY=12;
 OCI_HTYPE_LAST=12;

 OCI_DTYPE_FIRST=50;
 OCI_DTYPE_LOB=50;
 OCI_DTYPE_SNAP=51;
 OCI_DTYPE_RSET=52;
 OCI_DTYPE_PARAM=53;
 OCI_DTYPE_ROWID=54;
 OCI_DTYPE_COMPLEXOBJECTCOMP=55;
 OCI_DTYPE_FILE=56;
 OCI_DTYPE_LAST=56;

 OCI_STMT_SELECT  = 1;
 OCI_STMT_UPDATE  = 2;
 OCI_STMT_DELETE  = 3;
 OCI_STMT_INSERT  = 4;
 OCI_STMT_CREATE  = 5;
 OCI_STMT_DROP    = 6;
 OCI_STMT_ALTER   = 7;
 OCI_STMT_BEGIN   = 8;
 OCI_STMT_DECLARE = 9;

{ OCI_ATTR_FNCODE  1
 OCI_ATTR_OBJECT   2
 OCI_ATTR_NONBLOCKING_MODE  3
 OCI_ATTR_SQLCODE  4
 OCI_ATTR_ENV  5
}
 OCI_ATTR_SERVER=6;
 OCI_ATTR_SESSION=7;
 {
 OCI_ATTR_TRANS   8}
 OCI_ATTR_ROW_COUNT=9;
{ OCI_ATTR_SQLFNCODE 10
 OCI_ATTR_PREFETCH_ROWS  11
 OCI_ATTR_NESTED_PREFETCH_ROWS 12
 OCI_ATTR_PREFETCH_MEMORY 13
 OCI_ATTR_NESTED_PREFETCH_MEMORY 14
 OCI_ATTR_CHAR_COUNT  15
 OCI_ATTR_PDSCL   16
 OCI_ATTR_PDFMT   17}
 OCI_ATTR_PARAM_COUNT=18;
{ OCI_ATTR_ROWID   19
 OCI_ATTR_CHARSET  20
 OCI_ATTR_NCHAR   21
}
 OCI_ATTR_USERNAME=22;
 OCI_ATTR_PASSWORD=23;
 OCI_ATTR_STMT_TYPE=24;
{ OCI_ATTR_INTERNAL_NAME   25
 OCI_ATTR_EXTERNAL_NAME   26
 OCI_ATTR_XID     27
 OCI_ATTR_TRANS_LOCK 28
 OCI_ATTR_TRANS_NAME      29
 OCI_ATTR_HEAPALLOC 30
 OCI_ATTR_CHARSET_ID 31
 OCI_ATTR_CHARSET_FORM 32
 OCI_ATTR_MAXDATA_SIZE 33
 OCI_ATTR_CACHE_OPT_SIZE 34
 OCI_ATTR_CACHE_MAX_SIZE 35
 OCI_ATTR_PINOPTION 36
 OCI_ATTR_ALLOC_DURATION 37
 OCI_ATTR_PIN_DURATION 38
 OCI_ATTR_FDO          39
 OCI_ATTR_POSTPROCESSING_CALLBACK 40
 OCI_ATTR_POSTPROCESSING_CONTEXT 41
 OCI_ATTR_ROWS_RETURNED 42
 OCI_ATTR_FOCBK        43
 OCI_ATTR_IN_V8_MODE   44
 OCI_ATTR_LOBEMPTY     45
 OCI_ATTR_SESSLANG     46
}
 OCI_ATTR_DATA_SIZE     = 1 ;
 OCI_ATTR_DATA_TYPE     = 2 ;
 OCI_ATTR_DISP_SIZE     = 3 ;
 OCI_ATTR_NAME          = 4 ;
 OCI_ATTR_PRECISION     = 5 ;
 OCI_ATTR_SCALE         = 6 ;
 OCI_ATTR_IS_NULL       = 7 ;
 OCI_ATTR_TYPE_NAME     = 8 ;
 OCI_ATTR_SCHEMA_NAME   = 9 ;
 OCI_ATTR_SUB_NAME      = 10;
 OCI_ATTR_POSITION      = 11;

 OCI_ATTR_PARAM         = 124;

 OCI_SUCCESS=0;
 OCI_SUCCESS_WITH_INFO=1;
 OCI_NO_DATA=100;
 OCI_ERROR=-1;
 OCI_INVALID_HANDLE=-2;
 OCI_NEED_DATA=99;
 OCI_STILL_EXECUTING=-3123;
 OCI_CONTINUE=-24200;

 OCI_CRED_RDBMS=1;
 OCI_CRED_EXT=2;

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
 SQLT_CLOB = 112;
 SQLT_BLOB = 113;
{ #define SQLT_NTY  108
#define SQLT_REF  110
#define SQLT_BFILE 114
#define SQLT_CFILE 115
}
 SQLT_RSET = 116;
{#define SQLT_NCO  122
#define SQLT_VST  155
#define SQLT_ODT  156}

{------------------------ Transaction Start Flags --------------------------}
 OCI_TRANS_OLD         = $00000000;
 OCI_TRANS_NEW         = $00000001;
 OCI_TRANS_JOIN        = $00000002;
 OCI_TRANS_RESUME      = $00000004;
 OCI_TRANS_STARTMASK   = $000000ff;

 OCI_TRANS_READONLY    = $00000100;
 OCI_TRANS_READWRITE   = $00000200;
 OCI_TRANS_SERIALIZABLE= $00000400;
 OCI_TRANS_ISOLMASK    = $0000ff00;

{------------------------Scrollable Cursor Options--------------------------}
 OCI_FETCH_NEXT   = $02;
 OCI_FETCH_FIRST  = $04;
 OCI_FETCH_LAST   = $08;
 OCI_FETCH_PRIOR  = $10;
 OCI_FETCH_ABSOLUTE =$20;
 OCI_FETCH_RELATIVE =$40;

type
     sword=integer;
     eword=integer;
     sb4=integer;
     ub4=cardinal;
     sb2=smallint;
     ub2=word;
     sb1=shortint;
     ub1=byte;
     dvoid=pointer;
     size_t=integer;

     pOCIEnv = pointer;
     pOCIServer = pointer;
     pOCIError = pointer;
     pOCISvcCtx = pointer;
     pOCIStmt = pointer;
     pOCILobLocator = pointer;
     pOCIDefine = pointer;
     pOCISession = pointer;
     pOCISnapshot = pointer;
     pOCIParam = pointer;
     pOCIBind = pointer;
     pOCIDescribe = pointer;

     dword = longword;
     pDouble = ^Double;
     pByte   = ^Byte;
     pInteger = ^Integer;
     pInt64 = ^Int64;

TOCIInitialize=function(mode:ub4;
                        ctxp:pointer;
                        malocfp:pointer;
                        ralocfp:pointer;
                        mfreefp:pointer):sword; cdecl;
TOCIEnvInit = function(var envhpp:pOCIEnv;
                       mode:ub4;
                       xtramemsz:size_t;
                       usrmempp:pointer):sword; cdecl;
TOCIEnvCreate = function(var envhpp:pOCIEnv;
                       mode:ub4;
                       ctxp:pointer;
                       malocfp:pointer;
                       ralocfp:pointer;
                       mfreefp:pointer;
                       xtramemsz:size_t;
                       usrmempp:pointer):sword; cdecl;
TOCIHandleAlloc=function(parenth:pOCIEnv;
                        var hndlpp:pOCIEnv;
                        atype:ub4;
                        xtramem_sz:size_t;
                        usrmempp:pointer ):sword; cdecl;
TOCIServerAttach=function(srvhp:pOCIServer;
                         errhp:pOCIError;
                         dblink:pchar;
                         dblink_len:sb4;
                         mode:ub4):sword; cdecl;
TOCIAttrSet=function(trgthndlp:pointer;
                     trghndltyp:ub4;
                     attributep:pointer;
                     size:ub4;
                     attrtype:ub4;
                     errhp:pOCIError):sword; cdecl;
TOCISessionBegin=function(svchp:pOCISvcCtx;
                         errhp:pOCIError;
                         usrhp:pOCISession;
                         credt:ub4;
                         mode:ub4):sword; cdecl;
TOCISessionEnd=function(svchp:pOCISvcCtx;
                        errhp:pOCIError;
                        usrhp:pOCISession;
                        mode:ub4):sword; cdecl;
TOCIServerDetach=function(srvhp:pOCIServer;
                          errhp:pOCIError;
                          mode:ub4):sword; cdecl;
TOCIHandleFree=function(hndlp:pointer;
                       atype:ub4):sword; cdecl;

TOCIErrorGet=function(hndlp:pointer;
                      recordno:ub4;
                      sqlstate:pchar;
                      var errcodep:sb4;
                      bufp:pchar;
                      bufsiz:ub4;
                      atype:ub4):sword; cdecl;
TOCIStmtPrepare=function(stmtp:pOCIStmt;
                         errhp:pOCIError;
                         stmt:pchar;
                         stmt_len:ub4;
                         language:ub4;
                         mode:ub4):sword; cdecl;
TOCIStmtExecute=function(svchp:pOCISvcCtx;
                         stmtp:pOCIStmt;
                         errhp:pOCIError;
                         iters:ub4;
                         rowoff:ub4;
                         snap_in:pOCISnapshot;
                         snap_out:pOCISnapshot;
                         mode:ub4):sword; cdecl;
TOCIParamGet=function(hndlp:pointer;
                      htype:ub4;
                      errhp:pOCIError;
                      var parmdpp:pointer;
                      pos:ub4):sword; cdecl;
TOCIAttrGet=function(trgthndlp:pointer;
                     trghndltyp:ub4;
                     attributep:pointer;
                     sizep:pointer;
                     attrtype:ub4;
                     errhp:pOCIError):sword; cdecl;
TOCIStmtFetch=function(stmtp:pOCIStmt;
                       errhp:pOCIError;
                       nrows:ub4;
                       orientation:ub2;
                       mode:ub4):sword; cdecl;
TOCIDefineByPos=function(stmtp:pOCIStmt;
                         var defnpp:pOCIDefine;
                         errhp:pOCIError;
                         position:ub4;
                         valuep:pointer;
                         value_sz:sb4;
                         dty:ub2;
                         indp:pointer;
                         rlenp:pointer;
                         rcodep:pointer;
                         mode:ub4):sword; cdecl;
TOCIDefineArrayOfStruct=function(
                         defnpp:pOCIDefine;
                         errhp:pOCIError;
                         pvskip:ub4;
                         indskip:ub4;
                         rlskip:ub4;
                         rcskip:ub4
                         ):sword; cdecl;
TOCIBindByPos=function(  stmtp:pOCIStmt;
                         var bindpp:pOCIBind;
                         errhp:pOCIError;
                         position:ub4;
                         valuep:pointer;
                         value_sz:sb4;
                         dty:ub2;
                         indp:pointer;
                         var alenp:ub2;
                         var rcodep:ub2;
                         maxarr_len:ub4;
                         var curelep:ub4;
                         mode:ub4):sword;cdecl;
TOCIBindByName=function( stmtp:pOCIStmt;
                         var bindpp:pOCIBind;
                         errhp:pOCIError;
                         placeholder:PChar;
                         placeh_len:sb4;
                         valuep:pointer;
                         value_sz:sb4;
                         dty:ub2;
                         indp:pointer;
                         alenp:pointer;
                         rcodep:pointer;
                         maxarr_len:ub4;
                         curelep:pointer;
                         mode:ub4):sword;cdecl;
TOCITransStart=function( svchp:pOCISvcCtx;
                         errhp:pOCIError;
                         timeout:word;
                         flags:ub4):sword;cdecl;
TOCITransRollback=function(svchp:pOCISvcCtx;
                           errhp:pOCIError;
                           flags:ub4):sword;cdecl;
TOCITransCommit=function(svchp:pOCISvcCtx;
                         errhp:pOCIError;
                         flags:ub4):sword;cdecl;
TOCIDescribeAny=function(svchp:pOCISvcCtx;
                         errhp:pOCIError;
                         objptr:pointer;
                         objnm_len:ub4;
                         objptr_typ:ub1;
                         info_level:ub1;
                         objtyp:ub1;
                         dschp:pOCIDescribe):sword;cdecl;
TOCIBreak =     function(svchp:pOCISvcCtx;
                         errhp:pOCIError):sword;cdecl;

TOCIDescriptorAlloc = function(parenth:pOCIEnv;
                               descpp:pointer;
                               htype:ub4;
                               xtramem_sz:integer;
                               usrmempp:pointer):sword;cdecl;

TOCIDescriptorFree = function(descp:pointer;
                              htype:ub4):sword;cdecl;
TOCILobGetLength = function(svchp:pOCISvcCtx;
                            errhp:pOCIError;
                            locp:pOCILobLocator;
                            var amtp:ub4):sword; cdecl;

TOCILobRead = function(svchp:pOCISvcCtx;
                       errhp:pOCIError;
                       locp:pOCILobLocator;
                       var amtp:ub4;
                       offset:ub4;
                       bufp:pointer;
                       bufl:ub4;
                       ctxp:pointer;
                       OCICallbackLobRead:pointer;
                       csid:ub2;
                       csfrm:ub1):sword;cdecl;

TOCILobWrite= function(svchp:pOCISvcCtx;
                       errhp:pOCIError;
                       locp:pOCILobLocator;
                       var amtp:ub4;
                       offset:ub4;
                       bufp:pointer;
                       bufl:ub4;
                       piece:ub1;
                       ctxp:pointer;
                       OCICallbackLobWrite:pointer;
                       csid:ub2;
                       csfrm:ub1):sword;cdecl;

TOCILobErase= function(svchp:pOCISvcCtx;
                       errhp:pOCIError;
                       locp:pOCILobLocator;
                       var amount:ub4;
                       offset:ub4):sword;cdecl;

TOCILobTrim= function(svchp:pOCISvcCtx;
                       errhp:pOCIError;
                       locp:pOCILobLocator;
                       newlen:ub4):sword;cdecl;

TOCIStmtGetPieceInfo = function(stmtp:pOCIStmt;
                                errhp:pOCIError;
                                var hndlpp:pointer;
                                var typep:ub4;
                                var in_outp:ub1;
                                var iterp:ub4;
                                var idxp:ub4;
                                var piecep:ub1):sword;cdecl;

TOCIStmtSetPieceInfo = function(handle:pointer;
                                typep:ub4;
                                errhp:pOCIError;
                                buf:pointer;
                                var alenp:ub4;
                                piece:ub1;
                                indp:pointer;
                                var rcodep:ub2):sword;cdecl;

    oradate=record
     century,year,month,day,hour,minute,second:ub1;
    end;

implementation

end.
