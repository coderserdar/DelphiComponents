{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnRedisClient;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ����� Redis �ͻ���ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack������ CodeGame/ReverseKing
* ��    ע��
* ����ƽ̨��PWinXP + Delphi XE
* ���ݲ��ԣ�PWinXP/7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2016.09.10 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER7_UP}
  {$MESSAGE ERROR 'CnRedisClient only Supports Delphi 7 or above.'}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, TypInfo, Graphics, ScktComp, Contnrs;

const
  SCN_REDIS_CRLF = Char($0D) + Char($0A);
  SCN_REDISD_EFAULT_BUFFER_SIZE = 32 * 1024; //32K
  SCN_REDIS_INFO_SECTION_NAME: array[0..11] of string = ('default', 'all',
    'server', 'clients', 'memory', 'persistence', 'stats', 'replication', 'cpu',
    'commandstats', 'cluster', 'keyspace');
  SCN_REDIS_DATA_TYPE_NAME: array[0..7] of string = ('none', 'string', 'list',
    'set', 'zset', 'hash', 'integer', 'msg');
  SCN_REDIS_OPERATION: array[0..3] of string = ('AND', 'OR', 'XOR', 'NOT');
  SCN_REDIS_EMPTY_VALUE = '(empty)';

type
  ICnRedisSocket = interface(IInterface)
    ['{C5C77E7F-9483-4C68-AED5-45D9398EB425}']
    procedure SetHost(Value: string);
    function GetHost: string;
    procedure SetPort(Value: Word);
    function GetPort: Word;
    procedure SetPassword(Value: string);
    function GetPassword: string;
    procedure SetConnecting(Value: Boolean);
    function GetConnecting: Boolean;
    function Connect: Boolean;
    procedure Disconnect;
    function SendBuffer(Buffer: Pointer; Length: Cardinal): Integer;
    function RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer;
    property RedisHost: string read GetHost write SetHost;
    property RedisPort: Word read GetPort write SetPort;
    property Password: string read GetPassword write SetPassword;
    property Connecting: Boolean read GetConnecting write SetConnecting;
  end;

  TCnRedisAbstractSocket = class(TInterfacedPersistent, ICnRedisSocket)
  private
  protected
    FPassword: string;
    FConnecting: Boolean;
    procedure SetHost(Value: string); virtual; abstract;
    function GetHost: string; virtual; abstract;
    procedure SetPort(Value: Word); virtual; abstract;
    function GetPort: Word; virtual; abstract;
    procedure SetPassword(Value: string); virtual; abstract;
    function GetPassword: string; virtual; abstract;
    procedure SetConnecting(Value: Boolean); virtual; abstract;
    function GetConnecting: Boolean; virtual; abstract;
    function SendBuffer(Buffer: Pointer; Length: Cardinal): Integer; virtual; abstract;
    function RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    function Connect: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    property RedisHost: string read GetHost write SetHost;
    property RedisPort: Word read GetPort write SetPort;
    property Password: string read GetPassword write SetPassword;
    property Connecting: Boolean read GetConnecting write SetConnecting;
  end;

  TCnRedisAbstractSocketType = class of TCnRedisAbstractSocket;

  TCnRedisBuffer = array of Byte;

  TCnRedisDataType = (rdt_none, rdt_string, rdt_list, rdt_set, rdt_zset,
    rdt_hash, rdt_integer, rdt_msg);

  TCnRedisInfoSection = (ris_default, ris_all, ris_server, ris_clients,
    ris_memory, ris_persistence, ris_stats, ris_replication, ris_cpu,
    ris_commandstats, ris_cluster, ris_keyspace);

  TCnRedisOperation = (ro_AND, ro_OR, ro_XOR, or_NOT);

  TCnRedisKeyValue = packed record
    Key: string;
    Value: string;
  end;

  TCnRedisKeyValueArray = array of TCnRedisKeyValue;

  PCnRedisMultiBulkNode = ^TCnRedisMultiBulkNode;

  TCnRedisMultiBulkNode = class
  private
    FCurrIndex: Integer;
    FParent: TCnRedisMultiBulkNode;
    FMultiBulkRefs: TObjectList;
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    // ������ SetLength����֤ÿ��Ԫ�ض�����Ч�� Node����ʱ���ӣ���ʱ�ͷ�
    procedure ChangeMultiBulksSize(NewCount: Integer);

    property CurrIndex: Integer read FCurrIndex write FCurrIndex;
    property Parent: TCnRedisMultiBulkNode read FParent write FParent;
    property Value: string read FValue write FValue;
    property MultiBulkRefs: TObjectList read FMultiBulkRefs write FMultiBulkRefs;
  end;

  TCnRedisMultiBulk = TCnRedisMultiBulkNode;

  TCnRedisClientSocket = class(TCnRedisAbstractSocket)
  protected
    FSocket: TClientSocket;
    procedure SetHost(Value: string); override;
    function GetHost: string; override;
    procedure SetPort(Value: Word); override;
    function GetPort: Word; override;
    procedure SetPassword(Value: string); override;
    function GetPassword: string; override;
    procedure SetConnecting(Value: Boolean); override;
    function GetConnecting: Boolean; override;
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    function SendBuffer(Buffer: Pointer; Length: cardinal): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: cardinal): Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Connect: Boolean; override;
    procedure Disconnect; override;
  end;

  ICnRedisCommand280 = interface(IInterface)
    ['{3F9E4F71-0F9D-43F9-B4D2-6A6284E5EFFE}']
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
    {$REGION '==========Redis Command v2.8=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; //ɾ��������һ������key,����ֵ����ɾ��key������,�����ڷ���0��
    function DUMP(const Key: string): string; //�л����� key �������ر����л���ֵ��ʹ�� RESTORE ������Խ����ֵ�����л�Ϊ Redis ����
    function EXISTS(const Key: string): Boolean;  //������ key �Ƿ���ڡ�
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; //Ϊ���� key ��������ʱ�䣬�� key ����ʱ(����ʱ��Ϊ 0 )�����ᱻ�Զ�ɾ��,��λ�롣
    function EXPIREAT(const Key: string; UnixTime: Int64): Boolean; //���ܺ�EXPIREһ����������ܵ�ʱ������� UNIX ʱ���(unix timestamp)��
    function KEYS(const pattern: string; Value: TStrings): Integer; //�������з��ϸ���ģʽ pattern �� key
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; //�� key ԭ���Եشӵ�ǰʵ�����͵�Ŀ��ʵ����ָ�����ݿ���
    function MOVE(const Key: string; DB: Integer): Boolean; //����ǰ���ݿ�� key �ƶ������������ݿ� db ���С�
    function OBJECTREFCOUNT(const key: string): Integer; // ���ظ��� key �����������ֵ�Ĵ�������������Ҫ���ڳ���
    function OBJECTENCODING(const key: string): string; // ���ظ��� key �������ֵ��ʹ�õ��ڲ���ʾ(representation)��
    function OBJECTIDLETIME(const key: string): Integer; // ���ظ��� key �Դ��������Ŀ�תʱ��(idle�� û�б���ȡҲû�б�д��)������Ϊ��λ��
    function PERSIST(const Key: string): Boolean;  //�Ƴ����� key ������ʱ�䣬����� key �ӡ���ʧ�ġ�(������ʱ�� key )ת���ɡ��־õġ�(һ����������ʱ�䡢�������ڵ� key )��
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean;  //Ϊ���� key ��������ʱ�䣬�� key ����ʱ(����ʱ��Ϊ 0 )�����ᱻ�Զ�ɾ��,��λ���롣
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; //���ܺ�EXPIREATһ���������Ժ���Ϊ��λ���� key �Ĺ��� unix ʱ�����
    function TTL(const Key: string): Int64; //����Ϊ��λ�����ظ��� key ��ʣ������ʱ��
    function PTTL(const Key: string): Int64; //������������� TTL ��������Ժ���Ϊ��λ���� key ��ʣ������ʱ�䣬�������� TTL ��������������Ϊ��λ��
    function RANDOMKEY: string; //�ӵ�ǰ���ݿ����������(��ɾ��)һ�� key ��
    function RENAME(const Key, NewKey: string): Boolean; //��key����Ϊnewkey����key��newkey��ͬ����key������ʱ������һ�����󡣵�newkey�Ѿ�����ʱ��RENAME������Ǿ�ֵ��
    function RENAMENX(const Key, NewKey: string): Boolean; //���ҽ��� newkey ������ʱ���� key ����Ϊ newkey ��
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; //�����л����������л�ֵ���������͸�����key����������ttl�Ժ���Ϊ��λΪkey��������ʱ�䣻���ttlΪ0 ����ô����������ʱ�䡣
    function SORT(const Key, Param: string; Value: TStrings): Integer; //���ػ򱣴�����б����ϡ����򼯺� key �о��������Ԫ�ء�
    function _TYPE(const Key: string): TCnRedisDataType;
    function SCAN(cursor: Integer; MATCH: string; Count: Integer):
      TCnRedisMultiBulk; //2.8+

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; //��� key �Ѿ����ڲ�����һ���ַ����� APPEND ��� value ׷�ӵ� key ԭ����ֵ��ĩβ����� key �����ڣ� APPEND �ͼ򵥵ؽ����� key ��Ϊ value ������ִ�� SET key value һ����
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; //��������ַ����У�������Ϊ 1 �ı���λ������,���ر�����Ϊ 1 ��λ��������
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; //��һ���������������λ���ַ��� key ����λԪ����������������浽 destkey �ϡ�
    function DECR(const Key: string): Int64; //�� key �д��������ֵ��һ��
    function DECRBY(const Key: string; Decrement: Int64): Int64; //�� key �������ֵ��ȥ���� decrement ��
    function GETRANGE(const Key: string; Start, Stop: Integer): string; //���� key ���ַ���ֵ�����ַ������ַ����Ľ�ȡ��Χ�� start �� end ����ƫ��������(���� start �� end ����)��
    function GET(const Key: string): string;  //���� key ���������ַ���ֵ,���key��������ô��������ֵnil ��
    function GETBIT(const Key: string; offset: Integer): Integer;
    function _SET(const Key, Value: string; EXSecond: Cardinal; Exist: Byte):
      Boolean; //���ַ���ֵvalue������key,EXSecond��ֵ����ʱ����λ��,Exist��0�������Ƿ���ڶ����ã�1������ʱ�����ã�2��������ʱ������,�ɹ�����true
    function GETSET(const Key, Value: string): string; //������ key ��ֵ��Ϊ value �������� key �ľ�ֵ(old value)���� key ���ڵ������ַ�������ʱ������һ������
    function INCR(const Key: string): Int64;  //�� key �д��������ֵ��һ����� key �����ڣ���ô key ��ֵ���ȱ���ʼ��Ϊ 0 ��Ȼ����ִ�� INCR ����������ִ�� INCR ����֮�� key ��ֵ��
    function INCRBY(const Key: string; Increment: Int64): Int64;  //�� key �������ֵ�������� Increment �����ؼ��� Increment ֮�� key ��ֵ��
    function INCRBYFLOAT(const Key: string; Increment: Single): Single;  //�� key �������ֵ���ϸ������� Increment �����ؼ��� Increment ֮�� key �ĸ���ֵ��
    function MGET(const Keys: string; Value: TStrings): Integer; //��������(һ������)���� key ��ֵ��
    function MSET(const KVs: string): Boolean; //ͬʱ����һ������ key-value �ԣ����Ƿ���true
    function MSETNX(const KVs: string): Boolean; //ͬʱ����һ������ key-value �ԣ����ҽ������и��� key �������ڡ���ʹֻ��һ������ key �Ѵ��ڣ� MSETNX Ҳ��ܾ�ִ�����и��� key �����ò�����
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;  //�������� SETEX �������ƣ������Ժ���Ϊ��λ���� key ������ʱ�䣬�������� SETEX ��������������Ϊ��λ��
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; //�� key ��������ַ���ֵ�����û����ָ��ƫ�����ϵ�λ(bit)��
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; //��ֵ value ������ key ������ key ������ʱ����Ϊ seconds (����Ϊ��λ)����� key �Ѿ����ڣ� SETEX �����д��ֵ��
    function SETNX(const Key, Value: string): Boolean; //�� key ��ֵ��Ϊ value �����ҽ��� key �����ڡ��������� key �Ѿ����ڣ��� SETNX �����κζ�����
    function SETRANGE(const Key, value: string; Offset: Integer): Integer; //�� value ������д(overwrite)���� key ��������ַ���ֵ����ƫ���� Offset ��ʼ�������ڵ� key �����հ��ַ�������,���ر� SETRANGE �޸�֮���ַ����ĳ��ȡ�
    function STRLEN(const Key: string): Integer; //���� key ��������ַ���ֵ�ĳ��ȡ��� key ����Ĳ����ַ���ֵʱ������һ������

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; //ɾ����ϣ�� key �е�һ������ָ���򣬲����ڵ��򽫱����ԡ����ر�ɾ����������
    function HEXISTS(const Key, Field: string): Boolean; //�鿴��ϣ�� key �У������� Field �Ƿ���ڡ�
    function HGET(const Key, Field: string): string; //���ع�ϣ�� key �и����� Field ��ֵ��
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray): Integer;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; //Ϊ��ϣ�� key �е��� Field ��ֵ�������� Increment ������Ҳ����Ϊ�������൱�ڶԸ�������м�������������ִ�� HINCRBY ����֮�󣬹�ϣ�� key ���� Field ��ֵ��
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single; //Ϊ��ϣ�� key �е��� Field ��ֵ�������� Increment ������Ҳ����Ϊ�������൱�ڶԸ�������м�������������ִ�� HINCRBY ����֮�󣬹�ϣ�� key ���� Field ��ֵ��
    function HKEYS(const Key: string; Value: TStrings): Integer; //���ع�ϣ�� key �е�������
    function HLEN(const Key: string): Integer; //���ع�ϣ�� key �����������
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; //���ع�ϣ�� key �У�һ�������������ֵ��
    function HMSET(const Key, fvs: string): Boolean; //ͬʱ����� Field-Value (��-ֵ)�����õ���ϣ�� key �С�������Ḳ�ǹ�ϣ�����Ѵ��ڵ���
    function HSET(const Key, Field, Value: string): Boolean; //����ϣ�� key �е��� Field ��ֵ��Ϊvalue�����field�ǹ�ϣ���е�һ���½��򣬲���ֵ���óɹ�������1�������ϣ������field�Ѿ������Ҿ�ֵ�ѱ���ֵ���ǣ�����0��
    function HSETNX(const Key, Field, Value: string): Boolean; //����ϣ�� key �е��� Field ��ֵ����Ϊ Value �����ҽ����� Field �����ڡ����� Field �Ѿ����ڣ��ò�����Ч��
    function HVALS(const Key: string; Value: TStrings): Integer; //���ع�ϣ�� key ���������ֵ��
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; //���� LPOP ����������汾���������б���û���κ�Ԫ�ؿɹ�������ʱ�����ӽ��� BLPOP ����������ֱ���ȴ���ʱ���ֿɵ���Ԫ��Ϊֹ��
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; //���� RPOP ����������汾���������б���û���κ�Ԫ�ؿɹ�������ʱ�����ӽ��� BRPOP ����������ֱ���ȴ���ʱ���ֿɵ���Ԫ��Ϊֹ��
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; //�����棬���б� source �е����һ��Ԫ��(βԪ��)�����������ظ��ͻ��ˡ��� source ������Ԫ�ز��뵽�б� Destination ����Ϊ Destination �б�ĵ�ͷԪ�ء�
    function LINDEX(const Key: string; index: Integer): string; //�����б� key �У��±�Ϊ index ��Ԫ�ء�
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; //��ֵ Value ���뵽�б� key ���У�λ��ֵ Pivot ֮ǰ��֮�󡣵� Pivot ���������б� key ʱ����ִ���κβ�����
    function LLEN(const Key: string): Integer; //�����б� key �ĳ��ȡ�
    function LPOP(const Key: string): string; //�Ƴ��������б� key ��ͷԪ�ء�
    function LPUSH(const Key, values: string): Integer; //��һ������ֵ Value ���뵽�б� key �ı�ͷ
    function LPUSHX(const Key, Value: string): Integer; //��ֵ Value ���뵽�б� key �ı�ͷ�����ҽ��� key ���ڲ�����һ���б��� LPUSH �����෴���� key ������ʱ�� LPUSHX ����ʲôҲ������
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; //�����б� key ��ָ�������ڵ�Ԫ�أ�������ƫ���� Start �� Stop ָ����
    function LREM(const Key, Value: string; Count: Integer): Integer;  //���ݲ��� Count ��ֵ���Ƴ��б�������� Value ��ȵ�Ԫ�ء�
    function LSET(const Key, Value: string; Index: Integer): Boolean;   //���б� key �±�Ϊ index ��Ԫ�ص�ֵ����Ϊ Value ��
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean;
    function RPOP(const Key: string): string; //�Ƴ��������б� key ��βԪ�ء�
    function RPOPLPUSH(const source, Destination: string): string;
    function RPUSH(const Key, values: string): Integer; //��һ������ֵ Value ���뵽�б� key �ı�β(���ұ�)��
    function RPUSHX(const Key, Value: string): Integer; //��ֵ Value ���뵽�б� key �ı�β�����ҽ��� key ���ڲ�����һ���б��� RPUSH �����෴���� key ������ʱ��RPUSHX����ʲôҲ������

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; //��һ������ Member Ԫ�ؼ��뵽���� key ���У��Ѿ������ڼ��ϵ� Member Ԫ�ؽ������ԡ�
    function SCARD(const Key: string): Integer; //���ؼ��� key �Ļ���(������Ԫ�ص�����)��
    function SDIFF(const Keys: string; Value: TStrings): Integer; //����һ�����ϵ�ȫ����Ա���ü��������и�������֮��Ĳ��
    function SDIFFSTORE(const Destination, Keys: string): Integer; //�����������ú� SDIFF ���ƣ�������������浽 Destination ���ϣ������Ǽ򵥵ط��ؽ������
    function SINTER(const Keys: string; Value: TStrings): Integer;  //����һ�����ϵ�ȫ����Ա���ü��������и������ϵĽ�����
    function SINTERSTORE(const Keys, Destination: string): Integer; //������������� SINTER ���������������浽 Destination ���ϣ������Ǽ򵥵ط��ؽ������
    function SISMEMBER(const Key, Member: string): Boolean; // �ж� Member Ԫ���Ƿ񼯺� key �ĳ�Ա��
    function SMEMBERS(const Key: string; Value: TStrings): Integer;   //���ؼ��� key �е����г�Ա
    function SMOVE(const source, Destination, Member: string): Boolean;  //�� Member Ԫ�ش� source �����ƶ��� Destination ���ϡ�
    function SPOP(const Key: string): string; //�Ƴ������ؼ����е�һ�����Ԫ�ء�
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; //�������ִ��ʱ��ֻ�ṩ�� key ��������ô���ؼ����е�һ�����Ԫ�ء�
    function SREM(const Key, Members: string): Integer; //�Ƴ����� key �е�һ������ Member Ԫ�أ������ڵ� Member Ԫ�ػᱻ���ԡ�
    function SUNION(const Keys: string; Value: TStrings): Integer;  //����һ�����ϵ�ȫ����Ա���ü��������и������ϵĲ�����
    function SUNIONSTORE(const Keys, Destination: string): Integer; // ������������� SUNION ���������������浽 Destination ���ϣ������Ǽ򵥵ط��ؽ��������� Destination �Ѿ����ڣ����串�ǡ�
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; //

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer;  //��һ������ Member Ԫ�ؼ��� score ֵ���뵽���� key ���С�
    function ZCARD(const Key: string): Integer;   //�������� key �Ļ�����
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer;  //�������� key �У� score ֵ�� min �� max ֮��(Ĭ�ϰ��� score ֵ���� min �� max )�ĳ�Ա��������
    function ZINCRBY(const Key, Member: string; Increment: Single): Single; //Ϊ���� key �ĳ�Ա Member �� score ֵ�������� increment ��
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray;  //�������� key �У�ָ�������ڵĳ�Ա�����г�Ա��λ�ð� score ֵ����(��С����)������
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;  //�������� key �У����� score ֵ���� min �� max ֮��(�������� min �� max )�ĳ�Ա�����򼯳�Ա�� score ֵ����(��С����)�������С�
    function ZRANK(const Key, Member: string): Integer; //�������� key �г�Ա Member ���������������򼯳�Ա�� score ֵ����(��С����)˳�����С�
    function ZREM(const Key, Members: string): Integer; //�Ƴ����� key �е�һ��������Ա�������ڵĳ�Ա�������ԡ�
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer; //�Ƴ����� key �У�ָ������(rank)�����ڵ����г�Ա��
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; //�Ƴ����� key �У����� score ֵ���� min �� max ֮��(�������� min �� max )�ĳ�Ա��
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; //�������� key �У�ָ�������ڵĳ�Ա��
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; //�������� key �У� score ֵ���� max �� min ֮��(Ĭ�ϰ������� max �� min )�����еĳ�Ա�����򼯳�Ա�� score ֵ�ݼ�(�Ӵ�С)�Ĵ������С�
    function ZREVRANK(const Key, Member: string): Integer;  //�������� key �У�ָ�������ڵĳ�Ա��
    function ZSCORE(const Key, Member: string): string; //�������� key �У���Ա Member �� score ֵ��
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; //���������һ���������򼯵Ĳ��������и��� key ������������ numkeys ����ָ���������ò���(�����)���浽 Destination ��Ĭ������£��������ĳ����Ա�� score ֵ�����и������¸ó�Ա score ֵ֮ �� ��
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; //���������һ���������򼯵Ľ��������и��� key ������������ numkeys ����ָ���������ý���(�����)���浽 Destination ��Ĭ������£��������ĳ����Ա�� score ֵ�����и������¸ó�Ա score ֵ֮��.
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;  //����һ���������ϸ���ģʽ��Ƶ����
    function PUBLISH(const Channel, message: string): Integer; //����Ϣ message ���͵�ָ����Ƶ�� channel ��
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      // �鿴�����뷢��ϵͳ״̬����ʡ��� ����������ͬ��ʽ�����������
    function PUNSUBSCRIBE(const patterns: string): string; //ָʾ�ͻ����˶����и���ģʽ��
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer; //���ĸ�����һ������Ƶ������Ϣ��
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;  //ָʾ�ͻ����˶�������Ƶ����


    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; //ȡ�����񣬷���ִ��������ڵ��������
    function EXEC(Value: TStrings): Integer; //ִ������������ڵ����
    function MULTI: Boolean; //���һ�������Ŀ�ʼ��
    function UNWATCH: Boolean; //ȡ�� WATCH ��������� key �ļ��ӡ�
    function WATCH(const Keys: string): Boolean; //����һ��(����) key �����������ִ��֮ǰ���(����Щ) key �������������Ķ�����ô���񽫱���ϡ�

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer; //ͨ�����õ� Lua ������������ʹ�� EVAL ����� Lua �ű�������ֵ��
    function EVALSHA(const Sha1, Keys, Arg: string): string; //���ݸ����� sha1 У���룬�Ի����ڷ������еĽű�������ֵ��
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer; //����һ�������ű��� SHA1 У��ͣ�����һ������ 0 �� 1 ���б���ʾУ�����ָ���Ľű��Ƿ��Ѿ��������ڻ��浱�С�
    function SCRIPTFLUSH: Boolean; //������� Lua �ű����档
    function SCRIPTKILL: Boolean; //ɱ����ǰ�������е� Lua �ű������ҽ�������ű�û��ִ�й��κ�д����ʱ������������Ч��
    function SCRIPTLOAD(const Script: string): string; //���ű� script ��ӵ��ű������У�����������ִ������ű���

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; //����ƥ��ʱ���� OK �����򷵻�һ������
    function ECHO(const Msg: string): string;
    function PING: Boolean; //������������ͷ���һ�� PONG �����򷵻�һ�����Ӵ���.
    procedure QUIT; //����������ر��뵱ǰ�ͻ��˵����ӡ�
    function SELECT(DB: Integer): Boolean;  //�л���ָ�������ݿ⣬���ݿ������� index ������ֵָ������ 0 ��Ϊ��ʼ����ֵ��

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; //ִ��һ�� AOF�ļ� ��д��������д�ᴴ��һ����ǰ AOF �ļ�������Ż��汾��
    function BGSAVE: string; //�ں�̨�첽(Asynchronously)���浱ǰ���ݿ�����ݵ����̡�
    function CLIENTGETNAME: string; //���� CLIENT SETNAME ����Ϊ�������õ����֡�
    function CLIENTKILL(const IP: string; Port: Word): Boolean; //�رյ�ַΪ ip:port �Ŀͻ��ˡ�
    function CLIENTLIST(Value: TStrings): Integer;  //������ɶ��ĸ�ʽ�������������ӵ��������Ŀͻ�����Ϣ��ͳ�����ݡ�
    function CLIENTSETNAME(const Name: string): Boolean;  //Ϊ��ǰ���ӷ���һ�����֡�
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; //����ȡ�������е� Redis �����������ò���
    function CONFIGRESETSTAT: Boolean;  //���� INFO �����е�ĳЩͳ������
    function CONFIGREWRITE: Boolean; //������ Redis ������ʱ��ָ���� redis.conf �ļ����и�д
    function CONFIGSET(const Parameter, Value: string): Boolean; //���Զ�̬�ص��� Redis ������������(configuration)������������
    function DBSIZE: Int64; //���ص�ǰ���ݿ�� key ��������
    function DEBUGOBJECT(const Key: string): string; //DEBUG OBJECT ��һ�������������Ӧ���ͻ�����ʹ�á�
    procedure DEBUGSEGFAULT; //ִ��һ�����Ϸ����ڴ���ʴӶ��� Redis ���������ڿ���ʱ���� BUG ģ�⡣
    function FLUSHALL: Boolean; //������� Redis ������������(ɾ���������ݿ������ key)
    function FLUSHDB: Boolean; //��յ�ǰ���ݿ��е����� key��
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer; //��һ�����ڽ��ͣ�parse���������Ķ��ĸ�ʽ�����ع��� Redis �������ĸ�����Ϣ��ͳ����ֵ��
    function LASTSAVE: Int64; //�������һ�� Redis �ɹ������ݱ��浽�����ϵ�ʱ�䣬�� UNIX ʱ�����ʽ��ʾ��
    function MONITOR: Boolean; //ʵʱ��ӡ�� Redis ���������յ�����������á�
    function PSYNC(const MASTER_RUN_ID: string; Offset: Integer): string; //���ڸ��ƹ���(replication)���ڲ����
    function SAVE: Boolean; //ִ��һ��ͬ���������������ǰ Redis ʵ�����������ݿ���(snapshot)�� RDB �ļ�����ʽ���浽Ӳ�̡�
    function SHUTDOWN: string; //ֹͣ���пͻ������������һ��������ڵȴ���ִ�� SAVE ������� AOF ѡ��򿪣����� AOF �ļ��ر� redis ������(server)
    function SLAVEOF(const Host: string; Port: Word): Boolean; //SLAVEOF ���������� Redis ����ʱ��̬���޸ĸ���(replication)���ܵ���Ϊ
    function SLOWLOGLEN(const Parameter: string): Integer; //�鿴��ǰ��־������
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; //��ȡ����ָ������־
    function SLOWLOGRESET: Boolean; //��յ�ǰ��־
    function SYNC: string; //���ڸ��ƹ���(replication)���ڲ����
    function TIME: TCnRedisKeyValue;  //һ�����������ַ������б� ��һ���ַ����ǵ�ǰʱ��(�� UNIX ʱ�����ʽ��ʾ)�����ڶ����ַ����ǵ�ǰ��һ�����Ѿ���ȥ��΢������
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

  ICnRedisCommand = interface(ICnRedisCommand280)
    ['{A3162571-61A1-4E6A-8D70-2D77BE43CD6A}']
  end;

  TCnRedisProtocol = class(TInterfacedObject, ICnRedisCommand)
  private
    FInterfacedSocket: ICnRedisSocket;
    FRecvBuffer: TCnRedisBuffer;
    FPipelineMode: Boolean;
    FPipelineBuffer: AnsiString;
    function Serialize(const Value: string; var Serial: AnsiString): Integer;
    function Deserialize(Response: Pointer; Length: Integer; RespNode:
      TCnRedisMultiBulkNode): Boolean;
    procedure SetRecvBufferSize(Value: Cardinal);
    function GetConnecting: Boolean;
    function CreateSocketOfClassName(const Name: string): ICnRedisSocket;
  protected
    function Connect: Boolean; virtual;
    function SendAndReceive(const Send: string; Recv: TCnRedisMultiBulkNode;
      Force: Boolean = False): Boolean; virtual;
    function GetRedisDataTypeOfString(const Value: string): TCnRedisDataType;
  public
    // constructor Create(const SocketClassName: string); virtual; overload;
    constructor Create(ASocketIntf: ICnRedisSocket = nil); virtual;
    destructor Destroy; override;
    procedure Disconnect; virtual;
    procedure PipelineBegin;
    function PipelineEnd(Recv: TCnRedisMultiBulk): Boolean;
    procedure SetRedisServer(const Host: string; Port: Word; const Password: string);
    property Connecting: Boolean read GetConnecting;
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
    {$REGION '==========Redis Command v2.8=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; virtual; abstract;
    function DUMP(const Key: string): string; virtual; abstract;
    function EXISTS(const Key: string): Boolean; virtual; abstract;
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; virtual; abstract;
    function EXPIREAT(const Key: string; Timestamp: Int64): Boolean; virtual; abstract;
    function KEYS(const pattern: string; Value: TStrings): Integer; virtual; abstract;
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; virtual; abstract;
    function MOVE(const Key: string; DB: Integer): Boolean; virtual; abstract;
    function OBJECTREFCOUNT(const key: string): Integer; virtual; abstract;
    function OBJECTENCODING(const key: string): string; virtual; abstract;
    function OBJECTIDLETIME(const key: string): Integer; virtual; abstract;
    function PERSIST(const Key: string): Boolean; virtual; abstract;
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean; virtual; abstract;
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; virtual; abstract;
    function TTL(const Key: string): Int64; virtual; abstract;
    function PTTL(const Key: string): Int64; virtual; abstract;
    function RANDOMKEY: string; virtual; abstract;
    function RENAME(const Key, NewKey: string): Boolean; virtual; abstract;
    function RENAMENX(const Key, NewKey: string): Boolean; virtual; abstract;
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; virtual; abstract;
    function SORT(const Key, Param: string; Value: TStrings): Integer; virtual; abstract;
    function _TYPE(const Key: string): TCnRedisDataType; virtual; abstract;
    function SCAN(cursor: Integer; MATCH: string; Count: Integer):
      TCnRedisMultiBulk; virtual; abstract;

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; virtual; abstract;
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; virtual; abstract;
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; virtual; abstract;
    function DECR(const Key: string): Int64; virtual; abstract;
    function DECRBY(const Key: string; Decrement: Int64): Int64; virtual; abstract;
    function GETRANGE(const Key: string; Start, Stop: Integer): string; virtual; abstract;
    function GET(const Key: string): string; virtual; abstract;
    function GETBIT(const Key: string; offset: Integer): Integer; virtual; abstract;
    function _SET(const Key, Value: string; EXSecond: Cardinal; Exist: Byte):
      Boolean; virtual; abstract;
    function GETSET(const Key, Value: string): string; virtual; abstract;
    function INCR(const Key: string): Int64; virtual; abstract;
    function INCRBY(const Key: string; Increment: Int64): Int64; virtual; abstract;
    function INCRBYFLOAT(const Key: string; Increment: Single): Single; virtual; abstract;
    function MGET(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function MSET(const KVs: string): Boolean; virtual; abstract;
    function MSETNX(const KVs: string): Boolean; virtual; abstract;
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;
      virtual; abstract;
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; virtual;
      abstract;
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; virtual; abstract;
    function SETNX(const Key, Value: string): Boolean; virtual; abstract;
    function SETRANGE(const Key, value: string; Offset: Integer): Integer;
      virtual; abstract;
    function STRLEN(const Key: string): Integer; virtual; abstract;

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; virtual; abstract;
    function HEXISTS(const Key, Field: string): Boolean; virtual; abstract;
    function HGET(const Key, Field: string): string; virtual; abstract;
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray):
      Integer; virtual; abstract;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; virtual;
      abstract;
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single;
      virtual; abstract;
    function HKEYS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function HLEN(const Key: string): Integer; virtual; abstract;
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; virtual;
      abstract;
    function HMSET(const Key, fvs: string): Boolean; virtual; abstract;
    function HSET(const Key, Field, Value: string): Boolean; virtual; abstract;
    function HSETNX(const Key, Field, Value: string): Boolean; virtual; abstract;
    function HVALS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
      virtual; abstract;
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
      virtual; abstract;
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; virtual; abstract;
    function LINDEX(const Key: string; index: Integer): string; virtual; abstract;
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; virtual; abstract;
    function LLEN(const Key: string): Integer; virtual; abstract;
    function LPOP(const Key: string): string; virtual; abstract;
    function LPUSH(const Key, values: string): Integer; virtual; abstract;
    function LPUSHX(const Key, Value: string): Integer; virtual; abstract;
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; virtual; abstract;
    function LREM(const Key, Value: string; Count: Integer): Integer; virtual; abstract;
    function LSET(const Key, Value: string; Index: Integer): Boolean; virtual; abstract;
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean; virtual; abstract;
    function RPOP(const Key: string): string; virtual; abstract;
    function RPOPLPUSH(const source, Destination: string): string; virtual; abstract;
    function RPUSH(const Key, values: string): Integer; virtual; abstract;
    function RPUSHX(const Key, Value: string): Integer; virtual; abstract;

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; virtual; abstract;
    function SCARD(const Key: string): Integer; virtual; abstract;
    function SDIFF(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SDIFFSTORE(const Keys, Destination: string): Integer; virtual; abstract;
    function SINTER(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SINTERSTORE(const Destination, Keys: string): Integer; virtual; abstract;
    function SISMEMBER(const Key, Member: string): Boolean; virtual; abstract;
    function SMEMBERS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function SMOVE(const source, Destination, Member: string): Boolean; virtual; abstract;
    function SPOP(const Key: string): string; virtual; abstract;
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; virtual; abstract;
    function SREM(const Key, Members: string): Integer; virtual; abstract;
    function SUNION(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SUNIONSTORE(const Keys, Destination: string): Integer; virtual; abstract;
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer; virtual; abstract;
    function ZCARD(const Key: string): Integer; virtual; abstract;
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer; virtual; abstract;
    function ZINCRBY(const Key, Member: string; Increment: Single): Single;
      virtual; abstract;
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; virtual; abstract;
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZRANK(const Key, Member: string): Integer; virtual; abstract;
    function ZREM(const Key, Members: string): Integer; virtual; abstract;
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer;
      virtual; abstract;
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; virtual; abstract;
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZREVRANK(const Key, Member: string): Integer; virtual; abstract;
    function ZSCORE(const Key, Member: string): string; virtual; abstract;
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; virtual; abstract;
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; virtual; abstract;
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;
      virtual; abstract;
    function PUBLISH(const Channel, message: string): Integer; virtual; abstract;
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      virtual; abstract;
    function PUNSUBSCRIBE(const patterns: string): string; virtual; abstract;
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer;
      virtual; abstract;
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;
      virtual; abstract;

    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; virtual; abstract;
    function EXEC(Value: TStrings): Integer; virtual; abstract;
    function MULTI: Boolean; virtual; abstract;
    function UNWATCH: Boolean; virtual; abstract;
    function WATCH(const Keys: string): Boolean; virtual; abstract;

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer;
      virtual; abstract;
    function EVALSHA(const Sha1, Keys, Arg: string): string; virtual; abstract;
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer;
      virtual; abstract;
    function SCRIPTFLUSH: Boolean; virtual; abstract;
    function SCRIPTKILL: Boolean; virtual; abstract;
    function SCRIPTLOAD(const Script: string): string; virtual; abstract;

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; virtual; abstract;
    function ECHO(const Msg: string): string; virtual; abstract;
    function PING: Boolean; virtual; abstract;
    procedure QUIT; virtual; abstract;
    function SELECT(DB: Integer): Boolean; virtual; abstract;

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; virtual; abstract;
    function BGSAVE: string; virtual; abstract;
    function CLIENTGETNAME: string; virtual; abstract;
    function CLIENTKILL(const IP: string; Port: Word): Boolean; virtual; abstract;
    function CLIENTLIST(Value: TStrings): Integer; virtual; abstract;
    function CLIENTSETNAME(const Name: string): Boolean; virtual; abstract;
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; virtual;
      abstract;
    function CONFIGRESETSTAT: Boolean; virtual; abstract;
    function CONFIGREWRITE: Boolean; virtual; abstract;
    function CONFIGSET(const Parameter, Value: string): Boolean; virtual; abstract;
    function DBSIZE: Int64; virtual; abstract;
    function DEBUGOBJECT(const Key: string): string; virtual; abstract;
    procedure DEBUGSEGFAULT; virtual; abstract;
    function FLUSHALL: Boolean; virtual; abstract;
    function FLUSHDB: Boolean; virtual; abstract;
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer;
      virtual; abstract;
    function LASTSAVE: Int64; virtual; abstract;
    function MONITOR: Boolean; virtual; abstract;
    function PSYNC(const MASTER_RUN_ID: string; Offset: Integer): string;
      virtual; abstract;
    function SAVE: Boolean; virtual; abstract;
    function SHUTDOWN: string; virtual; abstract;
    function SLAVEOF(const Host: string; Port: Word): Boolean; virtual; abstract;
    function SLOWLOGLEN(const Parameter: string): Integer; virtual; abstract;
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; virtual; abstract;
    function SLOWLOGRESET: Boolean; virtual; abstract;
    function SYNC: string; virtual; abstract;
    function TIME: TCnRedisKeyValue; virtual; abstract;
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

  TCnRedisClient = class(TCnRedisProtocol)
  public
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
    {$REGION '==========Redis Command=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; override;
    function DUMP(const Key: string): string; override;//*
    function EXISTS(const Key: string): Boolean; override;
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; override;
    function EXPIREAT(const Key: string; UnixTime: Int64): Boolean; override;
    function KEYS(const pattern: string; Value: TStrings): Integer; override;
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; override;//*
    function MOVE(const Key: string; DB: Integer): Boolean; override;
    function OBJECTREFCOUNT(const key: string): Integer; override;
    function OBJECTENCODING(const key: string): string; override;
    function OBJECTIDLETIME(const key: string): Integer; override;
    function PERSIST(const Key: string): Boolean; override;
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean; override;
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; override;
    function TTL(const Key: string): Int64; override;
    function PTTL(const Key: string): Int64; override;
    function RANDOMKEY: string; override;
    function RENAME(const Key, NewKey: string): Boolean; override;
    function RENAMENX(const Key, NewKey: string): Boolean; override;
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; override;
    function SORT(const Key, Param: string; Value: TStrings): Integer; override;
    function _TYPE(const Key: string): TCnRedisDataType; override;
    function SCAN(cursor: Integer; MATCH: string = ''; Count: Integer = 0):
      TCnRedisMultiBulk; override;

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; override;
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; override;
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; override;
    function DECR(const Key: string): Int64; override;
    function DECRBY(const Key: string; Decrement: Int64): Int64; override;
    function GETRANGE(const Key: string; Start, Stop: Integer): string; override;
    function _SET(const Key, Value: string; EXSecond: Cardinal = 0; Exist: Byte = 0):
      Boolean; override;
    function GET(const Key: string): string; override;
    function GETBIT(const Key: string; offset: Integer): Integer; override;
    function GETSET(const Key, Value: string): string; override;
    function INCR(const Key: string): Int64; override;
    function INCRBY(const Key: string; Increment: Int64): Int64; override;
    function INCRBYFLOAT(const Key: string; Increment: Single): Single; override;
    function MGET(const Keys: string; Value: TStrings): Integer; override;
    function MSET(const KVs: string): Boolean; override;
    function MSETNX(const KVs: string): Boolean; override;
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean; override;
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; override;
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; override;
    function SETNX(const Key, Value: string): Boolean; override;
    function SETRANGE(const Key, value: string; Offset: Integer): Integer; override;
    function STRLEN(const Key: string): Integer; override;

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; override;
    function HEXISTS(const Key, Field: string): Boolean; override;
    function HGET(const Key, Field: string): string; override;
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray):
      Integer; override;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; override;
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single; override;
    function HKEYS(const Key: string; Value: TStrings): Integer; override;
    function HLEN(const Key: string): Integer; override;
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; override;
    function HMSET(const Key, fvs: string): Boolean; override;
    function HSET(const Key, Field, Value: string): Boolean; override;
    function HSETNX(const Key, Field, Value: string): Boolean; override;
    function HVALS(const Key: string; Value: TStrings): Integer; override;
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; override;
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; override;
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; override;
    function LINDEX(const Key: string; index: Integer): string; override;
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; override;
    function LLEN(const Key: string): Integer; override;
    function LPOP(const Key: string): string; override;
    function LPUSH(const Key, values: string): Integer; override;
    function LPUSHX(const Key, Value: string): Integer; override;
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; override;
    function LREM(const Key, Value: string; Count: Integer): Integer; override;
    function LSET(const Key, Value: string; Index: Integer): Boolean; override;
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean; override;
    function RPOP(const Key: string): string; override;
    function RPOPLPUSH(const source, Destination: string): string; override;
    function RPUSH(const Key, values: string): Integer; override;
    function RPUSHX(const Key, Value: string): Integer; override;

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; override;
    function SCARD(const Key: string): Integer; override;
    function SDIFF(const Keys: string; Value: TStrings): Integer; override;
    function SDIFFSTORE(const Keys, Destination: string): Integer; override;
    function SINTER(const Keys: string; Value: TStrings): Integer; override;
    function SINTERSTORE(const Destination, Keys: string): Integer; override;
    function SISMEMBER(const Key, Member: string): Boolean; override;
    function SMEMBERS(const Key: string; Value: TStrings): Integer; override;
    function SMOVE(const source, Destination, Member: string): Boolean; override;
    function SPOP(const Key: string): string; override;
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; override;
    function SREM(const Key, Members: string): Integer; override;
    function SUNION(const Keys: string; Value: TStrings): Integer; override;
    function SUNIONSTORE(const Keys, Destination: string): Integer; override;
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer; override;
    function ZCARD(const Key: string): Integer; override;
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer; override;
    function ZINCRBY(const Key, Member: string; Increment: Single): Single; override;
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; override;
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZRANK(const Key, Member: string): Integer; override;
    function ZREM(const Key, Members: string): Integer; override;
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer; override;
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; override;
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZREVRANK(const Key, Member: string): Integer; override;
    function ZSCORE(const Key, Member: string): string; override;
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; override;
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; override;
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer; override; //*
    function PUBLISH(const Channel, message: string): Integer; override;
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      override; //*
    function PUNSUBSCRIBE(const patterns: string): string; override;   //*
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer; override;  //*
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer; override;//*

    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; override;
    function EXEC(Value: TStrings): Integer; override;
    function MULTI: Boolean; override;
    function UNWATCH: Boolean; override;
    function WATCH(const Keys: string): Boolean; override;

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer; override;
    function EVALSHA(const Sha1, Keys, Arg: string): string; override;
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer; override;
    function SCRIPTFLUSH: Boolean; override;
    function SCRIPTKILL: Boolean; override;
    function SCRIPTLOAD(const Script: string): string; override;

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; override;
    function ECHO(const Msg: string): string; override;
    function PING: Boolean; override;
    procedure QUIT; override;
    function SELECT(DB: Integer): Boolean; override;

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; override;
    function BGSAVE: string; override;
    function CLIENTGETNAME: string; override;
    function CLIENTKILL(const IP: string; Port: Word): Boolean; override;
    function CLIENTLIST(Value: TStrings): Integer; override;
    function CLIENTSETNAME(const Name: string): Boolean; override;
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; override;
    function CONFIGRESETSTAT: Boolean; override;
    function CONFIGREWRITE: Boolean; override;
    function CONFIGSET(const Parameter, Value: string): Boolean; override;
    function DBSIZE: Int64; override;
    function DEBUGOBJECT(const Key: string): string; override;
    procedure DEBUGSEGFAULT; override;
    function FLUSHALL: Boolean; override;
    function FLUSHDB: Boolean; override;
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer; override;
    function LASTSAVE: Int64; override;
    function MONITOR: Boolean; override;
    function SAVE: Boolean; override;
    function SHUTDOWN: string; override;
    function SLAVEOF(const Host: string; Port: Word): Boolean; override;
    function SLOWLOGLEN(const Parameter: string): Integer; override;
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; override;
    function SLOWLOGRESET: Boolean; override;
    function TIME: TCnRedisKeyValue; override;
{$IFDEF IDE_EDITOR_SUPPORT_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

// �Ӷ�������ó�һ�� TCnRedisMultiBulkNode
function ObtainRedisMultiBulkNodeFromPool: TCnRedisMultiBulkNode;

// ��һ�� TCnRedisMultiBulkNode ʵ���黹�������
procedure RecycleRedisMultiBulkNode(ANode: TCnRedisMultiBulkNode);

implementation

var
  FCnRedisMultiBulkNodePool: TObjectList = nil;

{ TCnRedisClientSocket }

function TCnRedisClientSocket.Connect: Boolean;
begin
  if FSocket.Active then
    FSocket.Active := False;
  FSocket.Host := RedisHost;
  FSocket.Port := RedisPort;
  FSocket.ClientType := ctBlocking;
  FSocket.Active := True;
  Result := FSocket.Active;
end;

constructor TCnRedisClientSocket.Create;
begin
  inherited;
  FSocket := TClientSocket.Create(nil);
  FSocket.OnDisconnect := OnDisconnect;
end;

destructor TCnRedisClientSocket.Destroy;
begin
  Disconnect;
  FreeAndNil(FSocket);
  inherited;
end;

procedure TCnRedisClientSocket.Disconnect;
begin
  inherited;
  if FSocket.Active then
    FSocket.Active := False;
end;

function TCnRedisClientSocket.GetConnecting: Boolean;
begin
  Result := FConnecting;
end;

function TCnRedisClientSocket.GetHost: string;
begin
  Result := FSocket.Host;
end;

function TCnRedisClientSocket.GetPassword: string;
begin
  Result := FPassword;
end;

function TCnRedisClientSocket.GetPort: Word;
begin
  Result := FSocket.Port;
end;

procedure TCnRedisClientSocket.OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnecting := False;
end;

function TCnRedisClientSocket.RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer;
begin
  Result := FSocket.Socket.ReceiveBuf(Buffer^, Length);
end;

function TCnRedisClientSocket.SendBuffer(Buffer: Pointer; Length: Cardinal): Integer;
begin
  Result := FSocket.Socket.SendBuf(Buffer^, Length);
end;

procedure TCnRedisClientSocket.SetConnecting(Value: Boolean);
begin
  FConnecting := Value;
end;

procedure TCnRedisClientSocket.SetHost(Value: string);
begin
  FSocket.Host := Value;
end;

procedure TCnRedisClientSocket.SetPassword(Value: string);
begin
  FPassword := Value;
end;

procedure TCnRedisClientSocket.SetPort(Value: Word);
begin
  FSocket.Port := Value;
end;

{ TCnRedisProtocol }

function TCnRedisProtocol.Connect: Boolean;
begin
  Result := False;
  if FInterfacedSocket.Connect then
  begin
    AUTH(FInterfacedSocket.Password);
    Result := PING;
  end;
end;

//constructor TCnRedisProtocol.Create(const SocketClassName: string);
//var
//  _Class: TPersistentClass;
//begin
//  if not Assigned( GetClass(SocketClassName)) then
//     raise Exception.Create(Format('�޷�����[%s]��ʵ����',[SocketClassName]));
//  _Class:=  FindClass(SocketClassName);
//  if Assigned(_Class) then
//   begin
//      FInterfacedSocket:= TRedisAbstractSocketType(_Class).Create as IRedisSocket;
//      FInterfacedSocket.Password:='';
//      FInterfacedSocket.Connecting := False;
//      SetRecvBufferSize(cRedisDefaultBufferSize);
//   end;
//   exit;
//  FInterfacedSocket := CreateSocketOfClassName(SocketClassName);
//  FInterfacedSocket.Password := '';
//  FInterfacedSocket.Connecting := False;
//  SetRecvBufferSize(SCN_REDISD_EFAULT_BUFFER_SIZE);
//end;

constructor TCnRedisProtocol.Create(ASocketIntf: ICnRedisSocket);
begin
  if ASocketIntf = nil then
    FInterfacedSocket := TCnRedisClientSocket.Create
  else
    FInterfacedSocket := ASocketIntf;

  FInterfacedSocket.Password := '';
  FInterfacedSocket.Connecting := False;
  SetRecvBufferSize(SCN_REDISD_EFAULT_BUFFER_SIZE);
end;

function TCnRedisProtocol.CreateSocketOfClassName(const Name: string): ICnRedisSocket;
var
  _Class: TPersistentClass;
begin
  Result := nil;
  if not Assigned(GetClass(Name)) then
    raise Exception.Create(Format('δע��Socket����:[%s]��', [Name]));
  _Class := FindClass(Name);
  if Assigned(_Class) then
  begin
    Result := TCnRedisAbstractSocketType(_Class).Create as ICnRedisSocket;
  end
  else
    raise Exception.Create(Format('����[%s]ʵ��ʧ�ܣ�', [Name]));
end;

function TCnRedisProtocol.Deserialize(Response: Pointer; Length: Integer;
  RespNode: TCnRedisMultiBulkNode): Boolean;
var
  _Count: Integer;
  _Curr, _Pos: DWORD;
  I: Integer;
  _Value: AnsiString;
  CurNode: TCnRedisMultiBulkNode;

  function _ParseCount(p: DWORD; var Count: Integer): DWORD;
  var
    _Value: AnsiString;
  begin
    Count := 0;
    Result := 0;
    while PWord(p + Result)^ <> $0A0D do
      if Result < 4 then
        Inc(Result)
      else
        Exit;
    SetLength(_Value, Result - 1);
    CopyMemory(@_Value[1], Pointer(p + 1), Result - 1);
    Count := StrToInt(string(_Value));
    Inc(Result, 2);
    if Count < 0 then
      Count := 0;
  end;

begin
  Result := False;
  _Curr := 0;
  _Pos := 0;
  CurNode := RespNode;
  CurNode.ChangeMultiBulksSize(0);

  CurNode.Parent := nil;
  CurNode.CurrIndex := 0;
  if (Response = nil) or (Length < 2) then
    Exit;
  try
    while _Curr < DWORD(Length) do
    begin
      case PAnsiChar(DWORD(Response) + _Curr)^ of
        Char('*'):
          begin
            if CurNode.MultiBulkRefs.Count > 0 then
            begin
              CurNode := TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[CurNode.CurrIndex]);
              CurNode.CurrIndex := 0;
            end;
            _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
            CurNode.ChangeMultiBulksSize(_Count);

            if _Count = 0 then
              CurNode.Value := '(empty)';

            for I := 0 to _Count - 1 do
            begin
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[I]).Parent := CurNode;
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[I]).CurrIndex := 0;
            end;
          end;

        Char('$'):
          begin
            if CurNode.MultiBulkRefs.Count = 0 then
            begin
              _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
              SetLength(_Value, _Count);
              CopyMemory(@_Value[1], Pointer(DWORD(Response) + _Curr + _Pos), _Count);
              CurNode.Value := string(_Value);
            end
            else
            begin
              if CurNode.CurrIndex > CurNode.MultiBulkRefs.Count - 1 then
              begin
                CurNode := CurNode.Parent;
                Inc(CurNode.FCurrIndex);
              end;
              _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
              SetLength(_Value, _Count);
              CopyMemory(@_Value[1], Pointer(DWORD(Response) + _Curr + _Pos), _Count);
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[CurNode.CurrIndex]).Value := string(_Value);
              Inc(CurNode.FCurrIndex);
            end;
            Inc(_Pos, _Count + 2);
          end;

        Char('-'), Char('+'), Char(':'):
          begin
            CurNode.Value := string(PAnsiChar(DWORD(Response) + 1));
            SetLength(CurNode.FValue, Length - 3);  //��ȥ���/r/n 2�ַ�
            Break;
          end;
      end;
      Inc(_Curr, _Pos);
    end;
    Result := True;
  except
  end;
end;

destructor TCnRedisProtocol.Destroy;
begin
  if Connecting then
    FInterfacedSocket.Disconnect;
  SetRecvBufferSize(0);
  FInterfacedSocket := nil;
  inherited;
end;

procedure TCnRedisProtocol.Disconnect;
begin
  if Connecting then
    QUIT;
  FInterfacedSocket.Connecting := False;
end;

function TCnRedisProtocol.GetConnecting: Boolean;
begin
  Result := FInterfacedSocket.Connecting;
end;

function TCnRedisProtocol.GetRedisDataTypeOfString(const Value: string): TCnRedisDataType;
var
  _pType: PTypeData;
  I: Integer;
begin
  Result := rdt_none;
  _pType := GetTypeData(TypeInfo(TCnRedisDataType));
  for I := _pType.MinValue to _pType.MaxValue do
  begin
    if GetEnumName(TypeInfo(TCnRedisDataType), I) = 'rdt_' + Value then
      Result := TCnRedisDataType(I);
  end;
end;

procedure TCnRedisProtocol.PipelineBegin;
begin
  FPipelineMode := True;
  FPipelineBuffer := '';
end;

function TCnRedisProtocol.PipelineEnd(Recv: TCnRedisMultiBulk): Boolean;
var
  Len: integer;
begin
  Result := False;
  FPipelineMode := False;
  FInterfacedSocket.SendBuffer(@FPipelineBuffer[1], Length(FPipelineBuffer));
  Len := FInterfacedSocket.RecvBuffer(FRecvBuffer, Length(FRecvBuffer));
  if (Len < 1) or (Len > Integer(Length(Self.FRecvBuffer))) then
    Exit;
  Result := Deserialize(FRecvBuffer, Len, Recv);
end;

function TCnRedisProtocol.SendAndReceive(const Send: string; Recv:
  TCnRedisMultiBulkNode; Force: Boolean): Boolean;
var
  _Send: AnsiString;
  Len: Integer;
begin
  Result := False;
  if (not Force) and (not Connecting) then
    FInterfacedSocket.Connecting := Connect;
  Len := Serialize(Send, _Send);
  if Len < 1 then
    Exit;
  if FPipelineMode and (not Force) then
  begin
    FPipelineBuffer := FPipelineBuffer + _Send;
    Exit;
  end;
  FInterfacedSocket.SendBuffer(@_Send[1], Len);
  Len := FInterfacedSocket.RecvBuffer(FRecvBuffer, Length(FRecvBuffer));
  if (Len < 1) or (Len > Integer(Length(Self.FRecvBuffer))) then
    Exit;
  Result := Deserialize(FRecvBuffer, Len, Recv);
end;

function TCnRedisProtocol.Serialize(const Value: string; var Serial: AnsiString): Integer;
var
  I: Integer;
  _ParamList: TStrings;
begin
  Result := 0;
  Serial := '';
  if Length(Value) <= 0 then
    Exit;
  _ParamList := TStringList.Create;
  try
    _ParamList.Delimiter := ' ';
    _ParamList.DelimitedText := Value;
    Serial := Ansistring('*' + IntToStr(_ParamList.Count) + SCN_REDIS_CRLF);
    for I := 0 to _ParamList.Count - 1 do
    begin
      Serial := Serial + '$' + Ansistring(IntToStr(Length(_ParamList.Strings[I]))
        + SCN_REDIS_CRLF + _ParamList.Strings[I] + SCN_REDIS_CRLF);
    end;
    Result := Length(Serial);
  finally
    FreeAndNil(_ParamList);
  end;
end;

procedure TCnRedisProtocol.SetRecvBufferSize(value: Cardinal);
begin
  if Connecting then
    Exit;
  if value = Cardinal(Length(FRecvBuffer)) then
    Exit;
  SetLength(FRecvBuffer, value);
end;

procedure TCnRedisProtocol.SetRedisServer(const Host: string; Port: Word; const
  Password: string);
begin
  FInterfacedSocket.RedisHost := Host;
  FInterfacedSocket.RedisPort := Port;
  FInterfacedSocket.Password := Password;
end;

{ TCnRedisClient }

function TCnRedisClient.BGREWRITEAOF: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BGREWRITEAOF', []), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BGSAVE: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BGSAVE', []), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BITCOUNT(const Key: string; Start: Integer; Stop:
  Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Str: string;
begin
  Result := 0;
  if (Start = 0) and (Stop = 0) then
    _Str := Format('BITCOUNT %s ', [Key])
  else
    _Str := Format('BITCOUNT %s %d %d', [Key, Start, Stop]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Str, Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BITOP(operation: TCnRedisOperation; const DestKey, Keys:
  string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BITOP %s %s %s', [SCN_REDIS_OPERATION[Integer(operation)],
      DestKey, Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BLPOP %s %d', [Keys, Timeout]), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BRPOP %s %d', [Keys, Timeout]), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BRPOPLPUSH(const source, Destination: string; Timeout:
  Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BRPOPLPUSH %s %d', [source, Destination, Timeout]),
      Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTGETNAME: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CLIENT GETNAME', []), Reply) then
    Result := Reply.Value;
end;

function TCnRedisClient.CLIENTKILL(const IP: string; Port: Word): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CLIENT KILL %s:%d', [IP, Port]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTLIST(Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CLIENT LIST', []), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTSETNAME(const Name: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CLIENT SETNAME %s ', [Name]), Reply) then
    Result := UpperCase(Reply.Value) = 'OK';
end;

function TCnRedisClient.CONFIGGET(const Parameters: string): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
begin
  SetLength(Result, 0);
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIG GET %s', [Parameters]), Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CONFIGRESETSTAT: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CONFIG RESETSTAT ', []), Reply) then
    Result := UpperCase(Reply.Value) = 'OK';
end;

function TCnRedisClient.CONFIGREWRITE: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIG REWRITE ', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CONFIGSET(const Parameter, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIGSET %s %s', [Parameter, Value]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DISCARD: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DISCARD', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DUMP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DUMP %s', [Key]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ECHO(const Msg: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ECHO %s', [Msg]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Count: Integer;
  _Tlist: TStrings;
begin
  Result := 0;
  _Tlist := TStringList.Create;
  try
    _Tlist.DelimitedText := Keys;
    _Tlist.Delimiter := ' ';
    _Count := _Tlist.Count;
    _Tlist.DelimitedText := Arg;
    _Tlist.Delimiter := ' ';
    if _Count <> _Tlist.Count then
      Exit;
  finally
    FreeAndNil(_Tlist);
  end;

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EVAL %s %d %s %s', [Script, _Count, Keys, Arg]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EVALSHA(const Sha1, Keys, Arg: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EVALSHA %s %s %s', [Sha1, Keys, Arg]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXEC(Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXEC', []), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXISTS(const Key: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXISTS %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXPIRE(const Key: string; Seconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXPIRE %s %d', [Key, Seconds]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXPIREAT(const Key: string; UnixTime: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXPIREAT %s %d', [Key, UnixTime]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.FLUSHALL: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('FLUSHALL', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.FLUSHDB: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('FLUSHDB', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GET(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '(nil)';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('Get %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETBIT(const Key: string; offset: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETBIT %s %d', [Key, offset]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETRANGE(const Key: string; Start, Stop: Integer): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETRANGE %s %d %d', [Key, Start, Stop]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETSET(const Key, Value: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETSET %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HDEL(const Key, Fields: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HDEL %s %s', [Key, Fields]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HEXISTS(const Key, Field: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HEXISTS %s %s', [Key, Field]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HGET(const Key, Field: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HGET %s %s', [Key, Field]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HGETALL(const Key: string; var Value:
  TCnRedisKeyValueArray): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  SetLength(Value, 0);
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HGETALL %s', [Key]), Reply) then
    begin
      SetLength(Value, (Reply.MultiBulkRefs.Count) div 2);
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Value[Result].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[Result * 2]).Value;
        Value[Result].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[Result * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HINCRBY(const Key, Field: string; Increment: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HINCRBY %s %s %d', [Key, Field, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HINCRBYFLOAT %s %s %8.4f', [Key, Field, Increment]),
      Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HKEYS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HKEYS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HMGET(const Key, Fields: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HMGET %s %s', [Key, Fields]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HMSET(const Key, fvs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HMSET %s', [Key, fvs]), Reply) then
      if Reply.Value <> '' then
        Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := nil;
  _Cmd := Format('HSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  Reply.Value := '(nil)';
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    if Result = nil then
      RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSET(const Key, Field, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HSET %s %s %s', [Key, Field, Value]), Reply) then
      if Reply.Value <> '' then
        Result := (Reply.Value = '1') or (Reply.Value = '0');
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSETNX(const Key, Field, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HSETNX %s %s %s', [Key, Field, Value]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HVALS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HVALS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCR(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCR %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCRBY(const Key: string; Increment: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCRBY %s %d', [Key, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCRBYFLOAT(const Key: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCRBY %s %8.4f', [Key, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INFO %s', [SCN_REDIS_INFO_SECTION_NAME[Integer(Section)]]),
      Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.Keys(const pattern: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('KEYS %s', [pattern]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LASTSAVE: Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LASTSAVE', []), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LINDEX(const Key: string; index: Integer): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LINDEX %s %d', [Key, index]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LINSERT(const Key, Pivot, Value: string; IsBEFORE:
  Boolean): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := 0;
  if IsBEFORE then
    _Cmd := Format('LINSERT %s BEFORE', [Key])
  else
    _Cmd := Format('LINSERT %s AFTER', [Key]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('%s %s %s', [_Cmd, Pivot, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPOP %s', [Key]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPUSH(const Key, values: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPUSH %s %s', [Key, values]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPUSHX(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPUSHX %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LRANGE(const Key: string; Start, Stop: Integer; Value:
  TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LRANGE %s %d %d', [Key, Start, Stop]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LREM(const Key, Value: string; Count: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LREM %s %s %d', [Key, Value, Count]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LSET(const Key, Value: string; Index: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LSET %s %s %d', [Key, Value, Index]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LTRIM(const Key: string; Start, Stop: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LTRIM %s %d %d', [Key, Start, Stop]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MGET(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MGET %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MIGRATE(const Host: string; Port: Word; const Key:
  string; DestDB, Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := False;
  _Cmd := Format('MIGRATE %s %d %s %d %d ', [Host, Port, Key, DestDB, Timeout]);
  if IsCopy then
    _Cmd := Format('%s COPY', [_Cmd]);
  if IsReplace then
    _Cmd := Format('%s REPLACE', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MONITOR: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MONITOR', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MOVE(const Key: string; DB: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MOVE %s %d', [Key, DB]), Reply) then
      Result := UpperCase(Reply.Value) = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MSET(const KVs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    Result := SendAndReceive(Format('MSET %s', [KVs]), Reply);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MSETNX(const KVs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MSETNX %s', [KVs]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MULTI: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MULTI', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PERSIST(const Key: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PERSIST %s', [Key]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PEXPIRE %s %d', [Key, MilliSeconds]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PEXPIREAT(const Key: string; UnixTime: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PEXPIREAT %s %d', [Key, UnixTime]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PING: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive('PING', Reply, True) then
      Result := UpperCase(Reply.Value) = 'PONG';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.APPEND(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('APPEND %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTENCODING(const key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT ENCODING %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTIDLETIME(const key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT IDLETIME %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTREFCOUNT(const key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT REFCOUNT %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PSETEX %s %s %d', [Key, Value, MilliSeconds]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;
begin
  Result := 0;
end;

function TCnRedisClient.PTTL(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := -3;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PTTL %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PUBLISH(const Channel, message: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PUBLISH %s %s', [Channel, message]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PUBSUB(const Command, Arguments: string; Value: TStrings):
  Integer;
begin

end;

function TCnRedisClient.PUNSUBSCRIBE(const patterns: string): string;
begin

end;

function TCnRedisClient.AUTH(const Password: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('AUTH %s', [Password]), Reply, True) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

procedure TCnRedisClient.QUIT;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    SendAndReceive('QUIT', Reply, True);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RANDOMKEY: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RANDOMKEY', []), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RENAME(const Key, NewKey: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RENAME %s %s', [Key, NewKey]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RENAMENX(const Key, NewKey: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RENAMENX %s %s', [Key, NewKey]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RESTORE(const Key: string; ttl: Cardinal; const SValue:
  string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RESTORE %s %d %s', [Key, ttl, SValue]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPOP %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPOPLPUSH(const source, Destination: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPOPLPUSH %s %s', [source, Destination]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPUSH(const Key, values: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPUSH %s %s', [Key, values]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPUSHX(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPUSHX %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient._TYPE(const Key: string): TCnRedisDataType;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := rdt_none;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TYPE %s', [Key]), Reply) then
      Result := GetRedisDataTypeOfString(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SADD(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SADD %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SAVE: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SAVE', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCAN(cursor: Integer; MATCH: string; Count: Integer):
  TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  _Cmd := Format('SCAN %d', [cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Result := ObtainRedisMultiBulkNodeFromPool;
  Result.Value := '(nil)';
  if SendAndReceive(_Cmd, Result) then
    Result.Value := '(Multi-Bulk)';
end;

function TCnRedisClient.SCARD(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCARD %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT EXISTS %s', [Scripts]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTFLUSH: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT FLUSH', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTKILL: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT KILL', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTLOAD(const Script: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT LOAD %s', [Script]), Reply, False) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SDIFF(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SDIFF %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SDIFFSTORE(const Keys, Destination: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SDIFFSTORE %s', [Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SELECT(DB: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SELECT %d', [DB]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient._SET(const Key, Value: string; EXSecond: Cardinal; Exist:
  Byte): Boolean;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := False;
  case Exist of
    0:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s"', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d', [Key, Value, EXSecond]);
      end;
    1:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s" XX', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d XX', [Key, Value, EXSecond]);
      end;
    2:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s" NX', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d NX', [Key, Value, EXSecond]);
      end;
  end;

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DBSIZE: Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DBSIZE', []), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DEBUGOBJECT(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DEBUG OBJECT %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

procedure TCnRedisClient.DEBUGSEGFAULT;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    SendAndReceive(Format('DEBUG SEGFAULT', []), Reply);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DECR(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DECR %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DECRBY(const Key: string; Decrement: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DECR %s %d', [Key, Decrement]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETBIT(const Key: string; Offset, Value: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETBIT %s %d %d', [Key, Offset, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETEX(const Key, Value: string; Seconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETEX %s %s %d', [Key, Value, Seconds]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETNX(const Key, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETNX %s %s', [Key, Value]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETRANGE(const Key, value: string; Offset: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETRANGE %s %d %d', [Key, Offset, value]), Reply) then
      if Reply.value <> '' then
        Result := StrToInt(Reply.value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SHUTDOWN: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SHUTDOWN', []), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SINTER(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SINTER %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SINTERSTORE(const Destination, Keys: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SINTERSTORE %s %s', [Destination, Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SISMEMBER(const Key, Member: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SISMEMBER %s %s', [Key, Member]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLAVEOF(const Host: string; Port: Word): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLAVEOF %s %d', [Host, Port]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := nil;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  Reply.Value := '(nil)';
  try
    if SendAndReceive(Format('SLOWLOGGET %s', [Parameter]), Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    if Result = nil then
      RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGLEN(const Parameter: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLOWLOG LEN %s', [Parameter]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGRESET: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLOWLOGRESET', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SMEMBERS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SMEMBERS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SMOVE(const source, Destination, Member: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SMOVE %s %s %s', [source, Destination, Member]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SORT(const Key, Param: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Value.Clear;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SORT %s %s', [Key, Param]), Reply) then
    begin
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value)
      else
      begin
        for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SPOP %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SRANDMEMBER(const Key: string; Count: Integer; Value:
  TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := 0;
  if (Count = 0) or (Count = 1) then
    _Cmd := Format('SRANDMEMBER %s', [Key])
  else
    Format('SRANDMEMBER %s %d', [Key, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SREM(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SREM %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := nil;
  _Cmd := Format('SSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  Reply.Value := '(nil)';
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    if Result = nil then
      RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.STRLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('STRLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SUBSCRIBE(const Channels: string; Value: TStrings): Integer;
begin
  Result := -1;
end;

function TCnRedisClient.SUNION(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SUNION %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SUNIONSTORE(const Keys, Destination: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SUNIONSTORE %s %s', [Keys, Destination]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.TIME: TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TIME', []), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ttl(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := -3;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TTL %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;
begin
  Result := -1;
end;

function TCnRedisClient.UNWATCH: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('UNWATCH', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.WATCH(const Keys: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('WATCH %s', [Keys]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZADD(const Key, ScoreMembers: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZADD %s %s', [Key, ScoreMembers]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZCARD(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZCARD %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZCOUNT(const Key: string; Min, Max: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZCOUNT %s %d %d', [Key, Min, Max]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZINCRBY(const Key, Member: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZINCRBY %s %s %8.4f', [Key, Member, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZINTERSTORE(const Destination, Keys: string; KeyCount:
  Integer; WEIGHTS, AGGREGATE: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZINTERSTORE %s %s %d %s %s', [Destination, Keys,
      KeyCount, WEIGHTS, AGGREGATE]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANGE(const Key: string; Start, Stop: Integer;
  IsWITHSCORES: Boolean): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZRANGE %s %d %d', [Key, Start, Stop]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANK(const Key, Member: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('ZRANK %s %s', [Key, Member]), Reply) then
    if Reply.Value <> '' then
      Result := StrToInt(Reply.Value);
end;

function TCnRedisClient.ZREM(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREM %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREMRANGEBYRANK %s %d %d', [Key, Start, Stop]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREMRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANGE(const Key: string; Start, Stop: Integer;
  IsWITHSCORES: Boolean): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREVRANGE %s %d %d', [Key, Start, Stop]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREVRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANK(const Key, Member: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREVRANK %s %s', [Key, Member]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := nil;
  _Cmd := Format('ZSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  Reply.Value := '(nil)';
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    if Result = nil then
      RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZSCORE(const Key, Member: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '0';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('ZSCORE %s %s', [Key, Member]), Reply) then
    if Reply.Value <> '' then
      Result := Reply.Value;
end;

function TCnRedisClient.ZUNIONSTORE(const Destination, Keys: string; KeyCount:
  Integer; WEIGHTS, AGGREGATE: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZUNIONSTORE %s %s %d %s %s', [Destination, Keys,
      KeyCount, WEIGHTS, AGGREGATE]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DEL(const Keys: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DEL %s', [Keys]), Reply) then
      Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function ObtainRedisMultiBulkNodeFromPool: TCnRedisMultiBulkNode;
begin
  if (FCnRedisMultiBulkNodePool = nil) or (FCnRedisMultiBulkNodePool.Count = 0) then
  begin
    Result := TCnRedisMultiBulkNode.Create;
  end
  else
  begin
    Result := TCnRedisMultiBulkNode(FCnRedisMultiBulkNodePool.Items[FCnRedisMultiBulkNodePool.Count - 1]);
    FCnRedisMultiBulkNodePool.Delete(FCnRedisMultiBulkNodePool.Count - 1);
  end;
end;

procedure RecycleRedisMultiBulkNode(ANode: TCnRedisMultiBulkNode);
begin
  if ANode <> nil then
  begin
    if FCnRedisMultiBulkNodePool = nil then
      FCnRedisMultiBulkNodePool := TObjectList.Create(False);
    FCnRedisMultiBulkNodePool.Add(ANode);
  end;
end;

procedure FreeRedisMultiBulkNodePool;
var
  I: Integer;
begin
  if FCnRedisMultiBulkNodePool = nil then
    Exit;

  for I := 0 to FCnRedisMultiBulkNodePool.Count - 1 do
    TCnRedisMultiBulkNode(FCnRedisMultiBulkNodePool[I]).Free;
  FreeAndNil(FCnRedisMultiBulkNodePool);
end;

{ TCnRedisMultiBulkNode }

procedure TCnRedisMultiBulkNode.ChangeMultiBulksSize(NewCount: Integer);
var
  I: Integer;
begin
  if NewCount <= 0 then
  begin
    for I := 0 to FMultiBulkRefs.Count - 1 do
      RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
    FMultiBulkRefs.Clear;
  end
  else if NewCount > FMultiBulkRefs.Count then
  begin
    for I := 0 to NewCount - FMultiBulkRefs.Count do
      FMultiBulkRefs.Add(ObtainRedisMultiBulkNodeFromPool);
  end
  else if NewCount < FMultiBulkRefs.Count then
  begin
    for I := FMultiBulkRefs.Count - 1 downto NewCount - 1 do
    begin
      RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
      FMultiBulkRefs.Delete(I);
    end;
  end;
end;

constructor TCnRedisMultiBulkNode.Create;
begin
  FMultiBulkRefs := TObjectList.Create;
end;

destructor TCnRedisMultiBulkNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to FMultiBulkRefs.Count - 1 do
    RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
  FMultiBulkRefs.Free;
  inherited;
end;

initialization

finalization
  FreeRedisMultiBulkNodePool;

end.

