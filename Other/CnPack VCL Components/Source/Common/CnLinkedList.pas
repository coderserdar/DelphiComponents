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

unit CnLinkedList;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�˫�������Listʵ�ֵ�Ԫ
* ��Ԫ���ߣ��͹�ķ��
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��    ע��2010.01.20
*               ���벿���¹���
*           2008.05.23
*               ��ֲ��Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

{$IFNDEF COMPILER6_UP}
const
  sLineBreak = #13#10;
{$ENDIF}

type
{$IFNDEF COMPILER6_UP}
  PCardinal = ^Cardinal;
{$ENDIF}

  TCnLinkedListNotification = (lnAdded, lnExtracted, lnDeleted);

  PCnLinkedNode = ^TCnLinkedNode;
  TCnLinkedNode = packed record
  {* ˫������ڵ�ṹ}
    Previous: PCnLinkedNode; // ��һ�ڵ�
    Code: Pointer; // ���ڵ������
    Next: PCnLinkedNode; // ��һ�ڵ�
  end;

  PCnPAnsiCharItem = ^TCnPAnsiCharItem;
  TCnPAnsiCharItem = packed record
    AString: PAnsiChar;
    AObject: TObject;
  end;

  PCnAnsiStringItem = ^TCnAnsiStringItem;
  TCnAnsiStringItem = packed record
    AString: AnsiString;
    AObject: TObject;
  end;

  PCnPWideCharItem = ^TCnPWideCharItem;
  TCnPWideCharItem = packed record
    AString: PWideChar;
    AObject: TObject;
  end;

  PCnWideStringItem = ^TCnWideStringItem;
  TCnWideStringItem = packed record
    AString: WideString;
    AObject: TObject;
  end;

type
  TCnLinkedListEvent = procedure(Sender: TObject; AItem: Pointer) of object;
  TCnLinkedObjectListEvent = procedure(Sender: TObject; AObject: TObject) of object;
  TCnLinkedClassListEvent = procedure(Sender: TObject; AClass: TClass) of object;
  TCnLinkedPAnsiCharsEvent = procedure(Sender: TObject; AString: PAnsiChar) of object;
  TCnLinkedAnsiStringsEvent = procedure(Sender: TObject; AString: AnsiString) of object;
  TCnLinkedPWideCharsEvent = procedure(Sender: TObject; AString: PWideChar) of object;
  TCnLinkedWideStringsEvent = procedure(Sender: TObject; AString: WideString) of object;

  TCnLinkedOrderedListEvent = procedure(Sender: TObject; AItem: Pointer) of object;
  TCnLinkedOrderedObjectListEvent = procedure(Sender: TObject; AObject: TObject) of object;

type
  TCompare = function(Item1, Item2: Pointer): Integer;
  TObjectCompare = function(Object1, Object2: TObject): Integer;
  TClassCompare = function(Class1, Class2: TClass): Integer;

type
  ICnCustomLinkedListIterator = interface(IUnknown)
    ['{0380614D-F455-4FDA-8862-6E1505C0C5D4}']
  {* ˫������������ӿ�
     ʹ�÷���������

     var
       Iterator: ICnCustomLinkedListIterator;
       List: TCnLinkedList;
     begin
       ...

       Iterator := List.CreateIterator;
       while not Iterator.Eof do
       begin
         Iterator.GetItem;
         Iterator.Next;
       end;

       // Iterator �����ͷţ��Զ��ͷš�
     end;
  }
    function Bof: Boolean;
    {* �Ƿ񳬹�������ͷ}
    function Eof: Boolean;
    {* �Ƿ񳬹�������β}
    procedure First;
    {* ������ͷ}
    procedure Last;
    {* ������ĩβ}
    procedure Previous;
    {* ������ǰλ�õ���һ��}
    procedure Next;
    {* ������ǰλ�õ���һ��}
    //function GetCurrentItem: Pointer;
    {* �������ǰλ�õ�ֵ}
  end;

type
  TCnCustomLinkedList = class(TPersistent)
  private
    FFirst, FLast, FNode: PCnLinkedNode;
    FCount, FIndex: Integer;
    //FList: TList;
    FAutoClear: Boolean;

    FLock: TRTLCriticalSection;
    function GetItem(const Index: Integer): PCnLinkedNode;
    //function GetList: TList;
  protected
    procedure ClearEvent; virtual;

    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); virtual;
    procedure SetCount(const NewCount: Integer);

    function GetFirst: PCnLinkedNode; // �����׽ڵ�
    function GetLast: PCnLinkedNode; // ����β�ڵ�
    function GetBefore: PCnLinkedNode; // ����ǰһ�β��ҵĽڵ�
    function GetMiddle(const Index: Integer): PCnLinkedNode; // �����м�ڵ�

    function GetPrevious(Move: Boolean = False): PCnLinkedNode; // ������һ���ڵ�
    function GetNext(Move: Boolean = False): PCnLinkedNode; // ������һ���ڵ�

    function AddFirst(const Item: Pointer): Boolean; // ����׽ڵ�
    function AddLast(const Item: Pointer): Boolean; // ���β�ڵ�
    function AddMiddle(const Index: Integer; const Item: Pointer): Boolean; // ����м�ڵ�

    function DeleteFirst: Boolean; // ɾ���׽ڵ�
    function DeleteLast: Boolean; // ɾ��β�ڵ�
    function DeleteMiddle(const Index: Integer): Boolean; // ɾ���м�ڵ�
    function DeleteLastNode: Boolean; // ɾ�����һ���ڵ�

{* ����Ϊ��Ҫ�����ķ��������� }
    function Add(const Item: Pointer): Integer;
    {* ���һ��Ŀ������β}

    function First: Pointer;
    {* �������ͷ}
    function Last: Pointer;
    {* �������β}

    // ���������ƶ�������Ӱ�쵽�ڲ���ǰָ�룬���̷߳���ʱ��Ҫ����������
    function Previous: Pointer;
    {* �������ڲ��ĵ�ǰָ���ƶ�����һ��, �糬��ͷ, ��ǰָ��Ϊ nil}
    function Next: Pointer;
    {* �������ڲ��ĵ�ǰָ���ƶ�����һ��, �糬��β, ��ǰָ��Ϊ nil}

    // ���ϼ����ƶ�������Ӱ�쵽�ڲ���ǰָ�룬���̷߳���ʱ��Ҫ����������

    function IndexOf(const Item: Pointer): Integer;
    {* ����һ��Ŀ��������}
    function Insert(const Index: Integer; const Item: Pointer): Integer;
    {* ��ָ��λ�ò���һ��Ŀ}

    function Extract(const Item: Pointer): Pointer;
    {* ��ȡһ�ڵ�}
    function Remove(const Item: Pointer): Integer;
    {* ɾ��һ�ڵ�}
    procedure Pack;
    {* ѹ��}

    function CreateIterator: ICnCustomLinkedListIterator;
    {* ����һ����������ӿڣ��ɹ����������˱������̷߳��ʰ�ȫ}
    property Items[Index: Integer]: Pointer read Get write Put; default;
    {* ��������ֱ�ӷ�����Ŀ, ��������ʱЧ�ʲ���}
    property Count: Integer read FCount write SetCount;
    {* ��Ŀ����}
    //property List: TList read GetList;
    {* ����һ�������� TList, ����������������}
    property AutoClear: Boolean read FAutoClear write FAutoClear;
    {* �Ƿ���ɾ���ڵ�ʱ�Զ�Dispose�ڵ�����}

    procedure QuickSort(Left, Right: Integer; Compare: Pointer);
      // ���б��ڵĴ��������򣨺��Դ�Сд��ʹ��ð�����򷽷�
      //   LeftΪ��ʼ��ţ�RightΪ��ֹ��ţ�CompareΪ�ԱȺ���
    procedure Sort(Compare: TCompare);
    {* ����}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    {* ����, �����߳��в���ʹ��}
    procedure UnLock;
    {* ����, �����߳��в���ʹ��}
    function Clear: Integer;
    {* ���ȫ����Ŀ, ����ԭ����Ŀ����}
    function Delete(const Index: Integer): Integer;
    {* ɾ��ָ������������Ŀ}
    procedure Move(const CurIndex, NewIndex: Integer);
    {* �ƶ��ڵ�}
    procedure Exchange(const Index1, Index2: Integer);
    {* ����������Ŀλ��}
    procedure Assign(const AList: TCnCustomLinkedList); reintroduce; virtual;
    {* ����һ˫��������}
  published
  end;

type
  ICnLinkedListIterator = interface(ICnCustomLinkedListIterator)
    ['{EC753E86-3260-4665-9AF6-642B4D52C981}']
    function GetCurrentItem: Pointer;
    {* �������ǰλ�õ�ֵ}
  end;

type
  TCnLinkedList = class(TCnCustomLinkedList)
  private
    FOnAddItem: TCnLinkedListEvent;
    FOnExtractItem: TCnLinkedListEvent;
    FOnDeleteItem: TCnLinkedListEvent;
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(Item: Pointer); dynamic;

    procedure DoAddItem(Item: Pointer); dynamic;
    procedure DoExtractItem(Item: Pointer); dynamic;
    procedure DoDeleteItem(Item: Pointer); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create(const AAutoClear: Boolean); overload;

    function Add(const Item: Pointer): Integer;
    function First: Pointer;
    function Last: Pointer;
    function Previous: Pointer;
    function Next: Pointer;
    function IndexOf(const Item: Pointer): Integer;
    function Insert(const Index: Integer; const Item: Pointer): Integer;
    function Extract(const Item: Pointer): Pointer;
    function Remove(const Item: Pointer): Integer;
    procedure Pack;
    function CreateIterator: ICnLinkedListIterator;
    procedure Sort(Compare: TCompare);

    property Items;
    property Count;
  published
    property AutoClear;

    property OnAddItem: TCnLinkedListEvent read FOnAddItem write FOnAddItem;
    property OnExtractItem: TCnLinkedListEvent read FOnExtractItem write FOnExtractItem;
    property OnDeleteItem: TCnLinkedListEvent read FOnDeleteItem write FOnDeleteItem;
  end;

type
  ICnLinkedObjectListIterator = interface(ICnCustomLinkedListIterator)
    ['{AB6EBF29-DFA9-4C82-9416-377DB47D1640}']
    function GetCurrentItem: TObject;
    {* �������ǰλ�õ�ֵ}
  end;

type
  TCnLinkedObjectList = class(TCnCustomLinkedList)
  private
    FOnAddObject: TCnLinkedObjectListEvent;
    FOnExtractObject: TCnLinkedObjectListEvent;
    FOnDeleteObject: TCnLinkedObjectListEvent;
    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const AObject: TObject);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(AObject: TObject); dynamic;

    procedure DoAddObject(AObject: TObject); dynamic;
    procedure DoExtractObject(AObject: TObject); dynamic;
    procedure DoDeleteObject(AObject: TObject); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create(const AAutoClear: Boolean); overload;

    function Add(const AObject: TObject): Integer;
    function First: TObject;
    function Last: TObject;
    function Previous: TObject;
    function Next: TObject;
    function IndexOf(const AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer;
    function Insert(const Index: Integer; const AObject: TObject): Integer;
    function Extract(const AObject: TObject): TObject;
    function Remove(const AObject: TObject): Integer;
    procedure Pack;
    function CreateIterator: ICnLinkedObjectListIterator;
    procedure Sort(Compare: TObjectCompare);

    property Objects[Index: Integer]: TObject read GetObjects write SetObjects; default;
    property Count;
  published
    property AutoClear;

    property OnAddObject: TCnLinkedObjectListEvent read FOnAddObject write FOnAddObject;
    property OnExtractObject: TCnLinkedObjectListEvent read FOnExtractObject write FOnExtractObject;
    property OnDeleteObject: TCnLinkedObjectListEvent read FOnDeleteObject write FOnDeleteObject;
  end;

type
  ICnLinkedClassListIterator = interface(ICnCustomLinkedListIterator)
    ['{F7C947F8-4A8C-4CD8-BFBF-5CD4BA55F596}']
    function GetCurrentItem: TClass;
    {* �������ǰλ�õ�ֵ}
  end;

type
  TCnLinkedClassList = class(TCnCustomLinkedList)
  private
    FOnAddClass: TCnLinkedClassListEvent;
    FOnExtractClass: TCnLinkedClassListEvent;
    FOnDeleteClass: TCnLinkedClassListEvent;
    function GetClasses(Index: Integer): TClass;
    procedure SetClasses(Index: Integer; const AClass: TClass);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;

    procedure DoAddClass(AClass: TClass); dynamic;
    procedure DoExtractClass(AClass: TClass); dynamic;
    procedure DoDeleteClass(AClass: TClass); dynamic;

    procedure ClearEvent; override;
  public
    function Add(const AClass: TClass): Integer;
    function First: TClass;
    function Last: TClass;
    function Previous: TClass;
    function Next: TClass;
    function IndexOf(const AClass: TClass): Integer;
    function Insert(const Index: Integer; const AClass: TClass): Integer;
    function Extract(const AClass: TClass): TClass;
    function Remove(const AClass: TClass): Integer;
    procedure Pack;
    function CreateIterator: ICnLinkedClassListIterator;
    procedure Sort(Compare: TClassCompare);

    property Classes[Index: Integer]: TClass read GetClasses write SetClasses; default;
    property Count;
  published
    property OnAddClass: TCnLinkedClassListEvent read FOnAddClass write FOnAddClass;
    property OnExtractClass: TCnLinkedClassListEvent read FOnExtractClass write FOnExtractClass;
    property OnDeleteClass: TCnLinkedClassListEvent read FOnDeleteClass write FOnDeleteClass;
  end;

type
  ICnLinkedPAnsiCharsIterator = interface(ICnCustomLinkedListIterator)
    ['{1A7892D0-0529-4161-8AAE-C75F423EB608}']
    function GetCurrentString: PAnsiChar;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedPAnsiChars = class(TCnCustomLinkedList)
  private
    FText: PAnsiChar;
    FList: TCnLinkedList;
    FOnAddString: TCnLinkedPAnsiCharsEvent;
    FOnExtractString: TCnLinkedPAnsiCharsEvent;
    FOnDeleteString: TCnLinkedPAnsiCharsEvent;
    function GetStrings(Index: Integer): PAnsiChar;
    procedure SetStrings(Index: Integer; const AString: PAnsiChar);

    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const AObject: TObject);

    function GetText: PAnsiChar;
    procedure SetText(const Value: PAnsiChar);

    procedure ListDeleteItem(Sender: TObject; Item: Pointer);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(Item: PCnPAnsiCharItem); dynamic;

    procedure DoAddItem(Item: PCnPAnsiCharItem); dynamic;
    procedure DoExtractItem(Item: PCnPAnsiCharItem); dynamic;
    procedure DoDeleteItem(Item: PCnPAnsiCharItem); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create; overload;
    constructor Create(const AAutoClear: Boolean); overload;
    destructor Destroy; override;

    function Add(const AString: PAnsiChar): Integer;
    function AddObject(const AString: PAnsiChar; const AObject: TObject): Integer;
    procedure AddStrings(const AList: TCnLinkedPAnsiChars);
    function First: PAnsiChar;
    function Last: PAnsiChar;
    function Previous: PAnsiChar;
    function Next: PAnsiChar;
    function IndexOf(const AString: PAnsiChar): Integer;
    function Insert(const Index: Integer; const AString: PAnsiChar): Integer;
    function InsertObject(const Index: Integer; const AString: PAnsiChar;
      const AObject: TObject): Integer;
    function Extract(const AString: PAnsiChar): PAnsiChar;
    function Remove(const AString: PAnsiChar): Integer;
    procedure Pack;
    procedure Assign(const AList: TCnCustomLinkedList); override;
    procedure Sort;

    function CreateIterator: ICnLinkedPAnsiCharsIterator;

    property Strings[Index: Integer]: PAnsiChar read GetStrings write SetStrings; default;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Count;
    property Text: PAnsiChar read GetText write SetText;
  published
    property AutoClear;

    property OnAddString: TCnLinkedPAnsiCharsEvent read FOnAddString write FOnAddString;
    property OnExtractString: TCnLinkedPAnsiCharsEvent read FOnExtractString write FOnExtractString;
    property OnDeleteString: TCnLinkedPAnsiCharsEvent read FOnDeleteString write FOnDeleteString;
  end;

type
  ICnLinkedAnsiStringsIterator = interface(ICnCustomLinkedListIterator)
    ['{7BDE1405-E6E9-4310-9827-BD360777E650}']
    function GetCurrentString: AnsiString;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedAnsiStrings = class(TCnCustomLinkedList)
  private
    FOnAddString: TCnLinkedAnsiStringsEvent;
    FOnExtractString: TCnLinkedAnsiStringsEvent;
    FOnDeleteString: TCnLinkedAnsiStringsEvent;
    function GetStrings(Index: Integer): AnsiString;
    procedure SetStrings(Index: Integer; const AString: AnsiString);

    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const AObject: TObject);

    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(Item: PCnAnsiStringItem); dynamic;

    procedure DoAddItem(Item: PCnAnsiStringItem); dynamic;
    procedure DoExtractItem(Item: PCnAnsiStringItem); dynamic;
    procedure DoDeleteItem(Item: PCnAnsiStringItem); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create(const AAutoClear: Boolean); overload;

    function Add(const AString: AnsiString): Integer;
    function AddObject(const AString: AnsiString; const AObject: TObject): Integer;
    procedure AddStrings(const AList: TCnLinkedAnsiStrings);
    function First: AnsiString;
    function Last: AnsiString;
    function Previous: AnsiString;
    function Next: AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function Insert(const Index: Integer; const AString: AnsiString): Integer;
    function InsertObject(const Index: Integer; const AString: AnsiString;
      const AObject: TObject): Integer;
    function Extract(const AString: AnsiString): AnsiString;
    function Remove(const AString: AnsiString): Integer;
    procedure Pack;
    procedure Assign(const AList: TCnCustomLinkedList); override;
    function CreateIterator: ICnLinkedAnsiStringsIterator;
    procedure Sort;

    property Strings[Index: Integer]: AnsiString read GetStrings write SetStrings; default;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Count;
    property Text: AnsiString read GetText write SetText;
  published
    property AutoClear;

    property OnAddString: TCnLinkedAnsiStringsEvent read FOnAddString write FOnAddString;
    property OnExtractString: TCnLinkedAnsiStringsEvent read FOnExtractString write FOnExtractString;
    property OnDeleteString: TCnLinkedAnsiStringsEvent read FOnDeleteString write FOnDeleteString;
  end;

type
  ICnLinkedPWideCharsIterator = interface(ICnCustomLinkedListIterator)
    ['{D4AF0087-0679-4735-8961-F13071B0BC21}']
    function GetCurrentString: PWideChar;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedPWideChars = class(TCnCustomLinkedList)
  private
    FText: PWideChar;
    FList: TCnLinkedList;
    FOnAddString: TCnLinkedPWideCharsEvent;
    FOnExtractString: TCnLinkedPWideCharsEvent;
    FOnDeleteString: TCnLinkedPWideCharsEvent;
    function GetStrings(Index: Integer): PWideChar;
    procedure SetStrings(Index: Integer; const AString: PWideChar);

    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const AObject: TObject);

    function GetText: PWideChar;
    procedure SetText(const Value: PWideChar);

    procedure ListDeleteItem(Sender: TObject; Item: Pointer);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(Item: PCnPWideCharItem); dynamic;

    procedure DoAddItem(Item: PCnPWideCharItem); dynamic;
    procedure DoExtractItem(Item: PCnPWideCharItem); dynamic;
    procedure DoDeleteItem(Item: PCnPWideCharItem); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create; overload;
    constructor Create(const AAutoClear: Boolean); overload;
    destructor Destroy; override;

    function Add(const AString: PWideChar): Integer;
    function AddObject(const AString: PWideChar; const AObject: TObject): Integer;
    procedure AddStrings(const AList: TCnLinkedPWideChars);
    function First: PWideChar;
    function Last: PWideChar;
    function Previous: PWideChar;
    function Next: PWideChar;
    function IndexOf(const AString: PWideChar): Integer;
    function Insert(const Index: Integer; const AString: PWideChar): Integer;
    function InsertObject(const Index: Integer; const AString: PWideChar;
      const AObject: TObject): Integer;
    function Extract(const AString: PWideChar): PWideChar;
    function Remove(const AString: PWideChar): Integer;
    procedure Pack;
    procedure Assign(const AList: TCnCustomLinkedList); override;
    function CreateIterator: ICnLinkedPWideCharsIterator;
    procedure Sort;

    property Strings[Index: Integer]: PWideChar read GetStrings write SetStrings; default;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Count;
    property Text: PWideChar read GetText write SetText;
  published
    property AutoClear;

    property OnAddString: TCnLinkedPWideCharsEvent read FOnAddString write FOnAddString;
    property OnExtractString: TCnLinkedPWideCharsEvent read FOnExtractString write FOnExtractString;
    property OnDeleteString: TCnLinkedPWideCharsEvent read FOnDeleteString write FOnDeleteString;
  end;

type
  ICnLinkedWideStringsIterator = interface(ICnCustomLinkedListIterator)
    ['{C951BCFA-53B5-41AE-93CB-FBE72F85C33C}']
    function GetCurrentString: WideString;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedWideStrings = class(TCnCustomLinkedList)
  private
    FOnAddString: TCnLinkedWideStringsEvent;
    FOnExtractString: TCnLinkedWideStringsEvent;
    FOnDeleteString: TCnLinkedWideStringsEvent;
    function GetStrings(Index: Integer): WideString;
    procedure SetStrings(Index: Integer; const AString: WideString);

    function GetObjects(Index: Integer): TObject;
    procedure SetObjects(Index: Integer; const AObject: TObject);

    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure Notify(Ptr: Pointer; Action: TCnLinkedListNotification); override;
    procedure DeleteItemCode(Item: PCnWideStringItem); dynamic;

    procedure DoAddItem(Item: PCnWideStringItem); dynamic;
    procedure DoExtractItem(Item: PCnWideStringItem); dynamic;
    procedure DoDeleteItem(Item: PCnWideStringItem); dynamic;

    procedure ClearEvent; override;
  public
    constructor Create(const AAutoClear: Boolean); overload;

    function Add(const AString: WideString): Integer;
    function AddObject(const AString: WideString; const AObject: TObject): Integer;
    procedure AddStrings(const AList: TCnLinkedWideStrings);
    function First: WideString;
    function Last: WideString;
    function Previous: WideString;
    function Next: WideString;
    function IndexOf(const AString: WideString): Integer;
    function Insert(const Index: Integer; const AString: WideString): Integer;
    function InsertObject(const Index: Integer; const AString: WideString;
      const AObject: TObject): Integer;
    function Extract(const AString: WideString): WideString;
    function Remove(const AString: WideString): Integer;
    procedure Pack;
    procedure Assign(const AList: TCnCustomLinkedList); override;
    function CreateIterator: ICnLinkedWideStringsIterator;
    procedure Sort;

    property Strings[Index: Integer]: WideString read GetStrings write SetStrings; default;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Count;
    property Text: WideString read GetText write SetText;
  published
    property AutoClear;

    property OnAddString: TCnLinkedWideStringsEvent read FOnAddString write FOnAddString;
    property OnExtractString: TCnLinkedWideStringsEvent read FOnExtractString write FOnExtractString;
    property OnDeleteString: TCnLinkedWideStringsEvent read FOnDeleteString write FOnDeleteString;
  end;


type
  TCnLinkedCustomOrderedList = class(TObject) // ˳���б� ���к�ջ�Ļ��� ������˳�������������
  private
    FList: TCnLinkedList; // ˫���б���������洢�ڵ�
  protected
    procedure PushItem(AItem: Pointer); virtual; abstract;  // ���һ���ڵ㵽�б���
    function PopItem: Pointer; virtual;                     // ���б�βȡ����ɾ��һ���ڵ�
    function PeekItem: Pointer; virtual;                    // ���б�βȡ��һ���ڵ�

    property List: TCnLinkedList read FList;

  {* ����ΪҪ�����ķ��� *}
    function Push(AItem: Pointer): Pointer;                 // ���һ���ڵ㣨����/ջ��
    function Pop: Pointer;                                  // ȡ����ɾ��һ���ڵ㣨����/ջ��
    function Peek: Pointer;                                 // ȡ��һ���ڵ�
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;                                // �ܽڵ���
    function AtLeast(ACount: Integer): Boolean;             //
  end;

  TCnLinkedOrderedList = class(TCnLinkedCustomOrderedList)
  private
    FOnPush: TCnLinkedOrderedListEvent;
    FOnPop: TCnLinkedOrderedListEvent;
    procedure ClearEvent;
  protected
    procedure DoPush(AItem: Pointer); dynamic;
    procedure DoPop(AItem: Pointer); dynamic;

    procedure PushItem(AItem: Pointer); override;
    function PopItem: Pointer; override;

    property OnPush: TCnLinkedOrderedListEvent read FOnPush write FOnPush;
    property OnPop: TCnLinkedOrderedListEvent read FOnPop write FOnPop;
  public
    constructor Create;
    destructor Destroy; override;

    function Push(AItem: Pointer): Pointer;
    function Pop: Pointer;
    function Peek: Pointer;
  end;

  TCnLinkedOrderedObjectList = class(TCnLinkedCustomOrderedList)
  private
    FOnPush: TCnLinkedOrderedObjectListEvent;
    FOnPop: TCnLinkedOrderedObjectListEvent;
    procedure ClearEvent;
  protected
    procedure DoPush(AObject: TObject); dynamic;
    procedure DoPop(AObject: TObject); dynamic;

    procedure PushItem(AItem: Pointer); override;
    function PopItem: Pointer; override;

    property OnPush: TCnLinkedOrderedObjectListEvent read FOnPush write FOnPush;
    property OnPop: TCnLinkedOrderedObjectListEvent read FOnPop write FOnPop;
  public
    constructor Create;
    destructor Destroy; override;

    function Push(AObject: TObject): TObject;
    function Pop: TObject;
    function Peek: TObject;
  end;

  TCnLinkedStack = class(TCnLinkedOrderedList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

  TCnLinkedQueue = class(TCnLinkedOrderedList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

  TCnLinkedObjectStack = class(TCnLinkedOrderedObjectList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

  TCnLinkedObjectQueue = class(TCnLinkedOrderedObjectList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

function StrNewA(const Value: PAnsiChar): PAnsiChar;
procedure StrDisposeA(var Value: PAnsiChar);
function StrNewW(const Value: PWideChar): PWideChar;
procedure StrDisposeW(var Value: PWideChar);
function StrCmpA(const Value1, Value2: PAnsiChar): Integer;
function StrCmpW(const Value1, Value2: PWideChar): Integer;
{
procedure DeleteObject(var AObject: TObject);
procedure DeleteString(var AString: PAnsiChar); overload;
procedure DeleteString(var AString: AnsiString); overload;
procedure DeleteString(var AString: PWideChar); overload;
procedure DeleteString(var AString: WideString); overload;
}
implementation

function StrLenA(const Value: PAnsiChar): Cardinal;
begin
  Result := 0;
  if Value = nil then
    Exit;
  while Value[Result] <> #0 do
    Inc(Result);
end;

function StrLenW(const Value: PWideChar): Cardinal;
begin
  Result := 0;
  if Value = nil then
    Exit;
  while Value[Result] <> #0 do
    Inc(Result);
end;

function StrNewA(const Value: PAnsiChar): PAnsiChar;
var
  Len: Cardinal;
begin
  Result := nil;
  if Value = nil then
    Exit;

  Len := StrLenA(Value);
  if Len = 0 then
    Exit;

  GetMem(Result, (Len + 1) * SizeOf(AnsiChar) + SizeOf(Cardinal));
    // �����ڴ�ռ�
  PCardinal(Result)^ := (Len + 1) * SizeOf(AnsiChar) + SizeOf(Cardinal);
    // ��¼�ڴ�ռ��С
  Result := PAnsiChar(Cardinal(Result) + SizeOf(Cardinal));
    // ����������С���ڴ�ռ�
  ZeroMemory(Result, (Len + 1) * SizeOf(AnsiChar));
    // ����ڴ�����
  CopyMemory(Result, Value, Len * SizeOf(AnsiChar));
    // ��������
end;

function StrNewW(const Value: PWideChar): PWideChar;
var
  Len: Cardinal;
begin
  Result := nil;
  if Value = nil then
    Exit;

  Len := StrLenW(Value);
  if Len = 0 then
    Exit;

  GetMem(Result, (Len + 1) * SizeOf(WideChar) + SizeOf(Cardinal));
    // �����ڴ�ռ�
  PCardinal(Result)^ := (Len + 1) * SizeOf(WideChar) + SizeOf(Cardinal);
    // ��¼�ڴ�ռ��С
  Result := PWideChar(Cardinal(Result) + SizeOf(Cardinal));
    // ����������С���ڴ�ռ�
  ZeroMemory(Result, (Len + 1) * SizeOf(WideChar));
    // ����ڴ�����
  CopyMemory(Result, Value, Len * SizeOf(WideChar));
    // ��������
end;

procedure StrDisposeA(var Value: PAnsiChar);
begin
  Value := Pointer(Cardinal(Value) - SizeOf(Cardinal));
  FreeMem(Value, Cardinal(Value^));
end;

procedure StrDisposeW(var Value: PWideChar);
begin
  Value := Pointer(Cardinal(Value) - SizeOf(Cardinal));
  FreeMem(Value, Cardinal(Value^));
end;

function StrCmpA(const Value1, Value2: PAnsiChar): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    Value1, StrLenA(Value1), Value2, StrLenA(Value2)) - 2;
end;

function StrCmpW(const Value1, Value2: PWideChar): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    Value1, StrLenW(Value1), Value2, StrLenW(Value2)) - 2;
end;

procedure DeleteObject(var AObject: TObject);
begin
  if not Assigned(AObject) then
    Exit;

  try
    FreeAndNil(AObject);
  except
  end;
end;

procedure DeleteString(var AString: PAnsiChar); overload;
begin
  if AString = nil then
    Exit;

  try
    StrDisposeA(AString);
    AString := nil;
  except
  end;
end;

procedure DeleteString(var AString: AnsiString); overload;
begin
end;

procedure DeleteString(var AString: PWideChar); overload;
begin
  if AString = nil then
    Exit;

  try
    StrDisposeW(AString);
    AString := nil;
  except
  end;
end;

procedure DeleteString(var AString: WideString); overload;
begin
end;

resourcestring
  SListIndexError = 'List Out of Bounds (%d).';
  SListCountError = 'Invalid List Count (%d).';
  SEmptyString = '';

type
  TCnCustomLinkedListIterator = class(TInterfacedObject, ICnCustomLinkedListIterator)
  {* ˫�����������ʵ���࣬��װ��ͨ���ı�������}
  private
    FList: TCnCustomLinkedList;
    FBof: Boolean;
    FEof: Boolean;
    FCurrent: PCnLinkedNode;
  protected
    function GetCurrentItem: Pointer;
  public
    constructor Create(AList: TCnCustomLinkedList);

    function Bof: Boolean;
    function Eof: Boolean;
    procedure First;
    procedure Last;
    procedure Previous;
    procedure Next;
  end;

type
  TCnLinkedListIterator = class(TCnCustomLinkedListIterator, ICnLinkedListIterator)
  public
    function GetCurrentItem: Pointer;
  end;

type
  TCnLinkedObjectListIterator = class(TCnCustomLinkedListIterator, ICnLinkedObjectListIterator)
  public
    function GetCurrentItem: TObject;
  end;

type
  TCnLinkedClassListIterator = class(TCnCustomLinkedListIterator, ICnLinkedClassListIterator)
  public
    function GetCurrentItem: TClass;
  end;

type
  TCnLinkedPAnsiCharsIterator = class(TCnCustomLinkedListIterator, ICnLinkedPAnsiCharsIterator)
  public
    function GetCurrentString: PAnsiChar;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedAnsiStringsIterator = class(TCnCustomLinkedListIterator, ICnLinkedAnsiStringsIterator)
  public
    function GetCurrentString: AnsiString;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedPWideCharsIterator = class(TCnCustomLinkedListIterator, ICnLinkedPWideCharsIterator)
  public
    function GetCurrentString: PWideChar;
    function GetCurrentObject: TObject;
  end;

type
  TCnLinkedWideStringsIterator = class(TCnCustomLinkedListIterator, ICnLinkedWideStringsIterator)
  public
    function GetCurrentString: WideString;
    function GetCurrentObject: TObject;
  end;

{ TCnCustomLinkedList }

function TCnCustomLinkedList.Add(const Item: Pointer): Integer;
begin
  Result := FCount;
  if not AddLast(Item) then
    Result := -1;
end;

function TCnCustomLinkedList.AddFirst(const Item: Pointer): Boolean;
var
  AItem: PCnLinkedNode;
begin
  try
    AItem := New(PCnLinkedNode);
    AItem.Previous := nil;
    AItem.Code := Item;
    AItem.Next := FFirst;

    if FFirst = nil then //�������ӵ�һ���ڵ�
      FLast := AItem
    else
      FFirst.Previous := AItem;

    FFirst := AItem;

    if FIndex <> -1 then
      Inc(FIndex);

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);
    Result := True;
  except
    Result := False;
  end;
end;

function TCnCustomLinkedList.AddLast(const Item: Pointer): Boolean;
var
  AItem: PCnLinkedNode;
begin
  try
    AItem := New(PCnLinkedNode);
    AItem.Previous := FLast;
    AItem.Code := Item;
    AItem.Next := nil;

    if FLast = nil then //�������ӵ�һ���ڵ�
      FFirst := AItem
    else
      FLast.Next := AItem;

    FLast := AItem;

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);
    Result := True;
  except
    Result := False;
  end;
end;

function TCnCustomLinkedList.AddMiddle(const Index: Integer; const Item: Pointer): Boolean;
var
  Item_P, Item_N, AItem: PCnLinkedNode;
begin
  Result := False;
  try
    if (Index <= 0) or (Index >= FCount - 1) then
      Exit;

    Item_N := GetItem(Index); //��ǰ�ڵ�
    Item_P := GetPrevious;

    AItem := New(PCnLinkedNode);
    AItem.Previous := Item_P;
    AItem.Code := Item;
    AItem.Next := Item_N;

    Item_P.Next := AItem;
    Item_N.Previous := AItem;

    //if (FIndex <= Index) and (FIndex <> -1) then
    Inc(FIndex);

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);

    Result := True;
  except
  end;
end;

procedure TCnCustomLinkedList.Assign(const AList: TCnCustomLinkedList);
var
  Loop: Integer;
begin
  Clear;
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Add(AList.Items[0]);
  for Loop := 0 to AList.Count - 2 do
    Add(AList.Next);
end;

function TCnCustomLinkedList.Clear: Integer;
begin
  Result := FCount;
  SetCount(0);
  FFirst := nil;
  FLast := nil;
end;

procedure TCnCustomLinkedList.ClearEvent;
begin
end;

constructor TCnCustomLinkedList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  ClearEvent;

  FFirst := nil;
  FLast := nil;
  FNode := nil;
  FIndex := -1;
  FCount := 0;
  FAutoClear := False;
  //FList := TList.Create;
end;

function TCnCustomLinkedList.CreateIterator: ICnCustomLinkedListIterator;
begin
  Result := TCnCustomLinkedListIterator.Create(Self);
end;

function TCnCustomLinkedList.Delete(const Index: Integer): Integer;
begin
  Result := -1;
  if (Index < 0) or (Index >= FCount) then
    Exit;

  if FCount > 1 then
  begin
    if Index = 0 then //ɾ���׽ڵ�
    begin
      DeleteFirst;
      Result := Index;
    end
    else if Index = FCount - 1 then //ɾ��β�ڵ�
    begin
      DeleteLast;
      Result := Index;
    end
    else if DeleteMiddle(Index) then
      Result := Index;
  end
  else //�����ɾ�����һ���ڵ�
  begin
    DeleteLastNode;
    Result := 0;
  end;
end;

function TCnCustomLinkedList.DeleteFirst: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;

  if FFirst = nil then
    Exit;

  Item := FFirst;
  FFirst := FFirst.Next;
  FFirst.Previous := nil;

  if FIndex = 0 then
    FNode := FFirst
  else if FIndex <> -1 then
    Dec(FIndex);

  Dec(FCount);
  if Item.Code <> nil then
    Notify(Item.Code, lnDeleted);
  Dispose(Item);

  Result := True;
end;

function TCnCustomLinkedList.DeleteLast: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;

  if FLast = nil then
    Exit;

  Item := FLast;
  FLast := FLast.Previous;
  FLast.Next := nil;

  if FIndex = FCount - 1 then
  begin
    Dec(FIndex);
    FNode := FLast;
  end;

  Dec(FCount);
  if Item.Code <> nil then
    Notify(Item.Code, lnDeleted);
  Dispose(Item);

  Result := True;
end;

function TCnCustomLinkedList.DeleteLastNode: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;
  if FCount > 1 then
    Exit;

  Item := FFirst;

  FFirst := nil;
  FLast := nil;
  FNode := nil;
  FIndex := -1;

  Dec(FCount);
  if Item.Code <> nil then
    Notify(Item.Code, lnDeleted);
  Dispose(Item);

  Result := True;
end;

function TCnCustomLinkedList.DeleteMiddle(const Index: Integer): Boolean;
var
  Item_P, Item_N, Item: PCnLinkedNode;
begin
  Result := False;

  if (Index <= 0) or (Index >= FCount - 1) then
    Exit;

  Item := GetItem(Index); //��ǰ�ڵ�
  Item_P := GetPrevious; //��һ�ڵ�
  Item_N := GetNext; //��һ�ڵ�

  Item_P.Next := Item_N;
  Item_N.Previous := Item_P;

  FNode := Item_N;
{
  if FNode = Item then //�����ѯ�ýڵ�Ϊ��ǰҪɾ���Ľڵ�
    FNode := Item_N
  else if FIndex > Index then //���ɾ����ѯ�ڵ�ǰ�Ľڵ�
    Dec(FIndex);
}
  Dec(FCount);
  if Item.Code <> nil then
    Notify(Item.Code, lnDeleted);
  DisPose(Item);
  Result := True;
end;

destructor TCnCustomLinkedList.Destroy;
begin
  Lock;
  try
    //if Assigned(FList) then
      //FreeAndNil(FList);
    Clear;
    ClearEvent;
    FIndex := -1;
    FNode := nil;
    FFirst := nil;
    FLast := nil;
  finally
    UnLock;
    DeleteCriticalSection(FLock);
  end;
  inherited Destroy;
end;

procedure TCnCustomLinkedList.Exchange(const Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index1]));
  if (Index2 < 0) or (Index2 >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index2]));

  Item := GetItem(Index1).Code;
  GetItem(Index1).Code := GetItem(Index2).Code;
  GetItem(Index2).Code := Item;
end;

function TCnCustomLinkedList.Extract(const Item: Pointer): Pointer;
var
  Index: Integer;
  AAutoClear: Boolean;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
  begin
    Result := Item;
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(Index);
    AutoClear := AAutoClear;
    Notify(Result, lnExtracted);
  end
  else
    Result := nil;
end;

function TCnCustomLinkedList.First: Pointer;
begin
  if FFirst = nil then
    raise Exception.Create(Format(SListIndexError, [0]));
  Result := FFirst.Code;
end;

function TCnCustomLinkedList.Get(Index: Integer): Pointer;
var
  Item: PCnLinkedNode;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index]));

  Item := GetItem(Index);
  if Item <> nil then
    Result := Item.Code
  else
    Result := nil;
end;

function TCnCustomLinkedList.GetBefore: PCnLinkedNode;
begin
  Result := FNode;
end;

function TCnCustomLinkedList.GetFirst: PCnLinkedNode;
begin
  Result := FFirst;
  if FFirst = nil then
    raise Exception.Create(Format(SListIndexError, [0]));

  FIndex := 0;
  FNode := FFirst;
end;

function TCnCustomLinkedList.GetItem(const Index: Integer): PCnLinkedNode;
begin
  Result := nil;
  if (Index < 0) or (Index >= FCount) then
    Exit;

  if Index = 0 then //�����׽ڵ�
    Result := GetFirst
  else if Index = FCount - 1 then //����β�ڵ�
    Result := GetLast
  else if Index = FIndex - 1 then //������β������ϴβ��ҵ�ǰһ�ڵ�
    Result := GetPrevious(True)
  else if Index = FIndex + 1 then //������β������ϴβ��ҵĺ�һ�ڵ�
    Result := GetNext(True)
  else if Index = FIndex then //������β���λ�ú��ϴβ�����ͬ
    Result := GetBefore
  else
    Result := GetMiddle(Index);
end;

function TCnCustomLinkedList.GetLast: PCnLinkedNode;
begin
  Result := FLast;
  if FLast = nil then
    raise Exception.Create(Format(SListIndexError, [Count - 1]));

  FNode := FLast;
  FIndex := FCount - 1;
end;
{
function TCnCustomLinkedList.GetList: TList;
var
  Index: Integer;
begin
  FList.Clear;
  if FCount <> 0 then
  begin
    FList.Add(Get(0));
    for Index := 0 to FCount - 2 do
      FList.Add(Next);
  end;

  Result := FList;
end;
}

function TCnCustomLinkedList.GetMiddle(const Index: Integer): PCnLinkedNode;
var
  I, IFirst, ILast, ICode: Integer;
  PFirst, PLast: PCnLinkedNode;
begin
  if FIndex = -1 then //����ǵ�һ�β���
  begin
    FIndex := 0;
    FNode := FFirst;
  end
  else //����ϴβ��ҽڵ�λ�ñ����ڵ��
  begin
    FIndex := FCount - 1;
    FNode := FLast;
  end;

  if Index < FIndex then //������β��ҽڵ����ϴβ��ҽڵ�֮ǰ
  begin
    IFirst := 0; //ѭ��������ʼֵ
    ILast := FIndex; //ѭ��������ֵֹ
    PFirst := FFirst; //ѭ��������ʼ�ڵ�
    PLast := FNode; //ѭ��������ֹ�ڵ�
  end
  else
  begin
    IFirst := FIndex; //ѭ��������ʼֵ
    ILast := FCount - 1; //ѭ��������ֵֹ
    PFirst := FNode; //ѭ��������ʼ�ڵ�
    PLast := FLast; //ѭ��������ֹ�ڵ�
  end;
  ICode := (ILast - IFirst) div 2; //�����м�ֵ

  if Index < ICode then //���������ű��м�ֵС�ʹ���ʼλ�ÿ�ʼ����
  begin
    Result := PFirst;
    I := IFirst;
    while I <> Index do
    begin
      Result := Result.Next;
      Inc(I);
    end;
  end
  else //���������ű��м�ֵС�ʹ���ֹλ�ÿ�ʼ����
  begin
    Result := PLast;
    I := ILast;
    while I <> Index do
    begin
      Result := Result.Previous;
      Dec(I);
    end;
  end;

  FNode := Result;
  FIndex := Index;
end;

function TCnCustomLinkedList.GetNext(Move: Boolean): PCnLinkedNode;
begin
  if FNode = nil then
    raise Exception.Create(Format(SListIndexError, [FIndex]));

  Result := FNode.Next;
  if Result = nil then
    raise Exception.Create(Format(SListIndexError, [FIndex + 1]));
  if Move then
  begin
    Inc(FIndex);
    FNode := FNode.Next;
  end;
end;

function TCnCustomLinkedList.GetPrevious(Move: Boolean): PCnLinkedNode;
begin
  if FNode = nil then
    raise Exception.Create(Format(SListIndexError, [FIndex]));

  Result := FNode.Previous;
  if Result = nil then
    raise Exception.Create(Format(SListIndexError, [FIndex - 1]));
  if Move then
  begin
    Dec(FIndex);
    FNode := FNode.Previous;
  end;
end;

function TCnCustomLinkedList.IndexOf(const Item: Pointer): Integer;
begin
  Result := -1;
  if FCount = 0 then
    Exit;

  if Item = Get(0) then
    Result := 0
  else
  begin
    Result := 1;
    while (Result < FCount) and (Item <> Next) do
      Inc(Result);

    if Result = FCount then
      Result := -1;
  end;
end;

function TCnCustomLinkedList.Insert(const Index: Integer; const Item: Pointer): Integer;
var
  Flag: Boolean;
begin
  Result := -1;
  if Index < 0 then
    Exit;

  if Index = 0 then
    Flag := AddFirst(Item)
  else if Index >= FCount - 1 then
    Flag := AddLast(Item)
  else
    Flag := AddMiddle(Index, Item);

  if Flag then
    Result := Index;
end;

function TCnCustomLinkedList.Last: Pointer;
begin
  if FLast = nil then
    raise Exception.Create(Format(SListIndexError, [FCount - 1]));
  Result := FLast.Code;
end;

procedure TCnCustomLinkedList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TCnCustomLinkedList.Move(const CurIndex, NewIndex: Integer);
var
  Item: Pointer;
  AAutoClear: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      raise Exception.Create(Format(SListIndexError, [NewIndex]));

    Item := Get(CurIndex);
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(CurIndex);
    AutoClear := AAutoClear;
    Insert(NewIndex, Item);
  end;
end;

function TCnCustomLinkedList.Next: Pointer;
begin
  Result := GetNext(True)^.Code;
end;

procedure TCnCustomLinkedList.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
end;

procedure TCnCustomLinkedList.Pack;
var
  Loop: Integer;
begin
  for Loop := FCount - 1 downto 0 do
    if Get(Loop) = nil then
      Delete(Loop);
end;

function TCnCustomLinkedList.Previous: Pointer;
begin
  Result := GetPrevious(True)^.Code;
end;

procedure TCnCustomLinkedList.Put(Index: Integer; Item: Pointer);
var
  Code: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index]));

  Code := Get(Index);
  if Item <> Code then
  begin
    GetItem(Index).Code := Item;
    if Code <> nil then
      Notify(Code, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

procedure TCnCustomLinkedList.QuickSort(Left, Right: Integer;
  Compare: Pointer);
var
  ALeft, ARight, AOrdinal: Integer;
begin
  repeat
    ALeft := Left;
    ARight := Right;
    AOrdinal := (Left + Right) shr 1;
    repeat
      while TCompare(Compare)(Get(ALeft), Get(AOrdinal)) < 0 do
        Inc(ALeft);
      while TCompare(Compare)(Get(ARight), Get(AOrdinal)) > 0 do
        Dec(ARight);
      if ALeft <= ARight then
      begin
        Exchange(ALeft, ARight);
        if AOrdinal = ALeft then
          AOrdinal := ARight
        else if AOrdinal = ARight then
          AOrdinal := ALeft;
        Inc(ALeft);
        Dec(ARight);
      end;
    until ALeft > ARight;
    if Left < ARight then
      QuickSort(Left, ARight, Compare);
    Left := ALeft;
  until ALeft >= Right;
end;

function TCnCustomLinkedList.Remove(const Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnCustomLinkedList.SetCount(const NewCount: Integer);
var
  Loop: Integer;
begin
  if NewCount < 0 then
    raise Exception.Create(Format(SListCountError, [NewCount]));

  if NewCount > FCount then
    for Loop := 0 to NewCount - FCount do
      Add(nil)
  else
    for Loop := FCount - 1 downto NewCount do
      Delete(Loop);
  FCount := NewCount;
end;

procedure TCnCustomLinkedList.Sort(Compare: TCompare);
begin
  if FCount = 0 then
    Exit;
  QuickSort(0, FCount - 1, @Compare);
end;

procedure TCnCustomLinkedList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TCnLinkedList }

function TCnLinkedList.Add(const Item: Pointer): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TCnLinkedList.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddItem := nil;
  FOnDeleteItem := nil;
  FOnExtractItem := nil;
end;

constructor TCnLinkedList.Create(const AAutoClear: Boolean);
begin
  inherited Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedList.CreateIterator: ICnLinkedListIterator;
begin
  Result := TCnLinkedListIterator.Create(Self);
end;

procedure TCnLinkedList.DeleteItemCode(Item: Pointer);
begin
  if Item <> nil then
  try
    Dispose(Item);
  except
  end;
end;

procedure TCnLinkedList.DoAddItem(Item: Pointer);
begin
  if Assigned(FOnAddItem) then
    FOnAddItem(Self, Item);
end;

procedure TCnLinkedList.DoDeleteItem(Item: Pointer);
begin
  if Assigned(FOnDeleteItem) then
    FOnDeleteItem(Self, Item);
  if AutoClear then
    DeleteItemCode(Item);
end;

procedure TCnLinkedList.DoExtractItem(Item: Pointer);
begin
  if Assigned(FOnExtractItem) then
    FOnExtractItem(Self, Item);
end;

function TCnLinkedList.Extract(const Item: Pointer): Pointer;
begin
  Result := inherited Extract(Item);
end;

function TCnLinkedList.First: Pointer;
begin
  Result := inherited First;
end;

function TCnLinkedList.IndexOf(const Item: Pointer): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TCnLinkedList.Insert(const Index: Integer; const Item: Pointer): Integer;
begin
  Result := inherited Insert(Index, Item);
end;

function TCnLinkedList.Last: Pointer;
begin
  Result := inherited Last;
end;

function TCnLinkedList.Next: Pointer;
begin
  Result := inherited Next;
end;

procedure TCnLinkedList.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddItem(Ptr);
    lnDeleted: DoDeleteItem(Ptr);
    lnExtracted: DoExtractItem(Ptr);
  end;
end;

procedure TCnLinkedList.Pack;
begin
  inherited Pack;
end;

function TCnLinkedList.Previous: Pointer;
begin
  Result := inherited Previous;
end;

function TCnLinkedList.Remove(const Item: Pointer): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TCnLinkedList.Sort(Compare: TCompare);
begin
  inherited Sort(Compare);
end;

{ TCnLinkedObjectList }

function TCnLinkedObjectList.Add(const AObject: TObject): Integer;
begin
  Result := inherited Add(Pointer(AObject));
end;

procedure TCnLinkedObjectList.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddObject := nil;
  FOnDeleteObject := nil;
  FOnExtractObject := nil;
end;

constructor TCnLinkedObjectList.Create(const AAutoClear: Boolean);
begin
  inherited Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedObjectList.CreateIterator: ICnLinkedObjectListIterator;
begin
  Result := TCnLinkedObjectListIterator.Create(Self);
end;

procedure TCnLinkedObjectList.DeleteItemCode(AObject: TObject);
begin
  DeleteObject(AObject);
end;

procedure TCnLinkedObjectList.DoAddObject(AObject: TObject);
begin
  if Assigned(FOnAddObject) then
    FOnAddObject(Self, AObject);
end;

procedure TCnLinkedObjectList.DoDeleteObject(AObject: TObject);
begin
  if Assigned(FOnDeleteObject) then
    FOnDeleteObject(Self, AObject);
  if AutoClear then
    DeleteItemCode(AObject);
end;

procedure TCnLinkedObjectList.DoExtractObject(AObject: TObject);
begin
  if Assigned(FOnExtractObject) then
    FOnExtractObject(Self, AObject);
end;

function TCnLinkedObjectList.Extract(const AObject: TObject): TObject;
begin
  Result := TObject(inherited Extract(Pointer(AObject)));
end;

function TCnLinkedObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
var
  Loop: Integer;
begin
  Result := -1;
  for Loop := AStartAt to Count - 1 do
    if (AExact and
      (Objects[Loop].ClassType = AClass)) or
      (not AExact and
      Objects[Loop].InheritsFrom(AClass)) then
    begin
      Result := Loop;
      Break;
    end;
end;

function TCnLinkedObjectList.First: TObject;
begin
  Result := TObject(inherited First);
end;

function TCnLinkedObjectList.GetObjects(Index: Integer): TObject;
begin
  Result := TObject(inherited Items[Index]);
end;

function TCnLinkedObjectList.IndexOf(const AObject: TObject): Integer;
begin
  Result := inherited IndexOf(Pointer(AObject));
end;

function TCnLinkedObjectList.Insert(const Index: Integer; const AObject: TObject): Integer;
begin
  Result := inherited Insert(Index, Pointer(AObject));
end;

function TCnLinkedObjectList.Last: TObject;
begin
  Result := TObject(inherited Last);
end;

function TCnLinkedObjectList.Next: TObject;
begin
  Result := TObject(inherited Next);
end;

procedure TCnLinkedObjectList.Notify(Ptr: Pointer;
  Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddObject(Ptr);
    lnDeleted: DoDeleteObject(Ptr);
    lnExtracted: DoExtractObject(Ptr);
  end;
end;

procedure TCnLinkedObjectList.Pack;
begin
  inherited Pack;
end;

function TCnLinkedObjectList.Previous: TObject;
begin
  Result := TObject(inherited Previous);
end;

function TCnLinkedObjectList.Remove(const AObject: TObject): Integer;
begin
  Result := inherited Remove(Pointer(AObject));
end;

procedure TCnLinkedObjectList.SetObjects(Index: Integer; const AObject: TObject);
begin
  inherited Items[Index] := Pointer(AObject);
end;

procedure TCnLinkedObjectList.Sort(Compare: TObjectCompare);
begin
  inherited QuickSort(0, FCount - 1, @Compare);
end;

{ TCnLinkedClassList }

function TCnLinkedClassList.Add(const AClass: TClass): Integer;
begin
  Result := inherited Add(Pointer(AClass));
end;

procedure TCnLinkedClassList.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddClass := nil;
  FOnDeleteClass := nil;
  FOnExtractClass := nil;
end;

function TCnLinkedClassList.CreateIterator: ICnLinkedClassListIterator;
begin
  Result := TCnLinkedClassListIterator.Create(Self);
end;

procedure TCnLinkedClassList.DoAddClass(AClass: TClass);
begin
  if Assigned(FOnAddClass) then
    FOnAddClass(Self, AClass);
end;

procedure TCnLinkedClassList.DoDeleteClass(AClass: TClass);
begin
  if Assigned(FOnDeleteClass) then
    FOnDeleteClass(Self, AClass);
end;

procedure TCnLinkedClassList.DoExtractClass(AClass: TClass);
begin
  if Assigned(FOnExtractClass) then
    FOnExtractClass(Self, AClass);
end;

function TCnLinkedClassList.Extract(const AClass: TClass): TClass;
begin
  Result := TClass(inherited Extract(Pointer(AClass)));
end;

function TCnLinkedClassList.First: TClass;
begin
  Result := TClass(inherited First);
end;

function TCnLinkedClassList.GetClasses(Index: Integer): TClass;
begin
  Result := TClass(inherited Items[Index]);
end;

function TCnLinkedClassList.IndexOf(const AClass: TClass): Integer;
begin
  Result := inherited IndexOf(Pointer(AClass));
end;

function TCnLinkedClassList.Insert(const Index: Integer; const AClass: TClass): Integer;
begin
  Result := inherited Insert(Index, Pointer(AClass));
end;

function TCnLinkedClassList.Last: TClass;
begin
  Result := TClass(inherited Last);
end;

function TCnLinkedClassList.Next: TClass;
begin
  Result := TClass(inherited Next);
end;

procedure TCnLinkedClassList.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddClass(Ptr);
    lnDeleted: DoDeleteClass(Ptr);
    lnExtracted: DoExtractClass(Ptr);
  end;
end;

procedure TCnLinkedClassList.Pack;
begin
  inherited Pack;
end;

function TCnLinkedClassList.Previous: TClass;
begin
  Result := TClass(inherited Previous);
end;

function TCnLinkedClassList.Remove(const AClass: TClass): Integer;
begin
  Result := inherited Remove(Pointer(AClass));
end;

procedure TCnLinkedClassList.SetClasses(Index: Integer; const AClass: TClass);
begin
  inherited Items[Index] := Pointer(AClass);
end;

procedure TCnLinkedClassList.Sort(Compare: TClassCompare);
begin
  inherited QuickSort(0, FCount - 1, @Compare);
end;

{ TCnLinkedPAnsiChars }

function CnLinkedPAnsiCharsCompare(Item1, Item2: PCnPAnsiCharItem): Integer;
begin
  Result := StrCmpA(Item1^.AString, Item2^.AString);
end;

function TCnLinkedPAnsiChars.Add(const AString: PAnsiChar): Integer;
begin
  Result := AddObject(AString, nil);
end;

function TCnLinkedPAnsiChars.AddObject(const AString: PAnsiChar;
  const AObject: TObject): Integer;
var
  Item: PCnPAnsiCharItem;
begin
  Item := New(PCnPAnsiCharItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Add(Pointer(Item));
end;

procedure TCnLinkedPAnsiChars.AddStrings(const AList: TCnLinkedPAnsiChars);
var
  Loop: Integer;
begin
  if not Assigned(AList) then
    Exit;

  for Loop := 0 to AList.Count - 1 do
    AddObject(AList.Strings[Loop], AList.Objects[Loop]);
end;

procedure TCnLinkedPAnsiChars.Assign(const AList: TCnCustomLinkedList);
var
  Loop: Integer;
  Item: PCnPAnsiCharItem;
begin
  Clear;
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Item := AList.Items[0];
  AddObject(Item^.AString, Item^.AObject);
  for Loop := 0 to AList.Count - 2 do
  begin
    Item := AList.Next;
    AddObject(Item^.AString, Item^.AObject);
  end;
end;

procedure TCnLinkedPAnsiChars.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddString := nil;
  FOnDeleteString := nil;
  FOnExtractString := nil;
end;

constructor TCnLinkedPAnsiChars.Create;
begin
  inherited Create;
  FList := TCnLinkedList.Create;
  FList.OnDeleteItem := ListDeleteItem;
  FText := nil;
end;

constructor TCnLinkedPAnsiChars.Create(const AAutoClear: Boolean);
begin
  Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedPAnsiChars.CreateIterator: ICnLinkedPAnsiCharsIterator;
begin
  Result := TCnLinkedPAnsiCharsIterator.Create(Self);
end;

procedure TCnLinkedPAnsiChars.DeleteItemCode(Item: PCnPAnsiCharItem);
begin
  if Assigned(Item) then
  begin
    DeleteObject(Item^.AObject);
    DeleteString(Item^.AString);
  end;
end;

destructor TCnLinkedPAnsiChars.Destroy;
begin
  if FText <> nil then
    FreeMemory(FText);
  inherited Destroy;
  DeleteObject(TObject(FList));
end;

procedure TCnLinkedPAnsiChars.DoAddItem(Item: PCnPAnsiCharItem);
begin
  if Assigned(FOnAddString) then
    FOnAddString(Self, Item^.AString);
end;

procedure TCnLinkedPAnsiChars.DoDeleteItem(Item: PCnPAnsiCharItem);
begin
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Item^.AString);
  if AutoClear then
    DeleteItemCode(Item);
  Dispose(Item);
end;

procedure TCnLinkedPAnsiChars.DoExtractItem(Item: PCnPAnsiCharItem);
begin
  if Assigned(FOnExtractString) then
    FOnExtractString(Self, Item^.AString);

  if AutoClear then
    DeleteObject(Item^.AObject);
end;

function TCnLinkedPAnsiChars.Extract(const AString: PAnsiChar): PAnsiChar;
var
  Index: Integer;
  AAutoClear: Boolean;
  Item: PCnPAnsiCharItem;
begin
  Index := IndexOf(AString);
  if Index >= 0 then
  begin
    Result := AString;
    Item := New(PCnPAnsiCharItem);
    Item^.AString := GetStrings(Index);
    Item^.AObject := GetObjects(Index);
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(Index);
    AutoClear := AAutoClear;
    Notify(Item, lnExtracted);
    DisPose(Item);
  end
  else
    Result := nil;
end;

function TCnLinkedPAnsiChars.First: PAnsiChar;
begin
  Result := PCnPAnsiCharItem(inherited First)^.AString;
end;

function TCnLinkedPAnsiChars.GetObjects(Index: Integer): TObject;
begin
  Result := PCnPAnsiCharItem(inherited Items[Index])^.AObject;
end;

function TCnLinkedPAnsiChars.GetStrings(Index: Integer): PAnsiChar;
begin
  Result := PCnPAnsiCharItem(inherited Items[Index])^.AString;
end;

function TCnLinkedPAnsiChars.GetText: PAnsiChar;
var
  Loop, ResultLength, AStringLength: Integer;
  PResult, AString: PAnsiChar;
begin
  ResultLength := 0;
  for Loop := 0 to Count - 1 do
    Inc(ResultLength, StrLenA(GetStrings(Loop)) + StrLenA(sLineBreak));
      // ���������ַ������ܳ���
  if FText <> nil then
  begin
    FreeMemory(FText);
    FText := nil;
  end;
  FText := GetMemory((ResultLength + 1) * SizeOf(AnsiChar));
    // �����ڴ�
  ZeroMemory(FText, (ResultLength + 1) * SizeOf(AnsiChar));
  PResult := Pointer(FText);
  for Loop := 0 to Count - 1 do
  begin
    AString := GetStrings(Loop);
    AStringLength := StrLenA(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(AnsiChar));
      Inc(PResult, AStringLength);
    end;
    AString := sLineBreak;
    AStringLength := StrLenA(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(AnsiChar));
      Inc(PResult, AStringLength);
    end;
  end;
  Result := FText;
end;

function TCnLinkedPAnsiChars.IndexOf(const AString: PAnsiChar): Integer;
begin
  Result := -1;
  if Count = 0 then
    Exit;

  if StrCmpA(GetStrings(0), AString) = 0 then
    Result := 0
  else
  begin
    Result := 1;
    while (Result < Count) and (StrCmpA(Next, AString) <> 0) do
      Inc(Result);

    if Result = Count then
      Result := -1;
  end;
end;

function TCnLinkedPAnsiChars.Insert(const Index: Integer; const AString: PAnsiChar): Integer;
begin
  Result := InsertObject(Index, AString, nil);
end;

function TCnLinkedPAnsiChars.InsertObject(const Index: Integer;
  const AString: PAnsiChar; const AObject: TObject): Integer;
var
  Item: PCnPAnsiCharItem;
begin
  Item := New(PCnPAnsiCharItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Insert(Index, Pointer(Item));
end;

function TCnLinkedPAnsiChars.Last: PAnsiChar;
begin
  Result := PCnPAnsiCharItem(inherited Last)^.AString;
end;

procedure TCnLinkedPAnsiChars.ListDeleteItem(Sender: TObject;
  Item: Pointer);
begin
  try
    StrDisposeA(PAnsiChar(Item));
  except
  end;
end;

function TCnLinkedPAnsiChars.Next: PAnsiChar;
begin
  Result := PCnPAnsiCharItem(inherited Next)^.AString;
end;

procedure TCnLinkedPAnsiChars.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddItem(Ptr);
    lnDeleted: DoDeleteItem(Ptr);
    lnExtracted: DoExtractItem(Ptr);
  end;
end;

procedure TCnLinkedPAnsiChars.Pack;
var
  Loop: Integer;
  Item: PCnPAnsiCharItem;
begin
  for Loop := Count - 1 downto 0 do
  begin
    Item := inherited Items[Loop];
    if (not Assigned(Item)) or
      (not Assigned(Item^.AString) or (StrLenA(Item^.AString) = 0)) and
      (not Assigned(Item^.AObject)) then
      Delete(Loop);
  end;
end;

function TCnLinkedPAnsiChars.Previous: PAnsiChar;
begin
  Result := PCnPAnsiCharItem(inherited Previous)^.AString;
end;

function TCnLinkedPAnsiChars.Remove(const AString: PAnsiChar): Integer;
begin
  Result := IndexOf(AString);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnLinkedPAnsiChars.SetObjects(Index: Integer; const AObject: TObject);
var
  Item: PCnPAnsiCharItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear then
    DeleteObject(Item^.AObject);
  Item^.AObject := AObject;
end;

procedure TCnLinkedPAnsiChars.SetStrings(Index: Integer; const AString: PAnsiChar);
var
  Item: PCnPAnsiCharItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear and (Item^.AString <> nil) then
  try
    StrDisposeA(Item^.AString);
  except
  end;
  Item^.AString := AString;
end;

procedure TCnLinkedPAnsiChars.SetText(const Value: PAnsiChar);
var
  PValue, PStart, PItem, AString: PAnsiChar;
begin
  Clear;
  FList.Clear;

  PValue := Pointer(Value);
  if (PValue = nil) or (Length(Value) = 0) then
    Exit;

  while PValue^ <> #0 do
  begin
    PStart := PValue;
    while not (PValue^ in [#0, #10, #13]) do
      Inc(PValue);
    AString := GetMemory((PValue - PStart + 1) * SizeOf(AnsiChar));
    ZeroMemory(AString, (PValue - PStart + 1) * SizeOf(AnsiChar));
    System.Move(PStart^, Pointer(AString)^, (PValue - PStart) * SizeOf(AnsiChar));
    PItem := StrNewA(PAnsiChar(AString));
    Add(PItem);
    FreeMemory(AString);
    if not AutoClear then
      FList.Add(PItem);

    while PValue^ in [#10, #13] do
      Inc(PValue);
  end;
end;

procedure TCnLinkedPAnsiChars.Sort;
begin
  if Count = 0 then
    Exit;
  inherited QuickSort(0, Count - 1, @CnLinkedPAnsiCharsCompare);
end;

{ TCnLinkedAnsiStrings }

function CnLinkedAnsiStringsCompare(Item1, Item2: PCnAnsiStringItem): Integer;
begin
  Result := StrCmpA(PAnsiChar(Item1^.AString), PAnsiChar(Item2^.AString));
end;

function TCnLinkedAnsiStrings.Add(const AString: AnsiString): Integer;
begin
  Result := AddObject(AString, nil);
end;

function TCnLinkedAnsiStrings.AddObject(const AString: AnsiString; const AObject: TObject): Integer;
var
  Item: PCnAnsiStringItem;
begin
  Item := New(PCnAnsiStringItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Add(Pointer(Item));
end;

procedure TCnLinkedAnsiStrings.AddStrings(const AList: TCnLinkedAnsiStrings);
var
  Loop: Integer;
begin
  if not Assigned(AList) then
    Exit;

  for Loop := 0 to AList.Count - 1 do
    AddObject(AList.Strings[Loop], AList.Objects[Loop]);
end;

procedure TCnLinkedAnsiStrings.Assign(const AList: TCnCustomLinkedList);
var
  Loop: Integer;
  Item: PCnAnsiStringItem;
begin
  Clear;
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Item := AList.Items[0];
  AddObject(Item^.AString, Item^.AObject);
  for Loop := 0 to AList.Count - 2 do
  begin
    Item := AList.Next;
    AddObject(Item^.AString, Item^.AObject);
  end;
end;

procedure TCnLinkedAnsiStrings.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddString := nil;
  FOnDeleteString := nil;
  FOnExtractString := nil;
end;

constructor TCnLinkedAnsiStrings.Create(const AAutoClear: Boolean);
begin
  inherited Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedAnsiStrings.CreateIterator: ICnLinkedAnsiStringsIterator;
begin
  Result := TCnLinkedAnsiStringsIterator.Create(Self);
end;

procedure TCnLinkedAnsiStrings.DeleteItemCode(Item: PCnAnsiStringItem);
begin
  if Item <> nil then
  begin
    DeleteObject(Item^.AObject);
    DeleteString(Item^.AString);
  end;
end;

procedure TCnLinkedAnsiStrings.DoAddItem(Item: PCnAnsiStringItem);
begin
  if Assigned(FOnAddString) then
    FOnAddString(Self, Item^.AString);
end;

procedure TCnLinkedAnsiStrings.DoDeleteItem(Item: PCnAnsiStringItem);
begin
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Item^.AString);
  if AutoClear then
    DeleteItemCode(Item);
  Dispose(Item);
end;

procedure TCnLinkedAnsiStrings.DoExtractItem(Item: PCnAnsiStringItem);
begin
  if Assigned(FOnExtractString) then
    FOnExtractString(Self, Item^.AString);
  if AutoClear then
    DeleteObject(Item^.AObject);
end;

function TCnLinkedAnsiStrings.Extract(const AString: AnsiString): AnsiString;
var
  Index: Integer;
  AAutoClear: Boolean;
  Item: PCnAnsiStringItem;
begin
  Index := IndexOf(AString);
  if Index >= 0 then
  begin
    Result := AString;
    Item := New(PCnAnsiStringItem);
    Item^.AString := GetStrings(Index);
    Item^.AObject := GetObjects(Index);
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(Index);
    AutoClear := AAutoClear;
    Notify(Item, lnExtracted);
    DisPose(Item);
  end
  else
    SetLength(Result, 0);
end;

function TCnLinkedAnsiStrings.First: AnsiString;
begin
  Result := PCnAnsiStringItem(inherited First)^.AString;
end;

function TCnLinkedAnsiStrings.GetObjects(Index: Integer): TObject;
begin
  Result := PCnAnsiStringItem(inherited Items[Index])^.AObject;
end;

function TCnLinkedAnsiStrings.GetStrings(Index: Integer): AnsiString;
begin
  Result := PCnAnsiStringItem(inherited Items[Index])^.AString;
end;

function TCnLinkedAnsiStrings.GetText: AnsiString;
var
  Loop, ResultLength, AStringLength: Integer;
  PResult: PAnsiChar;
  AString: AnsiString;
begin
  ResultLength := 0;
  for Loop := 0 to Count - 1 do
    Inc(ResultLength, Length(GetStrings(Loop)) + Length(sLineBreak));
      // ���������ַ������ܳ���
  SetLength(Result, ResultLength);
    // �����ڴ�
  PResult := Pointer(Result);
  for Loop := 0 to Count - 1 do
  begin
    AString := GetStrings(Loop);
    AStringLength := Length(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(AnsiChar));
      Inc(PResult, AStringLength);
    end;
    AString := sLineBreak;
    AStringLength := Length(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(AnsiChar));
      Inc(PResult, AStringLength);
    end;
  end;
end;

function TCnLinkedAnsiStrings.IndexOf(const AString: AnsiString): Integer;
begin
  Result := -1;
  if Count = 0 then
    Exit;

  if StrCmpA(PAnsiChar(GetStrings(0)), PAnsiChar(AString)) = 0 then
    Result := 0
  else
    begin
      Result := 1;

      while (Result < Count) and (StrCmpA(PAnsiChar(Next), PAnsiChar(AString)) <> 0) do
        Inc(Result);

      if Result = Count then
        Result := -1;
    end;
end;

function TCnLinkedAnsiStrings.Insert(const Index: Integer; const AString: AnsiString): Integer;
begin
  Result := InsertObject(Index, AString, nil);
end;

function TCnLinkedAnsiStrings.InsertObject(const Index: Integer;
  const AString: AnsiString; const AObject: TObject): Integer;
var
  Item: PCnAnsiStringItem;
begin
  Item := New(PCnAnsiStringItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Insert(Index, Pointer(Item));
end;

function TCnLinkedAnsiStrings.Last: AnsiString;
begin
  Result := PCnAnsiStringItem(inherited Last)^.AString;
end;

function TCnLinkedAnsiStrings.Next: AnsiString;
begin
  Result := PCnAnsiStringItem(inherited Next)^.AString;
end;

procedure TCnLinkedAnsiStrings.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddItem(Ptr);
    lnDeleted: DoDeleteItem(Ptr);
    lnExtracted: DoExtractItem(Ptr);
  end;
end;

procedure TCnLinkedAnsiStrings.Pack;
var
  Loop: Integer;
  Item: PCnAnsiStringItem;
begin
  for Loop := Count - 1 downto 0 do
  begin
    Item := inherited Items[Loop];
    if (not Assigned(Item)) or
      (Length(Item^.AString) = 0) and (not Assigned(Item^.AObject)) then
      Delete(Loop);
  end;
end;

function TCnLinkedAnsiStrings.Previous: AnsiString;
begin
  Result := PCnAnsiStringItem(inherited Previous)^.AString;
end;

function TCnLinkedAnsiStrings.Remove(const AString: AnsiString): Integer;
begin
  Result := IndexOf(AString);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnLinkedAnsiStrings.SetObjects(Index: Integer; const AObject: TObject);
var
  Item: PCnAnsiStringItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear then
    DeleteObject(Item^.AObject);
  Item^.AObject := AObject;
end;

procedure TCnLinkedAnsiStrings.SetStrings(Index: Integer; const AString: AnsiString);
begin
  PCnAnsiStringItem(inherited Items[Index])^.AString := AString;
end;

procedure TCnLinkedAnsiStrings.SetText(const Value: AnsiString);
var
  PValue, PStart: PAnsiChar;
  AString: AnsiString;
begin
  Clear;
  PValue := Pointer(Value);
  if (PValue = nil) or (Length(Value) = 0) then
    Exit;

  while PValue^ <> #0 do
  begin
    PStart := PValue;
    while not (PValue^ in [#0, #10, #13]) do
      Inc(PValue);
    SetLength(AString, PValue - PStart);
    System.Move(PStart^, Pointer(AString)^, (PValue - PStart) * SizeOf(AnsiChar));
    Add(AString);

    while PValue^ in [#10, #13] do
      Inc(PValue);
  end;
end;

procedure TCnLinkedAnsiStrings.Sort;
begin
  if Count = 0 then
    Exit;
  inherited QuickSort(0, Count - 1, @CnLinkedAnsiStringsCompare);
end;

{ TCnLinkedPWideChars }

function CnLinkedPWideCharsCompare(Item1, Item2: PCnPWideCharItem): Integer;
begin
  Result := StrCmpW(Item1^.AString, Item2^.AString);
end;

function TCnLinkedPWideChars.Add(const AString: PWideChar): Integer;
begin
  Result := AddObject(AString, nil);
end;

function TCnLinkedPWideChars.AddObject(const AString: PWideChar;
  const AObject: TObject): Integer;
var
  Item: PCnPWideCharItem;
begin
  Item := New(PCnPWideCharItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Add(Pointer(Item));
end;

procedure TCnLinkedPWideChars.AddStrings(const AList: TCnLinkedPWideChars);
var
  Loop: Integer;
begin
  if not Assigned(AList) then
    Exit;

  for Loop := 0 to AList.Count - 1 do
    AddObject(AList.Strings[Loop], AList.Objects[Loop]);
end;

procedure TCnLinkedPWideChars.Assign(const AList: TCnCustomLinkedList);
var
  Loop: Integer;
  Item: PCnPWideCharItem;
begin
  Clear;
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Item := AList.Items[0];
  AddObject(Item^.AString, Item^.AObject);
  for Loop := 0 to AList.Count - 2 do
  begin
    Item := AList.Next;
    AddObject(Item^.AString, Item^.AObject);
  end;
end;

procedure TCnLinkedPWideChars.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddString := nil;
  FOnDeleteString := nil;
  FOnExtractString := nil;
end;

constructor TCnLinkedPWideChars.Create;
begin
  inherited Create;
  FList := TCnLinkedList.Create;
  FList.OnDeleteItem := ListDeleteItem;
  FText := nil;
end;

constructor TCnLinkedPWideChars.Create(const AAutoClear: Boolean);
begin
  Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedPWideChars.CreateIterator: ICnLinkedPWideCharsIterator;
begin
  Result := TCnLinkedPWideCharsIterator.Create(Self);
end;

procedure TCnLinkedPWideChars.DeleteItemCode(Item: PCnPWideCharItem);
begin
  if Item <> nil then
  begin
    DeleteObject(Item^.AObject);
    DeleteString(Item^.AString);
  end;
end;

destructor TCnLinkedPWideChars.Destroy;
begin
  if FText <> nil then
    FreeMemory(FText);
  inherited Destroy;
  DeleteObject(TObject(FList));
end;

procedure TCnLinkedPWideChars.DoAddItem(Item: PCnPWideCharItem);
begin
  if Assigned(FOnAddString) then
    FOnAddString(Self, Item^.AString);
end;

procedure TCnLinkedPWideChars.DoDeleteItem(Item: PCnPWideCharItem);
begin
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Item^.AString);
  if AutoClear then
    DeleteItemCode(Item);
  Dispose(Item);
end;

procedure TCnLinkedPWideChars.DoExtractItem(Item: PCnPWideCharItem);
begin
  if Assigned(FOnExtractString) then
    FOnExtractString(Self, Item^.AString);
end;

function TCnLinkedPWideChars.Extract(const AString: PWideChar): PWideChar;
var
  Index: Integer;
  AAutoClear: Boolean;
  Item: PCnPWideCharItem;
begin
  Index := IndexOf(AString);
  if Index >= 0 then
  begin
    Result := AString;
    Item := New(PCnPWideCharItem);
    Item^.AString := GetStrings(Index);
    Item^.AObject := GetObjects(Index);
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(Index);
    AutoClear := AAutoClear;
    Notify(Item, lnExtracted);
    DisPose(Item);
  end
  else
    Result := nil;
end;

function TCnLinkedPWideChars.First: PWideChar;
begin
  Result := PCnPWideCharItem(inherited First)^.AString;
end;

function TCnLinkedPWideChars.GetObjects(Index: Integer): TObject;
begin
  Result := PCnPWideCharItem(inherited Items[Index])^.AObject;
end;

function TCnLinkedPWideChars.GetStrings(Index: Integer): PWideChar;
begin
  Result := PCnPWideCharItem(inherited Items[Index])^.AString;
end;

function TCnLinkedPWideChars.GetText: PWideChar;
var
  Loop, ResultLength, AStringLength: Integer;
  PResult, AString: PWideChar;
begin
  ResultLength := 0;
  for Loop := 0 to Count - 1 do
    Inc(ResultLength, StrLenW(GetStrings(Loop)) + StrLenW(sLineBreak));
      // ���������ַ������ܳ���
  if FText <> nil then
  begin
    FreeMemory(FText);
    FText := nil;
  end;
  FText := GetMemory((ResultLength + 1) * SizeOf(WideChar));
    // �����ڴ�
  ZeroMemory(FText, (ResultLength + 1) * SizeOf(WideChar));
  PResult := Pointer(FText);
  for Loop := 0 to Count - 1 do
  begin
    AString := GetStrings(Loop);
    AStringLength := StrLenW(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(WideChar));
      Inc(PResult, AStringLength);
    end;
    AString := sLineBreak;
    AStringLength := StrLenW(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(WideChar));
      Inc(PResult, AStringLength);
    end;
  end;
  Result := FText;
end;

function TCnLinkedPWideChars.IndexOf(const AString: PWideChar): Integer;
begin
  Result := -1;
  if Count = 0 then
    Exit;

  if StrCmpW(GetStrings(0), AString) = 0 then
    Result := 0
  else
    begin
      Result := 1;

      while (Result < Count) and (StrCmpW(Next, AString) <> 0) do
        Inc(Result);

      if Result = Count then
        Result := -1;
    end;
end;

function TCnLinkedPWideChars.Insert(const Index: Integer; const AString: PWideChar): Integer;
begin
  Result := InsertObject(Index, AString, nil);
end;

function TCnLinkedPWideChars.InsertObject(const Index: Integer;
  const AString: PWideChar; const AObject: TObject): Integer;
var
  Item: PCnPWideCharItem;
begin
  Item := New(PCnPWideCharItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Insert(Index, Pointer(Item));
end;

function TCnLinkedPWideChars.Last: PWideChar;
begin
  Result := PCnPWideCharItem(inherited Last)^.AString;
end;

procedure TCnLinkedPWideChars.ListDeleteItem(Sender: TObject; Item: Pointer);
begin
  try
    StrDisposeW(PWideChar(Item));
  except
  end;
end;

function TCnLinkedPWideChars.Next: PWideChar;
begin
  Result := PCnPWideCharItem(inherited Next)^.AString;
end;

procedure TCnLinkedPWideChars.Notify(Ptr: Pointer;
  Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddItem(Ptr);
    lnDeleted: DoDeleteItem(Ptr);
    lnExtracted: DoExtractItem(Ptr);
  end;
end;

procedure TCnLinkedPWideChars.Pack;
var
  Loop: Integer;
  Item: PCnPWideCharItem;
begin
  for Loop := Count - 1 downto 0 do
  begin
    Item := inherited Items[Loop];
    if (not Assigned(Item)) or
      (not Assigned(Item^.AString) or (StrLenW(Item^.AString) = 0)) and
      (not Assigned(Item^.AObject)) then
      Delete(Loop);
  end;
end;

function TCnLinkedPWideChars.Previous: PWideChar;
begin
  Result := PCnPWideCharItem(inherited Previous)^.AString;
end;

function TCnLinkedPWideChars.Remove(const AString: PWideChar): Integer;
begin
  Result := IndexOf(AString);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnLinkedPWideChars.SetObjects(Index: Integer; const AObject: TObject);
var
  Item: PCnPWideCharItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear then
    DeleteObject(Item^.AObject);
  Item^.AObject := AObject;
end;

procedure TCnLinkedPWideChars.SetStrings(Index: Integer;
  const AString: PWideChar);
var
  Item: PCnPWideCharItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear and (Item^.AString <> nil) then
  try
    StrDisposeW(Item^.AString);
  except
  end;
  Item^.AString := AString;
end;

procedure TCnLinkedPWideChars.SetText(const Value: PWideChar);
var
  PValue, PStart, PItem, AString: PWideChar;
begin
  Clear;
  FList.Clear;

  PValue := Pointer(Value);
  if (PValue = nil) or (StrLenW(Value) = 0) then
    Exit;

  while PValue^ <> #0 do
  begin
    PStart := PValue;
    while (PValue^ <> #0) and (PValue^ <> #10) and (PValue^ <> #13) do
      Inc(PValue);
    AString := GetMemory((PValue - PStart + 1) * SizeOf(WideChar));
    ZeroMemory(AString, (PValue - PStart + 1) * SizeOf(WideChar));
    System.Move(PStart^, Pointer(AString)^, (PValue - PStart) * SizeOf(WideChar));
    PItem := StrNewW(PWideChar(AString));
    Add(PItem);
    FreeMemory(AString);
    if not AutoClear then
      FList.Add(PItem);

    while (PValue^ = #10) or (PValue^ = #13) do
      Inc(PValue);
  end;
end;

procedure TCnLinkedPWideChars.Sort;
begin
  if Count = 0 then
    Exit;
  inherited QuickSort(0, Count - 1, @CnLinkedPWideCharsCompare);
end;

{ TCnLinkedWideStrings }

function CnLinkedWideStringsCompare(Item1, Item2: PCnWideStringItem): Integer;
begin
  Result := StrCmpW(PWideChar(Item1^.AString), PWideChar(Item2^.AString));
end;

function TCnLinkedWideStrings.Add(const AString: WideString): Integer;
begin
  Result := AddObject(AString, nil);
end;

function TCnLinkedWideStrings.AddObject(const AString: WideString;
  const AObject: TObject): Integer;
var
  Item: PCnWideStringItem;
begin
  Item := New(PCnWideStringItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Add(Pointer(Item));
end;

procedure TCnLinkedWideStrings.AddStrings(const AList: TCnLinkedWideStrings);
var
  Loop: Integer;
begin
  if not Assigned(AList) then
    Exit;

  for Loop := 0 to AList.Count - 1 do
    AddObject(AList.Strings[Loop], AList.Objects[Loop]);
end;

procedure TCnLinkedWideStrings.Assign(const AList: TCnCustomLinkedList);
var
  Loop: Integer;
  Item: PCnWideStringItem;
begin
  Clear;
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Item := AList.Items[0];
  AddObject(Item^.AString, Item^.AObject);
  for Loop := 0 to AList.Count - 2 do
  begin
    Item := AList.Next;
    AddObject(Item^.AString, Item^.AObject);
  end;
end;

procedure TCnLinkedWideStrings.ClearEvent;
begin
  inherited ClearEvent;
  FOnAddString := nil;
  FOnDeleteString := nil;
  FOnExtractString := nil;
end;

constructor TCnLinkedWideStrings.Create(const AAutoClear: Boolean);
begin
  inherited Create;
  AutoClear := AAutoClear;
end;

function TCnLinkedWideStrings.CreateIterator: ICnLinkedWideStringsIterator;
begin
  Result := TCnLinkedWideStringsIterator.Create(Self);
end;

procedure TCnLinkedWideStrings.DeleteItemCode(Item: PCnWideStringItem);
begin
  if Item <> nil then
  begin
    DeleteObject(Item^.AObject);
    DeleteString(Item^.AString);
  end;
end;

procedure TCnLinkedWideStrings.DoAddItem(Item: PCnWideStringItem);
begin
  if Assigned(FOnAddString) then
    FOnAddString(Self, Item^.AString);
end;

procedure TCnLinkedWideStrings.DoDeleteItem(Item: PCnWideStringItem);
begin
  if Assigned(FOnDeleteString) then
    FOnDeleteString(Self, Item^.AString);
  if AutoClear then
    DeleteItemCode(Item);
  Dispose(Item);
end;

procedure TCnLinkedWideStrings.DoExtractItem(Item: PCnWideStringItem);
begin
  if Assigned(FOnExtractString) then
    FOnExtractString(Self, Item^.AString);
end;

function TCnLinkedWideStrings.Extract(const AString: WideString): WideString;
var
  Index: Integer;
  AAutoClear: Boolean;
  Item: PCnWideStringItem;
begin
  Index := IndexOf(AString);
  if Index >= 0 then
  begin
    Result := AString;
    Item := New(PCnWideStringItem);
    Item^.AString := GetStrings(Index);
    Item^.AObject := GetObjects(Index);
    AAutoClear := AutoClear;
    AutoClear := False;
    Delete(Index);
    AutoClear := AAutoClear;
    Notify(Item, lnExtracted);
    DisPose(Item);
  end
  else
    SetLength(Result, 0);
end;

function TCnLinkedWideStrings.First: WideString;
begin
  Result := PCnWideStringItem(inherited First)^.AString;
end;

function TCnLinkedWideStrings.GetObjects(Index: Integer): TObject;
begin
  Result := PCnWideStringItem(inherited Items[Index])^.AObject;
end;

function TCnLinkedWideStrings.GetStrings(Index: Integer): WideString;
begin
  Result := PCnWideStringItem(inherited Items[Index])^.AString;
end;

function TCnLinkedWideStrings.GetText: WideString;
var
  Loop, ResultLength, AStringLength: Integer;
  PResult: PWideChar;
  AString: WideString;
begin
  ResultLength := 0;
  for Loop := 0 to Count - 1 do
    Inc(ResultLength, Length(GetStrings(Loop)) + Length(sLineBreak));
      // ���������ַ������ܳ���
  SetLength(Result, ResultLength);
    // �����ڴ�
  PResult := Pointer(Result);
  for Loop := 0 to Count - 1 do
  begin
    AString := GetStrings(Loop);
    AStringLength := Length(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(WideChar));
      Inc(PResult, AStringLength);
    end;
    AString := sLineBreak;
    AStringLength := Length(AString);
    if AStringLength <> 0 then
    begin
      System.Move(Pointer(AString)^, PResult^, AStringLength * SizeOf(WideChar));
      Inc(PResult, AStringLength);
    end;
  end;
end;

function TCnLinkedWideStrings.IndexOf(const AString: WideString): Integer;
begin
  Result := -1;
  if Count = 0 then
    Exit;

  if StrCmpW(PWideChar(GetStrings(0)), PWideChar(AString)) = 0 then
    Result := 0
  else
    begin
      Result := 1;

      while (Result < Count) and (StrCmpW(PWideChar(Next), PWideChar(AString)) <> 0) do
        Inc(Result);

      if Result = Count then
        Result := -1;
    end;
end;

function TCnLinkedWideStrings.Insert(const Index: Integer; const AString: WideString): Integer;
begin
  Result := InsertObject(Index, AString, nil);
end;

function TCnLinkedWideStrings.InsertObject(const Index: Integer;
  const AString: WideString; const AObject: TObject): Integer;
var
  Item: PCnWideStringItem;
begin
  Item := New(PCnWideStringItem);
  Item^.AString := AString;
  Item^.AObject := AObject;
  Result := inherited Insert(Index, Pointer(Item));
end;

function TCnLinkedWideStrings.Last: WideString;
begin
  Result := PCnWideStringItem(inherited Last)^.AString;
end;

function TCnLinkedWideStrings.Next: WideString;
begin
  Result := PCnWideStringItem(inherited Next)^.AString;
end;

procedure TCnLinkedWideStrings.Notify(Ptr: Pointer; Action: TCnLinkedListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded: DoAddItem(Ptr);
    lnDeleted: DoDeleteItem(Ptr);
    lnExtracted: DoExtractItem(Ptr);
  end;
end;

procedure TCnLinkedWideStrings.Pack;
var
  Loop: Integer;
  Item: PCnWideStringItem;
begin
  for Loop := Count - 1 downto 0 do
  begin
    Item := inherited Items[Loop];
    if (not Assigned(Item)) or
      (Length(Item^.AString) = 0) and (not Assigned(Item^.AObject)) then
      Delete(Loop);
  end;
end;

function TCnLinkedWideStrings.Previous: WideString;
begin
  Result := PCnWideStringItem(inherited Previous)^.AString;
end;

function TCnLinkedWideStrings.Remove(const AString: WideString): Integer;
begin
  Result := IndexOf(AString);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnLinkedWideStrings.SetObjects(Index: Integer; const AObject: TObject);
var
  Item: PCnWideStringItem;
begin
  Item := inherited Items[Index];
  if Item = nil then
    Exit;

  if AutoClear then
    DeleteObject(Item^.AObject);
  Item^.AObject := AObject;
end;

procedure TCnLinkedWideStrings.SetStrings(Index: Integer; const AString: WideString);
begin
  PCnWideStringItem(inherited Items[Index])^.AString := AString;
end;

procedure TCnLinkedWideStrings.SetText(const Value: WideString);
var
  PValue, PStart: PWideChar;
  AString: WideString;
begin
  Clear;
  PValue := Pointer(Value);
  if (PValue = nil) or (Length(Value) = 0) then
    Exit;

  while PValue^ <> #0 do
  begin
    PStart := PValue;
    while (PValue^ <> #0) and (PValue^ <> #10) and (PValue^ <> #13) do
      Inc(PValue);
    SetLength(AString, PValue - PStart);
    System.Move(PStart^, Pointer(AString)^, (PValue - PStart) * SizeOf(WideChar));
    Add(AString);

    while (PValue^ = #10) or (PValue^ = #13) do
      Inc(PValue);
  end;
end;

procedure TCnLinkedWideStrings.Sort;
begin
  if Count = 0 then
    Exit;
  inherited QuickSort(0, Count - 1, @CnLinkedWideStringsCompare);
end;

{ TCnCustomLinkedListIterator }

function TCnCustomLinkedListIterator.Bof: Boolean;
begin
  Result := FBof;
end;

constructor TCnCustomLinkedListIterator.Create(AList: TCnCustomLinkedList);
begin
  inherited Create;

  FList := AList;
  if FList.Count = 0 then
  begin
    FBof := True;
    FEof := True;
  end
  else
    First;
end;

function TCnCustomLinkedListIterator.Eof: Boolean;
begin
  Result := FEof;
end;

procedure TCnCustomLinkedListIterator.First;
begin
  FCurrent := FList.FFirst;
  FBof := FCurrent = nil;
end;

function TCnCustomLinkedListIterator.GetCurrentItem: Pointer;
begin
  if FCurrent <> nil then
    Result := FCurrent^.Code
  else
    Result := nil;
end;

procedure TCnCustomLinkedListIterator.Last;
begin
  FCurrent := FList.FLast;
  FEof := FCurrent = nil;
end;

procedure TCnCustomLinkedListIterator.Next;
begin
  if FEof then
    Exit;

  FCurrent := FCurrent^.Next;
  FBof := False;
  FEof := FCurrent = nil;
end;

procedure TCnCustomLinkedListIterator.Previous;
begin
  if FBof then
    Exit;

  FCurrent := FCurrent^.Previous;
  FEof := False;
  FBof := FCurrent = nil;
end;

{ TCnLinkedListIterator }

function TCnLinkedListIterator.GetCurrentItem: Pointer;
begin
  Result := inherited GetCurrentItem;
end;

{ TCnLinkedObjectListIterator }

function TCnLinkedObjectListIterator.GetCurrentItem: TObject;
begin
  Result := TObject(inherited GetCurrentItem);
end;

{ TCnLinkedClassListIterator }

function TCnLinkedClassListIterator.GetCurrentItem: TClass;
begin
  Result := TClass(inherited GetCurrentItem);
end;

{ TCnLinkedPAnsiCharsIterator }

function TCnLinkedPAnsiCharsIterator.GetCurrentObject: TObject;
var
  Item: PCnPAnsiCharItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AObject
  else
    Result := nil;
end;

function TCnLinkedPAnsiCharsIterator.GetCurrentString: PAnsiChar;
var
  Item: PCnPAnsiCharItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AString
  else
    Result := nil;
end;

{ TCnLinkedAnsiStringsIterator }

function TCnLinkedAnsiStringsIterator.GetCurrentObject: TObject;
var
  Item: PCnAnsiStringItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AObject
  else
    Result := nil;
end;

function TCnLinkedAnsiStringsIterator.GetCurrentString: AnsiString;
var
  Item: PCnAnsiStringItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AString
  else
    Result := AnsiString(SEmptyString);
end;

{ TCnLinkedPWideCharsIterator }

function TCnLinkedPWideCharsIterator.GetCurrentObject: TObject;
var
  Item: PCnPWideCharItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AObject
  else
    Result := nil;
end;

function TCnLinkedPWideCharsIterator.GetCurrentString: PWideChar;
var
  Item: PCnPWideCharItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AString
  else
    Result := nil;
end;

{ TCnLinkedWideStringsIterator }

function TCnLinkedWideStringsIterator.GetCurrentObject: TObject;
var
  Item: PCnWideStringItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AObject
  else
    Result := nil;
end;

function TCnLinkedWideStringsIterator.GetCurrentString: WideString;
var
  Item: PCnWideStringItem;
begin
  Item := inherited GetCurrentItem;
  if Item <> nil then
    Result := Item^.AString
  else
    Result := SEmptyString;
end;

{ TCnLinkedCustomOrderedList }

function TCnLinkedCustomOrderedList.AtLeast(ACount: Integer): Boolean;
begin
  Result := List.Count >= ACount;
end;

function TCnLinkedCustomOrderedList.Count: Integer;
begin
  Result := List.Count;
end;

constructor TCnLinkedCustomOrderedList.Create;
begin
  inherited Create;
  FList := TCnLinkedList.Create;
end;

destructor TCnLinkedCustomOrderedList.Destroy;
begin
  DeleteObject(TObject(FList));
  inherited;
end;

function TCnLinkedCustomOrderedList.Peek: Pointer;
begin
  Result := PeekItem;
end;

function TCnLinkedCustomOrderedList.PeekItem: Pointer;
begin
  Result := List.Last;
end;

function TCnLinkedCustomOrderedList.Pop: Pointer;
begin
  Result := PopItem;
end;

function TCnLinkedCustomOrderedList.PopItem: Pointer;
begin
  Result := PeekItem;
  List.Delete(List.Count - 1);
end;

function TCnLinkedCustomOrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(AItem);
  Result := AItem;
end;

{ TCnLinkedOrderedList }

procedure TCnLinkedOrderedList.ClearEvent;
begin
  FOnPush := nil;
  FOnPop := nil;
end;

constructor TCnLinkedOrderedList.Create;
begin
  inherited Create;
  ClearEvent;
end;

destructor TCnLinkedOrderedList.Destroy;
begin
  ClearEvent;
  inherited Destroy;
end;

procedure TCnLinkedOrderedList.DoPop(AItem: Pointer);
begin
  if Assigned(FOnPop) then
    FOnPop(Self, AItem);
end;

procedure TCnLinkedOrderedList.DoPush(AItem: Pointer);
begin
  if Assigned(FOnPush) then
    FOnPush(Self, AItem);
end;

function TCnLinkedOrderedList.Peek: Pointer;
begin
  Result := inherited Peek;
end;

function TCnLinkedOrderedList.Pop: Pointer;
begin
  Result := inherited Pop;
end;

function TCnLinkedOrderedList.PopItem: Pointer;
begin
  Result := inherited PopItem;
  DoPop(Result);
end;

function TCnLinkedOrderedList.Push(AItem: Pointer): Pointer;
begin
  Result := inherited Push(AItem);
end;

procedure TCnLinkedOrderedList.PushItem(AItem: Pointer);
begin
  DoPush(AItem);
end;

{ TCnLinkedOrderedObjectList }

procedure TCnLinkedOrderedObjectList.ClearEvent;
begin
  FOnPush := nil;
  FOnPop := nil;
end;

constructor TCnLinkedOrderedObjectList.Create;
begin
  inherited Create;
  ClearEvent;
end;

destructor TCnLinkedOrderedObjectList.Destroy;
begin
  ClearEvent;
  inherited Destroy;
end;

procedure TCnLinkedOrderedObjectList.DoPop(AObject: TObject);
begin
  if Assigned(FOnPop) then
    FOnPop(Self, AObject);
end;

procedure TCnLinkedOrderedObjectList.DoPush(AObject: TObject);
begin
  if Assigned(FOnPush) then
    FOnPush(Self, AObject);
end;

function TCnLinkedOrderedObjectList.Peek: TObject;
begin
  Result := TObject(inherited Peek);
end;

function TCnLinkedOrderedObjectList.Pop: TObject;
begin
  Result := TObject(inherited Pop);
end;

function TCnLinkedOrderedObjectList.PopItem: Pointer;
begin
  Result := inherited PopItem;
  DoPop(TObject(Result));
end;

function TCnLinkedOrderedObjectList.Push(AObject: TObject): TObject;
begin
  Result := TObject(inherited Push(Pointer(AObject)));
end;

procedure TCnLinkedOrderedObjectList.PushItem(AItem: Pointer);
begin
  DoPush(TObject(AItem));
end;

{ TCnLinkedStack }

procedure TCnLinkedStack.PushItem(AItem: Pointer);
begin
  inherited PushItem(AItem);
  List.Add(AItem);
end;

{ TCnLinkedQueue }

procedure TCnLinkedQueue.PushItem(AItem: Pointer);
begin
  inherited PushItem(AItem);
  List.Insert(0, AItem);
end;

{ TCnLinkedObjectStack }

procedure TCnLinkedObjectStack.PushItem(AItem: Pointer);
begin
  inherited PushItem(AItem);
  List.Add(AItem);
end;

{ TCnLinkedObjectQueue }

procedure TCnLinkedObjectQueue.PushItem(AItem: Pointer);
begin
  inherited PushItem(AItem);
  List.Insert(0, AItem);
end;

end.

