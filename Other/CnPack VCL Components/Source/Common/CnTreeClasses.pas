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

unit CnTreeClasses;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ�ֶ���������������������������ֵ����������൥Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע���õ�ԪΪ���� TCnTree �� TCnLeaf ������
*           ������ TCnBinaryTree/Leaf���ֵ������� TCnTrieTree/Leaf��
*           TCnTree/Leaf ������ TTreeNodes/Node �Ĺ�ϵ��֧����Ⱥ͹�����ȱ�����
*           ֧�ְ�������ȵ�˳��������ֵ����ʽֱ�ӷ��ʸ����ڵ㡣
*           �������δ��ȫʵ�֣������á�
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6 + 10.3.1
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.09.10 V1.0 by LiuXiao
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// �� ENABLE_FMX ������ FMX �������Ƿ�֧�� FMX
{$IFNDEF ENABLE_FMX}
  {$UNDEF SUPPORT_FMX}
{$ENDIF}
  
uses
  SysUtils, Classes, Contnrs {$IFDEF MSWINDOWS}, ComCtrls {$ENDIF}
  {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}, Math, CnTree;
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

type
//==============================================================================
// ��������ʵ��
//==============================================================================

  ECnBinaryTreeException = class(Exception);

  TCnBinaryTree = class;

  TCnBinaryLeaf = class(TCnLeaf)
  {* �������ڵ����࣬�������ӽڵ�ķ�װ}
  private
    function GetLeftLeaf: TCnBinaryLeaf;
    function GetRightLeaf: TCnBinaryLeaf;
    procedure SetLeftLeaf(const Value: TCnBinaryLeaf);
    procedure SetRightLeaf(const Value: TCnBinaryLeaf);
    function GetTree: TCnBinaryTree;
    function GetParent: TCnBinaryLeaf;
    procedure SetParent(const Value: TCnBinaryLeaf);
  protected
    function GetSubTreeHeight: Integer; override;

    procedure DoPreOrderTravel;
    procedure DoInOrderTravel;
    procedure DoPostOrderTravel;

    function GetMostLeftLeaf: TCnBinaryLeaf;
    {* �ݹ��ȡ����������������ӽڵ�}
    function GetMostRightLeaf: TCnBinaryLeaf;
    {* �ݹ��ȡ����������������ӽڵ�}
  public
    constructor Create(ATree: TCnTree); override;
    function IsBalance: Boolean;
    {* �Դ˽ڵ�Ϊ���ڵ���Ӷ������Ƿ���ƽ�������}

    function AddLeftChild: TCnBinaryLeaf;
    {* �������ӽڵ㣬���Ѵ����򷵻� nil}
    function AddRightChild: TCnBinaryLeaf;
    {* �������ӽڵ㣬���Ѵ����򷵻� nil}
    procedure DeleteLeftChild;
    {* ɾ�����ӽڵ㣬Ҳ������ nil}
    procedure DeleteRightChild;
    {* ɾ�����ӽڵ㣬Ҳ������ nil}

    function GetMostRightLeafFromLeft: TCnBinaryLeaf;
    {* ��ȡ����������ҵĽڵ㣬Ҳ�����������ǰ���ڵ�}
    function GetMostLeftLeafFromRight: TCnBinaryLeaf;
    {* ��ȡ�����������Ľڵ㣬Ҳ����������ĺ����ڵ�}
    function GetBrotherLeaf: TCnBinaryLeaf;
    {* ��ȡ�ֵܽڵ㣬Ҳ���Ǹ��ڵ����һ�ӽڵ�}
    function GetUncleLeaf: TCnBinaryLeaf;
    {* ��ȡ�岮�ڵ㣬Ҳ���Ǹ��ڵ�ĸ��ڵ����һ�ӽڵ�}
    function GetGrandLeaf: TCnBinaryLeaf;
    {* ��ȡ���ڵ�ĸ��ڵ�}

    property Parent: TCnBinaryLeaf read GetParent write SetParent;
    {* ���ڵ�}
    property LeftLeaf: TCnBinaryLeaf read GetLeftLeaf write SetLeftLeaf;
    {* ���ӽڵ㣬ʹ�õ� 0 ���ӽڵ㣬���򷵻� nil������ʱע��ԭ�нڵ㲻���ͷţ���Ҫ���д���}
    property RightLeaf: TCnBinaryLeaf read GetRightLeaf write SetRightLeaf;
    {* ���ӽڵ㣬ʹ�õ� 1 ���ӽڵ㣬���򷵻� nil������ʱע��ԭ�нڵ㲻���ͷţ���Ҫ���д���}

    property Tree: TCnBinaryTree read GetTree;
    {* ��������һ��Ҷ��������һ����}
  end;

  TCnBinaryLeafClass = class of TCnBinaryLeaf;

  TCnBinaryTree = class(TCnTree)
  {* ������ʵ����}
  private
    FOnPostOrderTravelLeaf: TNotifyEvent;
    FOnInOrderTravelLeaf: TNotifyEvent;
    FOnPreOrderTravelLeaf: TNotifyEvent;
    procedure SetRoot(const Value: TCnBinaryLeaf);
  protected
    function GetHeight: Integer; override;  
    function DefaultLeafClass: TCnLeafClass; override;
    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); override;

    function GetRoot: TCnBinaryLeaf;
    function GetCount: Integer;

    procedure DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;

    procedure ReplaceLeaf(ALeaf, AChild: TCnBinaryLeaf);
    {* �� AChild ȡ�� ALeaf ���ڵĽڵ㣬ALeaf ����}

{$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* ��һ VCL �� TreeNode �ڵ��������ӽڵ㣬���ݹ���ã��ϻ������������ӽڵ������ }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ VCL �� TreeNode�����ݹ���� }
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* ��һ FMX �� TreeViewItem �ڵ��������ӽڵ㣬���ݹ���ã��ϻ������������ӽڵ������ }
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ FMX �� TreeViewItem�����ݹ���� }
{$ENDIF}
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnBinaryLeafClass); overload;
    {* ��һ���췽��}

    function AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* ��ָ���ڵ��������ӽڵ㣬���Ѵ����򷵻� nil}
    function AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* ��ָ���ڵ��������ӽڵ㣬���Ѵ����򷵻� nil}
    procedure DeleteLeftChild(AParent: TCnBinaryLeaf);
    {* ɾ��ָ���ڵ�����ӽڵ㣬Ҳ������ nil����ԭ���ӽڵ㲻���ͷ�}
    procedure DeleteRightChild(AParent: TCnBinaryLeaf);
    {* ɾ��ָ���ڵ�����ӽڵ㣬Ҳ������ nil����ԭ���ӽڵ㲻���ͷ�}

{$IFDEF MSWINDOWS}
    // �� TreeView �Ľ���������ע�� Root �����뽻��
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ��һ VCL �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootNode Ϊ nil ��ʾ�Ӹ�ɨ��ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ㡣
    ��������һ�� TreeNode�����һ���ӽڵ���Ϊ���������ڶ�����Ϊ�����������������ĺ���}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ���ڵ�����д��һ VCL �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootNode ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ��һ FMX �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootItem Ϊ nil ��ʾ�Ӹ�ɨ��ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ㡣
    ��������һ�� TreeNode�����һ���ӽڵ���Ϊ���������ڶ�����Ϊ�����������������ĺ���}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ���ڵ�����д��һ FMX �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootItem ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

    function IsFull: Boolean;
    {* �Ƿ����������������еײ�Ҷ�ڵ��ȫ�����Ҳ����ͬ}
    function IsComplete: Boolean;
    {* �Ƿ�����ȫ������}
    function IsBalance: Boolean;
    {* �Ƿ���ƽ�������}

    procedure PreOrderTravel;
    {* �ȸ����������������}
    procedure InOrderTravel;
    {* �и����������������}
    procedure PostOrderTravel;
    {* ������������������}

    property Root: TCnBinaryLeaf read GetRoot write SetRoot;
    {* ���ڵ㣬���Ǵ��ڣ����������������ⲿ����}
    property Count: Integer read GetCount;
    {* �����������нڵ����Ŀ������ Root }
    property Height: Integer read GetHeight;
    {* ���߶ȣ�ֻ�и��ڵ�ʱΪ 1}

    property OnPreOrderTravelLeaf: TNotifyEvent read FOnPreOrderTravelLeaf
      write FOnPreOrderTravelLeaf;
    {* �ȸ��������ʱ�������¼�}
    property OnInOrderTravelLeaf: TNotifyEvent read FOnInOrderTravelLeaf
      write FOnInOrderTravelLeaf;
    {* �и��������ʱ�������¼�}
    property OnPostOrderTravelLeaf: TNotifyEvent read FOnPostOrderTravelLeaf
      write FOnPostOrderTravelLeaf;
    {* ����������ʱ�������¼�}
  end;

//==============================================================================
// �����������ʵ��
//==============================================================================

  TCnBinarySortCompareEvent = function (Value: Integer; Leaf: TCnBinaryLeaf): Integer of object;
  {* ����������ıȽ��¼�}
  TCnBinarySortSetLeafEvent = procedure (Leaf: TCnBinaryLeaf; Value: Integer) of object;
  {* ����������������ӽڵ��¼�}

  TCnBinarySortTree = class(TCnBinaryTree)
  {* �����������ʵ��}
  private
    FRootInserted: Boolean;
    FOnCompare: TCnBinarySortCompareEvent;
    FOnSetLeaf: TCnBinarySortSetLeafEvent;
    procedure CheckCompareSetLeaf;
    function InternalSearch(Leaf: TCnBinaryLeaf; Value: Integer): TCnBinaryLeaf;
    function DefaultOnCompare(Value: Integer; Leaf: TCnBinaryLeaf): Integer;
    procedure DefaultOnSetLeaf(Leaf: TCnBinaryLeaf; Value: Integer);
  protected
    function IsEmpty: Boolean;
    function InternalInsert(Leaf: TCnBinaryLeaf; Value: Integer): TCnBinaryLeaf; virtual;
    function InternalDelete(Leaf: TCnBinaryLeaf; Value: Integer): Boolean; virtual;
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnBinaryLeafClass); overload;
    {* ��һ���췽��}

    procedure Clear; override;
    {* ��������ӽڵ���� Root}

    function Insert(Value: Integer): TCnBinaryLeaf; virtual;
    {* ����ָ��ֵ�Ľڵ㣬����ʱĬ�� Value ��ŵ� Leaf �� Data �����У����ظýڵ�}
    function Search(Value: Integer): TCnBinaryLeaf;
    {* ����ָ��ֵ�Ľڵ㣬δ�ҵ��򷵻� nil}
    function Delete(Value: Integer): Boolean; virtual;
    {* ɾ��ָ��ֵ������ɾ���ɹ����}
    property OnSetLeaf: TCnBinarySortSetLeafEvent read FOnSetLeaf write FOnSetLeaf;
    {* ����ڵ�ʱ��ֵ��ֵ���ڵ�ʱ������Ĭ��Ϊ���� Text �� Data}
    property OnCompare: TCnBinarySortCompareEvent read FOnCompare write FOnCompare;
    {* ����ʱ�Ƚϴ�����Ĭ��Ϊ Value �� Data �Ƚϴ�С}
  end;

//==============================================================================
// �������ʵ��
//==============================================================================

  TCnRedBlackTree = class;

  TCnRedBlackLeaf = class(TCnBinaryLeaf)
  {* ������ڵ����࣬�̳�����ͨ�������ڵ�}
  private
    FIsRed: Boolean;
    function GetLeftLeaf: TCnRedBlackLeaf;
    function GetRightLeaf: TCnRedBlackLeaf;
    procedure SetLeftLeaf(const Value: TCnRedBlackLeaf);
    procedure SetRightLeaf(const Value: TCnRedBlackLeaf);
    function GetTree: TCnRedBlackTree;
    function GetParent: TCnRedBlackLeaf;
    procedure SetParent(const Value: TCnRedBlackLeaf);
  protected
    function GetMostLeftLeaf: TCnRedBlackLeaf;
    {* �ݹ��ȡ����������������ӽڵ�}
    function GetMostRightLeaf: TCnRedBlackLeaf;
    {* �ݹ��ȡ����������������ӽڵ�}
  public
    constructor Create(ATree: TCnTree); override;

    function AddLeftChild: TCnRedBlackLeaf;
    {* �������ӽڵ㣬���Ѵ����򷵻� nil}
    function AddRightChild: TCnRedBlackLeaf;
    {* �������ӽڵ㣬���Ѵ����򷵻� nil}

    function GetMostRightLeafFromLeft: TCnRedBlackLeaf;
    {* ��ȡ����������ҵĽڵ㣬Ҳ�����������ǰ���ڵ�}
    function GetMostLeftLeafFromRight: TCnRedBlackLeaf;
    {* ��ȡ�����������Ľڵ㣬Ҳ����������ĺ����ڵ�}
    function GetBrotherLeaf: TCnRedBlackLeaf;
    {* ��ȡ�ֵܽڵ㣬Ҳ���Ǹ��ڵ����һ�ӽڵ�}
    function GetUncleLeaf: TCnRedBlackLeaf;
    {* ��ȡ�岮�ڵ㣬Ҳ���Ǹ��ڵ�ĸ��ڵ����һ�ӽڵ�}
    function GetGrandLeaf: TCnRedBlackLeaf;
    {* ��ȡ���ڵ�ĸ��ڵ�}

    property Parent: TCnRedBlackLeaf read GetParent write SetParent;
    {* ���ڵ�}
    property LeftLeaf: TCnRedBlackLeaf read GetLeftLeaf write SetLeftLeaf;
    {* ���ӽڵ㣬ʹ�õ� 0 ���ӽڵ㣬���򷵻� nil������ʱע��ԭ�нڵ㲻���ͷţ���Ҫ���д���}
    property RightLeaf: TCnRedBlackLeaf read GetRightLeaf write SetRightLeaf;
    {* ���ӽڵ㣬ʹ�õ� 1 ���ӽڵ㣬���򷵻� nil������ʱע��ԭ�нڵ㲻���ͷţ���Ҫ���д���}

    property IsRed: Boolean read FIsRed write FIsRed;
    {* �ӽڵ���ɫ�Ǻ컹�Ǻ�}
    property Tree: TCnRedBlackTree read GetTree;
    {* ��������һ��Ҷ��������һ����}
  end;

  TCnRedBlackLeafClass = class of TCnRedBlackLeaf;

  TCnRedBlackTree = class(TCnBinarySortTree)
  {* �������ʵ����}
  private
    procedure SetRoot(const Value: TCnRedBlackLeaf);
  protected
    function DefaultLeafClass: TCnLeafClass; override;
    function GetRoot: TCnRedBlackLeaf;
    procedure RotateLeft(ALeaf: TCnRedBlackLeaf);
    {* ��һ���ڵ㼰�����ӽڵ�ʵʩ����}
    procedure RotateRight(ALeaf: TCnRedBlackLeaf);
    {* ��һ���ڵ㼰�����ӽڵ�ʵʩ����}

    procedure InsertRepair(ALeaf: TCnRedBlackLeaf);
    {* ����������}
    procedure InsertRepairCase3(ALeaf: TCnRedBlackLeaf);
    procedure InsertRepairCase4(ALeaf: TCnRedBlackLeaf);
    procedure InsertRepairCase4Step2(ALeaf: TCnRedBlackLeaf);

    procedure DeleteOneChildLeaf(ALeaf: TCnRedBlackLeaf);
    {* ɾ��һ��ֻ��һ���ӽڵ�Ľڵ�}
    procedure DeleteCase1(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase2(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase3(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase4(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase5(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase6(ALeaf: TCnRedBlackLeaf);
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnRedBlackLeafClass); overload;
    {* ��һ���췽��}

    function Insert(Value: Integer): TCnRedBlackLeaf; reintroduce;
    {* ����һ���ڵ㣬���ز���Ľڵ㣬�ڲ��Զ�������ɫ����ת�Ȳ���}
    function Delete(Value: Integer): Boolean; reintroduce;
    {* ɾ��ָ��ֵ������ɾ���ɹ�����ڲ��Զ�������ɫ����ת�Ȳ���}

    property Root: TCnRedBlackLeaf read GetRoot write SetRoot;
    {* ���ڵ㣬���Ǵ���}
  end;

//==============================================================================
// �ֵ�����ʵ��
//==============================================================================

  TCnTrieTree = class;

  TCnTrieLeaf = class(TCnLeaf)
  {* �ֵ�������Ҷ�࣬ʹ�� Data �洢ǰ׺�ַ�}
  private
    FCharacter: Char;
    function GetCharacter: Char;
    procedure SetCharacter(const Value: Char);
    function GetTree: TCnTrieTree;
  protected
    function GetItems(Index: Integer): TCnTrieLeaf;
    procedure SetItems(Index: Integer; const Value: TCnTrieLeaf);

    function DoInsertChar(P: PChar): TCnTrieLeaf;
    function DoSearchChar(P: PChar): TCnTrieLeaf;
  public
    property Character: Char read GetCharacter write SetCharacter;
    property Items[Index: Integer]: TCnTrieLeaf read GetItems write SetItems; default;
    {* ת�������͵�ֱ��Ҷ�ڵ����� }
    property Tree: TCnTrieTree read GetTree;
    {* ת�������͵���������һ��Ҷ��������һ���� }
  end;

  TCnTrieTree = class(TCnTree)
  {* �ֵ���ʵ����}
  private
    FCaseSensitive: Boolean;
    FOnlyChar: Boolean;
    FAnsiFastMode: Boolean;
    function ConvertCharWithCase(C: Char): Char; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* ���ݴ�Сд���ã�����ԭ�ַ����д��ĸ�������Сд��ĸ��}
  protected
    function GetRoot: TCnTrieLeaf;
    function DefaultLeafClass: TCnLeafClass; override;
    function CreateTrieLeaf: TCnTrieLeaf;
  public
    constructor Create(ACaseSensitive: Boolean = True;
      AOnlyChar: Boolean = False; AnAnsiFastMode: Boolean = False); reintroduce; virtual;
    {* �ֵ������캯��������ָ����
      ACaseSensitive���Ƿ����ִ�Сд
      AOnlyChar���ڵ����Ƿ�ֻ�洢��ĸ����Ϊ����ڵ�� Text ���Ի�洢�ַ���ֵ
      AnAnsiFastMode���Ƿ�ʹ�� Ansi ����ģʽ���� Unicode ���뻷���´�������Ч
        ��Ϊ True����ڵ㽫��ǰ���� 256 ���հ��ӽڵ㹩����������������������洢}

    function InsertString(const Str: string): TCnTrieLeaf;
    {* �����ַ��������ز����Ҷ�ڵ㹩����������ݣ�����Ѵ����򷵻� nil}
    function SearchString(const Str: string): TCnTrieLeaf;
    {* �����ַ��������ز��ҵ���Ҷ�ڵ㣬���δ�ҵ��򷵻� nil}
    function StringExists(const Str: string): Boolean;
    {* �����ַ����������Ƿ����}

    property Root: TCnTrieLeaf read GetRoot;
    {* ���ڵ� }
    property OnlyChar: Boolean read FOnlyChar;
    {* �Ƿ�ֻ�洢��ĸ}
    property CaseSensitive: Boolean read FCaseSensitive;
    {* �Ƿ����ִ�Сд��ע�⣬��������ִ�Сд�� OnlyChar Ϊ False��
       ���ҵ��Ľڵ���洢���ַ������ݿ��ܲ����ϴ���ʱ�Ĵ�Сд���}
    property AnsiFastMode: Boolean read FAnsiFastMode;
    {* Ansi ģʽ���Ƿ�Ԥ�ȴ��� 255 ���ӽڵ㹩���ٶ�λ������ʹ����������}
  end;

implementation

//==============================================================================
// TCnBinaryTree
//==============================================================================

constructor TCnBinaryTree.Create;
begin
  inherited;

end;

function TCnBinaryTree.AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.LeftLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.LeftLeaf := Result;
  end
  else
    Result := nil;
end;

function TCnBinaryTree.AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.RightLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.RightLeaf := Result;
  end
  else
    Result := nil;
end;

constructor TCnBinaryTree.Create(LeafClass: TCnBinaryLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnBinaryTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnBinaryLeaf;
end;

function TCnBinaryTree.IsBalance: Boolean;
begin
  if Root = nil then
    Result := True
  else
    Result := Root.IsBalance;
end;

function TCnBinaryTree.IsComplete: Boolean;
var
  Queue: TQueue;
  Node: TCnBinaryLeaf;
begin
  Result := True;
  Queue := TQueue.Create;
  try
    Queue.Push(Root);
    Node := TCnBinaryLeaf(Queue.Pop);
    while Node <> nil do
    begin
      Queue.Push(Node.LeftLeaf);
      Queue.Push(Node.RightLeaf);
      Node := TCnBinaryLeaf(Queue.Pop);
    end;

    // ���й�����ȱ�������һ������ Node �� nil ʱ������һ��Ľڵ�ĺ����ӽڵ��Ѿ������˶���
    // ��ʱ�Ҷ����еķ� nil �㣬����У�˵������ȫ

    if Queue.Count = 0 then // ������б����Ľڵ㶼���� nil��˵������������������
      Exit;

    // ��ʱ���� nil �ˣ��Ҷ������ʣ��ڵ�
    while Queue.Count > 0 do
    begin
      Node := TCnBinaryLeaf(Queue.Pop);
      if Node <> nil then // ������У�������ȫ������
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

function TCnBinaryTree.IsFull: Boolean;
var
  Deep: Integer;
begin
  Deep := MaxLevel + 1;
  Result := Count = Power(2, Deep - 1);
end;

procedure TCnBinaryTree.ValidateComingLeaf(AParent, AChild: TCnLeaf);
begin
  if AParent.Count >= 2 then
    raise ECnBinaryTreeException.Create('Binary TreeNode Can Only Contains 2 Child.');
end;

procedure TCnBinaryTree.DeleteLeftChild(AParent: TCnBinaryLeaf);
begin
  if AParent.Tree = Self then
  begin
    if AParent.LeftLeaf <> nil then
      AParent.LeftLeaf.Parent := nil;
    AParent.LeftLeaf := nil;
  end;
end;

procedure TCnBinaryTree.DeleteRightChild(AParent: TCnBinaryLeaf);
begin
  if AParent.Tree = Self then
  begin
    if AParent.RightLeaf <> nil then
      AParent.RightLeaf.Parent := nil;
    AParent.RightLeaf := nil;
  end;
end;

procedure TCnBinaryTree.DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnInOrderTravelLeaf) then
    FOnInOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPostOrderTravelLeaf) then
    FOnPostOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPreOrderTravelLeaf) then
    FOnPreOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.InOrderTravel;
begin
  Root.DoInOrderTravel;
end;

procedure TCnBinaryTree.PostOrderTravel;
begin
  Root.DoPostOrderTravel;
end;

procedure TCnBinaryTree.PreOrderTravel;
begin
  Root.DoPreOrderTravel;
end;

function TCnBinaryTree.GetRoot: TCnBinaryLeaf;
begin
  Result := TCnBinaryLeaf(inherited GetRoot);
end;

procedure TCnBinaryTree.ReplaceLeaf(ALeaf, AChild: TCnBinaryLeaf);
begin
  AChild.Parent := ALeaf.Parent;
  if ALeaf = ALeaf.Parent.LeftLeaf then
    ALeaf.Parent.LeftLeaf := AChild
  else
    ALeaf.Parent.RightLeaf := AChild;
end;


{$IFDEF MSWINDOWS}

procedure TCnBinaryTree.LoadFromATreeNode(ALeaf: TCnLeaf;
  ANode: TTreeNode);
var
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      if ANode.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeNode(Leaf, ANode.Item[0]);
      end;
      if ANode.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeNode(Leaf, ANode.Item[1]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
var
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.Items.Count > 0 then
    begin
      if RootNode = nil then
        ANode := ATreeView.Items[0]
      else
        ANode := RootNode;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      ANode := ANode.GetNextSibling; // �˲�������һ����̽ڵ㣬��������
      if ANode <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
begin
  inherited SaveToATreeNode(ALeaf, ANode);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootNode, RootLeaf);
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBinaryTree.LoadFromATreeViewItem(ALeaf: TCnLeaf;
  AItem: TTreeViewItem);
var
  Leaf: TCnLeaf;
begin
  if (AItem <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeViewItem(ALeaf, AItem) then
    begin
      if AItem.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[0]);
      end;
      if AItem.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[1]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
var
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.GlobalCount > 0 then
    begin
      if RootItem = nil then
        AItem := ATreeView.Items[0]
      else
        AItem := RootItem;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      AItem := GetNextSiblingItem(AItem); // �˲�������һ����̽ڵ㣬��������
      if AItem <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeViewItem(ALeaf, AItem);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
begin
  inherited SaveToATreeViewItem(ALeaf, AItem);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootItem, RootLeaf);
end;

{$ENDIF}

function TCnBinaryTree.GetCount: Integer;
begin
  Result := Root.AllNonNilCount + 1;
end;

function TCnBinaryTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
end;

//==============================================================================
// TCnBinaryLeaf
//==============================================================================

function TCnBinaryLeaf.AddLeftChild: TCnBinaryLeaf;
begin
  Result := Tree.AddLeftChild(Self);
end;

function TCnBinaryLeaf.AddRightChild: TCnBinaryLeaf;
begin
  Result := Tree.AddRightChild(Self);
end;

constructor TCnBinaryLeaf.Create(ATree: TCnTree);
begin
  if not (ATree is TCnBinaryTree) then
    raise ECnTreeException.Create('Must be Binary Tree.');

  inherited;
  FList.Add(nil);  // ���ӽڵ�
  FList.Add(nil);  // ���ӽڵ�
end;

procedure TCnBinaryLeaf.DeleteLeftChild;
begin
  Tree.DeleteLeftChild(Self);
end;

procedure TCnBinaryLeaf.DeleteRightChild;
begin
  Tree.DeleteRightChild(Self);
end;

procedure TCnBinaryLeaf.DoInOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoInOrderTravel;
  Tree.DoInOrderTravelLeaf(Self);
  if RightLeaf <> nil then
    RightLeaf.DoInOrderTravel;
end;

procedure TCnBinaryLeaf.DoPostOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoPostOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPostOrderTravel;
  Tree.DoPostOrderTravelLeaf(Self);
end;

procedure TCnBinaryLeaf.DoPreOrderTravel;
begin
  Tree.DoPreOrderTravelLeaf(Self);
  if LeftLeaf <> nil then
    LeftLeaf.DoPreOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPreOrderTravel;
end;

function TCnBinaryLeaf.GetBrotherLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Parent <> nil then
  begin
    if Parent.LeftLeaf = Self then
      Result := Parent.RightLeaf
    else if Parent.RightLeaf = Self then
      Result := Parent.LeftLeaf;
  end;
end;

function TCnBinaryLeaf.GetGrandLeaf: TCnBinaryLeaf;
begin
  if GetParent <> nil then
    Result := GetParent.GetParent
  else
    Result := nil;
end;

function TCnBinaryLeaf.GetLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 0 then
    Result := TCnBinaryLeaf(Items[0]);
end;

function TCnBinaryLeaf.GetMostLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if LeftLeaf = nil then
    Exit;

  if LeftLeaf.LeftLeaf = nil then
    Result := LeftLeaf
  else
    Result := LeftLeaf.GetMostLeftLeaf;
end;

function TCnBinaryLeaf.GetMostLeftLeafFromRight: TCnBinaryLeaf;
begin
  if RightLeaf = nil then
    Result := nil
  else if RightLeaf.LeftLeaf = nil then
    Result := RightLeaf
  else
    Result := RightLeaf.GetMostLeftLeaf;
end;

function TCnBinaryLeaf.GetMostRightLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if RightLeaf = nil then
    Exit;

  if RightLeaf.RightLeaf = nil then
    Result := RightLeaf
  else
    Result := RightLeaf.GetMostRightLeaf;
end;

function TCnBinaryLeaf.GetMostRightLeafFromLeft: TCnBinaryLeaf;
begin
  if LeftLeaf = nil then
    Result := nil
  else if LeftLeaf.RightLeaf = nil then
    Result := LeftLeaf
  else
    Result := LeftLeaf.GetMostRightLeaf;
end;

function TCnBinaryLeaf.GetParent: TCnBinaryLeaf;
begin
  Result := TCnBinaryLeaf(inherited GetParent);
end;

function TCnBinaryLeaf.GetRightLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 1 then
    Result := TCnBinaryLeaf(Items[1]);
end;

function TCnBinaryLeaf.GetSubTreeHeight: Integer;
var
  L, R: Integer;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if (LeftLeaf = nil) and (RightLeaf = nil) then
    Result := 0
  else
  begin
    if LeftLeaf = nil then
      L := 0
    else
      L := LeftLeaf.SubTreeHeight;
    if RightLeaf = nil then
      R := 0
    else
      R := RightLeaf.SubTreeHeight;

    Result := Max(L, R) + 1;
  end;
end;

function TCnBinaryLeaf.GetTree: TCnBinaryTree;
begin
  Result := TCnBinaryTree(inherited GetTree);
end;

function TCnBinaryLeaf.GetUncleLeaf: TCnBinaryLeaf;
begin
  if Parent <> nil then
    Result := Parent.GetBrotherLeaf
  else
    Result := nil;
end;

function TCnBinaryLeaf.IsBalance: Boolean;
var
  L, R: Integer;
  LB, RB: Boolean;
begin
  L := 0;
  R := 0;
  LB := True;
  RB := True;

  if LeftLeaf <> nil then
  begin
    L := LeftLeaf.SubTreeHeight;
    LB := LeftLeaf.IsBalance;
  end;
  if RightLeaf <> nil then
  begin
    R := RightLeaf.SubTreeHeight;
    RB := RightLeaf.IsBalance;
  end;

  Result := LB and RB and ((L - R) <= 1) and ((L - R) >= -1);
end;

procedure TCnBinaryLeaf.SetLeftLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if Value = Self then
    raise ECnTreeException.Create('Left Leaf can NOT be Self.');

//  if (Value <> Items[0]) and (Items[0] <> nil) then
//  begin
//    // �ɽڵ�Ҫ��Ҫ�ͷţ�����ɽڵ���Ҷ�ӽڵ㣬ֱ���ͷ�û���⣬����������ͷ�
//    if ((Items[0] as TCnBinaryLeaf).LeftLeaf = nil) and
//      ((Items[0] as TCnBinaryLeaf).RightLeaf = nil) then
//      Items[0].Free; // �ͷžɽڵ�
//  end;

  Items[0] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

procedure TCnBinaryLeaf.SetParent(const Value: TCnBinaryLeaf);
begin
  if Value <> FParent then
    FParent := Value;
end;

procedure TCnBinaryLeaf.SetRightLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if Value = Self then
    raise ECnTreeException.Create('Right Leaf can NOT be Self.');

//  if (Value <> Items[1]) and (Items[1] <> nil) then
//  begin
//    // �ɽڵ�Ҫ��Ҫ�ͷţ�����ɽڵ���Ҷ�ӽڵ㣬ֱ���ͷ�û���⣬����������ͷ�
//    if ((Items[1] as TCnBinaryLeaf).LeftLeaf = nil) and
//      ((Items[1] as TCnBinaryLeaf).RightLeaf = nil) then
//      Items[1].Free; // �ͷžɽڵ�
//  end;

  Items[1] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

//==============================================================================
// TCnTrieLeaf
//==============================================================================

function TCnTrieLeaf.DoInsertChar(P: PChar): TCnTrieLeaf;
var
  C, CaseC: Char;
  I, Idx, Gt: Integer;
  Leaf: TCnTrieLeaf;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  C := P^;
  CaseC := Tree.ConvertCharWithCase(C);

  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] = nil then // �޴���ĸ��Ӧ�ӽڵ㣬ֱ�Ӵ���
    begin
      Leaf := Tree.CreateTrieLeaf;
      Leaf.Character := CaseC;
      Items[I] := Leaf;

      if not Tree.OnlyChar then
        Leaf.Text := Leaf.Parent.Text + C;

      Inc(P);
      if P^ = #0 then
        Result := Leaf
      else
        Result := Leaf.DoInsertChar(P);
    end
    else
    begin
      Inc(P);
      if P^ = #0 then // �������ַ����Ѿ�����
        Result := nil
      else
        Result := Items[I].DoInsertChar(P);
    end;
    Exit;
  end;

  if Count = 0 then // ���ӽڵ㣬ֱ�Ӵ���
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC;
    AddChild(Leaf);
    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C;

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
    Exit;
  end;

  Idx := -1;
  Gt := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Character = CaseC then
    begin
      Idx := I;
      Break;
    end
    else if Items[I].Character > CaseC then
    begin
      Gt := I;
      Break;
    end;
  end;

  if Idx >= 0 then // �ҵ�������ַ��Ľڵ�
  begin
    Inc(P);
    if P^ = #0 then // �������ַ����Ѿ�����
      Result := nil
    else
      Result := Items[Idx].DoInsertChar(P);
  end
  else // û����ַ��Ľڵ㣬Ҫ����
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC; // �����Сд�����У��� Character �д洢��д��ĸ

    if Gt = -1 then  // û�б����ַ���Ľڵ㣬��������
      AddChild(Leaf)
    else
      InsertChild(Leaf, Gt); // ������ڵ�һ��������ַ���Ľڵ��ǰ��

    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C; // ��ʵ���ֱ���ԭ��

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
  end;
end;

function TCnTrieLeaf.DoSearchChar(P: PChar): TCnTrieLeaf;
var
  CaseC: Char;
  I: Integer;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  CaseC := Tree.ConvertCharWithCase(P^);
  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] <> nil then
    begin
      Inc(P);
      if P^ = #0 then
        Result := Items[I]
      else
        Result := Items[I].DoSearchChar(P);
    end;
  end
  else
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].Character = CaseC then
      begin
        Inc(P);
        if P^ = #0 then
          Result := Items[I]
        else
          Result := Items[I].DoSearchChar(P);
      end;
    end;
  end;
end;

function TCnTrieLeaf.GetCharacter: Char;
begin
  Result := FCharacter;
end;

function TCnTrieLeaf.GetItems(Index: Integer): TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetItems(Index));
end;

function TCnTrieLeaf.GetTree: TCnTrieTree;
begin
  Result := TCnTrieTree(inherited GetTree);
end;

procedure TCnTrieLeaf.SetCharacter(const Value: Char);
begin
  FCharacter := Value;
end;

procedure TCnTrieLeaf.SetItems(Index: Integer; const Value: TCnTrieLeaf);
begin
  inherited SetItems(Index, Value);
end;

//==============================================================================
// TCnTrieTree �ֵ�������
//==============================================================================

function TCnTrieTree.ConvertCharWithCase(C: Char): Char;
begin
  Result := C;
  if not FCaseSensitive and
    {$IFDEF COMPILER12_UP}(Ord(C) <= $FF) and{$ENDIF}
    (AnsiChar(C) in ['a'..'z']) then
    Dec(Result, 32);
end;

constructor TCnTrieTree.Create(ACaseSensitive: Boolean; AOnlyChar: Boolean;
  AnAnsiFastMode: Boolean);
var
  I: Char;
begin
  inherited Create(TCnTrieLeaf);
  FCaseSensitive := ACaseSensitive;
  FOnlyChar := AOnlyChar;

{$IFDEF UNICODE}
  FAnsiFastMode := False;
{$ELSE}
  FAnsiFastMode := AnAnsiFastMode;
{$ENDIF}
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // Ԥ�ȼ��� 256 ��������
      Root.FList.Add(nil);
end;

function TCnTrieTree.CreateTrieLeaf: TCnTrieLeaf;
var
  I: Char;
begin
  Result := TCnTrieLeaf(CreateLeaf(Self));
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // Ԥ�ȼ��� 256 ��������
      Result.FList.Add(nil);
end;

function TCnTrieTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnTrieLeaf;
end;

function TCnTrieTree.GetRoot: TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetRoot);
end;

function TCnTrieTree.InsertString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoInsertChar(PChar(Str));
end;

function TCnTrieTree.SearchString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoSearchChar(PChar(Str));
end;

function TCnTrieTree.StringExists(const Str: string): Boolean;
begin
  Result := (SearchString(Str) <> nil);
end;

{ TCnBinarySortTree }

constructor TCnBinarySortTree.Create;
begin
  inherited;
  FOnCompare := DefaultOnCompare;
  FOnSetLeaf := DefaultOnSetLeaf;
end;

procedure TCnBinarySortTree.CheckCompareSetLeaf;
begin
  if not Assigned(FOnCompare) then
    raise ECnTreeException.Create('No OnCompare Event, can NOT Continue.');
  if not Assigned(FOnSetLeaf) then
    raise ECnTreeException.Create('No OnSetLeaf Event, can NOT Continue.');
end;

constructor TCnBinarySortTree.Create(LeafClass: TCnBinaryLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnBinarySortTree.DefaultOnCompare(Value: Integer;
  Leaf: TCnBinaryLeaf): Integer;
begin
  Result := Value - Leaf.Data;
end;

function TCnBinarySortTree.Insert(Value: Integer): TCnBinaryLeaf;
begin
  CheckCompareSetLeaf;
  if not FRootInserted then
  begin
    FOnSetLeaf(Root, Value);
    FRootInserted := True;
    Result := Root;
  end
  else
    Result := InternalInsert(Root, Value);
end;

function TCnBinarySortTree.InternalInsert(Leaf: TCnBinaryLeaf;
  Value: Integer): TCnBinaryLeaf;
var
  Cmp: Integer;
begin
  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
    raise ECnTreeException.CreateFmt('Value %d already Exists.', [Value])
  else if Cmp < 0 then // Value < Leaf
  begin
    if Leaf.LeftLeaf = nil then
    begin
      Result := Leaf.AddLeftChild;
      FOnSetLeaf(Result, Value);
    end
    else
      Result := InternalInsert(Leaf.LeftLeaf, Value);
  end
  else // Value > Leaf
  begin
    if Leaf.RightLeaf = nil then
    begin
      Result := Leaf.AddRightChild;
      FOnSetLeaf(Result, Value);
    end
    else
      Result := InternalInsert(Leaf.RightLeaf, Value);
  end;
end;

function TCnBinarySortTree.InternalSearch(Leaf: TCnBinaryLeaf;
  Value: Integer): TCnBinaryLeaf;
var
  Cmp: Integer;
begin
  if Leaf = nil then
  begin
    Result := nil;
    Exit;
  end;

  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
    Result := Leaf
  else if Cmp < 0 then // Value < Leaf
    Result := InternalSearch(Leaf.LeftLeaf, Value)
  else // Value > Leaf
    Result := InternalSearch(Leaf.RightLeaf, Value);
end;

function TCnBinarySortTree.IsEmpty: Boolean;
begin
  Result := FRootInserted or (GetCount > 1);
end;

function TCnBinarySortTree.Search(Value: Integer): TCnBinaryLeaf;
begin
  CheckCompareSetLeaf;
  if FRootInserted then
    Result := InternalSearch(Root, Value)
  else
    Result := nil;
end;

procedure TCnBinarySortTree.DefaultOnSetLeaf(Leaf: TCnBinaryLeaf;
  Value: Integer);
begin
  Leaf.Text := IntToStr(Value);
  Leaf.Data := Value;
end;

function TCnBinarySortTree.Delete(Value: Integer): Boolean;
begin
  CheckCompareSetLeaf;
  if FRootInserted then
  begin
    if (GetCount = 1) and (FOnCompare(Value, Root) = 0) then
    begin
      Result := True;
      FRootInserted := False; // Root �ڵ㲻ɾ��ֻ��һ��δ��ʼ�����
    end
    else
      Result := InternalDelete(Root, Value);
  end
  else
    Result := False;
end;

function TCnBinarySortTree.InternalDelete(Leaf: TCnBinaryLeaf;
  Value: Integer): Boolean;
var
  Cmp: Integer;
  AParent, ALeaf: TCnBinaryLeaf;
  IsParent: Boolean;
begin
  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
  begin
    Result := True;
    AParent := Leaf.Parent;
    // �ҵ��ˣ�Ҫɾ Leaf����Ҫע�������������Ĵ���״��
    if (Leaf.LeftLeaf = nil) and (Leaf.RightLeaf = nil) then // Ҷ�ڵ�ֱ��ɾ
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.DeleteLeftChild
        else if AParent.RightLeaf = Leaf then
          AParent.DeleteRightChild;
      end
      else
      begin
        // Leaf �� Root ��ֻ�� Root��ɾ�����ܴ�������˴˴���
        FRootInserted := False;
      end;
    end
    else if Leaf.LeftLeaf = nil then // Left �գ������������������Լ�ȡ�����������ͷ��Լ�
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.LeftLeaf := Leaf.RightLeaf
        else if AParent.RightLeaf = Leaf then
          AParent.RightLeaf := Leaf.RightLeaf;
      end
      else
      begin
        // Leaf �� Root����������Ϊ Root
        Root := Leaf.RightLeaf;
      end;

      Leaf.RightLeaf := nil;
      Leaf.Free;
    end
    else if Leaf.RightLeaf = nil then // Right �գ������������������Լ�ȡ�����������ͷ��Լ�
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.LeftLeaf := Leaf.LeftLeaf
        else if AParent.RightLeaf = Leaf then
          AParent.RightLeaf := Leaf.LeftLeaf;
      end
      else
      begin
        // Leaf �� Root����������Ϊ Root
        Root := Leaf.LeftLeaf;
      end;

      Leaf.LeftLeaf := nil;
      Leaf.Free;
    end
    else // ���Ҷ����գ����������Ƚ��鷳
    begin
      ALeaf := Leaf.GetMostRightLeafFromLeft;
      if ALeaf <> nil then
      begin
        IsParent := ALeaf.Parent = Leaf;

        // �ҵ� Leaf �����������ǰ���ڵ㣬ȡ�� Leaf��ע��ǰ���ڵ㲻���������������Ҳ��� Leaf ���ҽڵ�
        if ALeaf.LeftLeaf <> nil then
        begin
          if ALeaf.Parent.LeftLeaf = ALeaf then
          begin
            // ǰ���ڵ�Ҫɾ��ǰ���ڵ����丸��������ʱ����ǰ���ڵ���������ҵ�ǰ���ڵ�ĸ��ڵ�����
            ALeaf.Parent.LeftLeaf := ALeaf.LeftLeaf;
            ALeaf.LeftLeaf := nil;
          end
          else if ALeaf.Parent.RightLeaf = ALeaf then
          begin
            // ǰ���ڵ�Ҫɾ��ǰ���ڵ����丸��������ʱ����ǰ���ڵ���������ҵ�ǰ���ڵ�ĸ��ڵ�����
            ALeaf.Parent.RightLeaf := ALeaf.LeftLeaf;
            ALeaf.LeftLeaf := nil;
          end;
        end;

        // ���� ALeaf ǰ���ڵ���滻 Leaf ���ڵ�λ��
        if AParent <> nil then
        begin
          if AParent.LeftLeaf = Leaf then
            AParent.LeftLeaf := ALeaf
          else if AParent.RightLeaf = Leaf then
            AParent.RightLeaf := ALeaf;
        end
        else
        begin
          // ԭ�� Leaf �� Root����Ϊ ALeaf
          Root := ALeaf;
        end;

        // ��ԭ�� Leaf �������ҵ� ALeaf �£���� ALeaf ���� Leaf ���������Ļ�
        if not IsParent then
          ALeaf.LeftLeaf := Leaf.LeftLeaf;
        ALeaf.RightLeaf := Leaf.RightLeaf;

        // �ͷŴ˱�ɾ���� Leaf �ڵ�
        Leaf.LeftLeaf := nil;
        Leaf.RightLeaf := nil;
        Leaf.Free;
      end
      else // �ú����ڵ��滻 Leaf
      begin
        ALeaf := Leaf.GetMostLeftLeafFromRight;
        if ALeaf <> nil then
        begin
          IsParent := ALeaf.Parent = Leaf;

          // �ҵ� Leaf ����������ĺ����ڵ㣬ȡ�� Leaf��ע������ڵ㲻���������������Ҳ��� Leaf ����ڵ�
          if ALeaf.RightLeaf <> nil then
          begin
            if ALeaf.Parent.LeftLeaf = ALeaf then
            begin
              // �����ڵ�Ҫɾ�������ڵ����丸��������ʱ���Ѻ����ڵ���������ҵ�ǰ���ڵ�ĸ��ڵ�����
              ALeaf.Parent.LeftLeaf := ALeaf.RightLeaf;
              ALeaf.RightLeaf := nil;
            end
            else if ALeaf.Parent.RightLeaf = ALeaf then
            begin
              // �����ڵ�Ҫɾ�������ڵ����丸��������ʱ���Ѻ����ڵ���������ҵ�ǰ���ڵ�ĸ��ڵ�����
              ALeaf.Parent.RightLeaf := ALeaf.RightLeaf;
              ALeaf.RightLeaf := nil;
            end;
          end;

          // ���� ALeaf ǰ���ڵ���滻 Leaf ���ڵ�λ��
          if AParent <> nil then
          begin
            if AParent.LeftLeaf = Leaf then
              AParent.LeftLeaf := ALeaf
            else if AParent.RightLeaf = Leaf then
              AParent.RightLeaf := ALeaf;
          end
          else
          begin
            // ԭ�� Leaf �� Root����Ϊ ALeaf
            Root := ALeaf;
          end;

          // ��ԭ�� Leaf �������ҵ� ALeaf �£���� ALeaf ���� Leaf ��ֱ�������Ļ�
          ALeaf.LeftLeaf := Leaf.LeftLeaf;
          if not IsParent then
            ALeaf.RightLeaf := Leaf.RightLeaf;

          // �ͷŴ˱�ɾ���� Leaf �ڵ�
          Leaf.LeftLeaf := nil;
          Leaf.RightLeaf := nil;
          Leaf.Free;
        end
      end;
    end;
  end
  else if Cmp < 0 then
  begin
    if Leaf.LeftLeaf = nil then
      Result := False // �Ҳ���
    else
      Result := InternalDelete(Leaf.LeftLeaf, Value);
  end
  else // Value > Leaf
  begin
    if Leaf.RightLeaf = nil then // �Ҳ���
      Result := False
    else
      Result := InternalDelete(Leaf.RightLeaf, Value);
  end;
end;

procedure TCnBinarySortTree.Clear;
begin
  inherited;
  FRootInserted := False;
end;

procedure TCnBinaryTree.SetRoot(const Value: TCnBinaryLeaf);
begin
  if Value <> FRoot then
  begin
    FRoot := Value;
    Root.Parent := nil;
  end;
end;

{ TCnRedBlackLeaf }

function TCnRedBlackLeaf.AddLeftChild: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited AddLeftChild);
end;

function TCnRedBlackLeaf.AddRightChild: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited AddRightChild);
end;

constructor TCnRedBlackLeaf.Create(ATree: TCnTree);
begin
  if not (ATree is TCnRedBlackTree) then
    raise ECnTreeException.Create('Must be RedBlack Tree.');

  inherited;
end;

function TCnRedBlackLeaf.GetBrotherLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetBrotherLeaf);
end;

function TCnRedBlackLeaf.GetGrandLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetGrandLeaf);
end;

function TCnRedBlackLeaf.GetLeftLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetLeftLeaf);
end;

function TCnRedBlackLeaf.GetMostLeftLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostLeftLeaf);
end;

function TCnRedBlackLeaf.GetMostLeftLeafFromRight: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostLeftLeafFromRight);
end;

function TCnRedBlackLeaf.GetMostRightLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostRightLeaf);
end;

function TCnRedBlackLeaf.GetMostRightLeafFromLeft: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostRightLeafFromLeft);
end;

function TCnRedBlackLeaf.GetParent: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetParent);
end;

function TCnRedBlackLeaf.GetRightLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetRightLeaf);
end;

function TCnRedBlackLeaf.GetTree: TCnRedBlackTree;
begin
  Result := TCnRedBlackTree(inherited GetTree);
end;

function TCnRedBlackLeaf.GetUncleLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetUncleLeaf);
end;

procedure TCnRedBlackLeaf.SetLeftLeaf(const Value: TCnRedBlackLeaf);
begin
  inherited SetLeftLeaf(Value);
end;

procedure TCnRedBlackLeaf.SetParent(const Value: TCnRedBlackLeaf);
begin
  inherited SetParent(Value);
end;

procedure TCnRedBlackLeaf.SetRightLeaf(const Value: TCnRedBlackLeaf);
begin
  inherited SetRightLeaf(Value);
end;

{ TCnRedBlackTree }

constructor TCnRedBlackTree.Create;
begin
  inherited;

end;

constructor TCnRedBlackTree.Create(LeafClass: TCnRedBlackLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnRedBlackTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnRedBlackLeaf;
end;

function TCnRedBlackTree.Delete(Value: Integer): Boolean;
var
  ALeaf: TCnRedBlackLeaf;
begin
  ALeaf := TCnRedBlackLeaf(Search(Value));
  Result := ALeaf <> nil;
  if Result then
    DeleteOneChildLeaf(ALeaf); // Error
end;

procedure TCnRedBlackTree.DeleteCase1(ALeaf: TCnRedBlackLeaf);
begin
  if ALeaf.Parent <> nil then
    DeleteCase2(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase2(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if Brother.IsRed then
  begin
    ALeaf.Parent.IsRed := True;
    Brother.IsRed := False;
    if ALeaf = ALeaf.Parent.LeftLeaf then
      RotateLeft(ALeaf.Parent)
    else
      RotateRight(ALeaf.Parent)
  end;

  DeleteCase3(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase3(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if not ALeaf.Parent.IsRed and not Brother.IsRed and not Brother.LeftLeaf.IsRed
    and not Brother.RightLeaf.IsRed then
  begin
    Brother.IsRed := True;
    DeleteCase1(ALeaf.Parent);
  end
  else
    DeleteCase4(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase4(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if ALeaf.Parent.IsRed and not Brother.IsRed and not Brother.LeftLeaf.IsRed
    and not Brother.RightLeaf.IsRed then
  begin
    Brother.IsRed := True;
    ALeaf.Parent.IsRed := False;
  end
  else
    DeleteCase5(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase5(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if not Brother.IsRed then
  begin
    if (ALeaf = ALeaf.Parent.LeftLeaf) and not Brother.RightLeaf.IsRed
      and Brother.LeftLeaf.IsRed then
    begin
      Brother.IsRed := True;
      Brother.LeftLeaf.IsRed := False;
      RotateRight(Brother);
    end
    else if (ALeaf = ALeaf.Parent.RightLeaf) and not Brother.LeftLeaf.IsRed
      and Brother.RightLeaf.IsRed then
    begin
      Brother.IsRed := True;
      Brother.RightLeaf.IsRed := False;
      RotateLeft(Brother);
    end;
  end;

  DeleteCase6(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase6(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  Brother.IsRed := ALeaf.Parent.IsRed;
  ALeaf.Parent.IsRed := False;

  if ALeaf = ALeaf.Parent.LeftLeaf then
  begin
    Brother.RightLeaf.IsRed := False;
    RotateLeft(ALeaf.Parent);
  end
  else
  begin
    Brother.LeftLeaf.IsRed := False;
    RotateRight(ALeaf.Parent);
  end;
end;

procedure TCnRedBlackTree.DeleteOneChildLeaf(ALeaf: TCnRedBlackLeaf);
var
  Child: TCnRedBlackLeaf;
begin
  Child := ALeaf.LeftLeaf;
  if Child = nil then
    Child := ALeaf.RightLeaf;

  if Child = nil then
    raise ECnTreeException.Create('ALeaf has NO Child.');

  ReplaceLeaf(ALeaf, Child);
  if not ALeaf.IsRed then
  begin
    if Child.IsRed then
      Child.IsRed := False
    else
      DeleteCase1(Child);
  end;

  ALeaf.Free; // ALeaf �Ѱ��룬����ֱ��ɾ��
end;

function TCnRedBlackTree.GetRoot: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetRoot);
end;

function TCnRedBlackTree.Insert(Value: Integer): TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited Insert(Value));
  if Result <> nil then
  begin
    Result.IsRed := True;
    InsertRepair(Result);
  end;
end;

procedure TCnRedBlackTree.InsertRepair(ALeaf: TCnRedBlackLeaf);
begin
  if (ALeaf.Parent = nil) and (Root = ALeaf) then
    ALeaf.IsRed := False    // ���ڵ�ֱ��Ⱦ��
  else if not ALeaf.Parent.IsRed then
    ALeaf.IsRed := True
  else if (ALeaf.GetUncleLeaf <> nil) and ALeaf.GetUncleLeaf.IsRed then
    InsertRepairCase3(ALeaf)
  else
    InsertRepairCase4(ALeaf);
end;

procedure TCnRedBlackTree.InsertRepairCase3(ALeaf: TCnRedBlackLeaf);
begin
  ALeaf.Parent.IsRed := False;
  ALeaf.GetUncleLeaf.IsRed := False;
  if ALeaf.GetGrandLeaf <> nil then
  begin
    ALeaf.GetGrandLeaf.IsRed := True;
    InsertRepair(ALeaf.GetGrandLeaf);
  end;
end;

procedure TCnRedBlackTree.InsertRepairCase4(ALeaf: TCnRedBlackLeaf);
var
  P, G: TCnRedBlackLeaf;
begin
  P := ALeaf.Parent;
  G := ALeaf.GetGrandLeaf;

  if (ALeaf = P.RightLeaf) and (P = G.GetLeftLeaf) then
  begin
    RotateLeft(P);
    ALeaf := ALeaf.LeftLeaf;
  end
  else if (ALeaf = P.LeftLeaf) and (P = G.GetRightLeaf) then
  begin
    RotateRight(P);
    ALeaf := ALeaf.RightLeaf;
  end;
  InsertRepairCase4Step2(ALeaf);
end;

procedure TCnRedBlackTree.InsertRepairCase4Step2(ALeaf: TCnRedBlackLeaf);
var
  P, G: TCnRedBlackLeaf;
begin
  P := ALeaf.Parent;
  G := ALeaf.GetGrandLeaf;

  if ALeaf = P.LeftLeaf then
    RotateRight(G)
  else
    RotateLeft(G);

  P.IsRed := False;
  G.IsRed := True;
end;

procedure TCnRedBlackTree.RotateLeft(ALeaf: TCnRedBlackLeaf);
var
  Right: TCnRedBlackLeaf;
begin
  // ALeaf �����ӽڵ�ȡ���Լ���ALeaf ������ӽڵ㣬ԭ���ӽڵ�����ӽڵ��� ALeaf �����ӽڵ�
  if ALeaf = nil then
    Exit;
  if ALeaf.RightLeaf = nil then
    Exit;

  Right := ALeaf.RightLeaf;
  ALeaf.RightLeaf := Right.LeftLeaf;
  if ALeaf.Parent <> nil then
  begin
    if ALeaf.Parent.LeftLeaf <> ALeaf then
      raise ECnTreeException.Create('Rotate Left Failed');

    ALeaf.Parent.LeftLeaf := Right;
  end
  else if Root = ALeaf then // ��� ALeaf �Ǹ��ڵ㣬Ҫ������ڵ�
    Root := Right;
  Right.LeftLeaf := ALeaf;
end;

procedure TCnRedBlackTree.RotateRight(ALeaf: TCnRedBlackLeaf);
var
  Left: TCnRedBlackLeaf;
begin
  // ALeaf �����ӽڵ�ȡ���Լ���ALeaf ������ӽڵ㣬ԭ���ӽڵ�����ӽڵ��� ALeaf �����ӽڵ�
  if ALeaf = nil then
    Exit;
  if ALeaf.LeftLeaf = nil then
    Exit;

  Left := ALeaf.LeftLeaf;
  ALeaf.LeftLeaf := Left.RightLeaf;
  if ALeaf.Parent <> nil then
  begin
    if ALeaf.Parent.RightLeaf <> ALeaf then
      raise ECnTreeException.Create('Rotate Right Failed');

    ALeaf.Parent.RightLeaf := Left;
  end
  else if Root = ALeaf then // ��� ALeaf �Ǹ��ڵ㣬Ҫ������ڵ�
    Root := Left;
  Left.RightLeaf := ALeaf;
end;

procedure TCnRedBlackTree.SetRoot(const Value: TCnRedBlackLeaf);
begin
  inherited SetRoot(Value);
  if Value <> nil then
    Value.IsRed := False; // ���ڵ��ɫ
end;

end.
