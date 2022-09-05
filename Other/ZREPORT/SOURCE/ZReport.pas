unit ZReport;

interface

{$I ZRDefine.inc}

uses
  Windows, Messages,                               // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes, Graphics, Controls, Forms,              // Delphi VCL
  Dialogs, Buttons, ExtCtrls, DB,
{$IFDEF D6Above}
  Variants,
{$ENDIF}
  ZREscape, ZREval, ZRFormat, ZRPrntr;             // ZReport

const
  ZR_BASE   = WM_USER + 100;
  ZR_NOTIFY = ZR_BASE + 0;

type
  TZROperation  = (zopInsert, zopRemove, zopRename, zopSort, zopChild, zopDataSet);

  TZRNotify = packed record
    Msg       : Cardinal;
    Operation : TZROperation;
    Reserved1 : Byte;
    Reserved2 : Word;
    Sender    : TComponent;
    Result    : Longint;
  end;

type
  TZRBandType = (
    zbtPageHeader  ,
    zbtHeader      ,
    zbtColumnHeader,
    zbtGroupHeader ,
    zbtDetailHeader,
    zbtDetail      ,
    zbtDetailFooter,
    zbtGroupFooter ,
    zbtColumnFooter,
    zbtFooter      ,
    zbtPageFooter  ,
    zbtChild       ,
    zbtController  );
  TZRBandTypes = set of TZRBandType;

  { Forward declarations }
  TZReportControl     = class;
  TZRGroup            = class;
  TZRCustomBand       = class;
  TZRCustomController = class;
  TZReport            = class;

  { TZRFrame }
  TZRFrame = class(TPersistent)
  private
    fOwner  : TZReportControl;
    fValues : array[0..3] of Integer;
    function  GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; Value: Integer);
  protected
    procedure Draw;
    procedure Print(aLeft, aTop, aWidth, aHeight: Integer);
    property Owner : TZReportControl read fOwner;
  public
    constructor Create(aOwner: TZReportControl);
    procedure Assign(Source: TPersistent); override;
    property Width  : Integer index 4 read GetValue;
    property Height : Integer index 5 read GetValue;
  published
    property Left   : Integer index 0 read GetValue write SetValue default 0;
    property Top    : Integer index 1 read GetValue write SetValue default 0;
    property Right  : Integer index 2 read GetValue write SetValue default 0;
    property Bottom : Integer index 3 read GetValue write SetValue default 0;
  end;

  { TZReportControl }
  TZRAlign      = (zalNone, zalLeft, zalTop, zalRight, zalBottom, zalWidth, zalHeight);
  TZRFontStyle  = (zfsBold, zfsItalic, zfsUnderline, zfsSuperScript, zfsSubScript);
  TZRFontStyles = set of TZRFontStyle;

  TZRBeforePrintEvent = procedure (Sender : TObject; var DoPrint : Boolean) of object;
  TZRAfterPrintEvent  = TNotifyEvent;

  TZReportControl = class(TCustomControl)
  private
    fReport         : TZReport;
    fBand           : TZRCustomBand;

    fLeft,
    fTop,
    fWidth,
    fHeight         : Integer;

    fFrame          : TZRFrame;
    fAlign          : TZRAlign;
    fFontStyles     : TZRFontStyles;
    fEnabled        : Boolean;
    fVisible        : Boolean;

    fIsUpdating     : Boolean;
    fIsPrinting     : Boolean;

    pDoPrint        : Boolean;
    pLeft,
    pTop,
    pWidth,
    pHeight         : Integer;

    fBeforePrint    : TZRBeforePrintEvent;
    fAfterPrint     : TZRAfterPrintEvent;

    function  GetBound(Index: Integer): Integer;
    procedure SetBound(Index: Integer; Value: Integer);

    procedure SetAlign(Value: TZRAlign);
    procedure SetFontStyles(Value: TZRFontStyles);
    procedure SetFrame(Value: TZRFrame);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ZRNotify(var Message: TZRNotify); message ZR_NOTIFY;
    procedure Notify(Operation: TZROperation);
  protected
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(aParent: TWinControl); override;

    procedure Draw; dynamic;

    function CharSize: TPoint;

    procedure AdjustBounds; dynamic;
    procedure AlignBounds; dynamic;
    procedure RequestBounds; dynamic;
    procedure UpdateBounds;

    procedure Prepare; dynamic;
    procedure Unprepare; dynamic;
    procedure PrintString(X, Y: Integer; const S: String);
    procedure DoBeforePrint; dynamic;
    procedure DoAfterPrint; dynamic;
    procedure DoPrint(OfsX, OfsY: Integer); dynamic;

    property IsPrinting: Boolean read fIsPrinting;
    property IsUpdating: Boolean read fIsUpdating;

    property BeforePrint : TZRBeforePrintEvent read fBeforePrint write fBeforePrint;
    property AfterPrint  : TZRAfterPrintEvent  read fAfterPrint  write fAfterPrint;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    function FramedWidth : Integer;
    function FramedHeight: Integer;
    function FramedClientRect: TRect;

    property Band  : TZRCustomBand read fBand;
    property Report: TZReport      read fReport;

    property Right : Integer index 4 read GetBound stored False;
    property Bottom: Integer index 5 read GetBound stored False;

    property Align      : TZRAlign      read fAlign      write SetAlign      default zalNone;
    property FontStyles : TZRFontStyles read fFontStyles write SetFontStyles default [];
    property Enabled    : Boolean       read fEnabled    write fEnabled default True;
    property Visible    : Boolean       read fVisible    write fVisible default True;
    property Frame      : TZRFrame      read fFrame      write SetFrame;
  published
    property Left   : Integer index 0 read GetBound write SetBound;
    property Top    : Integer index 1 read GetBound write SetBound;
    property Width  : Integer index 2 read GetBound write SetBound;
    property Height : Integer index 3 read GetBound write SetBound;
  end;

  { TZRVariable }
  TZRVariableStatus = (zvsPrepare, zvsReady, zvsNeedUpdate, zvsUpdating);

  TZRVariable = class(TComponent)
  private
    fPrev,
    fValue  : Variant;
    fStatus : TZRVariableStatus;
    fFormat : TZFormat;
    fMaster : TZRCustomController;
    function  GetText : String; virtual;
    function  GetValue: Variant;
    procedure SetFormat(const Value: TZFormat);
    procedure SetMaster(Value : TZRCustomController); virtual;
    procedure Notify(Operation: TZROperation);
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    function  EvaluateValue : Variant; virtual;
    procedure ResetValue; virtual;
    function  GetIsEmpty   : Boolean;
    function  GetIsRepeated: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function HasParent : Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure Clear;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    procedure Update; virtual;
    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure Reset; virtual;
    property IsRepeated: Boolean read GetIsRepeated;
    property Master: TZRCustomController read fMaster write SetMaster;
    property Status: TZRVariableStatus read fStatus;
    property Text : String read GetText;
    property Value : Variant read GetValue;
  published
    property Format: TZFormat read fFormat write SetFormat;
  end;

  TZRVariableClass = class of TZRVariable;

  { TZRExpression }
  TZRGetValueEvent = procedure(Sender: TZRVariable; var Value: Variant) of object;

  TZRExpression = class(TZRVariable)
  private
    fEvaluator  : TZREvaluator;
    fExpression : String;
    fOnGetValue : TZRGetValueEvent;
    function  GetText : String; override;
    procedure SetExpression(const Value: String);
    function  Prepared : Boolean;
    function  UDF: Boolean;
  protected
    function  EvaluateValue : Variant; override;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  published
    property Expression: String read fExpression write SetExpression;
    property OnGetValue: TZRGetValueEvent read fOnGetValue write fOnGetValue;
  end;

  { TZRField }
  TZRField = class(TZRVariable)
  private
    fDataSet   : TDataSet;
    fDataField : String;
    fFieldNo   : Integer;
    fField     : TField;
    function  GetText : String; override;
    procedure SetDataField(const Value: String);
    procedure SetDataSet(Value : TDataSet);
    procedure SetMaster(Value : TZRCustomController); override;
  protected
    function EvaluateValue : Variant; override;
    procedure Notification(Component : TComponent; Operation : TOperation); override;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  published
    property DataField : String read fDataField write SetDataField;
    property DataSet   : TDataSet read fDataSet write SetDataSet;
  end;

  { TZRAggregator }
  TZRAggregator = class(TZRVariable)
  private
    fLevels    : Integer;
    fCount     : array of Integer;
    fPrevCount : array of Integer;
    fSum,
    fMin,
    fMax,
    fPrevSum,
    fPrevMin,
    fPrevMax   : array of Variant;
    fVariable  : TZRVariable;
    function  GetText : String; override;
    function GetCount(Index : Integer): Integer;
    function GetMin(Index : Integer): Variant;
    function GetMax(Index : Integer): Variant;
    function GetSum(Index : Integer): Variant;
    function GetAvg(Index : Integer): Variant;
    procedure SetVariable(Value : TZRVariable);
  protected
    function  EvaluateValue : Variant; override;
    procedure ResetValue; override;
  public
    procedure ResetValues(Index: Integer);
    procedure UpdateValues(Index: Integer; Value: Variant);
    procedure UndoValues(Index: Integer);
    procedure RedoValues(Index: Integer);
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Update; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure Reset; override;
    property Count[Index: Integer] : Integer read GetCount;
    property Min[Index: Integer] : Variant read GetMin;
    property Max[Index: Integer] : Variant read GetMax;
    property Sum[Index: Integer] : Variant read GetSum;
    property Avg[Index: Integer] : Variant read GetAvg;
  published
    property Variable: TZRVariable read fVariable write SetVariable;
  end;

  { TZRGroup }
  TZRGroupBands = class(TPersistent)
  private
    fOwner : TZRGroup;
    function  GetHasBand(BandType: TZRBandType): Boolean;
    procedure SetHasBand(BandType: TZRBandType; Value: Boolean);
  protected
    property Owner : TZRGroup read fOwner;
  public
    constructor Create(aOwner: TZRGroup);
  published
    property HasHeader : Boolean index zbtGroupHeader read GetHasBand write SetHasBand stored False;
    property HasFooter : Boolean index zbtGroupFooter read GetHasBand write SetHasBand stored False;
  end;

  TZRGroup = class(TComponent)
  private
    fBands        : TZRGroupBands;
    fHeader,
    fFooter       : TZRCustomBand;
    fMaster       : TZRCustomController;
    fVariable     : TZRVariable;
    fEnabled      : Boolean;
    fRepeatHeader : Boolean;
    fStarted,
    fFinished     : Boolean;
    function  GetBand(BandType: TZRBandType): TZRCustomBand;
    procedure SetBand(BandType: TZRBandType; Value: TZRCustomBand);
    function  GetOrder: Integer;
    procedure SetOrder(Value : Integer);
    procedure SetMaster(Value: TZRCustomController);
    procedure SetVariable(Value : TZRVariable);
    procedure Notify(Operation: TZROperation);
  protected
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;

    procedure Prepare;
    procedure Unprepare;
    function  Check : Boolean;
    procedure Clear;
    procedure Reset;
    procedure Finish;

    property Started : Boolean read fStarted ;
    property Finished: Boolean read fFinished;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    function HasParent : Boolean; override;
    function GetParentComponent: TComponent; override;

    property Header : TZRCustomBand index zbtGroupHeader read GetBand;
    property Footer : TZRCustomBand index zbtGroupFooter read GetBand;
    property Master : TZRCustomController read fMaster write SetMaster;
    property Order  : Integer read GetOrder write SetOrder;
  published
    property Bands : TZRGroupBands read fBands write fBands;
    property Enabled : Boolean read fEnabled write fEnabled default True;
    property RepeatHeader : Boolean read fRepeatHeader write fRepeatHeader default False;
    property Variable : TZRVariable read fVariable write SetVariable;
  end;

  { TZRCustomBand }
  TZRChildBands = zbtChild..zbtController;
  TZRForceKinds = (zfkPageBefore, zfkPageAfter, zfkColumnBefore, zfkColumnAfter);
  TZRForceKind  = set of TZRForceKinds;

  TZRCustomBand = class(TZReportControl)
  private
    fIsAligning    : Boolean;
    fBandType      : TZRBandType;
    fPrintType     : TZRBandType;
    fLabelList     : TList;
    fStretch       : Boolean;
    fGroup         : TZRGroup;
    fLoadedGroup   : Integer;
    fLoadedParents : array[TZRChildBands] of Integer;
    fParentLinks   : array[TZRChildBands] of TZRCustomBand;
    fMaster        : TZRCustomController;
    fForceKind     : TZRForceKind;

    procedure SetMaster(const Value: TZRCustomController);
    procedure SetBandType(const Value: TZRBandType);
    procedure SetStretch(const Value: Boolean);

    procedure ReadBandType(Reader: TReader);
    procedure WriteBandType(Writer: TWriter);

    procedure SetGroup(Value : TZRGroup);
    procedure ReadGroupOrder(Reader: TReader);
    procedure WriteGroupOrder(Writer: TWriter);

  public {TZRDesigner trick to keep the ParentBand & MasterBand properties protected}
    function  GetParentLink(Index: TZRBandType): TZRCustomBand;
    function  GetChildLink(Index: TZRBandType) : TZRCustomBand;
    procedure SetParentLink(Index: TZRBandType; Value : TZRCustomBand);

  private
    function  GetHasChild : Boolean;
    procedure SetHasChild(Value : Boolean);
    procedure ReadParentIndex(Reader: TReader);
    procedure WriteParentIndex(Writer: TWriter);

    function  GetHasController : Boolean;
    procedure SetHasController(Value : Boolean);
    procedure ReadMasterIndex(Reader: TReader);
    procedure WriteMasterIndex(Writer: TWriter);

    property PrintType: TZRBandType read fPrintType;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(aParent: TWinControl); override;
    procedure SetParentComponent(Value: TComponent); override;

    procedure Draw; override;
    procedure AlignBounds; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    procedure PrintControl(Control: TZReportControl; OfsX, OfsY: Integer);
    procedure DoBeforePrint; override;
    procedure DoAfterPrint; override;
    procedure DoPrint(OfsX, OfsY: Integer); override;

    property IsAligning : Boolean read fIsAligning;
    property ParentBand : TZRCustomBand index zbtChild      read GetParentLink write SetParentLink;
    property MasterBand : TZRCustomBand index zbtController read GetParentLink write SetParentLink;

    property ForceKind : TZRForceKind read fForceKind write fForceKind default [];
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;

    function  HasParent : Boolean; override;
    function  GetParentComponent: TComponent; override;

    property BandType: TZRBandType read fBandType write SetBandType stored False;

    property Group  : TZRGroup            read fGroup  write SetGroup;
    property Master : TZRCustomController read fMaster write SetMaster;

    property HasChild      : Boolean read GetHasChild      write SetHasChild      stored False;
    property HasController : Boolean read GetHasController write SetHasController stored False;
    property ChildBand     : TZRCustomBand index zbtChild      read GetChildLink;
    property SubController : TZRCustomBand index zbtController read GetChildLink;

    property LabelList : TList read fLabelList;
    property Stretch   : Boolean read fStretch write SetStretch;
  end;

  TZRBand = class(TZRCustomBand)
  published
    property Enabled;
    property ForceKind;
    property Frame;
    property HasChild;
    property HasController;
    property Stretch;
    property Visible;
    property BeforePrint;
    property AfterPrint;
  end;

  { TZRBands }
  TZRBands = class(TPersistent)
  private
    fOwner : TZRCustomController;
    function  GetHasBand(BandType: TZRBandType): Boolean;
    procedure SetHasBand(BandType: TZRBandType; Value: Boolean);
  protected
    property HasPageHeader   : Boolean index zbtPageHeader   read GetHasBand write SetHasBand stored False;
    property HasHeader       : Boolean index zbtHeader       read GetHasBand write SetHasBand stored False;
    property HasColumnHeader : Boolean index zbtColumnHeader read GetHasBand write SetHasBand stored False;
    property HasDetailHeader : Boolean index zbtDetailHeader read GetHasBand write SetHasBand stored False;
    property HasDetail       : Boolean index zbtDetail       read GetHasBand write SetHasBand stored False;
    property HasDetailFooter : Boolean index zbtDetailFooter read GetHasBand write SetHasBand stored False;
    property HasColumnFooter : Boolean index zbtColumnFooter read GetHasBand write SetHasBand stored False;
    property HasFooter       : Boolean index zbtFooter       read GetHasBand write SetHasBand stored False;
    property HasPageFooter   : Boolean index zbtPageFooter   read GetHasBand write SetHasBand stored False;
    property Owner : TZRCustomController read fOwner;
  public
    constructor Create(aOwner: TZRCustomController);
  end;

  { TZRCustomController }

  TZRDataRangeBegin = (zrbFirst, zrbCurrent);
  TZRDataRangeEnd   = (zreLast , zreCurrent);

  TZRDataOptions = class(TPersistent)
  private
    fRangeBegin : TZRDataRangeBegin;
    fRangeEnd   : TZRDataRangeEnd;
    fAutoOpen   : Boolean;
  public
    constructor Create;
  published
    property AutoOpen   : Boolean read fAutoOpen write fAutoOpen default False;
    property RangeBegin : TZRDataRangeBegin read fRangeBegin write fRangeBegin default zrbFirst;
    property RangeEnd   : TZRDataRangeEnd   read fRangeEnd   write fRangeEnd   default zreLast;
  end;

  TZRControllerCollection = class(TZRElementCollection)
  private
    fOwner : TZRCustomController;
  protected
    property Owner : TZRCustomController read fOwner;
    function InternalFindEntry(const Name: String;
                           var aLibrary  : TZRElementLibrary;
                           var aIndex    : Integer): Boolean; override;
  public
    constructor Create(aOwner: TZRCustomController);
  end;

  TZRControllerLibrary = class(TZRVariableLibrary)
  private
    function GetController: TZRCustomController;
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Calculate(Index : Integer; Arguments: TArguments): Variant; override;
    property Controller : TZRCustomController read GetController;
  public
    function IndexOf(const Name: String): Integer; override;
  end;

  TZRControllerState = set of TZRBandType;

  TZRDataStartEvent = procedure(Sender: TZRCustomController; var HasData : Boolean) of object;
  TZRDataNextEvent  = procedure(Sender: TZRCustomController; var MoreData: Boolean) of object;

  TZRCustomController = class(TZRCustomBand)
  private
    fBandList     : TList;
    fDataOptions  : TZRDataOptions;
    fDataSet      : TDataSet;
    fGroupList    : TList;
    fVariableList : TList;
    fCollection   : TZRElementCollection;
    fLibrary      : TZRElementLibrary;

    fPrintIfEmpty : Boolean;
    fOnDataStart  : TZRDataStartEvent;
    fOnDataNext   : TZRDataNextEvent;

    fRecordNumber,
    fRecordCount  : Integer;
    fSaveActive   : Boolean;
    fCurrent      : TBookmarkStr;

    fHasData      : Boolean;
    fPrintList    : TList;

    fColumnStarted  : Boolean;
    fColumnFinished : Boolean;
    fColumnInit     : Boolean;
    fIsExecuting    : Boolean;

    fStepState,
    fColumnState,
    fPrevState,
    fPrintState   : TZRControllerState;
    fPrintSize,
    fFooterSize   : TPoint;

    procedure SetDataSet(Value : TDataSet);
    procedure SortBands;

    function DataSetOK: Boolean;
    function GetRecordCount: Integer;

    procedure CalculateFooterSize(Current: TZRCustomBand);
    procedure CalculatePrintSize;

    procedure SetColumnInit(const Value: Boolean);
    property ColumnInit: Boolean read fColumnInit write SetColumnInit;

    property FooterSize  : TPoint read fFooterSize;
    property PrintSize   : TPoint read fPrintSize;
    property HasData     : Boolean read fHasData;
    property PrintList   : TList read fPrintList;
    property PrintState  : TZRControllerState read fPrintState;
    property ColumnState : TZRControllerState read fColumnState;
    property StepState   : TZRControllerState read fStepState;
    property ColumnStarted  : Boolean read fColumnStarted;
    property ColumnFinished : Boolean read fColumnFinished;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;

    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Execute; dynamic;
    procedure Step; dynamic;

    procedure NewColumn; dynamic;
    procedure EndColumn; dynamic;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function GetBand(BandType: TZRBandType): TZRCustomBand;
    function CreateBand(BandType : TZRBandType; Group: TZRGroup): TZRCustomBand;
    procedure AddBand   (Band: TZRCustomBand);
    procedure RemoveBand(Band: TZRCustomBand);

    function CreateGroup: TZRGroup;
    procedure AddGroup   (Group: TZRGroup);
    procedure RemoveGroup(Group: TZRGroup);
    procedure MoveGroup(Old, New: Integer);

    function CreateVariable(VariableClass: TZRVariableClass; Suffix: String): TZRVariable;
    procedure AddVariable   (Variable: TZRVariable);
    procedure RemoveVariable(Variable: TZRVariable);

    property BandList     : TList read fBandList;
    property GroupList    : TList read fGroupList;
    property VariableList : TList read fVariableList;

    property Collection   : TZRElementCollection read fCollection;
    property DataOptions  : TZRDataOptions read fDataOptions write fDataOptions;
    property DataSet      : TDataSet read fDataSet write SetDataSet;
    property RecordNumber : Integer read fRecordNumber;
    property RecordCount  : Integer read GetRecordCount;

    property PrintIfEmpty : Boolean read fPrintIfEmpty write fPrintIfEmpty default False;
    property OnDataStart : TZRDataStartEvent read fOnDataStart write fOnDataStart;
    property OnDataNext  : TZRDataNextEvent  read fOnDataNext  write fOnDataNext;
  end;

  { TZRSubDetailBands }
  TZRSubDetailBands = class(TZRBands)
  published
    property HasHeader;
    property HasColumnHeader;
    property HasDetailHeader;
    property HasDetailFooter;
    property HasColumnFooter;
    property HasFooter;
  end;

  { TZRSubDetail }
  TZRSubDetail = class(TZRCustomController)
  private
    fBands : TZRSubDetailBands;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Bands : TZRSubDetailBands read fBands write fBands;
    property DataOptions;
    property DataSet;
    property Enabled;
    property ForceKind;
    property Frame;
    property HasChild;
    property HasController;
    property PrintIfEmpty;
    property Stretch;
    property Visible;
    property OnDataStart;
    property OnDataNext;
    property BeforePrint;
    property AfterPrint;
  end;

  { TZReportBands }
  TZReportBands = class(TZRBands)
  published
    property HasPageHeader;
    property HasHeader;
    property HasColumnHeader;
    property HasDetailHeader;
    property HasDetail;
    property HasDetailFooter;
    property HasColumnFooter;
    property HasFooter;
    property HasPageFooter;
  end;

  { TZRMargins }
  TZRMargins = class(TPersistent)
  private
    fOwner  : TZReport;
    fMargins: TRect;
    function  GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TZReport);
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Left  : Integer index 0 read GetValue write SetValue default 2;
    property Top   : Integer index 1 read GetValue write SetValue default 1;
    property Right : Integer index 2 read GetValue write SetValue default 1;
    property Bottom: Integer index 3 read GetValue write SetValue default 1;
  end;

  { TZRColumns }
  TZRColumns = class(TPersistent)
  private
    fOwner : TZReport;
    fCount : Integer;
    fSpace : Integer;
    procedure Changed;
    procedure SetCount(const Value: Integer);
    procedure SetSpace(const Value: Integer);
  public
    constructor Create(AOwner: TZReport);
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Width  : Integer;
    function Height : Integer;
    property Owner  : TZReport read fOwner;
  published
    property Count : Integer read fCount write SetCount default 1;
    property Space : Integer read FSpace write SetSpace default 2;
  end;

  { TZReport }
  TZRGetFileNameEvent = procedure (Sender: TObject; var FileName: String) of object;

  TZReport = class(TZRCustomController, IZReport)
  private
    fMinMaxButton   : TSpeedButton;
    fMinimized      : Boolean;
    mWidth, mHeight : Integer;
    fBands          : TZReportBands;
    fColumns        : TZRColumns;
    fMargins        : TZRMargins;
    fOptions        : TZReportOptions;
    fDesignForm     : TForm;
    fPrinter        : TZRPrinter;
    fTitle          : String;
    fShowProgress   : Boolean;
    fCurrentX,
    fCurrentY       : Integer;
    fCurrentColumn  : Integer;
    fColumnTop      : Integer;
    fPageStarted    : Boolean;
    fPageFinished   : Boolean;
    fIsLastPage     : Boolean;
    fOnNewPage      : TNotifyEvent;
    fOnEndPage      : TNotifyEvent;
    fOnGetFileName  : TZRGetFileNameEvent;
    fOnPrintStart   : TNotifyEvent;
    fOnPrintFinish  : TNotifyEvent;

    function  GetFont: TFont;
    procedure SetFont(Value: TFont);
    function  GetOptions: TZReportOptions;
    procedure SetOptions(Value: TZReportOptions);
    function  GetTitle: String;
    procedure SetTitle(Value: String);

    function  GetFileName: String;
    function  GetPageWidth : Integer;
    function  GetPageHeight: Integer;

    function  GetPrinter: TZRPrinter;
    procedure SetPrinter(Value: TZRPrinter);
    function  IsPrinting: Boolean;

    procedure SetMinimized(Value: Boolean);
    procedure MinMaxClick(Sender: TObject);
    procedure ReadMinimized(Reader: TReader);
    procedure WriteMinimized(Writer: TWriter);

    procedure SetColumns(const Value: TZRColumns);
    procedure SetMargins(const Value: TZRMargins);

    procedure ZRNotify(var Message: TZRNotify); message ZR_NOTIFY;

    function GetAvailableSpace: TPoint;
    function GetPageCount : Integer;

    property AvailableSpace: TPoint read GetAvailableSpace;
    property CurrentX : Integer read fCurrentX write fCurrentX;
    property CurrentY : Integer read fCurrentY write fCurrentY;
    property CurrentColumn : Integer read fCurrentColumn write fCurrentColumn;
    property IsLastPage : Boolean read fIsLastPage;

    procedure PrintStart;
    procedure PrintFinish;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;

    procedure Draw; override;

    procedure AlignBands;
    procedure AdjustBounds; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Step; override;

    procedure NewColumn; override;
    procedure EndColumn; override;
    procedure EndPage;

    procedure DoPrint(OfsX, OfsY: Integer); override;
    procedure PrintBand(aBand: TZRCustomBand);
    procedure Generate(Composite: Boolean);

    property Minimized: Boolean read fMinimized write SetMinimized;
    property PageWidth : Integer read GetPageWidth;
    property PageHeight: Integer read GetPageHeight;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Preview;
    procedure Print;
    procedure NewPage;

    property DesignForm : TForm read fDesignForm write fDesignForm;
    property FileName   : String read GetFileName;
    property Printer    : TZRPrinter read GetPrinter;
    property PageCount  : Integer read GetPageCount;
  published
    property Bands   : TZReportBands read fBands   write fBands;
    property Columns : TZRColumns read fColumns write SetColumns;
    property DataOptions;
    property DataSet;

    property Font    : TFont           read GetFont    write SetFont;
    property Options : TZReportOptions read GetOptions write SetOptions;
    property Title   : String          read GetTitle   write SetTitle;

    property Margins : TZRMargins read fMargins write SetMargins;
    property PrintIfEmpty;
    property ShowProgress : Boolean read fShowProgress write fShowProgress default True;
    property OnDataStart;
    property OnDataNext;
    property BeforePrint;
    property AfterPrint;
    property OnNewPage: TNotifyEvent read fOnNewPage write fOnNewPage;
    property OnEndPage: TNotifyEvent read fOnEndPage write fOnEndPage;
    property OnGetFileName: TZRGetFileNameEvent read fOnGetFileName write fOnGetFileName;
    property OnPrintStart  : TNotifyEvent read fOnPrintStart  write fOnPrintStart;
    property OnPrintFinish : TNotifyEvent read fOnPrintFinish write fOnPrintFinish;
  end;

  { TZRComposite }
  TZRComposite = class(TComponent, IZReport)
  private
    fOptions      : TZReportOptions;
    fTitle        : String;
    fPrinter      : TZRPrinter;
    fFont         : TFont;
    fPageWidth,
    fPageHeight   : Integer;
    fShowProgress : Boolean;
    fIsPrinting   : Boolean;
    fReports      : TList;
    fOnAddReports : TNotifyEvent;
    fOnGetFileName: TZRGetFileNameEvent;
    fOnPrintStart : TNotifyEvent;
    fOnPrintFinish: TNotifyEvent;

    function  GetFont: TFont;
    procedure SetFont(Value: TFont);
    function  GetOptions: TZReportOptions;
    procedure SetOptions(Value: TZReportOptions);
    function  GetTitle: String;
    procedure SetTitle(Value: String);

    function  GetFileName: String;
    function  GetPageWidth : Integer;
    function  GetPageHeight: Integer;

    function  GetPrinter: TZRPrinter;
    procedure SetPrinter(Value: TZRPrinter);
    function  IsPrinting: Boolean;

    procedure Generate;

    procedure PrintStart;
    procedure PrintFinish;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Preview;
    procedure Print;

    property FileName: String read GetFileName;
    property Printer : TZRPrinter read GetPrinter;
    property Reports : TList read fReports;
  published
    property Font         : TFont           read GetFont    write SetFont;
    property Options      : TZReportOptions read GetOptions write SetOptions;
    property Title        : String          read GetTitle   write SetTitle;
    property PageWidth    : Integer read GetPageWidth  write fPageWidth;
    property PageHeight   : Integer read GetPageHeight write fPageHeight;
    property ShowProgress : Boolean read fShowProgress write fShowProgress default True;
    property OnAddReports : TNotifyEvent read fOnAddReports write fOnAddReports;
    property OnGetFileName: TZRGetFileNameEvent read fOnGetFileName write fOnGetFileName;
    property OnPrintStart  : TNotifyEvent read fOnPrintStart  write fOnPrintStart;
    property OnPrintFinish : TNotifyEvent read fOnPrintFinish write fOnPrintFinish;
  end;

const
  NoChildBands: set of TZRBandType = [
    zbtPageHeader, zbtDetailFooter, zbtColumnFooter, zbtPageFooter];
  NoControllerBands: set of TZRBandType = [
    zbtPageHeader, zbtDetailFooter, zbtColumnFooter, zbtPageFooter];
  NoStretchBands: set of TZRBandType = [
    {zbtPageHeader, }zbtDetailFooter, zbtColumnFooter, zbtPageFooter];

const
  ZRFontStyleMap: array[TZRFontStyle] of TFontStyles = (
    [fsBold], [fsItalic], [fsUnderline], [], []);

implementation

uses
  TypInfo, Math,
  ZRStrUtl, ZRUtils, ZRConst, ZRStream, ZRPrgres;

const
  clFrame        : TColor = clAppWorkSpace;
  clControl      : TColor = clWindow;
  clControlBorder: TColor = clWindowText;
  clBand         : TColor = TColor($C0DCC0+$202020);
  clController   : TColor = clBtnFace;
  clReport       : TColor = clAppWorkSpace;
  clGridBand     : TColor = clBtnShadow;
  clGridReport   : TColor = clBtnFace;

{$I ZRFrame.inc}

{$I ZROption.inc}

{$I ZRVar.inc}

{$I ZRGroup.inc}

{$I ZRContrl.inc}

{$I ZRBand.inc}

{$I ZRCtrllr.inc}

function PreparePrinter(Report: IZReport): Boolean;
var
  aPrinter : TZRPrinter;
begin
  Result := not Report.IsPrinting;
  if Result then begin
    if Assigned(Report.Printer) then begin
      Report.Printer.Free;
      Report.SetPrinter(nil);
    end;
    aPrinter := TZRPrinter.Create(Report);
    TZRProgressForm.CreateProgress(Application, aPrinter);
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZRComposite                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRComposite.Create(aOwner: TComponent);
begin
  inherited;
  fFont    := TFont.Create;
  fOptions := TZReportOptions.Create;
  fReports := TList.Create;
  fShowProgress := True;

  Font.Name  := 'Courier New';
  PageWidth  := 76;
  PageHeight := 60;
end;

destructor TZRComposite.Destroy;
begin
  inherited;
  fReports.Free;
  fOptions.Free;
  fFont.Free;
end;

function TZRComposite.GetFont: TFont;
begin
  Result := fFont;
end;
procedure TZRComposite.SetFont(Value: TFont);
begin
  fFont.Assign(Value);
end;

function TZRComposite.GetOptions: TZReportOptions;
begin
  Result := fOptions;
end;
procedure TZRComposite.SetOptions(Value: TZReportOptions);
begin
  fOptions.Assign(Value);
end;

function TZRComposite.GetTitle: String;
begin
  Result := fTitle;
end;
procedure TZRComposite.SetTitle(Value: String);
begin
  fTitle := Value;
end;

function TZRComposite.GetFileName: String;
begin
  Result := Options.FileName;
  if Assigned(OnGetFileName) then
    try
      OnGetFileName(Self, Result);
    except
    end;
end;

function TZRComposite.GetPageWidth: Integer;
begin
  Result := fPageWidth;
end;
function TZRComposite.GetPageHeight: Integer;
begin
  Result := fPageHeight;
end;

function TZRComposite.GetPrinter: TZRPrinter;
begin
  Result := fPrinter;
end;
procedure TZRComposite.SetPrinter(Value: TZRPrinter);
begin
  fPrinter := Value;
end;

function TZRComposite.IsPrinting: Boolean;
begin
  Result := fIsPrinting;
end;

procedure TZRComposite.PrintStart;
begin
  if Assigned(OnPrintStart) then OnPrintStart(Self);
end;
procedure TZRComposite.PrintFinish;
begin
  if Assigned(OnPrintFinish) then OnPrintFinish(Self);
end;

procedure TZRComposite.Generate;
var
  i            : Integer;
  LastCurrentY : Integer;
  LastColumn   : Integer;
  LastColCount : Integer;
  aReport      : TZReport;
begin
  Reports.Clear;
  if Assigned(fOnAddReports) then fOnAddReports(Self);
  if Reports.Count > 0 then
    try
      fIsPrinting := True;
      Printer.BeginDoc;
      try
        aReport := TZReport(Reports[0]);
        LastCurrentY  := aReport.Margins.Top;
        LastColumn    := 1;
        LastColCount  := aReport.Columns.Count;

        i := 0;
        while (i < Reports.Count) and Assigned(Printer) and not Printer.Cancelled do begin
          aReport := TZReport(Reports[i]);
          aReport.SetPrinter(Printer);
          try
            if (i = 0) or (Printer.Options.PaperType <> zptContinuous) and (LastColCount > 1) then begin
              aReport.NewPage;
            end else begin
              aReport.CurrentX      := aReport.Margins.Left;
              aReport.CurrentY      := LastCurrentY;
              aReport.CurrentColumn := LastColumn;
            end;
            Inc(i);
            aReport.Generate(True);
            LastCurrentY  := aReport.CurrentY;
            LastColumn    := aReport.CurrentColumn;
            LastColCount  := aReport.Columns.Count;
          finally
            aReport.SetPrinter(nil);
          end;
        end;
        if Printer.Cancelled then
          Printer.AbortDoc
        else begin
          aReport.fIsLastPage := True;
          aReport.EndPage;
          Printer.EndDoc;
        end;
      except
        Printer.AbortDoc;
      end;
    finally
      fIsPrinting := False;
    end;
end;

procedure TZRComposite.Preview;
begin
  if PreparePrinter(Self) then
    try
      Printer.Preview;
      Generate;
      if Assigned(Printer) then Printer.Finished;
      repeat
        Application.ProcessMessages;
      until not Assigned(Printer) or not Printer.InPreview or Application.Terminated;
    finally
      Printer.Free;
      SetPrinter(nil);
    end;
end;

procedure TZRComposite.Print;
begin
  if PreparePrinter(Self) then begin
    try
      if ShowProgress then Printer.ProgressForm.Show;
      Generate;
      if Assigned(Printer) then begin
        Printer.Finished;
        if not Printer.Cancelled then Printer.Print;
      end;
    finally
      Printer.Free;
      SetPrinter(nil);
    end;
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                  TZReport                              !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZReport.Create(aOwner: TComponent);
begin
  inherited;
  fReport       := Self;
  fShowProgress := True;

  fBands     := TZReportBands.Create(Self);
  fColumns   := TZRColumns.Create(Self);
  fMargins   := TZRMargins.Create(Self);
  fOptions   := TZReportOptions.Create;

  Font.Name  := 'Courier New';
  Color      := clReport;
  Width      := 76;
  Height     := 60;

  fMinMaxButton := TSpeedButton.Create(nil);
  fMinMaxButton.SetBounds(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
  fMinMaxButton.Parent    := Self;
  fMinMaxButton.Font.Name := 'Wingdings';
  fMinMaxButton.OnClick   := MinMaxClick;
  Minimized := False;
end;

destructor TZReport.Destroy;
begin
  fMinMaxButton.Free;
  inherited;
  DesignForm.Free;
  fOptions.Free;
  fMargins.Free;
  fColumns.Free;
  fBands.Free;
end;

procedure TZReport.SetMinimized(Value: Boolean);
begin
  if Value <> Minimized then begin
    if (csDesigning in ComponentState) then
      GetParentForm(Self).Designer.Modified;
    fMinimized := Value;
    if Minimized then begin
      mWidth  := Width;
      mHeight := Height;
    end else begin
      Width  := mWidth;
      Height := mHeight;
    end;
  end;
  UpdateBounds;
  if Minimized then
    fMinMaxButton.Caption := Char(168)
  else
    fMinMaxButton.Caption := Char(167);
end;

procedure TZReport.MinMaxClick(Sender: TObject);
begin
  Minimized := not Minimized;
end;

procedure TZReport.ReadMinimized(Reader: TReader);
begin
  fMinimized := Reader.ReadBoolean;
end;
procedure TZReport.WriteMinimized(Writer: TWriter);
begin
  Writer.WriteBoolean(Minimized);
end;

procedure TZReport.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Minimized', ReadMinimized, WriteMinimized, Minimized);
end;

procedure TZReport.Loaded;
begin
  inherited;
  mWidth  := Width;
  mHeight := Height;
  SetMinimized(Minimized);
  if not (csDesigning in ComponentState) then Hide;
end;

procedure TZReport.ZRNotify(var Message: TZRNotify);
begin
  if (csDesigning in ComponentState) and Assigned(DesignForm) then
    DesignForm.WindowProc(TMessage(Message));
end;

procedure TZReport.Draw;

  procedure DrawMargins;
  begin
    if Assigned(Margins) then with Canvas do begin
      Brush.Style:= bsClear;
      Pen.Style  := psSolid;
      Pen.Color  := clControlBorder;
      Pen.Width  := 2;
      Rectangle(
        Margins.Left   * CharSize.X,
        Margins.Top    * CharSize.Y,
        (Width -Margins.Right ) * CharSize.X + 2,
        (Height-Margins.Bottom) * CharSize.Y + 2);
    end;
  end;

  procedure DrawColumns;
  var
    i           : Integer;
  begin
    if Assigned(Columns) and (Columns.Count > 1) then with Canvas do begin
      Brush.Style:= bsClear;
      Pen.Style  := psDash;
      Pen.Width  := 1;
      Pen.Color  := clRed;
      for i := 1 to Columns.Count-1 do begin
        MoveTo(CharSize.X * (Margins.Left + i * Columns.Width + (i - 1) * Columns.Space), 0);
        LineTo(CharSize.X * (Margins.Left + i * Columns.Width + (i - 1) * Columns.Space),
               CharSize.Y * Margins.Top);

        MoveTo(CharSize.X * (Margins.Left + i * (Columns.Width + Columns.Space)), 0);
        LineTo(CharSize.X * (Margins.Left + i * (Columns.Width + Columns.Space)),
               CharSize.Y * Margins.Top);
      end;
    end;
  end;

var
  R: TRect;
  B: TBitmap;
begin
  inherited;
  if not Minimized then begin
    DrawMargins;
    DrawColumns;
  end else begin
    R := ClientRect;
    B := TBitmap.Create;
    try
      B.LoadFromResourceName(hInstance, ClassName);
      Canvas.Draw((R.Right-B.Width) div 2, (R.Bottom-B.Height) div 2,B);
    finally
      B.Free;
    end;
    Dec(R.Right);  Dec(R.Bottom);
    Frame3D(Canvas, R, clWhite, clBlack, 2);
  end;
end;

procedure TZReport.AdjustBounds;
begin
  inherited;
  Invalidate;
end;

procedure TZReport.AlignBands;

  function IsReportWide(Band: TZRCustomBand): Boolean;
  begin
    Result := (Band.BandType in [zbtPageHeader, zbtPageFooter]) or (
              (Band.BandType = zbtHeader) and (Band.Band = Band.Report));
  end;

var
  x, y         : Integer;
  BandLeft,
  BandTop,
  BandWidth,
  BandHeight,
  ColumnWidth,
  ColumnHeight : Integer;

  procedure AlignController(Controller: TZRCustomController); forward;

  procedure AlignBand(Band : TZRCustomBand);
  begin
    if Assigned(Band) then try
      Band.fIsAligning := True;
      Band.Left  := x;
      Band.Top   := y;
      if (Band.Master = Report) and IsReportWide(Band) then
        Band.Width := BandWidth
      else
        Band.Width := ColumnWidth;
      Inc(y, Band.Height);
      AlignBand(Band.ChildBand);
      AlignController(TZRCustomController(Band.SubController));
    finally
      Band.fIsAligning := False;
    end;
  end;

  procedure AlignController(Controller: TZRCustomController);
  var
    BT: TZRBandType;
    i : Integer;
  begin
    {if not Assigned(Controller) or
       (csDestroying in Controller.ComponentState) or
       not Assigned(Controller.BandList) then Exit;}
    if Assigned(Controller) then begin
      for BT := zbtPageHeader to Pred(zbtGroupHeader) do
        AlignBand(Controller.GetBand(BT));
      for i := 0 to Controller.GroupList.Count-1 do
        AlignBand(TZRGroup(Controller.GroupList[i]).Header);
      for BT := Succ(zbtGroupHeader) to Pred(zbtGroupFooter) do
        AlignBand(Controller.GetBand(BT));
      for i := Controller.GroupList.Count-1 downto 0 do
        AlignBand(TZRGroup(Controller.GroupList[i]).Footer);
      for BT := Succ(zbtGroupFooter) to zbtPageFooter do
        AlignBand(Controller.GetBand(BT));
    end;
  end;

begin
  {if fAligning or
     (csDestroying in ComponentState) or
     (csLoading    in ComponentState) or
     not Assigned(BandList) then Exit;}

  if not IsAligning then try
    fIsAligning := True;

    BandLeft     := 0;
    BandTop      := 0;
    BandWidth    := Width;
    BandHeight   := Height;
    ColumnWidth  := BandWidth;
    ColumnHeight := BandHeight;

    {if Assigned(Margins) then} begin
      Inc(BandLeft  , Margins.Left);
      Inc(BandTop   , Margins.Top);
      Dec(BandWidth , Margins.Left+Margins.Right);
      Dec(BandHeight, Margins.Top+Margins.Bottom);
      ColumnWidth  := Columns.Width;
      ColumnHeight := Columns.Height;
    end;

    x := BandLeft;
    y := BandTop;

    if Minimized and (csDesigning in ComponentState) then Inc(y, 10);

    AlignController(Self);

    {if Assigned(Margins) then} Inc(y, Margins.Bottom);
    if y > Height then Height := y;
    Invalidate;
  finally
    fIsAligning := False;
  end;
end;

procedure TZReport.SetColumns(const Value: TZRColumns);
begin
  fColumns.Assign(Value);
end;

procedure TZReport.SetMargins(const Value: TZRMargins);
begin
  fMargins.Assign(Value);
end;

function TZReport.GetFont: TFont;
begin
  Result := inherited Font;
end;
procedure TZReport.SetFont(Value: TFont);
begin
  inherited Font := Value;
end;

function TZReport.GetOptions: TZReportOptions;
begin
  Result := fOptions;
end;
procedure TZReport.SetOptions(Value: TZReportOptions);
begin
  fOptions.Assign(Value);
end;

function TZReport.GetTitle: String;
begin
  Result := fTitle;
end;
procedure TZReport.SetTitle(Value: String);
begin
  fTitle := Value;
end;

function TZReport.GetFileName: String;
begin
  Result := Options.FileName;
  if Assigned(OnGetFileName) then
    try
      OnGetFileName(Self, Result);
    except
    end;
end;

function TZReport.GetPageWidth  : Integer;
begin
  Result := fWidth;
end;
function TZReport.GetPageHeight : Integer;
begin
  Result := fHeight;
end;

function TZReport.GetPrinter: TZRPrinter;
begin
  Result := fPrinter;
end;
procedure TZReport.SetPrinter(Value: TZRPrinter);
begin
  fPrinter := Value;
end;

procedure TZReport.PrintStart;
begin
  if Assigned(OnPrintStart) then OnPrintStart(Self);
end;
procedure TZReport.PrintFinish;
begin
  if Assigned(OnPrintFinish) then OnPrintFinish(Self);
end;

function TZReport.IsPrinting: Boolean;
begin
  Result := inherited IsPrinting;
end;

function TZReport.GetPageCount : Integer;
begin
  if Assigned(Printer) then
    Result := Printer.PageCount
  else
    Result := 0;
end;

function TZReport.GetAvailableSpace: TPoint;
begin
  Result.X := CurrentColumn * (Columns.Width + Columns.Space) +
              Margins.Left - Columns.Space - CurrentX;
  Result.Y := Columns.Height + Margins.Top - CurrentY;
end;

procedure TZReport.Prepare;
begin
  inherited;
  fPageStarted  := False;
  fPageFinished := False;
end;

procedure TZReport.Unprepare;
begin
  inherited;
end;

procedure TZReport.NewPage;
begin
  if not fPageStarted then
    try
      fPageStarted  := True;
      EndPage;
      Printer.BeginPageY;
      fCurrentX := Margins.Left;
      fCurrentY := Margins.Top;
      fCurrentColumn := 0;
      if not (csDesigning in ComponentState) and Assigned(OnNewPage) then OnNewPage(Self);
      if (Printer.Options.PaperType <> zptContinuous) then begin
        if (PageCount > 1) or Printer.Options.FirstPageHeader then PrintBand(GetBand(zbtPageHeader));
      end;
      fColumnTop:= CurrentY;
      NewColumn;
    finally
      fPageStarted  := False;
    end;
end;

procedure TZReport.EndPage;
var
  PF : TZRCustomBand;
begin
  if not fPageFinished then
    try
      fPageFinished := True;
      if (PageCount > 0) then begin
        EndColumn;
        if (Printer.Options.PaperType <> zptContinuous) then begin
          PF := GetBand(zbtPageFooter);
          if Assigned(PF) and PF.Enabled and PF.Visible then begin
            fCurrentX := Margins.Left;
            fCurrentY := Printer.PageHeight - Margins.Bottom - PF.Height;
            if not IsLastPage or Printer.Options.LastPageFooter then PrintBand(GetBand(zbtPageFooter));
          end;
        end;
        if not (csDesigning in ComponentState) and Assigned(OnEndPage) then OnEndPage(Self);
      end;
    finally
      fPageFinished := False;
    end;
end;

procedure TZReport.NewColumn;
begin
  if not ColumnStarted and not ColumnFinished then begin
    if (CurrentColumn = 1) and (Printer.Options.PaperType = zptContinuous) or
       (CurrentColumn = Columns.Count) then
      NewPage
    else begin
      EndColumn;
      fCurrentX := Margins.Left + CurrentColumn * (Columns.Width + Columns.Space);
      fCurrentY := fColumnTop;
      Inc(fCurrentColumn);
      if (PageCount > 1) or (CurrentColumn > 1) then inherited;
    end;
  end;
end;

procedure TZReport.EndColumn;
begin
  if not ColumnStarted and not ColumnFinished then begin
    if (CurrentColumn > 0) and (CurrentColumn <= Columns.Count) then inherited;
  end;
end;

procedure TZReport.DoPrint(OfsX, OfsY: Integer);
begin
  Execute;
end;

procedure TZReport.Step;
begin
  if RecordCount > 0 then
    Printer.Progress := (RecordNumber * 100) div RecordCount
  else
    Printer.Progress := 0;
  Application.ProcessMessages;
end;

procedure TZReport.Generate(Composite: Boolean);
begin
  DoBeforePrint;
  if pDoPrint then
    try
      if not Composite then Printer.BeginDoc;
      Prepare;
      if not Composite then begin
        fIsLastPage := False;
        NewPage;
      end;
      PrintControl(Self, Margins.Left, Margins.Top);
      if not Composite then begin
        fIsLastPage := True;
        EndPage;
      end;
      Unprepare;
      DoAfterPrint;
    finally
      if not Composite then begin
        if Printer.Cancelled then
          Printer.AbortDoc
        else
          Printer.EndDoc;
      end;
    end;
end;

procedure TZReport.Preview;
begin
  if PreparePrinter(Self) then
    try
      Printer.Preview;
      Generate(False);
      if Assigned(Printer) then Printer.Finished;
      repeat
        Application.ProcessMessages;
      until not Assigned(Printer) or not Printer.InPreview or Application.Terminated;
    finally
      Printer.Free;
      SetPrinter(nil);
    end;
end;

procedure TZReport.Print;
begin
  if PreparePrinter(Self) then begin
    try
      if ShowProgress then Printer.ProgressForm.Show;
      Generate(False);
      if Assigned(Printer) then begin
        Printer.Finished;
        if not Printer.Cancelled then Printer.Print;
      end;
    finally
      Printer.Free;
      SetPrinter(nil);
    end;
  end;
end;

procedure TZReport.PrintBand(aBand: TZRCustomBand);
const
  ImplicitBands = [zbtPageHeader,zbtPageFooter,zbtColumnHeader,zbtDetailHeader];
var
  Space   : TPoint;
  PageNo  : Integer;
  aMaster : TZRCustomController;
begin
  if not Assigned(aBand) or not aBand.Enabled then Exit;

  with aBand do begin
    DoBeforePrint;
    if pDoPrint then begin

      if aBand is TZRCustomController then
        aMaster := TZRCustomController(aBand)
      else
        aMaster := Master;

      if (Printer.Options.PaperType <> zptContinuous) and
         not (BandType in ImplicitBands) and
         not (zbtHeader in aMaster.StepState)
         and aMaster.ColumnInit then begin
        if (zfkPageBefore   in ForceKind) then begin
          PageNo := PageCount;
          repeat aMaster.NewColumn until PageCount > PageNo;
        end else
        if (zfkColumnBefore in ForceKind) then aMaster.NewColumn;
      end;

      if not (BandType in [zbtPageHeader,zbtPageFooter]) then begin
        Space := AvailableSpace;
        Dec(Space.Y, Height);
        if (Printer.Options.PaperType <> zptContinuous) then begin
          aMaster.CalculateFooterSize(aBand);
          Dec(Space.Y, aMaster.FooterSize.Y);
        end;
        if Space.Y < 0 then aMaster.NewColumn;
      end;

      DoPrint(CurrentX, CurrentY);
      Inc(fCurrentY, Height);
      if (BandType = zbtHeader) and (aMaster = Report) then Inc(fColumnTop, Height);

      if (Printer.Options.PaperType <> zptContinuous) and
         not (BandType in ImplicitBands) and
         not (zbtFooter in aMaster.StepState)
         and aMaster.ColumnInit then begin
        if (zfkColumnAfter in ForceKind) then aMaster.NewColumn else
        if (zfkPageAfter   in ForceKind) then begin
          PageNo := PageCount;
          repeat aMaster.NewColumn until PageCount > PageNo;
        end;
      end;
      DoAfterPrint;
    end;
    if HasChild      then PrintBand(ChildBand);
    if HasController then TZRCustomController(SubController).Execute;
  end;
end;

end.

