{ cyDER  Components
  Description: Document Elements Recognition classes

  Todo:
  - Determine new Expression properties like isCellGrid: Boolean.
}

unit cyDocER;

interface

uses
  Classes, Windows, Controls, SysUtils, DateUtils, cyStrUtils, cyDERUtils, VCL.cyGraphics, cyDateUtils, Dialogs;

type
  TOCRCarInfo = record
    Value: String;
    PageNumber: Integer;
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    WordNumber: Integer;
  end;

  TcyDocER = class;

  TOCRPage = class
  private
    FTag: Integer;
    FPixelsWidth: Integer;
    FPixelsHeight: Integer;
    FResolution: Integer;
    FAngleRotation: Integer;
    FTagStr: String;
    procedure SetTagStr(const Value: String);
  protected
  public
    property Tag: Integer read FTag write FTag;
    property TagStr: String read FTagStr write SetTagStr;
    property AngleRotation: Integer read FAngleRotation write FAngleRotation;
    property PixelsWidth: Integer read FPixelsWidth write FPixelsWidth;
    property PixelsHeight: Integer read FPixelsHeight write FPixelsHeight;
    property Resolution: Integer read FResolution write FResolution;
  end;

  TOCRExpression = class
  private
    FValue: String;
    FAssociatedElementIndex: Integer;
    FAssociatedElementKeywordIndex: Integer;
    FAssociatedExpressionKeywordIndex: Integer; // Connexion to the expression's keyword found!
    FRecognizedFloat: String;
    FRecognizedNumbers: String;
    FRecognizedInteger: String;
    FRecognizedPercentage: String;
    FRecognizedWebMail: String;
    FRecognizedWebsite: String;
    FRecognizedMoney: String;
    FRecognizedDate: String;
    FRightPxPos: Integer;
    FBottomPxPos: Integer;
    FTopPxPos: Integer;
    FLeftPxPos: Integer;
    FPageNumber: Integer;
    FDERValue: DERString;
    FRecognizedType: TElementsType;
    FOCRConfidence: Extended;
  protected
  public
    // OCR information :
    function RecognizedValue: String;
    function RecognizedFloatvalue: Extended;
    function RecognizedDateValue: TDate;
    property LeftPxPos: Integer read FLeftPxPos;
    property TopPxPos: Integer read FTopPxPos;
    property RightPxPos: Integer read FRightPxPos;
    property BottomPxPos: Integer read FBottomPxPos;
    property Value: String read FValue write FValue;
    property PageNumber: Integer read FPageNumber;
    // DER information :
    property DERValue: DERString read FDERValue;
    property AssociatedElementIndex: Integer read FAssociatedElementIndex write FAssociatedElementIndex;
    property AssociatedElementKeywordIndex: Integer read FAssociatedElementKeywordIndex write FAssociatedElementKeywordIndex;
    property AssociatedExpressionKeywordIndex: Integer read FAssociatedExpressionKeywordIndex write FAssociatedExpressionKeywordIndex;
    property OCRConfidence: Extended read FOCRConfidence write FOCRConfidence;
    property RecognizedType: TElementsType read FRecognizedType write FRecognizedType;
    property RecognizedDate: String read FRecognizedDate write FRecognizedDate;
    property RecognizedMoney: String read FRecognizedMoney write FRecognizedMoney;
    property RecognizedWebsite: String read FRecognizedWebsite write FRecognizedWebsite;
    property RecognizedWebMail: String read FRecognizedWebMail write FRecognizedWebMail;
    property RecognizedPercentage: String read FRecognizedPercentage write FRecognizedPercentage;
    property RecognizedFloat: String read FRecognizedFloat write FRecognizedFloat;
    property RecognizedInteger: String read FRecognizedInteger write FRecognizedInteger;
    property RecognizedNumbers: String read FRecognizedNumbers write FRecognizedNumbers;
  end;



  TLookingDirectionFromKeyword = (ldOnLeft, ldOnTop, ldOnRight, ldAtBottom, ldOnRightIfNotFoundBottom, ldAtBottomIfNotFoundRight);
  TSearchLocation = (slFromLeft, slFromTop, slFromRight, slFromBottom);
  TPatternPositionMode = (ppFromTopLeftPage, ppFromTopLeftKeyword);
  TPatternPageNumberMode = (ppFromBeginning, ppFromEnd);

  TElement = class(TCollectionItem)
  private
    FTag: Integer;
    FKeyWords: TStrings;
    FPatternFromLeftMm: Extended;
    FPatternFromTopMm: Extended;
    FPatternToRightMm: Extended;
    FPatternToBottomMm: Extended;
    FPatternValueSizeMm: Extended;     // 2017-01-16 New property !
    FPatternValueLocation: TSearchLocation;
    FPatternValueNumber: Integer;
    FValueCount: Integer;
    FPatternPageNumber: Integer;
    FPatternPageNumberMode: TPatternPageNumberMode;
    FValueType: TElementsType;
    FValueMinCars: Integer;     // 2017-01-16 New property !
    FValueMaxCars: Integer;     // 2017-01-16 New property !
    FPatternPositionMode: TPatternPositionMode;
    fMask: string;
    FTagStr: String;
    FDescription: String;
    FKeywordLookingValueDirection: TLookingDirectionFromKeyword;
    FTag2: Integer;
    procedure SetKeyWords(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Description: String read FDescription write FDescription;
    property KeyWords: TStrings read FKeyWords write SetKeyWords;
    property KeywordLookingValueDirection: TLookingDirectionFromKeyword read FKeywordLookingValueDirection write FKeywordLookingValueDirection default ldOnRightIfNotFoundBottom;
    property Mask: string read fMask write fMask;
    property PatternPageNumber: Integer read FPatternPageNumber write FPatternPageNumber default 0;
    property PatternPageNumberMode: TPatternPageNumberMode read FPatternPageNumberMode write FPatternPageNumberMode default ppFromBeginning;
    property PatternFromLeftMm: Extended read FPatternFromLeftMm write FPatternFromLeftMm;
    property PatternFromTopMm: Extended read FPatternFromTopMm write FPatternFromTopMm;
    property PatternToRightMm: Extended read FPatternToRightMm write FPatternToRightMm;
    property PatternToBottomMm: Extended read FPatternToBottomMm write FPatternToBottomMm;
    property PatternValueSizeMm: Extended read FPatternValueSizeMm write FPatternValueSizeMm;
    property PatternPositionMode: TPatternPositionMode read FPatternPositionMode write FPatternPositionMode default ppFromTopLeftPage;
    property PatternValueLocation: TSearchLocation read FPatternValueLocation write FPatternValueLocation default slFromBottom;
    property PatternValueNumber: Integer read FPatternValueNumber write FPatternValueNumber default 1;   // Which value must be returned if more than one value in defined area
    property ValueCount: Integer read FValueCount write FValueCount default 1;          // Number of values that must be retrieved
    property ValueType: TElementsType read FValueType write FValueType default etMoney;
    property ValueMinCars: Integer read FValueMinCars write FValueMinCars default 0;
    property ValueMaxCars: Integer read FValueMaxCars write FValueMaxCars default 0;
    property Tag: Integer read FTag write FTag default 0;
    property Tag2: Integer read FTag2 write FTag2 default 0;
    property TagStr: String read FTagStr write FTagStr;
  end;

  TElementClass = class of TElement;

  TElements = Class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetElement(Index: Integer): TElement;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aDocument: TcyDocER; ElementClass: TElementClass);
    function Add: TElement;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TElement read GetElement; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyLocateOptions = class(TPersistent)
  private
    FIgnoreChars: string;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Sanytize(var aText: string; const ReplaceWith: String = '');
  published
    property IgnoreChars: string read FIgnoreChars write FIgnoreChars;
  end;

  TRecognitionElementsMode = (reKeywords, reValues, reKeywordsAndValues);
  TRecognitionPriorityMode = (rpKeywordsLenth, rpSinglelineKeywordsLength);

  TRecognitionOption = (roKeywordsByPriority,
                        roSmartNumbersRec,        // Will use DERToDERNCharset() to recognize letters returned by OCR as numbers (exemple: 'O', 'I', 'i')!
                        roSmartWebsiteRec,        // Will try to correct OCR result to retrieve website
                        roSmartKeywordRec);       // Will allow find keywords with some cars errors to compensate OCR result cars errors
  TRecognitionOptions = Set of TRecognitionOption;

  TExpressionOption = (eoDERValueAsString);       // Will avoid non DER chars removed and some similar cars subtitution like ';' replaced by ':' !
  TExpressionOptions = Set of TExpressionOption;

  TLocateExpressionOption = (lePartialKey,           // Partial (all search cars must be included) but must start at position 1
                             leRelativePositionKey,  // Partial starting on any position
                             lePartialKeyWord,       // Partial word but must start at position 1        // 2017-02-01
                             leRelativePositionWord, // Partial word starting on any position            // 2017-02-01
                             leInsensitive,          // Charcase insensitive
                             leSmartKeywordRec,      // Some cars can be different
                             leSmartNumberRec);      // Smart numbers recognition

  TLocateExpressionOptions = Set of TLocateExpressionOption;

  TValidateElementValueResult =(veValueOk, veInvalidValue, veInvalidValueStopSearching, veValueTooFar);

  // 2016-10-13
  TProcExpressionRecognizedTypeChanged = procedure (Sender: TObject; const aExpressionIndex: Integer) of object;
  TProcOnExpressionMerged = procedure (Sender: TObject; aExpressionIndex, toExpressionIndex: Integer) of object;
  TProcOnRecognizeElementKeyword = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex: Integer) of object;
  TProcValidateElementValue = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex, ExpressionKeywordIndex, ExpressionValueIndex: Integer; var ValidateElementValueResult: TValidateElementValueResult) of object;
  TProcValidateElementKeyword = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex: Integer; ExpressionsList: TStrings; var Accept: Boolean) of object;
  TProcRetrieveElementValuesFromRect = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex, ExpressionKeywordIndex: Integer; ExpressionList: TStrings) of object;
  // 2016-10-10
  TProcAcceptElementTypeEquivalence = procedure (Sender: TObject; const ElementIndex, ExpressionIndex: Integer; ElementType, ExpressionType: TElementsType; var Accept: Boolean) of object;

  // For individual component ... [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TcyDocER = class(TComponent)
  private
    FPages: TList;      // TOCRPage objects list
    FExpressions: TList;  // TOCRExpression objects list
    FElements: TElements;
    FRecognitionOptions: TRecognitionOptions;
    FBeforeRecognizeElementsValues: TNotifyEvent;
    //FBeforeRecognizeElementsKeywords: TNotifyEvent;
    FBeforeRecognizeElementKeyword: TProcOnRecognizeElementKeyword;
    FAfterRecognizeElementKeyword: TProcOnRecognizeElementKeyword;
    FOnValidateElementValue: TProcValidateElementValue;
    FOnValidateElementKeyword: TProcValidateElementKeyword;
    FRecognitionPriorityMode: TRecognitionPriorityMode;
    FOnRetrieveElementValuesFromRect: TProcRetrieveElementValuesFromRect;
    FOnExpressionMerged: TProcOnExpressionMerged;
    FDayNumberSuffixes: TStrings;
    FShortMonthNames: TStrings;
    fAutoMergeExpressionsRatio: Extended;
    FExpressionOptions: TExpressionOptions;
    FOnAcceptElementTypeEquivalence: TProcAcceptElementTypeEquivalence;
    FOnExpressionRecognizedTypeChanged: TProcExpressionRecognizedTypeChanged;
    FAfterInitializeRecognition: TNotifyEvent;
    FLocateOptions: TcyLocateOptions;
    function GetPages(Index: Integer): TOCRPage;
    function GetPageCount: Integer;
    function GetExpressions(Index: Integer): TOCRExpression;
    function GetExpressionCount: Integer;
    procedure SetElements(const Value: TElements);
    procedure SetRecognitionOptions(const Value: TRecognitionOptions);
    procedure SetDayNumberSuffixes(const Value: TStrings);
    procedure SetShortMonthNames(const Value: TStrings);
    procedure SetExpressionOptions(const Value: TExpressionOptions);
    procedure SetLocateOptions(const Value: TcyLocateOptions);
  protected
    procedure ExtractTesseractBoxlineInfo(const aLine: String; var RsltCar: String; var RsltPage, RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
    function RecognizeDay(aDERString: DERString): Integer;
    function RecognizeMonth(aDERString: DERString): Integer;
    function DoStringToDERCharSet(const aString: String; const SpaceCarConversion: Char = #0): String;
  public
    Recognition_MinDate: Integer;
    Recognition_MaxDate: Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // OCR feeding :
    procedure LoadFromTesseractString(aString: String; const aPageNumber: Integer);
    procedure LoadFromTesseractFile(const aFilename: String; const aPageNumber: Word{$IFDEF UNICODE}; const aEncoding: TEncoding{$ENDIF});
    procedure LoadFromTesseractBoxStringList(aStrings: TStrings; const ImageDocumentPages, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
    procedure LoadFromTesseractBoxFile(const aFilename: String; {$IFDEF UNICODE} const aEncoding: TEncoding; {$ENDIF} const ImageDocumentPages, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
    procedure LoadFromStringList(aStrings: TStrings; const ResetPagesAndExpressions: Boolean);
    // Load / Save :
    procedure LoadFromFile(const aFilename: String; {$IFDEF UNICODE} const aEncoding: TEncoding; {$ENDIF} const ResetPagesAndExpressions: Boolean);
    procedure SaveToStringList(aStrings: TStrings);
    procedure SaveToFile(const aFilename: String{$IFDEF UNICODE}; const aEncoding: TEncoding{$ENDIF});
    // Utilities :
    function GetPagePixelsWidth(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
    function GetPagePixelsHeight(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
    function GetPageResolution(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
    function MmToPx(const MmValue: Extended; const Resolution: Integer): Integer;
    function PxToMm(const pxValue: Extended; const Resolution: Integer): Extended;
    function MmToPxFromPage(const MmValue: Extended; const PageNumber: Integer): Integer;
    function PxToMmFromPage(const pxValue: Extended; const PageNumber: Integer): Extended;
    procedure ClearPages;
    procedure ClearExpressions;
    procedure GetExpressionListFromRect(const FromRect: TRect; const FromPage: Integer; ExpressionList: TStrings; const AllowPartiallyInside: Boolean = true);
    function GetOCRText(const FromPage: Integer = 0): String;
    function GetOCRTextFromList(aExpressionList: TStrings; const SeparWithSpace: boolean): String;
    function GetOCRTextFromRect(const FromRect: TRect; const FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
    function GetAsDocumentOCRText(const FromPage: Integer = 0): String;
    function GetAsDocumentOCRTextFromRect(const FromRect: TRect; const FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
    function NewPage(PageResolution, PagePixelsWidth, PagePixelsHeight: Integer; const PageAngleRotation: integer = 0; const PageTag: Integer = 0; const PageTagStr: String = ''): Integer;
    procedure RotatePageExpressions(const PageNumber, PageWidthBeforeRotation, PageHeightBeforeRotation: Integer; const ToRight: Boolean);
    function ExpressionInRect(const aExpressionIndex: Integer; const InRect: TRect; const AllowPartiallyInside: Boolean = true): Boolean; overload;
    function ExpressionInRect(const aExpressionIndex: Integer; const LeftPx, TopPx, RightPx, BottomPx: Integer; const AllowPartiallyInside: Boolean = true): Boolean; overload;
    procedure RecognizeExpressionType(const aExpressionIndex: Integer);
    function AddExpression(const aString: String; const aPageNumber: Word; const aOCRConfidence: Extended; const aRect: TRect): Integer; overload;
    function CloneExpression(const aExpressionIndex: Integer): Integer;
    function AddExpression(const aString: String; const aPageNumber: Word; const aOCRConfidence: Extended): Integer; overload;
    function ExpressionAdded: Integer;
    procedure ExpressionLoaded;
    procedure DeleteExpression(const aExpressionIndex: Integer);
    // Locate single expression or multiple expressions by text :
    function LocateExpression(Value: String; const FromIndex, ToIndex, MaxCarErrors: Integer; const Options: TLocateExpressionOptions; var RsltCarPos: Integer): Integer;
    procedure LocateExpressionAround(const aExpressionIndex, MaxCarErrors, ScopePx: Integer; const SearchValue: String; const SearchOptions: TLocateExpressionOptions; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
    function LocateExpressions(aText: String; FromExpressionIndex, ToExpressionIndex: Integer; const Locate1stWordOptions, LocateNextWordsOptions: TLocateExpressionOptions; RsltExpressionList: TStrings; const InlineKeyword: Boolean): Boolean;
    //
    function ExpressionsInSameLine(const aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
    function ExpressionsInSameColumn(const aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
    procedure GetAroundExpressions(const aExpressionIndex, ScopePx: Integer; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
    function GetNearestExpression(const FromExpressionIndex, ScopePx: Integer; const Location: TSearchLocation): Integer;
    function GetExpressionListInsertExpressionIndex(ExpressionList: TStrings; const aExpressionIndex: Integer; const ListOrder: TSearchLocation): Integer;
    function MergeExpressions(aExpressionList: TStrings; const SeparWithSpace: boolean): Integer; overload;
    procedure MergeExpressions(aExpressionIndex, ToExpressionIndex: Integer; const SeparWithSpace: boolean); overload;
    function ExpressionsSideBySide(const ExpressionIndexAtLeft, ExpressionIndexAtRight, MaxPxSpacing: Integer): Boolean;
    function IsElementKeyword(const aElementIndex: Integer; aStr: String): Integer;
    function IsExpressionsSameValue(const ExpressionIndex1, ExpressionIndex2: Integer): Boolean;
    function FindExpression(const StartIndex, AssociatedElementIndex, AssociatedElementKeywordIndex, AssociatedExpressionKeywordIndex: Integer; const IncludeElementsTypes, ExcludeElementsTypes: TElementsTypes): Integer; overload;
    function FindExpression(const Value: Variant; const ValueType: TElementsType; const FromIndex, ToIndex: Integer): Integer; overload;
    procedure DissociateExpressions(const FromElementIndex, FromElementKeywordIndex: Integer);
    procedure InitializeRecognition;
    function PatternRectDefined(const ElementIndex: Integer): Boolean;
    function SizeRestrictionDefined(const ElementIndex: Integer): Boolean;
    function RecognizeNextExpressionElementKeyword(const ElementIndex, KeywordIndex: Integer; FromExpressionIndex: Integer; const InlineKeyword: Boolean; var Accepted: Boolean): Integer;
    function RecognizeElementValuesFromRect(const OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex: Integer; const aElementValueType: TElementsType;
     const aPageNumber, LeftPos, TopPos, RightPos, BottomPos: Integer; const ValueLocation: TSearchLocation;
     ValueNumber, ValueCount: Integer; const PatternMode: Boolean; var UserAbort: Boolean): Integer;
    procedure RecognizeLongDates;
    procedure RecognizeElementValues(const ElementIndex: Integer);
    procedure RecognizeElementKeywordsValues(const ElementIndex: Integer);
    procedure RecognizeElementsKeywords;
    procedure RecognizeElementsValues;
    procedure RecognizeElements;
    // Properties :
    property ExpressionCount: Integer read GetExpressionCount;
    property Expressions[Index: Integer]: TOCRExpression read GetExpressions; default;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TOCRPage read GetPages;
  published
    property AutoMergeExpressionsRatio: Extended read fAutoMergeExpressionsRatio write fAutoMergeExpressionsRatio;
    property DayNumberSuffixes: TStrings read FDayNumberSuffixes write SetDayNumberSuffixes;
    property ShortMonthNames: TStrings read FShortMonthNames write SetShortMonthNames;
    property Elements: TElements read FElements write SetElements;
    property ExpressionOptions: TExpressionOptions read FExpressionOptions write SetExpressionOptions default [];
    property LocateOptions: TcyLocateOptions read FLocateOptions write SetLocateOptions;
    property RecognitionOptions: TRecognitionOptions read FRecognitionOptions write SetRecognitionOptions default [roSmartNumbersRec, roSmartWebsiteRec, roSmartKeywordRec, roKeywordsByPriority];
    property RecognitionPriorityMode: TRecognitionPriorityMode read FRecognitionPriorityMode write FRecognitionPriorityMode default rpSinglelineKeywordsLength;
    //property BeforeRecognizeElementsKeywords: TNotifyEvent read FBeforeRecognizeElementsKeywords write FBeforeRecognizeElementsKeywords;
    property BeforeRecognizeElementsValues: TNotifyEvent read FBeforeRecognizeElementsValues write FBeforeRecognizeElementsValues;
    property BeforeRecognizeElementKeyword: TProcOnRecognizeElementKeyword read FBeforeRecognizeElementKeyword write FBeforeRecognizeElementKeyword;
    property AfterInitializeRecognition: TNotifyEvent read FAfterInitializeRecognition write FAfterInitializeRecognition;
    property AfterRecognizeElementKeyword: TProcOnRecognizeElementKeyword read FAfterRecognizeElementKeyword write FAfterRecognizeElementKeyword;
    property OnValidateElementKeyword: TProcValidateElementKeyword read FOnValidateElementKeyword write FOnValidateElementKeyword;
    // 2016-10-10 :
    property OnAcceptElementTypeEquivalence: TProcAcceptElementTypeEquivalence read FOnAcceptElementTypeEquivalence write FOnAcceptElementTypeEquivalence;
    property OnExpressionRecognizedTypeChanged: TProcExpressionRecognizedTypeChanged read FOnExpressionRecognizedTypeChanged write FOnExpressionRecognizedTypeChanged;
    property OnRetrieveElementValuesFromRect: TProcRetrieveElementValuesFromRect read FOnRetrieveElementValuesFromRect write FOnRetrieveElementValuesFromRect;
    property OnValidateElementValue: TProcValidateElementValue read FOnValidateElementValue write FOnValidateElementValue;
    property OnExpressionMerged: TProcOnExpressionMerged read FOnExpressionMerged write FOnExpressionMerged;
  end;

var
  DefaultDocERPagePixelsHeight, DefaultDocERPagePixelsWidth, DefaultDocERPageResolution: Integer;


implementation

{ TOCRPage }
procedure TOCRPage.SetTagStr(const Value: String);
begin
  FTagStr := Value;

  if FTagStr <> '' then
    String_SubstFast(';', '', FTagStr);
end;

{ TOCRExpression }
function TOCRExpression.RecognizedFloatValue: Extended;
begin
  Result := 0;

  if FRecognizedFloat <> '' then
    Result := StrToFloat(FRecognizedFloat, cyDERUtils.LocalFormatSettings);
end;

function TOCRExpression.RecognizedDateValue: TDate;
begin
  Result := 0;

  if FRecognizedDate <> '' then
    Result := StrToInt(FRecognizedDate);
end;

function TOCRExpression.RecognizedValue: String;
begin
  case FRecognizedType of
    etText:              Result := FValue;
    etTextLine:          Result := FValue;
    etParagraph:         Result := FValue;
    etExpressionKeyWord: Result := FValue;
    etNumbers:           Result := FRecognizedNumbers;
    etInteger:           Result := FRecognizedInteger;
    etFloat:             Result := FRecognizedFloat;
    etPercentage:        Result := FRecognizedPercentage;
    etwebSite:           Result := FRecognizedWebsite;
    etWebMail:           Result := FRecognizedWebMail;
    etMoney:             Result := FRecognizedMoney;
    etDate:              Result := FRecognizedDate;
    etMonthYear:         Result := FRecognizedDate;
    etID:                Result := FDERValue;          // 2017-02-03 ..
  end;
end;

{ TElement }
constructor TElement.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDescription := '';
  FKeyWords := TStringList.Create;
  FKeywordLookingValueDirection := ldOnRightIfNotFoundBottom;
  FPatternPageNumber := 0;
  PatternPageNumberMode := ppFromBeginning;
  FPatternFromLeftMm := 0;
  FPatternFromTopMm := 0;
  FPatternToRightMm := 0;
  FPatternToBottomMm := 0;
  FPatternValueSizeMm := 0;
  FPatternPositionMode := ppFromTopLeftPage;
  FPatternValueLocation := slFromBottom;
  FPatternValueNumber := 1;
  FValueCount := 1;
  FValueType := etMoney;
  FValueMinCars := 0;
  FValueMaxCars := 0;
  FTag := 0;
  FTag2 := 0;
  FTagStr := '';
end;

destructor TElement.Destroy;
begin
  FKeyWords.Free;
  inherited;
end;

function TElement.GetDisplayName: string;
var k: Integer;
begin
  if FDescription <> ''
  then Result := FDescription + ' ('
  else Result := '(';

  for k := 0 to FKeyWords.Count-1 do
    if Result = ''
    then Result := FKeyWords[k]
    else Result := Result + ', ' + FKeyWords[k];

  Result := Result + ')';
end;

procedure TElement.SetKeyWords(const Value: TStrings);
begin
  if Assigned(FKeyWords) then
    FKeyWords.Assign(Value)
  else
    FKeyWords := Value;
end;

procedure TElement.Assign(Source: TPersistent);
begin
  if Source is TElement then
  begin
    FDescription := TElement(Source).FDescription;
    FKeyWords  := TElement(Source).FKeyWords;
    FPatternPageNumber := TElement(Source).FPatternPageNumber;
    FPatternPageNumberMode := TElement(Source).FPatternPageNumberMode;
    FPatternFromLeftMm := TElement(Source).FPatternFromLeftMm;
    FPatternFromTopMm := TElement(Source).FPatternFromTopMm;
    FPatternToRightMm := TElement(Source).FPatternToRightMm;
    FPatternToBottomMm := TElement(Source).FPatternToBottomMm;
    FPatternValueSizeMm := TElement(Source).FPatternValueSizeMm;
    FPatternPositionMode := TElement(Source).FPatternPositionMode;
    FPatternValueLocation := TElement(Source).FPatternValueLocation;
    FPatternValueNumber := TElement(Source).FPatternValueNumber;
    FValueCount := TElement(Source).FValueCount;
    FValueType := TElement(Source).FValueType;
    FValueMinCars := TElement(Source).FValueMinCars;
    FValueMaxCars := TElement(Source).FValueMaxCars;
    FTag := TElement(Source).FTag;
    FTag2 := TElement(Source).FTag2;
    FTagStr := TElement(Source).FTagStr;
  end;
//  inherited Assign(Source);
end;

{ TElements }
constructor TElements.Create(aDocument: TcyDocER; ElementClass: TElementClass);
begin
  inherited Create(ElementClass);
  FOwner := aDocument;
end;

function TElements.Add: TElement;
begin
  Result := TElement(inherited Add);
  Result.Changed(false);      // It will call TcyElements.Update only at run-time!
end;

procedure TElements.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

function TElements.GetElement(Index: Integer): TElement;
begin
  Result := TElement(inherited Items[Index]);
end;

function TElements.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// Event Called by setting properties/events of TElement :
procedure TElements.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


{ TcyLocateOptions }
constructor TcyLocateOptions.Create(AOwner: TComponent);
begin
  FIgnoreChars := '.:()-_/\';
end;

destructor TcyLocateOptions.Destroy;
begin

  inherited;
end;

procedure TcyLocateOptions.Assign(Source: TPersistent);
begin
  if Source is TcyLocateOptions then
  begin
    FIgnoreChars := TcyLocateOptions(Source).FIgnoreChars;
  end;
//  inherited Assign(Source);
end;

// 2017-02-22 Sanytize for use of Locate* functions
procedure TcyLocateOptions.Sanytize(var aText: string; const ReplaceWith: String = '');
var
  c: Integer;
begin
  // 2017-05-11 ... String_SubstFast('.', '', aText);
  // 2017-05-11 ... String_SubstFast('(', '', aText);
  // 2017-05-11 ... String_SubstFast(')', '', aText);

  // 2017-05-11 :
  for c := 1 to Length(fIgnoreChars) do
    String_SubstFast(fIgnoreChars[c], ReplaceWith, aText);
end;

{ TcyDocER }
constructor TcyDocER.Create(AOwner: TComponent);
var
  m: Integer;
  {$IFDEF DCC} // Directive that tells us that is a Delphi compiler since Delphi XE2 !!!
  fs: TFormatSettings;
  {$ENDIF DCC}
begin
  inherited Create(AOwner);

  Recognition_MinDate := 1970;
  Recognition_MaxDate := 2050;

  FPages := TList.Create;
  FExpressions := TList.Create;
  FDayNumberSuffixes := TStringList.Create;
  FShortMonthNames := TStringList.Create;

  {$IFDEF DCC} // Directive that tells us that is a Delphi compiler since Delphi XE2 !!!
  fs := TFormatSettings.Create;
  for m := 1 to 12 do
    FShortMonthNames.Add(fs.ShortMonthNames[m]);
  {$ELSE} // Delphi other
  for m := 1 to 12 do
    FShortMonthNames.Add(SysUtils.ShortMonthNames[m]);
  {$ENDIF DCC}

  // Default day number suffixes :
  FDayNumberSuffixes.Add('st');
  FDayNumberSuffixes.Add('nd');
  FDayNumberSuffixes.Add('rd');
  FDayNumberSuffixes.Add('th');

  FElements := TElements.Create(self, TElement);
  FAutoMergeExpressionsRatio := 1.5;

  FLocateOptions := TcyLocateOptions.Create(self);

  FExpressionOptions := [];
  FRecognitionOptions := [roKeywordsByPriority, roSmartNumbersRec, roSmartWebsiteRec, roSmartKeywordRec];
  FRecognitionPriorityMode := rpSinglelineKeywordsLength;
end;

destructor TcyDocER.Destroy;
begin
  ClearPages;
  ClearExpressions;
  FDayNumberSuffixes.Free;
  FShortMonthNames.Free;
  FLocateOptions.Free;
  FElements.Free;
  FElements := Nil;
  FPages.Free;
  FExpressions.Free;
  inherited;
end;

procedure TcyDocER.DissociateExpressions(const FromElementIndex, FromElementKeywordIndex: Integer);
var i: Integer;
begin
  for i := 0 to ExpressionCount-1 do
    if (Expressions[i].FAssociatedElementIndex = FromElementIndex) or (FromElementIndex = -1) then
      if (Expressions[i].FAssociatedElementKeywordIndex = FromElementKeywordIndex) or (FromElementKeywordIndex = -1) then
      begin
        Expressions[i].FAssociatedElementIndex := -1;
        Expressions[i].FAssociatedElementKeywordIndex := -1;
        Expressions[i].FAssociatedExpressionKeywordIndex := -1;
        if Expressions[i].FRecognizedType = etExpressionKeyWord then
          RecognizeExpressionType(i);
      end;
end;

function TcyDocER.DoStringToDERCharSet(const aString: String; const SpaceCarConversion: Char = #0): String;
begin
  (* 2017-02-06
  if eoDERValueAsString in FExpressionOptions then
  begin
    // 2017-02-01 Result := aString;
    Result := String_RemoveAccentsFromChars(aString);

    String_SubstFast(',', '.', Result);  // For float recognition ...
    String_SubstFast('°', 'º', Result);  // Not same character!!!    Ord('°') = 176     Ord('º') = 186

    if SpaceCarConversion <> #0
    then String_SubstFast(' ', SpaceCarConversion, Result)
    else String_SubstFast(' ', '', Result);  // For float recognition, after merging 2 expressions ...
  end
  else *)

  Result := StringToDERCharSet(aString, eoDERValueAsString in FExpressionOptions, SpaceCarConversion);
end;

procedure TcyDocER.SetDayNumberSuffixes(const Value: TStrings);
begin
  if Assigned(FDayNumberSuffixes) then
    FDayNumberSuffixes.Assign(Value)
  else
    FDayNumberSuffixes := Value;
end;

procedure TcyDocER.SetShortMonthNames(const Value: TStrings);
begin
  if Assigned(FShortMonthNames) then
    FShortMonthNames.Assign(Value)
  else
    FShortMonthNames := Value;
end;

procedure TcyDocER.SetElements(const Value: TElements);
begin
  FElements := Value;
end;

procedure TcyDocER.SetExpressionOptions(const Value: TExpressionOptions);
begin
  FExpressionOptions := Value;
end;

procedure TcyDocER.SetLocateOptions(const Value: TcyLocateOptions);
begin
  FLocateOptions.Assign(Value);
end;

procedure TcyDocER.SetRecognitionOptions(const Value: TRecognitionOptions);
begin
  FRecognitionOptions := Value;
end;

procedure TcyDocER.ClearPages;
var i: Integer;
begin
  // Free Pages :
  for i := 0 to PageCount-1 do
    try
      Pages[i].Free;
    except
    end;

  FPages.Clear;
end;

procedure TcyDocER.ClearExpressions;
var i: Integer;
begin
  // Free Expressions :
  for i := 0 to ExpressionCount-1 do
    try
      Expressions[i].Free;
    except
    end;

  FExpressions.Clear;
end;

function TcyDocER.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TcyDocER.GetPages(Index: Integer): TOCRPage;
begin
  Result := FPages[Index];
end;

function TcyDocER.GetExpressionCount: Integer;
begin
  Result := FExpressions.Count;
end;

function TcyDocER.GetExpressions(Index: Integer): TOCRExpression;
begin
  Result := FExpressions[Index];
end;

procedure TcyDocER.RecognizeExpressionType(const aExpressionIndex: Integer);
var
  _Numbers, _Integer, _Float, _Percentage, _webSite, _WebMail, _Money, _Date: String;
begin
  Expressions[aExpressionIndex].FDERValue := DoStringToDERCharSet(Expressions[aExpressionIndex].FValue);

  Expressions[aExpressionIndex].FRecognizedType := DERExecute(Expressions[aExpressionIndex].FDERValue,_Numbers,_Integer,_Float,_Percentage,_webSite,_WebMail,_Money,_Date,
                                                      (roSmartNumbersRec in FRecognitionOptions), roSmartWebsiteRec in FRecognitionOptions);
  Expressions[aExpressionIndex].FRecognizedDate       := _Date;
  Expressions[aExpressionIndex].FRecognizedMoney      := _Money;
  Expressions[aExpressionIndex].FRecognizedWebsite    := _Website;
  Expressions[aExpressionIndex].FRecognizedWebMail    := _WebMail;
  Expressions[aExpressionIndex].FRecognizedPercentage := _Percentage;
  Expressions[aExpressionIndex].FRecognizedFloat      := _Float;
  Expressions[aExpressionIndex].FRecognizedInteger    := _Integer;
  Expressions[aExpressionIndex].FRecognizedNumbers    := _Numbers;

  if Assigned(FOnExpressionRecognizedTypeChanged) then
    FOnExpressionRecognizedTypeChanged(Self, aExpressionIndex);
end;

function TcyDocER.AddExpression(const aString: String; const aPageNumber: Word; const aOCRConfidence: Extended; const aRect: TRect): Integer;
begin
  //if DebugHook <> 0 then
  //  if aString = '°' then
  //    Beep;

  Result := AddExpression(aString, aPageNumber, aOCRConfidence);

  if Result <> -1 then
  begin
    Expressions[Result].FLeftPxPos := aRect.Left;
    Expressions[Result].FTopPxPos := aRect.Top;
    Expressions[Result].FRightPxPos := aRect.Right;
    Expressions[Result].FBottomPxPos := aRect.Bottom;

    Result := ExpressionAdded;  // Merge Expression in some cases ...
  end;
end;

function TcyDocER.CloneExpression(const aExpressionIndex: Integer): Integer;
begin
  Result := AddExpression(Expressions[aExpressionIndex].Value, Expressions[aExpressionIndex].PageNumber, Expressions[aExpressionIndex].OCRConfidence);

  if Result <> -1 then
  begin
    Expressions[Result].FLeftPxPos := Expressions[aExpressionIndex].FLeftPxPos;
    Expressions[Result].FTopPxPos := Expressions[aExpressionIndex].FTopPxPos;
    Expressions[Result].FRightPxPos := Expressions[aExpressionIndex].FRightPxPos;
    Expressions[Result].FBottomPxPos := Expressions[aExpressionIndex].FBottomPxPos;

    // No merge ... Result := ExpressionAdded;
  end;
end;

function TcyDocER.AddExpression(const aString: String; const aPageNumber: Word; const aOCRConfidence: Extended): Integer;
var
  NewExpression: TOCRExpression;
begin
  Result := -1;
  if aString = '' then Exit;
  if DoStringToDERCharSet(aString) = '' then Exit;

  NewExpression := TOCRExpression.Create;
  FExpressions.Add(NewExpression);
  Result := FExpressions.Count - 1;

  Expressions[Result].FValue := aString;
  Expressions[Result].FPageNumber := aPageNumber;
  Expressions[Result].FAssociatedElementIndex := -1;
  Expressions[Result].FAssociatedElementKeywordIndex := -1;
  Expressions[Result].FAssociatedExpressionKeywordIndex := -1;
  Expressions[Result].FLeftPxPos := -1;
  Expressions[Result].FTopPxPos := -1;
  Expressions[Result].FRightPxPos := -1;
  Expressions[Result].FBottomPxPos := -1;
  Expressions[Result].FOCRConfidence := aOCRConfidence;

  // Recognize :
  RecognizeExpressionType(Result);

  // Result := ExpressionAdded() No need to call here because we don' t have pxPos ...
end;

procedure TcyDocER.DeleteExpression(const aExpressionIndex: Integer);
begin
  if aExpressionIndex < 0 then
    raise Exception.Create('Cannot remove expression index below 0!');

  if aExpressionIndex > FExpressions.Count - 1 then
    raise Exception.Create('Cannot remove expression index above max.!');

  // Free stored object :
  Expressions[aExpressionIndex].Free;

  // Delete from list :
  FExpressions.Delete(aExpressionIndex);
end;

function TcyDocER.NewPage(PageResolution, PagePixelsWidth, PagePixelsHeight: Integer; const PageAngleRotation: integer = 0; const PageTag: Integer = 0; const PageTagStr: String = ''): Integer;
var
  aPage: TOCRPage;
begin
  Result := -1;

  aPage := TOCRPage.Create;
  FPages.Add(aPage);
  Result := FPages.Count - 1;

  // 2016-04-06 Can' t be 0 :
  if PagePixelsHeight = 0 then PagePixelsHeight := DefaultDocERPagePixelsHeight;
  if PagePixelsWidth = 0 then PagePixelsWidth := DefaultDocERPagePixelsWidth;
  if PageResolution = 0 then PageResolution := DefaultDocERPageResolution;

  Pages[Result].FTag           := PageTag;
  Pages[Result].FTagStr        := PageTagStr;
  Pages[Result].FPixelsWidth   := PagePixelsWidth;
  Pages[Result].FPixelsHeight  := PagePixelsHeight;
  Pages[Result].FResolution    := PageResolution;
  Pages[Result].FAngleRotation := PageAngleRotation;
end;

// Retrieve Tesseract line information into variables :
procedure TcyDocER.ExtractTesseractBoxlineInfo(const aLine: String; var RsltCar: String; var RsltPage, RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
var
  PagePixelsHeight: Integer;
begin
  try
    RsltCar    := (aLine + ' ')[1];
    RsltPage   := StrToInt( SubString_Get(aLine, ' ', 6) ) + 1;
    RsltLeft   := StrToInt( SubString_Get(aLine, ' ', 2) );

    if (RsltPage > 0) and (RsltPage <= PageCount)
    then PagePixelsHeight := Pages[RsltPage-1].FPixelsHeight
    else PagePixelsHeight := DefaultDocERPagePixelsHeight;

    RsltBottom := PagePixelsHeight - StrToInt( SubString_Get(aLine, ' ', 3) );
    RsltRight  := StrToInt( SubString_Get(aLine, ' ', 4) );
    RsltTop    := PagePixelsHeight - StrToInt( SubString_Get(aLine, ' ', 5) );
  except
  end;
end;

function TcyDocER.FindExpression(const StartIndex, AssociatedElementIndex, AssociatedElementKeywordIndex, AssociatedExpressionKeywordIndex: Integer;
  const IncludeElementsTypes, ExcludeElementsTypes: TElementsTypes): Integer;
var i: Integer;
begin
  Result := -1;
  for i := StartIndex to ExpressionCount-1 do
    if (AssociatedElementIndex = -1) or (Expressions[i].AssociatedElementIndex = AssociatedElementIndex) then
      if (AssociatedElementKeywordIndex = -1) or (Expressions[i].AssociatedElementKeywordIndex = AssociatedElementKeywordIndex) then
        if (AssociatedExpressionKeywordIndex = -1) or (Expressions[i].AssociatedExpressionKeywordIndex = AssociatedExpressionKeywordIndex) then
          if (IncludeElementsTypes = []) or (Expressions[i].FRecognizedType in IncludeElementsTypes) then
            if not (Expressions[i].FRecognizedType in ExcludeElementsTypes) then
            begin
              Result := i;
              Break;
            end;
end;

procedure TcyDocER.LoadFromTesseractFile(const aFilename: String; const aPageNumber: Word{$IFDEF UNICODE}; const aEncoding: TEncoding{$ENDIF});
var
  FileLines: TStrings;
begin
  ClearPages;
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromTesseractString(FileLines.Text, aPageNumber);
  FileLines.Free;
end;

procedure TcyDocER.LoadFromFile(const aFilename: String; {$IFDEF UNICODE} const aEncoding: TEncoding; {$ENDIF} const ResetPagesAndExpressions: Boolean);
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromStringList(FileLines, ResetPagesAndExpressions);
  FileLines.Free;
end;

procedure TcyDocER.LoadFromStringList(aStrings: TStrings; const ResetPagesAndExpressions: Boolean);
var
  MaxPages, l, LengthStrLine: Integer;
  StrLine: String;
  ExpressionString: String;
  ExpressionPageNumber: Integer;
  ExpressionOCRConfidence: Extended;
  ExpressionLeft, ExpressionTop, ExpressionRight, ExpressionBottom: Integer;
  PagePixelsWidth, PagePixelsHeight, PageResolution, PageTag, PageAngleRotation: Integer;
  PageTagStr: String;
const
  SeparatorCar = ';';

        // Reading from right !!!
        function CutLineValue: String;
        begin
          Result := '';
          while LengthStrLine > 0 do
            if StrLine[LengthStrLine] = SeparatorCar then
            begin
              Delete(StrLine, LengthStrLine, 1);
              Dec(LengthStrLine);
              Break;
            end
            else begin
              Result := StrLine[LengthStrLine] + Result;
              Delete(StrLine, LengthStrLine, 1);
              Dec(LengthStrLine);
            end;
        end;

begin
  // 2017-03-08
  if ResetPagesAndExpressions then
  begin
    ClearExpressions;
    ClearPages;
  end;

  if aStrings.Count = 0 then
    Exit;

  // *** Header *** //
  StrLine := aStrings[0];
  LengthStrLine := Length(StrLine);

  // On old versions, we have only 2 separators to separate PagePixelsHeight + SeparatorCar + PagePixelsWidth + SeparatorCar + PageResolution
  if SubString_Count(StrLine, SeparatorCar) > 3 then   // Detect new version with multipages definitions ...
  begin
    // 2015-11-12 Load definition for each page !
    while LengthStrLine <> 0 do
    begin
      PageTagStr := CutLineValue;
      if not TryStrToInt(CutLineValue, PageTag) then PageTag := 0;
      if not TryStrToInt(CutLineValue, PageAngleRotation) then PageAngleRotation := 0;

      if not TryStrToInt(CutLineValue, PagePixelsHeight) then PagePixelsHeight := DefaultDocERPagePixelsHeight;
      if not TryStrToInt(CutLineValue, PagePixelsWidth) then PagePixelsWidth := DefaultDocERPagePixelsWidth;
      if not TryStrToInt(CutLineValue, PageResolution) then PageResolution := DefaultDocERPageResolution;

      NewPage(PageResolution, PagePixelsWidth, PagePixelsHeight, PageAngleRotation, PageTag, PageTagStr);
    end;
  end
  else begin
    // Compatibility with old versions without multipage definitions :
    PageAngleRotation := 0;
    PageTag := 0;
    PageTagStr := '';
    if not TryStrToInt(CutLineValue, PagePixelsHeight) then PagePixelsHeight := DefaultDocERPagePixelsHeight;
    if not TryStrToInt(CutLineValue, PagePixelsWidth) then PagePixelsWidth := DefaultDocERPagePixelsWidth;
    if not TryStrToInt(CutLineValue, PageResolution) then PageResolution := DefaultDocERPageResolution;

    // 2016-04-06 :
    NewPage(PageResolution, PagePixelsWidth, PagePixelsHeight, PageAngleRotation, PageTag, PageTagStr);
  end;

  MaxPages := 0;

  // *** Expressions *** //
  for l := 1 to aStrings.Count-1 do
  begin
    StrLine := aStrings[l];
    LengthStrLine := Length(StrLine);

    ExpressionPageNumber := 0;
    ExpressionOCRConfidence := 0;
    ExpressionLeft := 0;
    ExpressionTop := 0;
    ExpressionRight := 0;
    ExpressionBottom := 0;

    try ExpressionBottom := StrToInt(CutLineValue); except end;
    try ExpressionRight := StrToInt(CutLineValue); except end;
    try ExpressionTop := StrToInt(CutLineValue); except end;
    try ExpressionLeft := StrToInt(CutLineValue); except end;
    ExpressionOCRConfidence := cyStrUtils.String_ToFloat(CutLineValue);
    try ExpressionPageNumber := StrToInt(CutLineValue); except end;
    ExpressionString := StrLine;  // No matter if Strline contains SeparatorCar !

    if ExpressionPageNumber > MaxPages then
      MaxPages := ExpressionPageNumber;

    if AddExpression(ExpressionString, ExpressionPageNumber, ExpressionOCRConfidence, classes.Rect(ExpressionLeft, ExpressionTop, ExpressionRight, ExpressionBottom)) <> -1 then
      ExpressionLoaded;        // Retrieve short/long date :
  end;

  // Add all pages for retro compatibility (we don' t save definition for each page previously) :
  while PageCount < MaxPages do
    NewPage(PageResolution, PagePixelsWidth, PagePixelsHeight);
end;

procedure TcyDocER.SaveToFile(const aFilename: String{$IFDEF UNICODE}; const aEncoding: TEncoding{$ENDIF});
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  SaveToStringList(FileLines);
  FileLines.SaveToFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  FileLines.Free;
end;

procedure TcyDocER.SaveToStringList(aStrings: TStrings);
var
  i: Integer;
  Str, StrLine: String;

const
  SeparatorCar = ';';

begin
  aStrings.Clear;

  // *** Header *** //
  // OLD ... aStrings.Add(intToStr(FResolution) + SeparatorCar + intToStr(FPixelsWidth) + SeparatorCar + intToStr(FPixelsHeight));

  // 2015-11-12 Save definition for each page !
  StrLine := '';
  for i := 0 to PageCount-1 do
  begin
    if i <> 0 then
      StrLine := StrLine + SeparatorCar;  // Page separator ...

    StrLine := StrLine + intToStr(Pages[i].FResolution) + SeparatorCar + intToStr(Pages[i].FPixelsWidth) + SeparatorCar + intToStr(Pages[i].FPixelsHeight)
        + SeparatorCar + intToStr(Pages[i].FAngleRotation) + SeparatorCar + intToStr(Pages[i].FTag) + SeparatorCar + Pages[i].FTagStr;
  end;

  aStrings.Add(StrLine);

  // *** Expressions *** //
  for i := 0 to ExpressionCount-1 do
  begin
    Str := Expressions[i].Value;

    StrLine := Str + SeparatorCar + intToStr(Expressions[i].FPageNumber) + SeparatorCar + FloatToStr(Expressions[i].FOCRConfidence) + SeparatorCar
                + intToStr(Expressions[i].FLeftPxPos) + SeparatorCar + intToStr(Expressions[i].FTopPxPos) + SeparatorCar
                 + intToStr(Expressions[i].FRightPxPos) + SeparatorCar + intToStr(Expressions[i].FBottomPxPos);
    aStrings.Add(StrLine);
  end;
end;

procedure TcyDocER.LoadFromTesseractString(aString: String; const aPageNumber: Integer);
var
  i, lengthString: Integer;
  StrExpression: String;
begin
  while PageCount < aPageNumber do
    NewPage(DefaultDocERPageResolution, DefaultDocERPagePixelsWidth, DefaultDocERPagePixelsHeight);

  StrExpression := '';
  aString := aString + ' ';
  lengthString := length(aString);

  repeat
    aString := String_Subst(' %', '%', aString, csCaseSensitive, true);
  until pos(' %', aString) = 0;

  repeat
    aString := String_Subst(' €', '€', aString, csCaseSensitive, true);
  until pos(' €', aString) = 0;

  // Add all words from file :
  for i := 1 to lengthString do
    {$IFDEF UNICODE}
    if (CharInSet(aString[i], [#$D, #$A, ' '])) or (i = lengthString) then
    {$ELSE}
    if (aString[i] in [#$D, #$A, ' ']) or (i = lengthString) then
    {$ENDIF}
    begin
      AddExpression(StrExpression, aPageNumber, 0);
      StrExpression := '';
    end
    else
      StrExpression := StrExpression + aString[i];
end;

procedure TcyDocER.LoadFromTesseractBoxFile(const aFilename: String; {$IFDEF UNICODE} const aEncoding: TEncoding; {$ENDIF} const ImageDocumentPages, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromTesseractBoxStringList(FileLines, ImageDocumentPages, ImageResolution, ImagePixelsWidth, ImagePixelsHeight);
  FileLines.Free;
end;

// Convert Tesseract box file into Expressions :
procedure TcyDocER.LoadFromTesseractBoxStringList(aStrings: TStrings; const ImageDocumentPages, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
var
  l, LineCount: Integer;
  CarValue: String;
  CarPage, CarLeft, CarTop, CarRight, CarBottom: Integer;
  LineCars: Array of TOCRCarInfo;
  LineCarCount: Integer;
  MinLineTop, MaxLineBottom: Integer;

        // **************************************************************************************************************************

        procedure CutLineIntoExpressions;

                  // Handle small heights ("-" car) and small width ("i" car)
                  // If Height * 0.75 > Width ("i" char for exemple), we try to calculate correct font size
                  function CalcFontSize(CarIndex: Integer): Integer;
                  var MinSize, _width, _Height: Integer;
                  begin
                    MinSize := Round( (ImageResolution / 100) * 6 );    // Min font size recognition = 6
                    _Width  := LineCars[CarIndex].Right - LineCars[CarIndex].Left + 1;
                    _Height := LineCars[CarIndex].Bottom - LineCars[CarIndex].Top + 1;

                    if _Width < MinSize then
                      _Width := MinSize;

                    if _Height < MinSize then
                      _Height := MinSize;

                    if _Height * 0.75 > _Width then
                    begin
                      if _Height / _width <= 3
                      then
                        Result := Round((_Height * 0.9) * 0.8 + _Width * 0.2)  // H20/W7 = 20*0.9*0.8+7*0.2 = 15,8
                      else
                        if _Height / _Width <= 4.5
                        then Result := Round((_Height * 0.75) * 0.8 + _Width * 0.2)      // H25/W6 = 25*0.75*0.8+6*0.2 = 16,2
                        else Result := Round(_Height * 0.8);                             // H20/W4 = 20*0.8 = 16
                    end
                    else
                      if _Height > _Width
                      then Result := _Height
                      else Result := _Width;

                    if LineCars[CarIndex].Value = '@' then
                      Result := Round(Result * 0.6);
                  end;

                  // Space char is aproximatively 1/3 to 1/2 of font size
                  // Letters Spacing between cars is 15% font' s size
                  // Handle Courier new space char + anormal space between chars
                  // Handle when some word's caracters are embbeded into another ...
                  function CarBelongsToWord(CarIndex: Integer; LettersSpacing, NormalFontSize: Integer): Boolean;
                  var
                    MaxSpaceCharWidth: Integer;
                  begin
                    if NormalFontSize = 0 then
                    begin
                      Result := true;
                      Exit;
                    end;

                    Result := false;

                    // letters embedded into another (Logo or italic font):
                    if LettersSpacing < 0 then
                      LettersSpacing := 0;

                    if CalcFontSize(CarIndex) > NormalFontSize then
                      NormalFontSize := CalcFontSize(CarIndex);

                    // Handling spacing between "1" followed by another "1" (the normal char spacing is raised by 1/4 font px height):
                    {$IFDEF UNICODE}
                    if (CarIndex > 0) and (CharInSet(LineCars[CarIndex].Value[1], ['l', '1'])) then
                    {$ELSE}
                    if (CarIndex > 0) and (LineCars[CarIndex].Value[1] in ['l', '1']) then
                    {$ENDIF}
                      {$IFDEF UNICODE}
                      if CharInSet(LineCars[CarIndex-1].Value[1], ['l', '1']) then
                      {$ELSE}
                      if LineCars[CarIndex-1].Value[1] in ['l', '1'] then
                      {$ENDIF}
                        Inc(LettersSpacing, (LineCars[CarIndex].Bottom - LineCars[CarIndex].Top) div 4);

                    // Handling anormal spacing between number and decimal separator for float/money values (exemple: " 3 8  ,  0 0  € ") :
                    {$IFDEF UNICODE}
                    if (CarIndex > 0) and (CharInSet(LineCars[CarIndex].Value[1], [',', '.'])) then
                    {$ELSE}
                    if (CarIndex > 0) and (LineCars[CarIndex].Value[1] in [',', '.']) then
                    {$ENDIF}
                      {$IFDEF UNICODE}
                      if CharInSet(LineCars[CarIndex-1].Value[1], ['0'..'9']) then
                      {$ELSE}
                      if LineCars[CarIndex-1].Value[1] in ['0'..'9'] then
                      {$ENDIF}
                        Inc(LettersSpacing, NormalFontSize div 2);    // Ok ...

                    // 0.25 = (normal char space width (33% font size) + Normal Spacing between cars (15% font size)) div 2 :
                    MaxSpaceCharWidth := Round(NormalFontSize * 0.261);  // 0.26 is not enougth for numbers recognition ("01/11/2011")

                    if LineCars[CarIndex].Left - LineCars[CarIndex-1].Right < LettersSpacing + MaxSpaceCharWidth then
                      Result := true;
                  end;

        var
          i, j, CharFontSize, LettersSpacing, CurrentWordNumber: Integer;
          aWord: String;
          aRect: TRect;
        begin
          if LineCarCount = 0 then Exit;

          if LineCarCount = 1 then
          begin
            AddExpression(LineCars[0].Value, LineCars[0].PageNumber, 0, Rect(LineCars[0].Left, LineCars[0].Top, LineCars[0].Right, LineCars[0].Bottom));
            Exit;
          end;

          // At least, we have 2 chars :
          CharFontSize := 0;
          CurrentWordNumber := 1;
          LettersSpacing := LineCars[LineCarCount-1].Right - LineCars[0].Left;

          // Detect words:
          for i := 0 to LineCarCount-1 do
          begin
            LineCars[i].WordNumber := CurrentWordNumber;

            j := i;
            // Will allow detect single char word followed by spacing! exemple: "tel : 000"  ...
            while CarBelongsToWord(j, LettersSpacing, CharFontSize) do
            begin
              // Smallest char spacing between chars from same word:
              if CharFontSize <> 0 then   // not first word char
                if LineCars[j].Left - LineCars[j-1].Right < LettersSpacing then
                  LettersSpacing := LineCars[j].Left - LineCars[j-1].Right;

              // Bigger char size :
              if CalcFontSize(j) > CharFontSize then
                CharFontSize := CalcFontSize(j);

              inc(j);
              if j > LineCarCount-1 then
                Break;
            end;

            // Detect if next char is in a new word:
            if i <> LineCarCount-1 then
            begin
              if not CarBelongsToWord(i+1, LettersSpacing, CharFontSize) then
              begin
                Inc(CurrentWordNumber);
                CharFontSize := 0;
                LettersSpacing := LineCars[LineCarCount-1].Right - LineCars[i+1].Left;
              end;
            end;
          end;

          // Create Expressions :
          aWord := '';
          aRect := Rect(-1, -1, -1, -1);
          CurrentWordNumber := 1;
          for i := 0 to LineCarCount-1 do
          begin
            if LineCars[i].WordNumber <> CurrentWordNumber then
            begin
              AddExpression(aWord, LineCars[i-1].PageNumber, 0, aRect);
              CurrentWordNumber := LineCars[i].WordNumber;
              aWord := '';
              aRect := Rect(-1, -1, -1, -1);
            end;

            // Build Word cars + Rect:
            aWord := aWord + LineCars[i].Value;
            if (LineCars[i].Left < aRect.Left) or (aRect.Left = -1) then aRect.Left := LineCars[i].Left;
            if (LineCars[i].Top < aRect.Top) or (aRect.Top = -1) then aRect.Top := LineCars[i].Top;
            if (LineCars[i].Right > aRect.Right) or (aRect.Right = -1) then aRect.Right := LineCars[i].Right;
            if (LineCars[i].Bottom > aRect.Bottom) or (aRect.Bottom = -1) then aRect.Bottom := LineCars[i].Bottom;
          end;

          // Add last word :
          AddExpression(aWord, LineCars[LineCarCount-1].PageNumber, 0, aRect);
        end;


        function CarInSameLine: Boolean;
        var y: Integer;
        begin
          Result := True;
          if LineCarCount = 0 then Exit;  // No cars

          Result := false;
          if LineCars[0].PageNumber <> CarPage then Exit;

          // See if char is just after last inserted in LineCars []:
          // Handle bricked Logo letters (like Tetris pieces)
          // '.' char followed by italic letter like 'f'can have same left position
          if CarRight > LineCars[LineCarCount-1].Right then
          // Not work if chars begins at same left position ... if CarLeft >= LineCars[LineCarCount-1].Right - (LineCars[LineCarCount-1].Right-LineCars[LineCarCount-1].Left) div 2 then
          begin
            // Check horizontal position :
            y := CarTop + (CarBottom - CarTop) div 2;
            if (y >= MinLineTop) and (y <= MaxLineBottom) then
              Result := true;
          end;
        end;

begin
  while PageCount < ImageDocumentPages do
    NewPage(ImageResolution, ImagePixelsWidth, ImagePixelsHeight);

  l := 0;
  LineCount := aStrings.Count;

  LineCarCount := 0;
  SetLength(LineCars, 0);
  MinLineTop := -1;
  MaxLineBottom := -1;

  while l <= LineCount-1 do    // All caracters ...
  begin
    ExtractTesseractBoxlineInfo(aStrings[l], CarValue, CarPage, CarLeft, CarTop, CarRight, CarBottom);

    if not CarInSameLine then
    begin
      // Cut into Expressions :
      CutLineIntoExpressions;

      LineCarCount := 0;
      SetLength(LineCars, 0);
      MinLineTop := -1;
      MaxLineBottom := -1;
    end;

    Inc(LineCarCount);
    SetLength(LineCars, LineCarCount);
    LineCars[LineCarCount-1].Value := CarValue;
    LineCars[LineCarCount-1].PageNumber := CarPage;
    LineCars[LineCarCount-1].Left := CarLeft;
    LineCars[LineCarCount-1].Top := CarTop;
    LineCars[LineCarCount-1].Right := CarRight;
    LineCars[LineCarCount-1].Bottom := CarBottom;
    LineCars[LineCarCount-1].WordNumber := 0;     // Not defined yet

    if (CarTop < MinLineTop) or (MinLineTop = -1) then
      MinLineTop := CarTop;

    if (CarBottom > MaxLineBottom) or (MaxLineBottom = -1) then
      MaxLineBottom := CarBottom;

    inc(l);
  end;

  // Cut last line into Expressions :
  CutLineIntoExpressions;
end;

function TcyDocER.FindExpression(const Value: Variant; const ValueType: TElementsType; const FromIndex, ToIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  try
    for i := FromIndex to ToIndex do
    begin
      case ValueType of
        etText, etExpressionKeyWord, etTextLine, etParagraph, etID:
        begin
          if AnsiUpperCase(Expressions[i].FDERValue) = AnsiUpperCase(DoStringToDERCharSet(Value)) then
            Result := i;
        end;

        etNumbers:
        begin
          if Expressions[i].FRecognizedNumbers = Value then
            Result := i;
        end;

        etInteger:
        begin
          if Expressions[i].FRecognizedInteger <> '' then
            if Expressions[i].FRecognizedInteger = Value then
              Result := i;
        end;

        etFloat:
        begin
          if Expressions[i].FRecognizedFloat <> '' then
            if Expressions[i].FRecognizedFloat = Value then
              Result := i;
        end;

        etPercentage:
        begin
          if Expressions[i].FRecognizedFloat <> '' then
            if Expressions[i].FRecognizedFloat = Value then
              Result := i;
        end;

        etwebSite, etWebMail:
        begin
          if Expressions[i].FDERValue = Value then
            Result := i;
        end;

        etMoney:
        begin
          if Expressions[i].FRecognizedMoney <> '' then
            if Expressions[i].FRecognizedMoney = Value then
              Result := i;
        end;

        etDate, etMonthYear:
        begin
          if Expressions[i].FRecognizedDate <> '' then
            if Expressions[i].RecognizedDateValue = Value then
              Result := i;
        end;
      end;

      if Result <> -1 then
        Break;
    end;
  except
  end;
end;

function TcyDocER.LocateExpression(Value: String; const FromIndex, ToIndex, MaxCarErrors: Integer; const Options: TLocateExpressionOptions; var RsltCarPos: Integer): Integer;
var
  i, StartPos, LengthValue, LengthExpressionDERValue: Integer;
  OriginalExpressionValue: String;
  ExpressionDERValue, UnmodifiedCarCaseValue: DERString;

      function GetValuePos(const SearchFromPos, SearchToPos: Integer): Integer;
      var
        CarErrors, c, s: Integer;
      begin
        Result := 0;

        if leSmartKeywordRec in Options then
        begin
          if ExpressionDERValue = '' then
          begin
            if LengthValue <= MaxCarErrors then
            begin
              Result := 1;
              RsltCarPos := Result;
            end;

            Exit;
          end;

          // 2017-02-01 ... for c := SearchFromPos to (LengthExpressionDERValue - LengthValue) + 1 do
          for c := SearchFromPos to SearchToPos do             // Search at possible positions
          begin
            CarErrors := 0;

            for s := 1 to LengthValue do
              if Value[s] <> ExpressionDERValue[c+s-1] then
              begin
                {$IFDEF UNICODE}
                if not CharInSet(UnmodifiedCarCaseValue[s], ['a'..'z']) then  // Uppercase and symbols cars in Value must be the same as in expression
                {$ELSE}
                if not (UnmodifiedCarCaseValue[s] in ['a'..'z']) then  // Uppercase and symbols cars in Value must be the same as in expression
                {$ENDIF}
                  CarErrors := MaxCarErrors; // Let search at next c position ...

                Inc(CarErrors);
              end;

            if CarErrors <= MaxCarErrors then
            begin
              Result := c;
              RsltCarPos := Result;
              Exit;
            end;
          end;
        end
        else begin
          Result := pos(Value, ExpressionDERValue);
          RsltCarPos := Result;
        end;
      end;

begin
  Result := -1;

  if (lePartialKeyWord in Options) or (leRelativePositionWord in Options)
  then Value  := DoStringToDERCharSet(Value, '%')
  else Value  := DoStringToDERCharSet(Value);

  // ! Remove some cars ! //
  FLocateOptions.Sanytize(Value);

  UnmodifiedCarCaseValue := Value;

  // 2017-03-10
  if leSmartNumberRec in Options then
    Value := DERToDERNCharset(Value, true);

  LengthValue := Length(Value);

  if LengthValue = 0 then Exit;

  if leInsensitive in Options then
    Value  := AnsiUpperCase(Value);

  for i := FromIndex to ToIndex do
  begin
    OriginalExpressionValue := Expressions[i].Value;

    if (lePartialKeyWord in Options) or (leRelativePositionWord in Options) then
    begin
      ExpressionDERValue := StringToDERCharSet(OriginalExpressionValue, eoDERValueAsString in FExpressionOptions, '%');

      // 2017-05-31 Replace natural separators (like ':') by '%' because searching 'Phone' on expression value = 'Phone:0398877887' must be converted to 'Phone%0398877887'
      String_SubstFast(':', '%', ExpressionDERValue);
    end
    else
      ExpressionDERValue := Expressions[i].DERValue;

    // Remove chars from TcyLocateOptions.IgnoreCars
    FLocateOptions.Sanytize(ExpressionDERValue);

    // 2017-03-10
    if leSmartNumberRec in Options then
      ExpressionDERValue := DERToDERNCharset(ExpressionDERValue, true);

    LengthExpressionDERValue := Length(ExpressionDERValue);

    if LengthExpressionDERValue = 0 then
      Continue;

    if leInsensitive in Options then
      ExpressionDERValue := AnsiUpperCase(ExpressionDERValue);

    if lePartialKey in Options then
    begin
      // Must contain "value" at position 1 :
      if GetValuePos(1, 1) = 1 then
        Result := i;
    end
    else
      if leRelativePositionKey in Options then
      begin
        // Contain "value" at any position :
        if GetValuePos(1, (LengthExpressionDERValue - LengthValue) + 1) <> 0 then
          Result := i;
      end
      else
        if lePartialKeyWord in Options then      // Partial word but must start at position 1 ...
        begin
          StartPos := GetValuePos(1, 1);

          if StartPos = 1 then
            if (ExpressionDERValue + '%')[StartPos + LengthValue] = '%' then
              Result := i;
        end
        else
          if leRelativePositionWord in Options then      // Partial word starting on any position
          begin
            // Contain "value" at any position :
            StartPos := GetValuePos(1, (LengthExpressionDERValue - LengthValue) + 1);

            if StartPos <> 0 then
              if (ExpressionDERValue + '%')[StartPos + LengthValue] = '%' then
                Result := i;
          end
          else begin
            // Must be equal :
            if LengthValue = LengthExpressionDERValue then
              if GetValuePos(1, 1) = 1 then
                Result := i;
          end;

    if Result <> -1 then
      Break;
  end;
end;

procedure TcyDocER.LocateExpressionAround(const aExpressionIndex, MaxCarErrors, ScopePx: Integer; const SearchValue: String; const SearchOptions: TLocateExpressionOptions; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);

        function SearchValueMatch(const aIndex: Integer): Boolean;
        var RsltCarPos: Integer;
        begin
          Result := true;
          if SearchValue = '' then Exit;

          Result := LocateExpression(SearchValue, aIndex, aIndex, MaxCarErrors, SearchOptions, RsltCarPos) = aIndex;
        end;

var
  i, PosRsltLeft, PosRsltTop, PosRsltRight, PosRsltBottom: Integer;
  // LetterWidth: Integer;
begin
  RsltLeft := -1;    // Must be in same line
  RsltTop := -1;     // Must be in same column
  RsltRight := -1;   // Must be in same line
  RsltBottom := -1;  // Must be in same column

  PosRsltLeft := -1;
  PosRsltTop := -1;
  PosRsltRight := -1;
  PosRsltBottom := -1;

  (* 2016-10-17 ...
  if length(Expressions[aExpressionIndex].FValue) <> 0
  then LetterWidth := (Expressions[aExpressionIndex].FRightPxPos - Expressions[aExpressionIndex].FLeftPxPos) div length(Expressions[aExpressionIndex].FValue)
  else LetterWidth := 0; *)

  for i := 0 to ExpressionCount-1 do
  begin
    if (i = aExpressionIndex) or (Expressions[i].FPageNumber <> Expressions[aExpressionIndex].FPageNumber) then
      Continue;

    // Expression in same line ?
    if ExpressionsInSameLine(i, aExpressionIndex) then
    begin
      // Left :
      if Expressions[i].FLeftPxPos < Expressions[aExpressionIndex].FLeftPxPos then
        if Expressions[i].FRightPxPos + ScopePx >= Expressions[aExpressionIndex].FLeftPxPos then
          if (Expressions[i].FLeftPxPos > PosRsltLeft) or (PosRsltLeft = -1) then    // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltLeft := i
            else RsltLeft := -1;   // 24/04/2012

            PosRsltLeft := Expressions[i].FLeftPxPos;
          end;

      // Right :
      if Expressions[i].FLeftPxPos > Expressions[aExpressionIndex].FLeftPxPos then  // Compare left again
        if Expressions[i].FLeftPxPos - ScopePx <= Expressions[aExpressionIndex].FRightPxPos then
          if (Expressions[i].FLeftPxPos < PosRsltRight) or (PosRsltRight = -1) then
          begin
            if SearchValueMatch(i)
            then RsltRight := i
            else RsltRight := -1;

            PosRsltRight := Expressions[i].FLeftPxPos;
          end;
    end;

    // Expression in same "column" ?
    if ExpressionsInSameColumn(i, aExpressionIndex) then
(* 2016-10-17 ..    x := Expressions[i].FLeftPxPos + (Expressions[i].FRightPxPos - Expressions[i].FLeftPxPos) div 2;
    if (x >= Expressions[aExpressionIndex].FLeftPxPos) and (x <= Expressions[aExpressionIndex].FRightPxPos)
     or (abs(Expressions[i].FLeftPxPos - Expressions[aExpressionIndex].FLeftPxPos) < LetterWidth)
      or (abs(Expressions[i].FRightPxPos - Expressions[aExpressionIndex].FRightPxPos) < LetterWidth) then  // Solve problem if much more letters and text align = left or right  *)
    begin
      // Top :
      if Expressions[i].FTopPxPos < Expressions[aExpressionIndex].FTopPxPos then
        if Expressions[i].FBottomPxPos + ScopePx >= Expressions[aExpressionIndex].FTopPxPos then
          if (Expressions[i].FTopPxPos > PosRsltTop) or (PosRsltTop = -1) then     // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltTop := i
            else RsltTop := -1;  // 24/04/2012

            PosRsltTop := Expressions[i].FTopPxPos;
          end;

      // Bottom :
      if Expressions[i].FTopPxPos > Expressions[aExpressionIndex].FTopPxPos then  // Compare top again
        if Expressions[i].FTopPxPos - ScopePx <=  Expressions[aExpressionIndex].FBottomPxPos then
          if (Expressions[i].FTopPxPos < PosRsltBottom) or (PosRsltBottom = -1) then  // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltBottom := i
            else RsltBottom := -1;  // 24/04/2012

            PosRsltBottom := Expressions[i].FTopPxPos;
          end;
    end;
  end;
end;

function TcyDocER.LocateExpressions(aText: String; FromExpressionIndex, ToExpressionIndex: Integer; const Locate1stWordOptions, LocateNextWordsOptions: TLocateExpressionOptions; RsltExpressionList: TStrings; const InlineKeyword: Boolean): Boolean;
var
  TextLength, MaxCarErrors, LocateCarPos, IncCarErrors, i: Integer;
  CurrentWord: String;
  RemainedDERExpression: DERString;
  DERWordStack: TStrings;
  SearchWords: Boolean;
  SearchWordIndex, AroundExpressionIndex, ScopePx: Integer;
  LeftExpression, TopExpression, RightExpression, BottomExpression: Integer;
begin
  Result := false;
  FLocateOptions.Sanytize(aText);

  if aText = '' then Exit;

  // *** Initialize DERWordStack with words of aText[i] *** //
  TextLength := Length(aText);

  IncCarErrors := 0;
  DERWordStack := TStringlist.Create;
  CurrentWord := '';
  aText := aText + ' ';

  for i := 1 to length(aText) do
    if aText[i] <> ' ' then
    begin
      if aText[i] <> '.' then         // Points are ignored
        CurrentWord := CurrentWord + aText[i];
    end
    else
      if CurrentWord <> '' then
      begin
        CurrentWord := DoStringToDERCharSet(CurrentWord);

        if CurrentWord <> '' then
          DERWordStack.Add(CurrentWord);
        CurrentWord := '';
      end;

  if DERWordStack.Count = 0 then Exit;

  if FromExpressionIndex < 0 then
    FromExpressionIndex := 0;

  if ToExpressionIndex < 0 then
    ToExpressionIndex := ExpressionCount - 1;

  // * Search on interval * //
  while (FromExpressionIndex <> -1) and (FromExpressionIndex < ToExpressionIndex) and (not Result) do
  begin
    // * Locate first aText's word from FromExpressionIndex * //
    if roSmartKeywordRec in FRecognitionOptions
    then MaxCarErrors := Length(DERWordStack[0]) div 6 + IncCarErrors
    else MaxCarErrors := 0;

    FromExpressionIndex := LocateExpression(DERWordStack[0], FromExpressionIndex, ExpressionCount-1, MaxCarErrors, Locate1stWordOptions, LocateCarPos);


    if FromExpressionIndex <> -1 then
    begin
      // *** Search for the others keyword words *** //

      RsltExpressionList.Clear;
      RsltExpressionList.Add(intToStr(FromExpressionIndex));
      RemainedDERExpression := AnsiUppercase(Expressions[FromExpressionIndex].DERValue);
      FLocateOptions.Sanytize(RemainedDERExpression);

      // 2017-02-02 Done on DERWordStack list !!! String_SubstFast('.', '', RemainedDERExpression);

      // Remained Expression DERString :
      RemainedDERExpression := Copy(RemainedDERExpression, LocateCarPos + length(DERWordStack[0]), length(RemainedDERExpression));

      SearchWords := True;
      SearchWordIndex := 1;    // Search for 2nd keyword's word
      AroundExpressionIndex := FromExpressionIndex;

      while (SearchWords) and (SearchWordIndex <= DERWordStack.Count-1) do
      begin
        if RemainedDERExpression = '' then
        begin
          // Locate Expression Index with remained searched text:
          if InlineKeyword and (DERWordStack[SearchWordIndex] = ':')   // 21/04/2012 - Natural separator, can be far away from prior words ...
          then ScopePx := GetPagePixelsWidth(Expressions[AroundExpressionIndex].PageNumber)
          else ScopePx := (Expressions[AroundExpressionIndex].FBottomPxPos - Expressions[AroundExpressionIndex].FTopPxPos)* 2; // Twice of expression's height

          MaxCarErrors := Length(DERWordStack[SearchWordIndex]) div 6 + IncCarErrors;
          LocateExpressionAround(AroundExpressionIndex, MaxCarErrors, ScopePx, DERWordStack[SearchWordIndex], LocateNextWordsOptions, LeftExpression, TopExpression, RightExpression, BottomExpression);
          AroundExpressionIndex := RightExpression;

          if not InlineKeyword then
            if (BottomExpression <> -1) and (AroundExpressionIndex = -1) then
              AroundExpressionIndex := BottomExpression;

          if AroundExpressionIndex <> -1 then
          begin
            RsltExpressionList.Add(intToStr(AroundExpressionIndex));
            RemainedDERExpression := AnsiUppercase(Expressions[AroundExpressionIndex].DERValue);
            FLocateOptions.Sanytize(RemainedDERExpression);
            // 2017-02-02  String_SubstFast('.', '', RemainedDERExpression);
          end
          else begin
            SearchWords := false;
            Break;
          end;
        end
        else
          LocateCarPos := pos(AnsiUppercase(DERWordStack[SearchWordIndex]), RemainedDERExpression);      // RHR - roSmartKeywordRec not handled here !

        if LocateCarPos = 1 then
        begin
          RemainedDERExpression := Copy(RemainedDERExpression, 1 + length(DERWordStack[SearchWordIndex]), length(RemainedDERExpression));
          Inc(SearchWordIndex);
        end
        else
          SearchWords := false;
      end;

      if SearchWords then  // All keyword's word retrieved:
        Result := True;


      Inc(FromExpressionIndex);
    end;
  end;

  // * Finalization * //
  DERWordStack.Free;
end;

function TcyDocER.ExpressionsInSameLine(const aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
var
  y1: Integer;
begin
  Result := false;

  if Expressions[aExpressionIndex1].FPageNumber <> Expressions[aExpressionIndex2].FPageNumber then Exit;

  // Calculate horizontal position :
  y1 := Expressions[aExpressionIndex1].FTopPxPos + (Expressions[aExpressionIndex1].FBottomPxPos - Expressions[aExpressionIndex1].FTopPxPos) div 2;
  Result := (y1 >= Expressions[aExpressionIndex2].FTopPxPos) and (y1 <= Expressions[aExpressionIndex2].FBottomPxPos);
end;

function TcyDocER.ExpressionsInSameColumn(const aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
begin
  Result := false;

  if Expressions[aExpressionIndex1].FPageNumber <> Expressions[aExpressionIndex2].FPageNumber then Exit;

  if Expressions[aExpressionIndex1].FLeftPxPos = Expressions[aExpressionIndex2].FLeftPxPos then
    Result := true
  else
    if Expressions[aExpressionIndex1].FLeftPxPos < Expressions[aExpressionIndex2].FLeftPxPos then
      Result :=  Expressions[aExpressionIndex1].FRightPxPos >= Expressions[aExpressionIndex2].FLeftPxPos
    else
      Result :=  Expressions[aExpressionIndex1].FLeftPxPos <= Expressions[aExpressionIndex2].FRightPxPos;
end;

procedure TcyDocER.GetAroundExpressions(const aExpressionIndex, ScopePx: Integer; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
var
  i, PosRsltLeft, PosRsltTop, PosRsltRight, PosRsltBottom: Integer;
begin
  RsltLeft := -1;    // Must be in same line
  RsltTop := -1;     // Must be in same column
  RsltRight := -1;   // Must be in same line
  RsltBottom := -1;  // Must be in same column

  PosRsltLeft := -1;
  PosRsltTop := -1;
  PosRsltRight := -1;
  PosRsltBottom := -1;

  for i := 0 to ExpressionCount-1 do
  begin
    if (i = aExpressionIndex) or (Expressions[i].FPageNumber <> Expressions[aExpressionIndex].FPageNumber) then
      Continue;

    // Expression in same line ?
    if ExpressionsInSameLine(i, aExpressionIndex) then
    begin
      // Left :
      if Expressions[i].FLeftPxPos < Expressions[aExpressionIndex].FLeftPxPos then
        if (ScopePx = 0) or (Expressions[i].FRightPxPos + ScopePx >= Expressions[aExpressionIndex].FLeftPxPos) then
          if (Expressions[i].FLeftPxPos > PosRsltLeft) or (PosRsltLeft = -1) then
          begin
            RsltLeft := i;
            PosRsltLeft := Expressions[i].FLeftPxPos;
          end;

      // Right :
      if Expressions[i].FLeftPxPos > Expressions[aExpressionIndex].FLeftPxPos then
        if (ScopePx = 0) or (Expressions[i].FLeftPxPos - ScopePx <=  Expressions[aExpressionIndex].FRightPxPos) then
          if (Expressions[i].FLeftPxPos < PosRsltRight) or (PosRsltRight = -1) then
          begin
            RsltRight := i;
            PosRsltRight := Expressions[i].FLeftPxPos;
          end;
    end;

    // Expression in same "column" ?
    if ExpressionsInSameColumn(i, aExpressionIndex) then
    begin
      // Top :
      if Expressions[i].FTopPxPos < Expressions[aExpressionIndex].FTopPxPos then
        if (ScopePx = 0) or (Expressions[i].FBottomPxPos + ScopePx >= Expressions[aExpressionIndex].FTopPxPos) then
          if (Expressions[i].FTopPxPos > PosRsltTop) or (PosRsltTop = -1) then
          begin
            RsltTop := i;
            PosRsltTop := Expressions[i].FTopPxPos;
          end;

      // Bottom :
      if Expressions[i].FTopPxPos > Expressions[aExpressionIndex].FTopPxPos then  // Compare top again
        if (ScopePx = 0) or (Expressions[i].FTopPxPos - ScopePx <= Expressions[aExpressionIndex].FBottomPxPos) then
          if (Expressions[i].FTopPxPos < PosRsltBottom) or (PosRsltBottom = -1) then
          begin
            RsltBottom := i;
            PosRsltBottom := Expressions[i].FTopPxPos;
          end;
    end;
  end;
end;

function TcyDocER.GetNearestExpression(const FromExpressionIndex, ScopePx: Integer; const Location: TSearchLocation): Integer;
var
  LeftExpression, TopExpression, RightExpression, BottomExpression: Integer;
begin
  Result := -1;

  GetAroundExpressions(FromExpressionIndex, ScopePx, LeftExpression, TopExpression, RightExpression, BottomExpression);

  case Location of
    slFromLeft:   Result := LeftExpression;
    slFromTop:    Result := TopExpression;
    slFromRight:  Result := RightExpression;
    slFromBottom: Result := BottomExpression;
  end;
end;


// Insert expressions in order !
function TcyDocER.GetExpressionListInsertExpressionIndex(ExpressionList: TStrings; const aExpressionIndex: Integer; const ListOrder: TSearchLocation): Integer;
var
  i, ListExpressionIndex: Integer;
begin
  Result := -1;

  for i := 0 to ExpressionList.Count -1 do
  begin
    ListExpressionIndex := StrToInt(ExpressionList[i]);

    case ListOrder of
      slFromLeft:
      begin
        (* 2017-02-13  Searched value can be below because of merged keyword in 2 lines like :

        Invoice           ORIGINAL
        Nº 000258

        So, in order to not retrieve "ORIGINAL", we can' t use ExpressionsInSameLine !
        if ExpressionsInSameLine(aExpressionIndex, ListExpressionIndex) then
        begin
          if Expressions[aExpressionIndex].LeftPxPos < Expressions[ListExpressionIndex].LeftPxPos then
            Result := i;
        end
        else
          if Expressions[aExpressionIndex].TopPxPos < Expressions[ListExpressionIndex].TopPxPos then
            Result := i;            *)

        if Expressions[aExpressionIndex].LeftPxPos < Expressions[ListExpressionIndex].LeftPxPos then
          Result := i;
      end;

      slFromTop:
      begin
        if ExpressionsInSameLine(aExpressionIndex, ListExpressionIndex) then
        begin
          if Expressions[aExpressionIndex].LeftPxPos < Expressions[ListExpressionIndex].LeftPxPos then
            Result := i;
        end
        else
          if Expressions[aExpressionIndex].TopPxPos < Expressions[ListExpressionIndex].TopPxPos then
            Result := i;
      end;

      slFromRight:
      begin
        if Expressions[aExpressionIndex].RightPxPos > Expressions[ListExpressionIndex].RightPxPos then
          Result := i;

        (* 2017-02-13  Same reason as upper
        if ExpressionsInSameLine(aExpressionIndex, ListExpressionIndex) then
        begin
          if Expressions[aExpressionIndex].RightPxPos > Expressions[ListExpressionIndex].RightPxPos then
            Result := i;
        end
        else
          if Expressions[aExpressionIndex].TopPxPos < Expressions[ListExpressionIndex].TopPxPos then
             Result := i;       *)
      end;

      slFromBottom:
      begin
        if ExpressionsInSameLine(aExpressionIndex, ListExpressionIndex) then
        begin
          if Expressions[aExpressionIndex].LeftPxPos < Expressions[ListExpressionIndex].LeftPxPos then
            Result := i;
        end
        else
          if Expressions[aExpressionIndex].TopPxPos > Expressions[ListExpressionIndex].TopPxPos then
            Result := i;
      end;
    end;

    if Result <> -1 then // 2017-02-02 ... if i <> -1 then
      Break;
  end;
end;

function TcyDocER.MergeExpressions(aExpressionList: TStrings; const SeparWithSpace: boolean): Integer;
var
  i, m, MergingExpressionIndex: Integer;
begin
  Result := -1;

  for i := 1 to aExpressionList.Count-1 do
  begin
    MergingExpressionIndex := StrToInt(aExpressionList[i]);
    Result := StrToInt(aExpressionList[0]);
    MergeExpressions(StrToInt(aExpressionList[i]), Result, SeparWithSpace);

    // All expressions with index greater that MergingExpressionIndex moved up on Expression list !
    for m := 0 to aExpressionList.Count-1 do
      if StrToInt(aExpressionList[m]) > MergingExpressionIndex then
        aExpressionList[m] := intToStr( StrToInt(aExpressionList[m]) - 1 );
  end;
end;

procedure TcyDocER.MergeExpressions(aExpressionIndex, ToExpressionIndex: Integer; const SeparWithSpace: boolean);
var
  i: Integer;
  Separator: String;
begin
  if aExpressionIndex = ToExpressionIndex then Exit;

  if SeparWithSpace
  then Separator := ' '
  else Separator := '';

  Expressions[ToExpressionIndex].Value := Expressions[ToExpressionIndex].Value + Separator + Expressions[aExpressionIndex].Value;

  if Expressions[aExpressionIndex].FLeftPxPos < Expressions[ToExpressionIndex].FLeftPxPos then
    Expressions[ToExpressionIndex].FLeftPxPos := Expressions[aExpressionIndex].FLeftPxPos;

  if Expressions[aExpressionIndex].FTopPxPos < Expressions[ToExpressionIndex].FTopPxPos then
    Expressions[ToExpressionIndex].FTopPxPos := Expressions[aExpressionIndex].FTopPxPos;

  if Expressions[aExpressionIndex].FRightPxPos > Expressions[ToExpressionIndex].FRightPxPos then
    Expressions[ToExpressionIndex].FRightPxPos := Expressions[aExpressionIndex].FRightPxPos;

  if Expressions[aExpressionIndex].FBottomPxPos > Expressions[ToExpressionIndex].FBottomPxPos then
    Expressions[ToExpressionIndex].FBottomPxPos := Expressions[aExpressionIndex].FBottomPxPos;

  RecognizeExpressionType(ToExpressionIndex);

  // Delete aExpressionIndex from list :
  DeleteExpression(aExpressionIndex);

  // Resynch :
  if ToExpressionIndex > aExpressionIndex then
    Dec(ToExpressionIndex);

  for i := 0 to ExpressionCount-1 do
  begin
    if Expressions[i].FAssociatedExpressionKeywordIndex = aExpressionIndex then
      Expressions[i].FAssociatedExpressionKeywordIndex := ToExpressionIndex
    else
      if Expressions[i].FAssociatedExpressionKeywordIndex > aExpressionIndex then
        Expressions[i].FAssociatedExpressionKeywordIndex := Expressions[i].FAssociatedExpressionKeywordIndex - 1;
  end;

  // User Resynch :
  if Assigned(FOnExpressionMerged) then
    FOnExpressionMerged(Self, aExpressionIndex, toExpressionIndex);
end;

function TcyDocER.ExpressionsSideBySide(const ExpressionIndexAtLeft, ExpressionIndexAtRight, MaxPxSpacing: Integer): Boolean;
begin
  Result := false;
  if Expressions[ExpressionIndexAtLeft].FPageNumber <> Expressions[ExpressionIndexAtRight].FPageNumber then Exit;
  if Expressions[ExpressionIndexAtLeft].FLeftPxPos >= Expressions[ExpressionIndexAtRight].FLeftPxPos then Exit;

  // Check horizontal spacing between expressions:
  if Expressions[ExpressionIndexAtRight].FLeftPxPos - Expressions[ExpressionIndexAtLeft].FRightPxPos > MaxPxSpacing then Exit;

  // Check horizontal position (must intersect):
  if Expressions[ExpressionIndexAtLeft].FTopPxPos < Expressions[ExpressionIndexAtRight].FTopPxPos then
    Result := Expressions[ExpressionIndexAtLeft].FBottomPxPos >= Expressions[ExpressionIndexAtRight].FTopPxPos
  else
    Result := Expressions[ExpressionIndexAtLeft].FTopPxPos <= Expressions[ExpressionIndexAtRight].FBottomPxPos;
end;

// Returns located expression Index of the specicied keyword
// !!! Function called several times until keyword not found !!!
function TcyDocER.RecognizeNextExpressionElementKeyword(const ElementIndex, KeywordIndex: Integer; FromExpressionIndex: Integer; const InlineKeyword: Boolean; var Accepted: Boolean): Integer;
var
  ExpressionList: TStrings;
  DERWordStack: TStrings;
  RemainedDERExpression: DERString;
  aKeyword, CurrentWord: String;
  PatternPageNumber, PatternPageResolution: Integer;
  PatternLeftPx, PatternTopPx, PatternRightPx, PatternBottomPx: Integer;
  i, m, LocateCarPos, KeywordLength, SearchWordIndex, AroundExpressionIndex, ScopePx, MergingExpressionIndex: Integer;
  LeftExpression, TopExpression, RightExpression, BottomExpression: Integer;
  SearchWords: Boolean;
  isExpressionAssigned, isExpressionOutOfRange: Boolean;
  MaxCarErrors, IncCarErrors: Integer;
  _AssociatedElementIndex, _AssociatedElementKeywordIndex: Integer;
  Locate1stWordOptions, LocateAroundOptions: TLocateExpressionOptions;

      // Cannot split in the middle of a word (Because the user's keyword could be just some first letters of a word) !
      procedure TrySplitOnRemainedExpression(aExpressionIndex: Integer);
      var
        LengthValue, LengthRemainedDERExpression, k, n: Integer;
        RemainedStr, NewValue: String;
      begin
        LengthValue := length(Expressions[aExpressionIndex].FValue);
        LengthRemainedDERExpression := Length(RemainedDERExpression);

        for k := LengthValue downto 2 do
        begin
          RemainedStr := Copy(Expressions[aExpressionIndex].FValue, k, LengthValue); // Copy from k
          // 2017-02-02           String_SubstFast('.', '', RemainedStr);
          FLocateOptions.Sanytize(RemainedStr);

          if length(DoStringToDERCharSet(RemainedStr)) = LengthRemainedDERExpression then
          begin
            // 2017-02-14 Added Space to in clause !
            {$IFDEF UNICODE}
            if not CharInSet(Expressions[aExpressionIndex].FValue[k-1], ['.', ' ', 'a'..'z', 'A'..'Z', '0'..'9']) then  // Don' t split last word ("tot", "tel.", 'H.T.')
            {$ELSE}
            if not (Expressions[aExpressionIndex].FValue[k-1] in ['.', ' ', 'a'..'z', 'A'..'Z', '0'..'9']) then  // Don' t split last word ("tot", "tel.", 'H.T.')
            {$ENDIF}
            begin
              NewValue := RemainedStr;

              // Modify actual expression:
              Expressions[aExpressionIndex].FValue := Copy(Expressions[aExpressionIndex].FValue, 1, k - 1);
              RecognizeExpressionType(aExpressionIndex);

              n := AddExpression(NewValue, Expressions[aExpressionIndex].FPageNumber, Expressions[aExpressionIndex].FOCRConfidence);

              if n <> -1 then
                with Expressions[n] do
                begin
                  FLeftPxPos := Expressions[aExpressionIndex].FLeftPxPos;
                  FTopPxPos := Expressions[aExpressionIndex].FTopPxPos;
                  FRightPxPos := Expressions[aExpressionIndex].FRightPxPos;
                  FBottomPxPos := Expressions[aExpressionIndex].FBottomPxPos;
                  // ExpressionAdded; Can' t call here because it is not inserted in reading words order ...
                end;
            end;

            Break;
          end;
        end;
      end;

begin
  Result := -1;     // !!! RecognizeNextExpressionElementKeyword() called several times until Result = -1 !!!
  Accepted := false;

  if FromExpressionIndex < 0 then
    FromExpressionIndex := 0;

  aKeyword := Elements[ElementIndex].KeyWords[KeywordIndex];
  if aKeyword = '' then Exit;

  ExpressionList := TStringList.Create;

  KeywordLength := Length(aKeyword);
  aKeyword := aKeyword + ' ';

  // *** Initialize DERWordStack with words of aKeyWord[i] *** //
  IncCarErrors := 0;
  DERWordStack := TStringlist.Create;
  CurrentWord := '';
  for i := 1 to length(aKeyword) do
    if aKeyWord[i] <> ' ' then
    begin
      // 2017-05-12 ... Handled by LocateOptions.IgnoreChars ...
      // if aKeyWord[i] <> '.' then         // 05/05/2012  - points are ignored

      CurrentWord := CurrentWord + aKeyWord[i];
    end
    else
      if CurrentWord <> '' then
      begin
        CurrentWord := DoStringToDERCharSet(CurrentWord);
        FLocateOptions.Sanytize(CurrentWord);

        if CurrentWord <> '' then
          DERWordStack.Add(CurrentWord);
        CurrentWord := '';
      end;

  if DERWordStack.Count > 0 then    // At least one word on keyword ...
  begin
    if DERWordStack.Count = 1 then
    begin
      // * Single word to find * //

      // 1st Word detection :
      if aKeyword[1] = ' '                   // RHR
      then Locate1stWordOptions := [lePartialKeyWord, leInsensitive]          // First word must be found at position 1
      else Locate1stWordOptions := [leRelativePositionWord, leInsensitive];   // word in any position ...

      // Next word detection :
      LocateAroundOptions := [lePartialKeyWord, leInsensitive];     // First word must be found at position 1
    end
    else begin
      // * Several words to find * //
      // * In order to find 'facture nº' if ExpressionValue is 'facturenº' because of bad OCR result * //

      // 1st Word detection :
      if aKeyword[1] = ' '
      then Locate1stWordOptions := [lePartialKey, leInsensitive]
      else Locate1stWordOptions := [leRelativePositionKey, leInsensitive];

      // Next word detection :
      LocateAroundOptions := [lePartialKeyWord, leInsensitive];     // First word must be found at position 1
    end;

    // Advanced locate options:
    if roSmartKeywordRec in FRecognitionOptions then
    begin
      Locate1stWordOptions := Locate1stWordOptions + [leSmartKeywordRec];
      LocateAroundOptions := LocateAroundOptions + [leSmartKeywordRec];
    end;


    if DERWordStack.Count > 1 then
      IncCarErrors := 1;

    // 2017-01-13 :
    if Elements[ElementIndex].FPatternPageNumber = 0 then
      PatternPageNumber := 0
    else
      if Elements[ElementIndex].FPatternPageNumberMode = ppFromBeginning
      then PatternPageNumber := Elements[ElementIndex].FPatternPageNumber
      else PatternPageNumber := PageCount - Elements[ElementIndex].FPatternPageNumber + 1;

    // * Search since FromExpressionIndex * //
    while (FromExpressionIndex <> -1) and (Result = -1) do
    begin
      // * Locate first aKeyword's word from FromExpressionIndex * //
      if roSmartKeywordRec in FRecognitionOptions
      then MaxCarErrors := Length(DERWordStack[0]) div 6 + IncCarErrors
      else MaxCarErrors := 0;

      FromExpressionIndex := LocateExpression(DERWordStack[0], FromExpressionIndex, ExpressionCount-1, MaxCarErrors, Locate1stWordOptions, LocateCarPos);


      if FromExpressionIndex <> -1 then
      begin
        // Handle when any element's Keyword text is part of another element's keyword :
        isExpressionAssigned := false;
        isExpressionOutOfRange := (Expressions[FromExpressionIndex].FPageNumber <> PatternPageNumber) and (PatternPageNumber <> 0);
        _AssociatedElementIndex := Expressions[FromExpressionIndex].AssociatedElementIndex;
        _AssociatedElementKeywordIndex := Expressions[FromExpressionIndex].AssociatedElementKeywordIndex;

        if _AssociatedElementIndex <> -1 then
          if (_AssociatedElementIndex = ElementIndex) and (_AssociatedElementKeywordIndex = KeywordIndex) then
            isExpressionAssigned := true   // Already assigned to this element and keyword ...
          else
            if Expressions[FromExpressionIndex].RecognizedType = etExpressionKeyWord then
            begin
              // Check keywords length in order to reassign keyword to this Element :
              if (_AssociatedElementIndex <> -1) and (_AssociatedElementKeywordIndex <> -1) then
                if KeywordLength <= Length( Elements[_AssociatedElementIndex].KeyWords[_AssociatedElementKeywordIndex] ) then
                  isExpressionAssigned := true;
            end
            else
              isExpressionAssigned := true;   // Value associated to _AssociatedElementIndex ...

        // 2017-01-13 Advanced Out Of Range checking :
        if (not isExpressionOutOfRange) and (not isExpressionAssigned) then
          if PatternRectDefined(ElementIndex) then
            if Elements[ElementIndex].FPatternPositionMode = ppFromTopLeftPage then
            begin
              PatternPageResolution := GetPageResolution(Expressions[FromExpressionIndex].FPageNumber);

              PatternLeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm, PatternPageResolution);
              PatternTopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm, PatternPageResolution);
              PatternRightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm, PatternPageResolution);
              PatternBottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm, PatternPageResolution);

              isExpressionOutOfRange := not ExpressionInRect(FromExpressionIndex, PatternLeftPx, PatternTopPx, PatternRightPx, PatternBottomPx);
            end;

        if (not isExpressionAssigned) and (not isExpressionOutOfRange) then
        begin
          // *** Search for the others keyword words *** //

          ExpressionList.Clear;
          ExpressionList.Add(intToStr(FromExpressionIndex));
          RemainedDERExpression := AnsiUppercase(Expressions[FromExpressionIndex].DERValue);
          // 2017-02-02 String_SubstFast('.', '', RemainedDERExpression);
          FLocateOptions.Sanytize(RemainedDERExpression);

          // Remained Expression DERString :
          RemainedDERExpression := Copy(RemainedDERExpression, LocateCarPos + length(DERWordStack[0]), length(RemainedDERExpression));

          SearchWords := True;
          SearchWordIndex := 1;    // Search for 2nd keyword's word
          AroundExpressionIndex := FromExpressionIndex;

          while SearchWords and (SearchWordIndex <= DERWordStack.Count-1) do
          begin
            if RemainedDERExpression = '' then
            begin
              // Locate Expression Index with remained searched text:
              if InlineKeyword and (DERWordStack[SearchWordIndex] = ':')   // Natural separator, can be far away from prior words ...
              then ScopePx := GetPagePixelsWidth(Expressions[AroundExpressionIndex].PageNumber)
              else ScopePx := (Expressions[AroundExpressionIndex].FBottomPxPos - Expressions[AroundExpressionIndex].FTopPxPos) * 2; // Twice of expression's height

              MaxCarErrors := Length(DERWordStack[SearchWordIndex]) div 6 + IncCarErrors;
              LocateExpressionAround(AroundExpressionIndex, MaxCarErrors, ScopePx, DERWordStack[SearchWordIndex], LocateAroundOptions, LeftExpression, TopExpression, RightExpression, BottomExpression);
              AroundExpressionIndex := RightExpression;

              if not InlineKeyword then
                if (BottomExpression <> -1) and (AroundExpressionIndex = -1) then
                  AroundExpressionIndex := BottomExpression;

              if AroundExpressionIndex <> -1 then
              begin
                ExpressionList.Add(intToStr(AroundExpressionIndex));
                RemainedDERExpression := AnsiUppercase(Expressions[AroundExpressionIndex].DERValue);
                // 2017-02-02 String_SubstFast('.', '', RemainedDERExpression);
                FLocateOptions.Sanytize(RemainedDERExpression);
              end
              else begin
                SearchWords := false;
                Break;
              end;
            end
            else
              LocateCarPos := pos(AnsiUppercase(DERWordStack[SearchWordIndex]), RemainedDERExpression);      // RHR - roSmartKeywordRec not handled here !

            if LocateCarPos = 1 then
            begin
              RemainedDERExpression := Copy(RemainedDERExpression, 1 + length(DERWordStack[SearchWordIndex]), length(RemainedDERExpression));
              Inc(SearchWordIndex);
            end
            else
              SearchWords := false;
          end;

          if SearchWords then  // All keyword's word retrieved:
            Result := StrToInt(ExpressionList[0]);
        end;

        Inc(FromExpressionIndex);
      end;
    end;
  end;

  // *** Finalize *** //
  if Result <> -1 then
  begin
    Accepted := true;
    if Assigned(FOnValidateElementKeyword) then
      FOnValidateElementKeyword(Self, ElementIndex, KeywordIndex, ExpressionList, Accepted);
  end;

  if Accepted then
  begin
    // In some cases, Expression value contains element's keyword + element's value (exemple: "fax:06328974")
    if RemainedDERExpression <> '' then
    begin
      // Split expression in order to have the key in actual expression and the RemainedDERExpression in a new expression :
      TrySplitOnRemainedExpression(StrToInt(ExpressionList[ExpressionList.Count-1]));
    end;

    for i := 1 to ExpressionList.Count-1 do
    begin
      MergingExpressionIndex := StrToInt(ExpressionList[i]);
      MergeExpressions(MergingExpressionIndex, StrToInt(ExpressionList[0]), true);

      // All expressions with index greater that MergingExpressionIndex moved up on Expression list !
      for m := 0 to ExpressionList.Count-1 do
        if StrToInt(ExpressionList[m]) > MergingExpressionIndex then
          ExpressionList[m] := intToStr( StrToInt(ExpressionList[m]) - 1 );
    end;

    Expressions[StrToInt(ExpressionList[0])].FAssociatedElementIndex := ElementIndex;
    Expressions[StrToInt(ExpressionList[0])].FAssociatedElementKeywordIndex := KeywordIndex;
    Expressions[StrToInt(ExpressionList[0])].FRecognizedType := etExpressionKeyWord;

    if Assigned(FOnExpressionRecognizedTypeChanged) then
      FOnExpressionRecognizedTypeChanged(Self, StrToInt(ExpressionList[0]));
  end;

  DERWordStack.Free;
  ExpressionList.Free;
end;


// Recognize element's expressions that intersect with Rect :
function TcyDocER.RecognizeElementValuesFromRect(const OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex: Integer; const aElementValueType: TElementsType;
 const aPageNumber, LeftPos, TopPos, RightPos, BottomPos: Integer; const ValueLocation: TSearchLocation;
 ValueNumber, ValueCount: Integer; const PatternMode: Boolean; var UserAbort: Boolean): Integer;

       (* 2017-01-17 ...
       // Replaced by ExpressionInRect()
       function ExpressionInArea(aExpressionIndex: Integer): Boolean;
       begin
         // Check horizontal intersection :
         if Expressions[aExpressionIndex].FLeftPxPos <= LeftPos
         then Result := Expressions[aExpressionIndex].FRightPxPos >= LeftPos
         else Result := Expressions[aExpressionIndex].FLeftPxPos <= RightPos;

         // Check vertical intersection :
         if Result then
           if Expressions[aExpressionIndex].FTopPxPos <= TopPos
           then Result := Expressions[aExpressionIndex].FBottomPxPos >= TopPos
           else Result := Expressions[aExpressionIndex].FTopPxPos <= BottomPos;
       end;   *)

       function GetElementValueFromExpression(const OfElementIndex, FromExpressionIndex: Integer; const aValueType: TElementsType): String;
       var
         AllowEquivalence: Boolean;
       begin
         Result := '';

         if aValueType in [etText, etTextLine, etParagraph, etID] then // Expression can be recognized as other type
         begin
           AllowEquivalence := true;

           // 2016-10-12 :
           if Assigned(FOnAcceptElementTypeEquivalence) then
             FOnAcceptElementTypeEquivalence(Self, OfElementIndex, FromExpressionIndex, aValueType, Expressions[FromExpressionIndex].FRecognizedType, AllowEquivalence);

           if AllowEquivalence then
             Result := Expressions[FromExpressionIndex].FValue;
         end
         else
           if Expressions[FromExpressionIndex].FRecognizedType <> aValueType then
           begin
             case aValueType of
               // etExpressionKeyWord: AllowEquivalence := false;
               etNumbers: AllowEquivalence := Expressions[FromExpressionIndex].FRecognizedType in [etFloat, etInteger];
               // etInteger: AllowEquivalence := false;
               etFloat: AllowEquivalence := Expressions[FromExpressionIndex].FRecognizedType in [etInteger];
               etPercentage: AllowEquivalence := Expressions[FromExpressionIndex].FRecognizedType in [etFloat, etInteger];
               // etwebSite: AllowEquivalence := false;
               // etWebMail: AllowEquivalence := false;
               etMoney: AllowEquivalence := Expressions[FromExpressionIndex].FRecognizedType in [etFloat, etInteger];
               // etDate: AllowEquivalence := false;
               // etMonthYear: AllowEquivalence := false;
               else
                 AllowEquivalence := false;
             end;

             if Assigned(FOnAcceptElementTypeEquivalence) then
               FOnAcceptElementTypeEquivalence(Self, OfElementIndex, FromExpressionIndex, aValueType, Expressions[FromExpressionIndex].FRecognizedType, AllowEquivalence);

             if AllowEquivalence then
               Result := Expressions[FromExpressionIndex].RecognizedValue;
           end
           else
             Result := Expressions[FromExpressionIndex].RecognizedValue;
       end;

var
  PatternValueSizeMm: Extended;
  ValueMinCars, ValueMaxCars: Integer;

  i, j, r, InsertPos, LastAddedExpressionIndex: Integer;
  ExpressionIndex0, ExpressionIndex1: Integer;
  ExpressionList: TStringList;
  Str, ExpressionValue: String;
  ValidationResult: TValidateElementValueResult;
  Accept, IncludeExpression, StopSearching, RemoveAll, MaskOk: Boolean;

         // 2017-01-16
        function MergeAccepted(const ActualValue_ExpressionIndex, WithExpressionIndex: Integer): Boolean;
        begin
          Result := true;

          try
            if Result and (PatternValueSizeMm > 1) then
              if PxToMmFromPage(Expressions[WithExpressionIndex].RightPxPos - Expressions[ActualValue_ExpressionIndex].LeftPxPos, Expressions[ActualValue_ExpressionIndex].FPageNumber) > PatternValueSizeMm then
                Result := false;

            (* Already true !!!
            if Result and (ValueMinCars > 1) then
              if Length(Expressions[ActualValue_ExpressionIndex].RecognizedValue) + Length(Expressions[WithExpressionIndex].RecognizedValue) > ValueMinCars then
                Result := true; *)

            if Result and (ValueMaxCars > 1) then
              if Length(Expressions[ActualValue_ExpressionIndex].RecognizedValue) + Length(Expressions[WithExpressionIndex].RecognizedValue) > ValueMaxCars then
                Result := false;
          except
          end;
        end;

        // 2017-02-03  Merge expressions if first one terminates by '-' or '/' :
        // No Rect zone defined, no size limitations ...
        procedure RecognizeSingleIDValueFromList(aExpressionList: TStrings);
        var
          ScopePx, r: Integer;
          ExpressionIndex0, ExpressionIndex1: Integer;
        begin
          while aExpressionList.Count > 1 do
          begin
            ExpressionIndex0 := StrToInt(aExpressionList[0]);
            ExpressionIndex1 := StrToInt(aExpressionList[1]);

            if Expressions[ExpressionIndex0].Value = '' then
              Break;

            if not ExpressionsInSameLine(ExpressionIndex0, ExpressionIndex1) then
              Break;

            // Valid expression's type :
            if not (Expressions[ExpressionIndex0].RecognizedType in [etText, etID, etInteger, etNumbers]) then
              Break;

            if not (Expressions[ExpressionIndex1].RecognizedType in [etText, etID, etInteger, etNumbers]) then
              Break;


            ScopePx := (Expressions[ExpressionIndex0].FBottomPxPos - Expressions[ExpressionIndex0].FTopPxPos); // Expression's height

            // Expressions near ?
            if Expressions[ExpressionIndex0].RightPxPos + ScopePx <= Expressions[ExpressionIndex1].LeftPxPos then
              Break;

            // ID separator that makes the OCR brakes into 2 words ?
            {$IFDEF UNICODE}
            if not CharInSet(Expressions[ExpressionIndex0].Value[Length(Expressions[ExpressionIndex0].Value)], ['-', '/', '\']) then
            {$ELSE}
            if not (Expressions[ExpressionIndex0].Value[Length(Expressions[ExpressionIndex0].Value)] in ['-', '/', '\']) then
            {$ENDIF}
            begin
              if DERExtractID(Expressions[ExpressionIndex0].DERValue, roSmartNumbersRec in FRecognitionOptions) = '' then
              Break;

              if DERExtractID(Expressions[ExpressionIndex1].DERValue, roSmartNumbersRec in FRecognitionOptions) = '' then
              Break;
            end;

            if not MergeAccepted(ExpressionIndex0, ExpressionIndex1) then
              Break;

            MergeExpressions(ExpressionIndex1, ExpressionIndex0, false);
            aExpressionList.Delete(1);

              // Resynch:
              for r := 0 to aExpressionList.Count - 1 do
               if StrToInt(aExpressionList[r]) > ExpressionIndex1 then
                 aExpressionList[r] := intToStr( StrToInt(aExpressionList[r]) - 1 );
          end;
        end;

        procedure Pattern_RecognizeSingleValueFromList(aExpressionList: TStrings; const ResultType: TElementsType);
        var
          i, r: Integer;
          CanMerge, Merged, SeparWithSpace: Boolean;
          ExpressionIndex0, ExpressionIndex1: Integer;
        begin
          i := 0;
          while i <= aExpressionList.Count-2 do
          begin
            Merged := false;
            ExpressionIndex0 := StrToInt(aExpressionList[i]);
            ExpressionIndex1 := StrToInt(aExpressionList[i+1]);

            if ResultType = etParagraph then
            begin
              if MergeAccepted(ExpressionIndex0, ExpressionIndex1) then    // 2017-01-16
              begin
                if Expressions[ExpressionIndex0].BottomPxPos <= Expressions[ExpressionIndex1].TopPxPos then
                begin
                  Expressions[ExpressionIndex0].Value := Expressions[ExpressionIndex0].Value + #13#10;
                  RecognizeExpressionType(ExpressionIndex0);
                end;

                MergeExpressions(ExpressionIndex1, ExpressionIndex0, true);
                Merged := true;
                aExpressionList.Delete(i+1);

                // Resynch:
                for r := 0 to aExpressionList.Count - 1 do
                 if StrToInt(aExpressionList[r]) > ExpressionIndex1 then
                   aExpressionList[r] := intToStr( StrToInt(aExpressionList[r]) - 1 );
              end
              else
                Break;
            end
            else
              if ExpressionsInSameLine(ExpressionIndex0, ExpressionIndex1) then
                if MergeAccepted(ExpressionIndex0, ExpressionIndex1) then    // 2017-01-16
                begin
                  CanMerge := false;
                  SeparWithSpace := false;

                  case ResultType of
                    etText, etTextLine, etID:
                    begin
                      CanMerge := true;
                      SeparWithSpace := true;
                    end;

                    etNumbers: CanMerge := (Expressions[ExpressionIndex0].RecognizedType in [etInteger, etNumbers]) and (Expressions[ExpressionIndex1].RecognizedType in [etInteger, etNumbers]);
                    etInteger: CanMerge := (Expressions[ExpressionIndex0].RecognizedType in [etInteger]) and (Expressions[ExpressionIndex1].RecognizedType in [etInteger]);
                  end;

                  if CanMerge then
                  begin
                    MergeExpressions(ExpressionIndex1, ExpressionIndex0, SeparWithSpace);
                    Merged := true;
                    aExpressionList.Delete(i+1);

                    // Resynch:
                    for r := 0 to aExpressionList.Count - 1 do
                     if StrToInt(aExpressionList[r]) > ExpressionIndex1 then
                       aExpressionList[r] := intToStr( StrToInt(aExpressionList[r]) - 1 );
                  end;
                end;

            if not Merged then
              Inc(i);       // 2016-11-14 BUG !!! Added  ...
          end;
        end;

begin
  Result := 0;
  UserAbort := false;

  // 2017-01-16 :
  PatternValueSizeMm := FElements[OfElementIndex].FPatternValueSizeMm;
  ValueMinCars := FElements[OfElementIndex].FValueMinCars;
  ValueMaxCars := FElements[OfElementIndex].FValueMaxCars;

  ExpressionList := TStringList.Create;

  // * Retrieve all expressions in defined area sorted as expected //
  for i := 0 to ExpressionCount-1 do
    if (Expressions[i].FPageNumber = aPageNumber) or (aPageNumber = 0) then
      if (not IsDERSymbols(Expressions[i].FDERValue)) or (ExpressionList.Count <> 0) then   // Remove symbol ...
        if ExpressionInRect(i, LeftPos, TopPos, RightPos, BottomPos, true) then  // 2017-01-16 ... if ExpressionInArea(i) then
        begin
          IncludeExpression := true;

          if PatternMode then
          begin
            // Ignore expression from any already associated to another element :
            if Expressions[i].AssociatedElementIndex <> -1 then
              if Expressions[i].AssociatedElementIndex <> OfElementIndex then
                IncludeExpression := false;
          end
          else begin
            // Don' t include own keywords:
            if Expressions[i].AssociatedElementIndex = OfElementIndex then
              if Expressions[i].AssociatedElementKeywordIndex <> -1 then
                IncludeExpression := false;

            (* 2016-10-13 Create problems recognitions but don' t know why ...
            if IncludeExpression then
                if aElementValueType in [etInteger, etFloat, etMoney] then
                  if Length(Expressions[i].RecognizedFloat) > 1 then
                  begin
                    if Expressions[i].RecognizedFloat[1] = '0' then
                      if Expressions[i].RecognizedFloat[2] in ['0'..'9'] then
                        IncludeExpression := false;
                  end;  *)
          end;

          if IncludeExpression then
          begin
            // Insert expressions candidates in order !
            InsertPos := GetExpressionListInsertExpressionIndex(ExpressionList, i, ValueLocation);

            if InsertPos = -1
            then ExpressionList.Add(intToStr(i))
            else ExpressionList.Insert(InsertPos, intToStr(i));
          end;
        end;


(*  if DebugHook <> 0 then
    if (Elements[OfElementIndex].Tag = 11) and (ExpressionList.Text <> '') then
      Beep;      *)


  if not PatternMode then
  begin
    // Remove all expressions from any already associated to an element until the end of the sorted list
    // Except if associated element type is date and searched value is Money/Float/Integer:
    for i := 0 to ExpressionList.Count-1 do
      with Expressions[ StrToInt(ExpressionList[i]) ] do
        if FAssociatedElementIndex <> -1 then
          if FAssociatedElementIndex <> OfElementIndex then                  // Found another Keyword or associated expression to another element ...
          begin
            RemoveAll := true;
            if FElements[OfElementIndex].ValueType in [etFloat, etMoney] then
              if FElements[FAssociatedElementIndex].ValueType = etDate then
                RemoveAll := false;

            if RemoveAll then
            begin
              for j := ExpressionList.Count-1 downto i do
                ExpressionList.Delete(j);

              Break;
            end;
          end;
  end;


  // * Remove ValueNumber-1 or invalid expressions from list * //
  while ExpressionList.Count <> 0 do
  begin
    if ValueNumber > 1     // Value number for pattern recognition ...
    then Accept := false
    else Accept := GetElementValueFromExpression(OfElementIndex, StrToInt(ExpressionList[0]), aElementValueType) <> '';

    if not Accept then
    begin
      ExpressionList.Delete(0);
      Dec(ValueNumber);
    end
    else
      Break;
  end;


  // * Validate candidates list * //
  if Assigned(FOnRetrieveElementValuesFromRect) then
    FOnRetrieveElementValuesFromRect(Self, OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex, ExpressionList);

  // * Merge expressions as needed if mask defined * //
  if FElements[OfElementIndex].Mask <> '' then
  begin
    i := 0;
    Str := '';
    MaskOk := false;
    while i <= ExpressionList.Count-1 do
    begin
      Str := Str + Expressions[StrToInt(ExpressionList[i])].Value;

      if IsMatchMask2(FElements[OfElementIndex].Mask, Str) then
      begin
        MaskOk := true;
        Break;
      end
      else
        Inc(i);
    end;

    if MaskOk then
      for j := i downto 1 do
      begin
        ExpressionIndex0 := StrToInt(ExpressionList[j-1]);
        ExpressionIndex1 := StrToInt(ExpressionList[j]);

        MergeExpressions(ExpressionIndex1, ExpressionIndex0, false);
        ExpressionList.Delete(j);

        // Resynch:
        for r := 0 to ExpressionList.Count - 1 do
         if StrToInt(ExpressionList[r]) > ExpressionIndex1 then
           ExpressionList[r] := intToStr( StrToInt(ExpressionList[r]) - 1 );
      end;
  end;


  // * 2016-10-27 Merge expressions if etTextLine or etParagraph only if no mask defined * //
  if FElements[OfElementIndex].Mask = '' then
    if aElementValueType in [etTextLine, etParagraph] then
    begin
      i := 0;
      while i <= ExpressionList.Count-2 do
      begin
        ExpressionIndex0 := StrToInt(ExpressionList[i]);
        ExpressionIndex1 := StrToInt(ExpressionList[i+1]);

        if (aElementValueType = etParagraph) or ExpressionsInSameLine(ExpressionIndex0, ExpressionIndex1) then
        begin
          if MergeAccepted(ExpressionIndex0, ExpressionIndex1) then    // 2017-01-16
          begin
            if aElementValueType = etParagraph then
              if Expressions[ExpressionIndex0].BottomPxPos <= Expressions[ExpressionIndex1].TopPxPos then
              begin
                Expressions[ExpressionIndex0].Value := Expressions[ExpressionIndex0].Value + #13#10;
                RecognizeExpressionType(ExpressionIndex0);
              end;

            MergeExpressions(ExpressionIndex1, ExpressionIndex0, true);
            ExpressionList.Delete(i+1);

            // Resynch:
            for r := 0 to ExpressionList.Count - 1 do
             if StrToInt(ExpressionList[r]) > ExpressionIndex1 then
               ExpressionList[r] := intToStr( StrToInt(ExpressionList[r]) - 1 );
          end
          else
            Break;
        end
        else
          if ValueCount = 0
          then inc(i)
          else Break;
      end;
    end
    else     // 2016-10-28     Merge Invoice number, phone number etc ...
      if ValueCount = 1 then  // We need to retrieve single value ...
        if (PatternMode or SizeRestrictionDefined(OfElementIndex)) then
        begin
          if aElementValueType in [etText, etID, etNumbers] then
            Pattern_RecognizeSingleValueFromList(ExpressionList, aElementValueType);
        end
        else
          if aElementValueType = etID then
            RecognizeSingleIDValueFromList(ExpressionList);

  // * Handle ExpressionList * //
  if ExpressionList.Count <> 0 then
  begin
    // Assign Values to element and Keyword :
    StopSearching := false;
    LastAddedExpressionIndex := -1;
    while (ExpressionList.Count <> 0) and (not StopSearching) do
    begin
      if FElements[OfElementIndex].Mask <> '' then
      begin
        ExpressionValue := Expressions[ StrToInt(ExpressionList[0]) ].Value;
        MaskOk := IsMatchMask2(FElements[OfElementIndex].Mask, ExpressionValue);
      end
      else begin
        ExpressionValue := GetElementValueFromExpression(OfElementIndex, StrToInt(ExpressionList[0]), aElementValueType);
        MaskOk := true;
      end;

      if MaskOk and (ExpressionValue <> '') then   // Mask ok and same type ...
      begin
        ValidationResult := veValueOk;

        // Stop if distance is too big between current expression and LastAddedExpressionIndex :
        if (not PatternMode) and (LastAddedExpressionIndex <> -1) then
          if ValueLocation in [slFromTop, slFromBottom] then
            with Expressions[ StrToInt(ExpressionList[0]) ] do
              if Abs(FTopPxPos - Expressions[LastAddedExpressionIndex].FTopPxPos) > 3 * (FBottomPxPos - FTopPxPos) then
              begin
                ValidationResult := veValueTooFar;
                StopSearching := true;
              end;

        // Validate event :
        if Assigned(FOnValidateElementValue) then
          FOnValidateElementValue(Self, OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex, StrToInt(ExpressionList[0]), ValidationResult);

        if ValidationResult <> veValueOk then
        begin
          ExpressionList.Delete(0);
          if ValidationResult = veInvalidValueStopSearching then
          begin
            UserAbort := true;
            StopSearching := True;
          end;

          Continue;
        end;

        // Assign value to element:
        Inc(Result);
        LastAddedExpressionIndex := StrToInt(ExpressionList[0]);
        with Expressions[LastAddedExpressionIndex] do
        begin
          FAssociatedElementIndex := OfElementIndex;
          FAssociatedElementKeywordIndex := OfElementKeywordIndex;
          FAssociatedExpressionKeywordIndex := OfExpressionKeywordIndex;
        end;

        if aElementValueType in [etNumbers, etText, etID] then
        begin
          if ValueLocation in [slFromTop, slFromBottom] then
          begin
            if ValueCount <> 0 then // Retrieve a specified number of expressions ...
              if ValueCount = Result then
                StopSearching := True;
          end
          else               // Retrieving from Left / right
            if ValueCount = 1 then
              StopSearching := True;
        end
        else
          if ValueCount = 0 then
          begin
            if (not PatternMode) and (ValueLocation = slFromLeft) then   // Retrieve only first value in the horizontal position ...
              StopSearching := True;
          end
          else           // Retrieve a specified number of expressions ...
            if ValueCount = Result then
              StopSearching := True;

      end
      else
        StopSearching := true;

      ExpressionList.Delete(0);
    end;
  end;

  ExpressionList.Free;
end;

procedure TcyDocER.RecognizeElementValues(const ElementIndex: Integer);
var
  PatternExpressionIndex, i: Integer;
  PatternLeftPx, PatternTopPx, PatternRightPx, PatternBottomPx, PatternPageNumber, PatternPageResolution: Integer;
  UserAborted: Boolean;
begin
  if Elements[ElementIndex].FPatternPageNumber = 0 then
    PatternPageNumber := 0
  else
    if Elements[ElementIndex].FPatternPageNumberMode = ppFromBeginning
    then PatternPageNumber := Elements[ElementIndex].FPatternPageNumber
    else PatternPageNumber := PageCount - Elements[ElementIndex].FPatternPageNumber + 1;

  PatternPageResolution := GetPageResolution(PatternPageNumber);

  if Elements[ElementIndex].FPatternPositionMode = ppFromTopLeftPage then
  begin
    PatternLeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm, PatternPageResolution);
    PatternTopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm, PatternPageResolution);
    PatternRightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm, PatternPageResolution);
    PatternBottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm, PatternPageResolution);
  end
  else begin
    // Get 'manual' (not by keywords search engine) associated keyword:
    PatternExpressionIndex := -1;
    for i := 0 to ExpressionCount-1 do
      if Expressions[i].FAssociatedElementIndex = ElementIndex then
      begin
        PatternExpressionIndex := i;
        Break;
      end;

    if PatternExpressionIndex = -1 then Exit;

    PatternLeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm, PatternPageResolution) + Expressions[PatternExpressionIndex].FLeftPxPos;
    PatternTopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm, PatternPageResolution) + Expressions[PatternExpressionIndex].FTopPxPos;
    PatternRightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm, PatternPageResolution) + Expressions[PatternExpressionIndex].FRightPxPos;
    PatternBottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm, PatternPageResolution) + Expressions[PatternExpressionIndex].FBottomPxPos;
  end;

  RecognizeElementValuesFromRect(
                         ElementIndex,
                         -1,
                         -1,
                         Elements[ElementIndex].FValueType,
                         PatternPageNumber,
                         PatternLeftPx,
                         PatternTopPx,
                         PatternRightPx,
                         PatternBottomPx,
                         Elements[ElementIndex].FPatternValueLocation,
                         Elements[ElementIndex].FPatternValueNumber,
                         Elements[ElementIndex].FValueCount,
                         True, UserAborted);
end;

procedure TcyDocER.RecognizeElementKeywordsValues(const ElementIndex: Integer);
var
  PatternLeftPx, PatternTopPx, PatternRightPx, PatternBottomPx, PatternPageNumber, PatternPageResolution: Integer;
  ExpressionValuesCount, k, i: Integer;
  LookingDirectionFromKeyword1: TLookingDirectionFromKeyword;
  LookingDirectionFromKeyword2: TLookingDirectionFromKeyword;
  ValueLocation: TSearchLocation;
  LeftPx, TopPx, RightPx, BottomPx: Integer;
  UserAborted: Boolean;

      procedure InitializeVariablesFromKeyword(LookingDirectionFromKeyword: TLookingDirectionFromKeyword; ExpressionIndex: Integer);
      var
        NearestExpressionIndex, ExpressionHeight: Integer;
      begin
        case LookingDirectionFromKeyword of
          ldOnLeft:     // We will search values at expression left side
          begin
            ValueLocation := slFromRight;

            RightPx   := Expressions[ExpressionIndex].FLeftPxPos;

            if Elements[ElementIndex].ValueType in [etMoney, etPercentage] then   // 2016-10-13 Only money values can be to the right of rect ...
              LeftPx  := 0
            else
              if Elements[ElementIndex].ValueType in [etText, etID, etNumbers]  // 2017-02-03  don' t look too far !
              then LeftPx  := RightPx - GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber) div 4
              else LeftPx  := RightPx - GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber) div 2;

            // 2016-10-17 If no expression exists :
            NearestExpressionIndex := GetNearestExpression(ExpressionIndex, 0, slFromLeft);

            if NearestExpressionIndex <> -1 then
              if Expressions[NearestExpressionIndex].FLeftPxPos < LeftPx then
                LeftPx := Expressions[NearestExpressionIndex].FLeftPxPos;

            ExpressionHeight := Expressions[ExpressionIndex].FBottomPxPos - Expressions[ExpressionIndex].FTopPxPos;

            TopPx    := Expressions[ExpressionIndex].FTopPxPos + ExpressionHeight div 4;
            BottomPx := Expressions[ExpressionIndex].FBottomPxPos - ExpressionHeight div 4;

            if BottomPx <= TopPx then
              BottomPx  := TopPx + 1;
          end;

          ldOnRight:     // We will search values at expression right side
          begin
            ValueLocation := slFromLeft;
            LeftPx   := Expressions[ExpressionIndex].FLeftPxPos;
            // 2016-10-13 ... RightPx  := GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber);

            // 2016-10-17
            // if Elements[ElementIndex].ValueType in [etMoney, etPercentage]   // 2016-10-13 Only money values can be to the right of the page
            //then RightPx  := GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber)
            //else
            if Elements[ElementIndex].ValueType in [etText, etID, etNumbers]  // 2017-02-03  don' t look too far !
            then RightPx := Expressions[ExpressionIndex].FRightPxPos + GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber) div 4
            else RightPx := Expressions[ExpressionIndex].FRightPxPos + GetPagePixelsWidth(Expressions[ExpressionIndex].FPageNumber) div 2;   // 2016-10-13 ...

            // 2016-10-17 If no expression exists :
            NearestExpressionIndex := GetNearestExpression(ExpressionIndex, 0, slFromRight);

            if NearestExpressionIndex <> -1 then
              if String_End(Expressions[NearestExpressionIndex].DERValue, 1) = ':' then    // Natural separator ...
                NearestExpressionIndex := GetNearestExpression(NearestExpressionIndex, 0, slFromRight);

            if NearestExpressionIndex <> -1 then
              if Expressions[NearestExpressionIndex].FRightPxPos > RightPx then
                RightPx := Expressions[NearestExpressionIndex].FRightPxPos;

            ExpressionHeight := Expressions[ExpressionIndex].FBottomPxPos - Expressions[ExpressionIndex].FTopPxPos;

            TopPx    := Expressions[ExpressionIndex].FTopPxPos + ExpressionHeight div 4;
            BottomPx := Expressions[ExpressionIndex].FBottomPxPos - ExpressionHeight div 4;

            // 2016-10-12 :
            if BottomPx <= TopPx then
              BottomPx  := TopPx + 1;
          end;

          ldOnTop:    // We will search values at expression top
          begin
            ValueLocation := slFromBottom;
            LeftPx   := Expressions[ExpressionIndex].FLeftPxPos;   // We can have problems if values not aligned correctly RHR
            BottomPx    := Expressions[ExpressionIndex].FTopPxPos;
            RightPx  := Expressions[ExpressionIndex].FRightPxPos;  // We can have problems if values not aligned correctly RHR

            if Elements[ElementIndex].ValueType = etMoney then  // Only money values can be to the bottom of the page
              TopPx := 0
            else
              if Elements[ElementIndex].ValueType in [etText, etID, etNumbers]  // 2017-02-03  don' t look too far !
              then TopPx := BottomPx - (Expressions[ExpressionIndex].BottomPxPos - Expressions[ExpressionIndex].TopPxPos) * 2  // Expression above !
              else TopPx := BottomPx - GetPagePixelsHeight(Expressions[ExpressionIndex].FPageNumber) div 10;

            // 2017-02-06 If no expression exists :
            NearestExpressionIndex := GetNearestExpression(ExpressionIndex, 0, slFromTop);

            if NearestExpressionIndex <> -1 then
              if Expressions[NearestExpressionIndex].FTopPxPos < TopPx then
                TopPx := Expressions[NearestExpressionIndex].FTopPxPos;
          end;

          ldAtBottom:    // We will search values at expression bottom
          begin
            ValueLocation := slFromTop;
            LeftPx   := Expressions[ExpressionIndex].FLeftPxPos;   // We can have problems if values not aligned correctly RHR
            TopPx    := Expressions[ExpressionIndex].FTopPxPos;
            RightPx  := Expressions[ExpressionIndex].FRightPxPos;  // We can have problems if values not aligned correctly RHR

            if Elements[ElementIndex].ValueType = etMoney then  // 2016-10-13 Only money values can be to the bottom of the page
              BottomPx := GetPagePixelsHeight(Expressions[ExpressionIndex].FPageNumber)
            else
              if Elements[ElementIndex].ValueType in [etText, etID, etNumbers]  // 2017-02-03  don' t look too far !
              then BottomPx := TopPx + (Expressions[ExpressionIndex].BottomPxPos - Expressions[ExpressionIndex].TopPxPos) * 2  // Expression below !
              else BottomPx := TopPx + GetPagePixelsHeight(Expressions[ExpressionIndex].FPageNumber) div 10;  // 2016-10-13 ...

            // 2017-02-06 If no expression exists :
            NearestExpressionIndex := GetNearestExpression(ExpressionIndex, 0, slFromBottom);

            if NearestExpressionIndex <> -1 then
              if String_End(Expressions[NearestExpressionIndex].DERValue, 1) = ':' then    // Natural separator ...
                NearestExpressionIndex := GetNearestExpression(NearestExpressionIndex, 0, slFromBottom);

            if NearestExpressionIndex <> -1 then
              if Expressions[NearestExpressionIndex].FBottomPxPos > BottomPx then
                BottomPx := Expressions[NearestExpressionIndex].FBottomPxPos;
          end;
        end;

        // 2017-01-13 Restrict to pattern Rect :
        if Elements[ElementIndex].FPatternPositionMode = ppFromTopLeftPage then
        begin
          if LeftPx < PatternLeftPx then LeftPx := PatternLeftPx;
          if TopPx < PatternTopPx then TopPx := PatternTopPx;
          if RightPx < PatternRightPx then RightPx := PatternRightPx;
          if BottomPx < PatternBottomPx then BottomPx := PatternBottomPx;
        end
        else begin
          if LeftPx < PatternLeftPx + Expressions[ExpressionIndex].LeftPxPos then LeftPx := PatternLeftPx + Expressions[ExpressionIndex].LeftPxPos;
          if TopPx < PatternTopPx + Expressions[ExpressionIndex].TopPxPos then TopPx := PatternTopPx + Expressions[ExpressionIndex].TopPxPos;
          if RightPx < PatternRightPx + Expressions[ExpressionIndex].RightPxPos then RightPx := PatternRightPx + Expressions[ExpressionIndex].RightPxPos;
          if BottomPx < PatternBottomPx + Expressions[ExpressionIndex].BottomPxPos then BottomPx := PatternBottomPx + Expressions[ExpressionIndex].BottomPxPos;
        end;
      end;


begin
  if Elements[ElementIndex].FPatternPageNumber = 0 then
    PatternPageNumber := 0
  else
    if Elements[ElementIndex].FPatternPageNumberMode = ppFromBeginning
    then PatternPageNumber := Elements[ElementIndex].FPatternPageNumber
    else PatternPageNumber := PageCount - Elements[ElementIndex].FPatternPageNumber + 1;

  PatternPageResolution := GetPageResolution(PatternPageNumber);

  if PatternRectDefined(ElementIndex) then
  begin
    PatternLeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm, PatternPageResolution);
    PatternTopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm, PatternPageResolution);
    PatternRightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm, PatternPageResolution);
    PatternBottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm, PatternPageResolution);
  end
  else begin
    PatternLeftPx   := 0;
    PatternTopPx    := 0;
    PatternRightPx  := 0;
    PatternBottomPx := 0;
  end;

  case Elements[ElementIndex].FKeywordLookingValueDirection of
    ldOnLeft, ldOnTop, ldOnRight, ldAtBottom:
    begin
      LookingDirectionFromKeyword1 := Elements[ElementIndex].FKeywordLookingValueDirection;
      LookingDirectionFromKeyword2 := Elements[ElementIndex].FKeywordLookingValueDirection;
    end;

    ldOnRightIfNotFoundBottom:
    begin
      LookingDirectionFromKeyword1 := ldOnRight;
      LookingDirectionFromKeyword2 := ldAtBottom;
    end;

    ldAtBottomIfNotFoundRight:
    begin
      LookingDirectionFromKeyword1 := ldAtBottom;
      LookingDirectionFromKeyword2 := ldOnRight;
    end;
  end;

(* 2016-10-14     OLD ...
  if Elements[ElementIndex].ValueCount = 1 then
  begin
    LookingDirectionFromKeyword1 := ldFromRight;
    LookingDirectionFromKeyword2 := ldFromBottom;
  end
  else begin
    LookingDirectionFromKeyword1 := ldFromBottom;
    LookingDirectionFromKeyword2 := ldFromRight;
  end;    *)

  // Search values from keywords (listed by roKeywordsByPriority) :
  for k := 0 to Elements[ElementIndex].KeyWords.Count-1 do
  begin
    i := 0;
    while i <= ExpressionCount-1 do
    begin
      if Expressions[i].FRecognizedType = etExpressionKeyWord then
        if (Expressions[i].AssociatedElementIndex = ElementIndex) and (Expressions[i].AssociatedElementKeywordIndex = k) then
        begin
          // Read associated value(s) from ReadingDirection1 :
          InitializeVariablesFromKeyword(LookingDirectionFromKeyword1, i);

          ExpressionValuesCount := RecognizeElementValuesFromRect(
                                     ElementIndex,
                                     k,
                                     i,
                                     Elements[ElementIndex].FValueType,
                                     Expressions[i].FPageNumber,
                                     LeftPx,
                                     TopPx,
                                     RightPx,
                                     BottomPx,
                                     ValueLocation,
                                     1,
                                     Elements[ElementIndex].FValueCount, false, UserAborted);

          // Read associated value(s) from ReadingDirection2 :
          if (LookingDirectionFromKeyword2 <> LookingDirectionFromKeyword1) and (ExpressionValuesCount = 0) and (not UserAborted) then
          begin
            InitializeVariablesFromKeyword(LookingDirectionFromKeyword2, i);

            RecognizeElementValuesFromRect(
                                       ElementIndex,
                                       k,
                                       i,
                                       Elements[ElementIndex].FValueType,
                                       Expressions[i].FPageNumber,
                                       LeftPx,
                                       TopPx,
                                       RightPx,
                                       BottomPx,
                                       ValueLocation,
                                       1,
                                       Elements[ElementIndex].FValueCount, false, UserAborted);
          end;
        end;

      Inc(i);
    end;
  end;
end;

procedure TcyDocER.InitializeRecognition;
var
  i, m: Integer;
  MoneyHandled: Boolean;
begin
  // Initialize :
  for i := 0 to ExpressionCount-1 do
  begin
    Expressions[i].FAssociatedElementIndex := -1;
    Expressions[i].FAssociatedElementKeywordIndex := -1;
    Expressions[i].FAssociatedExpressionKeywordIndex := -1;
    if Expressions[i].FRecognizedType = etExpressionKeyWord then
      RecognizeExpressionType(i); // 2016-10-13 ... Expressions[i].FRecognizedType := etText;
  end;

  // Merge some separated cars in some cases :
  for i := ExpressionCount-1 downto 0 do
  begin
    // Merge percentage symbol with its value :
    if Expressions[i].FDERValue = '%' then
      if i > 0 then
        if Expressions[i-1].FRecognizedType in [etInteger, etFloat] then
          if ExpressionsSideBySide(i-1, i, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * AutoMergeExpressionsRatio)) then
            MergeExpressions(i, i-1, true);

    // Merge money char with its value:
    for m := 1 to length(DERMoneyCars) do
      if Expressions[i].FDERValue = DERMoneyCars[m] then
      begin
        MoneyHandled := false;

        // Value is before ?
        if i > 0 then
          if Expressions[i-1].FRecognizedType in [etInteger, etFloat] then
            if ExpressionsSideBySide(i-1, i, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * AutoMergeExpressionsRatio)) then
            begin
              MoneyHandled := true;
              MergeExpressions(i, i-1, true);
            end;

        // Value is after ?
        if not MoneyHandled then
          if i < ExpressionCount-1 then
            if Expressions[i+1].FRecognizedType in [etInteger, etFloat] then
              if ExpressionsSideBySide(i, i+1, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * AutoMergeExpressionsRatio)) then
                MergeExpressions(i, i+1, true);
      end;
  end;

  if Assigned(FAfterInitializeRecognition) then
    FAfterInitializeRecognition(Self);
end;

function TcyDocER.PatternRectDefined(const ElementIndex: Integer): Boolean;
begin
  Result := (Elements[ElementIndex].FPatternFromLeftMm <> 0) or (Elements[ElementIndex].FPatternFromTopMm <> 0)
            or (Elements[ElementIndex].FPatternToRightMm <> 0) or (Elements[ElementIndex].FPatternToBottomMm <> 0);
end;

function TcyDocER.SizeRestrictionDefined(const ElementIndex: Integer): Boolean;
begin
  Result := (Elements[ElementIndex].FValueMinCars > 1) or (Elements[ElementIndex].FValueMaxCars > 1) or (Elements[ElementIndex].FPatternValueSizeMm > 1);
end;

function TcyDocER.IsElementKeyword(const aElementIndex: Integer; aStr: String): Integer;
var
  k: Integer;
  aKeyword: String;
begin
  Result := -1;
  aStr := AnsiUpperCase( DoStringToDERCharSet(aStr) );

  for k := 0 to Elements[aElementIndex].KeyWords.Count-1 do
  begin
    aKeyword := AnsiUpperCase( DoStringToDERCharSet(Elements[aElementIndex].KeyWords[k]) );

    if aKeyword = aStr then
    begin
      Result := k;
      Break;
    end;
  end;
end;

procedure TcyDocER.RecognizeElementsKeywords;
type
  rKeywords = record
    ElementIndex: Integer;
    KeywordIndex: Integer;
    LengthKeyword: Integer;
  end;

          procedure OrderByLengthAndRecognizeKeywords;
          var
            SortedKeywords: Array of rKeywords;
            NewElementIndex, NewKeywordIndex, NewLengthKeyword: Integer;
            TmpElementIndex, TmpKeywordIndex, TmpLengthKeyword: Integer;
            i, s, LengthSortedKeywords: Integer;

            ExpressionIndexFound, k: Integer;
            Accepted: Boolean;
          begin
            LengthSortedKeywords := 0;

            for i := 0 to FElements.Count-1 do
              for k := 0 to FElements[i].FKeyWords.Count-1 do
              begin
                NewElementIndex  := i;
                NewKeywordIndex  := k;
                NewLengthKeyword := Length(FElements[i].FKeyWords[k]);

                // Insert in array ordered by keyword's length :
                for s := 0 to LengthSortedKeywords-1 do
                  if NewLengthKeyword > SortedKeywords[s].LengthKeyword then
                  begin
                    TmpElementIndex  := SortedKeywords[s].ElementIndex;
                    TmpKeywordIndex  := SortedKeywords[s].KeywordIndex;
                    TmpLengthKeyword := SortedKeywords[s].LengthKeyword;

                    SortedKeywords[s].ElementIndex  := NewElementIndex;
                    SortedKeywords[s].KeywordIndex  := NewKeywordIndex;
                    SortedKeywords[s].LengthKeyword := NewLengthKeyword;

                    NewElementIndex  := TmpElementIndex;
                    NewKeywordIndex  := TmpKeywordIndex;
                    NewLengthKeyword := TmpLengthKeyword;
                  end;

                // Insert at the end :
                Inc(LengthSortedKeywords);
                SetLength(SortedKeywords, LengthSortedKeywords);
                SortedKeywords[LengthSortedKeywords-1].ElementIndex  := NewElementIndex;
                SortedKeywords[LengthSortedKeywords-1].KeywordIndex  := NewKeywordIndex;
                SortedKeywords[LengthSortedKeywords-1].LengthKeyword := NewLengthKeyword;
              end;

            // Recognize inline (in a single line) keywords :
            if FRecognitionPriorityMode = rpSinglelineKeywordsLength then
              for k := 0 to LengthSortedKeywords-1 do
              begin
                ExpressionIndexFound := -1;

                if Assigned(FBeforeRecognizeElementKeyword) then
                  FBeforeRecognizeElementKeyword(Self, SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex);

                repeat
                  Inc(ExpressionIndexFound);
                  ExpressionIndexFound := RecognizeNextExpressionElementKeyword(SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex, ExpressionIndexFound, true, Accepted);   // Search same keyword until not ExpressionFound
                until ExpressionIndexFound = -1;

                if Assigned(FAfterRecognizeElementKeyword) then
                  FAfterRecognizeElementKeyword(Self, SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex);
              end;

            // Recognize keywords :
            for k := 0 to LengthSortedKeywords-1 do
            begin
              ExpressionIndexFound := -1;

              if Assigned(FBeforeRecognizeElementKeyword) then
                FBeforeRecognizeElementKeyword(Self, SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex);

              repeat
                Inc(ExpressionIndexFound);
                ExpressionIndexFound := RecognizeNextExpressionElementKeyword(SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex, ExpressionIndexFound, false, Accepted);   // Search same keyword until not ExpressionFound
              until ExpressionIndexFound = -1;

              if Assigned(FAfterRecognizeElementKeyword) then
                FAfterRecognizeElementKeyword(Self, SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex);
            end;
          end;

          // Find all element' s Keyword ocurrences in the document:
          procedure RecognizeElementKeywords(const ElementIndex: Integer);
          var
            ExpressionIndexFound, k: Integer;
            Accepted: Boolean;
          begin
            // Search for all keywords of specified element:
            for k := 0 to FElements[ElementIndex].FKeyWords.Count-1 do
            begin
              ExpressionIndexFound := -1;

              if Assigned(FBeforeRecognizeElementKeyword) then
                FBeforeRecognizeElementKeyword(Self, ElementIndex, k);

              repeat
                Inc(ExpressionIndexFound);
                ExpressionIndexFound := RecognizeNextExpressionElementKeyword(ElementIndex, k, ExpressionIndexFound, false, Accepted);   // Search same keyword until not ExpressionFound
              until ExpressionIndexFound = -1;

              if Assigned(FAfterRecognizeElementKeyword) then
                FAfterRecognizeElementKeyword(Self, ElementIndex, k);
            end;
          end;

var
  i: Integer;
begin
  //if Assigned(FBeforeRecognizeElementsKeywords) then
  //  FBeforeRecognizeElementsKeywords(Self);

  // *** Find Keywords *** //
  if roKeywordsByPriority in FRecognitionOptions then
  begin
    OrderByLengthAndRecognizeKeywords;
  end
  else begin
    for i := 0 to Elements.Count-1 do
      RecognizeElementKeywords(i);
  end;
end;

procedure TcyDocER.RecognizeElementsValues;
var
  i: Integer;
begin
  if Assigned(FBeforeRecognizeElementsValues) then
    FBeforeRecognizeElementsValues(Self);

  // *** 2016-10-28 Find pattern element values (because some expressions can be badly affected to an element despite of the zone was correctly identified to another element) *** //

  // Single value recognition :
  for i := 0 to Elements.Count-1 do
    if Elements[i].FKeyWords.Text = '' then
      if Elements[i].ValueCount = 1 then
        RecognizeElementValues(i);

  // Multiple values recognition :
  for i := 0 to Elements.Count-1 do
    if Elements[i].FKeyWords.Text = '' then
      if Elements[i].ValueCount <> 1 then
        RecognizeElementValues(i);



  // *** Find non pattern element values *** //

  // Single value recognition :
  for i := 0 to Elements.Count-1 do
    if Elements[i].FKeyWords.Text <> '' then
      if Elements[i].ValueCount = 1 then
        RecognizeElementKeywordsValues(i);

  // Multiple values recognition :
  for i := 0 to Elements.Count-1 do
    if Elements[i].FKeyWords.Text <> '' then
      if Elements[i].ValueCount <> 1 then
        RecognizeElementKeywordsValues(i);
end;

function TcyDocER.RecognizeDay(aDERString: DERString): Integer;
var
  TryInt, d: Integer;
begin
  Result := 0;

  if not TryStrToInt(aDERString, TryInt) then
  begin
    // Remove numbers prefixes :
    if FDayNumberSuffixes.Count = 0 then Exit;

    aDERString := AnsiUppercase(aDERString);
    for d := 0 to FDayNumberSuffixes.Count - 1 do
      if String_SubstFast( AnsiUppercase(FDayNumberSuffixes[d]), '', aDERString) <> 0 then
      begin
        if TryStrToInt(aDERString, TryInt) then
          Result := TryInt;
        Break;
      end;
  end
  else
    Result := TryInt;

  if Result <> 0 then
    if not (Result in [1..31]) then
      Result := 0;
end;

function TcyDocER.RecognizeMonth(aDERString: DERString): Integer;
var
  m, i: Integer;
  ShortMonthValue: DERString;
begin
  Result := 0;
  if FShortMonthNames.Count < 12 then Exit;

  aDERString := AnsiUppercase(aDERString);

  for m := 1 to 12 do
  begin
    ShortMonthValue := AnsiUppercase( DoStringToDERCharSet(FShortMonthNames[m-1]) );

    for i := 1 to length(ShortMonthValue) do
      {$IFDEF UNICODE}
      if not CharInSet(ShortMonthValue[i], ['A'..'Z']) then  // Remove any invalid char like '.' from "Jan." for exemple ...
      {$ELSE}
      if not (ShortMonthValue[i] in ['A'..'Z']) then  // Remove any invalid char like '.' from "Jan." for exemple ...
      {$ENDIF}
      begin
        Delete(ShortMonthValue, i, Length(ShortMonthValue));
        Break;
      end;

    if pos(ShortMonthValue, aDERString) > 0 then
    begin
      Result := m;
      Break;
    end;
  end;
end;

// Handle some anormal situations :
// - 3 625,00 divided into 2 expressions ("3" and "625,00")
// - 3 625,00 € divided into 2 expressions ("3" and "625,00€")
// - 3 625 divided into 2 expressions ("3" and "625")
// - 3 625 € divided into 2 expressions ("3" and "625€")
// - Long date detection ([day] + [Month name] + [year]) :
function TcyDocER.ExpressionAdded: Integer;
var
  RecognizePeriod, b: Boolean;
  pxTolerence1, pxTolerence2, pxTolerence, aYear, aMonth, aDay: Integer;
  aDate: TDateTime;

  DateSeparator: String;
  AnalyzeExpression: Integer;

      // See there' s natural separators into expression value: '()'
      function CanMergeExpressions(aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
      begin
        Result := true;
        if pos('(', Expressions[aExpressionIndex1].Value) <> 0 then Result := false;
        if pos(')', Expressions[aExpressionIndex1].Value) <> 0 then Result := false;

        if pos('(', Expressions[aExpressionIndex2].Value) <> 0 then Result := false;
        if pos(')', Expressions[aExpressionIndex2].Value) <> 0 then Result := false;
      end;

begin
  Result := ExpressionCount - 1;

  if Result <= 2 then Exit;
  if Expressions[Result].DERValue = '' then Exit;  // 2017-02-02 ...
  if Expressions[Result-1].DERValue = '' then Exit;  // 2017-02-02 ...

  pxTolerence1 := Round((Expressions[Result].FBottomPxPos - Expressions[Result].FTopPxPos) * AutoMergeExpressionsRatio);
  pxTolerence2 := Round((Expressions[Result - 1].FBottomPxPos - Expressions[Result - 1].FTopPxPos) * AutoMergeExpressionsRatio);

  if pxTolerence1 > pxTolerence2
  then pxTolerence := pxTolerence1
  else pxTolerence := pxTolerence2;

  // 2017-02-06 Merge with prior expression if some conditions :
  if string_end(Expressions[Result].FDERValue, 1) = 'º' then  // Exemple 'Nº', '1º' ...
      if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
      begin
        if CanMergeExpressions(Result, Result - 1) then
        begin
          MergeExpressions(Result, Result - 1, false);
          Dec(Result);
          if ExpressionCount <= 1 then Exit;
        end;
      end;

  // Merge with prior negative symbol :
  if Expressions[Result - 1].FDERValue = '-' then
    if Expressions[Result].FRecognizedType in [etMoney, etFloat, etInteger, etPercentage] then
      if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then        // Added on 2014/01/02 ...
      begin
        if CanMergeExpressions(Result, Result - 1) then
        begin
          MergeExpressions(Result, Result - 1, true);
          Dec(Result);
          if ExpressionCount <= 1 then Exit;
        end;
      end;

  // Short/long month date detection ("15" + "january" + "1977" or "15" + "jan" + "1977") :
  if FShortMonthNames.Count >= 12 then
    if (Result > 1) and (Expressions[Result].FRecognizedType = etInteger) then
      if Length(Expressions[Result].FRecognizedInteger) = 4 then
      begin
        aYear := StrToInt(Expressions[Result].FRecognizedInteger);

        if (aYear >= Recognition_MinDate) and (aYear <= Recognition_MaxDate) then
          if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
          begin
            aMonth := RecognizeMonth(Expressions[Result - 1].FDERValue);

            if aMonth <> 0 then
            begin
              aDay := 0;

              if ExpressionsSideBySide(Result-2, Result-1, pxTolerence) then
              begin
                aDay := RecognizeDay(Expressions[Result-2].FDERValue);

                if aDay <> 0 then
                  try
                    if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                    begin
                      // Merge 3 expressions into a date :
                      MergeExpressions(Result, Result - 1, true);
                      Dec(Result);

                      MergeExpressions(Result, Result - 1, true);  // Carefull: ExpressionCount was modified by MergeExpressions() call ...
                      Dec(Result);

                      with Expressions[Result] do
                      begin
                        FRecognizedDate := intToStr(Trunc(aDate));
                        FRecognizedType := etDate;

                        if Assigned(FOnExpressionRecognizedTypeChanged) then
                          FOnExpressionRecognizedTypeChanged(Self, Result);
                      end;
                    end;
                  except

                  end;
              end;

              if aDay = 0 then
              begin
                aDate := EncodeDate(aYear, aMonth, 1);

                // Merge last 2 expressions into a period :
                MergeExpressions(Result, Result - 1, true);
                Dec(Result);

                with Expressions[Result] do
                begin
                  FRecognizedDate := intToStr(Trunc(aDate));
                  FRecognizedType := etMonthYear;

                  if Assigned(FOnExpressionRecognizedTypeChanged) then
                    FOnExpressionRecognizedTypeChanged(Self, Result);
                end;
              end;
            end;
          end;
      end;

  // 2016-10-12   "30" + "/" + "o5/2016"
  if Expressions[Result].FRecognizedType = etMonthYear then
  begin
    aYear := YearOf(Expressions[Result].RecognizedDateValue);
    aMonth := MonthOf(Expressions[Result].RecognizedDateValue);

    if (aYear >= Recognition_MinDate) and (aYear <= Recognition_MaxDate) then
    begin
      aDay := 0;
      DateSeparator := '';
      AnalyzeExpression := Result - 1;

      // Search day :
      while (AnalyzeExpression >= 0) and (aDay = 0) do
      begin
        if not ExpressionsSideBySide(AnalyzeExpression, AnalyzeExpression+1, pxTolerence) then
          aDay := -1
        else
          if Expressions[AnalyzeExpression].FRecognizedType <> etInteger then
          begin
            if (DateSeparator = '') and ((Expressions[AnalyzeExpression].FDERValue = '/') or (Expressions[AnalyzeExpression].FDERValue = '-') or (Expressions[AnalyzeExpression].FDERValue = '.'))
            then DateSeparator := Expressions[AnalyzeExpression].FDERValue
            else aDay := -1;
          end
          else
            if not TryStrToInt(Expressions[AnalyzeExpression].FRecognizedInteger, aDay) then
              aDay := -1;

        dec(AnalyzeExpression);
      end;

      if (aDay > 0) and (aDay < 32) then
        try
          if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
          begin
            // Merge all expressions into a date :
            while Result > AnalyzeExpression+1 do
            begin
              MergeExpressions(Result, Result - 1, false);  // Info: ExpressionCount was modified on MergeExpressions() ...
              Dec(Result);
            end;


            with Expressions[Result] do
            begin
              FRecognizedDate := intToStr(Trunc(aDate));
              FRecognizedType := etDate;

              if Assigned(FOnExpressionRecognizedTypeChanged) then
                FOnExpressionRecognizedTypeChanged(Self, Result);
            end;
          end;
        except
        end;
    end;
  end;




  // dd/mm/yy(yy) date / period detection on several expressions ("15" + "/" + "01" + "/" + "1977") :   // Added on 19/05/2013
  if Expressions[Result].FRecognizedType = etInteger then
  begin
    aYear := 0;
    if Length(Expressions[Result].FRecognizedInteger) in [2, 4] then
      aYear := StrToInt(Expressions[Result].FRecognizedInteger);

    if Length(Expressions[Result].FRecognizedInteger) = 2 then
    begin
      aYear := aYear + 2000;
      RecognizePeriod := false;
    end
    else
      RecognizePeriod := true;

    if (aYear >= Recognition_MinDate) and (aYear <= Recognition_MaxDate) then
    begin
      aMonth := 0;
      DateSeparator := '';
      AnalyzeExpression := Result - 1;

      // Search month :
      while (AnalyzeExpression >= 0) and (aMonth = 0) do
      begin
        if not ExpressionsSideBySide(AnalyzeExpression, AnalyzeExpression+1, pxTolerence) then
          aMonth := -1
        else
          if Expressions[AnalyzeExpression].FRecognizedType <> etInteger then
          begin
            if (Expressions[AnalyzeExpression].FDERValue = '/') or (Expressions[AnalyzeExpression].FDERValue = '-') or (Expressions[AnalyzeExpression].FDERValue = '.')
            then DateSeparator := Expressions[AnalyzeExpression].FDERValue
            else aMonth := -1;
          end
          else
            if not TryStrToInt(Expressions[AnalyzeExpression].FRecognizedInteger, aMonth) then
              aMonth := -1;

        dec(AnalyzeExpression);
      end;

      if (aMonth > 0) and (aMonth < 13) then
      begin
        aDay := 0;

        while (AnalyzeExpression >= 0) and (aDay = 0) do
        begin
          if not ExpressionsSideBySide(AnalyzeExpression, AnalyzeExpression+1, pxTolerence) then
            aDay := -1
          else
            if Expressions[AnalyzeExpression].FRecognizedType <> etInteger then
            begin
              if (Expressions[AnalyzeExpression].FDERValue = '/') or (Expressions[AnalyzeExpression].FDERValue = '-') or (Expressions[AnalyzeExpression].FDERValue = '.') then
              begin
                if DateSeparator <> Expressions[AnalyzeExpression].FDERValue then
                  aDay := -1;
              end
              else
                aDay := -1;
            end
            else
              if not TryStrToInt(Expressions[AnalyzeExpression].FRecognizedInteger, aDay) then
                aDay := -1;

          dec(AnalyzeExpression);
        end;

        if (aDay > 0) and (aDay < 32) then
          try
            if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
            begin
              // Merge all expressions into a date :
              while Result > AnalyzeExpression+1 do
              begin
                MergeExpressions(Result, Result - 1, true);  // Info: ExpressionCount was modified on MergeExpressions() ...
                Dec(Result);
              end;

              with Expressions[Result] do
              begin
                FRecognizedDate := intToStr(Trunc(aDate));
                FRecognizedType := etDate;

                if Assigned(FOnExpressionRecognizedTypeChanged) then
                  FOnExpressionRecognizedTypeChanged(Self, Result);
              end;
            end;
          except
          end;

        if (aDay = -1) and RecognizePeriod then
        begin
          aDate := EncodeDate(aYear, aMonth, 1);

          // Merge all expressions into a period :
          while Result > AnalyzeExpression + 2 do
          begin
            MergeExpressions(Result, Result - 1, true);  // Info: ExpressionCount was modified on MergeExpressions() ...
            Dec(Result);
          end;


          with Expressions[Result] do
          begin
            FRecognizedDate := intToStr(Trunc(aDate));
            FRecognizedType := etMonthYear;

            if Assigned(FOnExpressionRecognizedTypeChanged) then
              FOnExpressionRecognizedTypeChanged(Self, Result);
          end;
        end;
      end;
    end;
  end;

  // Merge with prior added expression value for cutted float/integer/money/percentage values:
  case Expressions[Result].FRecognizedType of
    etMoney:
    begin
      if Result > 0 then
        {$IFDEF UNICODE}
        if CharInSet(Expressions[Result].FDERValue[1], ['.', '0'..'9']) then    // Merge if money symbol at the end!
        {$ELSE}
        if Expressions[Result].FDERValue[1] in ['.', '0'..'9'] then    // Merge if money symbol at the end!
        {$ENDIF}
        begin
          if Expressions[Result].FRecognizedFloat = Expressions[Result].FRecognizedInteger
          then b := Expressions[Result - 1].FRecognizedType in [etFloat, etInteger]                         // Money value is an integer
          else b := Expressions[Result - 1].FRecognizedType in [etInteger];                                 // Money value is a float

          if b then
            if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
              if CanMergeExpressions(Result, Result - 1) then
              begin
                MergeExpressions(Result, Result - 1, true);
                Dec(Result);
              end;
        end;
    end;

    etFloat:                //  "22  33,6"    or    "€22 33,6"     // Don' t merge if  "2015 11:06"  !!!
    begin
      if Result > 0 then
        case Expressions[Result - 1].FRecognizedType of
          etInteger:
            if pos(cyDERUtils.LocalFormatSettings.DecimalSeparator, Expressions[Result].RecognizedFloat) in [4,7,10] then    // merge '2015' with '000.09' but don' t merge  '2015' with '11:06'
              if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
                if CanMergeExpressions(Result, Result - 1) then
                begin
                  MergeExpressions(Result, Result - 1, true);
                  Dec(Result);
                end;

          etMoney:
            {$IFDEF UNICODE}
            if not CharInSet(Expressions[Result - 1].FDERValue[1], ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
            {$ELSE}
            if not (Expressions[Result - 1].FDERValue[1] in ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
            {$ENDIF}
              if Expressions[Result - 1].FRecognizedFloat = Expressions[Result - 1].FRecognizedInteger then    // Money value is an integer
                if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
                  if CanMergeExpressions(Result, Result - 1) then
                  begin
                    MergeExpressions(Result, Result - 1, true);
                    Dec(Result);
                  end;
        end;
    end;

    etInteger, etPercentage:
    begin
      if Result > 0 then
       if (Expressions[Result].FRecognizedType <> etPercentage) or (Expressions[Result - 1].FRecognizedType <> etMoney) then // Cannot merge money with percentage expressions
        case Expressions[Result - 1].FRecognizedType of
          etText, etID:
            if (Result > 1) and (Expressions[Result - 1].FDERValue = '.') then // Decimal separator
              if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
                if CanMergeExpressions(Result, Result - 1) then
                  if CanMergeExpressions(Result - 1, Result - 2) then
                    case Expressions[Result - 2].FRecognizedType of
                      etInteger:
                        if ExpressionsSideBySide(Result - 2, Result - 1, pxTolerence) then
                        begin
                          MergeExpressions(Result, Result - 1, true);
                          Dec(Result);

                          MergeExpressions(Result, Result - 1, true);
                          Dec(Result);
                        end;

                      etMoney:
                        {$IFDEF UNICODE}
                        if not CharInSet(Expressions[Result - 2].FDERValue[1], ['0'..'9']) then    // Merge if money symbol at the beginning!
                        {$ELSE}
                        if not (Expressions[Result - 2].FDERValue[1] in ['0'..'9']) then    // Merge if money symbol at the beginning!
                        {$ENDIF}
                          if ExpressionsSideBySide(Result - 2, Result - 1, pxTolerence) then
                          begin
                            MergeExpressions(Result, Result - 1, true);
                            Dec(Result);

                            MergeExpressions(Result, Result - 1, true);
                            Dec(Result);
                          end;
                    end;

          etInteger:
            if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
              if CanMergeExpressions(Result, Result - 1) then
              begin
                MergeExpressions(Result, Result - 1, true);
                Dec(Result);
              end;

          etFloat:
            if Expressions[Result].RecognizedFloat = Expressions[Result].RecognizedInteger then
              if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
                if CanMergeExpressions(Result, Result - 1) then
                begin
                  MergeExpressions(Result, Result - 1, true);
                  Dec(Result);
                end;

          etMoney:
            {$IFDEF UNICODE}
            if not CharInSet(Expressions[Result - 1].FDERValue[1], ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
            {$ELSE}
            if not (Expressions[Result - 1].FDERValue[1] in ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
            {$ENDIF}
              if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
                if CanMergeExpressions(Result, Result - 1) then
                begin
                  MergeExpressions(Result, Result - 1, true);
                  Dec(Result);
                end;
        end;
    end;

    etnumbers:       // 2017-02-02         merge ',73'  with prior '35' ...
    begin
      if Result > 0 then
        if Expressions[Result].FDERValue[1] = '.' then // Decimal separator
          if Expressions[Result - 1].FRecognizedType = etInteger then
            if ExpressionsSideBySide(Result - 1, Result, pxTolerence) then
              if CanMergeExpressions(Result, Result - 1) then
              begin
                MergeExpressions(Result, Result - 1, false);
                Dec(Result);
              end;
    end;
  end;
end;

procedure TcyDocER.ExpressionLoaded;
var
  WordsCount, aYear, aMonth, aDay: Integer;
  aDate: TDateTime;
begin
  if ExpressionCount = 0 then Exit;  // 2016-06-20 ...

  // short/long Date detection :
  if FShortMonthNames.Count >= 12 then
  begin
    WordsCount := SubString_Count(Expressions[ExpressionCount-1].FValue, ' ');

    if WordsCount >= 2 then
      if TryStrToInt( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 3 ), aYear ) then
        if (aYear >= Recognition_MinDate) and (aYear <= Recognition_MaxDate) then
        begin
          aMonth := RecognizeMonth( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 2 ) );

          if aMonth <> 0 then
          begin
            aDay := 0;

            // Day recognition :
            if WordsCount >= 3 then
            begin
              aDay :=  RecognizeDay(SubString_Get(Expressions[ExpressionCount-1].FDERValue, ' ', 1 ));
              // if TryStrToInt( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 1 ), aDay ) then    xxxx
              //  if (aDay > 0) and (aDay < 32) then
              if aDay <> 0 then
                try
                  if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                    with Expressions[ExpressionCount-1] do
                    begin
                      FRecognizedDate := intToStr(Trunc(aDate));
                      FRecognizedType := etDate;

                      if Assigned(FOnExpressionRecognizedTypeChanged) then
                        FOnExpressionRecognizedTypeChanged(Self, ExpressionCount-1);
                    end;
                except
                end;
            end;

            if aDay = 0 then
            begin
              aDate := EncodeDate(aYear, aMonth, 1);

              with Expressions[ExpressionCount-1] do
              begin
                FRecognizedDate := intToStr(Trunc(aDate));
                FRecognizedType := etMonthYear;

                if Assigned(FOnExpressionRecognizedTypeChanged) then
                  FOnExpressionRecognizedTypeChanged(Self, ExpressionCount-1);
              end;
            end;
          end;
        end;
  end;
end;

function TcyDocER.ExpressionInRect(const aExpressionIndex: Integer; const InRect: TRect; const AllowPartiallyInside: Boolean = true): Boolean;
var DestRect: TRect;
begin
  Result := false;
  with Expressions[aExpressionIndex] do
    if AllowPartiallyInside then
      Result := IntersectRect(DestRect, InRect, classes.Rect(FLeftPxPos, FTopPxPos, FRightPxPos, FBottomPxPos))
    else
      if (FLeftPxPos >= InRect.Left) and (FTopPxPos >= InRect.Top) and (FRightPxPos <= InRect.Right) and (FBottomPxPos <= InRect.Bottom) then
        Result := true;
end;

function TcyDocER.ExpressionInRect(const aExpressionIndex: Integer; const LeftPx, TopPx, RightPx, BottomPx: Integer; const AllowPartiallyInside: Boolean = true): Boolean;
begin
  Result := ExpressionInRect(aExpressionIndex, Rect(LeftPx, TopPx, RightPx, BottomPx), AllowPartiallyInside);
end;

// - Long date detection ([day] + [Month name] + [year]) :
procedure TcyDocER.RecognizeLongDates;
var
  i, pxTolerence, aYear, aMonth, aDay: Integer;
  aDate: TDateTime;
begin
  if FShortMonthNames.Count < 12 then Exit;
  i := ExpressionCount-1;

  while i > 0 do
  begin
    if (i >= 2) and (Expressions[i].FRecognizedType = etInteger) then
      if Length(Expressions[i].FRecognizedInteger) = 4 then
      begin
        aYear := StrToInt(Expressions[i].FRecognizedInteger);

        if (aYear >= Recognition_MinDate) and (aYear <= Recognition_MaxDate) then
        begin
          // Expressions can be separed with large spacing
          pxTolerence := GetPagePixelsWidth(Expressions[i].FPageNumber) div 5;  // 1/5 sheet width

          if ExpressionsSideBySide(i-1, i, pxTolerence) then
          begin
            aMonth := RecognizeMonth(Expressions[i-1].FDERValue);

            if aMonth <> 0 then
            begin
              aDay := 0;

              if ExpressionsSideBySide(i-2, i-1, pxTolerence) then
              begin
                aDay :=  RecognizeDay(Expressions[i-2].FDERValue);

                // if Expressions[i-2].FRecognizedType = etInteger then         xxxx
                //  if length(Expressions[i-2].FRecognizedInteger) < 3 then // We are in presence of a day !?
                if aDay <> 0 then
                    try
                      // Validate date :
                      // aDay := StrToInt(Expressions[i-2].FRecognizedInteger);

                      // if (aDay > 0) and (aDay < 32) then
                      if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                      begin
                        // Merge 3 expressions into a date :
                        MergeExpressions(i, i-1, true);
                        Dec(i);
                        MergeExpressions(i, i-1, true);  // Carefull: ExpressionCount was modified by MergeExpressions() call ...
                        Dec(i);

                        with Expressions[i] do
                        begin
                          FRecognizedDate := intToStr(Trunc(aDate));
                          FRecognizedType := etDate;

                          if Assigned(FOnExpressionRecognizedTypeChanged) then
                            FOnExpressionRecognizedTypeChanged(Self, i);
                        end;
                      end;
                    except
                    end;
              end;

              if aDay = 0 then
              begin
                aDate := EncodeDate(aYear, aMonth, 1);

                // Merge last 2 expressions into a period :
                MergeExpressions(i, i-1, true);
                Dec(i);

                with Expressions[i] do
                begin
                  FRecognizedDate := intToStr(Trunc(aDate));
                  FRecognizedType := etMonthYear;

                  if Assigned(FOnExpressionRecognizedTypeChanged) then
                    FOnExpressionRecognizedTypeChanged(Self, i);
                end;
              end;
            end;
          end;
        end;
      end;

    Dec(i);
  end;
end;

procedure TcyDocER.GetExpressionListFromRect(const FromRect: TRect; const FromPage: Integer; ExpressionList: TStrings; const AllowPartiallyInside: Boolean = true);
var
  i: Integer;
  IgnoreFromRect: Boolean;
  IncludeExpression: Boolean;
begin
  ExpressionList.Clear;
  IgnoreFromRect := (FromRect.Left = 0) and (FromRect.Top = 0) and (FromRect.Right = 0) and (FromRect.Bottom = 0);

  for i := 0 to ExpressionCount-1 do
  begin
    IncludeExpression := (FromPage = 0) or (FromPage = Expressions[i].PageNumber);

    if IncludeExpression and (not IgnoreFromRect) then
      IncludeExpression := ExpressionInRect(i, FromRect, AllowPartiallyInside);

    if IncludeExpression then
      ExpressionList.Add(intToStr(i));
  end;
end;

function TcyDocER.GetOCRTextFromList(aExpressionList: TStrings; const SeparWithSpace: boolean): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to aExpressionList.Count-1 do
  begin
    if (i <> 0) and SeparWithSpace then
      Result := Result + ' ';

    Result := Result + Expressions[ StrToInt(aExpressionList[i]) ].Value;
  end;
end;

function TcyDocER.GetOCRTextFromRect(const FromRect: TRect; const FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
var
  LastInsertedExpressionIndex, i: Integer;
  IgnoreFromRect: Boolean;
  IncludeExpression: Boolean;
  StrConexion: String;
begin
  Result := '';
  LastInsertedExpressionIndex := -1;
  IgnoreFromRect := (FromRect.Left = 0) and (FromRect.Top = 0) and (FromRect.Right = 0) and (FromRect.Bottom = 0);

  for i := 0 to ExpressionCount-1 do
  begin
    IncludeExpression := (FromPage = 0) or (FromPage = Expressions[i].PageNumber);

    if IncludeExpression and (not IgnoreFromRect) then
      IncludeExpression := ExpressionInRect(i, FromRect, AllowPartiallyInside);

    if IncludeExpression then
    begin
      StrConexion := '';

      if (Result <> '') and (LastInsertedExpressionIndex <> -1) then
        if Expressions[i].FPageNumber <> Expressions[LastInsertedExpressionIndex].FPageNumber then
          StrConexion := #13#10
        else
          if ExpressionsInSameLine(i, LastInsertedExpressionIndex)
          then StrConexion := ' '
          else StrConexion := #13#10;

      Result := Result + StrConexion + Expressions[i].FValue;
      LastInsertedExpressionIndex := i;
    end;
  end;
end;

function TcyDocER.GetOCRText(const FromPage: Integer): String;
begin
  Result := GetOCRTextFromRect(classes.Rect(0, 0, 0, 0), FromPage, true);
end;

function TcyDocER.GetAsDocumentOCRText(const FromPage: Integer = 0): String;
begin
  Result := GetAsDocumentOCRTextFromRect(classes.Rect(0, 0, 0, 0), FromPage, true);
end;

function TcyDocER.GetAsDocumentOCRTextFromRect(const FromRect: TRect; const FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
var
  i, c, CarsCount, EntersCount: Integer;
  CurrentPage, CurrentPageLinesCount: Integer;
  IgnoreFromRect: Boolean;
  IncludeExpression: Boolean;
  StrConexion: String;

  TotalCars: Int64;
  TotalCarsWidth, TotalExpressionsHeight: Int64;

const
  FontSize = 10;

      function CalcSpaceChars(const _Resolution, _PixelsWidth: Integer): Integer;
      begin
        Result := 0;
        if _Resolution = 0 then Exit;

        Result := round(_PixelsWidth / _Resolution * FontSize);
      end;

      function CalcEnters(const _Resolution, _PixelsHeight: Integer): Integer;
      begin
        Result := 0;
        if _Resolution = 0 then Exit;

        Result := round( (_PixelsHeight / _Resolution * FontSize) / AutoMergeExpressionsRatio);
      end;

      function GetLastLineLength(const aStr: String): Integer;
      var i: Integer;
      begin
        Result := 0;
        for i := Length(aStr) downto 1 do
          if aStr[i] <> #10
          then Inc(Result)
          else Break;
      end;

begin
  // Initialisation :
  Result := '';
  CurrentPage := -1;
  CurrentPageLinesCount := 1;

  // Determine font size :
  TotalCars := 0;
  TotalCarsWidth := 0;
  TotalExpressionsHeight := 0;
  for i := 0 to ExpressionCount-1 do
  begin
    inc(TotalCars, length(Expressions[i].FValue));
    inc(TotalCarsWidth, (Expressions[i].FRightPxPos - Expressions[i].FLeftPxPos));
    inc(TotalExpressionsHeight, (Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos));
  end;

  // Extract text :
  IgnoreFromRect := (FromRect.Left = 0) and (FromRect.Top = 0) and (FromRect.Right = 0) and (FromRect.Bottom = 0);

  for i := 0 to ExpressionCount-1 do
  begin
    if CurrentPage <> Expressions[i].PageNumber then
    begin
      CurrentPage := Expressions[i].PageNumber;
      CurrentPageLinesCount := 1;
    end;

    IncludeExpression := (FromPage = 0) or (FromPage = Expressions[i].PageNumber);

    if IncludeExpression and (not IgnoreFromRect) then
      IncludeExpression := ExpressionInRect(i, FromRect, AllowPartiallyInside);

    if IncludeExpression then
    begin
      if i > 0 then
      begin
        if ExpressionsInSameLine(i, i-1) and (Expressions[i].FPageNumber = Expressions[i-1].FPageNumber) then
        begin
          // *** Continue line *** //
          StrConexion := '';

          // Insert space chars :
          CarsCount := CalcSpaceChars(GetPageResolution(Expressions[i].FPageNumber), Expressions[i].FLeftPxPos) - GetLastLineLength(Result);
          if CarsCount <= 0 then CarsCount := 1;     // At least one char ...

          for c := 1 to CarsCount do
            StrConexion := StrConexion + ' ';
        end
        else begin
          // *** From new line/new page *** //
          StrConexion := '';

          // Insert Enters :
          EntersCount := CalcEnters(GetPageResolution(Expressions[i].FPageNumber), Expressions[i].FTopPxPos) - CurrentPageLinesCount;
          if EntersCount <= 0 then EntersCount := 1; // At least one Enter ...
          for c := 1 to EntersCount do
            StrConexion := StrConexion + #13#10;

          Inc(CurrentPageLinesCount, EntersCount);

          // Insert space chars :
          CarsCount := CalcSpaceChars(GetPageResolution(Expressions[i].FPageNumber), Expressions[i].FLeftPxPos);
          for c := 1 to CarsCount do
            StrConexion := StrConexion + ' ';
        end;
      end
      else begin
        // *** First document line *** //
        StrConexion := '';

        // Insert Enters :
        EntersCount := CalcEnters(GetPageResolution(Expressions[i].FPageNumber), Expressions[i].FTopPxPos);

        for c := 1 to EntersCount do
          StrConexion := StrConexion + #13#10;

        Inc(CurrentPageLinesCount, EntersCount);

        // Insert space chars :
        CarsCount := CalcSpaceChars(GetPageResolution(Expressions[i].FPageNumber), Expressions[i].FLeftPxPos);
        for c := 1 to CarsCount do
          StrConexion := StrConexion + ' ';
      end;

      Result := Result + StrConexion + Expressions[i].FValue;
    end;
  end;
end;

procedure TcyDocER.RotatePageExpressions(const PageNumber, PageWidthBeforeRotation, PageHeightBeforeRotation: Integer; const ToRight: Boolean);
var
  i, SaveLeftPxPos, SaveRightPxPos, SaveTopPxPos, SaveBottomPxPos: Integer;
begin
  for i := 0 to ExpressionCount-1 do
    if (PageNumber = 0) or (Expressions[i].FPageNumber = PageNumber) then
    begin
      SaveLeftPxPos := Expressions[i].FLeftPxPos;
      SaveRightPxPos := Expressions[i].FRightPxPos;
      SaveTopPxPos := Expressions[i].FTopPxPos;
      SaveBottomPxPos := Expressions[i].FBottomPxPos;

      if ToRight then
      begin
        Expressions[i].FLeftPxPos   := PageHeightBeforeRotation - SaveBottomPxPos;
        Expressions[i].FRightPxPos  := PageHeightBeforeRotation - SaveTopPxPos;
        Expressions[i].FTopPxPos    := SaveLeftPxPos;
        Expressions[i].FBottomPxPos := SaveRightPxPos;
      end
      else begin
        Expressions[i].FLeftPxPos   := SaveTopPxPos;
        Expressions[i].FRightPxPos  := SaveBottomPxPos;
        Expressions[i].FTopPxPos    := PageWidthBeforeRotation - SaveRightPxPos;
        Expressions[i].FBottomPxPos := PageWidthBeforeRotation - SaveLeftPxPos;
      end;
    end;
end;

function TcyDocER.IsExpressionsSameValue(const ExpressionIndex1, ExpressionIndex2: Integer): Boolean;
begin
  Result := false;

  if Expressions[ExpressionIndex1].RecognizedType = Expressions[ExpressionIndex2].RecognizedType then
    if Expressions[ExpressionIndex1].RecognizedValue = Expressions[ExpressionIndex2].RecognizedValue then
      Result := true;
end;

function TcyDocER.GetPagePixelsWidth(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
begin
  Result := DefaultDocERPagePixelsWidth;

  if (PageNumber > 0) and (PageNumber <= PageCount) then
  begin
    if FromBeginning
    then Result := Pages[PageNumber - 1].FPixelsWidth
    else Result := Pages[((PageCount - PageNumber) + 1) - 1].FPixelsWidth;
  end
  else
    if PageCount <> 0 then
      Result := Pages[0].FPixelsWidth;
end;

function TcyDocER.GetPagePixelsHeight(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
begin
  Result := DefaultDocERPagePixelsHeight;

  if (PageNumber > 0) and (PageNumber <= PageCount) then
  begin
    if FromBeginning
    then Result := Pages[PageNumber - 1].FPixelsHeight
    else Result := Pages[((PageCount - PageNumber) + 1) - 1].FPixelsHeight;
  end
  else
    if PageCount <> 0 then
      Result := Pages[0].FPixelsHeight;
end;

function TcyDocER.GetPageResolution(const PageNumber: Integer; const FromBeginning: Boolean = true): Integer;
begin
  Result := DefaultDocERPageResolution;

  if (PageNumber > 0) and (PageNumber <= PageCount) then
  begin
    if FromBeginning
    then Result := Pages[PageNumber - 1].FResolution
    else Result := Pages[((PageCount - PageNumber) + 1) - 1].FResolution;
  end
  else
    if PageCount <> 0 then
      Result := Pages[0].FResolution;
end;

function TcyDocER.MmToPx(const MmValue: Extended; const Resolution: Integer): Integer;
var
  Inches: Extended;
begin
  Inches := MmValue / 25.4;
  Result := Round(Inches * Resolution);
end;

function TcyDocER.MmToPxFromPage(const MmValue: Extended; const PageNumber: Integer): Integer;
begin
  Result := MmToPx(MmValue, GetPageResolution(PageNumber));
end;

function TcyDocER.PxToMmFromPage(const pxValue: Extended; const PageNumber: Integer): Extended;
begin
  Result := PxToMm(pxValue, GetPageResolution(PageNumber));
end;

function TcyDocER.PxToMm(const pxValue: Extended; const Resolution: Integer): Extended;
var
  Inches: Extended;
begin
  if Resolution = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Inches := pxValue / Resolution;
  Result := Inches * 25.4;
end;

procedure TcyDocER.RecognizeElements;
begin
  InitializeRecognition;

  RecognizeElementsKeywords;
  RecognizeElementsValues;
end;

initialization

DefaultDocERPagePixelsHeight := 3508;
DefaultDocERPagePixelsWidth := 2480;
DefaultDocERPageResolution := 300;

end.
