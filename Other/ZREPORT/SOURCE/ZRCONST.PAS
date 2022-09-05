unit ZRConst;

interface

{$R ZRConst.RES}

const
  szrBase = $A000;

(*
  {1}  - entries of v1.x not used by v2.x - left for compatibility
  {2}  - entries introduced since v2.x
  {2K} - entries introduced since v2K
*)

(*
  szrTitle        = szrBase +  0;                  {1}
  szrPageHeader   = szrBase +  1;                  {1}
  szrDetail       = szrBase +  2;                  {1}
  szrPageFooter   = szrBase +  3;                  {1}
  szrSummary      = szrBase +  4;                  {1}
  szrGroupHeader  = szrBase +  5;                  {1}
  szrGroupFooter  = szrBase +  6;                  {1}
  szrSubDetail    = szrBase +  7;                  {1}
  szrColumnHeader = szrBase +  8;                  {1}
  szrColumnFooter = szrBase +  9;                  {1}
  szrChild        = szrBase + 10;                  {1}
*)
(*
  szrTitleBandName        = szrBase + 11;          {1}
  szrPageHeaderBandName   = szrBase + 12;          {1}
  szrDetailBandName       = szrBase + 13;          {1}
  szrPageFooterBandName   = szrBase + 14;          {1}
  szrSummaryBandName      = szrBase + 15;          {1}
  szrGroupHeaderBandName  = szrBase + 16;          {1}
  szrGroupFooterBandName  = szrBase + 17;          {1}
  szrSubDetailBandName    = szrBase + 18;          {1}
  szrColumnHeaderBandName = szrBase + 19;          {1}
  szrColumnFooterBandName = szrBase + 20;          {1}
  szrChildBandName        = szrBase + 21;          {1}
*)

  // Report and controls
  szrParentReport         = szrBase +  30;
  szrParentBand           = szrBase +  31;
  szrCircularMaster       = szrBase +  36;
  szrNoChildBand          = szrBase + 110;         {2}
  szrNoControllerBand     = szrBase + 111;         {2}
  szrNoStretch            = szrBase + 112;         {2}
  szrBandDelete           = szrBase + 113;         {2}

  // ZRPrinter & preview
  szrPrinterNotReady      = szrBase +  32;
  szrPrinterNotFinished   = szrBase +  33;
  //szrIllegalNewPage       = szrBase +  34;         {1}
  szrPrinterPageIndex     = szrBase + 114;         {2}
  szrPrinterNotBusy       = szrBase + 115;         {2}
  szrIllegalPageRange     = szrBase +  35;
  szrInvalidFileName      = szrBase + 116;         {2}
  //szrNone                 = szrBase +  38;         {1}
  szrCompleted            = szrBase +  39;
  szrProcessing           = szrBase +  40;
  szrPrinting             = szrBase +  41;

  //szrPage                 = szrBase +  42;         {1}
  szrOf                   = szrBase +  43;
  //szrFileNotExist         = szrBase +  44;         {1}
  szrFirstPageHint        = szrBase +  45;
  szrPrevPageHint         = szrBase +  46;
  szrNextPageHint         = szrBase +  47;
  szrLastPageHint         = szrBase +  48;
  szrSaveReportHint       = szrBase +  49;
  szrLoadReportHint       = szrBase +  50;
  szrPrintReportHint      = szrBase +  51;
  szrPrintReport          = szrBase +  52;
  szrGotoPageNo           = szrBase + 117;         {2}
  szrPageNoHint           = szrBase + 118;         {2}
  szrSearchReport         = szrBase + 119;         {2}
  szrSearchHint           = szrBase + 120;         {2}

  // Printer Setup
  szrPrinterSetup         = szrBase + 53;
  szrFileFilter           = szrBase + 58;
  szrEscapeModelCustom    = szrBase + 56;          {2K}
  szrEscapeModelNone      = szrBase + 57;          {2K}

  szrConvertToOEM        = szrBase +  60;          {2K}
  szrIgnoreFontStyles    = szrBase +  61;          {2K}
  szrEscapeModel         = szrBase +  62;          {2K}
  szrPrintDialogCaption  = szrBase +  63;          {2K}
(*
  szrDestination          = szrBase + 54;          {1}
  szrDestinationFile      = szrBase + 55;          {1}
  szrDestinationLPT       = szrBase + 56;          {1}
  szrBrowse               = szrBase + 57;          {1}
  szrPages                = szrBase + 59;          {1}
  szrPagesAll             = szrBase + 60;          {1}
  szrPagesRange           = szrBase + 61;          {1}
  szrPagesRangeFrom       = szrBase + 62;          {1}
  szrPagesRangeTo         = szrBase + 63;          {1}
  szrCopies               = szrBase + 64;          {1}
*)

  szrOK                   = szrBase + 70;
  szrCancel               = szrBase + 71;
  szrExit                 = szrBase + 72;

  szrTime                 = szrBase + 80;
  szrDate                 = szrBase + 81;
  szrDateTime             = szrBase + 82;
  szrPageNumber           = szrBase + 83;
  szrReportTitle          = szrBase + 84;
  szrRecordNumber         = szrBase + 85;
  szrRecordCount          = szrBase + 86;

(*
  szrQSD              = SzrBase +  90;             {1}
  szrIfDesc           = SzrBase +  91;             {1}
  szrStrDesc          = SzrBase +  92;             {1}
  szrTimeDesc         = SzrBase +  93;             {1}
  szrDateDesc         = SzrBase +  94;             {1}
  szrSumDesc          = SzrBase +  95;             {1}
  szrCopyDesc         = SzrBase +  96;             {1}
  szrCountDesc        = SzrBase +  97;             {1}
  szrMaxDesc          = SzrBase +  98;             {1}
  szrMinDesc          = SzrBase +  99;             {1}
  szrAverageDesc      = SzrBase + 100;             {1}
  szrBoolDesc         = SzrBase + 101;             {1}
  szrIntDesc          = SzrBase + 102;             {1}
  szrFracDesc         = SzrBase + 103;             {1}
  szrSqrtDesc         = SzrBase + 104;             {1}
  szrTypeofDesc       = SzrBase + 105;             {1}
  szrUpperDesc        = SzrBase + 106;             {1}
  szrLowerDesc        = SzrBase + 107;             {1}
  szrPrettyDesc       = SzrBase + 108;             {1}
  szrFormatFloatDesc  = SzrBase + 109;             {1}
*)

  // Expression evaluator
  szrExpressionError   = szrBase +  37;
  szrExpressionParse   = szrBase + 130;            {2}
  szrInvalidNumeric    = szrBase + 131;            {2}
  szrInvalidDate       = szrBase + 132;            {2}
  szrInvalidString     = szrBase + 133;            {2}
  szrMissingBracket    = szrBase + 134;            {2}
  szrIllegalBracket    = szrBase + 135;            {2}
  szrUnknownEntry      = szrBase + 136;            {2}
  szrIllegalUnary      = szrBase + 137;            {2}

  szrBooleanFalse      = szrBase + 140;            {2}
  szrBooleanTrue       = szrBase + 141;            {2}
  szrIllegalLogics     = szrBase + 142;            {2}
  szrIllegalBitwise    = szrBase + 143;            {2}
  szrIllegalArguments  = szrBase + 144;            {2}
  szrIllegalDate       = szrBase + 145;            {2}
  szrCalculationError  = szrBase + 146;            {2}

  // Variables
  szrCircularVariable     = szrBase + 121;         {2}
  szrControllerLibrary    = szrBase + 122;         {2}

  // Designer form
  szrDesignNone            = szrBase + 200;        {2}
  szrDesignClose           = szrBase + 201;        {2}
  szrDesignTSBands         = szrBase + 202;        {2}
  szrDesignTSVars          = szrBase + 203;        {2}
  szrDesignGBBands         = szrBase + 204;        {2}
  szrDesignGBGroups        = szrBase + 205;        {2}
  szrDesignTVBands         = szrBase + 206;        {2}
  szrDesignLBGroups        = szrBase + 207;        {2}
  szrDesignACBandsNew      = szrBase + 208;        {2}
  szrDesignACBandsRename   = szrBase + 209;        {2}
  szrDesignACBandsDelete   = szrBase + 210;        {2}
  szrDesignACGroupsNew     = szrBase + 211;        {2}
  szrDesignACGroupsDelete  = szrBase + 212;        {2}
  szrDesignSureBandDelete  = szrBase + 213;        {2}
  szrDesignSureGroupDelete = szrBase + 214;        {2}

  szrDesignGBData          = szrBase + 215;        {2}
  szrDesignGRControllers   = szrBase + 216;        {2}
  szrDesignCellController  = szrBase + 217;        {2}
  szrDesignCellDataSet     = szrBase + 218;        {2}
  szrDesignLADataFields    = szrBase + 219;        {2}
  szrDesignLBDataFields    = szrBase + 220;        {2}

  szrDesignGBVariables           = szrBase + 221;  {2}
  szrDesignLAVariableFields      = szrBase + 222;  {2}
  szrDesignLBVariableFields      = szrBase + 223;  {2}
  szrDesignLAVariableExpressions = szrBase + 224;  {2}
  szrDesignLBVariableExpressions = szrBase + 225;  {2}
  szrDesignLAVariableTotals      = szrBase + 226;  {2}
  szrDesignLBVariableTotals      = szrBase + 227;  {2}

  szrDesignACVariablesField      = szrBase + 228;  {2}
  szrDesignACVariablesExpression = szrBase + 229;  {2}
  szrDesignACVariablesTotal      = szrBase + 230;  {2}
  szrDesignACVariablesDelete     = szrBase + 231;  {2}

  // Search
  szrSearchDlgCaption            = szrBase + 232;  {2K}
  szrSearchDlgInvitation         = szrBase + 233;  {2K}
  szrSearchDlgIgnoreCase         = szrBase + 234;  {2K}
  szrSearchDlgFromCurrent        = szrBase + 235;  {2K}
  szrSearchDlgStartSearch        = szrBase + 236;  {2K}
  szrSearchDlgContinueSearch     = szrBase + 237;  {2K}
  szrSearchNotFound              = szrBase + 238;  {2K}

implementation

end.

