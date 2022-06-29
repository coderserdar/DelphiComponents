unit FxConsts;

{$R FxConsts.RES}

interface

const
  { DecisionCube Dialog help contexts }
  hcDDecisionCubeEditor = 30170;
  hcDDecisionCubeDimensions = 30171;
  hcDDecisionCubeMemoryControl = 30172;
  hcDDecisionCubeDesignerOptions = 30173;
  hcDDecisionCubeLimits = 30174;
  hcDDecisionQueryEditor = 30180;
  hcDDecisionQueryDimensions = 30181;
  hcDDecisionQuerySQLEditor = 30182;

resourcestring
  sRowError             = 'row error';
  // FxArray
  // FxBin
  // FxCache
  // FxMap
  { Strings used in the Pivot }
  sAllValues            = 'All Values';
  sMovetoRow            = 'Move to Row Area';
  sMovetoCol            = 'Move to Column Area';
  sMakeDimOpen          = 'Open Dimension';
  sDrilled              = 'Drilled In';
  sCouldNotOpen         = 'The information requested could not be loaded. ';
  sTemplatePrefix       = 'Template: ';
  { Strings used in the CrossTabGrid }
  sGridCellError        = '[Error]';
  sTotalCaption         = 'Sum';
  sActivateLabel        = 'Inactive Dimensions';
  sRowCaption           = 'R';
  sColCaption           = 'C';
  sCaptionMenu1         = 'Display Data and Subtotals';
  sCaptionMenu2         = 'Display Data Only';
  sCaptionMenu3         = 'Display Subtotals Only';
  sDrillIn              = 'Drill in to this value';
  sGridMenu1            = 'Subtotals on/off';
  sGridMenu2            = 'Decision Cube Editor..';
  sGridMenu3            = 'Decision Query Editor..';
  sGridMenu4            = 'Show Detail Records..';

  { Strings used in mxarrays }
  sRowOutOfRange        = 'Row index out of range : %d';
  sColOutOfRange        = 'Column index out of range : %d';
  sQryNotInitialized    = 'Query could not be run. Check that the query, SQL text, and Database are correct.';
  sGeneralArrayError    = 'General array error.';

  {String used in the DecisionSource}
  sDimIndexError        = 'Недопустимый индекс массива';

  {String used in the DecisionCube}
  sIllegalValueForBin   = 'Initial Value is not legal for this type of Grouping';
  sIllegalDimMap        = 'Dimension Map is not the correct size';
  sNotAvailable         = 'Not Available';
  sGetValueCounts       = 'Information required to do Maximum Cell limit is not current.  Do you want to fetch it now?';
  sDateBinningNotAllowed = 'Date grouping is not allowed for fields of this type';

  { Strings use in Query UI}
  sNoDataSet            = 'Data set property is not assigned';
  sUnknownDims          = 'The dimension types for this dataset cannot be determined automatically.  You must map the fields to dimensions or summaries with the Decision Cube Editor';
  sGroupsMissing        = 'All dimension fields must be grouped. ';
  sDecisionQueryError   = 'The query may be incorrectly defined, or you may need to map its fields to active dimensions or summaries with the Decision Cube Editor';
  sDataSetError         = 'The dataset may be incorrectly defined, or you may need to map its fields to active dimensions or summaries with the Decision Cube Editor';
  sQueryError           = 'The query may be incorrectly defined, or you may need to map its fields to active dimensions or summaries with the Decision Cube Editor';
  sCountStar            = 'COUNTALL';  { INTL must be in upper case }
  sAddAvgWarning        = 'Average is calculated using sum and count summaries for each field. The necessary summaries have been added.';
  sAddAvgStarWarning    = 'Average is calculated using a field sum and count(*).  The necessary summaries have been added.';

  { Query UI Designer strings}
  sQueryLegal           = 'Query is legal.';
  sAddFieldExists       = ' is already in the query';
  sAggTypeNotAllowed    = ' is not an allowed summary type';
  sDimTypeNotAllowed    = ' is not an allowed dimension type';
  sAverageRequires      = 'Average summaries use Sum and Count';
  sWantToExit           = 'Do you still want to Exit?';
  sQueryIllegal         = 'The query you have created is not legal.';
  sQueryEditIllegal     = 'The query you have entered is not legal.  Please correct it before continuing.';
  sRemoveFieldError     = 'Could not remove the field';
  sAllFields            = 'All Fields';
  sQueryFields          = 'Query Fields';
  sEditDone             = '&Edit Done';
  sEditQuery            = '&Edit Query';

  { Used by the query parser}
  sQParseRemovedField   = 'One or more fields of a type which cannot be tabulated were removed from the query.';

  { used by mxstore }
  sCubeLimitsExceeded   = 'Decision Cube size excedes limits';
  sUserCanceled         = 'User canceled DecisionCube population.';
  sCreateDerivedSummaryError = 'Unable to create derived summary.';
  sBinTypeMismatch      = 'The bin type does not match the fieldtype.';
  sFatalCacheError      = 'Fatal error in cache: code: %d';
  sBuildingDataStore    = 'Building data store...';
  sUnknownException     = 'Unknown Exception';
  {String used in MXtables and mxcommon and mxqparse}
  sSumLabel             = 'Sum of %s';
  sCountLabel           = 'Count of %s';
  sMaxLabel             = 'Maximum of %s';
  sMinLabel             = 'Minimum of %s';
  sAverageLabel         = 'Average of %s';
  sVarLabel             = 'Variance of %s';
  sSDLabel              = 'Standard Deviation of %s';
  sAggLabel             = 'Summary of %s';

  sSelectFromError      = 'Query lacks a Select/From clause.';
  sArgumentExpected     = 'No argument provided for an operator or summary';
  sGroupOnExpressionError = 'An expression cannot be used for a grouping field';

  sIDAPILangID          = '0009';

implementation

end.
