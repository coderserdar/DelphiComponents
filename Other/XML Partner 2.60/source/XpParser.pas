(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: XPParser.PAS                              *}
{*********************************************************}
{* XMLPartner: XML parser code                           *}
{*********************************************************}

{$I XpDefine.inc}

unit XpParser;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
  Types,
{$ENDIF}
  SysUtils,
  Classes,
  XpBase,
  XpChrFlt;

type
  StringIds = array[0..1] of DOMString;

{== Event types ======================================================}
  TXpDTDAttDefinitionEvent = procedure(oOwner       : TObject;
                                       sName        : DOMString;
                                       wAttrType    : Integer;
                                       oEnumeration : TStringList;
                                       wValueType   : Integer;
                                       sValue       : DOMString) of object;
  TXpDTDConditionalStartEvent = procedure(oOwner : TObject;
                                          wType  : Integer) of object;
  TXpDTDElementStartEvent = procedure(oOwner       : TObject;
                                      sName        : DOMString;
                                      wContentType : Integer) of object;
  TXpDTDElementEndEvent = procedure(oOwner : TObject;
                                    sName  : DOMString) of object;
  TXpDTDElementContentEvent = procedure(oOwner       : TObject;
                                        sName        : DOMString;
                                        wOccurrence,
                                        wRelation    : Integer) of object;
  TXpDTDElementGroupEvent = procedure(oOwner      : TObject;
                                      wOccurrence,
                                      wRelation   : Integer) of object;
  TXpDTDEntityInternalEvent = procedure(oOwner  : TObject;
                                        bPeFlag : Boolean;
                                        sName,
                                        sValue  : DOMString) of object;
  TXpDTDEntityExternalEvent = procedure(oOwner        : TObject;
                                        bPeFlag       : Boolean;
                                        sName,
                                        sPublicId,
                                        sSystemId,
                                        sNotationName : DOMString) of object;
  TXpDTDNotationEvent = procedure(oOwner        : TObject;
                                  sNotationName,
                                  sPublicId,
                                  sSystemId     : DOMString) of object;
  TXpDTDExternalStartEvent = procedure(oOwner : TObject;
                                   var sFile  : DOMString) of object;
  TXpDocTypeDeclEvent = procedure(oOwner : TObject;
                                  sDecl,
                                  sId0,
                                  sId1   : DOMString) of object;
  TXpValueEvent = procedure(oOwner : TObject;
                            sValue : DOMString) of object;
  TXpAttributeEvent = procedure(oOwner     : TObject;
                                sName,
                                sValue     : DOMString;
                                bSpecified : Boolean) of object;
  TXpProcessInstrEvent = procedure(oOwner : TObject;
                                   sName,
                                   sValue : DOMString) of object;
  TXpResolveEvent = procedure(oOwner     : TObject;
                        const sName,                                   {!!.52}
                              sPublicId,
                              sSystemId  : DOMString;
                          var sValue     : DOMString) of object;
  TXpNonXMLEntityEvent = procedure(oOwner        : TObject;
                                   sEntityName,
                                   sPublicId,
                                   sSystemId,
                                   sNotationName : DOMString) of object;
  TXpPreserveSpaceEvent = procedure(oOwner       : TObject;
                                    sElementName : DOMString;
                                var bPreserve    : Boolean) of object;
{== Class types ======================================================}
{Begin !!.52}
  TXpDirContext = class(TXpBaseDirContext)
  protected
    procedure Evaluate(const sSrcName : string;
                             oContext : TXpBaseDirContext); override;
  public
    constructor Make(oCurrentContext : TXpBaseDirContext;
               const sSrcName : string); override;
  end;
{End !!.52}

  TXpParser = class(
{$IFDEF DACTIVEX}
  TCustomControl
{$ELSE}
  TXpComponent
{$ENDIF}
  )
  protected                                                            {!!.52}
    { Private declarations }
    FAttrEnum : TStringList;
    FAttributeType : TStringList;
    FBufferSize : Integer;
    FCDATA : Boolean;
    FContext : Integer;
    FCurrentElement : DOMString;
    FCurrentElementContent : Integer;
//    FCurrentPath : string;                                           {!!.52}
    FDataBuffer : DOMString;
    FDataBufferPos : Integer;    { Where last character was inserted.} {!!.55}
    FDataBufferSize : Integer;   { Current capacity }                  {!!.55}
    FDocLocation : string;                                             {!!.52}
    FDocName : string;                                                 {!!.52}
    FDocStack : TList;
    FElementInfo : TStringList;
    FEntityInfo : TStringList;
    FErrors : TStringList;
    FFilter : TXpInCharFilter;
    FInCharSet : TXpCharEncoding;
    FNormalizeData : Boolean;
    FNotationInfo : TStringList;
    FOnAttribute : TXpAttributeEvent;
    FOnCDATASection : TXpValueEvent;
    FOnCharData : TXpValueEvent;
    FOnComment : TXpValueEvent;
    FOnDocTypeDecl : TXpDocTypeDeclEvent;
    FOnDTDAttDefinition : TXpDTDAttDefinitionEvent;
    FOnDTDAttlistEnd : TNotifyEvent;
    FOnDTDAttlistStart : TXpValueEvent;
    FOnDTDConditionalEnd : TNotifyEvent;
    FOnDTDConditionalStart : TXpDTDConditionalStartEvent;
    FOnDTDElementContent : TXpDTDElementContentEvent;
    FOnDTDElementEnd : TXpDTDElementEndEvent;
    FOnDTDElementGroupEnd : TXpDTDElementGroupEvent;
    FOnDTDElementGroupStart : TNotifyEvent;
    FOnDTDElementStart : TXpDTDElementStartEvent;
    FOnDTDEnd : TNotifyEvent;
    FOnDTDEntityExternal : TXpDTDEntityExternalEvent;
    FOnDTDEntityInternal : TXpDTDEntityInternalEvent;
    FOnDTDExternalEnd : TNotifyEvent;
    FOnDTDExternalLoaded : TNotifyEvent;                               {!!.53}
    FOnDTDExternalStart : TXpDTDExternalStartEvent;
    FOnDTDNotation : TXpDTDNotationEvent;
    FOnDTDStart : TXpDocTypeDeclEvent;
    FOnEndDocument : TNotifyEvent;
    FOnEndElement : TXpValueEvent;
    FOnIgnorableWhitespace : TXpValueEvent;
    FOnNonXMLEntity : TXpNonXMLEntityEvent;
    FOnPreserveSpace : TXpPreserveSpaceEvent;
    FOnProcessingInstruction : TXpProcessInstrEvent;
    FOnResolveEntity : TXpResolveEvent;
    FOnStartDocument : TNotifyEvent;
    FOnStartElement : TXpValueEvent;
    FPassword : string;
    FPreserve : Boolean;
    FRaiseErrors : Boolean;
    FTagAttributes : TStringList;
    FTempFiles : TStringList;
    FUserName : string;
    FIsStandAlone : Boolean;
    FHasExternals : Boolean;
    FXMLDecParsed : Boolean;

    {$IFDEF XpUseInet}
    procedure AddTemporaryFile(const aFilename : string);
    {$ENDIF}
    procedure Cleanup;
    procedure CheckParamEntityNesting(const aString : DOMString);      {!!.51}
    procedure DataBufferAppend(const sVal : DOMString);
    procedure DataBufferFlush;
    procedure DataBufferInitialize;                                    {!!.55}
    procedure DataBufferNormalize;
    function DataBufferToString(aClearBuffer : Boolean) : DOMString;   {!!.55}
    function DeclaredAttributes(const sName : DOMString;               {!!.52}
                                      aIdx  : Integer) : TStringList;  {!!.52}
    function GetAttributeDefaultValueType(const sElemName,             {!!.52}
                                                sAttrName : DOMString)
                                                          : Integer;
    function GetAttributeExpandedValue(const sElemName,                {!!.52 - Start}
                                             sAttrName : DOMString;
                                             aIdx      : Integer)      {!!.52 - End}
                                                       : DOMString;
    function GetElementContentType(const sName : DOMString;            {!!.52}
                                         aIdx  : Integer) : Integer;   {!!.52}
    function GetElementIndexOf(const sElemName : DOMString) : Integer; {!!.52}
    function GetEntityIndexOf(const sEntityName : DOMString;           {!!.52}
                                    aPEAllowed  : Boolean) : Integer; virtual; {!!.57}
    function GetEntityNotationName(const sEntityName : DOMString)
                                                     : DOMString; virtual; {!!.57}
    function GetEntityPublicId(const sEntityName : DOMString)
                                                 : DOMString; virtual; {!!.57}
    function GetEntitySystemId(const sEntityName : DOMString)
                                                 : DOMString; virtual; {!!.57}
    function GetEntityType(const sEntityName : DOMString;
                                 aPEAllowed   : Boolean) : Integer; virtual; {!!.57}
    function GetEntityValue(const sEntityName : DOMString;
                                  aPEAllowed  : Boolean) : DOMString; virtual; {!!.57}
    function GetErrorCount : Integer;
    function GetExternalTextEntityValue(const sName,                   {!!.52}
                                              sPublicId : DOMString;   {!!.52}
                                              sSystemId : DOMString)
                                                        : DOMString; virtual; {!!.57}
    function GetInCharSet : TXpCharEncoding;                           {!!.51}
    procedure Initialize; virtual;                                     {!!.57}
    function IsEndDocument : Boolean;
    function IsWhitespace(const cVal : DOMChar) : Boolean;             {!!.52}
    function LoadDataSource(oCurContext : TXpBaseDirContext;           {!!.52}
                            const sSrcName   : string;          {!!.52}{!!.57}
                      const sUserName,                                 {!!.52}
                            sPassword  : string;
                            oErrors    : TStringList) : Boolean;
    procedure ParseAttDef(const sElemName : DOMString);
    procedure ParseAttlistDecl;
    function ParseAttribute(const sName : DOMString) : DOMString;
    function ParseEntityRef(bPEAllowed : Boolean) : DOMString;
    procedure ParseCDSect;
    function  ParseCharRef : DOMChar;                                  {!!.52}
    procedure ParseComment;
    procedure ParseConditionalSect;
    procedure ParseContent;
    procedure ParseContentSpec(const sName       : DOMString;
                                     aIsInternal : Boolean);
    procedure ParseCp;
    procedure ParseDefault(const sElemName,                            {!!.52}
                                 sName     : DOMString;
                                 wType     : Integer;
                           const sEnum     : DOMString);               {!!.52}
    procedure ParseDocTypeDecl;
    procedure ParseDocument;
    procedure ParseEndTag;
    procedure ParseEq;
    procedure ParseElement;
    procedure ParseElementDecl(aIsInternal : Boolean);
    procedure ParseElements;
    procedure ParseEntityDecl;
    procedure ParseEnumeration;
    procedure ParseIgnore;                                             {!!.52}
    function ParseInContext(oContext : TXpBaseDirContext;              {!!.52}
                      const sSource : string) : Boolean;               {!!.52}
    procedure ParseMarkupDecl(aIsInternal : Boolean);
    procedure ParseMisc;
    procedure ParseMixed(var wOccur : Integer);                        {!!.51}
    procedure ParseNotationDecl;
    procedure ParseNotationType;
    function ParseParameterEntityRef(aPEAllowed : Boolean;
                                     bSkip      : Boolean
    {$IFNDEF VER100}
     = False
     {$ENDIF}
     ) : DOMString;
    procedure ParsePCData(aInEntityRef : Boolean);
    procedure ParsePI;
    function ParsePIEx : Boolean;
      { Returns true if an XML declaration was found }
{Begin !!.52}
    procedure ParsePrim;
      { Common parse routine called from ParseDatasource, ParseMemory, &
        ParseStream. }
{End !!.52}
    procedure ParseProlog;
    procedure ParseUntil(const S : array of Longint);
    procedure ParseWhitespace;
    procedure ParseXMLDeclaration;
    procedure PopDocument;
    procedure PushDocument;
    procedure PushString(const sVal : DOMString);
    function ReadAttType : Integer;
    function ReadChar(const UpdatePos : Boolean) : DOMChar;            {!!.52}
    procedure ReadExternalIds(bInNotation : Boolean;
                          var sIds        : StringIds);
    function ReadLiteral(wFlags    : Integer;
                     var HasEntRef : Boolean) : DOMString;
    function ReadNameToken(aValFirst : Boolean) : DOMString;
    procedure Require(const S : array of Longint);
    procedure RequireWhitespace;
    procedure SetAttribute(const sElemName,                            {!!.52}
                                 sName      : DOMString;
                                 wType      : Integer;
                           const sEnum,                                {!!.52}
                                 sValue     : DOMString;
                                 wValueType : Integer);
//    procedure SetDirContext(oContext : TXpBaseDirContext);           {!!.52}{Deleted !!.54}
    function SetElement(const sName         : DOMString;               {!!.55}
                              wType         : Integer;
                        const sContentModel : DOMString)
                                            : Integer;                 {!!.55}
    procedure SetEntity(const sEntityName   : DOMString;
                              wClass        : Integer;
                        const sPublicId,
                              sSystemId,
                              sValue,
                              sNotationName : DOMString;
                              aIsPE         : Boolean);
    procedure SetExternalDataEntity(const sName,
                                          sPublicId,
                                          sSystemId,
                                          sNotationName : DOMString);
    procedure SetExternalTextEntity(const sName,
                                          sPublicId,
                                          sSystemId : DOMString);
    procedure SetInternalEntity(const sName, sValue : DOMString;
                                      aIsPE         : Boolean);
    procedure SetNotation(const sNotationName, sPublicId, sSystemId
                                                          : DOMString);
    procedure SkipChar;                                                {!!.51}
    procedure SkipWhitespace(aNextDoc : Boolean);                      {!!.51}
    function TryRead(const S : array of Longint) : Boolean;
    procedure ValidateAttribute(const aValue    : DOMString;
                                      HasEntRef : Boolean);
    procedure ValidateCData(const CDATA : DOMString);
    procedure ValidateElementName(const aName : DOMString);
    procedure ValidateEncName(const aValue : string);
    procedure ValidateEntityValue(const aValue   : DOMString;          {!!.52}
                                        aQuoteCh : DOMChar);           {!!.52}
    procedure ValidatePCData(const aString      : DOMString;
                                   aInEntityRef : Boolean);
    procedure ValidatePublicID(const aString : DOMString);
    procedure ValidateVersNum(const aString : string);

{$IFDEF DACTIVEX}
    function GetHeight : Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value : Integer);
    procedure SetWidth(Value : Integer);
{$ENDIF}

  protected
    { Protected declarations }
    property OnDTDExternalLoaded : TNotifyEvent
      read FOnDTDExternalLoaded
      write FOnDTDExternalLoaded;

    property OnIgnorableWhitespace : TXpValueEvent
      read FOnIgnorableWhitespace
      write FOnIgnorableWhitespace;

  public
    { Public declarations }
    constructor Create(oOwner : TComponent); override;
    destructor Destroy; override;

{$IFDEF DACTIVEX}
    procedure SetBounds(aLeft,
                        aTop,
                        aWidth,
                        aHeight : Integer); override;
{$ENDIF}

    function GetErrorMsg(wIdx : Integer) : DOMString;
    function ParseDataSource(const sSource : string) : Boolean;
    function ParseMemory(var aBuffer; aSize : Longint) : Boolean;
    function ParseStream(oStream : TStream) : Boolean;                 {!!.52}

    property ErrorCount : Integer
      read GetErrorCount;

    property Errors : TStringList
      read FErrors;

    property InCharSet : TXpCharEncoding
      read GetInCharSet;                                               {!!.51}

    property IsStandAlone : Boolean
      read FIsStandAlone;

    property HasExternals : Boolean
      read FHasExternals;

  published
    { Published declarations }
    property BufferSize : Integer
      read FBufferSize
      write FBufferSize
      default 8192;

{Begin !!.52}
    property DocLocation : string
      read FDocLocation;

    property DocName : string
      read FDocName;
{End !!.52}

    property NormalizeData : Boolean
      read FNormalizeData
      write FNormalizeData
      default True;

    property RaiseErrors : Boolean
      read FRaiseErrors
      write FRaiseErrors
      default False;

{$IFDEF DACTIVEX}
    property Height : Integer
      read GetHeight
      write SetHeight;

    property Width : Integer
      read GetWidth
      write SetWidth;
{$ENDIF}

    property Password : string
      read FPassword
      write FPassword;

    property UserName : string
      read FUserName
      write FUserName;

    { Published Events}
    property OnAttribute : TXpAttributeEvent
      read FOnAttribute
      write FOnAttribute;

    property OnCDATASection : TXpValueEvent
      read FOnCDATASection
      write FOnCDATASection;

    property OnCharData : TXpValueEvent
      read FOnCharData
      write FOnCharData;

    property OnComment : TXpValueEvent
      read FOnComment
      write FOnComment;

    property OnDocTypeDecl : TXpDocTypeDeclEvent
      read FOnDocTypeDecl
      write FOnDocTypeDecl;

    property OnDTDAttDefinition : TXpDTDAttDefinitionEvent
      read FOnDTDAttDefinition
      write FOnDTDAttDefinition;

    property OnDTDAttlistEnd : TNotifyEvent
      read FOnDTDAttlistEnd
      write FOnDTDAttlistEnd;

    property OnDTDAttlistStart : TXpValueEvent
      read FOnDTDAttlistStart
      write FOnDTDAttlistStart;

    property OnDTDConditionalEnd : TNotifyEvent
      read FOnDTDConditionalEnd
      write FOnDTDConditionalEnd;

    property OnDTDConditionalStart : TXpDTDConditionalStartEvent
      read FOnDTDConditionalStart
      write FOnDTDConditionalStart;

    property OnDTDElementContent : TXpDTDElementContentEvent
      read FOnDTDElementContent
      write FOnDTDElementContent;

    property OnDTDElementEnd : TXpDTDElementEndEvent
      read FOnDTDElementEnd
      write FOnDTDElementEnd;

    property OnDTDElementGroupEnd : TXpDTDElementGroupEvent
      read FOnDTDElementGroupEnd
      write FOnDTDElementGroupEnd;

    property OnDTDElementGroupStart : TNotifyEvent
      read FOnDTDElementGroupStart
      write FOnDTDElementGroupStart;

    property OnDTDElementStart : TXpDTDElementStartEvent
      read FOnDTDElementStart
      write FOnDTDElementStart;

    property OnDTDEnd : TNotifyEvent
      read FOnDTDEnd
      write FOnDTDEnd;

    property OnDTDEntityExternal : TXpDTDEntityExternalEvent
      read FOnDTDEntityExternal
      write FOnDTDEntityExternal;

    property OnDTDEntityInternal : TXpDTDEntityInternalEvent
      read FOnDTDEntityInternal
      write FOnDTDEntityInternal;

    property OnDTDExternalEnd : TNotifyEvent
      read FOnDTDExternalEnd
      write FOnDTDExternalEnd;

    property OnDTDExternalStart : TXpDTDExternalStartEvent
      read FOnDTDExternalStart
      write FOnDTDExternalStart;

    property OnDTDNotation : TXpDTDNotationEvent
      read FOnDTDNotation
      write FOnDTDNotation;

    property OnDTDStart : TXpDocTypeDeclEvent
      read FOnDTDStart
      write FOnDTDStart;

    property OnEndDocument : TNotifyEvent
      read FOnEndDocument
      write FOnEndDocument;

    property OnEndElement : TXpValueEvent
      read FOnEndElement
      write FOnEndElement;

    property OnNonXMLEntity : TXpNonXMLEntityEvent
      read FOnNonXMLEntity
      write FOnNonXMLEntity;

    property OnPreserveSpace : TXpPreserveSpaceEvent
      read FOnPreserveSpace
      write FOnPreserveSpace;

    property OnProcessingInstruction : TXpProcessInstrEvent
      read FOnProcessingInstruction
      write FOnProcessingInstruction;

    property OnResolveEntity : TXpResolveEvent
      read FOnResolveEntity
      write FOnResolveEntity;

    property OnStartDocument : TNotifyEvent
      read FOnStartDocument
      write FOnStartDocument;

    property OnStartElement : TXpValueEvent
      read FOnStartElement
      write FOnStartElement;
  end;

implementation

{.$R *.RES}

uses
{$IFDEF XpUseInet}
  XpInet,
{$ENDIF}
{$IFDEF XPTrialRun}
{$IFDEF UsingCLX}
  XpQTrial,
{$ELSE}
  XpTrial,
{$ENDIF}
{$ENDIF}
  XpExcept;


{== TXpEntityInfo ====================================================}
type
  TXpEntityInfo = class(TObject)
  private
    FEntityClass  : Integer;
    FIsPE         : Boolean;
    FPublicId     : DOMString;
    FSystemId     : DOMString;
    FValue        : DOMString;
    FNotationName : DOMString;
  public
    property EntityClass : Integer
      read FEntityClass
      write FEntityClass;

    property IsParameterEntity : Boolean
      read FIsPE
      write FIsPE;

    property NotationName : DOMString
      read FNotationName
      write FNotationName;

    property PublicId : DOMString
      read FPublicId
      write FPublicId;

    property SystemId : DOMString
      read FSystemId
      write FSystemId;

    property Value : DOMString
      read FValue
      write FValue;
  end;
{== TXpNotationInfo ==================================================}
  TXpNotationInfo = class(TObject)
  private
    FPublicId : DOMString;
    FSystemId : DOMString;
  public
    property PublicId : DOMString
      read FPublicId
      write FPublicId;

    property SystemId : DOMString
      read FSystemId
      write FSystemId;
  end;

{== TXpAttributeInfo =================================================}
  TXpAttributeInfo = class(TObject)
  private
    FType      : Integer;
    FValue     : DOMString;
    FValueType : Integer;
    FEnum      : DOMString;
    FLookup    : DOMString;
  public
    property AttrType : Integer
      read FType
      write FType;

    property Enum : DOMString
      read FEnum
      write FEnum;

    property Lookup : DOMString
      read FLookup
      write FLookup;

    property Value : DOMString
      read FValue
      write FValue;

    property ValueType : Integer
      read FValueType
      write FValueType;
  end;

{== TXpElementInfo ===================================================}
  TXpElementInfo = class(TObject)
  private
    FAttributeList : TStringList;
    FContentType   : Integer;
    FContentModel  : DOMString;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetAttribute(const sName     : DOMString;                {!!.52}
                                 oAttrInfo : TXpAttributeInfo);

    property AttributeList : TStringList
      read FAttributeList;

    property ContentModel : DOMString
      read FContentModel
      write FContentModel;

    property ContentType : Integer
      read FContentType
      write FContentType;
  end;
{=====================================================================}

{=== TXpElementInfo ==================================================}
constructor TXpElementInfo.Create;
begin
  inherited Create;
  FAttributeList := nil;
  FContentModel := '';
  FContentType := 0;
end;
{--------}
destructor TXpElementInfo.Destroy;
var
  i : Integer;
begin
  if FAttributeList <> nil then begin
    for i := 0 to FAttributeList.Count - 1 do
      TXpAttributeInfo(FAttributeList.Objects[i]).Free;
    FAttributeList.Free;
  end;
  inherited Destroy;
end;
{--------}
procedure TXpElementInfo.SetAttribute(const sName     : DOMString;     {!!.52}
                                            oAttrInfo : TXpAttributeInfo);
var
  wIdx : Integer;
begin
  if FAttributeList = nil then begin                                   {!!.52 - Start}
    FAttributeList := TStringList.Create;
    FAttributeList.Sorted := True;
    wIdx := -1
{Begin !!.55}
  end else begin
    wIdx := FAttributeList.IndexOf(sName);                             {!!.52 - End}
    { Verify names are identical (i.e., case-sensitive). }
    if (wIdx >= 0) and (FAttributeList[wIdx] <> sName) then
      wIdx := -1;
  end;
{End !!.55}

  if wIdx < 0 then
    FAttributeList.AddObject(sName, oAttrInfo)
  else begin
    TXpAttributeInfo(FAttributeList.Objects[wIdx]).Free;
    FAttributeList.Objects[wIdx] := oAttrInfo;
  end;
end;
{=====================================================================}

{===TXpDirContext=====================================================}
procedure TXpDirContext.Evaluate(const sSrcName : string;
                                       oContext : TXpBaseDirContext);
var
  wLen,
  wPos     : Integer;
begin
  { Assumption: http://, fttp://, and file:// removed prior to
    this method being called. }
  case FContextType of
    xpdcLocal :
      begin
        FDir := ExtractFilePath(sSrcName);
        if (FDir <> '') and (FDir[Length(FDir)] <> PathDelim) then
          FDir := FDir + PathDelim;
        if (FDir = '') and (oContext <> nil) then                      {!!.53}
          FDir := oContext.Dir;                                        {!!.53}
        FFile := ExtractFilename(sSrcName);
      end;
    xpdcHTTP, xpdcFTP :
      begin
        { Find the last forward slash in the path, if there is one. }
        wLen := Length(sSrcName);
        wPos := wLen;
        while wPos >= 1 do begin
          if sSrcName[wPos] = '/' then
            break
          else
            dec(wPos);
        end;

        { Found the last forward slash? }
        if wPos = 0 then begin
{Begin !!.57}
          { No. This is just a filename. }
          if (oContext <> nil) then
            FDir := oContext.Dir
          else
            FDir := '';
{End !!.57}
          FFile := sSrcName;
        end
        else begin
          FDir := Copy(sSrcName, 1, wPos);
          if (FDir = '') and (oContext <> nil) then                    {!!.53}
            FDir := oContext.Dir;                                      {!!.53}
          FFile := Copy(sSrcName, wPos + 1, wLen - wPos);
        end;
      end;
    xpdcInetRelative :
      begin
        { Find the last forward slash in the path, if there is one. }
        wLen := Length(sSrcName);
        wPos := wLen;
        while wPos >= 1 do begin
          if sSrcName[wPos] = '/' then
            break
          else
            dec(wPos);
        end;

        { Found the last forward slash? }
        if wPos = 0 then begin
          { No. This is just a filename. }
          FDir := '';
          FFile := sSrcName;
        end
        else begin
          FDir := Copy(sSrcName, 1, wPos);
          FFile := Copy(sSrcName, wPos + 1, wLen - wPos);
        end;
        if oContext <> nil then
          FDir := oContext.Dir + FDir;
      end;
    xpdcLocalRelative :
      begin
        { Find the last backslash in the path, if there is one. }
        wLen := Length(sSrcName);
        wPos := wLen;
        while wPos >= 1 do begin
          if sSrcName[wPos] = PathDelim then
            break
          else
            dec(wPos);
        end;

        { Found the last forward slash? }
        if wPos = 0 then begin
          { No. This is just a filename. }
          FDir := '';
          FFile := sSrcName;
        end
        else begin
          FDir := Copy(sSrcName, 1, wPos);
          FFile := Copy(sSrcName, wPos + 1, wLen - wPos);
        end;
        if oContext = nil then
          FDir := ExpandFilename(FDir)
        else
          FDir := oContext.Dir + FDir;
      end;
  end;  { case }
end;
{--------}
{$IFDEF LINUX}
constructor TXpDirContext.Make(oCurrentContext : TXpBaseDirContext;
                         const sSrcName : string);
var
  sTmpName : string;
begin
  inherited Create;
{$IFDEF XpUseInet}
  { Is this an absolute HTTP address? }
  if (StrLIComp(PChar(sSrcName), 'http://', 7) = 0) then begin
    { Yes. Set the context and grab the location. }
    FContextType := xpdcHTTP;
    Evaluate(Copy(sSrcName, 8, Length(sSrcName) - 7), oCurrentContext);
  end  { if http }
  else if (StrLIComp(PChar(sSrcName), 'ftp://', 6) = 0) then begin
    FContextType := xpdcFTP;
    Evaluate(Copy(sSrcName, 7, Length(sSrcName) - 6), oCurrentContext);
  end  { if ftp }
  else
{$ENDIF}
  begin
    { Prefixed with 'file://'?}
    if StrLIComp(PChar(sSrcName), 'file://', 7) = 0 then
      { Yes. Remove. }
      sTmpName := Copy(sSrcName, 8, Length(sSrcName) - 7)
    else
      sTmpName := sSrcName;

    { Absolute URL (e.g., /mfw/isopub.ent) ? }
    if sTmpName[1] = '/' then begin
      { Yes. Context is local? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate as local path. }
        FContextType := xpdcLocal;
        Evaluate(sTmpName, oCurrentContext);
      end
      else begin
        { Otherwise use current internet context. }
        FContextType := oCurrentContext.ContextType;
        Evaluate(sTmpName, oCurrentContext);
      end;
    end
    else
      { Relative URL (e.g., './xxx', '../xxx', 'somedir/xxx/)? }
    if (sTmpName[1] = '.') or (Pos('/', sTmpName)> 0) then begin
      { Yes. Is current context set to local? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate as local relative. }
        FContextType := xpdcLocalRelative;
        Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        FContextType := xpdcLocal;
      end
      else begin
        { No. Internet. Use as is. }
        FContextType := xpdcInetRelative;
        Evaluate(sTmpName, oCurrentContext);
        FContextType := oCurrentContext.ContextType;
      end;
    end  { if relative URL }
    { Relative DOS path? }
    else if (sTmpName[1] = '.') or
            ((Pos(':', sTmpName) = 0) and
             (Pos('\', sTmpName) > 1)) then begin
      { Yes. Currently in local or internet context? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate as local path. }
        FContextType := xpdcLocalRelative;
        Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        FContextType := xpdcLocal;
      end
      else begin
        { No. Evaluate as internet path. }
        FContextType := xpdcInetRelative;
        Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        FContextType := oCurrentContext.ContextType;
      end;
    end  { if relative DOS path }
      { Absolute DOS path (e.g., 'c:\test\ent.xml', '\delta\test.xml')? }
    else begin
      { Matches the current context? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate. }
        FContextType := xpdcLocal;
        Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
      end
      else begin
        { No. Contains a drive specifier? }
        if sTmpName[2] = ':' then
          { Yes. Raise an error. }
          raise EXpException.Create(Format(sMixedInetLocalPath,
                                           [QuotedStr(FDir),
                                            oCurrentContext.Dir]))
        else begin
          { Replace '\' with '/' and evaluate as internet path. }
          FContextType := oCurrentContext.ContextType;
          Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        end;
      end;
    end;
  end;
end;
{$ENDIF}
{--------}
{$IFDEF MSWINDOWS}
constructor TXpDirContext.Make(oCurrentContext : TXpBaseDirContext;
                         const sSrcName : string);
var
  sTmpName : string;
begin
  inherited Create;
  if sSrcName = '' then                                                {!!.53}
    Exit;                                                              {!!.53}
{$IFDEF XpUseInet}
  { Is this an absolute HTTP address? }
  if (StrLIComp(PChar(sSrcName), 'http://', 7) = 0) then begin
    { Yes. Set the context and grab the location. }
    FContextType := xpdcHTTP;
    Evaluate(Copy(sSrcName, 8, Length(sSrcName) - 7), oCurrentContext);
  end  { if http }
  else if (StrLIComp(PChar(sSrcName), 'ftp://', 6) = 0) then begin
    FContextType := xpdcFTP;
    Evaluate(Copy(sSrcName, 7, Length(sSrcName) - 6), oCurrentContext);
  end  { if ftp }
  else
{$ENDIF}
  begin
    { Prefixed with 'file://'?}
    if StrLIComp(PChar(sSrcName), 'file://', 7) = 0 then
      { Yes. Remove. }
      sTmpName := Copy(sSrcName, 8, Length(sSrcName) - 7)
    else
      sTmpName := sSrcName;

    { Absolute URL (e.g., /mfw/isopub.ent)? }
    if sTmpName[1] = '/' then begin
      { Yes. Context is local? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Convert to local path. }
        FContextType := xpdcLocal;
        Evaluate(XpStringReplaceAll(sTmpName, '/', '\'), oCurrentContext);
      end
      else begin
        { Otherwise use current internet context. }
        FContextType := oCurrentContext.ContextType;
        Evaluate(sTmpName, oCurrentContext)
      end;
    end
    else
    { Relative URL (e.g., './xxx', '../xxx', 'somedir/xxx')? }
    if (sTmpName[1] = '.') or (Pos('/', sTmpName) > 0) then begin
      { Yes. Is current context set to local? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Replace '/' with '\'. }
        FContextType := xpdcLocalRelative;
        Evaluate(XpStringReplaceAll(sTmpName, '/', '\'), oCurrentContext);
        FContextType := xpdcLocal;
      end
      else begin
        { No. Internet. Use as is. }
        FContextType := xpdcInetRelative;
        Evaluate(sTmpName, oCurrentContext);
        FContextType := oCurrentContext.ContextType;
      end;
    end { if relative URL}
    { Relative DOS path? }
    else if (XpIsRelativePath(sTmpName)) then begin                    {!!.57}
      { Yes. Currently in local or internet context? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate as local path. }
        FContextType := xpdcLocalRelative;
        Evaluate(sTmpName, oCurrentContext);
        FContextType := xpdcLocal;
      end
      else begin
        { No. Evaluate as internet path. }
        FContextType := xpdcInetRelative;
        Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        FContextType := oCurrentContext.ContextType;
      end;
    end
      { Absolute DOS path (e.g., 'c:\test\ent.xml', '\delta\test.xml')? }
    else begin
      { Matches the current context? }
      if (oCurrentContext = nil) or
         (oCurrentContext.ContextType = xpdcLocal) then begin
        { Yes. Evaluate. }
        FContextType := xpdcLocal;
        Evaluate(sTmpName, oCurrentContext);
      end
      else begin
        { No. Contains a drive specifier? }
        if sTmpName[2] = ':' then
          { Yes. Raise an error. }
          raise EXpException.Create(Format(sMixedInetLocalPath,
                                           [QuotedStr(FDir),
                                            oCurrentContext.Dir]))
        else begin
          { Replace '\' with '/' and evaluate as internet path. }
          FContextType := oCurrentContext.ContextType;
          Evaluate(XpStringReplaceAll(sTmpName, '\', '/'), oCurrentContext);
        end;
      end;
    end;  { local path }
  end;
end;
{$ENDIF}
{=====================================================================}

{=== TXpParser =======================================================}
constructor TXpParser.Create(oOwner : TComponent);
begin
  inherited Create(oOwner);

  {$IFDEF XPTrialRun}
  _CC_;
  _VC_;
  {$ENDIF}

  FErrors := TStringList.Create;
  FAttributeType := TStringList.Create;
  FAttributeType.AddObject('CDATA', Pointer(ATTRIBUTE_CDATA));
  FAttributeType.AddObject('ID', Pointer(ATTRIBUTE_ID));
  FAttributeType.AddObject('IDREF', Pointer(ATTRIBUTE_IDREF));
  FAttributeType.AddObject('IDREFS', Pointer(ATTRIBUTE_IDREFS));
  FAttributeType.AddObject('ENTITY', Pointer(ATTRIBUTE_ENTITY));
  FAttributeType.AddObject('ENTITIES', Pointer(ATTRIBUTE_ENTITIES));
  FAttributeType.AddObject('NMTOKEN', Pointer(ATTRIBUTE_NMTOKEN));
  FAttributeType.AddObject('NMTOKENS', Pointer(ATTRIBUTE_NMTOKENS));
  FAttributeType.AddObject('NOTATION', Pointer(ATTRIBUTE_NOTATION));
  FElementInfo := TStringList.Create;
  FElementInfo.Sorted := True;                                         {!!.52}
  FEntityInfo := TStringList.Create;
  FInCharSet := ceUnknown;                                             {!!.51}
  FNotationInfo := TStringList.Create;
  FNotationInfo.Sorted := true;
  FNotationInfo.Duplicates := dupIgnore;
  FTagAttributes := TStringList.Create;
  FAttrEnum := TStringList.Create;
  FDocStack := TList.Create;
  FNormalizeData := True;
  FCDATA := False;
  FPreserve := False;
  FRaiseErrors := False;
  FUserName := 'anonymous';
  FPassword := 'xmlpartner@turbopower.com';
  FFilter := nil;
  FBufferSize := 8192;
//  FCurrentPath := '';                                                {!!.52}
  FTempFiles := TStringList.Create;
  FIsStandAlone := False;
  FHasExternals := False;
  FXMLDecParsed := False;

  { Initialize the data buffer.}                                       {!!.55}
  DataBufferInitialize;                                                {!!.55}
end;
{--------}
destructor TXpParser.Destroy;
var
//  TempFilter : TXpInCharFilter;                                      {Deleted !!.53}
//  TempStream : TStream;                                              {Deleted !!.52}
  i          : Integer;
begin
  Cleanup;
{Begin !!.51}
  FTagAttributes.Free;
  FNotationInfo.Free;
  FEntityInfo.Free;
  FElementInfo.Free;
  FAttributeType.Free;
  FErrors.Free;
  if Assigned(FTempFiles) then begin
    for i := 0 to Pred(FTempFiles.Count) do
      DeleteFile(FTempFiles[i]);
    FTempFiles.Free;
  end;
  FAttrEnum.Free;
{End !!.51}
{Begin !!.53 - Moved}
//  if FDocStack.Count > 0 then begin
//    for i := Pred(FDocStack.Count) to 0 do begin
//      TempFilter := FDocStack[i];
//      TempStream := TempFilter.Stream;                               {Deleted !!.52}
//      TempFilter.Free;
//      TempStream.Free;                                               {Deleted !!.52}
//      FDocStack.Delete(i);
//    end;
//  end;
{End !!.53 - Moved}
  FDocStack.Free;
  inherited Destroy;
end;
{$IFDEF XpUseInet}
{--------}
procedure TXpParser.AddTemporaryFile(const aFileName : string);
begin
  FTempFiles.Add(aFileName);
end;
{$ENDIF}
{--------}
procedure TXpParser.CheckParamEntityNesting(const aString : DOMString);{!!.51 - Start}
var
  OpenPos : Integer;
  ClosePos : Integer;
begin
  OpenPos := XpPos('(', aString);
  ClosePos := XpPos(')', aString);
  if (((OpenPos <> 0) and
       (ClosePos = 0)) or
      ((ClosePos <> 0) and
       (OpenPos = 0))) then
     raise EXpParserError.CreateError(FFilter.Line,
                                      FFilter.LinePos,
                                      FFilter.URL,
                                      sBadParamEntNesting +
                                      aString);
end;                                                                   {!!.51 - End}
{--------}
procedure TXpParser.Cleanup;
var
  TempFilter : TXpInCharFilter;                                        {!!.53}
  i : Integer;
begin
{Begin !!.53}
  if FDocStack.Count > 0 then begin
    for i := Pred(FDocStack.Count) downto 0 do begin
      TempFilter := FDocStack[i];
      TempFilter.Free;
      FDocStack.Delete(i);
    end;
  end;
{End !!.53}

  if FElementInfo <> nil then begin
    for i := 0 to FElementInfo.Count - 1 do
      TXpElementInfo(FElementInfo.Objects[i]).Free;
    FElementInfo.Clear;
  end;

  if FEntityInfo <> nil then begin
    for i := 0 to FEntityInfo.Count - 1 do
      TXpEntityInfo(FEntityInfo.Objects[i]).Free;
    FEntityInfo.Clear;
  end;

  if FNotationInfo <> nil then begin
    for i := 0 to FNotationInfo.Count - 1 do
      TXpNotationInfo(FNotationInfo.Objects[i]).Free;
    FNotationInfo.Clear;
  end;
end;
{--------}
procedure TXpParser.DataBufferAppend(const sVal : DOMString);          {!!.55 - Rewritten}
var
  NewStrLen : Integer;
begin
  NewStrLen := Length(sVal);
  if (NewStrLen > 0) then begin
    { Can the new string fit within the buffer? }
    if (FDataBufferSize = 0) then
      DataBufferInitialize
    else if (FDataBufferPos + NewStrLen >= FDataBufferSize) then begin
      { Nope. Double the size of the buffer.}
      FDataBufferSize := (FDataBufferSize * 2) + NewStrLen;
      SetLength(FDataBuffer, FDataBufferSize);
    end;

    { Append the new string to the buffer. }
    Move(sVal[1], FDataBuffer[FDataBufferPos + 1], NewStrLen * 2);
    FDataBufferPos := FDataBufferPos + NewStrLen;
  end;
end;                                                                   {!!.55 - End rewritten}
{--------}
procedure TXpParser.DataBufferFlush;
begin
  if FNormalizeData and
     not FCDATA and
     not FPreserve then
    DataBufferNormalize;
  if (FDataBufferPos <> 0) then begin                                  {!!.55}
    SetLength(FDataBuffer, FDataBufferPos);                            {!!.55}
    FDataBufferSize := FDataBufferPos;                                 {!!.55}
    case FCurrentElementContent of
      CONTENT_MIXED, CONTENT_ANY :
        if FCDATA then begin
          ValidateCData(FDataBuffer);
          if Assigned(FOnCDATASection) then
            FOnCDATASection(self, FDataBuffer);
        end else begin
          if Assigned(FOnCharData) then
            FOnCharData(self, FDataBuffer);
        end;
      CONTENT_ELEMENTS :
        if Assigned(FOnIgnorableWhitespace) then
          FOnIgnorableWhitespace(self, FDataBuffer);
    end;
    {FDataBuffer := '';}                                               {!!.55}
    DataBufferInitialize;                                              {!!.55}
  end;
end;
{--------}
procedure TXpParser.DataBufferInitialize;                              {!!.55 - Added}
begin
  FDataBufferSize := 1024;
  SetLength(FDataBuffer, 1024);
  FDataBufferPos := 0;
end;                                                                   {!!.55 - End added}
{--------}
procedure TXpParser.DataBufferNormalize;                               {!!.52 - Rewritten - Start}
var
  j           : Integer;
  NumToDelete : Integer;                                               {!!.55 - Start}
begin
  if (FDataBufferPos > 0) then begin
    { Delete whitespace from the beginning of the buffer. }
    j := 1;
    while ((IsWhiteSpace(FDataBuffer[j])) and
           (FDataBufferPos >= j)) do
      j := j + 1;
    if (j > 1) then begin
      j := j - 1;
      FDataBufferPos := FDataBufferPos - j;
      Move(FDataBuffer[j + 1],
           FDataBuffer[1],
           (FDataBufferPos * SizeOf(DOMChar)));
    end;
    { Delete whitespace from the end of the buffer. }
    while ((FDataBufferPos > 0) and
           (IsWhiteSpace(FDataBuffer[FDataBufferPos]))) do
      FDataBufferPos := FDataBufferPos - 1;
    {Note: There is no reason to actually delete the characters from
           the buffer. The size of the string inside the buffer will
           eventually be set using FDataBufferPos which maintains the
           length of the actual string.}

    j := 1;
    while (j <= FDataBufferPos) do begin
      if IsWhiteSpace(FDataBuffer[j]) then begin
        { Force whitespace to a single space }
        FDataBuffer[j] := ' ';

        { Remove additional whitespace }
        j := j + 1;
        NumToDelete := 0;
        while ((j <= FDataBufferPos) and
               (IsWhiteSpace(FDataBuffer[j]))) do begin
          NumToDelete := NumToDelete + 1;
          j := j + 1;
        end;
        if (NumToDelete > 0) then begin
          Move(FDataBuffer[j],
               FDataBuffer[j - NumToDelete],
               ((FDataBufferPos - j + 1) * SizeOf(DOMChar)));
          FDataBufferPos := FDataBufferPos - NumToDelete;
          j := j - NumToDelete;                                        {!!.57}
        end;
      end;
      j := j + 1;
    end;
  end;                                                                 {!!.55 - End}
end;                                                                   {!!.52 - Rewritten - End}
{--------}
function TXpParser.DataBufferToString(aClearBuffer : Boolean)          {!!.55}
                                                   : DOMString;
begin
  SetLength(FDataBuffer, FDataBufferPos);                              {!!.55 - Start}
  FDataBufferSize := FDataBufferPos;                                   {!!.56}
  Result := FDataBuffer;
  {FDataBuffer := '';}
  if (aClearBuffer) then
    DataBufferInitialize;                                              {!!.55 - End}
end;
{--------}
function TXpParser.GetErrorCount : Integer;
begin
  Result := FErrors.Count;
end;
{--------}
function TXpParser.GetErrorMsg(wIdx : Integer) : DOMString;
begin
  Result := sIndexOutOfBounds;
  if (wIdx >= 0) and
     (wIdx < FErrors.Count) then
    Result := FErrors[wIdx];
end;
{--------}
function TXpParser.DeclaredAttributes(const sName : DOMString;         {!!.52 - Rewritten - Start}
                                            aIdx  : Integer)
                                                  : TStringList;
begin
  if aIdx < 0 then
    Result := nil
  else
    Result := TXpElementInfo(FElementInfo.Objects[aIdx]).AttributeList;
end;                                                                   {!!.52 - Rewritten - End}
{--------}
function TXpParser.GetAttributeDefaultValueType(const sElemName,       {!!.52}
                                                      sAttrName : DOMString)
                                                                : Integer;
var
  wIdx      : Integer;
  oAttrList : TStringList;
  oAttr     : TXpAttributeInfo;
begin
  Result := ATTRIBUTE_DEFAULT_UNDECLARED;
  wIdx := GetElementIndexOf(sElemName);
  if wIdx >= 0 then begin
    oAttrList := TXpElementInfo(FElementInfo.Objects[wIdx]).AttributeList;
    if oAttrList <> nil then begin
      wIdx := oAttrList.IndexOf(sAttrName);
      if wIdx >= 0 then begin
        oAttr := TXpAttributeInfo(oAttrList.Objects[wIdx]);
        Result := oAttr.AttrType;
      end;
    end;
  end;
end;
{--------}
function TXpParser.GetAttributeExpandedValue(const sElemName,          {!!.52}
                                                   sAttrName : DOMString;
                                                   aIdx      : Integer){!!.52}
                                                             : DOMString;
var
  wIdx      : Integer;
  oAttrList : TStringList;
  oAttr     : TXpAttributeInfo;
  HasEntRef : Boolean;
begin
  SetLength(Result, 0);                                                {!!.52}
  HasEntRef := False;
  {wIdx := GetElementIndexOf(sElemName);}                              {!!.52}
  if aIdx >= 0 then begin                                              {!!.52}
    oAttrList := TXpElementInfo(FElementInfo.Objects[aIdx]).AttributeList;{!!.52}
    if oAttrList <> nil then begin
      wIdx := oAttrList.IndexOf(sAttrName);
      if wIdx >= 0 then begin
        oAttr := TXpAttributeInfo(oAttrList.Objects[wIdx]);
        if (oAttr.Lookup = '') and
           (oAttr.Value <> '') then begin
          PushString('"' + oAttr.Value + '"');
          oAttr.Lookup := ReadLiteral(LIT_NORMALIZE or
                                      LIT_CHAR_REF or
                                      LIT_ENTITY_REF,
                                      HasEntRef);
          SkipWhitespace(True);
        end;
        Result := oAttr.Lookup;
      end;
    end;
  end;
end;
{--------}
function TXpParser.GetElementContentType(const sName : DOMString;      {!!.52 - Rewritten - Start}
                                               aIdx  : Integer)
                                                     : Integer;
begin
  if aIdx < 0 then
    Result := CONTENT_UNDECLARED
  else
    Result := TXpElementInfo(FElementInfo.Objects[aIdx]).ContentType;
end;                                                                   {!!.52 - Rewritten - End}
{--------}
function TXpParser.GetElementIndexOf(const sElemName : DOMString)      {!!.52 - Rewritten - Start}
                                                     : Integer;
begin
  Result := FElementInfo.IndexOf(sElemName);
end;                                                                   {!!.52 - Rewritten - End}
{--------}
function TXpParser.GetEntityIndexOf(const sEntityName : DOMString;     {!!.52}
                                          aPEAllowed  : Boolean)
                                                      : Integer;
begin
  for Result := 0 to FEntityInfo.Count - 1 do
    if FEntityInfo[Result] = sEntityName then begin
      if (not aPEAllowed) then begin
        if (not TXpEntityInfo(FEntityInfo.Objects[Result]).IsParameterEntity) then
          Exit;
      end else
        Exit;
    end;
  Result := -1;
end;
{--------}
function TXpParser.GetEntityNotationName(const sEntityName : DOMString)
                                                           : DOMString;
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TXpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.NotationName;
  end;
end;
{--------}
function TXpParser.GetEntityPublicId(const sEntityName : DOMString)
                                                       : DOMString;
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TXpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.PublicId;
  end;
end;
{--------}
function TXpParser.GetEntitySystemId(const sEntityName : DOMString)
                                                       : DOMString;
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, False);
  if wIdx >= 0 then begin
    oEntity := TXpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.SystemId;
  end;
end;
{--------}
function TXpParser.GetEntityType(const sEntityName : DOMString;
                                       aPEAllowed  : Boolean)
                                                   : Integer;
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  Result := ENTITY_UNDECLARED;
  wIdx := GetEntityIndexOf(sEntityName, aPEAllowed);
  if wIdx >= 0 then begin
    oEntity := TXpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.EntityClass;
  end;
end;
{--------}
function TXpParser.GetEntityValue(const sEntityName : DOMString;
                                        aPEAllowed  : Boolean)
                                                    : DOMString;
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  Result := '';
  wIdx := GetEntityIndexOf(sEntityName, aPEAllowed);
  if wIdx >= 0 then begin
    oEntity := TXpEntityInfo(FEntityInfo.Objects[wIdx]);
    Result := oEntity.Value;
  end;
end;
{--------}
function TXpParser.GetExternalTextEntityValue(const sName,             {!!.52}
                                                    sPublicId : DOMString;{!!.52}
                                                    sSystemId : DOMString)        
                                                              : DOMString;
var
//  CompletePath : string;                                             {!!.52}
  oDirContext : TXpBaseDirContext;                                     {!!.52}
begin
  DataBufferFlush;
  Result := '';

  FHasExternals := True;

  if Assigned(FOnResolveEntity) then
    FOnResolveEntity(Self, sName, sPublicId, sSystemId, sSystemId);

  if (sSystemId = '') then
    Exit;

  oDirContext := FFilter.DirContext;                                   {!!.52}{!!.53}
  PushDocument;
{Deleted !!.52}
{  if (XpPos('/', sSystemID) = 0) and
     (XpPos('\', sSystemID) = 0) then
    CompletePath := FCurrentPath + sSystemID
  else
    CompletePath := sSystemID;}
  try
    LoadDataSource(oDirContext, sSystemID, FUserName,                  {!!.52}
                   FPassword, FErrors);                                {!!.52}
  except                                                               
    PopDocument;
    raise;
  end;
end;
{Begin !!.51}
{--------}
function TXpParser.GetInCharSet : TXpCharEncoding;
begin
  if FFilter <> nil then
    Result := MapStreamFormatToCharEnc(FFilter.Format)
  else
    { If no current filter then return last known value. }
    Result := FInCharSet;
end;
{End !!.51}
{--------}
procedure TXpParser.Initialize;
begin
  {FDataBuffer := '';}                                                 {!!.55}
  DataBufferInitialize;                                                {!!.55}

  SetInternalEntity('amp', '&#38;', False);
  SetInternalEntity('lt', '&#60;', False);
  SetInternalEntity('gt', '&#62;', False);
  SetInternalEntity('apos', '&#39;', False);
  SetInternalEntity('quot', '&#34;', False);
end;
{--------}
function TXpParser.IsEndDocument : Boolean;
var
//  TheStream : TStream;                                               {Deleted !!.53}
  DocCount  : Integer;                                                 {!!.52}
begin
  DocCount := FDocStack.Count;                                         {!!.52}
  if (DocCount = 0) then                                               {!!.52}
    Result := FFilter.isEof                                            {!!.52}
  else begin
    Result := False;
    while FFilter.isEOF do begin                                       {!!.52}
      if (DocCount > 0) then begin                                     {!!.52}
//      TheStream := FFilter.Stream;                                   {Deleted !!.53}
        FFilter.Free;
        FFilter := nil;                                                {!!.55}
//      TheStream.Free;                                                {Deleted !!.53}
      end;
      PopDocument;
      DocCount := FDocStack.Count;                                     {!!.52}
    end;
  end;
end;
{--------}
function TXpParser.IsWhitespace(const cVal : DOMChar) : Boolean;       {!!.52}
begin
{Rewritten !!.51}
  Result := (cVal = #$20) or (cVal = #$09) or
            (cVal = #$0D) or (cVal = #$0A);
end;
{--------}
function TXpParser.LoadDataSource(oCurContext : TXpBaseDirContext;
                            const sSrcName  : string;                  {!!.57}
                            const sUserName,
                                  sPassword : string;
                                  oErrors   : TStringList) : Boolean;
{Rewritten !!.52}
var
  oContext : TXpBaseDirContext;
  oFileStream : TXpFileStream;
begin
  Result := False;
  oFileStream := nil;
  if oCurContext = nil then
    oContext := TXpDirContext.Make(nil, sSrcName)
  else
    oContext := TXpDirContext.Make(oCurContext, sSrcName);             {!!.53}
  try                                                                  {!!.55}
    case oContext.ContextType of
    {$IFDEF XpUseInet}
      xpdcHTTP :
        begin
          { Attempt to load via HTTP. }
          oFileStream := XpLoadHTTP(oContext.Dir + oContext.Filename,  {!!.58}
                                    sUserName, sPassword, oErrors);    {!!.58}
          Result := (oFileStream <> nil) and (oFileStream.Size > 0);
          if Result then
            AddTemporaryFile(oFileStream.Filename)
          else begin
            oErrors.Add(format(sFileNotFound, [sSrcName]));
            raise EXpParserError.CreateError(0, 0,
                                             sSrcName,
                                             Format(sFileNotFound,
                                                    [QuotedStr(oContext.URL)]));
          end;
        end;
      xpdcFTP :
        begin
          { Attempt to load via FTP. }
          oFileStream := XpLoadFTP(oContext.Dir + oContext.Filename, sUserName,
                                   sPassword, oErrors);
          Result := (oFileStream <> nil) and (oFileStream.Size > 0);
          if Result then
            AddTemporaryFile(oFileStream.Filename)
          else begin
            oErrors.Add(format(sFileNotFound, [sSrcName]));
            raise EXpParserError.CreateError(0, 0,
                                             sSrcName,
                                             Format(sFileNotFound,
                                                    [QuotedStr(oContext.URL)]));
          end;
       end;
    {$ENDIF}
      xpdcLocal :
        begin
          { Local or network file. }
          if FileExists(oContext.URL) then begin
            { The stream and filter are destroyed after the document is parsed. }
            oFileStream := TXpFileStream.CreateEx(fmOpenRead or        {!!.55}
                                                  fmShareDenyWrite,    {!!.55}
                                                  oContext.URL);       {!!.55}
            oFileStream.Position := 0;
            Result := True;
  //          SetDirContext(oContext);                                 {Deleted !!.54}
          end else begin
            oErrors.Add(format(sFileNotFound, [sSrcName]));
            raise EXpParserError.CreateError(0,
                                             0,
                                             sSrcName,
                                             Format(sFileNotFound,
                                                    [QuotedStr(oContext.URL)])); {!!.53}
          end;
        end;
    end;  { case }

    if Result then begin                                               {!!.55}
      try
        oFileStream.Position := 0;
        if (FFilter <> nil) then                                       {!!.55}
          FFilter.Free;                                                {!!.55}
        FFilter := TXpInCharFilter.Create(oFileStream, FBufferSize);
        FFilter.DirContext := oContext;
      except
        oFileStream.Free;
        raise;
      end;
    end else                                                           {!!.55}
      oContext.Free;                                                   {!!.55}
  except                                                               {!!.55}
    oContext.Free;                                                     {!!.55}
    raise;                                                             {!!.55}
  end;                                                                 {!!.55}
end;
{--------}
procedure TXpParser.ParseAttDef(const sElemName : DOMString);
var
  sEnum,
  sName : DOMString;
  wType : Integer;
begin
  SkipWhiteSpace(True);                                                {!!.51}
  sEnum := '';
  while TryRead(Xpc_ParamEntity) do begin                              {!!.51}{!!.55}
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);                                              {!!.51}
  end;                                                                 {!!.51}
  sName := ReadNameToken(True);
  RequireWhitespace;
  wType := ReadAttType;
  if (wType = ATTRIBUTE_ENUMERATED) or
     (wType = ATTRIBUTE_NOTATION) then
    sEnum := DataBufferToString(True);                                 {!!.55}
  RequireWhitespace;
  ParseDefault(sElemName, sName, wType, sEnum);
end;
{--------}
procedure TXpParser.ParseAttlistDecl;
var
  sElemName : DOMString;
begin
//  FAttrEnum.Clear;                                                   {Deleted !!.51}
  RequireWhitespace;
  sElemName := ReadNameToken(True);
  RequireWhitespace;
  if Assigned(FOnDTDAttlistStart) then
    FOnDTDAttlistStart(self, sElemName);
  while ((FFilter.ReadChar <> '>') and                                 {!!.52}
         (not FFilter.isEOF)) do begin                                 {!!.52}
    FAttrEnum.Clear;                                                   {!!.51}
    ParseAttDef(sElemName);
    SkipWhitespace(True);                                              {!!.51}{!!.52}
    if TryRead(Xpc_ParamEntity) then begin                             {!!.53}
      ParseParameterEntityRef(True, False);                            {!!.53}
      SkipWhitespace(True);                                            {!!.53}
    end;                                                               {!!.53}
  end;
  FFilter.SkipChar;                                                    {!!.52}
  SkipWhitespace(False);                                               {!!.52}
  if Assigned(FOnDTDAttlistEnd) then
    FOnDTDAttlistEnd(self);
end;
{--------}
function TXpParser.ParseAttribute(const sName : DOMString) : DOMString;
var
  sAttrName,
  sValue    : DOMString;
  wType     : Integer;
  HasEntRef : Boolean;
begin
  Result := '';
  HasEntRef := False;
  sAttrName := ReadNameToken(True);
  wType := GetAttributeDefaultValueType(sName, sAttrName);

  ParseEq;

  {we need to validate production 10 - 1st letter in quotes}

  if (wType = ATTRIBUTE_CDATA) or (wType = ATTRIBUTE_UNDECLARED) then
    sValue := ReadLiteral(LIT_CHAR_REF or LIT_ENTITY_REF, HasEntRef)
  else
    sValue := ReadLiteral(LIT_CHAR_REF or
                          LIT_ENTITY_REF or
                          LIT_NORMALIZE,
                          HasEntRef);
  if not HasEntRef then
    ValidateAttribute(sValue, HasEntRef);

  if Assigned(FOnAttribute) then
    FOnAttribute(self, sAttrName, sValue, True);
  {FDataBuffer := '';}                                                 {!!.55}
  DataBufferInitialize;                                                {!!.55}

  FTagAttributes.Add(sAttrName);
  if sAttrName = XpsXMLSpace then                                      {!!.53}
    Result := sValue;
end;
{--------}
procedure TXpParser.ParseCDSect;
{conditional section}
begin
  ParseUntil(Xpc_ConditionalEnd);
end;
{--------}
function TXpParser.ParseCharRef : DOMChar;                             {!!.52}
var
  TempChar  : DOMChar;                                                 {!!.52 - Start}
  {TempChar1 : DOMChar;}
  {TempChar2 : DOMChar;}                                               {!!.52 - End}
  Ucs4Chr   : TXpUcs4Char;
begin
  {TempChar := '';}                                                    {!!.52}
  Ucs4Chr := 0;
  if TryRead(Xpc_CharacterRefHex) then begin
   Ucs4Chr := 0;
    while True do begin
      TempChar := ReadChar(True);
      if (TempChar = '0') or (TempChar = '1') or (TempChar = '2') or
         (TempChar = '3') or (TempChar = '4') or (TempChar = '5') or
         (TempChar = '6') or (TempChar = '7') or (TempChar = '8') or
         (TempChar = '9') or (TempChar = 'A') or (TempChar = 'B') or
         (TempChar = 'C') or (TempChar = 'D') or (TempChar = 'E') or
         (TempChar = 'F') or (TempChar = 'a') or (TempChar = 'b') or
         (TempChar = 'c') or (TempChar = 'd') or (TempChar = 'e') or
         (TempChar = 'f') then begin
        Ucs4Chr := Ucs4Chr shl 4;
        Ucs4Chr := Ucs4Chr + StrToIntDef('$' + DOMString(TempChar), 0); {!!.52}
      end else if (TempChar = ';') then
        Break
      else
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         Format(sIllCharInRef,         {!!.53}
                                                [DOMString(TempChar),  {!!.53}
                                                 Ord(TempChar)]));     {!!.53}
    end;
  end else begin
    while True do begin
      TempChar := ReadChar(True);
      if (TempChar = '0') or (TempChar = '1') or (TempChar = '2') or
         (TempChar = '3') or (TempChar = '4') or (TempChar = '5') or
         (TempChar = '6') or (TempChar = '7') or (TempChar = '8') or
         (TempChar = '9') then begin
        Ucs4Chr := Ucs4Chr * 10;
        Ucs4Chr := Ucs4Chr + StrToIntDef(TempChar, 0);
      end else if (TempChar = ';') then
        Break
      else
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         Format(sIllCharInRef,         {!!.53}
                                                [DOMString(TempChar),  {!!.53}
                                                 Ord(TempChar)]));     {!!.53}
    end;
  end;
  XpUcs4ToWideChar(Ucs4Chr, Result);                                   {!!.52 - Start}
  {if TempChar2 = #0 then                   }
  {  Result := '' + TempChar1               }
  {else                                     }
  {  Result := '' + TempChar1 + TempChar2;  }                          {!!.52 - End}
  DataBufferAppend(Result);
end;
{--------}
procedure TXpParser.ParseComment;
var
  TempComment : DOMString;
begin
  ParseUntil(Xpc_CommentEnd);
  TempComment := DataBufferToString(True);                             {!!.55}
{Begin !!.51}
  { Did we find '--' within the comment? }
//  if (TempComment = '') or
  if (TempComment <> '') and
     ((XpPos('--', TempComment) <> 0) or
      (TempComment[Length(TempComment)] = '-')) then
    { Yes. Raise an error. }
{End !!.51}
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sInvalidCommentText);
  if Assigned(FOnComment) then
    FOnComment(self, TempComment);
end;
{--------}
procedure TXpParser.ParseConditionalSect;
begin
  SkipWhitespace(True);                                                {!!.51}
  if TryRead(Xpc_ParamEntity) then
    ParseParameterEntityRef(True, False);

  if TryRead(Xpc_ConditionalInclude) then begin
    SkipWhitespace(True);                                              {!!.51}
    Require(Xpc_BracketSquareLeft);
    SkipWhitespace(True);                                              {!!.51}
    if Assigned(FOnDTDConditionalStart) then
      FOnDTDConditionalStart(self, CONDITIONAL_INCLUDE);
    while not TryRead(Xpc_ConditionalEnd) do begin
      ParseMarkupDecl(False);
      SkipWhitespace(True);                                            {!!.51}
    end;
    if Assigned(FOnDTDConditionalEnd) then
      FOnDTDConditionalEnd(self);
  end else if TryRead(Xpc_ConditionalIgnore) then begin
    SkipWhitespace(True);                                              {!!.51}
    Require(Xpc_BracketSquareLeft);
    SkipWhitespace(True);                                              {!!.51}
    if Assigned(FOnDTDConditionalStart) then
      FOnDTDConditionalStart(self, CONDITIONAL_IGNORE);
{Begin !!.52}
    ParseIgnore;
//    while not TryRead(Xpc_ConditionalEnd) do begin
//      ParseMarkupDecl(False);
//      SkipWhitespace(True);                                            {!!.51}
//    end;
{End !!.52}
    if Assigned(FOnDTDConditionalEnd) then
      FOnDTDConditionalEnd(self);
  end else
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sIllCondSectStart);
end;
{--------}
procedure TXpParser.ParseContent;
var
  TempChar    : DOMChar;                                               {!!.52}
  TempStr     : DOMString;                                             {!!.52}
  EntRefs     : TStringList;
  OldLine     : Integer;
  OldPos      : Integer;
  TempInt     : Integer;
  StackLevel  : Integer;                                               {!!.51}
  LastCharAmp : Boolean;                                               {!!.52}
begin
  LastCharAmp := False;                                                {!!.52}
  StackLevel := 0;                                                     {!!.51}
  TempChar := #0;                                                      {!!.52}
  EntRefs := nil;
  try                                                                  {!!.55}
    while True do begin
      OldLine := FFilter.Line;
      OldPos := FFilter.LinePos;
      case FCurrentElementContent of
        CONTENT_ANY, CONTENT_MIXED :
          begin
            if Assigned(EntRefs) then begin
              if ((not ((FDataBufferPos = 1) and                       {!!.55}
                        (FDataBuffer[1] = '&'))) or                    {!!.52}{!!.55}
                  (LastCharAmp)) then begin                            {!!.52}
                ParsePCData(True);                                     {!!.52}
                LastCharAmp := False;                                  {!!.52}
              end;
              { Reset the last ent ref if we parsed something.}
              if (FFilter.Line <> OldLine) and
                 (FFilter.LinePos <> OldPos) then begin
                EntRefs.Free;
                EntRefs := nil;
              end;
            end else
              ParsePCData(TempChar <> '');
          end;
        CONTENT_ELEMENTS           : ParseWhitespace;
      end;
      if IsEndDocument then
        raise ExpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sUnexpectedEof);
      TempChar := ReadChar(False);                                     {Moved !!.52}
      if (TempChar = '&') then begin
        SkipChar;                                                      {!!.52}
        TempChar := ReadChar(False);
        if TempChar = '#' then begin
          SkipChar;                                                    {!!.52}
          TempChar := ParseCharRef;
          if EntRefs <> nil then                                       {!!.53}
            EntRefs.Clear;                                             {!!.53}
          if TempChar = '&' then                                       {!!.52}
            LastCharAmp := True;                                       {!!.52}
          if (FCurrentElementContent <> CONTENT_ANY) and
             (FCurrentElementContent <> CONTENT_MIXED) then
            PushString(TempChar);
        end else begin
          TempStr := ParseEntityRef(False);                            {!!.52}

          { Check for circular references...}

          { We can't have a circular reference if the parsed value
            doesn't contain another reference.}
          if (XpPos('&', TempStr) <> -1) then begin                    {!!.52}
            if (not Assigned(EntRefs)) then begin
              StackLevel := FDocStack.Count;                           {!!.51}{!!.53}
              EntRefs := TStringList.Create;
            end
            else begin
              StackLevel := FDocStack.Count;                           {!!.51}
              TempInt := EntRefs.IndexOf(TempStr);                     {!!.52}
              if (TempInt <> -1) then
                raise ExpParserError.CreateError(FFilter.Line,
                                                 FFilter.LinePos,
                                                 FFilter.URL,
                                                 sCircularEntRef +
                                                 TempStr);             {!!.52}
            end; {if..else}
            EntRefs.Add(TempStr);                                      {!!.52}
          end;
        end;
        if (FCurrentElementContent <> CONTENT_ANY) and
           (FCurrentElementContent <> CONTENT_MIXED) and
           (TempChar = '<') then begin
          DataBufferFlush;
          ParseElement;
        end else
          TempChar := ReadChar(False);
      end else if (TempChar = '<') then begin
        EntRefs.Free;
        EntRefs := nil;
        SkipChar;                                                      {!!.52}
        TempChar := ReadChar(False);
        if (TempChar = '!') then begin
          SkipChar;                                                    {!!.52}
          DataBufferFlush;
          TempChar := ReadChar(True);
          if (TempChar = '-') then begin
            Require(Xpc_Dash);
            ParseComment;
          end else if (TempChar = '[') then begin
            Require(Xpc_CDATAStart);
            FCDATA := True;
            ParseCDSect;
            ValidateCData(DataBufferToString(False));                  {!!.55}
            DataBufferFlush;
            FCDATA := False;
          end else
            raise ExpParserError.CreateError(FFilter.Line,
                                             FFilter.LinePos,
                                             FFilter.URL,
                                             sExpCommentOrCDATA +
                                             '(' + TempChar + ')');
        end else if (TempChar = '?') then begin
          EntRefs.Free;
          EntRefs := nil;
          SkipChar;                                                    {!!.52}
          DataBufferFlush;
          ParsePI;
        end else if (TempChar = '/') then begin
          SkipChar;                                                    {!!.52}
          DataBufferFlush;
          ParseEndTag;
          Exit;
        end else begin
          EntRefs.Free;
          EntRefs := nil;
          DataBufferFlush;
          ParseElement;
        end;
{Begin !!.52}
      end
      else begin
        raise ExpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sExpectedElement);
      end;  { if }
{End !!.52}
      if (Assigned(EntRefs)) and                                       {!!.51 - Start}
         (FDocStack.Count < StackLevel) then begin
        EntRefs.Clear;
        StackLevel := FDocStack.Count;
      end;                                                             {!!.51 - End}
      IsEndDocument;                                                   {!!.52}
    end;
  finally                                                              {!!.55}
    EntRefs.Free;                                                      {!!.55}
  end;                                                                 {!!.55}
end;
{--------}
procedure TXpParser.ParseContentSpec(const sName       : DOMString;
                                           aIsInternal : Boolean);
var                                                                    {!!.51}
  wOccur : Integer;                                                    {!!.51}
begin

  while TryRead(Xpc_ParamEntity) do begin                              {!!.51}{!!.52}
    CheckParamEntityNesting(ParseParameterEntityRef(True , False));    {!!.51}
    SkipWhiteSpace(True);                                              {!!.51}
  end;                                                                 {!!.51}

  if TryRead(Xpc_DTDElementEmpty) then begin
    if Assigned(FOnDTDElementStart) then
      FOnDTDElementStart(self, sName, CONTENT_EMPTY);
    SetElement(sName, CONTENT_EMPTY, '');
  end else if TryRead(Xpc_DTDElementAny) then begin
    if Assigned(FOnDTDElementStart) then
      FOnDTDElementStart(self, sName, CONTENT_ANY);
    SetElement(sName, CONTENT_ANY, '');
  end else begin
    Require(Xpc_ParenLeft);
    DataBufferAppend('(');
    SkipWhitespace(True);                                              {!!.51}

    while TryRead(Xpc_ParamEntity) do begin                            {!!.53}
      if aIsInternal then
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sNoPEInIntDTD)
      else
        ParseParameterEntityRef(True, False);
    end;  { while }                                                    {!!.53}

    if TryRead(Xpc_DTDElementCharData) then begin
      if Assigned(FOnDTDElementStart) then
        FOnDTDElementStart(self, sName, CONTENT_MIXED);
      DataBufferAppend('#PCDATA');
      wOccur := OCCURS_REQ_NOREPEAT;                                   {!!.51}
      if Assigned(FOnDTDElementGroupStart) then                        {!!.52}
        FOnDTDElementGroupStart(self);
      if Assigned(FOnDTDElementContent) then
        FOnDTDElementContent(self,
                             '#PCDATA',
                             wOccur,                                   {!!.51}
                             REL_OR);
      ParseMixed(wOccur);                                              {!!.51}
      if Assigned(FOnDTDElementGroupEnd) then
        FOnDTDElementGroupEnd(self, wOccur, REL_AND);                  {!!.51}
      SetElement(sName, CONTENT_MIXED, DataBufferToString(True));      {!!.55}
    end else begin
      if Assigned(FOnDTDElementStart) then
        FOnDTDElementStart(self, sName, CONTENT_ELEMENTS);
      if Assigned(FOnDTDElementGroupStart) then
        FOnDTDElementGroupStart(self);
      ParseElements;
      SetElement(sName, CONTENT_ELEMENTS, DataBufferToString(True));   {!!.55}
    end;
  end;
  if Assigned(FOnDTDElementEnd) then
    FOnDTDElementEnd(self, sName);
end;
{--------}
procedure TXpParser.ParseCp;
var
  sName    : DOMString;
  wOccurs,
  wRel     : Integer;
  TempChar : DOMChar;                                                  {!!.52}
begin
  if TryRead(Xpc_ParamEntity) then begin                               {!!.51 - Start}
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);
  end;                                                                 {!!.51 - End}

  if TryRead(Xpc_ParenLeft) then begin
    DataBufferAppend('(');
    if Assigned(FOnDTDElementGroupStart) then
      FOnDTDElementGroupStart(self);
    ParseElements;
  end else begin
    if TryRead(Xpc_ParamEntity) then
      ParseParameterEntityRef(True, False);                            {!!.51}

    sName := ReadNameToken(True);
    DataBufferAppend(sName);
    TempChar := ReadChar(False);
    wOccurs := OCCURS_REQ_NOREPEAT;
    if (TempChar = '*') then begin
      DataBufferAppend(TempChar);
      wOccurs := OCCURS_OPT_REPEAT;
      SkipChar;                                                        {!!.52}
    end else if (TempChar = '+') then begin
      DataBufferAppend(TempChar);
      wOccurs := OCCURS_REQ_REPEAT;
      SkipChar;                                                        {!!.52}
    end else if (TempChar = '?') then begin
      DataBufferAppend(TempChar);
      wOccurs := OCCURS_OPT_NOREPEAT;
      SkipChar;                                                        {!!.52}
    end;
    SkipWhitespace(True);                                              {!!.51}
{Begin !!.55}
    while TryRead(Xpc_ParamEntity) do begin
      ParseParameterEntityRef(True, False);
      SkipWhitespace(True);
    end;
{End !!.55}
    TempChar := ReadChar(False);
    wRel := REL_AND;
    if (TempChar = ')') then begin
    end else if (TempChar = ',') then begin
      DataBufferAppend(TempChar);
      wRel := REL_AND;
      SkipChar;                                                        {!!.52}
    end else if (TempChar = '|') then begin
      DataBufferAppend(TempChar);
      wRel := REL_OR;
      SkipChar;                                                        {!!.52}
    end else
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sBadSepInModel + TempChar);
    if Assigned(FOnDTDElementContent) then
      FOnDTDElementContent(self, sName, wOccurs, wRel);
  end;
end;
{--------}
function TXpParser.ParseDataSource(const sSource : string) : Boolean;
{Rewritten !!.52}
var
  sSavCurDir : string;
begin
  sSavCurDir := GetCurrentDir;
  try
    FErrors.Clear;
    FIsStandAlone := False;
    FHasExternals := False;
    Result := LoadDataSource(nil, sSource, FUserName, FPassword, FErrors);
    if Result then begin
//    FFilter.FreeStream := True;                                      {Deleted !!.53}
      FDocLocation := FFilter.DirContext.Dir;                          {!!.52}
      FDocName := FFilter.DirContext.Filename;                         {!!.52}
      ParsePrim;
    end
    else
      FErrors.Add(sSrcLoadFailed + sSource);
    Result := (FErrors.Count = 0);
  finally
    SetCurrentDir(sSavCurDir);
  end;
end;
{--------}
procedure TXpParser.ParseDefault(const sElemName, sName : DOMString;   {!!.52}
                                       wType            : Integer;
                                 const sEnum            : DOMString);  {!!.52}
var
  wValueType : Integer;
  sValue     : DOMString;
  HasEntRef  : Boolean;
begin
  HasEntRef := False;
  wValueType := ATTRIBUTE_DEFAULT_SPECIFIED;
  if TryRead(Xpc_ParamEntity) then begin
    ParseParameterEntityRef(True, False);
    RequireWhitespace;
  end;
  if TryRead(Xpc_CharacterRef) then begin
    if TryRead(Xpc_DTDAttFixed) then begin
      wValueType := ATTRIBUTE_DEFAULT_FIXED;
      RequireWhitespace;
      FContext := CONTEXT_ATTRIBUTEVALUE;
      sValue := ReadLiteral(LIT_CHAR_REF, HasEntRef);
      FContext := CONTEXT_DTD;
    end else if TryRead(Xpc_DTDAttRequired) then
      wValueType := ATTRIBUTE_DEFAULT_REQUIRED
    else if TryRead(Xpc_DTDAttImplied) then
      wValueType := ATTRIBUTE_DEFAULT_IMPLIED
    else
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sIllAttrDefKeyw +
                                       ReadNameToken(False));
  end else begin
    FContext := CONTEXT_ATTRIBUTEVALUE;
    sValue := ReadLiteral(LIT_CHAR_REF, HasEntRef);
    FContext := CONTEXT_DTD;
  end;
  SetAttribute(sElemName, sName, wType, sEnum, sValue, wValueType);

  if Assigned(FOnDTDAttDefinition) then
    FOnDTDAttDefinition(self,
                        sName,
                        wType,
                        FAttrEnum,
                        wValueType,
                        sValue);
end;
{--------}
procedure TXpParser.ParseDocTypeDecl;
var
  sDocTypeName : DOMString;
  sIds         : StringIds;
//  CompletePath : string;                                             {!!.52}
  oContext     : TXpBaseDirContext;                                    {!!.52}{!!.53}
//  oStream      : TStream;                                            {Deleted !!.53}
begin
  RequireWhitespace;
  sDocTypeName := ReadNameToken(True);
  SkipWhitespace(True);                                                {!!.51}
  ReadExternalIds(False, sIds);
  SkipWhitespace(True);                                                {!!.51}
  if Assigned(FOnDTDStart) then
    FOnDTDStart(self, sDocTypeName, sIds[0], sIds[1]);

  // Parse internal DTD
  if TryRead(Xpc_BracketSquareLeft) then begin
    while True do begin
      FContext := CONTEXT_DTD;
      SkipWhitespace(True);                                            {!!.51}
      if TryRead(Xpc_ParamEntity) then
        ParseParameterEntityRef(True, False);
      FContext := CONTEXT_NONE;
      SkipWhiteSpace(True);                                            {!!.51}
      if TryRead(Xpc_BracketSquareRight) then
        Break
      else begin
        FContext := CONTEXT_DTD;
        ParseMarkupDecl(True);
        FContext := CONTEXT_NONE;
      end;
    end;
  end;

  // Parse external DTD
  if sIds[1] <> '' then begin
    if Assigned(FOnDTDExternalStart) then
      FOnDTDExternalStart(self, sIds[1]);
    FXMLDecParsed := False;                                            {!!.52}
    oContext := FFilter.DirContext;                                    {!!.52}{!!.53}
    PushDocument;
{Deleted !!.52}
{    if (XpPos('/', sIDs[1]) = 0) and
       (XpPos('\', sIDs[1]) = 0) then
      CompletePath := FCurrentPath + sIDs[1]
    else
      CompletePath := sIDs[1];}
    try
      if LoadDataSource(oContext, sIDs[1], FUserName, FPassword,       {!!.52}
                        FErrors) then begin                            {!!.52}{!!.53}
        if Assigned(FOnDTDExternalLoaded) then                         {!!.53}
          FOnDTDExternalLoaded(Self);                                  {!!.53}
        while True do begin
          FContext := CONTEXT_DTD;
          SkipWhitespace(True);                                        {!!.51}
          if FFilter.isEOF then begin                                  {!!.52}
            FContext := CONTEXT_NONE;
            Break;
          end;
          ParseMarkupDecl(False);
          FContext := CONTEXT_NONE;
          SkipWhitespace(False);                                       {!!.51}
          {This part of the routine needs to know when we reach the end of
           the external DTD, so we are checking for the end of the
           document (filter) directly here.}
          if (FFilter.isEof) or                                        {!!.51}{!!.52}
             (FDocStack.Count = 0) then                                {!!.51}
            Break;
        end;  { while }
      end;  { if }                                                     {!!.53}
    except
      if (FDocStack.Count > 0) then begin                              {!!.51}{!!.52}
        if FFilter <> nil then begin                                   {!!.52}
//        oStream := FFilter.Stream;                                   {!!.52}{!!.53}
          FFilter.Free;                                                {!!.52}
          FFilter := nil;                                              {!!.55}
//        oStream.Free;                                                {!!.52}{!!.53}
        end;                                                           {!!.52}
        PopDocument;                                                   {!!.52}
      end;                                                             {!!.52}
      raise;                                                         
    end;
    FXMLDecParsed := True;                                             {!!.52}
    if Assigned(FOnDTDExternalEnd) then
      FOnDTDExternalEnd(self);
    IsEndDocument;                                                     {Moved !!.53}
  end;

  if sIds[1] <> '' then begin
    while True do begin
      FContext := CONTEXT_DTD;
      SkipWhitespace(True);                                            {!!.51}
      FContext := CONTEXT_NONE;
      if TryRead(Xpc_BracketAngleRight) then
        Break
      else begin
        FContext := CONTEXT_DTD;
        ParseMarkupDecl(False);
        FContext := CONTEXT_NONE;
      end;
    end;
  end else begin
    SkipWhitespace(True);                                              {!!.51}
    Require(Xpc_BracketAngleRight);
  end;

  if Assigned(FOnDTDEnd) then
    FOnDTDEnd(self);

  if Assigned(FOnDocTypeDecl) then
    FOnDocTypeDecl(self, sDocTypeName, sIds[0], sIds[1]);
end;
{--------}
procedure TXpParser.ParseDocument;
begin
  FXMLDecParsed := False;                                              {!!.51}
  ParseProlog;
  Require(Xpc_BracketAngleLeft);
  ParseElement;
  {try}                                                                {!!.57}             
    ParseMisc;
  {except
  end;}                                                                {!!.57}
  SkipWhiteSpace(True);                                                {!!.51}
  if (not IsEndDocument) then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sDataAfterValDoc);

//  if Assigned(FOnEndDocument) then                                   {Deleted !!.55}
//    FOnEndDocument(self);                                            {Deleted !!.55}
end;
{--------}
procedure TXpParser.ParseElement;
var
  wOldElementContent,
  i                  : Integer;
  sOldElement        : DOMString;
  sGi, sTmp, sTmp2   : DOMString;
  oTmpAttrs          : TStringList;
  bOldPreserve       : Boolean;
  TempChar           : DOMChar;                                        {!!.52}
  aList              : TStringList;
  ElemIdx            : Integer;                                        {!!.52}
  AttrIdx            : Integer;                                        {!!.55}
begin
  wOldElementContent := FCurrentElementContent;
  sOldElement := FCurrentElement;
  bOldPreserve := FPreserve;

  FTagAttributes.Clear;
  sGi := ReadNameToken(True);

  ValidateElementName(sGi);

  FCurrentElement := sGi;
  ElemIdx := GetElementIndexOf(sGi);                                   {!!.52}
  FCurrentElementContent := GetElementContentType(sGi, ElemIdx);       {!!.52}
  if FCurrentElementContent = CONTENT_UNDECLARED then
    FCurrentElementContent := CONTENT_ANY;

  SkipWhitespace(True);                                                {!!.51}
  sTmp := '';
  TempChar := ReadChar(False);
  while (TempChar <> '/') and
        (TempChar <> '>') do begin
    sTmp2 := ParseAttribute(sGi);
    if sTmp2 <> '' then
      sTmp := sTmp2;
    SkipWhitespace(True);                                              {!!.51}
    TempChar := ReadChar(False);
    { check for duplicate attributes }
    if FTagAttributes.Count > 1 then begin
      aList := TStringList.Create;
      try
        aList.Sorted := True;
        aList.Duplicates := dupIgnore;
        aList.Assign(FTagAttributes);
        if (aList.Count <> FTagAttributes.Count) then
          raise EXpParserError.CreateError(FFilter.Line,
                                           FFilter.LinePos,
                                           FFilter.URL,
                                           sRedefinedAttr);
      finally
        aList.Free;
      end;
    end;
  end;

  oTmpAttrs := DeclaredAttributes(sGi, ElemIdx);                       {!!.52}
  if oTmpAttrs <> nil then begin
    for i := 0 to oTmpAttrs.Count - 1 do begin
      AttrIdx := FTagAttributes.IndexOf(oTmpAttrs[i]);                 {!!.55}
      if (AttrIdx <> - 1) and                                          {!!.55}
         (oTmpAttrs[i] = FTagAttributes[AttrIdx]) then                 {!!.55}
        Continue;

      if Assigned(FOnAttribute) then begin
        sTmp2 := GetAttributeExpandedValue(sGi, oTmpAttrs[i], ElemIdx);{!!.52}
        if sTmp2 <> '' then
          FOnAttribute(self, oTmpAttrs[i], sTmp2, False);
      end;
    end;
  end;

  if sTmp = '' then
    sTmp := GetAttributeExpandedValue(sGi, XpsXMLSpace, ElemIdx);      {!!.52}{!!.53}
  if sTmp = XpsPreserve then                                           {!!.53}
    FPreserve := True
  else if sTmp = 'default' then
    FPreserve := not FNormalizeData;

  if Assigned(FOnPreserveSpace) then
    FOnPreserveSpace(self, sGi, FPreserve);

  TempChar := ReadChar(True);
  if (TempChar = '>') then begin
    if Assigned(FOnStartElement) then
      FOnStartElement(self, sGi);
    ParseContent;
  end else if (TempChar = '/') then begin
    Require(Xpc_BracketAngleRight);
    if Assigned(FOnStartElement) then
      FOnStartElement(self, sGi);
    if Assigned(FOnEndElement) then
      FOnEndElement(self, sGi);
  end;

  FPreserve := bOldPreserve;
  FCurrentElement := sOldElement;
  FCurrentElementContent := wOldElementContent;
end;
{--------}
procedure TXpParser.ParseElementDecl(aIsInternal : Boolean);
var
  sName : DOMString;
begin
  RequireWhitespace;
  sName := ReadNameToken(True);
  RequireWhitespace;
  ParseContentSpec(sName, aIsInternal);
  SkipWhitespace(True);                                                {!!.51}
  if TryRead(Xpc_ParamEntity) then begin                               {!!.53}
    ParseParameterEntityRef(True, False);                              {!!.53}
    SkipWhitespace(True);                                             
  end;                                                                 {!!.53}
  Require(Xpc_BracketAngleRight);
end;
{--------}
procedure TXpParser.ParseElements;
var
  wOccurs,
  wRel     : Integer;
  TempChar : DOMChar;                                                  {!!.52}
begin
  while True do begin
    SkipWhitespace(True);                                              {!!.51}
    ParseCp;
    SkipWhitespace(True);                                              {!!.51}
    TempChar := ReadChar(False);
    if TempChar = ')' then begin
      DataBufferAppend(')');
      SkipChar;                                                        {!!.52}
      Break;
    end;
  end;

  SkipWhiteSpace(True);                                                {!!.51}

  TempChar := ReadChar(False);
  wOccurs := OCCURS_REQ_NOREPEAT;
  if (TempChar = '*') then begin
    DataBufferAppend(TempChar);
    wOccurs := OCCURS_OPT_REPEAT;
    SkipChar;                                                          {!!.52}
  end else if (TempChar = '+') then begin
    DataBufferAppend(TempChar);
    wOccurs := OCCURS_REQ_REPEAT;
    SkipChar;                                                          {!!.52}
  end else if (TempChar = '?') then  begin
    DataBufferAppend(TempChar);
    wOccurs := OCCURS_OPT_NOREPEAT;
    SkipChar;                                                          {!!.52}
  end;

  SkipWhitespace(True);                                                {!!.51}
  TempChar := ReadChar(False);
  wRel := REL_AND;
  if (TempChar = ',') then begin
    DataBufferAppend(TempChar);
    wRel := REL_AND;
    SkipChar;                                                          {!!.52}
  end else if (TempChar = '|') then begin
    DataBufferAppend(TempChar);
    wRel := REL_OR;
    SkipChar;                                                          {!!.52}
  end;

  if Assigned(FOnDTDElementGroupEnd) then
    FOnDTDElementGroupEnd(self, wOccurs, wRel);
end;
{--------}
procedure TXpParser.ParseEndTag;
var
  sName : DOMString;
begin
  sName := ReadNameToken(True);
  if sName <> FCurrentElement then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sMismatchEndTag +
                                     'Start tag = "' + FCurrentElement +
                                     '" End tag = "' + sName + '"');
  SkipWhitespace(True);                                                {!!.51}
  Require(Xpc_BracketAngleRight);
  if Assigned(FOnEndElement) then
    FOnEndElement(self, FCurrentElement);
end;
{--------}
procedure TXpParser.ParseEntityDecl;
var
  bPeFlag       : Boolean;
  sNotationName,
  sName,
  sName2,
  sValue        : DOMString;
  sIds          : StringIds;
  TempChar      : DOMChar;                                             {!!.52}
  HasEntRef     : Boolean;
begin
  bPeFlag := False;
  HasEntRef := False;
  RequireWhitespace;
  if TryRead(Xpc_ParamEntity) then begin
    bPeFlag := True;
    RequireWhitespace;
  end;
  sName := ReadNameToken(True);
  sName2 := sName;
  RequireWhitespace;

  TempChar := ReadChar(False);                                         {!!.52}
  if (TempChar = '"') or                                               {!!.52}
     (TempChar = #39) then begin                                       {!!.52}
    FContext := CONTEXT_ENTITYVALUE;
    sValue := ReadLiteral(LIT_CHAR_REF, HasEntRef);
    if (not HasEntRef) and                                             {!!.51}
       (not bPeFlag) then                                              {!!.51}
      ValidateEntityValue(sValue, TempChar);                           {!!.52}
    FContext := CONTEXT_DTD;
    SetInternalEntity(sName, sValue, bPeFlag);
    if Assigned(FOnDTDEntityInternal) then
      FOnDTDEntityInternal(self, bPeFlag, sName2, sValue)
  end else begin
    ReadExternalIds(False, sIds);
    if sIds[1] = '' then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sSysIdMissing +
                                       QuotedStr(sName));
    TempChar := ReadChar(False);                                       {!!.52}
    if IsWhiteSpace(TempChar) then begin                               {!!.52}
      SkipChar;                                                        {!!.52}
      if TryRead(Xpc_UnparsedEntity) then begin
        if bPeFlag then
          raise EXpParserError.CreateError(FFilter.Line,
                                           FFilter.LinePos,
                                           FFilter.URL,
                                           sNoNDATAInPeDecl);
        RequireWhitespace;
        sNotationName := ReadNameToken(True);
{!!.52 - Deleted following IF statement. The IF statement is a validation check
  as per the XML spec, Section 4.2.2 but this is not a validating parser. }
{        if FNotationInfo.IndexOf(sNotationName) = -1 then
          raise EXpParserError.CreateError(FFilter.Line,
                                           FFilter.LinePos,
                                           FFilter.URL,
                                           sNotationNotDeclared +
                                           QuotedStr(sNotationName));}
        SetExternalDataEntity(sName, sIds[0], sIds[1], sNotationName);
      end else begin                                                   {!!.51}
        PushString(TempChar);                                          {!!.52}
//    end else                                                         {!!.51}
        SetExternalTextEntity(sName, sIds[0], sIds[1]);
      end;                                                             {!!.51}
    end else                                                           {!!.51}
      SetExternalTextEntity(sName, sIds[0], sIds[1]);                  {!!.51}

    if Assigned(FOnDTDEntityExternal) then
      FOnDTDEntityExternal(self,
                           bPeFlag,
                           sName2,
                           sIds[0],
                           sIds[1],
                           sNotationName);
  end;

  SkipWhitespace(True);                                                {!!.51}
  Require(Xpc_BracketAngleRight);
end;
{--------}
function TXpParser.ParseEntityRef(bPEAllowed : Boolean) : DOMString;
begin
  Result := ReadNameToken(True);
  Require(Xpc_GenParsedEntityEnd);
  case GetEntityType(Result, bPEAllowed) of
    ENTITY_UNDECLARED :
      begin
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sUndeclaredEntity +
                                         QuotedStr(Result));
      end;
    ENTITY_INTERNAL :
      PushString(GetEntityValue(Result, False));
    ENTITY_TEXT :
      begin
        (GetExternalTextEntityValue(Result,
                                    GetEntityPublicId(Result),
                                    GetEntitySystemId(Result)));
      end;
    ENTITY_NDATA :
      begin
        FHasExternals := True;
        if Assigned(FOnNonXMLEntity) then
          FOnNonXMLEntity(self,
                          Result,
                          GetEntityPublicId(Result),
                          GetEntitySystemId(Result),
                          GetEntityNotationName(Result));
      end;
  end;
end;
{--------}
procedure TXpParser.ParseEnumeration;
var
  sValue : DOMString;
begin
  DataBufferAppend('(');
  SkipWhitespace(True);                                                {!!.51}
  if TryRead(Xpc_ParamEntity) then begin                               {!!.51}{!!.52}
    ParseParameterEntityRef(True, False);                              {!!.51}
    SkipWhitespace(True);                                              {!!.52}
  end;                                                                 {!!.52}
  sValue := ReadNameToken(False);
  FAttrEnum.Add(sValue);
  DataBufferAppend(sValue);
  SkipWhitespace(True);                                                {!!.51}
  while not TryRead(Xpc_ParenRight) do begin
    Require(Xpc_ListOperator);
    DataBufferAppend('|');
    SkipWhitespace(True);                                              {!!.51}
    if TryRead(Xpc_ParamEntity) then                                   {!!.51}
      ParseParameterEntityRef(True, False);                            {!!.51}
    sValue := ReadNameToken(False);
    DataBufferAppend(sValue);
    FAttrEnum.Add(sValue);
    SkipWhitespace(True);                                              {!!.51}
{Begin !!.55}
    while TryRead(Xpc_ParamEntity) do begin
      ParseParameterEntityRef(True, False);
      SkipWhitespace(True);
    end;
{End !!.55}
  end;
  DataBufferAppend(')');
end;
{--------}
procedure TXpParser.ParseEq;
begin
  SkipWhitespace(True);                                                {!!.51}
  Require(Xpc_Equation);
  SkipWhitespace(True);                                                {!!.51}
end;
{Begin !!.52}
{--------}
procedure TXpParser.ParseIgnore;
var
  bAdded      : Boolean;
  bSavPreserve : Boolean;
  cTempChar   : DOMChar;
  sTempBuff   : DOMString;
  wBuffLength : Integer;
  wPos : Integer;
  wLevel      : Integer;
  wLine, wLinePos : Longint;
  wSavElementContent : Integer;
begin
  { Strategy: Scan through the conditional ignore, looking for a
    corresponding conditional end (i.e., ]]>). Take into account
    nested conditional sections (all are to be ignored). Put all
    characters into a buffer. The characters will be raised in
    an OnCharData event. }
  bAdded := False;
  bSavPreserve := FPreserve;
  FPreserve := True;
  wSavElementContent := FCurrentElementContent;
  FCurrentElementContent := CONTENT_MIXED;
  wLevel := 0;
  { Preserve the current line settings. }
  wLine := FFilter.Line;
  wLinePos := FFilter.LinePos;
  try
    { Pre-size the buffer used to store the ignored characters. }
    wPos := 1;
    wBuffLength := 50;
    SetLength(sTempBuff, wBuffLength);
    while True do begin
      cTempChar := ReadChar(False);
      if IsEndDocument then
        raise EXpParserError.CreateError(wLine,
                                         wLinePos,
                                         FFilter.URL,
                                         sMalformedIgnore)
      else if (cTempChar = '<') and
              TryRead(Xpc_ConditionalStart) then begin
        inc(wLevel);
        if ((wPos + 3) > wBuffLength) then begin
          wBuffLength := wBuffLength * 2;
          SetLength(sTempBuff, wBuffLength);
        end;
        sTempBuff[wPos] := '<';
        sTempBuff[wPos + 1] := '!';
        sTempBuff[wPos + 2] := '[';
        Inc(wPos, 3);
        bAdded := True;
      end
      else if (cTempChar = ']') and TryRead(Xpc_ConditionalEnd) then begin
        if ((wPos + 3) > wBuffLength) then begin
          wBuffLength := wBuffLength * 2;
          SetLength(sTempBuff, wBuffLength);
        end;
        sTempBuff[wPos] := ']';
        sTempBuff[wPos + 1] := ']';
        sTempBuff[wPos + 2] := '>';
        Inc(wPos, 3);
        bAdded := True;
        if wLevel = 0 then
          Break
        else
          dec(wLevel);
      end
      else begin
        if ((wPos + 1) > wBuffLength) then begin
          wBuffLength := wBuffLength * 2;
          SetLength(sTempBuff, wBuffLength);
        end;
        sTempBuff[wPos] := cTempChar;
        Inc(wPos);
        SkipChar;
        bAdded := True;
      end;
    end;  { while }
    if bAdded then begin
      SetLength(sTempBuff, wPos);
      DataBufferAppend(sTempBuff);
      DataBufferFlush;
    end;
  finally
    FPreserve := bSavPreserve;
    FCurrentElementContent := wSavElementContent;
  end;
end;
{End !!.52}
{Begin !!.52}
{--------}
function TXpParser.ParseInContext(oContext : TXpBaseDirContext;
                            const sSource : string) : Boolean;
var
  sSavCurDir : string;
begin
  sSavCurDir := GetCurrentDir;
  try
    FErrors.Clear;
    FIsStandAlone := False;
    FHasExternals := False;
    Result := LoadDataSource(oContext, sSource, FUserName, FPassword, FErrors);
    if Result then begin
//    FFilter.FreeStream := True;                                      {!!.53}
      FDocLocation := FFilter.DirContext.Dir;                          {!!.52}
      FDocName := FFilter.DirContext.Filename;                         {!!.52}
      ParsePrim;
    end
    else
      FErrors.Add(sSrcLoadFailed + sSource);
    Result := (FErrors.Count = 0);
  finally
    SetCurrentDir(sSavCurDir);
  end;
end;
{End !!.52}
{--------}
procedure TXpParser.ParseMarkupDecl(aIsInternal : Boolean);
var
  sName,
  sFile,
  cpData : DOMString;
begin
  if TryRead(Xpc_DTDElement) then
    ParseElementDecl(aIsInternal)
  else if TryRead(Xpc_DTDAttlist) then
    ParseAttlistDecl
  else if TryRead(Xpc_DTDEntity) then
    ParseEntityDecl
  else if TryRead(Xpc_DTDNotation) then
    ParseNotationDecl
  else if TryRead(Xpc_ProcessInstrStart) then
    ParsePI
  else if TryRead(Xpc_CommentStart) then
    ParseComment
  else if TryRead(Xpc_ConditionalStart) then
    if (not aIsInternal) then
      ParseConditionalSect
    else
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sNoIntConditional)
  else if TryRead(Xpc_ParamEntity) then begin
    // Parse external DTD
    sName := ReadNameToken(True);
    Require(Xpc_GenParsedEntityEnd);
    SkipWhiteSpace(True);                                              {!!.51}
    case GetEntityType(sName, True) of
      ENTITY_UNDECLARED :
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sUndeclaredEntity + sName);
      ENTITY_TEXT : sFile := GetEntitySystemId(sName);
    else
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sUndeclaredEntity + sName);
    end;

    if sFile <> '' then begin
      if Assigned(FOnDTDExternalStart) then
        FOnDTDExternalStart(self, sFile);
      FXMLDecParsed := False;                                          {!!.52}
      cpData := GetExternalTextEntityValue(sName,
                                           GetEntityPublicId(sName),
                                           GetEntitySystemId(sName));
      while True do begin
        FContext := CONTEXT_DTD;
        SkipWhitespace(False);                                         {!!.51}
        if FFilter.isEOF then begin                                    {!!.52}
          FContext := CONTEXT_NONE;
          Break;
        end;
        ParseMarkupDecl(aIsInternal);
        FContext := CONTEXT_NONE;
        if (FFilter.isEof) or                                          {!!.52}
           (FDocStack.Count = 0) then                                  {!!.52}
          Break;
      end;
      if Assigned(FOnDTDExternalEnd) then
        FOnDTDExternalEnd(self);
      FXMLDecParsed := True;                                           {!!.52}
    end;
  end else
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sExpMarkupDecl +
                                     ReadNameToken(False));
end;
{--------}
function TXpParser.ParseMemory(var aBuffer; aSize : Longint) : Boolean;
var
  MemStream  : TXpMemoryStream;
begin
  Assert(not Assigned(FFilter));

  FErrors.Clear;
  FPreserve := False;
  FIsStandAlone := False;
  FHasExternals := False;

  MemStream := TXpMemoryStream.Create;
  try
    Memstream.SetPointer(@aBuffer, aSize);
    if (FFilter <> nil) then                                           {!!.55}
      FFilter.Free;                                                    {!!.55}
    FFilter := TXpInCharFilter.Create(MemStream, BufferSize);
    FFilter.FreeStream := False;                                       {!!.53}
    FDocLocation := '';                                                {!!.52}
    FDocName := '';                                                    {!!.52}
    ParsePrim;                                                         {!!.52}
  finally
    MemStream.Free;
  end;

  Result := FErrors.Count = 0;
end;
{--------}
procedure TXpParser.ParseMisc;
var
  ParsedComment : Boolean;
begin
  ParsedComment := False;
  while True do begin
    SkipWhitespace(True);                                              {!!.51}
    if TryRead(Xpc_ProcessInstrStart) then begin
      if ParsePIEx and ParsedComment then
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sCommentBeforeXMLDecl)
      else
        FXMLDecParsed := True;
    end else if TryRead(Xpc_CommentStart) then begin
      FXMLDecParsed := True;
      ParsedComment := True;
      ParseComment;
    end else
      Exit;
  end;
end;
{--------}
procedure TXpParser.ParseMixed(var wOccur : Integer);                  {!!.51}
var
  sName : DOMString;
begin
  SkipWhitespace(True);                                                {!!.51}
  if TryRead(Xpc_ParenRight) then begin
    DataBufferAppend(')');                                             {!!.51}
    if TryRead(Xpc_OneOrMoreOpr) then begin                            {!!.51}
      wOccur := OCCURS_OPT_REPEAT;                                     {!!.51}
      DataBufferAppend('*');
    end;                                                               {!!.51}
    Exit;
  end;

  SkipWhitespace(True);                                                {!!.51}
{Start !!.55}
  while TryRead(Xpc_ParamEntity) do begin
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);
  end;
{End !!.55}
  while not TryRead(Xpc_ParenRight) do begin                           {!!.51}
    Require(Xpc_ListOperator);
    DataBufferAppend('|');
    SkipWhitespace(True);                                              {!!.51}
    while TryRead(Xpc_ParamEntity) do begin                            {!!.52 - Start}
      ParseParameterEntityRef(True, False);
      SkipWhiteSpace(True);
    end;                                                               {!!.52 - End}
    sName := ReadNameToken(True);
    if Assigned(FOnDTDElementContent) then
      FOnDTDElementContent(self, sName, OCCURS_REQ_NOREPEAT, REL_OR);
    DataBufferAppend(sName);
    SkipWhitespace(True);                                              {!!.51}
{Start !!.55}
    while TryRead(Xpc_ParamEntity) do begin
      ParseParameterEntityRef(True, False);
      SkipWhiteSpace(True);
    end;
{End !!.55}
  end;
{Begin !!.51}
  DataBufferAppend(')');
  if TryRead(Xpc_OneorMoreOpr) then begin
    wOccur := OCCURS_OPT_REPEAT;
    DataBufferAppend('*');
  end;
{End !!.51}
end;
{--------}
procedure TXpParser.ParseNotationDecl;
var
  sNotName : DOMString;
  sIds     : StringIds;
begin
  RequireWhitespace;
  sNotName := ReadNameToken(True);
  RequireWhitespace;
  ReadExternalIds(True, sIds);
  if (sIds[0] = '') and (sIds[1] = '') then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sExtModifMissing + sNotName);
  SetNotation(sNotName, sIds[0], sIds[1]);
  SkipWhitespace(True);                                                {!!.51}
  Require(Xpc_BracketAngleRight);
end;
{--------}
procedure TXpParser.ParseNotationType;
begin
  RequireWhitespace;
  Require(Xpc_ParenLeft);
  ParseEnumeration;
end;
{--------}
function TXpParser.ParseParameterEntityRef(aPEAllowed : Boolean;
                                           bSkip      : Boolean)
                                                      : DOMString;
var
  sName,
  sValue : DOMString;
begin
  sName := ReadNameToken(True);
  Require(Xpc_GenParsedEntityEnd);
  case GetEntityType(sName, aPEAllowed) of
    ENTITY_UNDECLARED :
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos - 3,
                                       FFilter.URL,
                                       sUndeclaredEntity + sName);
    ENTITY_INTERNAL :
      begin
        sValue := GetEntityValue(sName, aPEAllowed);
        if bSkip then
          DataBufferAppend(sValue)
        else
          PushString(sValue);
        Result := sValue;
      end;
    ENTITY_TEXT :
      begin
        sValue := GetExternalTextEntityValue(sName,
                                             GetEntityPublicId(sName),
                                             GetEntitySystemId(sName));
        if bSkip then
          DataBufferAppend(sValue);
        Result := sValue;
      end;
    ENTITY_NDATA :
      begin
        FHasExternals := True;
        if Assigned(FOnNonXMLEntity) then
          FOnNonXMLEntity(self,
                          sName,
                          GetEntityPublicId(sName),
                          GetEntitySystemId(sName),
                          GetEntityNotationName(sName));
      end;
  end;
end;
{--------}
procedure TXpParser.ParsePCData(aInEntityRef : Boolean);               {!!.52 - Rewritten - Start}
var
  TempBuff   : DOMString;
  TempChar   : DOMChar;
  CurrLength : Longint;
  BuffLength : Longint;
  Added      : Boolean;
begin
  Added := False;
  CurrLength := 0;
  BuffLength := 50;
  SetLength(TempBuff, BuffLength);
  while True do begin
    TempChar := ReadChar(False);
    if (TempChar = '<') or
       (TempChar = '&') then
      Break
    else if (FFilter.isEOF) then begin
      if IsEndDocument then
        Break;
    end
    else begin
      if ((CurrLength + 2) > BuffLength) then begin
        BuffLength := BuffLength * 2;
        SetLength(TempBuff, BuffLength);
      end;
      inc(CurrLength);
      TempBuff[CurrLength] := TempChar;
      SkipChar;
      Added := True;
    end;
  end;
  if Added then begin
    SetLength(TempBuff, CurrLength);
    ValidatePCData(TempBuff, aInEntityRef);
    DataBufferAppend(TempBuff);
  end;
end;                                                                   {!!.52 - Rewritten - End}
{--------}
procedure TXpParser.ParsePI;
begin
  ParsePIEx;
end;
{--------}
function TXpParser.ParsePIEx : Boolean;
var
  sName : DOMString;
begin
  Result := False;
  sName := ReadNameToken(True);
  if sName <> 'xml' then begin
    FXMLDecParsed := True;
    if not TryRead(Xpc_ProcessInstrEnd) then begin
      RequireWhitespace;
      ParseUntil(Xpc_ProcessInstrEnd);
    end;
  end else begin
    Result := True;
    ParseXMLDeclaration;
  end;
  if Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(self, sName, DataBufferToString(True))    {!!.55}
  else
    DataBufferToString(True);                                          {!!.55}
end;
{Begin !!.52}
{--------}
procedure TXpParser.ParsePrim;
begin
  try
    Initialize;

    if Assigned(FOnStartDocument) then
      FOnStartDocument(self);

    try
      ParseDocument;
    except
      on E: EXpFilterError do begin
{Begin !!.53}
        if E is EXpParserError then
          with EXpParserError(E) do
            FErrors.Add(Format(sFmtErrorMsg,
                               [URL, Line, LinePos, Message]))
        else
          FErrors.Add(Format(sFmtErrorMsg,
                             ['<unknown>', E.Line, E.LinePos, E.Message]));
{End !!.53}
        if FRaiseErrors then begin
          if Assigned(FOnEndDocument) then
            FOnEndDocument(self);
          Cleanup;
          raise;
        end;
      end;
    end;

    if Assigned(FOnEndDocument) then
      FOnEndDocument(self);

    Cleanup;
  finally
    FInCharSet := MapStreamFormatToCharEnc(FFilter.Format);
    FFilter.Free;
    FFilter := nil;
  end;
end;
{End !!.52}
{--------}
procedure TXpParser.ParseProlog;
begin
  ParseMisc;
  if TryRead(Xpc_DTDDocType) then begin
    FXMLDecParsed := True;
    ParseDocTypeDecl;
    ParseMisc;
  end;
end;
{Begin !!.52}
{--------}
function TXpParser.ParseStream(oStream : TStream) : Boolean;
begin
  Assert(not Assigned(FFilter));

  FErrors.Clear;
  FPreserve := False;
  FIsStandAlone := False;
  FHasExternals := False;

  if (FFilter <> nil) then                                             {!!.55}
    FFilter.Free;                                                      {!!.55}
  FFilter := TXpInCharFilter.Create(oStream, FBufferSize);             {!!.55}
  FFilter.FreeStream := False;                                         {!!.53}
  FDocLocation := '';
  FDocName := '';
  ParsePrim;
  Result := FErrors.Count = 0;
end;
{End !!.52}
{--------}
procedure TXpParser.ParseUntil(const S : array of Longint);            {!!.55 - Rewritten}
var
  TempStr      : DOMString;
  BuffCapacity : Integer;
  BuffSize     : Integer;
  i            : Integer;
  Found        : Boolean;
begin
  Found := TryRead(s);
  if (not Found) then begin
    { Setup a buffer to hold the data between this point and what we're
      looking for. Initializing the buffer size and assigning
      charactersis to each position in the buffer is much faster than
      multiple calls to DataBufferAppend -- prevents memory manager
      abuse.}
    BuffCapacity := 1024;
    BuffSize := 0;
    SetLength(TempStr, BuffCapacity);
    while ((not Found) and
           (not FFilter.isEOF)) do begin
      if (BuffSize >= BuffCapacity) then begin
        { If we've reached the capacity of our buffer, we need to grow
          it, so we're doubling it.}
        BuffCapacity := BuffCapacity * 2;
        SetLength(TempStr, BuffCapacity);
      end;
      BuffSize := BuffSize + 1;
      TempStr[BuffSize] := ReadChar(True);
      Found := TryRead(s);
    end;
    { Set the buffer to its actual size and append it to the data
      buffer.}
    SetLength(TempStr, BuffSize);
    DataBufferAppend(TempStr);
    if (not Found) then begin
      {$IFDEF DCC4OrLater}
      SetLength(TempStr, Length(S));
      {$ENDIF}
      for i := 0 to High(S) do
        TempStr[Succ(i)] := DOMChar(S[i]);
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sUnexpEndOfInput +
                                       QuotedStr(TempStr));
    end;
  end;
end;                                                                   {!!.55 - End rewritten}
{--------}
procedure TXpParser.ParseWhitespace;
var
  TempChar : DOMChar;                                                  {!!.52}
begin
  TempChar := ReadChar(False);
  while IsWhitespace(TempChar) do begin
    SkipChar;                                                          {!!.52}
    DataBufferAppend(TempChar);
    TempChar := ReadChar(False);
    if (TempChar = DOMChar(XpEndOfStream)) then                        {!!.56 - Start}
      if (not IsEndDocument) then begin
        TempChar := ReadChar(False);
      end;                                                             {!!.56 - End}
  end;
end;
{--------}
procedure TXpParser.ParseXMLDeclaration;
var
  sValue    : DOMString;
  Buffer    : DOMString;
  {TempChar  : DOMString;}                                             {!!.51}
  HasEntRef : Boolean;
begin
  if FXMLDecParsed then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sXMLDecNotAtBeg);
  HasEntRef := False;
  SkipWhitespace(True);                                                {!!.51}
  Require(Xpc_Version);
  DatabufferAppend('version');
  ParseEq;
  DatabufferAppend('="');
  Buffer := DatabufferToString(True);                                  {!!.55}
  sValue := ReadLiteral(0, HasEntRef);
  ValidateVersNum(sValue);
  Buffer := Buffer + sValue + '"';
  if (sValue <> XpXMLSpecification) then                               {!!.51}
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     Format(sInvalidXMLVersion,
                                            [XpXMLSpecification]));

  if ReadChar(False) = '?' then
    SkipWhitespace(True)                                               {!!.53}
  else                                                                 {!!.53}
    RequireWhitespace;                                                 {!!.53}
  {if IsWhiteSpace(ReadChar(False)) then begin}                        {!!.51}
   { TempChar := ReadChar(True);}                                      {!!.51}
    if TryRead(Xpc_Encoding) then begin
      DatabufferAppend('encoding');
      ParseEq;
      DataBufferAppend('="');
      Buffer := Buffer + ' ' + DataBufferToString(True);               {!!.55}
      sValue := ReadLiteral(LIT_CHAR_REF or
                            LIT_ENTITY_REF,
                            HasEntRef);
      ValidateEncName(sValue);
      Buffer := Buffer + sValue + '"';
      if CompareText(sValue, 'ISO-8859-1') = 0 then
        FFilter.Format := sfISO88591;
  {end else}                                                           {!!.51}
    {PushString(TempChar);}                                            {!!.51}
      SkipWhitespace(True);                                            {!!.51}
  end;

  {if IsWhiteSpace(ReadChar(False)) then begin}                        {!!.51}
    {TempChar := ReadChar(True);}                                      {!!.51}
    if TryRead(Xpc_Standalone) then begin
      DatabufferAppend('standalone');
      ParseEq;
      DatabufferAppend('="');
      Buffer := Buffer + ' ' + DataBufferToString(True);               {!!.55}
      sValue := ReadLiteral(LIT_CHAR_REF or
                            LIT_ENTITY_REF,
                            HasEntRef);
      if (not ((sValue = 'yes') or
               (sValue = 'no'))) then
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sInvStandAloneVal);
      Buffer := Buffer + sValue + '"';
      FIsStandalone := sValue = 'yes';
  {end else}                                                           {!!.51}
    {PushString(TempChar);}                                            {!!.51}
      SkipWhitespace(True)                                             {!!.51}
  end;

  {SkipWhiteSpace;}                                                    {!!.51}
  Require(Xpc_ProcessInstrEnd);
  DatabufferToString(True);                                            {!!.55}
  DatabufferAppend(Buffer);
end;
{--------}
procedure TXpParser.PopDocument;
begin
  Assert(FDocStack.Count > 0);

  if FDocStack.Count > 0 then begin
    if (FFilter <> nil) then                                           {!!.55}
      FFilter.Free;                                                    {!!.55}
    FFilter := FDocStack[Pred(FDocStack.Count)];
//    if FFilter.DirContext <> nil then                                {!!.52}{Deleted !!.54}
//      SetDirContext(FFilter.DirContext);                             {!!.52}{Deleted !!.54}
    FDocStack.Delete(Pred(FDocStack.Count));
  end;
end;
{--------}
procedure TXpParser.PushDocument;
begin
  Assert(Assigned(FFilter));

  FDocStack.Add(Pointer(FFilter));
  FFilter := nil;
end;
{--------}
procedure TXpParser.PushString(const sVal : DOMString);
var
  MemStream  : TXpMemoryStream;
  TmpStr : DOMString;
begin
  if Length(sVal) > 0 then begin
    PushDocument;
    MemStream := TXpMemoryStream.Create;
    { Force UTF-16 LE BOM. }
    TmpStr := DOMString(#$FEFF) + sVal;                                {!!.53}
    MemStream.Write(TmpStr[1], Length(TmpStr) * 2);
    MemStream.Position := 0;
    FFilter := TXpInCharFilter.Create(MemStream, BufferSize);
  end;
end;
{--------}
function TXpParser.ReadAttType : Integer;
var
  sTypeString : DOMString;
  wType       : Integer;
begin
  if TryRead(Xpc_ParamEntity) then begin                               {!!.52 - Start}
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);
  end;                                                                 {!!.52 - End}
  if TryRead(Xpc_ParenLeft) then begin
    ParseEnumeration;
    Result := ATTRIBUTE_ENUMERATED;
  end else begin
    sTypeString := ReadNameToken(True);
    if sTypeString = 'NOTATION' then
      ParseNotationType;
    wType := FAttributeType.IndexOf(sTypeString);
    if wType = -1 then begin
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sIllAttrType + '(' + sTypeString +
                                       ') in element ' + FCurrentElement);
    end else
      Result := Integer(FAttributeType.Objects[wType]);
  end;
end;
{--------}
function TXpParser.ReadChar(const UpdatePos : Boolean) : DOMChar;      {!!.52 - Rewritten - Start}
begin
  Result := FFilter.ReadChar;
  if ((Result = DOMChar(XpEndOfStream)) and                            {!!.55}
      UpdatePos and
      (not IsEndDocument)) then
    Result := FFilter.ReadChar;
  if (UpdatePos) then
    FFilter.SkipChar;
end;                                                                   {!!.52 - Rewritten - End}
{--------}
procedure TXpParser.ReadExternalIds(bInNotation : Boolean;
                                var sIds        : StringIds);
var
  HasEntRef : Boolean;
  TempChar  : DOMChar;                                                 {!!.51}{!!.52}
begin
  HasEntRef := False;
  if TryRead(Xpc_ExternalPublic) then begin
    RequireWhitespace;
    sIds[0] := ReadLiteral(LIT_NORMALIZE, HasEntRef);
    ValidatePublicID(sIds[0]);
    if bInNotation then begin
      SkipWhitespace(True);                                            {!!.51 - Start}
      TempChar := ReadChar(False);                                     {!!.52}
      if (TempChar = '''') or
         (TempChar = '"') then                                         {!!.51 - End}
        sIds[1] := ReadLiteral(0, HasEntRef);
    end else begin
      RequireWhitespace;
      sIds[1] := ReadLiteral(0, HasEntRef);
    end;
  end else if TryRead(Xpc_ExternalSystem) then begin
    RequireWhitespace;
    sIds[1] := ReadLiteral(0, HasEntRef);
  end;
end;
{--------}
function TXpParser.ReadLiteral(wFlags    : Integer;
                           var HasEntRef : Boolean) : DOMString;
var
  TempStr     : DOMString;                                             {!!.52}
  cDelim,
  TempChar    : DOMChar;                                               {!!.52}
  EntRefs     : TStringList;
  StackLevel  : Integer;                                               {!!.51}
  QuoteRef,                                                            {!!.53}
  CurrCharRef : Boolean;                                               {!!.51}
begin
  StackLevel := 0;                                                     {!!.51}
  CurrCharRef := False;                                                {!!.51}
  QuoteRef := False;                                                   {!!.53}
  Result := '';
  EntRefs := nil;
  try                                                                  {!!.55}
    cDelim := ReadChar(True);
    if (cDelim <> '"') and
       (cDelim <> #39) and
       (cDelim <> #126) and
       (cDelim <> #0) then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sQuoteExpected);
    TempChar := ReadChar(False);
    while (not IsEndDocument) and                                      {!!.51 - Start}
          (CurrCharRef or                                              {!!.53}
           QuoteRef or                                                 {!!.53}
           (TempChar <> cDelim)) do begin                              {!!.51 - End}
      if (TempChar = #$0A) then begin
        TempChar := ' ';
      end else if (TempChar = #$0D) then
        TempChar := ' '
      else if (TempChar = '&') then begin
        if wFlags and LIT_CHAR_REF <> 0 then begin
          if wFlags and LIT_ENTITY_REF <> 0 then
            CurrCharRef := True;                                       {!!.51}
          HasEntRef := True;
          SkipChar;                                                    {!!.52}
          TempChar := ReadChar(False);
          if TempChar = '#' then begin
            SkipChar;                                                  {!!.52}
            ParseCharRef;                                              {!!.51}
            TempChar := ReadChar(False);                               {!!.51}
            if (TempChar = DOMChar(XpEndOfStream)) then                {!!.52}{!!.55}
              TempChar := ReadChar(True);                              {!!.52}
            CurrCharRef := False;
            Continue;
          end else if wFlags and LIT_ENTITY_REF <> 0 then begin
            TempStr := ParseEntityRef(False);                          {!!.51 - Start}{!!.52}
            if (TempStr <> 'lt') and                                   {!!.52 - Start}
               (TempStr <> 'gt') and
               (TempStr <> 'amp') and
               (TempStr <> 'apos') and
               (TempStr <> 'quot') then begin                          {!!.52 - End}
              if (not Assigned(EntRefs)) then begin
                EntRefs := TStringList.Create;
                EntRefs.Sorted := True;
                EntRefs.Duplicates := dupError;
                StackLevel := FDocStack.Count;
              end else
                StackLevel := Succ(FDocStack.Count);
              try
                if FDocStack.Count = StackLevel then begin
                  EntRefs.Clear;
                  StackLevel := FDocStack.Count;
                end;
                EntRefs.Add(TempStr);                                  {!!.52}
              except
                on E:EStringListError do begin
                  EntRefs.Free;
                  EntRefs := nil;                                      {!!.55}
                  raise EXpParserError.CreateError(FFilter.Line,
                                                   FFilter.LinePos,
                                                   FFilter.URL,
                                                   sCircularEntRef +
                                                   TempChar);
                end;
                on E:EXpParserError do
                  raise;
              end;
            end else
              HasEntRef := False;
            {if not HasEntRef then}                                    {!!.51 - End}
            if wFlags and LIT_CHAR_REF <> 0 then
            TempChar := ReadChar(False);
            QuoteRef := (TempChar = cDelim);                           {!!.53}
            Continue;
          end else if wFlags and LIT_PE_REF <> 0 then begin
            ParseParameterEntityRef(False, True);
            Continue;
          end else begin                                               {!!.53}
            DataBufferAppend('&');                                     {!!.53}
            if (not Assigned(EntRefs)) then begin
              StackLevel := FDocStack.Count;                           {!!.51}
              EntRefs := TStringList.Create;
              EntRefs.Sorted := True;
              EntRefs.Duplicates := dupError;
              {EntRefs.Add('&' + TempChar);}                           {!!.52}
            end;                                                       {!!.51 - Start}
            try
              if StackLevel = FDocStack.Count then begin
                EntRefs.Clear;
                StackLevel := FDocStack.Count;
              end;                                                     {!!.51 - End}
              EntRefs.Add('&' + DOMString(TempChar));                  {!!.52}
            except
              on E:EStringListError do begin
                EntRefs.Free;
                EntRefs := nil;                                        {!!.05}
                raise EXpParserError.CreateError(FFilter.Line,
                                                 FFilter.LinePos,
                                                 FFilter.URL,
                                                 sCircularEntRef +
                                                 TempChar);
              end;
              on E:EXpParserError do
                raise;
            end;
          end;                                                         {!!.53}
        end;
      end;
      DataBufferAppend(TempChar);
      SkipChar;                                                        {!!.52}
      TempChar := ReadChar(False);                                     {!!.55 - Start}
      QuoteRef := False;                                               {!!.53}
      if (TempChar = DOMChar(XpEndOfStream)) then begin                {!!.55}
        IsEndDocument;
        TempChar := ReadChar(False);
      end;                                                             {!!.55 - End}
      CurrCharRef := False;                                            {!!.51}
    end;
    if TempChar <> cDelim then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       'Expected: ' + cDelim);

    SkipChar;                                                          {!!.52}

    if wFlags and LIT_NORMALIZE <> 0 then
      DataBufferNormalize;

    Result := DataBufferToString(True);                                {!!.55}
  finally                                                              {!!.55}
    EntRefs.Free;                                                      {!!.55}
  end;                                                                 {!!.55}
end;
{--------}
function TXpParser.ReadNameToken(aValFirst : Boolean) : DOMString;
var
  TempChar : DOMChar;                                                  {!!.52}
  First    : Boolean;
  ResultLen : Integer;                                                 {!!.52}
  CurrLen   : Integer;                                                 {!!.52}
begin
  if TryRead(Xpc_ParamEntity) then begin                               {!!.52 - Start}
    ParseParameterEntityRef(True, False);
    SkipWhiteSpace(True);
  end;                                                                 {!!.52 - End}
  First := aValFirst;
  Result := '';
  CurrLen := 0;                                                        {!!.52 - Start}
  ResultLen := 20;
  SetLength(Result, ResultLen);                                        {!!.52 - End}
  while True do begin
    TempChar := ReadChar(False);
    if (TempChar = '%') or (TempChar = '<') or (TempChar = '>') or
       (TempChar = '&') or (TempChar = ',') or (TempChar = '|') or
       (TempChar = '*') or (TempChar = '+') or (TempChar = '?') or
       (TempChar = ')') or (TempChar = '=') or (TempChar = #39) or
       (TempChar = '"') or (TempChar = '[') or (TempChar = ' ') or
       (TempChar = #9) or (TempChar = #$0A) or (TempChar = #$0D) or
       (TempChar = ';') or (TempChar = '/') or (TempChar = '') or      {!!.51}{!!.52}
       (TempChar = #1) then                                            {!!.52}
      Break
    else
      if XpValidNameChar(First, TempChar) then begin                   {!!.52 - Start}
        if (CurrLen + 2 > ResultLen) then begin
          ResultLen := ResultLen * 2;
          SetLength(Result, ResultLen);
        end;
        SkipChar;
        Inc(CurrLen);
        Result[CurrLen] := TempChar;
      end else                                                         {!!.52 - End}
        raise EXpParserError.CreateError(FFilter.Line,
                                         FFilter.LinePos,
                                         FFilter.URL,
                                         sInvalidName +
                                         QuotedStr(TempChar));
    First := False;
  end;
  SetLength(Result, CurrLen);                                          {!!.52}
end;
{--------}
procedure TXpParser.Require(const S : array of Longint);
var
  TempStr  : AnsiString;
  TempChar : AnsiChar;
  i        : Integer;
begin
  if not TryRead(S) then begin
    SetLength(TempStr, High(S) + 1);
    for i := 0 to High(S) do begin
      XPUcs4ToIso88591(s[i], TempChar);
      TempStr[i + 1] := TempChar;
    end;
    if ReadChar(False) = '&' then begin
      SkipChar;                                                        {!!.52}
      if ReadChar(False) = '#' then begin
        SkipChar;                                                      {!!.52}
        if ParseCharRef = TempStr then
          Exit;
      end;
    end;
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sExpectedString +
                                     QuotedStr(TempStr));
  end;
end;
{--------}
procedure TXpParser.RequireWhitespace;
begin
  IsEndDocument;                                                       {!!.52}
  if IsWhitespace(ReadChar(False)) then
    SkipWhitespace(True)                                               {!!.51}
  else
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sSpaceExpectedAt +
                                       'Line: ' + IntToStr(FFilter.Line) +
                                       ' Position: ' + IntToStr(FFilter.LinePos));
end;
{--------}
procedure TXpParser.SetAttribute(const sElemName, sName : DOMString;   {!!.52}
                                       wType            : Integer;
                                 const sEnum, sValue    : DOMString;   {!!.52}
                                       wValueType       : Integer);
var
  wIdx      : Integer;
  oElemInfo : TXpElementInfo;
  oAttrInfo : TXpAttributeInfo;
begin
  wIdx := GetElementIndexOf(sElemName);
  if wIdx < 0 then                                                     {!!.55}
    wIdx := SetElement(sElemName, CONTENT_UNDECLARED, '');             {!!.55}

  oElemInfo := TXpElementInfo(FElementInfo.Objects[wIdx]);
  oAttrInfo := TXpAttributeInfo.Create;
  oAttrInfo.AttrType := wType;
  oAttrInfo.Value := sValue;
  oAttrInfo.ValueType := wValueType;
  oAttrInfo.Enum := sEnum;
  oElemInfo.SetAttribute(sName, oAttrInfo);
end;
{Begin !!.52}
{Deleted !!.54}
{--------}
//procedure TXpParser.SetDirContext(oContext : TXpBaseDirContext);
//begin
//  case oContext.ContextType of
//    xpdcLocal :
//      SetCurrentDir(oContext.Dir);
//  end;
//end;
{End !!.52}
{--------}
function TXpParser.SetElement(const sName         : DOMString;         {!!.55}
                                    wType         : Integer;
                              const sContentModel : DOMString)
                                                  : Integer;           {!!.55}

var
  oElem : TXpElementInfo;
//  wIdx  : Integer;                                                   {!!.55}
begin
  Result := GetElementIndexOf(sName);                                  {!!.55}
  if Result < 0 then begin                                             {!!.55}
    oElem := TXpElementInfo.Create;
    Result := FElementInfo.AddObject(sName, oElem);                    {!!.55}
  end else
    oElem := TXpElementInfo(FElementInfo.Objects[Result]);             {!!.55}

  if wType <> CONTENT_UNDECLARED then
    oElem.ContentType := wType;

  if sContentModel <> '' then
    oElem.ContentModel := sContentModel;
end;
{--------}
procedure TXpParser.SetEntity(const sEntityName   : DOMString;
                                    wClass        : Integer;
                              const sPublicId,
                                    sSystemId,
                                    sValue,
                                    sNotationName : DOMString;
                                    aIsPE         : Boolean);
var
  wIdx    : Integer;
  oEntity : TXpEntityInfo;
begin
  wIdx := GetEntityIndexOf(sEntityName, aIsPE);
  if wIdx < 0 then begin
    oEntity := TXpEntityInfo.Create;
    oEntity.EntityClass := wClass;
    oEntity.PublicId := sPublicId;
    oEntity.SystemId := sSystemId;
    oEntity.Value := sValue;
    oEntity.NotationName := sNotationName;
    oEntity.IsParameterEntity := aIsPE;

    FEntityInfo.AddObject(sEntityName, oEntity);
  end;
end;
{--------}
procedure TXpParser.SetExternalDataEntity(const sName,
                                                sPublicId,
                                                sSystemId,
                                                sNotationName
                                                : DOMString);
begin
  SetEntity(sName,
            ENTITY_NDATA,
            sPublicId,
            sSystemId,
            '',
            sNotationName,
            False);
end;
{--------}
procedure TXpParser.SetExternalTextEntity(const sName,
                                                sPublicId,
                                                sSystemId : DOMString);
begin
  SetEntity(sName, ENTITY_TEXT, sPublicId, sSystemId, '', '', False);
end;
{--------}
procedure TXpParser.SetInternalEntity(const sName, sValue : DOMString;
                                            aIsPE         : Boolean);
begin
  SetEntity(sName, ENTITY_INTERNAL, '', '', sValue, '', aIsPE);
end;
{--------}
procedure TXpParser.SetNotation(const sNotationName,
                                      sPublicId,
                                      sSystemId     : DOMString);
var
  oNot : TXpNotationInfo;
  wIdx : Integer;
begin
  if not FNotationInfo.Find(sNotationName, wIdx) then begin
    oNot := TXpNotationInfo.Create;
    oNot.PublicId := sPublicId;
    oNot.SystemId := sSystemId;

    { Send notation declaration event }
    if Assigned(FOnDTDNotation) then
      FOnDTDNotation(self, sNotationName, sPublicId, sSystemId);

    FNotationInfo.AddObject(sNotationName, oNot);
  end;
end;
{--------}
procedure TXpParser.SkipChar;                                          {!!.52 - Added - Start}
begin
  FFilter.SkipChar;
end;                                                                   {!!.52 - Added - End}
{--------}
procedure TXpParser.SkipWhitespace(aNextDoc : Boolean);                {!!.51}
begin
  while (not FFilter.isEof) and                                        {!!.52}
        (IsWhitespace(ReadChar(False))) do
    SkipChar;                                                          {!!.52}
 if aNextDoc then begin                                                {!!.51}{!!.52}
   IsEndDocument;                                                      {!!.51}
   while (not FFilter.isEof) and                                       {!!.51}{!!.52}
         (IsWhitespace(ReadChar(False))) do                            {!!.51}
     SkipChar;                                                         {!!.51}{!!.52}
 end;                                                                  {!!.52}
end;
{--------}
function TXpParser.TryRead(const S : array of Longint) : Boolean;
begin
  Result := False;
  if (not IsEndDocument) then begin                                    {!!.51}
    Result := FFilter.TryRead(S);
    IsEndDocument;                                                     {!!.51}
  end;                                                                 {!!.51}
end;
{--------}
procedure TXpParser.ValidateAttribute(const aValue    : DOMString;
                                            HasEntRef : Boolean);
begin

  if (not HasEntRef) then
    if (XpPos('<', aValue) <> 0) then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvAttrChar + '''<''')
    else if (XpPos('&', aValue) <> 0) then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
{Begin !!.55}
                                       sInvAttrChar + '''&''');
//    else if (XpPos('"', aValue) <> 0) then
//      raise EXpParserError.CreateError(FFilter.Line,
//                                       FFilter.LinePos,
//                                       FFilter.URL,
//                                       sInvAttrChar + '''"''');
{End !!.55}
end;
{--------}
procedure TXpParser.ValidateCData(const CDATA : DOMString);
begin
  if (XpPos(']]>', CDATA) <> 0) then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sInvalidCDataSection);
end;
{--------}
procedure TXpParser.ValidateElementName(const aName : DOMString);
begin
  if (aName = '') or
     (aName = ' ') then
    raise EXpParserError.CreateError(FFilter.Line,
                                     FFilter.LinePos,
                                     FFilter.URL,
                                     sInvalidElementName +
                                     QuotedStr(aName));
end;
{--------}
procedure TXpParser.ValidateEncName(const aValue : string);
var
  i : Integer;
  Good : Boolean;
begin
  { Production [81]}
  for i := 1 to Length(aValue) do begin
    Good := False;
    if ((aValue[i] >= 'A') and
        (aValue[i] <= 'z')) then
      Good := True
    else if i > 1 then
      if (aValue[i] >= '0') and
         (aValue[i] <= '9') then
        Good := True
      else if aValue[i] = '.' then
        Good := True
      else if aValue[i] = '_' then
        Good := True
      else if aValue[i] = '-' then
        Good := True
      else if aValue[i] = '=' then
        Good := True;
    if not Good then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvEncName +
                                       QuotedStr(aValue));
  end;
end;
{--------}
procedure TXpParser.ValidateEntityValue(const aValue   : DOMString;    {!!.52}
                                              aQuoteCh : DOMChar);     {!!.52}
var
  TempChr : DOMChar;                                                   {!!.52}
  i       : Integer;
begin
  for i := 1 to Length(aValue) do begin
    TempChr := aValue[i];
    if (TempChr = '%') or
       (TempChr = '&') or
       (TempChr = aQuoteCh) then                                       {!!.52}
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvEntityValue +
                                       QuotedStr(TempChr));
  end;
end;
{--------}
procedure TXpParser.ValidatePCData(const aString      : DOMString;
                                         aInEntityRef : Boolean);
begin
  if (not aInEntityRef) then
    if (XpRPos('<', aString) <> 0) then                                {!!.52}
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvPCData + '''<''')
    else if (XpRPos('&', aString) <> 0) and                            {!!.52}
            (XpRPos(';', aString) = 0) then                            {!!.52}
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvPCData + '''&''')
    else if (XpRPos(']]>', aString) <> 0) then                         {!!.52}
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvPCData + ''']]>''');
end;
{--------}
procedure TXpParser.ValidatePublicID(const aString : DOMString);
var
  Ucs4Char : TXpUcs4Char;
  i        : Integer;
begin
  for i := 1 to Length(aString) do begin
    XPIso88591ToUcs4(AnsiChar(aString[i]), Ucs4Char);
    if (not XPIsPubidChar(Ucs4Char)) then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvPubIDChar +
                                       QuotedStr(aString[i]));
  end;
end;
{--------}
procedure TXpParser.ValidateVersNum(const aString : string);
var
  i       : Integer;
  TempChr : char;
  Good    : Boolean;
begin
  for i := 1 to Length(aString) do begin
    Good := False;
    TempChr := aString[i];
    if (TempChr >= 'A') and
       (TempChr <= 'z') then
      Good := True
    else if (TempChr >= '0') and
            (TempChr <= '9') then
      Good := True
    else if (TempChr = '.') then
      Good := True
    else if (TempChr = '_') then
      Good := True
    else if (TempChr = ':') then
      Good := True
    else if (TempChr = '-') then
      Good := True;
    if not Good then
      raise EXpParserError.CreateError(FFilter.Line,
                                       FFilter.LinePos,
                                       FFilter.URL,
                                       sInvVerNum +
                                       QuotedStr(aString));
  end;
end;
{== ActiveX methods ==================================================}
{$IFDEF DACTIVEX}
{--------}
function TXpParser.GetHeight : Integer;
begin
  Result := inherited Height;
end;
{--------}
function TXpParser.GetWidth : Integer;
begin
  Result := inherited Width;
end;
{--------}
procedure TXpParser.SetBounds(aLeft, aTop, aWidth, aHeight : Integer);
begin
  if csDesigning in ComponentState then begin
    if AWidth <> 28 then
      AWidth := 28;
    if AHeight <> 28 then
      AHeight := 28;
  end else begin
    if AWidth <> 0 then
      AWidth := 0;
    if AHeight <> 0 then
      AHeight := 0;
  end;
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;
{--------}
procedure TXpParser.SetHeight(Value : Integer);
begin
  if csDesigning in ComponentState then
    inherited Height := 28
  else
    inherited Height := 0;
end;
{--------}
procedure TXpParser.SetWidth(Value : Integer);
begin
  if csDesigning in ComponentState then
    inherited Width := 28
  else
    inherited Width := 0;
end;
{$ENDIF}
{=====================================================================}
end.
