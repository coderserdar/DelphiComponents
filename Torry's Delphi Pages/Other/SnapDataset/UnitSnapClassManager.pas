unit UnitSnapClassManager;

{*******************************************************************}
{                                                                   }
{       SnapObjectDataset Field Editor                              }
{                                                                   }
{       Copyright (c) 2006 by Cosimo De Michele.                    }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM THE AUTHOR.                                 }
{                                                                   }
{*******************************************************************}

interface

uses
  mwPasParser,
  Contnrs,
  Classes;

type
  TClassManager = class;
  TBrowseClassInfo = class;

  TOnParseFile = procedure(Sender: TClassManager; FileName: string; FileIndex, FileCount: Integer) of object;

  TMethodDeclare = (cdPrivate, cdProtected, cdPublic, cdPublished);

  TMethodType = (ctConstant, ctMethod, ctType, ctVariable, ctProperty);


  TBrowseMethodInfo = class(TCollectionItem)
  private
    // Full declaration of the member for display
    FDName: string;
    // Identifier only of the member for searching
    FRName: string;
    // Member visibility
    FMethodDeclare: TMethodDeclare;
    FMethodType: TMethodType;
    FLineNo: Integer;
    FcOverride: Boolean;
    FcAbstract: Boolean;
    FcVirtual: Boolean;
    FcMessage: Boolean;
    FcReintroduce: Boolean;
    FcOverload: Boolean;
    FcInherited: Boolean;
    fData: TObject;
    function GetCollection: TBrowseClassInfo;
    function GetTypeName: string;
  public
    procedure AfterConstruction; override;
    function GetInterfaceLine: Integer;
    function GetImplementationLine: Integer;
    property Collection: TBrowseClassInfo read GetCollection;

    property DName: string read FDName;
    property RName: string read FRName;
    property TypeName: string read GetTypeName;
    property MethodDeclare: TMethodDeclare read FMethodDeclare;
    property MethodType: TMethodType read FMethodType;
    property LineNo: Integer read FLineNo;
    property cOverride: Boolean read FcOverride;
    property cAbstract: Boolean read FcAbstract;
    property cVirtual: Boolean read FcVirtual;
    property cMessage: Boolean read FcMessage;
    property cInherited: Boolean read FcInherited;
    property cReintroduce: Boolean read FcReintroduce;
    property cOverload: Boolean read FcOverload;
    property Data: TObject read fData write fData;
  end;

  TBrowseClassInfo = class(TCollection)
  private
    FClassManager: TClassManager;
    // The complete list of object/interface ancestors
    FDerivedFrom: string;
    FUnitName: string;
    FLineNo: Integer;
    FFileName: string;
    FIsLoaded: Boolean;
    FName: string;
    procedure GetMethods(Parser: TmPasParser);
    function GetItem(Index: Integer): TBrowseMethodInfo;
    function GetText: string;
    procedure SetText(const New: string);
    procedure SetClassManager(AClassManager: TClassManager);
    function GetObjectDerivedFrom: string;
  public
    constructor Create;
    procedure LoadMethods;
    function Add: TBrowseMethodInfo;
    function SetParser(Parser: TmPasParser): Boolean; // Sets parser on interface line
    function RefreshLineNo: Integer;
    property Items[Index: Integer]: TBrowseMethodInfo read GetItem; default;
    property ClassManager: TClassManager read FClassManager write SetClassManager;
    property DerivedFrom: string read FDerivedFrom;
    property ObjectDerivedFrom: string read GetObjectDerivedFrom;
    property FileName: string read FFileName;
    property UnitName: string read FUnitName;
    property LineNo: Integer read FLineNo;
    property IsLoaded: Boolean read FIsLoaded;
    property Name: string read FName;
    property AsText: string read GetText;
  end;

  TBrowseUnitInfo = class(TCollectionItem)
  private
    fClasses: TObjectList;
    FName: string;
    function GetClassCount: Integer;
    function GetClassItem(Index: Integer): TBrowseClassInfo;
    procedure LoadClass(const FileName: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ClassManager: TClassManager;
    function AddClass: TBrowseClassInfo;
    function ClassByName(const AClassName: string): TBrowseClassInfo;
    property Name: string read FName write FName;

    property ClassCount: Integer read GetClassCount;
    property ClassItem[Index: Integer]: TBrowseClassInfo read GetClassItem;
  end;

  TClassManager = class(TCollection)
  private
    FOnEndParse: TNotifyEvent;
    FOnParseFile: TOnParseFile;
    function GetItem(const Index: integer): TBrowseUnitInfo;
  protected
    procedure DoParseFile(FileName: string; FileIndex, FileCount: Integer); virtual;
    procedure DoEndParse;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(AName: string): TBrowseUnitInfo;
    function ClassByName(const AClassName: string): TBrowseClassInfo;

    procedure LoadFromDir(const Dir: string; const Recurse: Boolean);
    procedure LoadFromProject;

    property Items[const Index: integer]: TBrowseUnitInfo read GetItem;

    property OnParseFile: TOnParseFile read FOnParseFile write FOnParseFile;
    property OnEndParse: TNotifyEvent read FOnEndParse write FOnEndParse;
  end;

implementation

uses
  mwEPTokenList,
  ToolsAPI,
  ToolIntf,
  ExptIntf,
  Dialogs,
  SysUtils;

function IsDprOrPas(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(FileName);
  Result := (SameText(FileExt, '.PAS') or SameText(FileExt, '.DPR'));
end;


constructor TClassManager.Create;
begin
  inherited Create(TBrowseUnitInfo);
end;

destructor TClassManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

resourcestring
  SParsingError = 'A parsing error occurred in ';

function EnumUnits(Param: Pointer;
                   const FileName, Unitname, FormName: string): Boolean; stdcall;
var
  Item: TBrowseUnitInfo;
  ClassManager: TClassManager;
begin
  Result := True;
  try
    if IsDprOrPas(FileName) then
    begin
      ClassManager := TClassManager(Param);

      Item := ClassManager.Add(ExtractFileName(FileName));
      try
        Item.LoadClass(FileName);
      except
        MessageDlg(SParsingError + Filename, mtError, [mbOK], 0);
      end;
      ClassManager.DoParseFile(FileName, Item.Index, ToolServices.GetUnitCount);
    end;
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

procedure TClassManager.LoadFromProject;
var
  i: Integer;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  Item: TBrowseUnitInfo;
begin
  Clear;
  //if ToolServices<>nil then
  //  ToolServices.EnumProjectUnits(EnumUnits, Pointer(Self))
  //else
  begin
    IModuleServices := BorlandIDEServices as IOTAModuleServices;

    CurrentProject := nil;
    IProjectGroup := nil;
    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      if Supports(IModule, IOTAProjectGroup, IProjectGroup) then
        Break;
    end;

    if not Assigned(IProjectGroup) then
    begin
      IModuleServices := BorlandIDEServices as IOTAModuleServices;

      for i := 0 to IModuleServices.ModuleCount - 1 do
        begin
          IModule := IModuleServices.Modules[i];
          if Supports(IModule, IOTAProject, CurrentProject) then
            Break;
        end;
    end;

    try
      // This raises exceptions in D5 with .bat projects active
      if Assigned(IProjectGroup) and (not Assigned(CurrentProject)) then
        CurrentProject := IProjectGroup.ActiveProject;
    except
      CurrentProject := nil;
    end;


    if Assigned(CurrentProject) then
    begin
      for i := 0 to CurrentProject.GetModuleCount - 1 do
      begin
        ModuleInfo := CurrentProject.GetModule(i);

        if ModuleInfo.FileName <> '' then
          begin
            if IsDprOrPas(ModuleInfo.FileName) then
            begin
              Item := Self.Add(ExtractFileName(ModuleInfo.FileName));
              try
                Item.LoadClass(ModuleInfo.FileName);
              except
                MessageDlg(SParsingError + ModuleInfo.Filename, mtError, [mbOK], 0);
              end;
              DoParseFile(ModuleInfo.FileName, Item.Index, CurrentProject.GetModuleCount);
            end;
          end;
      end;
    end;
  end;
  DoEndParse;
end;

procedure TClassManager.LoadFromDir(const Dir: string; const Recurse: Boolean);
var
  Mask: string;
  FileList: TStringList;

  procedure CollectFiles(Path: string);
  var
    Result: Integer;
    Search: TSearchRec;
  begin
    if Length(Path) > 0 then
      Path := IncludeTrailingPathDelimiter(Path);

    Result := FindFirst(Path + '*.*', faAnyFile, Search);
    try
      while Result = 0 do
      begin
        if (Search.Attr and faDirectory) <> 0 then
        begin
          if Recurse and (Search.Name <> '.') and (Search.Name <> '..') then
            CollectFiles(Path + Search.Name);
        end
        else
          if Pos('*' + ExtractFileExt(Search.Name), Mask) <> 0 then
          begin
            FileList.Add(Path + Search.Name);
          end;

        Result := FindNext(Search);
      end;
    finally
      SysUtils.FindClose(Search);
    end;
  end;

var
  i: Integer;
  Item: TBrowseUnitInfo;
begin
  Clear;
  Mask := '*.PAS;*.DPR;*.INC';

  FileList := TStringList.Create;
  try
    CollectFiles(Dir);

    { Now parse files }
    for i := 0 to FileList.Count-1 do
    begin
      try
        Item := Add(ExtractFileName(FileList[i]));;
        Item.LoadClass(FileList[i]);
      except
        MessageDlg(SParsingError + FileList[i], mtError, [mbOK], 0);
      end;
      DoParseFile(FileList[i], i + 1, FileList.Count);
    end;
  finally
    FileList.Free;

    //! StH: In case of an exception, do we really want to fire this event?
    DoEndParse;
  end;
end;

{ TBrowseClassInfoCollection }

constructor TBrowseClassInfo.Create;
begin
  inherited Create(TBrowseMethodInfo);
end;

function TBrowseClassInfo.Add: TBrowseMethodInfo;
begin
  Result := TBrowseMethodInfo(inherited Add);
end;

function TBrowseClassInfo.GetItem(Index: Integer): TBrowseMethodInfo;
begin
  Result := TBrowseMethodInfo(inherited GetItem(Index));
end;

function TBrowseClassInfo.GetText: string;
begin
  Result := #34 + Name + #34 + ',' + #34 + DerivedFrom + #34 + ',' +
    #34 + UnitName + #34 + ',' + #34 + IntToStr(LineNo) + #34 + ',' + #34 + FileName + #34;
end;

procedure TBrowseClassInfo.SetText(const New: string);
var
  i: Cardinal;
  CopyStartPos: Cardinal;

      function ExtractSubString: string;
      begin
        Result := Copy(New, CopyStartPos, i - CopyStartPos);
      end;

var
  TypeCounter: Cardinal;
  TextActive: Boolean;
begin
  try
    TextActive := False;
    TypeCounter := 0;
    CopyStartPos := 1;
    for i := 1 to Length(New) do
    begin
      if New[i] = '"' then
      begin
        if TextActive then
        begin
          case TypeCounter of
            0: FName        := ExtractSubString;
            1: FDerivedFrom := ExtractSubString;
            2: FUnitName    := ExtractSubString;
            3: FLineNo      := StrToInt(ExtractSubString);
            4: FFileName    := ExtractSubString;
          else
            Assert(False, 'Bad file format encountered.');
          end; // case
          Inc(TypeCounter);
          TextActive := False;
        end // if TextActive
        else
        begin
          CopyStartPos := i + 1;
          TextActive := True;
        end;
      end; // if New[i] = '"'
    end; // for
  except
    on E:Exception do
    begin
      // Ignore exceptions
    end;
  end;
end;

procedure TBrowseClassInfo.SetClassManager(AClassManager: TClassManager);
begin
  Assert(FClassManager = nil, 'Memory leak assigning class manager');
  FClassManager := AClassManager;
end;

function TBrowseClassInfo.SetParser(Parser: TmPasParser): Boolean;

      function GetInfo: TmInfoKind;
      begin
        Parser.NextNonSpace;
        case Parser.Token.ID of
          tkNull:      Result := ikUnknown;
          tkSemiColon: Result := ikClForward;
          tkOf:        Result := ikClReference;
        else
                       Result := ikClass;
        end;
      end;

begin
  Result := False;
  while Parser.Token.ID <> tkNull do
  begin
    Parser.NextClassLine;
    if GetInfo = ikClass then
    begin
      Parser.RunPos := Parser.LastIdentPos;
      if CompareText(Parser.Token.Data, Name) = 0 then
      begin
        FLineNo := Parser.Token.LineNumber; // Update LineNumber
        Result := True;
        Exit;
      end
      else
        Parser.NextNonJunk;
    end;
  end;
end;

function TBrowseClassInfo.RefreshLineNo: Integer;
var
  EditorStream: TMemoryStream;
  Parser: TmPasParser;
  //LocalEditReader: TEditReader;
begin
  Result := 0;

  //LocalEditReader := FClassList.EditRead;
  //LocalEditReader.FileName := FileName;
  //LocalEditReader.Reset;

  EditorStream := TMemoryStream.Create;
  try
    //LocalEditReader.SaveToStream(EditorStream);
    Parser := TmPasParser.Create;
    try
      Parser.Origin := EditorStream.Memory;
      if SetParser(Parser) then
      begin
        Result := Parser.Token.LineNumber;
        FLineNo := Result;
      end;
    finally
      Parser.Free;
    end;
  finally
    EditorStream.Free;
  end;
end;

procedure TBrowseClassInfo.LoadMethods;
var
  EditorStream: TMemoryStream;
  Parser: TmPasParser;
begin
  if FileExists(Filename) then
  begin
    Clear;

    FIsLoaded := True;
    EditorStream := TMemoryStream.Create;
    try
      EditorStream.LoadFromFile(FileName);

      Parser := TmPasParser.Create;
      try
        Parser.Origin := EditorStream.Memory;
        if SetParser(Parser) then
        begin
          while (Parser.Token.ID <> tkNull) and (Parser.Token.ID <> tkClass) do
            Parser.NextNonJunk;
          if Parser.Token.ID = tkClass then
            Parser.NextNonJunk;
          if Parser.Token.ID = tkRoundOpen then
            while (Parser.Token.ID <> tkNull) and (Parser.Token.ID <> tkRoundClose) do
              Parser.NextNonJunk;
          if Parser.Token.ID = tkRoundClose then
            Parser.NextNonJunk;
          if Parser.Token.ID <> tkSemiColon then
            GetMethods(Parser);
        end;
      finally
        Parser.Free;
      end;
    finally
      EditorStream.Free;
    end;
  end;
end;

procedure TBrowseClassInfo.GetMethods(Parser: TmPasParser);
var
  MethodVisibility: TTokenKind;

  procedure SetVisibility(MInfo: TBrowseMethodInfo);
  begin
    case MethodVisibility of
      tkPublished: MInfo.FMethodDeclare := cdPublished;
      tkPublic:    MInfo.FMethodDeclare := cdPublic;
      tkProtected: MInfo.FMethodDeclare := cdProtected;
      tkPrivate:   MInfo.FMethodDeclare := cdPrivate;
    end;
  end;

  procedure LoadProc;
  var
    RoundOpen: Integer;
    MInfo: TBrowseMethodInfo;
    BuildRName: Boolean;
    SavePos: LongInt;
    ProcedureToken: TTokenKind;
    RenamedEqualPosition: Integer;
  begin
    ProcedureToken := Parser.Token.ID;
    if ProcedureToken in [tkProcedure, tkFunction, tkConstructor, tkDestructor] then
    begin
      RoundOpen := 0;
      MInfo := Self.Add;
      MInfo.FDName := Parser.Token.Data;
      MInfo.FLineNo := Parser.Token.LineNumber;
      MInfo.FMethodType := ctMethod;
      SetVisibility(MInfo);
      Parser.NextToken;
      BuildRName := True;
      while (Parser.Token.ID <> tkNull) and
        (not ((Parser.Token.ID = tkSemiColon) and (RoundOpen = 0))) do
      begin
        case Parser.Token.ID of
          tkCRLF,
          tkSpace:
            MInfo.FDname := MInfo.FDName + ' ';

          // Accounts for "function xyz: Integer" which was not
          // previously parsed correctly
          tkColon:
            begin
              MInfo.FDname := MInfo.FDName + Parser.Token.Data;
              if (RoundOpen = 0) and (ProcedureToken = tkFunction) then
                BuildRName := False;
            end;

          tkRoundOpen:
            begin
              MInfo.FDName := MInfo.FDName + Parser.Token.Data;
              BuildRName := False;
              Inc(RoundOpen);
            end;

          tkRoundClose:
            begin
              MInfo.FDName := MInfo.FDName + Parser.Token.Data;
              Dec(RoundOpen);
            end;

          tkAnsiComment, tkBorComment, tkSlashesComment:
            begin
              // Ignore comments inside procedure declarations
            end;
        else
          begin
            MInfo.FDName := MInfo.DName + Parser.Token.Data;
            if BuildRName then
              MInfo.FRName := MInfo.FRName + Parser.Token.Data;
          end;
        end;
        Parser.NextToken;
      end; // while
      SavePos := 0;
      while (Parser.Token.ID = tkSemiColon) and (Parser.Token.ID <> tkNull) do
      begin
        SavePos := Parser.RunPos;
        Parser.NextNonJunk;
        case Parser.Token.ID of
          tkCRLF,
          tkSpace:
            MInfo.FDName := MInfo.FDName + ' ';
          tkAbstract:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcAbstract := True;
              Parser.NextNonJunk;
            end;
          tkOverride:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcOverride := True;
              Parser.NextNonJunk;
            end;
          tkMessage:
            begin
              MInfo.FcMessage := True;
              MInfo.FDName := MInfo.FDName + '; ';
              while (Parser.Token.ID <> tkSemiColon) and (Parser.Token.ID <> tkNull) do
              begin
                MInfo.FDName := MInfo.FDName + Parser.Token.Data;
                Parser.NextToken;
              end;
            end;
          tkVirtual:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcVirtual := True;
              Parser.NextNonJunk;
            end;
          tkReintroduce:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcReintroduce := True;
              Parser.NextNonJunk;
            end;
          tkOverload:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcOverload := True;
              Parser.NextNonJunk;
            end;
          tkStdcall, tkCdecl, tkPascal, tkRegister, tkSafeCall:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              Parser.NextNonJunk;
            end;
        end; // case
      end; // while
      if SavePos <> 0 then
        Parser.RunPos := SavePos;
      RenamedEqualPosition := Pos('=', MInfo.RName);
      // For renamed/redirected methods, RName should jump to the renamed method
      if RenamedEqualPosition > 0 then
        MInfo.FRName := Trim(Copy(MInfo.RName, RenamedEqualPosition + 1, 9999));
    end
  end;

  procedure LoadVariable;
  var
    SquareOpen, RoundOpen: Integer;
    MInfo: TBrowseMethodInfo;
    BuildRName: Boolean;
    CommaPosition: Integer;
  begin
    SquareOpen := 0; RoundOpen := 0;
    MInfo := Self.Add;
    MInfo.FLineNo := Parser.Token.LineNumber;
    MInfo.FMethodType := ctVariable;
    BuildRName := True;
    SetVisibility(MInfo);
    while (not ((Parser.Token.Id = tkSemiColon) and (RoundOpen = 0) and (SquareOpen = 0))) and
      (Parser.Token.ID <> tkNull) do
    begin
      MInfo.FDName := MInfo.FDName + Parser.Token.Data;
      if BuildRName then
        if not (Parser.Token.ID in [tkSpace, tkCRLF, tkColon]) then
          MInfo.FRName := MInfo.FRName + Parser.Token.Data;
      case Parser.Token.ID of
        tkRoundOpen:   Inc(RoundOpen);
        tkRoundClose:  Dec(RoundOpen);
        tkSquareOpen:  Inc(SquareOpen);
        tkSquareClose: Dec(SquareOpen);
        tkColon:       BuildRName := False;
      end;
      Parser.NextToken;
    end;
    CommaPosition := Pos(',', MInfo.RName);
    // With multiple var declarations on the same line, RName should match
    // only the first identifier when searching for a match later
    if CommaPosition > 0 then
      MInfo.FRName := Copy(MInfo.RName, 1, CommaPosition - 1);
  end;

  procedure LoadProperty;
  var
    SquareOpen, RoundOpen: Integer;
    MInfo: TBrowseMethodInfo;
    BuildRName: Boolean;
  begin
    SquareOpen := 0; RoundOpen := 0;
    MInfo := Self.Add;
    MInfo.FLineNo := Parser.Token.LineNumber;
    MInfo.FMethodType := ctProperty;
    BuildRName := False;
    SetVisibility(MInfo);
    while (not ((Parser.Token.Id = tkSemiColon) and (RoundOpen = 0) and (SquareOpen = 0))) and
      (Parser.Token.ID <> tkNull) do
    begin
      MInfo.FDName := MInfo.FDName + Parser.Token.Data;
      if BuildRName then
        if not (Parser.Token.ID in [tkSpace, tkCRLF, tkColon, tkSquareOpen, tkDefault, tkStored, tkRead, tkWrite]) then
          MInfo.FRName := MInfo.FRName + Parser.Token.Data;
      case Parser.Token.ID of
        tkRoundOpen:   Inc(RoundOpen);
        tkRoundClose:  Dec(RoundOpen);
        // Stop building RName when encountering a '['
        tkSquareOpen:
          begin
            Inc(SquareOpen);
            BuildRName := False;
          end;
        tkSquareClose: Dec(SquareOpen);
        tkProperty:    BuildRName := True;
        tkColon:       BuildRName := False;
        // Stop building RName when encountering default/stored/read/write
        tkDefault:     BuildRName := False;
        tkStored:      BuildRName := False;
        tkRead:        BuildRName := False;
        tkWrite:       BuildRName := False;
      end;
      Parser.NextToken;
    end;
  end;

begin
  MethodVisibility := tkPublished;
  while not (Parser.Token.ID in [tkEnd, tkNull]) do
  begin
    case Parser.Token.ID of
      tkProcedure,
      tkFunction,
      tkConstructor,
      tkDestructor: LoadProc;

      tkPrivate,
      tkProtected,
      tkPublished,
      tkPublic: MethodVisibility := Parser.Token.ID;

      tkIdentifier: LoadVariable;
      tkProperty: LoadProperty;
    end; // case
    Parser.NextToken;
  end;
end;

function TBrowseMethodInfo.GetCollection: TBrowseClassInfo;
begin
  Result := TBrowseClassInfo(inherited Collection);
end;

function TBrowseMethodInfo.GetInterfaceLine: Integer;

  function TokenMatchesMethodType(Token: TTokenKind; MethodType: TMethodType): Boolean;
  begin
    Result := True;
    case MethodType of
      ctProperty:  Result := Token = tkProperty;
      ctMethod:    Result := Token in [tkFunction, tkProcedure];
    end;
  end;

var
  EditorStream: TMemoryStream;
  Parser: TmPasParser;
  //LocalEditReader: TEditReader;
  LastTokenType: TTokenKind;
begin
  Result := 0;

  //LocalEditReader := Collection.ClassList.EditRead;
  //LocalEditReader.FileName := Collection.FileName;
  //LocalEditReader.Reset;

  EditorStream := TMemoryStream.Create;
  try
    //LocalEditReader.SaveToStream(EditorStream);
    Parser := TmPasParser.Create;
    try
      Parser.Origin := EditorStream.Memory;
      if Collection.SetParser(Parser) then
      begin
        // Temporarily set the line number in case we don't find a perfect match
        Result := Parser.Token.LineNumber;
        LastTokenType := Parser.Token.ID;
        while not (Parser.Token.ID in [tkEnd, tkRoundClose, tkPrivate, tkPublic, tkProtected, tkClass, tkColon, tkNull]) do
        begin
          LastTokenType := Parser.Token.ID;
          Parser.NextNonJunk;
        end;
        while not (Parser.Token.ID in [tkEnd, tkNull]) do
        begin
          if (CompareText(Parser.Token.Data, Self.RName) = 0) and TokenMatchesMethodType(LastTokenType, Self.FMethodType) then
          begin
            FLineNo := Parser.Token.LineNumber; // Might as well update line number in object
            Result := Parser.Token.LineNumber;
            Exit;
          end;
          LastTokenType := Parser.Token.ID;
          Parser.NextNonJunk;
        end;
      end;
    finally
      Parser.Free;
    end;
  finally
    EditorStream.Free;
  end;
end;

function TBrowseMethodInfo.GetImplementationLine: Integer;
var
  EditorStream: TMemoryStream;
  Parser: TmPasParser;
  //LocalEditReader: TEditReader;
begin
  //LocalEditReader := Collection.ClassList.EditRead;
  //LocalEditReader.FileName := Collection.FileName;
  //LocalEditReader.Reset;

  EditorStream := TMemoryStream.Create;
  try
    //LocalEditReader.SaveToStream(EditorStream);
    Parser := TmPasParser.Create;
    try
      Parser.Origin := EditorStream.Memory;
      Result := Parser.GetMethodImpLine(Collection.Name, Self.RName);
    finally
      Parser.Free;
    end;
  finally
    EditorStream.Free;
  end;
end;

function TBrowseClassInfo.GetObjectDerivedFrom: string;
var
  FirstAncestorEndPos: Integer;
begin
  FirstAncestorEndPos := Pos(',', DerivedFrom);
  if FirstAncestorEndPos > 0 then
    Result := Trim(Copy(FDerivedFrom, 0, FirstAncestorEndPos - 1))
  else
    Result := DerivedFrom;
end;



{ TBrowseUnitInfoItem }

function TBrowseUnitInfo.AddClass: TBrowseClassInfo;
begin
  Result := TBrowseClassInfo.Create;
  Result.ClassManager := Self.ClassManager;
  fClasses.Add(Result);
end;

procedure TBrowseUnitInfo.AfterConstruction;
begin
  inherited;
  fClasses := TObjectList.Create;
end;

procedure TBrowseUnitInfo.BeforeDestruction;
begin
  fClasses.Free;

  inherited;
end;

function TBrowseUnitInfo.ClassManager: TClassManager;
begin
  Result := TClassManager(Collection);
end;

function TBrowseUnitInfo.GetClassCount: Integer;
begin
  Result := fClasses.Count;
end;

function TBrowseUnitInfo.GetClassItem(
  Index: Integer): TBrowseClassInfo;
begin
  Result := TBrowseClassInfo(fClasses.Items[Index]);
end;

procedure TBrowseUnitInfo.LoadClass(const FileName: string);
var
  EditorStream: TMemoryStream;
  Parser: TmEPTokenList;
  Buf: array[0..30] of char;
  p1, i, j: Integer;
  ObjectInfo: TBrowseClassInfo;
  s: string;
  ExtPos: integer;
begin
  if IsDprOrPas(FileName) and FileExists(FileName) then
  begin

    //LocalEditReader := TClassList(Collection).EditRead;
    //LocalEditReader.FileName := FileName;
    //LocalEditReader.Reset;

    EditorStream := TMemoryStream.Create;
    try
      EditorStream.LoadFromFile(Filename);
      //LocalEditReader.SaveToStream(EditorStream);
      EditorStream.Position := EditorStream.Size;
      FillChar(Buf, 23, 0);
      EditorStream.WriteBuffer(Buf, 23);
      EditorStream.Position := 0;
      Parser := TmEPTokenList.Create;
      try
        Parser.SetOrigin(EditorStream.Memory, EditorStream.Size);
        for J := 0 to Parser.Searcher.ClassList.Count - 1 do
        begin
          Parser.RunIndex := Parser.Searcher.ClassList[J];
          case Parser.GetClassKind of
            ikClass,
            ikClEmpty:
              begin
                ObjectInfo := AddClass;
                p1 := 0;
                for i := 1 to Length(Parser.Info.Data) do
                begin
                  case Parser.Info.Data[i] of
                    '=': ObjectInfo.FName := Trim(Copy(Parser.Info.Data, 1, i - 1));
                    '(': p1 := i;
                    ')': begin
                           ObjectInfo.FDerivedFrom := Trim(Copy(Parser.Info.Data, p1 + 1, i - p1 - 1));
                           // Handle the shortcut implied TObject
                           if Trim(ObjectInfo.FDerivedFrom) = '' then
                             ObjectInfo.FDerivedFrom := 'TObject';
                           Break;
                         end;
                  end;
                end;
                //ObjectInfo.FFileName := LocalEditReader.FileName;
                ObjectInfo.FFileName := FileName;
                if Parser.Info.AI <> nil then
                  ObjectInfo.FUnitName := Parser.Info.AI.aiUnit
                else
                begin
                  s := ExtractFileName(ObjectInfo.FileName);
                  ExtPos := Pos(UpperCase(ExtractFileExt(s)), UpperCase(s));
                  if ExtPos > 0 then
                    Delete(s, ExtPos, Length(s));

                  ObjectInfo.FUnitName := s;
                end;
                ObjectInfo.LoadMethods;
              end;
          end; // case
        end;
      finally
        Parser.Free;
      end;
    finally
      EditorStream.Free;
    end;
  end;
end;

function TBrowseUnitInfo.ClassByName(
  const AClassName: string): TBrowseClassInfo;
var
  i: Integer;
  ClassInfo: TBrowseClassInfo;
begin
  Result := nil;
  for i := 0 to fClasses.Count-1 do
  begin
    ClassInfo := TBrowseClassInfo(fClasses.Items[i]);
    if SameText(ClassInfo.Name, AClassName) then
    begin
      Result := ClassInfo;
      Break;
    end;
  end;

end;

function TClassManager.Add(AName: string): TBrowseUnitInfo;
begin
  Result := TBrowseUnitInfo(inherited Add);
  Result.Name := AName;
end;

function TClassManager.GetItem(const Index: integer): TBrowseUnitInfo;
begin
  Result := TBrowseUnitInfo(inherited Items[Index]);
end;

procedure TClassManager.DoParseFile(FileName: string; FileIndex,
  FileCount: Integer);
begin
  if Assigned(FOnParseFile) then
    FOnParseFile(Self, FileName, FileIndex, FileCount);
end;

procedure TClassManager.DoEndParse;
begin
  if Assigned(fOnEndParse) then
    fOnEndParse(Self);
end;

function TBrowseMethodInfo.GetTypeName: string;
var
  i, f: integer;
  s: string;
begin
  s := '';
  i := Pos(':', DName);
  f := Pos(';', DName);
  if f=0 then
    f := Length(DName);
  if i>0 then
  begin
    s := Trim(Copy(DName, i + 1, f - i));
    f := Pos(' ', s);
    if f>0 then
      s := Copy(s, 1, f -1);
  end;
  Result := s;
end;

function TClassManager.ClassByName(
  const AClassName: string): TBrowseClassInfo;
var
  i: integer;
begin
  Result := nil;
  for i:=0 to Count - 1 do
  begin
    Result := Items[i].ClassByName(AClassName);
    if Result<>nil then
      break;
  end;
end;

procedure TBrowseMethodInfo.AfterConstruction;
begin
  inherited;
  fData := nil;
end;

end.
