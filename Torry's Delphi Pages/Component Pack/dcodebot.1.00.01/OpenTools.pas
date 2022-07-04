
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit OpenTools;

interface

uses
  Classes, Forms, ToolsAPI, SysUtils, Windows, PasParser;

type
	TWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard)
  protected
  	{ IOTANotifier }
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
    { IOTAWizard }
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual; abstract;
  end;

  TMenuWizard = class(TWizard, IOTAMenuWizard)
  protected
  	{ IOTAMenuWizard }
    function GetMenuText: string; virtual;
  end;

{ AddText procedure }

procedure AddText(const Text: string);

{ AddUnit procedure }

procedure AddUnit(Form: TCustomForm; const UnitName: string);

{ GetFormModule procdure }

function GetFormModule(Form: TCustomForm): IOTAModule;

{ GetOpenToolInterfaces }

{ procedure GetOpenToolInterfaces(const Obj; Strings: TStrings); }

{ GetUsesStrings }

procedure GetUsesStrings(const Buffer: string; Strings: TStrings);

implementation

{ TWizard.IOTANotifier }

procedure TWizard.AfterSave;
begin
end;

procedure TWizard.BeforeSave;
begin
end;

procedure TWizard.Destroyed;
begin
end;

{ TWizard.IOTAWizard }

function TWizard.GetIDString: string;
begin
 Result := 'codebot.' + ClassName;
end;

function TWizard.GetName: string;
begin
	Result := ClassName;
end;

function TWizard.GetState: TWizardState;
begin
	Result := [wsEnabled];
end;

procedure TWizard.Modified;
begin
end;

{ TMenuWizard.IOTAMenuWizard }

function TMenuWizard.GetMenuText: string;
begin
	Result := ClassName;
end;

{ Reading and writing routines }

function ReadBuffer(Editor: IOTASourceEditor; Position: Integer;
  Count: Integer): string;
var
  Reader: IOTAEditReader;
begin
  SetLength(Result, Count);
  FillChar(PChar(Result)^, Count, #0);
  Reader := Editor.CreateReader;
  Reader.GetText(Position, PChar(Result), Count);
  SetLength(Result, StrLen(PChar(Result)));
end;

procedure WriteBuffer(Editor: IOTASourceEditor; const Text: string); overload;
var
	EditView: IOTAEditView;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  Pos: Integer;
  Writer: IOTAEditWriter;
begin
  if Text = '' then Exit;
	if Editor.GetEditViewCount < 1 then Exit;
	EditView := Editor.GetEditView(0);
  EditPos := EditView.GetCursorPos;
	EditView.ConvertPos(True, EditPos, CharPos);
  Pos := EditView.CharPosToPos(CharPos);
  Writer := Editor.CreateUndoableWriter;
  Writer.CopyTo(Pos);
  Writer.DeleteTo(Pos);
  Writer.Insert(PChar(Text));
end;

procedure WriteBuffer(Editor: IOTASourceEditor; const Text: string;
  CopyPos, DeletePos: Integer); overload;
var
  Writer: IOTAEditWriter;
begin
  if Text = '' then Exit;
  Writer := Editor.CreateWriter;
  Writer.CopyTo(CopyPos);
  Writer.DeleteTo(DeletePos);
  Writer.Insert(PChar(Text));
end;

procedure AddText(const Text: string);
var
	ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  I: Integer;
begin
	if BorlandIDEServices = nil then Exit;
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then Exit;
  Module := ModuleServices.CurrentModule;
  if Module = nil then Exit;
  Editor := nil;
  for I := 0 to Module.GetModuleFileCount - 1 do
    if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, Editor) then
      Break;
  if Editor = nil then Exit;
  WriteBuffer(Editor,  Text);
end;

{ Pascal scanning routines }

type
  TSourcePos = record
    Column: Integer;
    CopyPos: Integer;
    DeletePos: Integer;
  end;

procedure GetLastUsesPosition(const Buffer: string;
  var SourcePos: TSourcePos);
begin
  with TPascalParser.Create(PChar(Buffer), Length(Buffer)) do
  try
    SourcePos.Column := -1;
    SourcePos.CopyPos := -1;
    SourcePos.DeletePos := -1;
    if Scan([tkUses]) <> tkNull then
      repeat
         if Scan([tkIdentifier, tkSemicolon]) = tkIdentifier then
         begin
           SourcePos.Column := Token.Col  + Token.Length;
           SourcePos.CopyPos := Token.Position + Token.Length;
         end
         else
           Break;
      until False;
      if (SourcePos.Column > -1) and (Token.Kind = tkSemiColon) then
        SourcePos.DeletePos := Token.Position;
  finally
    Free;
  end;
end;

procedure GetUsesStrings(const Buffer: string; Strings: TStrings);
var
  Parser: TPascalParser;
begin
  Parser := TPascalParser.Create(PChar(Buffer), Length(Buffer));
  Strings.Clear;
  Strings.BeginUpdate;
  try
    if Parser.Scan([tkUses]) <> tkNull then
      repeat
        if Parser.Scan([tkIdentifier, tkSemicolon]) = tkIdentifier then
          Strings.Add(Parser.Token.Text)
        else
          Break;
      until False;
  finally
    Strings.EndUpdate;
    Parser.Free;
  end;
end;

procedure AddUnit(Form: TCustomForm; const UnitName: string);
var
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  Strings: TStrings;
  Found: Boolean;
  SourcePos: TSourcePos;
  S: string;
  I: Integer;
begin
  Module := GetFormModule(Form);
  if Module = nil then Exit;
  Editor := nil;
  for I := 0 to Module.GetModuleFileCount - 1 do
    if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, Editor) then
      Break;
  if Editor = nil then Exit;
  I := 10240;
  S := '';
  repeat
    S := ReadBuffer(Editor, 0, I);
    I := I div 2;
  until (S <> '') or (I < 255);
  Strings := TStringList.Create;
  try
    GetUsesStrings(S, Strings);
    Found := False;
    for I := 0 to Strings.Count - 1 do
      if UpperCase(Strings[I]) = UpperCase(UnitName) then
      begin
        Found := True;
        Break;
      end;
  finally
    Strings.Free;
  end;
  if Found then Exit;
  GetLastUsesPosition(S, SourcePos);
  with SourcePos do
    if DeletePos > -1 then
    begin
      if Column + Length(', ' + UnitName) > 80 then
         WriteBuffer(Editor, ','#13#10'  ' + UnitName, CopyPos, DeletePos)
      else
         WriteBuffer(Editor, ', ' + UnitName, CopyPos, DeletePos);
    end;
end;

function GetFormModule(Form: TCustomForm): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Project: IOTAProject;
  I: Integer;
begin
  Result := nil;
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    Exit;
  Project := nil;
  for I := 0 to ModuleServices.ModuleCount - 1 do
  begin
    Result := ModuleServices.Modules[I];
    if Supports(Result, IOTAProject, Project) then
      Break;
  end;
  if Project <> nil then
    for I := 0 to Project.GetModuleCount - 1 do
      if Project.GetModule(I).FormName = Form.Name then
      begin
        Result := Project.GetModule(I).OpenModule;
        Break;
      end;
end;

{ procedure GetOpenToolInterfaces(const Obj; Strings: TStrings);
var
  Unknown: IUnknown absolute Obj;
  Output: IUnknown;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if Unknown.QueryInterface(IBorlandIDEServices, Output) = S_OK then
      Strings.Add('IBorlandIDEServices');
    if Unknown.QueryInterface(INTAComponent, Output) = S_OK then
      Strings.Add('INTAComponent');
    if Unknown.QueryInterface(INTACustomDrawMessage, Output) = S_OK then
      Strings.Add('INTACustomDrawMessage');
    if Unknown.QueryInterface(INTAEditWindow, Output) = S_OK then
      Strings.Add('INTAEditWindow');
    if Unknown.QueryInterface(INTAFormEditor, Output) = S_OK then
      Strings.Add('INTAFormEditor');
    if Unknown.QueryInterface(INTAServices, Output) = S_OK then
      Strings.Add('INTAServices');
    if Unknown.QueryInterface(INTAServices40, Output) = S_OK then
      Strings.Add('INTAServices40');
    if Unknown.QueryInterface(INTAToDoItem, Output) = S_OK then
      Strings.Add('INTAToDoItem');
    if Unknown.QueryInterface(IOTAActionServices, Output) = S_OK then
      Strings.Add('IOTAActionServices');
    if Unknown.QueryInterface(IOTAAddressBreakpoint, Output) = S_OK then
      Strings.Add('IOTAAddressBreakpoint');
    if Unknown.QueryInterface(IOTABreakpoint, Output) = S_OK then
      Strings.Add('IOTABreakpoint');
    if Unknown.QueryInterface(IOTABreakpoint40, Output) = S_OK then
      Strings.Add('IOTABreakpoint40');
    if Unknown.QueryInterface(IOTABreakpointNotifier, Output) = S_OK then
      Strings.Add('IOTABreakpointNotifier');
    if Unknown.QueryInterface(IOTABufferOptions, Output) = S_OK then
      Strings.Add('IOTABufferOptions');
    if Unknown.QueryInterface(IOTAComponent, Output) = S_OK then
      Strings.Add('IOTAComponent');
    if Unknown.QueryInterface(IOTACreator, Output) = S_OK then
      Strings.Add('IOTACreator');
    if Unknown.QueryInterface(IOTACustomMessage, Output) = S_OK then
      Strings.Add('IOTACustomMessage');
    if Unknown.QueryInterface(IOTACustomMessage50, Output) = S_OK then
      Strings.Add('IOTACustomMessage50');
    if Unknown.QueryInterface(IOTADebuggerNotifier, Output) = S_OK then
      Strings.Add('IOTADebuggerNotifier');
    if Unknown.QueryInterface(IOTADebuggerServices, Output) = S_OK then
      Strings.Add('IOTADebuggerServices');
    if Unknown.QueryInterface(IOTAEditActions, Output) = S_OK then
      Strings.Add('IOTAEditActions');
    if Unknown.QueryInterface(IOTAEditBlock, Output) = S_OK then
      Strings.Add('IOTAEditBlock');
    if Unknown.QueryInterface(IOTAEditBuffer, Output) = S_OK then
      Strings.Add('IOTAEditBuffer');
    if Unknown.QueryInterface(IOTAEditBufferIterator, Output) = S_OK then
      Strings.Add('IOTAEditBufferIterator');
    if Unknown.QueryInterface(IOTAEditLineNotifier, Output) = S_OK then
      Strings.Add('IOTAEditLineNotifier');
    if Unknown.QueryInterface(IOTAEditLineTracker, Output) = S_OK then
      Strings.Add('IOTAEditLineTracker');
    if Unknown.QueryInterface(IOTAEditOptions, Output) = S_OK then
      Strings.Add('IOTAEditOptions');
    if Unknown.QueryInterface(IOTAEditor, Output) = S_OK then
      Strings.Add('IOTAEditor');
    if Unknown.QueryInterface(IOTAEditorNotifier, Output) = S_OK then
      Strings.Add('IOTAEditorNotifier');
    if Unknown.QueryInterface(IOTAEditorServices, Output) = S_OK then
      Strings.Add('IOTAEditorServices');
    if Unknown.QueryInterface(IOTAEditPosition, Output) = S_OK then
      Strings.Add('IOTAEditPosition');
    if Unknown.QueryInterface(IOTAEditReader, Output) = S_OK then
      Strings.Add('IOTAEditReader');
    if Unknown.QueryInterface(IOTAEditView, Output) = S_OK then
      Strings.Add('IOTAEditView');
    if Unknown.QueryInterface(IOTAEditView40, Output) = S_OK then
      Strings.Add('IOTAEditView40');
    if Unknown.QueryInterface(IOTAEditWriter, Output) = S_OK then
      Strings.Add('IOTAEditWriter');
    if Unknown.QueryInterface(IOTAEnvironmentOptions, Output) = S_OK then
      Strings.Add('IOTAEnvironmentOptions');
    if Unknown.QueryInterface(IOTAFile, Output) = S_OK then
      Strings.Add('IOTAFile');
    if Unknown.QueryInterface(IOTAFileSystem, Output) = S_OK then
      Strings.Add('IOTAFileSystem');
    if Unknown.QueryInterface(IOTAFormEditor, Output) = S_OK then
      Strings.Add('IOTAFormEditor');
    if Unknown.QueryInterface(IOTAFormNotifier, Output) = S_OK then
      Strings.Add('IOTAFormNotifier');
    if Unknown.QueryInterface(IOTAFormWizard, Output) = S_OK then
      Strings.Add('IOTAFormWizard');
    if Unknown.QueryInterface(IOTAIDENotifier, Output) = S_OK then
      Strings.Add('IOTAIDENotifier');
    if Unknown.QueryInterface(IOTAIDENotifier50, Output) = S_OK then
      Strings.Add('IOTAIDENotifier50');
    if Unknown.QueryInterface(IOTAKeyBindingServices, Output) = S_OK then
      Strings.Add('IOTAKeyBindingServices');
    if Unknown.QueryInterface(IOTAKeyboardBinding, Output) = S_OK then
      Strings.Add('IOTAKeyboardBinding');
    if Unknown.QueryInterface(IOTAKeyboardDiagnostics, Output) = S_OK then
      Strings.Add('IOTAKeyboardDiagnostics');
    if Unknown.QueryInterface(IOTAKeyboardServices, Output) = S_OK then
      Strings.Add('IOTAKeyboardServices');
    if Unknown.QueryInterface(IOTAKeyContext, Output) = S_OK then
      Strings.Add('IOTAKeyContext');
    if Unknown.QueryInterface(IOTAMenuWizard, Output) = S_OK then
      Strings.Add('IOTAMenuWizard');
    if Unknown.QueryInterface(IOTAMessageServices, Output) = S_OK then
      Strings.Add('IOTAMessageServices');
    if Unknown.QueryInterface(IOTAMessageServices40, Output) = S_OK then
      Strings.Add('IOTAMessageServices40');
    if Unknown.QueryInterface(IOTAModule, Output) = S_OK then
      Strings.Add('IOTAModule');
    if Unknown.QueryInterface(IOTAModule40, Output) = S_OK then
      Strings.Add('IOTAModule40');
    if Unknown.QueryInterface(IOTAModuleCreator, Output) = S_OK then
      Strings.Add('IOTAModuleCreator');
    if Unknown.QueryInterface(IOTAModuleInfo, Output) = S_OK then
      Strings.Add('IOTAModuleInfo');
    if Unknown.QueryInterface(IOTAModuleNotifier, Output) = S_OK then
      Strings.Add('IOTAModuleNotifier');
    if Unknown.QueryInterface(IOTAModuleServices, Output) = S_OK then
      Strings.Add('IOTAModuleServices');
    if Unknown.QueryInterface(IOTANotifier, Output) = S_OK then
      Strings.Add('IOTANotifier');
    if Unknown.QueryInterface(IOTAOptions, Output) = S_OK then
      Strings.Add('IOTAOptions');
    if Unknown.QueryInterface(IOTAPackageServices, Output) = S_OK then
      Strings.Add('IOTAPackageServices');
    if Unknown.QueryInterface(IOTAProcess, Output) = S_OK then
      Strings.Add('IOTAProcess');
    if Unknown.QueryInterface(IOTAProcessModNotifier, Output) = S_OK then
      Strings.Add('IOTAProcessModNotifier');
    if Unknown.QueryInterface(IOTAProcessModule, Output) = S_OK then
      Strings.Add('IOTAProcessModule');
    if Unknown.QueryInterface(IOTAProcessNotifier, Output) = S_OK then
      Strings.Add('IOTAProcessNotifier');
    if Unknown.QueryInterface(IOTAProject, Output) = S_OK then
      Strings.Add('IOTAProject');
    if Unknown.QueryInterface(IOTAProject40, Output) = S_OK then
      Strings.Add('IOTAProject40');
    if Unknown.QueryInterface(IOTAProjectBuilder, Output) = S_OK then
      Strings.Add('IOTAProjectBuilder');
    if Unknown.QueryInterface(IOTAProjectBuilder40, Output) = S_OK then
      Strings.Add('IOTAProjectBuilder40');
    if Unknown.QueryInterface(IOTAProjectCreator, Output) = S_OK then
      Strings.Add('IOTAProjectCreator');
    if Unknown.QueryInterface(IOTAProjectCreator50, Output) = S_OK then
      Strings.Add('IOTAProjectCreator50');
    if Unknown.QueryInterface(IOTAProjectGroup, Output) = S_OK then
      Strings.Add('IOTAProjectGroup');
    if Unknown.QueryInterface(IOTAProjectGroupCreator, Output) = S_OK then
      Strings.Add('IOTAProjectGroupCreator');
    if Unknown.QueryInterface(IOTAProjectOptions, Output) = S_OK then
      Strings.Add('IOTAProjectOptions');
    if Unknown.QueryInterface(IOTAProjectOptions40, Output) = S_OK then
      Strings.Add('IOTAProjectOptions40');
    if Unknown.QueryInterface(IOTAProjectResource, Output) = S_OK then
      Strings.Add('IOTAProjectResource');
    if Unknown.QueryInterface(IOTAProjectWizard, Output) = S_OK then
      Strings.Add('IOTAProjectWizard');
    if Unknown.QueryInterface(IOTARecord, Output) = S_OK then
      Strings.Add('IOTARecord');
    if Unknown.QueryInterface(IOTAReplaceOptions, Output) = S_OK then
      Strings.Add('IOTAReplaceOptions');
    if Unknown.QueryInterface(IOTARepositoryWizard, Output) = S_OK then
      Strings.Add('IOTARepositoryWizard');
    if Unknown.QueryInterface(IOTAResourceEntry, Output) = S_OK then
      Strings.Add('IOTAResourceEntry');
    if Unknown.QueryInterface(IOTASearchOptions, Output) = S_OK then
      Strings.Add('IOTASearchOptions');
    if Unknown.QueryInterface(IOTAServices, Output) = S_OK then
      Strings.Add('IOTAServices');
    if Unknown.QueryInterface(IOTASourceBreakpoint, Output) = S_OK then
      Strings.Add('IOTASourceBreakpoint');
    if Unknown.QueryInterface(IOTASourceEditor, Output) = S_OK then
      Strings.Add('IOTASourceEditor');
    if Unknown.QueryInterface(IOTASpeedSetting, Output) = S_OK then
      Strings.Add('IOTASpeedSetting');
    if Unknown.QueryInterface(IOTAThread, Output) = S_OK then
      Strings.Add('IOTAThread');
    if Unknown.QueryInterface(IOTAThreadNotifier, Output) = S_OK then
      Strings.Add('IOTAThreadNotifier');
    if Unknown.QueryInterface(IOTAToDoManager, Output) = S_OK then
      Strings.Add('IOTAToDoManager');
    if Unknown.QueryInterface(IOTAToDoServices, Output) = S_OK then
      Strings.Add('IOTAToDoServices');
    if Unknown.QueryInterface(IOTATypeLibEditor, Output) = S_OK then
      Strings.Add('IOTATypeLibEditor');
    if Unknown.QueryInterface(IOTATypeLibModule, Output) = S_OK then
      Strings.Add('IOTATypeLibModule');
    if Unknown.QueryInterface(IOTAWizard, Output) = S_OK then
      Strings.Add('IOTAWizard');
    if Unknown.QueryInterface(IOTAWizardServices, Output) = S_OK then
      Strings.Add('IOTAWizardServices');
  finally
    Strings.EndUpdate;
  end;
end; }

end.
