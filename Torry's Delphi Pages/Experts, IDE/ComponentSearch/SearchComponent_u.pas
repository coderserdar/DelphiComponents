{-----------------------------------------------------------------------------
 Unit Name: SearchComponent_u
 Author:    Administrator
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit SearchComponent_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids,ToolsApi;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FcomponentList:TStringList;
    function GetPackageName(Index:Integer):String;
    function GxOtaGetCurrentModule: IOTAModule;
    function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
    function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex>=0 then
    Label3.Caption:=GetPackageName(Integer(ListBox1.Items.Objects[ListBox1.itemindex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FcomponentList:=TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FComponentList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ListBox1.Items.Assign(FcomponentList);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  I: Integer;
begin
  if Trim(Edit1.Text)='' then
  begin
    ListBox1.Items.Assign(FcomponentList);
    exit;
  end;
  ListBox1.Items.Clear;
  for I := 0 to FcomponentList.Count - 1 do    // Iterate
    if pos(UpperCase(Trim(Edit1.Text)),uppercase(FcomponentList.Strings[i]))>0 then
      ListBox1.Items.AddObject(FcomponentList.Strings[i],FcomponentList.Objects[i]);
end;

function TForm1.GetPackageName(Index: Integer):String;
var
  PackageServices: IOTAPackageServices;
  PackageCounter: Integer;
  ComponentCounter: Integer;
  InstalledComponentName: string;
begin
  PackageServices := BorlandIDEServices as IOTAPackageServices;
  Assert(Assigned(PackageServices));
  Result:='';
  if (Index<0) or (Index>PackageServices.PackageCount-1) then
    exit;
  Result:=Packageservices.PackageNames[Index];
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  CurrentModule: IOTAModule;
  CurrentForm: IOTAFormEditor;
  lComponent:IOTAComponent;
  lcs: TControlStyle;
begin
  if ListBox1.ItemIndex<0 then
  begin
    ShowMessage('Must Select One Component!');
    exit;
  end;
  CurrentModule := GxOtaGetCurrentModule;
  Assert(Assigned(CurrentModule));

  CurrentForm := GxOtaGetFormEditorFromModule(CurrentModule);
  Assert(Assigned(CurrentForm));
  if CurrentForm.GetSelCount>0 then
    lComponent:=CurrentForm.GetSelComponent(0)
  else
    lComponent:=CurrentForm.GetRootComponent;
  if not lComponent.IsTControl  then
  begin
    ShowMessage('The Component of Selected is not Container Control!');
    exit;
  end;
//  if not lComponent.GetPropValueByName('ControlStyle',lcs) then
//  begin
//    ShowMessage('Error Call GetProValueByName');
//    exit;
//  end;
//  if not (csAcceptsControls	in lcs) then
//  begin
//    ShowMessage('The Component of Selected is not Container Control!');
//    exit;
//  end;
  CurrentForm.CreateComponent(lComponent,ListBox1.Items.Strings[ListBox1.itemindex],0,0,50,50);
end;

function TForm1.GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.CurrentModule;
end;

function TForm1.GxOtaGetFormEditorFromModule(
  const Module: IOTAModule): IOTAFormEditor;
var
  i: Integer;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if Supports(Editor, IOTAFormEditor, FormEditor) then
    begin
      Assert(not Assigned(Result));
      Result := FormEditor;
      // In order to assert our assumptions that only one form
      // is ever associated with a module, do not call Break; here.
    end;
  end;
end;

function TForm1.GxOtaGetFileEditorForModule(Module: IOTAModule;
  Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  // BCB 5 crashes calling GetModuleFileEditor(1) for simple units
  {$IFDEF GX_BCB}{$IFDEF VER130}
  if IsCpp(Module.FileName) and (Module.GetModuleFileCount = 2) and (Index = 1) then
    Index := 2;
  {$ENDIF VER130}{$ENDIF GX_BCB}
  Result := Module.GetModuleFileEditor(Index);
end;

end.
