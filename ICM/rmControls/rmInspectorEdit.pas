{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmInspectorItems
Purpose  : This is THE designtime unit to help with selecting and creating
           rmInspectorItems.
Date     : 01-18-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmInspectorEdit;

interface

{$I CompilerDefines.INC}

{!@$DEFINE rmDesign}

uses
{$ifdef D6_or_higher}
  {$IFNDEF rmDesign}DesignIntf, DesignEditors, TypInfo, {$ENDIF}
{$else}
  {$IFNDEF rmDesign}DsgnIntf, TypInfo, {$ENDIF}
{$endif}
  Classes, Forms, ExtCtrls, StdCtrls, Controls,
  rmInspector, rmInspectorItems, windows, rmTreeNonView, Dialogs, rmTabs3x, rmLabel,
  ActnList;

type
  TfrmInspectorEditor = class(TForm)
    Panel3: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Label2: TLabel;
    Panel5: TPanel;
    rmTabSet1: TrmTabSet;
    Notebook1: TNotebook;
    Label10: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ItemType: TrmLabel;
    itemName: TEdit;
    ItemText: TEdit;
    ItemValue: TEdit;
    ItemDesc: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtStrPassChar: TEdit;
    edtStrMaxLen: TEdit;
    Panel6: TPanel;
    Label1: TLabel;
    rmInspector1: TrmInspector;
    btnNew: TButton;
    btnNewSub: TButton;
    btnDelete: TButton;
    Label5: TLabel;
    mCmbItems: TMemo;
    cbxIntUseRanges: TCheckBox;
    Panel7: TPanel;
    Label6: TLabel;
    edtIntMinVal: TEdit;
    Label11: TLabel;
    edtIntMaxVal: TEdit;
    Label12: TLabel;
    ActionList1: TActionList;
    actNewItem: TAction;
    actNewSubItem: TAction;
    actDeleteItem: TAction;
    Label13: TLabel;
    Label14: TLabel;
    edtDateformat: TEdit;
    procedure actNewExecute(Sender: TObject);
    procedure actNewSubExecute(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure rmInspector1ItemIndexChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rmInspector1ItemIndexChanging(CurrentNode,
      NewNode: TrmTreeNonViewNode; var AllowChange: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure rmTabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure edtStrMaxLenExit(Sender: TObject);
    procedure edtStrPassCharExit(Sender: TObject);
    procedure edtStrPassCharEnter(Sender: TObject);
    procedure edtStrMaxLenEnter(Sender: TObject);
    procedure cbxIntUseRangesClick(Sender: TObject);
    procedure edtIntMinValEnter(Sender: TObject);
    procedure edtIntMaxValEnter(Sender: TObject);
    procedure edtIntMinValExit(Sender: TObject);
    procedure ItemTextEnter(Sender: TObject);
    procedure itemNameEnter(Sender: TObject);
    procedure ItemValueEnter(Sender: TObject);
    procedure ItemTextExit(Sender: TObject);
    procedure ItemValueExit(Sender: TObject);
    procedure itemNameExit(Sender: TObject);
    procedure actNewSubItemUpdate(Sender: TObject);
    procedure actDeleteItemUpdate(Sender: TObject);
    procedure mCmbItemsEnter(Sender: TObject);
    procedure mCmbItemsExit(Sender: TObject);
    procedure edtDateformatEnter(Sender: TObject);
    procedure edtDateformatExit(Sender: TObject);
    procedure edtIntMaxValExit(Sender: TObject);
  private
    { Private declarations }
    fOldValue: string;
    procedure GetItemValues;
    procedure EnabledProperties(Enable: boolean);
    function UpdateItem: boolean;
    procedure NewItem(ParentNode:TrmTreeNonViewNode);
  public
    { Public declarations }
  end;

{$IFNDEF rmDesign}
  TrmInspectorItemsProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TrmInspectorEditor = class(TComponentEditor)
  private
    procedure ClearData;
    procedure EditItems;
  public
    function GetVerb(index: integer): string; override;
    function GetVerbCount: integer; override;
    procedure ExecuteVerb(index: integer); override;
  end;
{$ENDIF}

var
  frmInspectorEditor: TfrmInspectorEditor;

implementation

{$R *.DFM}

uses
  rmLibrary, sysutils, rmInspectorEdit2;

procedure TfrmInspectorEditor.actNewExecute(Sender: TObject);
begin
  NewItem(nil);
end;

procedure TfrmInspectorEditor.actNewSubExecute(Sender: TObject);
begin
   NewItem(rmInspector1.CurrentNode);
end;

procedure TfrmInspectorEditor.btnDeleteClick(Sender: TObject);
begin
  if assigned(rmInspector1.CurrentItem) then
    rmInspector1.DeleteItem(rmInspector1.CurrentItemPath);
end;

procedure TfrmInspectorEditor.rmInspector1ItemIndexChanged(
  Sender: TObject);
begin
  Notebook1.ActivePage := 'Standard';
  if assigned(rmInspector1.CurrentItem) then
  begin
    GetItemValues;
    EnabledProperties(true);
  end
  else
  begin
    ItemName.text := '';
    ItemDesc.Text := '';
    ItemValue.text := '';
    ItemText.Text := '';
    ItemType.Caption := '';
    EnabledProperties(false);
  end;
end;

procedure TfrmInspectorEditor.FormCreate(Sender: TObject);
begin
  enabledProperties(false);
  Notebook1.PageIndex := 0;
end;

procedure TfrmInspectorEditor.EnabledProperties(Enable: boolean);
begin
  ItemName.enabled := enable;
  ItemDesc.enabled := enable;
  ItemValue.enabled := enable;
  ItemText.enabled := enable;
  Notebook1.Enabled := enable;
  rmTabSet1.Enabled := enable;
end;

{$IFNDEF rmDesign}

{ TrmInspectorEditor }

procedure TrmInspectorEditor.ClearData;
begin
  if assigned(Component) then
  begin
    TrmInspector(Component).ClearItems;
    designer.modified;
  end;
end;

procedure TrmInspectorEditor.EditItems;
var
  MyDialog: TfrmInspectorEditor;
begin
  MyDialog := TfrmInspectorEditor.Create(Application);
  try
    MyDialog.rmInspector1.AssignItems(TrmInspector(Component));
    if MyDialog.ShowModal = mrOK then
    begin
      TrmInspector(Component).AssignItems(MyDialog.rmInspector1);
      designer.Modified;
    end;
  finally
    MyDialog.Free;
  end;
end;

procedure TrmInspectorEditor.ExecuteVerb(index: integer);
begin
  case index of
    0: EditItems;
    1: ClearData;
  end;
end;

function TrmInspectorEditor.GetVerb(index: integer): string;
begin
  case index of
    0: result := 'Edit Items...';
    1: result := 'Clear Items';
  end;
end;

function TrmInspectorEditor.GetVerbCount: integer;
begin
  result := 2;
end;

{ TrmInspectorItemsProperty }

function TrmInspectorItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
end;

function TrmInspectorItemsProperty.GetValue: string;
begin
  Result := '<TrmInspectorItems>';
end;

{$ENDIF}

procedure TfrmInspectorEditor.rmInspector1ItemIndexChanging(CurrentNode,
  NewNode: TrmTreeNonViewNode; var AllowChange: Boolean);
begin
{  if assigned(CurrentNode) then
    AllowChange := UpdateItem;}
end;

function TfrmInspectorEditor.UpdateItem: boolean;
var
  wPath : string;
begin
  result := false;
  if assigned(rmInspector1.CurrentNode) then
  begin
    if (ItemName.text <> '') and (ItemText.Text <> '') then
    begin
      if assigned(rmInspector1.currentNode.parent) then
         wPath := rmInspector1.currentNode.parent.NodePath
      else
         wPath := '';

      if (rmInspector1.Items.LocateNode(wPath+rmInspector1.SepChar+ItemText.Text) <> nil) then
      begin
         showmessage('An item with that text already appears in that level');
         exit;
      end;
//      ApplyItemChanges;
      result := true;
    end
    else
    begin
      showmessage('The Object Name and the Item Text properties must contain a value');
      exit;
    end;
  end;
end;

procedure TfrmInspectorEditor.Button1Click(Sender: TObject);
begin
  if UpdateItem then
    rmInspector1.Invalidate;
end;

procedure TfrmInspectorEditor.rmTabSet1Change(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
begin
  if NewTab = 1 then
    Notebook1.ActivePage := rmTabSet1.Tabs[NewTab] + '_' + rmInspector1.CurrentItem.ClassName
  else
    Notebook1.ActivePage := rmTabSet1.Tabs[NewTab];
end;

procedure TfrmInspectorEditor.edtStrMaxLenExit(Sender: TObject);
begin
  try
    strtoint(edtStrMaxLen.text);
    TrmStringInspectorItem(rmInspector1.CurrentItem).MaxLength := strtoint(edtStrMaxLen.text);
  except
    on e:Exception do
    begin
       ShowMessage(e.message);
       edtStrMaxLen.text := fOldValue;
    end;
  end;
end;

procedure TfrmInspectorEditor.edtStrPassCharExit(Sender: TObject);
begin
  try
    edtStrPassChar.text := chartostr(strtochar(edtStrPassChar.text));
    TrmStringInspectorItem(rmInspector1.CurrentItem).PasswordChar := strtochar(edtStrPassChar.text);
  except
    on e: exception do
    begin
      showmessage(e.message);
      edtstrpasschar.text := foldvalue;
    end;
  end;
end;

procedure TfrmInspectorEditor.edtStrPassCharEnter(Sender: TObject);
begin
  fOldValue := edtStrPassChar.text;
end;

procedure TfrmInspectorEditor.edtStrMaxLenEnter(Sender: TObject);
begin
  fOldValue := edtStrMaxLen.text;
end;

procedure TfrmInspectorEditor.GetItemValues;
var
  wItem: TrmCustomInspectorItem;
begin
  wItem := rmInspector1.CurrentItem;
  ItemName.text := wItem.Name;
  ItemDesc.Text := wItem.Hint;
  ItemValue.text := wItem.AsString;
  ItemText.Text := rmInspector1.CurrentNode.Text;
  ItemType.Caption := wItem.ClassName;

  if wItem.ClassType = TrmStringInspectorItem then
  begin
    with TrmStringInspectorItem(wItem) do
    begin
      edtStrPassChar.text := chartostr(PasswordChar);
      edtStrMaxLen.text := inttostr(MaxLength);
    end;
  end

  else if wItem.ClassType = TrmComplexInspectorItem then
  begin
     // Nothing To Set...
  end

  else if wItem.ClassType = TrmComboInspectorItem then
  begin
    with TrmComboInspectorItem(wItem) do
    begin
      mCmbItems.Lines.assign(Items);
    end;
  end

  else if wItem.ClassType = TrmIntegerInspectorItem then
  begin
    with TrmIntegerInspectorItem(wItem) do
    begin
      edtIntMinVal.text := IntToStr(MinValue);
      edtIntMaxVal.text := IntToStr(MaxValue);
      cbxIntUseRanges.Checked := UseRange;
    end;
  end

  else if wItem.ClassType = TrmDateInspectorItem then
  begin
    with TrmDateInspectorItem(wItem) do
    begin
       edtDateformat.text := DateFormat;
    end;
  end;
end;

procedure TfrmInspectorEditor.cbxIntUseRangesClick(Sender: TObject);
begin
  TrmIntegerInspectorItem(rmInspector1.CurrentItem).UseRange := cbxIntUseRanges.Checked;
  label6.Enabled := cbxIntUseRanges.Checked;
  label11.Enabled := cbxIntUseRanges.Checked;
  edtIntMinVal.Enabled := cbxIntUseRanges.Checked;
  edtIntMaxVal.Enabled := cbxIntUseRanges.Checked;
end;

procedure TfrmInspectorEditor.edtIntMinValEnter(Sender: TObject);
begin
  fOldValue := edtIntMinVal.Text;
end;

procedure TfrmInspectorEditor.edtIntMaxValEnter(Sender: TObject);
begin
  fOldValue := edtIntMaxVal.Text;
end;

procedure TfrmInspectorEditor.edtIntMinValExit(Sender: TObject);
begin
   try
     StrToInt(edtIntMinVal.text);
     TrmIntegerInspectorItem(rmInspector1.CurrentItem).MinValue := StrToInt(edtIntMinVal.text);
   except
     on E: Exception do
     begin
       showmessage(E.Message);
       edtIntMinVal.Text := fOldValue;
     end;
   end;
end;

procedure TfrmInspectorEditor.ItemTextEnter(Sender: TObject);
begin
   fOldValue := ItemText.text;
end;

procedure TfrmInspectorEditor.itemNameEnter(Sender: TObject);
begin
   fOldValue := ItemName.text;
end;

procedure TfrmInspectorEditor.ItemValueEnter(Sender: TObject);
begin
   fOldValue := ItemValue.text;
end;

procedure TfrmInspectorEditor.ItemTextExit(Sender: TObject);
var
   wPath : string;
   wNode : TrmTreeNonViewNode;
begin
   if ItemText.Text <> '' then
   begin
      if assigned(rmInspector1.currentNode.parent) then
         wPath := rmInspector1.currentNode.parent.NodePath
      else
         wPath := '';

      wNode := rmInspector1.Items.LocateNode(wPath+rmInspector1.SepChar+ItemText.Text);
      if (wNode <> nil) and (wNode <> rmInspector1.currentNode) then
      begin
         showmessage('An item with that text already appears in that level');
         ItemText.Text := foldValue;
      end
      else
         rmInspector1.CurrentNode.Text := ItemText.Text;
      rmInspector1.Invalidate;
   end
   else
   begin
      showmessage('Each item must have a Text value');
      ItemText.Text := foldValue;
   end
end;

procedure TfrmInspectorEditor.ItemValueExit(Sender: TObject);
begin
   try
      rmInspector1.CurrentItem.AsString := ItemValue.Text;
   except
      on e:Exception do
      begin
         showmessage(e.message);
         ItemValue.text := fOldValue;
      end;
   end;
end;

procedure TfrmInspectorEditor.itemNameExit(Sender: TObject);
begin
   try
      rmInspector1.CurrentItem.Name := ItemName.Text;
   except
      on e:Exception do
      begin
         showmessage(e.message);
         ItemName.text := fOldValue;
      end;
   end;
end;

procedure TfrmInspectorEditor.actNewSubItemUpdate(Sender: TObject);
begin
   actNewSubItem.enabled := rmInspector1.currentItem <> nil;
end;

procedure TfrmInspectorEditor.actDeleteItemUpdate(Sender: TObject);
begin
   actDeleteItem.Enabled := rmInspector1.currentItem <> nil;
end;

procedure TfrmInspectorEditor.NewItem(ParentNode: TrmTreeNonViewNode);
var
  wPath: string;
  frmItemTypes: TfrmInspectorItemTypes;
  wItem: TrmCustomInspectorItem;
  wIndex: integer;
  wValue : string;
  wFound : boolean;
begin
  frmItemTypes := TfrmInspectorItemTypes.create(self);
  try
    if frmItemTypes.ShowModal = mrok then
    begin
      wPath := rmInspector1.sepchar + 'New Item';

      if ParentNode <> nil then
         wPath := ParentNode.NodePath+wPath;

      wIndex := 1;
      while rmInspector1.FindInspectorItem(wPath + inttostr(wIndex)) <> nil do
           inc(wIndex);


      if frmItemTypes.ChosenType = TrmIntegerInspectorItem then
         wValue := '0'
      else if frmItemTypes.ChosenType = TrmDateInspectorItem then
         wValue := DateToStr(now)
      else
         wValue := '<Empty Value>';

      wItem := rmInspector1.AddInspectorItem(wPath + inttostr(wIndex), wValue, frmItemTypes.ChosenType);

      if assigned(wItem) then
      begin
        wValue := wItem.ClassName;
        delete(wValue, 1, 1);
        wIndex := 1;
        wFound := true;
        while wFound do
        try
           wItem.Name := wValue+inttostr(wIndex);;
           wFound := false;
        except
           inc(wIndex);
        end;
        rmInspector1.CurrentNode := wItem.PathNode;
      end;
    end;
  finally
    frmItemTypes.free;
  end;
end;

procedure TfrmInspectorEditor.mCmbItemsEnter(Sender: TObject);
begin
   fOldValue := mcmbitems.text;
end;

procedure TfrmInspectorEditor.mCmbItemsExit(Sender: TObject);
begin
   try
      TrmComboInspectorItem(rmInspector1.CurrentItem).Items.Text := mCmbItems.Text;
   except
      on e:Exception do
      begin
         showmessage(e.message);
         mCmbItems.Text := fOldValue;
      end;
   end;
end;

procedure TfrmInspectorEditor.edtDateformatEnter(Sender: TObject);
begin
   fOldValue := edtDateFormat.Text;
end;

procedure TfrmInspectorEditor.edtDateformatExit(Sender: TObject);
begin
   Try
      TrmDateInspectorItem(rmInspector1.CurrentItem).DateFormat := edtDateformat.text;
   except
      on E:Exception do
      begin
         ShowMessage(e.message);
         EdtDateFormat.Text := fOldValue; 
      end;
   end
end;

procedure TfrmInspectorEditor.edtIntMaxValExit(Sender: TObject);
begin
   try
     StrToInt(edtIntMaxVal.text);
     TrmIntegerInspectorItem(rmInspector1.CurrentItem).MaxValue := StrToInt(edtIntMaxVal.text);
   except
     on E: Exception do
     begin
       showmessage(E.Message);
       edtIntMaxVal.Text := fOldValue;
     end;
   end;
end;

end.

