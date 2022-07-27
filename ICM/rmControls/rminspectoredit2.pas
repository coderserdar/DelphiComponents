{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmInspectorItems
Purpose  : This is a designtime unit to help with selecting and creating
           rmInspectorItems.
Date     : 01-18-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmInspectorEdit2;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, rmInspector;

type
  TfrmInspectorItemTypes = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    function GetItemType: TrmCustomInspectorItemClass;
    procedure SetItemType(const Value: TrmCustomInspectorItemClass);
    { Private declarations }
  public
    { Public declarations }
    property ChosenType:TrmCustomInspectorItemClass read GetItemType write SetItemType;
  end;

implementation

{$R *.DFM}

uses rmInspectorItems;

procedure TfrmInspectorItemTypes.FormCreate(Sender: TObject);
begin
   ComboBox1.ItemIndex := 0;
end;

function TfrmInspectorItemTypes.GetItemType: TrmCustomInspectorItemClass;
begin
   case ComboBox1.itemindex of
     0:result := TrmCheckboxInspectorItem;
     1:result := TrmComboInspectorItem;
     2:result := TrmComplexInspectorItem;
     3:result := TrmDateInspectorItem;
     4:result := TrmIntegerInspectorItem;
     5:result := TrmStringInspectorItem;
   else
      raise exception.create('Unknown item index');
   end
end;

procedure TfrmInspectorItemTypes.SetItemType(
  const Value: TrmCustomInspectorItemClass);
begin
   if Value = TrmComboInspectorItem then
      ComboBox1.itemindex := 0
   else if Value = TrmComplexInspectorItem then
      ComboBox1.itemindex := 1
   else if Value = TrmDateInspectorItem then
      ComboBox1.itemindex := 2
   else if Value = TrmIntegerInspectorItem then
      ComboBox1.itemindex := 3
   else if Value = TrmStringInspectorItem then
      ComboBox1.itemindex := 4
   else
     raise exception.create('Unknown Item Class')
end;

end.
