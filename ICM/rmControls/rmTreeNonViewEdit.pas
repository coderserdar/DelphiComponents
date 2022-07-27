{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTreeNonViewEdit
Purpose  : Property editor for the TrmTreeNonView component.
Date     : 12-01-1999
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit was originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmTreeNonViewEdit;

interface

{$I CompilerDefines.INC}

{$ifdef D6_or_higher}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DesignIntf, DesignEditors, Registry, rmTreeNonView, StdCtrls, ComCtrls, rmPathTreeView;
{$else}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DsgnIntf, Registry, rmTreeNonView, StdCtrls, ComCtrls, rmPathTreeView;
{$endif}


type
  TfrmTreeNonViewEditor = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    grpItems: TGroupBox;
    tvItems: TrmPathTreeView;
    btnNew: TButton;
    btnNewSub: TButton;
    btnDelete: TButton;
    btnLoad: TButton;
    grpProperties: TGroupBox;
    Label1: TLabel;
    edText: TEdit;
    procedure btnNewClick(Sender: TObject);
    procedure btnNewSubClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvItemsChange(Sender: TObject; Node: TrmTreeNode);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TrmTreeNonViewItemsProperty = class( TPropertyEditor )
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

implementation

{$R *.DFM}

uses
  LibHelp;

const
  Section = 'rmTreeNonView Items Editor';


function TrmTreeNonViewItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];                  { Edit method will display a dialog }
end;


function TrmTreeNonViewItemsProperty.GetValue: string;
begin
  { The GetPropType method is used to retrieve information pertaining to the   }
  { property type being edited.  In this case, the Name of the property class  }
  { is displayed in the value column of the Object Inspector.                  }

  Result := Format( '(%s)', [ GetPropType^.Name ] );
end;


procedure TrmTreeNonViewItemsProperty.Edit;
var
  Component: TComponent;
  Dialog: TfrmTreeNonViewEditor;
  N : TrmTreeNonViewNode;
  A : TrmTreeNode;

begin
  Dialog := TfrmTreeNonViewEditor.Create(Application);
  try
    if (PropCount = 1) and (GetComponent(0) is TComponent) then
    begin
      Component := TComponent(GetComponent(0));

      with Component as TrmTreeNonView do
      begin
        Dialog.tvItems.SepChar := SepChar;
        N := Items.GetFirstNode;
        while N <> nil do
        begin
          Dialog.tvItems.AddPathNode(nil, NodePath(N));
          N := N.GetNext;
        end;

        if Dialog.ShowModal = mrOK then
        begin
          Items.Clear;
          A := Dialog.tvItems.Items.GetFirstNode;
          while A <> nil do
          begin
            AddPathNode(nil, Dialog.tvItems.NodePath(A));
            A := A.GetNext;
          end;
          Modified;
        end;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrmTreeNonViewEditor.btnNewClick(Sender: TObject);
var
  N : TrmTreeNode;

begin
  N := tvItems.Items.Add(nil, '');
  N.Selected := True;
  edText.SetFocus;
end;

procedure TfrmTreeNonViewEditor.btnNewSubClick(Sender: TObject);
var
  N : TrmTreeNode;

begin
  N := tvItems.Items.AddChild(tvItems.Selected, '');
  N.Selected := True;
  edText.SetFocus;
end;

procedure TfrmTreeNonViewEditor.btnDeleteClick(Sender: TObject);
begin
  if tvItems.Selected <> nil then
    tvItems.Selected.Delete;
end;

procedure TfrmTreeNonViewEditor.edTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if tvItems.Selected <> nil then
    tvItems.Selected.Text := edText.Text;
end;

procedure TfrmTreeNonViewEditor.tvItemsChange(Sender: TObject;
  Node: TrmTreeNode);
begin
  edText.Text := Node.Text;
end;

end.
