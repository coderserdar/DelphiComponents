{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: component editor helper     }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Explorer,
  fDlgStd, ExtCtrls;

type
  TExplorerNodeForm = class(TDialogForm)
    cbTypes: TComboBox;
    lbPrompt: TLabel;
    procedure FormShow(Sender: TObject);
    procedure cbTypesClick(Sender: TObject);
    procedure cmOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetExplorerNodesClass: TExplorerNodesClass;

implementation
uses vgVCLRes, vgUtils;

{$R *.DFM}

var
  NodeTypes: TList = nil;

const
  DefaultExplorerNodesClass = 'ExplorerNodesClass';

procedure RegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
begin
  ListAdd(NodeTypes, Pointer(ExplorerNodesClass));
end;

procedure UnRegisterExplorerNodes(ExplorerNodesClass: TExplorerNodesClass);
begin
  ListRemove(NodeTypes, Pointer(ExplorerNodesClass));
end;

function GetExplorerNodesClass: TExplorerNodesClass;
begin
  with TExplorerNodeForm.Create(nil) do
  try
    if ShowModal = mrOK then
    begin
      Result := NodeTypes[cbTypes.ItemIndex];
    end else
      Result := nil;
  finally
    Free;
  end;
end;

procedure TExplorerNodeForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  if Assigned(NodeTypes) then
  begin
    for I := 0 to NodeTypes.Count - 1 do
      cbTypes.Items.Add(TExplorerNodesClass(NodeTypes[I]).ClassName);

    S := ReadString(LoadStr(SRegIniFile), LoadStr(SRegIniSection), DefaultExplorerNodesClass, '', True);
    cbTypes.ItemIndex := Max(0, cbTypes.Items.IndexOf(S));
  end;
  cbTypesClick(nil);
end;

procedure TExplorerNodeForm.cbTypesClick(Sender: TObject);
begin
  cmOK.Enabled := cbTypes.ItemIndex >= 0;
end;

procedure TExplorerNodeForm.cmOKClick(Sender: TObject);
begin
  WriteString(LoadStr(SRegIniFile), LoadStr(SRegIniSection), DefaultExplorerNodesClass, cbTypes.Text, True);
end;

initialization
  RegisterExplorerNodesProc := RegisterExplorerNodes;
  UnRegisterExplorerNodesProc := UnRegisterExplorerNodes;

end.
