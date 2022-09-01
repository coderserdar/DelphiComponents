unit SnapDatasetReg;

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
  Designintf,
  DesignEditors,
  Classes;

type
  TAboutProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TFieldsEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  Forms,
  Windows,
  Controls,
  SnapVirtualDataset,
  SnapObjectDataset,
  UnitSnapFieldsEditor,
  UnitSnapAboutBoxEditor;

{$R dclSnapDataset.dcr}

{ TAboutProperty }

procedure TAboutProperty.Edit;
begin
  with TFormEditorAboutBox.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutProperty.GetValue: string;
begin
  Result:='(About)';
end;

{ TFieldsEditor }

procedure TFieldsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowSnapFieldEditor(TSnapObjectDataset(Component), Designer);
  (*
    1: if DoScriptEditor(TSelRec(Component)) then
         Designer.Modified;
    2: if DoCategorieEditor(TSelRec(Component)) then
         Designer.Modified;
  *)
  end;
end;

function TFieldsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Fields Editor';
  end;
end;

function TFieldsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents('Snap', [TSnapVirtualDataset, TSnapObjectDataset]);
  RegisterPropertyEditor(TypeInfo(string), TSnapObjectDataset, 'ABOUT', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TSnapVirtualDataset, 'ABOUT', TAboutProperty);
  RegisterComponentEditor(TSnapObjectDataset, TFieldsEditor);
end;


end.
