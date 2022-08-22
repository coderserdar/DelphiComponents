unit TB97TlbrEdReg;

interface

procedure Register;

implementation

uses
  DsgnIntf, Forms, TB97Tlbr, TB97TlbrEd;

type
  TToolbar97Editor = class(TDefaultEditor)
    function GetVerbCount : Integer; override;
    function GetVerb(Index : Integer) : string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;

{ TToolbar97Editor }

function TToolbar97Editor.GetVerbCount : Integer;
begin
  Result := 1;
end; { TToolbar97Editor.GetVerbCount }

function TToolbar97Editor.GetVerb(Index : Integer) : string;
begin
  Result := 'Add Buttons...';
end; { TToolbar97Editor.GetVerb }

procedure TToolbar97Editor.ExecuteVerb(Index : Integer);
begin
  with TFToolbar97Editor.Create(Application) do
  try
    ToolBar := TToolbar97(Component);
    ParentForm := TForm(GetParentForm(ToolBar));
    Caption :=  ParentForm.Name + '.' + ToolBar.Name +
      ' - ToolBar97 Editor';
    Left := (Screen.Width - Width) shr 1;
    Top := ParentForm.Top + ToolBar.Top + ToolBar.Height + 50;
    ShowModal;
  finally
    Free;
  end;
  Designer.Modified;
end; { TToolbar97Editor.ExecuteVerb }


procedure Register;
begin
  RegisterComponentEditor(TToolbar97, TToolbar97Editor);
end;

end.
