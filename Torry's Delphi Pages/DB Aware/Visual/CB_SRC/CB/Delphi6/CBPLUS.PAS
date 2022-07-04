{************************************************************}
{                                                            }
{      TDBComboBoxPlus & TComboBoxPlus Component             }
{                Registration Module                         }
{  Copyright (c) 1995,1996,1997,1998 Out & About Production  }
{Portions Copyright (c) 1995,96,97,98 Inprise International  }
{                                                            }
{                     Version 4.00                           }
{                                                            }
{************************************************************}

unit cbplus;

interface

uses Classes,
{$IFDEF VER140} //D6
   DesignIntf,
   DesignEditors,
{$ELSE}
   DsgnIntf,
{$ENDIF}
   Dialogs;

{$IFDEF Win32}
  {$R Cbplus32.DCR}
{$ELSE}
  {$R Cbplus16.DCR}
{$ENDIF}


procedure Register;

implementation

uses DBLup1a, DBLup1b, Cellsdlg, forms, sysUtils;

Type
TDropDownGridEditor = Class(TComponentEditor)
  procedure Edit; override;
  procedure ExecuteVerb(Index: Integer); override;
  function GetVerb(Index: Integer): string; override;
  function GetVerbCount: Integer; override;
  procedure ShowDropDownGridEditor;
end;

procedure TDropDownGridEditor.ShowDropDownGridEditor;
begin
  with TCellsForm.Create(Application) do
  try
    DlgDesigner := Self.Designer;
    DlgComponent := TComboBoxPlus(Self.Component);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TDropDownGridEditor.Edit;
begin
  ShowDropDownGridEditor;
end;

procedure TDropDownGridEditor.ExecuteVerb(Index: Integer);
begin
  Case Index of
    0: ShowDropDownGridEditor;
    1: MessageDlg('TComboBoxPlus && TDBComboBoxPlus'
                  +#10#13+ '(c) 95, 96, 97, 98, 99 O&&A Productions'
                  +#10#13+ 'support@o2a.com'
                  +#10#13+ 'sales@o2a.com'
                  +#10#13+ 'Fax 619.839.3834' ,mtInformation,[mbOK],0);

  end;
end;

function TDropDownGridEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0: Result := 'Designer ...';
    1: Result := 'Component Info';
  end;
end;

function TDropDownGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBComboBoxPlus]);
  RegisterComponents('Standard', [TComboBoxPlus]);
  RegisterComponentEditor(TComboBoxPlus, TDropDownGridEditor);
  RegisterComponentEditor(TDBComboBoxPlus, TDropDownGridEditor);
end;

end.
