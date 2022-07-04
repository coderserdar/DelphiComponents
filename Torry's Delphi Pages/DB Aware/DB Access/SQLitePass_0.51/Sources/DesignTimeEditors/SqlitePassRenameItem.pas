unit SqlitePassRenameItem;
{$i ..\..\Sources\SqlitePassDbo.inc}
interface

uses
 {$IFDEF FPC}
 Buttons, LResources,
 {$ELSE}
  Windows,
  Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSqlitePassFormRenameItem = class(TForm)
    Panel1: TPanel;
    BtImageIndexNew: TImage;
    EditNewName: TEdit;
    Panel2: TPanel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    LabelRenameItem: TLabel;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute(FormCaption, ItemNewName: String): Integer;
  end;

var
  FormRenameItem: TSqlitePassFormRenameItem;

implementation

 {$IFNDEF FPC}
  {$R *.DFM}
 {$ENDIF}

 { TSqlitePassFormRenameItem }

function TSqlitePassFormRenameItem.Execute(FormCaption, ItemNewName: String): Integer;
begin
Caption := Format(FormCaption, [ItemNewName]);
LabelRenameItem.Caption := Caption;
EditNewName.Text := ItemNewName;
EditNewName.SelectAll;
Result := ShowModal;
end;

initialization
 {$IFDEF FPC}
  {$i SqlitePassRenameItem.lrs}
 {$ENDIF}
end.
