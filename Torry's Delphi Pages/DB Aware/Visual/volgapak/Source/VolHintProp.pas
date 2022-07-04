unit VolHintProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF VER140} DesignIntf, DesignEditors,
{$ELSE}{$IFDEF VER150} DesignIntf, DesignEditors,
  {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  StdCtrls, Buttons, TypInfo;

type

  TfrmVHintEdit = class(TForm)
    Memo: TMemo;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    btnClear: TBitBtn;
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{ TVHintProperty }

  TVHintProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure Edit; override;
  end;

  procedure Register;

implementation

{$R *.DFM}
const BaseClass: TClass = TComponent;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', TVHintProperty);
end;

function TVHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TVHintProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else Result := 1024;
end;

procedure TVHintProperty.Edit;
var
  Comp: TPersistent;
begin
  with TfrmVHintEdit.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;
    Memo.Lines.Text := GetStrValue;
    Memo.MaxLength := GetEditLimit;
    if ShowModal = mrOk then begin
      SetStrValue(Memo.Lines.Text);
    end;
  finally
    Free;
  end;
end;

procedure TfrmVHintEdit.btnClearClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.
 