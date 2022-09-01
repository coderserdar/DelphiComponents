unit gridcolors_frm_setup_fields;

interface
{$I psc_defines.inc}

uses
  Windows,
  Messages,
  SysUtils,
{$IFDEF D6}
  Variants,
{$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  DB,

  psc_procs,
  psc_listbox;

type
  Tpsc_frm_setup_fields = class(TForm)
    PSCListBox1: TPSCListBox;
    Panel_Buttons: TPanel;
    Button_Ok: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    function SelectFields : string;
    procedure LoadFields(AFields : TFields; AText : string);
  end;

var
  psc_frm_setup_fields: Tpsc_frm_setup_fields;

implementation

{$R *.dfm}

{ Tpsc_frm_setup_fields }

procedure Tpsc_frm_setup_fields.LoadFields(AFields: TFields; AText : string);
var
  i : integer;
  MyPos : integer;
begin
  PSCListBox1.Items.Clear;
  for i := 0 to AFields.Count - 1 do
    with TPSCListItem(PSCListBox1.Items.Add) do
    begin
      Caption := AFields[i].FieldName;
      MyPos := Pos(AFields[i].FieldName, AText);
      Checked := (MyPos > 0);
    end;
end;

function Tpsc_frm_setup_fields.SelectFields: string;
var
  i : integer;
  MyDelimiter : string;
  MySelectedFields : integer; 
begin
  result := '';
  MySelectedFields := 0;
  for i := 0 to PSCListBox1.Items.Count - 1 do
    if PSCListBox1.Items[i].Checked then
    begin
      Inc(MySelectedFields);
      result := result + MyDelimiter + PSCListBox1.Items[i].Caption;
      MyDelimiter := ',';
    end;
  if MySelectedFields = PSCListBox1.Items.Count then
    result := '';
end;

end.
