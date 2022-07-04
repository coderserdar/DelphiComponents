unit LinkPropsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ELDgrm, ExtCtrls;

type
  TfrmLinkProps = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
  private
	FBeginType: TElDBLinkPointType;
	FEndType: TElDBLinkPointType;
    function GetBeginLines: TStrings;
    function GetEndLines: TStrings;
    function GetBeginLineIndex: Integer;
    function GetEndLineIndex: Integer;
    procedure SetBeginLineIndex(const Value: Integer);
    procedure SetEndLineIndex(const Value: Integer);
    { Private declarations }
  public
    { Public declarations }
    function Execute: Boolean;
    property BeginType: TElDBLinkPointType read FBeginType write FBeginType;
    property EndType: TElDBLinkPointType read FEndType write FEndType;
    property BeginLines: TStrings read GetBeginLines;
    property EndLines: TStrings read GetEndLines;
    property BeginLineIndex: Integer read GetBeginLineIndex write SetBeginLineIndex;
    property EndLineIndex: Integer read GetEndLineIndex write SetEndLineIndex; 
  end;

implementation

{$R *.dfm}

{ TdlgLinkProps }

function TfrmLinkProps.Execute: Boolean;
begin
  RadioGroup1.ItemIndex := Ord(BeginType);
  RadioGroup2.ItemIndex := Ord(EndType);
  Result := (ShowModal = mrOk);
  BeginType := TElDBLinkPointType(RadioGroup1.ItemIndex);
  EndType := TElDBLinkPointType(RadioGroup2.ItemIndex);
end;

function TfrmLinkProps.GetBeginLineIndex: Integer;
begin
  Result := ComboBox1.ItemIndex;
end;

function TfrmLinkProps.GetBeginLines: TStrings;
begin
  Result := ComboBox1.Items;
end;

function TfrmLinkProps.GetEndLineIndex: Integer;
begin
  Result := ComboBox2.ItemIndex;
end;

function TfrmLinkProps.GetEndLines: TStrings;
begin
  Result := ComboBox2.Items;
end;

procedure TfrmLinkProps.SetBeginLineIndex(const Value: Integer);
begin
  ComboBox1.ItemIndex := Value;
end;

procedure TfrmLinkProps.SetEndLineIndex(const Value: Integer);
begin
  ComboBox2.ItemIndex := Value;
end;

end.
