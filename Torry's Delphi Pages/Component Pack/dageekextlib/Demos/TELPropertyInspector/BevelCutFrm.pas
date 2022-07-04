unit BevelCutFrm;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ELPropInsp;

type
    TfrmBevelCut = class(TForm)
        Button1: TButton;
        Button2: TButton;
        ListBox1: TListBox;
        procedure Button1Click(Sender: TObject);
    private
        FValue: Integer;
        { Private declarations }
    public
        { Public declarations }
        function Execute: Boolean;
        property Value: Integer read FValue write FValue;
    end;

    // Property editor replaces constant names in the drop-down list of the
    // TBevelCut type property
    TBevelCutProperty = class(TELOrdinalPropEditor)
    private
        function FindValue(const AName: string): Integer;
    protected
        function GetAttrs: TELPropAttrs; override;
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        procedure GetValues(AValues: TStrings); override;
    end;

    // Property editor adds dialog capability
    TBevelCutDlgProperty = class(TBevelCutProperty)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure Edit; override;
    end;

    // Property editor adds extended graphics (red dotes near names
    // in the drop-down list) capability
    TBevelCutDrawProperty = class(TBevelCutDlgProperty)
    protected
        function GetAttrs: TELPropAttrs; override;
        { Graphics extensions }
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    end;

const
    BevelCutValues: array[0..3] of string = (
        'None', 'Lowered', 'Raised', 'Space'); // Without 'bv' prefix

var
    frmBevelCut: TfrmBevelCut;

implementation

{$R *.dfm}

{ TdlgBevelCut }

function TfrmBevelCut.Execute: Boolean;
var
    LI: Integer;
begin
    ListBox1.Items.Clear;
    for LI := Low(BevelCutValues) to High(BevelCutValues) do
        ListBox1.Items.Add(BevelCutValues[LI]);
    ListBox1.ItemIndex := Value;
    Result := (ShowModal = mrOk);
    Value := ListBox1.ItemIndex;
end;

{ TBevelCutProperty }

function TBevelCutProperty.FindValue(const AName: string): Integer;
var
    LI: Integer;
begin
    Result := -1;
    for LI := Low(BevelCutValues) to High(BevelCutValues) do
        if SameText(BevelCutValues[LI], AName) then
            begin
                Result := LI;
                Break;
            end;
end;

function TBevelCutProperty.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList];
end;

function TBevelCutProperty.GetValue: string;
begin
    // Function mast return string representation of the property value
    Result := BevelCutValues[GetOrdValue(0)];
end;

procedure TBevelCutProperty.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    // Adds values to drop-down list
    for LI := Low(BevelCutValues) to High(BevelCutValues) do
        AValues.Add(BevelCutValues[LI]);
end;

procedure TBevelCutProperty.SetValue(const Value: string);
var
    LI: Integer;
begin
    // Function mast set value using string representation of the property value
    // (Value parameter). Use SetXXXValue methods to change property value.
    LI := FindValue(Value);
    if LI = -1 then
        raise Exception.Create('Invalid property value');
    SetOrdValue(LI);
end;

procedure TfrmBevelCut.Button1Click(Sender: TObject);
begin
    if ListBox1.ItemIndex = -1 then
        raise Exception.Create('Property value is not selected');
    ModalResult := mrOk;
end;

{ TBevelCutDlgProperty }

procedure TBevelCutDlgProperty.Edit;
begin
    // Function is used to show editor dialod.
    // It will be executed when editor button (with dotes)
    // is clicked or when Ctrl+Enter button is pressed.
    with TfrmBevelCut.Create(Application) do
        begin
            Value := GetOrdValue(0);
            if Execute then
                SetOrdValue(Value);
            Free;
        end;
end;

function TBevelCutDlgProperty.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praDialog];
end;

{ TBevelCutDrawProperty }

function TBevelCutDrawProperty.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList,
        praDialog, praOwnerDrawValues];
end;

procedure TBevelCutDrawProperty.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
    LRight: Integer;
    LOldPenColor, LOldBrushColor: TColor;
begin
    // Function draws items in the drop-down list
    LRight := (ARect.Bottom - ARect.Top) + ARect.Left;
    with ACanvas do
        begin
            LOldPenColor := Pen.Color;
            LOldBrushColor := Brush.Color;
            Pen.Color := Brush.Color;
            Rectangle(ARect.Left, ARect.Top, LRight, ARect.Bottom);
            Brush.Color := clRed;
            Pen.Color := clRed;
            Rectangle(ARect.Left + 6, ARect.Top + 6, LRight - 6, ARect.Bottom - 6);
            Brush.Color := LOldBrushColor;
            Pen.Color := LOldPenColor;
            ACanvas.TextRect(
                Rect(LRight, ARect.Top, ARect.Right, ARect.Bottom),
                LRight + 1,
                ARect.Top + 1,
                AValue
                );
        end;
end;

procedure TBevelCutDrawProperty.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    // Function returns height of the items in the drop-down list
    AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TBevelCutDrawProperty.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    // Function returns width of the drop-down list
    AWidth := AWidth + ACanvas.TextHeight('Wg');
end;

end.

