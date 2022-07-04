unit Mainform;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Menus,
  SysUtils,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, Dblup2, DB, DBTables, DBCtrls;

type
  TAppMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitItem: TMenuItem;
    N1: TMenuItem;
    StatusBar: TPanel;
    SpeedPanel: TPanel;
    ExitBtn: TSpeedButton;
    Table1: TTable;
    DataSource1: TDataSource;
    Table2: TTable;
    DataSource2: TDataSource;
    Panel3: TPanel;
    Shape1: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    DBLookupComboPlus1: TDBLookupComboPlus;
    MaskEditDate: TMaskEdit;
    MaskEditDollars: TMaskEdit;
    EditWordDollars: TEdit;
    RadioGroupStyle: TRadioGroup;
    RadioGroupSpeedButton: TRadioGroup;
    RadioGroupBorderStyle: TRadioGroup;
    RadioGroupAutoDropDown: TRadioGroup;
    RadioGroupDropDownAlign: TRadioGroup;
    RadioGroupDropDownTop: TRadioGroup;
    Bevel1: TBevel;
    Label3: TLabel;
    RadioGroupPreSets: TRadioGroup;
    Label4: TLabel;
    Button1: TButton;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    Button3: TButton;
    Label5: TLabel;
    procedure ShowHint(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupStyleClick(Sender: TObject);
    procedure RadioGroupDropDownAlignClick(Sender: TObject);
    procedure RadioGroupDropDownTopClick(Sender: TObject);
    procedure RadioGroupBorderStyleClick(Sender: TObject);
    procedure RadioGroupAutoDropDownClick(Sender: TObject);
    procedure RadioGroupSpeedButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RadioGroupPreSetsClick(Sender: TObject);
    procedure MaskEditDollarsExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Procedure SetCheckColor(aColor : TColor);
    Procedure SetBorderColor(aColor : TColor);
    Procedure SetFieldColor(aColor : TColor);
    Function RealToCheckString(Amount : Real) : String;
  public
    { Public declarations }
  end;

var
  AppMainForm: TAppMainForm;

implementation

{$R *.DFM}

procedure TAppMainForm.ShowHint(Sender: TObject);
begin
  StatusBar.Caption := Application.Hint;
end;

procedure TAppMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TAppMainForm.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
  MaskEditDate.edittext := DateTimeToStr(Date);
  EditWordDollars.Text :=
    RealToCheckString(StrToFloat(MaskEditDollars.text));
end;

procedure TAppMainForm.RadioGroupStyleClick(Sender: TObject);
begin
  case  RadioGroupStyle.ItemIndex of
    0 : DBLookupComboPlus1.Style := csDropDown;
    1 : DBLookupComboPlus1.Style := csDropDownList;
    2 : DBLookupComboPlus1.Style := csIncSearch;
    3 : DBLookupComboPlus1.Style := csIncSrchEdit;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.RadioGroupDropDownAlignClick(Sender: TObject);
begin
  case  RadioGroupDropDownAlign.ItemIndex of
    0 : DBLookupComboPlus1.DropDownAlign := Dblup2.Left;
    1 : DBLookupComboPlus1.DropDownAlign := Dblup2.Right;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.RadioGroupDropDownTopClick(Sender: TObject);
begin
  case  RadioGroupDropDownTop.ItemIndex of
    0 : DBLookupComboPlus1.DropDownTop := Dblup2.Above;
    1 : DBLookupComboPlus1.DropDownTop := Dblup2.Below;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.RadioGroupBorderStyleClick(Sender: TObject);
begin
  case  RadioGroupBorderStyle.ItemIndex of
    0 : begin
          DBLookupComboPlus1.BorderStyle := bsNone;
          MaskEditDate.BorderStyle := bsNone;
          MaskEditDollars.BorderStyle := bsNone;
          EditWordDollars.BorderStyle := bsNone;
          DBLookupComboPlus1.Ctl3D := False;
          MaskEditDate.Ctl3D := False;
          MaskEditDollars.Ctl3D := False;
          EditWordDollars.Ctl3D := False;
          DBLookupComboPlus1.Height := 16;
          MaskEditDate.Height := 16;
          MaskEditDollars.Height := 16;
          EditWordDollars.Height := 16;
          Shape2.Visible := True;
          Shape3.Visible := True;
          Shape4.Visible := True;
        end;
    1 : Begin
          DBLookupComboPlus1.BorderStyle := bsSingle;
          MaskEditDate.BorderStyle := bsSingle;
          MaskEditDollars.BorderStyle := bsSingle;
          EditWordDollars.BorderStyle := bsSingle;
          DBLookupComboPlus1.Ctl3D := True;
          MaskEditDate.Ctl3D := True;
          MaskEditDollars.Ctl3D := True;
          EditWordDollars.Ctl3D := True;
          DBLookupComboPlus1.Height := 24;
          MaskEditDate.Height := 24;
          MaskEditDollars.Height := 24;
          EditWordDollars.Height := 24;
          Shape2.Visible := False;
          Shape3.Visible := False;
          Shape4.Visible := False;
        end;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.RadioGroupAutoDropDownClick(Sender: TObject);
begin
  case  RadioGroupAutoDropDown.ItemIndex of
    0 : DBLookupComboPlus1.AutoDropDown := True;
    1 : DBLookupComboPlus1.AutoDropDown := False;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.RadioGroupSpeedButtonClick(Sender: TObject);
begin
  case  RadioGroupSpeedButton.ItemIndex of
    0 : DBLookupComboPlus1.ShowSpeedButton := True;
    1 : DBLookupComboPlus1.ShowSpeedButton := False;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;

Procedure TAppMainForm.SetCheckColor(aColor : TColor);
begin
  Panel3.Color := aColor;
end;

Procedure TAppMainForm.SetBorderColor(aColor : TColor);
begin
  Shape1.Pen.Color := aColor;
end;

Procedure TAppMainForm.SetFieldColor(aColor : TColor);
begin
  DBLookupComboPlus1.Color := aColor;
  MaskEditDate.Color := aColor;
  MaskEditDollars.Color := aColor;
  EditWordDollars.Color := aColor;
end;

procedure TAppMainForm.Button1Click(Sender: TObject);
begin
    {initialise to current color}
  ColorDialog1.Color := Panel3.Color;
  If ColorDialog1.Execute then
    SetCheckColor(ColorDialog1.Color);
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.Button2Click(Sender: TObject);
begin
  ColorDialog1.Color := Shape1.Pen.Color;
  If ColorDialog1.Execute then
     SetBorderColor(ColorDialog1.Color);
  DBLookupComboPlus1.SetFocus;
end;

procedure TAppMainForm.Button3Click(Sender: TObject);
begin
  ColorDialog1.Color := DBLookupComboPlus1.Color;
  If ColorDialog1.Execute then
     SetFieldColor(ColorDialog1.Color);
  DBLookupComboPlus1.SetFocus;
end;


procedure TAppMainForm.RadioGroupPreSetsClick(Sender: TObject);
begin
  case  RadioGroupPreSets.ItemIndex of
    0 : begin  {Borland}
          DBLookupComboPlus1.AutoDropDown := False;
            RadioGroupAutoDropDown.ItemIndex := 1;
          DBLookupComboPlus1.ShowSpeedButton := True;
            RadioGroupSpeedButton.ItemIndex := 0;
          DBLookupComboPlus1.BorderStyle := bsSingle;
            RadioGroupBorderStyle.ItemIndex := 1;
          DBLookupComboPlus1.Style := csDropDown;
            RadioGroupStyle.ItemIndex := 0;
          SetCheckColor(clSilver);
          SetBorderColor(clBlack);
          SetFieldColor(clWindow);
        end;
    1 : Begin  {Quicken}
          DBLookupComboPlus1.AutoDropDown := True;
            RadioGroupAutoDropDown.ItemIndex := 0;
          DBLookupComboPlus1.ShowSpeedButton := True;
            RadioGroupSpeedButton.ItemIndex := 0;
          DBLookupComboPlus1.BorderStyle := bsNone;
            RadioGroupBorderStyle.ItemIndex := 0;
          DBLookupComboPlus1.Style := csIncSrchEdit;
            RadioGroupStyle.ItemIndex := 3;
          SetCheckColor(clYellow);
          SetBorderColor(clRed);
          SetFieldColor(clYellow);
        end;
  end; {Case}
  DBLookupComboPlus1.SetFocus;
end;


Function TAppMainForm.RealToCheckString(Amount : Real) : String;
var
  Dollars : LongInt;
  cents : Integer;

  function Num2Str(Num: LongInt): String;
  Const hundred = 100;
        thousand = 1000;
        million = 1000000;
        billion = 1000000000;
   begin
     if Num >= billion then
       if (Num mod billion) = 0 then
         Num2Str := Num2Str(Num div billion) + ' Billion'
       else
         Num2Str := Num2Str(Num div billion) + ' Billion ' +
                    Num2Str(Num mod billion)
     else
       if Num >= million then
         if (Num mod million) = 0 then
           Num2Str := Num2Str(Num div million) + ' Million'
         else
           Num2Str := Num2Str(Num div million) + ' Million ' +
                      Num2Str(Num mod million)
       else
         if Num >= thousand then
           if (Num mod thousand) = 0 then
             Num2Str := Num2Str(Num div thousand) + ' Thousand'
           else
             Num2Str := Num2Str(Num div thousand) + ' Thousand ' +
                        Num2Str(Num mod thousand)
         else
           if Num >= hundred then
             if (Num mod hundred) = 0 then
               Num2Str := Num2Str(Num div hundred) + ' Hundred'
             else
               Num2Str := Num2Str(Num div  hundred) + ' Hundred ' +
                          Num2Str(Num mod hundred)
           else
           case (Num div 10) of
          6,7,9: if (Num mod 10) = 0 then
                    Num2Str := Num2Str(Num div 10) + 'ty'
                  else
                    Num2Str := Num2Str(Num div 10) + 'ty-' +
                               Num2Str(Num mod 10);
               8: if Num = 80 then
                    Num2Str := 'Eighty'
                  else
                    Num2Str := 'Eighty-' + Num2Str(Num mod 10);
               5: if Num = 50 then
                    Num2Str := 'Fifty'
                  else
                    Num2Str := 'Fifty-' + Num2Str(Num mod 10);
               4: if Num = 40 then
                    Num2Str := 'Forty'
                  else
                    Num2Str := 'Forty-' + Num2Str(Num mod 10);
               3: if Num = 30 then
                    Num2Str := 'Thirty'
                  else
                    Num2Str := 'Thirty-' + Num2Str(Num mod 10);
               2: if Num = 20 then
                    Num2Str := 'Twenty'
                  else
                    Num2Str := 'Twenty-' + Num2Str(Num mod 10);
             0,1: case Num of
                     0: Num2Str := 'Zero';
                     1: Num2Str := 'One';
                     2: Num2Str := 'Two';
                     3: Num2Str := 'Three';
                     4: Num2Str := 'Four';
                     5: Num2Str := 'Five';
                     6: Num2Str := 'Six';
                     7: Num2Str := 'Seven';
                     8: Num2Str := 'Eight';
                     9: Num2Str := 'Nine';
                    10: Num2Str := 'Ten';
                    11: Num2Str := 'Eleven';
                    12: Num2Str := 'Twelve';
                    13: Num2Str := 'Thirteen';
                    14: Num2Str := 'Fourteen';
                    15: Num2Str := 'Fifteen';
                    16: Num2Str := 'Sixteen';
                    17: Num2Str := 'Seventeen';
                    18: Num2Str := 'Eightteen';
                    19: Num2Str := 'Nineteen'
                  end
           end
  end {Num2Str};

begin
  Dollars := Trunc(Amount);
  cents := Round(100*Frac(Amount));
  if Dollars > 0 then
    Result := Num2Str(Dollars) + ' and ';
  if cents > 0 then
    Result := Result + IntToStr(cents) + '/100'
  else
    Result := Result +  '00/100';
end;

procedure TAppMainForm.MaskEditDollarsExit(Sender: TObject);
begin
  EditWordDollars.Text :=
    RealToCheckString(StrToFloat(MaskEditDollars.text));
end;

procedure TAppMainForm.FormShow(Sender: TObject);
begin
  DBLookupComboPlus1.SetFocus;
end;

end.

