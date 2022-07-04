///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMPlotEdit;

{$B-,X+}

interface

uses
  Classes, Graphics, Forms, Buttons, Controls, StdCtrls, DesignIntf, DesignEditors;
  
type
  TAxisProperty=class(TClassProperty)
  public
    { Public declarations }
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TSerieProperty=class(TClassProperty)
  public
    { Public declarations }
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TAreaBorderProperty=class(TOrdinalProperty)
  private
    { Private declarations }
    function S12(S1, S2: string): string;
  public
    { Public declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses DMPlot, DMContainer, AxisDlg, SerieDlg, Windows, SysUtils, DMHTMLText;

{ TAxisProperty }

procedure TAxisProperty.Edit;
begin
  with TAxisPropsForm.Create(Application) do
  try
    if Execute(TAxis(GetOrdValue)) 
    then Modified; {notify designer if changed}
  finally
    Free;
  end;
end;

function TAxisProperty.GetAttributes: TPropertyAttributes;
begin 
  Result:=[paDialog, paSubProperties]; 
end;

{ TSerieProperty }

procedure TSerieProperty.Edit;
var 
  Serie: TSerie; 
  I: integer;
begin
  Serie:=TSerie(GetOrdValue);
  if Serie=nil then 
  begin 
    MessageBeep($ffffffff); 
    Exit; 
  end;
  SeriePropsForm:=TSeriePropsForm.Create(Application);
  SeriePropsForm.XColumnComboBox.Clear;
  for I:=1 to MaxCols do
  SeriePropsForm.XColumnComboBox.Items.Add('Column '+IntToStr(I));
  with SeriePropsForm do 
  begin
    YColumnComboBox.Items.Assign(XColumnComboBox.Items);
    XErrorBarColumnComboBox.Items.Assign(XColumnComboBox.Items);
    YErrorBarColumnComboBox.Items.Assign(XColumnComboBox.Items);
    WorksheetComboBox.Enabled:=false; // <- to be removed?!
{TODO: It's a good idea to fill worksheet combobox with form's containers
references. Also we can support global stringlist for expressions history!}
  end;  
  try
    if SeriePropsForm.Execute(Serie) then Modified;
  finally
    SeriePropsForm.Free;
  end;
end;

function TSerieProperty.GetAttributes: TPropertyAttributes;
begin 
  Result:=[paDialog]; 
end;

{ TAreaBorderProperty }

function TAreaBorderProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList]; 
end;

function TAreaBorderProperty.GetValue: string;
var 
  I: integer;
  Serie: TSerie; 
begin
  if GetComponent(0) is TSerie
  then Serie:=TSerie(GetComponent(0)) else
  begin 
    MessageBeep($ffffffff); 
    Exit; 
  end;
  if (Serie.AreaBorder<=abcNothing) 
    or (Serie.AreaBorder>Serie.Collection.Count)//nil
  then Result:='<no value>'
  else Result:='?????'; // something strange...
  if Serie.AreaBorder=abcXAxis
  then Result:=S12((Serie.Collection as TSeries).Plot.XAxis.Title, 'X Axis') else
  if Serie.AreaBorder=abcYAxis
  then Result:=S12((Serie.Collection as TSeries).Plot.YAxis.Title, 'Y Axis') else
  if Serie.AreaBorder=abcXAxis2
  then Result:=S12((Serie.Collection as TSeries).Plot.XAxis2.Title, 'X Axis 2') else
  if Serie.AreaBorder=abcYAxis2
  then Result:=S12((Serie.Collection as TSeries).Plot.YAxis2.Title, 'Y Axis 2') else
  for I:=0 to Serie.Collection.Count-1 do
  if Serie.AreaBorder=(Serie.Collection.Items[I] as TSerie).ID then
  begin
    Result:=S12((Serie.Collection.Items[I] as TSerie).Text, 'Series '+
      IntToStr((Serie.Collection.Items[I] as TSerie).Index));
    Break;
  end;
end;

procedure TAreaBorderProperty.GetValues(Proc: TGetStrProc);
var
  I: integer;
  Serie: TSerie;
begin
  if GetComponent(0) is TSerie
  then Serie:=TSerie(GetComponent(0)) else
  begin 
    MessageBeep($ffffffff); 
    Exit; 
  end;
  Proc(S12((Serie.Collection as TSeries).Plot.XAxis.Title, 'X Axis'));
  Proc(S12((Serie.Collection as TSeries).Plot.YAxis.Title, 'Y Axis'));
  Proc(S12((Serie.Collection as TSeries).Plot.XAxis2.Title, 'X Axis 2'));
  Proc(S12((Serie.Collection as TSeries).Plot.YAxis2.Title, 'Y Axis 2'));
  for I:=0 to Serie.Collection.Count-1 do
//!!!  if Serie.Collection.Items[I]<>Serie then
  begin
    Proc(S12((Serie.Collection.Items[I] as TSerie).Text, 'Series '+
      IntToStr((Serie.Collection.Items[I] as TSerie).Index)));
  end;
end;

function TAreaBorderProperty.S12(S1, S2: string): string;
begin
  if S1<>''
  then S2:=S2+' {'+S1+'}';
  S12:=HTML2Text(S2);
end;

procedure TAreaBorderProperty.SetValue(const Value: string);
var
  I,Obj: integer;
  Serie: TSerie;
begin
  //inherited;
  if GetComponent(0) is TSerie
  then Serie:=TSerie(GetComponent(0)) else
  begin 
    MessageBeep($ffffffff); 
    Exit; 
  end;
  Obj:=abcNothing;
  if Pos('X Axis 2', Value)>0 // before XAxis!
  then Obj:=abcXAxis2 else
  if Pos('X Axis', Value)>0
  then Obj:=abcXAxis else
  if Pos('Y Axis2', Value)>0
  then Obj:=abcYAxis2 else
  if Pos('Y Axis', Value)>0
  then Obj:=abcYAxis else
  for I:=0 to Serie.Collection.Count-1 do
  if Pos('Series '+IntToStr((Serie.Collection.Items[I] as TSerie).Index), 
    Value)>0 then
  begin
    Obj:=(Serie.Collection.Items[I] as TSerie).ID;
    Break;
  end;
  Serie.AreaBorder:=Obj;
  Modified;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAxis), TPlot, '', TAxisProperty);
  RegisterPropertyEditor(TypeInfo(TSerie), TPlot, '', TSerieProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TSerie, 
    'AreaBorder', TAreaBorderProperty);
end;

end.
