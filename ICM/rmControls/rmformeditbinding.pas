{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmFormEditBinding
Purpose  : dialog form for choosing key binding combinations
Date     : 10-26-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmFormEditBinding;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus;

type
  TrmFrmEditBinding = class(TForm)
    GroupBox1: TGroupBox;
    cbAlt: TCheckBox;
    cbCTRL: TCheckBox;
    cbShift: TCheckBox;
    cbxKey: TComboBox;
    Button1: TButton;
    Button2: TButton;
  private
    function GetBinding: TShortcut;
    procedure SetBinding(const Value: TShortcut);
    { Private declarations }
  public
    { Public declarations }
    property Binding:TShortcut read GetBinding write SetBinding default scNone;
  end;

implementation

{$R *.DFM}

{ TrmFrmEditBinding }

function TrmFrmEditBinding.GetBinding: TShortcut;
var
   wKey : Word;
   wShift : TShiftState;
begin
  if cbxKey.ItemIndex = 0 then
     result := scNone
  else
  begin
     ShortCutToKey(TextToShortCut(cbxKey.Text), wKey, wShift);
     wShift := [];
     if cbAlt.checked then
        wShift := wShift + [ssAlt];
     if cbCtrl.checked then
        wShift := wShift + [ssCtrl];
     if cbShift.checked then
        wShift := wShift + [ssShift];

     result := ShortCut(wKey, wShift);
  end;
end;

procedure TrmFrmEditBinding.SetBinding(const Value: TShortcut);
var
   wKey : Word;
   wShift : TShiftState;
begin
   ShortCutToKey(Value, wkey, wShift);
   cbAlt.checked := (ssAlt in wShift);
   cbCTRL.checked := (ssCtrl in wShift);
   cbShift.checked := (ssShift in wShift);
   cbxKey.ItemIndex := cbxKey.items.IndexOf(ShortCutToText(Shortcut(wKey, [])));
end;

end.
