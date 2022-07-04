{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{                                                       }
{*******************************************************}

unit rxHintProp;

interface

{$I RX.INC}

uses
  {$IFDEF RX_D6} DesignIntf, VCLEditors {$ELSE} DsgnIntf {$ENDIF}; // Polaris

type

{ THintProperty }

  THintProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure Edit; override;
  end;

implementation

 {$D-}

uses
  SysUtils, Classes, {$IFDEF RX_D3} rxStrLEdit, {$ELSE} rxStrEdit, {$ENDIF}
  TypInfo, Forms, Controls, rxStrUtils;

function THintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function THintProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else
    Result := 1024;
end;

procedure THintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
  I, Cnt: Integer;
begin
  with TStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else
      Caption := GetName;
    Temp := GetStrValue;
    Cnt := WordCount(Temp, [#13, #10]);
    for I := 1 to Cnt do
      Memo.Lines.Add(ExtractWord(I, Temp, [#13, #10]));
    Memo.MaxLength := GetEditLimit;
    UpdateStatus(nil);
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

end.