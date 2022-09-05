//*****************************************************************
//   Property editor for ZReport
//
//   2000 (c) Constantin M. Lushnikov <k_l@newmail.ru>
//*****************************************************************
unit ZRCapton;

interface

{$I ZRDefine.inc}

uses
  Windows, Classes, Graphics, Forms, Controls, Buttons, Dialogs,
{$IFDEF D6Above}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  RTLConsts,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  StdCtrls, ExtCtrls, TypInfo;

type

{ TZRCaptionProperty }

  TZRCaptionProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure Edit; override;
  end;

{ TZRCaptionForm }

  TZRCaptionForm = class(TForm)
    Memo: TMemo;
    LineCount: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OKBtn: TButton;
    CancelBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure UpdateStatus(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{$R *.DFM}

uses
  SysUtils, ZRCtrls;

{ TZRCaptionForm }

procedure TZRCaptionForm.FileOpen(Sender: TObject);
begin
  with OpenDialog do
    if Execute then Memo.Lines.LoadFromFile(FileName);
end;

procedure TZRCaptionForm.FileSave(Sender: TObject);
begin
  SaveDialog.FileName := OpenDialog.FileName;
  with SaveDialog do
    if Execute then Memo.Lines.SaveToFile(FileName);
end;

procedure TZRCaptionForm.UpdateStatus(Sender: TObject);
var
  Count: Integer;
begin
  Count := Memo.Lines.Count;
  if Count = 1 then
    LineCount.Caption := Format('%d Line', [Count])
  else
    LineCount.Caption := Format('%d Lines', [Count]);
end;

procedure TZRCaptionForm.MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then CancelBtn.Click;
end;

{ TZRCaptionProperty }

function TZRCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TZRCaptionProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else Result := 2048;
end;

procedure TZRCaptionProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
begin
  with TZRCaptionForm.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else
      Caption := GetName;
    if (Comp is TZRCustomLabel) and Assigned(TZRCustomLabel(Comp).Report) then
      Memo.Font.Assign(TZRCustomLabel(Comp).Report.Font);
    Temp := GetStrValue;
    Memo.Lines.Text := Temp;
    Memo.MaxLength := GetEditLimit;
    if ShowModal = mrOk then begin
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
