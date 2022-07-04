{$I asqlite_def.inc}

unit AMDSqlite3;

interface

uses
  Windows, Messages, SysUtils,
{$IFDEF ASQLITE_D6PLUS}
  Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFMD = class(TForm)
    LBMaster: TListBox;
    LBDetail: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    LBLinked: TListBox;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label3: TLabel;
    BitBtn5: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMD: TFMD;

implementation

{$R *.dfm}

procedure TFMD.BitBtn1Click(Sender: TObject);
var TheStr : string;
    i      : integer;
begin
 if (LBDetail.ItemIndex < 0) or (LBMaster.ItemIndex < 0) then exit;
 TheStr := LBDetail.Items[LBDetail.ItemIndex]+'='+ LBMaster.Items[LBMaster.ItemIndex];
 for i := 0 to LBLinked.Items.Count - 1 do
    if CompareText(TheStr, LBLinked.Items[0])=0 then exit;
 LBLinked.Items.Add(TheStr);
end;

procedure TFMD.BitBtn2Click(Sender: TObject);
{$IFNDEF ASQLITE_D6PLUS}
var i : integer;
{$ENDIF}
begin
{$IFDEF ASQLITE_D6PLUS}
 LBLinked.DeleteSelected;
{$ELSE}
 i := 0;
 while i < LBLinked.Items.Count - 1 do begin
 end;
{$ENDIF}
end;

procedure TFMD.BitBtn5Click(Sender: TObject);
var i, j : integer;
begin
 for i := 0 to LBDetail.Items.Count - 1 do begin
    LBDetail.ItemIndex := i;
    for j := 0 to LBMaster.Items.Count - 1 do begin
        LBMaster.ItemIndex := j;
        if CompareText(LBDetail.Items[i], LBMaster.Items[j])=0 then
           BitBtn1Click(Sender);
    end;
 end;
end;

end.
