unit SegSel;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TSelForm = class(TForm)
    Panel1: TPanel;
    ApplyBtn: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    GroupBox1: TGroupBox;
    ETarget: TEdit;
    LB: TListBox;
    FileCnt: TLabel;
    CanBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormResize(Sender: TObject);
    procedure LBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ApplyBtnClick(Sender: TObject);
    procedure ETargetExit(Sender: TObject);
    procedure CanBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateCount;
  public
    { Public declarations }
    procedure EntryPoint( var Target: String; var SL: TStringList );
  end;

var
  SelForm: TSelForm;

implementation

uses SegMain;

{$R *.DFM}
var
  SaveTarget:        String;

procedure TSelForm.UpdateCount;
begin
     FileCnt.Caption := 'Files: ' + IntToStr(LB.Items.Count);
     FileCnt.Update;
end;

procedure TSelForm.EntryPoint( var Target: String; var SL: TStringList );
var
   i:      Integer;
begin
     SaveTarget := Target;
     ETarget.Text := Target;
     LB.Clear;
     for i := 0 to pred(SL.Count) do
       LB.Items.Add(SL[i]);
     UpdateCount;
     ShowModal;
     SL.Clear;
     Target := ETarget.Text;
     for i := 0 to pred(LB.Items.Count) do
       SL.Add(LB.Items[i]);
end;

procedure TSelForm.FormResize(Sender: TObject);
begin
     if (Height < 255) then
       Height := 255;
     if (Width < 325) then
       Width := 325;
end;

procedure TSelForm.LBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
   i:     Integer;
begin
     if (Key = VK_DELETE) then
       begin
       while (LB.SelCount > 0) do
         begin
         for i := 0 to pred(LB.Items.Count) do
           begin
           if (LB.Selected[i]) then
             begin
             LB.Items.Delete(i);
             break;
             end;
           end;
         end;
       UpdateCount;
       LB.Update;
       end;
end;

procedure TSelForm.ApplyBtnClick(Sender: TObject);
begin
     Close;
end;

procedure TSelForm.ETargetExit(Sender: TObject);
begin
     if (ETarget.Text = '') then
       ETarget.Text := SaveTarget;
end;


procedure TSelForm.CanBtnClick(Sender: TObject);
begin
     SegForm.SegLHA.AbortOperation := True;
     Close;
end;

end.
