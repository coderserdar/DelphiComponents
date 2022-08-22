unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls,
  Buttons, DragListBox;

type
  TDualListDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    SrcList: TDragListBox;
    DstList: TDragListBox;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetButtons;
  end;

var
  DualListDlg: TDualListDlg;

implementation

{$R *.DFM}

procedure TDualListDlg.IncludeBtnClick(Sender: TObject);
begin
  SrcList.MoveTo(DstList);
  SetButtons;
end;

procedure TDualListDlg.ExcludeBtnClick(Sender: TObject);
begin
  DstList.MoveTo(SrcList);
  SetButtons;
end;

procedure TDualListDlg.IncAllBtnClick(Sender: TObject);
begin
  SrcList.MoveAll(DstList);
  SetButtons;
end;

procedure TDualListDlg.ExcAllBtnClick(Sender: TObject);
begin
  DstList.MoveAll(SrcList);
  SetButtons;
end;

procedure TDualListDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

procedure TDualListDlg.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDualListDlg.OKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TDualListDlg.DstListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  SetButtons;
end;

end.
