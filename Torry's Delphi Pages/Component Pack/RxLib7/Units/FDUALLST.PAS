{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2001,2002 SGB Software          }
{         Copyright (c) 1997, 1998 Fedor Koshevnikov,   }
{                        Igor Pavluk and Serge Korolev  }
{                                                       }
{*******************************************************}

unit FDualLst;

{$I RX.INC}
{$L-,S-}

interface

uses SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, RXCtrls,
  ExtCtrls, Buttons;

type
  TRxDualListForm = class(TForm)
    SrcList: TTextListBox;
    DstList: TTextListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncBtn: TButton;
    IncAllBtn: TButton;
    ExclBtn: TButton;
    ExclAllBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Bevel1: TBevel;
    procedure IncBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExclBtnClick(Sender: TObject);
    procedure ExclAllBtnClick(Sender: TObject);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListClick(Sender: TObject);
  private
    { Private declarations }
    function GetShowHelp: Boolean;
    procedure SetShowHelp(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    procedure SetButtons;
    property ShowHelp: Boolean read GetShowHelp write SetShowHelp
      default True;
end;

implementation

uses Consts, VCLUtils, BoxProcs;

{$R *.DFM}

{ TRxDualListForm }

procedure TRxDualListForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TRxDualListForm.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (SrcList.Items.Count = 0);
  DstEmpty := (DstList.Items.Count = 0);
  IncBtn.Enabled := not SrcEmpty and (SrcList.SelCount > 0);
  IncAllBtn.Enabled := not SrcEmpty;
  ExclBtn.Enabled := not DstEmpty and (DstList.SelCount > 0);
  ExclAllBtn.Enabled := not DstEmpty;
end;

function TRxDualListForm.GetShowHelp: Boolean;
begin
  Result := (HelpBtn.Enabled) and (HelpBtn.Visible);
end;

procedure TRxDualListForm.SetShowHelp(Value: Boolean);
const
  x_FrmBtn = 16;
  x_GrpBtn = 15;
  x_BtnBtn = 8;
begin
  with HelpBtn do begin
    Enabled := Value;
    Visible := Value;
  end;
  if Value then begin
    HelpBtn.Left := Width - HelpBtn.Width - x_FrmBtn;
    CancelBtn.Left := HelpBtn.Left - CancelBtn.Width - x_GrpBtn;
    OkBtn.Left := CancelBtn.Left - OkBtn.Width - x_BtnBtn;;
  end
  else begin
    CancelBtn.Left := Width - CancelBtn.Width - x_FrmBtn;
    OkBtn.Left := CancelBtn.Left - OkBtn.Width - x_BtnBtn;;
  end;
end;

procedure TRxDualListForm.IncBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(SrcList, DstList);
  SetButtons;
end;

procedure TRxDualListForm.IncAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(SrcList, DstList);
  SetButtons;
end;

procedure TRxDualListForm.ExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(DstList, SrcList);
  SetButtons;
end;

procedure TRxDualListForm.ExclAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(DstList, SrcList);
  SetButtons;
end;

procedure TRxDualListForm.SrcListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(SrcList, Source, X, Y, State, Accept, SrcList.Sorted);
  if State = dsDragLeave then
    (Source as TTextListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TTextListBox).SelCount > 1) then
    (Source as TTextListBox).DragCursor := crMultiDrag;
end;

procedure TRxDualListForm.DstListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DstList, Source, X, Y, State, Accept, DstList.Sorted);
  if State = dsDragLeave then
    (Source as TTextListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TTextListBox).SelCount > 1) then
    (Source as TTextListBox).DragCursor := crMultiDrag;
end;

procedure TRxDualListForm.SrcListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = DstList then ExclBtnClick(SrcList)
  else if Source = SrcList then begin
    BoxMoveFocusedItem(SrcList, SrcList.ItemAtPos(Point(X, Y), True));
  end;
end;

procedure TRxDualListForm.DstListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = SrcList then IncBtnClick(DstList)
  else if Source = DstList then begin
    BoxMoveFocusedItem(DstList, DstList.ItemAtPos(Point(X, Y), True));
  end;
end;

procedure TRxDualListForm.SrcListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not SrcList.Sorted then begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then begin
      if Key = VK_DOWN then Incr := 1
      else Incr := -1;
      BoxMoveFocusedItem(SrcList, SrcList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TRxDualListForm.DstListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not DstList.Sorted then begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then begin
      if Key = VK_DOWN then Incr := 1
      else Incr := -1;
      BoxMoveFocusedItem(DstList, DstList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TRxDualListForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TRxDualListForm.FormCreate(Sender: TObject);
begin
  OkBtn.Caption := ResStr(SOKButton);
  CancelBtn.Caption := ResStr(SCancelButton);
  HelpBtn.Caption := ResStr(SHelpButton);
  if NewStyleControls then Font.Style := [];
end;

procedure TRxDualListForm.ListClick(Sender: TObject);
begin
  SetButtons;
end;

end.
