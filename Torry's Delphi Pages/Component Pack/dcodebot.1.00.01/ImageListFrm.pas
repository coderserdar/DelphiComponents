
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ImageListFrm;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList;

{ TImageListForm }

type
  TImageListForm = class(TForm)
    CloseButton: TButton;
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
  private
    FImageList: TCustomImageList;
    procedure SetImageList(Value: TCustomImageList);
  public
    property ImageList: TCustomImageList read FImageList write SetImageList;
  end;

procedure ViewImageList(Images: TCustomImageList);

implementation

{$R *.DFM}

{ TImageListForm }

procedure TImageListForm.FormCreate(Sender: TObject);
begin
  ClientHeight := CloseButton.Top + 32;
  ClientWidth := ListView.Left + CloseButton.Left + CloseButton.Width;
end;

procedure TImageListForm.SetImageList(Value: TCustomImageList);
var
  I: Integer;
begin
  FImageList := Value;
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  if FImageList <> nil then
    for I := 0 to FImageList.Count - 1 do
      with ListView.Items.Add do
      begin
        Caption := IntToStr(I);
        ImageIndex := I;
      end;
  ListView.LargeImages := FImageList;
  ListView.Items.EndUpdate;
end;

procedure ViewImageList(Images: TCustomImageList);
begin
  with TImageListForm.Create(Application) do
  try
    ImageList := Images;
    ShowModal;
  finally
    Free;
  end;
end;

end.
