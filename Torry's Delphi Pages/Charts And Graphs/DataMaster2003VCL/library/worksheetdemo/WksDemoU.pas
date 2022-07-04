///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit WksDemoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Data, Grids, WorkGrid, ExtCtrls, StdCtrls, DMContainer, DMWorksheet;

type
  TWKSDemoForm = class(TForm)
    Panel1: TPanel;
    Worksheet: TWorksheet;
    TitlesMemo: TMemo;
    Label1: TLabel;
    CopyTitlesButton: TButton;
    AlignRightCheckBox: TCheckBox;
    DrawHeadersCheckBox: TCheckBox;
    CopyButton: TButton;
    PasteButton: TButton;
    InsLinesCheckBox: TCheckBox;
    Container: TContainer;
    DeleteButton: TButton;
    OverwriteCheckBox: TCheckBox;
    EditCheckBox: TCheckBox;
    ModifiedCheckBox: TCheckBox;
    procedure CopyTitlesButtonClick(Sender: TObject);
    procedure AlignRightCheckBoxClick(Sender: TObject);
    procedure DrawHeadersCheckBoxClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditCheckBoxClick(Sender: TObject);
    procedure ContainerChanged(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WKSDemoForm: TWKSDemoForm;

implementation

{$R *.DFM}

procedure TWKSDemoForm.CopyTitlesButtonClick(Sender: TObject);
begin
  Worksheet.Header.Assign(TitlesMemo.Lines);
end;

procedure TWKSDemoForm.AlignRightCheckBoxClick(Sender: TObject);
begin
  Worksheet.AlignRight:=AlignRightCheckBox.Checked;
end;

procedure TWKSDemoForm.DrawHeadersCheckBoxClick(Sender: TObject);
begin
  Worksheet.DrawHeaders:=DrawHeadersCheckBox.Checked;
end;

procedure TWKSDemoForm.CopyButtonClick(Sender: TObject);
begin
  Worksheet.CopyToClipBoard;
end;

procedure TWKSDemoForm.PasteButtonClick(Sender: TObject);
begin
  Worksheet.PasteFromClipBoard(InsLinesCheckBox.Checked,
  OverwriteCheckBox.Checked);
end;

procedure TWKSDemoForm.DeleteButtonClick(Sender: TObject);
begin
  Worksheet.Delete;
end;

procedure TWKSDemoForm.EditCheckBoxClick(Sender: TObject);
begin
  if EditCheckBox.Checked
  then Worksheet.Options:=Worksheet.Options+[goEditing]
  else Worksheet.Options:=Worksheet.Options-[goEditing];
end;

procedure TWKSDemoForm.ContainerChanged(Sender: TObject);
begin
  ModifiedCheckBox.Checked:=true;
end;

end.
