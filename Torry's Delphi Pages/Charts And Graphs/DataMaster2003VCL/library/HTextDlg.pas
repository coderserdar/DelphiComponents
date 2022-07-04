///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2008 RRR     //
///////////////////////////////////////

unit HTextDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ActnList, StdCtrls, ToolWin, ComCtrls, ImgList,
  Buttons;

type
  THTMLTextForm = class(TForm)
    ToolBar: TToolBar;
    Memo: TMemo;
    PreviewPaintBox: TPaintBox;
    ActionList: TActionList;
    Splitter: TSplitter;
    ImageList: TImageList;
    BoldAction: TAction;
    ItalicAction: TAction;
    UnderlineAction: TAction;
    SuperscriptAction: TAction;
    SubscriptAction: TAction;
    SymbolAction: TAction;
    UndoAction: TAction;
    BoldToolButton: TToolButton;
    ItalicToolButton: TToolButton;
    UnderlineToolButton: TToolButton;
    SuperscriptToolButton: TToolButton;
    SubscriptToolButton: TToolButton;
    SToolButton1: TToolButton;
    SymbolComboBox: TComboBox;
    SymbolToolButton: TToolButton;
    UndoToolButton: TToolButton;
    CancelBitBtn: TBitBtn;
    HelpBitBtn: TBitBtn;
    OkBitBtn: TBitBtn;
    SToolButton2: TToolButton;
    SToolButton3: TToolButton;
    procedure TagActionExecute(Sender: TObject);
    procedure TagActionUpdate(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure PreviewPaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
    procedure SymbolActionExecute(Sender: TObject);
  private
    { Private declarations }
    FUndoText: string;
    procedure TagSelection(const Tag: string);
  public
    { Public declarations } 
    function Execute(var Txt: string; C: TColor=clBtnFace; Fnt: TFont=nil): boolean;
  end;

var
  HTMLTextForm: THTMLTextForm;

implementation

uses DMHTMLText;

{$R *.dfm}

function THTMLTextForm.Execute(var Txt: string; C: TColor=clBtnFace; Fnt: TFont=nil): boolean;
begin
  Memo.Text:=Txt;
  if Assigned(Fnt)
  then PreviewPaintBox.Font:=Fnt;
  if C<>PreviewPaintBox.Color
  then PreviewPaintBox.Color:=C;
  FUndoText:='';
  Result:=(ShowModal=mrOk) and Memo.Modified;
  if Result then Txt:=Memo.Text;
end;

procedure THTMLTextForm.TagSelection(const Tag: string);
var
  S, SB, SE, buf: string;
  SSt: integer;
begin
  if Memo.SelLength=0
  then Exit;
  SSt:=Memo.SelStart;
  S:=Copy(Memo.Text, SSt+1, Memo.SelLength);
  SB:=Copy(Memo.Text, 0, SSt);
  SE:=Copy(Memo.Text, SSt+1+Memo.SelLength, 
    Length(Memo.Text)-SSt-Memo.SelLength);
  buf:=Memo.Text;
  Memo.Text:=SB+'<'+Tag+'>'+S+'</'+Tag+'>'+SE;
  Memo.SelStart:=SSt;
  Memo.Modified:=true;
  FUndoText:=buf; // after assignment, because FUndoText cleared in MemoChange 
end;

procedure THTMLTextForm.TagActionExecute(Sender: TObject);
begin
  if Sender=BoldAction
  then TagSelection('b');
  if Sender=ItalicAction
  then TagSelection('i');
  if Sender=UnderlineAction
  then TagSelection('u');
  if Sender=SubscriptAction
  then TagSelection('sub');
  if Sender=SuperscriptAction
  then TagSelection('sup');
end;

procedure THTMLTextForm.TagActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Memo.SelLength>0;
end;

procedure THTMLTextForm.MemoChange(Sender: TObject);
begin
  PreviewPaintBox.Refresh;
  FUndoText:='';
end;

procedure THTMLTextForm.PreviewPaintBoxPaint(Sender: TObject);
begin
  PreviewPaintBox.Canvas.Brush.Color:=PreviewPaintBox.Color; //  paint bkg
  PreviewPaintBox.Canvas.FillRect(PreviewPaintBox.ClientRect);
  HTMLTextOut(PreviewPaintBox.Canvas, Memo.Text, 5, 5, false); // paint text
end;

procedure THTMLTextForm.FormCreate(Sender: TObject);
var
  I: integer;
begin
  for I:=0 to High(Entities) do
  if Entities[I].IsSym then 
  SymbolComboBox.Items.AddObject(Entities[I].Text, pointer(I));
  SymbolComboBox.ItemIndex:=0;
end;

procedure THTMLTextForm.UndoActionExecute(Sender: TObject);
begin
  if FUndoText<>'' then
  begin
    Memo.Text:=FUndoText;
    FUndoText:='';
    Exit;
  end;
  if Memo.CanUndo
  then Memo.Undo;   
end;

procedure THTMLTextForm.UndoActionUpdate(Sender: TObject);
begin
  UndoAction.Enabled:=(FUndoText<>'') or Memo.CanUndo;
end;

procedure THTMLTextForm.SymbolActionExecute(Sender: TObject);
var
  S, SB, SE, buf: string;
  SSt: integer;
begin
  if SymbolComboBox.ItemIndex<3 // must be in sync with SymbolComboBox.Items! 
  then S:=Entities[SymbolComboBox.ItemIndex].Html
  else S:=Entities[integer(SymbolComboBox.Items.Objects[SymbolComboBox.ItemIndex])].Html;
  SSt:=Memo.SelStart;
  SB:=Copy(Memo.Text, 0, SSt);
  SE:=Copy(Memo.Text, SSt+1+Memo.SelLength, 
    Length(Memo.Text)-SSt-Memo.SelLength);
  buf:=Memo.Text;
  Memo.Text:=SB+'&'+S+';'+SE;
  Memo.SelStart:=SSt;
  Memo.Modified:=true;
  FUndoText:=buf; // after assignment, because FUndoText cleared in MemoChange 
end;

end.
