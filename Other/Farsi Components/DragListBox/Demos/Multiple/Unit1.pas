unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DragListBox;

type
  TForm1 = class(TForm)
    DragListBox1: TDragListBox;
    DragListBox2: TDragListBox;
    Label1: TLabel;
    DragListBox3: TDragListBox;
    Label2: TLabel;
    DragListBox4: TDragListBox;
    Label3: TLabel;
    DragListBox5: TDragListBox;
    Label4: TLabel;
    DragListBox6: TDragListBox;
    Label5: TLabel;
    DragListBox7: TDragListBox;
    Label6: TLabel;
    DragListBox8: TDragListBox;
    Label7: TLabel;
    DragListBox9: TDragListBox;
    Label8: TLabel;
    Label9: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Button1: TButton;
    procedure DragListBox2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DragListBox2DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if ((Sender as TListBox).Items.Count=4) and (Sender<>Source) then
    Accept:= False; 
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
