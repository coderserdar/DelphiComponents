unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgTools, ExtCtrls;

type
  TMainForm = class(TForm)
    bcMain: TBroadcaster;
    lb1: TListBox;
    cmDo1: TButton;
    lb2: TListBox;
    cmDo2: TButton;
    rgAction: TRadioGroup;
    procedure bcMainBroadcast(Sender: TObject; var Msg: TMessage;
      Item, Data: TObject; var Handled: Boolean);
    procedure cmDo1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmDo2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses vgUtils;

{$R *.DFM}

const
  WM_INSERT = WM_USER;
  WM_DELETE = WM_USER + 1;

procedure TMainForm.bcMainBroadcast(Sender: TObject;
  var Msg: TMessage; Item, Data: TObject; var Handled: Boolean);
var
  List: TStrings;
  I: Integer;
begin
  List := TStrings(Msg.LParam);
  case Msg.Msg of
    WM_INSERT:
      List.Add(TComponent(Item).Name);
    WM_DELETE:
      begin
        I := List.IndexOf(TComponent(Item).Name);
        if I >= 0 then List.Delete(I);
      end;
  end;
end;

procedure TMainForm.cmDo1Click(Sender: TObject);
begin
  bcMain.Broadcast(WM_INSERT + rgAction.ItemIndex, 0, Integer(lb1.Items));
end;

procedure TMainForm.cmDo2Click(Sender: TObject);
begin
  bcMain.Broadcast(WM_INSERT + rgAction.ItemIndex, 0, Integer(lb2.Items));
end;

procedure TMainForm.FormCreate(Sender: TObject);
  procedure DoInsert(Instance: TComponent; Data: Pointer);
  begin
    TBroadcaster(Data).InsertObject(Instance, nil);
  end;
begin
  ForEachComponent(Self, TComponent, @DoInsert, bcMain, False);
end;

end.
