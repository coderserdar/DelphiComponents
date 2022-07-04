unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_grbutton, API_valueholder, ExtCtrls, API_base;

type
  TForm1 = class(TForm)
    tAPI_grbutton1: tAPI_grbutton;
    tAPI_grbutton2: tAPI_grbutton;
    tAPI_grbutton3: tAPI_grbutton;
    tAPI_grbutton4: tAPI_grbutton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    tAPI_ValueHolder1: tAPI_ValueHolder;
    tAPI_grbutton5: tAPI_grbutton;
    tAPI_grbutton6: tAPI_grbutton;
    tAPI_grbutton7: tAPI_grbutton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label5: TLabel;
    procedure tAPI_grbutton1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure tAPI_grbutton3Click(Sender: TObject);
    procedure tAPI_grbutton2Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure tAPI_grbutton5Click(Sender: TObject);
    procedure tAPI_grbutton6Click(Sender: TObject);
    procedure tAPI_grbutton7Click(Sender: TObject);
    procedure tAPI_grbutton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure updategrplist;
    procedure updateitmlist(grp:string);
    procedure updatelabels;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.updategrplist;
var
  i: integer;
  sl: tstringlist;
//  grp: string;
begin
  listbox1.clear;

  { this is how it is done with first..last
  grp:=tapi_valueholder1.Group_First;
  if grp<>'' then
  begin
    listbox1.Items.add(grp);
    while not tapi_valueholder1.Group_Eof do
      listbox1.items.add(tapi_valueholder1.Group_Next);
  end;
  }

  sl:=tstringlist.create;
  tapi_valueholder1.grouplist(sl);
  for i:=0 to sl.count-1 do
  begin
    listbox1.Items.Add(sl[i]);
  end;
  sl.free;
end;

procedure TForm1.updateitmlist(grp:string);
var
  i: integer;
  sl: tstringlist;
begin
  listbox2.clear;

  sl:=tstringlist.create;
  tapi_valueholder1.itemlist(grp,sl);
  for i:=0 to sl.count-1 do
    listbox2.items.add(sl[i]);
  sl.free;
end;

procedure TForm1.updatelabels;
begin
  label1.caption:=inttostr(tapi_valueholder1.GroupIndex);
  label2.caption:=inttostr(tapi_valueholder1.ItemIndex);
  label3.caption:=tapi_valueholder1.Group;
  label4.caption:=tapi_valueholder1.Item;
  if tapi_valueholder1.Modified then
    label5.caption:='Modified'
    else label5.caption:='-';
end;

procedure TForm1.tAPI_grbutton1Click(Sender: TObject);
begin
  if tapi_valueholder1.Group_Add(edit1.text) then
  begin
    updategrplist;
    updatelabels;
  end else
    showmessage('Failed to add group.');
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  grp: string;
begin
  if listbox1.itemindex<>-1 then
  begin
    grp:=listbox1.items[listbox1.itemindex];
    updateitmlist(grp);
    updatelabels;
  end;
end;

procedure TForm1.tAPI_grbutton3Click(Sender: TObject);
begin
  if tapi_valueholder1.Item_Add(tapi_valueholder1.Group,edit2.text) then
  begin
    updateitmlist(tapi_valueholder1.group);
    updatelabels;
  end else
    showmessage('Failed to add item.');
end;

procedure TForm1.tAPI_grbutton2Click(Sender: TObject);
begin
   if tapi_valueholder1.Group_Delete(tapi_valueholder1.group) then
   begin
     listbox2.Clear;
     updategrplist;
     updatelabels;
   end else
     showmessage('Failed to delete group.');
end;

procedure TForm1.ListBox2Click(Sender: TObject);
var
  grp: string;
  itm: string;
begin
  if (listbox2.itemindex<>-1) and (listbox1.itemindex<>-1) then
  begin
    grp:=listbox1.items[listbox1.itemindex];
    itm:=listbox2.items[listbox2.itemindex];
    if tapi_valueholder1.Item_Locate(grp,itm) then
    begin
      updatelabels;
    end else
      showmessage('Did not find '+#13+'Grp='+grp+#13+'Itm='+itm+#13);
  end;
end;

procedure TForm1.tAPI_grbutton5Click(Sender: TObject);
begin
  tapi_valueholder1.Clear;
  listbox2.Clear;
  updategrplist;
  updatelabels;
end;

procedure TForm1.tAPI_grbutton6Click(Sender: TObject);
begin
  if opendialog1.execute then
  begin
    if tapi_valueholder1.Load(opendialog1.filename) then
    begin
      listbox2.clear;
      updategrplist;
      updatelabels;
    end else
      showmessage('Failed to load valueholder.');
  end;
end;

procedure TForm1.tAPI_grbutton7Click(Sender: TObject);
begin
  if savedialog1.execute then
  begin
    if tapi_valueholder1.save(Savedialog1.filename) then
    begin
      showmessage('Save ok.');
    end else
      showmessage('Failed to save valueholder.');
  end;
end;

procedure TForm1.tAPI_grbutton4Click(Sender: TObject);
var
  itm: string;
begin
  itm:=tapi_valueholder1.Item;
  if itm<>'' then
  if tapi_valueholder1.Item_Delete(tapi_valueholder1.Group,itm) then
  begin
    updateitmlist(tapi_valueholder1.Group);
    updatelabels;
  end else
    showmessage('Failed to delete item.');
end;

end.
