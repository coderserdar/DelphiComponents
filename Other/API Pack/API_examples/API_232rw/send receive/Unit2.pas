unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_files, StdCtrls, ExtCtrls, API_base;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    API_files1: TAPI_files;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Edit3: TEdit;
    Button2: TButton;
    Button3: TButton;
    Label6: TLabel;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    procedure addtolog(txt: string);
  end;

var
  Form1: TForm1;

implementation

uses
  API_232rw;

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  com_setdefaults;
  com_seteofchar( #13 );
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  if com_isopen then com_close;
end;

//------------------------------------------------------------------------------
procedure TForm1.addtolog(txt: string);
begin
  while memo1.lines.count > 500 do
    memo1.lines.delete(0);
  memo1.lines.add(timetostr(now)+'> '+txt);
end;

//------------------------------------------------------------------------------
procedure TForm1.Button4Click(Sender: TObject);
begin
  if com_isopen then
    if com_close then addtolog('com'+inttostr( com_getport )+' closed.')
      else addtolog('failed to close com'+inttostr( com_getport )+'.');
  try
    com_setport( combobox1.ItemIndex + 1 );
    com_setbaudrate( strtoint( edit1.text ) );
    if com_open then addtolog('com'+inttostr( com_getport )+' opened succesfully.')
      else addtolog('failed to open com'+inttostr( com_getport )+'.');
  except
    addtolog('exception during port open.');
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
begin
  edit2.text:= api_files1.BrowseFolderDialog('select target folder..', edit2.text, false);
end;

//------------------------------------------------------------------------------
procedure TForm1.Button2Click(Sender: TObject);
begin
  opendialog1.Filter:='All files (*.*)|*.*';
  if opendialog1.Execute then
    edit3.text:= opendialog1.FileName;
end;

//------------------------------------------------------------------------------
procedure TForm1.Timer1Timer(Sender: TObject);
var
  txt: string;
  p: integer;
  command: string;
  folder: string;
  filename: string;
  fsize: integer;
begin
  if com_isopen then
  begin
    if com_readln(txt) then
    begin
      p:= pos(' ', txt);
      if p>0 then
      begin
        // get filename
        command:= copy(txt, 1, p-1);
        folder:= includetrailingbackslash( edit2.text );
        filename:= extractfilename( copy(txt, p+1, length(txt)) );
        addtolog('receiving file '+filename+'..');
        com_writeln('+ filename received');
        if com_waitany(txt, 1000) then
        begin
          p:= pos(' ',txt);
          if p>0 then
          begin
            // get file size
            command:= copy(txt, 1, p-1);
            fsize:= strtoint( copy(txt, p+1, length(txt)) );
            addtolog('.. filesize is '+inttostr(fsize)+' bytes.');
            com_writeln('+ filesize received');
          end else
            com_writeln('- failed to receive filesize');
        end;
      end else
        com_writeln('- failed to receive filename');
    end;
  end;
end;

end.
