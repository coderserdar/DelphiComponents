// PL/SQL Wrapper Object Generator cant be 100% successfull.
// Known issues:
// 1) Overloaded procedures, which have differences in parameters:
//    PL/SQL:
//        proc(p NUMBER);
//        proc(p DATE);
//   Pascal:
//        proc(p Double); overload;
//        proc(p TDateTime); overload;
//  Pascal compiler allows such declaration. But, when you will
//  call such function, you will get error - "canr resolve call".
//  Here you will need to change Pascal procedure names manually. 
//  Search in DBMS_SQL.pas for '***' for details ...
// 2) Some complex datatypes are not supported. Example - 
//  PL/SQL record with field of type PL/SQL table.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, NCOciDB, DBMS_SQL;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
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

procedure TForm1.Button1Click(Sender: TObject);
var
    o: TOCISysDbmsSql;
    c: Double;
    cust_id: Double;
    cust_name: String;
    cust_address: String;
    cust_cred: Double;
    cust_comm: String;
    rn: Double;
begin
    o := TOCISysDbmsSql.Create(nil);
    o.DatabaseName := OCIDatabase1.DatabaseName;
    try
        c := o.OpenCursor;
        try
            o.Parse(c,
                'SELECT' +
                ' customer_id,' +    {num(6)}
                ' name,' +           {vc2(45)}
                ' address,' +        {vc2(40)}
                ' credit_limit,' +   {num(9,2)}
                ' comments ' +       {long}
                'FROM customer WHERE customer_id = :ID', {dbms_sql.native} 1);
            o.DefineColumn(c, 1, Double(cust_id));
            o.DefineColumn(c, 2, cust_name, 45);
            o.DefineColumn(c, 3, cust_address, 40);
            o.DefineColumn(c, 4, cust_cred);
            o.DefineColumn(c, 5, cust_comm, 256);
            o.BindVariable(c, 'ID', 106.0);
            rn := o.ExecuteAndFetch(c, True);
            o.ColumnValue(c, 1, cust_id);
            o.ColumnValue(c, 2, cust_name);
            o.ColumnValue(c, 3, cust_address);
            o.ColumnValue(c, 4, cust_cred);
            o.ColumnValue(c, 5, cust_comm);
        finally
            o.CloseCursor(c);
        end;
    finally
        o.Free;
    end;
    with Memo1.Lines do begin
        Clear;
        Add('rows = ' + FloatToStr(rn));
        Add('ID = ' + FloatToStr(cust_id));
        Add('name = "' + cust_name + '"');
        Add('address = "' + cust_address + '"');
        Add('credit_limit = ' + FloatToStr(cust_cred));
        Add('comments = "' + cust_comm + '"');
    end;
end;

end.

