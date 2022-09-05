unit WinForm;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, CalcExpress, SysUtils;

type
  TWinForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    lbVariables: System.Windows.Forms.ListBox;
    lbValues: System.Windows.Forms.ListBox;
    Button1: System.Windows.Forms.Button;
    tbExpr: System.Windows.Forms.TextBox;
    Button2: System.Windows.Forms.Button;
    tbResult: System.Windows.Forms.TextBox;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure Button2_Click(sender: System.Object; e: System.EventArgs);
    procedure Button1_Click(sender: System.Object; e: System.EventArgs);
    procedure TWinForm_Load(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create;
  end;

implementation

{$R 'WinForm.TWinForm.resources'}

uses
  System.Globalization;

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWinForm.InitializeComponent;
begin
  Self.lbVariables := System.Windows.Forms.ListBox.Create;
  Self.lbValues := System.Windows.Forms.ListBox.Create;
  Self.Button1 := System.Windows.Forms.Button.Create;
  Self.tbExpr := System.Windows.Forms.TextBox.Create;
  Self.Button2 := System.Windows.Forms.Button.Create;
  Self.tbResult := System.Windows.Forms.TextBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.SuspendLayout;
  // 
  // lbVariables
  // 
  Self.lbVariables.Location := System.Drawing.Point.Create(17, 160);
  Self.lbVariables.Name := 'lbVariables';
  Self.lbVariables.Size := System.Drawing.Size.Create(120, 147);
  Self.lbVariables.TabIndex := 0;
  // 
  // lbValues
  // 
  Self.lbValues.Location := System.Drawing.Point.Create(425, 160);
  Self.lbValues.Name := 'lbValues';
  Self.lbValues.Size := System.Drawing.Size.Create(120, 147);
  Self.lbValues.TabIndex := 1;
  // 
  // Button1
  // 
  Self.Button1.Location := System.Drawing.Point.Create(17, 323);
  Self.Button1.Name := 'Button1';
  Self.Button1.Size := System.Drawing.Size.Create(216, 23);
  Self.Button1.TabIndex := 2;
  Self.Button1.Text := 'Calculate result';
  Include(Self.Button1.Click, Self.Button1_Click);
  // 
  // tbExpr
  // 
  Self.tbExpr.Location := System.Drawing.Point.Create(353, 97);
  Self.tbExpr.Name := 'tbExpr';
  Self.tbExpr.Size := System.Drawing.Size.Create(192, 20);
  Self.tbExpr.TabIndex := 3;
  Self.tbExpr.Text := 'cos(x)*y';
  // 
  // Button2
  // 
  Self.Button2.Location := System.Drawing.Point.Create(329, 323);
  Self.Button2.Name := 'Button2';
  Self.Button2.Size := System.Drawing.Size.Create(216, 23);
  Self.Button2.TabIndex := 4;
  Self.Button2.Text := 'Close';
  Include(Self.Button2.Click, Self.Button2_Click);
  // 
  // tbResult
  // 
  Self.tbResult.Location := System.Drawing.Point.Create(353, 129);
  Self.tbResult.Name := 'tbResult';
  Self.tbResult.Size := System.Drawing.Size.Create(192, 20);
  Self.tbResult.TabIndex := 5;
  Self.tbResult.Text := '';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(17, 94);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(152, 23);
  Self.Label1.TabIndex := 6;
  Self.Label1.Text := 'Mathematical expression:';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(17, 126);
  Self.Label2.Name := 'Label2';
  Self.Label2.TabIndex := 7;
  Self.Label2.Text := 'Result:';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(16, 8);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(528, 40);
  Self.Label3.TabIndex := 8;
  Self.Label3.Text := 'CalcExpress is a freeware product. You can use it as you like at your own risk.'+ 
  ' There is no warranty of any kind. Visit our web site to get last version of t'+ 
  'his product (Delphi, C++ Builder or Kylix are supported). ';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(16, 56);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(528, 32);
  Self.Label4.TabIndex := 9;
  Self.Label4.Text := '(c) AidAim Software LLC (Tools for software developers, database and backup com'+ 
  'ponents for Delphi and C++ Builder). Web site: http://www.aidaim.com.';
  // 
  // TWinForm
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(576, 357);
  Self.Controls.Add(Self.Label4);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.tbResult);
  Self.Controls.Add(Self.Button2);
  Self.Controls.Add(Self.tbExpr);
  Self.Controls.Add(Self.Button1);
  Self.Controls.Add(Self.lbValues);
  Self.Controls.Add(Self.lbVariables);
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TWinForm';
  Self.Text := 'CalcExpress for Delphi.Net Demo. (c) AidAim Software LLC, 2000-2003. http://www'+ 
  '.aidaim.com';
  Include(Self.Load, Self.TWinForm_Load);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TWinForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TWinForm.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
end;

procedure TWinForm.TWinForm_Load(sender: System.Object; e: System.EventArgs);
var s: String;
begin
  s := 'x';
  lbVariables.Items.Add(s);
  s := 'y';
  lbVariables.Items.Add(s);
  s := '0';
  lbValues.Items.Add(s);
  s := '2';
  lbValues.Items.Add(s);
end;

procedure TWinForm.Button1_Click(sender: System.Object; e: System.EventArgs);
var I:    Integer;
    calc: TCalcExpress;
    res:  Extended;
    vals: array of Extended;
begin

  calc := TCalcExpress.Create(Self);
  for I := 0 to lbVariables.Items.Count - 1 do
   calc.Variables.Add(lbVariables.Items[i].ToString);
  calc.set_Formula(tbExpr.Text);
  SetLength(vals,lbValues.Items.Count);
  for I := 0 to lbValues.Items.Count - 1 do
   vals[i] := StrToFloat(lbValues.Items[i].ToString);
  res := calc.calc(vals);
  tbResult.Text := FloatToStr(res);

end;

procedure TWinForm.Button2_Click(sender: System.Object; e: System.EventArgs);
begin
  Close;
end;

end.
