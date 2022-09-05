unit CalcExpress;

{%File 'CalcExpress.bdsproj'}

interface

uses
  Windows,
  Classes,
  StrUtils,
  Math,
  Dialogs,
  SysUtils;

const DecimalSeparator = '.';

type
  TTree = class;



  TCalcExpress = class (TComponent)
  private
    FArgs: array of extended;
    Err: boolean;
    Bc: integer;
    PrevLex, Curlex: integer;
    Pos: integer;
    FFormula: string;
    FVariables: TStrings;
    FDefaultNames: boolean;

  private
    function SetFormula(s: string): TTree;
    function gettree(s: string): TTree;
    procedure Error(s: string);
    procedure SetVariables(Value: TStrings);
    function InternalCalc(t: TTREE): extended;

  public
    constructor Create(o: TComponent); override;
    function calc(args: array of extended): extended;
  published
    property Formula: string read FFormula write FFormula;
    property Variables: TStrings read FVariables write SetVariables;
  end;

  TTree = class
  public
    num:    Integer;
    con:    String;
    l, r:   TTree;
   constructor Create;
  end;

procedure Register;

implementation


function TCalcExpress.SetFormula(s: string): TTree;
begin
  Err := False;
  FFormula := s.ToLower;
  Prevlex := 0;
  Curlex := 0;
  Pos := 1;
  bc := 0;
  Result := GetTree(FFormula.ToLower);
  if (bc <> 0) or Err then
  begin
    ShowMessage('Error in formula.');
  end;
end;

//***************************************************************

function TCalcExpress.GetTree(s: string): TTree;
  //Get number from string
  function getnumber(s: string): string;
  begin
    Result := '';
    try
      //Begin
      while (pos <= length(s)) and (s[pos] in ['0'..'9']) do
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
      if pos > length(s) then exit;
      if s[pos] = DecimalSeparator then
      begin
        //Fraction part
        Result := Result + DecimalSeparator;
        inc(pos);
        if (pos > length(s)) or not (s[pos] in ['0'..'9']) then Error('Wrong number.');
        while (pos <= length(s)) and
          (s[pos] in ['0'..'9']) do
        begin
          Result := Result + s[pos];
          inc(pos);
        end;
      end;
      if pos > length(s) then exit;
      //Power
      if (s[pos] <> 'e') and (s[pos] <> 'E') then exit;
      Result := Result + s[pos];
      inc(pos);
      if pos > length(s) then Error('Wrong number.');
      if s[pos] in ['-', '+'] then
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
      if (pos > length(s)) or not (s[pos] in ['0'..'9']) then Error('Wrong number.');
      while (pos <= length(s)) and
        (s[pos] in ['0'..'9']) do
      begin
        Result := Result + s[pos];
        inc(pos);
      end;
    except
    end;
  end;
  //Read lexem from string
  procedure getlex(s: string; var num: integer; var con: string);
  begin
    con := '';
    //skip spaces
    while (pos <= length(s)) and (s[pos] = ' ') do inc(pos);
    if pos > length(s) then
    begin 
      num := 0;  
      exit; 
    end;

    case s[pos] of
      '(': num := 1;
      ')': num := 2;
      '+': num := 3;
      '-': 
      begin
        num := 4;
        if (pos < length(s)) and (s[pos + 1] in ['1'..'9', '0']) and (curlex in [0,1]) then
        begin
          inc(pos);
          con := '-' + getnumber(s);
          dec(pos);
          num := 7;
        end;
      end;
      '*': num := 5;
      '/': num := 6;
      '^': num := 31;
      'a'..'z', 'A'..'Z', '_':
      begin
        while (pos <= length(s)) and
          (s[pos] in ['a'..'z', 'A'..'Z', '_', '1'..'9', '0']) do
        begin
          con := con + s[pos];
          inc(pos);
        end;
        dec(pos);
        num := 8;
        if con = 'cos' then num := 10;
        if con = 'sin' then num := 11;
        if con = 'tg' then num := 12;
        if con = 'ctg' then num := 13;
        if con = 'abs' then num := 14;
        if (con = 'sgn') or (con = 'sign') then num := 15;
        if con = 'sqrt' then num := 16;
        if con = 'ln' then num := 17;
        if con = 'exp' then num := 18;
        if con = 'arcsin' then num := 19;
        if con = 'arccos' then num := 20;
        if (con = 'arctg') or (con = 'arctan') then num := 21;
        if con = 'arcctg' then num := 22;
        if (con = 'sh') or (con = 'sinh') then num := 23;
        if (con = 'ch') or (con = 'cosh') then num := 24;
        if (con = 'th') or (con = 'tanh') then num := 25;
        if (con = 'cth') or (con = 'coth') then num := 26;
        if (con = 'heaviside') or (con = 'h') then num := 27;
        if num = 8 then  con := IntToStr(FVariables.IndexOf(con));
      end;
      '1'..'9', '0':
      begin
        con := getnumber(s);
        dec(pos);
        num := 7;
      end;
    end;
    inc(pos);
    PrevLex := CurLex;
    CurLex := num;
  end;

  //****************************************************************
var
  neg: boolean;
  l, r, res: TTree;
  n, op: integer;
  c: string;
  //****************************************************************

  function getsingleop: TTree;
  var
    op, bracket: integer;
    opc: string;
    l, r, res: TTree;
  begin
    l := nil;
    try
      if n = 1 then
      begin
        inc(bc);
        l := gettree(s);
      end
      else
      begin
        // First operand
        if not (n in [7,8,10..30]) then Error('');
        op := n;
        opc := c;
        if n in [7,8] then
        begin
          // Number or variable
          l := TTree.Create;
          l.num := op;
          l.con := opc;
        end
        else
        begin
          //Function
          getlex(s, n, c);
          if n <> 1 then Error('');
          inc(bc);
          l := gettree(s);
          l.num := op;
          l.con := opc;
        end;
      end;
      //Operation symbol
      getlex(s, n, c);
      //Power symbol
      while n = 31 do
        begin
          getlex(s, n, c);
        bracket := 0;
        if n = 1 then
        begin
          bracket := 1;
          getlex(s, n, c);
        end;
        if (n <> 7) and (n <> 8) then Error('');
        r := TTree.Create;
        r.num := n;
        r.con := c;
        res := TTree.Create;
        res.l := l;
        res.r := r;
        res.num := 31;
        l := res;
        if bracket = 1 then
        begin
          getlex(s, n, c);
          if n <> 2 then Error('');
        end;
        getlex(s, n, c);
      end;
      Result := l;
    except
      Result := nil;
    end;
  end;
  //****************************************************************
  function getop: TTree;
  var
    op: integer;
    l, r, res: TTree;
  begin
    neg := False;
    getlex(s, n, c);
    // Unary - or +
    if prevlex in [0,1] then
    begin
      if n = 4 then
      begin
        neg := True;
        getlex(s, n, c);
      end;
      if n = 3 then getlex(s, n, c);
    end;
    l := getsingleop;
    // 2nd operand **************
    while n in [5,6] do
    begin
      op := n;
      getlex(s, n, c);
      r := getsingleop;
      res := TTree.Create;
      res.l := l;
      res.r := r;
      res.num := op;
      l := res;
    end;
    // Unary minus
    if neg then
    begin
      res := TTree.Create;
      res.l := l;
      res.r := nil;
      res.num := 9;
      l := res;
    end;
    Result := l;
  end;

  //****************************************************************
begin
  Result := nil;
  try
    l := getop;
    while True do
    begin
      if n in [0,2] then
      begin
        if n = 2 then dec(bc);
        Result := l;
        exit;
      end;
      if not (n in [3,4]) then Error(n.ToString);
      op := n;
      r := getop;
      res := TTree.Create;
      res.l := l;
      res.r := r;
      res.num := op;
      l := res;
    end;
    Result := l;
  except
    Result := nil;
  end;
end;

//******************************************************************

function TCalcExpress.InternalCalc(t: TTREE): extended;
var
    r: extended;
begin
    Result := 0;
    if (t <> nil) then
    case t.num of
      3: Result := InternalCalc(t.l) + InternalCalc(t.r);
      4: Result := InternalCalc(t.l) - InternalCalc(t.r);
      5: Result := InternalCalc(t.l) * InternalCalc(t.r);
      6: Result := InternalCalc(t.l) / InternalCalc(t.r);
      7: Result := strtofloat(t.con);
      8: Result := FArgs[StrToInt(t.con)];
      9: Result := -InternalCalc(t.l);
      10: Result := cos(InternalCalc(t.l));
      11: Result := sin(InternalCalc(t.l));
      12: Result := tan(InternalCalc(t.l));
      13: Result := 1 / tan(InternalCalc(t.l));
      14: Result := abs(InternalCalc(t.l));
      15:
      begin
        r := InternalCalc(t.l);
        if r < 0 then Result := -1
        else if r > 0 then Result := 1
        else
          Result := 0;
      end;
      16: Result := sqrt(InternalCalc(t.l));
      17: Result := ln(InternalCalc(t.l));
      18: Result := exp(InternalCalc(t.l));
      19: Result := arcsin(InternalCalc(t.l));
      20: Result := arccos(InternalCalc(t.l));
      21: Result := arctan(InternalCalc(t.l));
      22: Result := pi / 2 - arctan(InternalCalc(t.l));
      23:
      begin
        r := InternalCalc(t.l);
        Result := (exp(r) - exp(-r)) / 2;
      end;
      24:
      begin
        r := InternalCalc(t.l);
        Result := (exp(r) + exp(-r)) / 2;
      end;
      25:
      begin
        r := InternalCalc(t.l);
        Result := (exp(r) - exp(-r)) / (exp(r) + exp(-r));
      end;
      26:
      begin
        r := InternalCalc(t.l);
        Result := (exp(r) + exp(-r)) / (exp(r) - exp(-r));
      end;
      27:
      begin
        r := InternalCalc(t.l);
        if r >= 0 then Result := 1
        else
          Result := 0;
      end;
      31: Result := power(InternalCalc(t.l), InternalCalc(t.r));
    end;

end; // InternalCalc


//*********************************************************************
constructor TCalcExpress.Create(o: TComponent);
begin
  inherited;
  Formula := '0';
  FDefaultNames := False;
  FVariables := TStringList.Create;
end;



//*********************************************************************
function TCalcExpress.calc(args: array of extended): extended;
var ExprTree: TTree;
begin
  FArgs := args;
  ExprTree := SetFormula(FFormula);
  if (ExprTree = nil) then
   begin
    Result := 0;
    ShowMessage('Error in formula.');
   end
  else
   calc := InternalCalc(ExprTree);
end;



//Tree deletion

//****************************************************************
procedure TCalcExpress.SetVariables(Value: TStrings);
begin
  FVariables.Clear;
  FVariables.Assign(Value);
end;



procedure TCalcExpress.Error(s: string);
begin
  Err := True;
  raise Exception.Create(s);
end;


constructor TTree.Create;
begin
  inherited;
  num := 0;
  l := nil;
  r := nil;
end;



procedure Register;
begin
  RegisterComponents('Samples', [TCalcExpress]);
end;

end.
