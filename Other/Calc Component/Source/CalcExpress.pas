//==============================================================================
// Product name: CalcExpress
// Copyright 2000-2002 AidAim Software.
// Description:
//  CalcExpress is an interpreter for quick and easy
//  evaluation of mathematical expressions.
//  It is a smart tool easy in use.
//  Supports 5 operators, parenthesis, 18 mathematical functions and
//  user-defined variables.
// Date: 06/14/2001
//==============================================================================
unit CalcExpress;

interface

{DEFINE aaCLX} // set $ after { to get CLX version

uses
  SysUtils, Classes, Math,
{$IFDEF aaCLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls;
{$ELSE}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls;
{$ENDIF}

type

  TTree = record
    num: integer;
    con: string;
    l, r: pointer;
  end;

  PTree = ^TTree;

  TCalcExpress = class(TComponent)
  private
    Err: boolean;
    Bc: integer;
    PrevLex, Curlex: integer;
    Pos: integer;
    FFormula: string;
    Tree: pointer;
    FVariables: TStrings;
    FDefaultNames: boolean;
    procedure init(s: string);
    function gettree(s: string): pointer;
    function deltree(t: PTree): pointer;
    procedure Error(s: string);
    procedure SetVariables(Value: TStrings);
  public
    constructor Create(o: TComponent); override;
    destructor Destroy; override;
    function calc(args: array of extended): extended;
  published
    property Formula: string read FFormula write init;
    property Variables: TStrings read FVariables write SetVariables;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TCalcExpress]);
end;

//*********************************************************************
function TCalcExpress.calc(args: array of extended): extended;
  function c(t: PTREE): extended;
  var 
    r: extended;
  begin
    c := 0;
    case t^.num of
      3: c := c(t^.l) + c(t^.r);
      4: c := c(t^.l) - c(t^.r);
      5: c := c(t^.l) * c(t^.r);
      6: c := c(t^.l) / c(t^.r);
      7: c := strtofloat(t^.con);
      8: c := args[StrToInt(t^.con)];
      9: c := -c(t^.l);
      10: c := cos(c(t^.l));
      11: c := sin(c(t^.l));
      12: c := tan(c(t^.l));
      13: c := 1 / tan(c(t^.l));
      14: c := abs(c(t^.l));
      15: 
      begin
        r := c(t^.l);
        if r < 0 then c := -1 
        else if r > 0 then c := 1 
        else 
          c := 0;
      end;
      16: c := sqrt(c(t^.l));
      17: c := ln(c(t^.l));
      18: c := exp(c(t^.l));
      19: c := arcsin(c(t^.l));
      20: c := arccos(c(t^.l));
      21: c := arctan(c(t^.l));
      22: c := pi / 2 - arctan(c(t^.l));
      23: 
      begin
        r := c(t^.l);
        c := (exp(r) - exp(-r)) / 2;
      end;
      24: 
      begin
        r := c(t^.l);
        c := (exp(r) + exp(-r)) / 2;
      end;
      25: 
      begin
        r := c(t^.l);
        c := (exp(r) - exp(-r)) / (exp(r) + exp(-r));
      end;
      26: 
      begin
        r := c(t^.l);
        c := (exp(r) + exp(-r)) / (exp(r) - exp(-r));
      end;
      27: 
      begin
        r := c(t^.l);
        if r >= 0 then c := 1 
        else 
          c := 0;
      end;
      31: c := power(c(t^.l), c(t^.r));
    end;
  end;
begin
  calc := c(tree);
end;

procedure TCalcExpress.Error(s: string);
begin
  Err := True;
  raise Exception.Create(s);
end;
//*********************************************************************
constructor TCalcExpress.Create(o: TComponent);
begin
  inherited;
  Tree := nil;
  Formula := '0';
  FDefaultNames := False;
  FVariables := TStringList.Create;
end;
//*********************************************************************
destructor TCalcExpress.Destroy;
begin
  DelTree(Tree);
  FVariables.Free;
  inherited;
end;

//***************************************************************

function TCalcExpress.GetTree(s: string): pointer;
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
  l, r, res: PTree;
  n, op: integer;
  c: string;
  //****************************************************************
  function newnode: PTree;
  begin
    Result := allocmem(sizeof(TTree));
    Result^.l := nil;
    Result^.r := nil;
  end;

  function getsingleop: pointer;
  var 
    op, bracket: integer;
    opc: string;
    l, r, res: PTree;
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
          l := newnode; 
          l^.num := op; 
          l^.con := opc;
        end 
        else
        begin
          //Function
          getlex(s, n, c);
          if n <> 1 then Error('');
          inc(bc);
          l := newnode;
          l^.l := gettree(s); 
          l^.num := op; 
          l^.con := opc;
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
        r := newnode; 
        r^.num := n; 
        r^.con := c;
        res := newnode; 
        res^.l := l; 
        res^.r := r; 
        res^.num := 31; 
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
      DelTree(l);
      Result := nil;
    end;
  end;
  //****************************************************************
  function getop: pointer;
  var 
    op: integer;
    l, r, res: PTree;
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
      res := allocmem(sizeof(TTree));
      res^.l := l; 
      res^.r := r; 
      res^.num := op;
      l := res;
    end;
    // Unary minus
    if neg then
    begin
      res := allocmem(sizeof(TTree));
      res^.l := l; 
      res^.r := nil; 
      res^.num := 9;
      l := res;
    end;
    Result := l;
  end;

  //****************************************************************
begin
  l := nil;
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
      if not (n in [3,4]) then Error('');
      op := n;
      r := getop;
      res := allocmem(sizeof(TTree));
      res^.l := l; 
      res^.r := r; 
      res^.num := op;
      l := res;
    end;
    Result := l;
  except
    DelTree(l);
    Result := nil;
  end;
end;

//******************************************************************

procedure TCalcExpress.init(s: string);
begin
  deltree(tree);
  Err := False;
  FFormula := LowerCase(s);
  Prevlex := 0;  
  Curlex := 0;  
  Pos := 1;  
  bc := 0;
  Tree := GetTree(Lowercase(s));
  if (bc <> 0) or Err then
  begin
    ShowMessage('Error in formula.');
    Tree := DelTree(Tree);
  end;
end;

//Tree deletion

function TCalcExpress.deltree(t: PTree): pointer;
begin
  Result := nil;
  if t = nil then exit;
  if t^.l <> nil then Deltree(t^.l);
  if t^.r <> nil then Deltree(t^.r);
  freemem(t);
end;

//****************************************************************
procedure TCalcExpress.SetVariables(Value: TStrings);
begin
  FVariables.Clear;
  FVariables.Assign(Value);
  Init(Formula);
end;


end.