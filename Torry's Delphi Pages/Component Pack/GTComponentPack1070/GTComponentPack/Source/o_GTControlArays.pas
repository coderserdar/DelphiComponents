unit o_GTControlArays;

interface
uses
   Classes
  ,o_GTControlArray
  ;
type
{------------------------------------------------------------------------------}
  TgtButtonArray = class(TgtControlArray)
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
  end;
{------------------------------------------------------------------------------}




implementation
uses
  StdCtrls
  ;

{ TgtButtonArray }
{------------------------------------------------------------------------------}
constructor TgtButtonArray.Create(AOwner: TComponent);
begin
  inherited Create(Self);
  ControlClass := TButton;
end;
{------------------------------------------------------------------------------}

end.
