{.$D-,L-} {- Debug information }
{$D+,L+} {- Debug information }

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }

{$ifdef VER100}
  {$define Delphi3}
  {$define D3Above}
{$endif}

{$ifdef VER120}
  {$define Delphi4}
  {$define D3Above}
  {$define D4Above}
{$endif}

{$ifdef VER130}
  {$define Delphi5}
  {$define D3Above}
  {$define D4Above}
  {$define D5Above}
{$endif}

{$ifdef VER140}
  {$define Delphi6}
  {$define D3Above}
  {$define D4Above}
  {$define D5Above}
  {$define D6Above}
{$endif}

{$ifdef VER150}
  {$define Delphi7}
  {$define D3Above}
  {$define D4Above}
  {$define D5Above}
  {$define D6Above}
  {$define D7Above}
{$endif}

