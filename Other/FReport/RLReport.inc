{$ifdef DELPHI5}
  {$define DELPHI}
  {$define VCL}
{$endif}

{$ifdef DELPHI6}
  {$define DELPHI}
{$endif}

{$ifdef DELPHI7}
  {$define DELPHI}
  {$define DELPHI6}
{$endif}

{$ifdef LINUX}
  {$define KYLIX}
{$endif}

{$ifdef KYLIX}
  {$define CLX}
{$else}
  {$define WINDOWS}
{$endif}

