{
  rr.inc:	Include file, defines conditional compilation symbols
		for Borland Delphi / C++Builder RAD tools

  Author:       Robert Roﬂmair, Robert.Rossmair@t-online.de

  Version:      1.3, 28-Feb-2000

  Predefinded compiler version symbols are:
  (VERxyz means compiler version xy.z)

  VER80		Delphi 1.0
  VER90		Delphi 2.0
  VER93		C++Builder 1.0
  VER100	Delphi 3.0
  VER110	C++Builder 3.0
  VER120	Delphi 4.0
  VER125	C++Builder 4.0
  VER130	Delphi 5.0
  VER135	C++Builder 5.0
  VER140	Delphi 6.0
  VER150	Delphi 7.0
  VER160	Delphi 7.0
  VER170	Delphi 2005
}

{$B-} { Complete Boolean Evaluation }
{$R-} { Range-Checking }
{$V-} { Var-String Checking }
{$T-} { Typed @ operator }
{$X+} { Extended syntax }
{$P+} { Open string params }
{$IFDEF WIN32}
{$J+} { Writeable structured consts }
{$H+} { Use long strings by default }
{$ENDIF}

{$DEFINE VER90_up}
{$DEFINE VER93_up}
{$DEFINE VER100_up}
{$DEFINE VER110_up}
{$DEFINE VER120_up}
{$DEFINE VER125_up}
{$DEFINE VER130_up}
{$DEFINE VER135_up}
{$DEFINE VER140_up}
{$DEFINE VER150_up}
{$DEFINE VER160_up}
{$DEFINE VER170_up}

{$IFNDEF VER170}
  {$UNDEF VER170_up}
  {$IFNDEF VER160}
    {$UNDEF VER160_up}
    {$IFNDEF VER150}
      {$UNDEF VER150_up}
      {$IFNDEF VER140}
        {$UNDEF VER140_up}
        {$IFNDEF VER135}
          {$UNDEF VER135_up}
          {$IFNDEF VER130}
            {$UNDEF VER130_up}
            {$IFNDEF VER125}
              {$UNDEF VER125_up}
              {$IFNDEF VER120}
                {$UNDEF VER120_up}
                {$IFNDEF VER110}
                  {$UNDEF VER110_up}
                  {$IFNDEF VER100}
                    {$UNDEF VER100_up}
                    {$IFNDEF VER93}
                      {$UNDEF VER93_up}
                      {$IFNDEF VER90}
                        {$UNDEF VER90_up}
                      {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER93}  { Borland C++Builder 1.0 }
  {$DEFINE CBUILDER}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE CBUILDER}
{$ENDIF}

{$IFDEF VER125} { Borland C++Builder 4.0 }
  {$DEFINE CBUILDER}
{$ENDIF}

{$IFDEF VER135} { Borland C++Builder 5.0 }
  {$DEFINE CBUILDER}
{$ENDIF}


{$IFDEF CBUILDER}
  {$IFNDEF VER93}
  {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER100_up}		{ Delphi 3 and above }
  {$DEFINE RR_PackageSupport}
  {$DEFINE RR_Interfaces}
  {$DEFINE RR_Resourcestring}
  {$DEFINE RR_EnhancedTBitmap}
  {$DEFINE RR_JPEG}
  {$IFDEF VER110_up}		{ C++Builder 3 and above }
    {$DEFINE RR_DefaultParams}
    {$IFDEF VER120_up}		{ Delphi 4 and above }
      {$DEFINE RR_Overloading}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$DEFINE RR_PicEditBroken}    { PicEdit unit broken (missing LibConst) }

{$IFNDEF VER100 (not Delphi 3) }
  {$IFNDEF VER110 (not C++Builder 3) }
     {$IFNDEF VER120 (not Delphi 4) }
     {$UNDEF RR_PicEditBroken}
     {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER100}
  { Graphics.CopyBitmap bug: invalid parameters are passed to
    CreateDIBSection when HandleType = bmDIB and the value of the
    boolean expression (Width*Height=0) changes.
    This will cause an EOutOfResources exception. }
  {$DEFINE RR_CopyBitmapBug}
{$ENDIF}
