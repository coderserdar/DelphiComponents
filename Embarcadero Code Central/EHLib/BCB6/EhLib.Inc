{*******************************************************}
{                                                       }
{                       EhLib v3.0                      }
{     Conditional defines that affect compilation       }
{                                                       }
{   Copyright (c) 1998-2003 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

{$DEFINE EH_LIB_VCL}

{$IFDEF VER100} { Borland Delphi 3.0 }
  {$MESSAGE ERROR 'This version of EhLib does not support Delphi 3' }
{$ENDIF}

{$IFDEF VER110}         { C++Builder 3.0 }
  {$MESSAGE ERROR 'This version of EhLib does not support C++Builder 3' }
{$ENDIF}

{$IFDEF VER120}         { Borland Delphi 4.0 }
//  {$DEFINE EH_LIB_35}
//  {$DEFINE EH_LIB_4}
{$ENDIF}

{$IFDEF VER125}         { CPP Builder 4.0 }
//  {$DEFINE EH_LIB_35}
//  {$DEFINE EH_LIB_4}
{$ENDIF}

{$IFDEF VER130}         { Borland Delphi 5.0 or CPP Builder 5.0 }
//  {$DEFINE EH_LIB_35}
//  {$DEFINE EH_LIB_4}
  {$DEFINE EH_LIB_5}
{$ENDIF}

{$IFDEF VER140}         { Borland Delphi 6.0 or CPP Builder 6.0}
//  {$DEFINE EH_LIB_35}
//  {$DEFINE EH_LIB_4}
  {$DEFINE EH_LIB_5}
  {$DEFINE EH_LIB_6}
{$ENDIF}

{$IFDEF VER150}         { Borland Delphi 7.0 }
//  {$DEFINE EH_LIB_35}
//  {$DEFINE EH_LIB_4}
  {$DEFINE EH_LIB_5}
  {$DEFINE EH_LIB_6}
  {$DEFINE EH_LIB_7}
{$ENDIF}

{$IFDEF CIL}
  {$DEFINE EH_LIB_5}
  {$DEFINE EH_LIB_6}
  {$DEFINE EH_LIB_7}
  {$DEFINE EH_LIB_8}
{$ENDIF}

{$define eval}
