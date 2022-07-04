{$IFDEF VER120}      //delphi 4
  {$DEFINE D6M}
  {$DEFINE D6MFPC}
{$ENDIF}

{$IFDEF VER130}     // delphi 5
  {$DEFINE D6M}
  {$DEFINE D6MFPC}
{$ENDIF}

{$IFDEF VER140}     //delphi 6
  {$DEFINE D6P}
{$ENDIF}

{$IFDEF VER145}     //kylix
  {$DEFINE D6P}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE D6P}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE D6MFPC}
{$ENDIF} 

{$IFDEF LINUX}
  {$IFNDEF UNIX}
    {$DEFINE UNIX}
  {$ENDIF}
{$ENDIF}

//enamble this switch for interbase version < 6.0
//{$DEFINE FBL_IB5} 

//enable to switch to interbase 6 or firebird 1.0.x under linux with freepascal
//{$DEFINE LIBCRYPT}

//enable this switch with freepascal < 1.9.8
//{$DEFINE FPC_INFD}


//enable this switch for thread-safe feature for classes TFBLDatabase,TFBLTransaction,TFBDSql
//{$DEFINE FBL_THREADSAFE}
