{$IFDEF VER200}
  {$DEFINE DELPHI_2009}
{$ENDIF}

{$IFDEF VER185}
   {$DEFINE DELPHI_2007_BELOW}
  {$else}
    {$IFDEF VER180}
      {$DEFINE DELPHI_2007_BELOW}
     {$else}
      {$IFDEF VER170}
        {$DEFINE DELPHI_2007_BELOW}
       {$else}
         {$IFDEF VER150}
           {$DEFINE DELPHI_2007_BELOW}
         {$ENDIF}
      {$ENDIF}
    {$ENDIF}
{$ENDIF}

{$ifdef VER140}
    {$define DELPHI_6_BELOW}
 {$else}
    {$ifdef VER130}
      {$define DELPHI_6_BELOW}
    {$else}
      {$ifdef VER120}
         {$define DELPHI_6_BELOW}
      {$else}
         {$ifdef VER110}
            {$define DELPHI_6_BELOW}
         {$else}
            {$ifdef VER100}
               {$define DELPHI_6_BELOW}
            {$else}
               {$ifdef VER90}
                  {$define DELPHI_6_BELOW}
               {$else}
                  {$ifdef VER80}
                     {$define DELPHI_6_BELOW}
                  {$endif}
               {$endif}
            {$endif}
         {$endif}
      {$endif}
    {$endif}
 {$endif}

 {$IFDEF DELPHI_6_BELOW}
   {$DEFINE DELPHI_2007_BELOW}
 {$ENDIF}
