{$IFDEF WIN32}
  {$DEFINE D2_OR_HIGHER}
{$ENDIF}

{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER93}
      {$IFNDEF VER100}
        {$IFNDEF VER110}
          {$IFNDEF VER120}
            {$IFNDEF VER125}
              {$IFNDEF VER130}
                {$DEFINE Unknown_Version}
              {$ELSE}
                {$DEFINE BD5}
                {$DEFINE DELPHI_COMPILER}
              {$ENDIF}
              {$DEFINE BCB4_OR_HIGHER}
            {$ELSE}
              {$DEFINE BCB4}
              {$DEFINE BCB_COMPILER}
            {$ENDIF}
            {$DEFINE D4_OR_HIGHER}
          {$ELSE}
            {$DEFINE BD4}
            {$DEFINE DELPHI_COMPILER}
          {$ENDIF}
          {$DEFINE BCB3_OR_HIGHER}
        {$ELSE}
          {$DEFINE BCB3}
          {$DEFINE BCB_COMPILER}
        {$ENDIF}
        {$DEFINE D3_OR_HIGHER}
      {$ELSE}
        {$DEFINE BD3}
        {$DEFINE DELPHI_COMPILER}
      {$ENDIF}
      {$DEFINE BCB1_OR_HIGHER}
    {$ELSE}
      {$DEFINE BCB1}
      {$DEFINE BCB_COMPILER}
    {$ENDIF}
    {$DEFINE D2_OR_HIGHER}
  {$ELSE}
    {$DEFINE BD2}
    {$DEFINE DELPHI_COMPILER}
  {$ENDIF}
  {$DEFINE D1_OR_HIGHER}
{$ELSE}
  {$DEFINE BD1}
  {$DEFINE DELPHI_COMPILER}
{$ENDIF}

