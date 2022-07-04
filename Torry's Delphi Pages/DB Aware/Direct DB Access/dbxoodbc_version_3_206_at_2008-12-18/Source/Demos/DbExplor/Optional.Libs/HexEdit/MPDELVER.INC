{$IFDEF VER80}
	ERROR Delphi 1 not supported!
{$ENDIF}
{$IFDEF VER90}
        {$DEFINE DELPHI}
	{$DEFINE DELPHI2}
        {$DEFINE DELPHI2UP}
{$ENDIF}
{$IFDEF VER93}
        {$DEFINE BCB}
	{$DEFINE BCB1}
        {$DEFINE DELPHI2UP}
{$ENDIF}
{$IFDEF VER100}
        {$DEFINE DELPHI}
	{$DEFINE DELPHI3}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
{$ENDIF}
{$IFDEF VER110}
        {$DEFINE BCB}
	{$DEFINE BCB3}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
{$ENDIF}
{$IFDEF VER120}
        {$DEFINE DELPHI}
	{$DEFINE DELPHI4}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
        {$DEFINE DELPHI4UP}
{$ENDIF}
{$IFDEF VER125}
        {$DEFINE BCB}
	{$DEFINE BCB4}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
        {$DEFINE DELPHI4UP}
{$ENDIF}
{$IFDEF VER130}
        {$IFNDEF BCB}
                 {$DEFINE DELPHI}
	         {$DEFINE DELPHI5}
                 {$DEFINE DELPHI2UP}
                 {$DEFINE DELPHI3UP}
                 {$DEFINE DELPHI4UP}
                 {$DEFINE DELPHI5UP}
        {$ELSE}
               {$DEFINE BCB}
	       {$DEFINE BCB5}
               {$DEFINE DELPHI2UP}
               {$DEFINE DELPHI3UP}
               {$DEFINE DELPHI4UP}
               {$DEFINE DELPHI5UP}
        {$ENDIF}
{$ENDIF}
{$IFDEF VER140}
        {$IFNDEF BCB}
                 {$DEFINE DELPHI}
	         {$DEFINE DELPHI6}
                 {$DEFINE DELPHI2UP}
                 {$DEFINE DELPHI3UP}
                 {$DEFINE DELPHI4UP}
                 {$DEFINE DELPHI5UP}
                 {$DEFINE DELPHI6UP}
        {$ELSE}
               {$DEFINE BCB}
	       {$DEFINE BCB6}
               {$DEFINE DELPHI2UP}
               {$DEFINE DELPHI3UP}
               {$DEFINE DELPHI4UP}
               {$DEFINE DELPHI5UP}
               {$DEFINE DELPHI6UP}
        {$ENDIF}
{$ENDIF}
{$IFDEF VER150}
        {$IFNDEF BCB}
             {$DEFINE DELPHI}
             {$DEFINE DELPHI7}
             {$DEFINE DELPHI2UP}
             {$DEFINE DELPHI3UP}
             {$DEFINE DELPHI4UP}
             {$DEFINE DELPHI5UP}
             {$DEFINE DELPHI6UP}
             {$DEFINE DELPHI7UP}
        {$ELSE}
             {$DEFINE BCB}
             {$DEFINE BCB7}
             {$DEFINE DELPHI2UP}
             {$DEFINE DELPHI3UP}
             {$DEFINE DELPHI4UP}
             {$DEFINE DELPHI5UP}
             {$DEFINE DELPHI6UP}
             {$DEFINE DELPHI7UP}
        {$ENDIF}
{$ENDIF}
{$IFDEF VER160}
        {$DEFINE DELPHI}
        {$DEFINE DELPHI8}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
        {$DEFINE DELPHI4UP}
        {$DEFINE DELPHI5UP}
        {$DEFINE DELPHI6UP}
        {$DEFINE DELPHI7UP}
        {$DEFINE DELPHI8UP}
{$ENDIF}
{$IFDEF VER170}
        {$DEFINE DELPHI}
        {$DEFINE DELPHI2005}
        {$DEFINE DELPHI2UP}
        {$DEFINE DELPHI3UP}
        {$DEFINE DELPHI4UP}
        {$DEFINE DELPHI5UP}
        {$DEFINE DELPHI6UP}
        {$DEFINE DELPHI7UP}
        {$DEFINE DELPHI8UP}
        {$DEFINE DELPHI2005UP}
{$ENDIF}
{$IFDEF DELPHI4UP}
        {$DEFINE DYNARRAY}
{$ENDIF}
