// Do Not Edit!!!!!!!!!!!!!!

{$B-}  // Ensure that Complete Boolean Evaluation is off
{$J-}  // Constants from here are not assignable
{$T-}  // Ensure that the result of the @ operator is always an untyped Pointer


{$IFDEF VER120}
	{$DEFINE Delphi}
	{$DEFINE D4}
	{$DEFINE D4andAbove}
	{$DEFINE BelowD5}
	{$DEFINE BelowD6}
	{$DEFINE BelowD7}
	{$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF VER125}
	{$DEFINE BCB}
	{$DEFINE BCB4}
	{$DEFINE BCB4andAbove}
	{$DEFINE D4andAbove}
	{$DEFINE BelowD5}
	{$DEFINE BelowD6}
	{$DEFINE BelowD7}
	{$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF VER130}
	{$IFDEF BCB}
		{$DEFINE BCB5}
		{$DEFINE BCB5andAbove}
		{$DEFINE BCB4andAbove}
	{$ELSE}
		{$DEFINE Delphi}
		{$DEFINE D5}
	{$ENDIF}
	{$DEFINE D4andAbove}
	{$DEFINE D5andAbove}
	{$DEFINE BelowD6}
	{$DEFINE BelowD7}
	{$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF VER140}
	{$IFDEF BCB}
		{$DEFINE BCB6}
		{$DEFINE BCB6andAbove}
		{$DEFINE BCB5andAbove}
		{$DEFINE BCB4andAbove}
	{$ELSE}
		{$DEFINE Delphi}
		{$DEFINE D6}
	{$ENDIF}
	{$DEFINE D4andAbove}
	{$DEFINE D5andAbove}
	{$DEFINE D6andAbove}
	{$DEFINE BelowD7}
{$ENDIF}

{$IFDEF VER150}
	{$IFDEF BCB}
		{$DEFINE BCB7}
		{$DEFINE BCB7andAbove}
		{$DEFINE BCB6andAbove}
		{$DEFINE BCB5andAbove}
		{$DEFINE BCB4andAbove}
	{$ELSE}
		{$DEFINE Delphi}
		{$DEFINE D7}
	{$ENDIF}
	{$DEFINE D4andAbove}
	{$DEFINE D5andAbove}
	{$DEFINE D6andAbove}
	{$DEFINE D7andAbove}
{$ENDIF}

{$IFDEF LINUX}
	{$IFDEF CONDITIONALEXPRESSIONS}
		{$IFDEF CompilerVersion}
			{$IF RTLVersion = 14.1}
				{$DEFINE K2}
				{$DEFINE K1andAbove}
				{$DEFINE K2andAbove}
			{$IFEND}
			{$IF RTLVersion = 14.5}
				{$DEFINE K3}
				{$DEFINE K1andAbove}
				{$DEFINE K2andAbove}
			    {$DEFINE K3andAbove}
			{$IFEND}
		{$ELSE}
			{$DEFINE K1}
			{$DEFINE K1andAbove}
		{$ENDIF}
	{$ENDIF}
{$ENDIF}



