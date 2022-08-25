CAPICOM sample storesh
-----------------------
Date:  April 1st, 2002

This is a .NET/CAPICOM sample which demonstrates how to do certificate store management from C# using CAPICOM 2.0

1) To use CAPICOM from .NET, you need first to generate a Runtime Callable Wrapper (RCW). This is generated
with tlbimp.exe which is a tool that is included in the .NET framework SDK. In this sample, we call our RCW 
Interop.CAPICOM.dll, and to generate it we use the followingcommand:
	tlbimp capicom.dll /out:Interop.CAPICOM.dll

2) You can then build and run the sample using the Visual Studio Development enviroment

