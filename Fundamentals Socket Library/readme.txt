FUNDAMENTALS 3 - SOCKETS
========================

1. Description

   FUNDAMENTALS is a collection of Delphi code libraries.

   FUNDAMENTALS SOCKETS contains:

   Sockets\
       cWinSock.pas           WinSock functions
       cSocks.pas             SOCKS4/5 functions
       cSocketHostLookup.pas  Socket host lookup class
       cSockets.pas           Base class for sockets
       cSocketsTCP.pas        Base class for TCP sockets
       cSocketsTCPClient.pas  Base class for TCP client sockets
       cSocketsTCPServer.pas  Base class for TCP server sockets
       cSocketsUDP.pas        Base class for UDP sockets
       cTCPStream.pas         TCP stream class
       cTCPClient.pas         TCP client component
       cTCPServer.pas         TCP server component

   FUNDAMENTALS SOCKETS require FUNDAMENTALS UTILITIES (included).

   FUNDAMENTALS UTILITIES contain:

   Utils\
       cUtils.pas           Utility functions for Integer, Bit, Set, 
                            Float, Memory and Dynamic Arrays
       cStrings.pas         Ansi String functions
       cDateTime.pas        Date and Time functions
       cRandom.pas          Uniform random number generators
   Unicode\
       cUnicodeCodecs.pas   Unicode encoders/decoders
       cUnicodeChar.pas     Unicode character functions
       cUnicode.pas         Unicode string functions
       cUnicodeReader.pas   Unicode reader classes
   DataStructs\
       cTypes.pas           Base class for data structures
       cArrays.pas          Array classes
       cDictionaries.pas    Dictionary classes
       cLinkedLists.pas     Linked lists classes
   Streams\
       cReaders.pas         Data readers
       cWriters.pas         Data writers
       cStreams.pas         Streams
   System\
       cRegistry.pas        Windows Registry access
       cWindows.pas         Windows API utilities
       cThreads.pas         Thread wrapper
       cFileUtils.pas       File utility functions
       cDynLib.pas          Windows Dynamic libary functions
       cLog.pas             Log class
   Internet\
       cInternetUtils.pas   Internet related utility functions


2. Installation

   1. Extract all files from the ZIP file to a directory (have the
      "Use Folder Names" option checked), for example to c:\Fundamentals3

   2. Open the and install the included package files:

         1. Fundamentals3_UtilitiesD#.dpk
  
         2. Fundamentals3_SocketsD#.dpk

      where # is your version of Delphi.

      If you have previously installed a version of Fundamentals with
      a different package name, you will have to remove the old packages
      before installing the new packages.



3. Contact information

   Home page:
   http://www.eternallines.com/fundamentals

   Sourceforge page:
   http://fundementals.sourceforge.net

   Forum:
   http://sourceforge.net/forum/forum.php?forum_id=2117

   E-mail:
   david@e.co.za


4. Copyright information

   These units are copyrighted by the author (c) 1995-2003.

   There are no restrictions on use of these libraries in your applications,
   including commercial applications.

   Included is the complete source code. In this same spirit I ask that you
   contribute to its further development.

   You are not allowed to distribute a modified version of the source files,
   nor misrepresent the origin of these libraries.

   Please send any changes, suggestions or bug reports to the author for
   inclusion in future releases.
