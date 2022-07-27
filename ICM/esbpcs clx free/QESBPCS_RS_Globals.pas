{: Contains the Global Resource Strings for ESBPCS for CLX.

 International English Edition.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This can unit can be replaced with alternate strings and then
 ESBPCS recompiled.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCS_RS_Globals;

{$I esbpcs.inc}

interface

// For Globals
resourcestring
     rsEmpty = '<empty>';
     rsTAB = '<TAB>';
     rsColor = 'colour'; // v2.1
     rsColors = 'colours'; // v2.1

     // For Booleans and Buttons
resourcestring
     rsYes = 'Yes';
     rsNo = 'No';
     rsTrue = 'True';
     rsFalse = 'False';
     rsOn = 'On';
     rsOff = 'Off';
     rsOK = 'OK'; // v2.1
     rsCancel = 'Cancel'; // v2.1
     rsAbort = 'Abort'; // v2.1

     // Error Messages
resourcestring
     rsNA = 'N/A';
     rsError = 'Error!';
     rsUnaccept = 'Your entered Value is not acceptable!';

     // Messages
resourcestring
     rsField = 'Field';
     rsReason = 'Reason';

     // Date/Time Related Messages
resourcestring
     rsInvalidDate = 'Invalid Date';
     rsInvalidTime = 'Invalid Time';
     rsInvalidDateTime = 'Invalid Date/Time';
     rsInvalidDOW = 'Invalid Day of the Week'; //v2.3
     rsInvalidMonth = 'Invalid Month'; //v2.3
     rsInvalidWeek = 'Invalid Week'; //v2.3

     // Astronomical Related Messages
resourcestring
     rsUT = 'UT'; // Abbreviation for Universal Time used in Astronomy
     rsGST = 'GST'; // Abbreviation for Greenwich Sidereal Time in Astronomy
     rsLST = 'LST'; // Abbreviation for Local Sidereal Time in Astronomy
     rsUTStr = 'Universal Time';
     rsGSTStr = 'Greenwich Sidereal Time';
     rsLSTStr = 'Local Sidereal Time';

     // Astrological Signs
resourcestring
     rsAquarius = 'Aquarius';
     rsPisces = 'Pisces';
     rsAries = 'Aries';
     rsTaurus = 'Taurus';
     rsGemini = 'Gemini';
     rsCancer = 'Cancer';
     rsLeo = 'Leo';
     rsVirgo = 'Virgo';
     rsLibra = 'Libra';
     rsScorpio = 'Scorpio';
     rsSagittarius = 'Sagittarius';
     rsCapricorn = 'Capricorn';

     // Planets
resourcestring
     rsSun = 'Sun'; // our Sun
     rsMoon = 'Moon'; // our Moon
     rsMercury = 'Mercury';
     rsVenus = 'Venus';
     rsEarth = 'Earth';
     rsMars = 'Mars';
     rsJupiter = 'Jupiter';
     rsSaturn = 'Saturn';
     rsUranus = 'Uranus';
     rsNeptune = 'Neptune';
     rsPluto = 'Pluto';

     rsStar = 'star'; // as in star in the sky
     rsStars = 'stars';
     rsPlanet = 'planet';
     rsPlanets = 'planets';

     // Format Messages
resourcestring
     rsCustomFormat = 'Format can only be changed for FormatType = edfCustom';

implementation

end.
