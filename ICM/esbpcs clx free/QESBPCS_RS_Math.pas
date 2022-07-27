{: Contains the Math Related Resource Strings for ESBPCS for CLX.

 International English Edition.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 This can unit can be replaced with alternate strings and then
 ESBPCS recompiled.p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCS_RS_Math;

{$I esbpcs.inc}

interface

// Random
resourcestring
     rsInvalidShape = 'Invalid Shape';
     rsShapeTooSmall = 'Shape too small';
     rsVectorEmpty = 'Vector Empty';
     rsVectorTooSmall = 'Vector too small';

     // Error Messages
resourcestring
     rsCannot = 'Cannot have more than ';
     rsDecimalPlaces = ' Decimal Places';
     rsLowerBound = 'Your entered Value is below the acceptable limit of ';
     rsUpperBound = 'Your entered Value is above the acceptable limit of ';
     rsDivideByZero = 'Division by Zero';
     rsSqRootNeg = 'Square Root of Negative';
     rsCDivideByZero = 'Complex Division by Zero';
     rsAngleTooLarge = 'Angle Magnitude too large';
     rsInvalidValue = 'Invalid Value';
     rsValueLEOne = 'Value must be <= 1';
     rsValueGZero = 'Value must be > 0';
     rsValueGEZero = 'Value must be >= 0';
     rsValueLZero = 'Value must be < 0';
     rsValueLEZero = 'Value must be <= 0';
     rsValueNotZero = 'Value must not be 0';
     rsValue01 = 'Value must be strictly between 0 and 1';
     rsValue01Inc = 'Value must be between 0 and 1 inclusive';
     rsValue2 = 'Value must be strictly between 0.000002 and 0.999998';
     rsPowerInt64 = 'Power would cause Int64 overflow';
     rsZeroToNegPower = 'Cannot Raise 0 to a negative power';
     rsInvalidXtoY = 'Invalid XtoY';
     rsFactOverflow = 'Factorial Calculation cannot exceed 1754';
     rsPermInvalid = 'Invalid Permutation';
     rsPermOverflowed = 'Permutation Overflowed';
     rsBinomInvalid = 'Invalid Binomial Coefficient';
     rsBinomOverflowed = 'Binomial Coefficient Overflowed';
     rsBinomDistInvalid = 'Invalid Binomial Distribution';
     rsPoissonInvalid = 'Invalid Poisson Distribution';
     rsExpDistInvalid = 'Invalid Exponential Distribution';
     rsStdDev = 'Standard Deviation must be > 0';
     rsDF = 'Degrees of Freedom must be > 0';
     rsMedianNotFound = 'Median Not Found';
     rsQuartilesNotFound = 'Quartiles Not Found';
     rsNotDefinedForOne = 'Not defined for 1';
     rsNotDefinedForValue = 'Not defined for that value';
     rsIPAddrComp = 'IP Address Components must be between 0 and 255';
     rsAtLeastOne = 'Must have at Least 1 Value';
     rsAtLeastTwo = 'Must have at Least 2 Values';
     rsAtLeastThree = 'Must have at Least 3 Values';
     rsAtLeastFour = 'Must have at Least 4 Values';

     // Fractions
resourcestring
     rsDenomNotZero = 'Denominator cannot be 0';

     // Geometry
resourcestring
     rsLengthNotNeg = 'Length must not be Negative';
     rsInvalidSides = 'Invalid Sides';
     rsAngleNotLEPi = 'Angle must be less than or equal to Pi';

     // Matrices & Vectors
resourcestring
     rsMatrixIsEmpty = 'Matrix is Empty';
     rsVectorIsEmpty = 'Vector is Empty';
     rsSameDimensions = 'Must have the Same Dimensions';
     rsMatrixNeg = 'Matrix contains Negatives';
     rsVectorNeg = 'Vector contains Negatives';
     rsMatrixZero = 'Matrix contains Zeroes';
     rsVectorZero = 'Vector contains Zeroes';
     rsMatrixNotPos = 'Matrix contains values <= 0';
     rsVectorNotPos = 'Vector contains values <= 0';
     rsMatrixRect = 'Rectangular Matrix Required';
     rsMatrixSqr = 'Square Matrix Required';
     rsMatrixMult = 'Number of Columns in X does not equal'
          + #13 + 'the Number of Rows in Y';
     rsInvalidRow = 'Invalid Row Number';
     rsInvalidCol = 'Invalid Column Number';
     rsNoInvert = 'Unable to Invert the Matrix';
     rsVectorG1 = 'Vector must have more than 1 Element';
     rsVectorG2 = 'Vector must have more than 2 Elements';
     rsVectorG3 = 'Vector must have more than 3 Elements';
     rsMagZero = 'Cannot find Angle when Magnitude is Zero';

     // Positions
resourcestring
     rsLatitude = 'Latitude';
     rsLongitude = 'Longitude';
     rsInvalidLatitude = 'Invalid Latitude';
     rsInvalidLongitude = 'Invalid Longitude';
     rsNorth = 'North';
     rsSouth = 'South';
     rsEast = 'East';
     rsWest = 'West';
     rsNorthAbbr = 'N';
     rsSouthAbbr = 'S';
     rsEastAbbr = 'E';
     rsWestAbbr = 'W';

resourcestring
     rsInvalidAngle = 'Invalid Angle';

implementation

end.
