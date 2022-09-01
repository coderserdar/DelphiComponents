unit calc_demo_const;

interface
{$I psc_defines.inc}

const

  SPSCActionsColor = 'Actions';
  SPSCCalcColor = 'Color';
  SPSCDisplayBackColor = 'Display Back';
  SPSCDisplayBorderColor = 'Display Border';
  SPSCNumbersColor = 'Numbers';

  CPSCCalculatorColors : array[1..5] of string =
  (
    SPSCActionsColor,
    SPSCCalcColor,
    SPSCDisplayBackColor,
    SPSCDisplayBorderColor,
    SPSCNumbersColor
  );
implementation

end.
