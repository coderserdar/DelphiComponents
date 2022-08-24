{------------------------------------------------------------------------------
  DecoChartsPlusReg.pas

  DecoCharts Plus for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Registration of components

  Notes:      Requires a dynamic loading implementation of GDI+ API library
              ( http://www.progdigy.com, http://themiron.mirandaim.ru )

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2013  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.2 (2012-03-11)
  - added: TDecoCompareGridPlus, TDecoProgressBarPlus, TDecoRatingStarsPlus components

  Version 1.0 (2011-07-17)
  - The first release
}

{ Unit that performs the registration of DecoCharts Plus components. }
unit DecoChartsPlusReg;

interface

uses
  Classes,
  DecoBarPlus,
  DecoProgressGridPlus,
  DecoCompareGridPlus,
  DecoProgressBarPlus,
  DecoRatingStarsPlus;

{ Registration of components and their design-time editing options. }
procedure Register;

implementation

procedure Register;
const
  pal = 'DecoCharts Plus';
begin
  RegisterComponents(pal, [TDecoBarPlus, TDecoProgressGridPlus, TDecoCompareGridPlus,
                           TDecoProgressBarPlus, TDecoRatingStarsPlus]);
end;

end.

