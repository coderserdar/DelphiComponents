*****************************************************************************
******* Barcode library for Delphi, C++ Builder and Kylix.           ********
*****************************************************************************


Version : 4.4, created july 19, 2002


Library for design, print, export barcodes.

-----------------------------------------------------------------------------------
Supported platforms: Delphi 3-6 


News :
- added support for PDF417 full & truncated
- supported PDF417 compaction : binary, numeric, aplhabetic 
- support PDF417 error corrections levels 0-8, auto correction - based on number of codewords
- updated barcode component editor
- added property editor for property PDF417
- updated TEan popup menu (right click on barcode component)
- removed all memo based barcode components (TEanMemo, TDBEanMemo ...)
- packages & units for ReportBuilder and AceReporter moved to same directory as basic component
- added demo for PDF417 - see directory DEMOS\PDF417


Knows bugs :
- some problem with property PDF417 & ReportBuilder&AceReporter. This property is not stored in dfm file. 
	You must set this property in code (see demo from DEMOS\PDF417) . Sorry, we work on this.
- problem with AceReporter - if paint rectangle is smaller than min.size, barcode is unreadable. We work on correction.



Supported barcode types :
- Ean 8, Ean 13, UPC, JAN, ISBN, ISSN, ISMN ( with 2 or 5 additional digits), PDF417 full & truncated
- Code 2 of 5 Standard, Matrix, Interleaved, Invert, IATA, Coop
- Code 39
- Code 93
- Code 11
- Codabar, ABC Codabar
- Code 128, Ean 128
- OPC, ITF, MSI Plessey
- PostNet
- PZN


Supported Report tools :

- direct print without any report tool - methods for direct print to canvas
- QuickReport
- Report Builder
- ACE Reporter


Help file : Sorry, now not available. Under construction for new version.


-----------------------------------------------------------------------------------

Limitations  : trial version work only if IDE is running. On computer without IDE 
	barcodes are printed with one crossover line.

Installation :

For all versions of Delphi, Kylix and C++ Builder you must install one packaeg with 
basic components, and optionaly one package with property and component editors.
Barcode palette page name : Barcode components.
-----------------------------------------------------------------------------------
Delphi 3 : open units/d3/ean3.dpk and pres button Install. 
	Component editors package : units/d3/ean3Editors.dpk

Delphi 4 : open units/d4/ean4.dpk and pres button Install. 
	Component editors package : units/d4/ean4Editors.dpk

Delphi 5 : open units/d3/ean3.dpk and pres button Install. 
	Component editors package : units/d5/ean5Editors.dpk

Delphi 6 : open units/d3/ean3.dpk and pres button Install. 
	Component editors package : units/d6/ean6Editors.dpk


C++ Builder 5 : open units/bcb5/eanbcb5.dpk and pres button Install. 
	Component editors package : units/bcb5/eanbcb5editors.dpk

C++ Builder 6 : open units/bcb6/eanbcb6.dpk and pres button Install. 
	Component editors package : units/bcb6/eanbcb6editors.dpk

Kylix 1 : open units/kylix1/eankylix1.dpk and pres button Install. 
	Component editors package : units/kylix1/eankylix1editors.dpk

Kylix 2 : open units/kylix2/eankylix2.dpk and pres button Install. 
	Component editors package : units/kylix2/eankylix2editors.dpk


If you can install barcode components for ACE Reporter, 
please install package EanAceReport.dpk.
For ACE Reporter under Delphi 6 use EanAceReport6.dpk

If you can install barcode components for Report Builder, please install package EanReportBuilder.dpk






-----------------------------------------------------------------------------------

For news or registration please visit http://barcode.psoft.sk or main page 
http://www.psoft.sk

If you have some troubles or questions, please send email to peter@psoft.sk

-----------------------------------------------------------------------------------

july 19, 2002

Peter Cirip
PSOFT








 