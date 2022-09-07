program Test_D5;

(*****************************************************************************
 * Copyright 2003 by Matthew Greet
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details. (http://opensource.org/licenses/lgpl-license.php)
 * 
 * See http://www.warmachine.u-net.com/delphi_collections for updates and downloads.
 *
 * $Version: v1.0.3 $
 * $Revision: 1.1 $
 * $Log: D:\QVCS Repositories\Delphi Collections\Test\Test_D5.eqs $
   
     Initial version.
   
   Revision 1.1  by: Matthew Greet  Rev date: 13/02/04 20:12:52
     Removed references to all packages except standard VCL components and
     collections.
   
   Revision 1.0  by: Matthew Greet  Rev date: 06/04/03 11:22:26
     Initial revision.
   
   $Endlog$
 *****************************************************************************)

uses
  Forms,
  TestUI in 'TestUI.pas' {TestForm},
  FunctionalTest in 'FunctionalTest.pas',
  ComparisonUI in 'ComparisonUI.pas' {ComparisonTestForm},
  PerformanceTest in 'PerformanceTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestForm, TestForm);
  Application.CreateForm(TComparisonTestForm, ComparisonTestForm);
  Application.Run;
end.
