unit bcchksum;
{
Checksum Functions by Andreas Schmidt

21.07.1999


mailto://shmia@bizerba.de

or

mailto://a_j_schmidt@rocketmail.com
}


interface

uses SysUtils;

	{ used for EAN 8/13 }
	function CheckSumModulo10(const data:string):string;



implementation


{


}

function CheckSumModulo10(const data:string):string;
	var i,fak,sum : Integer;
begin
	sum := 0;
	fak := Length(data);
	for i:=1 to Length(data) do
	begin
		if (fak mod 2) = 0 then
			sum := sum + (StrToInt(data[i])*1)
		else
			sum := sum + (StrToInt(data[i])*3);
		dec(fak);
	end;
	if (sum mod 10) = 0 then
		result := data+'0'
	else
		result := data+IntToStr(10-(sum mod 10));
end;




end.
