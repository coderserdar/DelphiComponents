var Value1 : String;
var Value2 : String;

Response.ContentType := 'text/html';
Response.Status := '200 OK';
Response.Write('<html><body>');
Response.Write('ICS and DWScript demo<br>');
Response.Write('Server time is ' + DateTimeToStr(Now) + '<br>');
if not Request.CheckParamByName('Value1', Value1) then
    Response.Write('Missing Value1 parameter<br>')
else if not Request.CheckParamByName('Value2', Value2) then
    Response.Write('Missing Value2 parameter<br>')
else Response.Write(Value1 + ' + ' + Value2 + ' = ' + 
    IntToStr(StrToIntDef(Value1, 0) + StrToIntDef(Value2, 0)));
Response.Write('</body></html>');
