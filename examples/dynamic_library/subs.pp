{ 
  Example library 
  
  From chapter 12.2 of the FPC programmer's guide
  https://www.freepascal.org/docs-html/prog/progse55.html
}
library subs;

  function SubStr(CString: PChar; FromPos, ToPos: longint): PChar; cdecl;

  var
    Length: integer;

  begin
    Length := StrLen(CString);
    SubStr := CString + Length;
    if (FromPos > 0) and (ToPos >= FromPos) then
    begin
      if Length >= FromPos then
        SubStr := CString + FromPos - 1;
      if Length > ToPos then
        CString[ToPos] := #0;
    end;
  end;

exports
  SubStr;

end.
