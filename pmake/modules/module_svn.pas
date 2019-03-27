unit module_svn;

{$mode objfpc}{$H+}

{
  The module defines the following variables:
    SVN_EXECUTABLE     : Path to SVN command-line client.
    SVN_FOUND          : True if the SVN command-line client was found.
    SVN_VERSION_STRING : The version of SVN found.

  Example usage:
    init_module_SVN;
    if valb('SVN_FOUND') then
      message('SVN found: $(SVN_EXECUTABLE)');
}
interface

procedure init_module_svn;

implementation

uses
  Classes,
  Process,
  pmake_variables,
  pmake_utilities;

procedure init_module_svn;
var
  exe: string;
  tmp: string;
  s: TStrings;
begin
  exe := which('svn');

  set_('SVN_EXECUTABLE', exe);
  set_('SVN_FOUND', exe <> '');

  RunCommand(exe, ['--version'], tmp);

  s := TStringList.Create;
  s.Delimiter := ' ';
  s.DelimitedText := tmp;
  set_('SVN_VERSION_STRING', s[2]);
  s.Free;
end;

end.
