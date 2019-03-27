unit module_git;

{$mode objfpc}{$H+}

{
  The module defines the following variables:
    GIT_EXECUTABLE     : Path to Git command-line client.
    GIT_FOUND          : True if the Git command-line client was found.
    GIT_VERSION_STRING : The version of Git found.

  Example usage:
    init_module_GIT;
    if valb('GIT_FOUND') then
      message('Git found: $(GIT_EXECUTABLE)');
}

interface

procedure init_module_git;

implementation

uses
  Classes,
  Process,
  pmake_variables,
  pmake_utilities;

procedure init_module_git;
var
  exe: string;
  tmp: string;
  s: TStrings;
begin
  exe := which('git');

  set_('GIT_EXECUTABLE', exe);
  set_('GIT_FOUND', (exe <> ''));

  RunCommand(exe, ['--version'], tmp);

  s := TStringList.Create;
  s.Delimiter := ' ';
  s.DelimitedText := tmp;
  set_('GIT_VERSION_STRING', s[2]);
  s.Free;
end;


end.
