unit module_lcl;

{$mode objfpc}{$H+}

{
  See for more information:
    wiki.freepascal.org/Using_the_LCL_without_Lazarus

  The module defines the following variables:
    LAZARUS_PATH       : Path to the lazarus executable
    LCL_FOUND          : True if the lazarus executable path was found
    LCL_VERSION_STRING : The version of lazarus found
    LCL_TOOLKIT        : If not set in PMake, a default value will be set

  Example usage:
    init_module_LCL;
    if valb('LCL_FOUND') then
      message('LCL found: $(LAZARUS_PATH)');
}

interface

procedure init_module_LCL;

implementation

uses
  Classes,
  SysUtils,
  Process,
  pmake_api,
  pmake_variables,
  pmake_utilities;

procedure init_module_LCL;
var
  path: string;
  tmp: string;
  s: TStrings;
begin
  path := which('lazarus');

  if path = '' then
{$IFDEF WINDOWS}
    if FileExists('C:\lazarus\lazarus.exe') then
      path := 'C:\lazarus\lazarus.exe';
{$ENDIF}
{$IFDEF LINUX}
    if FileExists('/usr/lib/lazarus') then
      path := '/usr/lib/lazarus'
    else
      if FileExists('/usr/share/lazarus') then
        path := '/usr/share/lazarus';
{$ENDIF}
{$IFDEF DARWIN}
    if FileExists('/usr/local/bin/lazarus') then
      path := '/usr/local/bin/lazarus';
{$ENDIF}

  //set a default toolkit in case the user did not set it
  if vals('LCL_TOOLKIT') = '' then
{$IFDEF WINDOWS}
    set_('LCL_TOOLKIT', 'win32');
{$ENDIF}
{$IFDEF LINUX}
    set_('LCL_TOOLKIT', 'gtk2');
{$ENDIF}
{$IFDEF DARWIN}
    set_('LCL_TOOLKIT', 'cocoa');
{$ENDIF}

  set_('LAZARUS_PATH', ExtractFilePath(path));

  add_compile_option('-Fu$(LAZARUS_PATH)lcl/units/$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)');
  add_compile_option('-Fu$(LAZARUS_PATH)lcl/units/$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)/$(LCL_TOOLKIT)');
  add_compile_option('-Fu$(LAZARUS_PATH)components/lazutils/lib/$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)');
  add_compile_option('-Fu$(LAZARUS_PATH)packager/units/$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)');

  set_('LCL_FOUND', path <> '');

  //todo: getting the version from commandlinedoes not seem to work
  //RunCommand(path, ['--version'], tmp);
  //
  //s := TStringList.Create;
  //s.Delimiter := ' ';
  //s.DelimitedText := tmp;
  //set_('LCL_VERSION_STRING', s[2]);
  //s.Free;
end;

end.
