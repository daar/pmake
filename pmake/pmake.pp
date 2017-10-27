program pmake;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Classes,
  SysUtils,
  pmake_variables,
  pmake_utilities,
  pmake_api;

var
  str: TStrings;
  verbose: boolean = False;

  //parsing FPC output
  procedure command_callback(line: string);
  begin
    if verbose then
    begin
    str.Text := str.Text + line;

    while str.Count > 0 do
      writeln(str[0]);

    if (str.Count > 0) and (line = '') then
      writeln(str[0]);
    end;
  end;

  procedure create_and_build_make;
  var
    tmp: TStrings;
    param: TStrings;
    src_name: string;
    exit_code: Integer;
  begin
    writeln('-- Creating makefile');

    tmp := TStringList.Create;

    tmp.Add('program make;');
    tmp.Add('uses {$IFDEF UNIX} cthreads, {$ENDIF} make_main;');
    tmp.Add('begin');
    tmp.Add('  make_execute;');
    tmp.Add('end.');

    src_name := GetTempFileName('.', 'pmake');
    tmp.SaveToFile(src_name);
    tmp.Free;

    param := TStringList.Create;
    param.Add('-viq');
    //add the unit search path to the pmake source directory
    param.Add('-FU' + UnitsOutputDir(val_('PMAKE_BINARY_DIR')));
    param.Add('-Fu' + val_('PMAKE_SOURCE_DIR'));
    param.Add(src_name);
    param.Add(macros_expand('-omake$(EXE)'));

    if verbose then
      writeln('-- Executing ', val_('PMAKE_PAS_COMPILER'), ' ', param.Text);

    str := TStringList.Create;
    exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @command_callback);
    str.Free;

    //remove the object and source files
    if verbose then
      writeln('-- Deleting temporary files');

    DeleteFile(src_name);
    DeleteFile(ChangeFileExt(src_name, '.o'));

    if exit_code <> 0 then
      message(FATAL_ERROR, 'fatal error: cannot compile ' + macros_expand('make$(EXE)'));
  end;

  procedure parse_commandline;
  begin
    //need to implement a propoper command line parser here

    set_('PMAKE_SOURCE_DIR', IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1))));
    set_('PMAKE_BINARY_DIR', IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));

    verbose := True;
  end;

begin
  pmake_initialize;

  parse_commandline;

  if not FileExists(macros_expand('make$(EXE)')) then
    create_and_build_make;

  pmakecache_write;
end.
