unit pmake_main;

{$mode objfpc}{$H+}

interface

uses
  Classes;

procedure pmake_initialize(BinDir: string);
procedure create_and_build_make(debug: boolean);

var
  str: TStrings;

implementation

uses
  SysUtils,
  pmake_utilities,
  pmake_variables,
  pmake_api;

procedure pmake_initialize(BinDir: string);
begin
  //the binary dir is determined by the current directory pmake is invoked from
  set_('PMAKE_BINARY_DIR', BinDir);

  //folder where pmake is located, also location of units for make and make2
  set_('PMAKE_TOOL_DIR', ExtractFilePath(ParamStr(0)));

  CompilerDefaults;
end;

//parsing FPC output
procedure command_callback(line: string; active: boolean);
begin
  if verbose then
  begin
    str.Text := str.Text + line;

    while str.Count > 0 do
    begin
      OutputLn(str[0]);
      str.Delete(0);
    end;
  end;
end;

procedure create_and_build_make(debug: boolean);
var
  tmp: TStrings;
  param: TStrings;
  src_name: string;
  exit_code: integer;
begin
  OutputLn('-- Creating makefile');

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
  param.Add('-Fu' + val_('PMAKE_TOOL_DIR'));
  param.Add(src_name);
  param.Add(macros_expand('-o$(PMAKE_BINARY_DIR)make$(EXE)'));

  str := TStringList.Create;
  exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @command_callback);
  str.Free;

  //remove the object and source files
  if verbose then
    OutputLn('-- Deleting temporary files');

  if not debug then
    DeleteFile(src_name);

  DeleteFile(ChangeFileExt(src_name, '.o'));

  if exit_code <> 0 then
    messagefmt(FATAL_ERROR, '(1009) fatal error: cannot compile %s', [macros_expand('make$(EXE)')]);

  OutputLn('-- Build file has been written to: ' + macros_expand('make$(EXE)'));
end;

end.
