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
  force_build: boolean = False;

const
  CmdOptions: array[1..4] of TCmdOption = (
    (name: '--compiler'; descr: 'Use indicated binary as compiler'),
    (name: '--force'; descr: 'Force building make.exe'),
    (name: '--help'; descr: 'This message.'),
    (name: '--verbose'; descr: 'Be more verbose.')
    );

  //parsing FPC output
  procedure command_callback(line: string; active: boolean);
  begin
    if verbose then
    begin
      str.Text := str.Text + line;

      while str.Count > 1 do
      begin
        writeln(str[0]);
        str.Delete(0);
      end;

      while str.Count > 0 do
      begin
        writeln(str[0]);
        str.Delete(0);
      end;
    end;
  end;

  procedure create_and_build_make;
  var
    tmp: TStrings;
    param: TStrings;
    src_name: string;
    exit_code: integer;
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

    writeln('-- Build file has been written to: ', macros_expand('make$(EXE)'));
  end;

  procedure usage;
  var
    i: integer;
  begin
    writeln('PMake the pascal build tool. Version ', PMAKE_VERSION, ' [',
{$I %DATE%}
      , '] for ',
{$I %FPCTARGETCPU%}
      );
    writeln('Copyright (c) 2016-2017 by Darius Blaszyk');
    writeln;
    writeln('Usage ');
    writeln('  pmake [options] <path-to-source>');
    writeln;
    writeln('Options');

    for i := low(CmdOptions) to high(CmdOptions) do
      writeln(Format('  %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));

    halt(1);
  end;

  procedure parse_commandline;
  var
    i, j: integer;
    found: boolean;
  begin
    if ParamCount = 0 then
      usage;

    //the binary dir is determined by the current directory pmake is invoked from
    set_('PMAKE_BINARY_DIR', IncludeTrailingPathDelimiter(GetCurrentDir));

    i := 1;

    while i <= ParamCount do
    begin
      found := False;
      for j := low(CmdOptions) to high(CmdOptions) do
      begin
        if ParamStr(i) = CmdOptions[j].name then
        begin
          begin

            found := True;

            case CmdOptions[j].name of
              '--compiler':
              begin
                if i < ParamCount then
                begin
                  Inc(i);
                  set_('PMAKE_PAS_COMPILER', ParamStr(i));
                  if not FileExists(ParamStr(i)) then
                    message(FATAL_ERROR, 'fatal error: cannot find the pascal compiler');
                end
                else
                begin
                  writeln('error: please supply a valid path for the compiler');
                  usage;
                end;
              end;
              '--help': usage;
              '--verbose': verbose := True;
              '--force': force_build := True;
            end;
          end;
          if found then
            break;
        end
        else
        if ParamStr(i) <> '' then
          if DirectoryExists(ParamStr(i)) then
          begin
            found := True;
            set_('PMAKE_SOURCE_DIR', IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(i))));
            Inc(i);
          end;
      end;

      if not found then
      begin
        writeln('error: invalid commandline parameter ', ParamStr(i));
        usage;
      end;

      Inc(i);
    end;
  end;

begin
  parse_commandline;

  pmake_initialize;

  if not FileExists(macros_expand('make$(EXE)')) or force_build then
    create_and_build_make;

  pmakecache_write;
  writeln('-- Generating done');
end.
