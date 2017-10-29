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

type
  TCmdOption = record
    name: string;
    descr: string;
  end;

var
  str: TStrings;
  verbose: boolean = False;

const
  CmdOptions: array[1..3] of TCmdOption = (
    (name: '--compiler'; descr: 'Use indicated binary as compiler'),
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
    First: boolean;
  begin
    writeln('PMake the pascal build tool. Version ', PMAKE_VERSION, ' [',
{$I %DATE%}
      , '] for ',
{$I %FPCTARGETCPU%}
      );
    writeln('Copyright (c) 2016-2017 by Darius Blaszyk');
    writeln;
    writeln('usage: ', ParamStr(0), ' <subcommand> [options] [args]');
    writeln;

    First := True;
    for i := low(CmdOptions) to high(CmdOptions) do
      if pos('--', CmdOptions[i].name) = 0 then
      begin
        if First then
          writeln('Subcommands');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));
      end;

    if First = False then
      writeln;

    First := True;
    for i := low(CmdOptions) to high(CmdOptions) do
      if pos('--', CmdOptions[i].name) <> 0 then
      begin
        if First then
          writeln('Options');
        First := False;
        writeln(Format(' %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));
      end;

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
                    message(FATAL_ERROR, 'fatal error: cannot find the supplied compiler');
                end
                else
                begin
                  writeln('error: please supply a valid path for the compiler');
                  usage;
                end;
              end;
              '--help': usage;
              '--verbose': verbose := True;
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
  pmake_initialize;

  parse_commandline;

  if not FileExists(macros_expand('make$(EXE)')) then
    create_and_build_make;

  pmakecache_write;
  writeln('-- Generating done');
end.
