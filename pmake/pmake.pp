program pmake;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Classes,
  SysUtils,
  pmake_main,
  pmake_variables,
  pmake_utilities,
  pmake_api, 
  pmake_quickstart;

var
  force_build: boolean = False;
  debug: boolean = False;
  quickstart: boolean = False;

const
  CmdOptions: array[1..6] of TCmdOption = (
    (name: '--compiler'; descr: 'Use indicated binary as compiler'),
    (name: '--debug'; descr: 'Do not delete the make source file.'),
    (name: '--force'; descr: 'Force building the make executable'),
    (name: '--help'; descr: 'This message.'),
    (name: '--quickstart'; descr: 'Create PMake.txt files in the supplied source directory'),
    (name: '--verbose'; descr: 'Be more verbose.')
    );

  procedure usage;
  var
    i: integer;
  begin
    OutputLn('PMake the pascal build tool. Version ' + PMAKE_VERSION + ' [' +
{$I %DATE%}
      + '] for ' +
{$I %FPCTARGETCPU%}
      );
    OutputLn('Copyright (c) 2016-2017 by Darius Blaszyk');
    OutputLn('');
    OutputLn('Usage ');
    OutputLn('  pmake [options] <path-to-source>');
    OutputLn('');
    OutputLn('Options');

    for i := low(CmdOptions) to high(CmdOptions) do
      OutputLn(Format('  %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));

    halt(1);
  end;

  procedure parse_commandline;
  var
    i, j: integer;
    found: boolean;
  begin
    if ParamCount = 0 then
      usage;

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
                  StdErrLn('error: please supply a valid path for the compiler');
                  usage;
                end;
              end;
              '--help': usage;
              '--force': force_build := True;
              '--debug': debug := True;
              '--verbose': verbose := True;
              '--quickstart': quickstart := True;
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
        StdErrLn('error: invalid command-line parameter ' + ParamStr(i));
        usage;
      end;

      Inc(i);
    end;
  end;

begin
  pmake_initialize(IncludeTrailingPathDelimiter(GetCurrentDir));

  parse_commandline;

  if quickstart then
    pmake_run_quickstart(val_('PMAKE_SOURCE_DIR'));

  if not FileExists(macros_expand('make$(EXE)')) or force_build then
    create_and_build_make(debug);

  pmakecache_write;
  OutputLn('-- Generating done');
end.
