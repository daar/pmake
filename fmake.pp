program fmake;

{$mode objfpc}{$H+}
{ $define debug}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF} {$ifdef debug}
  HeapTrc, {$endif}
  Classes,
  SysUtils,
  getopts,
  ufmake,
  Process;

const
  FMakeVersion = '0.01';

var
  fname: string = '';
  f: Text;
  BasePath: string = '';
  verbose: boolean = false;

  Procedure PrintUsage;
  begin
    writeln('FMake the freepascal build tool. Version ', FMakeVersion, ' [', {$I %DATE%}, '] for ', {$I %FPCTARGETCPU%});
    writeln('Copyright (c) 2016 by Darius Blaszyk');
    writeln(ParamStr(0), ' [options]');
    writeln;
    writeln('Options');
    writeln(' -h, --help          This help screen.');
    writeln(' -f, --fpc <path>    Specify a custom FPC compiler location. If');
    writeln('                     omitted the PATH envronment variable is searched.');
    writeln(' -v, --verbose       Be more verbose.');
    halt(0);
  end;

  procedure ParseOptions;
  var
    c: char = #0;
    optionindex: Longint = 0;
    theopts: array[1..3] of TOption;
  begin
    with theopts[1] do
    begin
      Name := 'verbose';
      has_arg := 0;
      flag := nil;
      Value := 'v';
    end;
    with theopts[2] do
    begin
      Name := 'fpc';
      has_arg := 1;
      flag := nil;
      Value := 'f';
    end;
    with theopts[3] do
    begin
      Name := '';
      has_arg := 0;
      flag := nil;
    end;
    c := #0;
    repeat
      c := getlongopts('hf:v', @theopts[1], optionindex);
      case c of
        'h': PrintUsage;
        'f': ufmake.fpc := optarg;
        'v': verbose := true;
        '?', ':': PrintUsage;
      end;
    until c = endofoptions;

    if optind <= paramcount then
    begin
      Write('Unknown options : ');
      while optind <= paramcount do
      begin
        Write(ParamStr(optind), ' ');
        Inc(optind);
      end;
      writeln;
      PrintUsage;
    end;
  end;

  procedure FileSearch(const path: string);
  var
    info: TSearchRec;
    fi: Text;
    c: char;
    tmp: string;
  begin
    if FindFirst(path + '*', faAnyFile, info) = 0 then
    begin
      try
        repeat
          if (info.Attr and faDirectory) = 0 then
          begin
            //add FMake.txt to the compile 'script'
            if info.name = 'FMake.txt' then
            begin
              tmp := path + info.name;
              writeln('-- Found ', StringReplace(tmp, BasePath, '.' +
                DirectorySeparator, [rfReplaceAll]));
              assign(fi, path + info.name);
              reset(fi);
              writeln(f, '  add_subdirectory(''' +
                ExtractFilePath(path + info.name) + ''');');


              while not EOF(fi) do
              begin
                read(fi, c);
                write(f, c);
              end;
              close(fi);
            end;
          end
          else
          //start the recursive search
          if (info.name <> '.') and (info.name <> '..') then
            FileSearch(IncludeTrailingBackSlash(path + info.name));

        until FindNext(info) <> 0
      finally
        FindClose(info);
      end;
    end;
  end;

var
  fpc_out: TStrings;
  param: TStrings;
  fpc_msg: TFPList;
  ShowMsg: TMessages;

begin
  ParseOptions;

  if verbose then
    writeln('-- FPC compiler ', fpc);

  fname := GetTempFileName('.', 'fmake');
  Assign(f, fname);
  rewrite(f);

  writeln(f, 'program make;');
  writeln(f, 'uses ufmake;');
  writeln(f, 'begin');

  BasePath := IncludeTrailingBackSlash(GetCurrentDir);
  FileSearch(BasePath);

  writeln(f, '  run_make;');
  writeln(f, 'end.');
  Close(f);

  param := TStringList.Create;
  param.Add('-viq');

  //build the makefile

  {$ifdef debug}
  param.Add('-gh');
  {$endif}

  param.Add(fname);

  //based on the app extension of fmake, add the same extension to make
  param.Add('-omake' + ExtractFileExt(ParamStr(0)));

  fpc_out := RunFPCCommand(param);
  param.Free;

  fpc_msg := ParseFPCCommand(fpc_out);
  fpc_out.Free;

  if verbose then
    ShowMsg := mAll
  else
    ShowMsg := [mFail];

  WriteFPCCommand(fpc_msg, ShowMsg);
  fpc_msg.Free;

  //remove the object and source files
  {$ifndef debug}
  DeleteFile(fname);
  {$endif}
  DeleteFile(ChangeFileExt(fname, '.o'));

  writeln('-- Generating done');
  writeln('-- Build file has been written to: ', ExpandFileName('make'));
end.
