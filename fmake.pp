program fmake;

{$mode objfpc}{$H+}
{ $define debug}

uses
  {$ifdef debug}
  HeapTrc,
  {$endif}
  SysUtils, ufmake;

var
  fname: string;
  f: Text;
  fpc: string;
  sw : string = '';

  procedure FileSearch(const path: string);
  var
    info: TSearchRec;
    fi: Text;
    c: char;
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
              writeln('  -- found ', path + info.name);
              assign(fi, path + info.name);
              reset(fi);
              writeln(f, '  add_subdirectory(''' + ExtractFilePath(path + info.name) + ''');');
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

begin
  fname := GetTempFileName('.', 'fmake');
  Assign(f, fname);
  rewrite(f);

  writeln(f, 'program make;');
  writeln(f, 'uses ufmake;');
  writeln(f, 'begin');
  writeln(f, '  init_make;');

  FileSearch(IncludeTrailingBackSlash(GetCurrentDir));

  writeln(f, '  run_make;');
  writeln(f, 'end.');
  Close(f);

  //build the makefile
  {$ifdef debug}
  sw := '-gh';
  {$endif}
  fpc := ExeSearch('fpc', SysUtils.GetEnvironmentVariable('PATH'));
  ExecuteProcess(fpc, fname + ' -omake ' + sw, []);

  //remove the object and source files
  {$ifndef debug}
  DeleteFile(fname);
  {$endif}
  DeleteFile(ChangeFileExt(fname, '.o'));

  writeln('  -- Generating done');
  writeln('  -- Build file has been written to: ', ExpandFileName('make'));
end.
