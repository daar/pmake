program fmake;

{$mode objfpc}{$H+}
{ $define debug}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF} {$ifdef debug}
  HeapTrc, {$endif}
  Classes,
  SysUtils,
  ufmake,
  Process;

var
  fname: string = '';
  f: Text;
  BasePath: string = '';

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

begin
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

  WriteFPCCommand(fpc_msg, [mFail]);
  fpc_msg.Free;

  //remove the object and source files
  {$ifndef debug}
  DeleteFile(fname);
  {$endif}
  DeleteFile(ChangeFileExt(fname, '.o'));

  writeln('-- Generating done');
  writeln('-- Build file has been written to: ', ExpandFileName('make'));
end.
