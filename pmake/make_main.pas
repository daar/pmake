unit make_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure make_execute;
procedure search_pmake(const path: string);

implementation

uses
  crc16,
  pmake_utilities, pmake_variables, pmake_api;

var
  sline: TStrings;
  verbose: boolean = False;

//parsing FPC output
procedure command_callback(line: string; active: boolean);
begin
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
  begin
    writeln(sline[0]);
    sline.Delete(0);
  end;

  if not active then
  begin
    while sline.Count > 0 do
    begin
      writeln(sline[0]);
      sline.Delete(0);
    end;
  end;
end;

function check_rebuild_make2: boolean;
var
  i, idx: integer;
  f: TStrings;
  pmakecrc: word;
  p: string;
  c: word;
begin
  if not FileExists('PMakeCache.txt') then
    exit(True);

  if not FileExists(macros_expand('make2$(EXE)')) then
    exit(True);

  //return true if the PMake.txt is count is different
  if cache.GetValue('PMake/count', 0) <> pmakefiles.Count then
    exit(True);

  //return true if a crc / PMake.txt combination is not found
  f := TStringList.Create;
  for i := 0 to pmakefiles.Count - 1 do
  begin
    p := cache.GetValue(widestring(Format('PMake/item%d/path', [i + 1])), widestring(''));
    c := cache.GetValue(widestring(Format('PMake/item%d/crc', [i + 1])), 0);

    f.LoadFromFile(p);
    pmakecrc := crc_16(@f.Text[1], length(f.Text));

    idx := pmakefiles.IndexOf(p);

    //either the PMake.tx file does not exist, or the crc changed
    if (idx = -1) or (c <> pmakecrc) then
    begin
      f.Free;
      exit(True);
    end;
  end;
  f.Free;

  exit(False);
end;

//todo: replace this by a PMake interpreter that finds and follows the add_subdirectory function calls
procedure search_pmake(const path: string);
var
  info: TSearchRec;
begin
  if FindFirst(path + '*', faAnyFile, info) = 0 then
  begin
    try
      repeat
        if (info.Attr and faDirectory) = 0 then
        begin
          //add PMake.txt to the file list
          if info.Name = 'PMake.txt' then
            pmakefiles.Add(path + info.Name);
        end
        else
        //start the recursive search
        if (info.Name <> '.') and (info.Name <> '..') then
          search_pmake(IncludeTrailingBackSlash(path + info.Name));

      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

procedure make2_build;
var
  make2, f: TStrings;
  src_name: string;
  i, exit_code: integer;
  param: TStrings;
begin
  //create_pmakecache;

  make2 := TStringList.Create;

  make2.Add('program make2;');
  make2.Add('uses {$IFDEF UNIX} cthreads, {$ENDIF} make2_main, pmake_api, pmake_variables;');
  make2.Add('begin');
  make2.Add('  init_make2;');

  //insert code from PMake.txt files
  f := TStringList.Create;
  for i := 0 to pmakefiles.Count - 1 do
  begin
    f.LoadFromFile(pmakefiles[i]);
    make2.Add(f.Text);
  end;
  f.Free;

  make2.Add('  execute_make2;');
  make2.Add('end.');

  src_name := GetTempFileName('.', 'pmake');
  make2.SaveToFile(src_name);

  param := TStringList.Create;
  param.Add('-viq');
  //add the unit search path where the pmake executable is locate
  param.Add('-FU' + UnitsOutputDir(val_('PMAKE_BINARY_DIR')));
  param.Add('-Fu' + val_('PMAKE_SOURCE_DIR'));
  param.Add(src_name);
  param.Add(macros_expand('-omake2$(EXE)'));

  if verbose then
    writeln('-- Executing ', val_('PMAKE_PAS_COMPILER'), ' ', param.Text);

  sline := TStringList.Create;
  exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @command_callback);
  sline.Free;

  param.Free;

  //remove the object and source files
  if verbose then
    writeln('-- Deleting temporary files');

{$ifndef debug}
  DeleteFile(src_name);
{$endif}
  DeleteFile(ChangeFileExt(src_name, '.o'));

  if exit_code <> 0 then
    message(FATAL_ERROR, 'fatal error: cannot compile ' + macros_expand('make2$(EXE)'));
end;

procedure parse_commandline;
begin
  //need to implement a propoper command line parser here

  verbose := True;
end;

procedure make_execute;
var
  exit_code: integer;
begin
  pmake_initialize;
  pmakecache_read;

  parse_commandline;

  pmakefiles := TStringList.Create;
  search_pmake(val_('PMAKE_SOURCE_DIR'));

  if check_rebuild_make2 then
    make2_build;

  pmakecache_write;

  sline := TStringList.Create;
  exit_code := command_execute(macros_expand('make2$(EXE)', nil), nil, @command_callback);
  sline.Free;

  pmakefiles.Free;

  if exit_code <> 0 then
    message(FATAL_ERROR, 'fatal error: cannot execute ' + macros_expand('make2$(EXE)'));
end;

end.

