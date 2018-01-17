unit pmake_quickstart;

{$mode objfpc}{$H+}

interface

procedure pmake_run_quickstart(srcdir: string);

implementation

uses
  Classes,
  SysUtils,
  make_main,
  pmake_variables;

var
  filelist: TStringList;

function prompt(description, question, default: string): string;
var
  answer: string;
begin
  writeln(description);

  if default <> '' then
    write('> ', question, ' [', default, ']: ')
  else
    write('> ', question, ': ');

  readln(answer);
  if answer = '' then
    answer := default;

  writeln;

  Result := answer;
end;

procedure search_pascal_files(const path: string; recursive: boolean);
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
          case ExtractFileExt(info.Name) of
            '.pas', '.pp', '.inc': filelist.Add(path + info.Name);
          end;
        end
        else
          //start the recursive search
          if (info.Name <> '.') and (info.Name <> '..') and (recursive = True) then
            search_pascal_files(IncludeTrailingBackSlash(path + info.Name), recursive);

      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

procedure ParseSourceFile(fname: string; var pmtype, name: string);
var
  f: TStrings;
  i: integer;
  tmp: string;
begin
  f := TStringList.Create;
  f.LoadFromFile(fname);

  //name is here based on the directory the file is located in
  tmp := LowerCase(ExtractFileDir(fname) + '.tmp');
  name := LowerCase(ExtractFileName(ChangeFileExt(tmp, '')));

  //find pmtype: very naive, but it's a start!
  if ExtractFileExt(LowerCase(fname)) = '.inc' then
    pmtype := 'include'
  else
  begin
    for i := 0 to f.Count - 1 do
    begin
      if pos('unit', Trim(LowerCase(f[i]))) = 1 then
      begin
        pmtype := 'unit';
        break;
      end;
      if pos('program', Trim(LowerCase(f[i]))) = 1 then
      begin
        //name is here based on the source file
        name := LowerCase(ExtractFileName(ChangeFileExt(fname, '')));
        pmtype := 'program';
        break;
      end;
    end;
  end;
  f.Free;
end;

//a custom sort for the pmake file list
function ComparePath(List: TStringList; Index1, Index2: Integer): Integer;
var
  path1, path2: string;
begin
  path1 := ExtractFilePath(List[Index1]);
  path2 := ExtractFilePath(List[Index2]);

  if path1 < path2 then
    Result := -1
  else
    if path1 = path2 then
      Result := 0
    else
      Result := 1;
end;

procedure pmake_run_quickstart(srcdir: string);
var
  res, recursive, dir, pmtype, name: string;
  f: TStrings;
  i: integer;
begin
  writeln('Welcome to the PMake ', PMAKE_VERSION, ' quickstart utility.');
  writeln;
  writeln('Please enter values for the following settings (just press ENTER to');
  writeln('accept a default value, if one is given in brackets).');
  writeln;

  recursive := prompt('PMake can recursively search the source directory provided and add PMake.txt files as required.',
  'Would you like Pmake to search recursively? (y/n)', 'y');

  f := TStringList.Create;

  //root PMake.txt
  res := prompt('Some projects require a specific minimum compiler version, you can specify this now.', 'Please enter the minimum compiler version (' + val_('PMAKE_PAS_COMPILER_VERSION') + ')', '');
  f.Add('compiler_minimum_required(' + StringReplace(res, '.', ',', [rfReplaceAll]) + ');');

  res := prompt('PMake requires a project name for the entire project', 'Please enter the project name', '');
  f.Add('project(''' + res + ''');');

  writeln('processing all PMake.txt files now');

  filelist := TStringList.Create;
  search_pascal_files(srcdir, recursive = 'y');

  //first sort on directory
  filelist.CustomSort(@ComparePath);

  dir := srcdir;
  i := 1;
  while i < filelist.Count do
  begin
    //new folder found
    if ExtractFilePath(filelist[i]) <> dir then
    begin
      f.SaveToFile(dir + 'PMake.txt');
      f.Clear;
      dir := ExtractFilePath(filelist[i]);
    end;

    ParseSourceFile(filelist[i], pmtype, name);
    case pmtype of
      'unit': f.Add(Format('add_library(''lib_%s'', [''%s'']);', [name, ExtractFileName(filelist[i])]));
      'program': f.Add(Format('add_executable(''%s'', ''%s'', ''%s'', []);', [name, name, ExtractFileName(filelist[i])]));
      'include': f.Add(Format('include_directories(''lib_%s'', [''.'']);', [name]));
    end;
    inc(i);
  end;

  //save the last folder
  f.SaveToFile(dir + 'PMake.txt');

  filelist.Free;
  f.Free;

  writeln('done.');
  halt(0);
end;

end.

