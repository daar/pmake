(*
 * this program is used for testing reading and writing to PMakeCache.txt
 * based on http://wiki.lazarus.freepascal.org/XML_Tutorial#Usage_Examples
 *)
program rw_cache;

uses
  DOM,
  XMLRead,
  SysUtils;

var
  doc: TXMLDocument;
  child: TDOMNode;
  j: integer;
  count: integer;
  node: TDOMNodeList;
begin
  try
    ReadXMLFile(doc, 'PMakeCache.txt');

    child := doc.DocumentElement.FirstChild;

    while Assigned(child) do
    begin
      write(child.NodeName + ' ' + child.Attributes.GetNamedItem('type').NodeValue + ': ');

      case child.Attributes.Item[0].NodeValue of
        'boolean': writeln(child.Attributes.GetNamedItem('value').NodeValue);
        'integer': writeln(child.Attributes.GetNamedItem('value').NodeValue);
        'float': writeln(child.Attributes.GetNamedItem('value').NodeValue);
        'string': writeln(child.Attributes.GetNamedItem('value').NodeValue);
        'filecache':
          begin
            writeln;
            count := StrToInt(child.Attributes.GetNamedItem('count').NodeValue);
            node := child.ChildNodes;

            for j := 0 to count - 1 do
            begin
              write('    ' + node.Item[j].NodeName + ': crc=');
              write(node.Item[j].Attributes.GetNamedItem('crc').NodeValue + ' path=');
              writeln(node.Item[j].Attributes.GetNamedItem('path').NodeValue);
            end;
          end;
      end;

      //// using ChildNodes method
      //with child.ChildNodes do
      //  try
      //    for j := 0 to Count - 1 do
      //      writeln(format('%s %s (%s=%s; %s=%s)',
      //        [Item[j].NodeName,
      //        Item[j].FirstChild.NodeValue,
      //        Item[j].Attributes.Item[0].NodeName,
      //        // 1st attribute details
      //        Item[j].Attributes.Item[0].NodeValue,
      //        Item[j].Attributes.Item[1].NodeName,
      //        // 2nd attribute details
      //        Item[j].Attributes.Item[1].NodeValue
      //        ]));
      //  finally
      //    Free;
      //  end;
      child := child.NextSibling;
    end;
  finally
    doc.Free;
  end;

  readln;
end.
