(*
 * this program is used for testing reading and writing to PMakeCache.txt
 * based on http://wiki.lazarus.freepascal.org/XML_Tutorial#Usage_Examples
 *)
program rw_cache;

uses
  DOM,
  XMLRead,
  XMLWrite,
  SysUtils;

var
  doc: TXMLDocument;
  root: TDOMElement;

  procedure write_pmakecache_init;
  begin
    if doc = nil then
      doc := TXMLDocument.Create;

    //create the root node
    root := doc.CreateElement('PMakeCache');
    doc.AppendChild(root);
  end;

  procedure write_pmakecache_finish;
  begin
    //write to XML
    writeXMLFile(doc, 'PMakeCache.txt');

    doc.Free;
  end;

  procedure write_boolean_pmakecache(variable: string; Value: boolean);
  var
    elem: TDOMElement;
  begin
    elem := doc.CreateElement(variable);
    root.AppendChild(elem);

    TDOMElement(elem).SetAttribute('type', 'boolean');
    TDOMElement(elem).SetAttribute('value', BoolToStr(Value, True));
  end;

  procedure write_integer_pmakecache(variable: string; Value: integer);
  var
    elem: TDOMElement;
  begin
    elem := doc.CreateElement(variable);
    root.AppendChild(elem);

    TDOMElement(elem).SetAttribute('type', 'integer');
    TDOMElement(elem).SetAttribute('value', IntToStr(Value));
  end;

  procedure write_float_pmakecache(variable: string; Value: double);
  var
    elem: TDOMElement;
  begin
    elem := doc.CreateElement(variable);
    root.AppendChild(elem);

    TDOMElement(elem).SetAttribute('type', 'float');
    TDOMElement(elem).SetAttribute('value', FloatToStr(Value));
  end;

  procedure write_string_pmakecache(variable: string; Value: string);
  var
    elem: TDOMElement;
  begin
    elem := doc.CreateElement(variable);
    root.AppendChild(elem);

    TDOMElement(elem).SetAttribute('type', 'string');
    TDOMElement(elem).SetAttribute('value', Value);
  end;

  procedure write_filecache_pmakecache(variable: string; crc: word; path: string);
  var
    elem: TDOMElement;
    itm: TDOMElement;
    node: TDOMNode;
  begin
    elem := TDOMElement(doc.DocumentElement.FindNode(variable));

    if elem = nil then
    begin
      elem := doc.CreateElement(variable);
      root.AppendChild(elem);
      TDOMElement(elem).SetAttribute('type', 'filecache');
    end;

    itm := doc.CreateElement('item');
    elem.AppendChild(itm);

    TDOMElement(itm).SetAttribute('crc', IntToStr(crc));
    TDOMElement(itm).SetAttribute('path', path);
  end;

  procedure read_pmakecache;
  var
    child: TDOMNode;
    j: integer;
    nl: TDOMNodeList;
  begin
    try
      ReadXMLFile(doc, 'PMakeCache.txt');

      //get the first child under the root
      child := doc.FirstChild.FirstChild;

      while child <> nil do
      begin
        write(child.NodeName + ' ' + child.Attributes.GetNamedItem(
          'type').NodeValue + ': ');

        case child.Attributes.Item[0].NodeValue of
          'boolean': writeln(child.Attributes.GetNamedItem('value').NodeValue);
          'integer': writeln(child.Attributes.GetNamedItem('value').NodeValue);
          'float': writeln(child.Attributes.GetNamedItem('value').NodeValue);
          'string': writeln(child.Attributes.GetNamedItem('value').NodeValue);
          'filecache':
          begin
            writeln;

            nl := child.ChildNodes;

            for j := 0 to nl.Count - 1 do
            begin
              write('  crc=' + nl.Item[j].Attributes.GetNamedItem('crc').NodeValue + ' path=');
              writeln(nl.Item[j].Attributes.GetNamedItem('path').NodeValue);
            end;
          end;
        end;

        child := child.NextSibling;
      end;
    finally
      doc.Free;
    end;
  end;

begin
  write_pmakecache_init;
    write_boolean_pmakecache('PMAKE_BOOLEAN', True);
    write_integer_pmakecache('PMAKE_INTEGER', 1);
    write_float_pmakecache('PMAKE_FLOAT', pi);
    write_string_pmakecache('PMAKE_STRING', 'Hello World!');
    write_filecache_pmakecache('PMAKE_FILECACHE', 12345, 'mypath');
    write_filecache_pmakecache('PMAKE_FILECACHE', 54321, 'mydirectory');
  write_pmakecache_finish;

  read_pmakecache;

  readln;
end.
