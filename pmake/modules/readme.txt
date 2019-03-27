In this folder all supported modules are stored. A module is an extension to pmake. Pmake will autmatically compile them and link them into make2 so it can be used in PMake.txt. In principle anything is allowed as long as no external dependencies are introduced this way. In that case add a module in your own project and use the add_module_path function that is defined in the API. PMake also adds modules stored in that folder.

An examplehow to use a module:

  init_module_GIT;
  if valb('GIT_FOUND') then
    message('Git found: $(GIT_EXECUTABLE)');

--Darius 
--March 19th 2019