# A simple YAML parser written in Nix, suitable for parsing Spago lock files
# (but NOT suitable for parsing arbitrary YAML).
#
# Currently assumes there are no comments in the lockfile.
#
# Based on https://github.com/DavHau/fromYaml
{lib}: let
  fromYAML = text: let
    lines = lib.splitString "\n" text;

    # Remove empty lines. If Spago introduces other special lines
    # (like comments) then this can be expanded.
    filtered = let
      noEmpties = lib.filter (line: (builtins.match "[[:space:]]*" line == null)) lines;
    in
      if noEmpties == []
      then throw "Empty YAML file"
      else noEmpties;

    # Match the following structure for each line:
    #
    # {
    #   isListEntry = bool; # true if elem is part of a list
    #   indent = int; # indentation level, to represent nesting
    #   key = string | null;
    #   value = string | null;
    # }
    matchLine = line: let
      # Single line key value statement
      singleLine = builtins.match "([ -]*)(.*): (.*)" line;
      # Multi line key value (line contains only key)
      multiLine = builtins.match "([ -]*)(.*):$" line;
      # Is the line starting a new list element?
      listEntry = builtins.match "([[:space:]]*-[[:space:]]+)(.*)" line;
      # Handle list elements (lines starting with ' -')
    in
      if listEntry != null
      then rec {
        isListEntry = true;
        indent = (builtins.stringLength (builtins.elemAt listEntry 0)) / 2;
        key =
          if singleLine != null
          then builtins.elemAt singleLine 1
          else null;
        value =
          if singleLine != null
          then builtins.elemAt singleLine 2
          else builtins.elemAt listEntry 1;
      }
      # Handle single line key -> val assignments
      else if singleLine != null
      then {
        isListEntry = false;
        indent = (builtins.stringLength (builtins.elemAt singleLine 0)) / 2;
        key = builtins.elemAt singleLine 1;
        value = builtins.elemAt singleLine 2;
      }
      # Handle multi-line key -> object assignment
      else if multiLine != null
      then {
        isListEntry = false;
        indent = (builtins.stringLength (builtins.elemAt multiLine 0)) / 2;
        key = builtins.elemAt multiLine 1;
        value = null;
      }
      else null;

    # Extract indent, key, value, isListEntry for each line
    matched = builtins.map matchLine filtered;

    # Store total number of lines
    numLines = builtins.length filtered;

    #  Process line by line via a deep recursion, so that this function is
    #  executed once on each line.
    #
    #  It is each iteration's responsibility to traverse through its children
    #  (for example list elements under a key), and merge/update these correctly
    #  with itself.
    #
    #  By looking at the regex result of the current line and the next line,
    #  we know what type of structure the current line creates, if it has any
    #  children, and the the type of the children's structure.
    processLines = lines: index: let
      line = builtins.elemAt filtered index;

      nextLine = processLines lines (index + 1);

      match = builtins.elemAt matched index;

      nextMatch = builtins.elemAt matched (index + 1);

      indent =
        if index == -1
        then -1
        else match.indent;

      childrenMustBeList =
        nextMatch.indent
        > indent
        && nextMatch.isListEntry;

      findChildIndices = searchIndex: let
        matchSearch = builtins.elemAt matched searchIndex;
      in
        if searchIndex >= numLines
        then []
        else if matchSearch.indent > childIndent
        then findChildIndices (searchIndex + 1)
        else if matchSearch.indent == childIndent
        then
          if matchSearch.isListEntry == childrenMustBeList
          then [searchIndex] ++ findChildIndices (searchIndex + 1)
          else findChildIndices (searchIndex + 1)
        else [];

      childIndent =
        if nextMatch.indent > indent
        then nextMatch.indent
        else null;

      childIndices =
        if childIndent == null
        then []
        else findChildIndices (index + 1);

      childObjects = builtins.map (processLines lines) childIndices;

      childrenMerged =
        lib.foldl (all: currentObject: (
          if builtins.isAttrs currentObject
          then
            (
              if all == null
              then {}
              else all
            )
            // currentObject
          else
            (
              if all == null
              then []
              else all
            )
            ++ currentObject
        )) null
        childObjects;

      result =
        if index == (-1)
        then childrenMerged
        else if match.isListEntry
        then
          # Has key and value -> check if attr continue
          if match.key != null && match.value != null
          then
            # Attrs element follows
            if match.indent == nextMatch.indent && !nextMatch.isListEntry
            then [
              ({"${match.key}" = match.value;} // nextLine)
            ]
            # List or unindent follows
            else [
              {
                "${match.key}" = match.value;
              }
            ]
          # List begin with only a key (child list/attrs follow)
          else if match.key != null
          then [
            {
              "${match.key}" = childrenMerged;
            }
          ]
          # Value only (list elem with plain value)
          else [
            match.value
          ]
        # Not a list entry
        else
          # Has key and value
          if match.key != null && match.value != null
          then {
            "${match.key}" = match.value;
          }
          # Key only
          else {
            "${match.key}" = childrenMerged;
          };
    in
      result;
  in
    processLines filtered (-1);
in
  fromYAML
