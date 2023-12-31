## Bin2Inc
A Delphi binary file to .inc file tool with full source code.

This tool takes as input any file, reads it binary and creates a include file with constants arrays containing the file contents.

## Creating a .inc file
Simply run the bin2inc tool from command line and specify the parameters you want to personalizze your output file.

| Switch | Default | Mandatory | Multiple | Description |
|--------|---------|-----------|----------|-------------|
| -a:# | auto | :x: | :x: | Specifies the desired array alignment. # can be one of the values: 1 for byte alignment, 2 for word, 3 for cardinal, 4 for unsigned 64-bit integer. If a file cannot be aligned by specified alignment, automatic alignment will be used. |
| -c:## | 8 | :x: | :x: | Speficies the number of columns/values to use for line while describing arrays. |
| -f | :x: | :x: | :x: | Indicates to create constant reference structure to arrays at the end of include file. |
| -l | :x: | :x: | :x: | Specifies that hexadecimal values should be outputed as lowercase instead of uppercase. |
| -r | :x: | :x: | :x: | Specifies that files should be searched recursively in sub-folders. |
| -y | :x: | :x: | :x: | Specifies to automatically overwrite existing output file. |
| -i:mask | :x: | :white_check_mark: | :white_check_mark: | Specifies a mask (also using wildcards) for searching input files to be included. |
| -o:file | "file.inc" | :x: | :x: | Specifies the include output filename. |
| -s:## | 2 | :x: | :x: | Specifies that indentation should be made with the specified number of spaces. |
| -t | :x: | :x: | :x: | Specifies that indentation should be made with a tabulation character. |

## Including a constant in the code
You can include a .inc file almost anywhere in a .pas file putting a `$I` compiler directive as long as the code doesn't break, for example you could include it under your `interface` or `implementation` section like this.

```delphi
...
implementation

uses
  SysUtils;

{$I 'bin\test.inc'}
...
```

Or also, include this inside a function like this.

```delphi
...
procedure GenerateOutputFile;
{$I 'bin\test.inc'}
var
  I: Integer;
  S: String;
begin
...
```

## Using a .inc constant
You can access the constant as you wish, but you can move it back to a memory stream for example in this way:

```delphi
var MStream := TMemoryStream.Create;
MStream.Write(BIN_FILE_BIN[0], SizeOf(BIN_FILE_BIN));
```

## Run the test
Firstly build (but don't run)  Bin2Inc.dpr "bin" folder, then execute:

```
bin2inc -i:commedia.txt -o:test.inc -y
```

Now, build IncTest.dpr in the "bin" folder too and just run it from the folder, it will compare content of original file with that in the include file.

## Version history
| Version | Release date | Description |
|---------|--------------|-------------|
| 1.0 | 2002 | First version created in early '00s, released Sep 24, 2023 on GitHub. |
| 1.1 | 2023-09-27 | Fixed an old slow implementation and corrected small things. |
| 1.1 | 2023-10-20 | Added a sample project showing how to use this. |
| 1.2 | 2023-10-28 | Added the -f option. |