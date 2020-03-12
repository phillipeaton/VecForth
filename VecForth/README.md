## CHROMIUM_ANS.FTH

Forth .scr files are also know as *screen* files or *block* files and consist of sequential blocks of 1024 characters. Each block can hold Forth source code, editable on a fixed 64x14 character screen. There are no carriage returns or line feeds in the source code. Editing with a block editor is somewhat outdated, so modern Forth environments e.g. Gforth are generally used with plain text source code files. 

Chromium_ans.fth was created by running the original multicomp6809 chromium_ans.scr file through the UNIX dd command:

```
dd if=chromium_ans.scr of=chromium_ans.fth cbs=64 conv=unblock
```

This command adds a line break after every 64 characters to the original .scr file, then it can be edited using a stardard plain text file editor.

In the newly converted state, chromium.fth will not compile, as it contains compilation code that is specific blocks. The file needs splitting into separate source code files, editing to remove the block-specific compilation instructions and to have an include file created to load the separate source code files in the correct order.


