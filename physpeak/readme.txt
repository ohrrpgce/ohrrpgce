Some notes on the structure of this compiler.

-- hspeak_tld.py

During pass 1 the source code and all its includes are read for global 
definitions. Script headers are recorded as name and number of 
arguments only. Script bodies are skipped.

During pass 2 each script is read again. Part of the state is cleared, 
the script name is set and its arguments are registered as local 
variables. The script body is buffered and sent forward.

-- hspeak_ast.py

The script body is converted to AST.

-- hspeak_post.py

More local variables are defined and the AST is modified to match the 
interpreter's formatting requirements.

-- hspeak_gen.py

The AST is converted to the binary format required by the interpreter. 
In the process, string constants are recorded and a table is prepared.

-- hspeak_hs.py

The binary data is recorded into a temporary folder. The scripts, their 
arguments and default values are recorded. Finally the contents of the 
temporary folder are joined into the ".hs" product.

--

Some personal notes from me, Lenny.

I would like to dedicate this software to the memory of
Terrence Andrew Davis (December 15, 1969 - August 11, 2018)
His legend inspired me to finally become a professional.

I would also like to thank TMC from the slimesalad forums who provided 
unconditional support, appreciation and contributed vital parts of the 
compiler core that would have otherwise taken months for me to figure 
out.

Enjoy.
