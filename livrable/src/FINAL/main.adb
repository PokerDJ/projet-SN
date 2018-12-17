with Ada.Command_line; use Ada.Command_line;
with huffman; use huffman;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure main is

begin

    if Argument_count = 2 then
	if Argument(1) = "compress" then
	    Compress(Argument(2));
	end if;

	if Argument(1) = "uncompress" then
	    Uncompress(Argument(2));
	end if;
    end if;


end main;
