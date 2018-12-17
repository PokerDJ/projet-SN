with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Directories; use Ada.Directories;


package body reader is


    package Bin_IO is
	    new Ada.Sequential_IO(t_byte);
    use Bin_IO;

    package Mod_IO is
	    new Ada.Text_IO.Modular_IO(t_byte);
    use Mod_IO;

    file_compressed : Bin_IO.File_Type;

    procedure Open_Compressed_File is
    begin
	open(file_compressed, In_File, "./" & file_name);
    end;



    procedure Read_Octet is
	octet : T_Byte;
    begin
	Bin_IO.Read(file_compressed, octet);
	Update_Buffer(octet);
    end;


    function Read_Bit return Integer is
	bit : Integer;
    begin
	bit := Integer(buffer) / 2**7;
	buffer := buffer - T_Byte(bit*2**7);
	buffer := buffer * 2;
	return bit;
    end;


    procedure Update_Buffer(octet : T_Byte) is
    begin
	buffer := octet;
    end;


    procedure Clean_Buffer is
    begin
	buffer := 0;
    end;

    function Get_Buffer return T_Byte is
    begin
	return buffer;
    end;



    function Read_Text_Size return Integer is
	bit, Text_Size : Integer := 0;
    begin
	for i in 1..4 loop
	    Read_Octet;
	    for k in 1..8 loop
		bit := Read_Bit;
		Text_Size := Text_Size * 2 + bit;
	    end loop;
	end loop;
	return Text_Size;
    end;



    function Read_Tab_Huffman_Size return Integer is
	Tab_Huffman_Size, bit : Integer := 0;
	power : Integer := 7;
    begin
	Read_Octet;
	for k in 1..8 loop
	    Bit := Read_Bit;
	    Tab_Huffman_Size := Tab_Huffman_Size * 2 + bit;
	end loop;
	return Tab_Huffman_Size;
    end;


end;
