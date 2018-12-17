with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Sequential_IO;
with Ada.IO_Exceptions;
with tree; use tree;
with writer;
with reader;
with tableaux;

package body huffman is


    procedure Compress(file_name : in String) is

	package Writer_Compress is
		new writer(file_name);
	use Writer_Compress;

	type T_Cell is record
	    char : Character;
	    code : T_Code;
	end record;

	package Tab_Leaf is
		new tableaux(T_Tree, 127);
	use Tab_Leaf;

	package Tab_Cell is
		new tableaux(T_Cell, 127);
	use Tab_Cell;

	package Mod_IO is
		new Ada.Text_IO.Modular_IO(T_Byte);
	use Mod_IO;

	function Get_Code(Tab : in Tab_Cell.T_Tableau; Char : in Character) return T_Code is
	    i : Integer;
	    find : Boolean;
	begin
	    i := 1;
	    find := false;
	    while i <= Get_Size(Tab) and not find loop
		if Get_Element(Tab, i).char = Char then
		    find := true;
		else
		    i := i + 1;
		end if;
	    end loop;
	    return Get_Element(Tab, i).code;
	end;


	procedure Display_Octet(code: in T_Code) is
	    octet, bit : t_byte;
	begin
	    octet := code.byte;
	    for i in 1..8 loop
		bit := octet / 2**7;
		if i > (8 - code.length) then
		    Mod_IO.put(bit,1);
		end if;
		octet := octet * 2;
	    end loop;
	    New_Line;
	end;


	function Create_Tab_Frequency(file_name : in String) return Tab_Leaf.T_Tableau is

	    procedure Setup_Tab_Frequency(Tab : out Tab_Leaf.T_Tableau) is
		Leaf : T_Tree;
	    begin
		Initialize(Tab);
		for i in 1 .. 127 loop
		    Leaf := Create_Leaf(Character'Val(i));
		    Add(Tab, Leaf);
		end loop;
	    end;

	    procedure Analyze_File(file_name : in String; Tab : in out Tab_Leaf.T_Tableau) is
		file : Ada.Text_IO.file_type;
		c : Character;
		Leaf : T_Tree;
	    begin
		open(file, In_File, file_name);
		loop
		    if not end_of_file(file) then
			Get(file, c);
			Leaf := Get_Element(Tab, Character'Pos(c));
			Add_Occurence(Leaf);
		    end if;
		    if End_Of_Line(file) then
			Leaf := Get_Element(Tab, 10);
			Add_Occurence(Leaf);
		    end if;
		    exit when end_of_file(file);
		end loop;
		close(file);
	    end;

	    Tab : Tab_Leaf.T_Tableau;
	begin
	    Setup_Tab_Frequency(Tab);
	    Analyze_File(file_name, Tab);
	    return Tab;
	end;


	function Build_Tree_Huffman(Tab : in out Tab_Leaf.T_Tableau) return T_Tree is

	    function Is_Inf(a, b : T_Tree) return Boolean is
	    begin
		return Get_Occurence(a) < Get_Occurence(b);
	    end;

	    procedure Quick_Sort is new Tab_Leaf.Sort(Compare => Is_Inf);

	    procedure Clean_Tab(Tab : in out Tab_Leaf.T_Tableau) is
		Element : T_Tree;
	    begin
		loop
		    Element := Get_Element(Tab, 1);
		    if Get_Occurence(Element) = 0 then
			Delete(Tab, 1);
		    end if;
		    exit when Get_Occurence(Element) /= 0;
		end loop;
	    end;


	    a, b, Node : T_Tree;
	begin
	    Quick_Sort(Tab);
	    Clean_Tab(Tab);
	    loop
		a := Get_Element(Tab, 1);
		b := Get_Element(Tab, 2);
		Delete(Tab, 1);
		Delete(Tab, 1);
		Node := Bind(a, b);
		Add(Tab, Node);
		Quick_Sort(Tab);
		exit when Get_Size(Tab) = 1;
	    end loop;
	    return Get_Element(Tab, 1);
	end;


	function Build_Tab_Huffman(Tree : in T_Tree) return Tab_Cell.T_Tableau is

	    procedure Tab_Recur(Tree : in T_Tree; code : in out T_Code; Tab : out Tab_Cell.T_Tableau) is
		Cell : T_Cell;
		Save : T_Byte;
	    begin
		if not Is_Node(Tree) then
		    if code.length > 8 then
			code.length := 8;
		    end if;
		    Cell.char := Get_Character(Tree);
		    Cell.code := code;
		    Add(Tab, Cell);
		else
		    code.length := code.length + 1;
		    save := code.byte;
		    code.byte := (code.byte * 2) + 0 ;
		    Tab_Recur(Get_Children_Left(Tree), code, Tab);
		    code.byte := save;
		    code.byte := (code.byte * 2) + 1 ;
		    Tab_Recur(Get_Children_Right(Tree), code, Tab);
		    code.length := code.length - 1;
		end if;
	    end;

	    code : T_Code := (0, 0);
	    Tab : Tab_Cell.T_Tableau;
	begin
	    Initialize(Tab);
	    Tab_Recur(Tree, code, Tab);
	    return Tab;
	end;


	procedure Display_Tree(Tree : in T_Tree) is

	    procedure Display_Tree_Recur(Tree : in T_Tree; n : in out Integer) is
	    begin
		if not Is_Node(Tree) then
		    Put('(');
		    Put(Get_Occurence(Tree), 1);
		    Put(") ");
		    Put(''');
		    Put(Get_Character(Tree));
		    Put(''');
		else
		    Put('(');
		    Put(Get_Occurence(Tree), 1);
		    Put(") ");
		    New_Line;
		    Put(n * "        ");
		    n := n + 1;
		    Put("\--");
		    Put("0");
		    Put("-- ");
		    Display_Tree_Recur(Get_Children_Left(Tree), n);
		    New_Line;
		    n := n - 1;
		    Put(n * "        ");
		    n := n + 1;
		    Put("\--");
		    Put("1");
		    Put("-- ");
		    Display_Tree_Recur(Get_Children_Right(Tree), n);
		    n := n - 1;
		end if;
	    end;

	    n : Integer := 0;
	begin
	    Put_Line("============[ Version textuelle de l'arbre d'Huffman ]============");
	    New_Line;
	    Display_Tree_Recur(Tree, n);
	    New_Line(2);
	    Put_Line("===================================================================");
	end;


	procedure Display_Tab(Tab : in Tab_Cell.T_Tableau) is
	    Cell : T_Cell;
	begin
	    Put_Line("============[ Version textuelle de la table de Huffman ]============");
	    New_Line;
	    for i in 1 .. Get_Size(Tab) loop
		Cell := Get_Element(Tab, i);
		Put(''' & Cell.char & ''');
		Put(" -> ");
		Display_Octet(Cell.code);
	    end loop;

	    New_Line;
	    Put_Line("===================================================================");
	end;


	procedure Compress_file(file_name : in String ; Tree : in T_Tree ; Tab_Huffman : in Tab_Cell.T_Tableau) is

	    procedure Write_Tab_Size(Tab : in Tab_Cell.T_Tableau) is
	    begin
		Write_Integer(Get_Size(Tab));
	    end;

	    procedure Write_Char_Tree(Tab : in Tab_Cell.T_Tableau) is
		element : T_Cell;
	    begin
		for i in 1..Get_Size(Tab) loop
		    element := Get_Element(Tab, i);
		    Write_Integer(Character'Pos(element.char));
		end loop;
	    end;

	    procedure Write_Description_Tree(Tree : in T_Tree ; Tab : in Tab_Cell.T_Tableau) is

		procedure Write_Description_Tree_Recur(Tree : in T_Tree ; n : in out Integer ; max : in Integer) is
		begin
		    if not Is_Node(Tree) then
			n := n + 1;
		    else
			Write_Bit(0);
			Write_Description_Tree_Recur(Get_Children_Left(Tree), n, max);
			Write_Bit(1);
			Write_Bit(0);
			Write_Description_Tree_Recur(Get_Children_Right(Tree), n, max);
			if n < max then
			    Write_Bit(1);
			end if;
		    end if;
		end;

		n : Integer := 0;
		max : Integer := Get_Size(Tab);
	    begin
		Write_Description_Tree_Recur(Tree, n, max);
		Write_Bit(1);
		Force_Write_Buffer;
	    end;

	    procedure Write_Text(file_name : in String) is
		file : Ada.Text_IO.file_type;
		c : Character;
	    begin
		open(file, In_File, file_name);
		loop
		    if not End_Of_File(file) then
			Get(file, c);
			Write_Byte(Get_Code(Tab_Huffman, c));
		    end if;
		    if End_Of_Line(file) then
			Write_Byte(Get_Code(Tab_Huffman, Character'Val(10)));
		    end if;
		    exit when end_of_file(file);
		end loop;
		close(file);
		Force_Write_Buffer;
	    end;



	begin

	    -- Créer le nouveau fichier .txt.hf
	    Create_Compressed_File;

	    -- On écrit la taille du texte sur 4 octets
	    Write_Text_Size;

	    -- On écrit la taille de l'arbre sur 1 octet
	    Write_Tab_Size(Tab_Huffman);

	    -- On écrit chacun des caractère de l'arbre (parcours infixe, en profondeur) pour 1 octet par charactère
	    Write_Char_Tree(Tab_Huffman);

	    -- On écrit la description de l'arbre
	    Write_Description_Tree(Tree, Tab_Huffman);

	    -- On écrit le texte
	    Write_Text(file_name);

	    -- Coder le texte dans le fichier compressé
	    Close_Compressed_File;

	end;


	Tab_Frequency : Tab_Leaf.T_Tableau;
	Tab_Huffman : Tab_Cell.T_Tableau;
	Arbre : T_Tree;
    begin

	-- Création du tableau des occurences
	Tab_Frequency := Create_Tab_Frequency(file_name);

	-- Construction de l'arbre
	Arbre := Build_Tree_Huffman(Tab_Frequency);

	-- Construction de la table de huffman
	Tab_Huffman := Build_Tab_Huffman(Arbre);

	-- Affichage de l'arbre et de la table de huffman
	Display_Tree(Arbre);
	New_Line(2);
	Display_Tab(Tab_Huffman);

	-- Compression du fichier
	Compress_file(file_name, Arbre, Tab_Huffman);

	-- Destruction de l'arbre
	Clean_Up(Arbre);

    exception
	when ADA.IO_EXCEPTIONS.NAME_ERROR =>
	    put_line("Fichier non present - donner le nom d'un fichier en argument");

    end;


    procedure Uncompress(file_name : in String) is

	package Reader_Uncompress is
		new reader(file_name);
	use Reader_Uncompress;

	type T_Cell is record
	    char : Character;
	    code : T_Code;
	end record;

	package Tab_Cell is
		new tableaux(T_Cell, 127);
	use Tab_Cell;

	function Build_Tab_Huffman(size : in Integer) return T_Tableau is

	    type T_Tab_Huffman is array(1..Size) of T_Cell;
	    type position is (RIGHT, LEFT, NODE);


	    tab_huffman : T_Tab_Huffman;
	    element : T_Cell;

	    nb_code, bit_position : Integer := 0;
	    tree_Position : position := LEFT;
	    bit, length : Integer;
	    byte : T_Byte;

	    Tab : T_Tableau;
	    Cell : T_Cell;

	begin

	    for i in 1..Size loop
		Read_Octet;
		element.char := Character'Val(Integer(Get_Buffer));
		element.code := (0, 0);
		tab_huffman(i) := element;
	    end loop;

	    Read_Octet;
	    length := 0;
	    loop
		if bit_position = 8 then
		    Read_Octet;
		    bit_position:= 0 ;
		end if;

		bit_position := bit_position + 1;
		bit := Read_Bit;

		if bit = 0 then
		    length :=  length + 1;

		    if tree_position = NODE then
			tree_position := RIGHT;
		    end if;

		    if tree_position = RIGHT then
			byte := byte * 2 + 1 ;
			tree_position := LEFT;
		    elsif tree_position = LEFT then
			byte := byte * 2 ;
		    end if;

		else

		    if tree_position /= NODE  then
			nb_code := nb_code + 1;
			tab_huffman(nb_code).code := (byte, length);
		    end if;
		    tree_position := NODE;
		    length := length - 1;
		    byte := byte / 2 ;

		end if;

		exit when nb_code = size;
	    end loop;

	    initialize(tab);
	    for i in 1 .. size loop
		Cell := tab_huffman(i);
		Add(Tab, Cell);
	    end loop;
	    return tab;
	end;


	procedure Code_Decryption(Text_Size : in Integer; Tab_Huffman : in T_Tableau; Tab_Huffman_Size : in Integer) is

	    uncompressed_file : Ada.Text_IO.File_Type ;
	    bit, text_rode, length, bit_position : Integer := 0;
	    byte : T_Byte := 0;
	    convert : Boolean := false;
	    cell : T_Cell;
	    char : Character;
	    code : T_Code;
	    j : Integer := 1;

	begin
	    create(uncompressed_file, Out_File, file_name(file_name'First..(file_name'Length - 3))) ;

	    Read_Octet;
	    loop
		length := 0;
		byte := 0;
		loop

		    if bit_position = 8 then
			Read_Octet;
			bit_position:= 0 ;
		    end if;

		    bit := Read_Bit;
		    bit_position := bit_position + 1;
		    byte := byte * 2 + T_Byte(bit);
		    length := length + 1;
		    code := (byte, length);


		    while j <= Tab_Huffman_Size and not convert loop
			cell := Get_Element(Tab_Huffman, j);
			if cell.code.byte = byte and cell.code.length = length then
			    convert := true;
			    char := cell.char;
			end if;
			j := j + 1;
		    end loop;
		    j := 1;

		    exit when convert;
		end loop;
		put(uncompressed_file, char);
		put(char);
		text_rode := text_rode + 1;
		convert := false;
		exit when text_rode = Text_Size;
	    end loop;
	    close(uncompressed_file);

	end;


	Text_Size, Tab_Huffman_Size : Integer;
	Tab_Huffman : Tab_Cell.T_Tableau;

    begin

	-- On ouvre le fichier compressé
	Open_Compressed_File;

	-- On récupère la taille du texte
	Text_Size := Read_Text_Size;

	-- On récupère la taille du tableau
	Tab_Huffman_Size := Read_Tab_Huffman_Size;

	-- On construit la table d'Huffman
	Tab_Huffman := Build_Tab_Huffman(Tab_Huffman_Size);

	-- Décryptage du code + écriture dans le fichier .txt
	Code_Decryption(Text_Size, Tab_Huffman, Tab_Huffman_Size);

    exception
	when ADA.IO_EXCEPTIONS.NAME_ERROR =>
	    put_line("Fichier non present - donner le nom d'un fichier en argument");

    end;


end huffman;
