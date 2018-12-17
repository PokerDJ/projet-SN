generic

    file_name : String;

package reader is

    type T_Byte is mod 2**8;
    type T_Code is record
	byte : T_Byte;
	length : Integer;
    end record;

    -- Ouvre le fichier compressé .hf en simple lecture
    --
    -- Assure:
    --        Le fichier .hf est ouvert
    procedure Open_Compressed_File;


    -- Lit la taille du texte sur les 4 premiers octets
    --
    -- Renvoie:
    --         Text_Size : Entier
    --
    -- Assure:
    --         Le curseur est égale à 4
    function Read_Text_Size return Integer;

    -- Lit la taille de la table d'Huffman sur 1 octet
    --
    -- Renvoie:
    --         Tab_Huffman_Size : Entier
    --
    -- Assure:
    --          Le curseur est égale à 5
    function Read_Tab_Huffman_Size return Integer;

    -- Lit un octet et le place dans le tampon de lecture
    --
    -- Assure:
    --         L'octet en entier est dans le tampon et le curseur a avancé de 1
    procedure Read_Octet;


    -- Lit le premier bit en partant de la gauche sur le tampon
    --
    -- Renvoie:
    --         bit : Entier (0 ou 1)
    -- Assure:
    --        Le tampon est décalé vers la gauche, complété par des 0 à droite
    function Read_Bit return Integer;


    -- Avoir le tampon
    --
    -- Renvoie:
    --          buffer : T_Byte
    function Get_Buffer return T_Byte;



private

    buffer : T_Byte;

    procedure Update_Buffer(octet : in T_Byte);

    procedure Clean_Buffer;

end reader;
