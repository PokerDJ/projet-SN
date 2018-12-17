generic

    file_name : String;

package writer is

    type T_Size_Text is mod 2**(8*4);
    type T_Byte is mod 2**8;
    type T_Code is record
	byte : T_Byte;
	length : Integer;
    end record;


    -- Créer le fichier compressé .hf
    --
    -- Assure:
    --         Le fichier compressé a été créé et est ouvert
    procedure Create_Compressed_File;


    -- Ecrit un octet dans le tampon
    --
    -- Paramètres:
    --         code, définit comme (byte, longueur)
    -- Assure:
    --         L'octet est écrit dans le tampon
    procedure Write_Byte(code : in T_Code);

    -- Ecrit un bit dans le tampon
    --
    -- Paramètres:
    --         i entier, équivalent à code(1 ou 0, 1)
    -- Assure:
    --         Le bit est écrit dans le tampon
    procedure Write_Bit(i : in Integer);

    -- Ecrit un bit dans le tampon
    --
    -- Paramètres:
    --         i entier
    -- Assure:
    --         L'entier est écrit dans le tampon
    procedure Write_Integer(i : in Integer);

    -- Ecrit la taille du texte sur 4 octets
    --
    -- Assure:
    --        La taille du texte a été écrit sur 4 octet dans le fichier
    procedure Write_Text_Size;

    -- Force à écrire le tampon dans le fichier
    --
    -- Assure:
    --        Le tampon a été écrit dans le fichier. Si sa longueur est inférieur à 8, il est completé par des 0 à droite.
    procedure Force_Write_Buffer;

    -- Ferme le fichier compressé .hf
    --
    -- Assure:
    --        Le fichier est clos.
    procedure Close_Compressed_File;

private

    buffer : T_Code;
    text_size : T_Size_Text;

    -- Mettre à jour le buffer
    procedure Update_Buffer(code : in T_Code);

    -- Nettoie le tampon
    procedure Clean_Buffer;

    -- Ecrit le tampon dans le fichier
    procedure Write_Buffer;


end writer;
