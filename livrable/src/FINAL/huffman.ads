package huffman is


    -- Compresse un fichier .txt par la méthode d'Huffman
    --
    -- Assure:
    --        Le fichier est compressé dans un fichier [nom_du_fichier].txt.hf
    procedure Compress(file_name : in String);

    -- Décompresse un fichier .txt.hf par la méthode d'Huffman
    --
    -- Assure:
    --        Le fichier est décompressé dans un fichier [nom_du_fichier].txt
    procedure Uncompress(file_name : in String);


end huffman;
