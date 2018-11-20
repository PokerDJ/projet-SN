with Ada.Text_IO;                 use Ada.Text_IO;
with Liste_Generique;

procedure main is
   package liste_caractere is new Liste_Generique(Character);
   use liste_caractere;   
   MonFichier : File_type ;
   caractere_lu : Character := 'a'; --initialisation pour que l'algo marche
   caractere_lu_prec : Character;
   colone : integer ;
   ligne : integer:= 0;
   nb_caractere : Integer := 0;
   liste : T_Liste;
   fin_lecture : Boolean := False;
   
   
begin
   Put_Line("go");
   --ouverture fichier
   Open(MonFichier,in_File,"test_lecture_fichier.txt") ;--bug ici
   Put_Line("ouvert");
   --initialisation liste
   Initialiser(liste );
     
   
     --lecture du fichier
   loop
      begin
     
     
   --lecture de element d'une ligne
   colone := 0;
   loop  
      caractere_lu_prec := caractere_lu ;
            Get(MonFichier,caractere_lu);
            Put(caractere_lu);
      colone := colone+1;
      Ajouter_Element(liste,caractere_lu); --ordre de la liste import peu 
      
      exit when caractere_lu_prec = '/' and caractere_lu ='n'; --fin de ligne
   end loop;
         nb_caractere := nb_caractere + colone;
         
   
      Skip_Line(MonFichier,1);
   exception 
         when CONSTRAINT_ERROR => fin_lecture := True;
       
   end;
      exit when fin_lecture;
        end loop;
end main;