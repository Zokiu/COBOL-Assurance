       IDENTIFICATION DIVISION. 
       PROGRAM-ID. assur.
       AUTHOR. ThomasD & Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *Création de l'alias correspondant au fichier 'inventaire.txt' 
       SELECT FICHIER-ASSURANCE ASSIGN 
                       TO "assurances-68259db4e2e6f768575516.csv"

      *Lecture du fichier ligne par ligne
       ORGANIZATION IS LINE SEQUENTIAL.

      *Création de l'alias correspondant au fichier qui va être créé
       SELECT FICHIER-RAPPORT-ASSURANCE 
       ASSIGN TO "rapport-assurances.dat"

      *Ecriture du fichier ligne par ligne
       ORGANIZATION IS LINE SEQUENTIAL.



      *------------------------------DATA------------------------------- 
       DATA DIVISION.
       FILE SECTION.

      *Description des fichiers créés (groupe de variables)

       FD FICHIER-ASSURANCE.
           01 F-LIGNE-ASSURANCE.
               05 F-CODE-CONTRAT   PIC 9(8).
               05 FILLER           PIC X.
               05 F-NOM-CONTRAT    PIC X(14).
               05 FILLER           PIC X.
               05 F-NOM-PRODUIT    PIC X(14).
               05 FILLER           PIC X.
               05 F-NOM-CLIENT     PIC X(41).
               05 FILLER           PIC X.
               05 F-STATUT         PIC X(8).
               05 FILLER           PIC X.
               05 F-DATE-DEBUT.
                10 F-DEBUT-ANNEE   PIC 9(4).
                10 F-DEBUT-MOIS    PIC 9(2).
                10 F-DEBUT-JOUR    PIC 9(2).
               05 FILLER           PIC X.
               05 F-DATE-FIN.
                10 F-FIN-ANNEE     PIC 9(4).
                10 F-FIN-MOIS      PIC 9(2).
                10 F-FIN-JOUR      PIC 9(2).
               05 FILLER           PIC X.
               05 F-MONTANT        PIC 9(7)v9(2).
               05 FILLER           PIC X.
               05 F-DEVISE         PIC X(3).

       FD FICHIER-RAPPORT-ASSURANCE.
           01 F-ENTETE-RAPPORT             PIC X(121).
           01 F-LIGNE-RAPPORT-ASSURANCES.
               05 F-RAPPORT-CODE-CONTRAT   PIC X(10).
      
               05 F-RAPPORT-NOM-CONTRAT    PIC X(15).
      
               05 F-RAPPORT-NOM-PRODUIT    PIC X(15).
      
               05 F-RAPPORT-NOM-CLIENT     PIC X(42).
      
               05 F-RAPPORT-STATUT         PIC X(9).
      
               05 F-RAPPORT-DATE-DEBUT.
                10 F-RAPPORT-DEBUT-ANNEE   PIC X(4).
                10 F-RAPPORT-DEBUT-MOIS    PIC X(3).
                10 F-RAPPORT-DEBUT-JOUR    PIC X(4).
      
               05 F-RAPPORT-DATE-FIN.
                10 F-RAPPORT-FIN-ANNEE     PIC X(4).
                10 F-RAPPORT-FIN-MOIS      PIC X(3).
                10 F-RAPPORT-FIN-JOUR      PIC X(3).
      
               05 F-RAPPORT-MONTANT        PIC 9(7)v9(2).
               
               05 F-RAPPORT-DEVISE         PIC X(3).
       
       WORKING-STORAGE SECTION.
      
      *Création du tableau dans lequel on va stocker les données lues
      *pour les afficher

       01 WS-TABLEAU-ASSURANCE OCCURS 36 TIMES.

      
         05 WS-CODE-CONTRAT      PIC 9(8).
         05 FILLER               PIC X(8).
         05 WS-NOM-CONTRAT       PIC X(14).
         05 FILLER               PIC X.
         05 WS-NOM-PRODUIT       PIC X(14).
         05 FILLER               PIC X(2).
         05 WS-NOM-CLIENT        PIC X(41).
         05 FILLER               PIC X.
         05 WS-STATUT            PIC X(8).
         05 FILLER               PIC X.
         05 WS-DATE-DEBUT.
           10 WS-DEBUT-ANNEE     PIC 9(4).
           10 FILLER             PIC X         VALUE "/". 
           10 WS-DEBUT-MOIS      PIC 9(2).
           10 FILLER             PIC X         VALUE "/".
           10 WS-DEBUT-JOUR      PIC 9(2).
         05 FILLER               PIC X.
         05 WS-DATE-FIN.
           10 WS-FIN-ANNEE       PIC 9(4).
           10 FILLER             PIC X         VALUE "/".
           10 WS-FIN-MOIS        PIC 9(2).
           10 FILLER             PIC X         VALUE "/".
           10 WS-FIN-JOUR        PIC 9(2).
         05 FILLER               PIC X.
         05 WS-MONTANT           PIC 9(7)v9(2).
         05 FILLER               PIC X.
         05 WS-DEVISE            PIC X(4).

       
      *Création de l'index pour le tableau et d'une variable indiquant
      *la valeur maximale que celui-ci peut prendre  

       77  WS-INDEX-TABLEAU  PIC 9(2)    VALUE 1.

       77  WS-MAX-TABLEAU    PIC 9(2)    VALUE 36. 


      *Création d'une variable permettant la sortie de boucle à la fin 
      *lecture du fichier 

       01  WS-FIN-LECTURE    PIC X       VALUE "N".  


       PROCEDURE DIVISION.


       PERFORM 0100-LECTURE-DEBUT 
          THRU 0100-LECTURE-FIN.


       PERFORM 0100-AFFICHE-DEBUT 
          THRU 0100-AFFICHE-FIN.



       PERFORM 0100-ECRITURE-DEBUT
          THRU 0100-ECRITURE-FIN.

       STOP RUN.






      ******************************************************************

       0100-LECTURE-DEBUT .

      *Ouverture du fichier txt pour la lecture
       DISPLAY "Ouverture du fichier :"
       OPEN INPUT FICHIER-ASSURANCE.

      *Boucle pour lire le fichier ligne par ligne jusqu'à la dernière
       DISPLAY "Lecture du fichier ligne par ligne :"
       PERFORM UNTIL WS-FIN-LECTURE = "Y"
           
           READ FICHIER-ASSURANCE

      *On termine la boucle à la fin de lecture du fichier 
             AT END 
               MOVE "Y" TO WS-FIN-LECTURE

      *On ajoute les données de chaque ligne 
      *au tableau créé pour l'affichage et on incrémente l'index 

             NOT AT END
               IF WS-INDEX-TABLEAU <= WS-MAX-TABLEAU 

                   MOVE F-CODE-CONTRAT 
                    TO  WS-CODE-CONTRAT(WS-INDEX-TABLEAU)

                   MOVE F-NOM-CONTRAT
                    TO  WS-NOM-CONTRAT(WS-INDEX-TABLEAU) 
                   
                   MOVE F-NOM-PRODUIT
                    TO  WS-NOM-PRODUIT(WS-INDEX-TABLEAU) 

                   MOVE F-NOM-CLIENT
                    TO  WS-NOM-CLIENT(WS-INDEX-TABLEAU)

                   MOVE F-STATUT
                    TO  WS-STATUT(WS-INDEX-TABLEAU) 

                   MOVE F-DEBUT-ANNEE
                    TO  WS-DEBUT-ANNEE(WS-INDEX-TABLEAU) 

                   MOVE F-DEBUT-MOIS
                    TO  WS-DEBUT-MOIS(WS-INDEX-TABLEAU)

                   MOVE F-DEBUT-JOUR
                    TO  WS-DEBUT-JOUR(WS-INDEX-TABLEAU)        

                   MOVE F-FIN-ANNEE
                    TO  WS-FIN-ANNEE(WS-INDEX-TABLEAU)

                   MOVE F-FIN-MOIS
                    TO  WS-FIN-MOIS(WS-INDEX-TABLEAU)

                   MOVE F-FIN-JOUR
                    TO  WS-FIN-JOUR(WS-INDEX-TABLEAU) 

                   MOVE F-MONTANT
                    TO  WS-MONTANT(WS-INDEX-TABLEAU)

                   MOVE F-DEVISE
                    TO  WS-DEVISE(WS-INDEX-TABLEAU)   

                   ADD 1 TO WS-INDEX-TABLEAU 

               END-IF

           END-READ 

       END-PERFORM.

      *Fermeture du fichier  
       CLOSE FICHIER-ASSURANCE.



       0100-LECTURE-FIN .
       EXIT.


      *-----------------------------------------------

       0100-AFFICHE-DEBUT .

      *Affichage du tableau avec les données du fichier lu 

      

       DISPLAY "Code contrat "
       WITH NO ADVANCING " Nom contrat  "
       WITH NO ADVANCING "    Nom produit  "
       WITH NO ADVANCING "   Nom client  "  
       WITH NO ADVANCING "                               Statut "
       WITH NO ADVANCING " Date debut "
       WITH NO ADVANCING " Date fin "
       WITH NO ADVANCING "   Montant ".

       
       PERFORM VARYING WS-INDEX-TABLEAU FROM 1 BY 1 
                                UNTIL WS-INDEX-TABLEAU > WS-MAX-TABLEAU
           
           IF WS-INDEX-TABLEAU = 3 OR 7 

      
               
               DISPLAY WS-TABLEAU-ASSURANCE(WS-INDEX-TABLEAU)            
     
           END-IF 

           

       END-PERFORM.
       
       DISPLAY "Fin de lecture".  
       

       
       0100-AFFICHE-FIN .
       EXIT.


      *-----------------------------------------------
      
       0100-ECRITURE-DEBUT .

      *Ouverture du fichier dans lequel on va écrire
       DISPLAY "Ouverture du fichier".
       OPEN OUTPUT FICHIER-RAPPORT-ASSURANCE.

      

      *Ecriture de l'en-tête pour le fichier de sortie 
       STRING "Code      "
               "Contrat        " 
               "Produit        "
               "Client                                    "
               "Statut   "
               "Debut      "
               "Fin         "
               "Montant" 
       INTO F-ENTETE-RAPPORT.
           WRITE F-ENTETE-RAPPORT.


      *Extraction des enregistrements 3 et 7 et écriture du fichier

       DISPLAY "Ecriture du fichier ligne par ligne".

       PERFORM VARYING WS-INDEX-TABLEAU FROM 1 BY 1 
               UNTIL   WS-INDEX-TABLEAU > WS-MAX-TABLEAU

           IF WS-INDEX-TABLEAU = 3 OR 7
               MOVE WS-CODE-CONTRAT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-CODE-CONTRAT

               MOVE WS-NOM-CONTRAT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-NOM-CONTRAT 

               MOVE WS-NOM-PRODUIT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-NOM-PRODUIT

               MOVE WS-NOM-CLIENT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-NOM-CLIENT

               MOVE WS-STATUT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-STATUT

               MOVE WS-DATE-DEBUT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-DATE-DEBUT

               MOVE WS-DATE-FIN(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-DATE-FIN

               MOVE WS-MONTANT(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-MONTANT

               MOVE WS-DEVISE(WS-INDEX-TABLEAU) 
                TO  F-RAPPORT-DEVISE

               WRITE F-LIGNE-RAPPORT-ASSURANCES

           END-IF

       END-PERFORM.        

       DISPLAY "Fin du traitement - 2 enregistrements exportés".

      *Fermeture du fichier dans lequel on a écrit

       DISPLAY "Fin d'ecriture".
       CLOSE FICHIER-RAPPORT-ASSURANCE.  


       0100-ECRITURE-FIN .
       EXIT.
