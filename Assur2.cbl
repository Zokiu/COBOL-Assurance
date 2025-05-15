       IDENTIFICATION DIVISION.
       PROGRAM-ID. Assur2.
       AUTHOR.    Thomas-Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-ASSURANCE ASSIGN
                           TO "assurances-68259db4e2e6f768575516.csv"
                           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-RAPPORT ASSIGN
                           TO "rapport-assurances2.dat"
                           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-ASSURANCE.
       01  F-ASSURANCE           PIC X(121).

       FD FICHIER-RAPPORT.
       01 F-ENTETE-ED            PIC X(118).

       01 F-ASSURANCE-ED         PIC X(121).

       WORKING-STORAGE SECTION.
       
       01  WS-TABLE-ASSURANCE.
           05 WS-ASSURANCE OCCURS 36 TIMES.
               10 WS-STRING      PIC X(121).
        
       77  WS-INDEX              PIC 9(002) VALUE 1.

       77  WS-MAX-TABLE          PIC 9(002) VALUE 36.

       01  WS-UNSTRING OCCURS 36 TIMES.
           05 WS-CODE-CONTRAT    PIC X(08).
           05 WS-NOM-CONTRAT     PIC X(14).
           05 WS-NOM-PRODUIT     PIC X(14).
           05 WS-NOM-CLIENT      PIC X(41).
           05 WS-STATUT-CONTRAT  PIC X(08).
           05 WS-DEBUT-CONTRAT.
               10 WS-DEBUT-ANNEE PIC X(04).
               10 WS-DEBUT-MOIS  PIC X(02).
               10 WS-DEBUT-JOUR  PIC X(02).
           05 WS-FIN-CONTRAT.
               10 WS-FIN-ANNEE   PIC X(04).
               10 WS-FIN-MOIS    PIC X(02).
               10 WS-FIN-JOUR    PIC X(02).
           05 WS-MONTANT         PIC X(09).
           05 WS-DEVISE          PIC X(03).

       77  WS-FIN-FICHIER        PIC X(01) VALUE "N".

       PROCEDURE DIVISION.
       
           PERFORM 0100-READ-START
           THRU    0100-READ-END.

           PERFORM 0200-UNSTRING-START
           THRU    0200-UNSTRING-END.

           PERFORM 0300-OUTPUT-START
           THRU    0300-OUTPUT-END.

           PERFORM 0400-WRITE-START
           THRU    0400-WRITE-END.

           STOP RUN.
    
      ******************************************************************

       0100-READ-START.

           OPEN INPUT FICHIER-ASSURANCE.
           
           PERFORM UNTIL WS-FIN-FICHIER = "Y"
               
               READ FICHIER-ASSURANCE
                   AT END
                       MOVE "Y" TO WS-FIN-FICHIER
                   NOT AT END
                       IF WS-INDEX <= WS-MAX-TABLE
                           MOVE F-ASSURANCE TO
                                   WS-STRING(WS-INDEX)
                           ADD 1 TO WS-INDEX
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE FICHIER-ASSURANCE.

           EXIT.
       0100-READ-END.

       0200-UNSTRING-START.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > WS-MAX-TABLE
               UNSTRING WS-STRING(WS-INDEX)
                   DELIMITED BY "*"
                   INTO
                       WS-CODE-CONTRAT(WS-INDEX)
                       WS-NOM-CONTRAT(WS-INDEX)
                       WS-NOM-PRODUIT(WS-INDEX)
                       WS-NOM-CLIENT(WS-INDEX)
                       WS-STATUT-CONTRAT(WS-INDEX)
                       WS-DEBUT-CONTRAT(WS-INDEX)
                       WS-FIN-CONTRAT(WS-INDEX)
                       WS-MONTANT(WS-INDEX)
                       WS-DEVISE(WS-INDEX)
                DISPLAY WS-UNSTRING(WS-INDEX)
           END-PERFORM.

           EXIT.
       0200-UNSTRING-END.

       0300-OUTPUT-START.
           DISPLAY 
               "Code     "      
               "Contrat        "  WITH NO ADVANCING
               "Produit        "  WITH NO ADVANCING
               "Client                                    "
                                  WITH NO ADVANCING
               "Statut   "        WITH NO ADVANCING
               "Debut    "        WITH NO ADVANCING
               "Fin        "      WITH NO ADVANCING
               "Montant".

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX >= WS-MAX-TABLE
                    IF WS-INDEX = 3 OR WS-INDEX = 7
                    DISPLAY WS-STRING(WS-INDEX)
                    END-IF
           END-PERFORM.

           EXIT.
       0300-OUTPUT-END.

       0400-WRITE-START.

           OPEN OUTPUT FICHIER-RAPPORT.

           STRING "Code     "
               "Contrat       " 
               "Produit        "
               "Client                                   "
               "Statut   "
               "Debut      "
               "Fin         "
               "Montant" 
               INTO F-ENTETE-ED.
           WRITE F-ENTETE-ED.
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > WS-MAX-TABLE
                IF WS-INDEX = 3 OR WS-INDEX = 7
                    STRING WS-CODE-CONTRAT(WS-INDEX)   SPACE
                           WS-NOM-CONTRAT(WS-INDEX)
                           WS-NOM-PRODUIT(WS-INDEX)    SPACE
                           WS-NOM-CLIENT(WS-INDEX)
                           WS-STATUT-CONTRAT(WS-INDEX) SPACE
                           WS-DEBUT-JOUR(WS-INDEX)
                           "/"
                           WS-DEBUT-MOIS(WS-INDEX)
                           "/"
                           WS-DEBUT-ANNEE(WS-INDEX)    SPACE
                           WS-FIN-JOUR(WS-INDEX)
                           "/"
                           WS-FIN-MOIS(WS-INDEX)
                           "/"
                           WS-FIN-ANNEE(WS-INDEX)
                           WS-MONTANT(WS-INDEX)
                           WS-DEVISE(WS-INDEX)
                        INTO F-ASSURANCE-ED
                    WRITE F-ASSURANCE-ED
                END-IF
           END-PERFORM.

           DISPLAY "Fin du traitement - 2 enregistrements exportés".

           CLOSE FICHIER-RAPPORT.

           EXIT.
       0400-WRITE-END.
