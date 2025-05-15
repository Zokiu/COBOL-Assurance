       IDENTIFICATION DIVISION.
       PROGRAM-ID. Assur.
       AUTHOR.    Terry.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-ASSURANCE ASSIGN 
                           TO "assurances-68259db4e2e6f768575516.csv"
                           ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT FICHIER-RAPPORT ASSIGN
                           TO "rapport-assurances.dat"
                           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-ASSURANCE.
       01  F-ASSURANCE.
           05 F-CODE-CONTRAT             PIC 9(08).
           05 FILLER                     PIC X.
           05 F-NOM-CONTRAT              PIC X(14).
           05 FILLER                     PIC X.
           05 F-NOM-PRODUIT              PIC X(14).
           05 FILLER                     PIC X.
           05 F-NOM-CLIENT               PIC X(41).
           05 FILLER                     PIC X.
           05 F-STATUT-CONTRAT           PIC X(08).
           05 FILLER                     PIC X.
           05 F-DEBUT-CONTRAT.
               10 F-DEBUT-ANNEE          PIC 9(04).
               10 F-DEBUT-MOIS           PIC 9(02).
               10 F-DEBUT-JOUR           PIC 9(02).
           05 FILLER                     PIC X.
           05 F-FIN-CONTRAT.
               10 F-FIN-ANNEE            PIC 9(04).
               10 F-FIN-MOIS             PIC 9(02).
               10 F-FIN-JOUR             PIC 9(02).
           05 FILLER                     PIC X.
           05 F-MONTANT                  PIC 9(07)v9(02).
           05 FILLER                     PIC X.
           05 F-DEVISE                   PIC X(03).

       FD FICHIER-RAPPORT.
       01 F-ENTETE-ED                    PIC X(112).

       01 F-ASSURANCE-ED.
               05 F-CODE-CONTRAT-ED      PIC 9(08).
               05 FILLER                 PIC X.
               05 F-NOM-CONTRAT-ED       PIC X(14).
               05 F-NOM-PRODUIT-ED       PIC X(14).
               05 FILLER                 PIC X.
               05 F-NOM-CLIENT-ED        PIC X(41).
               05 F-STATUT-CONTRAT-ED    PIC X(08).
               05 F-DEBUT-CONTRAT-ED.
                    15 F-DEBUT-JOUR-ED   PIC 9(02).
                    15 FILLER            PIC X VALUE "/".
                    15 F-DEBUT-MOIS-ED   PIC 9(02).
                    15 FILLER            PIC X VALUE "/".
                    15 F-DEBUT-ANNEE-ED  PIC 9(02).
               05 F-FIN-CONTRAT-ED.
                    15 F-FIN-JOUR-ED     PIC 9(02).
                    15 FILLER            PIC X VALUE "/".
                    15 F-FIN-MOIS-ED     PIC 9(02).
                    15 FILLER            PIC X VALUE "/".
                    15 F-FIN-ANNEE-ED    PIC 9(04).
               05 F-MONTANT-ED           PIC 9(07)v9(02).
               05 F-DEVISE-ED            PIC X(03).

       WORKING-STORAGE SECTION.
       01  WS-TABLE-ASSURANCE.
           05 WS-ASSURANCE OCCURS 36 TIMES.
               10 FILLER                 PIC X(02).
               10 WS-CODE-CONTRAT        PIC 9(08).
               10 FILLER                 PIC X(05).
               10 WS-NOM-CONTRAT         PIC X(14).
               10 FILLER                 PIC X(01).
               10 WS-NOM-PRODUIT         PIC X(14).
               10 FILLER                 PIC X(05).
               10 WS-NOM-CLIENT          PIC X(41).
               10 FILLER                 PIC X(01).
               10 WS-STATUT-CONTRAT      PIC X(08).
               10 FILLER                 PIC X(10).
               10 WS-DEBUT-CONTRAT.
                   15 WS-DEBUT-JOUR      PIC 9(02).
                   15 FILLER             PIC X VALUE "/".
                   15 WS-DEBUT-MOIS      PIC 9(02).
                   15 FILLER             PIC X VALUE "/".
                   15 WS-DEBUT-ANNEE     PIC 9(04).
               10 FILLER                 PIC X(18).
               10 WS-FIN-CONTRAT.
                   15 WS-FIN-JOUR        PIC 9(02).
                   15 FILLER             PIC X VALUE "/".
                   15 WS-FIN-MOIS        PIC 9(02).
                   15 FILLER             PIC X VALUE "/".
                   15 WS-FIN-ANNEE       PIC 9(04).
               10 FILLER                 PIC X.
               10 WS-MONTANT             PIC 9(07)v9(02).
               10 FILLER                 PIC X(01).
               10 WS-DEVISE              PIC X(03).
               10 FILLER                 PIC X VALUE X"0A".

       77  WS-INDEX                      PIC 9(02) VALUE 1.
       77  WS-MAX-TABLE                  PIC 9(02) VALUE 36.

       77  WS-FIN-FICHIER                PIC X VALUE "N".


       PROCEDURE DIVISION.
       
           PERFORM 0100-READ-START
           THRU    0100-READ-END.

           PERFORM 0200-OUTPUT-START
           THRU    0200-OUTPUT-END.

           PERFORM 0300-WRITE-START
           THRU    0300-WRITE-END.

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
                           MOVE F-CODE-CONTRAT      TO
                                    WS-CODE-CONTRAT(WS-INDEX)
                           MOVE F-NOM-CONTRAT       TO
                                    WS-NOM-CONTRAT(WS-INDEX)
                           MOVE F-NOM-PRODUIT       TO
                                    WS-NOM-PRODUIT(WS-INDEX)
                           MOVE F-NOM-CLIENT        TO
                                    WS-NOM-CLIENT(WS-INDEX)
                           MOVE F-STATUT-CONTRAT    TO
                                    WS-STATUT-CONTRAT(WS-INDEX)
                           MOVE F-DEBUT-JOUR        TO
                                    WS-DEBUT-JOUR(WS-INDEX)
                           MOVE F-DEBUT-MOIS        TO
                                    WS-DEBUT-MOIS(WS-INDEX)
                           MOVE F-DEBUT-ANNEE       TO
                                    WS-DEBUT-ANNEE(WS-INDEX)
                           MOVE F-FIN-JOUR          TO
                                    WS-FIN-JOUR(WS-INDEX)
                           MOVE F-FIN-MOIS          TO
                                    WS-FIN-MOIS(WS-INDEX)
                           MOVE F-FIN-ANNEE         TO
                                    WS-FIN-ANNEE(WS-INDEX)
                           MOVE F-MONTANT           TO
                                    WS-MONTANT(WS-INDEX)
                           MOVE F-DEVISE            TO
                                    WS-DEVISE(WS-INDEX)
                           ADD 1 TO WS-INDEX
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE FICHIER-ASSURANCE.

           EXIT.
       0100-READ-END.

       0200-OUTPUT-START.
           DISPLAY 
               "Code Contrat "      
               "Nom de Contrat   "  WITH NO ADVANCING
               "Nom de produit   "  WITH NO ADVANCING
               "Nom de client   "   WITH NO ADVANCING
               "Statut du contrat " WITH NO ADVANCING
               "Date de debut "     WITH NO ADVANCING
               "Date de fin "       WITH NO ADVANCING
               "Montant".

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX >= WS-MAX-TABLE
                    IF WS-INDEX = 3 OR WS-INDEX = 7
                    DISPLAY WS-ASSURANCE(WS-INDEX)
                    END-IF
           END-PERFORM.

           EXIT.
       0200-OUTPUT-END.

       0300-WRITE-START.
           
           OPEN OUTPUT FICHIER-RAPPORT.

           STRING "Code    "      
               "Contrat       " 
               "Produit       "
               "Client                                 "
               "Statut  "
               "Debut      "  
               "Fin        "    
               "Montant" INTO F-ENTETE-ED.
           WRITE F-ENTETE-ED.

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX >= WS-MAX-TABLE
                    IF WS-INDEX = 3 OR WS-INDEX = 7
                        MOVE WS-CODE-CONTRAT(WS-INDEX)     TO
                                F-CODE-CONTRAT-ED
                        MOVE WS-NOM-CONTRAT(WS-INDEX)      TO
                                F-NOM-CONTRAT-ED
                        MOVE WS-NOM-PRODUIT(WS-INDEX)      TO
                                F-NOM-PRODUIT-ED
                        MOVE WS-NOM-CLIENT(WS-INDEX)       TO
                                F-NOM-CLIENT-ED
                        MOVE WS-STATUT-CONTRAT(WS-INDEX)   TO
                                F-CODE-CONTRAT-ED
                        MOVE WS-DEBUT-JOUR(WS-INDEX)       TO
                                F-DEBUT-JOUR-ED
                        MOVE WS-DEBUT-MOIS(WS-INDEX)       TO
                                F-DEBUT-MOIS-ED
                        MOVE WS-DEBUT-ANNEE(WS-INDEX)      TO
                                F-DEBUT-ANNEE-ED
                        MOVE WS-FIN-JOUR(WS-INDEX)         TO
                                F-FIN-JOUR-ED
                        MOVE WS-FIN-MOIS(WS-INDEX)         TO
                                F-FIN-MOIS-ED
                        MOVE WS-FIN-ANNEE(WS-INDEX)        TO
                                F-FIN-ANNEE-ED
                        MOVE WS-MONTANT(WS-INDEX)          TO
                                F-MONTANT-ED
                        MOVE WS-DEVISE(WS-INDEX)           TO
                                F-DEVISE-ED
                        WRITE F-ASSURANCE-ED
                    END-IF
           END-PERFORM.

           DISPLAY "Fin de traitement - 2 enregistrements exportés".

           CLOSE FICHIER-RAPPORT.

           EXIT.
       0300-WRITE-END.
