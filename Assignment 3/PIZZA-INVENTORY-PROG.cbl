       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PIZZA-INVENTORY-PROG.
       AUTHOR.     AUSTIN_OGLETREE.
      **********************************************************
      *  This program is made to take a file of two trucks and
      *  print its contents, show the price of the contents,
      *  and total value of the trucks, and print them.
         
      **********************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBMPC.
       OBJECT-COMPUTER.    IBMPC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PR3FA22-TREAT  
               ASSIGN TO 'PR3FA22-TREAT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PIZZA-TRUCK-OUTPUT-FILE 
               ASSIGN TO 'PIZZA-TRUCK-OUTPUT.TXT'.
 
      *
       DATA DIVISION.
       FILE SECTION.


       FD  PR3FA22-TREAT
           RECORD CONTAINS 118 CHARACTERS.
       01  PIZZA-TRUCK-REC.
           05  TRUCK-ID-IN        PIC X(4).
           05  EMPLOYEE-ID-IN     PIC X(5).
           05  EMPLOYEE-TITLE-IN  PIC A(2).
           05  EMPLOYEE-LNAME-IN  PIC X(10).
           05  EMPLOYEE-FNAME-IN  PIC X(10).
           05  HIRE-DATE-IN       PIC 9(8).
           05  FILLER             PIC X(2).
           05  CURRENT-YEARLY-IN  PIC 9(5).
           05  TREAT-ARRAY-IN OCCURS 3 TIMES.
               10 TREAT-NAME-IN PIC X(15).
               10 TREAT-SIZE-IN    PIC X.
               10 NUM-IN-STOCK-IN  PIC 9(4).
               10 SELLING-PRICE-IN PIC 99V99.
      *
       FD    PIZZA-TRUCK-OUTPUT-FILE
             RECORD CONTAINS 80 CHARACTERS.

       01    PIZZA-OUTPUT-REC            PIC X(80).
      *********
       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.

      *  Total Inven Cost holds the total cost for each page.
      *  Cal 1 holds the calculation for this computation.
      *  Grand total holds the value for both trucks, to be printed
      *  last.
      *  Page num holds the page number.
             05    TOTAL-INVEN-COST          PIC 99999999V99     .
             05    CAL1                      PIC 99999999V99     .
             05    GRAND-TOTAL-COST          PIC 99999999V99     .
             05    PAGENUM                   PIC 99     VALUE 01 .

      *   Flags and switches hold the values neccessary for
      *  reading the loop and the array index, respectively.

       01  FLAGS-N-SWITCHES.
           05 FIRST-RECORD                         VALUE 'Y'.
           05 EOF-FLAG                             VALUE 'Y'.
           05  SUB                          PIC 99      VALUE 1.

      *  The hold field is for the control break between both
      *  trucks.
      
       01 HOLD-FIELD.
           05  TRUCK-HOLD                   PIC X(4).
           
           
      *  Current date is to correctly print the date.
      *  Merely for formatting reasons.

       01  CURRENT-DATE.
           05  CD-YEAR             PIC XXXX.
           05  CD-MONTH            PIC XX.
           05  CD-DAY              PIC XX.

      *  Treat array to catch the incoming array.
      *  The purpose of this is to feed into the detail
      *  line, so it prints correctly.

       01  TREAT-ARRAY-OUT OCCURS 3 TIMES.
               
           05 TREAT-NAME-ARRAY    PIC X(15).
           05 TREAT-SIZE-ARRAY    PIC X(1).   
           05 NUM-IN-STOCK-ARRAY  PIC 9(4).
           05 SELLING-PRICE-ARRAY PIC 99V99.
       
       
       

      *************************OUTPUT AREA*****************************

      *  Filler9 is for my write a line function.
      *  All it does is hold one spot to print a line.

       01 FILLER9 PIC X VALUE SPACES.

       01 REPORT-HEADER.
          05  H1-DATE.
               10  H1-MONTH        PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-DAY          PIC XX.
               10  FILLER          PIC X         VALUE '/'.
               10  H1-YEAR         PIC XXXX.

          05 FILLER            PIC X(25) VALUE SPACES.
          05 COMPANY-NAME      PIC X(13) VALUE 'ROLLING PIZZA'.

          05 FILLER            PIC X(20) VALUE SPACES.
          05 PAGENUM-OUT       PIC 9(2) VALUE '00'.
      *
       01 REPORT-HEADER-2.
          05 FILLER          PIC X(30) VALUE SPACES.
          05 REPORT-LINE     PIC X(10) VALUE 'NEW TRUCK '.
          05 REPORT-LINE2    PIC X(13) VALUE 'DETAIL REPORT'.
          

       01 TRUCK-HEADER.
          05 FILLER          PIC X(2) VALUE SPACES.
          05 TRUCK           PIC X(7) VALUE 'TRUCK: '.
          05 TRUCK-OUT        PIC X(10)              .

          
          
       01 TREAT-HEADER.
          05 FILLER           PIC X(5) VALUE SPACES .
          05 TREAT-NAME-TITLE PIC X(10) VALUE 'TREAT NAME'.

          05 FILLER          PIC X(11) VALUE SPACES .
          05 SIZE1           PIC X(4) VALUE 'SIZE'  .

          05 FILLER          PIC X(8) VALUE SPACES  .
          05 STOCK           PIC X(5) VALUE 'STOCK' .

          05 FILLER          PIC X(5) VALUE SPACES  .
          05 PRICE           PIC X(5) VALUE 'PRICE' .

          05 FILLER          PIC X(8) VALUE SPACES   .
          05 COST            PIC X(7) VALUE 'REVENUE'.
          

       01 DETAIL-LINE1.
          
          05 FILLER                 PIC X(3) VALUE SPACES.
          05 TREAT-NAME-OUT         PIC X(15).

          05 FILLER                 PIC X(5) VALUE SPACES.
          05 TREAT-SIZE-OUT         PIC X(10).

          05 FILLER                 PIC X(5) VALUE SPACES.
          05 NUM-IN-STOCK-OUT       PIC Z,ZZ9.

          05 FILLER                 PIC X(5) VALUE SPACES.               
          05 SELLING-PRICE-OUT     PIC $Z9.99  .
          
          05 FILLER                 PIC X(5) VALUE SPACES.     
          05 REVENUE-OUT    PIC $ZZZ,ZZZ.99         .
               

      *  Two total lines are needed to correctly print new
      *  pages, one for the initial total and one for the last.

       01 TOTAL-LINE1.
          05 FILLER              PIC  X(3) VALUE SPACES   .
          05 TOTAL               PIC  X(6)  VALUE 'TOTAL '.
          05 POSSIBLE            PIC  X(9)  VALUE 'POSSIBLE '.
          05 TOTAL-IN-STOCK-OUT  PIC X(9) VALUE 'REVENUE: '.
         
          
          05 TOTAL-OUT       PIC  $Z,ZZZ,ZZZ.99         .


       01 TOTAL-LINE2.
        05 FILLER   PIC X(3) VALUE SPACES.
        05 GRANDTOTAL PIC X(30) VALUE 'GRAND TOTAL POSSIBLE REVENUE: '.
        05 GRAND-TOTAL-OUT PIC $ZZ,ZZZ,ZZZ.99        .
 
       PROCEDURE DIVISION.

      *  House keeping to open files, read file
      *  to read through the file, final total is
      *  to print the final total correctly, and 
      *  the close routine is to close all files.

       100-MAIN-MODULE.

           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-FILE
           PERFORM 225-FINAL-TOTAL
           PERFORM 250-CLOSE-ROUTINE
           

           .

      *  My earlier mentioned write a line function.
      *  When called it only writes a line.

       110-WRITE-LINE.
             MOVE FILLER9 TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
             AFTER ADVANCING 1 LINE
           .

      *  Opens files and sets date.
       125-HOUSEKEEPING.

           OPEN    INPUT     PR3FA22-TREAT
                   OUTPUT    PIZZA-TRUCK-OUTPUT-FILE
           
           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
           MOVE CD-MONTH TO H1-MONTH
           MOVE CD-DAY TO H1-DAY
           MOVE CD-YEAR TO H1-YEAR

      *  This is important for printing the first truck.
      *  Later I execute my print page function.
      *  Without iterating through the
           

           
           
            
                  .
       130-CONTROL-BREAK.

      *  Control break is called by the next function
      *  which reads through the file, which is the 150
      *  function. As it reads the file, it executes this
      *  control break function, which is the master function.

           EVALUATE TRUE
              WHEN FIRST-RECORD = 'Y'
                   MOVE TRUCK-ID-IN TO TRUCK-HOLD
                   PERFORM 165-WRITE-NEW-PAGE
    
      *  This is important for printing the first truck.
      *  Later I execute my print page function.
      *  Without iterating through the array, Truck
      *  hold has no way to determine when to print
      *  Mobile or Montgomery for the write new page
      *  function.

                   MOVE 'N' TO FIRST-RECORD
              WHEN TRUCK-ID-IN NOT EQUAL TO TRUCK-HOLD

      *  When a control break is detected, it writes a
      *  line, writes the truck total, then flushes the
      *  total inven cost variable to zero, to re begin
      *  counting the total for each individiual truck.
      *  
      *  Otherwise, it would just add the first and
      *  second truck to the second truck total,
      *  instead of just the second trucks total.
      *  This is not neccessary for the grand total,
      *  as we want the total of both trucks for that line.

                   PERFORM 110-WRITE-LINE
                   PERFORM 215-TRUCK-TOTAL

      *  215 writes the total for one truck.

                   MOVE '0' TO TOTAL-INVEN-COST
                   PERFORM 200-TRUCK-BREAK

      *  200 Truck Break calls to write a new page when
      *  the second truck is read into memory.

           END-EVALUATE

           PERFORM 110-WRITE-LINE

           MOVE TREAT-HEADER          TO PIZZA-OUTPUT-REC
              WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 1 LINE

           PERFORM 110-WRITE-LINE
    
           PERFORM 175-WRITE-ARRAY
                    
      *  175 is where the data is altered and sent to
      *  output.
           
           
                  .
      
      *  Standard read file, with control break.

       150-READ-FILE.

           PERFORM UNTIL EOF-FLAG = 'N'
               READ PR3FA22-TREAT
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 130-CONTROL-BREAK
               END-READ
           END-PERFORM
          . 

      *  Any time I need to write a new page, this
      *  function does so.
      *  It writes my headers with proper spacing.

       165-WRITE-NEW-PAGE.
           ADD 1 TO PAGENUM-OUT           

           MOVE REPORT-HEADER    TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 1 LINE

           MOVE REPORT-HEADER-2  TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE

           MOVE TRUCK-ID-IN TO TRUCK-OUT


           IF TRUCK-ID-IN EQUAL 'MOBL'
              MOVE 'Mobile' TO TRUCK-OUT
           ELSE MOVE 'Montgomery' TO TRUCK-OUT
            END-IF

             
             MOVE TRUCK-HEADER      TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                   AFTER ADVANCING 2 LINE
           
            
          .

      *  175 does the bulk of the data manipulation.

       175-WRITE-ARRAY.

           
      *  Standard perform varying.

           PERFORM VARYING SUB
           FROM 1 BY 1 UNTIL SUB > 3

      *  Moves the input array into working storage
      *  array.

           MOVE TREAT-ARRAY-IN(SUB) TO TREAT-ARRAY-OUT(SUB)
        
      *  This if statement makes it print only the first name
      *  of each treat.
   
           IF SUB EQUAL 1
               MOVE TREAT-NAME-ARRAY(SUB) TO TREAT-NAME-OUT
           ELSE 
               MOVE SPACES TO TREAT-NAME-OUT
           END-IF

      *  Move treat size array moves the treat size to
      *  the output line.
           
           MOVE TREAT-SIZE-ARRAY(SUB) TO TREAT-SIZE-OUT

      *  The following evaluate makes it such that if
      *  a treat is large, it writes 'LARGE' instead
      *  of just 'L', ect.

                
                EVALUATE TRUE
                   WHEN TREAT-SIZE-ARRAY(SUB) EQUALS 'L'
                   MOVE 'LARGE' TO TREAT-SIZE-OUT

                   WHEN TREAT-SIZE-ARRAY(SUB) EQUALS 'M'
                   MOVE 'MEDIUM' TO TREAT-SIZE-OUT

                   WHEN TREAT-SIZE-ARRAY(SUB) EQUALS 'S'
                   MOVE 'SMALL' TO TREAT-SIZE-OUT

                   WHEN TREAT-SIZE-OUT NOT EQUALS 'LARGE'
                   OR 'SMALL' OR 'SMALL'
                   MOVE 'ERROR' TO TREAT-SIZE-OUT

                   
                END-EVALUATE

      *  If num in stock is not a number, the following
      *  evaluate moves a zero to the output line.
               
                EVALUATE TRUE
                  WHEN NUM-IN-STOCK-ARRAY(SUB) NOT NUMERIC
                  MOVE '0' TO NUM-IN-STOCK-ARRAY(SUB)
               END-EVALUATE

      *  Num in stock array is moved to output line.

           MOVE NUM-IN-STOCK-ARRAY(SUB) TO NUM-IN-STOCK-OUT

      *  This evaluate is the same as the last, validates that
      *  only numbers make it to the output.

               EVALUATE TRUE
                  WHEN SELLING-PRICE-ARRAY(SUB) NOT NUMERIC
                  MOVE '0' TO SELLING-PRICE-ARRAY(SUB)
               END-EVALUATE

      *  Selling price out is printed.

           MOVE SELLING-PRICE-ARRAY(SUB) TO SELLING-PRICE-OUT
           
      *  Cal1 stores the total value of each item in bulk.
      *  So, how much of each size of candy in total that
      *  is on the truck.
           
           COMPUTE CAL1 = NUM-IN-STOCK-ARRAY(SUB) *
                                 SELLING-PRICE-ARRAY(SUB)

      *   Moves the total revenue of each item to the output
      *   line corresponding to the correct candies.

           MOVE CAL1 TO REVENUE-OUT

      *  This makes the totals of each truck work, by adding
      *  to each, as the program iterates, it stores a mounting
      *  total to each truck line. Total inven is flushed
      *  witht the control break to correctly print the second
      *  truck total, while grand is not, as it prints both
      *  trucks' totals as one value.
           
           ADD CAL1 TO TOTAL-INVEN-COST
           
           ADD CAL1 TO GRAND-TOTAL-COST

      *  This makes it so only the first name of each
      *  candy is printed, by overidding the previous statement
      *  to fill indexes one and two with spaces.

           MOVE SPACES TO TREAT-NAME-ARRAY(2)
           MOVE SPACES TO TREAT-NAME-ARRAY(3)

      *  Prints the modified array.
                
           MOVE DETAIL-LINE1          TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
                 AFTER ADVANCING 1 LINE

           

           END-PERFORM
           
           
          

           
           .

      *  The truck break. Also writes a new page.
      *  As the second truck is read, it must
      *  print the second truck.

       200-TRUCK-BREAK. 
           MOVE TRUCK-ID-IN TO TRUCK-HOLD
           PERFORM 165-WRITE-NEW-PAGE
           
           
           .
          
      *  This prints the individual truck total for
      *  the first page.

       215-TRUCK-TOTAL.
           MOVE TOTAL-INVEN-COST TO TOTAL-OUT

           MOVE TOTAL-LINE1 TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
           AFTER ADVANCING 2 LINE
           
           .

      *  Final total prints the first truck and
      *  the grand total of both trucks.
       225-FINAL-TOTAL.
           MOVE TOTAL-INVEN-COST TO TOTAL-OUT

           MOVE TOTAL-LINE1 TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
           AFTER ADVANCING 3 LINE

           MOVE GRAND-TOTAL-COST TO GRAND-TOTAL-OUT

           PERFORM 110-WRITE-LINE

           MOVE TOTAL-LINE2 TO PIZZA-OUTPUT-REC
           WRITE PIZZA-OUTPUT-REC
           AFTER ADVANCING 1 LINE
          .
           
      *  250 Closes the program.

       250-CLOSE-ROUTINE.


              CLOSE    PR3FA22-TREAT
                       PIZZA-TRUCK-OUTPUT-FILE

              STOP RUN
           .


