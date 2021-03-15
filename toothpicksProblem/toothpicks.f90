! Code for Buffon's Needle
! Estimate PI by dropping toothpicks across an area with parallel verti
!cal lines

       PROGRAM Toothpicks
       
              ! declare variables
              REAL :: e1X, e1Y, e2X, e2Y, length
              INTEGER :: boxSize, tileSize, numToothpicks, line
              REAL :: theta, pi, count1
       
              !INTEGER :: iostat, error
              !CHARACTER :: exit
       
              ! random initialization
              INTEGER :: count = 0
              CALL SYSTEM_CLOCK(count)
              CALL SRAND(60*count)
              count1 = 1
       
              ! drop toothpicks
              boxSize = 120
              tileSize = 30
              length = 7   
              
              numToothpicks = 10000
              DO i = 1, numToothpicks
                     theta = (90*rand())     
                     theta = theta*(3.14/180)
                                          
             ! rotation angle
                                            
                     e1X = (rand() * boxSize)            
             ! starting position x 
                     e1Y = (rand() * boxSize)            
             ! starting position y
                     e2X = e1X + (length * cos(theta))                         
             ! find endpoint of toothpick in the x-axis
                     e2Y = e1Y + (length * sin(theta))                          
             ! find endpoint of toothpick in the y-axis
             !WRITE(*,*) "points", e1X, e1Y, e2X, e2Y
                     DO j = 1, (boxSize / (tileSize))
                            line = j * 30
                            IF(((line.gt.e1X).and.(line.lt.e2X)).or.((line.lt.e1X).and.(line.gt.e2X))) THEN
                                   !crosses a line in the x-axis
                                   count1 = count1 + 1
                    
                            ENDIF
                            IF(((line.gt.e1Y).and.(line.lt.e2Y)).or.((line.lt.e1Y).and.(line.gt.e2Y))) THEN
                                   !crosses a line in the y-axis
                                   count1 = count1 + 1
           
                            ENDIF
                     ENDDO
              ENDDO
              WRITE(*,*) length, tileSize, count1, numToothpicks
              pi = (4.0 * length) / (tileSize * (count1 / REAL(numToothpicks)))
              WRITE(*,*) "count1 ", count1, "pi: ", pi
       
              !READ(*,*) exit
              END PROGRAM Toothpicks