MODULE mod_readwrite
    USE mod_variables
    USE, INTRINSIC :: iso_fortran_env, ONLY : error_unit
    IMPLICIT NONE

    CONTAINS

    ! To read values of the variables from input file
    SUBROUTINE read_input(file)
        CHARACTER(LEN=*), INTENT(IN) :: file ! file name
        INTEGER :: funit, io_status
        CHARACTER(LEN=100) :: text
        CHARACTER(LEN=100) :: text1
        CHARACTER(LEN=100) :: text2

        ! opening file in reading mode
        OPEN(NEWUNIT=funit, FILE=file, ACTION='read', &
             IOSTAT=io_status, STATUS='old', POSITION='REWIND')
        IF(io_status/=0) THEN
            WRITE(error_unit,*) 'Unable to open file.'
            STOP 'stopping the execution' ! stops the code execution
        END IF

        DO WHILE(.TRUE.)
            READ(funit,*,IOSTAT=io_status) text ! Reads until the first white space and the cursor moves to the next line
            ! READ(funit,'(A)',IOSTAT=io_status) text ! Reads the entire line and the cursor moves to the next line
            IF (io_status/=0) EXIT ! End of file

            text = TRIM(ADJUSTL(text))
            ! To ignore text with leading ! (considered as comments) 
            ! and to ignore text which does not have = sign, i.e. no assignment.
            IF( text(1:1)=='!' .OR. .NOT.(index(text,'=') > 0) ) THEN 
                CYCLE
            END IF

            ! Splitting text
            text1 = TRIM(ADJUSTL(text(1:index(text,'=')-1)))
            text2 = TRIM(ADJUSTL(text(index(text,'=')+1:))) 
            IF (LEN_TRIM(text1)==0 .OR. LEN_TRIM(text2)==0) THEN
                WRITE(error_unit,*) TRIM(text),' Empty variable name or value';
                CYCLE
            END IF

            ! Assigning values to the variables
            SELECT CASE (trim(adjustl(text1)))
                CASE ('RHO');        READ(text2,*) RHO
                CASE ('MU');         READ(text2,*) MU
                CASE ('P_ATM');      READ(text2,*) P_ATM 
                CASE ('K');          READ(text2,*) K
                CASE ('L');          READ(text2,*) L
                CASE ('Ne');         READ(text2,*) Ne
                CASE ('DT');         READ(text2,*) DT
                CASE ('NT');         READ(text2,*) NT   
                CASE ('save_every'); READ(text2,*) save_every    
                CASE DEFAULT; print*, TRIM(text),' variable is not recognized';
                END SELECT
        END DO
        WRITE(*,*) 'Input has been read.'

        ! Assigning derived variables
        Nn = Ne+1 ! Number of nodes in each direction, Nn = Ne+1
        H  = L/Ne ! Uniform grid size, H = L/Ne
        WRITE(*,*) 'Derived variables have been assigned.'

    END SUBROUTINE read_input


    ! To write fluid domain data into Tecplot readable files
    SUBROUTINE write_contour_file(file)
        CHARACTER(LEN=*), INTENT(IN) :: file      ! file name
        !REAL, DIMENSION(:,:), INTENT(IN) :: data  ! matrix of data
        ! Assumed-shape arrays/strings are by reference
        ! INTENT(IN) ensures the elements can not be changed here.
        INTEGER funit, io_status
        INTEGER ix, iy, iz

        ! Opening file
        OPEN(NEWUNIT=funit, FILE=file, ACTION='write', &
             IOSTAT=io_status, STATUS='unknown', POSITION='REWIND')
        IF(io_status/=0) THEN
            PRINT*,'Unable to create file.'
            STOP 'stopping the execution' ! stops the code execution
        END IF

        ! Writing data
        WRITE(funit, '(A,A,A)', ADVANCE='YES') 'TITLE = "', file,'"'
        WRITE(funit, '(A)', ADVANCE='YES') 'VARIABLES = "x","y","z","u","v","w","p"'
        WRITE(funit, '(A)', ADVANCE='NO') 'ZONE T="zone_1" '
        WRITE(funit, '(A,I10,A,I10,A,I10)', ADVANCE='YES')  &
              'I=', SIZE(x,1) ,', J=', SIZE(x,2) ,' K=', SIZE(x,3)
        
        DO ix = 1,SIZE(x,1)
            DO iy = 1,SIZE(x,2)
                DO iz = 1,SIZE(x,3)
                    WRITE(funit,'(E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8)', ADVANCE='YES') &
                          x(ix,iy,iz,1),' ',x(ix,iy,iz,2),' ',x(ix,iy,iz,3),' ', &
                          u(ix,iy,iz,1),' ',u(ix,iy,iz,2),' ',u(ix,iy,iz,3),' ', &
                          p(ix,iy,iz)
                END DO
            END DO
        END DO
        
        ! Closing the opened file
        CLOSE(UNIT=funit)
    END SUBROUTINE write_contour_file


    ! To write fiber data into data file
    SUBROUTINE write_fiber_file(file)
        CHARACTER(LEN=*), INTENT(IN) :: file      ! file name
        !REAL, DIMENSION(:,:), INTENT(IN) :: data  ! matrix of data
        ! Assumed-shape arrays/strings are by reference
        ! INTENT(IN) ensures the elements can not be changed here.
        INTEGER funit, io_status
        INTEGER in

        ! opening file
        OPEN(NEWUNIT=funit, FILE=file, ACTION='write', &
             IOSTAT=io_status, STATUS='unknown', POSITION='REWIND')
        IF(io_status/=0) THEN
            PRINT*,'Unable to create file.'
            STOP 'stopping the execution' ! stops the code execution
        END IF

        ! Writing data
        WRITE(funit, '(A,A,A)', ADVANCE='YES') 'TITLE = "', file,'"'
        WRITE(funit, '(A)', ADVANCE='YES') 'VARIABLES = "x","y","z","x0","y0","z0","fx","fy","fz"'
        WRITE(funit, '(A)', ADVANCE='NO') 'ZONE T="zone_2" '
        WRITE(funit, '(A,I10)', ADVANCE='YES') 'I=', SIZE(xf1,1) 

        DO in = 1,SIZE(xf1,1)
            WRITE(funit,'(E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8,A,E15.8)', ADVANCE='YES') &
                    xf1(in,1),  ' ', xf1(in,2) , ' ', xf1(in,3),  ' ', &
                    x0f1(in,1), ' ', x0f1(in,2), ' ', x0f1(in,3), ' ', & 
                    forcef1(in,1), ' ', forcef1(in,2), ' ', forcef1(in,3)
        END DO
        
        ! Closing the opened file
        CLOSE(UNIT=funit)
    END SUBROUTINE write_fiber_file


END MODULE mod_readwrite