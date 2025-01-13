MODULE mod_readwrite
    USE mod_variables
    USE mod_initialize
    USE mod_solve
	USE mod_utility
    USE, INTRINSIC :: iso_fortran_env, ONLY : error_unit
    IMPLICIT NONE

    CONTAINS

    ! To read values of the variables from input file and assigning derived variables
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
                CASE ('result_dir'); READ(text2,*) result_dir  
                CASE ('save_restart_every'); READ(text2,*) save_restart_every  
                CASE ('use_restart');  READ(text2,*) use_restart  
                CASE ('restart_file'); READ(text2,*) restart_file 
									   CALL replace_char(restart_file,'\','/')
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


    ! To save restart file that includes x(:,:,:,:), u(:,:,:,:), p(:,:,:), xf1(:,:), x0f1(:,:)
    SUBROUTINE save_restart_file(file, time_step)
        CHARACTER(LEN=*), INTENT(IN) :: file      ! file name
        INTEGER, INTENT(IN) :: time_step
        INTEGER funit, dim, nx, ny, nz, nf, dimf

        !! Number of nodes in x, y, z directions and dimension (whcih is always 3)
        nx = SIZE(x,1)
        ny = SIZE(x,2)
        nz = SIZE(x,3)
        dim = SIZE(x,4)
        nf = SIZE(xf1,1)
        dimf = SIZE(xf1,2)
        
        ! opening file (binary)
        OPEN(NEWUNIT=funit, FILE=file, &
             FORM='UNFORMATTED', &
             ACCESS='SEQUENTIAL')
             !ACCESS='DIRECT', &
             !RECL=nx*ny*nz*STORAGE_SIZE(x)) ! Record length

        ! Writing data
        WRITE(funit) time_step
        WRITE(funit) DT
        WRITE(funit) time
        WRITE(funit) (/dim,nx,ny,nz,dimf,nf/)
        WRITE(funit) x
        WRITE(funit) u
        WRITE(funit) p
        WRITE(funit) xf1
        WRITE(funit) x0f1

        print*, STORAGE_SIZE(dim)*4/8 + (nx*ny*nz*dim + nx*ny*nz*dim + nx*ny*nz + &
                 SIZE(xf1,1)*(SIZE(xf1,2))*2 ) * STORAGE_SIZE(x)/8, ' + 24 or 48 bytes' 

        CLOSE(funit)
        PRINT*, 'Restart file has been written.'
    END SUBROUTINE save_restart_file


    ! Reading restart file and setting up restart case
    SUBROUTINE setup_restart_case(file, time_step)
        CHARACTER(LEN=*), INTENT(IN) :: file      ! file name
        INTEGER, INTENT(OUT) :: time_step
        INTEGER funit, dim, nx, ny, nz, nf, dimf
        REAL(doubtype) :: DT_restart
        INTEGER :: arr(6), io_status
		
		PRINT*, 'Reading restart file:',file

        ! opening file (binary)
        OPEN(NEWUNIT=funit, FILE=file, &
             FORM='UNFORMATTED', &
             ACCESS='SEQUENTIAL', IOSTAT=io_status)

        ! Reading data
        READ(funit) time_step
        READ(funit) DT_restart
        READ(funit) time
        PRINT*, 'Last time step is       :', time_step
        PRINT*, 'DT from restart file is :', DT_restart, ' s'
        PRINT*, 'DT from input file is   :', DT, ' s (is used now)'
        PRINT*, 'Last time is            :',time, ' s'
        READ(funit) arr
        dim = arr(1); nx = arr(2); ny = arr(3); nz = arr(4); dimf = arr(5); nf = arr(6);

        ! Allocating variables
        ALLOCATE(x(nx,ny,nz,dim), u(nx,ny,nz,dim), p(nx,ny,nz), force_dom(nx,ny,nz,dim))
        ALLOCATE(xf1(nf,dimf), x0f1(nf,dimf), forcef1(nf,dimf))

        ! Reading data
        READ(funit) x
        READ(funit) u
        READ(funit) p
        READ(funit) xf1
        READ(funit) x0f1

        ! Updating fiber and domain forces
        CALL update_fiber_force()  ! Using the current fiber location
        CALL update_domain_force() ! Using the obtained fiber force

        ! Allocating auxiliary variables
        CALL allocate_aux() 

        ! Nn and H are already assigned in SUBROUTINE read_input(file)
        print*, 'Restart has been setup.'
    END SUBROUTINE setup_restart_case

END MODULE mod_readwrite