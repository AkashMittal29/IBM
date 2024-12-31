MODULE mod_utility
    USE mod_variables
    IMPLICIT NONE


    INTERFACE num2str
        MODULE PROCEDURE num2str_real, num2str_integer
    END INTERFACE


    CONTAINS

    ! To generate a 3d/2d mesh from arrays
    SUBROUTINE meshgrid_3d(arr1, arr2, arr3, i1_mat, i2_mat, i3_mat) 
        ! for 2d case, arr3 is an array with only one element describing value of z coordinate,
        ! and shape of i_mat is (SIZE(arr1,1), SIZE(arr2,1), SIZE(arr3,1)=1)
        REAL, INTENT(IN) :: arr1(:), arr2(:), arr3(:) ! arr1, arr2, arr3 correspond x, y, z direction respectively
        REAL, INTENT(OUT) :: i1_mat(:,:,:), i2_mat(:,:,:), i3_mat(:,:,:) ! the resultant meshgrid matrices
        INTEGER n_dim(3), i

        n_dim(1) = SIZE(arr1,1); n_dim(2) = SIZE(arr2,1); n_dim(3) = SIZE(arr3,1);

        ! Assigning i1_mat
        i1_mat(:,1,1) = arr1
        DO i=1,n_dim(2)
            i1_mat(:,i,1) = i1_mat(:,1,1)
        END DO
        DO i=1,n_dim(3)
            i1_mat(:,:,i) = i1_mat(:,:,1)
        END DO

        ! Assigning i2_mat
        i2_mat(1,:,1) = arr2 
        DO i=1,n_dim(1)
            i2_mat(i,:,1) = i2_mat(1,:,1)
        END DO
        DO i=1,n_dim(3)
            i2_mat(:,:,i) = i2_mat(:,:,1)
        END DO

        ! Assigning i3_mat
        i3_mat(1,1,:) = arr3
        DO i=1,n_dim(1)
            i3_mat(i,1,:) = i3_mat(1,1,:)
        END DO
        DO i=1,n_dim(2)
            i3_mat(:,i,:) = i3_mat(:,1,:)
        END DO
    END SUBROUTINE meshgrid_3d


    ! To print a real valued matrix
    SUBROUTINE print_matrix_real_3d(a)
        REAL, DIMENSION(:,:,:), INTENT(IN) :: a ! By-reference; INTENT(IN)->can not be modified here
        INTEGER r,c,h,i,j,k

        r = SIZE(a,1)
        c = SIZE(a,2)
        h = SIZE(a,3)

        DO k=1,h,1
            print*, '(:,:,',k,') ---'
            DO i=1,r,1
                DO j=1,c,1
                    write(*,'(E10.4,A)',ADVANCE='NO') a(i,j,k), ', '
                END DO
                print* ! new line
            END DO
            print* ! new line
        END DO
    END SUBROUTINE print_matrix_real_3d


    ! To write fluid domain data into Tecplot readable files
    SUBROUTINE write_contour_file(file)
        CHARACTER(LEN=*), INTENT(IN) :: file      ! file name
        !REAL, DIMENSION(:,:), INTENT(IN) :: data  ! matrix of data
        ! Assumed-shape arrays/strings are by reference
        ! INTENT(IN) ensures the elements can not be changed here.
        INTEGER funit, io_status
        INTEGER ix, iy, iz

        ! opening file
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


    ! To convert number to string
    FUNCTION num2str_real(format, value) RESULT(output)
        CHARACTER (LEN=*) :: format
        REAL :: value
        CHARACTER(LEN=100) :: output
        WRITE(output,format) value
        output = ADJUSTL(output)
    END FUNCTION num2str_real

    ! To convert integer to string
    FUNCTION num2str_integer(format, value) RESULT(output)
        CHARACTER (LEN=*) :: format
        INTEGER :: value
        CHARACTER(LEN=100) :: output
        WRITE(output,format) value
        output = ADJUSTL(output)
    END FUNCTION num2str_integer




END MODULE mod_utility
