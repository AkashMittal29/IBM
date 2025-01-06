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
        REAL(doubtype), INTENT(IN) :: arr1(:), arr2(:), arr3(:) ! arr1, arr2, arr3 correspond x, y, z direction respectively
        REAL(doubtype), INTENT(OUT) :: i1_mat(:,:,:), i2_mat(:,:,:), i3_mat(:,:,:) ! the resultant meshgrid matrices
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


    ! To convert number to string. Use TRIM(output) at function call to remove white spaces at the end of the string if needed.
    FUNCTION num2str_real(format, value) RESULT(output)
        CHARACTER (LEN=*) :: format
        REAL :: value
        CHARACTER(LEN=100) :: output
        WRITE(output,format) value
        output = ADJUSTL(output)
    END FUNCTION num2str_real


    ! To convert integer to string. Use TRIM(output) at function call to remove white spaces at the end of the string if needed.
    FUNCTION num2str_integer(format, value) RESULT(output)
        CHARACTER (LEN=*) :: format
        INTEGER :: value
        CHARACTER(LEN=100) :: output
        WRITE(output,format) value
        output = ADJUSTL(output)
    END FUNCTION num2str_integer


END MODULE mod_utility
