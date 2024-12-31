
PROGRAM main
    USE mod_variables
    USE mod_initialize
    USE mod_utility
    USE mod_solve
    USE omp_lib
    IMPLICIT NONE
    

    ! Defining variables
    INTEGER it

    CALL initialize()

    print*, xf1, forcef1

    ! Time loop 
    DO it = 1,1 !0000
        CALL solve()
        print*,it,' ',wtime

        IF MOD(it,1) then
            CALL write_Tecplot_file() ! Writing x, u, p, xf1, x0f1, forcef1
        END IF
    END DO

    print*, xf1, forcef1

END PROGRAM main