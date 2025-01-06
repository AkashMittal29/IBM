
PROGRAM main
    USE mod_variables
    USE mod_readwrite
    USE mod_initialize
    USE mod_utility
    USE mod_solve
    USE omp_lib
    IMPLICIT NONE
    

    ! Defining variables
    INTEGER it

    CALL read_input('input_file.inp')
    CALL initialize()
    CALL SYSTEM('mkdir Result')

    CALL write_contour_file('Result/step_'//TRIM(num2str('(I6.6)',0))//'.dat') 
    CALL write_fiber_file('Result/fiber_step_'//TRIM(num2str('(I6.6)',0))//'.dat')

    ! Time loop 
    DO it = 1,500000*2
        CALL solve()
        
        IF(MOD(it,5000)==0) THEN
            print*,it,'t = ',it*DT,' ',wtime
            ! Writing x, u, p, xf1, x0f1, forcef1
            CALL write_contour_file('Result/step_'//TRIM(num2str('(I6.6)',it))//'.dat') 
            CALL write_fiber_file('Result/fiber_step_'//TRIM(num2str('(I6.6)',it))//'.dat')
        END IF
    END DO


END PROGRAM main