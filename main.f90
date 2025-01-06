
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
    CHARACTER(LEN=100) :: dir

    dir = 'Result_'

    CALL read_input('input_file.inp')
    CALL initialize()
    CALL SYSTEM('mkdir '//TRIM(ADJUSTL(dir)))
    
    CALL write_contour_file(TRIM(ADJUSTL(dir))//'/step_'//TRIM(num2str('(I6.6)',0))//'.dat') 
    CALL write_fiber_file(TRIM(ADJUSTL(dir))//'/fiber_step_'//TRIM(num2str('(I6.6)',0))//'.dat')
    ! Time loop 
    DO it = 1,NT
        CALL solve()
        
        IF(MOD(it,save_every)==0) THEN
            print*,it,'t = ',it*DT,' ',wtime
            ! Writing x, u, p, xf1, x0f1, forcef1
            CALL write_contour_file(TRIM(ADJUSTL(dir))//'/step_'//TRIM(num2str('(I6.6)',it))//'.dat') 
            CALL write_fiber_file(TRIM(ADJUSTL(dir))//'/fiber_step_'//TRIM(num2str('(I6.6)',it))//'.dat')
        END IF
    END DO


END PROGRAM main