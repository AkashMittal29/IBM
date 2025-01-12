
PROGRAM main
    USE mod_variables
    USE mod_readwrite
    USE mod_initialize
    USE mod_utility
    USE mod_solve
    USE omp_lib
    IMPLICIT NONE
    
    ! Defining variables
    INTEGER time_step

    use_restart=0          ! Default value
    result_dir = 'Result'  ! Default directory
    save_every = -1         ! Default value
    save_restart_every = -1 ! Default value

    CALL read_input('input_file.inp')

    CALL SYSTEM('mkdir '//TRIM(ADJUSTL(result_dir)))

    IF(use_restart==1) THEN
        CALL setup_restart_case(TRIM(ADJUSTL(restart_file)),time_step)
        NT = NT + time_step
    ELSE
        time_step = 0
        time = 0
        CALL initialize()

        ! Writing initial data
        CALL write_contour_file(TRIM(ADJUSTL(result_dir))//'/step_'//TRIM(num2str('(I9.9)',0))//'.dat') 
        CALL write_fiber_file(TRIM(ADJUSTL(result_dir))//'/fiber_step_'//TRIM(num2str('(I9.9)',0))//'.dat')
    END IF

    ! Time loop 
    DO WHILE(.TRUE.)
        time_step = time_step+1
        time = time + DT
        IF(time_step>NT) THEN
            EXIT
        END IF

        CALL solve()
        
        IF( (save_every/=-1 .AND. MOD(time_step,save_every)==0) .OR. time_step==NT) THEN
            print*,time_step,'t = ',time_step*DT,' ',wtime ! Wall time taken for the current time step computation
            ! Writing x, u, p, xf1, x0f1, forcef1
            CALL write_contour_file(TRIM(ADJUSTL(result_dir))//'/step_'//TRIM(num2str('(I9.9)',time_step))//'.dat') 
            CALL write_fiber_file(TRIM(ADJUSTL(result_dir))//'/fiber_step_'//TRIM(num2str('(I9.9)',time_step))//'.dat')
        END IF
        IF((save_restart_every/=-1 .AND. MOD(time_step,save_restart_every)==0) .OR. time_step==NT) THEN
            CALL save_restart_file(TRIM(ADJUSTL(result_dir))//'/restart_'//TRIM(num2str('(I9.9)',time_step))//'.bin', time_step)
        END IF    
    END DO
    
END PROGRAM main

