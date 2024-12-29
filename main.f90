
PROGRAM main
    USE mod_variables
    USE mod_initialize
    USE mod_utility

    ! Defining variables
    INTEGER i

    i = 2
    print*, i, 'itext'
    print*, RHO, 'complex: ', I_UNIT

    CALL initialize()
    !print*, u(1,1,1,:), p(1,1,1)
    !print*, 'x values'; CALL print_matrix_real_3d(x(:,:,:,1))
    !print*, 'y values'; CALL print_matrix_real_3d(x(:,:,:,2))
    !print*, 'z values'; CALL print_matrix_real_3d(x(:,:,:,3))
    print*,'xof1: ', x0f1
    print*, 'xf1', xf1
    print*, 'forcef1', forcef1

    

END PROGRAM main