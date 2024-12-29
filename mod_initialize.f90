MODULE mod_initialize
    USE mod_variables
    USE mod_utility
    IMPLICIT NONE


    CONTAINS

      ! Initialization main
    SUBROUTINE initialize()
        CALL initialize_fiber()
        CALL initialize_domain()
    END SUBROUTINE initialize


     ! Initializing fiber
    SUBROUTINE initialize_fiber()
        INTEGER :: n_points = 5 ! No. of fiber nodes/points
        INTEGER i
        print*,'in fiber initialization' 
        
        ! Allocating the variables
        ALLOCATE(xf1(n_points,3), x0f1(n_points,3), forcef1(n_points,3))

        ! Assigning values
        xf1(:,1)  = 0.5                    ! x coordinates
        xf1(:,2)  = 0.45 + [(i, i=0,4)]*H  ! y coordinates
        xf1(:,3)  = 0.0                    ! z coordinates
        x0f1(:,1) = 0.6                    ! x coordinates
        x0f1(:,2) = xf1(:,2)               ! y coordinates
        x0f1(:,3) = 0.0                    ! z coordinates
        forcef1   = -K*(xf1-x0f1)/n_points ! Force = -K*dx, -ve if dx is +ve, distributed among nodes
    END SUBROUTINE initialize_fiber


    ! Initializing fluid domain
    SUBROUTINE initialize_domain()
        REAL, DIMENSION(Nn) :: arr1, arr2
        REAL, DIMENSION(1)  :: arr3 ! For 2d case
        INTEGER i
        print*,'in fluid initialization'

        ! Allocating flow variables for a 2d case
        ALLOCATE(x(Nn,Nn,1,3), u(Nn,Nn,1,3), p(Nn,Nn,1), force_dom(Nn,Nn,1,3)) 

        ! Assigning grid and initializing flow variables
        arr1 = [(i, i=0,Nn-1)]*H ! x-axis
        arr2 = [(i, i=0,Nn-1)]*H ! y-axis
        arr3 = [0]               ! z-axis, for 2d case
        CALL meshgrid_3d(arr1, arr2, arr3, x(:,:,:,1), x(:,:,:,2), x(:,:,:,3))
        
        u = 0  ! Considering flow stationary at t = 0
        P = P_ATM

        ! Call subroutine to initialize the force_domain from the fiber forces
    END SUBROUTINE initialize_domain


END MODULE mod_initialize