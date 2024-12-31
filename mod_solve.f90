MODULE mod_solve
    USE mod_variables
    USE mod_utility
    USE omp_lib
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    include 'fftw3.f03'

    CONTAINS

    ! To update fiber location and fiber forces
    SUBROUTINE update_fiber()
        INTEGER i, dim, i1, i2, i3, ax, ay, az, n_points
        REAL,    DIMENSION(3)   :: b ! Fiber node index in terms of domain node indexing
        INTEGER, DIMENSION(4,3) :: a ! Domain node indices in all three directions around a fiber node
        INTEGER, DIMENSION(3)   :: n_nodes_around ! Stores no. of domain nodes around the fiber node (4 or 3)

        n_points = SIZE(xf1,1)
        Fiber_Loop: DO i=1,n_points ! Fiber node loop; to be parallelized
            Dim_Loop: DO dim=1,3
                b(dim) = xf1(i,dim)/H+1 ! Floating index of fiber node in domain. 
                                        ! If fiber node's x location is b/w domain nodes 2 and 3, 
                                        ! then b(1) is a real number between 2.0 and 3.0.
                                        ! ENSURE: domain origin is at left, bottom, rear corner.
                ! Finding domain nodes indices in each direction interacting with the fiber node
                    !
                    !   x1    x2    x3    x4
                    !   .     .   o .     .
                    ! 
                    ! o -> fiber node. Exactly four x-nodes are used to interact with the fiber node 
                    ! in smoothed Dirac Delta function. Hence, in 3d, total 4^3 = 64 domain nodes are
                    ! interacting with each fiber node.
                    !
                a(1,dim) = FLOOR(b(dim))
                a(2,dim) = a(1,dim)-1
                a(3,dim) = a(1,dim)+1
                IF(a(1,dim) == CEILING(b(dim))) THEN ! when fiber node lies exactly on the domain node
                    n_nodes_around(dim) = 3
                ELSE
                    a(4,dim) = a(1,dim)+2
                    n_nodes_around(dim) = 4
                END IF
            END DO Dim_Loop

            IF(SIZE(u,3)/=1) THEN ! for 3d case
                dom_nodes_loop_3d: &
                DO i1 = 1,n_nodes_around(1)
                DO i2 = 1,n_nodes_around(2)
                DO i3 = 1,n_nodes_around(3)
                    ! Applying periodic boundary condition
                    IF(a(i1,1)<1) THEN; ax = a(i1,1)+Nn; 
                        ELSE IF(a(i1,1)>Nn) THEN; ax = a(i1,1)-Nn; 
                        ELSE; ax = a(i1,1); END IF
                    IF(a(i2,2)<1) THEN; ay = a(i2,2)+Nn; 
                        ELSE IF(a(i2,2)>Nn) THEN; ay = a(i2,2)-Nn; 
                        ELSE; ay = a(i2,2); END IF
                    IF(a(i3,3)<1) THEN; az = a(i3,3)+Nn; 
                        ELSE IF(a(i3,3)>Nn) THEN; az = a(i3,3)-Nn; 
                        ELSE; az = a(i3,3); END IF
                    
                    ! Updating the fiber node position
                    xf1(i,:) = xf1(i,:) + DT * u(ax,ay,az,:) &
                                *smooth_delta_3d(b-[a(i1,1),a(i2,2),a(i3,3)]) ! Fiber can go out of domain but velocity field
                                                                              ! is as per periodic domain.
                END DO
                END DO
                END DO dom_nodes_loop_3d

            ELSE IF(SIZE(u,3)==1) THEN ! for 2d case
                dom_nodes_loop_2d: &
                DO i1 = 1,n_nodes_around(1)
                DO i2 = 1,n_nodes_around(2)
                        ! Applying periodic boundary condition
                        IF(a(i1,1)<1) THEN; ax = a(i1,1)+Nn; 
                            ELSE IF(a(i1,1)>Nn) THEN; ax = a(i1,1)-Nn; 
                            ELSE; ax = a(i1,1); END IF
                        IF(a(i2,2)<1) THEN; ay = a(i2,2)+Nn; 
                            ELSE IF(a(i2,2)>Nn) THEN; ay = a(i2,2)-Nn; 
                            ELSE; ay = a(i2,2); END IF
                        
                        ! Updating the fiber node position
                        xf1(i,:) = xf1(i,:) + DT * u(ax,ay,1,:) &
                                    *smooth_delta_2d(b(1:2)-[a(i1,1),a(i2,2)]) ! Fiber can go out of domain but velocity field
                                                                                ! is as per periodic domain.
                END DO
                END DO dom_nodes_loop_2d
            END IF

            ! Updating the fiber node force
            forcef1(i,:)   = -K*(xf1(i,:)-x0f1(i,:))/n_points ! Force = -K*dx, -ve if dx is +ve, distributed among nodes

        END DO Fiber_Loop

    END SUBROUTINE update_fiber


    ! To update domain forces by distributing the fiber forces through the smoothed delta function
    SUBROUTINE update_domain_force()
        INTEGER i, dim, i1, i2, i3, ax, ay, az, n_points
        REAL,    DIMENSION(3)   :: b ! Fiber node index in terms of domain node indexing
        INTEGER, DIMENSION(4,3) :: a ! Domain node indices in all three directions around a fiber node
        INTEGER, DIMENSION(3)   :: n_nodes_around ! Stores no. of domain nodes around the fiber node (4 or 3)

        force_dom=0 ! Assigning zero value to each domain force 
        n_points = SIZE(xf1,1)
        Fiber_Loop: DO i=1,n_points ! Fiber node loop; to be parallelized
            Dim_Loop: DO dim=1,3
                b(dim) = xf1(i,dim)/H+1 ! Floating index of fiber node in domain. 
                ! Details as in SUBROUTINE update_fiber                      
                a(1,dim) = FLOOR(b(dim))
                a(2,dim) = a(1,dim)-1
                a(3,dim) = a(1,dim)+1
                IF(a(1,dim) == CEILING(b(dim))) THEN ! when fiber node lies exactly on the domain node
                    n_nodes_around(dim) = 3
                ELSE
                    a(4,dim) = a(1,dim)+2
                    n_nodes_around(dim) = 4
                END IF
            END DO Dim_Loop

            IF(SIZE(u,3)/=1) THEN ! for 3d case
                dom_nodes_loop_3d: &
                DO i1 = 1,n_nodes_around(1)
                DO i2 = 1,n_nodes_around(2)
                DO i3 = 1,n_nodes_around(3)
                    ! Applying periodic boundary condition
                    IF(a(i1,1)<1) THEN; ax = a(i1,1)+Nn; 
                        ELSE IF(a(i1,1)>Nn) THEN; ax = a(i1,1)-Nn; 
                        ELSE; ax = a(i1,1); END IF
                    IF(a(i2,2)<1) THEN; ay = a(i2,2)+Nn; 
                        ELSE IF(a(i2,2)>Nn) THEN; ay = a(i2,2)-Nn; 
                        ELSE; ay = a(i2,2); END IF
                    IF(a(i3,3)<1) THEN; az = a(i3,3)+Nn; 
                        ELSE IF(a(i3,3)>Nn) THEN; az = a(i3,3)-Nn; 
                        ELSE; az = a(i3,3); END IF
                    
                    ! Distributing the fiber force onto domain nodes
                    force_dom(ax,ay,az,:) = force_dom(ax,ay,az,:) + forcef1(i,:) &
                                           *smooth_delta_3d(b-[a(i1,1),a(i2,2),a(i3,3)]) ! Fiber can go out of domain
                END DO
                END DO
                END DO dom_nodes_loop_3d

            ELSE IF(SIZE(u,3)==1) THEN ! for 2d case
                dom_nodes_loop_2d: &
                DO i1 = 1,n_nodes_around(1)
                DO i2 = 1,n_nodes_around(2)
                        ! Applying periodic boundary condition
                        IF(a(i1,1)<1) THEN; ax = a(i1,1)+Nn; 
                            ELSE IF(a(i1,1)>Nn) THEN; ax = a(i1,1)-Nn; 
                            ELSE; ax = a(i1,1); END IF
                        IF(a(i2,2)<1) THEN; ay = a(i2,2)+Nn; 
                            ELSE IF(a(i2,2)>Nn) THEN; ay = a(i2,2)-Nn; 
                            ELSE; ay = a(i2,2); END IF
                        
                        ! Updating the fiber node position
                        force_dom(ax,ay,1,:) = force_dom(ax,ay,1,:) + forcef1(i,:) &
                                              *smooth_delta_2d(b(1:2)-[a(i1,1),a(i2,2)]) ! Fiber can go out of domain
                END DO
                END DO dom_nodes_loop_2d
            END IF
        END DO Fiber_Loop
    END SUBROUTINE update_domain_force


    FUNCTION smooth_delta_3d(r) RESULT(res)
        ! r shall be < 2
        REAL, DIMENSION(3), INTENT(IN) :: r
        REAL :: res
        COMPLEX, DIMENSION(3) :: temp, rabs ! Complex type due to SQRT involved to evaluate temp. 
                                            ! The imaginary value is not used.
        rabs = ABS(r)
        temp = ((1.0-FLOOR(REAL(rabs))) *(3.0-2.0*rabs+SQRT( 1.0+ 4.0*rabs-4.0*r**2.0 )) &
              +(CEILING(REAL(rabs))-1.0)*(5.0-2.0*rabs-SQRT(-7.0+12.0*rabs-4.0*r**2.0 )))/8
        res = REAL(temp(1)*temp(2)*temp(3))
        RETURN
    END FUNCTION smooth_delta_3d


    FUNCTION smooth_delta_2d(r) RESULT(res)
        ! r shall be < 2
        REAL, DIMENSION(2), INTENT(IN) :: r
        REAL :: res
        COMPLEX, DIMENSION(2) :: temp, rabs ! Complex type due to SQRT involved to evaluate temp. 
                                            ! The imaginary value is not used.
        rabs = ABS(r)
        temp = ((1.0-FLOOR(REAL(rabs))) *(3.0-2.0*rabs+SQRT( 1.0+ 4.0*rabs-4.0*r**2.0 )) &
              +(CEILING(REAL(rabs))-1.0)*(5.0-2.0*rabs-SQRT(-7.0+12.0*rabs-4.0*r**2.0 )))/8
              res = REAL(temp(1)*temp(2))
        RETURN
    END FUNCTION smooth_delta_2d


    ! Subroutine to carry out solution for the next time step
    SUBROUTINE solve()
        wtime = omp_get_wtime() ! wall clock time in sec

        ! u, p, force_dom are already known
        CALL compute_v()
        CALL fft_forward(u(:,:,:,1),u_hat(:,:,:,1))
        CALL fft_forward(u(:,:,:,2),u_hat(:,:,:,2))
        CALL fft_forward(u(:,:,:,3),u_hat(:,:,:,3))

        CALL fft_forward(p(:,:,:),p_hat(:,:,:))

        CALL fft_forward(v(:,:,:,1),v_hat(:,:,:,1))
        CALL fft_forward(v(:,:,:,2),v_hat(:,:,:,2))
        CALL fft_forward(v(:,:,:,3),v_hat(:,:,:,3))

        CALL compute_next() ! solving for u_hat and v_hat for the next time step

        CALL fft_backward(u_hat(:,:,:,1),u(:,:,:,1))
        CALL fft_backward(u_hat(:,:,:,2),u(:,:,:,2))
        CALL fft_backward(u_hat(:,:,:,3),u(:,:,:,3))

        CALL fft_backward(p_hat(:,:,:),p(:,:,:))

        CALL update_fiber()        ! New fiber position and fiber force
        CALL update_domain_force() ! New domain fores

        wtime = omp_get_wtime()-wtime
    END SUBROUTINE solve


    ! To compute the vector of explicit term, v comprising of convective terms and domain forces
    SUBROUTINE compute_v()
        INTEGER i, xi, yi, zi, xm1, xp1, ym1, yp1, zm1, zp1, dim
        
        ! Assigning zero to v
        v=0

        IF (SIZE(u,3)==1) THEN; dim = 2 ! For 2d case
        ELSE; dim=3; END IF

        dim_loop: DO i=1,dim
            x_loop: DO xi=1,SIZE(u,1) ! to be parallelized
                xm1 = xi-1; xp1=xi+1;
                IF(xi==1)  THEN; xm1 = SIZE(u,1); END IF  ! Applying periodic boundary condition
                IF(xi==SIZE(u,1)) THEN; xp1 =  1; END IF
                y_loop: DO yi=1,SIZE(u,2) ! to be parallelized
                    ym1 = yi-1; yp1=yi+1;
                    IF(yi==1)  THEN; ym1 = SIZE(u,2); END IF 
                    IF(yi==SIZE(u,2)) THEN; yp1 =  1; END IF
                    z_loop: DO zi=1,SIZE(u,3)
                        zm1 = zi-1; zp1=zi+1;
                        IF(zi==1)  THEN; zm1 = SIZE(u,3); END IF 
                        IF(zi==SIZE(u,3)) THEN; zp1 =  1; END IF
                        
                        ! Convective term by Upwind Scheme
                        IF(u(xi,yi,zi,1)<0) THEN
                            v(xi,yi,zi,i) = u(xi,yi,zi,1)*( u(xp1,yi,zi,i)-u(xi,yi,zi,i)  )/H ! u_1*du_i/dx, forward diff.
                        ELSE
                            !print*, xi, xm1,yi,zi,i, u(xi,yi,zi,1)*( u(xi,yi,zi,i) -u(xm1,yi,zi,i) )/H, v(xi,yi,zi,i)
                            v(xi,yi,zi,i) = u(xi,yi,zi,1)*( u(xi,yi,zi,i) -u(xm1,yi,zi,i) )/H ! u_1*du_i/dx, backward diff.
                        END IF

                        IF(u(xi,yi,zi,2)<0) THEN
                            v(xi,yi,zi,i) = v(xi,yi,zi,i) &
                                          + u(xi,yi,zi,2)*( u(xi,yp1,zi,i)-u(xi,yi,zi,i)  )/H ! u_2*du_i/dy, forward diff.
                        ELSE
                            v(xi,yi,zi,i) = v(xi,yi,zi,i) &
                                          + u(xi,yi,zi,2)*( u(xi,yi,zi,i) -u(xi,ym1,zi,i) )/H ! u_2*du_i/dy, backward diff.
                        END IF

                        IF (SIZE(u,3)/=1) THEN ! Only executed for 3d case
                        IF(u(xi,yi,zi,3)<0) THEN
                            v(xi,yi,zi,i) = v(xi,yi,zi,i) &
                                            + u(xi,yi,zi,3)*( u(xi,yi,zp1,i)-u(xi,yi,zi,i)  )/H ! u_3*du_i/dz, forward diff.
                        ELSE
                            v(xi,yi,zi,i) = v(xi,yi,zi,i) &
                                            + u(xi,yi,zi,3)*( u(xi,yi,zi,i) -u(xi,yi,zm1,i) )/H ! u_3*du_i/dz, backward diff.
                        END IF
                        END IF

                        v(xi,yi,zi,i) = u(xi,yi,zi,i)-DT/RHO*v(xi,yi,zi,i)+DT/RHO*force_dom(xi,yi,zi,i)

                    END DO z_loop
                END DO y_loop
            END DO x_loop
        END DO dim_loop
    END SUBROUTINE compute_v


    ! To convert into frequency domain
    SUBROUTINE fft_forward(phi, phi_hat)
        REAL,    INTENT(IN)  :: phi(:,:,:)
        COMPLEX, INTENT(OUT) :: phi_hat(:,:,:) ! Must have same shape as of phi
        COMPLEX(C_DOUBLE_COMPLEX), ALLOCATABLE :: in(:,:,:), out(:,:,:)
        type(C_PTR) :: plan

        ! Allocating variables in and out that have datatype consistent with the fftw functions
        ALLOCATE( in( SIZE(phi,1),SIZE(phi,2),SIZE(phi,3)), &
                  out(SIZE(phi,1),SIZE(phi,2),SIZE(phi,3)) )

        ! Assigning values to variable in and converting type to CMPLX with KIND=C_DOUBLE_COMPLEX
        in = CMPLX(phi(:,:,:),KIND=C_DOUBLE_COMPLEX)

        ! Creating plan for FFTW
        plan = fftw_plan_dft_3d(size(phi,1), size(phi,2), size(phi,3), &
                                in, out, FFTW_FORWARD, FFTW_ESTIMATE)

        call fftw_execute_dft(plan, in, out)   ! Calling FFTW
		call fftw_destroy_plan(plan)           ! Destroying plan to free memory space
        
        ! Converting to default complex kind
        phi_hat = CMPLX(out, KIND=KIND(0.0)) /SIZE(phi,1)/SIZE(phi,2)/SIZE(phi,3) 
    END SUBROUTINE fft_forward


    ! To convert into time domain from frequency domain
    SUBROUTINE fft_backward(phi_hat, phi)
        COMPLEX, INTENT(IN)  :: phi_hat(:,:,:)
        REAL,    INTENT(OUT) :: phi(:,:,:) ! Must have same shape as of phi_hat
        COMPLEX(C_DOUBLE_COMPLEX), ALLOCATABLE :: in(:,:,:), out(:,:,:)
        type(C_PTR) :: plan

        ! Allocating variables in and out that have datatype consistent with the fftw functions
        ALLOCATE( in( SIZE(phi,1),SIZE(phi,2),SIZE(phi,3)), &
                  out(SIZE(phi,1),SIZE(phi,2),SIZE(phi,3)) )

        ! Assigning values to variable in and converting type to CMPLX with KIND=C_DOUBLE_COMPLEX
        in = CMPLX(phi_hat(:,:,:),KIND=C_DOUBLE_COMPLEX)

        ! Creating plan for FFTW
        plan = fftw_plan_dft_3d(size(phi,1), size(phi,2), size(phi,3), &
                                in, out, FFTW_BACKWARD, FFTW_ESTIMATE)

        call fftw_execute_dft(plan, in, out) ! Calling FFTW
		call fftw_destroy_plan(plan)         ! Destroying plan to free memory space	
        phi = REAL(out, KIND=KIND(0.0))      ! Extracting real part. Imaginary part is zero.
    END SUBROUTINE fft_backward


    ! To compute values of frequency domain variables for the next time step
    SUBROUTINE compute_next()
        INTEGER, DIMENSION(3) :: n
        INTEGER k1, k2, k3
        REAL, DIMENSION(3) :: sin_val
        REAL ak

        n=[SIZE(x,1), SIZE(x,2), SIZE(x,3)]
        
        DO k1=0,n(1)-1 ! To be parallelized
            sin_val(1) = SIN(2*PI*k1/n(1))
            DO k2=0,n(2)-1 ! To be parallelized
                sin_val(2) = SIN(2*PI*k2/n(2))
                DO k3=0,n(3)-1 ! To be parallelized
                    sin_val(3) = SIN(2*PI*k3/n(3))

                    ! Calculating p_hat for next time step
                    IF(sin_val(1)==0 .AND. sin_val(2)==0 .AND. sin_val(3)==0) THEN
                        p_hat(k1+1,k2+1,k3+1) = 0.0
                    ELSE
                        p_hat(k1+1,k2+1,k3+1) = SUM(v_hat(k1+1,k2+1,k3+1,:)*sin_val(:)) &
                                                /(DT/RHO/H*I_UNIT*(SUM(sin_val**2)))                            
                    END IF

                    ! Calculating u_hat for next time step
                    ak = 1 - MU*DT/RHO*(-4.0)/(H**2)*((SIN(PI*k1/n(1)))**2 &
                                                     +(SIN(PI*k2/n(2)))**2 &
                                                     +(SIN(PI*k3/n(3)))**2)
                    u_hat(k1+1,k2+1,k3+1,:) = v_hat(k1+1,k2+1,k3+1,:) &
                                             -I_UNIT/H*DT/RHO*sin_val &
                                             *p_hat(k1+1,k2+1,k3+1)/ak
                END DO
            END DO
        END DO
    END SUBROUTINE compute_next


END MODULE mod_solve