MODULE mod_variables
    IMPLICIT NONE

    ! Constants
    COMPLEX, PARAMETER :: I_UNIT = (0.0,1.0)      ! root of -1
    REAL, PARAMETER    :: PI = 3.141592653589793; ! pi
    REAL, PARAMETER    :: RHO   = 30.0          ! Fluid density, kg/m^3
    REAL, PARAMETER    :: MU    = 1.0 !0.0010518       ! Dynamic viscosity, N*s/m2
    REAL, PARAMETER    :: P_ATM = 101325.0        ! Atmospheric pressure, N/m^2
    REAL, PARAMETER    :: K     = 100000.0         ! Spring constant (stiffness) for fiber nodes, N/m

    ! Variables
    !! Domain discretization
    REAL,    PARAMETER :: L  = 1.0           ! Domain length (equal in all directions) -> cubic domain
    INTEGER, PARAMETER :: Ne  = 25           ! Number of elements in each direction
    INTEGER, PARAMETER :: Nn = Ne+1          ! Number of nodes in each direction = N+1
    REAL,    PARAMETER :: H  = L/Ne          ! Uniform grid size
    REAL, ALLOCATABLE  :: x(:,:,:,:)         ! Domain node coordinates. Last dimension is 3 -> 3d; vector
    REAL, ALLOCATABLE  :: u(:,:,:,:)         ! Fluid velocity; vector
    REAL, ALLOCATABLE  :: p(:,:,:)           ! Fluid pressure; scalar
    REAL, ALLOCATABLE  :: force_dom(:,:,:,:) ! Force on fluid domain due to fiber; vector

    !! Single fiber discretization
    REAL, ALLOCATABLE  :: xf1(:,:)     ! Fiber node coordinates (n,3)-> [X,Y,Z]
    REAL, ALLOCATABLE  :: x0f1(:,:)    ! Equilibrium location of Fiber nodes (n,3)-> [X,Y,Z]. Here zero force exists.
    REAL, ALLOCATABLE  :: forcef1(:,:) ! Force on fiber nodes (n,3)-> [FX, FY, FZ]

    !! Time discretization
    REAL,    PARAMETER :: DT = 0.0000001  ! Time step size, sec
    INTEGER, PARAMETER :: NT = 1       ! No. of time steps

    ! Auxiliary variables
    REAL, ALLOCATABLE    :: v(:,:,:,:)   ! Explicit term comprising of convective terms and domain forces; vector
    COMPLEX, ALLOCATABLE :: u_hat(:,:,:,:) ! Time coefficient for flow velocity; vector
    COMPLEX, ALLOCATABLE :: p_hat(:,:,:)   ! Time coefficient for pressure; vector
    COMPLEX, ALLOCATABLE :: v_hat(:,:,:,:) ! Time coefficient for Explicit term; vector
    REAL (KIND = 8) wtime ! To store computation time

END MODULE mod_variables