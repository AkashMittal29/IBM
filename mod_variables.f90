MODULE mod_variables
    IMPLICIT NONE

    ! Constants
    COMPLEX, PARAMETER :: I_UNIT = (0.0,1.0)      ! root of -1
    REAL,    PARAMETER :: PI = 3.141592653589793; ! pi
    INTEGER, PARAMETER :: singtype = selected_real_kind(6,37)     ! single precision and exponential range -37 to +37
    INTEGER, PARAMETER :: doubtype = selected_real_kind(15,307)   ! double precision
    !INTEGER, PARAMETER :: quadtype = selected_real_kind(33,4931) ! extended precision

    
    ! Variables
    !! Parameters
    REAL(singtype)     :: RHO   = 30.0       ! Fluid density, kg/m^3
    REAL(singtype)     :: MU    = 1.0        ! Dynamic viscosity, N*s/m2
    REAL(singtype)     :: P_ATM = 101325.0   ! Atmospheric pressure, N/m^2
    REAL(singtype)     :: K     = 100000.0   ! Spring constant (stiffness) for fiber nodes, N/m

    !! Domain discretization
    REAL(doubtype)               :: L  = 1.0           ! Domain length (equal in all directions) -> cubic domain
    INTEGER                      :: Ne  = 25           ! Number of elements in each direction 
    REAL(doubtype), ALLOCATABLE  :: x(:,:,:,:)         ! Domain node coordinates. Last dimension is 3 -> 3d; vector
    REAL(doubtype), ALLOCATABLE  :: u(:,:,:,:)         ! Fluid velocity; vector
    REAL(doubtype), ALLOCATABLE  :: p(:,:,:)           ! Fluid pressure; scalar
    REAL(doubtype), ALLOCATABLE  :: force_dom(:,:,:,:) ! Force on fluid domain due to fiber; vector
    !!! Derived variables
    INTEGER                      :: Nn                 ! Number of nodes in each direction, Nn = Ne+1
    REAL(doubtype)               :: H                  ! Uniform grid size, H = L/Ne

    !! Single fiber discretization
    REAL(doubtype), ALLOCATABLE  :: xf1(:,:)           ! Fiber node coordinates (n,3)-> [X,Y,Z]
    REAL(doubtype), ALLOCATABLE  :: x0f1(:,:)          ! Equilibrium location of Fiber nodes (n,3)-> [X,Y,Z]. Here zero force exists.
    REAL(doubtype), ALLOCATABLE  :: forcef1(:,:)       ! Force on fiber nodes (n,3)-> [FX, FY, FZ]

    !! Time discretization
    REAL(doubtype)               :: DT = 0.0000001     ! Time step size, sec
    INTEGER                      :: NT = 1             ! No. of time steps


    ! Auxiliary variables
    REAL(doubtype),    ALLOCATABLE :: v(:,:,:,:)       ! Explicit term comprising of convective terms and domain forces; vector
    COMPLEX(doubtype), ALLOCATABLE :: u_hat(:,:,:,:)   ! Time coefficient for flow velocity; vector
    COMPLEX(doubtype), ALLOCATABLE :: p_hat(:,:,:)     ! Time coefficient for pressure; vector
    COMPLEX(doubtype), ALLOCATABLE :: v_hat(:,:,:,:)   ! Time coefficient for Explicit term; vector
    REAL (KIND = 8) wtime                    ! To store computation time
    INTEGER save_every                       ! Save data every save_every-th time step

END MODULE mod_variables