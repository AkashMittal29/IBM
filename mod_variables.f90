MODULE mod_variables
    IMPLICIT NONE

    ! Constants
    COMPLEX, PARAMETER :: I_UNIT = (0.0,1.0)      ! root of -1
    REAL, PARAMETER    :: PI = 3.141592653589793; ! pi
    REAL, PARAMETER    :: RHO   = 1000.0          ! Fluid density, kg/m^3
    REAL, PARAMETER    :: MU    = 0.0010518       ! Dynamic viscosity, N*s/m2
    REAL, PARAMETER    :: P_ATM = 101325.0        ! Atmospheric pressure, N/m^2
    REAL, PARAMETER    :: K     = 10.0            ! Spring constant (stiffness) for fiber nodes, N/m

    ! Variables
    !! Domain discretization
    REAL,    PARAMETER :: L  = 1.0           ! Domain length (equal in all directions) -> cubic domain
    INTEGER, PARAMETER :: N  = 25            ! Number of elements in each direction
    INTEGER, PARAMETER :: Nn = N+1           ! Number of nodes in each direction = N+1
    REAL,    PARAMETER :: H  = L/N           ! Uniform grid size
    REAL, ALLOCATABLE  :: x(:,:,:,:)         ! Domain node coordinates. Last dimension is 3 -> 3d; vector
    REAL, ALLOCATABLE  :: u(:,:,:,:)         ! Fluid velocity; vector
    REAL, ALLOCATABLE  :: p(:,:,:)           ! Fluid pressure; scalar
    REAL, ALLOCATABLE  :: force_dom(:,:,:,:) ! Force on fluid domain due to fiber; vector

    !! Single fiber discretization
    REAL, ALLOCATABLE  :: xf1(:,:)     ! Fiber node coordinates (n,3)-> [X,Y,Z]
    REAL, ALLOCATABLE  :: x0f1(:,:)    ! Equilibrium location of Fiber nodes (n,3)-> [X,Y,Z]. Here zero force exists.
    REAL, ALLOCATABLE  :: forcef1(:,:) ! Force on fiber nodes (n,3)-> [FX, FY, FZ]

END MODULE mod_variables