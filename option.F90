module Option_module

! IMPORTANT NOTE: This module can have no dependencies on other modules!!!


#include "petsc/finclude/petscsys.h"
  use petscsys
  use PFLOTRAN_Constants_module

  implicit none

  private

  type, public :: option_type

    PetscInt :: id                         ! id of realization
    PetscInt :: successful_exit_code       ! code passed out of PFLOTRAN
                                           ! indicating successful completion
                                           ! of simulation
    PetscMPIInt :: global_comm             ! MPI_COMM_WORLD
    PetscMPIInt :: global_rank             ! rank in MPI_COMM_WORLD
    PetscMPIInt :: global_commsize         ! size of MPI_COMM_WORLD
    PetscMPIInt :: global_group            ! id of group for MPI_COMM_WORLD

    PetscMPIInt :: mycomm                  ! PETSC_COMM_WORLD
    PetscMPIInt :: myrank                  ! rank in PETSC_COMM_WORLD
    PetscMPIInt :: mycommsize              ! size of PETSC_COMM_WORLD
    PetscMPIInt :: mygroup                 ! id of group for PETSC_COMM_WORLD
    PetscMPIInt :: mygroup_id

    character(len=MAXSTRINGLENGTH) :: io_buffer
    character(len=MAXSTRINGLENGTH) :: input_filename

! don't place a character string near here.  It causes the Windows Intel compiler
! to crash.  Don't know why....

    PetscMPIInt :: io_rank
    PetscInt :: fid_out

    PetscBool :: print_to_screen
    PetscBool :: print_to_file

    PetscBool :: print_screen_flag
    PetscBool :: print_file_flag


  end type option_type

  PetscInt, parameter, public :: SUBSURFACE_SIM_TYPE = 1
  PetscInt, parameter, public :: MULTISIMULATION_SIM_TYPE = 2
  PetscInt, parameter, public :: STOCHASTIC_SIM_TYPE = 3

  interface printMsg
    module procedure printMsg1
    module procedure printMsg2
  end interface

  interface printMsgAnyRank
    module procedure printMsgAnyRank1
    module procedure printMsgAnyRank2
  end interface

  interface printMsgByRank
    module procedure printMsgByRank1
    module procedure printMsgByRank2
  end interface

  interface printErrMsg
    module procedure printErrMsg1
    module procedure printErrMsg2
  end interface

  interface printWrnMsg
    module procedure printWrnMsg1
    module procedure printWrnMsg2
  end interface

  interface OptionInitMPI
    module procedure OptionInitMPI1
    module procedure OptionInitMPI2
  end interface

  public :: OptionCreate, &
            printErrMsg, &
            printWrnMsg, &
            printMsg, &
            printMsgAnyRank, &
            printMsgByRank, &
            printMsgByCell, &
            OptionPrint, &
            OptionPrintToScreen, &
            OptionPrintToFile, &
            OptionInitMPI, &
            OptionInitPetsc, &
            OptionFinalize, &
            OptionDestroy

contains

! ************************************************************************** !

function OptionCreate()
  !
  ! Allocates and initializes a new Option object
  !
  ! Author: Glenn Hammond
  ! Date: 10/25/07
  !

  implicit none

  type(option_type), pointer :: OptionCreate

  type(option_type), pointer :: option

  allocate(option)

  ! DO NOT initialize members of the option type here.  One must decide
  ! whether the member needs initialization once for all stochastic
  ! simulations or initialization for every realization (e.g. within multiple
  ! stochastic simulations).  This is done in OptionInitAll() and
  ! OptionInitRealization()
  call OptionInitAll(option)
  OptionCreate => option

end function OptionCreate

! ************************************************************************** !

subroutine OptionInitAll(option)
  !
  ! Initializes all option variables
  !
  ! Author: Glenn Hammond
  ! Date: 10/25/07
  !

  implicit none

  type(option_type) :: option

  ! These variables should only be initialized once at the beginning of a
  ! PFLOTRAN run (regardless of whether stochastic)

  option%id = 0
  option%successful_exit_code = 0

  option%global_comm = 0
  option%global_rank = 0
  option%global_commsize = 0
  option%global_group = 0

  option%mycomm = 0
  option%myrank = 0
  option%mycommsize = 0
  option%mygroup = 0
  option%mygroup_id = 0

  option%io_rank = 0

  option%input_filename = ''

end subroutine OptionInitAll

! ************************************************************************** !

subroutine printErrMsg1(option)
  !
  ! Prints the error message from p0 and stops
  !
  ! Author: Glenn Hammond
  ! Date: 10/26/07
  !

  implicit none

  type(option_type) :: option

  call printErrMsg2(option,option%io_buffer)

end subroutine printErrMsg1

! ************************************************************************** !

subroutine printErrMsg2(option,string)
  !
  ! Prints the error message from p0 and stops
  !
  ! Author: Glenn Hammond
  ! Date: 10/26/07
  !

  implicit none

  type(option_type) :: option
  character(len=*) :: string

  PetscBool :: petsc_initialized
  PetscErrorCode :: ierr

  if (OptionPrintToScreen(option)) then
    print *
    print *, 'ERROR: ' // trim(string)
    print *
    print *, 'Stopping!'
  endif
  call MPI_Barrier(option%mycomm,ierr)
  call PetscInitialized(petsc_initialized, ierr);CHKERRQ(ierr)
  if (petsc_initialized) then
    call PetscFinalize(ierr);CHKERRQ(ierr)
  endif
  stop

end subroutine printErrMsg2

! ************************************************************************** !

subroutine printWrnMsg1(option)
  !
  ! Prints the warning message from p0
  !
  ! Author: Glenn Hammond
  ! Date: 10/26/07
  !

  implicit none

  type(option_type) :: option

  call printWrnMsg2(option,option%io_buffer)

end subroutine printWrnMsg1

! ************************************************************************** !

subroutine printWrnMsg2(option,string)
  !
  ! Prints the warning message from p0
  !
  ! Author: Glenn Hammond
  ! Date: 10/26/07
  !

  implicit none

  type(option_type) :: option
  character(len=*) :: string

  if (OptionPrintToScreen(option)) print *, 'WARNING: ' // trim(string)

end subroutine printWrnMsg2

! ************************************************************************** !

subroutine printMsg1(option)
  !
  ! Prints the message from p0
  !
  ! Author: Glenn Hammond
  ! Date: 11/14/07
  !

  implicit none

  type(option_type) :: option

  call printMsg2(option,option%io_buffer)

end subroutine printMsg1

! ************************************************************************** !

subroutine printMsg2(option,string)
  !
  ! Prints the message from p0
  !
  ! Author: Glenn Hammond
  ! Date: 11/14/07
  !

  implicit none

  type(option_type) :: option
  character(len=*) :: string

  if (OptionPrintToScreen(option)) print *, trim(string)

end subroutine printMsg2

! ************************************************************************** !

subroutine printMsgAnyRank1(option)
  !
  ! Prints the message from any processor core
  !
  ! Author: Glenn Hammond
  ! Date: 01/12/12
  !

  implicit none

  type(option_type) :: option

  if (option%print_to_screen) call printMsgAnyRank2(option%io_buffer)

end subroutine printMsgAnyRank1

! ************************************************************************** !

subroutine printMsgAnyRank2(string)
  !
  ! Prints the message from any processor core
  !
  ! Author: Glenn Hammond
  ! Date: 01/12/12
  !

  implicit none

  character(len=*) :: string
  
  print *, trim(string)

end subroutine printMsgAnyRank2

! ************************************************************************** !

subroutine printMsgByRank1(option)
  !
  ! Prints a message from processor along with rank
  !
  ! Author: Glenn Hammond
  ! Date: 03/27/12
  !

  implicit none

  type(option_type) :: option

  call printMsgByRank2(option,option%io_buffer)

end subroutine printMsgByRank1

! ************************************************************************** !

subroutine printMsgByRank2(option,string)
  !
  ! Prints a message from processor along with rank
  !
  ! Author: Glenn Hammond
  ! Date: 03/27/12
  !

  implicit none

  type(option_type) :: option
  character(len=*) :: string

  character(len=MAXWORDLENGTH) :: word

  if (option%print_to_screen) then
    write(word,*) option%myrank
    print *, '(' // trim(adjustl(word)) // '): ' // trim(string)
  endif

end subroutine printMsgByRank2

! ************************************************************************** !

subroutine printMsgByCell(option,cell_id,string)
  !
  ! Prints the message from p0
  !
  ! Author: Glenn Hammond
  ! Date: 11/14/07
  !

  implicit none

  type(option_type) :: option
  PetscInt :: cell_id
  character(len=*) :: string

  character(len=MAXWORDLENGTH) :: word

  write(word,*) cell_id
  word = adjustl(word)
  option%io_buffer = trim(string) // ' for cell ' // trim(word) // '.'
  call printMsgByRank(option)

end subroutine printMsgByCell

! ************************************************************************** !

function OptionPrintToScreen(option)
  !
  ! Determines whether printing should occur
  !
  ! Author: Glenn Hammond
  ! Date: 12/09/08
  !

  implicit none

  type(option_type) :: option

  PetscBool :: OptionPrintToScreen

  if (option%myrank == option%io_rank .and. option%print_to_screen) then
    OptionPrintToScreen = PETSC_TRUE
  else
    OptionPrintToScreen = PETSC_FALSE
  endif

end function OptionPrintToScreen

! ************************************************************************** !

function OptionPrintToFile(option)
  !
  ! Determines whether printing to file should occur
  !
  ! Author: Glenn Hammond
  ! Date: 01/29/09
  !

  implicit none

  type(option_type) :: option

  PetscBool :: OptionPrintToFile

  if (option%myrank == option%io_rank .and. option%print_to_file) then
    OptionPrintToFile = PETSC_TRUE
  else
    OptionPrintToFile = PETSC_FALSE
  endif

end function OptionPrintToFile

! ************************************************************************** !

subroutine OptionPrint(string,option)
  !
  ! Determines whether printing to file should occur
  !
  ! Author: Glenn Hammond
  ! Date: 01/29/09
  !
  use PFLOTRAN_Constants_module

  implicit none

  character(len=*) :: string
  type(option_type) :: option

  ! note that these flags can be toggled off specific time steps
  if (option%print_screen_flag) then
    write(STDOUT_UNIT,'(a)') trim(string)
  endif
  if (option%print_file_flag) then
    write(option%fid_out,'(a)') trim(string)
  endif

end subroutine OptionPrint

! ************************************************************************** !

subroutine OptionInitMPI1(option)
  !
  ! Initializes base MPI communicator
  !
  ! Author: Glenn Hammond
  ! Date: 06/06/13
  !

  implicit none

  type(option_type) :: option

  PetscErrorCode :: ierr

  call MPI_Init(ierr)
  call OptionInitMPI2(option,MPI_COMM_WORLD)

end subroutine OptionInitMPI1

! ************************************************************************** !

subroutine OptionInitMPI2(option,communicator)
  !
  ! Initializes base MPI communicator
  !
  ! Author: Glenn Hammond
  ! Date: 06/06/13
  !

  implicit none

  type(option_type) :: option

  PetscMPIInt :: communicator
  PetscErrorCode :: ierr

  option%global_comm = communicator
  call MPI_Comm_rank(communicator,option%global_rank, ierr)
  call MPI_Comm_size(communicator,option%global_commsize,ierr)
  call MPI_Comm_group(communicator,option%global_group,ierr)
  option%mycomm = option%global_comm
  option%myrank = option%global_rank
  option%mycommsize = option%global_commsize
  option%mygroup = option%global_group

end subroutine OptionInitMPI2

! ************************************************************************** !

subroutine OptionInitPetsc(option)
  !
  ! Initialization of PETSc.
  !
  ! Author: Glenn Hammond
  ! Date: 06/07/13
  !

  implicit none

  type(option_type) :: option

  character(len=MAXSTRINGLENGTH) :: string
  PetscErrorCode :: ierr

  PETSC_COMM_WORLD = option%mycomm
  call PetscInitialize(PETSC_NULL_CHARACTER, ierr);CHKERRQ(ierr)    !fmy: tiny memory leak here (don't know why)

end subroutine OptionInitPetsc

! ************************************************************************** !

subroutine OptionFinalize(option)
  !
  ! End the simulation.
  !
  ! Author: Glenn Hammond
  ! Date: 06/07/13
  !

  implicit none

  type(option_type), pointer :: option

  PetscInt :: iflag
  PetscErrorCode :: ierr

  call PetscOptionsSetValue(PETSC_NULL_OPTIONS, &
                            '-options_left','no',ierr);CHKERRQ(ierr)
  ! list any PETSc objects that have not been freed - for debugging
  call PetscOptionsSetValue(PETSC_NULL_OPTIONS, &
                            '-objects_left','yes',ierr);CHKERRQ(ierr)
  call MPI_Barrier(option%global_comm,ierr)
  iflag = option%successful_exit_code
  call OptionDestroy(option)
  call PetscFinalize(ierr);CHKERRQ(ierr)
  call MPI_Finalize(ierr)
  call exit(iflag)

end subroutine OptionFinalize

! ************************************************************************** !

subroutine OptionDestroy(option)
  !
  ! Deallocates an option
  !
  ! Author: Glenn Hammond
  ! Date: 10/26/07
  !

  implicit none

  type(option_type), pointer :: option

  ! all the below should be placed somewhere other than option.F90

  deallocate(option)
  nullify(option)

end subroutine OptionDestroy

end module Option_module
